program test_archive_metadata_cli
    use, intrinsic :: iso_c_binding, only: c_char, c_long, c_size_t, c_null_char
    use fo_test_archive_fixture, only: archive_fixture_main, archive_fixture_exit
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, make_directory, make_symlink
    use fo_test_harness, only: join_path, write_text, remove_tree
    use fo_test_harness, only: current_directory, file_exists
    use fo_test_harness, only: assert_process_ok, assert_equal_string, assert_true
    use fo_test_harness, only: assert_equal_integer
    use fo_test_harness, only: assert_contains
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_fs, only: fs_find_executable, fs_copy_exec
    implicit none

    interface
        integer(c_long) function c_readlink(path, buffer, capacity) &
                bind(C, name='readlink')
            import :: c_char, c_long, c_size_t
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: capacity
        end function c_readlink
    end interface

    character(:), allocatable :: driver, scratch, dependency, consumer
    character(:), allocatable :: tools, ar_path, archiver_link, platform, native_path
    character(:), allocatable :: self_executable, helper_image
    character(len=4096) :: helper_ar, helper_path_check
    type(process_result_t) :: result, lookup
    type(string_list_t) :: arguments, environment
    integer :: helper_status
    integer :: copy_status
    logical :: found

    helper_ar = ''
    call get_environment_variable('FO_ARCHIVE_HELPER_REAL_AR', helper_ar, &
        status=helper_status)
    if (helper_status == 0 .and. len_trim(helper_ar) > 0) then
        call archive_fixture_exit(archive_fixture_main(trim(helper_ar)))
    end if

    call resolve_driver(driver)
    call make_scratch('fo-archive-metadata', scratch)
    tools = join_path(scratch, 'tools')
    dependency = join_path(scratch, 'dependency')
    consumer = join_path(scratch, 'consumer')
    archiver_link = join_path(tools, 'ar')
    call make_directory(tools)
    call resolve_self_executable(self_executable)
    helper_image = join_path(tools, 'archive-metadata-helper')
    copy_status = fs_copy_exec(self_executable, helper_image)
    call assert_equal_integer(copy_status, 0, 'copy running test to stable helper image')
    call fs_find_executable(helper_image, helper_path_check, found)
    call assert_true(found, 'copied helper image is executable at an absolute path')
    if (found) helper_image = trim(helper_path_check)

    native_path = '/usr/bin:/bin:/opt/homebrew/bin'
    call list_add(arguments, '-s')
    call run_external('/usr/bin/uname', arguments, scratch, lookup)
    call assert_process_ok(lookup, 'identify host platform')
    platform = first_line(lookup%stdout)
    arguments = string_list_t()
    call fs_find_executable('/usr/bin/ar', helper_ar, found)
    call assert_true(found, 'system archiver is executable at /usr/bin/ar')
    ar_path = trim(helper_ar)
    if (platform == 'Darwin') call assert_equal_string(ar_path, '/usr/bin/ar', &
        'Darwin native archiver is Apple cctools /usr/bin/ar')
    call make_symlink(helper_image, archiver_link)

    call write_text(join_path(dependency, 'fpm.toml'), 'name = "archive_provider"' // new_line('a'))
    call write_text(join_path(dependency, 'src/provider.f90'), provider_source(8))
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "archive_consumer"' // new_line('a') // '[dependencies]' // new_line('a') // &
        'archive_provider = { path = "../dependency" }' // new_line('a'))
    call write_text(join_path(consumer, 'app/probe.f90'), &
        'program probe' // new_line('a') // 'use provider, only: current_value' // new_line('a') // &
        'implicit none' // new_line('a') // "print '(i0)', current_value()" // new_line('a') // &
        'end program probe' // new_line('a'))
    call list_add(environment, 'PATH=' // native_path)

    call expect_value(8, 'native-cache', 'cold archive from the native archiver')
    if (platform == 'Darwin') then
        call verify_native_index(scratch, ar_path)
        write(*, '(a)') 'native Apple ar: ' // ar_path // ' reports __.SYMDEF SORTED'
    end if
    call expect_value(8, 'native-cache', 'warm archive from the native archiver')

    call set_controlled_environment('apple-index')
    call expect_value(8, 'controlled-cache', 'cold archive with controlled Apple symbol index')
    call expect_value(8, 'controlled-cache', 'warm archive with controlled Apple symbol index')

    call expect_rejected('extra-object', 11, 'unexpected object member')
    call expect_rejected('extra-payload', 12, 'unexpected payload member')
    call expect_rejected('missing', 13, 'missing expected object')
    call set_controlled_environment('apple-index')
    call write_text(join_path(dependency, 'src/provider.f90'), provider_source(14))
    call expect_value(14, 'controlled-cache', 'build archive for duplicate-listing preflight')
    call verify_duplicate_listing(scratch)
    call expect_rejected('duplicate', 14, 'duplicate expected object')
    call expect_rejected('corrupt-member', 15, 'archive member differs from its object')
    call expect_rejected('truncate', 16, 'truncated archive is rejected')
    call verify_missing_native_index()

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'archive-metadata-cli: native/controlled index accepted; invalid members rejected'

contains

    subroutine verify_missing_native_index()
        character(:), allocatable :: archive_path, root
        character(len=60) :: header
        type(process_result_t) :: child
        type(string_list_t) :: args, env

        root = join_path(scratch, 'missing-index-preflight')
        archive_path = join_path(root, 'plain.a')
        ! A valid one-member archive without a symbol table, independent of
        ! the helper's listing implementation and of the host ar's defaults.
        write (header, '(a16,a12,a6,a6,a8,a10,a2)') &
            'payload.txt     ', '0', '0', '0', '100644', '8', '`'//new_line('a')
        call write_text(archive_path, '!<arch>'//new_line('a')// &
            header//'payload'//new_line('a'))
        call list_add(args, 't')
        call list_add(args, archive_path)
        call run_external(ar_path, args, root, child)
        call assert_process_ok(child, 'native ar accepts the index-free fixture')
        call assert_equal_string(child%stdout, 'payload.txt'//new_line('a'), &
            'native index-free fixture lists its one payload member')

        call list_add(env, 'FO_ARCHIVE_HELPER_REAL_AR='//ar_path)
        call list_add(env, 'FO_ARCHIVE_MEMBER_MODE=apple-index')
        call list_add(env, 'FO_ARCHIVE_PLATFORM=Darwin')
        call run_external(helper_image, args, root, child, env)
        call assert_equal_integer(child%exit_code, 73, &
            'Darwin policy rejects a listing without its native sorted index')
        call assert_equal_string(child%stdout, 'payload.txt'//new_line('a'), &
            'Darwin policy preserves native listing without a synthetic index')
    end subroutine verify_missing_native_index

    function provider_source(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: digits

        write(digits, '(i0)') value
        text = 'module provider' // new_line('a') // 'implicit none' // new_line('a') // &
            'contains' // new_line('a') // 'integer function current_value()' // new_line('a') // &
            'current_value = ' // trim(digits) // new_line('a') // &
            'end function current_value' // new_line('a') // 'end module provider' // new_line('a')
    end function provider_source

    subroutine expect_value(value, cache_name, context)
        integer, intent(in) :: value
        character(len=*), intent(in) :: cache_name, context
        character(len=32) :: digits

        write(digits, '(i0)') value
        call list_add(arguments, 'exec')
        call list_add(arguments, 'probe')
        call run_fo(driver, arguments, consumer, join_path(scratch, cache_name), result, environment)
        if (result%exit_code /= 0) then
            write(*, '(a)') 'fo stdout: ' // result%stdout
            write(*, '(a)') 'fo stderr: ' // result%stderr
        end if
        call assert_process_ok(result, context)
        call assert_equal_string(result%stdout, trim(digits) // new_line('a'), &
            context // ': linked program returns expected value')
        arguments = string_list_t()
    end subroutine expect_value

    subroutine expect_rejected(mode, value, context)
        character(len=*), intent(in) :: mode, context
        integer, intent(in) :: value

        call write_text(join_path(dependency, 'src/provider.f90'), provider_source(value))
        call set_controlled_environment(mode)
        call list_add(arguments, 'exec')
        call list_add(arguments, 'probe')
        call run_fo(driver, arguments, consumer, join_path(scratch, 'controlled-cache'), &
            result, environment)
        if (result%exit_code == 0) then
            write(*, '(a)') 'unexpected fo stdout: ' // result%stdout
        end if
        call assert_true(result%exit_code /= 0, context // ': archive is rejected')
        arguments = string_list_t()
    end subroutine expect_rejected

    subroutine verify_duplicate_listing(root)
        character(len=*), intent(in) :: root
        character(:), allocatable :: archive_path
        type(process_result_t) :: child

        arguments = string_list_t()
        call list_add(arguments, root)
        call list_add(arguments, '-type')
        call list_add(arguments, 'f')
        call list_add(arguments, '-name')
        call list_add(arguments, '*.a')
        call run_external('/usr/bin/find', arguments, root, child)
        call assert_process_ok(child, 'find archive for duplicate-listing preflight')
        archive_path = first_line(child%stdout)
        call assert_true(len_trim(archive_path) > 0, 'archive exists before duplicate preflight')

        arguments = string_list_t()
        call list_add(arguments, 't')
        call list_add(arguments, archive_path)
        call set_controlled_environment('duplicate')
        call run_external('ar', arguments, consumer, child, environment)
        call assert_process_ok(child, 'duplicate mutation emits a successful archive listing')
        call assert_true(has_duplicate_object(child%stdout), &
            'controlled duplicate listing contains two identical ordinary members')
        arguments = string_list_t()
    end subroutine verify_duplicate_listing

    logical function has_duplicate_object(listing) result(found)
        character(len=*), intent(in) :: listing
        character(len=1024) :: seen(128)
        character(:), allocatable :: member
        integer :: position, newline_position, n_seen, i

        found = .false.
        seen = ''
        n_seen = 0
        position = 1
        do while (position <= len(listing))
            newline_position = index(listing(position:), new_line('a'))
            if (newline_position > 0) then
                if (newline_position > 1) then
                    member = trim(listing(position:position + newline_position - 2))
                else
                    member = ''
                end if
                position = position + newline_position
            else
                member = trim(listing(position:))
                position = len(listing) + 1
            end if
            if (len(member) == 0 .or. index(member, '__.SYMDEF') == 1) cycle
            do i = 1, n_seen
                if (trim(seen(i)) == member) then
                    found = .true.
                    return
                end if
            end do
            if (n_seen < size(seen)) then
                n_seen = n_seen + 1
                seen(n_seen) = member
            end if
        end do
    end function has_duplicate_object

    subroutine verify_native_index(root, real_ar)
        character(len=*), intent(in) :: root, real_ar
        character(:), allocatable :: archive_path
        type(process_result_t) :: child

        arguments = string_list_t()
        call list_add(arguments, root)
        call list_add(arguments, '-type')
        call list_add(arguments, 'f')
        call list_add(arguments, '-name')
        call list_add(arguments, '*.a')
        call run_external('/usr/bin/find', arguments, root, child)
        call assert_process_ok(child, 'find archive built by native archiver')
        archive_path = first_line(child%stdout)
        call assert_true(len_trim(archive_path) > 0, 'native archive exists for index inspection')

        arguments = string_list_t()
        call list_add(arguments, '-t')
        call list_add(arguments, archive_path)
        call run_external(real_ar, arguments, root, child)
        call assert_process_ok(child, 'list members with native Apple ar')
        call assert_contains(child%stdout, '__.SYMDEF SORTED', &
            'real Apple archive contains its native sorted symbol index')
        arguments = string_list_t()
    end subroutine verify_native_index

    subroutine set_controlled_environment(mode)
        character(len=*), intent(in) :: mode

        environment = string_list_t()
        call list_add(environment, 'PATH=' // tools // ':' // native_path)
        call list_add(environment, 'FO_ARCHIVE_HELPER_REAL_AR=' // ar_path)
        call list_add(environment, 'FO_ARCHIVE_PLATFORM=' // platform)
        call list_add(environment, 'FO_ARCHIVE_MEMBER_MODE=' // mode)
    end subroutine set_controlled_environment

    subroutine resolve_self_executable(path)
        character(:), allocatable, intent(out) :: path
        character(len=4096) :: executable, search_path
        character(kind=c_char) :: executable_bytes(4096)
        character(:), allocatable :: cwd, proc_path
        integer(c_long) :: bytes_read
        integer :: status, i
        logical :: found

        bytes_read = c_readlink('/proc/self/exe'//c_null_char, executable_bytes, &
            int(size(executable_bytes), c_size_t))
        if (bytes_read > 0_c_long .and. &
            bytes_read < int(size(executable_bytes), c_long)) then
            allocate(character(len=int(bytes_read)) :: proc_path)
            do i = 1, int(bytes_read)
                proc_path(i:i) = executable_bytes(i)
            end do
            call fs_find_executable(trim(proc_path), search_path, found)
            if (found) then
                path = trim(search_path)
                return
            end if
        end if

        executable = ''
        call get_command_argument(0, executable, status=status)
        call assert_true(status == 0 .and. len_trim(executable) > 0, &
            'archive fixture can read its executable path')
        if (len_trim(executable) == 0) then
            path = ''
            return
        end if
        if (executable(1:1) == '/') then
            path = trim(executable)
        else if (index(trim(executable), '/') > 0) then
            call current_directory(cwd)
            path = join_path(cwd, trim(executable))
        else
            call fs_find_executable(trim(executable), search_path, found)
            if (found) then
                path = trim(search_path)
            else
                call current_directory(cwd)
                path = join_path(cwd, trim(executable))
            end if
        end if
        call assert_true(file_exists(path), &
            'archive fixture executable path names the running test binary')
    end subroutine resolve_self_executable

    function first_line(text) result(line)
        character(len=*), intent(in) :: text
        character(:), allocatable :: line
        integer :: newline_position

        newline_position = index(text, new_line('a'))
        if (newline_position > 0) then
            line = text(:newline_position - 1)
        else
            line = trim(text)
        end if
    end function first_line

end program test_archive_metadata_cli
