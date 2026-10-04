program test_archive_metadata_cli
    use fo_test_archive_fixture, only: archive_fixture_main, archive_fixture_exit
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, make_directory, make_symlink
    use fo_test_harness, only: join_path, write_text, remove_tree
    use fo_test_harness, only: current_directory, file_exists
    use fo_test_harness, only: assert_process_ok, assert_equal_string, assert_true
    use fo_test_harness, only: assert_contains
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_fs, only: fs_find_executable
    implicit none

    character(:), allocatable :: driver, scratch, dependency, consumer
    character(:), allocatable :: tools, ar_path, archiver_link, platform, native_path
    character(:), allocatable :: self_executable
    character(len=4096) :: helper_ar
    type(process_result_t) :: result, lookup
    type(string_list_t) :: arguments, environment
    integer :: helper_status
    logical :: found

    helper_ar = ''
    call get_environment_variable('FO_ARCHIVE_HELPER_REAL_AR', helper_ar, &
        status=helper_status)
    if (helper_status == 0 .and. len_trim(helper_ar) > 0) then
        call archive_fixture_exit(archive_fixture_main(trim(helper_ar)))
    end if

    call resolve_driver(driver)
    call resolve_self_executable(self_executable)
    call make_scratch('fo-archive-metadata', scratch)
    tools = join_path(scratch, 'tools')
    dependency = join_path(scratch, 'dependency')
    consumer = join_path(scratch, 'consumer')
    archiver_link = join_path(tools, 'ar')
    call make_directory(tools)

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
    call make_symlink(self_executable, archiver_link)

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

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'archive-metadata-cli: native/controlled index accepted; invalid members rejected'

contains

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
        character(:), allocatable :: cwd
        integer :: status
        logical :: found

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
