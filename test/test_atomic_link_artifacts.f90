program test_atomic_link_artifacts
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: remove_path, file_exists, assert_process_ok
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_file_exists
    use fo_test_harness, only: finish_assertions, spawn_process, poll_process
    use fo_test_harness, only: terminate_process_group
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, project, fake_bin, fake_ar
    character(:), allocatable :: library_dir, archive, marker, cache, real_ar
    character(:), allocatable :: path_value
    character(:), allocatable :: archive_state_before
    type(string_list_t) :: args, env
    type(process_result_t) :: result, external
    integer :: child = -1, status, archive_count, exec_attempt = 0
    logical :: found, macos

    call resolve_driver(driver)
    call make_scratch('fo-atomic-link-fortran', scratch)
    call run_external('/usr/bin/uname', words([character(len=8) :: '-s']), &
        scratch, external)
    call assert_process_ok(external, 'identify native archive platform')
    macos = external%stdout == 'Darwin' // new_line('a')
    project = join_path(scratch, 'fixture')
    fake_bin = join_path(scratch, 'bin')
    fake_ar = join_path(fake_bin, 'ar')
    cache = join_path(scratch, 'cache')
    library_dir = join_path(project, 'build/fo/lib')
    marker = join_path(scratch, 'archiver-started')
    call make_path(project)
    call make_path(fake_bin)
    real_ar = find_tool('ar')
    call write_fixture()
    call install_archiver()

    call run_build('normal', result)
    call assert_process_ok(result, 'cold archive build succeeds')
    call find_archive(archive, archive_count)
    call assert_equal_integer(archive_count, 1, 'one keyed archive is published')
    call run_program(result)
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'published archive links and executes with expected result')

    call run_build('normal', result)
    call assert_process_ok(result, 'warm archive build succeeds')
    call find_archive(archive, archive_count)
    call assert_equal_integer(archive_count, 1, 'warm build retains one archive')
    call write_text(archive, 'truncated archive')
    call write_text(join_path(project, 'app/main.f90'), app_source('changed consumer'))
    call run_build('normal', result)
    call assert_process_ok(result, 'corrupt cached archive is replaced')
    call run_program(result)
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'repaired archive is independently executable')

    call source_files(39, 3)
    if (file_exists(marker)) call remove_path(marker)
    args = words(['build'])
    env = build_environment('delay')
    call spawn_process(command(driver, args), project, child, env)
    call wait_for(marker, 100, child, found)
    call assert_true(found, 'archiver reached partial-output point')
    call assert_file_exists(marker, 'archiver reached partial-output point')
    call run_build('normal', result)
    call assert_process_ok(result, 'concurrent build consumer succeeds')
    call poll_until(child, status)
    call assert_equal_integer(status, 0, 'concurrent producer succeeds')
    call find_archive(archive, archive_count)
    call assert_equal_integer(archive_count, 2, &
        'changed inputs receive a separate content-key archive')
    call run_program(result)
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'concurrent consumer executes complete published artifact')

    call source_files(38, 4)
    archive_state_before = archive_snapshot()
    marker = join_path(scratch, 'hold-marker')
    if (file_exists(marker)) call remove_path(marker)
    args = words(['build'])
    env = build_environment('hold')
    call spawn_process(command(driver, args), project, child, env)
    call wait_for(marker, 100, child, found)
    call assert_true(found, 'partial archiver entered hold mode')
    call assert_file_exists(marker, 'partial archiver entered hold mode')
    call terminate_owned(child, status)
    call assert_equal_integer(status, -15, 'interrupted producer exits by SIGTERM')
    call assert_equal_string(archive_snapshot(), archive_state_before, &
        'interrupted output preserves archive paths and digests')
    call find_archive(archive, archive_count)
    call assert_equal_integer(archive_count, 2, &
        'interrupted partial output is not published as a reusable archive')
    call run_build('normal', result)
    call assert_process_ok(result, 'later producer recovers after interruption')
    call run_program(result)
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'recovered archive links and runs')

    if (child > 0) call cleanup_child()
    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'atomic-link-artifacts: warm, corrupt, concurrent and interrupted paths pass'

contains

    function words(values) result(list)
        character(len=*), intent(in) :: values(:)
        type(string_list_t) :: list
        integer :: i

        do i = 1, size(values)
            call list_add(list, trim(values(i)))
        end do
    end function words

    subroutine make_path(path)
        character(len=*), intent(in) :: path
        type(string_list_t) :: command

        call list_add(command, '-p')
        call list_add(command, path)
        call run_external('/bin/mkdir', command, scratch, external)
        call assert_process_ok(external, 'create fixture directory')
    end subroutine make_path

    function find_tool(name) result(path)
        character(len=*), intent(in) :: name
        character(:), allocatable :: path
        type(string_list_t) :: command

        call list_add(command, name)
        call run_external('/usr/bin/which', command, scratch, external)
        call assert_process_ok(external, 'locate required fixture tool')
        path = trim(external%stdout)
        if (len(path) > 0) then
            if (path(len(path):) == new_line('a')) path = path(:len(path) - 1)
        end if
    end function find_tool

    subroutine write_fixture()
        call write_text(join_path(project, 'fpm.toml'), 'name = "atomic_link_fixture"' // &
            new_line('a'))
        call source_files(40, 2)
        call write_text(join_path(project, 'app/main.f90'), app_source('initial'))
    end subroutine write_fixture

    subroutine source_files(value, child_value)
        integer, intent(in) :: value, child_value
        character(32) :: value_text, child_text

        write(value_text, '(i0)') value
        write(child_text, '(i0)') child_value
        call write_text(join_path(project, 'src/model.f90'), &
            'module artifact_model' // new_line('a') // 'interface' // new_line('a') // &
            'module integer function root_value()' // new_line('a') // &
            'end function root_value' // new_line('a') // &
            'module integer function leaf_value()' // new_line('a') // &
            'end function leaf_value' // new_line('a') // 'end interface' // &
            new_line('a') // 'end module artifact_model' // new_line('a'))
        call write_text(join_path(project, 'src/10_root.f90'), &
            'submodule (artifact_model) root_impl' // new_line('a') // 'contains' // &
            new_line('a') // 'module procedure root_value' // new_line('a') // &
            'root_value = ' // trim(value_text) // new_line('a') // &
            'end procedure root_value' // new_line('a') // 'end submodule root_impl' // &
            new_line('a'))
        call write_text(join_path(project, 'src/20_leaf.f90'), &
            'submodule (artifact_model:root_impl) leaf_impl' // new_line('a') // &
            'contains' // new_line('a') // 'module procedure leaf_value' // &
            new_line('a') // 'leaf_value = ' // trim(child_text) // new_line('a') // &
            'end procedure leaf_value' // new_line('a') // 'end submodule leaf_impl' // &
            new_line('a'))
    end subroutine source_files

    function app_source(comment) result(source)
        character(len=*), intent(in) :: comment
        character(:), allocatable :: source

        source = 'program artifact_consumer' // new_line('a') // &
            'use artifact_model, only: root_value, leaf_value' // new_line('a') // &
            'implicit none' // new_line('a') // '! ' // trim(comment) // new_line('a') // &
            "print '(i0)', root_value() + leaf_value()" // new_line('a') // &
            'end program artifact_consumer' // new_line('a')
    end function app_source

    subroutine install_archiver()
        call write_text(fake_ar, '#!/bin/sh' // new_line('a') // &
            'if [ "$1" = rcs ]; then' // new_line('a') // &
            '  echo "$FO_TEST_AR_MODE $PPID $2" >> "$FO_TEST_AR_LOG"' // new_line('a') // &
            '  case "$FO_TEST_AR_MODE" in' // new_line('a') // &
            '    delay) : > "$FO_TEST_MARKER"; sleep 1 ;;' // &
            new_line('a') // '    hold) echo partial > "$2"; : > "$FO_TEST_MARKER"; exec sleep 60 ;;' // &
            new_line('a') // '    fail) echo partial > "$2"; exit 37 ;;' // new_line('a') // &
            '    omit) exec "$FO_TEST_REAL_AR" rcs "$2" ;;' // new_line('a') // &
            '  esac' // new_line('a') // 'fi' // new_line('a') // &
            'exec "$FO_TEST_REAL_AR" "$@"' // new_line('a'))
        call make_executable(fake_ar)
    end subroutine install_archiver

    subroutine make_executable(path)
        character(len=*), intent(in) :: path
        type(string_list_t) :: command

        call list_add(command, '755')
        call list_add(command, path)
        call run_external('/bin/chmod', command, scratch, external)
        call assert_process_ok(external, 'mark compiler wrapper executable')
    end subroutine make_executable

    function build_environment(mode) result(environment)
        character(len=*), intent(in) :: mode
        type(string_list_t) :: environment

        path_value = fake_bin // ':' // environment_value('PATH')
        call list_add(environment, 'PATH=' // path_value)
        call list_add(environment, 'FO_JOBS=1')
        call list_add(environment, 'FO_TEST_AR_MODE=' // trim(mode))
        call list_add(environment, 'FO_TEST_REAL_AR=' // real_ar)
        call list_add(environment, 'FO_TEST_AR_LOG=' // join_path(scratch, 'ar.log'))
        call list_add(environment, 'FO_TEST_MARKER=' // marker)
    end function build_environment

    function environment_value(name) result(value)
        character(len=*), intent(in) :: name
        character(:), allocatable :: value
        integer :: length

        call get_environment_variable(name, length=length)
        allocate(character(len=length) :: value)
        call get_environment_variable(name, value=value)
    end function environment_value

    subroutine run_build(mode, child_result)
        character(len=*), intent(in) :: mode
        type(process_result_t), intent(out) :: child_result
        type(string_list_t) :: command, environment

        command = words(['build'])
        environment = build_environment(mode)
        call run_fo(driver, command, project, cache, child_result, environment, 120000)
    end subroutine run_build

    subroutine run_program(child_result)
        type(process_result_t), intent(out) :: child_result
        type(string_list_t) :: command, environment

        command = words([character(len=32) :: 'exec', '--no-build', 'atomic_link_fixture'])
        environment = build_environment('normal')
        exec_attempt = exec_attempt + 1
        call run_fo(driver, command, project, cache, child_result, environment, 120000)
        if (child_result%exit_code /= 0) call describe_launch_failure(child_result)
        call assert_process_ok(child_result, 'run linked archive consumer')
    end subroutine run_program

    subroutine describe_launch_failure(failed)
        type(process_result_t), intent(in) :: failed
        character(:), allocatable :: target, digest
        character(len=4096) :: evidence_dir
        type(process_result_t) :: inspected
        integer :: env_status

        target = join_path(project, 'build/fo/bin/atomic_link_fixture')
        write (*, '(a,i0)') 'FAILED_EXEC_ATTEMPT ', exec_attempt
        write (*, '(a)') 'FAILED_EXEC_TARGET ' // target
        write (*, '(a)') 'FAILED_EXEC_CACHE ' // cache
        write (*, '(a)') 'FAILED_EXEC_STDOUT ' // failed%stdout
        write (*, '(a)') 'FAILED_EXEC_STDERR ' // failed%stderr
        if (.not. file_exists(target)) return
        digest = file_digest(target)
        write (*, '(a)') 'FAILED_EXEC_SHA256 ' // digest
        call run_external('/bin/ls', words([character(len=512) :: '-l', target]), &
            scratch, inspected)
        write (*, '(a)') inspected%stdout // inspected%stderr
        if (macos) then
            call run_external('/usr/bin/otool', &
                words([character(len=512) :: '-hv', target]), scratch, inspected)
            write (*, '(a)') inspected%stdout // inspected%stderr
        end if
        call get_environment_variable('FO_ARTIFACT_EVIDENCE_DIR', evidence_dir, &
            status=env_status)
        if (env_status /= 0 .or. len_trim(evidence_dir) == 0 .or. len(digest) /= 64) &
            return
        call run_external('/bin/cp', &
            words([character(len=4096) :: target, &
            join_path(trim(evidence_dir), 'failed-exec-' // digest)]), &
            scratch, inspected)
        call assert_process_ok(inspected, 'retain exact failed publication image')
    end subroutine describe_launch_failure

    function command(program, arguments) result(combined)
        character(len=*), intent(in) :: program
        type(string_list_t), intent(in) :: arguments
        type(string_list_t) :: combined
        integer :: i

        call list_add(combined, program)
        if (allocated(arguments%items)) then
            do i = 1, size(arguments%items)
                call list_add(combined, arguments%items(i)%value)
            end do
        end if
    end function command

    subroutine wait_for(path, limit, process_id, found)
        character(len=*), intent(in) :: path
        integer, intent(in) :: limit
        integer, intent(inout) :: process_id
        logical, intent(out) :: found
        integer :: i, ignored_status

        found = .false.
        do i = 1, limit
            if (file_exists(path)) then
                found = .true.
                return
            end if
            call pause_briefly()
        end do
        call terminate_owned(process_id, ignored_status)
    end subroutine wait_for

    subroutine pause_briefly()
        type(string_list_t) :: command

        call list_add(command, '0.02')
        call run_external('/bin/sleep', command, scratch, external)
    end subroutine pause_briefly

    subroutine cleanup_child()
        integer :: ignored_status

        call terminate_owned(child, ignored_status)
    end subroutine cleanup_child

    subroutine terminate_owned(process_id, exit_status)
        integer, intent(inout) :: process_id
        integer, intent(out) :: exit_status
        logical :: owned_reaped

        call terminate_process_group(process_id, exit_status, owned_reaped)
        call assert_true(owned_reaped, 'owned producer is reaped before cleanup')
        if (.not. owned_reaped) error stop 'preserving scratch after reap failure'
    end subroutine terminate_owned

    subroutine poll_until(process_id, exit_status)
        integer, intent(inout) :: process_id
        integer, intent(out) :: exit_status
        integer :: i

        exit_status = 999
        do i = 1, 300
            call poll_process(process_id, exit_status)
            if (exit_status /= 999) then
                if (exit_status == -999 .or. exit_status == -998) then
                    call terminate_owned(process_id, exit_status)
                    call assert_true(.false., 'poll fixture process status')
                else
                    process_id = -1
                end if
                return
            end if
            call pause_briefly()
        end do
        call terminate_owned(process_id, exit_status)
        call assert_true(.false., 'fixture process completes before timeout')
    end subroutine poll_until

    subroutine find_archive(path, count)
        character(:), allocatable, intent(out) :: path
        integer, intent(out) :: count
        type(string_list_t) :: command
        type(process_result_t) :: listing
        integer :: line_start, line_end

        path = ''
        count = 0
        call list_add(command, '-1')
        call list_add(command, library_dir)
        call run_external('/bin/ls', command, project, listing)
        if (listing%exit_code /= 0) return
        line_start = 1
        do while (line_start <= len(listing%stdout))
            line_end = index(listing%stdout(line_start:), new_line('a'))
            if (line_end == 0) line_end = len(listing%stdout) - line_start + 2
            if (index(listing%stdout(line_start:line_start + min(7, line_end - 2)), &
                'objects_') == 1) then
                count = count + 1
                path = join_path(library_dir, listing%stdout(line_start:line_start + line_end - 2))
            end if
            line_start = line_start + line_end
        end do
    end subroutine find_archive

    function archive_snapshot() result(snapshot)
        character(:), allocatable :: snapshot, listing_text, name, path
        type(string_list_t) :: command
        type(process_result_t) :: listing
        integer :: cursor, newline_at, last

        call list_add(command, '-1')
        call list_add(command, library_dir)
        call run_external('/bin/ls', command, project, listing)
        call assert_process_ok(listing, 'list independently published archive paths')
        listing_text = listing%stdout
        snapshot = ''
        cursor = 1
        do while (cursor <= len(listing_text))
            newline_at = index(listing_text(cursor:), new_line('a'))
            if (newline_at == 0) then
                last = len(listing_text)
            else
                last = cursor + newline_at - 2
            end if
            name = listing_text(cursor:last)
            if (len(name) >= 8) then
                if (index(name, 'objects_') == 1) then
                    if (len(name) >= 2) then
                        if (name(len(name) - 1:) == '.a') then
                            path = join_path(library_dir, name)
                            snapshot = snapshot // path // ' ' // file_digest(path) // &
                                new_line('a')
                        end if
                    end if
                end if
            end if
            if (newline_at == 0) exit
            cursor = cursor + newline_at
        end do
    end function archive_snapshot

    function file_digest(path) result(digest)
        character(len=*), intent(in) :: path
        character(:), allocatable :: digest
        type(string_list_t) :: command
        type(process_result_t) :: hashed

        if (macos) then
            call list_add(command, '-a')
            call list_add(command, '256')
        end if
        call list_add(command, path)
        if (macos) then
            call run_external('/usr/bin/shasum', command, project, hashed)
        else
            call run_external('/usr/bin/sha256sum', command, project, hashed)
        end if
        call assert_process_ok(hashed, 'compute independent archive digest')
        digest = ''
        if (len(hashed%stdout) < 64) return
        digest = hashed%stdout(:64)
    end function file_digest

end program test_atomic_link_artifacts
