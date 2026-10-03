program test_atomic_link_artifacts
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: remove_path, file_exists, assert_process_ok
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_file_exists
    use fo_test_harness, only: finish_assertions, spawn_process, poll_process
    use fo_test_harness, only: signal_process_group
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, project, fake_bin, fake_ar
    character(:), allocatable :: library_dir, archive, marker, cache, real_ar
    character(:), allocatable :: path_value
    type(string_list_t) :: args, env
    type(process_result_t) :: result, external
    integer :: child, status, archive_count

    call resolve_driver(driver)
    call make_scratch('fo-atomic-link-fortran', scratch)
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
    call wait_for(marker, 100)
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
    marker = join_path(scratch, 'hold-marker')
    if (file_exists(marker)) call remove_path(marker)
    args = words(['build'])
    env = build_environment('hold')
    call spawn_process(command(driver, args), project, child, env)
    call wait_for(marker, 100)
    call assert_file_exists(marker, 'partial archiver entered hold mode')
    call signal_process_group(child, 15)
    call poll_until(child, status)
    call assert_true(status < 0, 'interrupted producer exits by signal')
    call find_archive(archive, archive_count)
    call assert_equal_integer(archive_count, 2, &
        'interrupted partial output is not published as a reusable archive')
    call run_build('normal', result)
    call assert_process_ok(result, 'later producer recovers after interruption')
    call run_program(result)
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'recovered archive links and runs')

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
        call run_fo(driver, command, project, cache, child_result, environment, 120000)
        call assert_process_ok(child_result, 'run linked archive consumer')
    end subroutine run_program

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

    subroutine wait_for(path, limit)
        character(len=*), intent(in) :: path
        integer, intent(in) :: limit
        integer :: i

        do i = 1, limit
            if (file_exists(path)) return
            call pause_briefly()
        end do
    end subroutine wait_for

    subroutine pause_briefly()
        type(string_list_t) :: command

        call list_add(command, '0.02')
        call run_external('/bin/sleep', command, scratch, external)
    end subroutine pause_briefly

    subroutine poll_until(process_id, exit_status)
        integer, intent(in) :: process_id
        integer, intent(out) :: exit_status
        integer :: i

        exit_status = 999
        do i = 1, 300
            call poll_process(process_id, exit_status)
            if (exit_status /= 999) return
            call pause_briefly()
        end do
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

end program test_atomic_link_artifacts
