program test_cmake_cache_configure_cli
    !! The one-shot PASS cache computes its case digests from the configure the
    !! run already made: an unchanged second `fo test` on CMake invokes CMake
    !! configure as often with the cache as without it.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory, write_text
    use fo_test_harness, only: read_text, remove_path, remove_tree, environment_value
    use fo_test_harness, only: assert_equal_integer, assert_true, assert_contains
    use fo_test_harness, only: finish_assertions
    use fo_test_harness, only: spawn_process, poll_process, sleep_ms
    use fo_test_harness, only: terminate_process_group
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_fs, only: fs_find_executable
    use fo_fake_tool, only: fake_tool_dispatch, install_fake_tool
    implicit none

    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project, shadow, log, path_value
    character(len=4096) :: found
    integer :: with_cache, without_cache
    logical :: located
    type(process_result_t) :: result
    type(string_list_t) :: environment

    call fake_tool_dispatch()
    call resolve_driver(driver)
    call make_scratch('fo-cmake-cache-configure', scratch)
    project = join_path(scratch, 'project')
    shadow = join_path(scratch, 'shadow')
    log = join_path(scratch, 'cmake.log')
    call write_text(join_path(project, 'CMakeLists.txt'), &
        'cmake_minimum_required(VERSION 3.22)'//nl//'project(cache_cfg NONE)'//nl// &
        'enable_testing()'//nl//'add_test(NAME ok COMMAND sh -c true)'//nl)
    call fs_find_executable('cmake', found, located)
    call assert_true(located, 'real cmake is on PATH')
    ! Every cmake call is logged with its arguments, then forwarded.
    call install_fake_tool(join_path(shadow, 'cmake'), &
        'append "'//log//'" "$ARGS\n"'//nl//'exec "'//trim(found)//'" $@'//nl)
    path_value = environment_value('PATH')
    call list_add(environment, 'PATH='//shadow//':'//path_value)
    call list_add(environment, 'FO_BACKEND=cmake')
    call list_add(environment, 'FO_TEST_RERUN=0')
    call list_add(environment, 'TMPDIR='//join_path(scratch, 'tmp'))
    call make_directory(join_path(scratch, 'tmp'))

    call fo_test('cache', 'first run', with_cache, .false.)
    call fo_test('cache', 'unchanged second run', with_cache, .true.)
    call assert_contains(result%stdout, 'reused', 'the second run reuses the PASS: '//result%stdout//result%stderr)
    call fo_test('nocache', 'first run', without_cache, .false.)
    call fo_test('nocache', 'unchanged second run', without_cache, .true.)
    call assert_equal_integer(with_cache, without_cache, &
        'the PASS cache adds no CMake configure to an unchanged run')
    call assert_true(with_cache > 0, 'the run configures at least once')

    ! A built tree is immutable during --no-build selection. In particular,
    ! Gremlin runs parallel children against the same File API reply tree.
    call install_fake_tool(join_path(shadow, 'cmake'), &
        'append "'//log//'" "$ARGS\n"'//nl//'exit 98'//nl)
    call no_build_test(.false.)
    call no_build_test(.true.)
    call parallel_no_build()

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine no_build_test(no_cache)
        logical, intent(in) :: no_cache
        type(string_list_t) :: arguments

        call write_text(log, '')
        call list_add(arguments, 'test')
        call list_add(arguments, '--no-build')
        if (no_cache) call list_add(arguments, '--no-cache')
        call list_add(arguments, 'ok')
        call run_fo(driver, arguments, project, join_path(scratch, 'cache-no-build'), &
            result, environment)
        call assert_equal_integer(result%exit_code, 0, &
            'no-build passes with configure forbidden: '//result%stderr)
        call assert_equal_integer(configure_calls(read_text(log)), 0, &
            'no-build reads existing CMake replies without configuring')
    end subroutine no_build_test

    subroutine parallel_no_build()
        type(string_list_t) :: command, child_environment
        integer :: children(2), status, i, attempt
        logical :: reaped
        character(len=1) :: ordinal
        character(:), allocatable :: execution_cwd

        call write_text(log, '')
        call list_add(command, driver)
        call list_add(command, 'test')
        call list_add(command, '--no-build')
        call list_add(command, '--no-cache')
        call list_add(command, 'ok')
        do i = 1, size(children)
            child_environment = environment
            write(ordinal, '(i1)') i
            execution_cwd = join_path(scratch, 'execution-'//ordinal)
            call make_directory(execution_cwd)
            call list_add(child_environment, 'FO_DISABLE_SELF_REFRESH=1')
            call list_add(child_environment, 'FO_SELF_REFRESH=0')
            call list_add(child_environment, 'FO_GREMLIN_FROZEN_ROOT='//project)
            call list_add(child_environment, 'FO_GREMLIN_EXECUTION_CWD='//execution_cwd)
            call spawn_process(command, project, children(i), child_environment)
        end do
        do i = 1, size(children)
            status = 999
            do attempt = 1, 1500
                call poll_process(children(i), status)
                if (status /= 999) exit
                call sleep_ms(20)
            end do
            if (status == 999) then
                call terminate_process_group(children(i), status, reaped)
                call assert_true(.false., 'parallel no-build child timed out')
            end if
            call assert_equal_integer(status, 0, &
                'parallel Gremlin-shaped no-build child passes')
        end do
        call assert_equal_integer(configure_calls(read_text(log)), 0, &
            'parallel children do not mutate shared CMake replies')
    end subroutine parallel_no_build

    subroutine fo_test(mode, label, configures, count_it)
        character(len=*), intent(in) :: mode, label
        integer, intent(inout) :: configures
        logical, intent(in) :: count_it
        type(string_list_t) :: arguments

        call remove_path(log)
        call list_add(arguments, 'test')
        if (mode == 'nocache') call list_add(arguments, '--no-cache')
        call run_fo(driver, arguments, project, join_path(scratch, 'cache-'//mode), &
            result, environment)
        call assert_equal_integer(result%exit_code, 0, mode//' '//label//' passes')
        if (count_it) configures = configure_calls(read_text(log))
    end subroutine fo_test

    integer function configure_calls(text) result(count)
        !! Logged cmake calls that are not a build, install or script run.
        character(len=*), intent(in) :: text
        integer :: start, finish

        count = 0
        start = 1
        do while (start <= len(text))
            finish = index(text(start:), nl)
            if (finish == 0) finish = len(text) - start + 2
            if (finish > 1) then
                if (index(text(start:start + finish - 2), '--build') /= 1 .and. &
                    index(text(start:start + finish - 2), '-E') /= 1) count = count + 1
            end if
            start = start + finish
        end do
    end function configure_calls

end program test_cmake_cache_configure_cli
