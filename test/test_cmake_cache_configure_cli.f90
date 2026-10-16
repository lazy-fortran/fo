program test_cmake_cache_configure_cli
    !! The one-shot PASS cache computes its case digests from the configure the
    !! run already made: an unchanged second `fo test` on CMake invokes CMake
    !! configure as often with the cache as without it.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory, write_text
    use fo_test_harness, only: read_text, remove_path, remove_tree, environment_value
    use fo_test_harness, only: assert_equal_integer, assert_true, assert_contains
    use fo_test_harness, only: finish_assertions
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

    call remove_tree(scratch)
    call finish_assertions()

contains

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
