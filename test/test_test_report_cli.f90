program test_test_report_cli
    !! Report artifacts of `fo test`: the verdict names the wall clock whenever
    !! fo measured it (also on the delegated CTest route), each failing run
    !! keeps its own full log under the project build tree, and the JUnit
    !! report fails a flaky case under --strict-flaky.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: make_directory, remove_path, read_text, file_exists
    use fo_test_harness, only: assert_equal_integer, assert_true, assert_contains
    use fo_test_harness, only: assert_not_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_arguments
    implicit none

    character(:), allocatable :: driver, scratch, cache, cmake_dir, first, second
    character(:), allocatable :: xml, junit
    type(process_result_t) :: result
    type(string_list_t) :: environment

    call resolve_driver(driver)
    call make_scratch('fo-test-report', scratch)
    cache = join_path(scratch, 'fo-cache')
    call list_add(environment, 'TMPDIR='//join_path(scratch, 'tmp'))
    call list_add(environment, 'FO_DISABLE_SELF_REFRESH=1')
    call list_add(environment, 'FO_SELF_REFRESH=0')
    call list_add(environment, 'FO_CACHE_DIR='//cache)
    call list_add(environment, 'FO_TEST_TIMEOUT=120')
    call make_directory(join_path(scratch, 'tmp'))

    call check_wall_label_on_delegated_ctest()
    call check_failure_logs_and_junit()

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine run_fo(dir, arguments, extra)
        character(len=*), intent(in) :: dir
        character(len=*), intent(in) :: arguments(:)
        character(len=*), intent(in), optional :: extra
        type(string_list_t) :: list, env
        integer :: i

        call list_add(list, 'test')
        do i = 1, size(arguments)
            call list_add(list, trim(arguments(i)))
        end do
        env = environment
        if (present(extra)) call list_add(env, extra)
        call run_arguments(driver, list, dir, result, env, 300000)
    end subroutine run_fo

    subroutine check_wall_label_on_delegated_ctest()
        cmake_dir = join_path(scratch, 'cmake')
        call make_directory(cmake_dir)
        call write_text(join_path(cmake_dir, 'CMakeLists.txt'), &
            'cmake_minimum_required(VERSION 3.22)'//new_line('a')// &
            'project(wall_label NONE)'//new_line('a')// &
            'enable_testing()'//new_line('a')// &
            'add_test(NAME ok COMMAND sh -c true)'//new_line('a'))
        call run_fo(cmake_dir, [character(len=16) :: '--rerun'], &
            'FO_CTEST_DELEGATE=1')
        call assert_equal_integer(result%exit_code, 0, 'delegated CTest run passes')
        call assert_contains(result%stdout, 's wall)', &
            'the delegated CTest verdict is labelled wall: '//result%stdout)
        call assert_not_contains(result%stdout, 'summed', &
            'the delegated CTest verdict is not summed')
    end subroutine check_wall_label_on_delegated_ctest

    subroutine check_failure_logs_and_junit()
        character(len=*), parameter :: nl = new_line('a')
        character(:), allocatable :: dir

        dir = join_path(scratch, 'fpm')
        call make_directory(join_path(dir, 'test'))
        call write_text(join_path(dir, 'fpm.toml'), 'name = "report_probe"'//nl)
        call write_text(join_path(dir, 'test/test_always_fails.f90'), &
            'program test_always_fails'//nl//"print '(a)', 'ALWAYS_FAILS_MARKER'"// &
            nl//'error stop 3'//nl//'end program test_always_fails'//nl)
        call write_text(join_path(dir, 'test/test_flaky_attempt.f90'), &
            'program test_flaky_attempt'//nl//'implicit none'//nl// &
            'logical :: seen'//nl//'integer :: unit, ios'//nl// &
            "inquire(file='first-attempt.marker', exist=seen)"//nl// &
            'if (.not. seen) then'//nl// &
            "open(newunit=unit, file='first-attempt.marker', status='new', "// &
            'iostat=ios)'//nl//'if (ios == 0) close(unit)'//nl// &
            'error stop 23'//nl//'end if'//nl//'end program test_flaky_attempt'//nl)

        call run_fo(dir, [character(len=300) :: 'test_always_fails', '--rerun'])
        call assert_true(result%exit_code /= 0, 'the failing case fails the run')
        first = log_handle(result%stdout)
        call run_fo(dir, [character(len=300) :: 'test_always_fails', '--rerun'])
        second = log_handle(result%stdout)
        call assert_true(len(first) > 0 .and. len(second) > 0, &
            'both failing runs name a log: '//result%stdout)
        call assert_true(first /= second, 'each failing run has its own log handle')
        call assert_true(index(first, '/build/fo/test-logs/') > 0, &
            'the log lives in the project build tree: '//first)
        call assert_true(file_exists(first) .and. file_exists(second), &
            'both retained logs still exist')
        call assert_contains(read_text(first), 'ALWAYS_FAILS_MARKER', &
            'the retained log holds the case output')

        junit = join_path(scratch, 'flaky.xml')
        call remove_path(join_path(dir, 'first-attempt.marker'))
        call run_fo(dir, [character(len=300) :: 'test_flaky_attempt', '--rerun', &
            '--junit', junit])
        call assert_equal_integer(result%exit_code, 0, 'a flaky case passes by default')
        xml = read_text(junit)
        call assert_contains(xml, '<flakyFailure', 'the flaky case keeps its flaky record')
        call assert_contains(xml, 'failures="0"', 'a tolerated flaky case is no failure')

        call remove_path(join_path(dir, 'first-attempt.marker'))
        call run_fo(dir, [character(len=300) :: 'test_flaky_attempt', '--rerun', &
            '--strict-flaky', '--junit', junit])
        call assert_true(result%exit_code /= 0, '--strict-flaky fails a flaky case')
        xml = read_text(junit)
        call assert_contains(xml, 'failures="1"', &
            '--strict-flaky counts the flaky case as a failure: '//xml)
        call assert_contains(xml, '<failure message="flaky', &
            '--strict-flaky renders the flaky case as a failure')
    end subroutine check_failure_logs_and_junit

    function log_handle(output) result(path)
        !! The path after the `log: ` line of a failing run's report.
        character(len=*), intent(in) :: output
        character(:), allocatable :: path
        integer :: start, finish

        path = ''
        start = index(output, 'log: ')
        if (start == 0) return
        start = start + 5
        finish = index(output(start:), new_line('a'))
        if (finish == 0) then
            path = trim(output(start:))
        else
            path = output(start:start + finish - 2)
        end if
    end function log_handle

end program test_test_report_cli
