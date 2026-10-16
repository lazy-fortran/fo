program test_test_cases_twin
    !! Oracle for cases inside test executables (#217): a twin fpm and CMake
    !! fixture with one multi-case executable that implements the fo listing
    !! protocol (FO_TEST_LIST, FO_TEST_CASE) and one scheduling executable
    !! with a steady, a flaky and a slow case. On both backends: `fo test
    !! --list` shows the cases of the built executable, `fo test exe::case`
    !! runs exactly that case in its own process, a case that fails once and
    !! then passes is FLAKY (and fails under --strict-flaky), and a slow case
    !! prints a SLOW line and is terminated as TIMEOUT. Runs the driver named
    !! by FO.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, remove_tree
    use fo_test_harness, only: file_exists, remove_path, make_directory
    use fo_test_harness, only: assert_contains, assert_not_contains
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_cases_fixture, only: cases_fixture_write
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value
    implicit none

    character(:), allocatable :: driver, scratch, cache
    integer :: pass

    call resolve_driver(driver)
    call make_scratch('fo-test-cases', scratch)
    cache = join_path(scratch, 'cache')
    do pass = 1, 2
        call run_backend(merge('fpm  ', 'cmake', pass == 1))
    end do
    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine run_backend(backend)
        character(len=*), intent(in) :: backend
        character(:), allocatable :: dir, marks, state
        type(string_list_t) :: env
        type(process_result_t) :: result

        dir = join_path(scratch, trim(backend))
        marks = join_path(scratch, trim(backend)//'-marks')
        state = join_path(scratch, trim(backend)//'-flaky.state')
        call cases_fixture_write(dir, trim(backend))
        call list_add(env, 'TC_MARK_DIR='//marks)
        call list_add(env, 'FLAKY_STATE='//state)
        call list_add(env, 'FO_JOBS=2')
        if (backend == 'cmake') call list_add(env, 'FO_BACKEND=cmake')
        call make_directory(marks)

        call run(dir, env, 'build', result, [character(len=24) :: 'build', '--tests'])
        call assert_equal_integer(result%exit_code, 0, backend//' twin builds: '// &
            result%stderr)

        ! One case, one process: only the named case runs.
        call run(dir, env, 'list', result, [character(len=24) :: 'test', '--list'])
        call assert_equal_integer(result%exit_code, 0, backend//' lists: '// &
            result%stderr)
        call assert_contains(result%stdout, 'test_multi::one', &
            backend//' lists case one')
        call assert_contains(result%stdout, 'test_multi::two', &
            backend//' lists case two')
        call assert_contains(result%stdout, 'test_sched::flaky', &
            backend//' lists the flaky case')
        call run(dir, env, 'only two', result, [character(len=24) :: 'test', 'test_multi::two', &
            '--json=full'])
        call assert_equal_integer(result%exit_code, 0, backend// &
            ' runs one case: '//result%stdout//result%stderr)
        call assert_case(result, 1, 'test_multi::two', 'pass', backend//' case two')
        call assert_true(file_exists(join_path(marks, 'ran_two')), &
            backend//' case two ran')
        call assert_true(.not. file_exists(join_path(marks, 'ran_one')), &
            backend//' case one did not run')

        ! The executable name selects all of its cases; a glob selects some.
        call remove_path(join_path(marks, 'ran_two'))
        call run(dir, env, 'whole exe', result, [character(len=24) :: 'test', 'test_multi', '--json=full'])
        call assert_equal_integer(result%exit_code, 0, backend// &
            ' runs the executable: '//result%stdout//result%stderr)
        call assert_equal_integer(json_size(json_member(parsed(result), 'tests')), 2, &
            backend//' the executable runs as two cases')
        call assert_true(file_exists(join_path(marks, 'ran_one')) .and. &
            file_exists(join_path(marks, 'ran_two')), backend//' both cases ran')
        call run(dir, env, 'glob', result, [character(len=24) :: 'test', 'test_multi::t*', '--json=full'])
        call assert_equal_integer(json_size(json_member(parsed(result), 'tests')), 1, &
            backend//' a case glob selects one case')
        call run(dir, env, 'unknown', result, [character(len=24) :: 'test', 'test_multi::nope'])
        call assert_true(result%exit_code /= 0, backend// &
            ' an unknown case fails the run')

        ! FLAKY: fails once, passes on the retry; never PASS.
        call remove_path(state)
        call run(dir, env, 'flaky', result, [character(len=24) :: 'test', 'test_sched::flaky', &
            '--json=full'])
        call assert_equal_integer(result%exit_code, 0, backend// &
            ' flaky case exits 0: '//result%stdout//result%stderr)
        call assert_case(result, 1, 'test_sched::flaky', 'flaky', backend//' flaky case')
        call remove_path(state)
        call run(dir, env, 'strict flaky', result, [character(len=24) :: 'test', 'test_sched::flaky', &
            '--strict-flaky'])
        call assert_true(result%exit_code /= 0, backend// &
            ' --strict-flaky fails a flaky case')
        call remove_path(state)
        call run(dir, env, 'no retries', result, [character(len=24) :: 'test', 'test_sched::flaky', &
            '--retries', '0', '--json=full'])
        call assert_case(result, 1, 'test_sched::flaky', 'fail', &
            backend//' --retries 0 leaves the failure')

        ! SLOW: warned after the period, terminated after terminate-after.
        call run(dir, env, 'slow', result, [character(len=24) :: 'test', 'test_sched::slow', '--json=full'])
        call assert_true(result%exit_code /= 0, backend//' a slow case is terminated')
        call assert_case(result, 1, 'test_sched::slow', 'timeout', &
            backend//' slow case')
        call assert_contains(result%stdout//result%stderr, 'SLOW', &
            backend//' a SLOW line is shown')
    end subroutine run_backend

    subroutine run(dir, env, label, result, arguments)
        character(len=*), intent(in) :: dir, label
        type(string_list_t), intent(in) :: env
        type(process_result_t), intent(out) :: result
        character(len=*), intent(in) :: arguments(:)
        type(string_list_t) :: list
        integer :: i

        do i = 1, size(arguments)
            call list_add(list, trim(arguments(i)))
        end do
        call run_fo(driver, list, dir, cache, result, env, 240000)
        if (len_trim(label) == 0) return
    end subroutine run

    function parsed(result) result(report)
        type(process_result_t), intent(in) :: result
        type(json_value_t) :: report

        call parse_json_report(result, report, 'test report')
    end function parsed

    subroutine assert_case(result, index, name, status, label)
        type(process_result_t), intent(in) :: result
        integer, intent(in) :: index
        character(len=*), intent(in) :: name, status, label
        type(json_value_t) :: report, entry

        report = parsed(result)
        entry = json_element(json_member(report, 'tests'), index)
        call assert_equal_string(json_string_value(json_member(entry, 'name')), name, &
            label//' name')
        call assert_equal_string(json_string_value(json_member(entry, 'status')), &
            status, label//' status')
    end subroutine assert_case

end program test_test_cases_twin
