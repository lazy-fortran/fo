program test_flaky_output_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_path
    use fo_test_harness, only: make_directory
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_true, assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_arguments, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value
    implicit none

    character(len=*), parameter :: failure_marker = &
        'UNIQUE_FIRST_ATTEMPT_ASSERTION_DIAGNOSTIC'
    character(len=*), parameter :: retry_marker = 'SECOND_ATTEMPT_PASS_MARKER'
    character(:), allocatable :: driver, scratch, source, child_tmp, cache
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    type(json_value_t) :: report, tests, entry, field

    call resolve_driver(driver)
    call make_scratch('fo-flaky-output', scratch)
    child_tmp = join_path(scratch, 'tmp')
    cache = join_path(scratch, 'fo-cache')
    call make_directory(child_tmp)
    call make_directory(join_path(scratch, 'test'))
    call write_text(join_path(scratch, 'fpm.toml'), 'name = "flaky_output_probe"')
    source = 'program test_flaky_attempt'//new_line('a')// &
        'implicit none'//new_line('a')// &
        'logical :: seen'//new_line('a')// &
        'integer :: unit, ios'//new_line('a')// &
        "inquire(file='first-attempt.marker', exist=seen)"//new_line('a')// &
        'if (.not. seen) then'//new_line('a')// &
        "open(newunit=unit, file='first-attempt.marker', status='new', iostat=ios)"// &
        new_line('a')// &
        'if (ios == 0) close(unit)'//new_line('a')// &
        "write(*, '(a)') '"//failure_marker//"'"//new_line('a')// &
        'error stop 23'//new_line('a')// &
        'end if'//new_line('a')// &
        "write(*, '(a)') '"//retry_marker//"'"//new_line('a')// &
        'end program test_flaky_attempt'//new_line('a')
    call write_text(join_path(scratch, 'test/test_flaky_attempt.f90'), source)
    call list_add(environment, 'TMPDIR='//child_tmp)
    call list_add(environment, 'FO_TEST_TIMEOUT=120')
    call list_add(environment, 'FO_DISABLE_SELF_REFRESH=1')
    call list_add(environment, 'FO_SELF_REFRESH=0')
    call list_add(environment, 'FO_CACHE_DIR='//cache)
    call list_add(environment, 'FO='//driver)
    call list_add(environment, 'FO_BIN='//driver)

    arguments = string_list_t()
    call list_add(arguments, 'test')
    call list_add(arguments, 'test_flaky_attempt')
    call list_add(arguments, '--json')
    call run_arguments(driver, arguments, scratch, result, environment, 120000)
    call assert_process_ok(result, 'named flaky test JSON command succeeds on retry')
    call parse_json_report(result, report, 'flaky test JSON report')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), 1, 'one named test is reported')
    entry = json_element(tests, 1)
    field = json_member(entry, 'name')
    call assert_equal_string(json_string_value(field), 'test_flaky_attempt', &
        'report retains the exact named target')
    field = json_member(entry, 'status')
    call assert_equal_string(json_string_value(field), 'flaky', &
        'report identifies failed-first, passed-retry outcome')
    field = json_member(entry, 'output')
    call assert_true(index(json_string_value(field), failure_marker) > 0, &
        'JSON output retains the first-attempt assertion diagnostic')
    call assert_true(index(json_string_value(field), retry_marker) == 0, &
        'output is limited to the failed first attempt')
    field = json_member(entry, 'attempts')
    call assert_equal_string(json_string_value(json_member(field, 'first')), &
        'fail', 'JSON names the failed first attempt')
    call assert_equal_string(json_string_value(json_member(field, 'retry')), &
        'pass', 'JSON names the passing isolated retry')

    call remove_path(join_path(scratch, 'first-attempt.marker'))
    arguments = string_list_t()
    call list_add(arguments, 'test')
    call list_add(arguments, 'test_flaky_attempt')
    call run_arguments(driver, arguments, scratch, result, environment, 120000)
    call assert_process_ok(result, 'named flaky test human command succeeds on retry')
    call assert_true(index(result%stdout, 'FLAKY') > 0, &
        'human output reports the flaky test outcome')
    call assert_true(index(result%stdout, failure_marker) > 0, &
        'human output retains the first-attempt assertion diagnostic')
    call assert_true(index(result%stdout, 'retry passed') > 0, &
        'human output exposes the passing retry verdict')

    call finish_assertions()
end program test_flaky_output_cli
