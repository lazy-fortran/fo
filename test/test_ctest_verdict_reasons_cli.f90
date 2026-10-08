program test_ctest_verdict_reasons_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add, &
        make_scratch, join_path, write_text, assert_true, assert_equal_integer, &
        assert_equal_string, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size, &
        json_string_value
    implicit none
    character(:), allocatable :: driver, scratch
    type(string_list_t) :: args, env
    type(process_result_t) :: observed
    type(json_value_t) :: report, tests, entry

    call resolve_driver(driver)
    call make_scratch('fo-ctest-reasons', scratch)
    call write_text(join_path(scratch, 'CMakeLists.txt'), &
        'cmake_minimum_required(VERSION 3.22)'//new_line('a')// &
        'project(reasons NONE)'//new_line('a')// &
        'enable_testing()'//new_line('a')// &
        'add_test(NAME missing_regex COMMAND "${CMAKE_COMMAND}" -E echo "actual")'// &
        new_line('a')// &
        'set_tests_properties(missing_regex PROPERTIES '// &
        'PASS_REGULAR_EXPRESSION "not printed")'//new_line('a')// &
        'add_test(NAME forbidden_regex COMMAND "${CMAKE_COMMAND}" -E echo "actual")'// &
        new_line('a')// &
        'set_tests_properties(forbidden_regex PROPERTIES '// &
        'FAIL_REGULAR_EXPRESSION "actual")'//new_line('a')// &
        'add_test(NAME expected_failure COMMAND "${CMAKE_COMMAND}" -E false)'// &
        new_line('a')// &
        'set_tests_properties(expected_failure PROPERTIES WILL_FAIL TRUE)')
    call list_add(env, 'FO_BACKEND=cmake')
    call check_case('missing_regex', 'fail', .false.)
    call check_case('forbidden_regex', 'fail', .false.)
    call check_case('expected_failure', 'pass', .true.)
    call finish_assertions()
contains
    subroutine check_case(name, expected, success)
        character(len=*), intent(in) :: name, expected
        logical, intent(in) :: success
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, name)
        call list_add(args, '--json')
        call run_fo(driver, args, scratch, join_path(scratch, 'cache'), observed, env)
        call assert_true((observed%exit_code == 0) .eqv. success, &
            'process verdict agrees with native CTest for '//name)
        call parse_json_report(observed, report, 'registered CTest reason is JSON')
        tests = json_member(report, 'tests')
        call assert_equal_integer(json_size(tests), 1, &
            'registered verdict retained for '//name)
        if (json_size(tests) == 1) then
            entry = json_element(tests, 1)
            call assert_equal_string(json_string_value(json_member(entry, 'status')), &
                expected, 'CTest rules determine verdict for '//name)
        end if
    end subroutine check_case
end program test_ctest_verdict_reasons_cli
