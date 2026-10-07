program test_test_results_json
    use, intrinsic :: iso_fortran_env, only: real64
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_equal_integer
    use fo_test_harness, only: assert_equal_string, assert_true, assert_process_ok
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_number, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_number_value
    use fo_test_harness, only: finish_assertions
    implicit none

    integer, parameter :: name_count = 402
    character(len=1400) :: names(name_count)
    character(:), allocatable :: driver, scratch, cmake, test_name, command_name
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    type(json_value_t) :: report, tests, summary, entry, field
    integer :: i, j, position
    logical :: seen(name_count)
    real(real64) :: seconds

    call resolve_driver(driver)
    call make_scratch('fo-test-json', scratch)
    call list_add(environment, 'FO_TEST_TIMEOUT=120')
    do i = 1, 397
        names(i) = 'test_report_' // integer_text(i)
    end do
    names(398) = 'test_quoted_"name'
    names(399) = 'test_backslash_\name'
    names(400) = 'test_late_failure'
    names(401) = 'test_long_name_' // repeat('a', 1200)
    names(402) = 'test_long_name_' // repeat('b', 1300)

    cmake = 'cmake_minimum_required(VERSION 3.20)' // new_line('a') // &
        'project(fo_json_report NONE)' // new_line('a') // 'enable_testing()' // new_line('a')
    do i = 1, name_count
        command_name = 'true'
        if (i == 400) command_name = 'false'
        cmake = cmake // 'add_test(NAME [=[' // trim(names(i)) // &
            ']=] COMMAND "${CMAKE_COMMAND}" -E ' // trim(command_name) // ')' // new_line('a')
    end do
    call write_text(join_path(scratch, 'CMakeLists.txt'), cmake)

    arguments = string_list_t()
    call list_add(arguments, 'build')
    call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
    call assert_process_ok(result, 'configure CTest report fixture')
    arguments = string_list_t()
    call list_add(arguments, 'test')
    call list_add(arguments, '--json')
    call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
    call assert_equal_integer(result%term_signal, 0, 'large test report command exits normally')
    call assert_equal_integer(result%exit_code, 1, 'late CTest failure propagates nonzero status')
    call parse_json_report(result, report, 'large test result JSON')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), name_count, 'every result is retained')
    seen = .false.
    do i = 1, json_size(tests)
        entry = json_element(tests, i)
        field = json_member(entry, 'name')
        test_name = json_string_value(field)
        position = 0
        do j = 1, name_count
            if (test_name == trim(names(j))) then
                position = j
                exit
            end if
        end do
        call assert_true(position > 0, 'every plain and escaped test name survives JSON generation')
        if (position <= 0) cycle
        call assert_true(.not. seen(position), 'test report retains each name exactly once')
        seen(position) = .true.
        field = json_member(entry, 'status')
        command_name = 'pass'
        if (test_name == 'test_late_failure') command_name = 'fail'
        call assert_equal_string(json_string_value(field), command_name, &
            'the expected CTest result status survives')
        field = json_member(entry, 'seconds')
        call assert_equal_integer(field%kind, json_number, 'test duration is a JSON number')
        seconds = json_number_value(field)
        call assert_true(seconds >= 0.0_real64, 'test duration is nonnegative')
    end do
    do i = 1, name_count
        call assert_true(seen(i), 'report includes every expected test name')
    end do

    summary = json_member(report, 'summary')
    field = json_member(summary, 'passed')
    call assert_equal_integer(nint(json_number_value(field)), 401, '401 results pass')
    field = json_member(summary, 'failed')
    call assert_equal_integer(nint(json_number_value(field)), 1, 'one result fails')
    field = json_member(summary, 'skipped')
    call assert_equal_integer(nint(json_number_value(field)), 0, 'no results are skipped')
    field = json_member(summary, 'total_seconds')
    call assert_equal_integer(field%kind, json_number, 'total duration is a JSON number')
    call assert_true(json_number_value(field) >= 0.0_real64, 'total duration is nonnegative')
    field = json_member(report, 'exit_code')
    call assert_true(nint(json_number_value(field)) /= 0, 'JSON report retains nonzero exit code')
    call assert_true(len(result%stdout) > 16384, 'report exceeds the former output limit')
    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a,i0,a,i0,a)') 'test-results-json: ', name_count, &
        ' complete entries, ', len(result%stdout), ' bytes; failure and escapes preserved'

contains

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

end program test_test_results_json
