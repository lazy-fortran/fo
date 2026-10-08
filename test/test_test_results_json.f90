program test_test_results_json
    use, intrinsic :: iso_fortran_env, only: real64
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_equal_integer
    use fo_test_harness, only: assert_equal_string, assert_true, assert_process_ok
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_number, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_number_value, json_parse
    use fo_test_results, only: test_result_entry_t, parse_test_results, &
        format_test_results_json, format_test_results_text
    use fo_test_harness, only: finish_assertions
    implicit none

    integer, parameter :: name_count = 402
    character(len=1400) :: names(name_count)
    character(:), allocatable :: driver, scratch, cmake, test_name, command_name
    character(:), allocatable :: formatted, parse_message
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    type(json_value_t) :: report, tests, summary, entry, field
    type(test_result_entry_t), allocatable :: parsed(:)
    integer :: i, j, position, n_parsed, parse_error
    logical :: seen(name_count), valid
    real(real64) :: seconds

    call resolve_driver(driver)
    call make_scratch('fo-test-json', scratch)
    call list_add(environment, 'FO_TEST_TIMEOUT=120')
    call list_add(environment, 'TMPDIR='//scratch)
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
    call write_text(join_path(scratch, 'malformed-results.log'), &
        'TEST_RESULT test_ok PASS - 0.00' // new_line('a') // &
        'TEST_RESULT test_broken' // new_line('a'))
    call parse_test_results(join_path(scratch, 'malformed-results.log'), &
        parsed, n_parsed, parse_error)
    call assert_true(parse_error /= 0, 'malformed result is rejected explicitly')
    call write_text(join_path(scratch, 'invalid-status.log'), &
        'TEST_RESULT test_bad UNKNOWN - 0.01'//new_line('a'))
    call parse_test_results(join_path(scratch, 'invalid-status.log'), &
        parsed, n_parsed, parse_error)
    call assert_true(parse_error /= 0, 'unknown native status is reported')
    call write_text(join_path(scratch, 'invalid-exit.log'), &
        'TEST_RESULT test_bad FAIL garbage 0.01'//new_line('a'))
    call parse_test_results(join_path(scratch, 'invalid-exit.log'), &
        parsed, n_parsed, parse_error)
    call assert_true(parse_error /= 0, 'malformed exit status is reported')
    call write_text(join_path(scratch, 'invalid-time.log'), &
        'TEST_RESULT test_bad PASS - NaN'//new_line('a'))
    call parse_test_results(join_path(scratch, 'invalid-time.log'), &
        parsed, n_parsed, parse_error)
    call assert_true(parse_error /= 0, 'nonfinite duration is reported')
    call write_text(join_path(scratch, 'long-duration.log'), &
        'TEST_RESULT long_first PASS - 60000.50'//new_line('a')// &
        'TEST_RESULT long_second PASS - 60000.50'//new_line('a'))
    call parse_test_results(join_path(scratch, 'long-duration.log'), &
        parsed, n_parsed, parse_error)
    call assert_equal_integer(parse_error, 0, 'supported long durations parse')
    call format_test_results_json(parsed, n_parsed, 0, formatted)
    call json_parse(formatted, report, valid, parse_message)
    call assert_true(valid, &
        'aggregate duration beyond fixed numeric width is valid JSON')
    summary = json_member(report, 'summary')
    field = json_member(summary, 'total_seconds')
    call assert_true(abs(json_number_value(field) - 120001.0_real64) < 0.01_real64, &
        'aggregate duration remains a complete exact JSON number')
    call format_test_results_text(parsed, n_parsed, &
        join_path(scratch, 'long-duration.log'), .false., formatted)
    call assert_true(index(formatted, '120001.00s)') > 0, &
        'human summary retains duration beyond its former numeric width')
    call write_text(join_path(scratch, 'negative-zero.log'), &
        'TEST_RESULT zero PASS - -0.0'//new_line('a'))
    call parse_test_results(join_path(scratch, 'negative-zero.log'), &
        parsed, n_parsed, parse_error)
    call assert_equal_integer(parse_error, 0, 'finite negative zero duration parses')
    call format_test_results_json(parsed, n_parsed, 0, formatted)
    call json_parse(formatted, report, valid, parse_message)
    call assert_true(valid, 'negative zero duration produces valid JSON numbers')
    call write_text(join_path(scratch, 'large-finite-duration.log'), &
        'TEST_RESULT huge_first PASS - 3.0e38'//new_line('a')// &
        'TEST_RESULT huge_second PASS - 3.0e38'//new_line('a'))
    call parse_test_results(join_path(scratch, 'large-finite-duration.log'), &
        parsed, n_parsed, parse_error)
    call assert_equal_integer(parse_error, 0, 'two large finite durations parse')
    call format_test_results_json(parsed, n_parsed, 0, formatted)
    call json_parse(formatted, report, valid, parse_message)
    call assert_true(valid, 'large finite duration sum remains valid JSON')
    summary = json_member(report, 'summary')
    field = json_member(summary, 'total_seconds')
    call assert_true(abs(json_number_value(field)/6.0e38_real64 - 1.0_real64) < &
        1.0e-6_real64, 'large finite durations accumulate without overflow')
    test_name = 'BUILD_FAIL "quoted" café '//repeat('d', 24000)//' END  '//new_line('a')
    call write_text(join_path(scratch, 'build-failure.log'), test_name)
    call parse_test_results(join_path(scratch, 'build-failure.log'), &
        parsed, n_parsed, parse_error)
    call format_test_results_json(parsed, n_parsed, 1, formatted, &
        join_path(scratch, 'build-failure.log'))
    call json_parse(formatted, report, valid, parse_message)
    call assert_true(valid, 'failure without executed tests produces valid JSON')
    field = json_member(report, 'output')
    call assert_equal_string(json_string_value(field), test_name, &
        'failure without executed tests preserves the complete raw diagnostic log')
    call test_public_compilation_failure()
    call test_public_malformed_ctest_record()
    call write_text(join_path(scratch, 'malformed-ctest-results.log'), &
        '1/1 Test #1: test_missing_status'//new_line('a'))
    call parse_test_results(join_path(scratch, 'malformed-ctest-results.log'), &
        parsed, n_parsed, parse_error)
    call assert_true(parse_error /= 0, &
        'malformed CTest result is rejected instead of silently omitted')
    call write_text(join_path(scratch, 'status-words-in-names.log'), &
        '1/3 Test #1: test_Passed_(Disabled) ... Passed 0.01 sec'//new_line('a')// &
        '2/3 Test #2: test_Skipped_word ... Skipped 0.02 sec'//new_line('a')// &
        '3/4 Test #3: test_Not_Run_word ... Not Run 0.00 sec'//new_line('a')// &
        '4/4 Test #4: test_disabled ... Not Run (Disabled) 0.00 sec'//new_line('a'))
    call parse_test_results(join_path(scratch, 'status-words-in-names.log'), &
        parsed, n_parsed, parse_error)
    call assert_equal_integer(parse_error, 0, 'CTest status-word names parse')
    call assert_equal_integer(n_parsed, 4, 'all CTest status-word names remain')
    if (n_parsed >= 4) then
        call assert_equal_string(parsed(1)%name, 'test_Passed_(Disabled)', &
            'Passed in a test name is not treated as its status')
        call assert_equal_string(parsed(1)%status, 'PASS', 'CTest pass status')
        call assert_equal_string(parsed(1)%reason, '', &
            'Disabled in a passing test name does not create a skipped cause')
        call assert_equal_string(parsed(2)%name, 'test_Skipped_word', &
            'Skipped in a test name is not treated as its status')
        call assert_equal_string(parsed(2)%status, 'SKIP', 'CTest skipped status')
        call assert_equal_string(parsed(3)%name, 'test_Not_Run_word', &
            'Not Run in a test name is not treated as its status')
        call assert_equal_string(parsed(3)%status, 'UNTESTED', 'CTest not-run status')
        call assert_equal_string(parsed(4)%status, 'SKIP', &
            'disabled CTest remains skip')
        call format_test_results_json(parsed, n_parsed, 1, formatted)
        call json_parse(formatted, report, valid, parse_message)
        call assert_true(valid, 'mixed CTest statuses produce valid JSON')
        summary = json_member(report, 'summary')
        field = json_member(summary, 'untested')
        call assert_equal_integer(int(json_number_value(field)), 1, &
            'one untested CTest is counted separately')
        field = json_member(summary, 'skipped')
        call assert_equal_integer(int(json_number_value(field)), 2, &
            'only disabled and explicitly skipped tests count as skips')
        call format_test_results_text(parsed, n_parsed, &
            join_path(scratch, 'status-words-in-names.log'), .true., formatted)
        call assert_true(index(formatted, &
            'Summary: 1 passed, 0 failed, 2 skipped, 1 untested (') > 0, &
            'human CTest summary separates skipped and untested work')
    end if
    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a,i0,a,i0,a)') 'test-results-json: ', name_count, &
        ' complete entries, ', len(result%stdout), ' bytes; failure and escapes preserved'

contains

    subroutine test_public_malformed_ctest_record()
        type(process_result_t) :: malformed_result
        type(string_list_t) :: malformed_arguments
        type(json_value_t) :: malformed_report, malformed_field

        call write_text(join_path(scratch, 'malformed-result.cmake'), &
            'message("1/1 Test #1: malformed_duration ... Passed NaN sec")'// &
            new_line('a')//'message(FATAL_ERROR "malformed record oracle")')
        call write_text(join_path(scratch, 'CMakeLists.txt'), cmake// &
            'add_test(NAME malformed_ctest_record COMMAND "${CMAKE_COMMAND}"'// &
            ' -P "${CMAKE_CURRENT_SOURCE_DIR}/malformed-result.cmake")'//new_line('a'))
        call list_add(malformed_arguments, 'test')
        call list_add(malformed_arguments, '--json')
        call list_add(malformed_arguments, 'malformed_ctest_record')
        call run_fo(driver, malformed_arguments, scratch, &
            join_path(scratch, 'fo-cache'), malformed_result, environment)
        call assert_equal_integer(malformed_result%term_signal, 0, &
            'CLI malformed CTest result exits normally')
        call assert_equal_integer(malformed_result%exit_code, 1, &
            'CLI malformed CTest result exits nonzero')
        call parse_json_report(malformed_result, malformed_report, &
            'CLI malformed CTest result produces valid error JSON')
        malformed_field = json_member(malformed_report, 'tests')
        call assert_equal_integer(json_size(malformed_field), 0, &
            'CLI malformed results do not claim executed verdicts')
        malformed_field = json_member(malformed_report, 'error')
        call assert_equal_string(json_string_value(malformed_field), &
            'could not parse test results', 'CLI identifies malformed result input')
        malformed_field = json_member(malformed_report, 'exit_code')
        call assert_equal_integer(int(json_number_value(malformed_field)), 1, &
            'CLI malformed result JSON retains its failure status')
    end subroutine test_public_malformed_ctest_record

    subroutine test_public_compilation_failure()
        character(len=:), allocatable :: project
        type(process_result_t) :: failure_result
        type(string_list_t) :: failure_arguments
        type(json_value_t) :: failure_report, failure_field

        project = join_path(scratch, 'compile-failure-project')
        call write_text(join_path(project, 'fpm.toml'), &
            'name = "cli_compile_failure_probe"')
        call write_text(join_path(project, 'test/test_compile_failure.f90'), &
            'program test_compile_failure'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'CLI_COMPILE_DIAGNOSTIC_TOKEN_119'//new_line('a')// &
            'end program'//new_line('a'))
        call list_add(failure_arguments, 'test')
        call list_add(failure_arguments, '--json')
        call list_add(failure_arguments, 'test_compile_failure')
        call run_fo(driver, failure_arguments, project, &
            join_path(project, 'fo-cache'), failure_result, environment)
        call assert_equal_integer(failure_result%term_signal, 0, &
            'CLI failed compilation exits normally')
        call assert_true(failure_result%exit_code /= 0, &
            'CLI failed compilation reports nonzero exit status')
        call parse_json_report(failure_result, failure_report, &
            'CLI failed compilation returns a JSON report on stdout')
        failure_field = json_member(failure_report, 'tests')
        call assert_equal_integer(json_size(failure_field), 0, &
            'CLI failed compilation claims no executed tests')
        failure_field = json_member(failure_report, 'exit_code')
        call assert_true(json_number_value(failure_field) /= 0.0_real64, &
            'CLI failed compilation retains its failure in JSON')
        failure_field = json_member(failure_report, 'output')
        call assert_true(index(json_string_value(failure_field), &
            'CLI_COMPILE_DIAGNOSTIC_TOKEN_119') > 0, &
            'CLI failed compilation retains the compiler diagnostic in JSON')
    end subroutine test_public_compilation_failure

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

end program test_test_results_json
