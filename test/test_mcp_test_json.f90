program test_mcp_test_json
    use, intrinsic :: iso_fortran_env, only: real64
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_json, only: json_value_t, json_number, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_number_value
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    integer, parameter :: test_count = 402
    character(len=256) :: names(test_count)
    character(:), allocatable :: driver, scratch, cache, input, cmake, name
    character(:), allocatable :: request_args, body, error_message
    type(process_result_t) :: process
    type(json_value_t), allocatable :: responses(:)
    type(json_value_t) :: result_object, content, first, field, report, tests, entry
    type(json_value_t) :: summary
    type(string_list_t) :: arguments
    logical :: framed, valid
    logical :: seen(test_count)
    integer :: i, j, expected_index

    call resolve_driver(driver)
    call make_scratch('fo-mcp-test-json', scratch)
    cache = join_path(scratch, 'cache')
    do i = 1, 397
        names(i) = 'test_report_' // integer_text(i) // '_' // repeat('x', 90)
    end do
    names(398) = 'test_quoted_"name"'
    names(399) = 'test_backslash_\name'
    names(400) = 'test_late_failure'
    names(401) = 'test_named_'//repeat('p', 140)// &
        '_selected'
    names(402) = 'test_named_'//repeat('p', 140)// &
        '_neighbor'
    cmake = 'cmake_minimum_required(VERSION 3.20)' // new_line('a') // &
        'project(fo_mcp_json_report NONE)' // new_line('a') // 'enable_testing()' // new_line('a')
    do i = 1, test_count
        name = trim(names(i))
        if (i == 400) then
            cmake = cmake // 'add_test(NAME [=[' // name // &
                ']=] COMMAND "${CMAKE_COMMAND}" -E false)' // new_line('a')
        else
            cmake = cmake // 'add_test(NAME [=[' // name // &
                ']=] COMMAND "${CMAKE_COMMAND}" -E true)' // new_line('a')
        end if
    end do
    call write_text(join_path(scratch, 'CMakeLists.txt'), cmake)
    arguments = string_list_t()
    call list_add(arguments, 'build')
    call run_fo(driver, arguments, scratch, cache, process)
    call assert_equal_integer(process%exit_code, 0, 'CMake report fixture configures')

    do i = 1, 2
        framed = i == 1
        input = ''
        call append(mcp_request(1, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}'))
        request_args = '{"action":"test","dir":' // mcp_quote(scratch) // &
            ',"json":"full"}'
        call append(mcp_call(2, request_args))
        call append(mcp_call(3, '{"action":"unknown_action"}'))
        request_args = '{"action":"test","dir":'//mcp_quote(scratch)// &
            ',"args":["'//trim(names(401))//'"],"json":"full"}'
        call append(mcp_call(4, request_args))
        call append(mcp_request(5, 'shutdown'))
        call mcp_exchange(driver, scratch, cache, input, framed, responses, process)
        call assert_equal_integer(process%exit_code, 0, 'large-report MCP server exits cleanly')
        result_object = json_member(responses(2), 'result')
        field = json_member(result_object, 'isError')
        call assert_true(field%boolean, 'late test failure remains MCP error')
        content = json_member(result_object, 'content')
        first = json_element(content, 1)
        field = json_member(first, 'text')
        body = json_string_value(field)
        call assert_true(len(body) > 32768, 'complete MCP report exceeds former response limits')
        call json_parse(body, report, valid, error_message)
        call assert_true(valid, 'large escaped MCP report parses as JSON')
        tests = json_member(report, 'tests')
        call assert_equal_integer(json_size(tests), test_count, 'all CTest results survive framing')
        seen = .false.
        do j = 1, json_size(tests)
            entry = json_element(tests, j)
            field = json_member(entry, 'name')
            name = json_string_value(field)
            expected_index = index_of(name)
            call assert_true(expected_index > 0, 'plain and escaped test names are preserved')
            if (expected_index == 0) cycle
            call assert_true(.not. seen(expected_index), 'each report entry occurs once')
            seen(expected_index) = .true.
            field = json_member(entry, 'status')
            if (name == 'test_late_failure') then
                call assert_equal_string(json_string_value(field), 'fail', &
                    'late failure status survives MCP report')
            else
                call assert_equal_string(json_string_value(field), 'pass', &
                    'successful test status survives MCP report')
            end if
            field = json_member(entry, 'seconds')
            call assert_true(field%kind == json_number .and. &
                json_number_value(field) >= 0.0_real64, &
                'test time is a nonnegative JSON number')
        end do
        do j = 1, test_count
            call assert_true(seen(j), 'report retains each requested name')
        end do
        summary = json_member(report, 'summary')
        field = json_member(summary, 'passed')
        call assert_equal_integer(int(json_number_value(field)), 401, &
            'report retains 401 passes')
        field = json_member(summary, 'failed')
        call assert_equal_integer(int(json_number_value(field)), 1, 'report retains one failure')
        field = json_member(report, 'exit_code')
        call assert_true(int(json_number_value(field)) /= 0, 'report retains nonzero exit code')
        result_object = json_member(responses(3), 'error')
        field = json_member(result_object, 'code')
        call assert_equal_integer(int(json_number_value(field)), -32602, &
            'unknown action remains invalid-params error')

        result_object = json_member(responses(4), 'result')
        field = json_member(result_object, 'isError')
        call assert_true(.not. field%boolean, 'long named CTest request passes')
        content = json_member(result_object, 'content')
        first = json_element(content, 1)
        field = json_member(first, 'text')
        body = json_string_value(field)
        call json_parse(body, report, valid, error_message)
        call assert_true(valid, 'long named MCP result parses as JSON')
        tests = json_member(report, 'tests')
        call assert_equal_integer(json_size(tests), 1, &
            'same-prefix neighbor is excluded from named CTest result')
        entry = json_element(tests, 1)
        field = json_member(entry, 'name')
        call assert_equal_string(json_string_value(field), trim(names(401)), &
            'MCP preserves the full requested CTest ID')
        field = json_member(entry, 'status')
        call assert_equal_string(json_string_value(field), 'pass', &
            'long named CTest result reports PASS')
    end do

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'mcp-test-json: 402 results and long named CTest IDs survive MCP'

contains

    subroutine append(message)
        character(len=*), intent(in) :: message

        input = input // mcp_encode(message, framed)
    end subroutine append

    integer function index_of(value) result(found)
        character(len=*), intent(in) :: value
        integer :: k

        found = 0
        do k = 1, test_count
            if (trim(names(k)) == value) then
                found = k
                return
            end if
        end do
    end function index_of

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

end program test_mcp_test_json
