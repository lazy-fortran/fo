program test_mcp_test_json
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: real64
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_harness, only: make_directory, environment_value
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_json, only: json_value_t, json_number, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_number_value
    use fo_fs, only: fs_sleep_ms
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start, &
        mcp_session_call, mcp_session_request, mcp_session_shutdown
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    interface
        function c_setenv(key, value, overwrite) bind(C, name='setenv') result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: key(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: rc
        end function c_setenv
        function c_chmod(path, mode) bind(C, name='chmod') result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
            integer(c_int) :: rc
        end function c_chmod
    end interface

    integer, parameter :: test_count = 603
    character(len=256) :: names(test_count)
    character(:), allocatable :: driver, scratch, cache, input, cmake, name
    character(:), allocatable :: request_args, body, error_message, diagnostic
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
    do i = 1, 597
        names(i) = 'test_report_' // integer_text(i) // '_' // repeat('x', 12)
    end do
    names(598) = 'test_quoted_"name"'
    names(599) = 'test_backslash_\name'
    names(600) = 'test_late_failure'
    names(601) = 'test_named_'//repeat('p', 140)// &
        '_selected'
    names(602) = 'test_named_'//repeat('p', 140)// &
        '_neighbor'
    names(603) = 'test_unavailable'
    diagnostic = new_line('a')//'  late diagnostic "quoted" \path café  '// &
        new_line('a')//'Start recovery...  '//new_line('a')// &
        repeat('z', 24000)//' END-DIAG  '
    call write_text(join_path(scratch, 'fail.cmake'), &
        'message([=['//diagnostic//']=])'//new_line('a')// &
        'message(FATAL_ERROR "fixture fails")'//new_line('a'))
    cmake = 'cmake_minimum_required(VERSION 3.20)' // new_line('a') // &
        'project(fo_mcp_json_report NONE)' // new_line('a') // 'enable_testing()' // new_line('a')
    do i = 1, test_count
        name = trim(names(i))
        if (i == 603) then
            cmake = cmake//'add_test(NAME test_unavailable COMMAND '// &
                '"${CMAKE_CURRENT_SOURCE_DIR}/missing_executable")'//new_line('a')
        else if (i == 600) then
            cmake = cmake // 'add_test(NAME [=[' // name // &
                ']=] COMMAND "${CMAKE_COMMAND}" -P '// &
                '"${CMAKE_CURRENT_SOURCE_DIR}/fail.cmake")' // new_line('a')
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
            ',"args":["'//trim(names(601))//'"],"json":"full"}'
        call append(mcp_call(4, request_args))
        request_args = '{"action":"test","dir":'//mcp_quote(scratch)//',"args":['
        do j = 1, test_count - 1
            if (j > 1) request_args = request_args//','
            request_args = request_args//mcp_quote(trim(names(j)))
        end do
        request_args = request_args//']'
        call append(mcp_call(5, request_args//',"json":"full"}'))
        call append(mcp_call(6, request_args//'}'))
        call append(mcp_call(7, '{"action":"test","dir":'//mcp_quote(scratch)// &
            ',"args":["'//trim(names(601))//'",false]}'))
        call append(mcp_call(8, '{"action":"test","dir":'//mcp_quote(scratch)// &
            ',"args":["test_unavailable"],"json":"full"}'))
        call append(mcp_call(9, '{"action":"test","dir":'//mcp_quote(scratch)// &
            ',"json":false}'))
        call append(mcp_request(10, 'shutdown'))
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
                field = json_member(entry, 'output')
                call assert_true(index(json_string_value(field), &
                    'Start recovery...  '//new_line('a')) > 0, &
                    'diagnostic Start recovery and trailing spaces survive MCP JSON')
                call assert_true(index(json_string_value(field), &
                    '  late diagnostic "quoted" \path café  ') > 0, &
                    'escaped UTF-8 diagnostic and spaces survive MCP JSON')
                call assert_true(index(json_string_value(field), &
                    repeat('z', 24000)//' END-DIAG  ') > 0, &
                    'complete long CTest diagnostic survives MCP JSON')
            else if (name == 'test_unavailable') then
                call assert_equal_string(json_string_value(field), 'untested', &
                    'missing executable remains untested rather than executed skip')
                field = json_member(entry, 'reason')
                call assert_true(index(json_string_value(field), &
                    'missing_executable') > 0, &
                    'untested result retains its missing-executable cause')
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
        call assert_equal_integer(int(json_number_value(field)), 601, &
            'report retains 601 passes')
        field = json_member(summary, 'untested')
        call assert_equal_integer(int(json_number_value(field)), 1, &
            'report retains one separate untested case')
        field = json_member(summary, 'skipped')
        call assert_equal_integer(int(json_number_value(field)), 0, &
            'an untested case does not increment executed skips')
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
        call assert_equal_string(json_string_value(field), trim(names(601)), &
            'MCP preserves the full requested CTest ID')
        field = json_member(entry, 'status')
        call assert_equal_string(json_string_value(field), 'pass', &
            'long named CTest result reports PASS')
        result_object = json_member(responses(5), 'result')
        content = json_member(result_object, 'content')
        first = json_element(content, 1)
        field = json_member(first, 'text')
        call json_parse(json_string_value(field), report, valid, error_message)
        call assert_true(valid, 'large explicit selection returns JSON')
        tests = json_member(report, 'tests')
        call assert_equal_integer(json_size(tests), test_count - 1, &
            'selection beyond 512 retains every selected result')
        result_object = json_member(responses(6), 'result')
        content = json_member(result_object, 'content')
        first = json_element(content, 1)
        field = json_member(first, 'text')
        body = json_string_value(field)
        call assert_true(len(body) > 16384, 'human MCP output exceeds former limit')
        call assert_true(index(body, 'Start recovery...  '//new_line('a')) > 0, &
            'human MCP output preserves diagnostic Start recovery and trailing spaces')
        call assert_true(index(body, repeat('z', 24000)//' END-DIAG  ') > 0, &
            'human MCP output preserves the complete long diagnostic')
        call assert_true(index(body, trim(names(601))) > 0, &
            'human MCP output preserves complete long selected name')
        call assert_true(index(body, 'Summary: 601 passed, 1 failed') > 0, &
            'human MCP output retains exact late summary')
        result_object = json_member(responses(7), 'error')
        field = json_member(result_object, 'code')
        call assert_equal_integer(int(json_number_value(field)), -32602, &
            'malformed test selection reports invalid params without partial execution')
        result_object = json_member(responses(8), 'result')
        content = json_member(result_object, 'content')
        first = json_element(content, 1)
        field = json_member(first, 'text')
        call json_parse(json_string_value(field), report, valid, error_message)
        tests = json_member(report, 'tests')
        entry = json_element(tests, 1)
        field = json_member(entry, 'status')
        call assert_equal_string(json_string_value(field), 'untested', &
            'explicit unavailable selection remains untested')
        field = json_member(entry, 'reason')
        call assert_true(index(json_string_value(field), 'missing_executable') > 0, &
            'explicit unavailable selection retains the exact cause')
        result_object = json_member(responses(9), 'error')
        field = json_member(result_object, 'code')
        call assert_equal_integer(int(json_number_value(field)), -32602, &
            'malformed test JSON mode reports invalid params before execution')
    end do

    call check_large_check_consumers()
    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'mcp-test-json: 603 results and long named CTest IDs survive MCP'

contains

    subroutine check_large_check_consumers()
        type(mcp_session_t) :: server
        type(json_value_t) :: response, status, contents, item
        character(len=:), allocatable :: project, source, raw, original_path, poison_dir
        integer :: attempt, exit_code, env_status
        logical :: finished, parsed

        project = join_path(scratch, 'check-project')
        source = 'module oracle'//new_line('a')//'implicit none'//new_line('a')// &
            'integer, parameter :: token = 1'//new_line('a')// &
            'end module'//new_line('a')
        call write_text(project//'/fpm.toml', 'name = "large_check_probe"')
        call write_text(project//'/src/oracle.f90', source)
        call write_text(project//'/test/test_summaries.f90', &
            'program test_summaries'//new_line('a')// &
            'use oracle, only: token'//new_line('a')//'implicit none'//new_line('a')// &
            'integer :: i'//new_line('a')//'if (token < 0) stop 1'//new_line('a')// &
            'do i = 1, 301'//new_line('a')// &
            'if (i == 301) then'//new_line('a')// &
            "write (*, '(a,i0,a,a)') 'suite_', i, '_', &"//new_line('a')// &
            "    repeat('x', 5000)//': 0 pass, 1 fail'"//new_line('a')// &
            'else'//new_line('a')// &
            "write (*, '(a,i0,a,a)') 'suite_', i, '_', &"//new_line('a')// &
            "    repeat('x', 80)//': 0 pass, 1 fail'"//new_line('a')// &
            'end if'//new_line('a')//'end do'//new_line('a')// &
            'end program'//new_line('a'))
        original_path = environment_value('PATH')
        poison_dir = join_path(scratch, 'poison-path')
        call make_directory(poison_dir)
        call write_text(poison_dir//'/fo', '#!/bin/sh'//new_line('a')// &
            'exit 97'//new_line('a'))
        env_status = c_chmod(poison_dir//'/fo'//c_null_char, int(o'700', c_int))
        call assert_equal_integer(env_status, 0, 'creates hostile PATH fo oracle')
        env_status = c_setenv('PATH'//c_null_char, &
            poison_dir//':'//original_path//c_null_char, 1_c_int)
        call assert_equal_integer(env_status, 0, 'installs hostile PATH for MCP child')
        call mcp_session_start(server, driver, project, cache, &
            scratch//'/check-state', scratch//'/check-mcp.stderr')
        env_status = c_setenv('PATH'//c_null_char, original_path//c_null_char, 1_c_int)
        call assert_equal_integer(env_status, 0, 'restores fixture parent PATH')
        call mcp_session_request(server, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}', response)
        call mcp_session_call(server, '{"action":"check","dir":'// &
            mcp_quote(project)//',"json":"full"}', response, 120000)
        call verify_large_check(tool_text(response), 'synchronous check')

        source = 'module oracle'//new_line('a')//'implicit none'//new_line('a')// &
            'integer, parameter :: token = 2'//new_line('a')// &
            'end module'//new_line('a')
        call write_text(project//'/src/oracle.f90', source)
        call mcp_session_call(server, '{"action":"check","mode":"start","root":'// &
            mcp_quote(project)//',"json":"full"}', response)
        finished = .false.
        do attempt = 1, 1200
            call mcp_session_call(server, '{"action":"status"}', response)
            call json_parse(tool_text(response), status, parsed, raw)
            field = json_member(status, 'state')
            if (json_string_value(field) == 'finished') then
                finished = .true.
                exit
            end if
            call fs_sleep_ms(50)
        end do
        call assert_true(finished, &
            'large async check finishes before its safety deadline')
        call mcp_session_call(server, '{"action":"diagnostics"}', response)
        call verify_large_check(tool_text(response), 'async diagnostics')
        call mcp_session_request(server, 'resources/read', &
            '{"uri":"fo://diagnostics"}', response)
        result_object = json_member(response, 'result')
        contents = json_member(result_object, 'contents')
        item = json_element(contents, 1)
        field = json_member(item, 'text')
        call verify_large_check(json_string_value(field), 'diagnostics resource')
        call write_text(project//'/src/oracle.f90', &
            'module oracle'//new_line('a')// &
            'INVALID_COMPILE_DIAGNOSTIC_TOKEN_777'//new_line('a')// &
            'end module'//new_line('a'))
        call mcp_session_call(server, '{"action":"test","dir":'// &
            mcp_quote(project)//',"json":"full"}', response, 120000)
        call json_parse(tool_text(response), status, parsed, raw)
        call assert_true(parsed, 'native failed test compilation produces valid JSON')
        field = json_member(status, 'tests')
        call assert_equal_integer(json_size(field), 0, &
            'failed compilation does not claim executed test results')
        field = json_member(status, 'output')
        call assert_true(index(json_string_value(field), &
            'INVALID_COMPILE_DIAGNOSTIC_TOKEN_777') > 0, &
            'public MCP failed compilation retains its concrete diagnostic')
        call mcp_session_shutdown(server, exit_code)
        call assert_equal_integer(exit_code, 0, 'large check MCP server shuts down')
    end subroutine check_large_check_consumers

    subroutine verify_large_check(raw, label)
        character(len=*), intent(in) :: raw, label
        character(len=:), allocatable :: line, message
        type(json_value_t) :: document, checked_tests
        integer :: first_pos, last_pos, separator, found
        logical :: parsed

        call assert_true(len(raw) > 32768, label//' exceeds old output buffers')
        first_pos = 1
        found = 0
        do while (first_pos <= len(raw))
            separator = index(raw(first_pos:), new_line('a'))
            last_pos = len(raw)
            if (separator > 0) last_pos = first_pos + separator - 2
            line = ''
            if (last_pos >= first_pos) line = raw(first_pos:last_pos)
            call json_parse(line, document, parsed, message)
            if (parsed) then
                checked_tests = json_member(document, 'tests')
                if (json_size(checked_tests) > 0) then
                    found = found + 1
                    call assert_equal_integer(json_size(checked_tests), 301, &
                        label//' retains every failing suite')
                    field = json_member(document, 'failed_tests')
                    call assert_equal_integer(json_size(field), 301, &
                        label//' retains every failing identity')
                    call assert_true(index(line, 'suite_301_'//repeat('x', 5000)) > 0, &
                        label//' retains the complete late failing name')
                end if
            end if
            if (separator == 0) exit
            first_pos = last_pos + 2
        end do
        call assert_equal_integer(found, 1, label//' retains one complete JSON receipt')
    end subroutine verify_large_check

    function tool_text(response) result(text)
        type(json_value_t), intent(in) :: response
        character(len=:), allocatable :: text
        type(json_value_t) :: object, content, first, member

        object = json_member(response, 'result')
        content = json_member(object, 'content')
        first = json_element(content, 1)
        member = json_member(first, 'text')
        text = json_string_value(member)
    end function tool_text

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
