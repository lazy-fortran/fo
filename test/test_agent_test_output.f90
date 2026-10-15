program test_agent_test_output
    !! Agent-facing `fo test` output (#218) and live progress (#130): a suite of
    !! 200 passing cases and one failure reports in a bounded compact JSON with
    !! the failure's file:line, an excerpt and a working log handle, the same
    !! through the CLI and the MCP tool; and the peers of a slow case are
    !! reported while that case still runs.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_null_ptr, &
        c_ptr, c_loc
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_process_ok
    use fo_test_harness, only: finish_assertions, read_text, file_exists
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_number_value
    use fo_test_mcp, only: mcp_request, mcp_call, mcp_quote, mcp_exchange
    use fo_fs, only: fs_sleep_ms
    implicit none

    !! Documented bound of the compact report for one failing case: the verdict,
    !! counts and one excerpt (at most 2 KiB before JSON escaping).
    integer, parameter :: REPORT_BOUND = 4096
    integer, parameter :: PASSING = 200

    interface
        function c_setenv(key, value, overwrite) bind(C, name='setenv') result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: key(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: rc
        end function c_setenv
        function c_spawn_capture(arguments, cwd, stdout_path, stderr_path) &
                bind(C, name='fo_test_spawn_capture') result(pid)
            import :: c_char, c_int, c_ptr
            type(c_ptr), intent(in) :: arguments(*)
            character(kind=c_char), intent(in) :: cwd(*), stdout_path(*), stderr_path(*)
            integer(c_int) :: pid
        end function c_spawn_capture
        function c_wait_child(process, code, signal_number, blocking) &
                bind(C, name='fo_test_wait_child') result(waited)
            import :: c_int
            integer(c_int), value :: process, blocking
            integer(c_int), intent(out) :: code, signal_number
            integer(c_int) :: waited
        end function c_wait_child
    end interface

    character(:), allocatable :: driver, scratch, cache
    integer :: rc

    call resolve_driver(driver)
    call make_scratch('fo-agent-test-output', scratch)
    cache = join_path(scratch, 'cache')
    rc = c_setenv('TMPDIR'//c_null_char, trim(scratch)//c_null_char, 1_c_int)
    rc = c_setenv('FO_CACHE_DIR'//c_null_char, trim(cache)//c_null_char, 1_c_int)
    rc = c_setenv('FO_DISABLE_SELF_REFRESH'//c_null_char, '1'//c_null_char, 1_c_int)
    rc = c_setenv('FO_SELF_REFRESH'//c_null_char, '0'//c_null_char, 1_c_int)
    rc = c_setenv('FO_JOBS'//c_null_char, '2'//c_null_char, 1_c_int)

    call test_compact_report()
    call test_live_peer_lines()

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine test_compact_report()
        character(:), allocatable :: project, diagnostic, cmake, log_text
        type(process_result_t) :: cli, human
        type(string_list_t) :: arguments
        type(json_value_t) :: report, cases, failing, field, mcp_report
        type(json_value_t), allocatable :: responses(:)
        character(:), allocatable :: body, input, verdict_cli, last_line
        integer :: i, last_break
        logical :: valid
        character(:), allocatable :: error_message

        project = join_path(scratch, 'compact')
        diagnostic = ''
        do i = 1, 60
            diagnostic = diagnostic//'  trace line '//integer_text(i)//new_line('a')
        end do
        call write_text(join_path(project, 'fail.cmake'), &
            'message([=['//new_line('a')//diagnostic//repeat('z', 5000)// &
            new_line('a')//'  TAIL_MARKER'//new_line('a')//']=])'//new_line('a')// &
            'message(FATAL_ERROR "fixture fails")'//new_line('a'))
        cmake = 'cmake_minimum_required(VERSION 3.20)'//new_line('a')// &
            'project(agent_output NONE)'//new_line('a')//'enable_testing()'//new_line('a')
        do i = 1, PASSING
            cmake = cmake//'add_test(NAME case_'//integer_text(i)// &
                ' COMMAND "${CMAKE_COMMAND}" -E true)'//new_line('a')
        end do
        cmake = cmake//'add_test(NAME case_failing COMMAND "${CMAKE_COMMAND}" -P '// &
            '"${CMAKE_CURRENT_SOURCE_DIR}/fail.cmake")'//new_line('a')
        call write_text(join_path(project, 'CMakeLists.txt'), cmake)
        arguments = string_list_t()
        call list_add(arguments, 'build')
        call run_fo(driver, arguments, project, cache, cli)
        call assert_process_ok(cli, 'configure the 201-case fixture')

        ! CLI --json
        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, '--json')
        call run_fo(driver, arguments, project, cache, cli)
        call assert_equal_integer(cli%exit_code, 1, 'the failing case fails the run')
        call assert_true(len(cli%stdout) < REPORT_BOUND, &
            'CLI --json report stays under the documented bound')
        call parse_json_report(cli, report, 'CLI --json report parses')
        field = json_member(report, 'v')
        verdict_cli = json_string_value(field)
        call assert_true(index(verdict_cli, '200 pass, 1 FAIL (') == 1, &
            'the verdict counts the passing cases instead of listing them')
        call assert_true(index(cli%stdout, '{"v":') == 1, 'the verdict is the first key')
        call assert_true(index(cli%stdout, 'case_17') == 0, &
            'a passing case is not named in the default report')
        cases = json_member(report, 'cases')
        call assert_equal_integer(json_size(cases), 1, 'only the failing case is listed')
        failing = json_element(cases, 1)
        field = json_member(failing, 'at')
        call assert_true(index(json_string_value(field), 'fail.cmake:') > 0, &
            'the failure names file:line')
        field = json_member(failing, 'cut')
        call assert_true(field%boolean, 'a long failure is marked as an excerpt')
        field = json_member(failing, 'out')
        call assert_true(len(json_string_value(field)) <= 2100, &
            'the excerpt is bounded')
        field = json_member(report, 'log')
        call assert_true(file_exists(json_string_value(field)), &
            'the log handle names an existing file')
        log_text = read_text(json_string_value(field))
        call assert_true(index(log_text, 'TAIL_MARKER') > 0 .and. &
            index(log_text, repeat('z', 5000)) > 0, &
            'the log handle retains the complete failure output')

        ! MCP default: the same routine, so the same report.
        input = ''
        input = input//mcp_request(1, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}')//new_line('a')
        input = input//mcp_call(2, '{"action":"test","dir":'//mcp_quote(project)//'}')// &
            new_line('a')
        input = input//mcp_request(3, 'shutdown')//new_line('a')
        call mcp_exchange(driver, project, cache, input, .false., responses, human)
        field = json_element(json_member(json_member(responses(2), 'result'), &
            'content'), 1)
        body = json_string_value(json_member(field, 'text'))
        call assert_true(len(body) < REPORT_BOUND, &
            'default MCP test report stays under the documented bound')
        call json_parse(body, mcp_report, valid, error_message)
        call assert_true(valid, 'the default MCP test report is JSON')
        call assert_true(index(body, verdict_cli(1:index(verdict_cli, ' (') - 1)) == 7, &
            'MCP and CLI report the same verdict')
        field = json_member(json_element(json_member(mcp_report, 'cases'), 1), 'at')
        call assert_true(index(json_string_value(field), 'fail.cmake:') > 0, &
            'the MCP failure names file:line')
        field = json_member(mcp_report, 'log')
        call assert_true(file_exists(json_string_value(field)), &
            'the MCP log handle names an existing file')

        ! Human text ends with the verdict line.
        arguments = string_list_t()
        call list_add(arguments, 'test')
        call run_fo(driver, arguments, project, cache, human)
        last_break = index(human%stdout(:len(human%stdout) - 1), new_line('a'), back=.true.)
        last_line = human%stdout(last_break + 1:len(human%stdout) - 1)
        call assert_true(index(last_line, '200 pass, 1 FAIL (') == 1, &
            'human output ends with the verdict line')
        call assert_true(index(human%stdout, 'log: ') > 0, &
            'human output names the retained log')
    end subroutine test_compact_report

    subroutine test_live_peer_lines()
        !! The gate case waits for a file this test creates only after it has
        !! read the completion lines of both fast peers from stderr. If the
        !! lines only appeared at the end of the run, the gate would time out.
        character(:), allocatable :: project, gate, out_path, err_path, err_text
        type(c_ptr), target :: argv(3)
        character(kind=c_char), allocatable, target :: driver_c(:), test_c(:)
        character(kind=c_char), allocatable :: cwd_c(:), out_c(:), err_c(:)
        integer(c_int) :: pid, waited, code, signal_number
        integer(8) :: started, now, rate
        logical :: peers_seen, gate_line_seen, child_alive_when_seen

        project = join_path(scratch, 'live')
        gate = join_path(scratch, 'gate.flag')
        out_path = join_path(scratch, 'live.out')
        err_path = join_path(scratch, 'live.err')
        call write_text(join_path(project, 'fpm.toml'), 'name = "live_probe"')
        call write_text(join_path(project, 'test/test_a_gate.f90'), &
            'program test_a_gate'//new_line('a')//'implicit none'//new_line('a')// &
            'logical :: ready'//new_line('a')//'integer :: i'//new_line('a')// &
            'do i = 1, 600'//new_line('a')// &
            '  inquire (file="'//gate//'", exist=ready)'//new_line('a')// &
            '  if (ready) stop 0'//new_line('a')// &
            '  call sleep(1)'//new_line('a')//'end do'//new_line('a')// &
            'error stop "gate was never opened"'//new_line('a')// &
            'end program'//new_line('a'))
        call write_text(join_path(project, 'test/test_b_fast.f90'), &
            'program test_b_fast'//new_line('a')//'end program'//new_line('a'))
        call write_text(join_path(project, 'test/test_c_fast.f90'), &
            'program test_c_fast'//new_line('a')//'end program'//new_line('a'))

        call c_string(driver, driver_c)
        call c_string('test', test_c)
        argv(1) = c_loc(driver_c)
        argv(2) = c_loc(test_c)
        argv(3) = c_null_ptr
        call c_string(project, cwd_c)
        call c_string(out_path, out_c)
        call c_string(err_path, err_c)
        pid = c_spawn_capture(argv, cwd_c, out_c, err_c)
        call assert_true(pid > 0, 'start fo test on the gate fixture')
        if (pid <= 0) return

        call system_clock(started, rate)
        peers_seen = .false.
        child_alive_when_seen = .false.
        gate_line_seen = .false.
        do
            waited = c_wait_child(pid, code, signal_number, 0_c_int)
            if (waited /= 0) exit
            if (file_exists(err_path)) then
                err_text = read_text(err_path)
                if (index(err_text, 'PASS test_b_fast') > 0 .and. &
                    index(err_text, 'PASS test_c_fast') > 0) then
                    gate_line_seen = index(err_text, 'test_a_gate') > 0
                    child_alive_when_seen = c_wait_child(pid, code, signal_number, &
                        0_c_int) == 0
                    peers_seen = child_alive_when_seen
                    exit
                end if
            end if
            call system_clock(now)
            if (real(now - started) / real(rate) > 240.0) exit
            call fs_sleep_ms(100)
        end do
        call assert_true(peers_seen, &
            'the fast peers are reported while the slow case is still running')
        call assert_true(child_alive_when_seen .and. .not. gate_line_seen, &
            'the slow case has not finished when its peers are reported')
        call write_text(gate, 'open')
        do
            waited = c_wait_child(pid, code, signal_number, 0_c_int)
            if (waited /= 0) exit
            call system_clock(now)
            if (real(now - started) / real(rate) > 300.0) exit
            call fs_sleep_ms(100)
        end do
        call assert_true(waited == 1, 'fo test finishes once the gate opens')
        call assert_equal_integer(int(code), 0, 'the gated suite passes')
        err_text = read_text(err_path)
        call assert_true(index(err_text, '3/3') > 0, &
            'the last completion line counts every case')
        call assert_true(index(read_text(out_path), '3 pass (') > 0, &
            'the verdict names wall time and ends the report')
        call assert_true(index(read_text(out_path), 's wall)') > 0, &
            'the run time is labelled as wall clock')
    end subroutine test_live_peer_lines

    subroutine c_string(text, bytes)
        character(len=*), intent(in) :: text
        character(kind=c_char), allocatable, intent(out) :: bytes(:)
        integer :: i

        allocate (bytes(len(text) + 1))
        do i = 1, len(text)
            bytes(i) = text(i:i)
        end do
        bytes(len(text) + 1) = c_null_char
    end subroutine c_string

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

end program test_agent_test_output
