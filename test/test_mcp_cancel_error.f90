program test_mcp_cancel_error
    use, intrinsic :: iso_c_binding, only: c_int64_t
    use fo_test_harness, only: make_directory, write_text, read_text, start_sentinel
    use fo_test_harness, only: process_alive, stop_sentinel
    use fo_test_harness, only: remove_path
    use fo_test_harness, only: assert_true, assert_equal_integer, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_fifo
    use fo_test_gremlin_oracle, only: gremlin_wait_file, gremlin_release_fifo
    use fo_test_gremlin_oracle, only: gremlin_wait_ms
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start
    use fo_test_mcp_session, only: mcp_session_request, mcp_session_notify
    use fo_test_mcp_session, only: mcp_session_call, mcp_session_shutdown
    use fo_test_mcp, only: mcp_quote
    use fo_test_process_identity, only: mcp_process_start_time
    use fo_test_process_identity, only: mcp_process_identity_running
    use fo_test_process_identity, only: mcp_kill_owned_tree
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_string_value, json_number_value, json_boolean_value
    use fo_test_json, only: json_boolean
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: project_b, project_bad, ready_a, ready_b
    character(:), allocatable :: gate_a, gate_b, done_b, start_arguments, arguments
    character(:), allocatable :: bad_arguments, diagnostics, body, compiler_log
    character(len=32) :: number
    type(mcp_session_t) :: server, server_b
    type(json_value_t) :: response, payload, status_payload, field, full_check
    integer :: exit_code, sentinel = 0, run_id, run_id_b, bad_run_id, error_code
    integer :: child_a = 0, child_b = 0
    integer(c_int64_t) :: child_a_start = 0_c_int64_t
    integer(c_int64_t) :: child_b_start = 0_c_int64_t
    integer :: started_at, finished_at, clock_rate
    logical :: found, valid

    call gremlin_setup(driver, scratch, project, cache, state)
    project_b = scratch//'/mcp-independent-b'
    project_bad = scratch//'/mcp-early-output'
    ready_a = scratch//'/cancel-a-ready'
    ready_b = scratch//'/cancel-b-ready'
    gate_a = scratch//'/cancel-a-release.fifo'
    gate_b = scratch//'/cancel-b-release.fifo'
    done_b = scratch//'/cancel-b-complete'
    call prepare_check_fixture(project, ready_a, gate_a, 'mcp_cancel_probe')
    call prepare_check_fixture(project_b, ready_b, gate_b, &
        'mcp_independent_probe', done_b)
    call mcp_session_start(server, driver, project, cache, state, &
        scratch//'/mcp-cancel.stderr')
    call mcp_session_request(server, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{},'// &
        '"clientInfo":{"name":"fo-cancel-native-test","version":"1"}}', response)
    call mcp_session_notify(server, 'notifications/initialized')
    call start_sentinel(sentinel)

    call mcp_session_call(server, '{"action":"cancel","run_id":999999}', response)
    error_code = rpc_error_code(response)
    call assert_equal_integer(error_code, -32602, &
        'idle cancellation rejects a stale public run ID')

    start_arguments = '{"action":"check","mode":"start","root":'// &
        mcp_quote(project)//'}'
    call mcp_session_call(server, start_arguments, response)
    payload = json_member(response, 'result')
    run_id = int(json_number_value(json_member(payload, 'run_id')))
    call assert_true(run_id > 0, 'asynchronous public check returns its owned run ID')
    if (run_id <= 0) goto 900

    call mcp_session_call(server, '{"action":"status"}', response)
    call extract_payload(response, status_payload)
    call assert_true(json_string_value(json_member(status_payload, 'state')) == &
        'running', 'owned asynchronous check is live before cancellation')
    field = json_member(status_payload, 'run_id')
    call assert_equal_integer(int(json_number_value(field)), run_id, &
        'status associates the active process with its returned owner ID')
    call gremlin_wait_file(ready_a, 30000, found)
    call assert_true(found, 'first check reaches its native release barrier')
    if (.not. found) goto 900
    call read_child_identity(ready_a, child_a, child_a_start, found)
    if (.not. found) goto 900

    call mcp_session_start(server_b, driver, project_b, cache, state, &
        scratch//'/mcp-independent.stderr')
    call mcp_session_request(server_b, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{},'// &
        '"clientInfo":{"name":"fo-independent-native-test","version":"1"}}', &
        response)
    call mcp_session_notify(server_b, 'notifications/initialized')
    arguments = '{"action":"check","mode":"start","root":'// &
        mcp_quote(project_b)//'}'
    call mcp_session_call(server_b, arguments, response)
    payload = json_member(response, 'result')
    run_id_b = int(json_number_value(json_member(payload, 'run_id')))
    call assert_true(run_id_b > 0, 'independent MCP server starts its own check')
    if (run_id_b <= 0) goto 900
    call gremlin_wait_file(ready_b, 30000, found)
    call assert_true(found, 'second check reaches its native release barrier')
    if (.not. found) goto 900
    call read_child_identity(ready_b, child_b, child_b_start, found)
    if (.not. found) goto 900

    write(number, '(i0)') run_id + 1
    arguments = '{"action":"cancel","run_id":'//trim(number)//'}'
    call mcp_session_call(server, arguments, response)
    call assert_equal_integer(rpc_error_code(response), -32602, &
        'stale run ID cannot cancel the live process')
    call mcp_session_call(server, '{"action":"status"}', response)
    call extract_payload(response, status_payload)
    call assert_true(json_string_value(json_member(status_payload, 'state')) == &
        'running', 'rejected stale cancellation preserves live ownership')
    field = json_member(status_payload, 'run_id')
    call assert_equal_integer(int(json_number_value(field)), run_id, &
        'rejected stale ID leaves the active run identity unchanged')

    call system_clock(started_at, clock_rate)
    write(number, '(i0)') run_id
    arguments = '{"action":"cancel","run_id":'//trim(number)//'}'
    call mcp_session_call(server, arguments, response)
    call extract_payload(response, payload)
    call system_clock(finished_at)
    call assert_true(json_boolean_value(json_member(payload, 'cancelled')) .and. &
        int(json_number_value(json_member(payload, 'run_id'))) == run_id, &
        'valid cancellation reports completion for the exact owned process')
    if (clock_rate > 0) then
        call assert_true(real(finished_at - started_at) / real(clock_rate) < 5.0, &
            'owned process cancellation completes within five seconds')
    end if
    call mcp_session_call(server, '{"action":"status"}', response)
    call extract_payload(response, status_payload)
    call assert_true(json_string_value(json_member(status_payload, 'state')) == &
        'finished', 'cancelled asynchronous process reaches terminal status')
    field = json_member(status_payload, 'exitcode')
    call assert_equal_integer(int(json_number_value(field)), 130, &
        'terminal status records the public cancellation exit code')
    call wait_child_gone(child_a, child_a_start, &
        'public cancellation stops the exact blocked child before cleanup')

    call mcp_session_call(server_b, '{"action":"status"}', response)
    call extract_payload(response, status_payload)
    call assert_true(json_string_value(json_member(status_payload, 'state')) == &
        'running', 'cancelling server A leaves independent server B active')
    field = json_member(status_payload, 'run_id')
    call assert_equal_integer(int(json_number_value(field)), run_id_b, &
        'server B retains its distinct active run identity')
    call assert_true(mcp_process_identity_running(child_b, child_b_start), &
        'server B blocked child remains alive after server A cancellation')
    call gremlin_release_fifo(gate_b)
    call gremlin_wait_file(done_b, 10000, found)
    call assert_true(found, 'server B test progresses after server A cancellation')
    call wait_finished(server_b, run_id_b, status_payload)
    call assert_equal_integer(int(json_number_value( &
        json_member(status_payload, 'exitcode'))), 0, &
        'independent server B check completes successfully')
    call wait_child_gone(child_b, child_b_start, &
        'server B child exits after its own completion token')

    call prepare_failure_fixture(project_bad)
    bad_arguments = '{"action":"check","mode":"start","json":"full","root":'// &
        mcp_quote(project_bad)//'}'
    call mcp_session_call(server_b, bad_arguments, response)
    payload = json_member(response, 'result')
    bad_run_id = int(json_number_value(json_member(payload, 'run_id')))
    call assert_true(bad_run_id > 0, 'starts a naturally failing early-exit check')
    if (bad_run_id <= 0) goto 900
    call wait_finished(server_b, bad_run_id, status_payload)
    field = json_member(status_payload, 'exitcode')
    call assert_true(int(json_number_value(field)) /= 0, &
        'invalid source check exits before diagnostics are requested')
    write(number, '(i0)') bad_run_id
    arguments = '{"action":"diagnostics","run_id":'//trim(number)//'}'
    call mcp_session_call(server_b, arguments, response)
    diagnostics = response_text(response)
    call write_text(scratch//'/early-diagnostics.txt', diagnostics)
    call read_full_check(diagnostics, full_check)
    call assert_true(.not. json_boolean_value(json_member(full_check, 'tests_ok')), &
        'full completed receipt retains the failing test outcome')
    compiler_log = json_string_value(json_member(full_check, 'log_path'))
    call assert_true(len(compiler_log) > 0, &
        'full completed receipt references its retained compiler output artifact')
    diagnostics = ''
    if (len(compiler_log) > 0) diagnostics = read_text(compiler_log)
    call assert_true(index(diagnostics, 'test_mcp_failure.f90') > 0, &
        'finished child diagnostics retain the concrete failing source path')
    call assert_true(index(diagnostics, 'mcp_failure_probe') > 0 .or. &
        index(diagnostics, 'syntax') > 0 .or. index(diagnostics, 'Error') > 0, &
        'finished child diagnostics retain compiler failure output')

    write(number, '(i0)') run_id
    arguments = '{"action":"cancel","run_id":'//trim(number)//'}'
    call mcp_session_call(server, arguments, response)
    call assert_equal_integer(rpc_error_code(response), -32602, &
        'duplicate cancellation rejects the now-stale run ID')
    call assert_true(process_alive(sentinel), &
        'cancelling owned work preserves an unrelated sentinel process')
900 call cleanup_fixture()
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine read_full_check(text, document)
        character(len=*), intent(in) :: text
        type(json_value_t), intent(out) :: document
        type(json_value_t) :: candidate, build_field, tests_field
        character(:), allocatable :: line, message
        integer :: first, last, separator
        logical :: valid, found

        first = 1
        found = .false.
        do while (first <= len(text))
            separator = index(text(first:), new_line('a'))
            last = len(text)
            if (separator > 0) last = first + separator - 2
            line = ''
            if (last >= first) line = text(first:last)
            call json_parse(line, candidate, valid, message)
            if (valid) then
                build_field = json_member(candidate, 'build_ok')
                tests_field = json_member(candidate, 'tests_ok')
                if (build_field%kind == json_boolean .and. &
                        tests_field%kind == json_boolean) then
                    call assert_true(.not. found, &
                        'completed diagnostics contain exactly one full check receipt')
                    document = candidate
                    found = .true.
                end if
            end if
            if (separator == 0) exit
            first = last + 2
        end do
        call assert_true(found, &
            'public completed diagnostics retain valid full check JSON after progress')
    end subroutine read_full_check

    subroutine read_child_identity(path, pid, start_time, found)
        character(len=*), intent(in) :: path
        integer, intent(out) :: pid
        integer(c_int64_t), intent(out) :: start_time
        logical, intent(out) :: found
        integer :: unit, status

        pid = 0
        start_time = 0_c_int64_t
        found = .false.
        open(newunit=unit, file=path, status='old', action='read', iostat=status)
        if (status == 0) then
            read(unit, *, iostat=status) pid
            close(unit)
        end if
        call assert_true(status == 0 .and. pid > 0, &
            'native barrier publishes its actual blocked child PID')
        if (status /= 0 .or. pid <= 0) return
        start_time = mcp_process_start_time(pid)
        found = start_time > 0_c_int64_t
        call assert_true(found, 'captures the blocked child start identity')
    end subroutine read_child_identity

    subroutine wait_child_gone(pid, start_time, assertion)
        integer, intent(in) :: pid
        integer(c_int64_t), intent(in) :: start_time
        character(len=*), intent(in) :: assertion
        integer :: attempt

        do attempt = 1, 200
            if (.not. mcp_process_identity_running(pid, start_time)) exit
            call gremlin_wait_ms(25)
        end do
        call assert_true(.not. mcp_process_identity_running(pid, start_time), &
            assertion)
    end subroutine wait_child_gone

    subroutine cleanup_fixture()
        integer :: status

        if (server%handle > 0) then
            call mcp_session_shutdown(server, exit_code)
            call assert_equal_integer(exit_code, 0, &
                'MCP server exits cleanly during owned cleanup')
        end if
        if (server_b%handle > 0) then
            call mcp_session_shutdown(server_b, exit_code)
            call assert_equal_integer(exit_code, 0, &
                'independent MCP server exits cleanly during owned cleanup')
        end if
        if (child_a > 0 .and. child_a_start > 0_c_int64_t) then
            if (mcp_process_identity_running(child_a, child_a_start)) then
                call mcp_kill_owned_tree(child_a, child_a_start, status)
                call assert_equal_integer(status, 0, &
                    'cleanup stops only the exact first child tree')
            end if
            call wait_child_gone(child_a, child_a_start, &
                'owned first child is absent before scratch retention')
        end if
        if (child_b > 0 .and. child_b_start > 0_c_int64_t) then
            if (mcp_process_identity_running(child_b, child_b_start)) then
                call mcp_kill_owned_tree(child_b, child_b_start, status)
                call assert_equal_integer(status, 0, &
                    'cleanup stops only the exact second child tree')
            end if
            call wait_child_gone(child_b, child_b_start, &
                'owned second child is absent before scratch retention')
        end if
        if (sentinel > 0) then
            call assert_true(process_alive(sentinel), &
                'unrelated sentinel survives both server cleanup paths')
            call stop_sentinel(sentinel)
        end if
        call remove_path(gate_a)
        call remove_path(gate_b)
    end subroutine cleanup_fixture

    subroutine prepare_check_fixture(project_dir, ready_path, gate_path, package, &
            completed_path)
        character(len=*), intent(in) :: project_dir, ready_path, gate_path, package
        character(len=*), intent(in), optional :: completed_path

        call make_directory(project_dir)
        call write_text(project_dir//'/fpm.toml', &
            'name = "'//trim(package)//'"'//new_line('a'))
        call gremlin_fifo(gate_path)
        call make_directory(project_dir//'/test')
        body = 'program test_mcp_barrier'//new_line('a')// &
            'use, intrinsic :: iso_c_binding, only: c_int'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'interface'//new_line('a')// &
            'integer(c_int) function getpid() bind(C, name="getpid")'// &
            new_line('a')//'import :: c_int'//new_line('a')// &
            'end function getpid'//new_line('a')//'end interface'//new_line('a')// &
            'integer :: unit, gate_unit'//new_line('a')// &
            'character :: token'//new_line('a')// &
            'open(newunit=unit, file="'//ready_path// &
                '", status="replace", action="write")'//new_line('a')// &
            'write(unit, "(i0)") getpid()'//new_line('a')//'close(unit)'// &
            new_line('a')//'open(newunit=gate_unit, file="'//gate_path// &
                '", status="old", access="stream", form="unformatted", '// &
                'action="read")'//new_line('a')//'read(gate_unit) token'// &
            new_line('a')//'close(gate_unit)'
        if (present(completed_path)) body = body//new_line('a')// &
            'open(newunit=unit, file="'//completed_path// &
                '", status="replace", action="write")'//new_line('a')// &
            'write(unit, "(a)") "completed"'//new_line('a')//'close(unit)'
        body = body//new_line('a')//'end program test_mcp_barrier'//new_line('a')
        call write_text(project_dir//'/test/test_mcp_barrier.f90', body)
    end subroutine prepare_check_fixture

    subroutine prepare_failure_fixture(project_dir)
        character(len=*), intent(in) :: project_dir

        call make_directory(project_dir)
        call make_directory(project_dir//'/test')
        call write_text(project_dir//'/fpm.toml', &
            'name = "mcp_failure_probe"'//new_line('a'))
        call write_text(project_dir//'/test/test_mcp_failure.f90', &
            'program test_mcp_failure'//new_line('a')// &
            'this source is intentionally invalid'//new_line('a')// &
            'end program test_mcp_failure'//new_line('a'))
    end subroutine prepare_failure_fixture

    subroutine wait_finished(session, expected_run, document)
        type(mcp_session_t), intent(inout) :: session
        integer, intent(in) :: expected_run
        type(json_value_t), intent(out) :: document
        integer :: attempt
        logical :: finished

        finished = .false.
        do attempt = 1, 400
            call mcp_session_call(session, '{"action":"status"}', response)
            call extract_payload(response, document)
            field = json_member(document, 'state')
            finished = json_string_value(field) == 'finished'
            if (finished) exit
            call gremlin_wait_ms(25)
        end do
        call assert_true(finished, &
            'asynchronous check reaches terminal status in bound')
        field = json_member(document, 'run_id')
        call assert_equal_integer(int(json_number_value(field)), expected_run, &
            'terminal check status names its exact run')
    end subroutine wait_finished

    subroutine extract_payload(envelope, document)
        type(json_value_t), intent(in) :: envelope
        type(json_value_t), intent(out) :: document
        type(json_value_t) :: result, content, first
        character(:), allocatable :: raw, parse_message

        result = json_member(envelope, 'result')
        content = json_member(result, 'content')
        first = json_element(content, 1)
        raw = json_string_value(json_member(first, 'text'))
        call json_parse(raw, document, valid, parse_message)
        call assert_true(valid, 'independent parser reads MCP async response')
    end subroutine extract_payload

    integer function rpc_error_code(envelope) result(code)
        type(json_value_t), intent(in) :: envelope
        type(json_value_t) :: error, field

        error = json_member(envelope, 'error')
        field = json_member(error, 'code')
        code = int(json_number_value(field))
    end function rpc_error_code

    function response_text(envelope) result(text)
        type(json_value_t), intent(in) :: envelope
        type(json_value_t) :: result, content, first
        character(:), allocatable :: text

        result = json_member(envelope, 'result')
        content = json_member(result, 'content')
        first = json_element(content, 1)
        text = json_string_value(json_member(first, 'text'))
    end function response_text

end program test_mcp_cancel_error
