program test_mcp_gremlin_stale
    use, intrinsic :: iso_c_binding, only: c_int64_t
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, assert_true
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_wait_ms, gremlin_wait_file, gremlin_stop_lane
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start
    use fo_test_mcp_session, only: mcp_session_call, mcp_session_request
    use fo_test_mcp_session, only: mcp_session_notify
    use fo_test_mcp_session, only: mcp_session_shutdown
    use fo_test_process_identity, only: mcp_process_start_time
    use fo_test_process_identity, only: mcp_process_identity_running, mcp_kill_owned_tree
    use fo_test_mcp, only: mcp_quote
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size
    use fo_test_json, only: json_string_value
    use fx_hash, only: sha256_file
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: session_id, arguments, status_arguments
    character(:), allocatable :: blocked_body, case_id, case_status, state_name
    character(:), allocatable :: blocked_pid_path, blocked_pid_text
    character(:), allocatable :: stale_driver, pass_marker
    character(len=4096) :: stale_image
    character(len=64) :: stale_digest, driver_digest
    type(mcp_session_t) :: server
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: response, payload, events, event, result
    integer :: exit_code, attempt, blocked_pid, status, cleanup_status, i
    character(len=16) :: exit_text
    integer(c_int64_t) :: blocked_start
    logical :: found, pass_seen, fail_seen, blocked_seen

    call gremlin_setup(driver, scratch, project, cache, state)
    stale_driver = driver
    call get_environment_variable('FO_MCP_STALE_DRIVER', stale_image, status=status)
    if (status == 0 .and. len_trim(stale_image) > 0) then
        stale_driver = trim(stale_image)
        call sha256_file(stale_driver, stale_digest, status)
        call assert_true(status == 0, 'hashes the supplied immutable older MCP image')
        call sha256_file(driver, driver_digest, status)
        call assert_true(status == 0, 'hashes the current CLI image')
        call assert_true(stale_digest /= driver_digest, &
            'supplied older MCP and current CLI use distinct immutable images')
    end if
    call write_text(project//'/fpm.toml', &
        'name = "mcp_stale_owner_probe"'//new_line('a'))
    pass_marker = scratch//'/pass.done'
    call gremlin_write_case(project, 'test_stale_pass', &
        'integer :: unit'//new_line('a')// &
        'open(newunit=unit,file="'//pass_marker//'",status="replace")'// &
        new_line('a')//'write(unit,"(a)") "stale-owner-pass-token-9711"'// &
        new_line('a')//'close(unit)')
    call gremlin_write_case(project, 'test_stale_fail', &
        'print *, ''{"tests":[{"name":"test_stale_fail","status":"pass"}]}'''// &
        new_line('a')//'error stop 19')
    blocked_pid_path = scratch//'/blocked-child.pid'
    blocked_body = 'integer :: status'//new_line('a')// &
        'call execute_command_line("sh -c ''sleep 60 & echo $! > '// &
        blocked_pid_path//'; wait''", exitstat=status)'
    call gremlin_write_case(project, 'test_stale_blocked', blocked_body)

    call mcp_session_start(server, stale_driver, project, cache, state, &
        scratch//'/mcp-stale.stderr')
    call mcp_session_request(server, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{},'// &
        '"clientInfo":{"name":"fo-stale-native-test","version":"1"}}', response)
    call assert_true(len(json_string_value(json_member(&
        json_member(response, 'result'), 'protocolVersion'))) > 0, &
        'old interactive MCP process initializes before CLI owner starts')
    call mcp_session_notify(server, 'notifications/initialized')
    call mcp_session_request(server, 'tools/list', '', response)
    result = json_member(response, 'result')
    call assert_true(result%kind /= 0, &
        'persistent MCP retains a valid tool catalog')

    args = string_list_t()
    call list_add(args, 'gremlin')
    call list_add(args, 'start')
    call list_add(args, '--dir')
    call list_add(args, project)
    call list_add(args, '--lane')
    call list_add(args, 'new-cli-lane')
    call list_add(args, '--target')
    call list_add(args, 'test_stale_pass')
    call list_add(args, '--target')
    call list_add(args, 'test_stale_fail')
    call list_add(args, '--target')
    call list_add(args, 'test_stale_blocked')
    call list_add(args, '--json')
    call gremlin_json(driver, project, cache, state, args, payload, process, 30000)
    if (process%exit_code /= 0) then
        write (exit_text, '(i0)') process%exit_code
        call assert_true(.false., 'new CLI failed to start lane (exit='//trim(exit_text)// &
            '); stderr='//trim(process%stderr)//'; stdout='//trim(process%stdout))
        call stop_lane_without_owner()
        call mcp_session_shutdown(server, exit_code)
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    session_id = json_string_value(json_member(payload, 'session_id'))
    if (len(session_id) == 0) then
        call assert_true(.false., 'CLI response omitted its owner ID; stderr='// &
            trim(process%stderr)//'; stdout='//trim(process%stdout))
        call stop_lane_without_owner()
        call mcp_session_shutdown(server, exit_code)
        call finish_assertions(retain_failed_scratch=.true.)
    end if

    call gremlin_wait_file(blocked_pid_path, 30000, found)
    call assert_true(found, 'blocked test publishes its sleeper PID before stop')
    call assert_true(index(read_text(pass_marker), 'stale-owner-pass-token-9711') > 0, &
        'passing fixture independently publishes its completion token')
    blocked_pid = 0
    blocked_start = 0_c_int64_t
    if (found) then
        blocked_pid_text = read_text(blocked_pid_path)
        read (blocked_pid_text, *, iostat=status) blocked_pid
        call assert_true(status == 0 .and. blocked_pid > 0, &
            'reads the blocked test sleeper PID')
        if (status == 0 .and. blocked_pid > 0) then
            blocked_start = mcp_process_start_time(blocked_pid)
        end if
    end if
    call assert_true(blocked_start > 0_c_int64_t .and. &
        mcp_process_identity_running(blocked_pid, blocked_start), &
        'blocked test owns a live sleeper with its recorded start identity')

    do attempt = 1, 600
        status_arguments = '{"action":"gremlin_status","dir":'//mcp_quote(project)// &
            ',"lane_id":"new-cli-lane","session_id":'//mcp_quote(session_id)//'}'
        call mcp_session_call(server, status_arguments, response)
        call extract_payload(response, payload)
        if (json_string_value(json_member(payload, 'session_id')) == session_id) exit
        state_name = json_string_value(json_member(payload, 'state'))
        if (state_name == 'stopped' .or. state_name == 'error') then
            call assert_true(.false., 'persistent MCP saw terminal owner state before its ID: '// &
                state_name)
            call stop_lane_without_owner()
            call mcp_session_shutdown(server, exit_code)
            call finish_assertions(retain_failed_scratch=.true.)
        end if
        call gremlin_wait_ms(50)
    end do
    call assert_true(attempt <= 600, &
        'already-running MCP observes a CLI lane created after its startup')
    pass_seen = .false.
    fail_seen = .false.
    blocked_seen = .false.
    do attempt = 1, 600
        arguments = '{"action":"gremlin_events","dir":'//mcp_quote(project)// &
            ',"lane_id":"new-cli-lane","session_id":'//mcp_quote(session_id)// &
            ',"cursor":0,"max_records":128,"max_bytes":262144}'
        call mcp_session_call(server, arguments, response)
        call extract_payload(response, payload)
        events = json_member(payload, 'events')
        pass_seen = .false.
        fail_seen = .false.
        blocked_seen = .false.
        do i = 1, json_size(events)
            event = json_element(events, i)
            case_id = json_string_value(json_member(event, 'case_id'))
            case_status = json_string_value(json_member(event, 'status'))
            if (case_id == 'test_stale_pass' .and. case_status == 'PASS') then
                pass_seen = .true.
            end if
            if (case_id == 'test_stale_fail' .and. case_status == 'FAIL') then
                fail_seen = .true.
            end if
            if (case_id == 'test_stale_blocked') blocked_seen = .true.
        end do
        if (pass_seen .and. fail_seen) exit
        if (mod(attempt, 20) == 0) then
            call mcp_session_call(server, status_arguments, response)
            call extract_payload(response, payload)
            state_name = json_string_value(json_member(payload, 'state'))
            if (state_name == 'stopped' .or. state_name == 'error') then
                call assert_true(.false., &
                    'persistent MCP reports terminal owner state before pass receipt: '// &
                    state_name)
                call stop_lane_without_owner()
                call mcp_session_shutdown(server, exit_code)
                call finish_assertions(retain_failed_scratch=.true.)
            end if
        end if
        call gremlin_wait_ms(50)
    end do
    call assert_true(pass_seen, &
        'already-running MCP sees the CLI completion receipt')
    call assert_true(fail_seen, &
        'MCP preserves a real FAIL despite PASS-shaped fixture output')
    call assert_true(.not. blocked_seen, &
        'MCP leaves the in-flight blocked case without a terminal event')
    call assert_true(mcp_process_identity_running(blocked_pid, blocked_start), &
        'blocked child is still running while MCP reports the pass receipt')
    call assert_true(mcp_process_identity_running(server%pid, server%start_time), &
        'CLI lane creation and completion do not replace or terminate the MCP process')
    call gremlin_stop_lane(driver, project, cache, state, 'new-cli-lane', session_id)
    if (blocked_start > 0_c_int64_t) then
        do attempt = 1, 250
            if (.not. mcp_process_identity_running(blocked_pid, blocked_start)) exit
            call gremlin_wait_ms(20)
        end do
        if (mcp_process_identity_running(blocked_pid, blocked_start)) then
            call assert_true(.false., 'stopping the lane reaps its blocked test sleeper')
            call mcp_kill_owned_tree(blocked_pid, blocked_start, cleanup_status)
        else
            call assert_true(.true., 'stopping the lane reaps its blocked test sleeper')
        end if
    end if
    call assert_true(mcp_process_identity_running(server%pid, server%start_time), &
        'stopping the CLI lane leaves the persistent MCP process alive')
    call mcp_session_shutdown(server, exit_code)
    call assert_true(exit_code == 0, &
        'persistent MCP exits cleanly after stale-owner check')
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine stop_lane_without_owner()
        type(string_list_t) :: stop_args
        type(json_value_t) :: stop_reply
        type(process_result_t) :: stop_process
        character(:), allocatable :: cleanup_owner

        call list_add(stop_args, 'gremlin')
        call list_add(stop_args, 'stop')
        call list_add(stop_args, '--dir')
        call list_add(stop_args, project)
        call list_add(stop_args, '--lane')
        call list_add(stop_args, 'new-cli-lane')
        call list_add(stop_args, '--json')
        call gremlin_json(driver, project, cache, state, stop_args, stop_reply, &
            stop_process, 10000)
        call assert_true(stop_process%exit_code == 0, &
            'cleanup by lane name succeeds after start omitted its owner ID; stderr='// &
            trim(stop_process%stderr))
        cleanup_owner = json_string_value(json_member(stop_reply, 'session_id'))
        if (len(cleanup_owner) > 0) then
            call gremlin_stop_lane(driver, project, cache, state, &
                'new-cli-lane', cleanup_owner)
        end if
    end subroutine stop_lane_without_owner

    subroutine extract_payload(envelope, document)
        type(json_value_t), intent(in) :: envelope
        type(json_value_t), intent(out) :: document
        type(json_value_t) :: result, content, first
        character(:), allocatable :: raw, parse_message
        logical :: valid

        result = json_member(envelope, 'result')
        content = json_member(result, 'content')
        first = json_element(content, 1)
        raw = json_string_value(json_member(first, 'text'))
        call json_parse(raw, document, valid, parse_message)
        call assert_true(valid, 'independent parser reads persistent MCP status')
    end subroutine extract_payload

end program test_mcp_gremlin_stale
