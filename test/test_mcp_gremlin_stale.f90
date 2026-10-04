program test_mcp_gremlin_stale
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, assert_true, assert_equal_integer
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_wait_ms, gremlin_stop_lane
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start
    use fo_test_mcp_session, only: mcp_session_call, mcp_session_request
    use fo_test_mcp_session, only: mcp_session_notify
    use fo_test_mcp_session, only: mcp_session_shutdown
    use fo_test_process_identity, only: mcp_process_identity_running
    use fo_test_mcp, only: mcp_quote
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size
    use fo_test_json, only: json_string_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: session_id, arguments
    type(mcp_session_t) :: server
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: response, payload, events, event, field, result
    integer :: exit_code, attempt
    character(len=16) :: exit_text

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/fpm.toml', &
        'name = "mcp_stale_owner_probe"'//new_line('a'))
    call gremlin_write_case(project, 'test_stale_owner', &
        'print *, "stale-owner-pass-token-9711"')

    call mcp_session_start(server, driver, project, cache, state, &
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
    call list_add(args, 'test_stale_owner')
    call list_add(args, '--random-count')
    call list_add(args, '0')
    call list_add(args, '--json')
    call gremlin_json(driver, project, cache, state, args, payload, process, 30000)
    if (process%exit_code /= 0) then
        write (exit_text, '(i0)') process%exit_code
        call assert_true(.false., 'new CLI failed to start lane (exit='//trim(exit_text)// &
            '); stderr='//trim(process%stderr)//'; stdout='//trim(process%stdout))
        call stop_lane_without_owner()
        call mcp_session_shutdown(server, exit_code)
        call finish_assertions()
    end if
    session_id = json_string_value(json_member(payload, 'session_id'))
    if (len(session_id) == 0) then
        call assert_true(.false., 'CLI response omitted its owner ID; stderr='// &
            trim(process%stderr)//'; stdout='//trim(process%stdout))
        call stop_lane_without_owner()
        call mcp_session_shutdown(server, exit_code)
        call finish_assertions()
    end if

    do attempt = 1, 600
        arguments = '{"action":"gremlin_status","dir":'//mcp_quote(project)// &
            ',"lane_id":"new-cli-lane","session_id":'//mcp_quote(session_id)//'}'
        call mcp_session_call(server, arguments, response)
        call extract_payload(response, payload)
        if (json_string_value(json_member(payload, 'session_id')) == session_id) exit
        call gremlin_wait_ms(50)
    end do
    call assert_true(attempt <= 600, &
        'already-running MCP observes a CLI lane created after its startup')
    do attempt = 1, 600
        arguments = '{"action":"gremlin_events","dir":'//mcp_quote(project)// &
            ',"lane_id":"new-cli-lane","session_id":'//mcp_quote(session_id)// &
            ',"cursor":0,"max_records":128,"max_bytes":262144}'
        call mcp_session_call(server, arguments, response)
        call extract_payload(response, payload)
        events = json_member(payload, 'events')
        if (json_size(events) == 1) exit
        call gremlin_wait_ms(50)
    end do
    call assert_true(attempt <= 600, &
        'already-running MCP sees the CLI completion receipt')
    call assert_equal_integer(json_size(events), 1, &
        'MCP reads the CLI owner durable receipt from shared state')
    if (json_size(events) == 1) then
        event = json_element(events, 1)
        field = json_member(event, 'status')
        call assert_true(json_string_value(field) == 'PASS', &
            'cross-process receipt reports the independent passing behavior')
    end if
    call assert_true(mcp_process_identity_running(server%pid, server%start_time), &
        'CLI lane creation and completion do not replace or terminate the MCP process')
    call gremlin_stop_lane(driver, project, cache, state, 'new-cli-lane', session_id)
    call mcp_session_shutdown(server, exit_code)
    call assert_true(exit_code == 0, &
        'persistent MCP exits cleanly after stale-owner check')
    call finish_assertions()

contains

    subroutine stop_lane_without_owner()
        type(string_list_t) :: stop_args
        type(json_value_t) :: stop_reply
        type(process_result_t) :: stop_process

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
