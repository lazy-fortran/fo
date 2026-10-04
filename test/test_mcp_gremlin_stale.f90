program test_mcp_gremlin_stale
    use, intrinsic :: iso_c_binding, only: c_int64_t
    use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, assert_true
    use fo_test_harness, only: finish_assertions, current_directory
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_wait_ms, gremlin_wait_file, gremlin_stop_lane
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start
    use fo_test_mcp_session, only: mcp_session_call, mcp_session_request
    use fo_test_mcp_session, only: mcp_session_notify
    use fo_test_mcp_session, only: mcp_session_shutdown
    use fo_test_process_identity, only: mcp_process_start_time
    use fo_test_process_identity, only: mcp_process_identity_running, mcp_kill_owned_tree
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size
    use fo_test_json, only: json_string_value, json_number_value
    use fx_hash, only: sha256_file
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: session_id
    character(:), allocatable :: case_id, case_status, state_name
    character(:), allocatable :: blocked_pid_path, blocked_pid_text
    character(:), allocatable :: stale_driver, pass_marker, peer_cwd
    character(len=4096) :: stale_image, self_image
    character(len=32) :: mode
    character(len=64) :: stale_digest, driver_digest
    type(mcp_session_t) :: server
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: response, payload, events, event, result, actions
    integer :: exit_code, attempt, blocked_pid, status, cleanup_status, i
    character(len=16) :: exit_text
    integer(c_int64_t) :: blocked_start
    logical :: found, pass_seen, fail_seen, blocked_seen, has_gremlin, catalog_valid

    call get_command_argument(1, mode, status=status)
    if (status == 0 .and. trim(mode) == 'mcp-server') then
        call serve_unsupported_peer()
        stop
    end if

    call gremlin_setup(driver, scratch, project, cache, state)
    ! Run this native fixture as a controlled peer, never build historical Fo.
    call get_command_argument(0, self_image, status=status)
    call assert_true(status == 0 .and. len_trim(self_image) > 0, &
        'resolves the exact native peer executable')
    if (status /= 0 .or. len_trim(self_image) == 0) then
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    stale_driver = trim(self_image)
    if (stale_driver(1:1) /= '/') then
        call current_directory(peer_cwd)
        stale_driver = peer_cwd//'/'//stale_driver
    end if
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
    call write_native_blocked_case()

    call mcp_session_start(server, stale_driver, project, cache, state, &
        scratch//'/mcp-stale.stderr')
    call mcp_session_request(server, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{},'// &
        '"clientInfo":{"name":"fo-stale-native-test","version":"1"}}', response)
    call assert_true(len(json_string_value(json_member(&
        json_member(response, 'result'), 'protocolVersion'))) > 0, &
        'unsupported MCP peer initializes before CLI owner starts')
    call mcp_session_notify(server, 'notifications/initialized')
    call mcp_session_request(server, 'tools/list', '', response)
    result = json_member(response, 'result')
    event = json_element(json_member(result, 'tools'), 1)
    actions = json_member(json_member(json_member(json_member(event, &
        'inputSchema'), 'properties'), 'action'), 'enum')
    call assert_true(json_size(actions) > 0, &
        'peer advertises an explicit action catalog')
    has_gremlin = .false.
    catalog_valid = json_size(actions) > 0
    do i = 1, json_size(actions)
        case_id = json_string_value(json_element(actions, i))
        if (len(case_id) == 0) catalog_valid = .false.
        if (index(case_id, 'gremlin_') == 1) &
            has_gremlin = .true.
    end do
    call assert_true(catalog_valid, 'peer catalog contains decoded nonempty action names')
    call assert_true(.not. has_gremlin, 'live MCP peer has no Gremlin actions')
    if (.not. catalog_valid .or. has_gremlin) then
        call mcp_session_shutdown(server, exit_code)
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    call mcp_session_call(server, '{"action":"gremlin_status"}', response)
    result = json_member(response, 'error')
    status = int(json_number_value(json_member(result, 'code')))
    call assert_true(status == -32601 .or. status == -32602, &
        'unsupported peer explicitly rejects a Gremlin request')
    call assert_true(mcp_process_identity_running(server%pid, server%start_time), &
        'unsupported request leaves the recorded peer process connected')

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
        call cli_observe('status', payload)
        if (json_string_value(json_member(payload, 'session_id')) == session_id) exit
        state_name = json_string_value(json_member(payload, 'state'))
        if (state_name == 'stopped' .or. state_name == 'error') then
            call assert_true(.false., &
                'new CLI saw terminal owner state before its ID: '//state_name)
            call stop_lane_without_owner()
            call mcp_session_shutdown(server, exit_code)
            call finish_assertions(retain_failed_scratch=.true.)
        end if
        call gremlin_wait_ms(50)
    end do
    call assert_true(attempt <= 600, &
        'new CLI observes its lane while unsupported MCP remains connected')
    pass_seen = .false.
    fail_seen = .false.
    blocked_seen = .false.
    do attempt = 1, 600
        call cli_observe('events', payload)
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
            call cli_observe('status', payload)
            state_name = json_string_value(json_member(payload, 'state'))
            if (state_name == 'stopped' .or. state_name == 'error') then
                call assert_true(.false., &
                    'new CLI reports terminal owner state before pass receipt: '// &
                    state_name)
                call stop_lane_without_owner()
                call mcp_session_shutdown(server, exit_code)
                call finish_assertions(retain_failed_scratch=.true.)
            end if
        end if
        call gremlin_wait_ms(50)
    end do
    call assert_true(pass_seen, &
        'new CLI sees its completion receipt with an unsupported MCP connected')
    call assert_true(fail_seen, &
        'new CLI preserves a real FAIL despite PASS-shaped fixture output')
    call assert_true(.not. blocked_seen, &
        'new CLI leaves the in-flight blocked case without a terminal event')
    call assert_true(mcp_process_identity_running(blocked_pid, blocked_start), &
        'blocked child is still running while new CLI reports the pass receipt')
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

    subroutine write_native_blocked_case()
        character(:), allocatable :: source

        source = 'program test_stale_blocked'//new_line('a')// &
            'use, intrinsic :: iso_c_binding, only: c_int'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'interface'//new_line('a')// &
            'integer(c_int) function child_fork() bind(C, name="fork")'// &
            new_line('a')//'import :: c_int'//new_line('a')// &
            'end function child_fork'//new_line('a')// &
            'integer(c_int) function child_sleep(seconds) bind(C, name="sleep")'// &
            new_line('a')//'import :: c_int'//new_line('a')// &
            'integer(c_int), value :: seconds'//new_line('a')// &
            'end function child_sleep'//new_line('a')// &
            'integer(c_int) function child_wait(pid, status, options) '// &
            'bind(C, name="waitpid")'//new_line('a')// &
            'import :: c_int'//new_line('a')// &
            'integer(c_int), value :: pid, options'//new_line('a')// &
            'integer(c_int), intent(out) :: status'//new_line('a')// &
            'end function child_wait'//new_line('a')// &
            'end interface'//new_line('a')// &
            'integer(c_int) :: child, rc, wait_status'//new_line('a')// &
            'integer :: unit'//new_line('a')// &
            'child = child_fork()'//new_line('a')// &
            'if (child < 0_c_int) error stop 1'//new_line('a')// &
            'if (child == 0_c_int) then'//new_line('a')// &
            'rc = child_sleep(60_c_int)'//new_line('a')// &
            'stop'//new_line('a')//'end if'//new_line('a')// &
            'open(newunit=unit, file="'//blocked_pid_path// &
            '", status="replace", action="write")'//new_line('a')// &
            'write(unit, "(i0)") child'//new_line('a')// &
            'close(unit)'//new_line('a')// &
            'rc = child_wait(child, wait_status, 0_c_int)'//new_line('a')// &
            'if (rc /= child) error stop 2'//new_line('a')// &
            'end program test_stale_blocked'//new_line('a')
        call write_text(project//'/test/test_stale_blocked.f90', source)
    end subroutine write_native_blocked_case

    subroutine serve_unsupported_peer()
        character(len=32768) :: request
        character(len=32) :: id_text
        character(:), allocatable :: method, body, message, response_key
        type(json_value_t) :: document, id
        integer :: ios
        logical :: valid

        do
            read(input_unit, '(a)', iostat=ios) request
            if (ios /= 0) exit
            call json_parse(trim(request), document, valid, message)
            if (.not. valid) error stop 2
            id = json_member(document, 'id')
            if (id%kind == 0) cycle
            write(id_text, '(i0)') int(json_number_value(id))
            method = json_string_value(json_member(document, 'method'))
            response_key = 'result'
            select case (method)
            case ('initialize')
                body = '{"protocolVersion":"2025-11-25",'// &
                    '"capabilities":{"tools":{}},'// &
                    '"serverInfo":{"name":"native-no-gremlin-peer","version":"1"}}'
            case ('tools/list')
                body = '{"tools":[{"name":"fo","inputSchema":{"type":"object",'// &
                    '"properties":{"action":{"type":"string",'// &
                    '"enum":["check"]}}}}]}'
            case ('shutdown')
                body = 'null'
            case ('tools/call')
                response_key = 'error'
                body = '{"code":-32602,"message":"Gremlin actions are unavailable"}'
            case default
                error stop 3
            end select
            write(output_unit, '(a)') '{"jsonrpc":"2.0","id":'//trim(id_text)// &
                ',"'//response_key//'":'//body//'}'
            flush(output_unit)
            if (method == 'shutdown') exit
        end do
    end subroutine serve_unsupported_peer

    subroutine cli_observe(action, document)
        character(len=*), intent(in) :: action
        type(json_value_t), intent(out) :: document
        type(string_list_t) :: query
        type(process_result_t) :: observed

        call list_add(query, 'gremlin')
        call list_add(query, action)
        call list_add(query, '--dir')
        call list_add(query, project)
        call list_add(query, '--lane')
        call list_add(query, 'new-cli-lane')
        call list_add(query, '--session')
        call list_add(query, session_id)
        call list_add(query, '--json')
        if (action == 'events') then
            call list_add(query, '--cursor')
            call list_add(query, '0')
            call list_add(query, '--max-records')
            call list_add(query, '128')
            call list_add(query, '--max-bytes')
            call list_add(query, '262144')
        end if
        call gremlin_json(driver, project, cache, state, query, document, &
            observed, 10000)
        call assert_true(observed%exit_code == 0, &
            'actual current CLI observes its lane without an MCP Gremlin request')
    end subroutine cli_observe

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

end program test_mcp_gremlin_stale
