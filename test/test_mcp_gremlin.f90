program test_mcp_gremlin
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, append_text, file_exists
    use fo_test_harness, only: make_directory, make_symlink, assert_true
    use fo_test_harness, only: assert_equal_string, assert_equal_integer
    use fo_test_harness, only: assert_contains, finish_assertions, run_process, current_directory
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_field, gremlin_run
    use fo_test_gremlin_oracle, only: gremlin_write_case, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_gate_create, gremlin_wait_file, gremlin_stop_lane
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start
    use fo_test_mcp_session, only: mcp_session_call, mcp_session_request
    use fo_test_mcp_session, only: mcp_session_notify, mcp_session_shutdown
    use fo_test_mcp, only: mcp_quote, mcp_call, mcp_encode, mcp_exchange
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_number_value
    use fo_test_json, only: json_boolean_value, json_object, json_array
    use fo_gremlin_session, only: gremlin_get_session_journal_path
    use fx_hash, only: sha256_file
    use fo_test_gremlin_oracle, only: gremlin_gate_read_source, gremlin_pid_binding
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, owner
    character(:), allocatable :: identity, generation, gate, blocked, failure_marker
    character(:), allocatable :: parity, gate_project, parity_owner, cli_owner, gate_owner
    character(:), allocatable :: original_ids, expected_ids, mcp_ids, cli_ids
    character(:), allocatable :: cwd, snapshot_image
    character(len=4096) :: journal
    character(len=256) :: message
    type(mcp_session_t) :: server
    type(json_value_t) :: response, document, other, events, event, failure, catalog
    type(json_value_t) :: actions, modes, mcp_sample, cli_sample
    type(string_list_t) :: arguments
    type(process_result_t) :: process
    integer :: status, i, j, attempt, before, unit
    integer(int64) :: started, finished, rate
    logical :: found
    character(len=24), parameter :: public_actions(7) = [character(len=24) :: &
        'gremlin_start', 'gremlin_status', 'gremlin_wait', 'gremlin_events', &
        'gremlin_failures', 'gremlin_reproduce', 'gremlin_stop']

    call current_directory(cwd)
    call gremlin_setup(driver, scratch, project, cache, state)
    gate = scratch//'/blocked.fifo'
    blocked = scratch//'/blocked.started'
    failure_marker = scratch//'/failure.executions'
    call gremlin_gate_create(gate)
    call write_text(project//'/fpm.toml', 'name="mcp_gremlin_probe"'//new_line('a'))
    call gremlin_write_case(project, 'test_mcp_pass', marker_body(scratch//'/pass.done'))
    call gremlin_write_case(project, 'test_mcp_fail', marker_body(failure_marker)// &
        new_line('a')//'print *, "mcp-gremlin-failure-token"'//new_line('a')//'error stop 7')
    call gremlin_write_case(project, 'test_mcp_blocked', &
        'integer :: gate_unit'//new_line('a')//'character :: token'//new_line('a')// &
        marker_body(blocked)//new_line('a')// &
        gremlin_gate_read_source(gate)//new_line('a')// &
        marker_body(scratch//'/blocked.done', .true.))
    call mcp_session_start(server, driver, project, cache, state, scratch//'/mcp.stderr')
    call mcp_session_request(server, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{}}', response)
    call assert_equal_string(gremlin_field(json_member(response, 'result'), &
        'protocolVersion'), '2025-11-25', 'MCP negotiates the requested public protocol')
    call mcp_session_notify(server, 'notifications/initialized')
    call mcp_session_request(server, 'tools/list', '', response)
    catalog = json_element(json_member(json_member(response, 'result'), 'tools'), 1)
    catalog = json_member(json_member(catalog, 'inputSchema'), 'properties')
    actions = json_member(json_member(catalog, 'action'), 'enum')
    do i = 1, size(public_actions)
        found = .false.
        do j = 1, json_size(actions)
            if (json_string_value(json_element(actions, j)) == trim(public_actions(i))) &
                found = .true.
        end do
        call assert_true(found, 'MCP advertises '//trim(public_actions(i)))
    end do
    modes = json_member(json_member(catalog, 'detail'), 'enum')
    call assert_equal_integer(json_size(modes), 2, 'detail has exactly two public modes')
    call assert_equal_string(json_string_value(json_element(modes, 1)), &
        'summary', 'first advertised detail is summary')
    call assert_equal_string(json_string_value(json_element(modes, 2)), &
        'full', 'second advertised detail is full')
    call system_clock(started, rate)
    call call_tool('{"action":"gremlin_start","dir":'//mcp_quote(project)// &
        ',"lane_id":"mcp-probe","targets":["test_mcp_pass","test_mcp_fail",'// &
        '"test_mcp_blocked"],"random_count":0,"seed":1729,'// &
        '"timeout_seconds":5,"campaign_seconds":60}', document)
    call system_clock(finished)
    call assert_true(real(finished - started, real64)/real(rate, real64) < 3.0_real64, &
        'public MCP detached start returns within three seconds')
    owner = gremlin_field(document, 'session_id')
    call assert_true(len(owner) > 0, 'start publishes a reconnectable owner identity')
    call assert_true(gremlin_field(document, 'state') == 'running' .or. &
        gremlin_field(document, 'state') == 'testing', 'start returns an active owner')
    identity = ',"dir":'//mcp_quote(project)//',"lane_id":"mcp-probe",'// &
        '"session_id":'//mcp_quote(owner)
    call cli_begin('start', project, 'mcp-probe', '')
    call list_add(arguments, '--target'); call list_add(arguments, 'test_mcp_pass')
    call list_add(arguments, '--target'); call list_add(arguments, 'test_mcp_fail')
    call list_add(arguments, '--target'); call list_add(arguments, 'test_mcp_blocked')
    call list_add(arguments, '--random'); call list_add(arguments, '0')
    call list_add(arguments, '--seed'); call list_add(arguments, '1729')
    call list_add(arguments, '--timeout-seconds'); call list_add(arguments, '5')
    call list_add(arguments, '--campaign-seconds'); call list_add(arguments, '60')
    call cli_read(project, other)
    call assert_equal_string(gremlin_field(other, 'session_id'), owner, &
        'same-place CLI start attaches to the MCP owner')
    call assert_equal_string(gremlin_field(other, 'state'), 'attached', &
        'same-policy public CLI reports attachment')
    ! The client makes no requests while independently executed test markers appear.
    call gremlin_wait_file(scratch//'/pass.done', 30000, found)
    call assert_true(found, 'real passing test progresses with an idle MCP client')
    call gremlin_wait_file(failure_marker, 30000, found)
    call assert_true(found, 'real failing test progresses with an idle MCP client')
    call gremlin_wait_file(blocked, 30000, found)
    call assert_true(found, 'third real test enters its blocked execution')
    call assert_true(.not. file_exists(scratch//'/blocked.done'), &
        'blocked execution has no invented completion marker')
    do attempt = 1, 300
        call call_tool('{"action":"gremlin_status"'//identity// &
            ',"detail":"full"}', document)
        events = json_member(document, 'events')
        if (json_size(events) >= 2 .and. &
            gremlin_field(document, 'current_test') == 'test_mcp_blocked') exit
        call gremlin_wait_ms(100)
    end do
    call validate_requests()
    call cli_begin('status', project, 'mcp-probe', owner)
    call list_add(arguments, '--detail'); call list_add(arguments, 'full')
    call cli_read(project, other)
    call assert_true(same_json(document, other), 'CLI and MCP share exact full status behavior')
    call call_tool('{"action":"gremlin_wait"'//identity//',"wait_ms":100}', document)
    call assert_summary(document)
    call assert_equal_string(gremlin_field(document, 'state'), 'testing', &
        'wait reconnects to the detached blocked owner')
    call call_tool('{"action":"gremlin_status"'//identity//'}', document)
    call assert_summary(document)
    failure = json_member(document, 'latest_failure')
    call assert_equal_string(gremlin_field(failure, 'case_id'), 'test_mcp_fail', &
        'summary retains the independently executed current failure')
    call assert_equal_string(gremlin_field(failure, 'status'), 'FAIL', &
        'summary retains the actual failure verdict')
    call assert_true(len(gremlin_field(failure, 'log_path')) > 0, &
        'summary retains the completed failure artifact reference')
    call call_tool('{"action":"gremlin_events"'//identity// &
        ',"cursor":0,"max_records":8,"max_bytes":8192}', document)
    call assert_equal_string(gremlin_field(document, 'action'), 'events', &
        'first event page retains its public action')
    events = json_member(document, 'events')
    call assert_receipt(events, 'test_mcp_pass', 'PASS')
    call assert_receipt(events, 'test_mcp_fail', 'FAIL')
    do i = 1, json_size(events)
        event = json_element(events, i)
        call assert_true(gremlin_field(event, 'case_id') /= 'test_mcp_blocked', &
            'blocked case has no invented terminal receipt')
        if (gremlin_field(event, 'case_id') == 'test_mcp_fail') failure = event
    end do
    call call_tool('{"action":"gremlin_failures"'//identity//'}', document)
    call assert_receipt(json_member(document, 'events'), 'test_mcp_fail', 'FAIL', &
        gremlin_field(failure, 'generation'))
    before = count_lines(read_text(failure_marker))
    call assert_true(before > 0, 'failure receipt came from actual executed code')
    call call_tool('{"action":"gremlin_reproduce"'//identity// &
        ',"case_id":"test_mcp_fail","generation_id":'// &
        mcp_quote(gremlin_field(failure, 'generation'))//'}', document, .true.)
    call assert_equal_string(gremlin_field(document, 'session_id'), owner, &
        'reproduce retains the exact owner identity')
    call assert_equal_string(gremlin_field(document, 'state'), 'FAIL', &
        'reproduced failing code reports FAIL and MCP isError')
    call assert_true(count_lines(read_text(failure_marker)) > before, &
        'reproduce executes the captured failed test again')
    call call_tool('{"action":"gremlin_stop"'//identity//'}', document)
    call assert_equal_string(gremlin_field(document, 'state'), 'stopping', &
        'scoped MCP stop acknowledges the stopping owner')
    call gremlin_stop_lane(driver, project, cache, state, 'mcp-probe', owner)
    call call_tool('{"action":"gremlin_wait"'//identity//',"wait_ms":100}', document)
    call assert_equal_string(gremlin_field(document, 'session_id'), owner, &
        'MCP reconnects to the exact terminal owner')
    call assert_equal_string(gremlin_field(document, 'state'), 'stopped', &
        'MCP observes completed stop through terminal replay')
    call selection_and_quiescence()
    call receipt_pagination()
    call mcp_session_shutdown(server, status)
    call assert_equal_integer(status, 0, 'interactive MCP exits after owned lane cleanup')
    call finish_assertions(retain_failed_scratch=.true.)

contains

    function marker_body(path, declared) result(body)
        character(len=*), intent(in) :: path
        logical, optional, intent(in) :: declared
        character(:), allocatable :: body
        logical :: omit_declaration

        omit_declaration = .false.
        if (present(declared)) omit_declaration = declared
        body = ''
        if (.not. omit_declaration) body = 'integer :: marker_unit'//new_line('a')
        body = body//'open(newunit=marker_unit,file="'//path// &
            '",status="unknown",position="append")'//new_line('a')// &
            'write(marker_unit,"(a)") "executed"'//new_line('a')//'close(marker_unit)'
    end function marker_body

    subroutine call_tool(request, body, expected_error)
        character(len=*), intent(in) :: request
        type(json_value_t), intent(out) :: body
        logical, optional, intent(in) :: expected_error
        type(json_value_t) :: result, content, field
        character(:), allocatable :: parse_message
        logical :: valid, error_expected

        error_expected = .false.
        if (present(expected_error)) error_expected = expected_error
        call mcp_session_call(server, request, response, timeout_ms=30000)
        result = json_member(response, 'result')
        field = json_member(result, 'isError')
        call assert_true(json_boolean_value(field) .eqv. error_expected, &
            'MCP tool error flag matches the independently expected outcome')
        content = json_element(json_member(result, 'content'), 1)
        call json_parse(gremlin_field(content, 'text'), body, valid, parse_message)
        call assert_true(valid, 'MCP tool body parses independently: '//parse_message)
    end subroutine call_tool

    subroutine cli_begin(action, directory, lane, session)
        character(len=*), intent(in) :: action, directory, lane, session

        arguments = string_list_t()
        call list_add(arguments, 'gremlin'); call list_add(arguments, action)
        call list_add(arguments, '--dir'); call list_add(arguments, directory)
        call list_add(arguments, '--lane'); call list_add(arguments, lane)
        if (len(session) > 0) then
            call list_add(arguments, '--session'); call list_add(arguments, session)
        end if
    end subroutine cli_begin

    subroutine cli_read(directory, body, expected_exit)
        character(len=*), intent(in) :: directory
        type(json_value_t), intent(out) :: body
        integer, optional, intent(in) :: expected_exit
        integer :: expected

        expected = 0
        if (present(expected_exit)) expected = expected_exit
        call gremlin_json(driver, directory, cache, state, arguments, body, process, 30000)
        call assert_equal_integer(process%exit_code, expected, 'public CLI expected exit code')
    end subroutine cli_read

    recursive logical function same_json(left, right) result(equal)
        type(json_value_t), intent(in) :: left, right
        type(json_value_t) :: child, counterpart
        integer :: index

        equal = .false.
        if (left%kind /= right%kind) return
        select case (left%kind)
        case (json_object, json_array)
            if (json_size(left) /= json_size(right)) return
            do index = 1, json_size(left)
                child = left%children(index)
                if (left%kind == json_object) then
                    counterpart = json_member(right, child%name)
                else
                    counterpart = json_element(right, index)
                end if
                if (.not. same_json(child, counterpart)) return
            end do
        case default
            if (allocated(left%text) .neqv. allocated(right%text)) return
            if (allocated(left%text)) then
                if (left%text /= right%text) return
            end if
            if (left%boolean .neqv. right%boolean) return
        end select
        equal = .true.
    end function same_json

    subroutine assert_summary(value)
        type(json_value_t), intent(in) :: value
        type(json_value_t) :: absent

        call assert_equal_string(gremlin_field(value, 'detail'), 'summary', &
            'public MCP status and wait default to bounded summary')
        call assert_equal_string(gremlin_field(value, 'session_id'), owner, &
            'summary retains exact owner identity')
        call assert_equal_string(gremlin_field(value, 'lane_id'), 'mcp-probe', &
            'summary retains exact lane identity')
        absent = json_member(value, 'events')
        call assert_equal_integer(absent%kind, 0, 'summary omits the durable event page')
    end subroutine assert_summary

    subroutine assert_receipt(receipts, case_id, verdict, expected_generation)
        type(json_value_t), intent(in) :: receipts
        character(len=*), intent(in) :: case_id, verdict
        character(len=*), optional, intent(in) :: expected_generation
        type(json_value_t) :: receipt
        integer :: index
        logical :: present_receipt

        present_receipt = .false.
        do index = 1, json_size(receipts)
            receipt = json_element(receipts, index)
            if (gremlin_field(receipt, 'case_id') /= case_id) cycle
            if (gremlin_field(receipt, 'status') /= verdict) cycle
            if (present(expected_generation)) then
                if (gremlin_field(receipt, 'generation') /= expected_generation) cycle
            end if
            present_receipt = .true.
        end do
        call assert_true(present_receipt, 'public journal retains actual '//case_id//' '//verdict)
    end subroutine assert_receipt

    integer function count_lines(text)
        character(len=*), intent(in) :: text
        integer :: position

        count_lines = 0
        do position = 1, len(text)
            if (text(position:position) == new_line('a')) count_lines = count_lines + 1
        end do
    end function count_lines

    function digits(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function digits

    subroutine validate_requests()
        type(json_value_t) :: body, cli_body, literal, escaped, field
        type(json_value_t), allocatable :: malformed(:)
        character(:), allocatable :: base, input, path, relative, encoded, extra, diagnostic
        integer :: index, length
        character(len=26), parameter :: raw_keys(4) = [character(len=26) :: &
            'lane_\u00e9id', 'd\u00e9ir', 'dir ', 'action ']

        base = '{"action":"gremlin_start","dir":'//mcp_quote(project)// &
            ',"lane_id":"invalid"'
        do index = 1, 4
            call cli_begin('start', project, 'invalid', '')
            select case (index)
            case (1)
                extra = ',"random_count":33'
                diagnostic = 'random_count must be between 0 and 32'
                call list_add(arguments, '--random'); call list_add(arguments, '33')
            case (2)
                extra = ',"random_count":"thirty-three"'
                diagnostic = 'integer request field has the wrong JSON type'
                call list_add(arguments, '--random'); call list_add(arguments, 'thirty-three')
            case (3)
                extra = ',"not_a_field":"value"'
                diagnostic = 'unsupported Gremlin request field: not_a_field'
                call list_add(arguments, '--not-a-field'); call list_add(arguments, 'value')
            case (4)
                extra = ',"lane_id":"duplicate"'
                diagnostic = 'duplicate Gremlin request field: lane_id'
                call list_add(arguments, '--lane'); call list_add(arguments, 'duplicate')
            end select
            if (index == 3) then
                call gremlin_run(driver, project, cache, state, arguments, process, timeout=30000)
                call assert_true(process%exit_code /= 0, 'unknown CLI option fails')
                call assert_contains(process%stdout//process%stderr, &
                    'fo gremlin: unknown option --not-a-field', 'CLI retains parse diagnostic')
            else
                call cli_read(project, cli_body, 2)
            end if
            call call_tool(base//extra//'}', body, .true.)
            call assert_contains(gremlin_field(body, 'error'), diagnostic, &
                'invalid MCP request retains exact shared-core error')
            if (index /= 3) call assert_true(same_json(body, cli_body), &
                'invalid public CLI and MCP requests have exact shared-core parity')
        end do
        input = mcp_encode(mcp_call(1, base//',"random_count":1 "seed":2}'), .false.)// &
            mcp_encode(mcp_call(2, base//',"targets":["test_mcp_pass",]}'), .false.)// &
            mcp_encode(mcp_call(3, base//',"targets":["test_mcp_pass" "test_mcp_fail"]}'), .false.)// &
            mcp_encode(mcp_call(4, base//',"random_count":01}'), .false.)// &
            mcp_encode('{"jsonrpc":"2.0","id":5,"method":"shutdown"}', .false.)
        call mcp_exchange(driver, project, cache, input, .false., malformed, process)
        do index = 1, 4
            field = json_member(malformed(index), 'error')
            call assert_equal_integer(int(json_number_value(json_member(field, 'code'))), &
                -32600, 'malformed envelope is rejected before core validation')
            call assert_equal_string(gremlin_field(field, 'message'), &
                'invalid JSON-RPC request', 'malformed envelope retains protocol diagnostic')
        end do
        call cli_begin('events', project, 'mcp-probe', owner)
        call list_add(arguments, '--cursor'); call list_add(arguments, '2147483648')
        call list_add(arguments, '--max-records'); call list_add(arguments, '1')
        call cli_read(project, cli_body, 2)
        call call_tool('{"action":"gremlin_events"'//identity// &
            ',"cursor":2147483648,"max_records":1}', body, .true.)
        call assert_true(same_json(body, cli_body), 'wide int64 cursor has CLI/MCP parity')
        call assert_contains(gremlin_field(body, 'error'), 'cursor is beyond', &
            'wide cursor reaches journal range check without int32 truncation')
        call unicode_requests()
    end subroutine validate_requests

    subroutine unicode_requests()
        type(json_value_t) :: body, literal, escaped
        character(:), allocatable :: path, relative, encoded, base
        integer :: index, length
        character(len=26), parameter :: keys(4) = [character(len=26) :: &
            'lane_\u00e9id', 'd\u00e9ir', 'dir ', 'action ']

        path = scratch//'/project-café'
        call make_symlink(project, path)
        base = ',"lane_id":"mcp-probe","session_id":'//mcp_quote(owner)
        call call_tool('{"action":"gremlin_status","dir":'//mcp_quote(path)// &
            base//'}', literal)
        encoded = mcp_quote(scratch//'/project-caf')
        encoded = encoded(:len(encoded) - 1)//'\u00e9"'
        call call_tool('{"action":"gremlin_status","dir":'//encoded//base//'}', escaped)
        call assert_true(same_json(literal, escaped), &
            'literal and escaped BMP Unicode directory names resolve identically')
        call assert_equal_string(gremlin_field(escaped, 'session_id'), owner, &
            'Unicode alias reaches the exact owner session')
        do index = 1, 2
            call call_tool('{"action":"gremlin_status"'//identity// &
                ',"'//trim(keys(index))//'":"shadow"}', body, .true.)
            call assert_contains(gremlin_field(body, 'error'), &
                'unsupported Gremlin request field', 'exact unknown key reaches core validation')
        end do
        ! Preserve trailing spaces in the two exact unsupported keys.
        call call_tool('{"action":"gremlin_status"'//identity// &
            ',"dir ":"shadow"}', body, .true.)
        call assert_contains(gremlin_field(body, 'error'), 'unsupported Gremlin request field', &
            'padded dir key is not normalized to the supported name')
        call call_tool('{"action":"gremlin_status"'//identity// &
            ',"action ":"shadow"}', body, .true.)
        call assert_contains(gremlin_field(body, 'error'), 'unsupported Gremlin request field', &
            'padded action key is not normalized to the supported name')
        call call_tool('{"action":"gremlin_status","dir":'//mcp_quote(project)// &
            ',"lane_id":"mcp\u00e9-probe","session_id":'//mcp_quote(owner)//'}', &
            escaped, .true.)
        call call_tool('{"action":"gremlin_status","dir":'//mcp_quote(project)// &
            ',"lane_id":"mcpé-probe","session_id":'//mcp_quote(owner)//'}', &
            body, .true.)
        call assert_true(same_json(body, escaped), 'literal and escaped Unicode lanes agree')
        path = scratch//'/project-🚀'
        call make_symlink(project, path)
        encoded = mcp_quote(scratch//'/project-')
        encoded = encoded(:len(encoded) - 1)//'\ud83d\ude80"'
        call call_tool('{"action":"gremlin_status","dir":'//encoded//base//'}', body)
        call assert_true(same_json(body, literal), &
            'supplementary surrogate pair reaches the same physical project')
        relative = '../'//repeat('a', 180)//'/'//repeat('b', 180)//'/'// &
            repeat('c', 135)//'/project-🚀'
        call assert_equal_integer(len(relative), 513, 'long relative path has 513 UTF8 bytes')
        path = scratch//'/'//repeat('a', 180)//'/'//repeat('b', 180)//'/'//repeat('c', 135)
        call make_directory(path)
        call make_symlink(project, path//'/project-🚀')
        call call_tool('{"action":"gremlin_status","dir":'// &
            mcp_quote(relative)//base//'}', literal)
        encoded = mcp_quote(relative(:len(relative) - 4))
        encoded = encoded(:len(encoded) - 1)//'\ud83d\ude80"'
        call call_tool('{"action":"gremlin_status","dir":'//encoded//base//'}', escaped)
        call assert_true(same_json(escaped, literal), &
            'literal and escaped supplementary paths beyond 512 bytes are equivalent')
        call assert_equal_string(gremlin_field(literal, 'session_id'), owner, &
            'long decoded relative directory reaches the exact owner')
        do index = 1, 2
            length = 4097
            if (index == 2) length = 8193
            call call_tool('{"action":"gremlin_status","dir":'// &
                mcp_quote('.'//repeat(' ', length - 1))//base//'}', body, .true.)
            call assert_contains(gremlin_field(body, 'error'), 'dir is too long', &
                'untrimmed decoded directory length rejects excess bytes safely')
        end do
        call call_tool('{"action":"gremlin_status","dir":"bad\u0000dir"'// &
            base//'}', body, .true.)
        call assert_contains(gremlin_field(body, 'error'), 'control character', &
            'NUL directory is rejected before OS path resolution')
    end subroutine unicode_requests

    subroutine selection_and_quiescence()
        type(json_value_t) :: body, cli_body, receipt, counterpart, ordinary
        character(:), allocatable :: first_generation, first_snapshot, second_snapshot
        character(:), allocatable :: quiet_receipts, name, request_identity
        integer :: index, prior, matches, attempt

        parity = scratch//'/parity'
        gate_project = scratch//'/gate-only'
        call make_directory(parity//'/test')
        call make_directory(gate_project//'/test')
        call write_text(parity//'/fpm.toml', 'name="mcp_parity"'//new_line('a'))
        call write_text(gate_project//'/fpm.toml', 'name="mcp_gate_only"'//new_line('a'))
        do index = 1, 4
            name = 'test_parity_'//achar(iachar('a') + index - 1)
            if (index <= 3) call gremlin_write_case(parity, name, 'print *, "parity-case"')
            call gremlin_write_case(gate_project, name, 'print *, "gate-case"')
        end do
        call call_tool('{"action":"gremlin_start","dir":'//mcp_quote(parity)// &
            ',"lane_id":"mcp-parity","random_count":2,"seed":1729,'// &
            '"timeout_seconds":5,"campaign_seconds":30}', body)
        parity_owner = gremlin_field(body, 'session_id')
        call cli_begin('start', parity, 'cli-parity', '')
        call sample_arguments(2, '1729')
        call cli_read(parity, cli_body)
        cli_owner = gremlin_field(cli_body, 'session_id')
        call sample_status(.true., parity, 'mcp-parity', parity_owner, mcp_sample)
        call sample_status(.false., parity, 'cli-parity', cli_owner, cli_sample)
        mcp_sample = json_member(mcp_sample, 'events')
        cli_sample = json_member(cli_sample, 'events')
        call assert_equal_integer(case_receipt_count(mcp_sample), 3, &
            'MCP seed selects one priority case and two additional random cases')
        call assert_equal_integer(case_receipt_count(cli_sample), 3, &
            'CLI seed selects one priority case and two additional random cases')
        do index = 1, json_size(mcp_sample)
            receipt = json_element(mcp_sample, index)
            name = gremlin_field(receipt, 'case_id')
            if (name == '<build>') cycle
            call assert_equal_string(gremlin_field(receipt, 'status'), 'PASS', &
                'real MCP first-sample case passes')
            call assert_equal_integer(int(json_number_value(json_member(receipt, 'seed'))), &
                1729, 'MCP sample retains the requested seed')
            matches = 0
            do prior = 1, json_size(cli_sample)
                counterpart = json_element(cli_sample, prior)
                if (gremlin_field(counterpart, 'case_id') == '<build>') cycle
                call assert_equal_string(gremlin_field(counterpart, 'status'), 'PASS', &
                    'real CLI first-sample case passes')
                call assert_equal_integer(int(json_number_value(json_member(counterpart, 'seed'))), &
                    1729, 'CLI sample retains the requested seed')
                if (gremlin_field(counterpart, 'case_id') == name) matches = matches + 1
            end do
            call assert_equal_integer(matches, 1, 'both adapters select each case exactly once')
            do prior = 1, index - 1
                counterpart = json_element(mcp_sample, prior)
                call assert_true(gremlin_field(counterpart, 'case_id') /= name, &
                    'first random sample contains distinct actual cases')
            end do
        end do
        call gremlin_stop_lane(driver, parity, cache, state, 'mcp-parity', parity_owner)
        call gremlin_stop_lane(driver, parity, cache, state, 'cli-parity', cli_owner)
        call call_tool('{"action":"gremlin_start","dir":'//mcp_quote(gate_project)// &
            ',"lane_id":"mcp-gate-only","targets":["test_parity_a"],'// &
            '"random_count":0,"seed":20261005,"timeout_seconds":5,'// &
            '"campaign_seconds":30}', body)
        gate_owner = gremlin_field(body, 'session_id')
        call cli_begin('start', gate_project, 'mcp-gate-only', '')
        call list_add(arguments, '--target'); call list_add(arguments, 'test_parity_a')
        call sample_arguments(0, '20261005')
        call cli_read(gate_project, cli_body)
        call assert_equal_string(gremlin_field(cli_body, 'session_id'), gate_owner, &
            'zero-random CLI start attaches to the MCP owner')
        request_identity = ',"dir":'//mcp_quote(gate_project)// &
            ',"lane_id":"mcp-gate-only","session_id":'//mcp_quote(gate_owner)
        call sample_status(.true., gate_project, 'mcp-gate-only', gate_owner, body)
        ordinary = json_member(json_member(body, 'coverage'), 'ordinary')
        call assert_true(json_number_value(json_member(ordinary, 'remaining')) > 0, &
            'zero-random gate completion leaves ordinary cases unexecuted')
        first_generation = gremlin_field(body, 'active_generation')
        first_snapshot = cache_snapshot()
        call assert_true(len(first_snapshot) > 0, 'quiescence observes a populated cache')
        call collect_events(.true., gate_project, 'mcp-gate-only', gate_owner, quiet_receipts)
        call gremlin_wait_ms(11000)
        call call_tool('{"action":"gremlin_status"'//request_identity//'}', body)
        call assert_equal_string(gremlin_field(body, 'state'), 'quiescent', &
            'zero-random owner remains quiescent across two maintenance periods')
        second_snapshot = cache_snapshot()
        call assert_equal_string(second_snapshot, first_snapshot, &
            'quiescent owner leaves all cache metadata and file bytes unchanged')
        call collect_events(.true., gate_project, 'mcp-gate-only', gate_owner, mcp_ids)
        call assert_equal_string(mcp_ids, quiet_receipts, &
            'quiescent maintenance publishes no new completion receipts')
        call append_text(gate_project//'/test/test_parity_a.f90', &
            '! relevant edit wakes the owner'//new_line('a'))
        do attempt = 1, 300
            call sample_status(.true., gate_project, 'mcp-gate-only', gate_owner, body)
            if (gremlin_field(body, 'active_generation') /= first_generation) exit
            call gremlin_wait_ms(100)
        end do
        call assert_true(gremlin_field(body, 'active_generation') /= first_generation, &
            'relevant target edit captures a new generation')
        call assert_equal_integer(int(json_number_value(json_member(body, 'gate_required'))), &
            1, 'the edited target remains the only required gate')
        call assert_equal_integer(int(json_number_value(json_member(body, 'gate_passed'))), &
            1, 'the new generation executes and passes the required gate')
        call gremlin_stop_lane(driver, gate_project, cache, state, 'mcp-gate-only', gate_owner)
    end subroutine selection_and_quiescence

    integer function case_receipt_count(receipts) result(count)
        type(json_value_t), intent(in) :: receipts
        type(json_value_t) :: receipt
        integer :: index

        count = 0
        do index = 1, json_size(receipts)
            receipt = json_element(receipts, index)
            if (gremlin_field(receipt, 'case_id') == '<build>') cycle
            count = count + 1
        end do
    end function case_receipt_count

    subroutine sample_arguments(random_count, seed)
        integer, intent(in) :: random_count
        character(len=*), intent(in) :: seed

        call list_add(arguments, '--random'); call list_add(arguments, digits(random_count))
        call list_add(arguments, '--seed'); call list_add(arguments, seed)
        call list_add(arguments, '--timeout-seconds'); call list_add(arguments, '5')
        call list_add(arguments, '--campaign-seconds'); call list_add(arguments, '30')
    end subroutine sample_arguments

    subroutine sample_status(mcp, directory, lane, session, body)
        logical, intent(in) :: mcp
        character(len=*), intent(in) :: directory, lane, session
        type(json_value_t), intent(out) :: body
        integer :: attempt

        do attempt = 1, 300
            if (mcp) then
                call call_tool('{"action":"gremlin_status","dir":'//mcp_quote(directory)// &
                    ',"lane_id":'//mcp_quote(lane)//',"session_id":'//mcp_quote(session)// &
                    ',"detail":"full","max_records":8,"max_bytes":8192}', body)
            else
                call cli_begin('status', directory, lane, session)
                call list_add(arguments, '--detail'); call list_add(arguments, 'full')
                call list_add(arguments, '--max-records'); call list_add(arguments, '8')
                call list_add(arguments, '--max-bytes'); call list_add(arguments, '8192')
                call cli_read(directory, body)
            end if
            if (gremlin_field(body, 'state') == 'quiescent') exit
            call gremlin_wait_ms(100)
        end do
        call assert_equal_string(gremlin_field(body, 'state'), 'quiescent', &
            'real finite campaign reaches quiescence within its bound')
    end subroutine sample_status

    function cache_snapshot() result(snapshot)
        character(:), allocatable :: snapshot, listing, line, path
        character(len=64) :: digest
        type(string_list_t) :: command
        type(process_result_t) :: result
        integer :: position, ending, separator, hash_status

        if (.not. allocated(snapshot_image)) then
            snapshot_image = scratch//'/cache_snapshot'
            call list_add(command, 'cc')
            call list_add(command, cwd//'/test-fixtures/c/cache_snapshot.c')
            call list_add(command, '-o'); call list_add(command, snapshot_image)
            call run_process(command, project, result, timeout_ms=30000)
            call assert_equal_integer(result%exit_code, 0, 'builds native metadata enumerator')
        end if
        command = string_list_t()
        call list_add(command, snapshot_image); call list_add(command, cache)
        call run_process(command, project, result, timeout_ms=30000)
        call assert_equal_integer(result%exit_code, 0, 'enumerates all cache metadata')
        snapshot = result%stdout
        listing = result%stdout
        position = 1
        do while (position <= len(listing))
            ending = index(listing(position:), new_line('a'))
            if (ending == 0) exit
            ending = position + ending - 1
            line = listing(position:ending - 1)
            if (len(line) > 0) then
                if (line(1:1) == 'F') then
                    separator = index(line, achar(9), back=.true.)
                    path = line(separator + 1:)
                    call sha256_file(path, digest, hash_status)
                    call assert_equal_integer(hash_status, 0, 'hashes independent cache file bytes')
                    snapshot = snapshot//path//':'//digest//new_line('a')
                end if
            end if
            position = ending + 1
        end do
    end function cache_snapshot

    subroutine receipt_pagination()
        character(:), allocatable :: completion, row
        integer :: index

        call collect_events(.true., parity, 'mcp-parity', parity_owner, original_ids)
        call collect_events(.false., parity, 'mcp-parity', parity_owner, cli_ids)
        call assert_true(len(original_ids) > 0, 'stopped owner retains actual durable receipts')
        call assert_equal_string(original_ids, cli_ids, 'both adapters read original receipt order')
        call gremlin_get_session_journal_path(parity, 'mcp-parity', parity_owner, &
            journal, status, message)
        call assert_equal_integer(status, 0, 'locates exact stopped owner journal')
        generation = gremlin_field(json_element(mcp_sample, 1), 'generation')
        expected_ids = original_ids
        do index = 0, 299
            completion = 'pagination-'//digits(index)
            row = '{"completion_id":'//mcp_quote(completion)// &
                ',"session_id":'//mcp_quote(parity_owner)//',"lane_id":"mcp-parity",'// &
                '"generation":'//mcp_quote(generation)//',"case_id":'// &
                mcp_quote('test_pagination_'//digits(index))// &
                ',"outcome":"pass","status":"PASS"}'//new_line('a')
            call append_text(trim(journal), row)
            expected_ids = expected_ids//completion//new_line('a')
        end do
        call assert_true(count_lines(expected_ids) > 256, 'independent stream exceeds 256 receipts')
        call collect_events(.true., parity, 'mcp-parity', parity_owner, mcp_ids)
        call collect_events(.false., parity, 'mcp-parity', parity_owner, cli_ids)
        call assert_equal_string(mcp_ids, expected_ids, 'MCP pages retain every receipt once in order')
        call assert_equal_string(cli_ids, expected_ids, 'CLI pages retain every receipt once in order')
        call assert_equal_string(mcp_ids, cli_ids, 'CLI/MCP traverse the identical complete stream')
    end subroutine receipt_pagination

    subroutine collect_events(mcp, directory, lane, session, ids)
        logical, intent(in) :: mcp
        character(len=*), intent(in) :: directory, lane, session
        character(:), allocatable, intent(out) :: ids
        type(json_value_t) :: body, receipts, receipt
        integer :: cursor, next_cursor, page, index

        ids = ''
        cursor = 0
        do page = 1, 100
            if (mcp) then
                call call_tool('{"action":"gremlin_events","dir":'//mcp_quote(directory)// &
                    ',"lane_id":'//mcp_quote(lane)//',"session_id":'//mcp_quote(session)// &
                    ',"cursor":'//digits(cursor)//',"max_records":128,"max_bytes":8192}', body)
            else
                call cli_begin('events', directory, lane, session)
                call list_add(arguments, '--cursor'); call list_add(arguments, digits(cursor))
                call list_add(arguments, '--max-records'); call list_add(arguments, '128')
                call list_add(arguments, '--max-bytes'); call list_add(arguments, '8192')
                call cli_read(directory, body)
            end if
            receipts = json_member(body, 'events')
            call assert_true(json_size(receipts) <= 128, 'page obeys requested record bound')
            do index = 1, json_size(receipts)
                receipt = json_element(receipts, index)
                ids = ids//gremlin_field(receipt, 'completion_id')//new_line('a')
            end do
            if (.not. json_boolean_value(json_member(body, 'has_more'))) return
            next_cursor = int(json_number_value(json_member(body, 'next_cursor')))
            call assert_true(next_cursor > cursor, 'nonterminal cursor advances without truncation')
            if (next_cursor <= cursor) return
            cursor = next_cursor
        end do
        call assert_true(.false., 'receipt pagination completes within its bounded page count')
    end subroutine collect_events

end program test_mcp_gremlin
