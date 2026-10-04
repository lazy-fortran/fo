program test_mcp_gremlin
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, assert_true, assert_equal_integer
    use fo_test_harness, only: assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_wait_ms, gremlin_stop_lane
    use fo_gremlin_session, only: gremlin_get_session_journal_path
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start
    use fo_test_mcp_session, only: mcp_session_call, mcp_session_request
    use fo_test_mcp_session, only: mcp_session_notify
    use fo_test_mcp_session, only: mcp_session_shutdown
    use fo_test_mcp, only: mcp_quote
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_number_value, json_boolean_value
    implicit none

    integer, parameter :: seeded_count = 270
    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: session_id, journal_path, cli_session
    character(:), allocatable :: mcp_generation, cli_generation
    character(len=32) :: exit_code_text
    character(len=512) :: message
    type(mcp_session_t) :: server
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: response, payload, events
    integer :: ierr, exit_code, unit

    call gremlin_setup(driver, scratch, project, cache, state)
    lane = 'native-mcp-parity'
    call write_text(project//'/fpm.toml', &
        'name = "native_mcp_parity_probe"'//new_line('a'))
    call gremlin_write_case(project, 'test_parity_a', 'print *, "parity-a"')
    call gremlin_write_case(project, 'test_parity_b', 'print *, "parity-b"')
    call gremlin_write_case(project, 'test_parity_c', 'print *, "parity-c"')
    call mcp_session_start(server, driver, project, cache, state, &
        scratch//'/mcp.stderr')
    call mcp_session_request(server, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{},'// &
        '"clientInfo":{"name":"fo-native-test","version":"1"}}', response)
    call assert_true(len(json_string_value(json_member(&
        json_member(response, 'result'), 'protocolVersion'))) > 0, &
        'persistent MCP server completes protocol initialization')
    call mcp_session_notify(server, 'notifications/initialized')

    call start_mcp_lane('mcp-parity', payload)
    session_id = json_string_value(json_member(payload, 'session_id'))
    call assert_true(len(session_id) > 0, 'MCP starts a durable Gremlin lane')
    if (len(session_id) == 0) then
        call stop_mcp_lane('mcp-parity', session_id)
        call mcp_session_shutdown(server, exit_code)
        call finish_assertions()
    end if
    call wait_for_events('mcp-parity', session_id, 3, events)
    mcp_generation = query_generation('mcp-parity', session_id, .true.)
    call stop_mcp_lane('mcp-parity', session_id)

    call start_cli_lane('cli-parity', cli_session)
    call assert_true(len(cli_session) > 0, 'CLI starts an equivalent Gremlin lane')
    call wait_for_events('cli-parity', cli_session, 3, events)
    cli_generation = query_generation('cli-parity', cli_session, .false.)
    call gremlin_stop_lane(driver, project, cache, state, 'cli-parity', cli_session)
    call compare_receipts('mcp-parity', session_id, 'cli-parity', cli_session)

    call gremlin_get_session_journal_path(project, 'mcp-parity', session_id, &
        journal_path, ierr, message)
    call assert_true(ierr == 0, 'resolves MCP session receipt journal')
    if (ierr /= 0) then
        call mcp_session_shutdown(server, exit_code)
        call finish_assertions()
    end if
    call append_page_receipts(journal_path, session_id, 'mcp-parity', &
        mcp_generation, seeded_count)
    call gremlin_get_session_journal_path(project, 'cli-parity', cli_session, &
        journal_path, ierr, message)
    call assert_true(ierr == 0, 'resolves CLI session receipt journal')
    if (ierr /= 0) then
        call mcp_session_shutdown(server, exit_code)
        call finish_assertions()
    end if
    call append_page_receipts(journal_path, cli_session, 'cli-parity', &
        cli_generation, seeded_count)
    call assert_paged_events('mcp-parity', session_id, .true.)
    call assert_paged_events('cli-parity', cli_session, .false.)
    call mcp_session_shutdown(server, exit_code)
    call assert_equal_integer(exit_code, 0, 'persistent MCP process shuts down cleanly')
    call finish_assertions()

contains

    subroutine start_mcp_lane(lane_id, document)
        character(len=*), intent(in) :: lane_id
        type(json_value_t), intent(out) :: document
        character(:), allocatable :: arguments

        arguments = '{"action":"gremlin_start","dir":'//mcp_quote(project)// &
            ',"lane_id":'//mcp_quote(lane_id)// &
            ',"targets":["test_parity_a","test_parity_b","test_parity_c"],'// &
            '"random_count":0,"seed":991,"campaign_seconds":60,'// &
            '"timeout_seconds":10}'
        call mcp_session_call(server, arguments, response)
        call extract_payload(response, document)
    end subroutine start_mcp_lane

    subroutine stop_mcp_lane(lane_id, owner)
        character(len=*), intent(in) :: lane_id, owner
        character(:), allocatable :: arguments
        integer :: attempt
        type(json_value_t) :: status_document

        arguments = '{"action":"gremlin_stop","dir":'//mcp_quote(project)// &
            ',"lane_id":'//mcp_quote(lane_id)// &
            ',"session_id":'//mcp_quote(owner)//'}'
        call mcp_session_call(server, arguments, response)
        call extract_payload(response, payload)
        call assert_true(.not. response_is_error(response), &
            'MCP stop accepts the exact lane owner')
        do attempt = 1, 200
            call query_mcp_status(lane_id, owner, status_document)
            if (json_string_value(json_member(status_document, 'state')) == 'stopped') &
                return
            call gremlin_wait_ms(25)
        end do
        call assert_true(.false., 'MCP observes terminal stop and owner exit')
    end subroutine stop_mcp_lane

    subroutine query_mcp_status(lane_id, owner, document)
        character(len=*), intent(in) :: lane_id, owner
        type(json_value_t), intent(out) :: document
        character(:), allocatable :: arguments

        arguments = '{"action":"gremlin_status","dir":'//mcp_quote(project)// &
            ',"lane_id":'//mcp_quote(lane_id)// &
            ',"session_id":'//mcp_quote(owner)//'}'
        call mcp_session_call(server, arguments, response)
        call extract_payload(response, document)
    end subroutine query_mcp_status

    subroutine start_cli_lane(lane_id, owner)
        character(len=*), intent(in) :: lane_id
        character(:), allocatable, intent(out) :: owner
        type(json_value_t) :: document

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'start')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane_id)
        call list_add(args, '--target')
        call list_add(args, 'test_parity_a')
        call list_add(args, '--target')
        call list_add(args, 'test_parity_b')
        call list_add(args, '--target')
        call list_add(args, 'test_parity_c')
        call list_add(args, '--random-count')
        call list_add(args, '0')
        call list_add(args, '--seed')
        call list_add(args, '991')
        call list_add(args, '--campaign-seconds')
        call list_add(args, '60')
        call list_add(args, '--timeout-seconds')
        call list_add(args, '10')
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, document, process, 30000)
        call assert_true(process%exit_code == 0, 'CLI starts its Gremlin lane')
        owner = json_string_value(json_member(document, 'session_id'))
    end subroutine start_cli_lane

    function query_generation(lane_id, owner, over_mcp) result(generation)
        character(len=*), intent(in) :: lane_id, owner
        logical, intent(in) :: over_mcp
        character(:), allocatable :: generation
        type(json_value_t) :: document

        if (over_mcp) then
            call query_mcp_status(lane_id, owner, document)
        else
            args = string_list_t()
            call list_add(args, 'gremlin')
            call list_add(args, 'status')
            call list_add(args, '--dir')
            call list_add(args, project)
            call list_add(args, '--lane')
            call list_add(args, lane_id)
            call list_add(args, '--session')
            call list_add(args, owner)
            call list_add(args, '--json')
            call gremlin_json(driver, project, cache, state, args, document, &
                process, 10000)
            call assert_true(process%exit_code == 0, 'CLI reads lane generation')
        end if
        generation = json_string_value(json_member(document, 'active_generation'))
        call assert_true(len(generation) == 64, &
            'status exposes the active source generation identity')
    end function query_generation

    subroutine wait_for_events(lane_id, owner, needed, document)
        character(len=*), intent(in) :: lane_id, owner
        integer, intent(in) :: needed
        type(json_value_t), intent(out) :: document
        integer :: attempt

        do attempt = 1, 600
            call query_events(lane_id, owner, document, .false., 0)
            events = json_member(document, 'events')
            if (json_size(events) >= needed) return
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'lane publishes all independent terminal receipts')
    end subroutine wait_for_events

    subroutine query_events(lane_id, owner, document, over_mcp, cursor_value)
        character(len=*), intent(in) :: lane_id, owner
        type(json_value_t), intent(out) :: document
        logical, intent(in) :: over_mcp
        integer, intent(in) :: cursor_value
        character(len=32) :: number
        character(:), allocatable :: arguments

        write(number, '(i0)') cursor_value
        if (over_mcp) then
            arguments = '{"action":"gremlin_events","dir":'//mcp_quote(project)// &
                ',"lane_id":'//mcp_quote(lane_id)// &
                ',"session_id":'//mcp_quote(owner)//',"cursor":'//trim(number)// &
                ',"max_records":128,"max_bytes":262144}'
            call mcp_session_call(server, arguments, response)
            call extract_payload(response, document)
        else
            args = string_list_t()
            call list_add(args, 'gremlin')
            call list_add(args, 'events')
            call list_add(args, '--dir')
            call list_add(args, project)
            call list_add(args, '--lane')
            call list_add(args, lane_id)
            call list_add(args, '--session')
            call list_add(args, owner)
            call list_add(args, '--cursor')
            call list_add(args, trim(number))
            call list_add(args, '--max-records')
            call list_add(args, '128')
            call list_add(args, '--max-bytes')
            call list_add(args, '262144')
            call list_add(args, '--json')
            call gremlin_json(driver, project, cache, state, args, document, process, &
                10000)
            call assert_true(process%exit_code == 0, 'CLI reads durable receipt page')
        end if
    end subroutine query_events

    subroutine compare_receipts(mcp_lane, mcp_owner, cli_lane, cli_owner)
        character(len=*), intent(in) :: mcp_lane, mcp_owner, cli_lane, cli_owner
        type(json_value_t) :: mcp_document, cli_document, mcp_events, cli_events
        integer :: i

        call query_events(mcp_lane, mcp_owner, mcp_document, .true., 0)
        call query_events(cli_lane, cli_owner, cli_document, .false., 0)
        mcp_events = json_member(mcp_document, 'events')
        cli_events = json_member(cli_document, 'events')
        call assert_equal_integer(json_size(mcp_events), 3, &
            'MCP exposes each target terminal receipt once')
        call assert_equal_integer(json_size(cli_events), 3, &
            'CLI exposes each target terminal receipt once')
        do i = 1, min(json_size(mcp_events), json_size(cli_events))
            call assert_equal_string(event_case(mcp_events, i), &
                event_case(cli_events, i), 'CLI and MCP agree on selected case order')
            call assert_equal_string(event_status(mcp_events, i), &
                event_status(cli_events, i), 'CLI and MCP agree on terminal outcome')
        end do
    end subroutine compare_receipts

    subroutine append_page_receipts(path, owner, lane_id, generation, count)
        character(len=*), intent(in) :: path, owner, lane_id, generation
        integer, intent(in) :: count
        integer :: i, status

        open(newunit=unit, file=path, status='old', position='append', &
            action='write', iostat=status)
        call assert_true(status == 0, &
            'opens stopped session journal for pagination oracle')
        if (status /= 0) return
        do i = 1, count
            write(exit_code_text, '(i4.4)') i
            write(unit, '(a)', iostat=status) &
                '{"completion_id":"native-page-'//trim(exit_code_text)// &
                '","session_id":"'//owner//'","lane_id":"'//lane_id// &
                '","generation":"'//generation// &
                '","case_id":"test_page_'//trim(exit_code_text)// &
                '","outcome":"pass","status":"PASS"}'
            if (status /= 0) exit
        end do
        close(unit, iostat=status)
        call assert_true(status == 0, 'writes pagination receipts into durable journal')
    end subroutine append_page_receipts

    subroutine assert_paged_events(lane_id, owner, over_mcp)
        character(len=*), intent(in) :: lane_id, owner
        logical, intent(in) :: over_mcp
        type(json_value_t) :: document, page_events, field
        character(len=256) :: completion_ids(seeded_count + 3)
        character(:), allocatable :: completion_id
        logical :: seen(seeded_count), has_more
        integer :: position, previous, next_cursor, i, j, total, page_count
        integer :: marker, seeded_id
        integer :: read_status

        position = 0
        previous = -1
        total = 0
        page_count = 0
        seen = .false.
        do while (total < seeded_count + 3)
            call query_events(lane_id, owner, document, over_mcp, position)
            page_events = json_member(document, 'events')
            call assert_true(json_size(page_events) <= 128, &
                'event transport respects the record limit')
            do i = 1, json_size(page_events)
                completion_id = event_completion(page_events, i)
                do j = 1, page_count
                    call assert_true(trim(completion_ids(j)) /= completion_id, &
                        'event pagination never repeats a durable receipt')
                end do
                if (page_count < size(completion_ids)) then
                    page_count = page_count + 1
                    completion_ids(page_count) = completion_id
                else
                    call assert_true(.false., 'receipt stream fits bounded test oracle')
                end if
                marker = index(completion_id, 'native-page-')
                if (marker == 1 .and. len(completion_id) >= 16) then
                    read(completion_id(13:16), *, iostat=read_status) seeded_id
                    if (read_status == 0) then
                        if (seeded_id >= 1 .and. seeded_id <= seeded_count) then
                            call assert_true(.not. seen(seeded_id), &
                                'seeded page receipt appears exactly once')
                            seen(seeded_id) = .true.
                        else
                            call assert_true(.false., &
                                'seeded receipt ID is in expected range')
                        end if
                    else
                        call assert_true(.false., &
                            'seeded receipt ID has numeric suffix')
                    end if
                end if
            end do
            total = total + json_size(page_events)
            field = json_member(document, 'has_more')
            has_more = json_boolean_value(field)
            if (.not. has_more) exit
            field = json_member(document, 'next_cursor')
            next_cursor = int(json_number_value(field))
            call assert_true(next_cursor > position .and. next_cursor > previous, &
                'event pagination advances a byte cursor')
            previous = position
            position = next_cursor
            if (json_size(page_events) == 0) exit
        end do
        call assert_equal_integer(total, seeded_count + 3, &
            'transport returns every original and seeded receipt exactly once')
        call assert_true(all(seen), &
            'pagination preserves every independently seeded receipt')
    end subroutine assert_paged_events

    function event_completion(collection, index_value) result(value)
        type(json_value_t), intent(in) :: collection
        integer, intent(in) :: index_value
        character(:), allocatable :: value
        type(json_value_t) :: item

        item = json_element(collection, index_value)
        value = json_string_value(json_member(item, 'completion_id'))
    end function event_completion

    function event_case(collection, index_value) result(value)
        type(json_value_t), intent(in) :: collection
        integer, intent(in) :: index_value
        character(:), allocatable :: value
        type(json_value_t) :: item

        item = json_element(collection, index_value)
        value = json_string_value(json_member(item, 'case_id'))
    end function event_case

    function event_status(collection, index_value) result(value)
        type(json_value_t), intent(in) :: collection
        integer, intent(in) :: index_value
        character(:), allocatable :: value
        type(json_value_t) :: item

        item = json_element(collection, index_value)
        value = json_string_value(json_member(item, 'status'))
    end function event_status

    subroutine extract_payload(envelope, document)
        type(json_value_t), intent(in) :: envelope
        type(json_value_t), intent(out) :: document
        type(json_value_t) :: result, content, first, error
        character(:), allocatable :: raw, parse_message
        logical :: valid

        error = json_member(envelope, 'error')
        call assert_true(error%kind == 0, 'MCP JSON-RPC response has no protocol error')
        result = json_member(envelope, 'result')
        call assert_true(result%kind /= 0, 'MCP JSON-RPC response carries a result')
        call assert_true(.not. response_is_error(envelope), 'MCP tool action succeeds')
        content = json_member(result, 'content')
        first = json_element(content, 1)
        raw = json_string_value(json_member(first, 'text'))
        call json_parse(raw, document, valid, parse_message)
        call assert_true(valid, 'independent parser reads MCP Gremlin result payload')
    end subroutine extract_payload

    logical function response_is_error(envelope) result(is_error)
        type(json_value_t), intent(in) :: envelope
        type(json_value_t) :: result

        result = json_member(envelope, 'result')
        is_error = json_boolean_value(json_member(result, 'isError'))
    end function response_is_error

end program test_mcp_gremlin
