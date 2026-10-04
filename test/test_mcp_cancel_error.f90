program test_mcp_cancel_error
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, write_text, start_sentinel
    use fo_test_harness, only: process_alive, stop_sentinel
    use fo_test_harness, only: assert_true, assert_equal_integer, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start
    use fo_test_mcp_session, only: mcp_session_request, mcp_session_notify
    use fo_test_mcp_session, only: mcp_session_call, mcp_session_shutdown
    use fo_test_mcp, only: mcp_quote
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_string_value, json_number_value, json_boolean_value
    implicit none

    integer, parameter :: source_count = 60, comment_count = 400
    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: file_path, start_arguments, arguments
    character(len=32) :: number
    character(len=32) :: file_number
    type(mcp_session_t) :: server
    type(json_value_t) :: response, payload, status_payload, field
    integer :: i, j, exit_code, sentinel, run_id, error_code
    integer :: started_at, finished_at, clock_rate
    logical :: valid

    call gremlin_setup(driver, scratch, project, cache, state)
    call prepare_check_fixture()
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
    call extract_payload(response, payload)
    run_id = int(json_number_value(json_member(payload, 'run_id')))
    call assert_true(run_id > 0, 'asynchronous public check returns its owned run ID')
    if (run_id <= 0) then
        call stop_sentinel(sentinel)
        call mcp_session_shutdown(server, exit_code)
        call finish_assertions()
    end if

    call mcp_session_call(server, '{"action":"status"}', response)
    call extract_payload(response, status_payload)
    call assert_true(json_string_value(json_member(status_payload, 'state')) == &
        'running', 'owned asynchronous check is live before cancellation')
    field = json_member(status_payload, 'run_id')
    call assert_equal_integer(int(json_number_value(field)), run_id, &
        'status associates the active process with its returned owner ID')

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

    write(number, '(i0)') run_id
    arguments = '{"action":"cancel","run_id":'//trim(number)//'}'
    call mcp_session_call(server, arguments, response)
    call assert_equal_integer(rpc_error_code(response), -32602, &
        'duplicate cancellation rejects the now-stale run ID')
    call assert_true(process_alive(sentinel), &
        'cancelling owned work preserves an unrelated sentinel process')
    call stop_sentinel(sentinel)
    call mcp_session_shutdown(server, exit_code)
    call assert_equal_integer(exit_code, 0, &
        'MCP server exits cleanly after cancellation')
    call finish_assertions()

contains

    subroutine prepare_check_fixture()
        integer :: source_unit

        call write_text(project//'/fpm.toml', &
            'name = "mcp_cancel_native_probe"'//new_line('a'))
        call make_directory(project//'/src')
        do i = 1, source_count
            write(file_number, '(i3.3)') i
            file_path = project//'/src/cancel_load_'//trim(file_number)//'.f90'
            open(newunit=source_unit, file=file_path, status='replace', &
                action='write')
            write(source_unit, '(a)') 'module cancel_load_'//trim(file_number)
            write(source_unit, '(a)') 'implicit none'
            do j = 1, comment_count
                write(source_unit, '(a)') &
                    '! source scan workload for cancellation oracle'
            end do
            write(source_unit, '(a)') 'end module cancel_load_'//trim(file_number)
            close(source_unit)
        end do
    end subroutine prepare_check_fixture

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

end program test_mcp_cancel_error
