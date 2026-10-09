program test_mcp_request_structure
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory, write_text
    use fo_test_harness, only: remove_tree, assert_true, assert_equal_string
    use fo_test_harness, only: assert_equal_integer, assert_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_string_value, json_boolean_value, json_number_value
    use fo_test_json, only: json_string, json_number, json_null
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, input
    type(process_result_t) :: process
    type(json_value_t), allocatable :: responses(:)
    type(json_value_t) :: payload, result_object, field, content, first, tools, tool
    type(json_value_t) :: schema, properties, rpc_id, error_object
    type(string_list_t) :: arguments

    call resolve_driver(driver)
    call make_scratch('fo-mcp-request-structure', scratch)
    project = join_path(scratch, 'project')
    cache = join_path(scratch, 'cache')
    call make_directory(project)
    call write_text(join_path(project, 'fpm.toml'), 'name = "mcp_request_probe"' // new_line('a'))

    input = ''
    call append('{"jsonrpc":"2.0","id":1,"params":{"name":"fo",' // &
        '"arguments ":{"action":"gremlin_stop"},"_meta":{"method":"tools/list"},' // &
        '"arguments":{"action":"gremlin_status","dir":' // mcp_quote(project) // &
        ',"lane_id":"safe"}},"method ":"tools/list","method":"tools/call"}')
    call append(mcp_request(2, 'tools/call', &
        '{"name":"fo","_meta":{"action":"gremlin_stop","arguments":' // &
        '{"action":"gremlin_stop","session_id":"metadata-session"}},' // &
        '"arguments":{"action":"gremlin_status","dir":' // mcp_quote(project) // &
        ',"lane_id":"safe"}}'))
    call append(mcp_request(3, 'tools/list'))
    call append(mcp_call(4, '{"action":"gremlin_status","dir":' // mcp_quote(project) // &
        ',"lane_id":"safe","odd":{"deep":[true,{"text":"x,}["}]}}'))
    call append(mcp_call(41, '{"action":"gremlin_status","dir":' // mcp_quote(project) // &
        ',"lane_id ":"safe"}'))
    call append(mcp_call(42, '{"action":"gremlin_status ","dir":' // mcp_quote(project) // &
        ',"lane_id":"safe"}'))
    call append(mcp_call(5, '{"action":"gremlin_start","dir":' // mcp_quote(project) // &
        ',"lane_id":"safe","background":false}'))
    call append(mcp_call(43, '{"action":"graph","dir":' // mcp_quote(project) // &
        ',"dot":true}'))
    call append(mcp_call(44, '{"action":"graph","dir":' // mcp_quote(project) // &
        ',"dot":"true"}'))
    call append('{"jsonrpc":"2.0","id":"trace\u002d\"quoted",' // &
        '"method":"unsupported/method"}')
    call append('{"jsonrpc":"2.0","id":9007199254740993,' // &
        '"method":"unsupported/method"}')
    call append('{"jsonrpc":"2.0","id":9,"method":"tools/list"}{}')
    call append(mcp_request(6, 'shutdown'))
    call mcp_exchange(driver, project, cache, input, .false., responses, process)
    call assert_equal_integer(process%exit_code, 0, 'MCP server exits cleanly')

    payload = tool_payload(responses(1))
    field = json_member(payload, 'action')
    call assert_equal_string(json_string_value(field), 'status', &
        'top-level method and params.arguments control dispatch')
    payload = tool_payload(responses(2))
    field = json_member(payload, 'action')
    call assert_equal_string(json_string_value(field), 'status', &
        '_meta fields cannot replace params.arguments')

    result_object = json_member(responses(3), 'result')
    tools = json_member(result_object, 'tools')
    tool = json_element(tools, 1)
    schema = json_member(tool, 'inputSchema')
    properties = json_member(schema, 'properties')
    field = json_member(properties, 'background')
    call assert_true(field%kind == 0, 'background is not advertised while unsupported')
    field = json_member(properties, 'dot')
    content = json_member(field, 'type')
    call assert_equal_string(json_string_value(content), 'boolean', &
        'Graphviz DOT output is advertised as a boolean option')

    payload = tool_payload(responses(4), .true.)
    field = json_member(payload, 'error')
    call assert_contains(json_string_value(field), 'unsupported Gremlin request field', &
        'nested composite argument reaches strict core validation')
    payload = tool_payload(responses(5), .true.)
    field = json_member(payload, 'error')
    call assert_contains(json_string_value(field), 'field', &
        'trailing-space unknown field remains visible')
    payload = tool_payload(responses(6), .true.)
    field = json_member(payload, 'error')
    call assert_contains(json_string_value(field), 'exact public name', &
        'padded action preserves exact name validation')
    payload = tool_payload(responses(7), .true.)
    field = json_member(payload, 'error')
    call assert_contains(json_string_value(field), 'background', &
        'unsupported background request is rejected')

    result_object = json_member(responses(8), 'result')
    content = json_member(result_object, 'content')
    first = json_element(content, 1)
    field = json_member(first, 'text')
    call assert_true(index(json_string_value(field), 'digraph {') == 1, &
        'boolean dot option selects Graphviz output')
    result_object = json_member(responses(9), 'result')
    content = json_member(result_object, 'content')
    first = json_element(content, 1)
    field = json_member(first, 'text')
    call assert_true(index(json_string_value(field), 'digraph {') == 0, &
        'string dot option does not select Graphviz output')

    rpc_id = json_member(responses(10), 'id')
    call assert_equal_string(json_string_value(rpc_id), 'trace-"quoted', &
        'legal string RPC id decodes through the independent test parser')
    call assert_true(index(process%stdout, '"id":"trace\u002d\"quoted"') > 0, &
        'string RPC id preserves its original escape spelling')
    rpc_id = json_member(responses(11), 'id')
    call assert_equal_string(rpc_id%text, '9007199254740993', &
        'large legal integer RPC id preserves its exact digits')

    error_object = json_member(responses(12), 'error')
    field = json_member(error_object, 'code')
    call assert_equal_integer(int(json_number_value(field)), -32600, &
        'trailing JSON after the RPC envelope is rejected')
    call test_envelope_validation()

    arguments = string_list_t()
    call list_add(arguments, 'gremlin')
    call list_add(arguments, 'start')
    call list_add(arguments, '--dir')
    call list_add(arguments, project)
    call list_add(arguments, '--lane')
    call list_add(arguments, 'safe')
    call list_add(arguments, '--background')
    call run_fo(driver, arguments, project, cache, process)
    call assert_true(process%exit_code /= 0, 'CLI rejects unsupported background flag')

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'mcp-request-structure: envelope, composite values and strict fields passed'

contains

    subroutine test_envelope_validation()
        character(len=80), parameter :: invalid_fields(9) = [character(len=80) :: &
            '"jsonrpc":"2.0","id":{}', '"jsonrpc":"2.0","id":[]', &
            '"jsonrpc":"2.0","id":true', '"id":1', &
            '"jsonrpc":"1.0","id":1', '"jsonrpc":"2.0 ","id":1', &
            '"jsonrpc":2,"id":1', '"jsonrpc":null,"id":1', &
            '"jsonrpc":"2.0","jsonrpc":"1.0","id":1']
        type(json_value_t), allocatable :: replies(:)
        type(json_value_t) :: reply_id, reply_error, reply_field
        type(process_result_t) :: exchange
        character(:), allocatable :: messages
        integer :: framing_case, index, first_valid
        logical :: framed

        do framing_case = 1, 2
            framed = framing_case == 2
            messages = mcp_encode('{"jsonrpc":"2.0","method":"tools/list"}', framed)
            do index = 1, size(invalid_fields)
                messages = messages // &
                    mcp_encode('{' // trim(invalid_fields(index)) // &
                    ',"method":"tools/list"}', framed)
            end do
            messages = messages // mcp_encode('{"jsonrpc":"2.0","id":null,' // &
                '"method":"unsupported/method"}', framed) // &
                mcp_encode('{"jsonrpc":"\u0032.0","id":"trace\u002d\"quoted",' // &
                '"method":"unsupported/method"}', framed) // &
                mcp_encode('{"jsonrpc":"2.0","id":9007199254740993,' // &
                '"method":"unsupported/method"}', framed) // &
                mcp_encode('{"jsonrpc":"2.0","id":1.25e+2,' // &
                '"method":"unsupported/method"}', framed) // &
                mcp_encode('{"jsonrpc":"2.0","method":"unsupported/method"}', &
                framed) // &
                mcp_encode(mcp_request(7, 'shutdown'), framed)
            call mcp_exchange(driver, project, cache, messages, framed, replies, exchange)
            call assert_equal_integer(exchange%exit_code, 0, &
                'invalid envelopes preserve a usable server in both framing modes')
            call assert_equal_integer(size(replies), size(invalid_fields) + 5, &
                'known and unknown lawful notifications emit no response')
            if (size(replies) /= size(invalid_fields) + 5) cycle
            do index = 1, size(invalid_fields)
                reply_error = json_member(replies(index), 'error')
                reply_field = json_member(reply_error, 'code')
                call assert_equal_integer(int(json_number_value(reply_field)), -32600, &
                    'invalid envelope is rejected before method dispatch: ' // &
                    trim(invalid_fields(index)))
                reply_id = json_member(replies(index), 'id')
                call assert_equal_integer(reply_id%kind, json_null, &
                    'invalid envelope returns a null response ID')
            end do
            first_valid = size(invalid_fields) + 1
            do index = first_valid, first_valid + 3
                reply_error = json_member(replies(index), 'error')
                reply_field = json_member(reply_error, 'code')
                call assert_equal_integer(int(json_number_value(reply_field)), -32601, &
                    'lawful IDs reach method dispatch after invalid envelopes')
            end do
            reply_id = json_member(replies(first_valid), 'id')
            call assert_equal_integer(reply_id%kind, json_null, &
                'explicit null ID remains a request with a response')
            reply_id = json_member(replies(first_valid + 1), 'id')
            call assert_equal_integer(reply_id%kind, json_string, 'string ID remains a string')
            call assert_equal_string(json_string_value(reply_id), 'trace-"quoted', &
                'escaped string ID and escaped protocol version decode losslessly')
            reply_id = json_member(replies(first_valid + 2), 'id')
            call assert_equal_integer(reply_id%kind, json_number, 'integer ID stays numeric')
            call assert_equal_string(reply_id%text, '9007199254740993', &
                'large integer ID preserves digits beyond exact floating-point range')
            reply_id = json_member(replies(first_valid + 3), 'id')
            call assert_equal_integer(reply_id%kind, json_number, 'real ID stays numeric')
            call assert_equal_string(reply_id%text, '1.25e+2', &
                'real ID preserves its exact legal numeric token')
        end do
    end subroutine test_envelope_validation

    subroutine append(message)
        character(len=*), intent(in) :: message

        input = input // mcp_encode(message, .false.)
    end subroutine append

    function tool_payload(response, expect_error) result(document)
        type(json_value_t), intent(in) :: response
        logical, intent(in), optional :: expect_error
        type(json_value_t) :: document, response_result, item, text_field, error_flag
        character(:), allocatable :: text
        character(:), allocatable :: error_message
        logical :: parsed

        response_result = json_member(response, 'result')
        call assert_true(response_result%kind /= 0, 'tool result has protocol result')
        error_flag = json_member(response_result, 'isError')
        if (present(expect_error)) then
            call assert_true(json_boolean_value(error_flag) .eqv. expect_error, &
                'tool error state matches response id ' // response_id(response))
        end if
        item = json_element(json_member(response_result, 'content'), 1)
        text_field = json_member(item, 'text')
        text = json_string_value(text_field)
        call json_parse(text, document, parsed, error_message)
        call assert_true(parsed, 'tool text parses with test-only JSON parser')
    end function tool_payload

    function response_id(response) result(text)
        type(json_value_t), intent(in) :: response
        character(:), allocatable :: text
        character(len=32) :: buffer
        type(json_value_t) :: id_field

        id_field = json_member(response, 'id')
        write(buffer, '(i0)') int(json_number_value(id_field))
        text = trim(buffer)
    end function response_id

end program test_mcp_request_structure
