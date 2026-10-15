program test_mcp_protocol_contract
    !! Every MCP reply is valid JSON by construction, JSON-RPC ids follow the
    !! 2.0 rules, and a tool option that is duplicated or has the wrong type is
    !! an error instead of silently reading as its default.
    use fo_test_harness, only: process_result_t, make_scratch, join_path, write_text
    use fo_test_harness, only: remove_tree, assert_true, assert_equal_string
    use fo_test_harness, only: assert_equal_integer, assert_contains, finish_assertions
    use fo_test_cli, only: resolve_driver
    use fo_test_json, only: json_value_t, json_member, json_number_value, json_element
    use fo_test_json, only: json_string_value, json_null
    use fo_test_mcp, only: mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(:), allocatable :: driver, scratch, project, input
    type(json_value_t), allocatable :: replies(:)
    type(process_result_t) :: result
    type(json_value_t) :: error, identifier

    call resolve_driver(driver)
    call make_scratch('fo-mcp-contract', scratch)
    project = join_path(scratch, 'project')
    call write_text(join_path(project, 'fpm.toml'), 'name = "contract"'//nl)

    input = mcp_request(1, 'initialize', '{"protocolVersion":"2025-11-25"}')//nl
    ! The action text reaches the error message and must be escaped.
    input = input//mcp_call(2, '{"action":"na\"me\\x\u0001"}')//nl
    input = input//'{"jsonrpc":"2.0","id":{"a":1},"method":"tools/list"}'//nl
    input = input//'{"jsonrpc":"2.0","id":[1],"method":"tools/list"}'//nl
    input = input//'{"jsonrpc":"2.0","id":true,"method":"tools/list"}'//nl
    input = input//mcp_call(3, '{"action":"graph","dot":"yes","dir":'// &
        mcp_quote(project)//'}')//nl
    input = input//mcp_call(4, '{"action":"clean","cache":true,"cache":false,"dir":'// &
        mcp_quote(project)//'}')//nl
    input = input//mcp_call(5, '{"action":"lint","fix":1,"dir":'//mcp_quote(project)//'}')//nl
    input = input//mcp_call(6, '{"action":"lint","deep":null,"dir":'// &
        mcp_quote(project)//'}')//nl
    input = input//mcp_call(7, '{"action":"install","prefix":5,"dir":'// &
        mcp_quote(project)//'}')//nl
    input = input//mcp_call(8, '{"action":"info","dir":'//mcp_quote(project)// &
        ',"dir":'//mcp_quote(project)//'}')//nl
    input = input//mcp_call(9, '{"action":"gremlin_status","dir":'//mcp_quote(project)// &
        ',"cursor":99999999999999999999}')//nl
    input = input//mcp_call(10, '{"action":"gremlin_status","dir":'//mcp_quote(project)// &
        ',"cursor":"7"}')//nl
    input = input//mcp_call(11, '{"action":"gremlin_status","dir":'//mcp_quote(project)// &
        ',"cursor":1.5}')//nl
    input = input//mcp_request(12, 'shutdown', '{}')//nl
    call mcp_exchange(driver, scratch, join_path(scratch, 'cache'), input, .false., &
        replies, result)
    call assert_equal_integer(size(replies), 15, 'every request is answered')
    if (size(replies) /= 15) then
        call remove_tree(scratch)
        call finish_assertions()
        stop
    end if

    error = json_member(replies(2), 'error')
    call assert_equal_integer(int(json_number_value(json_member(error, 'code'))), -32602, &
        'unknown action is an invalid-params error')
    call assert_equal_string(json_string_value(json_member(error, 'message')), &
        'unknown action: na"me\x'//achar(1), &
        'the error message carries the action text verbatim')
    call rejected_id(replies(3), 'object id')
    call rejected_id(replies(4), 'array id')
    call rejected_id(replies(5), 'boolean id')
    call invalid_params(replies(6), 'graph dot of the wrong type')
    call invalid_params(replies(7), 'duplicate clean cache option')
    call invalid_params(replies(8), 'lint fix of the wrong type')
    call invalid_params(replies(9), 'lint deep of the wrong type')
    call invalid_params(replies(10), 'install prefix of the wrong type')
    call invalid_params(replies(11), 'duplicate dir')
    call cursor_rejected(replies(12), 'oversized cursor')
    call cursor_rejected(replies(13), 'string cursor')
    call cursor_rejected(replies(14), 'fractional cursor')

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine rejected_id(reply, label)
        type(json_value_t), intent(in) :: reply
        character(len=*), intent(in) :: label

        call assert_equal_integer(int(json_number_value(json_member(json_member(reply, &
            'error'), 'code'))), -32600, label//' is an invalid request')
        identifier = json_member(reply, 'id')
        call assert_true(identifier%kind == json_null, label//' is answered with a null id')
    end subroutine rejected_id

    subroutine invalid_params(reply, label)
        type(json_value_t), intent(in) :: reply
        character(len=*), intent(in) :: label

        call assert_equal_integer(int(json_number_value(json_member(json_member(reply, &
            'error'), 'code'))), -32602, label//' is rejected')
    end subroutine invalid_params

    subroutine cursor_rejected(reply, label)
        type(json_value_t), intent(in) :: reply
        character(len=*), intent(in) :: label
        type(json_value_t) :: content
        character(:), allocatable :: text

        content = json_member(json_member(reply, 'result'), 'content')
        text = json_string_value(json_member(json_element(content, 1), 'text'))
        call assert_contains(text, 'error', label//' returns an error')
    end subroutine cursor_rejected

end program test_mcp_protocol_contract
