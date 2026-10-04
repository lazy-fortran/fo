program test_mcp_response_ownership
    use fo_test_harness, only: process_result_t, make_scratch, join_path, write_text
    use fo_test_harness, only: remove_tree, assert_true, assert_equal_integer
    use fo_test_harness, only: assert_equal_string, finish_assertions
    use fo_test_cli, only: resolve_driver
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element, json_size
    use fo_test_json, only: json_number_value, json_string_value
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_exchange
    implicit none

    integer, parameter :: list_calls = 128
    character(:), allocatable :: driver, scratch, cache, input, error_message
    type(process_result_t) :: process
    type(json_value_t), allocatable :: responses(:), retained(:)
    type(json_value_t) :: field, tools, first_tool, partial, second_parse
    logical :: framed, valid
    integer :: i

    call resolve_driver(driver)
    call make_scratch('fo-mcp-response-ownership', scratch)
    cache = join_path(scratch, 'cache')
    call write_text(join_path(scratch, 'fpm.toml'), &
        'name = "mcp_response_ownership"' // new_line('a'))

    framed = .false.
    input = mcp_encode(mcp_request(1, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{}}'), framed)
    do i = 1, list_calls
        input = input // mcp_encode(mcp_request(i + 1, 'tools/list', '{}'), framed)
    end do
    input = input // mcp_encode(mcp_request(list_calls + 2, 'shutdown'), framed)
    call mcp_exchange(driver, scratch, cache, input, framed, responses, process)
    call assert_equal_integer(process%exit_code, 0, 'large response exchange exits cleanly')
    call assert_equal_integer(size(responses), list_calls + 2, &
        'all initialized, tool-list and shutdown responses are accumulated')

    allocate(retained(size(responses)))
    do i = 1, size(responses)
        retained(i) = responses(i)
    end do
    call assert_equal_integer(size(retained), size(responses), &
        'defined assignment retains a deep copy of accumulated MCP trees')
    do i = 2, list_calls + 1
        field = json_member(retained(i), 'id')
        call assert_equal_integer(int(json_number_value(field)), i, &
            'copied response retains its original request id')
        tools = json_member(json_member(retained(i), 'result'), 'tools')
        call assert_true(json_size(tools) > 0, 'copied response retains nested tool definitions')
        first_tool = json_element(tools, 1)
        field = json_member(first_tool, 'name')
        call assert_equal_string(json_string_value(field), 'fo', &
            'copied nested tool name remains valid')
    end do

    input = mcp_encode(mcp_request(201, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{}}'), framed)
    input = input // mcp_encode(mcp_request(202, 'shutdown'), framed)
    call mcp_exchange(driver, scratch, cache, input, framed, responses, process)
    call assert_equal_integer(size(responses), 2, 'response array can be reused for a smaller exchange')
    call assert_equal_integer(process%exit_code, 0, 'reused MCP exchange exits cleanly')
    field = json_member(retained(2), 'id')
    call assert_equal_integer(int(json_number_value(field)), 2, &
        'independent deep copy survives reuse and release of the source array')

    call json_parse('{"outer":[{"deep":[true,false,', partial, valid, error_message)
    call assert_true(.not. valid, 'malformed nested JSON is rejected')
    call json_parse('{"recovered":{"items":[1,2,3]}}', partial, valid, error_message)
    call assert_true(valid, 'partial parser tree can be reused after malformed input')
    second_parse = json_member(partial, 'recovered')
    tools = json_member(second_parse, 'items')
    call assert_equal_integer(json_size(tools), 3, 'reused parser returns complete nested value')

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'mcp-response-ownership: growth, deep copy, reuse and malformed cleanup passed'
end program test_mcp_response_ownership
