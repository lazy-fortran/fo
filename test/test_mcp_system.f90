program test_mcp_system
    use fo_test_harness, only: process_result_t, make_scratch, join_path
    use fo_test_harness, only: make_directory, write_text, remove_tree, current_directory
    use fo_test_harness, only: assert_true, assert_equal_string, assert_equal_integer
    use fo_test_harness, only: assert_contains, assert_file_exists, assert_file_absent
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver
    use fo_test_json, only: json_value_t, json_object, json_array, json_number, json_null
    use fo_test_json, only: json_parse, json_member, json_element, json_size, json_string_value
    use fo_test_json, only: json_number_value, json_boolean_value
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    character(:), allocatable :: driver, scratch, cache, input, project, marker, repo_dir
    character(:), allocatable :: text
    type(process_result_t) :: result
    type(json_value_t), allocatable :: replies(:)
    type(json_value_t) :: field, result_object, content, tools, tool, schema, properties
    type(json_value_t) :: actions, required, resources
    integer :: i
    logical :: framed

    call resolve_driver(driver)
    call make_scratch('fo-mcp-system', scratch)
    call current_directory(repo_dir)
    call write_text(join_path(scratch, 'fpm.toml'), &
        'name = "mcp_system_check_fixture"' // new_line('a'))
    call make_directory(join_path(scratch, 'test'))
    call write_text(join_path(scratch, 'test/test_smoke.f90'), &
        'program test_smoke' // new_line('a') // 'implicit none' // new_line('a') // &
        "print '(a)', 'mcp check fixture passed'" // new_line('a') // &
        'end program test_smoke' // new_line('a'))
    do i = 1, 2
        framed = i /= 2
        input = ''
        call send(mcp_request(1, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":' // &
            '{"name":"test","version":"1.0"}}'))
        call send('{"jsonrpc":"2.0","method":"initialized"}')
        call send(mcp_request(99, 'tools/list', '{}'))
        call send(mcp_request(2, 'tools/list', '{}'))
        call send(mcp_request(3, 'resources/list', '{}'))
        call send(mcp_call(4, '{"action":"info"}'))
        call send(mcp_call(8, '{"action":"lint"}'))
        call send(mcp_call(9, '{"action":"check","dir":' // mcp_quote(scratch) // '}'))
        call send(mcp_request(5, 'bogus/method', '{}'))
        call send(mcp_call(6, '{"action":"nonexistent"}'))
        call send(mcp_call(10, '{}'))
        call send(mcp_request(7, 'shutdown', '{}'))
        call mcp_exchange(driver, repo_dir, join_path(scratch, 'main-cache'), input, &
            framed, replies, result)
        call assert_equal_integer(result%exit_code, 0, 'system MCP server exits cleanly')
        call assert_equal_integer(result%term_signal, 0, 'system MCP server was not signalled')
        call assert_true(len(result%stderr) == 0, 'system MCP server has no stderr output')

        call assert_equal_integer(size(replies), 11, 'system exchange response count')
        field = json_member(replies(1), 'jsonrpc')
        call assert_equal_string(json_string_value(field), '2.0', 'JSON-RPC version')
        field = json_member(replies(1), 'id')
        call assert_equal_integer(int(json_number_value(field)), 1, 'initialize request id')
        result_object = json_member(replies(1), 'result')
        field = json_member(result_object, 'protocolVersion')
        call assert_equal_string(json_string_value(field), '2025-11-25', &
            'protocol version negotiation')
        field = json_member(result_object, 'serverInfo')
        schema = json_member(field, 'name')
        call assert_equal_string(json_string_value(schema), 'fo', 'MCP server name')
        schema = json_member(field, 'version')
        call assert_equal_string(json_string_value(schema), '0.3.2', 'MCP server version')
        field = json_member(result_object, 'capabilities')
        schema = json_member(field, 'tools')
        call assert_true(schema%kind == json_object, &
            'initialize advertises tools')
        schema = json_member(field, 'resources')
        call assert_true(schema%kind == json_object, &
            'initialize advertises resources')

        result_object = json_member(replies(2), 'result')
        field = json_member(replies(2), 'id')
        call assert_equal_integer(int(json_number_value(field)), 99, &
            'server remains alive after initialized notification')
        result_object = json_member(replies(3), 'result')
        tools = json_member(result_object, 'tools')
        call assert_equal_integer(tools%kind, json_array, 'tools/list returns array')
        tool = find_named(tools, 'fo')
        call assert_true(tool%kind == json_object, 'fo tool is discovered')
        schema = json_member(tool, 'inputSchema')
        properties = json_member(schema, 'properties')
        field = json_member(properties, 'action')
        actions = json_member(field, 'enum')
        call assert_true(array_has(actions, 'check'), 'tool schema includes check')
        call assert_true(array_has(actions, 'info'), 'tool schema includes info')
        call assert_true(array_has(actions, 'lint'), 'tool schema includes lint')
        required = json_member(schema, 'required')
        call assert_true(array_has(required, 'action'), 'action is required')

        result_object = json_member(replies(4), 'result')
        resources = json_member(result_object, 'resources')
        call assert_equal_integer(resources%kind, json_array, 'resources/list returns array')
        call assert_true(resource_exists(resources, 'fo://diagnostics'), &
            'diagnostics resource is discovered')
        call check_tool_response(replies(5), text, .false.)
        call assert_contains(text, 'backend:', 'info returns backend information')
        call check_tool_response(replies(6), text)
        call assert_contains(text, 'unused_imports', 'lint returns JSON diagnostics')
        call check_tool_response(replies(7), text)
        call assert_agent_json(text)
        call assert_equal_integer(json_error_code(replies(8)), -32601, &
            'unknown method returns method-not-found error')
        call assert_true(json_has_member(replies(9), 'error'), &
            'unknown action returns JSON-RPC error')
        call assert_true(json_has_member(replies(10), 'error'), &
            'missing action returns malformed-request error')
        result_object = json_member(replies(11), 'result')
        call assert_equal_integer(result_object%kind, json_null, 'shutdown returns null')
    end do

    project = join_path(scratch, 'clean-project')
    cache = join_path(scratch, 'clean-cache')
    marker = join_path(cache, 'store/v2/marker')
    framed = .true.
    call make_directory(join_path(project, 'build'))
    call make_directory(join_path(cache, 'store/v2'))
    call write_text(join_path(project, 'fpm.toml'), 'name = "mcp_clean_fixture"' // new_line('a'))
    call write_text(marker, 'keep')
    input = ''
    call send(mcp_request(101, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{}}'))
    call send(mcp_call(102, '{"action":"clean"}'))
    call send(mcp_request(103, 'shutdown', '{}'))
    call mcp_exchange(driver, project, cache, input, .true., replies, result)
    call assert_equal_integer(result%exit_code, 0, 'clean MCP server exits cleanly')
    call check_tool_response(replies(2), text, .false.)
    call assert_file_absent(join_path(project, 'build'), 'default clean removes project build tree')
    call assert_file_exists(marker, 'default clean preserves shared cache')
    call assert_contains(text, 'cache kept', 'default clean reports preserved cache')

    call make_directory(join_path(project, 'build'))
    input = ''
    call send(mcp_request(104, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{}}'))
    call send(mcp_call(105, '{"action":"clean","cache":true}'))
    call send(mcp_request(106, 'shutdown', '{}'))
    call mcp_exchange(driver, project, cache, input, .true., replies, result)
    call assert_equal_integer(result%exit_code, 0, 'purge MCP server exits cleanly')
    call check_tool_response(replies(2), text, .false.)
    call assert_file_absent(marker, 'cache purge removes shared cache marker')
    call assert_contains(text, 'cache cleared', 'cache purge reports cleared cache')

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'mcp-system: framing, negotiation, discovery, calls, errors, clean and shutdown passed'

contains

    subroutine send(message)
        character(len=*), intent(in) :: message

        input = input // mcp_encode(message, framed)
    end subroutine send

    function find_named(array, name) result(found)
        type(json_value_t), intent(in) :: array
        character(len=*), intent(in) :: name
        type(json_value_t) :: found, item, field
        integer :: j

        do j = 1, json_size(array)
            item = json_element(array, j)
            field = json_member(item, 'name')
            if (json_string_value(field) == name) then
                found = item
                return
            end if
        end do
    end function find_named

    logical function array_has(array, value)
        type(json_value_t), intent(in) :: array
        character(len=*), intent(in) :: value
        type(json_value_t) :: item
        integer :: j

        array_has = .false.
        do j = 1, json_size(array)
            item = json_element(array, j)
            if (json_string_value(item) == value) then
                array_has = .true.
                return
            end if
        end do
    end function array_has

    logical function resource_exists(array, uri)
        type(json_value_t), intent(in) :: array
        character(len=*), intent(in) :: uri
        type(json_value_t) :: item, field
        integer :: j

        resource_exists = .false.
        do j = 1, json_size(array)
            item = json_element(array, j)
            field = json_member(item, 'uri')
            if (json_string_value(field) == uri) then
                resource_exists = .true.
                return
            end if
        end do
    end function resource_exists

    subroutine check_tool_response(response, body, is_error)
        type(json_value_t), intent(in) :: response
        character(:), allocatable, intent(out) :: body
        logical, intent(in), optional :: is_error
        type(json_value_t) :: result_value, content_value, first, field

        result_value = json_member(response, 'result')
        call assert_true(result_value%kind == json_object, 'tool response has result')
        if (present(is_error)) then
            field = json_member(result_value, 'isError')
            call assert_true(json_boolean_value(field) .eqv. is_error, 'tool response error flag')
        end if
        content_value = json_member(result_value, 'content')
        first = json_element(content_value, 1)
        field = json_member(first, 'text')
        body = json_string_value(field)
    end subroutine check_tool_response

    subroutine assert_agent_json(body)
        character(len=*), intent(in) :: body
        type(json_value_t) :: document, field
        logical :: valid
        character(:), allocatable :: error

        call json_parse(body, document, valid, error)
        call assert_true(valid, 'check result parses as JSON')
        call assert_member(document, 'ok')
        call assert_member(document, 'stage')
        call assert_member(document, 'target')
        call assert_member(document, 'summary')
        call assert_member(document, 'hint')
        call assert_member(document, 'rerun')
        call assert_member(document, 'log_path')
        field = json_member(document, 'elapsed_s')
        call assert_equal_integer(field%kind, json_number, 'check JSON has numeric elapsed_s')
    end subroutine assert_agent_json

    subroutine assert_member(document, name)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: name
        type(json_value_t) :: field

        field = json_member(document, name)
        call assert_true(field%kind /= 0, 'check JSON has ' // name)
    end subroutine assert_member

    integer function json_error_code(response) result(code)
        type(json_value_t), intent(in) :: response
        type(json_value_t) :: field

        field = json_member(json_member(response, 'error'), 'code')
        code = int(json_number_value(field))
    end function json_error_code

    logical function json_has_member(document, name)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: name
        type(json_value_t) :: field

        field = json_member(document, name)
        json_has_member = field%kind /= 0
    end function json_has_member

end program test_mcp_system
