program test_mcp_stderr
    !! The MCP server owns stdout for the protocol; build and test progress
    !! must never reach its stderr, whatever the order of actions.
    use fo_test_harness, only: process_result_t, make_scratch, join_path
    use fo_test_harness, only: make_directory, write_text, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_integer, finish_assertions
    use fo_test_cli, only: resolve_driver
    use fo_test_json, only: json_value_t
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    character(:), allocatable :: driver, scratch, input, name
    type(process_result_t) :: result
    type(json_value_t), allocatable :: replies(:)
    integer :: i

    call resolve_driver(driver)
    call make_scratch('fo-mcp-stderr', scratch)
    call write_text(join_path(scratch, 'fpm.toml'), &
        'name = "mcp_stderr_fixture"' // new_line('a'))
    call make_directory(join_path(scratch, 'src'))
    call write_text(join_path(scratch, 'src/lib_a.f90'), &
        'module lib_a' // new_line('a') // 'end module lib_a' // new_line('a'))
    call make_directory(join_path(scratch, 'test'))
    do i = 1, 4
        name = 'test_case' // char(ichar('0') + i)
        call write_text(join_path(scratch, 'test/' // name // '.f90'), &
            'program ' // name // new_line('a') // 'implicit none' // new_line('a') // &
            'end program ' // name // new_line('a'))
    end do
    input = ''
    call send(mcp_request(1, 'initialize', &
        '{"protocolVersion":"2025-11-25","capabilities":{}}'))
    call send(mcp_call(2, '{"action":"build","dir":' // mcp_quote(scratch) // '}'))
    call send(mcp_call(3, '{"action":"check","dir":' // mcp_quote(scratch) // '}'))
    call send(mcp_call(7, '{"action":"clean","dir":' // mcp_quote(scratch) // '}'))
    call send(mcp_call(8, '{"action":"build","dir":' // mcp_quote(scratch) // '}'))
    call send(mcp_call(4, '{"action":"test","dir":' // mcp_quote(scratch) // '}'))
    call send(mcp_call(5, '{"action":"test","dir":' // mcp_quote(scratch) // '}'))
    call send(mcp_request(6, 'shutdown', '{}'))
    call mcp_exchange(driver, scratch, join_path(scratch, 'cache'), input, .true., &
        replies, result)
    call assert_equal_integer(result%exit_code, 0, 'MCP server exits cleanly')
    call assert_true(len(result%stderr) == 0, 'MCP build and test calls write no stderr: ' // &
        result%stderr(1:min(len(result%stderr), 200)))
    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine send(message)
        character(len=*), intent(in) :: message

        input = input // mcp_encode(message, .true.)
    end subroutine send

end program test_mcp_stderr
