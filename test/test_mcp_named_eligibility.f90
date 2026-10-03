program test_mcp_named_eligibility
    use fo_test_harness, only: process_result_t, make_scratch, join_path
    use fo_test_harness, only: make_directory, write_text, read_text
    use fo_test_harness, only: remove_path, remove_tree, assert_true, assert_equal_string
    use fo_test_harness, only: assert_equal_integer, assert_file_exists, assert_file_absent
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_number_value
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    character(len=*), parameter :: pass_name = 'test_mcp_named_pass'
    character(len=*), parameter :: slow_name = 'test_mcp_named_slow'
    character(len=*), parameter :: module_name = 'test_mcp_named_module_only'
    character(:), allocatable :: driver, scratch, project, cache, receipt_path
    character(:), allocatable :: source, text, name, error_message
    type(process_result_t) :: process
    type(json_value_t), allocatable :: responses(:)
    type(json_value_t) :: result_object, content, first, field, report, tests, entry, summary
    logical :: valid

    call resolve_driver(driver)
    call make_scratch('fo-mcp-named-eligibility', scratch)
    project = join_path(scratch, 'project')
    cache = join_path(scratch, 'cache')
    call make_directory(join_path(project, 'test'))
    call write_text(join_path(project, 'fpm.toml'), 'name = "mcp_named_probe"' // new_line('a'))
    call write_test(pass_name)
    call write_test(slow_name)
    source = 'module mcp_named_module_support' // new_line('a') // &
        'implicit none' // new_line('a') // 'end module mcp_named_module_support' // new_line('a')
    call write_text(join_path(project, 'test/' // module_name // '.f90'), source)

    receipt_path = join_path(project, pass_name // '.receipt')
    call exchange_named(pass_name, responses, process)
    call assert_equal_integer(process%exit_code, 0, 'successful named MCP server exits cleanly')
    result_object = json_member(responses(2), 'result')
    field = json_member(result_object, 'isError')
    call assert_true(.not. field%boolean, 'eligible named test succeeds')
    content = json_member(result_object, 'content')
    first = json_element(content, 1)
    field = json_member(first, 'text')
    call json_parse(json_string_value(field), report, valid, error_message)
    call assert_true(valid, 'named MCP report parses with independent JSON parser')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), 1, 'report contains only requested named test')
    entry = json_element(tests, 1)
    field = json_member(entry, 'name')
    call assert_equal_string(json_string_value(field), pass_name, 'report names requested test')
    summary = json_member(report, 'summary')
    field = json_member(summary, 'passed')
    call assert_equal_integer(int(json_number_value(field)), 1, 'one selected case passed')
    field = json_member(summary, 'failed')
    call assert_equal_integer(int(json_number_value(field)), 0, 'selected case has no failures')
    call assert_file_exists(receipt_path, 'positive case leaves independent execution receipt')
    call assert_equal_string(read_text(receipt_path), 'executed' // new_line('a'), &
        'selected case performed its file side effect')

    call expect_rejected('test_mcp_named_typo', &
        'fo: unknown test: test_mcp_named_typo')
    call expect_rejected(module_name, 'fo: unknown test: ' // module_name)
    call expect_rejected(slow_name, &
        'fo: slow test test_mcp_named_slow requires --all')

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'mcp-named-eligibility: positive execution and three rejection cases passed'

contains

    subroutine write_test(test_name)
        character(len=*), intent(in) :: test_name
        character(:), allocatable :: test_source, test_receipt

        test_receipt = join_path(project, test_name // '.receipt')
        test_source = 'program ' // test_name // new_line('a') // 'implicit none' // &
            new_line('a') // 'integer :: unit' // new_line('a') // &
            "open(newunit=unit, file='" // test_receipt // &
            "', status='replace', action='write')" // new_line('a') // &
            "write(unit, '(a)') 'executed'" // new_line('a') // 'close(unit)' // &
            new_line('a') // 'end program' // new_line('a')
        call write_text(join_path(project, 'test/' // test_name // '.f90'), test_source)
    end subroutine write_test

    subroutine exchange_named(test_name, output, child)
        character(len=*), intent(in) :: test_name
        type(json_value_t), allocatable, intent(out) :: output(:)
        type(process_result_t), intent(out) :: child
        character(:), allocatable :: messages

        messages = mcp_encode(mcp_request(1, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}'), .false.)
        messages = messages // mcp_encode(mcp_call(2, &
            '{"action":"test","dir":' // mcp_quote(project) // ',"args":[' // &
            mcp_quote(test_name) // '],"json":"full"}'), .false.)
        messages = messages // mcp_encode(mcp_request(3, 'shutdown'), .false.)
        call mcp_exchange(driver, project, cache, messages, .false., output, child)
    end subroutine exchange_named

    subroutine expect_rejected(test_name, expected)
        character(len=*), intent(in) :: test_name, expected
        type(json_value_t), allocatable :: output(:)
        type(process_result_t) :: child

        call remove_path(join_path(project, test_name // '.receipt'))
        call exchange_named(test_name, output, child)
        call assert_equal_integer(child%exit_code, 0, 'rejected named request exits cleanly')
        result_object = json_member(output(2), 'result')
        field = json_member(result_object, 'isError')
        call assert_true(field%boolean, test_name // ' is rejected')
        content = json_member(result_object, 'content')
        first = json_element(content, 1)
        field = json_member(first, 'text')
        call assert_equal_string(json_string_value(field), expected, &
            test_name // ' receives exact eligibility diagnostic')
        call assert_file_absent(join_path(project, test_name // '.receipt'), &
            test_name // ' did not execute')
    end subroutine expect_rejected

end program test_mcp_named_eligibility
