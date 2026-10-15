program test_command_parity
    !! The CLI and the MCP server run one routine per command, so their results
    !! agree and large results are complete. Every command runs through both
    !! surfaces on the same project and the outputs are compared.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, make_directory, join_path, write_text
    use fo_test_harness, only: remove_tree, file_exists
    use fo_test_harness, only: assert_true, assert_equal_string, assert_equal_integer
    use fo_test_harness, only: assert_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_boolean_value
    use fo_test_mcp, only: mcp_request, mcp_call, mcp_quote, mcp_exchange
    use fo_fs, only: fs_realpath
    use fx_string, only: replace_str
    implicit none

    character(len=1), parameter :: nl = achar(10)
    integer, parameter :: n_imports = 260, n_chain = 300
    character(:), allocatable :: driver, scratch, cache, real_scratch
    character(:), allocatable :: many, chain, broken, app, noisy
    character(:), allocatable :: cli_text, mcp_text, mcp_err
    type(process_result_t) :: cli_result
    character(len=4096) :: resolved
    logical :: ok

    call resolve_driver(driver)
    call make_scratch('fo-command-parity', scratch)
    cache = join_path(scratch, 'cache')
    call fs_realpath(scratch, resolved, ok)
    real_scratch = scratch
    if (ok) real_scratch = trim(resolved)
    many = join_path(scratch, 'many')
    chain = join_path(scratch, 'chain')
    broken = join_path(scratch, 'broken')
    app = join_path(scratch, 'app')
    noisy = join_path(scratch, 'noisy')
    call write_many_imports(many)
    call write_chain(chain)
    call write_broken(broken)
    call write_app(app)
    call write_noisy(noisy)

    call test_info()
    call test_graph()
    call test_changed()
    call test_lint()
    call test_check()
    call test_install()
    call test_clean()
    call test_build_log()

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine write_many_imports(root)
        character(len=*), intent(in) :: root
        character(:), allocatable :: provider, consumer
        character(len=16) :: name
        integer :: i

        provider = 'module provider'//nl//'implicit none'//nl
        consumer = 'module consumer'//nl
        do i = 1, n_imports
            write (name, '(a,i3.3)') 'p', i
            provider = provider//'integer, parameter :: '//trim(name)//' = 1'//nl
            consumer = consumer//'use provider, only: '//trim(name)//nl
        end do
        provider = provider//'end module provider'//nl
        consumer = consumer//'implicit none'//nl//'private'//nl//'integer :: i'//nl// &
            'end module consumer'//nl
        call write_text(join_path(root, 'fpm.toml'), 'name = "parity_many"'//nl)
        call write_text(join_path(root, 'src/provider.f90'), provider)
        call write_text(join_path(root, 'src/consumer.f90'), consumer)
        call write_text(join_path(root, 'test/test_smoke.f90'), &
            'program test_smoke'//nl//'use consumer'//nl//'implicit none'//nl// &
            'error stop 1'//nl//'end program test_smoke'//nl)
    end subroutine write_many_imports

    subroutine write_chain(root)
        character(len=*), intent(in) :: root
        character(len=64) :: name, previous
        character(:), allocatable :: text
        integer :: i

        call write_text(join_path(root, 'fpm.toml'), 'name = "parity_chain"'//nl)
        do i = 1, n_chain
            write (name, '(a,i3.3)') 'graph_chain_module_with_a_long_name_', i
            text = 'module '//trim(name)//nl
            if (i > 1) then
                write (previous, '(a,i3.3)') 'graph_chain_module_with_a_long_name_', i - 1
                text = text//'use '//trim(previous)//nl
            end if
            text = text//'implicit none'//nl//'end module '//trim(name)//nl
            call write_text(join_path(root, 'src/'//trim(name)//'.f90'), text)
        end do
    end subroutine write_chain

    subroutine write_broken(root)
        character(len=*), intent(in) :: root

        call write_text(join_path(root, 'fpm.toml'), 'name = "parity_broken"'//nl)
        call write_text(join_path(root, 'app/main.f90'), &
            'program broken'//nl//'  value = identity{integer'//nl// &
            'end program broken'//nl)
    end subroutine write_broken

    subroutine write_app(root)
        character(len=*), intent(in) :: root

        call write_text(join_path(root, 'fpm.toml'), 'name = "parity_app"'//nl)
        call write_text(join_path(root, 'app/main.f90'), &
            'program parity_app'//nl//'print *, 1'//nl//'end program parity_app'//nl)
    end subroutine write_app

    subroutine write_noisy(root)
        !! A build whose compiler log is far larger than a fixed buffer.
        character(len=*), intent(in) :: root
        character(:), allocatable :: text
        character(len=8) :: digits
        integer :: i

        text = 'module noisy'//nl//'implicit none'//nl//'contains'//nl// &
            'subroutine s()'//nl//'integer :: i'//nl
        do i = 1, 400
            write (digits, '(i3.3)') i
            text = text//'i = undefined_variable_with_a_long_name_'//trim(digits)//nl
        end do
        text = text//'end subroutine s'//nl//'end module noisy'//nl
        call write_text(join_path(root, 'fpm.toml'), 'name = "parity_noisy"'//nl)
        call write_text(join_path(root, 'src/noisy.f90'), text)
    end subroutine write_noisy

    subroutine cli(project, first, second, third)
        character(len=*), intent(in) :: project, first
        character(len=*), intent(in), optional :: second, third
        type(string_list_t) :: arguments

        call list_add(arguments, first)
        if (present(second)) call list_add(arguments, second)
        if (present(third)) call list_add(arguments, third)
        call run_fo(driver, arguments, project, cache, cli_result)
        cli_text = scrub(cli_result%stdout)
    end subroutine cli

    subroutine mcp(arguments_json, text, err_text, is_error)
        character(len=*), intent(in) :: arguments_json
        character(:), allocatable, intent(out) :: text, err_text
        logical, intent(out) :: is_error
        type(json_value_t), allocatable :: replies(:)
        type(process_result_t) :: result
        type(json_value_t) :: content
        character(:), allocatable :: input

        input = mcp_request(1, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}')//nl
        input = input//mcp_call(2, arguments_json)//nl//mcp_request(3, 'shutdown', '{}')//nl
        call mcp_exchange(driver, scratch, cache, input, .false., replies, result)
        text = ''
        err_text = ''
        is_error = .true.
        if (size(replies) < 2) then
            call assert_true(.false., 'MCP answered the tool call')
            return
        end if
        content = json_member(json_member(replies(2), 'result'), 'content')
        if (json_size(content) >= 1) text = scrub(json_string_value( &
            json_member(json_element(content, 1), 'text')))
        if (json_size(content) >= 2) err_text = scrub(json_string_value( &
            json_member(json_element(content, 2), 'text')))
        is_error = json_boolean_value(json_member(json_member(replies(2), 'result'), &
            'isError'))
    end subroutine mcp

    function scrub(text) result(clean)
        !! Paths relative to the project, as the MCP server reports them.
        character(len=*), intent(in) :: text
        character(:), allocatable :: clean

        character(len=6), parameter :: names(5) = [character(len=6) :: &
            'many', 'chain', 'broken', 'app', 'noisy']
        integer :: i

        clean = text
        do i = 1, size(names)
            clean = replace_str(clean, real_scratch//'/'//trim(names(i))//'/', '')
            clean = replace_str(clean, scratch//'/'//trim(names(i))//'/', '')
        end do
        clean = replace_str(clean, nl//'./', nl)
        clean = replace_str(clean, ' ./', ' ')
        if (len(clean) > 1) then
            if (clean(1:2) == './') clean = clean(3:)
        end if
    end function scrub

    function dir_of(project) result(argument)
        character(len=*), intent(in) :: project
        character(:), allocatable :: argument

        argument = ',"dir":'//mcp_quote(project)
    end function dir_of

    subroutine same_result(label, is_error)
        character(len=*), intent(in) :: label
        logical, intent(in) :: is_error

        call assert_equal_string(mcp_text, cli_text, label//': MCP text equals CLI stdout')
        call assert_true((cli_result%exit_code /= 0) .eqv. is_error, &
            label//': MCP isError equals the CLI exit status')
    end subroutine same_result

    subroutine test_info()
        logical :: is_error

        call cli(many, 'info')
        call mcp('{"action":"info"'//dir_of(many)//'}', mcp_text, mcp_err, is_error)
        call same_result('info', is_error)
        call assert_contains(mcp_text, 'modules: 3'//nl, 'info ends with a complete line')
    end subroutine test_info

    subroutine test_graph()
        logical :: is_error

        call cli(chain, 'graph')
        call mcp('{"action":"graph"'//dir_of(chain)//'}', mcp_text, mcp_err, is_error)
        call same_result('graph', is_error)
        call assert_true(len(mcp_text) > 16384, 'graph output exceeds 16 KiB')
        call assert_contains(mcp_text, 'name_300 -> ', 'graph keeps its final node')
        call cli(chain, 'graph', '--dot')
        call mcp('{"action":"graph","dot":true'//dir_of(chain)//'}', mcp_text, mcp_err, &
            is_error)
        call same_result('graph dot', is_error)
        call assert_true(len(mcp_text) > 16384, 'DOT graph exceeds 16 KiB')
        call assert_contains(mcp_text, '}', 'DOT graph is complete')
    end subroutine test_graph

    subroutine test_changed()
        logical :: is_error

        call cli(many, 'changed')
        call mcp('{"action":"changed"'//dir_of(many)//'}', mcp_text, mcp_err, is_error)
        call same_result('changed', is_error)
        call assert_contains(mcp_text, 'affected tests (1):'//nl//'  test_smoke', &
            'changed lists affected tests from the project model')
        call cli(chain, 'changed')
        call mcp('{"action":"changed"'//dir_of(chain)//'}', mcp_text, mcp_err, is_error)
        call same_result('changed on a large project', is_error)
        call assert_true(len(mcp_text) > 16384, 'changed output exceeds 16 KiB')
        call assert_contains(mcp_text, 'name_300', 'changed keeps its final module')
    end subroutine test_changed

    subroutine test_lint()
        type(json_value_t) :: report
        character(:), allocatable :: error_message
        logical :: valid, is_error

        call cli(many, 'lint', '--json')
        call mcp('{"action":"lint"'//dir_of(many)//'}', mcp_text, mcp_err, is_error)
        call same_result('lint', is_error)
        call assert_true(len(mcp_text) > 16384, 'lint output exceeds 16 KiB')
        call json_parse(mcp_text, report, valid, error_message)
        call assert_true(valid, 'large MCP lint output is valid JSON')
        if (valid) call assert_equal_integer(json_size(json_member(report, &
            'unused_imports')), n_imports, 'MCP lint reports every finding')
        call cli(many, 'lint', '--json', '--deep')
        call mcp('{"action":"lint","deep":true'//dir_of(many)//'}', mcp_text, mcp_err, &
            is_error)
        call same_result('lint --deep', is_error)
        call assert_contains(mcp_text, '"deep_', 'MCP lint reaches the deep analysis')
    end subroutine test_lint

    subroutine test_check()
        type(json_value_t) :: cli_report, mcp_report
        character(:), allocatable :: error_message
        character(len=8), parameter :: fields(5) = [character(len=8) :: &
            'stage', 'target', 'summary', 'hint', 'rerun']
        logical :: valid, is_error
        integer :: i

        call cli(broken, 'check', '--json=compact')
        call mcp('{"action":"check","json":"compact"'//dir_of(broken)//'}', mcp_text, &
            mcp_err, is_error)
        call assert_true((cli_result%exit_code /= 0) .eqv. is_error, &
            'check isError equals the CLI exit status')
        call assert_equal_integer(cli_result%exit_code, 1, 'broken project fails check')
        call json_parse(cli_text, cli_report, valid, error_message)
        call assert_true(valid, 'CLI check JSON parses')
        call json_parse(mcp_text, mcp_report, valid, error_message)
        call assert_true(valid, 'MCP check JSON parses')
        do i = 1, size(fields)
            call assert_equal_string( &
                json_string_value(json_member(mcp_report, trim(fields(i)))), &
                json_string_value(json_member(cli_report, trim(fields(i)))), &
                'check field '//trim(fields(i))//' matches the CLI')
        end do
        call assert_contains(scrub(cli_result%stderr), 'fo: frontend error: app/main.f90:2', &
            'CLI check reports the frontend diagnostic')
        call assert_equal_string(mcp_err, extract_frontend(scrub(cli_result%stderr)), &
            'MCP check reports the same frontend diagnostics as the CLI')
    end subroutine test_check

    function extract_frontend(stderr) result(lines)
        !! The frontend diagnostic lines of the CLI stderr (progress lines dropped).
        character(len=*), intent(in) :: stderr
        character(:), allocatable :: lines
        integer :: start, stop_at, next

        lines = ''
        start = 1
        do while (start <= len(stderr))
            next = index(stderr(start:), nl)
            if (next == 0) then
                stop_at = len(stderr)
            else
                stop_at = start + next - 1
            end if
            if (index(stderr(start:stop_at), 'fo: frontend ') == 1) &
                lines = lines//stderr(start:stop_at)
            start = stop_at + 1
        end do
    end function extract_frontend

    subroutine test_install()
        character(:), allocatable :: cli_prefix, mcp_prefix
        logical :: is_error

        call cli(many, 'install', '--prefix', join_path(scratch, 'prefix-none'))
        call assert_equal_integer(cli_result%exit_code, 1, 'install without executables fails')
        call mcp('{"action":"install","prefix":'//mcp_quote(join_path(scratch, &
            'prefix-none'))//dir_of(many)//'}', mcp_text, mcp_err, is_error)
        call assert_true(is_error, 'MCP install without executables is an error')
        call assert_equal_string(mcp_text, scrub(cli_result%stderr), &
            'install failure text equals the CLI stderr')

        cli_prefix = join_path(scratch, 'prefix-cli')
        mcp_prefix = join_path(scratch, 'prefix-mcp')
        call cli(app, 'install', '--prefix', cli_prefix)
        call assert_equal_integer(cli_result%exit_code, 0, 'CLI install succeeds')
        call mcp('{"action":"install","prefix":'//mcp_quote(mcp_prefix)//dir_of(app)//'}', &
            mcp_text, mcp_err, is_error)
        call assert_true(.not. is_error, 'MCP install succeeds')
        call assert_equal_string(replace_str(mcp_text, 'prefix-mcp', '<prefix>'), &
            replace_str(cli_text, 'prefix-cli', '<prefix>'), 'install text equals the CLI')
        call assert_true(file_exists(join_path(mcp_prefix, 'bin/parity_app')), &
            'MCP install publishes the executable')
    end subroutine test_install

    subroutine test_clean()
        logical :: is_error

        call make_directory(join_path(app, 'build'))
        call cli(app, 'clean')
        call make_directory(join_path(app, 'build'))
        call mcp('{"action":"clean"'//dir_of(app)//'}', mcp_text, mcp_err, is_error)
        call same_result('clean', is_error)
        call assert_contains(mcp_text, 'build tree cleared', 'clean removes the build tree')
        call make_directory(join_path(app, 'build'))
        call cli(app, 'clean', '--cache')
        call make_directory(join_path(app, 'build'))
        call mcp('{"action":"clean","cache":true'//dir_of(app)//'}', mcp_text, mcp_err, &
            is_error)
        call same_result('clean --cache', is_error)
        call assert_contains(mcp_text, 'cache cleared', 'clean --cache purges the store')
    end subroutine test_clean

    subroutine test_build_log()
        !! The MCP build result carries the whole compiler log.
        character(:), allocatable :: last_line
        logical :: is_error

        call mcp('{"action":"build"'//dir_of(noisy)//'}', mcp_text, mcp_err, is_error)
        call assert_true(len(mcp_text) > 8192, 'noisy build log exceeds 8 KiB')
        call assert_contains(mcp_text, 'undefined_variable_with_a_long_name_400', &
            'MCP build output keeps the last warning')
    end subroutine test_build_log

end program test_command_parity
