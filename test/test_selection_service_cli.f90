program test_selection_service_cli
    !! One select/plan/run service: `fo check`, bare `fo test` and the MCP
    !! test tool select on the project model like `fo test --only-changed`
    !! and `fo test --list`.
    !!  - `fo check` runs the cases the Git delta affects, not the cases of
    !!    compile-cache misses: a clean tree runs nothing, an edit runs the
    !!    cases that depend on it (an unaffected failing case stays unrun).
    !!  - `--only-changed` sees edits in a path dependency outside the
    !!    project root.
    !!  - bare fpm `fo test`, `--list` and the unnamed MCP test apply
    !!    `[extra.fo.test-defaults] exclude`.
    !!  - A CTest name with a space selects exactly that case.
    !! Runs the driver named by FO.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_lines, append_text
    use fo_test_harness, only: remove_tree, assert_equal_integer
    use fo_test_harness, only: assert_contains, assert_not_contains, assert_true
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_boolean_value
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    character(:), allocatable :: driver, scratch, cache
    type(string_list_t) :: fpm_env, cmake_env

    call resolve_driver(driver)
    call make_scratch('fo-selection-service-cli', scratch)
    cache = join_path(scratch, 'cache')
    call list_add(fpm_env, 'FO_JOBS=2')
    call list_add(cmake_env, 'FO_BACKEND=cmake')
    call list_add(cmake_env, 'FO_JOBS=2')

    call check_follows_git_delta()
    call only_changed_sees_path_dependency()
    call defaults_apply_to_bare_and_mcp()
    call ctest_name_with_space()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'selection-service-cli: check, delta, defaults and names passed'

contains

    subroutine program_file(path, name, uses, fails)
        character(len=*), intent(in) :: path, name, uses
        logical, intent(in) :: fails
        character(len=48) :: lines(4)
        integer :: n

        lines(1) = 'program '//name
        lines(2) = 'implicit none'
        n = 2
        if (fails) then
            n = 3
            lines(n) = 'error stop "'//name//' fails"'
        else if (len_trim(uses) > 0) then
            lines(2) = 'use '//uses
            lines(3) = 'implicit none'
            n = 3
        end if
        lines(n + 1) = 'end program'
        call write_lines(path, lines(1:n + 1))
    end subroutine program_file

    subroutine module_file(path, name)
        character(len=*), intent(in) :: path, name
        character(len=48) :: lines(3)

        lines(1) = 'module '//name
        lines(2) = 'implicit none'
        lines(3) = 'end module'
        call write_lines(path, lines)
    end subroutine module_file

    subroutine git(dir, text)
        character(len=*), intent(in) :: dir, text
        type(string_list_t) :: arguments
        type(process_result_t) :: result
        integer :: pos, next

        pos = 1
        do while (pos <= len_trim(text))
            next = index(text(pos:), ' ')
            if (next == 0) next = len_trim(text) - pos + 2
            if (next > 1) call list_add(arguments, text(pos:pos + next - 2))
            pos = pos + next
        end do
        call run_external('git', arguments, dir, result)
        call assert_equal_integer(result%exit_code, 0, 'git '//text//': '// &
            result%stderr)
    end subroutine git

    subroutine commit_all(dir)
        character(len=*), intent(in) :: dir

        call write_lines(join_path(dir, '.gitignore'), [character(len=8) :: 'build/'])
        call git(dir, 'init --quiet')
        call git(dir, 'add -A')
        call git(dir, '-c user.name=T -c user.email=t@example.invalid '// &
            '-c commit.gpgsign=false commit --quiet -m base')
    end subroutine commit_all

    subroutine fo(dir, env, first, second, third, result)
        character(len=*), intent(in) :: dir, first, second, third
        type(string_list_t), intent(in) :: env
        type(process_result_t), intent(out) :: result
        type(string_list_t) :: arguments

        call list_add(arguments, first)
        if (len(second) > 0) call list_add(arguments, second)
        if (len(third) > 0) call list_add(arguments, third)
        call run_fo(driver, arguments, dir, cache, result, env, 240000)
    end subroutine fo

    subroutine check_follows_git_delta()
        !! sel_a uses mod_a; sel_b is independent and fails. The model delta
        !! of a clean tree is empty and of a mod_a edit is sel_a only, so
        !! sel_b never runs; compile-cache misses made every case affected.
        character(:), allocatable :: dir
        type(process_result_t) :: result

        dir = join_path(scratch, 'check')
        call write_lines(join_path(dir, 'fpm.toml'), [character(len=32) :: &
            'name = "chk"'])
        call module_file(join_path(dir, 'src/mod_a.f90'), 'mod_a')
        call program_file(join_path(dir, 'test/sel_a.f90'), 'sel_a', 'mod_a', .false.)
        call program_file(join_path(dir, 'test/sel_b.f90'), 'sel_b', '', .true.)
        call commit_all(dir)
        call fo(dir, fpm_env, 'build', '', '', result)
        call assert_equal_integer(result%exit_code, 0, 'check fixture builds: '// &
            result%stderr)
        call fo(dir, fpm_env, 'check', '--json=compact', '', result)
        call assert_equal_integer(result%exit_code, 0, &
            'check on a clean tree runs no case: '//result%stdout)
        call append_text(join_path(dir, 'src/mod_a.f90'), '! edited'//new_line('a'))
        call fo(dir, fpm_env, 'check', '--json=compact', '', result)
        call assert_equal_integer(result%exit_code, 0, &
            'check after a mod_a edit skips the unaffected failing case: '// &
            result%stdout)
        ! The edit breaks sel_a: check must run it.
        call program_file(join_path(dir, 'test/sel_a.f90'), 'sel_a', '', .true.)
        call fo(dir, fpm_env, 'check', '--json=compact', '', result)
        call assert_true(result%exit_code /= 0, &
            'check runs the edited failing case: '//result%stdout)
        call assert_contains(result%stdout//result%stderr, 'sel_a', &
            'check names the failing affected case')
    end subroutine check_follows_git_delta

    subroutine only_changed_sees_path_dependency()
        character(:), allocatable :: app, dep
        type(process_result_t) :: result

        app = join_path(scratch, 'app')
        dep = join_path(scratch, 'dep')
        call write_lines(join_path(dep, 'fpm.toml'), [character(len=32) :: &
            'name = "dep"'])
        call module_file(join_path(dep, 'src/dep_mod.f90'), 'dep_mod')
        call commit_all(dep)
        call write_lines(join_path(app, 'fpm.toml'), [character(len=48) :: &
            'name = "app"', '[dependencies]', 'dep = { path = "../dep" }'])
        call module_file(join_path(app, 'src/app_mod.f90'), 'app_mod')
        call program_file(join_path(app, 'test/dep_user.f90'), 'dep_user', &
            'dep_mod', .false.)
        call commit_all(app)
        call fo(app, fpm_env, 'test', '--list', '--only-changed', result)
        call assert_not_contains(result%stdout, 'dep_user', &
            'clean trees select nothing')
        call append_text(join_path(dep, 'src/dep_mod.f90'), '! edited'//new_line('a'))
        call fo(app, fpm_env, 'test', '--list', '--only-changed', result)
        call assert_equal_integer(result%exit_code, 0, 'dependency list: '// &
            result%stderr)
        call assert_contains(result%stdout, 'dep_user', &
            'an edit in a path dependency outside the root affects the cases')
    end subroutine only_changed_sees_path_dependency

    subroutine defaults_apply_to_bare_and_mcp()
        !! The excluded `noisy` case fails: a bare run, the list and the
        !! unnamed MCP test all leave it out.
        character(:), allocatable :: dir, text
        type(process_result_t) :: result
        type(json_value_t), allocatable :: responses(:)
        type(json_value_t) :: field
        character(:), allocatable :: messages

        dir = join_path(scratch, 'defaults')
        call write_lines(join_path(dir, 'fpm.toml'), [character(len=64) :: &
            'name = "dfl"', '[extra.fo.test-groups]', &
            'noisy = { tests = ["test/noisy*"] }', '[extra.fo.test-defaults]', &
            'exclude = ["noisy"]'])
        call module_file(join_path(dir, 'src/dfl.f90'), 'dfl')
        call program_file(join_path(dir, 'test/quiet.f90'), 'quiet', '', .false.)
        call program_file(join_path(dir, 'test/noisy_one.f90'), 'noisy_one', '', .true.)
        call fo(dir, fpm_env, 'test', '--list', '', result)
        call assert_contains(result%stdout, 'quiet', 'list selects quiet')
        call assert_not_contains(result%stdout, 'noisy_one', 'list excludes noisy')
        call fo(dir, fpm_env, 'test', '', '', result)
        call assert_equal_integer(result%exit_code, 0, &
            'bare fo test leaves out the excluded case: '//result%stdout// &
            result%stderr)
        messages = mcp_encode(mcp_request(1, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}'), .false.)
        messages = messages//mcp_encode(mcp_call(2, '{"action":"test","dir":'// &
            mcp_quote(dir)//',"json":"full"}'), .false.)
        messages = messages//mcp_encode(mcp_request(3, 'shutdown'), .false.)
        call mcp_exchange(driver, dir, cache, messages, .false., responses, result)
        field = json_member(json_member(responses(2), 'result'), 'isError')
        call assert_true(.not. json_boolean_value(field), 'unnamed MCP test passes')
        field = json_member(json_element(json_member(json_member(responses(2), &
            'result'), 'content'), 1), 'text')
        text = json_string_value(field)
        call assert_not_contains(text, 'noisy_one', 'unnamed MCP test excludes noisy')
    end subroutine defaults_apply_to_bare_and_mcp

    subroutine ctest_name_with_space()
        !! `hasspace` fails; only `has space` passes, so a regex that drops
        !! the space selects the wrong case.
        character(:), allocatable :: dir
        type(process_result_t) :: result

        dir = join_path(scratch, 'spaces')
        call write_lines(join_path(dir, 'CMakeLists.txt'), [character(len=96) :: &
            'cmake_minimum_required(VERSION 3.20)', 'project(sp Fortran)', &
            'enable_testing()', 'add_executable(good good.f90)', &
            'add_executable(bad bad.f90)', &
            'add_test(NAME "has space" COMMAND good)', &
            'add_test(NAME hasspace COMMAND bad)'])
        call program_file(join_path(dir, 'good.f90'), 'good', '', .false.)
        call program_file(join_path(dir, 'bad.f90'), 'bad', '', .true.)
        call fo(dir, cmake_env, 'test', 'has space', '', result)
        call assert_equal_integer(result%exit_code, 0, &
            'a CTest name with a space selects exactly that case: '// &
            result%stdout//result%stderr)
        call assert_not_contains(result%stdout, 'hasspace', &
            'the decoy without the space does not run')
    end subroutine ctest_name_with_space

end program test_selection_service_cli
