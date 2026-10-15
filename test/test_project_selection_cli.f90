program test_project_selection_cli
    !! `fo test` selection on the project model: --list on an fpm and a CMake
    !! fixture, unknown names, labels, and CLI/MCP agreement. Runs the driver
    !! named by FO (the worktree driver for changed CLI behaviour).
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_lines, write_text
    use fo_test_harness, only: remove_tree, assert_file_absent
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_contains, assert_not_contains, assert_true
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_boolean_value
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    character(:), allocatable :: driver, scratch, fpm_dir, cmake_dir, cache
    type(string_list_t) :: cmake_env, fpm_env

    call resolve_driver(driver)
    call make_scratch('fo-project-selection-cli', scratch)
    cache = join_path(scratch, 'cache')
    fpm_dir = join_path(scratch, 'fpm')
    cmake_dir = join_path(scratch, 'cmake')
    call list_add(cmake_env, 'FO_BACKEND=cmake')
    call list_add(cmake_env, 'FO_JOBS=2')
    call list_add(fpm_env, 'FO_JOBS=2')
    call write_fpm_fixture()
    call write_cmake_fixture()

    call check_fpm()
    call check_cmake()
    call check_mcp_agreement()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'project-selection-cli: list, errors and MCP agreement passed'

contains

    subroutine program_file(dir, name, uses_core)
        character(len=*), intent(in) :: dir, name
        logical, intent(in) :: uses_core

        character(:), allocatable :: text
        character(len=1), parameter :: nl = new_line('a')

        text = 'program ' // name // nl
        if (uses_core) then
            text = text // 'use sel_core, only: answer' // nl // &
                'implicit none' // nl // 'if (answer() /= 42) error stop' // nl
        end if
        text = text // 'end program' // nl
        call write_text(join_path(dir, name // '.f90'), text)
    end subroutine program_file

    subroutine write_fpm_fixture()
        call write_lines(join_path(fpm_dir, 'fpm.toml'), [character(len=64) :: &
            'name = "sel_fpm"'])
        call write_lines(join_path(fpm_dir, 'src/sel_core.f90'), [character(len=64) :: &
            'module sel_core', 'implicit none', 'contains', &
            'integer function answer()', 'answer = 42', 'end function', &
            'end module'])
        call program_file(join_path(fpm_dir, 'test'), 'test_fast', .true.)
        call program_file(join_path(fpm_dir, 'test'), 'test_other', .false.)
        call program_file(join_path(fpm_dir, 'test'), 'test_heavy_slow', .false.)
    end subroutine write_fpm_fixture

    subroutine write_cmake_fixture()
        call write_lines(join_path(cmake_dir, 'CMakeLists.txt'), [character(len=96) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(sel_cmake Fortran)', 'enable_testing()', &
            'add_library(sel_core STATIC sel_core.f90)', &
            'add_executable(unit_a unit_a.f90)', &
            'target_link_libraries(unit_a sel_core)', &
            'add_executable(unit_b unit_b.f90)', &
            'add_executable(perf_c perf_c.f90)', &
            'add_test(NAME unit_a COMMAND unit_a)', &
            'add_test(NAME unit_b COMMAND unit_b)', &
            'add_test(NAME perf_c COMMAND perf_c)', &
            'set_tests_properties(unit_a PROPERTIES LABELS "unit")', &
            'set_tests_properties(unit_b PROPERTIES LABELS "unit;io")', &
            'set_tests_properties(perf_c PROPERTIES LABELS "slow")'])
        call write_lines(join_path(cmake_dir, 'sel_core.f90'), [character(len=64) :: &
            'module sel_core', 'implicit none', 'contains', &
            'integer function answer()', 'answer = 42', 'end function', &
            'end module'])
        call program_file(cmake_dir, 'unit_a', .true.)
        call program_file(cmake_dir, 'unit_b', .false.)
        call program_file(cmake_dir, 'perf_c', .false.)
    end subroutine write_cmake_fixture

    subroutine fo(dir, env, words, result)
        character(len=*), intent(in) :: dir, words(:)
        type(string_list_t), intent(in) :: env
        type(process_result_t), intent(out) :: result
        type(string_list_t) :: arguments
        integer :: i

        do i = 1, size(words)
            call list_add(arguments, trim(words(i)))
        end do
        call run_fo(driver, arguments, dir, cache, result, env)
    end subroutine fo

    function listing(dir, env, words, result) result(names)
        !! Names of the selected cases from `fo test --list --json words`,
        !! each followed by ';'.
        character(len=*), intent(in) :: dir, words(:)
        type(string_list_t), intent(in) :: env
        type(process_result_t), intent(out) :: result
        character(:), allocatable :: names
        type(json_value_t) :: report, cases
        character(:), allocatable :: message
        logical :: valid
        integer :: i

        call fo(dir, env, [character(len=48) :: 'test', '--list', '--json', words], &
            result)
        names = ''
        call assert_equal_integer(result%exit_code, 0, 'list exits 0: ' // result%stderr)
        call json_parse(result%stdout, report, valid, message)
        call assert_true(valid, 'list JSON parses')
        if (.not. valid) return
        cases = json_member(report, 'cases')
        do i = 1, json_size(cases)
            names = names // json_string_value(json_member(json_element(cases, i), &
                'name')) // ';'
        end do
    end function listing

    logical function json_has(array, name) result(found)
        type(json_value_t), intent(in) :: array
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        do i = 1, json_size(array)
            if (json_string_value(json_element(array, i)) == name) found = .true.
        end do
    end function json_has

    subroutine check_fpm()
        type(process_result_t) :: result

        call assert_equal_string(listing(fpm_dir, fpm_env, [character(len=48) :: '--json'], &
            result), 'test_fast;test_other;', 'fpm default selection skips slow')
        call assert_equal_string(listing(fpm_dir, fpm_env, &
            [character(len=48) :: '--all'], result), &
            'test_fast;test_heavy_slow;test_other;', 'fpm --all adds slow')
        call assert_equal_string(listing(fpm_dir, fpm_env, &
            [character(len=48) :: 'test_f*'], result), 'test_fast;', 'fpm glob')
        call assert_equal_string(listing(fpm_dir, fpm_env, &
            [character(len=48) :: '-L', 'slow'], result), 'test_heavy_slow;', &
            'fpm -L slow lifts the default exclusion')
        call assert_equal_string(listing(fpm_dir, fpm_env, &
            [character(len=48) :: '--all', '-LE', 'slow'], result), &
            'test_fast;test_other;', 'fpm -LE slow wins over --all')
        call fo(fpm_dir, fpm_env, [character(len=48) :: 'test', '--list'], result)
        call assert_contains(result%stdout, 'test_fast', 'text list names the case')
        call assert_contains(result%stdout, 'plan targets:', 'text list has the plan')
        call assert_contains(result%stdout, 'widened:', 'text list has the flag')

        call fo(fpm_dir, fpm_env, [character(len=48) :: 'test', 'no_such_test'], result)
        call assert_equal_integer(result%exit_code, 1, 'unknown name exits 1')
        call assert_contains(result%stderr, 'fo: unknown test: no_such_test', &
            'unknown name diagnostic')
        call fo(fpm_dir, fpm_env, [character(len=48) :: 'test', 'test_heavy_slow'], &
            result)
        call assert_equal_integer(result%exit_code, 1, 'slow name needs --all')
        call assert_contains(result%stderr, &
            'fo: slow test test_heavy_slow requires --all', 'slow name diagnostic')
        call fo(fpm_dir, fpm_env, [character(len=48) :: 'test', '--layer', 'nope'], &
            result)
        call assert_equal_integer(result%exit_code, 1, 'unknown layer exits 1')
        call fo(fpm_dir, fpm_env, [character(len=48) :: 'test', '-L', 'nolabel'], &
            result)
        call assert_equal_integer(result%exit_code, 1, 'empty selection exits 1')

        call fo(fpm_dir, fpm_env, [character(len=48) :: 'test', 'test_f*', '--json'], &
            result)
        call assert_equal_integer(result%exit_code, 0, 'glob run passes: ' // &
            result%stderr)
        call assert_contains(result%stdout, 'test_fast', 'glob run reports the case')
        call assert_not_contains(result%stdout, 'test_other', 'glob run skips others')
    end subroutine check_fpm

    subroutine check_cmake()
        type(process_result_t) :: result
        type(json_value_t) :: report
        character(:), allocatable :: message
        logical :: valid

        ! Unbuilt tree: the commands come from the generated CTestTestfile.
        call assert_equal_string(listing(cmake_dir, cmake_env, &
            [character(len=48) :: '--json'], result), 'unit_a;unit_b;', &
            'cmake default selection skips the slow label')
        call json_parse(result%stdout, report, valid, message)
        call assert_true(.not. json_boolean_value(json_member(report, 'widened')), &
            'unbuilt tree does not widen')
        call assert_true(.not. json_has(json_member(report, 'targets'), 'perf_c'), &
            'unbuilt plan omits the slow-labelled target')
        call assert_equal_string(listing(cmake_dir, cmake_env, &
            [character(len=48) :: '-L', 'io'], result), 'unit_b;', 'unbuilt -L io')
        call json_parse(result%stdout, report, valid, message)
        call assert_true(json_has(json_member(report, 'targets'), 'unit_b'), &
            'unbuilt -L io plans its target')
        call assert_true(.not. json_has(json_member(report, 'targets'), 'unit_a'), &
            'unbuilt -L io plans no other case target')
        call fo(cmake_dir, cmake_env, [character(len=48) :: 'test', '-L', 'io'], &
            result)
        call assert_equal_integer(result%exit_code, 0, 'unbuilt -L io run: ' // &
            result%stderr)
        call assert_file_absent(join_path(cmake_dir, 'build/unit_a'), &
            'unbuilt -L io run builds only its own targets')
        call assert_equal_string(listing(cmake_dir, cmake_env, &
            [character(len=48) :: '--all'], result), 'unit_a;unit_b;perf_c;', &
            'cmake --all adds the slow label')
        call assert_equal_string(listing(cmake_dir, cmake_env, &
            [character(len=48) :: '-L', 'io'], result), 'unit_b;', 'cmake -L')
        call assert_equal_string(listing(cmake_dir, cmake_env, &
            [character(len=48) :: '-L', 'unit', '-LE', 'io'], result), 'unit_a;', &
            'cmake -L with -LE')
        call assert_equal_string(listing(cmake_dir, cmake_env, &
            [character(len=48) :: '--label=^unit$', 'unit_*'], result), &
            'unit_a;unit_b;', 'cmake glob and label')

        call fo(cmake_dir, cmake_env, [character(len=48) :: 'test', 'no_such_test'], &
            result)
        call assert_equal_integer(result%exit_code, 1, 'cmake unknown name exits 1')
        call assert_contains(result%stderr, 'fo: unknown test: no_such_test', &
            'cmake unknown name diagnostic')
        call fo(cmake_dir, cmake_env, [character(len=48) :: 'test', 'perf_c'], result)
        call assert_equal_integer(result%exit_code, 1, 'slow-labelled name needs --all')
        call assert_contains(result%stderr, 'requires --all', 'slow label diagnostic')

        ! Built tree: commands are known, the plan is exactly the closure.
        call fo(cmake_dir, cmake_env, [character(len=48) :: 'build'], result)
        call assert_equal_integer(result%exit_code, 0, 'cmake fixture builds: ' // &
            result%stderr)
        call assert_equal_string(listing(cmake_dir, cmake_env, &
            [character(len=48) :: '-L', 'unit', '-LE', 'io'], result), 'unit_a;', &
            'built tree selection')
        call json_parse(result%stdout, report, valid, message)
        call assert_true(.not. json_boolean_value(json_member(report, 'widened')), &
            'built tree does not widen')
        call assert_true(json_has(json_member(report, 'targets'), 'unit_a'), &
            'plan has the command target')
        call assert_true(json_has(json_member(report, 'targets'), 'sel_core'), &
            'plan has the link dependency')
        call assert_true(.not. json_has(json_member(report, 'targets'), 'perf_c'), &
            'plan omits unselected targets')

        call fo(cmake_dir, cmake_env, [character(len=48) :: 'test', '-L', 'unit', &
            '--json'], result)
        call assert_equal_integer(result%exit_code, 0, 'label run passes: ' // &
            result%stderr)
        call json_parse(result%stdout, report, valid, message)
        call assert_equal_integer(json_size(json_member(report, 'tests')), 2, &
            'label run reports the two unit cases')
    end subroutine check_cmake

    subroutine mcp_text(dir, names, text, is_error)
        character(len=*), intent(in) :: dir, names(:)
        character(:), allocatable, intent(out) :: text
        logical, intent(out) :: is_error
        type(json_value_t), allocatable :: output(:)
        type(process_result_t) :: child
        character(:), allocatable :: messages, args
        type(json_value_t) :: result_object
        integer :: i

        args = ''
        do i = 1, size(names)
            if (i > 1) args = args // ','
            args = args // mcp_quote(trim(names(i)))
        end do
        messages = mcp_encode(mcp_request(1, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}'), .false.)
        messages = messages // mcp_encode(mcp_call(2, &
            '{"action":"test","dir":' // mcp_quote(dir) // ',"args":[' // args // &
            '],"json":"full"}'), .false.)
        messages = messages // mcp_encode(mcp_request(3, 'shutdown'), .false.)
        call mcp_exchange(driver, dir, cache, messages, .false., output, child)
        result_object = json_member(output(2), 'result')
        is_error = json_boolean_value(json_member(result_object, 'isError'))
        text = json_string_value(json_member(json_element( &
            json_member(result_object, 'content'), 1), 'text'))
    end subroutine mcp_text

    function reported_names(text) result(names)
        character(len=*), intent(in) :: text
        character(:), allocatable :: names
        type(json_value_t) :: report, tests
        character(:), allocatable :: message
        logical :: valid
        integer :: i

        names = ''
        call json_parse(text, report, valid, message)
        call assert_true(valid, 'MCP report parses')
        if (.not. valid) return
        tests = json_member(report, 'tests')
        do i = 1, json_size(tests)
            names = names // json_string_value(json_member(json_element(tests, i), &
                'name')) // ';'
        end do
    end function reported_names

    subroutine check_mcp_agreement()
        type(process_result_t) :: result
        character(:), allocatable :: text, cli
        logical :: is_error

        ! The same request through the CLI list and the MCP tool selects the
        ! same cases, on both backends.
        cli = listing(fpm_dir, fpm_env, [character(len=48) :: 'test_f*'], result)
        call mcp_text(fpm_dir, [character(len=48) :: 'test_f*'], text, is_error)
        call assert_true(.not. is_error, 'MCP glob succeeds: ' // text)
        call assert_equal_string(reported_names(text), cli, 'fpm MCP agrees on a glob')

        cli = listing(cmake_dir, cmake_env, [character(len=48) :: 'unit_*'], result)
        call mcp_text(cmake_dir, [character(len=48) :: 'unit_*'], text, is_error)
        call assert_true(.not. is_error, 'MCP cmake glob succeeds: ' // text)
        ! CTest orders results by its cost data, so compare the case set.
        call assert_equal_integer(len(reported_names(text)), len(cli), &
            'cmake MCP reports as many cases as the list selects')
        call assert_true(index(reported_names(text), 'unit_a;') > 0 .and. &
            index(reported_names(text), 'unit_b;') > 0, &
            'cmake MCP agrees on a glob')

        call fo(fpm_dir, fpm_env, [character(len=48) :: 'test', 'no_such_test'], result)
        call mcp_text(fpm_dir, [character(len=48) :: 'no_such_test'], text, is_error)
        call assert_true(is_error, 'MCP rejects an unknown name')
        call assert_contains(result%stderr, text, &
            'MCP and CLI share the unknown-name diagnostic')
        call fo(fpm_dir, fpm_env, [character(len=48) :: 'test', 'test_heavy_slow'], &
            result)
        call mcp_text(fpm_dir, [character(len=48) :: 'test_heavy_slow'], text, is_error)
        call assert_true(is_error, 'MCP rejects a slow name without --all')
        call assert_contains(result%stderr, text, &
            'MCP and CLI share the slow-name diagnostic')
        call fo(cmake_dir, cmake_env, [character(len=48) :: 'test', 'no_such_test'], &
            result)
        call mcp_text(cmake_dir, [character(len=48) :: 'no_such_test'], text, is_error)
        call assert_true(is_error, 'MCP rejects an unknown cmake name')
        call assert_contains(result%stderr, text, &
            'MCP and CLI share the cmake diagnostic')
    end subroutine check_mcp_agreement

end program test_project_selection_cli
