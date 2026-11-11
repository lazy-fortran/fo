program test_test_cache_cli
    !! One PASS cache for one-shot `fo test` and the MCP test tool, keyed by
    !! the case input closure digest like Gremlin's case reuse. Every case
    !! writes a marker line per process start outside the project, so the
    !! oracle is observable: a reused case starts no process.
    !!  - an unchanged rerun reuses every PASS and starts no test process;
    !!  - a module edit reruns only the case that uses it;
    !!  - --rerun and MCP rerun:true run everything;
    !!  - an undeclared data file edit widens to every case (as 337dae0);
    !!  - a FAIL is never reused, not even with an unchanged closure;
    !!  - MCP and CLI share the cache;
    !!  - `fo test --list --json` reports, without starting a test process,
    !!    the digest under which the first run records each PASS (#362);
    !!  - `fo test --cases FILE` runs exactly the listed cases and reports a
    !!    case whose inputs changed since the listing as CHANGED (#363);
    !!  - with --json that run prints one object: every result with the
    !!    digest it ran under, the CHANGED cases and the targets built.
    !! Runs the driver named by FO.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, read_text
    use fo_test_harness, only: remove_tree, assert_equal_integer
    use fo_test_harness, only: assert_contains, assert_not_contains, assert_true
    use fo_test_harness, only: finish_assertions, assert_equal_string
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_json, only: json_value_t, json_member, json_element, json_string_value
    use fo_test_json, only: json_parse, json_size
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(:), allocatable :: driver, scratch, project, markers, cache
    type(string_list_t) :: env
    integer :: c_base = 0

    call resolve_driver(driver)
    call make_scratch('fo-test-cache-cli', scratch)
    cache = join_path(scratch, 'cache')
    project = join_path(scratch, 'project')
    markers = join_path(scratch, 'markers')
    call list_add(env, 'FO_JOBS=2')
    call list_add(env, 'FO_TEST_RERUN=0')
    call write_fixture()

    call check_reuse_and_edits()
    call check_failures_are_not_reused()
    call check_mcp()
    call check_cmake()
    call check_case_list()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'test-cache-cli: reuse, edits, rerun, failures and MCP passed'

contains

    subroutine case_program(name, uses)
        character(len=*), intent(in) :: name, uses
        character(:), allocatable :: text

        text = 'program '//name//nl
        if (len(uses) > 0) text = text//'use '//uses//nl
        text = text//'implicit none'//nl//'logical :: stop_now'//nl// &
            'open (7, file="'//join_path(markers, name)//'", position="append")'// &
            nl//'write (7, "(a)") "run"'//nl//'close (7)'//nl// &
            'inquire (file="'//join_path(scratch, 'fail_'//name)//'", exist=stop_now)'// &
            nl//'if (stop_now) error stop "'//name//' fails on request"'//nl// &
            'end program'//nl
        call write_text(join_path(project, 'test/'//name//'.f90'), text)
    end subroutine case_program

    subroutine write_fixture()
        call write_text(join_path(project, 'fpm.toml'), 'name = "tcache"'//nl)
        call write_text(join_path(project, 'src/mod_a.f90'), &
            'module mod_a'//nl//'implicit none'//nl//'end module'//nl)
        call write_text(join_path(project, 'src/mod_b.f90'), &
            'module mod_b'//nl//'implicit none'//nl//'end module'//nl)
        call write_text(join_path(markers, 'keep'), '')
        call case_program('case_a', 'mod_a')
        call case_program('case_b', 'mod_b')
        call case_program('case_c', '')
    end subroutine write_fixture

    subroutine check_case_list()
        type(process_result_t) :: result
        character(:), allocatable :: listed, plan, digest_a, digest_c
        integer :: a0, b0, c0

        call fo_test([character(len=16) :: '--list', '--json'], result)
        listed = listed_digests(result%stdout)
        digest_a = listed(index(listed, 'case_a=') + 7:index(listed, 'case_a=') + 70)
        digest_c = listed(index(listed, 'case_c=') + 7:index(listed, 'case_c=') + 70)
        a0 = runs('case_a')
        b0 = runs('case_b')
        c0 = runs('case_c')

        plan = join_path(scratch, 'plan.json')
        call write_text(plan, '{"cases":[{"name":"case_c","closure_digest":"'// &
            digest_c//'"}]}')
        call fo_test([character(len=256) :: '--rerun', '--cases', plan], result)
        call assert_equal_integer(result%exit_code, 0, 'listed case runs: '// &
            result%stdout//result%stderr)
        call expect_runs(a0, b0, c0 + 1, 'only the listed case runs')
        call assert_contains(result%stdout, 'plan targets: ', &
            'the run names the targets it builds: '//result%stdout)
        call assert_not_contains(result%stdout, 'case_a', &
            'the targets of unlisted cases are not built: '//result%stdout)

        call write_text(join_path(project, 'src/mod_a.f90'), &
            'module mod_a'//nl//'implicit none'//nl//'integer :: planned'//nl// &
            'end module'//nl)
        call write_text(plan, '{"cases":[{"name":"case_a","closure_digest":"'// &
            digest_a//'"},{"name":"case_c","closure_digest":"'//digest_c//'"}]}')
        call fo_test([character(len=256) :: '--rerun', '--cases', plan], result)
        call assert_true(result%exit_code /= 0, 'a changed case fails the command')
        call assert_contains(result%stdout, 'CHANGED case_a', &
            'the edited case is reported CHANGED: '//result%stdout)
        call expect_runs(a0, b0, c0 + 2, 'a CHANGED case does not run, the other does')

        call fo_test([character(len=256) :: '--rerun', '--json=full', '--cases', &
            plan], result)
        call assert_true(result%exit_code /= 0, 'a changed case fails the JSON run')
        listed = reported_cases(result%stdout)
        call assert_contains(listed, 'case_c=pass='//digest_c//';', &
            'a result carries the digest it ran under: '//result%stdout)
        call assert_contains(listed, 'case_a=changed=', &
            'the report lists the changed case: '//result%stdout)
        call assert_not_contains(listed, 'case_a=changed=;', &
            'a changed case carries its current digest: '//result%stdout)
        call assert_not_contains(listed, 'case_a=changed='//digest_a, &
            'the current digest differs from the listed one: '//result%stdout)
        call assert_contains(listed, 'targets=', &
            'the report names the targets built: '//result%stdout)
        call assert_contains(listed(index(listed, 'targets='):), 'case_c,', &
            'the target of the case run is built: '//result%stdout)
        call assert_not_contains(listed(index(listed, 'targets='):), 'case_a', &
            'the target of the changed case is not planned: '//result%stdout)
        call expect_runs(a0, b0, c0 + 3, 'the JSON run starts only the unchanged case')

        call write_text(plan, '{"cases":[{"name":"case_a","closure_digest":"'// &
            digest_a//'"}]}')
        call fo_test([character(len=256) :: '--json', '--cases', plan], result)
        call assert_true(result%exit_code /= 0, 'only changed cases fail the run')
        call assert_contains(result%stdout, '"st":"changed"', &
            'a run of only changed cases still reports them: '//result%stdout)
        call expect_runs(a0, b0, c0 + 3, 'a run of only changed cases starts nothing')

        call write_text(plan, '{"cases":[{"name":"no_such_case"}]}')
        call fo_test([character(len=256) :: '--cases', plan], result)
        call assert_true(result%exit_code /= 0, 'an unknown case fails: '// &
            result%stdout//result%stderr)
        call expect_runs(a0, b0, c0 + 3, 'an unknown case runs nothing')
    end subroutine check_case_list

    function listed_digests(json) result(text)
        !! 'name=digest;' for every listed case, in listing order.
        character(len=*), intent(in) :: json
        character(:), allocatable :: text, message
        type(json_value_t) :: report, cases, entry
        logical :: valid
        integer :: i

        text = ''
        call json_parse(json, report, valid, message)
        if (.not. valid) return
        cases = json_member(report, 'cases')
        do i = 1, json_size(cases)
            entry = json_element(cases, i)
            text = text//json_string_value(json_member(entry, 'name'))//'='// &
                json_string_value(json_member(entry, 'closure_digest'))//';'
        end do
    end function listed_digests

    function reported_cases(json) result(text)
        !! 'name=status=digest;' for every result of a --json=full report,
        !! then 'targets=' and 'name,' for every target; '' unless the
        !! output is one JSON object.
        character(len=*), intent(in) :: json
        character(:), allocatable :: text, message
        type(json_value_t) :: report, items, entry
        logical :: valid
        integer :: i

        text = 'targets='
        call json_parse(json, report, valid, message)
        if (.not. valid) return
        text = ''
        items = json_member(report, 'tests')
        do i = 1, json_size(items)
            entry = json_element(items, i)
            text = text//json_string_value(json_member(entry, 'name'))//'='// &
                json_string_value(json_member(entry, 'status'))//'='// &
                json_string_value(json_member(entry, 'closure_digest'))//';'
        end do
        text = text//'targets='
        items = json_member(report, 'targets')
        do i = 1, json_size(items)
            text = text//json_string_value(json_element(items, i))//','
        end do
    end function reported_cases

    function recorded_digests(dir, listed) result(text)
        !! 'name=digest;' for the cases of listed, in that order, as the
        !! PASS records of build/fo-test-index
        !! ('status|digest|cpu_ms|peak_kb|write_bytes|name').
        character(len=*), intent(in) :: dir, listed
        character(:), allocatable :: text, index_text, name
        integer :: at, eq, semi, row

        text = ''
        index_text = read_text(join_path(dir, 'build/fo-test-index'))
        at = 1
        do while (at <= len(listed))
            eq = at + index(listed(at:), '=') - 1
            semi = at + index(listed(at:), ';') - 1
            name = listed(at:eq - 1)
            row = index(index_text, '|'//name//nl)
            text = text//name//'='
            if (row > 0) then
                ! Back to the start of the row, then past 'PASS|'.
                row = index(index_text(1:row), nl, back=.true.) + 6
                text = text//index_text(row:row + 63)
            end if
            text = text//';'
            at = semi + 1
        end do
    end function recorded_digests

    integer function runs(name) result(count)
        !! Process starts of a case: the lines of its marker file.
        character(len=*), intent(in) :: name
        character(:), allocatable :: text
        logical :: present
        integer :: i

        count = 0
        inquire (file=join_path(markers, name), exist=present)
        if (.not. present) return
        text = read_text(join_path(markers, name))
        do i = 1, len(text)
            if (text(i:i) == nl) count = count + 1
        end do
    end function runs

    subroutine fo_test(words, result)
        character(len=*), intent(in) :: words(:)
        type(process_result_t), intent(out) :: result
        type(string_list_t) :: arguments
        integer :: i

        call list_add(arguments, 'test')
        do i = 1, size(words)
            call list_add(arguments, trim(words(i)))
        end do
        call run_fo(driver, arguments, project, cache, result, env, 240000)
    end subroutine fo_test

    subroutine expect_runs(a, b, c, label)
        integer, intent(in) :: a, b, c
        character(len=*), intent(in) :: label

        call assert_equal_integer(runs('case_a'), a, label//': case_a process starts')
        call assert_equal_integer(runs('case_b'), b, label//': case_b process starts')
        call assert_equal_integer(runs('case_c'), c, label//': case_c process starts')
    end subroutine expect_runs

    subroutine check_reuse_and_edits()
        type(process_result_t) :: result
        character(:), allocatable :: listed

        call fo_test([character(len=16) :: '--list', '--json'], result)
        call assert_equal_integer(result%exit_code, 0, 'plan lists: '//result%stderr)
        listed = listed_digests(result%stdout)
        call expect_runs(0, 0, 0, 'listing starts no test process')
        call assert_true(len(listed) == 3*(6 + 1 + 64 + 1), &
            'every case is listed with a 64-character digest: '//listed)

        call fo_test([character(len=16) :: ], result)
        call assert_equal_integer(result%exit_code, 0, 'first run passes: '// &
            result%stdout//result%stderr)
        call assert_contains(result%stdout, '3 pass', 'first run runs all three')
        call expect_runs(1, 1, 1, 'first run')
        call assert_equal_string(recorded_digests(project, listed), listed, &
            'the listed digests are the keys the first run records')

        call fo_test([character(len=16) :: ], result)
        call assert_equal_integer(result%exit_code, 0, 'unchanged rerun passes: '// &
            result%stdout//result%stderr)
        call assert_contains(result%stdout, '3 reused', &
            'unchanged rerun reports every case reused: '//result%stdout)
        call expect_runs(1, 1, 1, 'unchanged rerun starts no test process')

        call write_text(join_path(project, 'src/mod_a.f90'), &
            'module mod_a'//nl//'implicit none'//nl//'integer :: edited'//nl// &
            'end module'//nl)
        call fo_test([character(len=16) :: ], result)
        call assert_equal_integer(result%exit_code, 0, 'module edit run passes: '// &
            result%stdout//result%stderr)
        call assert_contains(result%stdout, '1 pass, 2 reused', &
            'a module edit reruns one case: '//result%stdout)
        call expect_runs(2, 1, 1, 'module edit reruns only its user')

        call fo_test([character(len=16) :: '--rerun'], result)
        call assert_equal_integer(result%exit_code, 0, '--rerun passes')
        call assert_not_contains(result%stdout, 'reused', &
            '--rerun reuses nothing: '//result%stdout)
        call expect_runs(3, 2, 2, '--rerun runs all')

        call write_text(join_path(project, 'test/data.txt'), 'undeclared data'//nl)
        call fo_test([character(len=16) :: ], result)
        call assert_equal_integer(result%exit_code, 0, 'data edit run passes')
        call assert_not_contains(result%stdout, 'reused', &
            'an undeclared data file widens to every case: '//result%stdout)
        call expect_runs(4, 3, 3, 'data edit reruns all')
    end subroutine check_reuse_and_edits

    subroutine check_failures_are_not_reused()
        type(process_result_t) :: result

        call write_text(join_path(scratch, 'fail_case_c'), 'x')
        c_base = runs('case_c')
        call fo_test([character(len=16) :: '--rerun'], result)
        call assert_true(result%exit_code /= 0, 'failing case fails the run')
        ! The runner retries a failing case, so a failing run may start the
        ! case more than once: its starts are compared relative.
        call assert_true(runs('case_c') > c_base, 'the failing case ran')
        c_base = runs('case_c')
        call assert_equal_integer(runs('case_a'), 5, 'failing run: case_a starts')
        call assert_equal_integer(runs('case_b'), 4, 'failing run: case_b starts')
        call fo_test([character(len=16) :: ], result)
        call assert_true(result%exit_code /= 0, &
            'an unchanged failing case fails again, never reused: '//result%stdout)
        call assert_contains(result%stdout, '2 reused', &
            'the passing cases are reused beside the failure: '//result%stdout)
        call assert_true(runs('case_c') > c_base, 'unchanged FAIL runs again')
        c_base = runs('case_c')
        call remove_file(join_path(scratch, 'fail_case_c'))
        call fo_test([character(len=16) :: ], result)
        call assert_equal_integer(result%exit_code, 0, 'fixed case passes')
        call assert_equal_integer(runs('case_c'), c_base + 1, 'fixed case runs once')
        call fo_test([character(len=16) :: ], result)
        call assert_contains(result%stdout, '3 reused', &
            'the new PASS is reused: '//result%stdout)
        call assert_equal_integer(runs('case_c'), c_base + 1, &
            'the new PASS starts no process')
    end subroutine check_failures_are_not_reused

    subroutine check_cmake()
        !! The same reuse on a CMake project: an edit of one library source
        !! reruns only the case that links it.
        character(:), allocatable :: cm
        type(process_result_t) :: result
        type(string_list_t) :: cmake_env
        type(string_list_t) :: arguments, listing
        character(:), allocatable :: listed

        cm = join_path(scratch, 'cmake')
        call write_text(join_path(cm, 'CMakeLists.txt'), 'cmake_minimum_required(VERSION 3.20)'//nl// &
            'project(tc Fortran)'//nl//'enable_testing()'//nl// &
            'add_library(tc_core STATIC core.f90)'//nl// &
            'add_executable(unit_a unit_a.f90)'//nl//'target_link_libraries(unit_a tc_core)'//nl// &
            'add_executable(unit_b unit_b.f90)'//nl// &
            'add_test(NAME unit_a COMMAND unit_a)'//nl//'add_test(NAME unit_b COMMAND unit_b)'//nl)
        call write_text(join_path(cm, 'core.f90'), 'module core'//nl//'end module'//nl)
        call write_text(join_path(cm, 'unit_a.f90'), 'program unit_a'//nl//'use core'//nl// &
            'end program'//nl)
        call write_text(join_path(cm, 'unit_b.f90'), 'program unit_b'//nl//'end program'//nl)
        call list_add(cmake_env, 'FO_BACKEND=cmake')
        call list_add(cmake_env, 'FO_JOBS=2')
        call list_add(cmake_env, 'FO_TEST_RERUN=0')
        call list_add(arguments, 'test')
        call list_add(listing, 'test')
        call list_add(listing, '--list')
        call list_add(listing, '--json')
        call run_fo(driver, listing, cm, cache, result, cmake_env, 240000)
        listed = listed_digests(result%stdout)
        call assert_true(len(listed) == 2*(6 + 1 + 64 + 1), &
            'cmake: every case is listed with a 64-character digest: '//listed)
        call run_fo(driver, arguments, cm, cache, result, cmake_env, 240000)
        call assert_contains(result%stdout, '2 pass', 'cmake first run: '//result%stdout//result%stderr)
        call assert_equal_string(recorded_digests(cm, listed), listed, &
            'cmake: the listed digests are the keys the first run records')
        call run_fo(driver, arguments, cm, cache, result, cmake_env, 240000)
        call assert_contains(result%stdout, '2 reused', 'cmake unchanged rerun reuses: '//result%stdout)
        call write_text(join_path(cm, 'core.f90'), 'module core'//nl//'integer :: x'//nl//'end module'//nl)
        call run_fo(driver, arguments, cm, cache, result, cmake_env, 240000)
        call assert_contains(result%stdout, '1 pass, 1 reused', 'cmake edit reruns only the linking case: '//result%stdout)
    end subroutine check_cmake

    subroutine remove_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, status

        open (newunit=unit, file=path, status='old', iostat=status)
        if (status == 0) close (unit, status='delete')
    end subroutine remove_file

    function mcp_test(arguments) result(text)
        !! Text of the MCP test tool answer for the given extra arguments.
        character(len=*), intent(in) :: arguments
        character(:), allocatable :: text, messages
        type(json_value_t), allocatable :: output(:)
        type(process_result_t) :: child
        type(json_value_t) :: result_object

        messages = mcp_encode(mcp_request(1, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}'), .false.)
        messages = messages//mcp_encode(mcp_call(2, &
            '{"action":"test","dir":'//mcp_quote(project)//arguments//'}'), .false.)
        messages = messages//mcp_encode(mcp_request(3, 'shutdown'), .false.)
        call mcp_exchange(driver, project, cache, messages, .false., output, child, env)
        result_object = json_member(output(2), 'result')
        text = json_string_value(json_member(json_element( &
            json_member(result_object, 'content'), 1), 'text'))
    end function mcp_test

    subroutine check_mcp()
        character(:), allocatable :: text

        text = mcp_test('')
        call assert_contains(text, '3 reused', &
            'MCP reuses what the CLI recorded: '//text)
        call expect_runs(5, 4, c_base + 1, 'MCP reuse starts no process')
        text = mcp_test(',"rerun":true')
        call assert_not_contains(text, 'reused', &
            'MCP rerun:true reuses nothing: '//text)
        call expect_runs(6, 5, c_base + 2, 'MCP rerun:true runs all')
    end subroutine check_mcp

end program test_test_cache_cli
