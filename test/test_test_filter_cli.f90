program test_test_filter_cli
    !! The case filter expression of `fo test -E`, the MCP test `filter` and
    !! the machine outputs for the user's own CI: JUnit XML and the
    !! deterministic `--partition`. Runs the driver named by FO.
    !!  - predicates, operators and the existing flags as sugar;
    !!  - failed() recalls the last run, affected() the Git delta;
    !!  - a bad expression is rejected;
    !!  - hash and count partitions are disjoint, complete and stable;
    !!  - JUnit has a testcase per result, escapes markup and drops
    !!    characters XML 1.0 forbids.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, read_text
    use fo_test_harness, only: remove_tree, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_contains, assert_not_contains, assert_true
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(:), allocatable :: driver, scratch, layered, failing, delta, cache
    type(string_list_t) :: env

    call resolve_driver(driver)
    call make_scratch('fo-test-filter-cli', scratch)
    cache = join_path(scratch, 'cache')
    layered = join_path(scratch, 'layered')
    failing = join_path(scratch, 'failing')
    delta = join_path(scratch, 'delta')
    call list_add(env, 'FO_JOBS=2')
    call list_add(env, 'FO_TEST_RERUN=0')
    call write_layered()
    call write_failing()

    call check_expressions()
    call check_partitions()
    call check_failed_and_junit()
    call check_affected()
    call check_mcp_filter()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'test-filter-cli: filter, partition and JUnit passed'

contains

    subroutine put(root, path, text)
        character(len=*), intent(in) :: root, path, text

        call write_text(join_path(root, path), text)
    end subroutine put

    subroutine write_layered()
        call put(layered, 'fpm.toml', 'name = "lay"'//nl//'[extra.fo.layers]'//nl// &
            'base = { source = ["src/l_base.f90"] }'//nl// &
            "mid = { modules = ['l_mid'], uses = ['base'] }"//nl// &
            '[extra.fo.test-groups]'//nl// &
            'physics = { tests = ["test_mid", "test_top"] }'//nl// &
            'smoke = { tests = ["test_free"] }'//nl)
        call put(layered, 'src/l_base.f90', 'module l_base'//nl//'end module'//nl)
        call put(layered, 'src/l_mid.f90', 'module l_mid'//nl//'use l_base'//nl// &
            'end module'//nl)
        call put(layered, 'test/test_base.f90', 'program test_base'//nl// &
            'use l_base'//nl//'end program'//nl)
        call put(layered, 'test/test_mid.f90', 'program test_mid'//nl//'use l_mid'// &
            nl//'end program'//nl)
        call put(layered, 'test/test_top.f90', 'program test_top'//nl//'use l_mid'// &
            nl//'end program'//nl)
        call put(layered, 'test/test_free.f90', 'program test_free'//nl// &
            'end program'//nl)
        call put(layered, 'test/test_lay_slow.f90', 'program test_lay_slow'//nl// &
            'end program'//nl)
    end subroutine write_layered

    subroutine write_failing()
        call put(failing, 'fpm.toml', 'name = "bad"'//nl)
        call put(failing, 'test/case_ok.f90', 'program case_ok'//nl//'end program'//nl)
        call put(failing, 'test/case_other.f90', 'program case_other'//nl// &
            'end program'//nl)
        call put(failing, 'test/case_bad.f90', 'program case_bad'//nl// &
            'implicit none'//nl// &
            'write (*, "(a)") "markup <b> & ""quotes"" ''apos'' ]]> end"'//nl// &
            'write (*, "(a)") "bell:"//achar(7)//"|esc:"//achar(27)//"|ok"'//nl// &
            'error stop "case_bad fails"'//nl//'end program'//nl)
    end subroutine write_failing

    subroutine fo_in(dir, words, result)
        character(len=*), intent(in) :: dir, words(:)
        type(process_result_t), intent(out) :: result
        type(string_list_t) :: arguments
        integer :: i

        do i = 1, size(words)
            call list_add(arguments, trim(words(i)))
        end do
        call run_fo(driver, arguments, dir, cache, result, env, 240000)
    end subroutine fo_in

    function listing(dir, words) result(names)
        !! Names of the cases `fo test --list --json words` selects, each
        !! followed by ';'. An error yields 'ERROR'.
        character(len=*), intent(in) :: dir, words(:)
        character(:), allocatable :: names
        type(process_result_t) :: result
        type(json_value_t) :: report, cases
        character(:), allocatable :: message
        logical :: valid
        integer :: i

        call fo_in(dir, [character(len=160) :: 'test', '--list', '--json', words], &
            result)
        names = 'ERROR'
        if (result%exit_code /= 0) return
        call json_parse(result%stdout, report, valid, message)
        if (.not. valid) return
        names = ''
        cases = json_member(report, 'cases')
        do i = 1, json_size(cases)
            names = names//json_string_value(json_member(json_element(cases, i), &
                'name'))//';'
        end do
    end function listing

    subroutine check_expressions()
        type(process_result_t) :: result

        call assert_equal_string(listing(layered, [character(len=160) :: '-E', &
            'name(test_[mt]*)']), 'test_mid;test_top;', 'name(glob)')
        call assert_equal_string(listing(layered, [character(len=160) :: '-E', &
            'label(^phys)']), 'test_mid;test_top;', 'label(regex)')
        call assert_equal_string(listing(layered, [character(len=160) :: '-E', &
            'smoke | physics']), 'test_free;test_mid;test_top;', &
            'bare label words joined by |')
        call assert_equal_string(listing(layered, [character(len=160) :: '-E', &
            '!physics & !smoke']), 'test_base;', 'negation and &; slow stays default-excluded')
        call assert_equal_string(listing(layered, [character(len=160) :: '--all', &
            '--filter', 'slow']), 'test_lay_slow;', '--filter with the slow label under --all')
        call assert_equal_string(listing(layered, [character(len=160) :: '-E', &
            'name(test_free) | (label(physics) & !name(test_top))']), &
            'test_free;test_mid;', 'parentheses and precedence')
        call assert_equal_string(listing(layered, [character(len=160) :: '-E', &
            'layer(base)']), listing(layered, [character(len=160) :: '--layer', &
            'base']), 'layer(NAME) agrees with --layer')
        call assert_true(listing(layered, [character(len=160) :: '-E', &
            'layer(base)']) /= 'ERROR', 'layer(base) selects')
        call assert_equal_string(listing(layered, [character(len=160) :: '-L', &
            'physics', '-E', '!name(test_top)']), 'test_mid;', &
            'the filter narrows the existing flags')
        call assert_equal_string(listing(layered, [character(len=160) :: &
            '--filter=name(test_free)']), 'test_free;', '--filter=EXPR spelling')

        call fo_in(layered, [character(len=160) :: 'test', '--list', '-E', &
            'name(test_free'], result)
        call assert_true(result%exit_code /= 0, 'unbalanced expression is rejected')
        call assert_contains(result%stderr, 'invalid filter', &
            'bad expression diagnostic: '//result%stderr)
        call fo_in(layered, [character(len=160) :: 'test', '--list', '-E', &
            'nosuch(x)'], result)
        call assert_true(result%exit_code /= 0, 'unknown predicate is rejected')
        call fo_in(layered, [character(len=160) :: 'test', '--list', '-E', &
            'label([)'], result)
        call assert_true(result%exit_code /= 0, 'invalid regex is rejected')
        call fo_in(layered, [character(len=160) :: 'test', '-E', 'name(nothing_*)'], &
            result)
        call assert_true(result%exit_code /= 0, 'an empty filter result is an error')
    end subroutine check_expressions

    subroutine check_partitions()
        character(:), allocatable :: full, joined
        character(len=2048) :: parts(6)
        character(len=16) :: names(5) = [character(len=16) :: 'test_base;', 'test_free;', 'test_lay_slow;', 'test_mid;', 'test_top;']
        integer :: k, n
        type(process_result_t) :: result

        full = listing(layered, [character(len=160) :: '--all'])
        call assert_equal_string(full, &
            'test_base;test_free;test_lay_slow;test_mid;test_top;', 'five cases')
        ! count: contiguous blocks of the sorted list, balanced.
        parts(1) = listing(layered, [character(len=160) :: '--all', '--partition', &
            'count:1/2'])
        parts(2) = listing(layered, [character(len=160) :: '--all', '--partition', &
            'count:2/2'])
        call assert_equal_string(trim(parts(1))//trim(parts(2)), full, &
            'count partitions cover the list in order')
        call assert_true(len_trim(parts(1)) > 0 .and. len_trim(parts(2)) > 0, &
            'count partitions are nonempty')
        call assert_equal_string(trim(parts(1)), listing(layered, &
            [character(len=160) :: '--all', '--partition=count:1/2']), &
            'count partition is stable')
        ! hash: disjoint and complete over three partitions.
        joined = ''
        do k = 1, 3
            write (parts(k), '(a)') listing(layered, [character(len=160) :: '--all', &
                '--partition', 'hash:'//digit(k)//'/3'])
            joined = joined//trim(parts(k))
        end do
        n = 0
        n = count([(index(joined, trim(names(k))) > 0, k=1, 5)])
        call assert_equal_integer(n, 5, 'hash partitions cover every case')
        call assert_equal_integer(len(joined), len(full), &
            'hash partitions are disjoint (no case twice)')
        call fo_in(layered, [character(len=160) :: 'test', '--partition', 'hash:4/3', &
            '--list'], result)
        call assert_true(result%exit_code /= 0, 'partition index beyond N is rejected')
        call fo_in(layered, [character(len=160) :: 'test', '--partition', 'mod:1/3'], &
            result)
        call assert_true(result%exit_code /= 0, 'unknown partition kind is rejected')
        ! Running a partition runs exactly its cases.
        call fo_in(layered, [character(len=160) :: 'test', '--partition', 'count:2/2', &
            '--all', '--rerun', '--verbose'], result)
        call assert_equal_integer(result%exit_code, 0, 'partition run passes: '// &
            result%stdout//result%stderr)
        call assert_not_contains(result%stdout, 'test_base', 'other partition not run')
        call assert_contains(result%stdout, 'test_top', 'own partition runs')
    end subroutine check_partitions

    function digit(k) result(text)
        integer, intent(in) :: k
        character(len=1) :: text

        write (text, '(i1)') k
    end function digit

    subroutine check_failed_and_junit()
        type(process_result_t) :: result
        character(:), allocatable :: xml, path

        call assert_equal_string(listing(failing, [character(len=160) :: '-E', &
            'failed()']), '', 'failed() without a recorded run selects nothing')
        call fo_in(failing, [character(len=160) :: 'test', '--rerun'], result)
        call assert_true(result%exit_code /= 0, 'the failing case fails the run')
        call assert_equal_string(listing(failing, [character(len=160) :: '-E', &
            'failed()']), 'case_bad;', 'failed() selects the last run failures')
        call assert_equal_string(listing(failing, [character(len=160) :: '-E', &
            '!failed()']), 'case_ok;case_other;', '!failed() selects the rest')

        path = join_path(scratch, 'report.xml')
        call fo_in(failing, [character(len=160) :: 'test', '--junit', path], result)
        call assert_true(result%exit_code /= 0, 'junit run keeps the failing exit code')
        xml = read_text(path)
        call assert_contains(xml, '<?xml version="1.0" encoding="UTF-8"?>', 'XML header')
        call assert_contains(xml, '<testsuites tests="3" failures="1" errors="0" '// &
            'skipped="0"', 'suite counts: '//xml)
        call assert_contains(xml, 'name="case_ok"', 'a testcase per case (case_ok)')
        call assert_contains(xml, 'name="case_other"', 'a testcase per case')
        call assert_contains(xml, '<failure message=', 'the failure element')
        call assert_contains(xml, 'markup &lt;b&gt; &amp; &quot;quotes&quot; '// &
            '&apos;apos&apos; ]]&gt; end', 'markup is escaped: '//xml)
        call assert_equal_integer(index(xml, achar(7)), 0, 'no raw bell character')
        call assert_equal_integer(index(xml, achar(27)), 0, 'no raw escape character')
        call assert_contains(xml, 'bell:?|esc:?|ok', 'forbidden characters become ?')
        call assert_contains(xml, 'name="case_ok" time=', 'passing case has a time')

        call fo_in(failing, [character(len=160) :: 'test', '-E', '!failed()', '--junit', &
            path], result)
        call assert_equal_integer(result%exit_code, 0, 'filtered run passes')
        xml = read_text(path)
        call assert_contains(xml, 'reused', 'a reused case is in the report: '//xml)
        call assert_contains(xml, '<testsuites tests="2" failures="0"', &
            'filtered suite counts')
        call fo_in(failing, [character(len=160) :: 'test', '-E', 'name(none_*)', &
            '--junit', path], result)
        call assert_true(result%exit_code /= 0, 'no match still fails')
    end subroutine check_failed_and_junit

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
        call assert_equal_integer(result%exit_code, 0, 'git '//text//': '//result%stderr)
    end subroutine git

    subroutine check_affected()
        call put(delta, 'fpm.toml', 'name = "dlt"'//nl)
        call put(delta, '.gitignore', 'build/'//nl)
        call put(delta, 'src/mod_a.f90', 'module mod_a'//nl//'end module'//nl)
        call put(delta, 'test/use_a.f90', 'program use_a'//nl//'use mod_a'//nl// &
            'end program'//nl)
        call put(delta, 'test/solo.f90', 'program solo'//nl//'end program'//nl)
        call git(delta, 'init --quiet')
        call git(delta, 'add -A')
        call git(delta, '-c user.name=T -c user.email=t@example.invalid '// &
            '-c commit.gpgsign=false commit --quiet -m base')
        call assert_equal_string(listing(delta, [character(len=160) :: '-E', &
            'affected()']), '', 'a clean tree affects no case')
        call put(delta, 'src/mod_a.f90', 'module mod_a'//nl//'integer :: x'//nl// &
            'end module'//nl)
        call assert_equal_string(listing(delta, [character(len=160) :: '-E', &
            'affected()']), 'use_a;', 'affected() selects the cases the edit reaches')
        call assert_equal_string(listing(delta, [character(len=160) :: '-E', &
            '!affected()']), 'solo;', '!affected() selects the others')
        call assert_equal_string(listing(delta, [character(len=160) :: '-E', &
            'affected() | name(solo)']), 'solo;use_a;', 'affected() combines with |')
    end subroutine check_affected

    subroutine check_mcp_filter()
        type(json_value_t), allocatable :: output(:)
        type(process_result_t) :: child
        character(:), allocatable :: messages, text
        type(json_value_t) :: result_object

        messages = mcp_encode(mcp_request(1, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}'), .false.)
        messages = messages//mcp_encode(mcp_call(2, &
            '{"action":"test","dir":'//mcp_quote(layered)// &
            ',"filter":"name(test_free) | physics","json":"full","rerun":true}'), &
            .false.)
        messages = messages//mcp_encode(mcp_call(3, &
            '{"action":"test","dir":'//mcp_quote(layered)//',"filter":"name(("}'), &
            .false.)
        messages = messages//mcp_encode(mcp_request(4, 'shutdown'), .false.)
        call mcp_exchange(driver, layered, cache, messages, .false., output, child, env)
        result_object = json_member(output(2), 'result')
        text = json_string_value(json_member(json_element( &
            json_member(result_object, 'content'), 1), 'text'))
        call assert_contains(text, 'test_free', 'MCP filter runs test_free: '//text)
        call assert_contains(text, 'test_mid', 'MCP filter runs test_mid')
        call assert_not_contains(text, 'test_base', 'MCP filter skips test_base')
        result_object = json_member(output(3), 'result')
        text = json_string_value(json_member(json_element( &
            json_member(result_object, 'content'), 1), 'text'))
        call assert_contains(text, 'invalid filter', 'MCP reports a bad filter: '//text)
    end subroutine check_mcp_filter

end program test_test_filter_cli
