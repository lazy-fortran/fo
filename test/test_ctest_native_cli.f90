program test_ctest_native_cli
    !! Native CTest execution (#194, #195): the verdicts of cases that fo runs
    !! from `ctest --show-only=json-v1` properties equal real ctest's on the
    !! same build tree, the ordering/exclusion constraints hold, and ctest is
    !! used only for the case that needs a property fo does not implement.
    use fo_test_harness, only: process_result_t, string_list_t, list_add, &
        make_scratch, join_path, write_lines, read_text, remove_tree, &
        run_process, assert_true, assert_equal_string, assert_equal_integer, &
        assert_contains, assert_not_contains, assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size, &
        json_string_value
    implicit none
    character(len=*), parameter :: DELEGATED = 'deleg...ated'
    character(len=16), parameter :: NAMES(*) = [character(len=16) :: &
        'n_pass', 'n_fail', 'n_willfail', 'n_pass_re_ok', 'n_pass_re_miss', &
        'n_fail_re', 'n_fail_re_wf', 'n_skip_code', 'n_skip_re', 'n_disabled', &
        'n_files_missing', 'n_files_ok', 'n_timeout', 'n_env', 'n_cwd', &
        'fx_setup', 'fx_req', 'fx_cleanup', 'bad_setup', 'blocked_req', &
        'bad_cleanup', 'dep_z', 'dep_a', 'lock_a', 'lock_b', 'ser', 'wide']
    character(len=12), parameter :: EXPECTED(*) = [character(len=12) :: &
        'pass', 'fail', 'pass', 'pass', 'fail', 'fail', 'pass', 'skip', 'skip', &
        'skip', 'untested', 'pass', 'timeout', 'pass', 'pass', 'pass', 'pass', &
        'pass', 'fail', 'untested', 'pass', 'pass', 'pass', 'pass', 'pass', &
        'pass', 'pass']
    character(:), allocatable :: driver, scratch, build, order, last_test, xml
    type(string_list_t) :: args, env
    type(process_result_t) :: observed
    type(json_value_t) :: report, tests
    integer :: k

    call resolve_driver(driver)
    call make_scratch('fo-ctest-native', scratch)
    build = join_path(scratch, 'build')
    call write_fixture(scratch)
    call list_add(env, 'FO_BACKEND=cmake')
    call list_add(env, 'FO_JOBS=4')

    ! All cases through fo. Everything but DELEGATED runs natively.
    call fo_test('--all', observed, report)
    tests = json_member(report, 'tests')
    order = read_text(join_path(build, 'order.log'))
    last_test = read_text(join_path(build, 'Testing/Temporary/LastTest.log'))
    call assert_equal_integer(json_size(tests), size(NAMES) + 2, &
        'every registered case is reported once')
    do k = 1, size(NAMES)
        call assert_equal_string(status_of(tests, trim(NAMES(k))), &
            trim(EXPECTED(k)), 'native verdict of '//trim(NAMES(k)))
    end do
    call assert_equal_string(status_of(tests, DELEGATED), 'pass', &
        'delegated case keeps its full name and verdict')
    call assert_contains(last_test, DELEGATED, 'ctest ran the unsupported case')
    call assert_equal_string(status_of(tests, 'semi_env'), 'pass', &
        'an ENVIRONMENT element holding a semicolon is delegated and kept whole')
    call assert_contains(last_test, 'semi_env', 'ctest ran the semicolon case')
    do k = 1, size(NAMES)
        call assert_not_contains(last_test, trim(NAMES(k))//char(10), &
            'ctest did not run '//trim(NAMES(k)))
    end do
    call check_order(order)

    ! Real ctest on the same build tree reaches the same verdicts.
    call list_add(args, 'ctest')
    call list_add(args, '--test-dir')
    call list_add(args, build)
    call list_add(args, '--output-junit')
    xml = join_path(scratch, 'ctest.xml')
    call list_add(args, xml)
    call run_process(args, scratch, observed)
    xml = read_text(xml)
    call assert_true(len(xml) > 0, 'real ctest produced a JUnit report')
    do k = 1, size(NAMES)
        call assert_equal_string(status_of(tests, trim(NAMES(k))), &
            ctest_verdict(xml, trim(NAMES(k))), 'fo agrees with ctest on '// &
            trim(NAMES(k)))
    end do

    ! A named fixture requirer pulls in its setup and cleanup as ctest does.
    call fo_test('fx_req', observed, report)
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), 3, 'fixture cases are pulled in')
    call assert_equal_string(status_of(tests, 'fx_setup'), 'pass', 'pulled-in setup ran')
    call assert_equal_string(status_of(tests, 'fx_cleanup'), 'pass', &
        'pulled-in cleanup ran')
    call remove_tree(scratch)
    call finish_assertions()
contains
    subroutine fo_test(selector, result, doc)
        character(len=*), intent(in) :: selector
        type(process_result_t), intent(out) :: result
        type(json_value_t), intent(out) :: doc

        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, selector)
        call list_add(args, '--json=full')
        call run_fo(driver, args, scratch, join_path(scratch, 'fo-cache'), result, env)
        call parse_json_report(result, doc, 'CTest report for '//selector)
        args = string_list_t()
    end subroutine fo_test

    function status_of(list, name) result(status)
        type(json_value_t), intent(in) :: list
        character(len=*), intent(in) :: name
        character(:), allocatable :: status
        integer :: i

        status = 'missing'
        do i = 1, json_size(list)
            if (json_string_value(json_member(json_element(list, i), 'name')) &
                == name) status = json_string_value(json_member( &
                json_element(list, i), 'status'))
        end do
    end function status_of

    function ctest_verdict(text, name) result(verdict)
        !! Verdict of a case in a ctest JUnit file, as fo spells it.
        character(len=*), intent(in) :: text, name
        character(:), allocatable :: verdict, item
        integer :: first, last

        verdict = 'missing'
        first = index(text, '<testcase name="'//name//'"')
        if (first == 0) return
        last = first + index(text(first:), '</testcase>')
        item = text(first:last)
        if (index(item, 'status="run"') > 0) then
            verdict = 'pass'
        else if (index(item, 'status="disabled"') > 0) then
            verdict = 'skip'
        else if (index(item, 'status="fail"') > 0) then
            verdict = 'fail'
            if (index(item, 'message="Timeout"') > 0) verdict = 'timeout'
        else if (index(item, 'message="SKIP_') > 0) then
            verdict = 'skip'
        else
            verdict = 'untested'
        end if
    end function ctest_verdict

    subroutine check_order(log)
        character(len=*), intent(in) :: log

        call assert_true(first(log, 'fx_setup') < first(log, 'fx_req'), &
            'fixture setup runs before its requirer')
        call assert_true(first(log, 'fx_req') < first(log, 'fx_cleanup'), &
            'fixture cleanup runs after its requirer')
        call assert_true(first(log, 'bad_setup') < first(log, 'bad_cleanup'), &
            'cleanup of a failed setup still runs after it')
        call assert_equal_integer(first(log, 'blocked_req'), 0, &
            'a requirer of a failed setup never starts')
        call assert_true(first(log, 'dep_a') < first(log, 'dep_z'), &
            'DEPENDS orders dep_a before dep_z')
    end subroutine check_order

    integer function first(log, name) result(position)
        character(len=*), intent(in) :: log, name

        position = index(log, name//char(10))
    end function first

    subroutine write_fixture(dir)
        character(len=*), intent(in) :: dir
        character(len=*), parameter :: lock = '"mkdir L && sleep 0.3 && rmdir L"'

        call write_lines(join_path(dir, 'CMakeLists.txt'), [character(len=200) :: &
            'cmake_minimum_required(VERSION 3.22)', &
            'project(native_ctest NONE)', &
            'enable_testing()', &
            'file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/wd")', &
            'function(fo_case name body)', &
            '  add_test(NAME ${name} COMMAND sh -c "echo ${name} >> '// &
            "'${CMAKE_BINARY_DIR}/order.log' && ${body}"//'")', &
            'endfunction()', &
            'fo_case(n_pass true)', &
            'fo_case(n_fail "exit 3")', &
            'fo_case(n_willfail false)', &
            'fo_case(n_pass_re_ok "echo hello; exit 5")', &
            'fo_case(n_pass_re_miss "echo hello")', &
            'fo_case(n_fail_re "echo BOOM")', &
            'fo_case(n_fail_re_wf "echo BOOM")', &
            'fo_case(n_skip_code "exit 7")', &
            'fo_case(n_skip_re "echo skip-me")', &
            'fo_case(n_disabled true)', &
            'fo_case(n_files_missing true)', &
            'fo_case(n_files_ok true)', &
            'fo_case(n_timeout "sleep 5")', &
            'fo_case(n_env [=[test "$FO_PROP" = "a b"]=])', &
            'fo_case(n_cwd [=[test "$(basename "$PWD")" = wd]=])', &
            'fo_case(fx_setup true)', &
            'fo_case(fx_req true)', &
            'fo_case(fx_cleanup true)', &
            'fo_case(bad_setup "exit 1")', &
            'fo_case(blocked_req true)', &
            'fo_case(bad_cleanup true)', &
            'fo_case(dep_z true)', &
            'fo_case(dep_a true)', &
            'fo_case(lock_a '//lock//')', &
            'fo_case(lock_b '//lock//')', &
            'fo_case(ser '//lock//')', &
            'fo_case(wide true)', &
            'fo_case(deleg...ated [=[test "$FO_X" = 1]=])', &
            'fo_case(semi_env [=[test "$FO_SEMI" = "1;2"]=])', &
            'set_tests_properties(n_willfail PROPERTIES WILL_FAIL TRUE)', &
            'set_tests_properties(n_pass_re_ok PROPERTIES PASS_REGULAR_EXPRESSION "hel+o")', &
            'set_tests_properties(n_pass_re_miss PROPERTIES PASS_REGULAR_EXPRESSION "absent")', &
            'set_tests_properties(n_fail_re PROPERTIES FAIL_REGULAR_EXPRESSION "BO+M")', &
            'set_tests_properties(n_fail_re_wf PROPERTIES FAIL_REGULAR_EXPRESSION "BO+M" '// &
            'WILL_FAIL TRUE)', &
            'set_tests_properties(n_skip_code PROPERTIES SKIP_RETURN_CODE 7)', &
            'set_tests_properties(n_skip_re PROPERTIES SKIP_REGULAR_EXPRESSION "skip-m.")', &
            'set_tests_properties(n_disabled PROPERTIES DISABLED TRUE)', &
            'set_tests_properties(n_files_missing PROPERTIES REQUIRED_FILES '// &
            '"${CMAKE_BINARY_DIR}/missing")', &
            'set_tests_properties(n_files_ok PROPERTIES REQUIRED_FILES '// &
            '"${CMAKE_SOURCE_DIR}/CMakeLists.txt")', &
            'set_tests_properties(n_timeout PROPERTIES TIMEOUT 1)', &
            'set_tests_properties(n_env PROPERTIES ENVIRONMENT "FO_PROP=a b;FO_OTHER=x")', &
            'set_tests_properties(n_cwd PROPERTIES WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/wd")', &
            'set_tests_properties(fx_setup PROPERTIES FIXTURES_SETUP f1)', &
            'set_tests_properties(fx_req PROPERTIES FIXTURES_REQUIRED f1)', &
            'set_tests_properties(fx_cleanup PROPERTIES FIXTURES_CLEANUP f1)', &
            'set_tests_properties(bad_setup PROPERTIES FIXTURES_SETUP f2)', &
            'set_tests_properties(blocked_req PROPERTIES FIXTURES_REQUIRED f2)', &
            'set_tests_properties(bad_cleanup PROPERTIES FIXTURES_CLEANUP f2)', &
            'set_tests_properties(dep_z PROPERTIES DEPENDS dep_a)', &
            'set_tests_properties(lock_a lock_b PROPERTIES RESOURCE_LOCK L)', &
            'set_tests_properties(ser PROPERTIES RUN_SERIAL TRUE)', &
            'set_tests_properties(wide PROPERTIES PROCESSORS 64)', &
            'set_tests_properties(deleg...ated PROPERTIES ENVIRONMENT_MODIFICATION '// &
            '"FO_X=set:1")', &
            'set_tests_properties(semi_env PROPERTIES ENVIRONMENT "FO_SEMI=1\;2")'])
    end subroutine write_fixture
end program test_ctest_native_cli
