program test_ctest_native_many_fixtures_cli
    !! A case that requires four fixtures. CTest expands every fixture a case
    !! requires into its DEPENDS property, so fo sees the same predecessor
    !! twice, once through DEPENDS and once through the fixture match, and its
    !! predecessor list has to hold both sources without being written past.
    use fo_test_harness, only: process_result_t, string_list_t, list_add, &
        make_scratch, join_path, write_lines, read_text, remove_tree, &
        run_process, assert_true, assert_equal_string, assert_equal_integer, &
        assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size, &
        json_string_value
    implicit none
    character(len=5), parameter :: NAMES(*) = [character(len=5) :: 's1', 's2', &
        's3', 's4', 'req', 'clean']
    character(:), allocatable :: driver, scratch, build, order
    type(string_list_t) :: args, env
    type(process_result_t) :: observed
    type(json_value_t) :: report, tests
    integer :: k

    call resolve_driver(driver)
    call make_scratch('fo-ctest-fan', scratch)
    build = join_path(scratch, 'build')
    call write_fixture(scratch)
    call list_add(env, 'FO_BACKEND=cmake')
    call list_add(env, 'FO_JOBS=4')

    args = string_list_t()
    call list_add(args, 'test')
    call list_add(args, 'req')
    call list_add(args, '--json=full')
    call run_fo(driver, args, scratch, join_path(scratch, 'fo-cache'), observed, env)
    call assert_process_ok(observed, 'four fixture predecessors of one case run')
    call parse_json_report(observed, report, 'CTest report for req')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), size(NAMES), &
        'the four setups, the requirer and the cleanup ran')
    do k = 1, size(NAMES)
        call assert_equal_string(status_of(tests, trim(NAMES(k))), 'pass', &
            'verdict of '//trim(NAMES(k)))
    end do

    order = read_text(join_path(build, 'order.log'))
    do k = 1, size(NAMES)
        call assert_equal_integer(runs(order, trim(NAMES(k))), 1, &
            trim(NAMES(k))//' ran exactly once')
    end do
    do k = 1, 4
        call assert_true(first(order, trim(NAMES(k))) < first(order, 'req'), &
            trim(NAMES(k))//' runs before its requirer')
    end do
    call assert_true(first(order, 'req') < first(order, 'clean'), &
        'the cleanup runs after the case it cleans up')

    ! Real ctest on the same build tree reaches the same verdicts.
    args = string_list_t()
    call list_add(args, 'ctest')
    call list_add(args, '--test-dir')
    call list_add(args, build)
    call run_process(args, scratch, observed)
    call assert_process_ok(observed, 'ctest agrees that the fan-in passes')
    call remove_tree(scratch)
    call finish_assertions()
contains
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

    integer function first(log, name) result(position)
        character(len=*), intent(in) :: log, name

        position = index(log, name//char(10))
    end function first

    integer function runs(log, name) result(count)
        !! How often a case wrote its name to the order log.
        character(len=*), intent(in) :: log, name
        integer :: start

        count = 0
        start = index(log, name//char(10))
        do while (start > 0)
            count = count + 1
            start = index(log(start + 1:), name//char(10))
        end do
    end function runs

    subroutine write_fixture(dir)
        character(len=*), intent(in) :: dir

        call write_lines(join_path(dir, 'CMakeLists.txt'), [character(len=200) :: &
            'cmake_minimum_required(VERSION 3.22)', &
            'project(fixture_fan NONE)', &
            'enable_testing()', &
            'foreach(n s1 s2 s3 s4 req clean)', &
            '  add_test(NAME ${n} COMMAND sh -c "echo ${n} >> ${CMAKE_BINARY_DIR}/order.log")', &
            'endforeach()', &
            'set_tests_properties(s1 PROPERTIES FIXTURES_SETUP f1)', &
            'set_tests_properties(s2 PROPERTIES FIXTURES_SETUP f2)', &
            'set_tests_properties(s3 PROPERTIES FIXTURES_SETUP f3)', &
            'set_tests_properties(s4 PROPERTIES FIXTURES_SETUP f4)', &
            'set_tests_properties(req PROPERTIES FIXTURES_REQUIRED "f1;f2;f3;f4")', &
            'set_tests_properties(clean PROPERTIES FIXTURES_CLEANUP "f1;f2;f3;f4")'])
    end subroutine write_fixture
end program test_ctest_native_many_fixtures_cli
