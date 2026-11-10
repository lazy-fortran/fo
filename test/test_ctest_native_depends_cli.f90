program test_ctest_native_depends_cli
    !! A selected consumer needs its transitive DEPENDS producers in the same
    !! execution view; a fresh build tree has none of their artifacts.
    use fo_test_harness, only: process_result_t, string_list_t, list_add, &
        make_scratch, join_path, write_lines, read_text, remove_tree, remove_path, &
        run_process, assert_true, assert_equal_string, assert_equal_integer, &
        assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size, &
        json_string_value
    implicit none
    character(len=8), parameter :: NAMES(*) = [character(len=8) :: &
        'seed', 'producer', 'consumer']
    character(:), allocatable :: driver, scratch, build, order
    type(string_list_t) :: args, env
    type(process_result_t) :: observed
    type(json_value_t) :: report, tests
    integer :: k

    call resolve_driver(driver)
    call make_scratch('fo-ctest-depends', scratch)
    build = join_path(scratch, 'build')
    call write_fixture(scratch)
    call list_add(env, 'FO_BACKEND=cmake')
    call list_add(env, 'FO_JOBS=4')

    args = string_list_t()
    call list_add(args, 'test')
    call list_add(args, 'consumer')
    call list_add(args, '--json=full')
    call run_fo(driver, args, scratch, join_path(scratch, 'fo-cache'), observed, env)
    call assert_process_ok(observed, 'selected consumer runs with its transitive producers')
    call parse_json_report(observed, report, 'CTest report for consumer')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), size(NAMES), &
        'only the consumer and its transitive producers ran')
    do k = 1, size(NAMES)
        call assert_equal_string(status_of(tests, trim(NAMES(k))), 'pass', &
            'verdict of '//trim(NAMES(k)))
    end do

    order = read_text(join_path(build, 'order.log'))
    do k = 1, size(NAMES)
        call assert_equal_integer(runs(order, trim(NAMES(k))), 1, &
            trim(NAMES(k))//' ran exactly once')
    end do
    call assert_true(first(order, 'seed') < first(order, 'producer'), &
        'transitive seed runs before producer')
    call assert_true(first(order, 'producer') < first(order, 'consumer'), &
        'producer runs before consumer')

    ! Start the native authority without artifacts left by fo.
    call remove_path(join_path(build, 'seed.dat'))
    call remove_path(join_path(build, 'product.dat'))
    args = string_list_t()
    call list_add(args, 'ctest')
    call list_add(args, '-R')
    call list_add(args, '^(seed|producer|consumer|missing_dep)$')
    call list_add(args, '--test-dir')
    call list_add(args, build)
    call run_process(args, scratch, observed)
    call assert_process_ok(observed, 'native CTest verifies the registered dependency graph')
    ! Unknown predecessors keep CTest's existing ignore semantics.
    args = string_list_t()
    call list_add(args, 'test')
    call list_add(args, 'missing_dep')
    call list_add(args, '--json=full')
    call run_fo(driver, args, scratch, join_path(scratch, 'fo-cache'), observed, env)
    call assert_process_ok(observed, 'unknown DEPENDS name is ignored as by CTest')
    call parse_json_report(observed, report, 'unknown dependency report')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), 1, 'unknown name adds no case')

    args = string_list_t()
    call list_add(args, 'test')
    call list_add(args, 'cycle_a')
    call list_add(args, '--json=full')
    call run_fo(driver, args, scratch, join_path(scratch, 'fo-cache'), observed, env)
    call assert_true(observed%exit_code /= 0, 'a dependency cycle is rejected')
    call parse_json_report(observed, report, 'cycle report')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), 2, 'both cycle members are reported')
    call assert_equal_string(status_of(tests, 'cycle_a'), 'untested', &
        'first cycle member never runs')
    call assert_equal_string(status_of(tests, 'cycle_b'), 'untested', &
        'second cycle member never runs')
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
        integer :: start, offset

        count = 0
        offset = 1
        do
            start = index(log(offset:), name//char(10))
            if (start == 0) exit
            count = count + 1
            offset = offset + start
        end do
    end function runs

    subroutine write_fixture(dir)
        character(len=*), intent(in) :: dir

        character(len=200), parameter :: cmake_lines(*) = [character(len=200) :: &
            'cmake_minimum_required(VERSION 3.22)', &
            'project(depends_closure NONE)', &
            'enable_testing()', &
            'function(dep_case name body)', &
            '  add_test(NAME ${name} COMMAND sh -c "${body} && echo ${name} >> order.log")', &
            'endfunction()', &
            'dep_case(seed "echo seed > seed.dat")', &
            'dep_case(producer "test -f seed.dat && cp seed.dat product.dat")', &
            'dep_case(consumer "test -f product.dat")', &
            'dep_case(unrelated "exit 1")', &
            'set_tests_properties(producer PROPERTIES DEPENDS seed)', &
            'set_tests_properties(consumer PROPERTIES DEPENDS producer)', &
            'dep_case(missing_dep "true")', &
            'set_tests_properties(missing_dep PROPERTIES DEPENDS absent)', &
            'dep_case(cycle_a "true")', &
            'dep_case(cycle_b "true")', &
            'set_tests_properties(cycle_a PROPERTIES DEPENDS cycle_b)', &
            'set_tests_properties(cycle_b PROPERTIES DEPENDS cycle_a)']

        call write_lines(join_path(dir, 'CMakeLists.txt'), cmake_lines)
    end subroutine write_fixture
end program test_ctest_native_depends_cli
