program test_dependency_membership_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_symlink, write_text
    use fo_test_harness, only: remove_path, remove_tree, assert_equal_integer
    use fo_test_harness, only: assert_equal_string, assert_file_equals, assert_process_ok
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value
    use fo_test_harness, only: finish_assertions
    implicit none

    character(:), allocatable :: driver, scratch, consumer, dependency
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    type(json_value_t) :: report, tests, entry, field

    call resolve_driver(driver)
    call make_scratch('fo-dependency-membership', scratch)
    consumer = join_path(scratch, 'consumer')
    dependency = join_path(scratch, 'dependency-real')
    call make_symlink(dependency, join_path(scratch, 'dependency'))
    call write_text(join_path(dependency, 'fpm.toml'), 'name = "provider_library"' // new_line('a'))
    call write_text(join_path(dependency, 'src/provider.f90'), provider_source(2, .false.))
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "membership_probe"' // new_line('a') // &
        '[dependencies]' // new_line('a') // &
        'provider_library = { path = "../dependency" }' // new_line('a') // &
        '[extra.fo]' // new_line('a') // 'link = "shared"' // new_line('a') // &
        'pic = "true"' // new_line('a'))
    call write_text(join_path(consumer, 'app/probe.f90'), &
        'program probe' // new_line('a') // &
        'use provider, only: current_value' // new_line('a') // &
        'implicit none' // new_line('a') // &
        "print '(i0)', current_value()" // new_line('a') // 'end program probe' // new_line('a'))
    call write_text(join_path(consumer, 'test/test_probe.f90'), &
        'program test_probe' // new_line('a') // &
        'use provider, only: current_value' // new_line('a') // &
        'implicit none' // new_line('a') // 'integer :: unit' // new_line('a') // &
        "open(newunit=unit, file='probe.receipt', status='replace')" // new_line('a') // &
        "write(unit, '(i0)') current_value()" // new_line('a') // 'close(unit)' // new_line('a') // &
        'end program test_probe' // new_line('a'))
    call list_add(environment, 'FO_JOBS=4')

    call expect_value(2)
    call expect_value(2)
    call write_text(join_path(dependency, 'src/semantic/analyzers/added_context.f90'), &
        'module added_context' // new_line('a') // 'implicit none' // new_line('a') // &
        'contains' // new_line('a') // 'integer function base()' // new_line('a') // &
        'base = 5' // new_line('a') // 'end function base' // new_line('a') // &
        'end module added_context' // new_line('a'))
    call write_text(join_path(dependency, 'src/provider.f90'), provider_source(5, .true.))
    call expect_value(5)
    call expect_value(5)

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') &
        'dependency-membership-cli: uncommitted module under symlinked dependency is built cold/warm'

contains

    function provider_source(value, use_helper) result(source)
        integer, intent(in) :: value
        logical, intent(in) :: use_helper
        character(:), allocatable :: source

        source = 'module provider' // new_line('a')
        if (use_helper) source = source // 'use added_context, only: base' // new_line('a')
        source = source // 'implicit none' // new_line('a') // 'contains' // new_line('a') // &
            'integer function current_value()' // new_line('a')
        if (use_helper) then
            source = source // 'current_value = base()' // new_line('a')
        else
            source = source // 'current_value = ' // integer_text(value) // new_line('a')
        end if
        source = source // 'end function current_value' // new_line('a') // &
            'end module provider' // new_line('a')
    end function provider_source

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

    subroutine expect_value(expected)
        integer, intent(in) :: expected
        character(:), allocatable :: expected_text
        type(json_value_t) :: report, selected_tests, test, status

        expected_text = integer_text(expected)
        arguments = string_list_t()
        call list_add(arguments, 'exec')
        call list_add(arguments, 'probe')
        call run_fo(driver, arguments, consumer, join_path(scratch, 'fo-cache'), result, environment)
        call assert_process_ok(result, 'application uses current dependency behavior')
        call assert_equal_string(result%stdout, expected_text // new_line('a'), &
            'application output follows dependency behavior')
        call remove_path(join_path(consumer, 'probe.receipt'))

        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, '--all')
        call list_add(arguments, '--json')
        call run_fo(driver, arguments, consumer, join_path(scratch, 'fo-cache'), result, environment)
        call assert_process_ok(result, 'dependency test selection')
        call parse_json_report(result, report, 'dependency report')
        selected_tests = json_member(report, 'tests')
        call assert_equal_integer(json_size(selected_tests), 1, 'one dependency test is reported')
        test = json_element(selected_tests, 1)
        status = json_member(test, 'name')
        call assert_equal_string(json_string_value(status), 'test_probe', 'dependency test name')
        status = json_member(test, 'status')
        call assert_equal_string(json_string_value(status), 'pass', 'dependency test passes')
        call assert_file_equals(join_path(consumer, 'probe.receipt'), expected_text // new_line('a'), &
            'dependency test executes current behavior')
    end subroutine expect_value

end program test_dependency_membership_cli
