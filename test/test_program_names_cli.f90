program test_program_names_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_lines, write_text
    use fo_test_harness, only: remove_path, remove_tree, assert_equal_integer
    use fo_test_harness, only: assert_equal_string, assert_file_equals, assert_process_ok
    use fo_test_harness, only: assert_true
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value
    use fo_test_harness, only: finish_assertions
    implicit none

    character(len=32), parameter :: names(3) = &
        [character(len=32) :: 'test_first', 'test_second', 'test_third']
    character(len=32), parameter :: apps(2) = &
        [character(len=32) :: 'first_app', 'second_app']
    character(:), allocatable :: driver, scratch, source
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    type(json_value_t) :: report, tests, entry, field
    character(len=192) :: source_lines(15)
    character(:), allocatable :: number
    integer :: i, pass, j, found

    call resolve_driver(driver)
    call make_scratch('fo-program-names', scratch)
    call write_text(join_path(scratch, 'fpm.toml'), 'name = "program_names_probe"' // new_line('a'))
    call write_lines(join_path(scratch, 'src/external_bridge.f90'), &
        [character(len=64) :: 'integer function external_bridge()', &
        'implicit none', 'external_bridge = 40', 'end function external_bridge'])
    call list_add(environment, 'FO_JOBS=4')
    do i = 1, size(names)
        number = integer_text(i)
        source_lines = ''
        source_lines(1) = 'module support_' // number
        source_lines(2) = 'implicit none'
        source_lines(3) = 'contains'
        source_lines(4) = 'integer function value()'
        source_lines(5) = 'value = ' // number
        source_lines(6) = 'end function value'
        source_lines(7) = 'end module support_' // number
        call write_lines(join_path(scratch, 'test/support_' // integer_text(i) // '.f90'), &
            source_lines(1:7))
        source_lines = ''
        source_lines(1) = 'module local_' // number
        source_lines(2) = 'implicit none'
        source_lines(3) = 'end module local_' // number
        source_lines(4) = 'program private_name'
        source_lines(5) = 'use support_' // number // ', only: value'
        source_lines(6) = 'implicit none'
        source_lines(7) = 'interface'
        source_lines(8) = 'integer function external_bridge()'
        source_lines(9) = 'end function external_bridge'
        source_lines(10) = 'end interface'
        source_lines(11) = 'integer :: unit'
        source_lines(12) = "open(newunit=unit, file='" // trim(names(i)) // &
            ".receipt', status='replace')"
        source_lines(13) = "write(unit, '(i0)') value() + external_bridge()"
        source_lines(14) = 'close(unit)'
        source_lines(15) = 'end program private_name'
        call write_lines(join_path(scratch, 'test/' // trim(names(i)) // '.f90'), &
            source_lines)
    end do

    do pass = 1, 2
        do i = 1, size(names)
            call remove_path(join_path(scratch, trim(names(i)) // '.receipt'))
        end do
        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, '--all')
        call list_add(arguments, '--json')
        call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
        call assert_process_ok(result, 'all tests sharing private PROGRAM name')
        call parse_json_report(result, report, 'program-name report')
        tests = json_member(report, 'tests')
        call assert_equal_integer(json_size(tests), size(names), 'three public tests are retained')
        do i = 1, json_size(tests)
            entry = json_element(tests, i)
            field = json_member(entry, 'name')
            found = 0
            do j = 1, size(names)
                if (json_string_value(field) == trim(names(j))) found = j
            end do
            call assert_true(found > 0, 'report contains only the three registered tests')
            field = json_member(entry, 'status')
            call assert_equal_string(json_string_value(field), 'pass', 'shared-name test passes')
        end do
        do i = 1, size(names)
            call assert_file_equals(join_path(scratch, trim(names(i)) // '.receipt'), &
                integer_text(i + 40) // new_line('a'), &
                'distinct public tests run separately with external bridge')
        end do
    end do

    do i = 1, size(names)
        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, trim(names(i)))
        call list_add(arguments, '--json')
        call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
        call assert_process_ok(result, 'named test with private PROGRAM name')
        call parse_json_report(result, report, 'named program-name report')
        tests = json_member(report, 'tests')
        call assert_equal_integer(json_size(tests), 1, 'named selection reports one test')
        entry = json_element(tests, 1)
        field = json_member(entry, 'name')
        call assert_equal_string(json_string_value(field), trim(names(i)), 'exact public test name')
        field = json_member(entry, 'status')
        call assert_equal_string(json_string_value(field), 'pass', 'named test passes')
    end do

    source = join_path(scratch, 'src/app_support.f90')
    call write_text(source, provider_source('integer'))
    do i = 1, size(apps)
        call write_lines(join_path(scratch, 'app/' // trim(apps(i)) // '.f90'), &
            [character(len=160) :: 'program private_app_name', &
            'use app_support, only: value', 'implicit none', &
            "print '(i0)', int(value())", 'end program private_app_name'])
        arguments = string_list_t()
        call list_add(arguments, 'exec')
        call list_add(arguments, trim(apps(i)))
        call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
        call assert_process_ok(result, trim(apps(i)) // ' with original provider interface')
        call assert_equal_string(result%stdout, '1' // new_line('a'), 'integer provider output')
    end do

    call write_text(source, provider_source('real'))
    do i = 1, size(apps)
        arguments = string_list_t()
        call list_add(arguments, 'exec')
        call list_add(arguments, trim(apps(i)))
        call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
        call assert_process_ok(result, trim(apps(i)) // ' after provider interface change')
        call assert_equal_string(result%stdout, '7' // new_line('a'), 'caller recompiles after change')
    end do

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') &
        'program-names-cli: separate public targets sharing one PROGRAM name run cold, warm and named'

contains

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

    function provider_source(result_type) result(text)
        character(len=*), intent(in) :: result_type
        character(:), allocatable :: text

        text = 'module app_support' // new_line('a') // 'implicit none' // new_line('a') // &
            'contains' // new_line('a') // trim(result_type) // ' function value()' // new_line('a')
        if (trim(result_type) == 'integer') then
            text = text // 'value = 1' // new_line('a')
        else
            text = text // 'value = 7.0' // new_line('a')
        end if
        text = text // 'end function value' // new_line('a') // 'end module app_support' // new_line('a')
    end function provider_source

end program test_program_names_cli
