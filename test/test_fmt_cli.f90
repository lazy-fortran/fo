program test_fmt_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, read_text, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_string, assert_equal_integer
    use fo_test_harness, only: assert_contains, assert_process_ok
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    type :: source_case_t
        character(:), allocatable :: name, source, expected, closing
        logical :: literal = .false.
    end type source_case_t

    character(len=24), parameter :: identifiers(20) = [character(len=24) :: &
        'function', 'subroutine', 'helper_function', 'helper_subroutine', &
        'myfunction', 'mysubroutine', 'a_function', 'a_subroutine', &
        'value1function', 'value1subroutine', 'function_count', 'subroutine_count', &
        'FUNCTION', 'SUBROUTINE', 'helper_FUNCTION', 'helper_SUBROUTINE', &
        'prefix2_function', 'prefix2_subroutine', 'xfunction', 'xsubroutine']
    character(:), allocatable :: driver, scratch, project, formatted, quote, escaped
    character(:), allocatable :: source, expected, line, identifier, digits
    type(source_case_t), allocatable :: cases(:)
    type(process_result_t) :: result
    type(string_list_t) :: command
    integer :: i, row, item, gap, quotient

    call resolve_driver(driver)
    call make_scratch('fo-fmt-cli', scratch)
    project = join_path(scratch, 'project')
    call write_text(join_path(project, 'fpm.toml'), 'name = "format_probe"' // new_line('a'))
    allocate(cases(41))

    do i = 1, size(identifiers)
        row = i
        identifier = trim(identifiers(i))
        digits = integer_text(i)
        cases(row)%name = 'probe_' // digits
        cases(row)%closing = 'end program ' // cases(row)%name
        cases(row)%expected = digits // new_line('a')
        source = 'program ' // cases(row)%name // new_line('a') // 'implicit none' // &
            new_line('a') // 'integer :: ' // identifier // new_line('a') // &
            identifier // ' = ' // digits // new_line('a') // &
            "print '(i0)', " // identifier // new_line('a') // cases(row)%closing // new_line('a')
        cases(row)%source = source
        call write_text(join_path(project, 'app/' // cases(row)%name // '.f90'), source)
    end do

    row = 21
    cases(row)%name = 'typed_function'
    cases(row)%closing = 'end program typed_function'
    cases(row)%expected = ' 7.0' // new_line('a')
    cases(row)%source = 'program typed_function' // new_line('a') // 'implicit none' // &
        new_line('a') // 'integer, parameter :: kind_function = 4' // new_line('a') // &
        "print '(f4.1)', answer()" // new_line('a') // 'contains' // new_line('a') // &
        'real(kind=kind_function ) function answer()' // new_line('a') // &
        'answer = 7.0' // new_line('a') // 'end function answer' // new_line('a') // &
        cases(row)%closing // new_line('a')
    call write_text(join_path(project, 'app/' // cases(row)%name // '.f90'), cases(row)%source)

    do i = 1, 20
        row = 21 + i
        cases(row)%name = 'literal_' // integer_text(i)
        cases(row)%closing = 'end program ' // cases(row)%name
        quote = "'"
        if (mod(i, 2) == 0) quote = '"'
        escaped = quote // quote
        line = '&right!case' // integer_text(i)
        source = 'program ' // cases(row)%name // new_line('a') // 'implicit none' // &
            new_line('a') // 'character(len=80) :: text' // new_line('a') // &
            'text = ' // quote // 'left &' // new_line('a')
        gap = mod(i, 4)
        if (gap == 0) source = source // '! between literal segments' // new_line('a') // new_line('a')
        quotient = mod(i, 3)
        if (quotient == 0) then
            source = source // line // '&' // new_line('a') // '&' // escaped // &
                'quoted' // quote // new_line('a')
        else
            source = source // line // escaped // 'quoted' // quote // new_line('a')
        end if
        source = source // "print '(a)', trim(text)" // new_line('a') // &
            cases(row)%closing // new_line('a')
        expected = 'left right!case' // integer_text(i) // quote // 'quoted' // new_line('a')
        cases(row)%source = source
        cases(row)%expected = expected
        cases(row)%literal = .true.
        call write_text(join_path(project, 'app/' // cases(row)%name // '.f90'), source)
    end do

    do row = 1, size(cases)
        call exercise_program(cases(row))
    end do
    command = words([character(len=16) :: 'fmt'])
    call invoke(project, command, result)
    call assert_process_ok(result, 'format all generated Fortran sources')
    do row = 1, size(cases)
        formatted = read_text(join_path(project, 'app/' // cases(row)%name // '.f90'))
        call assert_contains(formatted, new_line('a') // cases(row)%closing // new_line('a'), &
            cases(row)%name // ': closing scope begins in column one')
        if (cases(row)%literal) then
            call assert_contains(formatted, new_line('a') // "    print '(a)', trim(text)" // &
                new_line('a'), cases(row)%name // ': continued literal leaves one statement scope')
        else if (cases(row)%name == 'typed_function') then
            call assert_contains(formatted, new_line('a') // '        answer = 7.0' // new_line('a'), &
                'typed procedure body opens exactly one scope')
        else
            identifier = trim(identifiers(row))
            line = identifier // ' = ' // integer_text(row)
            call assert_contains(formatted, new_line('a') // '    ' // line // new_line('a'), &
                cases(row)%name // ': assignment does not open a procedure scope')
        end if
        call exercise_program(cases(row))
        cases(row)%source = formatted
    end do
    command = words([character(len=16) :: 'fmt'])
    call invoke(project, command, result)
    call assert_process_ok(result, 'format all sources a second time')
    do row = 1, size(cases)
        formatted = read_text(join_path(project, 'app/' // cases(row)%name // '.f90'))
        call assert_equal_string(formatted, cases(row)%source, &
            cases(row)%name // ': formatter is byte-idempotent')
    end do

    call remove_tree(scratch)
    write(*, '(a)') 'fmt-cli: executable outputs, scope placement and byte idempotence pass for 41 programs'

contains

    function words(items) result(list)
        character(len=*), intent(in) :: items(:)
        type(string_list_t) :: list
        integer :: k

        do k = 1, size(items)
            call list_add(list, trim(items(k)))
        end do
    end function words

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=24) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

    subroutine invoke(directory, arguments, child)
        character(len=*), intent(in) :: directory
        type(string_list_t), intent(in) :: arguments
        type(process_result_t), intent(out) :: child

        call run_fo(driver, arguments, directory, join_path(scratch, '.fo-cache'), child)
    end subroutine invoke

    subroutine exercise_program(test_case)
        type(source_case_t), intent(in) :: test_case
        type(string_list_t) :: command
        type(process_result_t) :: child

        command = words([character(len=64) :: 'exec', test_case%name])
        call invoke(project, command, child)
        call assert_process_ok(child, test_case%name // ': executable runs')
        call assert_equal_string(child%stdout, test_case%expected, &
            test_case%name // ': runtime output remains unchanged')
    end subroutine exercise_program

end program test_fmt_cli
