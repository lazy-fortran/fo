program test_test_human_output
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, make_directory
    use fo_test_harness, only: remove_tree
    use fo_test_harness, only: assert_equal_integer
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_cli, only: resolve_driver, run_arguments
    implicit none

    integer, parameter :: n_cases = 40
    integer, parameter :: n_diagnostic_lines = 25
    integer, parameter :: fail_stride = 4
    character(len=*), parameter :: stem = 'test_human_'
    character(:), allocatable :: driver, scratch, child_tmp, cache
    character(:), allocatable :: name, source
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    integer :: i, n_failing

    n_failing = n_cases / fail_stride
    call resolve_driver(driver)
    call make_scratch('fo-test-human-output', scratch)
    child_tmp = join_path(scratch, 'tmp')
    cache = join_path(scratch, 'fo-cache')
    call make_directory(child_tmp)
    call make_directory(join_path(scratch, 'test'))
    call write_text(join_path(scratch, 'fpm.toml'), 'name = "human_output_probe"')
    do i = 1, n_cases
        name = stem // three_digits(i)
        if (mod(i, fail_stride) == 0) then
            source = failing_program(name)
        else
            source = passing_program(name)
        end if
        call write_text(join_path(join_path(scratch, 'test'), name // '.f90'), &
            source)
    end do

    call list_add(environment, 'TMPDIR=' // child_tmp)
    call list_add(environment, 'FO_TEST_TIMEOUT=120')
    call list_add(environment, 'FO_DISABLE_SELF_REFRESH=1')
    call list_add(environment, 'FO_SELF_REFRESH=0')
    call list_add(environment, 'FO_CACHE_DIR=' // cache)
    call list_add(environment, 'FO=' // driver)
    call list_add(environment, 'FO_BIN=' // driver)
    arguments = string_list_t()
    call list_add(arguments, 'test')
    call run_arguments(driver, arguments, scratch, result, environment, 600000)
    call assert_equal_integer(result%term_signal, 0, &
        'human suite command exits normally')
    call assert_equal_integer(result%exit_code, 1, &
        'human suite with failing cases exits nonzero')
    call assert_true(len(result%stdout) > 16384, &
        'human suite output exceeds the former 16 KB limit')
    call assert_equal_integer(count_text(result%stdout, ' FAIL '), n_failing, &
        'every failing case has a status line, including the late failure')
    do i = 1, n_cases
        if (mod(i, fail_stride) /= 0) cycle
        name = stem // three_digits(i)
        call assert_true(has_status_line(result%stdout, name, 'FAIL'), &
            'each failing case is named with its FAIL status')
        call assert_true(index(result%stdout, &
            'DIAG ' // name // ' line 25 ') > 0, &
            'the last diagnostic line of each failing case is printed')
    end do
    call assert_true(index(result%stdout, 'Summary: 30 passed, 10 failed, ' // &
        '0 skipped (') > 0, 'the summary with exact counts is printed last')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a,i0,a,i0,a)') 'test-human-output: ', n_cases, ' cases, ', &
        len(result%stdout), ' bytes of human output with late failure and summary'

contains

    function three_digits(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=3) :: buffer

        write (buffer, '(i3.3)') value
        text = buffer
    end function three_digits

    integer function count_text(text, pattern) result(total)
        character(len=*), intent(in) :: text, pattern
        integer :: start, offset

        total = 0
        start = 1
        do
            if (start > len(text)) exit
            offset = index(text(start:), pattern)
            if (offset == 0) exit
            total = total + 1
            start = start + offset - 1 + len(pattern)
        end do
    end function count_text

    logical function has_status_line(text, name, status) result(found)
        character(len=*), intent(in) :: text, name, status
        integer :: start, stop

        found = .false.
        start = 1
        do
            if (start > len(text)) exit
            stop = index(text(start:), new_line('a'))
            if (stop == 0) then
                stop = len(text) + 1
            else
                stop = start + stop - 1
            end if
            if (index(text(start:stop - 1), name // ' ') == 1 .and. &
                index(text(start:stop - 1), ' ' // status // ' ') > 0) then
                found = .true.
                return
            end if
            start = stop + 1
        end do
    end function has_status_line

    function passing_program(name) result(source)
        character(len=*), intent(in) :: name
        character(:), allocatable :: source

        source = 'program ' // name // new_line('a') // &
            '    implicit none' // new_line('a') // &
            "    write (*, '(a)') 'PASS " // name // "'" // new_line('a') // &
            'end program ' // name // new_line('a')
    end function passing_program

    function failing_program(name) result(source)
        character(len=*), intent(in) :: name
        character(:), allocatable :: source

        source = 'program ' // name // new_line('a') // &
            '    implicit none' // new_line('a') // &
            '    integer :: k' // new_line('a') // &
            '    do k = 1, 25' // new_line('a') // &
            "        write (*, '(a,i0,a,a)') 'DIAG " // name // " line ', k, " // &
            "' ', repeat('y', 60)" // new_line('a') // &
            '    end do' // new_line('a') // &
            '    error stop 10' // new_line('a') // &
            'end program ' // name // new_line('a')
    end function failing_program

end program test_test_human_output
