! Slow correctness: a 270-case native suite takes minutes to build, so the
! _slow suffix keeps it under fo test --all. It checks a suite past both former
! limits (256 entries, 16 KB of output) with diagnostics from failing cases.
program test_test_json_scale_slow
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, make_directory
    use fo_test_harness, only: remove_tree
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_cli, only: resolve_driver, run_arguments, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_number_value, json_number
    implicit none

    integer, parameter :: n_cases = 270
    integer, parameter :: n_diagnostic_lines = 25
    integer, parameter :: fail_stride = 10
    integer, parameter :: suite_timeout_ms = 1200000
    character(len=*), parameter :: stem = 'test_scale_'
    character(:), allocatable :: driver, scratch, child_tmp, cache
    character(:), allocatable :: name, output, expected_status, source
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    type(json_value_t) :: report, tests, entry, field, summary
    logical :: seen(n_cases)
    integer :: i, j, index_value

    call resolve_driver(driver)
    call make_scratch('fo-test-json-scale', scratch)
    child_tmp = join_path(scratch, 'tmp')
    cache = join_path(scratch, 'fo-cache')
    call make_directory(child_tmp)
    call make_directory(join_path(scratch, 'test'))
    call write_text(join_path(scratch, 'fpm.toml'), 'name = "json_scale_probe"')
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
    call list_add(arguments, '--json')
    call run_arguments(driver, arguments, scratch, result, environment, &
        suite_timeout_ms)
    call assert_equal_integer(result%term_signal, 0, &
        'suite JSON command exits normally')
    call assert_equal_integer(result%exit_code, 1, &
        'suite with failing cases exits nonzero')
    call assert_true(len(result%stdout) > 16384, &
        'suite JSON exceeds the former 16 KB limit')
    call parse_json_report(result, report, 'scale suite JSON report')

    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), n_cases, &
        'every selected case has exactly one JSON entry')
    seen = .false.
    do j = 1, json_size(tests)
        entry = json_element(tests, j)
        name = json_string_value(json_member(entry, 'name'))
        index_value = case_index(name, stem)
        if (index_value < 1 .or. index_value > n_cases) then
            call assert_true(.false., 'every JSON entry names a selected case')
            cycle
        end if
        call assert_true(.not. seen(index_value), &
            'each selected case is reported once')
        seen(index_value) = .true.
        if (mod(index_value, fail_stride) == 0) then
            expected_status = 'fail'
        else
            expected_status = 'pass'
        end if
        call assert_equal_string(json_string_value(json_member(entry, 'status')), &
            expected_status, 'outcome of each case survives the report')
        field = json_member(entry, 'seconds')
        call assert_equal_integer(field%kind, json_number, &
            'duration of each case is a JSON number')
        output = json_string_value(json_member(entry, 'output'))
        if (mod(index_value, fail_stride) == 0) then
            call assert_equal_integer(count_text(output, &
                'DIAG ' // name // ' line '), n_diagnostic_lines, &
                'every diagnostic line of a failing case is retained')
            call assert_true(index(output, &
                'DIAG ' // name // ' line 25 ') > 0, &
                'the last diagnostic line of a failing case is retained')
            call assert_true(index(output, 'DIAG ' // name // ' long ' // &
                repeat('L', 3000) // ' end') > 0, &
                'a diagnostic line longer than the reader chunk survives')
            call assert_true(index(output, 'DIAG ' // name // ' utf8 ' // &
                char(195) // char(169) // ' end') > 0, &
                'UTF-8 diagnostic bytes survive the JSON round trip')
            call assert_true(index(output, 'DIAG ' // name // ' esc ' // &
                char(9) // char(92) // char(34)) > 0, &
                'tab, backslash and quote survive the JSON round trip')
        else
            call assert_equal_integer(len(output), 0, &
                'a passing case carries no diagnostic output')
        end if
    end do
    call assert_equal_integer(count(seen), n_cases, &
        'the report covers every selected case without loss')

    summary = json_member(report, 'summary')
    call assert_equal_integer(nint(json_number_value( &
        json_member(summary, 'passed'))), n_cases - n_cases / fail_stride, &
        'passed count is exact')
    call assert_equal_integer(nint(json_number_value( &
        json_member(summary, 'failed'))), n_cases / fail_stride, &
        'failed count is exact, including the late failure')
    call assert_equal_integer(nint(json_number_value( &
        json_member(summary, 'skipped'))), 0, 'skipped count is exact')
    call assert_true(nint(json_number_value( &
        json_member(report, 'exit_code'))) /= 0, &
        'report keeps a nonzero exit code for the failing suite')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a,i0,a,i0,a)') 'test-json-scale: ', n_cases, ' cases, ', &
        len(result%stdout), ' bytes of JSON with exact counts and diagnostics'

contains

    function three_digits(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=3) :: buffer

        write (buffer, '(i3.3)') value
        text = buffer
    end function three_digits

    function case_index(name, prefix) result(value)
        character(len=*), intent(in) :: name, prefix
        integer :: value
        integer :: ios

        value = 0
        if (len(name) /= len(prefix) + 3) return
        if (name(1:len(prefix)) /= prefix) return
        read (name(len(prefix) + 1:), '(i3)', iostat=ios) value
        if (ios /= 0) value = 0
    end function case_index

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
            "' quote:""x"" ', repeat('x', 40)" // new_line('a') // &
            '    end do' // new_line('a') // &
            "    write (*, '(a)') 'DIAG " // name // " long ' // " // &
            "repeat('L', 3000) // ' end'" // new_line('a') // &
            "    write (*, '(a)') 'DIAG " // name // " utf8 ' // char(195) // " // &
            "char(169) // ' end'" // new_line('a') // &
            "    write (*, '(a)') 'DIAG " // name // " esc ' // char(9) // " // &
            "char(92) // char(34)" // new_line('a') // &
            '    error stop 10' // new_line('a') // &
            'end program ' // name // new_line('a')
    end function failing_program

end program test_test_json_scale_slow
