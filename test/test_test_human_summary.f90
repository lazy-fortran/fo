program test_test_human_summary
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, make_directory
    use fo_test_harness, only: remove_tree
    use fo_test_harness, only: assert_equal_integer
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_cli, only: resolve_driver, run_arguments
    implicit none

    character(:), allocatable :: driver, scratch, child_tmp, cache
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment

    call resolve_driver(driver)
    call make_scratch('fo-test-human-summary', scratch)
    child_tmp = join_path(scratch, 'tmp')
    cache = join_path(scratch, 'fo-cache')
    call make_directory(child_tmp)
    call make_directory(join_path(scratch, 'test'))
    call write_text(join_path(scratch, 'fpm.toml'), 'name = "human_summary_probe"')
    call write_text(join_path(join_path(scratch, 'test'), 'test_summary_flaky.f90'), &
        'program test_summary_flaky'//new_line('a')// &
        '    implicit none'//new_line('a')// &
        '    logical :: seen'//new_line('a')// &
        '    integer :: unit, ios'//new_line('a')// &
        "    inquire (file='first-attempt.marker', exist=seen)"//new_line('a')// &
        '    if (.not. seen) then'//new_line('a')// &
        "        open (newunit=unit, file='first-attempt.marker', status='new', " // &
        "iostat=ios)"//new_line('a')// &
        '        if (ios == 0) close (unit)'//new_line('a')// &
        "        write (*, '(a)') 'FIRST_ATTEMPT_DIAGNOSTIC'"//new_line('a')// &
        '        error stop 23'//new_line('a')// &
        '    end if'//new_line('a')// &
        'end program test_summary_flaky'//new_line('a'))
    call write_text(join_path(join_path(scratch, 'test'), 'test_summary_pass.f90'), &
        'program test_summary_pass'//new_line('a')// &
        '    implicit none'//new_line('a')// &
        "    write (*, '(a)') 'PASS_OUTPUT'"//new_line('a')// &
        'end program test_summary_pass'//new_line('a'))

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
        'summary run exits normally')
    call assert_equal_integer(result%exit_code, 0, &
        'a flaky test that passes its retry leaves the suite green')
    call assert_true(index(result%stdout, ' FLAKY ') > 0, &
        'the flaky case keeps its FLAKY status line')
    call assert_true(index(result%stdout, 'attempts: first failed, retry passed') > &
        0, 'the flaky attempts are described')
    call assert_true(index(result%stdout, &
        'Summary: 2 passed, 0 failed, 0 skipped (') > 0, &
        'summary mode still prints the exact pass, fail and skip counts')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a,i0,a)') 'test-human-summary: ', len(result%stdout), &
        ' bytes with flaky status and exact summary counts'
end program test_test_human_summary
