program test_test_driver_env
    !! fo test passes its own resolved image to test children as FO/FO_BIN,
    !! and keeps an explicit caller choice.
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, join_path, write_text, read_text, assert_process_ok, &
        assert_equal_string, finish_assertions
    use fo_test_cli, only: resolve_driver, run_external
    implicit none

    character(:), allocatable :: driver, scratch, project, marker, expected
    type(string_list_t) :: arguments
    type(process_result_t) :: child

    call resolve_driver(driver)
    call make_scratch('fo-test-driver-env', scratch)
    project = join_path(scratch, 'project')
    marker = join_path(scratch, 'seen.txt')
    call write_text(join_path(project, 'fpm.toml'), 'name = "driver_env"'// &
        new_line('a'))
    call write_text(join_path(project, 'test/test_seen.f90'), &
        'program test_seen'//new_line('a')// &
        'character(len=4096) :: fo, fo_bin, path'//new_line('a')// &
        'integer :: u'//new_line('a')// &
        'call get_environment_variable("FO", fo)'//new_line('a')// &
        'call get_environment_variable("FO_BIN", fo_bin)'//new_line('a')// &
        'call get_environment_variable("DRIVER_ENV_MARKER", path)'//new_line('a')// &
        'open(newunit=u, file=trim(path), status="replace")'//new_line('a')// &
        'write(u, "(a)") trim(fo)'//new_line('a')// &
        'write(u, "(a)") trim(fo_bin)'//new_line('a')// &
        'close(u)'//new_line('a')// &
        'end program test_seen'//new_line('a'))

    ! Independent oracle for the running image: the realpath utility.
    call list_add(arguments, driver)
    call run_external('realpath', arguments, scratch, child)
    call assert_process_ok(child, 'realpath resolves the driver under test')
    expected = trim(child%stdout)
    if (len(expected) > 0) then
        if (expected(len(expected):) == new_line('a')) &
            expected = expected(:len(expected) - 1)
    end if

    call run_driver_test([character(len=6) :: '-u', 'FO', '-u', 'FO_BIN'])
    call assert_equal_string(read_text(marker), &
        expected//new_line('a')//expected//new_line('a'), &
        'test children receive the running driver when FO is unset')

    call run_driver_test([character(len=23) :: 'FO=/explicit/fo', &
        'FO_BIN=/explicit/fo-bin'])
    call assert_equal_string(read_text(marker), &
        '/explicit/fo'//new_line('a')//'/explicit/fo-bin'//new_line('a'), &
        'an explicit FO and FO_BIN reach test children unchanged')

    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine run_driver_test(environment_edits)
        character(len=*), intent(in) :: environment_edits(:)
        type(string_list_t) :: command
        type(process_result_t) :: result
        integer :: i

        do i = 1, size(environment_edits)
            call list_add(command, trim(environment_edits(i)))
        end do
        call list_add(command, 'FO_DISABLE_SELF_REFRESH=1')
        call list_add(command, 'FO_SELF_REFRESH=0')
        call list_add(command, 'FO_CACHE_DIR='//join_path(scratch, 'cache'))
        call list_add(command, 'DRIVER_ENV_MARKER='//marker)
        call list_add(command, driver)
        call list_add(command, 'test')
        call list_add(command, 'test_seen')
        call run_external('/usr/bin/env', command, project, result)
        call assert_process_ok(result, 'fo test runs the driver-environment fixture')
    end subroutine run_driver_test

end program test_test_driver_env
