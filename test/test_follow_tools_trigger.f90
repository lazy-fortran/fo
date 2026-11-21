program test_follow_tools_trigger
    !! fo starts an installed follow-tools.sh in the background when its last
    !! check is older than five minutes, not when it is recent, and never
    !! with FO_NO_FOLLOW=1.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: remove_path, file_exists, sleep_ms, assert_true
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(:), allocatable :: driver, scratch, state
    type(process_result_t) :: result

    call resolve_driver(driver)
    call make_scratch('fo-follow', scratch)
    state = join_path(scratch, 'state')
    call write_text(join_path(state, 'src/fortyard/scripts/follow-tools.sh'), &
        ': > "'//state//'/ran"; : > "'//state//'/checked"'//new_line('a'))

    call invoke('')
    call assert_true(appears(join_path(state, 'ran')), 'a first run starts the follower')

    call remove_path(join_path(state, 'ran'))
    call invoke('')
    call sleep_ms(1500)
    call assert_true(.not. file_exists(join_path(state, 'ran')), &
        'a check less than five minutes old starts nothing')

    call remove_path(join_path(state, 'checked'))
    call invoke('FO_NO_FOLLOW=1')
    call sleep_ms(1500)
    call assert_true(.not. file_exists(join_path(state, 'ran')), &
        'FO_NO_FOLLOW=1 starts nothing')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'follow-tools-trigger: started when due, otherwise not'

contains

    subroutine invoke(extra)
        character(len=*), intent(in) :: extra
        type(string_list_t) :: arguments, environment

        call list_add(arguments, 'fmt')
        call list_add(arguments, '--check')
        call list_add(arguments, 'missing.f90')
        call list_add(environment, 'FY_TOOLS_HOME='//state)
        if (len(extra) > 0) call list_add(environment, extra)
        call run_fo(driver, arguments, scratch, join_path(scratch, 'cache'), result, &
            extra_environment=environment, timeout_ms=60000)
    end subroutine invoke

    logical function appears(path)
        character(len=*), intent(in) :: path
        integer :: i

        appears = .false.
        do i = 1, 50
            if (file_exists(path)) then
                appears = .true.
                return
            end if
            call sleep_ms(100)
        end do
    end function appears
end program test_follow_tools_trigger
