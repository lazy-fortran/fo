program test_gremlin_state_loss
    !! A lane never outlives its state: deleting the state directory of a
    !! running owner, queued behind a full admission domain or admitted, makes
    !! that owner exit within a bound and its admission guardian release the
    !! host work slots.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, assert_true, finish_assertions, remove_tree
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run
    use fo_test_gremlin_oracle, only: gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_owner_pid, gremlin_wait_exit
    use fo_gremlin_state, only: gremlin_lease_group_t, gremlin_session_state_dir, &
        gremlin_host_lease_acquire_weighted, gremlin_host_lease_release
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    type(string_list_t) :: args, environment
    type(process_result_t) :: process
    type(gremlin_lease_group_t) :: held_slots
    character(len=4096) :: lane_dir
    character(len=512) :: message
    integer :: status, attempt
    logical :: busy

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/fpm.toml', 'name = "state_loss"'//new_line('a'))
    call gremlin_write_case(project, 'test_loss', "print '(a)', 'loss'")
    call list_add(environment, 'FO_GREMLIN_ADMISSION_SCOPE=')

    ! An owner queued behind both host work slots.
    call gremlin_host_lease_acquire_weighted('gremlin-heavy-work', 2, 2, &
        held_slots, busy, status, message)
    call assert_true(status == 0 .and. .not. busy, 'oracle holds both work slots')
    if (status /= 0 .or. busy) call finish_assertions(retain_failed_scratch=.true.)
    call lose_state('queued-loss', .true.)
    call gremlin_host_lease_release(held_slots, status, message)

    ! An owner admitted and building (payload children in flight).
    call lose_state('build-loss', .false.)

    ! Both admission guardians release their slots although the lane state
    ! they watched is gone.
    do attempt = 1, 200
        call gremlin_host_lease_acquire_weighted('gremlin-heavy-work', 2, 2, &
            held_slots, busy, status, message)
        if (.not. busy) exit
        call gremlin_wait_ms(50)
    end do
    call assert_true(status == 0 .and. .not. busy, &
        'slots of the deleted lanes are released for reuse')
    if (status == 0 .and. .not. busy) &
        call gremlin_host_lease_release(held_slots, status, message)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine lose_state(lane, queued)
        character(len=*), intent(in) :: lane
        logical, intent(in) :: queued
        character(:), allocatable :: session
        integer :: owner, i

        call gremlin_start_args(args, project, lane, 'test_loss')
        call gremlin_run(driver, project, cache, state, args, process, environment, 30000)
        call assert_true(process%exit_code == 0, lane//' lane starts')
        if (process%exit_code /= 0) return
        call json_session(process%stdout, session)
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        do i = 1, 400
            call gremlin_run(driver, project, cache, state, args, process, environment, &
                10000)
            if (queued) then
                if (index(process%stdout, 'waiting for a host Gremlin work slot') > 0) exit
            else
                if (index(process%stdout, '"state":"building"') > 0) exit
            end if
            call gremlin_wait_ms(50)
        end do
        owner = gremlin_owner_pid(session)
        call assert_true(owner > 0, lane//' owner is identified')
        call gremlin_session_state_dir(project, lane, lane_dir, status, message)
        call assert_true(status == 0, lane//' state directory resolves')
        call remove_tree(trim(lane_dir))
        call assert_true(gremlin_wait_exit(owner, 15000), &
            lane//' owner exits after its state directory is deleted')
    end subroutine lose_state

    subroutine json_session(text, session)
        character(len=*), intent(in) :: text
        character(:), allocatable, intent(out) :: session
        integer :: a, b

        session = ''
        a = index(text, '"session_id"')
        if (a == 0) return
        a = a + 12 + index(text(a + 12:), '"') - 1
        b = a + index(text(a + 1:), '"')
        session = text(a + 1:b - 1)
    end subroutine json_session

end program test_gremlin_state_loss
