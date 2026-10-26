program test_gremlin_slot_stop
    !! An owner queued behind a full host work-slot admission domain still
    !! honours `gremlin stop` promptly and exits.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, assert_true, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_process_running
    use fo_test_json, only: json_value_t
    use fo_gremlin_state, only: gremlin_lease_group_t, &
        gremlin_host_lease_acquire_weighted, gremlin_host_lease_release
    use fo_gremlin_campaign, only: GREMLIN_HOST_WORK_LEASE
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    type(string_list_t) :: args, environment
    type(process_result_t) :: process
    type(json_value_t) :: reply
    type(gremlin_lease_group_t) :: held_slots
    character(len=512) :: lease_message
    integer(int64) :: started, now, rate
    integer :: status, owner_pid, dash, ios, i
    logical :: busy
    character(len=*), parameter :: lane = 'slot-stop'

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/fpm.toml', 'name = "slot_stop"'//new_line('a'))
    call gremlin_write_case(project, 'test_slot', "print '(a)', 'slot'")
    call gremlin_start_args(args, project, lane, 'test_slot')
    call list_add(environment, 'FO_GREMLIN_ADMISSION_SCOPE=')

    call gremlin_host_lease_acquire_weighted(GREMLIN_HOST_WORK_LEASE, 2, 2, &
        held_slots, busy, status, lease_message)
    call assert_true(status == 0 .and. .not. busy, 'oracle holds both work slots: '// &
        trim(lease_message))
    if (status /= 0 .or. busy) call finish_assertions(retain_failed_scratch=.true.)

    call gremlin_run(driver, project, cache, state, args, process, environment, 30000)
    call assert_true(process%exit_code == 0, 'queued lane starts')
    args = string_list_t()
    call list_add(args, 'gremlin')
    call list_add(args, 'status')
    call list_add(args, '--dir')
    call list_add(args, project)
    call list_add(args, '--lane')
    call list_add(args, lane)
    do i = 1, 200
        call gremlin_run(driver, project, cache, state, args, process, environment, 10000)
        if (index(process%stdout, 'waiting for a host Gremlin work slot') > 0) exit
        call gremlin_wait_ms(50)
    end do
    call assert_true(index(process%stdout, 'waiting for a host Gremlin work slot') > 0, &
        'owner waits for a host work slot')
    call json_session(process%stdout)

    args%items(2)%value = 'stop'
    call system_clock(started, rate)
    call gremlin_run(driver, project, cache, state, args, process, environment, 20000)
    call system_clock(now)
    call assert_true(process%exit_code == 0 .and. .not. process%timed_out, &
        'stop returns while the owner waits for a slot')
    call assert_true(real(now - started)/real(rate) < 10.0, 'stop returns promptly')
    owner_pid = -1
    dash = index(session, '-')
    if (dash > 1) read (session(:dash - 1), *, iostat=ios) owner_pid
    do i = 1, 100
        if (.not. gremlin_process_running(owner_pid)) exit
        call gremlin_wait_ms(100)
    end do
    call assert_true(.not. gremlin_process_running(owner_pid), &
        'owner exits although both work slots stay held')
    call gremlin_host_lease_release(held_slots, status, lease_message)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine json_session(text)
        character(len=*), intent(in) :: text
        integer :: a, b
        a = index(text, '"session_id"')
        session = ''
        if (a == 0) return
        a = a + index(text(a + 12:), '"')
        b = a + index(text(a + 1:), '"')
        session = text(a + 1:b - 1)
    end subroutine json_session

end program test_gremlin_slot_stop
