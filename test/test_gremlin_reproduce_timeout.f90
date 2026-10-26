program test_gremlin_reproduce_timeout
    !! `gremlin reproduce` bounds its generation build (#139): a hung compiler
    !! is cancelled at the case wall timeout, the result says TIMEOUT, the
    !! hung process is gone and the host heavy-work slot is released, so the
    !! next reproduction acquires it and runs. This program doubles as the
    !! hung compiler when FO_TEST_HANG_PIDFILE is set.
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: iso_c_binding, only: c_int
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, file_exists, process_alive
    use fo_test_harness, only: assert_true, assert_contains, finish_assertions
    use fo_test_harness, only: resolve_test_executable, sleep_ms
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    use fo_gremlin_state, only: gremlin_lease_group_t, &
        gremlin_host_lease_acquire_weighted, gremlin_host_lease_release
    use fo_gremlin_campaign, only: GREMLIN_HOST_WORK_LEASE
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: generation, self, pid_path
    type(string_list_t) :: args, environment
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer(int64) :: started, now, rate
    integer :: hung_pid, io, status
    character(len=64) :: pid_text
    character(len=512) :: lease_message
    type(gremlin_lease_group_t) :: held_slots
    logical :: ok, busy
    character(len=*), parameter :: lane = 'reproduce-timeout'

    call get_environment_variable('FO_TEST_HANG_PIDFILE', length=io, status=status)
    if (status == 0 .and. io > 0) call hang_as_compiler()

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/fpm.toml', 'name = "reproduce_timeout"'// &
        new_line('a'))
    call gremlin_write_case(project, 'test_hang_anchor', &
        "print '(a)', 'anchor'")
    call gremlin_start_args(args, project, lane, 'test_hang_anchor')
    call gremlin_json(driver, project, cache, state, args, reply, process, 30000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, 'lane starts')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)
    call wait_for_gate(generation, ok)
    call assert_true(ok, 'anchor generation is verified')
    if (.not. ok) then
        call gremlin_stop_lane(driver, project, cache, state, lane, session, &
            allow_terminal_error=.true.)
        call finish_assertions(retain_failed_scratch=.true.)
    end if

    call resolve_test_executable(self, ok)
    call assert_true(ok, 'resolves the hung-compiler fixture')
    pid_path = scratch//'/hung-compiler.pid'
    call list_add(environment, 'FO_FC='//self)
    call list_add(environment, 'FO_TEST_HANG_PIDFILE='//pid_path)
    call reproduce_args(args, '3')
    call system_clock(started, rate)
    call gremlin_run(driver, project, cache, state, args, process, environment, &
        timeout=60000)
    call system_clock(now)
    call assert_true(.not. process%timed_out .and. .not. process%runner_failed, &
        'reproduce returns although the compiler hangs')
    call assert_true(real(now - started)/real(rate) < 45.0, &
        'reproduce returns within a small multiple of its bound')
    call assert_true(process%exit_code /= 0, 'hung build is not a PASS')
    call assert_contains(process%stdout, 'TIMEOUT', 'result reports the timeout')
    call assert_true(file_exists(pid_path), 'the fake compiler was started')
    if (file_exists(pid_path)) then
        pid_text = read_text(pid_path)
        read (pid_text, *, iostat=status) hung_pid
        call gremlin_wait_ms(500)
        call assert_true(status == 0 .and. .not. process_alive(hung_pid), &
            'hung compiler process is cancelled')
    end if

    ! The case bound also covers the wait for a host work slot. Hold every
    ! slot of the fixture's admission domain; the child must not delegate to
    ! this process's scope.
    do io = 1, 600
        call gremlin_host_lease_acquire_weighted(GREMLIN_HOST_WORK_LEASE, 2, 2, &
            held_slots, busy, status, lease_message)
        if (.not. busy) exit
        call gremlin_wait_ms(50)
    end do
    call assert_true(status == 0, 'oracle holds both fixture work slots: '// &
        trim(lease_message))
    environment = string_list_t()
    call list_add(environment, 'FO_GREMLIN_ADMISSION_SCOPE=')
    call reproduce_args(args, '2')
    call system_clock(started, rate)
    call gremlin_run(driver, project, cache, state, args, process, environment, &
        timeout=60000)
    call system_clock(now)
    call assert_true(.not. process%timed_out .and. .not. process%runner_failed, &
        'reproduce returns while every host work slot is held')
    call assert_true(real(now - started)/real(rate) < 30.0, &
        'slot wait stays within a small multiple of the case bound')
    call assert_true(process%exit_code /= 0, 'unadmitted reproduction is not a PASS')
    call assert_contains(process%stdout, 'work slot', &
        'result names the unavailable host work slot')
    if (status == 0) call gremlin_host_lease_release(held_slots, status, lease_message)
    call assert_true(status == 0, 'oracle releases the fixture work slots')

    ! A leaked heavy-work slot would block this reproduction forever.
    call reproduce_args(args, '60')
    call gremlin_run(driver, project, cache, state, args, process, timeout=90000)
    call assert_true(.not. process%timed_out .and. process%exit_code == 0, &
        'the heavy-work slot is released: next reproduction runs')
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine hang_as_compiler()
        character(len=4096) :: path
        integer :: unit, k

        call get_environment_variable('FO_TEST_HANG_PIDFILE', path)
        open (newunit=unit, file=trim(path), status='replace', action='write')
        write (unit, '(i0)') getpid_value()
        close (unit)
        ! Self-bounded so a failed oracle cannot leave an immortal process.
        do k = 1, 1200
            call sleep_ms(100)
        end do
        stop 3
    end subroutine hang_as_compiler

    integer function getpid_value()
        interface
            integer(c_int) function c_getpid() bind(C, name='getpid')
                import :: c_int
            end function c_getpid
        end interface
        getpid_value = int(c_getpid())
    end function getpid_value

    subroutine reproduce_args(list, seconds)
        type(string_list_t), intent(out) :: list
        character(len=*), intent(in) :: seconds

        call list_add(list, 'gremlin')
        call list_add(list, 'reproduce')
        call list_add(list, 'test_hang_anchor')
        call list_add(list, '--dir')
        call list_add(list, project)
        call list_add(list, '--lane')
        call list_add(list, lane)
        call list_add(list, '--session')
        call list_add(list, session)
        call list_add(list, '--generation')
        call list_add(list, generation)
        call list_add(list, '--timeout-seconds')
        call list_add(list, seconds)
    end subroutine reproduce_args

    subroutine wait_for_gate(active, found)
        character(:), allocatable, intent(out) :: active
        logical, intent(out) :: found
        type(string_list_t) :: status_args
        type(json_value_t) :: view
        type(process_result_t) :: observed
        integer :: poll

        found = .false.
        active = ''
        call list_add(status_args, 'gremlin')
        call list_add(status_args, 'status')
        call list_add(status_args, '--dir')
        call list_add(status_args, project)
        call list_add(status_args, '--lane')
        call list_add(status_args, lane)
        call list_add(status_args, '--session')
        call list_add(status_args, session)
        do poll = 1, 4800
            call gremlin_json(driver, project, cache, state, status_args, view, &
                observed, 10000)
            if (observed%exit_code /= 0) return
            active = gremlin_field(view, 'active_generation')
            if (len(active) == 64) then
                found = json_boolean_value(json_member(view, 'local_gate_green'))
                if (found) return
            end if
            call gremlin_wait_ms(50)
        end do
    end subroutine wait_for_gate

end program test_gremlin_reproduce_timeout
