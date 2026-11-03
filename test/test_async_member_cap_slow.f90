program test_async_member_cap_slow
    !! A tracked command whose tree spawns more short-lived descendants than
    !! the 4096-member registry cap (a long build) stays pollable and
    !! cancellable. Slow: members register at the rate of durable record
    !! writes, about a minute to reach the cap.
    use, intrinsic :: iso_fortran_env, only: output_unit, int64
    use fo_fs, only: fs_remove_tree, fs_sleep_ms, fs_is_windows, fs_dir_entry_count
    use fo_process, only: argv_push, process_poll_pid, process_set_async_scope, &
        process_start_argv_logged, process_cancel_pid
    use fo_gremlin_state, only: gremlin_client_identity
    use fo_test_harness, only: make_scratch, write_text
    implicit none

    character(len=64) :: start_text
    character(len=:), allocatable :: args, scratch, registry
    integer :: owner_pid, pid, ierr, exitcode, n_args, members, peak
    integer(int64) :: started, now, rate, after_cap
    logical :: done, polled, failed

    failed = .false.
    if (fs_is_windows()) then
        write (output_unit, '(a)') 'SKIP: POSIX descendant registry'
        stop
    end if
    call gremlin_client_identity(.false., owner_pid, start_text, ierr)
    call check(ierr == 0, 'reads the owner process identity')
    if (ierr /= 0) error stop 1
    call make_scratch('fo-async-member-cap', scratch)
    if (len(scratch) == 0) error stop 1
    call process_set_async_scope(trim(scratch), owner_pid, trim(start_text), ierr)
    call check(ierr == 0, 'registers the exact async scope')
    if (ierr /= 0) error stop 1

    ! Waves of short sleeps until the registry has neared its cap, then
    ! more registrations past it, then a tail to cancel. The waves end with
    ! the scratch directory, so a failed run leaks no spawner.
    args = ''
    n_args = 0
    call argv_push(args, n_args, '/bin/sh')
    call argv_push(args, n_args, '-c')
    call argv_push(args, n_args, 'i=0; while [ ! -e stop ] && ' // &
        '[ -e command.log ] && [ $i -lt 600 ]; ' // &
        'do j=0; while [ $j -lt 100 ]; do sleep 0.5 & j=$((j+1)); done; ' // &
        'wait; i=$((i+1)); done; sleep 60')
    call process_start_argv_logged(trim(scratch), args, n_args, &
        trim(scratch)//'/command.log', pid, ierr)
    call check(ierr == 0 .and. pid > 0, 'starts the tracked command')
    if (ierr /= 0 .or. pid <= 0) error stop 1
    registry = trim(scratch)//'/async-processes/'// &
        trim(adjustl(int_text(owner_pid)))//'-'//trim(start_text)

    ! Each poll scans the process table, so progress is bounded by wall time.
    polled = .true.
    after_cap = -1
    peak = 0
    call system_clock(started, rate)
    do
        call process_poll_pid(pid, done, exitcode)
        if (done .or. exitcode /= 0) then
            polled = .false.
            exit
        end if
        call system_clock(now)
        ! At the cap the registry either stays full or drops exited members.
        members = fs_dir_entry_count(registry)
        peak = max(peak, members)
        if (after_cap < 0 .and. (members >= 4000 .or. &
            (peak >= 3000 .and. members < peak - 100))) after_cap = now - started
        if (after_cap >= 0 .and. (now - started) - after_cap >= 10 * rate) exit
        if (now - started >= 300 * rate) exit
        call fs_sleep_ms(20)
    end do
    call check(after_cap >= 0, 'the command registers members up to the cap')
    call check(polled, 'polls the command past the member cap')
    call write_text(trim(scratch)//'/stop', '')
    call process_cancel_pid(pid, exitcode)
    call check(exitcode == 0, 'cancels the command past the member cap')
    call process_poll_pid(pid, done, exitcode)
    call check(done, 'the cancelled command is gone')

    call fs_remove_tree(trim(scratch))
    if (failed) error stop 1
    write (output_unit, '(a)') 'PASS: async member cap'
contains
    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=16) :: text

        write (text, '(i0)') value
    end function int_text

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label

        if (.not. condition) then
            failed = .true.
            write (output_unit, '(a)') 'FAIL: '//label
        end if
    end subroutine check
end program test_async_member_cap_slow
