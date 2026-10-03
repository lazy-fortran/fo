program test_gremlin_coverage_transactions
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_gremlin_coverage, only: coverage_epoch_t, coverage_open, &
        coverage_record_path, COVERAGE_OK
    implicit none

    interface
        integer(c_int) function lock_coverage(path) &
                bind(C, name='fo_c_gremlin_coverage_lock')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function lock_coverage
        subroutine unlock_coverage(fd) bind(C, name='fo_c_gremlin_coverage_unlock')
            import :: c_int
            integer(c_int), value :: fd
        end subroutine unlock_coverage
        integer(c_int) function fork_process() bind(C, name='fork')
            import :: c_int
        end function fork_process
        integer(c_int) function close_fd(fd) bind(C, name='close')
            import :: c_int
            integer(c_int), value :: fd
        end function close_fd
        subroutine exit_child(code) bind(C, name='_exit')
            import :: c_int
            integer(c_int), value :: code
        end subroutine exit_child
        integer(c_int) function wait_child(pid, status, options) bind(C, name='waitpid')
            import :: c_int
            integer(c_int), value :: pid, options
            integer(c_int), intent(out) :: status
        end function wait_child
        integer(c_int) function delay(microseconds) bind(C, name='usleep')
            import :: c_int
            integer(c_int), value :: microseconds
        end function delay
    end interface

    type(coverage_epoch_t) :: state
    character(len=16) :: names(3), outcomes(3)
    character(len=256) :: message
    character(len=64) :: state_path
    integer(c_int) :: lock_fd, children(3), pid, child_status, result
    integer :: i, status, clock_start, clock_now, clock_rate
    logical :: exists, blocked(3)

    inquire(file='/proc/self/wchan', exist=exists)
    if (.not. exists) then
        print '(a)', 'Coverage transactions: skipped (requires Linux /proc)'
        stop
    end if
    state_path = 'gremlin-coverage-concurrent.state'
    call execute_command_line('rm -f '//trim(state_path))
    names = [character(len=16) :: 'concurrent_1', 'concurrent_2', 'concurrent_3']
    outcomes = [character(len=16) :: 'PASS', 'FAIL', 'TIMEOUT']
    call coverage_open(trim(state_path), repeat('e', 64), names, 3, 123, &
        state, status, message)
    call check(status == COVERAGE_OK, 'initialize concurrent fixture')
    lock_fd = lock_coverage(trim(state_path)//c_null_char)
    call check(lock_fd >= 0, 'hold transaction lock before starting writers')
    do i = 1, 3
        pid = fork_process()
        call check(pid >= 0, 'fork independent coverage writer')
        if (pid == 0) then
            ! Drop the inherited descriptor without unlocking the parent's lock.
            result = close_fd(lock_fd)
            call coverage_record_path(trim(state_path), repeat('e', 64), &
                names(i), outcomes(i), status, message)
            if (status /= COVERAGE_OK) call exit_child(1_c_int)
            call exit_child(0_c_int)
        end if
        children(i) = pid
    end do
    ! Each writer must be waiting on the actual file lock before its release.
    ! Publication-only locking lets all writers read the same old snapshot here.
    blocked = .false.
    call system_clock(clock_start, clock_rate)
    do
        do i = 1, 3
            blocked(i) = waiting_on_lock(children(i))
        end do
        if (all(blocked)) exit
        call system_clock(clock_now)
        if (real(clock_now - clock_start)/real(clock_rate) > 10.0) exit
        result = delay(1000_c_int)
    end do
    call unlock_coverage(lock_fd)
    do i = 1, 3
        result = wait_child(children(i), child_status, 0_c_int)
        call check(result == children(i), 'reap exact owned writer')
        call check(child_status == 0, 'concurrent writer completed successfully')
    end do
    call check(all(blocked), 'all independent writers reached the lock barrier')
    call coverage_open(trim(state_path), repeat('e', 64), names, 3, 123, &
        state, status, message, persist=.false.)
    call check(status == COVERAGE_OK, 'read final transaction state')
    call check(all(state%outcome == outcomes), &
        'concurrent different-case outcomes all survive without lost updates')
    call execute_command_line('rm -f '//trim(state_path)//' '//trim(state_path)//'.lock')
    print '(a)', 'Coverage transaction behavioral oracle: PASS'

contains

    logical function waiting_on_lock(pid) result(waiting)
        integer(c_int), intent(in) :: pid
        character(len=64) :: proc_path, pid_text, wait_channel
        integer :: unit, ios

        write(pid_text, '(i0)') pid
        proc_path = '/proc/'//trim(pid_text)//'/wchan'
        waiting = .false.
        open(newunit=unit, file=trim(proc_path), status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(unit, '(a)', iostat=ios) wait_channel
        close(unit)
        if (ios /= 0) return
        waiting = index(wait_channel, 'lock') > 0
    end function waiting_on_lock

    subroutine check(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) return
        print '(a)', 'FAIL: '//description
        error stop 1
    end subroutine check
end program test_gremlin_coverage_transactions
