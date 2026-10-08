program test_async_adopted_reap
    use fo_util, only: temporary_root
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_fs, only: fs_remove_tree, fs_sleep_ms
    use fo_process, only: argv_push, process_getpid, process_poll_pid, &
        process_reap_adopted_scope_zombies, process_set_async_scope, &
        process_start_argv_logged, process_exit
    use fo_test_process_identity, only: mcp_process_start_time
    implicit none

    interface
        integer(c_int) function c_fork() bind(C, name='fork')
            import :: c_int
        end function c_fork

        integer(c_int) function c_waitpid(pid, status, options) bind(C, name='waitpid')
            import :: c_int
            integer(c_int), value :: pid, options
            integer(c_int), intent(out) :: status
        end function c_waitpid

        integer(c_int) function c_mkdir(path, mode) bind(C, name='mkdir')
            import :: c_char, c_int
            character(c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_mkdir
    end interface

    character(len=64) :: pid_text, start_text
    character(len=256) :: scratch, pid_file, log_file
    character(len=:), allocatable :: args
    integer(c_int) :: intermediate, adopted, waited, status
    integer :: owner_pid, active_pid, ierr, exitcode, n_args, attempt
    logical :: done, linux, failed

    inquire(file='/proc/self/stat', exist=linux)
    if (.not. linux) then
        write (output_unit, '(a)') 'SKIP: Linux subreaper oracle'
        stop
    end if
    failed = .false.
    owner_pid = process_getpid()
    write (pid_text, '(i0)') owner_pid
    write (start_text, '(i0)') mcp_process_start_time(owner_pid)
    scratch = temporary_root()//'/fo-async-adopted-'//trim(pid_text)
    pid_file = trim(scratch)//'/adopted.pid'
    log_file = trim(scratch)//'/active.log'
    ierr = c_mkdir(trim(scratch)//c_null_char, 448_c_int)
    call check(ierr == 0, 'creates a private scope directory')
    if (ierr /= 0) error stop 1
    call process_set_async_scope(trim(scratch), owner_pid, trim(start_text), ierr)
    call check(ierr == 0, 'registers the exact subreaper scope')
    if (ierr /= 0) error stop 1

    args = ''
    n_args = 0
    call argv_push(args, n_args, '/usr/bin/sleep')
    call argv_push(args, n_args, '1')
    call process_start_argv_logged(trim(scratch), args, n_args, &
        trim(log_file), active_pid, ierr)
    call check(ierr == 0 .and. active_pid > 0, 'starts a tracked async leader')
    if (ierr /= 0 .or. active_pid <= 0) error stop 1

    call spawn_adopted_child(trim(pid_file), adopted)

    call process_poll_pid(active_pid, done, exitcode)
    call check(.not. done .and. exitcode == 0, &
        'keeps the tracked leader active while sweeping adopted children')
    waited = c_waitpid(adopted, status, 1_c_int)
    call check(waited == -1 .and. mcp_process_start_time(int(adopted)) == 0, &
        'reaps a dead adopted child without consuming the tracked leader')

    do attempt = 1, 200
        call process_poll_pid(active_pid, done, exitcode)
        if (done) exit
        call fs_sleep_ms(10)
    end do
    call check(done .and. exitcode == 0, 'preserves the tracked leader exit status')

    pid_file = trim(scratch)//'/idle.pid'
    call spawn_adopted_child(trim(pid_file), adopted)
    call process_reap_adopted_scope_zombies(ierr)
    call check(ierr == 0, 'idle owner sweeps adopted children')
    waited = c_waitpid(adopted, status, 1_c_int)
    call check(waited == -1 .and. mcp_process_start_time(int(adopted)) == 0, &
        'idle sweep reaps the child with no async leader to poll')

    intermediate = c_fork()
    if (intermediate == 0) then
        call process_reap_adopted_scope_zombies(ierr)
        if (ierr == 0) call process_exit(5)
        call process_exit(0)
    end if
    if (intermediate < 0) error stop 1
    waited = c_waitpid(intermediate, status, 0_c_int)
    call check(waited == intermediate .and. status == 0, &
        'inherited scope cannot reap from a different PID')

    call fs_remove_tree(trim(scratch))
    if (failed) error stop 1
    write (output_unit, '(a)') 'PASS: adopted child reaping'
contains
    subroutine spawn_adopted_child(path, child_pid)
        character(len=*), intent(in) :: path
        integer(c_int), intent(out) :: child_pid

        integer(c_int) :: parent_pid, child_status, result
        integer :: child_unit, child_ios

        parent_pid = c_fork()
        if (parent_pid == 0) then
            child_pid = c_fork()
            if (child_pid < 0) call process_exit(2)
            if (child_pid == 0) then
                call fs_sleep_ms(100)
                call process_exit(0)
            end if
            open(newunit=child_unit, file=path, status='new', iostat=child_ios)
            if (child_ios /= 0) call process_exit(3)
            write (child_unit, '(i0)', iostat=child_ios) child_pid
            close(child_unit)
            if (child_ios /= 0) call process_exit(4)
            call process_exit(0)
        end if
        call check(parent_pid > 0, 'forks an intermediate process')
        if (parent_pid < 0) error stop 1
        result = c_waitpid(parent_pid, child_status, 0_c_int)
        call check(result == parent_pid .and. child_status == 0, &
            'intermediate exits before its child')
        open(newunit=child_unit, file=path, status='old', iostat=child_ios)
        call check(child_ios == 0, 'receives the adopted child PID')
        if (child_ios /= 0) error stop 1
        read (child_unit, *, iostat=child_ios) child_pid
        close(child_unit)
        call check(child_ios == 0 .and. child_pid > 0, 'reads the adopted child PID')
        if (child_ios /= 0 .or. child_pid <= 0) error stop 1
        call fs_sleep_ms(250)
    end subroutine spawn_adopted_child

    subroutine check(ok, description)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: description
        if (ok) return
        failed = .true.
        write (error_unit, '(a,a)') 'FAIL: ', description
    end subroutine check
end program test_async_adopted_reap
