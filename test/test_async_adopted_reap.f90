program test_async_adopted_reap
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_fs, only: fs_remove_tree, fs_sleep_ms
    use fo_process, only: argv_push, process_getpid, process_poll_pid, &
        process_set_async_scope, process_start_argv_logged, process_exit
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
    integer :: owner_pid, active_pid, ierr, exitcode, n_args, unit, ios, attempt
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
    scratch = '/var/tmp/fo-async-adopted-'//trim(pid_text)
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
    call process_start_argv_logged('/var/tmp', args, n_args, &
        trim(log_file), active_pid, ierr)
    call check(ierr == 0 .and. active_pid > 0, 'starts a tracked async leader')
    if (ierr /= 0 .or. active_pid <= 0) error stop 1

    intermediate = c_fork()
    call check(intermediate >= 0, 'forks an intermediate process')
    if (intermediate == 0) then
        adopted = c_fork()
        if (adopted < 0) call process_exit(2)
        if (adopted == 0) then
            call fs_sleep_ms(100)
            call process_exit(0)
        end if
        open(newunit=unit, file=trim(pid_file), status='new', iostat=ios)
        if (ios /= 0) call process_exit(3)
        write (unit, '(i0)', iostat=ios) adopted
        close(unit)
        if (ios /= 0) call process_exit(4)
        call process_exit(0)
    end if
    if (intermediate < 0) error stop 1
    waited = c_waitpid(intermediate, status, 0_c_int)
    call check(waited == intermediate, 'intermediate exits before its child')
    open(newunit=unit, file=trim(pid_file), status='old', iostat=ios)
    call check(ios == 0, 'receives the adopted child PID')
    if (ios /= 0) error stop 1
    read (unit, *, iostat=ios) adopted
    close(unit)
    call check(ios == 0 .and. adopted > 0, 'reads the adopted child PID')
    if (ios /= 0 .or. adopted <= 0) error stop 1
    call fs_sleep_ms(250)

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
    call fs_remove_tree(trim(scratch))
    if (failed) error stop 1
    write (output_unit, '(a)') 'PASS: adopted child reaping'
contains
    subroutine check(ok, description)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: description
        if (ok) return
        failed = .true.
        write (error_unit, '(a,a)') 'FAIL: ', description
    end subroutine check
end program test_async_adopted_reap
