program test_async_job_orphans
    !! A running job that detaches a child hands it to this process (a Linux
    !! subreaper once it starts a job). While the job is polled, that child is
    !! collected when it exits instead of staying a zombie until the job ends.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_fs, only: fs_sleep_ms
    use fo_process, only: argv_push, process_poll_pid, process_start_argv_logged, &
        process_cancel_pid, process_getpid
    use fo_test_harness, only: make_scratch, assert_true, finish_assertions
    implicit none

    character(len=:), allocatable :: args, scratch
    integer :: runner, ierr, exitcode, n_args, attempt, zombies
    logical :: done, linux

    inquire (file='/proc/self/stat', exist=linux)
    if (.not. linux) then
        write (output_unit, '(a)') 'SKIP: Linux subreaper oracle'
        stop
    end if
    call make_scratch('fo-async-orphans', scratch)

    ! The job keeps running; a subshell detaches a short sleep, which this
    ! process adopts when the subshell exits.
    args = ''
    n_args = 0
    call argv_push(args, n_args, '/bin/sh')
    call argv_push(args, n_args, '-c')
    call argv_push(args, n_args, '(sleep 0.2 &); sleep 5')
    call process_start_argv_logged(scratch, args, n_args, scratch//'/runner.log', runner, ierr)
    call assert_true(ierr == 0 .and. runner > 0, 'starts a job that detaches a child')
    zombies = -1
    do attempt = 1, 100
        call process_poll_pid(runner, done, exitcode)
        call fs_sleep_ms(20)
        if (attempt < 30) cycle
        zombies = zombie_children()
        if (zombies == 0) exit
    end do
    call assert_true(.not. done, 'the job is still running')
    call assert_true(zombies == 0, 'an adopted child is collected once it exits')
    call process_cancel_pid(runner, exitcode)
    call finish_assertions()

contains

    integer function zombie_children() result(count)
        !! Exited, uncollected children of this process.
        character(len=4096) :: line, path
        character(len=512) :: stat
        integer :: unit, ios, at, pid, close_paren

        count = 0
        write (path, '(a,i0,a)') '/proc/self/task/', process_getpid(), '/children'
        open (newunit=unit, file=trim(path), action='read', iostat=ios)
        if (ios /= 0) return
        line = ''
        read (unit, '(a)', iostat=ios) line
        close (unit)
        at = 1
        do
            do while (at <= len_trim(line) .and. line(at:at) == ' ')
                at = at + 1
            end do
            if (at > len_trim(line)) exit
            read (line(at:), *) pid
            do while (at <= len_trim(line) .and. line(at:at) /= ' ')
                at = at + 1
            end do
            write (path, '(a,i0,a)') '/proc/', pid, '/stat'
            open (newunit=unit, file=trim(path), action='read', iostat=ios)
            if (ios /= 0) cycle
            stat = ''
            read (unit, '(a)', iostat=ios) stat
            close (unit)
            close_paren = index(stat, ')', back=.true.)
            if (close_paren > 0) then
                if (stat(close_paren + 2:close_paren + 2) == 'Z') count = count + 1
            end if
        end do
    end function zombie_children

end program test_async_job_orphans
