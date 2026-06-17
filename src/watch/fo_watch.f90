module fo_watch
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fx_watch, only: watcher_t, watcher_init, watcher_add, watcher_poll, &
        watcher_close, watcher_mark_self_written, &
        WATCH_MODIFY, WATCH_CREATE, WATCH_DELETE
    use fo_process, only: process_run_argv_logged, argv_push
    use fo_util, only: make_tmpfile, delete_tmpfile
    implicit none
    private
    public :: watch_loop

    integer, parameter :: WATCH_PATH_LEN = 4096

contains

    subroutine watch_loop(dir, fmt_mode)
        !! Rebuild on every Fortran source change. Uses the in-tree fx_watch
        !! file watcher (kqueue on macOS, inotify on Linux): no inotifywait,
        !! no mkfifo, no shell.
        character(len=*), intent(in) :: dir
        logical, intent(in), optional :: fmt_mode

        type(watcher_t) :: w
        character(len=WATCH_PATH_LEN) :: changed
        integer :: event_type, ierr
        logical :: do_fmt, got_event

        do_fmt = .false.
        if (present(fmt_mode)) do_fmt = fmt_mode

        call watcher_init(w, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: cannot start file watcher'
            return
        end if
        call watcher_add(w, trim(dir), .true., ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: cannot watch '//trim(dir)
            call watcher_close(w)
            return
        end if

        if (do_fmt) then
            write (output_unit, '(a)') &
                'fo: watching for Fortran file changes (--fmt enabled)...'
        else
            write (output_unit, '(a)') 'fo: watching for Fortran file changes...'
        end if

        do
            call watcher_poll(w, changed, event_type, 1000, got_event)
            if (.not. got_event) cycle
            if (event_type /= WATCH_MODIFY .and. event_type /= WATCH_CREATE &
                .and. event_type /= WATCH_DELETE) cycle
            if (.not. is_fortran_source(changed)) cycle

            write (output_unit, '(a,a)') 'change: ', trim(changed)

            if (do_fmt .and. event_type /= WATCH_DELETE) then
                call format_in_place(trim(changed))
                ! Suppress the watch event our own reformat will produce.
                call watcher_mark_self_written(w, trim(changed))
            end if

            call run_check()
        end do

        call watcher_close(w)
    end subroutine watch_loop

    logical function is_fortran_source(path) result(yes)
        !! True for *.f90 *.F90 *.f *.F (the set the watcher cares about).
        character(len=*), intent(in) :: path
        integer :: n

        yes = .false.
        n = len_trim(path)
        if (n >= 4) then
            if (path(n - 3:n) == '.f90' .or. path(n - 3:n) == '.F90') yes = .true.
        end if
        if (n >= 2) then
            if (path(n - 1:n) == '.f' .or. path(n - 1:n) == '.F') yes = .true.
        end if
    end function is_fortran_source

    subroutine format_in_place(file)
        !! Run fprettify on one file (no shell). Output is discarded.
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: packed
        character(len=512) :: logf
        integer :: n_args, exitcode

        call make_tmpfile('fo_watch_fmt', logf)
        n_args = 0
        call argv_push(packed, n_args, 'fprettify')
        call argv_push(packed, n_args, '-i')
        call argv_push(packed, n_args, '4')
        call argv_push(packed, n_args, '-l')
        call argv_push(packed, n_args, '88')
        call argv_push(packed, n_args, '--strict-indent')
        call argv_push(packed, n_args, file)
        call process_run_argv_logged('', packed, n_args, trim(logf), .false., &
            120, exitcode)
        call delete_tmpfile(logf)
    end subroutine format_in_place

    subroutine run_check()
        !! Run `fo check` (found on PATH) and echo its output, reporting a
        !! one-line OK/FAIL verdict.
        character(len=:), allocatable :: packed
        character(len=512) :: logf, line
        integer :: n_args, exitcode, u, ios

        call make_tmpfile('fo_watch_check', logf)
        n_args = 0
        call argv_push(packed, n_args, 'fo')
        call argv_push(packed, n_args, 'check')
        call process_run_argv_logged('', packed, n_args, trim(logf), .false., &
            600, exitcode)

        open (newunit=u, file=trim(logf), status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read (u, '(a)', iostat=ios) line
                if (ios /= 0) exit
                write (output_unit, '(a)') trim(line)
            end do
            close (u)
        end if
        call delete_tmpfile(logf)

        if (exitcode == 0) then
            write (output_unit, '(a)') 'watch: OK'
        else
            write (output_unit, '(a)') 'watch: FAIL'
        end if
    end subroutine run_check

end module fo_watch
