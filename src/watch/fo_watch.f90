module fo_watch
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, int64
    use fo_change_watch, only: change_watch_t, change_watch_init, &
        change_watch_add_root, change_watch_poll, change_watch_close, &
        change_watch_mark_self_written, change_watch_relevant, &
        CHANGE_DELETE, CHANGE_RECONCILE
    use fx_proc, only: proc_scan_files
    use fo_process, only: process_run_argv_logged, process_has_fortran_source_ext, &
        argv_push
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_format, only: format_file
    implicit none
    private
    public :: watch_loop

    integer, parameter :: WATCH_PATH_LEN = 4096

contains

    subroutine watch_loop(dir, fmt_mode)
        !! Rebuild on every Fortran source change using the shared native
        !! change provider (FSEvents on macOS, inotify on Linux): no inotifywait,
        !! no mkfifo, no shell.
        character(len=*), intent(in) :: dir
        logical, intent(in), optional :: fmt_mode

        type(change_watch_t) :: w
        character(len=WATCH_PATH_LEN) :: changed
        character(len=WATCH_PATH_LEN) :: watch_message
        integer :: event_type, ierr
        logical :: do_fmt, got_event
        integer(int64) :: debounce_until, now, rate

        do_fmt = .false.
        if (present(fmt_mode)) do_fmt = fmt_mode

        call change_watch_init(w, trim(dir), ierr, watch_message)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: cannot start file watcher: '// &
                trim(watch_message)
            call change_watch_close(w)
            return
        end if
        call change_watch_add_root(w, trim(dir), ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: cannot watch '//trim(dir)
            call change_watch_close(w)
            return
        end if

        if (do_fmt) then
            write (output_unit, '(a)') &
                'fo: watching for Fortran file changes (--fmt enabled)...'
        else
            write (output_unit, '(a)') 'fo: watching for Fortran file changes...'
        end if

        debounce_until = 0_8
        do
            call change_watch_poll(w, 100, changed, event_type, got_event, ierr)
            if (ierr /= 0) then
                write (error_unit, '(a)') 'fo: file watcher reconciliation failed'
                call change_watch_close(w)
                return
            end if
            call system_clock(count=now, count_rate=rate)
            if (got_event) then
                if (is_fortran_source(changed) .or. event_type == CHANGE_RECONCILE) then
                    write (output_unit, '(a,a)') 'change: ', trim(changed)
                    debounce_until = now + max(1_int64, rate / 4_int64)
                    if (do_fmt .and. event_type == CHANGE_RECONCILE) then
                        call format_reconciled_tree(w)
                    end if
                    if (do_fmt .and. event_type /= CHANGE_DELETE .and. &
                        event_type /= CHANGE_RECONCILE) then
                        call format_in_place(trim(changed))
                        call change_watch_mark_self_written(w, trim(changed))
                    end if
                end if
                cycle
            end if
            if (debounce_until == 0_8 .or. now < debounce_until) cycle
            debounce_until = 0_8

            call run_check()
        end do

        call change_watch_close(w)
    end subroutine watch_loop

    logical function is_fortran_source(path) result(yes)
        !! Share source membership with the native scanner.
        character(len=*), intent(in) :: path
        yes = process_has_fortran_source_ext(path)
    end function is_fortran_source

    subroutine format_reconciled_tree(watch)
        !! Lost or structural notifications require catch-up for files populated
        !! before the directory subscription. This scan runs only on an event.
        type(change_watch_t), intent(inout) :: watch
        character(len=:), allocatable :: files(:)
        integer :: count, ierr, i

        call proc_scan_files(trim(watch%roots(1)), files, count, ierr)
        if (ierr /= 0) return
        do i = 1, count
            if (.not. change_watch_relevant(watch, trim(files(i)))) cycle
            if (.not. is_fortran_source(trim(files(i)))) cycle
            call format_in_place(trim(files(i)))
            call change_watch_mark_self_written(watch, trim(files(i)))
        end do
    end subroutine format_reconciled_tree

    subroutine format_in_place(file)
        !! Format one changed file in place with fo's native formatter, so
        !! watch --fmt and the `fo fmt` / format-check path agree. Best-effort:
        !! a formatting failure is ignored (the rebuild reports real errors).
        character(len=*), intent(in) :: file
        integer :: exitcode

        call format_file(file, exitcode)
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
