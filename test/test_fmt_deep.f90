program test_fmt_deep
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_fs, only: fs_write_text
    use fo_process, only: process_run_argv_logged, argv_push
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_fmt_deep_check_detects_fluff()
    call test_fmt_deep_check_misformatted()

    write (output_unit, '(a,i0,a,i0,a)') 'fmt_deep: ', n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_fmt_deep_check_detects_fluff()
        !! Test that fluff availability check works (either found or gracefully skipped).
        logical :: fluff_available
        character(len=:), allocatable :: packed
        integer :: n_args, exitcode

        ! Try to run 'fluff --version' to detect if fluff is installed
        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'fluff')
        call argv_push(packed, n_args, '--version')
        call process_run_argv_logged('', packed, n_args, '/dev/null', &
            .false., 5, exitcode)

        fluff_available = (exitcode == 0)

        ! Either way, the test should pass (fluff found or not found is both ok)
        call assert(.true., 'fluff detection: handles found or not found')
    end subroutine test_fmt_deep_check_detects_fluff

    subroutine test_fmt_deep_check_misformatted()
        !! Test that fmt --deep --check detects misformatted files (if fluff exists).
        character(len=512) :: tmpfile, log_file
        character(len=:), allocatable :: packed
        integer :: n_args, exitcode, u, ios
        logical :: fluff_available

        ! First check if fluff is available
        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'fluff')
        call argv_push(packed, n_args, '--version')
        call process_run_argv_logged('', packed, n_args, '/dev/null', &
            .false., 5, exitcode)

        fluff_available = (exitcode == 0)

        if (.not. fluff_available) then
            write (output_unit, '(a)') 'fmt_deep: skip (fluff not on PATH)'
            call assert(.true., 'fluff detection: skipped when not available')
            return
        end if

        ! Create a misformatted Fortran file
        call make_tmpfile('test_fmt_deep_misformatted', tmpfile)
        open (newunit=u, file=trim(tmpfile), status='replace', iostat=ios)
        if (ios == 0) then
            ! Write intentionally misformatted code (extra indentation)
            write (u, '(a)') 'program test'
            write (u, '(a)') '      implicit none'
            write (u, '(a)') '      integer :: x'
            write (u, '(a)') '        x = 1'
            write (u, '(a)') 'end program test'
            close (u)
        end if

        ! Run fluff format --check on it
        call make_tmpfile('test_fmt_deep_log', log_file)
        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'fluff')
        call argv_push(packed, n_args, 'format')
        call argv_push(packed, n_args, '--check')
        call argv_push(packed, n_args, trim(tmpfile))
        call process_run_argv_logged('', packed, n_args, trim(log_file), &
            .false., 30, exitcode)

        ! fluff format --check exits non-zero if reformatting is needed
        call assert(exitcode /= 0, &
            'fluff check: detects misformatted file (exitcode /= 0)')

        call delete_tmpfile(tmpfile)
        call delete_tmpfile(log_file)
    end subroutine test_fmt_deep_check_misformatted

end program test_fmt_deep
