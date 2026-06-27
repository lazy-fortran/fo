module fo_cover
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_build_backend, only: backend_t, detect_backend, backend_build, &
        backend_test, BACKEND_NONE, BACKEND_GFORTRAN
    use fo_diagnostics, only: diagnostic_t, diagnostic_from_log
    use fo_fs, only: fs_make_dir
    use fo_process, only: process_run_argv_logged, argv_push
    use fo_util, only: make_tmpfile, delete_tmpfile
    implicit none
    private

    public :: fo_cover_run, coverage_total_percent

    character(len=*), parameter :: COVER_FLAGS = '--coverage'

contains

    subroutine fo_cover_run()
        type(backend_t) :: b
        integer :: exitcode
        logical :: include_slow
        character(len=32) :: fail_under
        character(len=512) :: build_log, test_log, fortcov_log
        character(len=512) :: report_path
        character(len=32) :: total

        if (has_help_arg()) then
            call print_cover_usage()
            return
        end if

        call parse_cover_args(include_slow, fail_under)

        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo cover: no fpm.toml found'
            stop 1, quiet=.true.
        end if
        if (b%kind /= BACKEND_GFORTRAN) then
            write (error_unit, '(a)') 'fo cover: only fpm/gfortran backend supported'
            stop 1, quiet=.true.
        end if

        call fs_make_dir('build/coverage')
        call fs_make_dir('build/gcov')
        report_path = 'build/coverage/coverage.md'

        call make_tmpfile('fo-cover-build', build_log)
        call backend_build(b, exitcode, COVER_FLAGS, build_log, with_tests=.true., &
            use_cache=.false.)
        if (exitcode /= 0) then
            call report_stage_failure('build', build_log, 'fo cover')
            stop 1, quiet=.true.
        end if
        call delete_tmpfile(build_log)

        call make_tmpfile('fo-cover-test', test_log)
        call backend_test(b, exitcode, include_slow, test_log, flags=COVER_FLAGS, &
            use_cache=.false.)
        if (exitcode /= 0) then
            call report_stage_failure('test', test_log, 'fo cover')
            stop 1, quiet=.true.
        end if
        call delete_tmpfile(test_log)

        call make_tmpfile('fo-cover-fortcov', fortcov_log)
        call run_fortcov(report_path, fail_under, fortcov_log, exitcode)
        call coverage_total_percent(report_path, total)
        if (len_trim(total) > 0) then
            write (output_unit, '(a,a)') 'Coverage: total ', trim(total)
        else
            write (output_unit, '(a,a)') 'Coverage: report ', trim(report_path)
        end if
        if (exitcode /= 0) then
            call report_stage_failure('cover', fortcov_log, 'fo cover')
            stop exitcode, quiet=.true.
        end if
        call delete_tmpfile(fortcov_log)
    end subroutine fo_cover_run

    subroutine parse_cover_args(include_slow, fail_under)
        logical, intent(out) :: include_slow
        character(len=*), intent(out) :: fail_under
        character(len=256) :: arg
        integer :: i, eq

        include_slow = .false.
        fail_under = ''
        i = 2
        do while (i <= command_argument_count())
            call get_command_argument(i, arg)
            if (trim(arg) == '--all') then
                include_slow = .true.
            else if (trim(arg) == '--fail-under') then
                if (i == command_argument_count()) then
                    write (error_unit, '(a)') 'fo cover: --fail-under needs a value'
                    stop 1, quiet=.true.
                end if
                i = i + 1
                call get_command_argument(i, fail_under)
            else
                eq = index(arg, '--fail-under=')
                if (eq == 1) then
                    fail_under = arg(14:)
                else
                    write (error_unit, '(a,a)') 'fo cover: unknown option: ', trim(arg)
                    stop 1, quiet=.true.
                end if
            end if
            i = i + 1
        end do
    end subroutine parse_cover_args

    logical function has_help_arg()
        character(len=256) :: arg
        integer :: i

        has_help_arg = .false.
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--help' .or. trim(arg) == '-h') then
                has_help_arg = .true.
                return
            end if
        end do
    end function has_help_arg

    subroutine print_cover_usage()
        write (output_unit, '(a)') 'usage: fo cover [--all] [--fail-under N]'
        write (output_unit, '(a)') 'Build tests with coverage flags, run tests, then run fortcov.'
    end subroutine print_cover_usage

    subroutine run_fortcov(report_path, fail_under, log_file, exitcode)
        character(len=*), intent(in) :: report_path, fail_under, log_file
        integer, intent(out) :: exitcode
        character(len=:), allocatable :: packed
        character(len=128) :: gcov_command
        integer :: n_args

        call coverage_gcov_command(gcov_command)
        n_args = 0
        call argv_push(packed, n_args, 'fortcov')
        call argv_push(packed, n_args, '--gcov')
        call argv_push(packed, n_args, '--no-auto-test')
        call argv_push(packed, n_args, '--gcov-executable')
        call argv_push(packed, n_args, trim(gcov_command))
        call argv_push(packed, n_args, '--gcov-output-dir')
        call argv_push(packed, n_args, 'build/gcov')
        call argv_push(packed, n_args, '--output')
        call argv_push(packed, n_args, trim(report_path))
        if (len_trim(fail_under) > 0) then
            call argv_push(packed, n_args, '--fail-under')
            call argv_push(packed, n_args, trim(fail_under))
        end if
        call process_run_argv_logged('.', packed, n_args, log_file, .false., 0, &
            exitcode)
    end subroutine run_fortcov

    subroutine coverage_gcov_command(command)
        character(len=*), intent(out) :: command
        character(len=128) :: override, version, major, candidate
        character(len=512) :: tmpfile, line
        integer :: status, unit, ios, dot

        command = 'gcov'
        call get_environment_variable('FO_GCOV', override, status=status)
        if (status == 0 .and. len_trim(override) > 0) then
            command = trim(override)
            return
        end if

        call make_tmpfile('fo-gfortran-version', tmpfile)
        call execute_command_line('gfortran -dumpversion > '//trim(tmpfile), &
            exitstat=status)
        if (status /= 0) then
            call delete_tmpfile(tmpfile)
            return
        end if

        line = ''
        open (newunit=unit, file=trim(tmpfile), status='old', action='read', &
            iostat=ios)
        if (ios == 0) then
            read (unit, '(a)', iostat=ios) line
            close (unit)
        end if
        call delete_tmpfile(tmpfile)
        if (ios /= 0) return

        version = adjustl(line)
        dot = index(version, '.')
        if (dot > 1) then
            major = version(1:dot - 1)
        else
            major = trim(version)
        end if
        if (len_trim(major) == 0) return

        candidate = 'gcov-'//trim(major)
        call execute_command_line(trim(candidate)//' --version >/dev/null 2>&1', &
            exitstat=status)
        if (status == 0) command = trim(candidate)
    end subroutine coverage_gcov_command

    subroutine report_stage_failure(kind, log_file, rerun)
        character(len=*), intent(in) :: kind, log_file, rerun
        type(diagnostic_t) :: diag
        character(len=32) :: lnum

        call diagnostic_from_log(kind, log_file, rerun, diag)
        write (error_unit, '(a,a,a,a)') 'fo cover: ', trim(kind), &
            ' failed: ', trim(diag%message)
        if (len_trim(diag%file) > 0) then
            write (lnum, '(i0)') diag%line
            write (error_unit, '(a,a,a,a)') 'fo cover: at: ', trim(diag%file), &
                ':', trim(lnum)
        end if
        write (error_unit, '(a,a)') 'fo cover: full log: ', trim(log_file)
    end subroutine report_stage_failure

    subroutine coverage_total_percent(report_path, total)
        character(len=*), intent(in) :: report_path
        character(len=*), intent(out) :: total
        character(len=512) :: line
        integer :: u, ios

        total = ''
        open (newunit=u, file=trim(report_path), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'TOTAL') > 0) call percent_from_line(line, total)
        end do
        close (u)
    end subroutine coverage_total_percent

    subroutine percent_from_line(line, total)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: total
        integer :: pct, start

        pct = index(line, '%', back=.true.)
        if (pct == 0) return
        start = pct
        do while (start > 1)
            select case (line(start - 1:start - 1))
            case (' ', '|', char(9))
                exit
            case default
                start = start - 1
            end select
        end do
        total = line(start:pct)
    end subroutine percent_from_line

end module fo_cover
