program test_check
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_check, only: check_result_t, fo_check_run, check_result_json, &
                        check_result_compact_json, check_result_full_json
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_check_from_child_reports_backend_error()
    call test_check_reports_test_failure_advice()
    call test_check_result_json()
    call test_check_result_compact_json_success()
    call test_check_result_compact_json_failure()
    call test_check_result_full_json_diagnostics()

    write (output_unit, '(a,i0,a,i0,a)') 'check: ', n_pass, ' pass, ', n_fail, ' fail'
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

    subroutine test_check_from_child_reports_backend_error()
        type(check_result_t) :: res
        character(len=512) :: project_dir

        call make_tmp_path('fo_bad_project', project_dir)
        call make_bad_project(project_dir)
        call fo_check_run(trim(project_dir)//'/src/nested', res)

        call assert(.not. res%build_ok, 'bad project build fails')
        call assert( &
            index(res%error_msg, 'Fatal Error:') > 0 .or. &
            index(res%error_msg, 'Error:') > 0, &
            'error summary includes compiler line')
        call assert(index(res%error_msg, 'log: /tmp/fo-build-') > 0, &
                    'error summary includes build log')
        call assert(index(res%error_msg, 'rerun: fo build') > 0, &
                    'error summary includes rerun command')
        call assert(res%diag_line > 0 .and. res%diag_column > 0, &
                    'build diagnostic includes numeric location')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_check_from_child_reports_backend_error

    subroutine test_check_reports_test_failure_advice()
        type(check_result_t) :: res
        character(len=512) :: project_dir

        call make_tmp_path('fo_failing_test_project', project_dir)
        call make_failing_test_project(project_dir)
        call fo_check_run(trim(project_dir)//'/test', res)

        call assert(res%build_ok, 'failing-test project builds')
        call assert(.not. res%tests_ok, 'failing-test project tests fail')
        call assert(index(res%error_msg, 'rerun: fo test') > 0, &
                    'test error summary includes rerun command')
        call assert(index(res%error_msg, 'timed-out tests faster') > 0 .and. &
                    index(res%error_msg, '*_slow') > 0, &
                    'test error summary includes slow-test advice')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_check_reports_test_failure_advice

    subroutine test_check_result_json()
        type(check_result_t) :: res
        character(len=2048) :: line

        res%build_ok = .true.
        res%tests_ok = .false.
        res%n_modules = 7
        res%n_cached = 3
        res%n_changed = 2
        res%n_affected = 5
        res%elapsed = 0.25
        res%error_msg = 'bad "quote" '//achar(92)//'path'

        line = check_result_json(res)

        call assert(index(line, '"build_ok":true') > 0, &
                    'json includes build_ok boolean')
        call assert(index(line, '"tests_ok":false') > 0, &
                    'json includes tests_ok boolean')
        call assert(index(line, '"modules":7') > 0, &
                    'json includes module count')
        call assert(index(line, '"elapsed_s":0.250') > 0, &
                    'json includes valid elapsed number')
        if (index(line, 'bad '//achar(92)//'"quote') <= 0) then
            write (error_unit, '(a,a)') 'json line: ', trim(line)
        end if
        call assert(index(line, 'bad '//achar(92)//'"quote') > 0, &
                    'json escapes quotes')
        call assert(index(line, achar(92)//achar(92)//'path') > 0, &
                    'json escapes backslashes')
    end subroutine test_check_result_json

    subroutine test_check_result_compact_json_success()
        type(check_result_t) :: res
        character(len=2048) :: line

        res%build_ok = .true.
        res%tests_ok = .true.
        res%elapsed = 0.125
        res%stage = 'done'

        line = check_result_compact_json(res)

        call assert(index(line, '"ok":true') > 0, &
                    'compact success includes ok')
        call assert(index(line, '"stage":"done"') > 0, &
                    'compact success includes done stage')
        call assert(index(line, '"summary":"build and tests passed"') > 0, &
                    'compact success includes summary')
        call assert(len_trim(line) < 8192, 'compact success stays bounded')
    end subroutine test_check_result_compact_json_success

    subroutine test_check_result_compact_json_failure()
        type(check_result_t) :: res
        character(len=2048) :: line

        res%build_ok = .true.
        res%tests_ok = .false.
        res%stage = 'test'
        res%target = 'test_x'
        res%summary = 'test_x returned exit code 1'
        res%hint = 'make this test faster or mark it slow'
        res%rerun = 'fo test test_x'
        res%log_path = '/tmp/fo-test.log'
        res%elapsed = 0.5

        line = check_result_compact_json(res)

        call assert(index(line, '"ok":false') > 0, &
                    'compact failure includes ok false')
        call assert(index(line, '"stage":"test"') > 0, &
                    'compact failure includes stage')
        call assert(index(line, '"target":"test_x"') > 0, &
                    'compact failure includes target')
        call assert(index(line, '"rerun":"fo test test_x"') > 0, &
                    'compact failure includes rerun')
        call assert(index(line, 'make this test faster or mark it slow') > 0, &
                    'compact failure includes slow hint')
        call assert(len_trim(line) < 8192, 'compact failure stays bounded')
    end subroutine test_check_result_compact_json_failure

    subroutine test_check_result_full_json_diagnostics()
        type(check_result_t) :: res
        character(len=4096) :: line

        res%build_ok = .false.
        res%tests_ok = .false.
        res%stage = 'build'
        res%summary = 'src/x.f90:12:5: Error: bad token'
        res%hint = 'fix the first compiler diagnostic, then rerun fo build'
        res%rerun = 'fo build'
        res%log_path = '/tmp/fo-build.log'
        res%diag_file = 'src/x.f90'
        res%diag_line = 12
        res%diag_column = 5
        res%elapsed = 0.75

        line = check_result_full_json(res)

        call assert(index(line, '"build_ok":false') > 0, &
                    'full json keeps legacy fields')
        call assert(index(line, '"diagnostics":[{') > 0, &
                    'full json includes diagnostics array')
        call assert(index(line, '"kind":"build"') > 0, &
                    'full json diagnostic includes kind')
        call assert(index(line, '"file":"src/x.f90"') > 0, &
                    'full json diagnostic includes file')
        call assert(index(line, '"line":12') > 0, &
                    'full json diagnostic includes line')
        call assert(index(line, '"column":5') > 0, &
                    'full json diagnostic includes column')
        call assert(index(line, '"log_path":"/tmp/fo-build.log"') > 0, &
                    'full json includes log path')
    end subroutine test_check_result_full_json_diagnostics

    subroutine make_bad_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('mkdir -p '//trim(project_dir)//'/src/nested')

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_bad_project"'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/src/broken.f90', status='replace')
        write (u, '(a)') 'module broken'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine fail()'
        write (u, '(a)') 'integer :: x'
        write (u, '(a)') 'x ='
        write (u, '(a)') 'end subroutine fail'
        write (u, '(a)') 'end module broken'
        close (u)
    end subroutine make_bad_project

    subroutine make_failing_test_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('mkdir -p '//trim(project_dir)//'/src')
        call execute_command_line('mkdir -p '//trim(project_dir)//'/test')

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_failing_test_project"'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/src/ok.f90', &
              status='replace')
        write (u, '(a)') 'module ok'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine noop()'
        write (u, '(a)') 'end subroutine noop'
        write (u, '(a)') 'end module ok'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/test/test_fail.f90', &
              status='replace')
        write (u, '(a)') 'program test_fail'
        write (u, '(a)') 'use ok, only: noop'
        write (u, '(a)') 'call noop()'
        write (u, '(a)') 'stop 1'
        write (u, '(a)') 'end program test_fail'
        close (u)
    end subroutine make_failing_test_project

    subroutine make_tmp_path(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0)') '/tmp/', trim(prefix), '-', &
            count, '-', serial
    end subroutine make_tmp_path

end program test_check
