program test_check
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_check, only: check_result_t, fo_check_run, check_result_json
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_check_from_child_reports_backend_error()
    call test_check_reports_test_failure_advice()
    call test_check_result_json()

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

        call make_bad_project()
        call fo_check_run('/tmp/fo_bad_project/src/nested', res)

        call assert(.not. res%build_ok, 'bad project build fails')
        call assert( &
            index(res%error_msg, 'Fatal Error:') > 0 .or. &
            index(res%error_msg, 'Error:') > 0, &
            'error summary includes compiler line')
        call assert(index(res%error_msg, 'log: /tmp/fo-build-') > 0, &
                    'error summary includes build log')
        call assert(index(res%error_msg, 'rerun: fo build') > 0, &
                    'error summary includes rerun command')

        call execute_command_line('rm -rf /tmp/fo_bad_project')
    end subroutine test_check_from_child_reports_backend_error

    subroutine test_check_reports_test_failure_advice()
        type(check_result_t) :: res

        call make_failing_test_project()
        call fo_check_run('/tmp/fo_failing_test_project/test', res)

        call assert(res%build_ok, 'failing-test project builds')
        call assert(.not. res%tests_ok, 'failing-test project tests fail')
        call assert(index(res%error_msg, 'rerun: fo test') > 0, &
                    'test error summary includes rerun command')
        call assert(index(res%error_msg, 'timed-out tests faster') > 0 .and. &
                    index(res%error_msg, '*_slow') > 0, &
                    'test error summary includes slow-test advice')

        call execute_command_line('rm -rf /tmp/fo_failing_test_project')
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

    subroutine make_bad_project()
        integer :: u

        call execute_command_line('rm -rf /tmp/fo_bad_project')
        call execute_command_line('mkdir -p /tmp/fo_bad_project/src/nested')

        open (newunit=u, file='/tmp/fo_bad_project/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_bad_project"'
        close (u)

        open (newunit=u, file='/tmp/fo_bad_project/src/broken.f90', status='replace')
        write (u, '(a)') 'module broken'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine fail()'
        write (u, '(a)') 'integer :: x'
        write (u, '(a)') 'x ='
        write (u, '(a)') 'end subroutine fail'
        write (u, '(a)') 'end module broken'
        close (u)
    end subroutine make_bad_project

    subroutine make_failing_test_project()
        integer :: u

        call execute_command_line('rm -rf /tmp/fo_failing_test_project')
        call execute_command_line('mkdir -p /tmp/fo_failing_test_project/src')
        call execute_command_line('mkdir -p /tmp/fo_failing_test_project/test')

        open (newunit=u, file='/tmp/fo_failing_test_project/fpm.toml', &
              status='replace')
        write (u, '(a)') 'name = "fo_failing_test_project"'
        close (u)

        open (newunit=u, file='/tmp/fo_failing_test_project/src/ok.f90', &
              status='replace')
        write (u, '(a)') 'module ok'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'end module ok'
        close (u)

        open (newunit=u, file='/tmp/fo_failing_test_project/test/test_fail.f90', &
              status='replace')
        write (u, '(a)') 'program test_fail'
        write (u, '(a)') 'stop 1'
        write (u, '(a)') 'end program test_fail'
        close (u)
    end subroutine make_failing_test_project

end program test_check
