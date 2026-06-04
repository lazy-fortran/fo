program test_backend
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_build_backend, only: backend_t, detect_backend, detect_nproc, &
                                BACKEND_FPM, BACKEND_CMAKE, BACKEND_NONE
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_detect_fpm()
    call test_detect_fpm_from_child()
    call test_detect_cmake()
    call test_detect_none()
    call test_nproc()
    call test_fpm_skips_slow_by_default()

    write (output_unit, '(a,i0,a,i0,a)') 'backend: ', n_pass, ' pass, ', n_fail, ' fail'
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

    subroutine test_detect_fpm()
        type(backend_t) :: b
        integer :: u

        call execute_command_line('mkdir -p /tmp/fo_test_fpm')
        open (newunit=u, file='/tmp/fo_test_fpm/fpm.toml', status='replace')
        write (u, '(a)') 'name = "test"'
        close (u)

        b = detect_backend('/tmp/fo_test_fpm')
        call assert(b%kind == BACKEND_FPM, 'detect fpm')

        call execute_command_line('rm -rf /tmp/fo_test_fpm')
    end subroutine test_detect_fpm

    subroutine test_detect_fpm_from_child()
        type(backend_t) :: b
        integer :: u

        call execute_command_line('mkdir -p /tmp/fo_test_fpm_parent/src/nested')
        open (newunit=u, file='/tmp/fo_test_fpm_parent/fpm.toml', status='replace')
        write (u, '(a)') 'name = "test"'
        close (u)

        b = detect_backend('/tmp/fo_test_fpm_parent/src/nested')
        call assert(b%kind == BACKEND_FPM, 'detect fpm from child')
        call assert(trim(b%project_dir) == '/tmp/fo_test_fpm_parent', &
                    'detected project root')

        call execute_command_line('rm -rf /tmp/fo_test_fpm_parent')
    end subroutine test_detect_fpm_from_child

    subroutine test_detect_cmake()
        type(backend_t) :: b
        integer :: u

        call execute_command_line('mkdir -p /tmp/fo_test_cmake')
        open (newunit=u, file='/tmp/fo_test_cmake/CMakeLists.txt', status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        close (u)

        b = detect_backend('/tmp/fo_test_cmake')
        call assert(b%kind == BACKEND_CMAKE, 'detect cmake')

        call execute_command_line('rm -rf /tmp/fo_test_cmake')
    end subroutine test_detect_cmake

    subroutine test_detect_none()
        type(backend_t) :: b

        call execute_command_line('mkdir -p /tmp/fo_test_none')

        b = detect_backend('/tmp/fo_test_none')
        call assert(b%kind == BACKEND_NONE, 'detect none')

        call execute_command_line('rm -rf /tmp/fo_test_none')
    end subroutine test_detect_none

    subroutine test_nproc()
        integer :: np

        np = detect_nproc()
        call assert(np >= 1, 'nproc >= 1')
    end subroutine test_nproc

    subroutine test_fpm_skips_slow_by_default()
        type(backend_t) :: b
        integer :: exitcode

        call make_slow_fpm_project()

        b = detect_backend('/tmp/fo_test_slow_fpm')
        call b%test(exitcode, log_file='/tmp/fo_backend_fast.log')
        call assert(exitcode == 0, 'fpm test skips slow by default')

        call b%test(exitcode, include_slow=.true., &
                    log_file='/tmp/fo_backend_slow.log')
        call assert(exitcode /= 0, 'fpm test --all includes slow')

        call execute_command_line('rm -rf /tmp/fo_test_slow_fpm')
        call execute_command_line('rm -f /tmp/fo_backend_fast.log '// &
                                  '/tmp/fo_backend_slow.log')
    end subroutine test_fpm_skips_slow_by_default

    subroutine make_slow_fpm_project()
        integer :: u

        call execute_command_line('rm -rf /tmp/fo_test_slow_fpm')
        call execute_command_line('mkdir -p /tmp/fo_test_slow_fpm/src')
        call execute_command_line('mkdir -p /tmp/fo_test_slow_fpm/test')

        open (newunit=u, file='/tmp/fo_test_slow_fpm/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_test_slow_fpm"'
        close (u)

        open (newunit=u, file='/tmp/fo_test_slow_fpm/src/lib.f90', &
              status='replace')
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine noop()'
        write (u, '(a)') 'end subroutine noop'
        write (u, '(a)') 'end module lib'
        close (u)

        open (newunit=u, file='/tmp/fo_test_slow_fpm/test/test_fast.f90', &
              status='replace')
        write (u, '(a)') 'program test_fast'
        write (u, '(a)') 'end program test_fast'
        close (u)

        open (newunit=u, file='/tmp/fo_test_slow_fpm/test/test_kernel_slow.f90', &
              status='replace')
        write (u, '(a)') 'program test_kernel_slow'
        write (u, '(a)') 'stop 1'
        write (u, '(a)') 'end program test_kernel_slow'
        close (u)
    end subroutine make_slow_fpm_project

end program test_backend
