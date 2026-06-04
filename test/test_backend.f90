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
        character(len=512) :: project_dir

        call make_tmp_path('fo_test_fpm', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir))
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "test"'
        close (u)

        b = detect_backend(project_dir)
        call assert(b%kind == BACKEND_FPM, 'detect fpm')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_detect_fpm

    subroutine test_detect_fpm_from_child()
        type(backend_t) :: b
        integer :: u
        character(len=512) :: project_dir

        call make_tmp_path('fo_test_fpm_parent', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir)//'/src/nested')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "test"'
        close (u)

        b = detect_backend(trim(project_dir)//'/src/nested')
        call assert(b%kind == BACKEND_FPM, 'detect fpm from child')
        call assert(trim(b%project_dir) == trim(project_dir), &
                    'detected project root')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_detect_fpm_from_child

    subroutine test_detect_cmake()
        type(backend_t) :: b
        integer :: u
        character(len=512) :: project_dir

        call make_tmp_path('fo_test_cmake', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir))
        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', &
              status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        close (u)

        b = detect_backend(project_dir)
        call assert(b%kind == BACKEND_CMAKE, 'detect cmake')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_detect_cmake

    subroutine test_detect_none()
        type(backend_t) :: b
        character(len=512) :: project_dir

        call make_tmp_path('fo_test_none', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir))

        b = detect_backend(project_dir)
        call assert(b%kind == BACKEND_NONE, 'detect none')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_detect_none

    subroutine test_nproc()
        integer :: np

        np = detect_nproc()
        call assert(np >= 1, 'nproc >= 1')
    end subroutine test_nproc

    subroutine test_fpm_skips_slow_by_default()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: project_dir, fast_log, slow_log

        call make_tmp_path('fo_test_slow_fpm', project_dir)
        call make_tmp_path('fo_backend_fast', fast_log)
        call make_tmp_path('fo_backend_slow', slow_log)
        call make_slow_fpm_project(project_dir)

        b = detect_backend(project_dir)
        call b%test(exitcode, log_file=fast_log)
        call assert(exitcode == 0, 'fpm test skips slow by default')

        call b%test(exitcode, include_slow=.true., &
                    log_file=slow_log)
        call assert(exitcode /= 0, 'fpm test --all includes slow')

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('rm -f '//trim(fast_log)//' '//trim(slow_log))
    end subroutine test_fpm_skips_slow_by_default

    subroutine make_slow_fpm_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('mkdir -p '//trim(project_dir)//'/src')
        call execute_command_line('mkdir -p '//trim(project_dir)//'/test')

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_test_slow_fpm"'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/src/lib.f90', &
              status='replace')
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine noop()'
        write (u, '(a)') 'end subroutine noop'
        write (u, '(a)') 'end module lib'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/test/test_fast.f90', &
              status='replace')
        write (u, '(a)') 'program test_fast'
        write (u, '(a)') 'use lib, only: noop'
        write (u, '(a)') 'call noop()'
        write (u, '(a)') 'end program test_fast'
        close (u)

        open (newunit=u, file=trim(project_dir)// &
              '/test/test_kernel_slow.f90', status='replace')
        write (u, '(a)') 'program test_kernel_slow'
        write (u, '(a)') 'use lib, only: noop'
        write (u, '(a)') 'call noop()'
        write (u, '(a)') 'stop 1'
        write (u, '(a)') 'end program test_kernel_slow'
        close (u)
    end subroutine make_slow_fpm_project

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

end program test_backend
