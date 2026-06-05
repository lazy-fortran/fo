program test_backend
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_build_backend, only: backend_t, detect_backend, detect_nproc, &
                                detect_jobs, &
                                BACKEND_CMAKE, BACKEND_NONE, BACKEND_GFORTRAN
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_detect_fpm()
    call test_detect_fpm_from_child()
    call test_detect_cmake()
    call test_detect_cmake_over_fpm()
    call test_detect_none()
    call test_nproc()
    call test_detect_jobs()
    call test_fpm_skips_slow_by_default()
    call test_cmake_skips_slow_by_default()
    call test_cmake_named_tests_select_regex()
    call test_fpm_path_with_spaces()
    call test_cmake_path_with_spaces()
    call test_cmake_regex_metachar_name()

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
        call assert(b%kind == BACKEND_GFORTRAN, 'detect fpm.toml -> gfortran backend')

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
        call assert(b%kind == BACKEND_GFORTRAN, 'detect fpm.toml from child -> gfortran backend')
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

    subroutine test_detect_cmake_over_fpm()
        type(backend_t) :: b
        integer :: u
        character(len=512) :: project_dir

        call make_tmp_path('fo_test_cmake_over_fpm', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir))
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "test"'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', &
              status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        close (u)

        b = detect_backend(project_dir)
        call assert(b%kind == BACKEND_CMAKE, 'detect cmake over fpm')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_detect_cmake_over_fpm

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

    subroutine test_detect_jobs()
        integer :: jobs

        jobs = detect_jobs()
        call assert(jobs >= 1, 'jobs >= 1')
    end subroutine test_detect_jobs

    subroutine test_fpm_skips_slow_by_default()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_gfortran_run', project_dir)
        call make_tmp_path('fo_backend_gfortran', log_file)
        call make_simple_fpm_project(project_dir)

        b = detect_backend(project_dir)
        call b%build(exitcode, log_file=log_file)
        call assert(exitcode == 0, 'gfortran build succeeds on simple project')
        call b%test(exitcode, log_file=log_file)
        call assert(exitcode == 0, 'gfortran test runs passing tests')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_fpm_skips_slow_by_default

    subroutine test_cmake_skips_slow_by_default()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: project_dir, fast_log, slow_log

        call make_tmp_path('fo_test_slow_cmake', project_dir)
        call make_tmp_path('fo_backend_cmake_fast', fast_log)
        call make_tmp_path('fo_backend_cmake_slow', slow_log)
        call make_cmake_tests_project(project_dir, .false.)

        b = detect_backend(project_dir)
        call b%test(exitcode, log_file=fast_log)
        call assert(exitcode == 0, 'cmake test skips excluded labels by default')

        call b%test(exitcode, include_slow=.true., log_file=slow_log)
        call assert(exitcode /= 0, 'cmake test --all includes slow')

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('rm -f '//trim(fast_log)//' '//trim(slow_log))
    end subroutine test_cmake_skips_slow_by_default

    subroutine test_cmake_named_tests_select_regex()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: project_dir, log_file
        character(len=128) :: names(2)

        call make_tmp_path('fo_test_named_cmake', project_dir)
        call make_tmp_path('fo_backend_cmake_named', log_file)
        call make_cmake_tests_project(project_dir, .true.)

        b = detect_backend(project_dir)
        names(1) = 'test_a'
        names(2) = 'test_b'
        call b%test_names(names, 2, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'cmake named tests select requested tests')
        call assert(file_contains(log_file, 'Test #1: test_a') .or. &
                    file_contains(log_file, 'Test #2: test_a') .or. &
                    file_contains(log_file, 'test_a'), &
                    'cmake named log includes selected test')

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_cmake_named_tests_select_regex

    subroutine test_fpm_path_with_spaces()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: base_dir, project_dir, log_file

        call make_tmp_path('fo_test_fpm_spaces', base_dir)
        project_dir = trim(base_dir)//' path with spaces'
        call make_tmp_path('fo_backend_fpm_spaces', log_file)
        call make_simple_fpm_project(project_dir)

        b = detect_backend(project_dir)
        call b%build(exitcode, log_file=log_file)
        call assert(exitcode == 0, 'fpm build handles project path with spaces')
        call b%test(exitcode, log_file=log_file)
        call assert(exitcode == 0, 'fpm test handles project path with spaces')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_fpm_path_with_spaces

    subroutine test_cmake_path_with_spaces()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: base_dir, project_dir, log_file

        call make_tmp_path('fo_test_cmake_spaces', base_dir)
        project_dir = trim(base_dir)//' path with spaces'
        call make_tmp_path('fo_backend_cmake_spaces', log_file)
        call make_cmake_space_project(project_dir)

        b = detect_backend(project_dir)
        call b%build(exitcode, log_file=log_file)
        call assert(exitcode == 0, 'cmake build handles project path with spaces')
        call b%test(exitcode, log_file=log_file)
        call assert(exitcode == 0, 'ctest handles project path with spaces')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_cmake_path_with_spaces

    subroutine test_cmake_regex_metachar_name()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: project_dir, log_file
        character(len=128) :: names(1)

        call make_tmp_path('fo_test_cmake_regex', project_dir)
        call make_tmp_path('fo_backend_cmake_regex', log_file)
        call make_cmake_regex_project(project_dir)

        b = detect_backend(project_dir)
        call b%build(exitcode, log_file=log_file)
        call assert(exitcode == 0, 'cmake regex fixture configures')

        names(1) = 'test.dot'
        call b%test_names(names, 1, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'ctest selected names escape regex metacharacters')
        call assert(file_contains(log_file, 'test.dot'), &
                    'ctest regex fixture runs selected dotted name')

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_cmake_regex_metachar_name

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

    subroutine make_simple_fpm_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/test')

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_test_spaces"'
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
    end subroutine make_simple_fpm_project

    subroutine make_cmake_tests_project(project_dir, unselected_fails)
        character(len=*), intent(in) :: project_dir
        logical, intent(in) :: unselected_fails
        integer :: u, exitcode

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('mkdir -p '//trim(project_dir))

        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', &
              status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        write (u, '(a)') 'project(fo_backend_cmake_tests NONE)'
        write (u, '(a)') 'enable_testing()'
        write (u, '(a)') 'add_test(NAME test_a COMMAND ${CMAKE_COMMAND} -E true)'
        write (u, '(a)') 'add_test(NAME test_b COMMAND ${CMAKE_COMMAND} -E true)'
        if (unselected_fails) then
            write (u, '(a)') &
                'add_test(NAME test_unselected COMMAND ${CMAKE_COMMAND} -E false)'
        else
            write (u, '(a)') &
                'add_test(NAME test_unselected COMMAND ${CMAKE_COMMAND} -E true)'
        end if
        write (u, '(a)') &
            'add_test(NAME test_kernel_slow COMMAND ${CMAKE_COMMAND} -E false)'
        write (u, '(a)') &
            'set_tests_properties(test_kernel_slow PROPERTIES LABELS slow)'
        write (u, '(a)') &
            'add_test(NAME test_regression COMMAND ${CMAKE_COMMAND} -E false)'
        write (u, '(a)') &
            'set_tests_properties(test_regression PROPERTIES LABELS regression)'
        write (u, '(a)') &
            'add_test(NAME test_performance COMMAND ${CMAKE_COMMAND} -E false)'
        write (u, '(a)') &
            'set_tests_properties(test_performance PROPERTIES LABELS performance)'
        write (u, '(a)') &
            'add_test(NAME test_scalability COMMAND ${CMAKE_COMMAND} -E false)'
        write (u, '(a)') &
            'set_tests_properties(test_scalability PROPERTIES LABELS scalability)'
        close (u)

        call execute_command_line('cd '//trim(project_dir)// &
                                  ' && cmake -S . -B build >/dev/null 2>&1', &
                                  exitstat=exitcode, wait=.true.)
    end subroutine make_cmake_tests_project

    subroutine make_cmake_space_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call remove_tree(project_dir)
        call make_dir(project_dir)

        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', &
              status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        write (u, '(a)') 'project(fo_backend_cmake_spaces NONE)'
        write (u, '(a)') 'enable_testing()'
        write (u, '(a)') 'add_test(NAME test_space COMMAND ${CMAKE_COMMAND} -E true)'
        close (u)
    end subroutine make_cmake_space_project

    subroutine make_cmake_regex_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('mkdir -p '//trim(project_dir))

        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', &
              status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        write (u, '(a)') 'project(fo_backend_cmake_regex NONE)'
        write (u, '(a)') 'enable_testing()'
        write (u, '(a)') 'add_test(NAME test.dot COMMAND ${CMAKE_COMMAND} -E true)'
        write (u, '(a)') 'add_test(NAME testXdot COMMAND ${CMAKE_COMMAND} -E false)'
        close (u)
    end subroutine make_cmake_regex_project

    logical function file_contains(path, needle)
        character(len=*), intent(in) :: path, needle

        character(len=512) :: line
        integer :: u, iostat

        file_contains = .false.
        open (newunit=u, file=path, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, trim(needle)) > 0) then
                file_contains = .true.
                exit
            end if
        end do
        close (u)
    end function file_contains

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

    subroutine make_dir(path)
        character(len=*), intent(in) :: path

        call execute_command_line('mkdir -p "'//trim(path)//'"')
    end subroutine make_dir

    subroutine remove_tree(path)
        character(len=*), intent(in) :: path

        call execute_command_line('rm -rf "'//trim(path)//'"')
    end subroutine remove_tree

end program test_backend
