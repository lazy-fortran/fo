program test_backend_cmake
    !! CMake backend tests (configure+build+ctest), split out so the slow
    !! CMake cycles run in parallel with the gfortran tests.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_build_backend, only: backend_t, detect_backend, detect_nproc, &
        detect_jobs, backend_build, backend_test, &
        backend_test_names, &
        BACKEND_CMAKE, BACKEND_NONE, BACKEND_GFORTRAN
    use fo_gfortran_build, only: gfortran_build, gfortran_test, &
        gfortran_test_names, config_flags_str
    use fo_fpm_config, only: fpm_config_t
    use fo_process, only: process_getpid
    use fo_exec_target, only: resolve_exec_target
    implicit none
    integer :: n_pass, n_fail

    interface
        function setenv(name, value, overwrite) bind(C, name='setenv') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: ierr
        end function setenv

        function unsetenv(name) bind(C, name='unsetenv') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: ierr
        end function unsetenv
    end interface

    n_pass = 0
    n_fail = 0

    call test_cmake_skips_slow_by_default()
    call test_cmake_named_tests_select_regex()
    call test_cmake_path_with_spaces()
    call test_cmake_regex_metachar_name()
    call test_cmake_dirty_fetchcontent_retries_clean()
    call test_cmake_env_args_reach_configure()
    call test_cmake_exec_resolves_build_root_binary()

    call report('backend_cmake')

contains

    subroutine test_cmake_exec_resolves_build_root_binary()
        type(backend_t) :: b
        character(len=512) :: project_dir, bin_path
        integer :: u
        logical :: found

        call make_tmp_path('fo_test_cmake_exec_target', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir)//'/build')
        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/build/simple.x', status='replace')
        write (u, '(a)') '#!/bin/sh'
        close (u)

        b = detect_backend(project_dir)
        call resolve_exec_target(b, 'simple', bin_path, found)
        call assert(found, 'cmake exec resolves build/<target>.x')
        call assert(trim(bin_path) == trim(project_dir)//'/build/simple.x', &
            'cmake exec path points at build/simple.x')

        call resolve_exec_target(b, 'simple.x', bin_path, found)
        call assert(found, 'cmake exec resolves explicit .x target')
        call assert(trim(bin_path) == trim(project_dir)//'/build/simple.x', &
            'cmake explicit .x path points at build/simple.x')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_cmake_exec_resolves_build_root_binary

    subroutine test_cmake_dirty_fetchcontent_retries_clean()
        type(backend_t) :: b
        integer :: exitcode, u
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_cmake_dirty_fetchcontent', project_dir)
        call make_tmp_path('fo_backend_cmake_dirty_fetchcontent', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/build/_deps/libneo-src')

        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        write (u, '(a)') 'project(fo_backend_cmake_dirty_fetchcontent NONE)'
        write (u, '(a)') 'if(EXISTS "${CMAKE_BINARY_DIR}/_deps/libneo-src")'
        write (u, '(a)') '  message(FATAL_ERROR "Failed to unstash changes in: ${CMAKE_BINARY_DIR}/_deps/libneo-src")'
        write (u, '(a)') 'endif()'
        close (u)

        b = detect_backend(project_dir)
        call backend_build(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, &
            'cmake dirty FetchContent checkout clears build tree and retries')
        call assert(file_exists(trim(project_dir)//'/build/CMakeCache.txt'), &
            'cmake retry leaves configured build tree')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_cmake_dirty_fetchcontent_retries_clean

    subroutine test_cmake_env_args_reach_configure()
        type(backend_t) :: b
        integer :: exitcode, u, ierr
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_cmake_env_args', project_dir)
        call make_tmp_path('fo_backend_cmake_env_args', log_file)
        call remove_tree(project_dir)
        call make_dir(project_dir)

        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        write (u, '(a)') 'project(fo_backend_cmake_env_args NONE)'
        write (u, '(a)') 'if(NOT FO_TEST_OPTION STREQUAL "enabled")'
        write (u, '(a)') '  message(FATAL_ERROR "FO_TEST_OPTION not configured")'
        write (u, '(a)') 'endif()'
        close (u)

        ierr = setenv('FO_CMAKE_ARGS'//c_null_char, &
            '-DFO_TEST_OPTION=enabled'//c_null_char, 1_c_int)
        call assert(ierr == 0, 'set FO_CMAKE_ARGS')

        b = detect_backend(project_dir)
        call backend_build(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'cmake configure receives FO_CMAKE_ARGS')

        ierr = unsetenv('FO_CMAKE_ARGS'//c_null_char)
        call assert(ierr == 0, 'unset FO_CMAKE_ARGS')
        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_cmake_env_args_reach_configure

    include 'test_backend_helpers.inc'

end program test_backend_cmake
