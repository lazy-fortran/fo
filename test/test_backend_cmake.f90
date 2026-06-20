program test_backend_cmake
    !! CMake backend tests (configure+build+ctest), split out so the slow
    !! CMake cycles run in parallel with the gfortran tests.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
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

    n_pass = 0
    n_fail = 0

    call test_cmake_skips_slow_by_default()
    call test_cmake_named_tests_select_regex()
    call test_cmake_path_with_spaces()
    call test_cmake_regex_metachar_name()
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

    include 'test_backend_helpers.inc'

end program test_backend_cmake
