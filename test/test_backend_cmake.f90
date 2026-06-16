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
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_cmake_skips_slow_by_default()
    call test_cmake_named_tests_select_regex()
    call test_cmake_path_with_spaces()
    call test_cmake_regex_metachar_name()

    call report('backend_cmake')

contains

    include 'test_backend_helpers.inc'

end program test_backend_cmake
