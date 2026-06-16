program test_backend
    !! Backend detection + first half of the gfortran build/cache tests. Heavy
    !! build tests are split across test_backend_gfortran and test_backend_cmake
    !! so fo's process-parallel runner runs them at once. Shared helpers are in
    !! test_backend_helpers.inc.
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

    call test_detect_fpm()
    call test_detect_fpm_from_child()
    call test_detect_cmake()
    call test_detect_cmake_over_fpm()
    call test_detect_none()
    call test_nproc()
    call test_detect_jobs()
    call test_config_flags_str_joins_with_spaces()
    call test_fpm_skips_slow_by_default()
    call test_gfortran_named_tests_select_requested()
    call test_gfortran_named_test_restores_cached_object()
    call test_gfortran_recovers_from_root_mod_shadow()
    call test_gfortran_restores_deleted_outputs()
    call test_gfortran_app_main_keeps_package_name()

    call report('backend')

contains

    include 'test_backend_helpers.inc'

end program test_backend
