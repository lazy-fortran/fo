program test_backend_gfortran
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_build_backend, only: backend_t, detect_backend, detect_nproc, &
        detect_jobs, backend_build, backend_test, &
        backend_test_names, &
        BACKEND_NATIVE, BACKEND_NONE
    use fo_gfortran_build, only: gfortran_build, gfortran_test, &
        gfortran_test_names, config_flags_str
    use fo_fpm_config, only: fpm_config_t
    use fo_process, only: process_getpid
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call isolate_backend_cache()
    call test_gfortran_flags_change_action_id()
    call test_gfortran_compiler_identity_changes_action_id()
    call test_gfortran_private_change_keeps_dependent_cached()
    call test_gfortran_interface_change_rebuilds_dependent()
    call test_gfortran_parallel_test_loop_restores_cached_objects()
    call test_gfortran_test_links_helper_modules_and_lib()
    call test_gfortran_named_test_links_helper_modules()
    call test_gfortran_builds_path_dependency()
    call test_gfortran_names_binary_from_manifest_executable()
    call test_gfortran_path_dep_ignores_coexisting_fpm_tree()
    call test_gfortran_test_link_ignores_coexisting_fpm_tree()
    call test_gfortran_test_drops_stale_path_dep_objects()
    call test_gfortran_link_failure_reports_fail()
    call test_gfortran_bootstraps_git_dependency()
    call test_gfortran_dep_library_object_marker_not_dropped()
    call test_gfortran_test_builds_dev_dependency()
    call test_fpm_path_with_spaces()
    call test_gfortran_rejects_compile_errors()

    call report('backend_gfortran')

contains

    include 'test_backend_helpers.inc'

end program test_backend_gfortran
