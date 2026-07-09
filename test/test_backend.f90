program test_backend
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_build_backend, only: backend_t, detect_backend, detect_nproc, &
        detect_jobs, backend_build, backend_test, &
        backend_test_names, backend_clean, &
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
    call test_detect_fpm()
    call test_detect_fpm_from_child()
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
    call test_backend_clean_keeps_shared_store()
    call test_backend_clean_purge_removes_store()

    call report('backend')

contains

    subroutine test_backend_clean_keeps_shared_store()
        use fo_cache, only: cache_root
        character(len=512) :: project_dir, root, sentinel, marker
        logical :: build_removed, store_removed, marker_exists, sentinel_exists
        integer :: u

        call make_tmp_path('fo_test_clean_keep', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir)//'/build/fo')
        marker = trim(project_dir)//'/build/fo/marker'
        open (newunit=u, file=trim(marker), status='replace')
        write (u, '(a)') 'x'
        close (u)
        call cache_root(root)
        call execute_command_line('mkdir -p '//trim(root)//'/store/v1')
        sentinel = trim(root)//'/store/v1/clean_sentinel'
        open (newunit=u, file=trim(sentinel), status='replace')
        write (u, '(a)') 'x'
        close (u)

        call backend_clean(trim(project_dir), .false., build_removed, &
            store_removed)

        inquire (file=trim(marker), exist=marker_exists)
        inquire (file=trim(sentinel), exist=sentinel_exists)
        call assert(build_removed, 'plain clean reports build removed')
        call assert(.not. marker_exists, 'plain clean drops project build tree')
        call assert(.not. store_removed, &
            'plain clean does not report store removed')
        call assert(sentinel_exists, 'plain clean preserves the shared store')
    end subroutine test_backend_clean_keeps_shared_store

    subroutine test_backend_clean_purge_removes_store()
        use fo_cache, only: cache_root
        character(len=512) :: project_dir, root, sentinel
        logical :: build_removed, store_removed, sentinel_exists
        integer :: u

        call make_tmp_path('fo_test_clean_purge', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir))
        call cache_root(root)
        call execute_command_line('mkdir -p '//trim(root)//'/store/v1')
        sentinel = trim(root)//'/store/v1/purge_sentinel'
        open (newunit=u, file=trim(sentinel), status='replace')
        write (u, '(a)') 'x'
        close (u)

        call backend_clean(trim(project_dir), .true., build_removed, &
            store_removed)

        inquire (file=trim(sentinel), exist=sentinel_exists)
        call assert(store_removed, 'purge clean reports store removed')
        call assert(.not. sentinel_exists, 'purge clean removes the shared store')
    end subroutine test_backend_clean_purge_removes_store

    include 'test_backend_helpers.inc'

end program test_backend
