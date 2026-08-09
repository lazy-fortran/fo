program test_backend
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_build_backend, only: backend_t, detect_backend, detect_nproc, &
        detect_jobs, backend_build, backend_test, &
        backend_test_names, backend_test_affected, backend_clean, &
        BACKEND_NATIVE, BACKEND_CMAKE, BACKEND_NONE
    use fo_gfortran_build, only: gfortran_build, gfortran_test, &
        gfortran_test_names, config_flags_str
    use fo_fpm_config, only: fpm_config_t
    use fo_process, only: process_getpid
    use fo_exec_target, only: resolve_exec_target
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call isolate_backend_cache()
    call test_detect_fpm()
    call test_detect_fpm_preferred_over_cmake()
    call test_detect_cmake_tests_preferred_over_fpm()
    call test_detect_cmake_override()
    call test_detect_fpm_from_child()
    call test_detect_cmake_from_child()
    call test_detect_none()
    call test_nproc()
    call test_detect_jobs()
    call test_config_flags_str_joins_with_spaces()
    call test_fpm_skips_slow_by_default()
    call test_cmake_build_and_test()
    call test_native_combined_build_keeps_apps()
    call test_cmake_named_test_rebuilds_changed_source()
    call test_cmake_exec_target_resolution()
    call test_cmake_affected_tests_use_registered_names()
    call test_native_honors_auto_executables()
    call test_gfortran_named_tests_select_requested()
    call test_gfortran_named_test_restores_cached_object()
    call test_gfortran_recovers_from_root_mod_shadow()
    call test_gfortran_restores_deleted_outputs()
    call test_gfortran_app_main_keeps_package_name()
    call test_backend_clean_keeps_shared_store()
    call test_backend_clean_purge_removes_store()

    call report('backend')

contains

    subroutine test_native_combined_build_keeps_apps()
        type(backend_t) :: b
        character(len=512) :: project_dir, log_file
        integer :: u, exitcode
        logical :: app_exists

        call make_tmp_path('fo_test_combined_apps', project_dir)
        call make_tmp_path('fo_test_combined_apps_log', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/app')
        call make_dir(trim(project_dir)//'/test')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "combined_apps"'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/app/tool.f90', status='replace')
        write (u, '(a)') 'program tool'
        write (u, '(a)') 'end program tool'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/test/check.f90', status='replace')
        write (u, '(a)') 'program check'
        write (u, '(a)') 'end program check'
        close (u)

        b = detect_backend(project_dir)
        call backend_build(b, exitcode, log_file=log_file, with_tests=.true.)
        inquire (file=trim(project_dir)//'/build/fo/bin/tool', exist=app_exists)
        call assert(exitcode == 0 .and. app_exists, &
            'combined native build keeps application targets')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_native_combined_build_keeps_apps

    subroutine test_native_honors_auto_executables()
        !! fpm's auto-executables switch is the oracle: an explicit executable
        !! remains available, while an unregistered app program is not a target.
        type(backend_t) :: b
        integer :: exitcode, u
        character(len=512) :: base, log_file, bin_path
        logical :: selected_exists, ignored_exists, found

        call make_tmp_path('fo_test_auto_executables', base)
        call make_tmp_path('fo_backend_auto_executables', log_file)
        call make_dir(trim(base)//'/app')
        open (newunit=u, file=trim(base)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "auto_executables"'
        close (u)
        open (newunit=u, file=trim(base)//'/app/selected.f90', status='replace')
        write (u, '(a)') 'program selected'
        write (u, '(a)') 'end program selected'
        close (u)
        open (newunit=u, file=trim(base)//'/app/ignored.f90', status='replace')
        write (u, '(a)') 'program ignored'
        write (u, '(a)') 'end program ignored'
        close (u)

        b = detect_backend(base)
        call backend_build(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'auto-executables project builds initially')
        inquire (file=trim(base)//'/build/fo/bin/ignored', exist=ignored_exists)
        call assert(ignored_exists, 'auto-discovered app program is initially built')

        open (newunit=u, file=trim(base)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "auto_executables"'
        write (u, '(a)') '[build]'
        write (u, '(a)') 'auto-executables = false'
        write (u, '(a)') '[[executable]]'
        write (u, '(a)') 'name = "selected"'
        write (u, '(a)') 'source-dir = "app"'
        write (u, '(a)') 'main = "selected.f90"'
        close (u)

        call backend_build(b, exitcode, log_file=log_file)
        inquire (file=trim(base)//'/build/fo/bin/selected', exist=selected_exists)
        inquire (file=trim(base)//'/build/fo/bin/ignored', exist=ignored_exists)
        call resolve_exec_target(b, 'selected', bin_path, found)
        call assert(exitcode == 0, 'auto-executables=false project builds')
        call assert(found, 'explicit executable remains resolvable')
        call resolve_exec_target(b, 'ignored', bin_path, found)
        call assert(.not. found, 'undeclared app program is not resolvable')
        call assert(selected_exists .and. .not. ignored_exists, &
            'only the explicit app program is linked')

        call remove_tree(base)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_native_honors_auto_executables

    subroutine test_detect_cmake_override()
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

        type(backend_t) :: b
        character(len=512) :: project_dir
        integer :: u
        integer(c_int) :: ierr

        call make_tmp_path('fo_test_cmake_override', project_dir)
        call make_dir(project_dir)
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "cmake_override"'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', status='replace')
        close (u)

        ierr = setenv('FO_BACKEND'//c_null_char, 'cmake'//c_null_char, 1_c_int)
        call assert(ierr == 0, 'set FO_BACKEND for CMake override')
        b = detect_backend(project_dir)
        call assert(b%kind == BACKEND_CMAKE, &
            'FO_BACKEND=cmake selects CMake when both manifests coexist')
        ierr = unsetenv('FO_BACKEND'//c_null_char)
        call assert(ierr == 0, 'unset FO_BACKEND after CMake override')
        call remove_tree(project_dir)
    end subroutine test_detect_cmake_override

    subroutine test_cmake_exec_target_resolution()
        type(backend_t) :: b
        character(len=512) :: base, bin_path
        integer :: u
        logical :: found

        call make_tmp_path('fo_test_cmake_exec', base)
        call make_dir(trim(base)//'/build/subdir')
        open (newunit=u, file=trim(base)//'/CMakeLists.txt', status='replace')
        close (u)
        open (newunit=u, file=trim(base)//'/build/subdir/demo.x', status='replace')
        close (u)

        b = detect_backend(base)
        call resolve_exec_target(b, 'demo.x', bin_path, found)
        call assert(found, 'CMake executable resolves by unique basename')
        call assert(trim(bin_path) == trim(base)//'/build/subdir/demo.x', &
            'CMake executable resolves to its nested build path')
        call resolve_exec_target(b, 'subdir/demo.x', bin_path, found)
        call assert(found, 'CMake executable resolves by build-relative path')

        call make_dir(trim(base)//'/build/other')
        open (newunit=u, file=trim(base)//'/build/other/demo.x', status='replace')
        close (u)
        call resolve_exec_target(b, 'demo.x', bin_path, found)
        call assert(.not. found, 'ambiguous CMake executable name is rejected')

        call remove_tree(base)
    end subroutine test_cmake_exec_target_resolution

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
