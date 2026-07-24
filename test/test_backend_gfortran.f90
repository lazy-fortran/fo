program test_backend_gfortran
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_build_backend, only: backend_t, detect_backend, detect_nproc, &
        detect_jobs, backend_build, backend_test, &
        backend_test_names, backend_test_affected, &
        BACKEND_NATIVE, BACKEND_CMAKE, BACKEND_NONE
    use fo_gfortran_build, only: gfortran_build, gfortran_test, &
        gfortran_test_names, config_flags_str
    use fo_fpm_config, only: fpm_config_t
    use fo_cache, only: cache_t, cache_init, cache_key_for, cache_store_action, &
        HASH_LEN
    use fo_process, only: process_getpid
    use fo_compiler_flags, only: append_array_temporary_warning_flag
    use fx_dag, only: MAX_NODES
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
    call test_gfortran_worktree_path_dep_bootstraps_git_dependency()
    call test_gfortran_dep_library_object_marker_not_dropped()
    call test_gfortran_test_builds_dev_dependency()
    call test_fpm_path_with_spaces()
    call test_gfortran_rejects_compile_errors()
    call test_gfortran_rebuilds_cached_module_without_mod()
    call test_array_temporary_warning_flag_policy()
    call test_gfortran_warns_about_array_temporaries()
    call test_gfortran_named_tests_fit_default_stack()

    call report('backend_gfortran')

contains

    subroutine test_array_temporary_warning_flag_policy()
        character(len=128) :: flags

        flags = ''
        call append_array_temporary_warning_flag('GNU Fortran 15.1', flags)
        call assert(trim(flags) == '-Warray-temporaries', &
            'GNU Fortran receives the default array-temporary warning flag')
        call append_array_temporary_warning_flag('gfortran', flags)
        call assert(trim(flags) == '-Warray-temporaries', &
            'default array-temporary warning flag is not duplicated')
        flags = '-O3 -Wno-array-temporaries'
        call append_array_temporary_warning_flag('gfortran', flags)
        call assert(trim(flags) == '-O3 -Wno-array-temporaries', &
            'explicit array-temporary warning opt-out is preserved')
        flags = ''
        call append_array_temporary_warning_flag('flang-new', flags)
        call assert(len_trim(flags) == 0, &
            'non-GNU compiler does not receive a GNU-only warning flag')
    end subroutine test_array_temporary_warning_flag_policy

    subroutine test_gfortran_warns_about_array_temporaries()
        type(backend_t) :: backend
        character(len=512) :: project_dir, log_file, source
        integer :: exitcode, u

        call make_tmp_path('fo_array_temporary_project', project_dir)
        call make_tmp_path('fo_array_temporary_build', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "array_temporary_fixture"'
        close (u)
        source = trim(project_dir)//'/src/fixture.f90'
        open (newunit=u, file=trim(source), status='replace')
        write (u, '(a)') 'module array_temporary_fixture'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine trigger(matrix)'
        write (u, '(a)') 'real, intent(in) :: matrix(2, 2)'
        write (u, '(a)') 'call consume(matrix(1, :))'
        write (u, '(a)') 'end subroutine trigger'
        write (u, '(a)') 'subroutine consume(vector)'
        write (u, '(a)') 'real, contiguous, intent(in) :: vector(:)'
        write (u, '(a)') 'end subroutine consume'
        write (u, '(a)') 'end module array_temporary_fixture'
        close (u)

        backend = detect_backend(project_dir)
        call backend_build(backend, exitcode, log_file=log_file, &
            with_tests=.true., use_cache=.false.)
        call assert(exitcode == 0, 'array-temporary warning fixture builds')
        call assert(file_contains(log_file, 'array temporary'), &
            'gfortran build emits array-temporary warnings by default')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_warns_about_array_temporaries
    subroutine test_gfortran_named_tests_fit_default_stack()
        character(len=512), volatile :: filenames(MAX_NODES)
        character(len=512), volatile :: changed_files(MAX_NODES)
        character(len=512), volatile :: lint_files(MAX_NODES)
        character(len=128), volatile :: test_names(MAX_NODES)
        character(len=512) :: project_dir, dependency_dir, log_file
        character(len=128) :: selected(1)
        integer :: exitcode

        call make_tmp_path('fo_stack_project', project_dir)
        call make_tmp_path('fo_stack_dependency', dependency_dir)
        call make_tmp_path('fo_stack_backend', log_file)
        call make_linked_named_project(project_dir, dependency_dir)
        filenames = project_dir
        changed_files = dependency_dir
        lint_files = log_file
        test_names = 'test_a'
        selected(1) = 'test_a'

        call gfortran_test_names(project_dir, selected, 1, log_file, exitcode)

        call assert(exitcode == 0 .and. &
            filenames(MAX_NODES) == project_dir .and. &
            changed_files(MAX_NODES) == dependency_dir .and. &
            lint_files(MAX_NODES) == log_file .and. &
            test_names(MAX_NODES) == 'test_a', &
            'named test with pipeline state fits the default stack')
        call remove_tree(project_dir)
        call remove_tree(dependency_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_named_tests_fit_default_stack

    subroutine make_linked_named_project(project_dir, dependency_dir)
        character(len=*), intent(in) :: project_dir, dependency_dir
        character(len=1024) :: command
        integer :: u

        call make_named_fpm_project(project_dir)
        call make_dir(trim(dependency_dir)//'/src')
        call make_dir(trim(dependency_dir)//'/build')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_stack_project"'
        write (u, '(a)') '[build]'
        write (u, '(a)') 'link = ["stack_dependency"]'
        write (u, '(a)') '[dependencies]'
        write (u, '(a)') 'stack_dependency = { path = "'// &
            trim(dependency_dir)//'" }'
        close (u)
        open (newunit=u, file=trim(dependency_dir)//'/fpm.toml', &
            status='replace')
        write (u, '(a)') 'name = "stack_dependency"'
        close (u)
        open (newunit=u, file=trim(dependency_dir)//'/src/marker.f90', &
            status='replace')
        write (u, '(a)') 'module stack_dependency_marker'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'end module stack_dependency_marker'
        close (u)
        command = 'ar rcs "'//trim(dependency_dir)// &
            '/build/libstack_dependency.a"'
        call execute_command_line(trim(command))
    end subroutine make_linked_named_project

    subroutine test_gfortran_rebuilds_cached_module_without_mod()
        type(cache_t) :: cache
        character(len=512) :: project_dir, log_file, source, object, mod_dir
        character(len=HASH_LEN) :: action_id, output_id, dep_keys(1)
        integer :: u, ierr, exitcode, n_compiled
        logical :: mod_exists

        call make_tmp_path('fo_test_missing_cached_mod', project_dir)
        call make_tmp_path('fo_backend_missing_cached_mod', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "missing_cached_mod"'
        close (u)
        source = trim(project_dir)//'/src/provider.f90'
        open (newunit=u, file=trim(source), status='replace')
        write (u, '(a)') 'module provider'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'end module provider'
        close (u)

        call make_dir(trim(project_dir)//'/seed')
        object = trim(project_dir)//'/seed/provider.o'
        mod_dir = trim(project_dir)//'/seed/mod'
        call make_dir(mod_dir)
        open (newunit=u, file=trim(object), status='replace')
        write (u, '(a)') 'cached object without module output'
        close (u)
        dep_keys = ''
        action_id = cache_key_for(source, 'fixture-compiler', '', dep_keys, 0)
        call cache_init(cache, ierr)
        call cache_store_action(cache, action_id, object, mod_dir, 'provider', &
            output_id, ierr)

        call gfortran_build(project_dir, log_file, exitcode, n_compiled, &
            compiler_id='fixture-compiler')
        inquire (file=trim(project_dir)//'/build/fo/mod/provider.mod', &
            exist=mod_exists)
        call assert(exitcode == 0 .and. n_compiled == 1 .and. mod_exists, &
            'module cache hit without .mod recompiles provider')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_rebuilds_cached_module_without_mod

    include 'test_backend_helpers.inc'

end program test_backend_gfortran
