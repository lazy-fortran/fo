program test_backend_gfortran
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
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
    use fo_process, only: process_getpid, process_getcwd, &
        process_run_argv_logged, argv_push
    use fo_fs, only: fs_find_executable, fs_remove_file, fs_identity, fs_stat
    use fo_compiler_flags, only: append_array_temporary_warning_flag, append_pipe_flag
    use fo_linker_policy, only: linker_should_try_lld
    use fx_dag, only: MAX_NODES
    implicit none
    integer :: n_pass, n_fail, n_args
    character(len=64) :: selector

    n_pass = 0
    n_fail = 0
    n_args = command_argument_count()
    selector = ''
    if (n_args > 0) call get_command_argument(1, selector)
    if (n_args > 1) selector = '__invalid__'

    call isolate_backend_cache()
    if (n_args == 0) then
        call test_gfortran_flags_change_action_id()
        call test_compiler_baseline_flags_change_action_id()
        call test_gfortran_compiler_identity_changes_action_id()
        call test_gfortran_private_change_keeps_dependent_cached()
        call test_gfortran_interface_change_rebuilds_dependent()
        call test_gfortran_parallel_test_loop_restores_cached_objects()
        call test_gfortran_parallel_warm_restore()
        call test_gfortran_multiple_modules()
        call test_gfortran_test_skips_app_but_build_restores_it()
        call test_gfortran_test_links_helper_modules_and_lib()
        call test_gfortran_named_test_links_helper_modules()
        call test_gfortran_named_test_uses_manifest_name()
        call test_gfortran_app_links_only_reachable_library_objects()
        call test_compiler_switch_clears_the_tree()
        call test_slow_test_gets_its_own_timeout()
        call test_test_budget_respects_available_clock()
        call test_process_tree_supervision()
        call test_gfortran_builds_manifest_example()
        call test_gfortran_preprocesses_lowercase_f90()
        call test_gfortran_builds_nested_auto_example()
        call test_gfortran_builds_c_source_with_public_header()
        call test_gfortran_builds_path_dependency()
        call test_gfortran_names_binary_from_manifest_executable()
        call test_gfortran_path_dep_ignores_coexisting_fpm_tree()
        call test_gfortran_test_link_ignores_coexisting_fpm_tree()
        call test_gfortran_test_drops_stale_path_dep_objects()
        call test_gfortran_link_failure_reports_fail()
        call test_gfortran_bootstraps_git_dependency()
        call test_gfortran_bootstraps_git_dev_dependency_closure()
        call test_gfortran_repairs_partial_git_dependency_artifacts()
        call test_gfortran_worktree_path_dep_bootstraps_git_dependency()
        call test_gfortran_dep_library_object_marker_not_dropped()
        call test_gfortran_test_builds_dev_dependency()
        call test_fpm_path_with_spaces()
        call test_gfortran_rejects_compile_errors()
        call test_gfortran_rebuilds_cached_module_without_mod()
        call test_array_temporary_warning_flag_policy()
        call test_pipe_flag_policy()
        call test_linker_policy()
        call test_lld_failure_falls_back_to_default_linker()
        call test_gfortran_warns_about_array_temporaries()
        call test_gfortran_named_tests_fit_default_stack()
        call test_gfortran_passes_manifest_test_arguments()
        call test_backend_tmpdir()
        call report('backend_gfortran')
    else
        select case (trim(selector))
        case ('multiple-modules')
            call test_gfortran_multiple_modules()
            call report('backend_gfortran/multiple-modules')
        case ('parallel-warm-restore')
            call test_gfortran_parallel_warm_restore()
            call report('backend_gfortran/parallel-warm-restore')
        case ('dependency-bootstrap')
            call test_gfortran_bootstraps_git_dev_dependency_closure()
            call report('backend_gfortran/dependency-bootstrap')
        case ('process-tree-timeout')
            call test_process_tree_supervision()
            call report('backend_gfortran/process-tree-timeout')
        case ('named-stack')
            call test_gfortran_named_tests_fit_default_stack()
            call report('backend_gfortran/named-stack')
        case ('tmpdir')
            call test_backend_tmpdir()
            call report('backend_gfortran/tmpdir')
        case ('tmpdir-child')
            call emit_backend_tmp_path()
        case default
            write (error_unit, '(a)') 'unknown backend_gfortran subcase: '// &
                trim(selector)
            n_fail = 1
            call report('backend_gfortran/unknown-subcase')
        end select
    end if

contains

    subroutine test_gfortran_multiple_modules()
        character(len=512) :: project, log_file, binary, output, provider
        integer :: u, code, compiled

        call make_tmp_path('fo_multiple_modules', project)
        log_file = trim(project)//'.log'
        output = trim(project)//'.out'
        call remove_tree(project)
        call make_dir(trim(project)//'/src')
        call make_dir(trim(project)//'/app')
        open (newunit=u, file=trim(project)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name="multiple_modules"'
        close (u)
        open (newunit=u, file=trim(project)//'/src/m_base.f90', status='replace')
        write (u, '(a)') 'module base_scope'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'integer, parameter :: base=50'
        write (u, '(a)') 'end module'
        close (u)
        open (newunit=u, file=trim(project)//'/src/a_consumer.f90', status='replace')
        write (u, '(a)') 'module consumer'
        write (u, '(a)') 'use secondary, only: value'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'integer function answer()'
        write (u, '(a)') 'answer=value()+1'
        write (u, '(a)') 'end function'
        write (u, '(a)') 'end module'
        close (u)
        open (newunit=u, file=trim(project)//'/app/main.f90', status='replace')
        write (u, '(a)') 'program main'
        write (u, '(a)') 'use consumer, only: answer'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'print *, answer()'
        write (u, '(a)') 'end program'
        close (u)
        provider = trim(project)//'/src/z_provider.f90'
        call write_multiple_module_provider(provider, 10)
        call gfortran_build(project, log_file, code, compiled)
        call assert(code == 0 .and. compiled == 4, &
            'cold source graph compiles two-module provider once before second user')
        binary = trim(project)//'/build/fo/bin/multiple_modules'
        call run_backend_argv(binary, [character(len=1) ::], output, code)
        call assert(code == 0 .and. file_contains(output, '61'), &
            'second module implementation produces independent runtime value61')
        call remove_tree(trim(project)//'/build/fo')
        call gfortran_build(project, log_file, code, compiled)
        call assert(code == 0 .and. compiled == 0, &
            'fresh warm tree restores complete multiple-module action without compile')
        call fs_remove_file(trim(project)//'/build/fo/mod/secondary.mod')
        call gfortran_build(project, log_file, code, compiled)
        call assert(code == 0 .and. compiled == 0, &
            'missing second interface restores from complete warm packet')
        call run_backend_argv(binary, [character(len=1) ::], output, code)
        call assert(code == 0 .and. file_contains(output, '61'), &
            'restored second module keeps runtime value61')
        call write_multiple_module_provider(provider, 11)
        call gfortran_build(project, log_file, code, compiled)
        call assert(code == 0 .and. compiled == 1, &
            'second module body change recompiles provider while clients stay cached')
        call run_backend_argv(binary, [character(len=1) ::], output, code)
        call assert(code == 0 .and. file_contains(output, '62'), &
            'changed second-module body reaches runtime value62')
        call remove_tree(project)
        call fs_remove_file(log_file)
        call fs_remove_file(output)
    end subroutine test_gfortran_multiple_modules

    subroutine write_multiple_module_provider(path, value)
        character(len=*), intent(in) :: path
        integer, intent(in) :: value
        integer :: u
        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') 'module primary'
        write (u, '(a)') 'use base_scope, only: base'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'end module'
        write (u, '(a)') 'module secondary'
        write (u, '(a)') 'use primary, only: base'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'integer function value()'
        write (u, '(a,i0)') 'value=base+', value
        write (u, '(a)') 'end function'
        write (u, '(a)') 'end module'
        close (u)
    end subroutine write_multiple_module_provider

    subroutine test_gfortran_parallel_warm_restore()
        !! Independent modules restore together; the app consumes their .mods
        !! only after the whole provider level has completed.
        character(len=512) :: project_dir, log_file, source, binary, output
        character(len=32) :: name, value
        character(len=32) :: old_jobs
        integer :: i, u, exitcode, n_first, n_restore, env_status, restore_exit

        call make_tmp_path('fo_parallel_warm_restore', project_dir)
        log_file = trim(project_dir)//'.log'
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/app')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "parallel_warm_restore"'
        close (u)

        do i = 1, 12
            write (name, '(a,i0)') 'provider_', i
            write (value, '(i0)') i
            source = trim(project_dir)//'/src/'//trim(name)//'.f90'
            open (newunit=u, file=trim(source), status='replace')
            write (u, '(a)') 'module '//trim(name)
            write (u, '(a)') 'integer, parameter :: value = '//trim(value)
            write (u, '(a)') 'end module '//trim(name)
            close (u)
        end do
        source = trim(project_dir)//'/app/main.f90'
        open (newunit=u, file=trim(source), status='replace')
        write (u, '(a)') 'program main'
        do i = 1, 12
            write (name, '(a,i0)') 'provider_', i
            write (u, '(a)') 'use '//trim(name)//', only: value_'// &
                trim(name)//' => value'
        end do
        write (u, '(a)') 'integer :: total'
        write (u, '(a)') 'total = 0'
        do i = 1, 12
            write (name, '(a,i0)') 'provider_', i
            write (u, '(a)') 'total = total + value_'//trim(name)
        end do
        write (u, '(a)') 'if (total /= 78) error stop 1'
        write (u, '(a)') 'print *, total'
        write (u, '(a)') 'end program main'
        close (u)

        old_jobs = ''
        call get_environment_variable('FO_JOBS', old_jobs, status=env_status)
        call set_env('FO_JOBS', '8')
        call gfortran_build(project_dir, log_file, exitcode, n_first)
        call assert(exitcode == 0 .and. n_first == 13, &
            'parallel warm restore fixture compiles all sources once')
        call execute_command_line('rm -rf "'//trim(project_dir)// &
            '/build/fo/obj" "'//trim(project_dir)//'/build/fo/mod"')
        call gfortran_build(project_dir, log_file, exitcode, n_restore)
        restore_exit = exitcode
        if (restore_exit /= 0 .or. n_restore /= 0) then
            write(error_unit, '(a,i0,a,i0,a,i0)') &
                'warm restore diagnostic: cold actions=', n_first, &
                ', restored build exit=', restore_exit, ', compiler launches=', n_restore
            write(error_unit, '(a)') 'Retained warm restore project: '//trim(project_dir)
            write(error_unit, '(a)') 'Retained warm restore log: '//trim(log_file)
        end if
        call assert(exitcode == 0 .and. n_restore == 0, &
            'parallel warm restore reuses all cached actions')
        binary = trim(project_dir)//'/build/fo/bin/parallel_warm_restore'
        output = trim(project_dir)//'.out'
        call execute_command_line('"'//trim(binary)//'" > "'// &
            trim(output)//'"', exitstat=exitcode)
        call assert(exitcode == 0 .and. file_contains(output, '78'), &
            'dependent app executes with restored provider modules')

        if (env_status == 0) then
            call set_env('FO_JOBS', trim(old_jobs))
        else
            call set_env('FO_JOBS', '')
        end if
        if (restore_exit == 0 .and. n_restore == 0) then
            call remove_tree(project_dir)
            call execute_command_line('rm -f "'//trim(log_file)//'" "'// &
                trim(output)//'"')
        end if
    end subroutine test_gfortran_parallel_warm_restore

    subroutine test_compiler_baseline_flags_change_action_id()
        character(len=512) :: project_dir, log_file, gnu_wrapper, flang_wrapper
        character(len=512) :: old_fc
        integer :: u, exitcode, changed_exitcode, fc_status
        character(len=512) :: real_compiler
        logical :: compiler_found

        call make_tmp_path('fo_baseline_flags_project', project_dir)
        call make_tmp_path('fo_baseline_flags_log', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "baseline-flags-fixture"'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/src/fixture.f90', &
            status='replace')
        write (u, '(a)') 'module baseline_flags_fixture'
        write (u, '(a)') 'character(len=*), parameter :: long_value = "'// &
            repeat('x', 120)//'"'
        write (u, '(a)') 'end module baseline_flags_fixture'
        close (u)

        call fs_find_executable('gfortran', real_compiler, compiler_found)
        call assert(compiler_found, 'baseline flags fixture locates gfortran')
        if (.not. compiler_found) then
            call remove_tree(project_dir)
            call fs_remove_file(log_file)
            return
        end if
        gnu_wrapper = trim(project_dir)//'/fixture-gfortran'
        flang_wrapper = trim(project_dir)//'/fixture-flang'
        call compile_backend_command_fixture(gnu_wrapper, real_compiler)
        call compile_backend_command_fixture(flang_wrapper, real_compiler)
        call set_env('FO_TEST_BACKEND_REAL_COMPILER', trim(real_compiler))
        call set_env('FO_TEST_BACKEND_LLD_MARKER', '')
        call set_env('FO_TEST_BACKEND_WRAPPER_DEPTH', '')
        call get_environment_variable('FO_FC', old_fc, status=fc_status)

        call set_env('FO_FC', trim(gnu_wrapper))
        call gfortran_build(project_dir, log_file, exitcode)
        call assert(exitcode == 0, &
            'baseline flags fixture first build accepts a long free-form line')

        call set_env('FO_FC', trim(flang_wrapper))
        call gfortran_build(project_dir, log_file, exitcode)
        changed_exitcode = exitcode
        if (fc_status == 0) then
            call set_env('FO_FC', trim(old_fc))
        else
            call set_env('FO_FC', '')
        end if
        call set_env('FO_TEST_BACKEND_REAL_COMPILER', '')
        call set_env('FO_TEST_BACKEND_WRAPPER_DEPTH', '')
        ! Reset the backend's resolved executable before later tests supply a
        ! synthetic compiler identity without asking for compiler detection.
        call gfortran_build(project_dir, log_file, exitcode, use_cache=.false.)
        call assert(exitcode == 0, &
            'baseline flags fixture restores the original compiler')
        call assert(changed_exitcode /= 0, &
            'changed compiler baseline flags miss stamps and action cache')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_compiler_baseline_flags_change_action_id

    subroutine compile_backend_command_fixture(executable, compiler)
        character(len=*), intent(in) :: executable, compiler
        character(len=512) :: project_root, source, log_file
        character(len=512) :: arguments(3)
        integer :: cwd_status, run_status
        logical :: exists

        call process_getcwd(project_root, cwd_status)
        if (cwd_status /= 0) error stop 'cannot locate the fo source tree'
        source = trim(project_root)// &
            '/test-fixtures/fortran/fo_backend_gfortran_command.f90'
        call make_tmp_path('fo_backend_fixture_compile', log_file)
        arguments(1) = trim(source)
        arguments(2) = '-o'
        arguments(3) = trim(executable)
        call run_backend_argv(trim(compiler), arguments, log_file, run_status)
        if (run_status /= 0) error stop 'cannot compile native backend fixture'
        inquire (file=trim(executable), exist=exists)
        if (.not. exists) then
            error stop 'native backend fixture compiler made no executable'
        end if
        call fs_remove_file(log_file)
    end subroutine compile_backend_command_fixture

    subroutine run_backend_program(executable, log_file, exitcode)
        character(len=*), intent(in) :: executable, log_file
        integer, intent(out) :: exitcode
        character(len=:), allocatable :: packed
        integer :: n_args

        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call process_run_argv_logged('', packed, n_args, trim(log_file), &
            .false., 60, exitcode)
    end subroutine run_backend_program

    subroutine test_gfortran_preprocesses_lowercase_f90()
        character(len=512) :: project_dir, log_file
        integer :: u, exitcode

        call make_tmp_path('fo_preprocess_project', project_dir)
        call make_tmp_path('fo_preprocess_log', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/app')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "preprocess_fixture"'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/src/fixture.f90', &
            status='replace')
        write (u, '(a)') 'module preprocess_fixture'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine active_branch'
        write (u, '(a)') '#ifdef FO_INACTIVE_BRANCH'
        write (u, '(a)') 'call symbol_that_must_not_be_linked'
        write (u, '(a)') '#endif'
        write (u, '(a)') 'end subroutine active_branch'
        write (u, '(a)') 'end module preprocess_fixture'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/app/main.f90', &
            status='replace')
        write (u, '(a)') 'program main'
        write (u, '(a)') 'use preprocess_fixture, only: active_branch'
        write (u, '(a)') 'call active_branch'
        write (u, '(a)') 'end program main'
        close (u)

        call gfortran_build(project_dir, log_file, exitcode, use_cache=.false.)
        call assert(exitcode == 0, &
            'lowercase .f90 preprocesses inactive branches before linking')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_gfortran_preprocesses_lowercase_f90

    subroutine test_gfortran_passes_manifest_test_arguments()
        character(len=512) :: project_dir, log_file
        integer :: u, exitcode

        call make_tmp_path('fo_manifest_test_args', project_dir)
        call make_tmp_path('fo_manifest_test_args_log', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/test')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "manifest-test-args"'
        write (u, '(a)') '[extra.fo.test-args]'
        write (u, '(a)') 'test_arguments = ["first argument", "--second"]'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/test/test_arguments.f90', &
            status='replace')
        write (u, '(a)') 'program test_arguments'
        write (u, '(a)') 'character(len=32) :: first, second'
        write (u, '(a)') 'if (command_argument_count() /= 2) error stop 1'
        write (u, '(a)') 'call get_command_argument(1, first)'
        write (u, '(a)') 'call get_command_argument(2, second)'
        write (u, '(a)') 'if (trim(first) /= "first argument") error stop 2'
        write (u, '(a)') 'if (trim(second) /= "--second") error stop 3'
        write (u, '(a)') 'end program test_arguments'
        close (u)

        call gfortran_test(project_dir, log_file, exitcode, include_slow=.true., &
            use_cache=.false.)
        call assert(exitcode == 0, 'native test with manifest arguments passes')
        call assert(file_contains(log_file, 'TEST_RESULT test_arguments PASS'), &
            'native test with manifest arguments reports its result')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_gfortran_passes_manifest_test_arguments

    subroutine test_compiler_switch_clears_the_tree()
        !! The native build tree is shared by every compiler. Reusing it after
        !! a switch leaves incompatible modules and, worse, lets `fo exec` run
        !! a binary the caller did not ask for. The stamp must clear it.
        character(len=512) :: project_dir, log_file, marker
        integer :: u, exitcode
        logical :: exists

        call make_tmp_path('fo_compiler_switch', project_dir)
        call make_tmp_path('fo_compiler_switch_log', log_file)
        call remove_tree(project_dir)
        call make_simple_fpm_project(project_dir)
        call gfortran_build(project_dir, log_file, exitcode)
        call assert(exitcode == 0, 'compiler switch: first build succeeds')

        ! A marker inside the tree stands in for every artifact the previous
        ! compiler left behind.
        marker = trim(project_dir)//'/build/fo/obj/previous.marker'
        open (newunit=u, file=trim(marker), status='replace')
        write (u, '(a)') 'stale'
        close (u)

        call set_env('FO_FC', 'f77')
        call gfortran_build(project_dir, log_file, exitcode)
        call set_env('FO_FC', '')
        inquire (file=trim(marker), exist=exists)
        call assert(.not. exists, &
            'compiler switch: the previous tree is cleared')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_compiler_switch_clears_the_tree

    subroutine test_slow_test_gets_its_own_timeout()
        !! A test marked slow must be allowed to take longer than the fast
        !! budget. Both tests here sleep for the same time; only the name
        !! differs, so a shared budget makes the slow one time out too.
        character(len=512) :: project_dir, log_file
        integer :: u, exitcode

        call make_tmp_path('fo_slow_timeout', project_dir)
        call make_tmp_path('fo_slow_timeout_log', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/test')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "slow-timeout"'
        close (u)
        call write_sleeping_test(trim(project_dir)//'/test/test_patient_slow.f90', &
            'test_patient_slow')

        call set_env('FO_TEST_TIMEOUT', '1')
        call set_env('FO_SLOW_TEST_TIMEOUT', '60')
        call gfortran_test(project_dir, log_file, exitcode, include_slow=.true., &
            use_cache=.false.)
        call assert(exitcode == 0, 'a slow-marked test may exceed the fast budget')
        call assert(file_contains(log_file, 'TEST_RESULT test_patient_slow PASS'), &
            'the slow test reports a pass')

        call remove_tree(project_dir)
        call remove_tree(trim(project_dir)//'-fast')
        call make_dir(trim(project_dir)//'-fast/test')
        open (newunit=u, file=trim(project_dir)//'-fast/fpm.toml', status='replace')
        write (u, '(a)') 'name = "slow-timeout"'
        close (u)
        call write_sleeping_test(trim(project_dir)//'-fast/test/test_patient.f90', &
            'test_patient')
        call gfortran_test(trim(project_dir)//'-fast', log_file, exitcode, &
            include_slow=.true., use_cache=.false.)
        call assert(exitcode /= 0, 'an unmarked test still hits the fast budget')

        call set_env('FO_TEST_TIMEOUT', '')
        call set_env('FO_SLOW_TEST_TIMEOUT', '')
        call remove_tree(trim(project_dir)//'-fast')
        call fs_remove_file(log_file)
    end subroutine test_slow_test_gets_its_own_timeout

    subroutine test_test_budget_respects_available_clock()
        !! Linux measures live child CPU time; other platforms use a wall-time
        !! budget. Sleeping and busy children distinguish those behaviors, and
        !! each timeout must name the limit that fired.
        character(len=512) :: project_dir, log_file
        integer :: u, exitcode
        logical :: cpu_measurable

        call make_tmp_path('fo_cpu_budget', project_dir)
        call make_tmp_path('fo_cpu_budget_log', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/test')
        cpu_measurable = host_can_measure_child_cpu(project_dir)
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "cpu-budget"'
        close (u)
        call write_idle_test(trim(project_dir)//'/test/test_idle.f90', 'test_idle', 2)

        call set_env('FO_TEST_TIMEOUT', '1')
        call gfortran_test(project_dir, log_file, exitcode, use_cache=.false.)
        if (cpu_measurable) then
            call assert(exitcode == 0, 'cpu budget: an idle test outlives its budget')
            call assert(file_contains(log_file, 'TEST_RESULT test_idle PASS'), &
                'cpu budget: the idle test reports a pass')
        else
            call assert(exitcode == 124, 'fallback budget: an idle test is killed')
            call assert(file_contains(log_file, 'TEST_RESULT test_idle TIMEOUT'), &
                'fallback budget: the idle test reports a timeout')
            call assert(file_contains(log_file, 'budget of 1 s exceeded in wall time'), &
                'fallback budget: the timeout names the wall-time budget')
        end if

        call set_env('FO_TEST_WALL_TIMEOUT', '1')
        call write_idle_test(trim(project_dir)//'/test/test_idle.f90', 'test_idle', 30)
        call gfortran_test(project_dir, log_file, exitcode, use_cache=.false.)
        call assert(exitcode == 124, 'wall cap: a hung idle test is killed')
        call assert(file_contains(log_file, 'wall-clock cap of 1 s exceeded'), &
            'wall cap: the timeout names the wall-clock cap')
        call set_env('FO_TEST_WALL_TIMEOUT', '')
        call set_env('FO_TEST_TIMEOUT', '')

        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/test')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "cpu-budget"'
        write (u, '(a)') '[extra.fo]'
        write (u, '(a)') 'test-timeout = 1'
        close (u)
        call write_sleeping_test(trim(project_dir)//'/test/test_busy.f90', &
            'test_busy')
        call gfortran_test(project_dir, log_file, exitcode, use_cache=.false.)
        call assert(exitcode == 124, '[extra.fo] test-timeout: a busy test is killed')
        if (cpu_measurable) then
            call assert(file_contains(log_file, 'CPU budget of 1 s exceeded'), &
                'cpu budget: the timeout names the CPU budget')
        else
            call assert(file_contains(log_file, 'budget of 1 s exceeded in wall time'), &
                'fallback budget: the timeout names the wall-time budget')
        end if
        call assert(file_contains(log_file, 'TEST_RESULT test_busy TIMEOUT'), &
            'cpu budget: the busy test reports a timeout')

        call set_env('FO_TEST_TIMEOUT', '60')
        call gfortran_test(project_dir, log_file, exitcode, use_cache=.false.)
        call assert(exitcode == 0, 'FO_TEST_TIMEOUT overrides [extra.fo]')
        call set_env('FO_TEST_TIMEOUT', '')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_test_budget_respects_available_clock

    subroutine test_process_tree_supervision()
        use, intrinsic :: iso_c_binding, only: c_int
        interface
            integer(c_int) function c_kill(pid, signal) bind(C, name='kill')
                import :: c_int
                integer(c_int), value :: pid, signal
            end function c_kill
        end interface
        character(len=512) :: project_dir, log_file, tmpdir, source, pid_file
        character(len=64) :: selected(1)
        integer :: u, exitcode, clock, pid, sentinel_pid, env_status
        integer(c_int) :: rc
        logical :: alive

        call get_environment_variable('TMPDIR', tmpdir, status=env_status)
        if (env_status /= 0 .or. len_trim(tmpdir) == 0) &
            error stop 'process-tree fixture requires a task-private TMPDIR'
        call system_clock(clock)
        write (project_dir, '(a,a,i0,a,i0)') trim(tmpdir), &
            '/fo_backend_process_tree-', process_getpid(), '-', clock
        call remove_tree(trim(project_dir))
        call make_dir(trim(project_dir)//'/test')
        log_file = trim(project_dir)//'/test.log'
        source = trim(project_dir)//'/test/test_process_tree.f90'
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "process-tree"'
        write (u, '(a)') '[extra.fo]'
        write (u, '(a)') 'test-timeout = 1'
        write (u, '(a)') 'test-wall-timeout = 2'
        write (u, '(a)') '[[test]]'
        write (u, '(a)') 'name = "test_process_tree"'
        write (u, '(a)') 'source-dir = "test"'
        write (u, '(a)') 'main = "test_process_tree.f90"'
        close (u)
        open (newunit=u, file=trim(source), status='replace')
        write (u, '(a)') 'program test_process_tree'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'character(len=16) :: mode'
        write (u, '(a)') 'character(len=512) :: pid_file, command'
        write (u, '(a)') 'integer :: status, env_status'
        write (u, '(a)') 'call get_environment_variable(''FO_TEST_CHILD_MODE'', mode, status=env_status)'
        write (u, '(a)') 'if (env_status /= 0) error stop ''missing child mode'''
        write (u, '(a)') 'call get_environment_variable(''FO_TEST_CHILD_PID_FILE'', pid_file, status=env_status)'
        write (u, '(a)') 'if (env_status /= 0) error stop ''missing child PID path'''
        write (u, '(a)') 'select case (trim(mode))'
        write (u, '(a)') 'case (''busy'')'
        write (u, '(a)') 'command = "sh -c ''echo $$ > "//trim(pid_file)// &'
        write (u, '(a)') '    "; while :; do :; done''"'
        write (u, '(a)') 'case (''blocked'')'
        write (u, '(a)') 'command = "sh -c ''sleep 30 & echo $! > "//trim(pid_file)// &'
        write (u, '(a)') '    "; wait''"'
        write (u, '(a)') 'case (''complete'')'
        write (u, '(a)') 'command = "sh -c ''sleep 30 & echo $! > "//trim(pid_file)//"''"'
        write (u, '(a)') 'case default'
        write (u, '(a)') 'error stop ''invalid child mode'''
        write (u, '(a)') 'end select'
        write (u, '(a)') 'call execute_command_line(trim(command), exitstat=status)'
        write (u, '(a)') 'if (status /= 0) error stop ''child command failed'''
        write (u, '(a)') 'write (*, ''(a)'') ''PARENT_COMPLETE'''
        write (u, '(a)') 'end program test_process_tree'
        close (u)

        pid_file = trim(project_dir)//'/sentinel.pid'
        call execute_command_line("sh -c 'sleep 60 & echo $! > "// &
            trim(pid_file)//"'", exitstat=exitcode)
        call assert(exitcode == 0, 'starts an unrelated process sentinel')
        sentinel_pid = read_process_id(pid_file)
        call assert(sentinel_pid > 0, 'records the unrelated sentinel PID')
        alive = process_is_alive(sentinel_pid)
        call assert(alive, 'unrelated sentinel is running before the fixture')

        selected(1) = 'test_process_tree'
        call set_env('FO_TEST_TIMEOUT', '')
        call set_env('FO_TEST_WALL_TIMEOUT', '')
        call run_process_tree_case(project_dir, log_file, selected, 'busy', .true., &
            .true., sentinel_pid)
        call run_process_tree_case(project_dir, log_file, selected, 'busy', .false., &
            .true., sentinel_pid)
        call run_process_tree_case(project_dir, log_file, selected, 'blocked', .true., &
            .true., sentinel_pid)
        call run_process_tree_case(project_dir, log_file, selected, 'blocked', .false., &
            .true., sentinel_pid)
        call run_process_tree_case(project_dir, log_file, selected, 'complete', .true., &
            .false., sentinel_pid)
        call run_process_tree_case(project_dir, log_file, selected, 'complete', .false., &
            .false., sentinel_pid)

        rc = c_kill(int(sentinel_pid, c_int), 9_c_int)
        call assert(rc == 0, 'stops the exact unrelated sentinel')
        call set_env('FO_TEST_CHILD_MODE', '')
        call set_env('FO_TEST_CHILD_PID_FILE', '')
        call set_env('FO_TEST_TIMEOUT', '')
        call set_env('FO_TEST_WALL_TIMEOUT', '')
        call remove_tree(trim(project_dir))
    end subroutine test_process_tree_supervision

    subroutine run_process_tree_case(project_dir, log_file, selected, mode, named, &
            expect_timeout, sentinel_pid)
        use, intrinsic :: iso_c_binding, only: c_int
        interface
            integer(c_int) function c_kill(pid, signal) bind(C, name='kill')
                import :: c_int
                integer(c_int), value :: pid, signal
            end function c_kill
        end interface
        character(len=*), intent(in) :: project_dir, log_file, mode
        character(len=*), intent(in) :: selected(:)
        logical, intent(in) :: named, expect_timeout
        integer, intent(in) :: sentinel_pid
        character(len=512) :: pid_file
        integer :: exitcode, pid, clock
        integer(c_int) :: rc
        logical :: alive

        call system_clock(clock)
        write (pid_file, '(a,a,a,i0)') trim(project_dir), '/child-', trim(mode), clock
        call fs_remove_file(trim(pid_file))
        call fs_remove_file(trim(log_file))
        call set_env('FO_TEST_CHILD_MODE', trim(mode))
        call set_env('FO_TEST_CHILD_PID_FILE', trim(pid_file))
        if (named) then
            call gfortran_test_names(project_dir, selected, 1, log_file, exitcode)
        else
            call gfortran_test(project_dir, log_file, exitcode)
        end if
        if (expect_timeout) then
            call assert(exitcode /= 0, trim(mode)//' child hits the bounded wall cap')
            if (host_can_measure_child_cpu(project_dir)) then
                call assert(file_contains(log_file, &
                    'test process itself used only '), &
                    trim(mode)//' timeout identifies CPU as test-process-only')
            else
                call assert(file_contains(log_file, &
                    'budget of 1 s exceeded in wall time') .and. &
                    file_contains(log_file, &
                    'child CPU time is not measurable on this platform'), &
                    trim(mode)//' timeout identifies the fallback wall budget')
            end if
            call assert(.not. file_contains(log_file, 'deadlocked'), &
                trim(mode)//' timeout does not infer deadlock from parent CPU')
        else
            call assert(exitcode == 0, 'completed test returns success before wall cap')
            call assert(file_contains(log_file, 'TEST_RESULT test_process_tree PASS'), &
                'completed test records PASS before child cleanup')
        end if
        pid = read_process_id(pid_file)
        call assert(pid > 0, trim(mode)//' fixture records its child PID')
        if (pid > 0) then
            alive = process_is_alive(pid)
            call assert(.not. alive, trim(mode)//' child is cleaned after its parent')
            if (alive) then
                write (error_unit, '(a,a,a,l1,a,i0)') &
                    'Native process oracle reports running child: mode=', trim(mode), &
                    ' named=', named, ' pid=', pid
                rc = c_kill(int(pid, c_int), 9_c_int)
            end if
        end if
        alive = process_is_alive(sentinel_pid)
        call assert(alive, 'process-group cleanup preserves unrelated sentinel')
    end subroutine run_process_tree_case

    integer function read_process_id(path) result(pid)
        character(len=*), intent(in) :: path
        integer :: unit, io_status

        pid = -1
        open (newunit=unit, file=trim(path), status='old', action='read', &
            iostat=io_status)
        if (io_status /= 0) return
        read (unit, *, iostat=io_status) pid
        close (unit)
        if (io_status /= 0) pid = -1
    end function read_process_id

    logical function process_is_alive(pid) result(alive)
        interface
            integer(c_int) function c_process_running(process) &
                    bind(C, name='fo_test_process_running')
                import :: c_int
                integer(c_int), value :: process
            end function c_process_running
        end interface
        integer, intent(in) :: pid

        alive = c_process_running(int(pid, c_int)) == 1_c_int
    end function process_is_alive

    logical function host_can_measure_child_cpu(project_dir) result(supported)
        !! Select the expected clock independently of the process runner.
        character(len=*), intent(in) :: project_dir
        character(len=512) :: host_file, line
        integer :: unit, status

        host_file = trim(project_dir)//'/host.txt'
        call run_backend_argv('uname', [character(len=2) :: '-s'], &
            host_file, status)
        if (status /= 0) error stop 'cannot determine the host for the budget oracle'
        open (newunit=unit, file=trim(host_file), status='old', action='read')
        read (unit, '(a)') line
        close (unit)
        ! Darwin reports per-process CPU through proc_pid_rusage.
        supported = trim(line) == 'Darwin'
        if (supported .or. trim(line) /= 'Linux') return
        open (newunit=unit, file='/proc/self/stat', status='old', &
            action='read', iostat=status)
        if (status /= 0) return
        read (unit, '(a)', iostat=status) line
        close (unit)
        supported = status == 0
    end function host_can_measure_child_cpu

    subroutine write_idle_test(path, name, seconds)
        !! A test that spends its time asleep, using almost no CPU.
        character(len=*), intent(in) :: path, name
        integer, intent(in) :: seconds
        integer :: u

        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') 'program '//trim(name)
        write (u, '(a)') 'use, intrinsic :: iso_c_binding, only: c_int'
        write (u, '(a)') 'interface'
        write (u, '(a)') 'integer(c_int) function c_sleep(s) bind(C, name="sleep")'
        write (u, '(a)') 'import :: c_int'
        write (u, '(a)') 'integer(c_int), value :: s'
        write (u, '(a)') 'end function c_sleep'
        write (u, '(a)') 'end interface'
        write (u, '(a,i0,a)') 'if (c_sleep(', seconds, '_c_int) /= 0) error stop 1'
        write (u, '(a)') 'print *, "rested"'
        write (u, '(a)') 'end program '//trim(name)
        close (u)
    end subroutine write_idle_test

    subroutine write_sleeping_test(path, name)
        !! A test that burns 1.5 s of CPU time (not wall time), so it exceeds a
        !! one-second CPU budget however loaded the host is.
        character(len=*), intent(in) :: path, name
        integer :: u

        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') 'program '//trim(name)
        write (u, '(a)') 'real :: t'
        write (u, '(a)') 'do'
        write (u, '(a)') '    call cpu_time(t)'
        write (u, '(a)') '    if (t > 1.5) exit'
        write (u, '(a)') 'end do'
        write (u, '(a)') 'print *, "done"'
        write (u, '(a)') 'end program '//trim(name)
        close (u)
    end subroutine write_sleeping_test

    subroutine test_gfortran_builds_manifest_example()
        character(len=512) :: project_dir, log_file, binary
        integer :: u, exitcode
        logical :: exists

        call make_tmp_path('fo_manifest_example', project_dir)
        call make_tmp_path('fo_manifest_example_log', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/example')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "manifest-example"'
        write (u, '(a)') '[[example]]'
        write (u, '(a)') 'name = "public_demo"'
        write (u, '(a)') 'source-dir = "example"'
        write (u, '(a)') 'main = "internal_demo.f90"'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/example/internal_demo.f90', &
            status='replace')
        write (u, '(a)') 'program internal_demo'
        write (u, '(a)') 'print "(a)", "EXAMPLE_OK"'
        write (u, '(a)') 'end program internal_demo'
        close (u)

        call gfortran_build(project_dir, log_file, exitcode, use_cache=.false.)
        binary = trim(project_dir)//'/build/fo/bin/public_demo'
        inquire (file=trim(binary), exist=exists)
        call assert(exitcode == 0 .and. exists, &
            'manifest example builds under its public target name')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_gfortran_builds_manifest_example

    subroutine test_gfortran_builds_nested_auto_example()
        character(len=512) :: project_dir, log_file, binary, run_output
        integer :: u, exitcode, run_status
        logical :: exists

        call make_tmp_path('fo_nested_auto_example', project_dir)
        call make_tmp_path('fo_nested_auto_example_log', log_file)
        call make_tmp_path('fo_nested_auto_example_output', run_output)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/example/demo')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "nested-auto-example"'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/example/demo/demo.f90', &
            status='replace')
        write (u, '(a)') 'program demo'
        write (u, '(a)') 'print "(a)", "NESTED_EXAMPLE_OLD"'
        write (u, '(a)') 'end program demo'
        close (u)

        call gfortran_build(project_dir, log_file, exitcode)
        binary = trim(project_dir)//'/build/fo/bin/demo'
        inquire (file=trim(binary), exist=exists)
        call assert(exitcode == 0 .and. exists, &
            'nested automatic example uses its source stem as target name')
        call run_backend_program(binary, run_output, run_status)
        call assert(run_status == 0 .and. &
            file_contains(run_output, 'NESTED_EXAMPLE_OLD'), &
            'nested automatic example runs its initial program body')

        open (newunit=u, file=trim(project_dir)//'/example/demo/demo.f90', &
            status='replace')
        write (u, '(a)') 'program demo'
        write (u, '(a)') 'print "(a)", "NESTED_EXAMPLE_NEW"'
        write (u, '(a)') 'end program demo'
        close (u)
        call gfortran_build(project_dir, log_file, exitcode)
        call run_backend_program(binary, run_output, run_status)
        call assert(exitcode == 0 .and. run_status == 0 .and. &
            file_contains(run_output, 'NESTED_EXAMPLE_NEW'), &
            'changed nested example body replaces the cached executable')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
        call fs_remove_file(run_output)
    end subroutine test_gfortran_builds_nested_auto_example

    subroutine test_gfortran_builds_c_source_with_public_header()
        character(len=512) :: project_dir, log_file, binary
        integer :: u, exitcode, run_status

        call make_tmp_path('fo_c_public_header', project_dir)
        call make_tmp_path('fo_c_public_header_log', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/include')
        call make_dir(trim(project_dir)//'/app')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "c-public-header"'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/include/answer.h', &
            status='replace')
        write (u, '(a)') '#define ANSWER 42'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/src/answer.c', &
            status='replace')
        write (u, '(a)') '#include "answer.h"'
        write (u, '(a)') 'int answer(void) { return ANSWER; }'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/app/main.f90', &
            status='replace')
        write (u, '(a)') 'program main'
        write (u, '(a)') 'use, intrinsic :: iso_c_binding, only: c_int'
        write (u, '(a)') 'interface'
        write (u, '(a)') 'function answer() bind(C) result(value)'
        write (u, '(a)') 'import c_int'
        write (u, '(a)') 'integer(c_int) :: value'
        write (u, '(a)') 'end function answer'
        write (u, '(a)') 'end interface'
        write (u, '(a)') 'if (answer() /= 42_c_int) error stop 1'
        write (u, '(a)') 'end program main'
        close (u)

        call gfortran_build(project_dir, log_file, exitcode, use_cache=.false.)
        binary = trim(project_dir)//'/build/fo/bin/c-public-header'
        run_status = 1
        if (exitcode == 0) then
            call run_backend_program(binary, log_file, run_status)
        end if
        call assert(exitcode == 0 .and. run_status == 0, &
            'native build passes project include directory to C compiler')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_gfortran_builds_c_source_with_public_header

    subroutine test_gfortran_named_test_uses_manifest_name()
        character(len=512) :: project_dir, log_file
        character(len=128) :: selected(1)
        integer :: u, exitcode

        call make_tmp_path('fo_manifest_test_name', project_dir)
        call make_tmp_path('fo_manifest_test_name_log', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/test/nested')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "manifest-test-name"'
        write (u, '(a)') '[[test]]'
        write (u, '(a)') 'name = "public_name"'
        write (u, '(a)') 'main = "nested/test_internal_name.f90"'
        close (u)
        open (newunit=u, file=trim(project_dir)// &
            '/test/nested/test_internal_name.f90', status='replace')
        write (u, '(a)') 'program test_internal_name'
        write (u, '(a)') 'print "(a)", "PASS"'
        write (u, '(a)') 'end program test_internal_name'
        close (u)

        selected(1) = 'public_name'
        call gfortran_test_names(project_dir, selected, 1, log_file, exitcode, &
            use_cache=.false.)
        call assert(exitcode == 0 .and. &
            file_contains(log_file, 'TEST_RESULT public_name PASS'), &
            'named test uses the public name from its manifest entry')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_gfortran_named_test_uses_manifest_name

    subroutine test_gfortran_app_links_only_reachable_library_objects()
        character(len=512) :: project_dir, log_file, binary
        integer :: u, exitcode
        logical :: exists

        call make_tmp_path('fo_app_reachable_objects', project_dir)
        call make_tmp_path('fo_app_reachable_objects_log', log_file)
        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/app')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "reachable_app"'
        write (u, '(a)') '[fortran]'
        write (u, '(a)') 'implicit-external = true'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/src/used.f90', status='replace')
        write (u, '(a)') 'module used'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine hello()'
        write (u, '(a)') 'print "(a)", "OK"'
        write (u, '(a)') 'end subroutine hello'
        write (u, '(a)') 'end module used'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/src/unused.f90', status='replace')
        write (u, '(a)') 'module unused'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine unavailable_path()'
        write (u, '(a)') 'call deliberately_missing_external()'
        write (u, '(a)') 'end subroutine unavailable_path'
        write (u, '(a)') 'end module unused'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/app/main.f90', status='replace')
        write (u, '(a)') 'program main'
        write (u, '(a)') 'use used, only: hello'
        write (u, '(a)') 'call hello()'
        write (u, '(a)') 'end program main'
        close (u)

        call gfortran_build(project_dir, log_file, exitcode, use_cache=.false.)
        binary = trim(project_dir)//'/build/fo/bin/reachable_app'
        inquire (file=trim(binary), exist=exists)
        call assert(exitcode == 0 .and. exists, &
            'application excludes unreachable library objects from its link')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_gfortran_app_links_only_reachable_library_objects

    subroutine test_gfortran_test_skips_app_but_build_restores_it()
        character(len=512) :: project_dir, log_file, app_path
        integer :: u, exitcode, run_exit
        logical :: app_exists

        call make_tmp_path('fo_test_without_app', project_dir)
        call make_tmp_path('fo_test_without_app_log', log_file)
        call make_simple_fpm_project(project_dir)
        call make_dir(trim(project_dir)//'/app')
        open (newunit=u, file=trim(project_dir)//'/app/main.f90', status='replace')
        write (u, '(a)') 'program main'
        write (u, '(a)') 'print "(a)", "APP_OK"'
        write (u, '(a)') 'end program main'
        close (u)
        app_path = trim(project_dir)//'/build/fo/bin/fo_test_spaces'

        call gfortran_test(project_dir, log_file, exitcode)
        inquire (file=trim(app_path), exist=app_exists)
        call assert(exitcode == 0 .and. .not. app_exists, &
            'test-only build does not link an unrelated application')

        call gfortran_build(project_dir, log_file, exitcode)
        inquire (file=trim(app_path), exist=app_exists)
        run_exit = 1
        if (app_exists) call run_backend_program(app_path, log_file, run_exit)
        call assert(exitcode == 0 .and. app_exists .and. run_exit == 0, &
            'normal build after tests creates a runnable application')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_gfortran_test_skips_app_but_build_restores_it

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

    subroutine test_pipe_flag_policy()
        character(len=128) :: flags

        flags = '-fimplicit-none'
        call append_pipe_flag('GNU Fortran 16.1', flags)
        call assert(trim(flags) == '-fimplicit-none -pipe', &
            'GNU Fortran uses pipes between compilation stages')
        call append_pipe_flag('gfortran', flags)
        call assert(trim(flags) == '-fimplicit-none -pipe', &
            'pipe flag is not duplicated')
        flags = '-fimplicit-none'
        call append_pipe_flag('flang-new', flags)
        call assert(trim(flags) == '-fimplicit-none', &
            'non-GNU compiler does not receive the GCC-specific pipe flag')
    end subroutine test_pipe_flag_policy

    subroutine test_linker_policy()
        call assert(linker_should_try_lld('gfortran', .true., 'auto'), &
            'auto linker policy prefers available LLD for gfortran')
        call assert(linker_should_try_lld('flang-new', .true., 'lld'), &
            'explicit LLD policy supports the flang driver')
        call assert(.not. linker_should_try_lld('lfortran', .true., 'auto'), &
            'compiler without -fuse-ld support keeps its default linker')
        call assert(.not. linker_should_try_lld('gfortran', .false., 'auto'), &
            'missing LLD keeps the default linker')
        call assert(.not. linker_should_try_lld('gfortran', .true., 'default'), &
            'explicit default policy disables LLD')
    end subroutine test_linker_policy

    subroutine test_lld_failure_falls_back_to_default_linker()
        character(len=:), allocatable :: old_path
        character(len=512) :: project_dir, log_file, fake_dir, marker
        character(len=512) :: real_compiler, old_fc, old_linker
        integer :: exitcode, path_length, fc_status, linker_status
        logical :: attempted, compiler_found

        call make_tmp_path('fo_lld_fallback_project', project_dir)
        call make_tmp_path('fo_lld_fallback_log', log_file)
        call make_tmp_path('fo_lld_fallback_bin', fake_dir)
        call make_simple_fpm_project(project_dir)
        call make_dir(fake_dir)
        call fs_find_executable('gfortran', real_compiler, compiler_found)
        call assert(compiler_found, 'fallback fixture locates the real compiler')
        if (.not. compiler_found) then
            call remove_tree(project_dir)
            call remove_tree(fake_dir)
            call fs_remove_file(log_file)
            return
        end if
        marker = trim(fake_dir)//'/attempted'
        call compile_backend_command_fixture(trim(fake_dir)//'/ld.lld', &
            real_compiler)
        call compile_backend_command_fixture(trim(fake_dir)//'/gfortran', &
            real_compiler)

        call get_environment_variable('PATH', length=path_length)
        allocate (character(len=max(1, path_length)) :: old_path)
        call get_environment_variable('PATH', old_path)
        old_fc = ''
        call get_environment_variable('FO_FC', old_fc, status=fc_status)
        old_linker = ''
        call get_environment_variable('FO_LINKER', old_linker, &
            status=linker_status)
        call set_env('PATH', trim(fake_dir)//':'//trim(old_path))
        call set_env('FO_LINKER', 'lld')
        call set_env('FO_FC', trim(fake_dir)//'/gfortran')
        call set_env('FO_TEST_BACKEND_REAL_COMPILER', trim(real_compiler))
        call set_env('FO_TEST_BACKEND_LLD_MARKER', trim(marker))
        call set_env('FO_TEST_BACKEND_WRAPPER_DEPTH', '')
        call gfortran_test(project_dir, log_file, exitcode, use_cache=.false.)
        call set_env('PATH', trim(old_path))
        if (linker_status == 0 .and. len_trim(old_linker) > 0) then
            call set_env('FO_LINKER', trim(old_linker))
        else
            call set_env('FO_LINKER', '')
        end if
        if (fc_status == 0 .and. len_trim(old_fc) > 0) then
            call set_env('FO_FC', trim(old_fc))
        else
            call set_env('FO_FC', '')
        end if
        call set_env('FO_TEST_BACKEND_REAL_COMPILER', '')
        call set_env('FO_TEST_BACKEND_LLD_MARKER', '')

        inquire (file=trim(marker), exist=attempted)
        call assert(attempted, 'link invokes the selected LLD executable')
        call assert(exitcode == 0, &
            'failed LLD link retries successfully with the default linker')

        call remove_tree(project_dir)
        call remove_tree(fake_dir)
        call fs_remove_file(log_file)
    end subroutine test_lld_failure_falls_back_to_default_linker

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
        call fs_remove_file(log_file)
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
        call fs_remove_file(log_file)
    end subroutine test_gfortran_named_tests_fit_default_stack

    subroutine make_linked_named_project(project_dir, dependency_dir)
        character(len=*), intent(in) :: project_dir, dependency_dir
        character(len=512) :: compiler, archiver
        character(len=512) :: compile_args(6), archive_args(3)
        integer :: u, run_status
        logical :: found

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
        ! -J keeps the module file out of the working directory (the fo
        ! checkout), where it would shadow build/fo/mod on the next build.
        call fs_find_executable('gfortran', compiler, found)
        if (.not. found) error stop 'linked fixture needs gfortran'
        compile_args(1) = '-c'
        compile_args(2) = trim(dependency_dir)//'/src/marker.f90'
        compile_args(3) = '-J'
        compile_args(4) = trim(dependency_dir)//'/build'
        compile_args(5) = '-o'
        compile_args(6) = trim(dependency_dir)//'/build/marker.o'
        call run_backend_argv(trim(compiler), compile_args, '/dev/null', &
            run_status)
        if (run_status /= 0) error stop 'cannot compile linked fixture module'
        call fs_find_executable('ar', archiver, found)
        if (.not. found) error stop 'linked fixture needs ar'
        archive_args(1) = 'rcs'
        archive_args(2) = trim(dependency_dir)//'/build/libstack_dependency.a'
        archive_args(3) = trim(dependency_dir)//'/build/marker.o'
        call run_backend_argv(trim(archiver), archive_args, '/dev/null', &
            run_status)
        if (run_status /= 0) error stop 'cannot archive linked fixture module'
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
        call cache_store_action(cache, action_id, object, mod_dir, &
            [character(len=1) ::], &
            output_id, ierr)

        call gfortran_build(project_dir, log_file, exitcode, n_compiled, &
            compiler_id='fixture-compiler')
        inquire (file=trim(project_dir)//'/build/fo/mod/provider.mod', &
            exist=mod_exists)
        call assert(exitcode == 0 .and. n_compiled == 1 .and. mod_exists, &
            'module cache hit without .mod recompiles provider')

        call remove_tree(project_dir)
        call fs_remove_file(log_file)
    end subroutine test_gfortran_rebuilds_cached_module_without_mod

    include 'test_backend_helpers.inc'

end program test_backend_gfortran
