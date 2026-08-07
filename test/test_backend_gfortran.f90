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
    use fo_compiler_flags, only: append_array_temporary_warning_flag, append_pipe_flag
    use fo_linker_policy, only: linker_should_try_lld
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
    call test_gfortran_test_skips_app_but_build_restores_it()
    call test_gfortran_test_links_helper_modules_and_lib()
    call test_gfortran_named_test_links_helper_modules()
    call test_gfortran_named_test_uses_manifest_name()
    call test_gfortran_app_links_only_reachable_library_objects()
    call test_compiler_switch_clears_the_tree()
    call test_slow_test_gets_its_own_timeout()
    call test_gfortran_builds_manifest_example()
    call test_gfortran_builds_nested_auto_example()
    call test_gfortran_builds_c_source_with_public_header()
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
    call test_pipe_flag_policy()
    call test_linker_policy()
    call test_lld_failure_falls_back_to_default_linker()
    call test_gfortran_warns_about_array_temporaries()
    call test_gfortran_named_tests_fit_default_stack()
    call test_gfortran_passes_manifest_test_arguments()

    call report('backend_gfortran')

contains

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
        call execute_command_line('rm -f '//trim(log_file))
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
        call execute_command_line('rm -f '//trim(log_file))
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
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_slow_test_gets_its_own_timeout

    subroutine write_sleeping_test(path, name)
        character(len=*), intent(in) :: path, name
        integer :: u

        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') 'program '//trim(name)
        write (u, '(a)') 'integer(8) :: start, now, rate'
        write (u, '(a)') 'call system_clock(start, rate)'
        write (u, '(a)') 'do'
        write (u, '(a)') '    call system_clock(now)'
        ! Stay clearly beyond the one-second fast-test budget without making
        ! this regression consume most of its caller's ten-second budget.
        write (u, '(a)') '    if (real(now - start)/real(rate) > 1.5) exit'
        write (u, '(a)') 'end do'
        write (u, '(a)') 'print *, "done"'
        write (u, '(a)') 'end program '//trim(name)
        close (u)
    end subroutine write_sleeping_test

    subroutine set_env(name, value)
        !! Set or clear an environment variable for this process.
        use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
        interface
            function c_setenv(name, value, overwrite) bind(C, name='setenv') &
                    result(ierr)
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: name(*), value(*)
                integer(c_int), value :: overwrite
                integer(c_int) :: ierr
            end function c_setenv
            function c_unsetenv(name) bind(C, name='unsetenv') result(ierr)
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: name(*)
                integer(c_int) :: ierr
            end function c_unsetenv
        end interface
        character(len=*), intent(in) :: name, value
        integer(c_int) :: ierr

        if (len_trim(value) == 0) then
            ierr = c_unsetenv(trim(name)//c_null_char)
        else
            ierr = c_setenv(trim(name)//c_null_char, trim(value)//c_null_char, 1_c_int)
        end if
    end subroutine set_env

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
        call execute_command_line('rm -f '//trim(log_file))
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
        call execute_command_line( &
            trim(binary)//' > '//trim(run_output), exitstat=run_status)
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
        call execute_command_line( &
            trim(binary)//' > '//trim(run_output), exitstat=run_status)
        call assert(exitcode == 0 .and. run_status == 0 .and. &
            file_contains(run_output, 'NESTED_EXAMPLE_NEW'), &
            'changed nested example body replaces the cached executable')

        call remove_tree(project_dir)
        call execute_command_line( &
            'rm -f '//trim(log_file)//' '//trim(run_output))
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
            call execute_command_line(trim(binary), exitstat=run_status)
        end if
        call assert(exitcode == 0 .and. run_status == 0, &
            'native build passes project include directory to C compiler')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
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
        call execute_command_line('rm -f '//trim(log_file))
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
        call execute_command_line('rm -f '//trim(log_file))
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
        if (app_exists) call execute_command_line('"'//trim(app_path)//'"', &
            exitstat=run_exit)
        call assert(exitcode == 0 .and. app_exists .and. run_exit == 0, &
            'normal build after tests creates a runnable application')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
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
        use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
        use fo_fs, only: fs_find_executable
        interface
            function setenv(name, value, overwrite) bind(C, name='setenv') &
                    result(ierr)
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

        character(len=:), allocatable :: old_path
        character(len=512) :: project_dir, log_file, fake_dir, marker
        character(len=512) :: real_compiler, old_fc
        integer :: u, exitcode, path_length, fc_status
        integer(c_int) :: ierr
        logical :: attempted, compiler_found

        call make_tmp_path('fo_lld_fallback_project', project_dir)
        call make_tmp_path('fo_lld_fallback_log', log_file)
        call make_tmp_path('fo_lld_fallback_bin', fake_dir)
        call make_simple_fpm_project(project_dir)
        call make_dir(fake_dir)
        call fs_find_executable('gfortran', real_compiler, compiler_found)
        call assert(compiler_found, 'fallback fixture locates the real compiler')
        marker = trim(fake_dir)//'/attempted'
        open (newunit=u, file=trim(fake_dir)//'/ld.lld', status='replace')
        write (u, '(a)') '#!/bin/sh'
        write (u, '(a)') 'exit 1'
        close (u)
        call execute_command_line('chmod +x "'//trim(fake_dir)//'/ld.lld"')
        open (newunit=u, file=trim(fake_dir)//'/gfortran', status='replace')
        write (u, '(a)') '#!/bin/sh'
        write (u, '(a)') 'for arg in "$@"; do'
        write (u, '(a)') '  if [ "$arg" = "-fuse-ld=lld" ]; then'
        write (u, '(a)') '    touch "'//trim(marker)//'"'
        write (u, '(a)') '    exit 1'
        write (u, '(a)') '  fi'
        write (u, '(a)') 'done'
        write (u, '(a)') 'exec "'//trim(real_compiler)//'" "$@"'
        close (u)
        call execute_command_line('chmod +x "'//trim(fake_dir)//'/gfortran"')

        call get_environment_variable('PATH', length=path_length)
        allocate (character(len=max(1, path_length)) :: old_path)
        call get_environment_variable('PATH', old_path)
        old_fc = ''
        call get_environment_variable('FO_FC', old_fc, status=fc_status)
        ierr = setenv('PATH'//c_null_char, &
            trim(fake_dir)//':'//trim(old_path)//c_null_char, 1_c_int)
        ierr = setenv('FO_LINKER'//c_null_char, 'lld'//c_null_char, 1_c_int)
        ierr = setenv('FO_FC'//c_null_char, &
            trim(fake_dir)//'/gfortran'//c_null_char, 1_c_int)
        call gfortran_test(project_dir, log_file, exitcode, use_cache=.false.)
        ierr = setenv('PATH'//c_null_char, trim(old_path)//c_null_char, 1_c_int)
        ierr = unsetenv('FO_LINKER'//c_null_char)
        if (fc_status == 0 .and. len_trim(old_fc) > 0) then
            ierr = setenv('FO_FC'//c_null_char, trim(old_fc)//c_null_char, 1_c_int)
        else
            ierr = unsetenv('FO_FC'//c_null_char)
        end if

        inquire (file=trim(marker), exist=attempted)
        call assert(attempted, 'link invokes the selected LLD executable')
        call assert(exitcode == 0, &
            'failed LLD link retries successfully with the default linker')

        call remove_tree(project_dir)
        call remove_tree(fake_dir)
        call execute_command_line('rm -f '//trim(log_file))
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
        command = 'gfortran -c "'//trim(dependency_dir)// &
            '/src/marker.f90" -o "'//trim(dependency_dir)// &
            '/build/marker.o"'
        call execute_command_line(trim(command))
        command = 'ar rcs "'//trim(dependency_dir)// &
            '/build/libstack_dependency.a" "'//trim(dependency_dir)// &
            '/build/marker.o"'
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
