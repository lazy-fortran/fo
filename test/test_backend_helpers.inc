    ! Shared backend-test helpers, textually included by the split
    ! test_backend* programs (each supplies use-statements + n_pass/n_fail).
    subroutine report(name)
        character(len=*), intent(in) :: name
        write (output_unit, '(a,a,i0,a,i0,a)') trim(name), ': ', &
            n_pass, ' pass, ', n_fail, ' fail'
        if (n_fail > 0) stop 1
    end subroutine report

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_detect_fpm()
        type(backend_t) :: b
        integer :: u
        character(len=512) :: project_dir

        call make_tmp_path('fo_test_fpm', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir))
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "test"'
        close (u)

        b = detect_backend(project_dir)
        call assert(b%kind == BACKEND_GFORTRAN, 'detect fpm.toml -> gfortran backend')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_detect_fpm

    subroutine test_detect_fpm_from_child()
        type(backend_t) :: b
        integer :: u
        character(len=512) :: project_dir

        call make_tmp_path('fo_test_fpm_parent', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir)//'/src/nested')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "test"'
        close (u)

        b = detect_backend(trim(project_dir)//'/src/nested')
        call assert(b%kind == BACKEND_GFORTRAN, 'detect fpm.toml from child -> gfortran backend')
        call assert(trim(b%project_dir) == trim(project_dir), &
            'detected project root')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_detect_fpm_from_child

    subroutine test_detect_cmake()
        type(backend_t) :: b
        integer :: u
        character(len=512) :: project_dir

        call make_tmp_path('fo_test_cmake', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir))
        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', &
            status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        close (u)

        b = detect_backend(project_dir)
        call assert(b%kind == BACKEND_CMAKE, 'detect cmake')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_detect_cmake

    subroutine test_detect_cmake_over_fpm()
        type(backend_t) :: b
        integer :: u
        character(len=512) :: project_dir

        call make_tmp_path('fo_test_cmake_over_fpm', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir))
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "test"'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', &
            status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        close (u)

        b = detect_backend(project_dir)
        call assert(b%kind == BACKEND_CMAKE, 'detect cmake over fpm')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_detect_cmake_over_fpm

    subroutine test_detect_none()
        type(backend_t) :: b
        character(len=512) :: project_dir

        call make_tmp_path('fo_test_none', project_dir)
        call execute_command_line('mkdir -p '//trim(project_dir))

        b = detect_backend(project_dir)
        call assert(b%kind == BACKEND_NONE, 'detect none')

        call execute_command_line('rm -rf '//trim(project_dir))
    end subroutine test_detect_none

    subroutine test_nproc()
        integer :: np

        np = detect_nproc()
        call assert(np >= 1, 'nproc >= 1')
    end subroutine test_nproc

    subroutine test_detect_jobs()
        integer :: jobs

        jobs = detect_jobs()
        call assert(jobs >= 1, 'jobs >= 1')
    end subroutine test_detect_jobs

    subroutine test_fpm_skips_slow_by_default()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_gfortran_run', project_dir)
        call make_tmp_path('fo_backend_gfortran', log_file)
        call make_slow_fpm_project(project_dir)

        b = detect_backend(project_dir)
        call backend_build(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'gfortran build succeeds on simple project')
        call backend_test(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'gfortran test skips slow tests')
        call backend_test(b, exitcode, include_slow=.true., log_file=log_file)
        call assert(exitcode /= 0, 'gfortran test --all includes slow tests')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_fpm_skips_slow_by_default

    subroutine test_gfortran_named_tests_select_requested()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: project_dir, log_file
        character(len=128) :: names(1)

        call make_tmp_path('fo_test_gfortran_named', project_dir)
        call make_tmp_path('fo_backend_gfortran_named', log_file)
        call make_named_fpm_project(project_dir)

        b = detect_backend(project_dir)
        names(1) = 'test_a'
        call backend_test_names(b, names, 1, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'gfortran named tests run selected test only')
        call assert(file_contains(log_file, 'selected test ran'), &
            'gfortran named log includes selected test output')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_named_tests_select_requested

    subroutine test_gfortran_named_test_restores_cached_object()
        integer :: exitcode, n_first, n_second
        character(len=512) :: project_dir, log_file
        character(len=128) :: names(1)

        call make_tmp_path('fo_test_gfortran_named_cache', project_dir)
        call make_tmp_path('fo_backend_gfortran_named_cache', log_file)
        call make_named_fpm_project(project_dir)

        names(1) = 'test_a'
        call gfortran_test_names(project_dir, names, 1, log_file, exitcode, &
            n_compiled=n_first)
        call gfortran_test_names(project_dir, names, 1, log_file, exitcode, &
            n_compiled=n_second)

        call assert(exitcode == 0, 'gfortran named cached test succeeds')
        call assert(n_first > 0, 'gfortran named test first run compiles')
        call assert(n_second == 0, &
            'gfortran named test second run restores test object')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_named_test_restores_cached_object

    subroutine test_gfortran_recovers_from_root_mod_shadow()
        type(backend_t) :: b
        integer :: exitcode, u
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_gfortran_stale_mod', project_dir)
        call make_tmp_path('fo_backend_gfortran_stale_mod', log_file)
        call make_simple_fpm_project(project_dir)

        open (newunit=u, file=trim(project_dir)//'/src/lib.f90', &
            status='replace')
        write (u, '(a)') '! fixture '//trim(project_dir)
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'character(len=*), parameter :: stale_mod_fixture = "'// &
            trim(project_dir)//'"'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine noop()'
        write (u, '(a)') 'end subroutine noop'
        write (u, '(a)') 'end module lib'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/src/consumer.f90', &
            status='replace')
        write (u, '(a)') '! fixture '//trim(project_dir)
        write (u, '(a)') 'module consumer'
        write (u, '(a)') 'use lib, only: noop'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine call_noop()'
        write (u, '(a)') 'call noop()'
        write (u, '(a)') 'end subroutine call_noop'
        write (u, '(a)') 'end module consumer'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/lib.mod', status='replace')
        write (u, '(a)') 'not a gfortran module'
        close (u)

        b = detect_backend(project_dir)
        call backend_build(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'gfortran build recovers from root .mod shadow')
        call assert(file_contains(log_file, 'fortran.linter.modOutput'), &
            'stale root module hint reaches build log')
        call assert(.not. file_exists(trim(project_dir)//'/lib.mod'), &
            'stale root module is removed after matched failure')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_recovers_from_root_mod_shadow

    subroutine test_gfortran_restores_deleted_outputs()
        integer :: exitcode, n_first, n_restore
        character(len=512) :: project_dir, log_file
        logical :: obj_exists, mod_exists

        call make_tmp_path('fo_test_gfortran_restore', project_dir)
        call make_tmp_path('fo_backend_gfortran_restore', log_file)
        call make_simple_fpm_project(project_dir)
        call rewrite_lib_unique(project_dir)

        call gfortran_build(project_dir, log_file, exitcode, n_first)
        call assert(exitcode == 0 .and. n_first > 0, &
            'gfortran restore fixture first build compiles')
        call execute_command_line('rm -rf '//trim(project_dir)// &
            '/build/fo/obj '//trim(project_dir)// &
            '/build/fo/mod', wait=.true.)
        call gfortran_build(project_dir, log_file, exitcode, n_restore)
        call assert(exitcode == 0 .and. n_restore == 0, &
            'deleted outputs restore without recompiling')
        inquire (file=trim(project_dir)//'/build/fo/obj/src_lib.f90.o', &
            exist=obj_exists)
        inquire (file=trim(project_dir)//'/build/fo/mod/lib.mod', exist=mod_exists)
        call assert(obj_exists .and. mod_exists, &
            'deleted object and module outputs are restored')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_restores_deleted_outputs

    subroutine test_gfortran_app_main_keeps_package_name()
        !! Regression: with several app programs, app/main.f90 must take the
        !! package name and every other app program its own stem -- regardless
        !! of source order. A program sorting before main.f90 used to steal the
        !! package binary (e.g. span_mismatch_sub linked as `tx`).
        integer :: exitcode, n_first, u
        character(len=512) :: project_dir, log_file, out_file, line

        call make_tmp_path('fo_test_appname', project_dir)
        call make_tmp_path('fo_backend_appname', log_file)
        call make_tmp_path('fo_appname_out', out_file)

        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/app')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "appnamepkg"'
        close (u)
        ! Sorts before main.f90; this is the program that used to steal the name.
        open (newunit=u, file=trim(project_dir)//'/app/aaa_extra.f90', &
            status='replace')
        write (u, '(a)') '! fixture '//trim(project_dir)
        write (u, '(a)') 'program aaa_extra'
        write (u, '(a)') "print '(a)', 'FO_APPNAME_EXTRA'"
        write (u, '(a)') 'end program aaa_extra'
        close (u)
        open (newunit=u, file=trim(project_dir)//'/app/main.f90', status='replace')
        write (u, '(a)') '! fixture '//trim(project_dir)
        write (u, '(a)') 'program main'
        write (u, '(a)') "print '(a)', 'FO_APPNAME_MAIN'"
        write (u, '(a)') 'end program main'
        close (u)

        call gfortran_build(project_dir, log_file, exitcode, n_first)
        call assert(exitcode == 0 .and. n_first > 0, &
            'appname fixture builds')

        ! The package binary must be main, not the alphabetically-first program.
        call run_capture(trim(project_dir)//'/build/fo/bin/appnamepkg', &
            trim(out_file), line)
        call assert(trim(line) == 'FO_APPNAME_MAIN', &
            'package binary is app/main.f90, not the first app source')
        ! The other program is named by its own stem, cleanly (no app_ prefix).
        call run_capture(trim(project_dir)//'/build/fo/bin/aaa_extra', &
            trim(out_file), line)
        call assert(trim(line) == 'FO_APPNAME_EXTRA', &
            'non-main app program is named by its source stem')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file)//' '//trim(out_file))
    end subroutine test_gfortran_app_main_keeps_package_name

    subroutine run_capture(exe, out_file, first_line)
        !! Run exe, capturing its first stdout line. Empty if exe is missing.
        character(len=*), intent(in) :: exe, out_file
        character(len=*), intent(out) :: first_line
        integer :: u, ios
        logical :: exists

        first_line = ''
        inquire (file=trim(exe), exist=exists)
        if (.not. exists) return
        call execute_command_line('"'//trim(exe)//'" > "'//trim(out_file)// &
            '" 2>/dev/null', wait=.true.)
        open (newunit=u, file=trim(out_file), status='old', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) first_line
        close (u)
    end subroutine run_capture

    subroutine test_config_flags_str_joins_with_spaces()
        !! Regression: multiple [build] flags must be space-separated.
        !! The old code added a space then trimmed it back off, producing
        !! e.g. "-O2-fopenmp" which gfortran rejects.
        type(fpm_config_t) :: config

        config%n_flags = 2
        config%flags(1) = '-O2'
        config%flags(2) = '-fopenmp'
        call assert(trim(config_flags_str(config)) == '-O2 -fopenmp', &
            'config_flags_str joins two flags with a space')

        config%n_flags = 3
        config%flags(1) = '-O2'
        config%flags(2) = '-fopenmp'
        config%flags(3) = '-funroll-loops'
        call assert(trim(config_flags_str(config)) == '-O2 -fopenmp -funroll-loops', &
            'config_flags_str joins three flags with spaces')

        config%n_flags = 1
        config%flags(1) = '-fopenmp'
        call assert(trim(config_flags_str(config)) == '-fopenmp', &
            'config_flags_str passes a single flag through')
    end subroutine test_config_flags_str_joins_with_spaces

    subroutine test_gfortran_flags_change_action_id()
        integer :: exitcode, n_first, n_same, n_changed
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_gfortran_flags', project_dir)
        call make_tmp_path('fo_backend_gfortran_flags', log_file)
        call make_simple_fpm_project(project_dir)
        call rewrite_lib_unique(project_dir)

        call gfortran_build(project_dir, log_file, exitcode, n_first, '-O0')
        call assert(exitcode == 0 .and. n_first > 0, &
            'gfortran flags fixture first build compiles')
        call gfortran_build(project_dir, log_file, exitcode, n_same, '-O0')
        call assert(exitcode == 0 .and. n_same == 0, &
            'same flags restore from action cache')
        call gfortran_build(project_dir, log_file, exitcode, n_changed, '-O3')
        call assert(exitcode == 0 .and. n_changed > 0, &
            'changed flags miss action cache')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_flags_change_action_id

    subroutine test_gfortran_compiler_identity_changes_action_id()
        integer :: exitcode, n_first, n_same, n_changed
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_gfortran_compiler_id', project_dir)
        call make_tmp_path('fo_backend_gfortran_compiler_id', log_file)
        call make_simple_fpm_project(project_dir)
        call rewrite_lib_unique(project_dir)

        call gfortran_build(project_dir, log_file, exitcode, n_first, &
            compiler_id='compiler-a')
        call assert(exitcode == 0 .and. n_first > 0, &
            'compiler identity fixture first build compiles')
        call gfortran_build(project_dir, log_file, exitcode, n_same, &
            compiler_id='compiler-a')
        call assert(exitcode == 0 .and. n_same == 0, &
            'same compiler identity restores from action cache')
        call gfortran_build(project_dir, log_file, exitcode, n_changed, &
            compiler_id='compiler-b')
        call assert(exitcode == 0 .and. n_changed > 0, &
            'changed compiler identity misses action cache')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_compiler_identity_changes_action_id

    subroutine rewrite_lib_unique(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        open (newunit=u, file=trim(project_dir)//'/src/lib.f90', status='replace')
        write (u, '(a)') '! fixture '//trim(project_dir)
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'character(len=*), parameter :: fixture = "'// &
            trim(project_dir)//'"'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine noop()'
        write (u, '(a)') 'end subroutine noop'
        write (u, '(a)') 'end module lib'
        close (u)
    end subroutine rewrite_lib_unique

    subroutine test_gfortran_private_change_keeps_dependent_cached()
        integer :: exitcode, n_first, n_update
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_private_change', project_dir)
        call make_tmp_path('fo_backend_private_change', log_file)
        call make_two_module_project(project_dir)

        call gfortran_build(project_dir, log_file, exitcode, n_first)
        call assert(exitcode == 0 .and. n_first == 2, &
            'private change fixture first build compiles two modules')
        call write_lib_private_body(project_dir, 2)
        call gfortran_build(project_dir, log_file, exitcode, n_update)
        call assert(exitcode == 0 .and. n_update == 1, &
            'private implementation change does not rebuild dependent')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_private_change_keeps_dependent_cached

    subroutine test_gfortran_interface_change_rebuilds_dependent()
        integer :: exitcode, n_first, n_update
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_interface_change', project_dir)
        call make_tmp_path('fo_backend_interface_change', log_file)
        call make_two_module_project(project_dir)

        call gfortran_build(project_dir, log_file, exitcode, n_first)
        call assert(exitcode == 0 .and. n_first == 2, &
            'interface change fixture first build compiles two modules')
        call write_lib_interface(project_dir)
        call gfortran_build(project_dir, log_file, exitcode, n_update)
        call assert(exitcode == 0 .and. n_update == 2, &
            'interface change rebuilds dependent module')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_interface_change_rebuilds_dependent

    subroutine test_gfortran_parallel_test_loop_restores_cached_objects()
        integer :: exitcode, n_first, n_second
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_parallel_tests', project_dir)
        call make_tmp_path('fo_backend_parallel_tests', log_file)
        call make_many_test_fpm_project(project_dir, 8)

        call gfortran_test(project_dir, log_file, exitcode, n_compiled=n_first)
        call assert(exitcode == 0, 'parallel gfortran test suite succeeds')
        call assert(n_first == 8, 'parallel gfortran test first run compiles')

        call gfortran_test(project_dir, log_file, exitcode, n_compiled=n_second)
        call assert(exitcode == 0, 'parallel gfortran test suite rerun succeeds')
        call assert(n_second == 0, &
            'parallel gfortran test second run restores tests')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_parallel_test_loop_restores_cached_objects

    subroutine test_gfortran_test_links_helper_modules_and_lib()
        !! A test program that uses a module-only helper file under test/ and a
        !! library module whose name starts with test_ must build and link: the
        !! helper module is compiled and linked, and the test_-named library
        !! module is part of the library (not mistaken for a test). Mirrors fpm.
        integer :: exitcode
        character(len=512) :: project_dir, log_file

        call make_tmp_path('fo_test_helpers_link', project_dir)
        call make_tmp_path('fo_backend_helpers_link', log_file)
        call make_helper_module_project(project_dir)

        call gfortran_test(project_dir, log_file, exitcode)
        call assert(exitcode == 0, &
            'test links module-only test helper and test_ library module')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_test_links_helper_modules_and_lib

    subroutine test_gfortran_builds_path_dependency()
        !! A project with a path dependency compiles the dependency's library
        !! sources through fo's own cache and links them, with no prior fpm
        !! build of build/dependencies. The app prints a value defined in the
        !! dep, and a warm rebuild leaves the dep object untouched (cached).
        type(backend_t) :: b
        integer :: exitcode, u, ios
        character(len=512) :: base, libdir, appdir, log_file, outfile, depobj
        character(len=64) :: line
        integer :: mtime_a, mtime_b, bin_a, bin_b

        call make_tmp_path('fo_test_pathdep', base)
        libdir = trim(base)//'/lib'
        appdir = trim(base)//'/app'
        call make_tmp_path('fo_backend_pathdep', log_file)

        call execute_command_line('mkdir -p '//trim(libdir)//'/src '// &
            trim(appdir)//'/app')
        open (newunit=u, file=trim(libdir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "deplib"'
        close (u)
        open (newunit=u, file=trim(libdir)//'/src/depmod.f90', status='replace')
        write (u, '(a)') 'module depmod'
        write (u, '(a)') '  implicit none'
        write (u, '(a)') '  integer, parameter :: dep_value = 7'
        write (u, '(a)') 'end module depmod'
        close (u)
        open (newunit=u, file=trim(appdir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "depapp"'
        write (u, '(a)') '[dependencies]'
        write (u, '(a)') 'deplib = { path = "../lib" }'
        close (u)
        open (newunit=u, file=trim(appdir)//'/app/main.f90', status='replace')
        write (u, '(a)') 'program main'
        write (u, '(a)') '  use depmod, only: dep_value'
        write (u, '(a)') '  print "(i0)", dep_value'
        write (u, '(a)') 'end program main'
        close (u)

        b = detect_backend(appdir)
        call backend_build(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'project with path dependency builds + links')

        outfile = trim(base)//'/out.txt'
        call execute_command_line(trim(appdir)//'/build/fo/bin/depapp > '// &
            trim(outfile))
        line = ''
        open (newunit=u, file=trim(outfile), status='old', iostat=ios)
        if (ios == 0) then
            read (u, '(a)', iostat=ios) line
            close (u)
        end if
        call assert(trim(adjustl(line)) == '7', &
            'binary links path dep and prints the dep value')

        ! warm rebuild must not recompile the cached dep object, nor relink the
        ! unchanged binary (the link cache leaves a matching output untouched)
        depobj = ''
        call first_match(trim(appdir)//'/build/fo/obj', 'depmod', depobj)
        mtime_a = file_mtime(depobj)
        bin_a = file_mtime(trim(appdir)//'/build/fo/bin/depapp')
        call backend_build(b, exitcode, log_file=log_file)
        mtime_b = file_mtime(depobj)
        bin_b = file_mtime(trim(appdir)//'/build/fo/bin/depapp')
        call assert(exitcode == 0, 'warm rebuild of path-dep project succeeds')
        call assert(mtime_a > 0 .and. mtime_a == mtime_b, &
            'warm rebuild leaves cached dep object untouched')
        call assert(bin_a > 0 .and. bin_a == bin_b, &
            'warm rebuild leaves the unchanged binary unrelinked (link cache)')

        call remove_tree(base)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_builds_path_dependency

    subroutine test_gfortran_bootstraps_git_dependency()
        type(backend_t) :: b
        integer :: exitcode, u, ios
        character(len=512) :: base, libdir, appdir, log_file, outfile
        character(len=64) :: line

        call make_tmp_path('fo_test_gitdep', base)
        libdir = trim(base)//'/lib'
        appdir = trim(base)//'/app'
        call make_tmp_path('fo_backend_gitdep', log_file)

        call execute_command_line('mkdir -p '//trim(libdir)//'/src '// &
            trim(appdir)//'/app')
        open (newunit=u, file=trim(libdir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "deplib"'
        close (u)
        open (newunit=u, file=trim(libdir)//'/src/depmod.f90', status='replace')
        write (u, '(a)') 'module depmod'
        write (u, '(a)') '  implicit none'
        write (u, '(a)') '  integer, parameter :: dep_value = 13'
        write (u, '(a)') 'end module depmod'
        close (u)
        call execute_command_line('git -C '//trim(libdir)//' init -q')
        call execute_command_line('git -C '//trim(libdir)// &
            ' -c user.email=fo@example.invalid -c user.name=fo add fpm.toml src/depmod.f90')
        call execute_command_line('git -C '//trim(libdir)// &
            ' -c user.email=fo@example.invalid -c user.name=fo commit -q -m init')

        open (newunit=u, file=trim(appdir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "depapp"'
        write (u, '(a)') '[dependencies]'
        write (u, '(a,a,a)') 'deplib = { git = "file://', trim(libdir), '" }'
        close (u)
        open (newunit=u, file=trim(appdir)//'/app/main.f90', status='replace')
        write (u, '(a)') 'program main'
        write (u, '(a)') '  use depmod, only: dep_value'
        write (u, '(a)') '  print "(i0)", dep_value'
        write (u, '(a)') 'end program main'
        close (u)

        b = detect_backend(appdir)
        call backend_build(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'project with git dependency bootstraps through fpm')

        outfile = trim(base)//'/out.txt'
        call execute_command_line(trim(appdir)//'/build/fo/bin/depapp > '// &
            trim(outfile))
        line = ''
        open (newunit=u, file=trim(outfile), status='old', iostat=ios)
        if (ios == 0) then
            read (u, '(a)', iostat=ios) line
            close (u)
        end if
        call assert(trim(adjustl(line)) == '13', &
            'binary links git dep and prints the dep value')

        call remove_tree(base)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_gfortran_bootstraps_git_dependency

    subroutine first_match(dir, infix, path)
        !! Return the first file under dir whose name contains infix. Shell-based
        !! to match the other build-test helpers and avoid extra module imports.
        character(len=*), intent(in) :: dir, infix
        character(len=*), intent(out) :: path
        character(len=512) :: tmp
        integer :: u, ios

        path = ''
        call make_tmp_path('fo_match', tmp)
        call execute_command_line('find "'//trim(dir)//'" -name "*'// &
            trim(infix)//'*" 2>/dev/null | head -1 > '//trim(tmp))
        open (newunit=u, file=trim(tmp), status='old', iostat=ios)
        if (ios == 0) then
            read (u, '(a)', iostat=ios) path
            close (u)
        end if
        call execute_command_line('rm -f '//trim(tmp))
    end subroutine first_match

    integer function file_mtime(path) result(t)
        !! Seconds-resolution mtime via stat(1); 0 if missing. Test-only helper.
        character(len=*), intent(in) :: path
        character(len=512) :: tmp
        integer :: u, ios

        t = 0
        if (len_trim(path) == 0) return
        call make_tmp_path('fo_mtime', tmp)
        call execute_command_line('(stat -c %Y "'//trim(path)//'" 2>/dev/null || '// &
            'stat -f %m "'//trim(path)//'" 2>/dev/null) > '//trim(tmp))
        open (newunit=u, file=trim(tmp), status='old', iostat=ios)
        if (ios == 0) then
            read (u, *, iostat=ios) t
            close (u)
        end if
        call execute_command_line('rm -f '//trim(tmp))
    end function file_mtime

    subroutine make_helper_module_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/test')

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_helper_modules"'
        close (u)

        ! Library module with a test_ name prefix: it is a library source, not a
        ! test, and must end up in the linked library.
        open (newunit=u, file=trim(project_dir)//'/src/test_lib_helpers.f90', &
            status='replace')
        write (u, '(a)') 'module test_lib_helpers'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'function lib_answer() result(r)'
        write (u, '(a)') 'integer :: r'
        write (u, '(a)') 'r = 42'
        write (u, '(a)') 'end function lib_answer'
        write (u, '(a)') 'end module test_lib_helpers'
        close (u)

        ! Module-only helper file under test/ (not a program).
        open (newunit=u, file=trim(project_dir)//'/test/test_support.f90', &
            status='replace')
        write (u, '(a)') 'module test_support'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'function support_value() result(r)'
        write (u, '(a)') 'integer :: r'
        write (u, '(a)') 'r = 1'
        write (u, '(a)') 'end function support_value'
        write (u, '(a)') 'end module test_support'
        close (u)

        ! Test program using both the helper module and the test_ library module.
        open (newunit=u, file=trim(project_dir)//'/test/test_uses_helpers.f90', &
            status='replace')
        write (u, '(a)') 'program test_uses_helpers'
        write (u, '(a)') 'use test_support, only: support_value'
        write (u, '(a)') 'use test_lib_helpers, only: lib_answer'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'if (support_value() + lib_answer() /= 43) stop 1'
        write (u, '(a)') 'end program test_uses_helpers'
        close (u)
    end subroutine make_helper_module_project

    subroutine make_two_module_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_two_module"'
        close (u)
        call write_lib_private_body(project_dir, 1)
        open (newunit=u, file=trim(project_dir)//'/src/consumer.f90', &
            status='replace')
        write (u, '(a)') '! fixture '//trim(project_dir)
        write (u, '(a)') 'module consumer'
        write (u, '(a)') 'use lib, only: value'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine call_value(x)'
        write (u, '(a)') 'integer, intent(out) :: x'
        write (u, '(a)') 'x = 1'
        write (u, '(a)') 'end subroutine call_value'
        write (u, '(a)') 'end module consumer'
        close (u)
    end subroutine make_two_module_project

    subroutine write_lib_private_body(project_dir, value)
        character(len=*), intent(in) :: project_dir
        integer, intent(in) :: value
        integer :: u

        open (newunit=u, file=trim(project_dir)//'/src/lib.f90', status='replace')
        write (u, '(a)') '! fixture '//trim(project_dir)
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'integer function value()'
        write (u, '(a,i0)') 'value = ', value
        write (u, '(a)') 'end function value'
        write (u, '(a)') 'end module lib'
        close (u)
    end subroutine write_lib_private_body

    subroutine write_lib_interface(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        open (newunit=u, file=trim(project_dir)//'/src/lib.f90', status='replace')
        write (u, '(a)') '! fixture '//trim(project_dir)
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'integer function value(scale)'
        write (u, '(a)') 'integer, intent(in) :: scale'
        write (u, '(a)') 'value = scale'
        write (u, '(a)') 'end function value'
        write (u, '(a)') 'end module lib'
        close (u)
    end subroutine write_lib_interface

    subroutine test_cmake_skips_slow_by_default()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: project_dir, fast_log, slow_log

        call make_tmp_path('fo_test_slow_cmake', project_dir)
        call make_tmp_path('fo_backend_cmake_fast', fast_log)
        call make_tmp_path('fo_backend_cmake_slow', slow_log)
        call make_cmake_tests_project(project_dir, .false.)

        b = detect_backend(project_dir)
        call backend_test(b, exitcode, log_file=fast_log)
        call assert(exitcode == 0, 'cmake test skips excluded labels by default')

        call backend_test(b, exitcode, include_slow=.true., log_file=slow_log)
        call assert(exitcode /= 0, 'cmake test --all includes slow')

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('rm -f '//trim(fast_log)//' '//trim(slow_log))
    end subroutine test_cmake_skips_slow_by_default

    subroutine test_cmake_named_tests_select_regex()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: project_dir, log_file
        character(len=128) :: names(2)

        call make_tmp_path('fo_test_named_cmake', project_dir)
        call make_tmp_path('fo_backend_cmake_named', log_file)
        call make_cmake_tests_project(project_dir, .true.)

        b = detect_backend(project_dir)
        names(1) = 'test_a'
        names(2) = 'test_b'
        call backend_test_names(b, names, 2, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'cmake named tests select requested tests')
        call assert(file_contains(log_file, 'Test #1: test_a') .or. &
            file_contains(log_file, 'Test #2: test_a') .or. &
            file_contains(log_file, 'test_a'), &
            'cmake named log includes selected test')

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_cmake_named_tests_select_regex

    subroutine test_fpm_path_with_spaces()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: base_dir, project_dir, log_file

        call make_tmp_path('fo_test_fpm_spaces', base_dir)
        project_dir = trim(base_dir)//' path with spaces'
        call make_tmp_path('fo_backend_fpm_spaces', log_file)
        call make_simple_fpm_project(project_dir)

        b = detect_backend(project_dir)
        call backend_build(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'fpm build handles project path with spaces')
        call backend_test(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'fpm test handles project path with spaces')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_fpm_path_with_spaces

    subroutine test_cmake_path_with_spaces()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: base_dir, project_dir, log_file

        call make_tmp_path('fo_test_cmake_spaces', base_dir)
        project_dir = trim(base_dir)//' path with spaces'
        call make_tmp_path('fo_backend_cmake_spaces', log_file)
        call make_cmake_space_project(project_dir)

        b = detect_backend(project_dir)
        call backend_build(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'cmake build handles project path with spaces')
        call backend_test(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'ctest handles project path with spaces')

        call remove_tree(project_dir)
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_cmake_path_with_spaces

    subroutine test_cmake_regex_metachar_name()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: project_dir, log_file
        character(len=128) :: names(1)

        call make_tmp_path('fo_test_cmake_regex', project_dir)
        call make_tmp_path('fo_backend_cmake_regex', log_file)
        call make_cmake_regex_project(project_dir)

        b = detect_backend(project_dir)
        call backend_build(b, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'cmake regex fixture configures')

        names(1) = 'test.dot'
        call backend_test_names(b, names, 1, exitcode, log_file=log_file)
        call assert(exitcode == 0, 'ctest selected names escape regex metacharacters')
        call assert(file_contains(log_file, 'test.dot'), &
            'ctest regex fixture runs selected dotted name')

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('rm -f '//trim(log_file))
    end subroutine test_cmake_regex_metachar_name

    subroutine make_slow_fpm_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('mkdir -p '//trim(project_dir)//'/src')
        call execute_command_line('mkdir -p '//trim(project_dir)//'/test')

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_test_slow_fpm"'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/src/lib.f90', &
            status='replace')
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine noop()'
        write (u, '(a)') 'end subroutine noop'
        write (u, '(a)') 'end module lib'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/test/test_fast.f90', &
            status='replace')
        write (u, '(a)') 'program test_fast'
        write (u, '(a)') 'use lib, only: noop'
        write (u, '(a)') 'call noop()'
        write (u, '(a)') 'end program test_fast'
        close (u)

        open (newunit=u, file=trim(project_dir)// &
            '/test/test_kernel_slow.f90', status='replace')
        write (u, '(a)') 'program test_kernel_slow'
        write (u, '(a)') 'use lib, only: noop'
        write (u, '(a)') 'call noop()'
        write (u, '(a)') 'stop 1'
        write (u, '(a)') 'end program test_kernel_slow'
        close (u)
    end subroutine make_slow_fpm_project

    subroutine make_named_fpm_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/test')

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_test_named_fpm"'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/src/lib.f90', &
            status='replace')
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine noop()'
        write (u, '(a)') 'end subroutine noop'
        write (u, '(a)') 'end module lib'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/test/test_a.f90', &
            status='replace')
        write (u, '(a)') '! fixture '//trim(project_dir)
        write (u, '(a)') 'program test_a'
        write (u, '(a)') 'use lib, only: noop'
        write (u, '(a)') 'call noop()'
        write (u, '(a)') 'print *, "selected test ran"'
        write (u, '(a)') 'end program test_a'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/test/test_unselected.f90', &
            status='replace')
        write (u, '(a)') '! fixture '//trim(project_dir)
        write (u, '(a)') 'program test_unselected'
        write (u, '(a)') 'use lib, only: noop'
        write (u, '(a)') 'call noop()'
        write (u, '(a)') 'stop 1'
        write (u, '(a)') 'end program test_unselected'
        close (u)
    end subroutine make_named_fpm_project

    subroutine make_simple_fpm_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/test')

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_test_spaces"'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/src/lib.f90', &
            status='replace')
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine noop()'
        write (u, '(a)') 'end subroutine noop'
        write (u, '(a)') 'end module lib'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/test/test_fast.f90', &
            status='replace')
        write (u, '(a)') 'program test_fast'
        write (u, '(a)') 'use lib, only: noop'
        write (u, '(a)') 'call noop()'
        write (u, '(a)') 'end program test_fast'
        close (u)
    end subroutine make_simple_fpm_project

    subroutine make_many_test_fpm_project(project_dir, n_tests)
        character(len=*), intent(in) :: project_dir
        integer, intent(in) :: n_tests
        integer :: u, i
        character(len=32) :: name

        call remove_tree(project_dir)
        call make_dir(trim(project_dir)//'/src')
        call make_dir(trim(project_dir)//'/test')

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "fo_test_parallel_fpm"'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/src/lib.f90', &
            status='replace')
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'subroutine noop()'
        write (u, '(a)') 'end subroutine noop'
        write (u, '(a)') 'end module lib'
        close (u)

        do i = 1, n_tests
            write (name, '(a,i0)') 'test_', i
            open (newunit=u, file=trim(project_dir)//'/test/'//trim(name)//'.f90', &
                status='replace')
            write (u, '(a)') '! fixture '//trim(project_dir)
            write (u, '(a)') 'program '//trim(name)
            write (u, '(a)') 'use lib, only: noop'
            write (u, '(a)') 'call noop()'
            write (u, '(a)') 'end program '//trim(name)
            close (u)
        end do
    end subroutine make_many_test_fpm_project

    subroutine make_cmake_tests_project(project_dir, unselected_fails)
        character(len=*), intent(in) :: project_dir
        logical, intent(in) :: unselected_fails
        integer :: u, exitcode

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('mkdir -p '//trim(project_dir))

        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', &
            status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        write (u, '(a)') 'project(fo_backend_cmake_tests NONE)'
        write (u, '(a)') 'enable_testing()'
        write (u, '(a)') 'add_test(NAME test_a COMMAND ${CMAKE_COMMAND} -E true)'
        write (u, '(a)') 'add_test(NAME test_b COMMAND ${CMAKE_COMMAND} -E true)'
        if (unselected_fails) then
            write (u, '(a)') &
                'add_test(NAME test_unselected COMMAND ${CMAKE_COMMAND} -E false)'
        else
            write (u, '(a)') &
                'add_test(NAME test_unselected COMMAND ${CMAKE_COMMAND} -E true)'
        end if
        write (u, '(a)') &
            'add_test(NAME test_kernel_slow COMMAND ${CMAKE_COMMAND} -E false)'
        write (u, '(a)') &
            'set_tests_properties(test_kernel_slow PROPERTIES LABELS slow)'
        write (u, '(a)') &
            'add_test(NAME test_regression COMMAND ${CMAKE_COMMAND} -E false)'
        write (u, '(a)') &
            'set_tests_properties(test_regression PROPERTIES LABELS regression)'
        write (u, '(a)') &
            'add_test(NAME test_performance COMMAND ${CMAKE_COMMAND} -E false)'
        write (u, '(a)') &
            'set_tests_properties(test_performance PROPERTIES LABELS performance)'
        write (u, '(a)') &
            'add_test(NAME test_scalability COMMAND ${CMAKE_COMMAND} -E false)'
        write (u, '(a)') &
            'set_tests_properties(test_scalability PROPERTIES LABELS scalability)'
        close (u)

        call execute_command_line('cd '//trim(project_dir)// &
            ' && cmake -S . -B build >/dev/null 2>&1', &
            exitstat=exitcode, wait=.true.)
    end subroutine make_cmake_tests_project

    subroutine make_cmake_space_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call remove_tree(project_dir)
        call make_dir(project_dir)

        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', &
            status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        write (u, '(a)') 'project(fo_backend_cmake_spaces NONE)'
        write (u, '(a)') 'enable_testing()'
        write (u, '(a)') 'add_test(NAME test_space COMMAND ${CMAKE_COMMAND} -E true)'
        close (u)
    end subroutine make_cmake_space_project

    subroutine make_cmake_regex_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('mkdir -p '//trim(project_dir))

        open (newunit=u, file=trim(project_dir)//'/CMakeLists.txt', &
            status='replace')
        write (u, '(a)') 'cmake_minimum_required(VERSION 3.20)'
        write (u, '(a)') 'project(fo_backend_cmake_regex NONE)'
        write (u, '(a)') 'enable_testing()'
        write (u, '(a)') 'add_test(NAME test.dot COMMAND ${CMAKE_COMMAND} -E true)'
        write (u, '(a)') 'add_test(NAME testXdot COMMAND ${CMAKE_COMMAND} -E false)'
        close (u)
    end subroutine make_cmake_regex_project

    logical function file_contains(path, needle)
        character(len=*), intent(in) :: path, needle

        character(len=512) :: line
        integer :: u, iostat

        file_contains = .false.
        open (newunit=u, file=path, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, trim(needle)) > 0) then
                file_contains = .true.
                exit
            end if
        end do
        close (u)
    end function file_contains

    logical function file_exists(path)
        character(len=*), intent(in) :: path

        inquire (file=trim(path), exist=file_exists)
    end function file_exists

    subroutine make_tmp_path(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        ! Include the pid so the parallel test_backend* processes never collide
        ! on a scratch path (that was a cross-test race under parallel execution).
        write (path, '(a,a,a,i0,a,i0,a,i0)') '/tmp/', trim(prefix), '-', &
            process_getpid(), '-', count, '-', serial
    end subroutine make_tmp_path

    subroutine make_dir(path)
        character(len=*), intent(in) :: path

        call execute_command_line('mkdir -p "'//trim(path)//'"')
    end subroutine make_dir

    subroutine remove_tree(path)
        character(len=*), intent(in) :: path

        call execute_command_line('rm -rf "'//trim(path)//'"')
    end subroutine remove_tree
