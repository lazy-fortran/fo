module fo_gfortran_build
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, &
        fpm_config_allocate, manifest_exe_name, &
        manifest_executable_selected, &
        manifest_test_args, manifest_example_name, dep_kind, &
        DEP_PATH, DEP_REGISTRY, valid_manifest_module_name
    use fo_scan, only: scan_unit_t, scan_dir, scan_dir_regex, scan_dir_cached, &
        source_defines_module, &
        MAX_UNITS, MAX_NAME, MAX_PATH
    use fo_dag_bridge, only: build_dag_from_units
    use fo_dep_update, only: dep_acquire_sources
    use fo_dep_resolve, only: resolved_src_t, resolve_dep_srcs, &
        resolve_dev_dep_srcs, MAX_RESOLVED, join_path, merge_dep_link_libs
    use fo_registry, only: registry_config_path
    use fo_stat_memo, only: memo_save, memo_hash_file
    use fo_build_tree, only: native_output_dir, native_profiles_dir, &
        native_record_profile
    use fo_gremlin_execution_view, only: execution_view_copy_app_outputs
    use fo_test_budget, only: test_timeout_seconds, test_budget_seconds, &
        test_wall_cap_seconds, timeout_detail
    use fx_action_cache, only: cache_set_file_hash_hook
    use fo_compdb, only: compdb_write
    use fx_dag, only: dag_t, dag_find_node, dag_topo_sort, dag_levels, MAX_NODES
    use fo_cache, only: cache_t, cache_init, cache_key_for, &
        cache_restore_action, cache_store_action, hash_mod_file, &
        HASH_LEN, cache_digest, cache_file_digest, &
        cache_store_binary, cache_restore_binary, cache_binary_matches
    use fo_util, only: make_tmpfile, make_sibling_tmpfile, delete_tmpfile, &
        read_text_file, clean_root_build_artifacts
    use fo_process, only: process_run_logged, &
        process_run_argv_logged, argv_push, argv_push_split, &
        argv_push_split_nl, process_detect_nproc, process_setenv_default
    use fo_lock, only: lock_check
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_remove_file, fs_append_file, &
        fs_delete_suffix, fs_collect_files, fs_collect_mod_dirs, fs_copy_exec, &
        fs_find_executable, fs_rename, fs_mkdir_excl, fs_identity, fs_path_is_absolute
    use fo_progress, only: progress_begin, progress_step, progress_end
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t, &
        selected_compiler_command, COMPILER_NVFORTRAN, COMPILER_IFX, &
        COMPILER_FLANG
    use fo_compiler_flags, only: append_array_temporary_warning_flag, append_pipe_flag
    use fo_linker_policy, only: select_linker
    use fo_macho_image, only: macho_image_valid
    use fo_capabilities, only: compiler_supports_section_splitting
    use fo_external_modules, only: collect_external_module_dirs
    use fo_build_stamp, only: build_stamp_matches, build_stamp_quick_matches, &
        build_stamp_save
    use fo_compiler_memo, only: compiler_memo_load, compiler_memo_save
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_long_long
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    implicit none
    private

    integer, parameter :: MAX_DEP_DIRS = 64
    integer, parameter :: MAX_DEP_OBJS = 1024
    integer, parameter :: MAX_SRC_OBJS = 2048
    character(len=512), save :: detected_fc = ''
    character(len=512), save :: detected_fc_path = ''
    logical, save :: detected_fc_is_executable = .false.
    character(len=512), save :: detected_compiler = ''
    character(len=HASH_LEN), save :: detected_tool_key = ''
    !> The flags the current build was asked for, kept so the baseline can add
    !> its debug-info diet (`-g0`) without overwriting a caller's `-g`.
    !> `--debug` and `--asan` arrive here as request flags, the same reason
    !> fpm's own profile flags have to be visible to the compile path.
    character(len=512), save :: build_request_flags = ''

    type, private :: current_test_t
        !! One selected test: what to run, where its log went, how it ended.
        character(len=MAX_PATH) :: name = ''
        character(len=MAX_PATH) :: bin = ''
        character(len=512) :: log = ''
        character(len=:), allocatable :: args
        integer :: exit = 0
        integer :: rerun_exit = 0
        real :: secs = 0.0
        real :: cpu_secs = -1.0
        logical :: flaky = .false.
        logical :: ran = .false.
    end type current_test_t

    public :: gfortran_build, gfortran_test, gfortran_test_names
    public :: gfortran_run_tests
    public :: config_flags_str
    public :: gfortran_app_source_name, gfortran_test_source_name
    public :: source_has_marker, dispatch_target
    public :: gfortran_selected_test_names, gfortran_named_test_exists

    interface
        integer(c_int) function c_unsetenv(name) bind(C, name='unsetenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
        end function c_unsetenv
    end interface

contains

    logical function gremlin_execution_view_requested() result(requested)
        character(len=MAX_PATH) :: execution_cwd
        integer :: status

        execution_cwd = ''
        call get_environment_variable('FO_GREMLIN_EXECUTION_CWD', execution_cwd, &
            status=status)
        requested = .false.
        if (status == 0) requested = len_trim(execution_cwd) > 0
    end function gremlin_execution_view_requested

    integer function native_jobs() result(jobs)
        character(len=32) :: value
        integer :: status, iostat

        jobs = process_detect_nproc()
        if (jobs < 1) jobs = 1
        call get_environment_variable('FO_JOBS', value, status=status)
        if (status /= 0 .or. len_trim(value) == 0) return
        read (value, *, iostat=iostat) jobs
        if (iostat /= 0 .or. jobs < 1) jobs = process_detect_nproc()
        if (jobs < 1) jobs = 1
    end function native_jobs

    subroutine gfortran_build(project_dir, log_file, exitcode, n_compiled, flags, &
            compiler_id, use_cache, up_to_date, tests_ready, build_apps, apps_ready, &
            cached_test_dir)
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode
        integer, intent(out), optional :: n_compiled
        character(len=*), intent(in), optional :: flags
        character(len=*), intent(in), optional :: compiler_id
        logical, intent(in), optional :: use_cache
        logical, intent(out), optional :: up_to_date, tests_ready
        logical, intent(in), optional :: build_apps
        logical, intent(out), optional :: apps_ready
        character(len=*), intent(out), optional :: cached_test_dir

        type(fpm_config_t), allocatable :: config
        integer :: ierr, n_dep_includes, n_dep_objs, n_src_objs, nc
        character(len=512) :: mod_dir, obj_dir, bin_dir
        character(len=512), allocatable :: dep_includes(:)
        character(len=512), allocatable :: dep_objs(:)
        character(len=512), allocatable :: src_objs(:)
        logical, allocatable :: is_prog_arr(:)
        character(len=512) :: lf
        character(len=512) :: flag_text, compiler, request_flags, stamp_test_dir
        character(len=512) :: stamp_flags, stamp_request_flags
        character(len=512), allocatable :: stamp_roots(:)
        character(len=256) :: lock_message
        logical :: lock_ok, stamp_ok, stamp_hit, allow_cache, stamp_tests_ready
        logical :: want_apps, stamp_apps_ready
        integer :: n_stamp_roots

        lf = log_file
        if (present(up_to_date)) up_to_date = .false.
        if (present(tests_ready)) tests_ready = .false.
        if (present(apps_ready)) apps_ready = .false.
        if (present(cached_test_dir)) cached_test_dir = ''
        if (len_trim(lf) == 0) lf = '/dev/null'
        call cache_set_file_hash_hook(memo_hash_file)
        flag_text = ''
        if (present(flags)) flag_text = flags
        request_flags = flag_text
        build_request_flags = request_flags
        stamp_request_flags = request_key_flags(request_flags)
        if (present(compiler_id)) then
            compiler = compiler_id
        else
            call detect_compiler(compiler)
        end if
        allow_cache = .true.
        if (present(use_cache)) allow_cache = use_cache
        want_apps = .true.
        if (present(build_apps)) want_apps = build_apps
        stamp_hit = .false.
        if (allow_cache) then
            call build_stamp_quick_matches(project_dir, compiler, &
                stamp_request_flags, &
                stamp_hit, stamp_tests_ready, stamp_apps_ready, stamp_test_dir)
        end if
        if (stamp_hit .and. want_apps .and. .not. stamp_apps_ready) &
            stamp_hit = .false.
        if (stamp_hit) then
            if (present(n_compiled)) n_compiled = 0
            if (present(up_to_date)) up_to_date = .true.
            if (present(tests_ready)) tests_ready = stamp_tests_ready
            if (present(apps_ready)) apps_ready = stamp_apps_ready
            if (present(cached_test_dir)) cached_test_dir = stamp_test_dir
            exitcode = 0
            return
        end if

        call fpm_config_allocate(config)
        allocate (dep_includes(MAX_DEP_DIRS), dep_objs(MAX_DEP_OBJS), &
            src_objs(MAX_SRC_OBJS), is_prog_arr(MAX_SRC_OBJS))
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: cannot read or validate fpm.toml in '// &
                trim(project_dir)
            exitcode = 1
            return
        end if
        ! Combine config flags with CLI flags
        call merge_flags(config, flag_text)
        call lock_check(project_dir, flag_text, lock_ok, lock_message)
        if (.not. lock_ok) then
            write (error_unit, '(a)') 'fo: '//trim(lock_message)
            exitcode = 1
            return
        end if
        call append_array_temporary_warning_flag(fc_command(), flag_text)

        mod_dir = trim(project_dir)//'/build/fo/mod'
        obj_dir = trim(project_dir)//'/build/fo/obj'
        bin_dir = native_output_dir(project_dir, request_flags, 'bin')
        ! The native tree is shared by every compiler, so a build with a
        ! different one would overwrite the modules, objects and binaries of
        ! the last. The .mod formats are incompatible, and worse, `fo exec`
        ! would then run a binary built by a compiler the caller did not ask
        ! for: a benchmark switching FO_FC measures the wrong lane and says
        ! nothing. Clear the tree when the compiler changes.
        call guard_compiler_switch(project_dir, mod_dir, obj_dir, bin_dir)
        call fs_make_dir(mod_dir)
        call fs_make_dir(obj_dir)
        call fs_make_dir(bin_dir)
        call native_record_profile(project_dir, request_flags)
        exitcode = 0
        if (exitcode /= 0) return

        call truncate_file(trim(lf))

        call guard_root_mod_shadow(project_dir, lf)

        ! request_flags, not flag_text: the CLI-resolved profile/optimization
        ! flags only, captured before merge_flags folds in this project's own
        ! [fortran] warning/error flags (e.g. -Warray-temporaries,
        ! -Werror=implicit-interface). Those are this project's policy, not
        ! a third-party dependency's, and appending them to a git dependency
        ! build can turn warnings already present in vendored code into
        ! build noise (or, with -Werror flags, failures) that has nothing to
        ! do with the optimization-level bug this is fixing.
        call validate_native_deps(project_dir, exitcode)
        if (exitcode /= 0) return
        call merge_dep_link_libs(project_dir, config)

        call find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs)
        stamp_flags = compile_key_flags(flag_text)
        allocate (stamp_roots(8 * MAX_RESOLVED))
        call collect_stamp_roots(project_dir, stamp_roots, n_stamp_roots, stamp_ok)
        stamp_hit = .false.
        if (allow_cache .and. stamp_ok) then
            call build_stamp_matches(project_dir, compiler, stamp_flags, stamp_roots, &
                n_stamp_roots, stamp_hit, stamp_tests_ready, stamp_apps_ready, &
                stamp_test_dir)
        end if
        if (stamp_hit .and. want_apps .and. .not. stamp_apps_ready) &
            stamp_hit = .false.
        if (stamp_hit) then
            if (present(n_compiled)) n_compiled = 0
            if (present(up_to_date)) up_to_date = .true.
            if (present(tests_ready)) tests_ready = stamp_tests_ready
            if (present(apps_ready)) apps_ready = stamp_apps_ready
            if (present(cached_test_dir)) cached_test_dir = stamp_test_dir
            exitcode = 0
            return
        end if

        nc = 0
        call compile_sources(project_dir, config%source_dir, config%app_dir, &
            config%example_dir, config%auto_examples .or. config%n_examples > 0, &
            mod_dir, obj_dir, dep_includes, n_dep_includes, dep_objs, n_dep_objs, lf, &
            src_objs, n_src_objs, is_prog_arr, exitcode, nc, &
            flag_text, compiler, want_apps, config, use_cache)
        if (exitcode /= 0) return

        if (present(n_compiled)) n_compiled = nc

        if (want_apps) then
            call link_app_binaries(project_dir, config, bin_dir, &
                native_output_dir(project_dir, request_flags, 'app'), src_objs, &
                n_src_objs, is_prog_arr, dep_objs, n_dep_objs, lf, exitcode, &
                flags=flag_text, use_cache=use_cache)
        end if
        if (exitcode == 0 .and. allow_cache .and. stamp_ok) then
            call build_stamp_save(project_dir, compiler, stamp_flags, &
                stamp_request_flags, stamp_roots, n_stamp_roots, .false., want_apps, &
                config%test_dir)
        end if
        if (present(apps_ready)) apps_ready = want_apps
        if (present(cached_test_dir)) cached_test_dir = config%test_dir
        call memo_save()
    end subroutine gfortran_build

    subroutine validate_native_deps(project_dir, exitcode)
        character(len=*), intent(in) :: project_dir
        integer, intent(out) :: exitcode
        type(resolved_src_t), allocatable :: deps(:)
        integer :: n_deps, n_unresolved

        call dep_acquire_sources(project_dir, exitcode)
        if (exitcode /= 0) return
        allocate (deps(MAX_RESOLVED))
        call resolve_dep_srcs(project_dir, deps, n_deps, n_unresolved, exitcode)
        if (exitcode /= 0) return
        if (n_unresolved > 0) then
            write (error_unit, '(a)') 'fo: native dependency sources remain unresolved'
            exitcode = 1
            return
        end if
        call resolve_dev_dep_srcs(project_dir, deps, n_deps, exitcode)
    end subroutine validate_native_deps

    subroutine guard_compiler_switch(project_dir, mod_dir, obj_dir, bin_dir)
        !! Record which compiler owns the native build tree and wipe it when
        !! that changes.
        use fo_fs, only: fs_remove_tree, fs_write_text
        character(len=*), intent(in) :: project_dir, mod_dir, obj_dir, bin_dir

        character(len=512) :: stamp_path, recorded, current
        integer :: unit, iostat

        current = trim(fc_command())
        stamp_path = trim(project_dir)//'/build/fo/compiler'
        recorded = ''
        open (newunit=unit, file=trim(stamp_path), status='old', &
            action='read', iostat=iostat)
        if (iostat == 0) then
            read (unit, '(a)', iostat=iostat) recorded
            close (unit)
        end if

        if (len_trim(recorded) > 0 .and. trim(recorded) /= trim(current)) then
            write (error_unit, '(a)') 'fo: compiler changed from '// &
                trim(recorded)//' to '//trim(current)//'; rebuilding'
            call fs_remove_tree(trim(mod_dir))
            call fs_remove_tree(trim(obj_dir))
            call fs_remove_tree(trim(bin_dir))
            call fs_remove_tree(trim(project_dir)//'/build/fo/bin')
            call fs_remove_tree(trim(project_dir)//'/build/fo/app')
            call fs_remove_tree(native_profiles_dir(project_dir))
        end if
        call fs_make_dir(trim(project_dir)//'/build/fo')
        call fs_write_text(trim(stamp_path), trim(current)//new_line('a'))
    end subroutine guard_compiler_switch

    subroutine guard_root_mod_shadow(project_dir, log_file)
        !! Stale root module and object files silently shadow
        !! build/fo/mod: gfortran searches the compile working directory before
        !! the -I include dirs, so a leftover root module pins an old interface
        !! and a fresh source change appears as "symbol not found". Editors such
        !! as VS Code Modern Fortran write modules to the project root by
        !! default. Remove them up front and tell the user why, on stderr and in
        !! the build log.
        character(len=*), intent(in) :: project_dir, log_file
        character(len=200) :: line1, line2
        integer :: n_removed, u, ios

        call clean_root_build_artifacts(project_dir, n_removed)
        if (n_removed == 0) return

        write (line1, '(a,i0,a)') 'fo: removed ', n_removed, &
            ' stale root .mod, .smod, and .o build artifact(s)'
        line2 = 'fo: they shadow build/fo/mod and pin stale module interfaces; '// &
            'point your editor''s module output outside the project root '// &
            '(VS Code Modern Fortran: fortran.linter.modOutput)'
        write (error_unit, '(a)') trim(line1)
        write (error_unit, '(a)') trim(line2)
        if (len_trim(log_file) == 0) return
        open (newunit=u, file=trim(log_file), status='unknown', position='append', &
            iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') trim(line1)
        write (u, '(a)') trim(line2)
        close (u)
    end subroutine guard_root_mod_shadow

    subroutine gfortran_test(project_dir, log_file, exitcode, include_slow, &
            n_compiled, flags, build_only, use_cache)
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        integer, intent(out), optional :: n_compiled
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: build_only
        logical, intent(in), optional :: use_cache

        type(fpm_config_t), allocatable :: config
        integer :: ierr, n_dep_includes, n_dep_objs, n_lib_objs
        character(len=512) :: mod_dir, obj_dir, bin_dir
        character(len=512), allocatable :: dep_includes(:)
        character(len=512), allocatable :: dep_objs(:)
        character(len=512), allocatable :: lib_objs(:)
        character(len=512) :: lf, flag_text, request_flags, test_dir
        character(len=128) :: no_names(1)
        logical :: slow, bonly, build_current, tests_current, apps_current

        lf = log_file
        if (len_trim(lf) == 0) lf = '/dev/null'
        slow = .false.
        if (present(include_slow)) slow = include_slow
        flag_text = ''
        if (present(flags)) flag_text = flags
        request_flags = flag_text
        bonly = .false.
        if (present(build_only)) bonly = build_only
        if (present(n_compiled)) n_compiled = 0

        call gfortran_build(project_dir, lf, exitcode, flags=flag_text, &
            use_cache=use_cache, up_to_date=build_current, &
            tests_ready=tests_current, &
            build_apps=gremlin_execution_view_requested(), &
            apps_ready=apps_current, cached_test_dir=test_dir)
        if (exitcode /= 0) return

        bin_dir = native_output_dir(project_dir, request_flags, 'bin')
        if (build_current .and. tests_current .and. .not. bonly .and. &
            len_trim(test_dir) > 0) then
            call run_current_tests(project_dir, test_dir, bin_dir, no_names, &
                0, slow, lf, exitcode)
            return
        end if

        call fpm_config_allocate(config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            exitcode = 1
            return
        end if
        call merge_dep_link_libs(project_dir, config)
        call merge_flags(config, flag_text)

        mod_dir = trim(project_dir)//'/build/fo/mod'
        obj_dir = trim(project_dir)//'/build/fo/obj'
        bin_dir = native_output_dir(project_dir, request_flags, 'bin')
        if (build_current .and. tests_current .and. .not. bonly) then
            call run_current_tests(project_dir, config%test_dir, bin_dir, no_names, &
                0, slow, lf, exitcode)
            return
        end if

        allocate (dep_includes(MAX_DEP_DIRS), dep_objs(MAX_DEP_OBJS), &
            lib_objs(MAX_SRC_OBJS))
        call find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs)
        call collect_current_lib_objs(project_dir, config, obj_dir, dep_includes, &
            n_dep_includes, dep_objs, n_dep_objs, lib_objs, n_lib_objs)

        call compile_and_run_tests(project_dir, config%test_dir, mod_dir, obj_dir, &
            bin_dir, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs, lib_objs, n_lib_objs, &
            config%link_libs, config%n_link_libs, lf, &
            no_names, 0, slow, exitcode, n_compiled, &
            flags=flag_text, &
            build_only=bonly, use_cache=use_cache)
        if (exitcode == 0) call refresh_build_stamp(project_dir, flag_text, &
            request_flags, apps_current, config%test_dir, use_cache)
        call memo_save()
    end subroutine gfortran_test

    subroutine gfortran_test_names(project_dir, names, n_names, log_file, &
            exitcode, include_slow, n_compiled, flags, use_cache, build_only)
        character(len=*), intent(in) :: project_dir, log_file
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        integer, intent(out), optional :: n_compiled
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache
        logical, intent(in), optional :: build_only

        type(fpm_config_t), allocatable :: config
        integer :: ierr, n_dep_includes, n_dep_objs, n_lib_objs
        character(len=512) :: mod_dir, obj_dir, bin_dir
        character(len=512), allocatable :: dep_includes(:)
        character(len=512), allocatable :: dep_objs(:)
        character(len=512), allocatable :: lib_objs(:)
        character(len=512) :: lf, flag_text, request_flags, test_dir
        logical :: slow, bonly, build_current, tests_current, apps_current
        logical :: target_outputs_ready

        lf = log_file
        if (len_trim(lf) == 0) lf = '/dev/null'
        slow = .false.
        if (present(include_slow)) slow = include_slow
        bonly = .false.
        if (present(build_only)) bonly = build_only
        flag_text = ''
        if (present(flags)) flag_text = flags
        request_flags = flag_text
        if (present(n_compiled)) n_compiled = 0

        call gfortran_build(project_dir, lf, exitcode, flags=flag_text, &
            use_cache=use_cache, up_to_date=build_current, &
            tests_ready=tests_current, &
            build_apps=gremlin_execution_view_requested(), &
            apps_ready=apps_current, cached_test_dir=test_dir)
        if (exitcode /= 0) return

        bin_dir = native_output_dir(project_dir, request_flags, 'bin')
        if (build_current .and. tests_current) then
            call selected_test_targets_ready(project_dir, test_dir, bin_dir, &
                names, n_names, slow, target_outputs_ready)
            if (.not. target_outputs_ready) tests_current = .false.
        end if
        if (build_current .and. tests_current .and. len_trim(test_dir) > 0) then
            if (.not. bonly) call run_current_tests(project_dir, test_dir, bin_dir, &
                names, n_names, slow, lf, exitcode)
            return
        end if

        call fpm_config_allocate(config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            exitcode = 1
            return
        end if
        call merge_dep_link_libs(project_dir, config)
        call merge_flags(config, flag_text)

        mod_dir = trim(project_dir)//'/build/fo/mod'
        obj_dir = trim(project_dir)//'/build/fo/obj'
        bin_dir = native_output_dir(project_dir, request_flags, 'bin')
        if (build_current .and. tests_current) then
            if (.not. bonly) call run_current_tests(project_dir, config%test_dir, &
                bin_dir, names, n_names, slow, lf, exitcode)
            return
        end if

        allocate (dep_includes(MAX_DEP_DIRS), dep_objs(MAX_DEP_OBJS), &
            lib_objs(MAX_SRC_OBJS))
        call find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs)
        call collect_current_lib_objs(project_dir, config, obj_dir, dep_includes, &
            n_dep_includes, dep_objs, n_dep_objs, lib_objs, n_lib_objs)

        call compile_and_run_tests(project_dir, config%test_dir, mod_dir, obj_dir, &
            bin_dir, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs, lib_objs, n_lib_objs, &
            config%link_libs, config%n_link_libs, lf, &
            names, n_names, slow, exitcode, n_compiled, &
            flags=flag_text, build_only=bonly, use_cache=use_cache)
        if (exitcode == 0) call refresh_build_stamp(project_dir, flag_text, &
            request_flags, apps_current, config%test_dir, use_cache)
    end subroutine gfortran_test_names

    subroutine gfortran_run_tests(project_dir, log_file, exitcode, include_slow, &
            names, n_names, flags)
        !! Run already-built tests from the tree of the given requested flags.
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode
        logical, intent(in) :: include_slow
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        character(len=*), intent(in), optional :: flags

        type(fpm_config_t), allocatable :: config
        character(len=512) :: bin_dir, lf, request_flags
        integer :: ierr

        exitcode = 1
        if (n_names < 0 .or. n_names > size(names)) return
        call fpm_config_allocate(config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) return
        lf = log_file
        if (len_trim(lf) == 0) lf = '/dev/null'
        request_flags = ''
        if (present(flags)) request_flags = flags
        bin_dir = native_output_dir(project_dir, request_flags, 'bin')
        call run_current_tests(project_dir, config%test_dir, bin_dir, names, &
            n_names, include_slow, lf, exitcode)
    end subroutine gfortran_run_tests

    subroutine collect_stamp_roots(project_dir, roots, n_roots, ok)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(out) :: roots(:)
        integer, intent(out) :: n_roots
        logical, intent(out) :: ok

        type(resolved_src_t) :: deps(MAX_RESOLVED), devs(MAX_RESOLVED)
        integer :: n_deps, n_dev, n_unresolved, ierr, i, j, slash
        character(len=512) :: registry_config

        roots = ''
        call resolve_dep_srcs(project_dir, deps, n_deps, n_unresolved, ierr)
        ok = ierr == 0 .and. n_unresolved == 0
        n_roots = 0
        if (.not. ok) return
        do i = 1, n_deps
            if (n_roots >= size(roots)) then
                ok = .false.
                return
            end if
            n_roots = n_roots + 1
            roots(n_roots) = deps(i)%dir
            if (deps(i)%kind /= DEP_REGISTRY) cycle
            if (index(trim(deps(i)%dir), trim(project_dir)// &
                    '/build/dependencies/') == 1) cycle
            if (n_roots + 2 > size(roots)) then
                ok = .false.
                return
            end if
            if (deps(i)%registry_latest) then
                slash = index(trim(deps(i)%dir), '/', back=.true.)
                n_roots = n_roots + 1
                roots(n_roots) = deps(i)%dir(:slash - 1)
            end if
            call registry_config_path(registry_config, ierr)
            if (ierr /= 0) then
                ok = .false.
                return
            end if
            n_roots = n_roots + 1
            roots(n_roots) = trim(registry_config)
        end do
        call resolve_dev_dep_srcs(project_dir, devs, n_dev, ierr)
        if (ierr /= 0) then
            ok = .false.
            return
        end if
        do i = 1, n_dev
            do j = 1, n_roots
                if (trim(roots(j)) == trim(devs(i)%dir)) exit
            end do
            if (j <= n_roots) cycle
            if (n_roots >= size(roots)) then
                ok = .false.
                return
            end if
            n_roots = n_roots + 1
            roots(n_roots) = devs(i)%dir
            if (devs(i)%kind /= DEP_REGISTRY) cycle
            if (index(trim(devs(i)%dir), trim(project_dir)// &
                    '/build/dependencies/') == 1) cycle
            if (n_roots + 2 > size(roots)) then
                ok = .false.
                return
            end if
            if (devs(i)%registry_latest) then
                slash = index(trim(devs(i)%dir), '/', back=.true.)
                n_roots = n_roots + 1
                roots(n_roots) = devs(i)%dir(:slash - 1)
            end if
            call registry_config_path(registry_config, ierr)
            if (ierr /= 0) then
                ok = .false.
                return
            end if
            n_roots = n_roots + 1
            roots(n_roots) = trim(registry_config)
        end do
    end subroutine collect_stamp_roots

    subroutine refresh_build_stamp(project_dir, flags, request_flags, apps_ready, &
            test_dir, use_cache)
        character(len=*), intent(in) :: project_dir, flags, request_flags
        logical, intent(in) :: apps_ready
        character(len=*), intent(in) :: test_dir
        logical, intent(in), optional :: use_cache

        character(len=512) :: compiler, stamp_flags, stamp_request_flags
        character(len=512), allocatable :: roots(:)
        integer :: n_roots
        logical :: ok, allow_cache

        allow_cache = .true.
        if (present(use_cache)) allow_cache = use_cache
        if (.not. allow_cache) return
        allocate (roots(8 * MAX_RESOLVED))
        call collect_stamp_roots(project_dir, roots, n_roots, ok)
        if (.not. ok) return
        stamp_flags = flags
        call append_array_temporary_warning_flag(fc_command(), stamp_flags)
        stamp_flags = compile_key_flags(stamp_flags)
        stamp_request_flags = request_key_flags(request_flags)
        call detect_compiler(compiler)
        call build_stamp_save(project_dir, compiler, stamp_flags, &
            stamp_request_flags, roots, n_roots, .true., apps_ready, test_dir)
    end subroutine refresh_build_stamp

    integer function dispatcher_node(filenames, is_prog, topo_order, n_order, &
            config, test_dir) &
            result(node)
        !! The DAG node that builds the consolidated dispatcher program, found
        !! by its public manifest name. 0 when a project declares a dispatcher it
        !! does not ship: marked modules then have no runnable target, while
        !! programs can still run through their own node.
        character(len=*), intent(in) :: filenames(:), test_dir
        logical, intent(in) :: is_prog(:)
        type(fpm_config_t), intent(in) :: config
        integer, intent(in) :: topo_order(:), n_order
        character(len=256) :: base
        integer :: i

        node = 0
        do i = 1, n_order
            if (len_trim(filenames(topo_order(i))) == 0) cycle
            if (.not. is_prog(topo_order(i))) cycle
            base = gfortran_test_source_name(config, test_dir, &
                filenames(topo_order(i)))
            if (trim(base) == trim(config%dispatcher)) then
                node = topo_order(i)
                return
            end if
        end do
    end function dispatcher_node

    subroutine run_current_tests(project_dir, test_dir, bin_dir, selected_names, &
            n_selected, include_slow, log_file, exitcode)
        !! Run the tests the current flag profile already built, and report each
        !! by name. Independent executables run as a team (suite wall used to be
        !! the sum of ~500 serial 20ms runs while 30 cores sat idle); results
        !! still report in scan order, so the fail-name set is run-width
        !! independent.
        character(len=*), intent(in) :: project_dir, test_dir, bin_dir, log_file
        character(len=*), intent(in) :: selected_names(:)
        integer, intent(in) :: n_selected
        logical, intent(in) :: include_slow
        integer, intent(out) :: exitcode

        type(scan_unit_t), allocatable :: units(:)
        type(fpm_config_t), allocatable :: config
        type(current_test_t), allocatable :: tests(:)
        integer :: n_units, n_tests, ierr, warn_s, i
        character(len=MAX_PATH) :: execution_cwd

        exitcode = 0
        call fpm_config_allocate(config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            exitcode = 1
            return
        end if
        call scan_project_test_units(project_dir, config, units, n_units, ierr, &
            use_cached=.true.)
        if (ierr /= 0) then
            exitcode = 1
            return
        end if
        warn_s = test_warn_seconds(test_timeout_seconds(config))
        call select_current_tests(project_dir, config, units, n_units, test_dir, &
            bin_dir, selected_names, n_selected, include_slow, tests, n_tests)
        execution_cwd = test_execution_cwd(project_dir)
        if (len_trim(execution_cwd) == 0) then
            exitcode = 1
            return
        end if
        call prepare_test_app_outputs(project_dir, execution_cwd, bin_dir, exitcode)
        if (exitcode /= 0) return
        call run_current_team(project_dir, execution_cwd, config, tests, n_tests)
        call report_current_tests(project_dir, log_file, tests, n_tests, warn_s, &
            exitcode)
        do i = 1, n_tests
            if (allocated(tests(i)%args)) deallocate (tests(i)%args)
        end do
    end subroutine run_current_tests

    subroutine prepare_test_app_outputs(project_dir, execution_cwd, bin_dir, ierr)
        character(len=*), intent(in) :: project_dir, execution_cwd, bin_dir
        integer, intent(out) :: ierr

        character(len=MAX_PATH) :: message, app_output_dir
        integer :: bin_marker

        ierr = 0
        if (trim(execution_cwd) == trim(project_dir)) return
        message = ''
        app_output_dir = bin_dir
        bin_marker = len_trim(app_output_dir) - 3
        if (bin_marker < 1) then
            ierr = 1
        else if (app_output_dir(bin_marker:bin_marker + 3) /= '/bin') then
            ierr = 1
        else
            app_output_dir(bin_marker + 1:bin_marker + 3) = 'app'
            call execution_view_copy_app_outputs(project_dir, execution_cwd, &
                trim(app_output_dir), bin_dir, ierr, message)
        end if
        if (ierr /= 0) then
            if (len_trim(message) == 0) &
                message = 'cannot resolve profile app output directory'
            write (error_unit, '(a)') 'fo: '//trim(message)
        end if
    end subroutine prepare_test_app_outputs

    function test_execution_cwd(project_dir) result(execution_cwd)
        character(len=*), intent(in) :: project_dir
        character(len=MAX_PATH) :: execution_cwd, requested_execution_cwd
        integer(c_int) :: unset_status
        integer :: cwd_status

        execution_cwd = project_dir
        requested_execution_cwd = ''
        call get_environment_variable('FO_GREMLIN_EXECUTION_CWD', &
            requested_execution_cwd, status=cwd_status)
        if (cwd_status == 0 .and. len_trim(requested_execution_cwd) > 0) then
            ! Consume this CLI payload's private routing hint after selecting
            ! its cwd; nested Fo commands then use their own project cwd.
            unset_status = c_unsetenv('FO_GREMLIN_EXECUTION_CWD'//c_null_char)
            if (unset_status /= 0_c_int) then
                write (error_unit, '(a)') &
                    'fo: cannot clear internal Gremlin execution cwd from test environment'
                execution_cwd = ''
            else
                execution_cwd = trim(requested_execution_cwd)
            end if
        end if
    end function test_execution_cwd

    subroutine selected_test_targets_ready(project_dir, test_dir, bin_dir, &
            selected_names, n_selected, include_slow, ready)
        !! A build stamp records that test outputs were once produced, but a
        !! targeted build may have materialized only part of the suite. Before
        !! taking the stamped fast path, confirm every selected executable is
        !! still present (including dispatcher-routed targets).
        character(len=*), intent(in) :: project_dir, test_dir, bin_dir
        character(len=*), intent(in) :: selected_names(:)
        integer, intent(in) :: n_selected
        logical, intent(in) :: include_slow
        logical, intent(out) :: ready

        type(scan_unit_t), allocatable :: units(:)
        type(fpm_config_t), allocatable :: config
        type(current_test_t), allocatable :: tests(:)
        integer :: n_units, n_tests, ierr, i
        logical :: exists

        ready = .false.
        call fpm_config_allocate(config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) return
        call scan_project_test_units(project_dir, config, units, n_units, ierr, &
            use_cached=.true.)
        if (ierr /= 0) return
        call select_current_tests(project_dir, config, units, n_units, test_dir, &
            bin_dir, selected_names, n_selected, include_slow, tests, n_tests)
        ready = .true.
        do i = 1, n_tests
            inquire (file=trim(tests(i)%bin), exist=exists)
            if (.not. exists) ready = .false.
            call delete_tmpfile(tests(i)%log)
            if (allocated(tests(i)%args)) deallocate (tests(i)%args)
        end do
    end subroutine selected_test_targets_ready

    logical function unit_has_dispatcher_marker(unit, project_dir, test_dir) &
            result(has)
        !! Whether a scanned test unit opts into the dispatcher. The scanned
        !! `filename` is already openable as it stands (`scan_file` opens it
        !! directly), so joining it onto `project_dir/test_dir/` doubles the
        !! path and silently misses the marker - which reads as "routing does
        !! not fire" while every piece looks correctly wired. Try the scanned
        !! path first, then the joined one, so the marker is found whichever
        !! form the scanner handed back.
        type(scan_unit_t), intent(in) :: unit
        character(len=*), intent(in) :: project_dir, test_dir

        has = source_has_marker(trim(unit%filename), '! fo: dispatcher')
        if (has) return
        has = source_has_marker(trim(project_dir)//'/'//trim(test_dir)//'/'// &
            trim(unit%filename), '! fo: dispatcher')
    end function unit_has_dispatcher_marker

    logical function test_source_eligible(filename, is_program, has_dispatcher, &
            has_node) result(eligible)
        !! Runnable sources need a build unit; marked modules also need a
        !! real dispatcher program.
        character(len=*), intent(in) :: filename
        logical, intent(in) :: is_program, has_dispatcher, has_node

        eligible = .false.
        if (.not. has_node) return
        eligible = .true.
        if (is_program) return
        eligible = .false.
        if (.not. has_dispatcher) return
        eligible = source_has_marker(filename, '! fo: dispatcher')
    end function test_source_eligible

    logical function scanned_dispatcher_available(config, test_dir, units, &
            n_units) result(available)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: test_dir
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units
        character(len=MAX_PATH) :: name
        integer :: i

        available = .false.
        if (len_trim(config%dispatcher) == 0) return
        do i = 1, n_units
            if (.not. units(i)%is_program) cycle
            name = gfortran_test_source_name(config, test_dir, units(i)%filename)
            if (len_trim(name) == 0) cycle
            if (trim(name) /= trim(config%dispatcher)) cycle
            available = .true.
            return
        end do
    end function scanned_dispatcher_available

    logical function gfortran_named_test_exists(project_dir, name) result(exists)
        !! Validate public manifest names against runnable sources, including
        !! the dispatcher itself when explicitly requested.
        character(len=*), intent(in) :: project_dir, name
        type(fpm_config_t), allocatable :: config
        type(scan_unit_t), allocatable :: units(:)
        character(len=MAX_PATH) :: public_name
        integer :: i, n_units, ierr
        logical :: has_dispatcher

        exists = .false.
        call fpm_config_allocate(config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) return
        call scan_project_test_units(project_dir, config, units, n_units, ierr)
        if (ierr /= 0) return
        has_dispatcher = scanned_dispatcher_available(config, config%test_dir, &
            units, n_units)
        do i = 1, n_units
            if (.not. test_source_eligible(units(i)%filename, &
                units(i)%is_program, has_dispatcher, &
                units(i)%is_program .or. len_trim(units(i)%module_name) > 0)) cycle
            public_name = gfortran_test_source_name(config, config%test_dir, &
                units(i)%filename)
            if (trim(public_name) /= trim(name)) cycle
            exists = .true.
            return
        end do
    end function gfortran_named_test_exists

    subroutine dispatch_target(bin_dir, dispatcher, name, bin, args)
        !! Where a marked test actually runs: inside the consolidated binary,
        !! with its own name as argv, instead of linking a fresh copy of the
        !! library. The dispatcher itself never dispatches to itself - that
        !! recursion has no base case and shows up as a hung suite.
        character(len=*), intent(in) :: bin_dir, dispatcher, name
        character(len=*), intent(out) :: bin
        character(len=:), allocatable, intent(out) :: args
        character(len=:), allocatable :: args_

        if (name == trim(dispatcher)) then
            bin = trim(bin_dir)//'/'//trim(name)
            return
        end if
        bin = trim(bin_dir)//'/'//trim(dispatcher)
        allocate (character(len=len_trim(name)) :: args_)
        args_ = trim(name)
        call move_alloc(args_, args)
    end subroutine dispatch_target

    logical function source_has_marker(path, marker) result(has)
        !! Whether a test source carries the dispatcher marker. Explicit per
        !! file, so the conversion is visible in `git diff` and a test that
        !! needs its own executable (shell-driven parity oracle, corpus
        !! walker) simply never gets the marker.
        character(len=*), intent(in) :: path, marker
        character(len=4096) :: line
        integer :: u, ios

        has = .false.
        open (newunit=u, file=trim(path), action='read', status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, marker) == 1) then
                has = .true.
                exit
            end if
        end do
        close (u)
    end function source_has_marker

    subroutine gfortran_selected_test_names(project_dir, filenames, node_ids, &
            n_ids, include_slow, names, n_names, selected_node_ids)
        !! Selection uses source identities, not DAG module labels. Marked
        !! module-only cases keep their filenames as public names; ordinary
        !! helper modules and the dispatcher's infrastructure are excluded.
        character(len=*), intent(in) :: project_dir, filenames(:)
        integer, intent(in) :: node_ids(:), n_ids
        logical, intent(in) :: include_slow
        character(len=*), intent(out) :: names(:)
        integer, intent(out) :: n_names
        integer, intent(out), optional :: selected_node_ids(:)

        type(fpm_config_t), allocatable :: config
        type(scan_unit_t), allocatable :: units(:)
        character(len=MAX_PATH) :: name
        integer :: i, j, n_units, ierr
        logical :: has_dispatcher

        names = ''
        n_names = 0
        if (present(selected_node_ids)) selected_node_ids = 0
        call fpm_config_allocate(config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) return
        call scan_project_test_units(project_dir, config, units, n_units, ierr)
        if (ierr /= 0) return
        has_dispatcher = scanned_dispatcher_available(config, config%test_dir, &
            units, n_units)
        do i = 1, n_units
            if (.not. test_source_eligible(units(i)%filename, &
                units(i)%is_program, has_dispatcher, &
                units(i)%is_program .or. len_trim(units(i)%module_name) > 0)) cycle
            do j = 1, n_ids
                if (trim(filenames(node_ids(j))) == trim(units(i)%filename)) exit
            end do
            if (j > n_ids) cycle
            name = gfortran_test_source_name(config, config%test_dir, &
                units(i)%filename)
            if (len_trim(name) == 0) cycle
            if (trim(name) == trim(config%dispatcher)) cycle
            if (.not. include_slow .and. is_slow_name(name)) cycle
            if (selected_test(name, names, n_names)) cycle
            if (n_names >= size(names)) exit
            if (present(selected_node_ids)) then
                if (n_names >= size(selected_node_ids)) exit
            end if
            n_names = n_names + 1
            names(n_names) = name
            if (present(selected_node_ids)) &
                selected_node_ids(n_names) = node_ids(j)
        end do
    end subroutine gfortran_selected_test_names

    subroutine select_current_tests(project_dir, config, units, n_units, &
            test_dir, bin_dir, selected_names, n_selected, include_slow, &
            tests, n_tests)
        !! The tests to run: programs of this profile, minus slow unless asked,
        !! intersected with an explicit selection; each gets its own log path.
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(in) :: config
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units, n_selected
        character(len=*), intent(in) :: test_dir, bin_dir, selected_names(:)
        logical, intent(in) :: include_slow
        type(current_test_t), allocatable, intent(out) :: tests(:)
        integer, intent(out) :: n_tests

        character(len=MAX_PATH) :: name
        integer :: i
        logical :: has_dispatcher

        has_dispatcher = scanned_dispatcher_available(config, test_dir, units, &
            n_units)
        allocate (tests(max(1, n_units)))
        n_tests = 0
        do i = 1, n_units
            if (.not. test_source_eligible(units(i)%filename, &
                units(i)%is_program, has_dispatcher, &
                units(i)%is_program .or. len_trim(units(i)%module_name) > 0)) cycle
            name = gfortran_test_source_name(config, test_dir, units(i)%filename)
            if (len_trim(name) == 0) cycle
            ! The dispatcher is infrastructure, not a test: scanned as one it
            ! gets run bare and fails its own usage check, which would read as
            ! a suite regression. It still builds, because routed tests link it
            ! through their own node, and `fo test <dispatcher>` by explicit
            ! request reaches it below this line.
            if (len_trim(config%dispatcher) > 0 .and. &
                trim(name) == trim(config%dispatcher) .and. &
                .not. selected_test(name, selected_names, n_selected)) cycle
            if (.not. include_slow .and. is_slow_name(name)) cycle
            if (n_selected > 0) then
                if (.not. selected_test(name, selected_names, n_selected)) cycle
            end if
            n_tests = n_tests + 1
            tests(n_tests)%name = name
            tests(n_tests)%bin = trim(bin_dir)//'/'//trim(name)
            tests(n_tests)%args = manifest_test_args(config, name)
            if (has_dispatcher .and. &
                unit_has_dispatcher_marker(units(i), project_dir, test_dir) &
                ) then
                call dispatch_target(bin_dir, config%dispatcher, name, &
                    tests(n_tests)%bin, tests(n_tests)%args)
            end if
            call make_tmpfile('fo_test_case', tests(n_tests)%log)
        end do
    end subroutine select_current_tests

    subroutine run_current_team(project_dir, execution_cwd, config, tests, n_tests)
        !! Run independent test executables as an OpenMP team, `FO_JOBS`-wide.
        !! A single flaky rerun happens inside the worker, so the recorded exit
        !! is the same one the serial runner would have reported.
        character(len=*), intent(in) :: project_dir, execution_cwd
        type(fpm_config_t), intent(in) :: config
        type(current_test_t), intent(inout) :: tests(:)
        integer, intent(in) :: n_tests

        integer :: i
        integer :: rerun_log
        integer :: team_width, n_shardable, shard_allow
        character(len=8) :: shard_txt
        integer(8) :: clk0, clk1, clk_rate

        ! The team is the concurrency. A conformance walker running inside a
        ! team member must not shard its own corpus on top of it: 24 test
        ! processes each opening 16 compiler children is oversubscription, and
        ! the measured cost is that every one of them gets slower (ffc suite:
        ! team 24 + shard 16 -> 361 s wall and a 1241 s test-time sum, team 24
        ! + serial shards -> 235 s wall and 779 s). A value the user exported
        ! still wins, because this only sets a default.
        ! A blanket 1 was the wrong answer. It fixed oversubscription by
        ! putting the ceiling on the wrong axis: the suite wall is not the
        ! link phase and not the sum, it is the single longest test. Measured
        ! on ffc, `test_conformance_gauntlet_smoke` alone runs 139.87 s, so
        ! no amount of parallelism across the other 499 gets the wall under
        ! 140 s (sum 736 s / 12 jobs = 61 s ideal, actual 262 s). The long
        ! conformance walkers carry their own corpus sharding, so they are
        ! allowed to use the cores the team is not holding, bounded so the
        ! total never exceeds the machine: cores left over, divided by how
        ! many tests may shard at once. Set once, not per test, because the
        ! environment is process-global and a parallel region would race it.
        if (n_tests > 1) then
            ! Team at a quarter of the machine: the four conformance walkers
            ! are the critical path (gauntlet_smoke 158 s at jobs=1,
            ! sampling 85 s, corpus 79 s, flake 63 s, w04n), and 495 short
            ! tests sum to ~570 s, so team 8 keeps their floor at ~71 s
            ! while the leftover 24 cores shard the walkers 6-wide -
            ! their walls divide by ~6 (w04n sum 954 s, wall 264 s).
            team_width = max(1, min(n_tests, native_jobs()/4))
            n_shardable = 0
            do i = 1, n_tests
                ! `is_slow_name` matches only names containing `_slow_`, and no
                ! conformance walker is named that - so with that predicate
                ! alone n_shardable stayed 0, the variable was never set, and
                ! the suite wall stayed at the floor set by the longest single
                ! test (test_conformance_gauntlet_smoke, ~145 s). The walkers
                ! are the long ones, so they are what gets the leftover cores.
                if (is_slow_name(tests(i)%name) .or. &
                    index(tests(i)%name, 'conformance') > 0) &
                    n_shardable = n_shardable + 1
            end do
            ! The default the walkers use when the user says nothing.
            shard_allow = 1
            if (n_shardable > 0) &
                shard_allow = max(1, (native_jobs() - team_width) / n_shardable)
            write(shard_txt, '(i0)') shard_allow
            call process_setenv_default('FFC_CONFORMANCE_JOBS', trim(shard_txt))
        end if
        !$omp parallel do if (n_tests > 1) num_threads(max(1, min(n_tests, native_jobs()))) &
        !$omp& schedule(dynamic) private(i, rerun_log, clk0, clk1, clk_rate)
        do i = 1, n_tests
            call run_one_current_test(project_dir, execution_cwd, config, tests(i))
        end do
        !$omp end parallel do
        ! A failure may have lost a race with a concurrent test; rerun each once,
        ! serially and in isolation, to tell a genuine failure from flakiness.
        do i = 1, n_tests
            call rerun_failed_current_test(execution_cwd, config, tests(i))
        end do
    end subroutine run_current_team

    subroutine rerun_failed_current_test(execution_cwd, config, test)
        character(len=*), intent(in) :: execution_cwd
        type(fpm_config_t), intent(in) :: config
        type(current_test_t), intent(inout) :: test

        character(len=512) :: rerun_log

        if (.not. test%ran) return
        if (test%exit == 0 .or. test%exit == 124) return
        call make_tmpfile('fo_test_rerun', rerun_log)
        call run_test_binary(execution_cwd, test%bin, test%args, &
            rerun_log, config, is_slow_name(test%name), test%rerun_exit)
        call delete_tmpfile(rerun_log)
        if (test%rerun_exit == 0) then
            test%flaky = .true.
            test%exit = 0
        else
            test%exit = test%rerun_exit
        end if
    end subroutine rerun_failed_current_test

    subroutine run_one_current_test(project_dir, execution_cwd, config, test)
        !! One worker: run and time one test; reruns happen after the team.
        character(len=*), intent(in) :: project_dir, execution_cwd
        type(fpm_config_t), intent(in) :: config
        type(current_test_t), intent(inout) :: test

        integer(8) :: clk0, clk1, clk_rate

        call system_clock(clk0, clk_rate)
        call run_test_binary(execution_cwd, test%bin, test%args, &
            test%log, config, is_slow_name(test%name), test%exit, test%cpu_secs)
        call system_clock(clk1, clk_rate)
        test%ran = .true.
        if (clk_rate > 0) test%secs = real(clk1 - clk0) / real(clk_rate)
    end subroutine run_one_current_test

    subroutine report_current_tests(project_dir, log_file, tests, n_tests, warn_s, &
            exitcode)
        !! Emit result lines in scan order, never in completion order, so the
        !! fail-name set does not depend on how wide the team ran.
        character(len=*), intent(in) :: project_dir, log_file
        type(current_test_t), intent(in) :: tests(:)
        integer, intent(in) :: n_tests, warn_s
        integer, intent(out) :: exitcode

        character(len=8) :: exit_str
        real :: secs
        integer :: i

        exitcode = 0
        do i = 1, n_tests
            if (.not. tests(i)%ran) cycle
            if (tests(i)%flaky) then
                call append_test_stdout_block(tests(i)%log, log_file, tests(i)%name)
            else if (tests(i)%exit == 0) then
                call append_log_file(tests(i)%log, log_file)
            else
                call append_test_stdout_block(tests(i)%log, log_file, tests(i)%name)
                if (tests(i)%exit == 124) then
                    call append_timeout_status(log_file, tests(i)%name)
                else
                    call append_test_status(log_file, tests(i)%name, tests(i)%exit)
                end if
                if (exitcode == 0) exitcode = tests(i)%exit
            end if
            if (tests(i)%flaky) then
                call append_flaky_status(log_file, tests(i)%name)
                call write_test_result_line(log_file, tests(i)%name, 'FLAKY', '-', &
                    tests(i)%secs)
            else if (tests(i)%exit == 124) then
                call write_test_result_line(log_file, tests(i)%name, 'TIMEOUT', &
                    '124', tests(i)%secs)
            else if (tests(i)%exit == 0) then
                call write_test_result_line(log_file, tests(i)%name, 'PASS', '-', &
                    tests(i)%secs)
            else
                write (exit_str, '(i0)') tests(i)%exit
                call write_test_result_line(log_file, tests(i)%name, 'FAIL', &
                    trim(exit_str), tests(i)%secs)
            end if
            call delete_tmpfile(tests(i)%log)
            secs = tests(i)%secs
            ! Warn by CPU time where known: wall time also measures host load.
            if (tests(i)%cpu_secs >= 0.0) secs = tests(i)%cpu_secs
            if (tests(i)%exit == 0 .and. secs >= real(warn_s)) &
                call warn_current_slow_test(tests(i)%name, secs, &
                tests(i)%cpu_secs >= 0.0, log_file)
        end do
    end subroutine report_current_tests

    subroutine warn_current_slow_test(name, secs, is_cpu, log_file)
        character(len=*), intent(in) :: name, log_file
        real, intent(in) :: secs
        logical, intent(in) :: is_cpu

        integer :: u, ios
        character(len=:), allocatable :: unit_text

        if (is_slow_name(name)) return
        unit_text = 's'
        if (is_cpu) unit_text = 's CPU'
        write (error_unit, '(a,f0.1,a)') 'fo: slow test '//trim(name)//' took ', &
            secs, unit_text//'; name it *_slow or speed it up'
        open (newunit=u, file=trim(log_file), position='append', status='old', &
            iostat=ios)
        if (ios /= 0) return
        write (u, '(a,f0.1,a)') 'fo: warning: slow test '//trim(name)//' took ', &
            secs, unit_text//' (threshold above is advisory)'
        close (u)
    end subroutine warn_current_slow_test

    subroutine find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs)
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(in) :: config
        character(len=512), intent(out) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(out) :: n_dep_includes
        character(len=512), intent(out) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(out) :: n_dep_objs

        type(resolved_src_t), allocatable :: deps(:)
        type(resolved_src_t) :: devs(MAX_RESOLVED)
        type(fpm_config_t), allocatable :: dep_config
        integer :: i, n_deps, n_devs, n_unresolved, ierr

        n_dep_includes = 0
        n_dep_objs = 0
        allocate (deps(MAX_RESOLVED))
        call fpm_config_allocate(dep_config)
        call resolve_dep_srcs(project_dir, deps, n_deps, n_unresolved, ierr)
        if (ierr /= 0) return
        call resolve_dev_dep_srcs(project_dir, devs, n_devs, ierr)
        if (ierr /= 0) n_devs = 0

        call append_library_include_dir(project_dir, config, &
            dep_includes, n_dep_includes)
        call collect_external_module_dirs(config%external_modules, &
            config%n_external_modules, dep_includes, n_dep_includes, &
            MAX_DEP_DIRS)
        do i = 1, n_deps
            call fpm_config_parse(deps(i)%dir, dep_config, ierr)
            if (ierr /= 0) cycle
            call append_library_include_dir(deps(i)%dir, dep_config, &
                dep_includes, n_dep_includes)
            call collect_external_module_dirs(dep_config%external_modules, &
                dep_config%n_external_modules, dep_includes, &
                n_dep_includes, MAX_DEP_DIRS)
        end do
        do i = 1, n_devs
            call fpm_config_parse(devs(i)%dir, dep_config, ierr)
            if (ierr /= 0) cycle
            call append_library_include_dir(devs(i)%dir, dep_config, &
                dep_includes, n_dep_includes)
            call collect_external_module_dirs(dep_config%external_modules, &
                dep_config%n_external_modules, dep_includes, &
                n_dep_includes, MAX_DEP_DIRS)
        end do
    end subroutine find_dep_artifacts

    subroutine append_library_include_dir(project_dir, config, &
            directories, n_directories)
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(in) :: config
        character(len=512), intent(inout) :: directories(MAX_DEP_DIRS)
        integer, intent(inout) :: n_directories
        character(len=512) :: include_dir
        integer :: i, j
        logical :: exists

        do j = 1, config%n_include_dirs
            call join_path(project_dir, trim(config%include_dirs(j)), include_dir)
            inquire (file=trim(include_dir), exist=exists)
            if (.not. exists) cycle
            do i = 1, n_directories
                if (trim(directories(i)) == trim(include_dir)) exit
            end do
            if (i <= n_directories) cycle
            if (n_directories >= size(directories)) return
            n_directories = n_directories + 1
            directories(n_directories) = include_dir
        end do
    end subroutine append_library_include_dir


    function dep_object_module_key(basename) result(key)
        !! Reduce an fpm dependency object basename to its module identity by
        !! dropping everything up to and including the first '_src_' marker, so
        !! git-dep and path-dep spellings of the same module compare equal.
        character(len=*), intent(in) :: basename
        character(len=512) :: key
        integer :: p

        p = index(basename, '_src_')
        if (p == 0) then
            key = basename
        else
            key = basename(p + 5:)
        end if
    end function dep_object_module_key

    subroutine compile_sources(project_dir, src_dir, app_dir, example_dir, &
            include_examples, mod_dir, obj_dir, &
            dep_includes, n_dep_includes, dep_objs, n_dep_objs, log_file, &
            src_objs, n_src_objs, is_prog_arr, exitcode, &
            n_compiled, flags, compiler, include_apps, config, use_cache)
        character(len=*), intent(in) :: project_dir, src_dir, app_dir, example_dir
        logical, intent(in) :: include_examples
        character(len=*), intent(in) :: mod_dir, obj_dir, log_file
        character(len=*), intent(in) :: flags, compiler
        type(fpm_config_t), intent(in) :: config
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(in) :: n_dep_includes
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs
        character(len=512), intent(out) :: src_objs(MAX_SRC_OBJS)
        integer, intent(out) :: n_src_objs
        logical, intent(out) :: is_prog_arr(MAX_SRC_OBJS)
        integer, intent(out) :: exitcode, n_compiled
        logical, intent(in) :: include_apps
        logical, intent(in), optional :: use_cache

        type(scan_unit_t), allocatable :: units_a(:), units_b(:), units_c(:)
        type(scan_unit_t), allocatable :: all_units(:)
        integer :: na, nb, nc, n_all, i, ii, ierr, node_id
        type(dag_t) :: dag
        character(len=MAX_PATH), allocatable :: filenames(:)
        logical, allocatable :: is_prog(:), is_test_arr(:)
        integer, allocatable :: topo_order(:), node_levels(:)
        integer :: n_order, n_levels, lvl, total_source
        logical :: has_cycle
        logical :: allow_cache
        character(len=512) :: obj_path
        character(len=4096) :: includes_flag
        character(len=512) :: c_line
        character(len=512), allocatable :: cfiles(:)
        character(len=MAX_PATH), allocatable :: compdb_sources(:)
        character(len=512), allocatable :: compdb_objects(:)
        character(len=4096), allocatable :: source_flags(:), compdb_flags(:)
        character(len=4096), allocatable :: source_key_flags(:)
        integer :: n_cfiles, ic
        integer :: n_compdb
        type(resolved_src_t) :: deps(MAX_RESOLVED)
        integer :: n_deps_resolved
        integer :: ii_failed

        type(cache_t) :: c
        integer :: cache_ierr
        character(len=HASH_LEN), allocatable :: old_mod_keys(:), new_mod_keys(:)
        character(len=HASH_LEN) :: output_id
        integer, allocatable :: compile_nodes(:)
        character(len=HASH_LEN), allocatable :: compile_keys(:)
        integer, allocatable :: level_nodes(:)
        character(len=HASH_LEN), allocatable :: level_keys(:)
        logical, allocatable :: level_restored(:)
        logical, allocatable :: smod_cacheable(:)
        character(len=2 * MAX_NAME + 1), allocatable :: smod_names(:)
        integer, allocatable :: compile_exits(:)
        character(len=512), allocatable :: per_logs(:)
        integer :: n_compile, n_level, team_size
        character(len=MAX_PATH) :: fname_local
        character(len=512) :: per_log_local
        character(len=512) :: level_obj_path
        character(len=HASH_LEN), allocatable :: level_dep_keys(:)
        character(len=HASH_LEN) :: level_source_key
        integer :: level_dep_count
        logical :: level_complete, level_hit

        n_src_objs = 0
        is_prog_arr = .false.
        exitcode = 0
        n_compiled = 0

        allocate (filenames(MAX_NODES), is_prog(MAX_NODES), is_test_arr(MAX_NODES))
        allocate (topo_order(MAX_NODES), node_levels(MAX_NODES))
        allocate (old_mod_keys(MAX_NODES), new_mod_keys(MAX_NODES))
        allocate (compile_nodes(MAX_NODES), compile_keys(MAX_NODES))
        allocate (level_nodes(MAX_NODES), level_keys(MAX_NODES))
        allocate (level_restored(MAX_NODES))
        allocate (smod_cacheable(MAX_NODES))
        allocate (smod_names(MAX_NODES))
        allocate (compile_exits(MAX_NODES), per_logs(MAX_NODES))
        allocate (compdb_sources(MAX_NODES), compdb_objects(MAX_NODES))
        allocate (source_flags(MAX_NODES), compdb_flags(MAX_NODES))
        allocate (source_key_flags(MAX_NODES))

        call scan_dir(trim(project_dir)//'/'//trim(src_dir), units_a, na, ierr)
        call scan_dir(trim(project_dir)//'/'//trim(app_dir), units_b, nb, ierr)
        nc = 0
        if (include_examples) &
            call scan_dir(trim(project_dir)//'/'//trim(example_dir), units_c, nc, ierr)

        n_all = na
        allocate (all_units(na + nb + nc))
        do i = 1, nb
            n_all = n_all + 1
            all_units(n_all) = units_b(i)
        end do
        do i = 1, na
            all_units(i) = units_a(i)
        end do
        do i = 1, nc
            n_all = n_all + 1
            all_units(n_all) = units_c(i)
        end do

        ! Fold path dependencies and any missing external-dependency module
        ! providers into the same source-ordered native DAG.
        call add_dep_sources(project_dir, all_units, n_all, deps, n_deps_resolved)

        call validate_module_naming(config, all_units(:n_all), deps, &
            n_deps_resolved, exitcode)
        if (exitcode /= 0) return

        call build_dag_from_units(all_units, n_all, dag, filenames, is_test_arr, is_prog)
        call dag_topo_sort(dag, topo_order, n_order, has_cycle)
        call dag_levels(dag, topo_order, n_order, node_levels, n_levels)
        call remove_shadow_mods(project_dir, dag)
        call make_includes_flag(mod_dir, dep_includes, n_dep_includes, includes_flag)
        call package_source_flags(filenames, flags, config, deps, n_deps_resolved, &
            source_flags)

        old_mod_keys = ''
        new_mod_keys = ''
        smod_names = ''
        smod_cacheable = .true.
        call load_mod_keys(mod_dir, dag, n_order, topo_order, old_mod_keys)
        call cache_init(c, cache_ierr)
        allow_cache = .true.
        if (present(use_cache)) allow_cache = use_cache
        if (.not. allow_cache) cache_ierr = 1

        total_source = 0
        n_compdb = 0
        do i = 1, n_order
            node_id = topo_order(i)
            if (is_test_arr(node_id)) cycle
            if (len_trim(filenames(node_id)) == 0) cycle
            if (.not. include_apps .and. source_is_in_dir(filenames(node_id), &
                project_dir, app_dir)) cycle
            if (.not. include_apps .and. source_is_in_dir(filenames(node_id), &
                project_dir, example_dir)) cycle
            if (is_prog(node_id) .and. .not. app_program_selected( &
                filenames(node_id), project_dir, app_dir, config)) cycle
            call source_smod_name(filenames(node_id), dag%nodes(node_id)%label, &
                smod_names(node_id), smod_cacheable(node_id))
            total_source = total_source + 1
            n_compdb = n_compdb + 1
            compdb_sources(n_compdb) = filenames(node_id)
            compdb_flags(n_compdb) = source_flags(node_id)
            ! Construct deferred-length compiler policy strings before workers
            ! start. Some gfortran versions use process-global result-length
            ! temporaries even for recursive deferred-length functions.
            source_key_flags(node_id) = compile_key_flags(source_flags(node_id))
            call make_obj_path(filenames(node_id), project_dir, obj_dir, &
                compdb_objects(n_compdb))
        end do
        call progress_begin('build', total_source)

        if (cache_ierr == 0) then
            do lvl = 0, n_levels - 1
                n_compile = 0
                n_level = 0
                compile_exits = 0
                ii_failed = 0

                do i = 1, n_order
                    node_id = topo_order(i)
                    if (node_levels(i) /= lvl) cycle
                    if (is_test_arr(node_id)) cycle
                    if (len_trim(filenames(node_id)) == 0) cycle
                    if (.not. include_apps .and. source_is_in_dir( &
                        filenames(node_id), project_dir, app_dir)) cycle
                    if (.not. include_apps .and. source_is_in_dir( &
                        filenames(node_id), project_dir, example_dir)) cycle
                    if (is_prog(node_id) .and. .not. app_program_selected( &
                        filenames(node_id), project_dir, app_dir, config)) cycle

                    n_level = n_level + 1
                    level_nodes(n_level) = node_id
                end do

                team_size = max(1, min(n_level, native_jobs()))
                !$omp parallel do if(n_level > 1) num_threads(team_size) &
                !$omp schedule(static) private(node_id, level_dep_keys, &
                !$omp level_dep_count, level_complete, level_source_key, &
                !$omp level_obj_path, level_hit)
                do i = 1, n_level
                    node_id = level_nodes(i)
                    level_keys(i) = ''
                    level_hit = .false.

                    call collect_dep_keys_source_order(all_units, n_all, dag, &
                        filenames(node_id), &
                        new_mod_keys, dep_includes, n_dep_includes, &
                        level_dep_keys, level_dep_count, level_complete)
                    if (.not. level_complete) then
                        level_restored(i) = .false.
                        cycle
                    end if
                    level_source_key = cache_key_for(filenames(node_id), compiler, &
                        source_key_flags(node_id), level_dep_keys, level_dep_count, &
                        include_dirs=dep_includes(:n_dep_includes))
                    level_keys(i) = level_source_key

                    call make_obj_path(filenames(node_id), project_dir, obj_dir, &
                        level_obj_path)
                    if (cache_ierr == 0 .and. smod_cacheable(node_id)) then
                        if (source_defines_module(filenames(node_id))) then
                            call cache_restore_action(c, level_source_key, &
                                level_obj_path, mod_dir, level_hit, required_mod_name= &
                                dag%nodes(node_id)%label, required_smod_name= &
                                trim(smod_names(node_id)))
                        else
                            call cache_restore_action(c, level_source_key, &
                                level_obj_path, mod_dir, level_hit, &
                                required_smod_name=trim(smod_names(node_id)))
                        end if
                        if (level_hit) then
                            call get_mod_key(dag%nodes(node_id)%label, mod_dir, &
                                new_mod_keys(node_id), smod_names(node_id))
                            call progress_step()
                        end if
                    end if
                    level_restored(i) = level_hit
                end do
                !$omp end parallel do

                do i = 1, n_level
                    if (level_restored(i)) cycle
                    n_compile = n_compile + 1
                    compile_nodes(n_compile) = level_nodes(i)
                    compile_keys(n_compile) = level_keys(i)
                end do

                do ii = 1, n_compile
                    call make_tmpfile('fo_compile', per_logs(ii))
                end do

                team_size = max(1, min(n_compile, native_jobs()))
                !$omp parallel do if(n_compile > 1) num_threads(team_size) schedule(static) &
                !$omp private(node_id, obj_path, fname_local, per_log_local)
                do ii = 1, n_compile
                    node_id = compile_nodes(ii)
                    fname_local = filenames(node_id)
                    per_log_local = per_logs(ii)
                    call make_obj_path(fname_local, project_dir, obj_dir, obj_path)
                    if (len_trim(smod_names(node_id)) > 0) call fs_remove_file( &
                        trim(mod_dir)//'/'//trim(smod_names(node_id))//'.smod')
                    call compile_f90(project_dir, fname_local, obj_path, &
                        with_user_flags(includes_flag, source_flags(node_id)), &
                        per_log_local, compile_exits(ii))
                    call progress_step()
                end do
                !$omp end parallel do

                ! A compiler launch from a crowded OpenMP region can fail
                ! transiently (for example, exec returning ENOENT despite the
                ! compiler being present). Retry only failed actions once after
                ! the workers have joined. A genuine compiler diagnostic fails
                ! again and is reported normally.
                do ii = 1, n_compile
                    if (compile_exits(ii) == 0) cycle
                    call truncate_file(trim(per_logs(ii)))
                    node_id = compile_nodes(ii)
                    call make_obj_path(filenames(node_id), project_dir, obj_dir, &
                        obj_path)
                    call compile_f90(project_dir, filenames(node_id), obj_path, &
                        with_user_flags(includes_flag, source_flags(node_id)), &
                        per_logs(ii), compile_exits(ii))
                end do

                do ii = 1, n_compile
                    call append_log_file(trim(per_logs(ii)), log_file)
                end do
                ii_failed = 0
                do ii = 1, n_compile
                    if (compile_exits(ii) /= 0) then
                        call append_compile_failure_source(log_file, filenames( &
                            compile_nodes(ii)))
                        ii_failed = ii
                        exit
                    end if
                    node_id = compile_nodes(ii)
                    call make_obj_path(filenames(node_id), project_dir, obj_dir, &
                        obj_path)
                    call get_mod_key(dag%nodes(node_id)%label, mod_dir, &
                        new_mod_keys(node_id), smod_names(node_id))
                    if (len_trim(compile_keys(ii)) > 0 .and. &
                        smod_cacheable(node_id)) then
                        call cache_store_action(c, compile_keys(ii), obj_path, mod_dir, &
                            dag%nodes(node_id)%label, output_id, cache_ierr, &
                            smod_name=trim(smod_names(node_id)))
                    end if
                end do
                do ii = 1, n_compile
                    call delete_tmpfile(per_logs(ii))
                end do
                if (ii_failed > 0) then
                    call progress_end()
                    exitcode = compile_exits(ii_failed)
                    return
                end if
                n_compiled = n_compiled + n_compile
            end do
            call save_mod_keys(mod_dir, dag, n_order, topo_order, new_mod_keys)
        else
            do i = 1, n_order
                node_id = topo_order(i)
                if (len_trim(filenames(node_id)) == 0) cycle
                if (is_test_arr(node_id)) cycle
                if (.not. include_apps .and. source_is_in_dir(filenames(node_id), &
                    project_dir, app_dir)) cycle
                if (.not. include_apps .and. source_is_in_dir(filenames(node_id), &
                    project_dir, example_dir)) cycle
                if (is_prog(node_id) .and. .not. app_program_selected( &
                    filenames(node_id), project_dir, app_dir, config)) cycle
                call make_obj_path(filenames(node_id), project_dir, obj_dir, obj_path)
                call compile_f90(project_dir, filenames(node_id), obj_path, &
                    with_user_flags(includes_flag, source_flags(node_id)), log_file, &
                    exitcode)
                if (exitcode /= 0) then
                    call progress_end()
                    return
                end if
                call progress_step()
                n_compiled = n_compiled + 1
            end do
        end if
        call progress_end()

        call compdb_write(trim(project_dir)//'/build/compile_commands.json', &
            project_dir, compdb_sources, compdb_objects, n_compdb, &
            fc_command(), fc_base_flags(), includes_flag, flags, compdb_flags)

        do i = 1, n_order
            node_id = topo_order(i)
            if (len_trim(filenames(node_id)) == 0) cycle
            if (is_test_arr(node_id)) cycle
            if (.not. include_apps .and. source_is_in_dir(filenames(node_id), &
                project_dir, app_dir)) cycle
            if (.not. include_apps .and. source_is_in_dir(filenames(node_id), &
                project_dir, example_dir)) cycle
            if (is_prog(node_id) .and. .not. app_program_selected( &
                filenames(node_id), project_dir, app_dir, config)) cycle
            call make_obj_path(filenames(node_id), project_dir, obj_dir, obj_path)
            if (n_src_objs < MAX_SRC_OBJS) then
                n_src_objs = n_src_objs + 1
                src_objs(n_src_objs) = obj_path
                is_prog_arr(n_src_objs) = is_prog(node_id)
            end if
        end do

        allocate (cfiles(MAX_SRC_OBJS))
        call collect_c_family(trim(project_dir)//'/'//trim(src_dir), &
            cfiles, n_cfiles)
        do ic = 1, n_cfiles
            c_line = cfiles(ic)
            if (len_trim(c_line) == 0) cycle
            call make_obj_path(trim(c_line), project_dir, obj_dir, obj_path)
            call compile_c_family(trim(c_line), obj_path, &
                project_dir, log_file, exitcode)
            if (exitcode /= 0) then
                deallocate (cfiles)
                return
            end if
            if (n_src_objs < MAX_SRC_OBJS) then
                n_src_objs = n_src_objs + 1
                src_objs(n_src_objs) = obj_path
                is_prog_arr(n_src_objs) = .false.
            end if
        end do
        deallocate (cfiles)

        call compile_dep_c_sources(deps, n_deps_resolved, project_dir, obj_dir, &
            log_file, src_objs, n_src_objs, is_prog_arr, exitcode)
    end subroutine compile_sources

    logical function app_program_selected(source, project_dir, app_dir, config) &
            result(selected)
        character(len=*), intent(in) :: source, project_dir, app_dir
        type(fpm_config_t), intent(in) :: config
        character(len=128) :: stem

        selected = .true.
        if (.not. source_is_in_dir(source, project_dir, app_dir)) return
        call file_basename(source, stem)
        selected = manifest_executable_selected(config, app_dir, stem)
    end function app_program_selected

    subroutine source_smod_name(path, label, smod_name, cacheable)
        character(len=*), intent(in) :: path, label
        character(len=*), intent(out) :: smod_name
        logical, intent(out) :: cacheable

        character(len=512) :: line, lower, ancestor
        integer :: u, ios, open_pos, close_pos, colon_pos, comment_pos
        integer :: n_modules, n_submodules

        smod_name = ''
        cacheable = .true.
        n_modules = 0
        n_submodules = 0
        open (newunit=u, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) then
            cacheable = .false.
            return
        end if
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            lower = adjustl(line)
            call lowercase_inplace(lower)
            comment_pos = index(lower, '!')
            if (comment_pos > 0) lower(comment_pos:) = ''
            if (starts_with_submodule(lower)) then
                n_submodules = n_submodules + 1
                open_pos = index(lower, '(')
                close_pos = index(lower, ')')
                if (close_pos <= open_pos + 1) then
                    cacheable = .false.
                    exit
                end if
                ancestor = adjustl(lower(open_pos + 1:close_pos - 1))
                colon_pos = index(ancestor, ':')
                if (colon_pos > 0) ancestor = ancestor(:colon_pos - 1)
                smod_name = trim(ancestor)//'@'//trim(label)
                cycle
            end if
            if (index(lower, 'submodule') == 1) cacheable = .false.
            if (index(lower, 'module ') == 1 .and. &
                index(lower, ' function ') == 0 .and. &
                index(lower, ' subroutine ') == 0 .and. &
                index(lower, ' procedure ') == 0) n_modules = n_modules + 1
            if (index(lower, 'module ') == 1 .and. &
                (index(lower, ' subroutine ') > 0 .or. &
                index(lower, ' function ') > 0 .or. &
                index(lower, ' procedure ') > 0)) then
                if (n_submodules == 0) smod_name = label
            end if
        end do
        close (u)
        if (n_modules + n_submodules > 1) cacheable = .false.
        if (.not. cacheable) smod_name = ''
    end subroutine source_smod_name

    subroutine lowercase_inplace(text)
        character(len=*), intent(inout) :: text

        integer :: i

        do i = 1, len_trim(text)
            if (text(i:i) >= 'A' .and. text(i:i) <= 'Z') then
                text(i:i) = achar(iachar(text(i:i)) + 32)
            end if
        end do
    end subroutine lowercase_inplace

    logical function starts_with_submodule(text) result(matches)
        character(len=*), intent(in) :: text

        integer :: open_pos

        matches = .false.
        if (index(text, 'submodule') /= 1) return
        open_pos = index(text, '(')
        if (open_pos == 0) return
        if (open_pos > 10) then
            if (len_trim(text(10:open_pos - 1)) > 0) return
        end if
        matches = .true.
    end function starts_with_submodule

    subroutine append_compile_failure_source(log_file, source_file)
        character(len=*), intent(in) :: log_file, source_file

        integer :: u, ios

        open (newunit=u, file=trim(log_file), position='append', iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') 'fo: failed source: '//trim(source_file)
        close (u)
    end subroutine append_compile_failure_source


    subroutine add_dep_sources(project_dir, all_units, n_all, deps, n_deps)
        !! Scan every transitive path-dependency's library source dir and append
        !! its module units to all_units. Program units in a dep are skipped: a
        !! dependency contributes a library, never an executable of ours. The
        !! resolved dep list is returned so the caller can also compile each
        !! dep's C sources for linking. Acquired external dependencies normally
        !! keep using fpm artifacts; only source providers for modules absent
        !! from those artifacts are added to the native DAG.
        character(len=*), intent(in) :: project_dir
        type(scan_unit_t), allocatable, intent(inout) :: all_units(:)
        integer, intent(inout) :: n_all
        type(resolved_src_t), intent(out) :: deps(MAX_RESOLVED)
        integer, intent(out) :: n_deps
        type(scan_unit_t), allocatable :: ud(:), grown(:)
        integer :: n_unres, ierr, d, j, nu, old_n

        n_deps = 0
        call resolve_dep_srcs(project_dir, deps, n_deps, n_unres, ierr)
        if (ierr /= 0) return

        do d = 1, n_deps
            call scan_dir(trim(deps(d)%src_dir), ud, nu, ierr)
            if (ierr /= 0) cycle
            old_n = n_all
            allocate (grown(old_n + nu))
            if (old_n > 0) grown(1:old_n) = all_units(1:old_n)
            do j = 1, nu
                if (ud(j)%is_program) cycle
                n_all = n_all + 1
                grown(n_all) = ud(j)
            end do
            call move_alloc(grown, all_units)
        end do
    end subroutine add_dep_sources

    subroutine validate_module_naming(root_config, units, deps, n_deps, exitcode)
        type(fpm_config_t), intent(in) :: root_config
        type(scan_unit_t), intent(in) :: units(:)
        type(resolved_src_t), intent(in) :: deps(:)
        integer, intent(in) :: n_deps
        integer, intent(out) :: exitcode
        type(fpm_config_t), allocatable :: package
        character(len=:), allocatable :: prefix
        integer :: i, d, owner, longest, n, ierr

        exitcode = 0
        if (.not. root_config%module_naming) return
        call fpm_config_allocate(package)
        do i = 1, size(units)
            if (len_trim(units(i)%module_name) == 0) cycle
            owner = 0
            longest = 0
            do d = 1, n_deps
                prefix = trim(deps(d)%src_dir)//'/'
                n = len(prefix)
                if (len_trim(units(i)%filename) < n) cycle
                if (units(i)%filename(:n) /= prefix) cycle
                if (n <= longest) cycle
                owner = d
                longest = n
            end do
            if (owner == 0) then
                package = root_config
            else
                call fpm_config_parse(trim(deps(owner)%dir), package, ierr)
                if (ierr /= 0) then
                    exitcode = 1
                    return
                end if
                ! FPM's root policy enforces package names even for dependencies
                ! whose own manifest disables naming. Their custom prefix is
                ! available only when enabled by that dependency's manifest.
                package%module_naming = .true.
            end if
            if (valid_manifest_module_name(package, trim(units(i)%module_name))) cycle
            write (error_unit, '(a)') 'fo: module '//trim(units(i)%module_name)// &
                ' in '//trim(units(i)%filename)//' does not match package '// &
                trim(package%name)//' or its [build] module-naming prefix'
            exitcode = 1
        end do
    end subroutine validate_module_naming



    logical function dep_provider_object_exists(source, dep_objs, n_dep_objs) &
            result(found)
        !! fpm object names end in the original source basename plus '.o',
        !! prefixed by the dependency-relative path with '_' separators.
        character(len=*), intent(in) :: source
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs

        character(len=512) :: source_base, object_base, needle
        integer :: i, slash, nbase, nneedle

        found = .false.
        slash = index(trim(source), '/', back=.true.)
        if (slash > 0) then
            source_base = source(slash + 1:len_trim(source))
        else
            source_base = trim(source)
        end if
        needle = trim(source_base)//'.o'
        nneedle = len_trim(needle)
        do i = 1, n_dep_objs
            slash = index(trim(dep_objs(i)), '/', back=.true.)
            if (slash > 0) then
                object_base = dep_objs(i)(slash + 1:len_trim(dep_objs(i)))
            else
                object_base = trim(dep_objs(i))
            end if
            nbase = len_trim(object_base)
            if (nbase < nneedle) cycle
            if (object_base(nbase - nneedle + 1:nbase) /= needle(1:nneedle)) cycle
            if (nbase > nneedle) then
                if (object_base(nbase - nneedle:nbase - nneedle) /= '_') cycle
            end if
            found = .true.
            return
        end do
    end function dep_provider_object_exists

    logical function unit_set_defines_module(units, n_units, name) result(found)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units
        character(len=*), intent(in) :: name

        integer :: i

        found = .false.
        do i = 1, n_units
            if (trim(units(i)%module_name) /= trim(name)) cycle
            found = .true.
            return
        end do
    end function unit_set_defines_module

    subroutine append_module_unit(units, n_units, candidate)
        type(scan_unit_t), allocatable, intent(inout) :: units(:)
        integer, intent(inout) :: n_units
        type(scan_unit_t), intent(in) :: candidate

        type(scan_unit_t), allocatable :: grown(:)

        allocate (grown(n_units + 1))
        if (n_units > 0) grown(1:n_units) = units(1:n_units)
        n_units = n_units + 1
        grown(n_units) = candidate
        call move_alloc(grown, units)
    end subroutine append_module_unit

    subroutine append_module_units(units, n_units, candidates, n_candidates)
        type(scan_unit_t), allocatable, intent(inout) :: units(:)
        integer, intent(inout) :: n_units
        type(scan_unit_t), intent(in) :: candidates(:)
        integer, intent(in) :: n_candidates

        type(scan_unit_t), allocatable :: grown(:)
        integer :: i, old_n

        old_n = n_units
        allocate (grown(old_n + n_candidates))
        if (old_n > 0) grown(1:old_n) = units(1:old_n)
        do i = 1, n_candidates
            if (candidates(i)%is_program) cycle
            n_units = n_units + 1
            grown(n_units) = candidates(i)
        end do
        call move_alloc(grown, units)
    end subroutine append_module_units

    subroutine compile_dep_c_sources(deps, n_deps, project_dir, obj_dir, &
            log_file, src_objs, n_src_objs, is_prog_arr, exitcode)
        !! Compile each path-dependency's C sources (e.g. fortfront's
        !! stdout_sanitizer.c) and add the objects to the link set, so symbols
        !! a dep's Fortran calls into are resolved.
        type(resolved_src_t), intent(in) :: deps(MAX_RESOLVED)
        integer, intent(in) :: n_deps
        character(len=*), intent(in) :: project_dir, obj_dir, log_file
        character(len=512), intent(inout) :: src_objs(MAX_SRC_OBJS)
        integer, intent(inout) :: n_src_objs
        logical, intent(inout) :: is_prog_arr(MAX_SRC_OBJS)
        integer, intent(inout) :: exitcode

        character(len=512), allocatable :: cfiles(:)
        character(len=512) :: c_line, obj_path
        integer :: d, ic, n_cfiles

        allocate (cfiles(MAX_SRC_OBJS))
        do d = 1, n_deps
            call collect_c_family(trim(deps(d)%src_dir), cfiles, n_cfiles)
            do ic = 1, n_cfiles
                c_line = cfiles(ic)
                if (len_trim(c_line) == 0) cycle
                call make_obj_path(trim(c_line), project_dir, obj_dir, obj_path)
                call compile_c_family(trim(c_line), obj_path, &
                    trim(deps(d)%dir), log_file, exitcode)
                if (exitcode /= 0) then
                    deallocate (cfiles)
                    return
                end if
                if (n_src_objs < MAX_SRC_OBJS) then
                    n_src_objs = n_src_objs + 1
                    src_objs(n_src_objs) = obj_path
                    is_prog_arr(n_src_objs) = .false.
                end if
            end do
        end do
        deallocate (cfiles)
    end subroutine compile_dep_c_sources

    subroutine compile_test_dev_c_sources(deps, n_deps, project_dir, obj_dir, &
            log_file, objects, n_objects, exitcode)
        !! Compile C/C++ sources from dev-dependencies into test-only links.
        type(resolved_src_t), intent(in) :: deps(MAX_RESOLVED)
        integer, intent(in) :: n_deps
        character(len=*), intent(in) :: project_dir, obj_dir, log_file
        character(len=512), intent(inout) :: objects(MAX_DEP_OBJS)
        integer, intent(inout) :: n_objects
        integer, intent(inout) :: exitcode

        character(len=512), allocatable :: cfiles(:)
        character(len=512) :: source, object_path
        integer :: d, i, n_cfiles

        allocate (cfiles(MAX_SRC_OBJS))
        do d = 1, n_deps
            call collect_c_family(trim(deps(d)%src_dir), cfiles, n_cfiles)
            do i = 1, n_cfiles
                if (n_objects >= MAX_DEP_OBJS) then
                    exitcode = 1
                    deallocate (cfiles)
                    return
                end if
                source = cfiles(i)
                call make_obj_path(trim(source), project_dir, obj_dir, object_path)
                call compile_c_family(trim(source), object_path, &
                    trim(deps(d)%dir), log_file, exitcode)
                if (exitcode /= 0) then
                    deallocate (cfiles)
                    return
                end if
                n_objects = n_objects + 1
                objects(n_objects) = object_path
            end do
        end do
        deallocate (cfiles)
    end subroutine compile_test_dev_c_sources

    subroutine add_external_dep_keys(units, n_units, source_path, &
            dep_includes, n_dep_includes, dep_keys, n_dep)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units, n_dep_includes
        character(len=*), intent(in) :: source_path
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        character(len=HASH_LEN), allocatable, intent(out) :: dep_keys(:)
        integer, intent(inout) :: n_dep

        integer :: i, j
        character(len=512) :: modpath
        logical :: found

        n_dep = 0
        do i = 1, n_units
            if (trim(units(i)%filename) /= trim(source_path)) cycle
            ! Reserve one further key for the linked project library.
            allocate (dep_keys(units(i)%n_deps + 1))
            dep_keys = ''
            do j = 1, units(i)%n_deps
                ! Reachable test helpers have already compiled. Their .mod
                ! interfaces can change even when their object bytes do not.
                call find_dep_mod_file(units(i)%deps(j), dep_includes, &
                    n_dep_includes, modpath, found)
                if (.not. found) cycle
                n_dep = n_dep + 1
                call hash_mod_file(modpath, dep_keys(n_dep))
            end do
            return
        end do
        allocate (dep_keys(1))
        dep_keys = ''
    end subroutine add_external_dep_keys

    subroutine collect_dep_keys_source_order(units, n_units, dag, source_path, &
            mod_keys, dep_includes, &
            n_dep_includes, dep_keys, n_dep, complete)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units, n_dep_includes
        character(len=*), intent(in) :: source_path
        type(dag_t), intent(in) :: dag
        character(len=HASH_LEN), intent(in) :: mod_keys(MAX_NODES)
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        character(len=HASH_LEN), allocatable, intent(out) :: dep_keys(:)
        integer, intent(out) :: n_dep
        logical, intent(out) :: complete

        integer :: i, j, dep_id
        character(len=512) :: modpath
        logical :: found

        n_dep = 0
        complete = .true.
        do i = 1, n_units
            if (trim(units(i)%filename) /= trim(source_path)) cycle

            allocate (dep_keys(units(i)%n_deps))
            dep_keys = ''
            do j = 1, units(i)%n_deps
                dep_id = dag_find_node(dag, units(i)%deps(j))
                if (dep_id > 0) then
                    if (len_trim(mod_keys(dep_id)) == 0) then
                        complete = .false.
                        return
                    end if
                    n_dep = n_dep + 1
                    dep_keys(n_dep) = mod_keys(dep_id)
                else
                    call find_dep_mod_file(units(i)%deps(j), dep_includes, &
                        n_dep_includes, modpath, found)
                    if (.not. found) cycle
                    n_dep = n_dep + 1
                    call hash_mod_file(modpath, dep_keys(n_dep))
                end if
            end do
            return
        end do
        allocate (dep_keys(0))
    end subroutine collect_dep_keys_source_order

    subroutine find_dep_mod_file(modname, dep_includes, n_dep_includes, modpath, &
            found)
        character(len=*), intent(in) :: modname
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(in) :: n_dep_includes
        character(len=*), intent(out) :: modpath
        logical, intent(out) :: found

        character(len=MAX_NAME) :: lower_name
        integer :: i

        found = .false.
        modpath = ''
        lower_name = modname
        do i = 1, len_trim(lower_name)
            if (lower_name(i:i) >= 'A' .and. lower_name(i:i) <= 'Z') then
                lower_name(i:i) = achar(iachar(lower_name(i:i)) + 32)
            end if
        end do

        do i = 1, n_dep_includes
            modpath = trim(dep_includes(i))//'/'//trim(lower_name)//'.mod'
            inquire (file=trim(modpath), exist=found)
            if (found) return
        end do
    end subroutine find_dep_mod_file

    subroutine load_mod_keys(mod_dir, dag, n_order, order, keys)
        character(len=*), intent(in) :: mod_dir
        type(dag_t), intent(in) :: dag
        integer, intent(in) :: n_order, order(n_order)
        character(len=HASH_LEN), intent(out) :: keys(MAX_NODES)

        character(len=512) :: hashfile
        character(len=MAX_NAME) :: label
        character(len=HASH_LEN) :: key
        integer :: u, ios, i, node_id

        keys = ''
        hashfile = trim(mod_dir)//'/mod_hashes.dat'
        open (newunit=u, file=hashfile, status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, *, iostat=ios) label, key
            if (ios /= 0) exit
            if (len_trim(label) == 0) cycle
            do i = 1, n_order
                node_id = order(i)
                if (trim(dag%nodes(node_id)%label) == trim(label)) then
                    keys(node_id) = trim(key)
                    exit
                end if
            end do
        end do
        close (u)
    end subroutine load_mod_keys

    subroutine save_mod_keys(mod_dir, dag, n_order, order, keys)
        character(len=*), intent(in) :: mod_dir
        type(dag_t), intent(in) :: dag
        integer, intent(in) :: n_order, order(n_order)
        character(len=HASH_LEN), intent(in) :: keys(MAX_NODES)

        character(len=512) :: hashfile
        integer :: u, ios, i, node_id

        hashfile = trim(mod_dir)//'/mod_hashes.dat'
        open (newunit=u, file=hashfile, status='replace', iostat=ios)
        if (ios /= 0) return
        do i = 1, n_order
            node_id = order(i)
            if (len_trim(keys(node_id)) == 0) cycle
            write (u, '(a,1x,a)') trim(dag%nodes(node_id)%label), trim(keys(node_id))
        end do
        close (u)
    end subroutine save_mod_keys

    subroutine get_mod_key(label, mod_dir, key, smod_name)
        character(len=*), intent(in) :: label, mod_dir
        character(len=HASH_LEN), intent(out) :: key
        character(len=*), intent(in), optional :: smod_name

        character(len=MAX_NAME) :: lower_label
        character(len=512) :: modpath
        character(len=HASH_LEN) :: smod_key
        integer :: i

        lower_label = label
        do i = 1, len_trim(lower_label)
            if (lower_label(i:i) >= 'A' .and. lower_label(i:i) <= 'Z') &
                lower_label(i:i) = achar(iachar(lower_label(i:i)) + 32)
        end do

        modpath = trim(mod_dir)//'/'//trim(lower_label)//'.mod'
        call hash_mod_file(modpath, key)
        if (.not. present(smod_name)) return
        if (len_trim(smod_name) == 0) return
        call hash_mod_file(trim(mod_dir)//'/'//trim(smod_name)//'.smod', smod_key)
        if (len_trim(smod_key) == 0) then
            key = ''
        else if (len_trim(key) == 0) then
            key = smod_key
        else
            key = cache_digest([key, smod_key], 2)
        end if
    end subroutine get_mod_key

    subroutine remove_shadow_mods(project_dir, dag)
        character(len=*), intent(in) :: project_dir
        type(dag_t), intent(in) :: dag
        character(len=MAX_NAME) :: lower_label
        integer :: i, j

        do i = 1, dag%n_nodes
            lower_label = dag%nodes(i)%label
            do j = 1, len_trim(lower_label)
                if (lower_label(j:j) >= 'A' .and. lower_label(j:j) <= 'Z') &
                    lower_label(j:j) = achar(iachar(lower_label(j:j)) + 32)
            end do
            call fs_delete_suffix(trim(project_dir)//'/build/dependencies', &
                trim(lower_label)//'.mod', .true.)
        end do
    end subroutine remove_shadow_mods

    subroutine append_log_file(src, dst)
        character(len=*), intent(in) :: src, dst
        call fs_append_file(trim(src), trim(dst))
        call fs_remove_file(trim(src))
    end subroutine append_log_file

    subroutine append_test_stdout_block(src, dst, name)
        !! Append a failing test's captured output to the master log wrapped in
        !! name-keyed delimiters, so downstream formatting can attribute stdout
        !! to a test regardless of where the TEST_RESULT summary lines land.
        character(len=*), intent(in) :: src, dst, name
        integer :: u, ios

        open (newunit=u, file=trim(dst), position='append', status='unknown', &
            iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') '--- stdout '//trim(name)//' ---'
        close (u)
        call fs_append_file(trim(src), trim(dst))
        open (newunit=u, file=trim(dst), position='append', status='old', &
            iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') '--- end stdout '//trim(name)//' ---'
        close (u)
    end subroutine append_test_stdout_block

    subroutine link_app_binaries(project_dir, config, bin_dir, app_bin_dir, src_objs, &
            n_src_objs, is_prog_arr, dep_objs, n_dep_objs, log_file, exitcode, &
            flags, use_cache)
        character(len=*), intent(in) :: project_dir, bin_dir, app_bin_dir, log_file
        type(fpm_config_t), intent(in) :: config
        character(len=512), intent(in) :: src_objs(MAX_SRC_OBJS)
        integer, intent(in) :: n_src_objs
        logical, intent(in) :: is_prog_arr(MAX_SRC_OBJS)
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache

        integer :: i, n_lib, n_link_inputs, copy_rc
        character(len=512), allocatable :: lib_objs(:)
        character(len=512) :: prog_obj, bin_path, app_bin_path
        character(len=512) :: archive_path, link_inputs(1)
        character(len=128) :: prog_name
        character(len=512) :: link_flags
        type(cache_t) :: c
        integer :: cache_ierr
        character(len=HASH_LEN) :: base_digest
        logical :: allow_cache

        link_flags = ''
        if (present(flags)) link_flags = flags
        exitcode = 0
        ! Switching off auto-discovery must not leave an old app binary
        ! addressable through `fo exec`. Re-materialize the selected targets;
        ! link-cache hits restore them without recompiling. Staging directories
        ! belong to concurrent producers, so only stale executables are removed.
        if (.not. config%auto_executables) then
            call remove_unselected_outputs(config, src_objs, n_src_objs, &
                is_prog_arr, bin_dir)
            call remove_unselected_outputs(config, src_objs, n_src_objs, &
                is_prog_arr, app_bin_dir)
            call fs_make_dir(trim(bin_dir))
        end if
        n_lib = 0
        allocate (lib_objs(MAX_SRC_OBJS))
        do i = 1, n_src_objs
            if (.not. is_prog_arr(i)) then
                n_lib = n_lib + 1
                lib_objs(n_lib) = src_objs(i)
            end if
        end do

        ! Precompute the shared link inputs digest once, then link each program
        ! through the link cache (a relink is skipped on a digest hit).
        call cache_init(c, cache_ierr)
        allow_cache = .true.
        if (present(use_cache)) allow_cache = use_cache
        if (.not. allow_cache) cache_ierr = 1
        base_digest = ''
        if (cache_ierr == 0) call link_base_digest(project_dir, lib_objs, n_lib, dep_objs, &
            n_dep_objs, config%link_libs, config%n_link_libs, base_digest)
        call fs_make_dir(app_bin_dir)
        n_link_inputs = 0
        archive_path = ''
        if (n_lib > 0) then
            call archive_objects(project_dir, lib_objs, n_lib, archive_path, &
                log_file, exitcode, base_digest)
            if (exitcode /= 0) return
            n_link_inputs = 1
            link_inputs(1) = archive_path
        end if

        do i = 1, n_src_objs
            if (.not. is_prog_arr(i)) cycle
            prog_obj = src_objs(i)
            prog_name = app_binary_name(config, prog_obj)
            bin_path = trim(bin_dir)//'/'//trim(prog_name)
            call link_binary(project_dir, prog_obj, link_inputs, n_link_inputs, &
                dep_objs, n_dep_objs, config%link_libs, config%n_link_libs, bin_path, &
                log_file, exitcode, link_flags, c, base_digest)
            if (exitcode /= 0) then
                if (len_trim(base_digest) == 0 .and. len_trim(archive_path) > 0) &
                    call remove_ephemeral_link_artifact(archive_path)
                return
            end if
            app_bin_path = trim(app_bin_dir)//'/'//trim(prog_name)
            call publish_app_copy(bin_path, app_bin_path, copy_rc)
            if (copy_rc /= 0) then
                write (error_unit, '(a)') 'fo: failed to create '//trim(app_bin_path)
                exitcode = 1
                if (len_trim(base_digest) == 0 .and. len_trim(archive_path) > 0) &
                    call remove_ephemeral_link_artifact(archive_path)
                return
            end if
        end do
        if (len_trim(base_digest) == 0 .and. len_trim(archive_path) > 0) &
            call remove_ephemeral_link_artifact(archive_path)
    end subroutine link_app_binaries

    subroutine publish_app_copy(source, final_path, copy_rc)
        !! Publish an app copy by rename from a private stage on the same
        !! filesystem. A running app binary keeps its inode, and readers of
        !! final_path see the old or the new complete image, never a partial copy.
        character(len=*), intent(in) :: source, final_path
        integer, intent(out) :: copy_rc

        character(len=1024) :: stage_dir, staged_output
        integer :: reserve_rc
        logical :: valid_image

        call reserve_link_stage(final_path, stage_dir, staged_output, reserve_rc)
        if (reserve_rc /= 0) then
            copy_rc = 1
            return
        end if
        copy_rc = fs_copy_exec(trim(source), trim(staged_output))
        if (copy_rc == 0) then
            call native_image_valid(staged_output, valid_image)
            if (.not. valid_image) copy_rc = 1
        end if
        if (copy_rc == 0) copy_rc = fs_rename(trim(staged_output), trim(final_path))
        call fs_remove_tree(trim(stage_dir))
    end subroutine publish_app_copy

    subroutine remove_unselected_outputs(config, src_objs, n_src_objs, &
            is_prog_arr, dir)
        !! Remove published executables in dir that this build does not select.
        !! Selected executables are republished by the link; directories, such as
        !! another producer's staging stage, are never listed and never removed.
        type(fpm_config_t), intent(in) :: config
        character(len=512), intent(in) :: src_objs(MAX_SRC_OBJS)
        integer, intent(in) :: n_src_objs
        logical, intent(in) :: is_prog_arr(MAX_SRC_OBJS)
        character(len=*), intent(in) :: dir

        character(len=512), allocatable :: files(:)
        character(len=128) :: selected(MAX_SRC_OBJS)
        character(len=512) :: base
        integer :: i, j, n_files, n_selected
        logical :: keep

        n_selected = 0
        do i = 1, n_src_objs
            if (.not. is_prog_arr(i)) cycle
            n_selected = n_selected + 1
            selected(n_selected) = app_binary_name(config, src_objs(i))
        end do
        allocate (files(MAX_SRC_OBJS))
        call fs_collect_files(trim(dir), '', '', '', files, n_files, &
            recursive=.false.)
        do i = 1, n_files
            call file_basename(files(i), base)
            keep = .false.
            do j = 1, n_selected
                if (trim(base) == trim(selected(j))) keep = .true.
            end do
            if (.not. keep) call fs_remove_file(trim(files(i)))
        end do
    end subroutine remove_unselected_outputs

    subroutine link_base_digest(project_dir, lib_objs, n_lib, dep_objs, n_dep, link_libs, &
            n_link, digest)
        !! One digest standing for every shared link input: the library objects,
        !! the dependency objects, and the resolved external archives (hashed by
        !! content so a rebuilt liric.a relinks). Computed once per build; each
        !! per-program link folds in only its own object, so the link key is
        !! cheap and correct.
        character(len=*), intent(in) :: project_dir
        character(len=512), intent(in) :: lib_objs(:)
        integer, intent(in) :: n_lib
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep
        character(len=128), intent(in) :: link_libs(*)
        integer, intent(in) :: n_link
        character(len=HASH_LEN), intent(out) :: digest

        character(len=HASH_LEN) :: lib_hash, h
        character(len=1024) :: token
        character(len=512) :: tmpfile, dirs(2*MAX_DEP_DIRS)
        integer :: n_dirs, n_env, i, u, ios
        logical :: exists

        digest = ''
        call hash_lib_objs(lib_objs, n_lib, lib_hash)
        call make_tmpfile('fo_link_base', tmpfile)
        open (newunit=u, file=trim(tmpfile), status='replace', iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') trim(lib_hash)
        do i = 1, n_dep
            call hash_mod_file(dep_objs(i), h)
            write (u, '(a)') trim(h)//' '//trim(dep_objs(i))
        end do
        call build_lib_search_dirs(dirs, n_dirs, n_env)
        do i = 1, n_link
            call resolve_link_token(project_dir, trim(link_libs(i)), dirs, n_dirs, n_env, &
                token)
            inquire (file=trim(token), exist=exists)
            if (exists) then
                call hash_mod_file(trim(token), h)
                write (u, '(a)') trim(h)//' '//trim(token)
            else
                write (u, '(a)') trim(token)
            end if
        end do
        close (u)
        call hash_mod_file(tmpfile, digest)
        call delete_tmpfile(tmpfile)
    end subroutine link_base_digest

    subroutine scan_project_test_units(project_dir, config, units, n_units, ierr, &
            use_cached)
        !! Explicit test roots may be nested under test/ or anywhere in the
        !! project. Keep each source once when declared roots overlap.
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(in) :: config
        type(scan_unit_t), allocatable, intent(out) :: units(:)
        integer, intent(out) :: n_units, ierr
        logical, intent(in), optional :: use_cached
        logical :: cached
        type(scan_unit_t), allocatable :: found(:), merged(:)
        character(len=MAX_PATH) :: root
        integer :: d, i, j, n_found, n_new
        integer(c_long_long) :: device, inode
        logical :: root_exists

        cached = .false.
        if (present(use_cached)) cached = use_cached
        allocate (units(0))
        n_units = 0
        ierr = 0
        do d = 0, config%n_tests
            if (d == 0) then
                if (.not. config%auto_tests) cycle
                root = config%test_dir
                ! An absent automatic test directory means the project has no
                ! automatic tests; only declared test roots must exist.
                call fs_identity(trim(project_dir)//'/'//trim(root), device, inode, &
                    root_exists)
                if (.not. root_exists) cycle
            else
                root = config%tests(d)%source_dir
                do j = 1, d - 1
                    if (trim(config%tests(j)%source_dir) == trim(root)) exit
                end do
                if (j < d) cycle
                if (config%auto_tests) then
                    if (trim(root) == trim(config%test_dir)) cycle
                end if
            end if
            if (cached) then
                call scan_dir_cached(trim(project_dir)//'/'//trim(root), &
                    found, n_found, ierr)
            else
                call scan_dir(trim(project_dir)//'/'//trim(root), found, n_found, ierr)
            end if
            if (ierr /= 0) return
            allocate (merged(n_units + n_found))
            merged(:n_units) = units(:n_units)
            n_new = n_units
            do i = 1, n_found
                do j = 1, n_new
                    if (trim(found(i)%filename) == trim(merged(j)%filename)) exit
                end do
                if (j <= n_new) cycle
                n_new = n_new + 1
                merged(n_new) = found(i)
            end do
            n_units = n_new
            call move_alloc(merged, units)
        end do
    end subroutine scan_project_test_units

    function gfortran_test_source_name(config, test_dir, source) result(name)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: test_dir, source
        character(len=MAX_PATH) :: name, declared_source
        integer :: i

        name = ''
        do i = 1, config%n_tests
            declared_source = trim(config%project_dir)//'/'// &
                trim(config%tests(i)%source_dir)//'/'//trim(config%tests(i)%main)
            if (trim(source) /= trim(declared_source)) cycle
            name = config%tests(i)%name
            return
        end do
        if (.not. config%auto_tests) return
        if (.not. source_is_in_dir(source, config%project_dir, test_dir)) return
        call file_basename(source, name)
    end function gfortran_test_source_name

    function gfortran_app_source_name(config, source) result(name)
        !! Classify a selected program using the same naming rules as linking.
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: source
        character(len=MAX_PATH) :: name
        character(len=512) :: obj_path

        name = ''
        if (.not. app_program_selected(source, config%project_dir, &
            config%app_dir, config)) return
        call make_obj_path(source, config%project_dir, '', obj_path)
        name = app_binary_name(config, obj_path)
    end function gfortran_app_source_name

    function app_binary_name(config, obj_path) result(name)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: obj_path
        character(len=MAX_PATH) :: name, manifest_name, object_name
        logical :: is_example

        call file_basename(obj_path, object_name)
        is_example = index(trim(object_name), trim(config%example_dir)//'_') == 1
        if (is_example) then
            call app_prog_stem(obj_path, config%example_dir, name)
            manifest_name = manifest_example_name(config, config%example_dir, name)
            if (len_trim(manifest_name) > 0) name = trim(manifest_name)
        else
            call app_prog_stem(obj_path, config%app_dir, name)
            manifest_name = manifest_exe_name(config, config%app_dir, name)
            if (len_trim(manifest_name) > 0) then
                name = trim(manifest_name)
            else if (name == 'main' .and. len_trim(config%name) > 0) then
                name = trim(config%name)
            end if
        end if
    end function app_binary_name

    subroutine app_prog_stem(obj_path, app_dir, stem)
        !! Source stem of an app program from its object path. Object names
        !! encode the project-relative source path with '/'->'_' and a trailing
        !! '.o' (e.g. app/main.f90 -> app_main.f90.o). Strip the object suffix,
        !! the .f90/.F90 extension, and the leading "<app_dir>_" prefix so that
        !! app_main.f90.o -> main and app_span_mismatch_sub.f90.o ->
        !! span_mismatch_sub.
        character(len=*), intent(in) :: obj_path, app_dir
        character(len=*), intent(out) :: stem
        character(len=512) :: base
        character(len=128) :: prefix
        integer :: dot, middle, plen

        call file_basename(obj_path, base) ! drops dir and trailing '.o'
        dot = index(trim(base), '.', back=.true.)
        if (dot > 1) base = base(1:dot - 1) ! drop .f90 / .F90
        prefix = trim(app_dir)//'_'
        plen = len_trim(prefix)
        if (len_trim(base) > plen .and. base(1:plen) == trim(prefix)) &
            base = base(plen + 1:)
        ! FPM's conventional nested layout is example/name/name.f90. The
        ! object-path encoding flattens both path components with underscores,
        ! yielding name_name; recover the public source stem when the two
        ! halves are exactly equal.
        middle = (len_trim(base) + 1)/2
        if (middle > 1) then
            if (mod(len_trim(base), 2) == 1) then
                if (base(middle:middle) == '_') then
                    if (base(:middle - 1) == &
                        base(middle + 1:len_trim(base))) &
                        base = base(:middle - 1)
                end if
            end if
        end if
        stem = trim(base)
    end subroutine app_prog_stem

    subroutine compile_and_run_tests(project_dir, test_dir, mod_dir, obj_dir, &
            bin_dir, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs, lib_objs, n_lib_objs, &
            link_libs, n_link_libs, log_file, &
            selected_names, n_selected, include_slow, &
            exitcode, n_compiled, flags, build_only, use_cache)
        character(len=*), intent(in) :: project_dir, test_dir, mod_dir
        character(len=*), intent(in) :: obj_dir, bin_dir, log_file
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(in) :: n_dep_includes
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs
        character(len=512), intent(in) :: lib_objs(:)
        integer, intent(in) :: n_lib_objs
        character(len=128), intent(in) :: link_libs(*)
        integer, intent(in) :: n_link_libs
        character(len=*), intent(in) :: selected_names(:)
        integer, intent(in) :: n_selected
        logical, intent(in) :: include_slow
        integer, intent(out) :: exitcode
        integer, intent(out), optional :: n_compiled
        character(len=*), intent(in), optional :: flags
        ! build_only: compile and link the test binaries but do not run them.
        logical, intent(in), optional :: build_only
        logical, intent(in), optional :: use_cache

        type(scan_unit_t), allocatable :: tunits(:)
        integer :: n_tests, i, ierr, node_id, n_run, team_size
        type(dag_t) :: dag
        character(len=MAX_PATH), allocatable :: filenames(:)
        logical, allocatable :: is_prog(:), is_test_arr(:)
        integer, allocatable :: topo_order(:)
        integer, allocatable :: run_nodes(:), run_exits(:), run_build_indices(:)
        character(len=512), allocatable :: run_logs(:)
        character(len=512), allocatable :: run_bins(:)
        character(len=HASH_LEN), allocatable :: run_keys(:)
        character(len=MAX_PATH), allocatable :: run_names(:)
        character(len=4096), allocatable :: run_args(:)
        logical, allocatable :: run_compiled(:)
        logical, allocatable :: ran(:), flaky(:)
        real, allocatable :: run_secs(:), run_cpu(:)
        integer :: test_timeout, test_warn
        integer(8) :: clk0, clk1, clk_rate
        integer :: n_order
        logical :: has_cycle, restored, bonly
        logical :: allow_cache
        character(len=512) :: obj_path, bin_path
        character(len=4096) :: incl_flag, helper_includes
        character(len=MAX_PATH) :: tname
        character(len=MAX_PATH) :: fname_local
        character(len=512) :: log_local, rerun_log
        type(cache_t) :: c
        integer :: cache_ierr
        character(len=256) :: compiler
        character(len=HASH_LEN), allocatable :: dep_keys(:)
        character(len=HASH_LEN) :: output_key, lib_hash
        character(len=HASH_LEN) :: link_base
        integer :: n_dep, n_test_includes
        character(len=512) :: test_includes(MAX_DEP_DIRS)
        character(len=1024) :: test_flags
        integer :: dnode
        character(len=:), allocatable :: test_key_flags
        character(len=512), allocatable :: helper_objs(:)
        character(len=512), allocatable :: all_lib_objs(:), link_inputs(:)
        integer :: n_helper_objs, n_all_lib, n_link_inputs, n_link_dep_objs
        character(len=512) :: test_dep_objs(MAX_DEP_OBJS)
        integer :: n_test_dep_objs
        character(len=512) :: archive_path
        character(len=MAX_PATH) :: execution_cwd
        logical :: in_lib
        type(resolved_src_t) :: devsrcs(MAX_RESOLVED)
        character(len=4096), allocatable :: helper_flags(:)
        integer :: n_dev, d, nud
        type(scan_unit_t), allocatable :: udev(:)
        type(fpm_config_t), allocatable :: manifest_config

        bonly = .false.
        if (present(build_only)) bonly = build_only
        execution_cwd = project_dir
        if (.not. bonly) then
            execution_cwd = test_execution_cwd(project_dir)
            if (len_trim(execution_cwd) == 0) then
                exitcode = 1
                return
            end if
            call prepare_test_app_outputs(project_dir, execution_cwd, bin_dir, &
                exitcode)
            if (exitcode /= 0) return
        end if
        test_flags = ''
        if (present(flags)) test_flags = flags
        call append_array_temporary_warning_flag(fc_command(), test_flags)
        exitcode = 0
        if (present(n_compiled)) n_compiled = 0
        allocate (filenames(MAX_NODES), is_prog(MAX_NODES), is_test_arr(MAX_NODES))
        allocate (topo_order(MAX_NODES))
        allocate (run_nodes(MAX_NODES), run_exits(MAX_NODES), run_logs(MAX_NODES))
        allocate (run_build_indices(MAX_NODES), run_bins(MAX_NODES))
        allocate (run_keys(MAX_NODES))
        allocate (run_names(MAX_NODES), run_args(MAX_NODES))
        allocate (run_compiled(MAX_NODES), run_secs(MAX_NODES), run_cpu(MAX_NODES))
        allocate (ran(MAX_NODES), flaky(MAX_NODES))
        ran = .false.
        flaky = .false.
        run_secs = 0.0
        run_cpu = -1.0
        call fpm_config_allocate(manifest_config)
        call fpm_config_parse(project_dir, manifest_config, ierr)
        if (ierr /= 0) return
        call scan_project_test_units(project_dir, manifest_config, &
            tunits, n_tests, ierr)
        if (ierr /= 0) then
            exitcode = 1
            return
        end if
        if (n_tests == 0) return
        test_timeout = test_timeout_seconds(manifest_config)
        test_warn = test_warn_seconds(test_timeout)

        call resolve_dev_dep_srcs(project_dir, devsrcs, n_dev, ierr)
        if (ierr /= 0) then
            exitcode = 1
            return
        end if
        do d = 1, n_dev
            call scan_dir(trim(devsrcs(d)%src_dir), udev, nud, ierr)
            if (ierr /= 0) then
                exitcode = 1
                return
            end if
            call validate_module_naming(manifest_config, udev(:nud), devsrcs, &
                n_dev, exitcode)
            if (exitcode /= 0) return
            call append_module_units(tunits, n_tests, udev, nud)
        end do

        call build_dag_from_units(tunits, n_tests, dag, filenames, is_test_arr, is_prog)
        call dag_topo_sort(dag, topo_order, n_order, has_cycle)
        call make_includes_flag(mod_dir, dep_includes, n_dep_includes, incl_flag)
        helper_includes = incl_flag
        if (len_trim(test_flags) > 0) then
            incl_flag = with_user_flags(incl_flag, test_flags)
        end if
        call cache_init(c, cache_ierr)
        allow_cache = .true.
        if (present(use_cache)) allow_cache = use_cache
        if (.not. allow_cache) cache_ierr = 1
        call detect_compiler(compiler)

        ! Select the test programs to run first, so helper compilation and the
        ! link line are scoped to exactly the tests' dependency closure (as fpm
        ! does) and never pull in an unrelated, possibly broken, test module.
        n_run = 0
        dnode = 0
        if (len_trim(manifest_config%dispatcher) > 0) then
            dnode = dispatcher_node(filenames, is_prog, topo_order, n_order, &
                manifest_config, test_dir)
        end if
        do i = 1, n_order
            node_id = topo_order(i)
            if (len_trim(filenames(node_id)) == 0) cycle
            if (.not. test_source_eligible(filenames(node_id), &
                is_prog(node_id), dnode > 0, .true.)) cycle
            tname = gfortran_test_source_name(manifest_config, test_dir, &
                filenames(node_id))
            if (len_trim(tname) == 0) cycle
            if (.not. include_slow .and. is_slow_name(tname)) cycle
            if (trim(tname) == trim(manifest_config%dispatcher)) then
                if (.not. selected_test(tname, selected_names, n_selected)) cycle
            end if
            if (n_selected > 0 .and. .not. selected_test(tname, selected_names, &
                n_selected)) cycle
            n_run = n_run + 1
            run_nodes(n_run) = node_id
            run_names(n_run) = tname
            run_args(n_run) = manifest_test_args(manifest_config, tname)
            run_bins(n_run) = trim(bin_dir)//'/'//trim(tname)
            if (dnode > 0 .and. &
                tname /= trim(manifest_config%dispatcher) .and. &
                source_has_marker(filenames(node_id), '! fo: dispatcher')) then
                ! Build the dispatcher once and run it with this test's name.
                ! `run_names` keeps the original name, so `fo` still reports
                ! every test by name; pointing the node at the dispatcher is
                ! what stops the library from being recompiled and relinked per
                ! test. Without the remap the link step feeds this test's own
                ! objects to the dispatcher path, one binary per test anyway.
                run_nodes(n_run) = dnode
                run_args(n_run) = tname
                run_bins(n_run) = trim(bin_dir)//'/'//trim(manifest_config%dispatcher)
            end if
            run_build_indices(n_run) = n_run
            do d = 1, n_run - 1
                if (run_nodes(d) /= run_nodes(n_run)) cycle
                run_build_indices(n_run) = d
                exit
            end do
            call make_tmpfile('fo_test_case', run_logs(n_run))
        end do

        ! Module-only files under test/ are helper modules a test program uses
        ! (fpm compiles them and links their objects into the test executable).
        ! Compile only those reachable from a selected test, in dependency order,
        ! so their .mod files exist before the test programs compile, then fold
        ! their objects into the link line.
        allocate (helper_objs(MAX_SRC_OBJS), helper_flags(MAX_NODES))
        call package_source_flags(filenames, test_flags, manifest_config, &
            devsrcs, n_dev, helper_flags)
        do i = 1, size(helper_flags)
            helper_flags(i) = with_user_flags(helper_includes, &
                helper_flags(i))
        end do
        call compile_test_helpers(project_dir, obj_dir, dag, filenames, is_prog, &
            topo_order, n_order, run_nodes, n_run, incl_flag, log_file, &
            helper_objs, n_helper_objs, exitcode, helper_flags)
        if (exitcode /= 0) return

        test_dep_objs = dep_objs
        n_test_dep_objs = n_dep_objs
        call compile_test_dev_c_sources(devsrcs, n_dev, project_dir, obj_dir, &
            log_file, test_dep_objs, n_test_dep_objs, exitcode)
        if (exitcode /= 0) return

        allocate (all_lib_objs(MAX_SRC_OBJS))
        n_all_lib = 0
        do i = 1, n_lib_objs
            if (n_all_lib >= MAX_SRC_OBJS) exit
            n_all_lib = n_all_lib + 1
            all_lib_objs(n_all_lib) = lib_objs(i)
        end do
        do i = 1, n_helper_objs
            if (n_all_lib >= MAX_SRC_OBJS) exit
            in_lib = .false.
            do d = 1, n_lib_objs
                if (trim(helper_objs(i)) == trim(lib_objs(d))) then
                    in_lib = .true.
                    exit
                end if
            end do
            if (in_lib) cycle
            n_all_lib = n_all_lib + 1
            all_lib_objs(n_all_lib) = helper_objs(i)
        end do
        test_includes = ''
        test_includes(1) = trim(mod_dir)
        n_test_includes = 1
        do i = 1, min(n_dep_includes, MAX_DEP_DIRS - 1)
            n_test_includes = n_test_includes + 1
            test_includes(i + 1) = dep_includes(i)
        end do

        ! A test's pass/fail depends on the IMPLEMENTATION of the whole library
        ! it links, not just the interfaces it uses. The .mod (interface) hashes
        ! from add_external_dep_keys miss private-body changes, so a cached pass
        ! could survive a behaviour change in a dependency. Fold a hash of the
        ! linked library objects into every test key so any implementation
        ! change invalidates the cached result.
        call hash_lib_objs(all_lib_objs, n_all_lib, lib_hash)
        link_base = ''
        if (cache_ierr == 0) call link_base_digest(project_dir, all_lib_objs, n_all_lib, &
            test_dep_objs, n_test_dep_objs, link_libs, n_link_libs, link_base)
        allocate (link_inputs(MAX_SRC_OBJS))
        link_inputs = ''
        n_link_inputs = 0
        n_link_dep_objs = n_test_dep_objs
        archive_path = ''
        if (n_all_lib > 0) then
            ! `link = "shared"` folds the library once into a .so and lets
            ! every test executable link against that, instead of relinking the
            ! whole static archive ~500 times. Measured: a test that links the
            ! archive comes out 15.6 MB and pays the archive scan; the same
            ! test against the .so links in 0.109 s at 2.1 MB, and runs.
            if (manifest_config%link_shared) then
                call shared_library(project_dir, all_lib_objs, n_all_lib, &
                    archive_path, log_file, exitcode, test_dep_objs, n_test_dep_objs, &
                    link_libs, n_link_libs, test_flags, link_base)
                if (is_macos()) n_link_dep_objs = 0
            else
                call archive_objects(project_dir, all_lib_objs, n_all_lib, &
                    archive_path, log_file, exitcode, link_base)
            end if
            if (exitcode /= 0) return
            n_link_inputs = 1
            link_inputs(1) = archive_path
            if (manifest_config%link_shared) then
                ! An executable that links a .so has to find it at run time.
                ! RPATH on the binary means `fo test` needs no exported
                ! LD_LIBRARY_PATH, which the suite runner does not set and
                ! should not have to.
                test_flags = trim(test_flags)//' -Wl,-rpath,'// &
                    trim(project_dir)//'/build/fo/lib'
            end if
        end if

        do i = 1, n_run
            if (run_build_indices(i) /= i) cycle
            node_id = run_nodes(i)
            n_dep = 0
            call add_external_dep_keys(tunits, n_tests, filenames(node_id), &
                test_includes, n_test_includes, &
                dep_keys, n_dep)
            if (len_trim(lib_hash) > 0) then
                n_dep = n_dep + 1
                dep_keys(n_dep) = lib_hash
            end if
            test_key_flags = compile_key_flags(test_flags)
            if (len_trim(link_base) > 0) test_key_flags = &
                trim(test_key_flags)//new_line('a')//'test-link:'//trim(link_base)
            if (len_trim(run_args(i)) > 0) then
                test_key_flags = trim(test_key_flags)//new_line('a')// &
                    'test-args:'//trim(run_args(i))
            end if
            run_keys(i) = cache_key_for(filenames(node_id), compiler, &
                test_key_flags, dep_keys, n_dep, &
                include_dirs=dep_includes(:n_dep_includes))
        end do

        run_exits = 0
        run_compiled = .false.
        if (bonly) then
            call progress_begin('build tests', n_run)
        else
            call progress_begin('test', n_run)
        end if
        team_size = max(1, min(n_run, native_jobs()))
        !$omp parallel do if(n_run > 1) num_threads(team_size) schedule(dynamic) &
        !$omp& private(node_id, fname_local, &
        !$omp& obj_path, bin_path, tname, log_local, restored, clk0, clk1, &
        !$omp& clk_rate)
        do i = 1, n_run
            if (run_build_indices(i) /= i) cycle
            node_id = run_nodes(i)
            fname_local = filenames(node_id)
            log_local = run_logs(i)
            call make_obj_path(fname_local, project_dir, obj_dir, obj_path)
            restored = .false.
            if (cache_ierr == 0) then
                call cache_restore_action(c, run_keys(i), obj_path, mod_dir, &
                    restored)
            end if
            if (.not. restored) then
                call compile_f90(project_dir, fname_local, obj_path, incl_flag, &
                    log_local, run_exits(i))
                run_compiled(i) = run_exits(i) == 0
            end if
            if (run_exits(i) == 0) then
                bin_path = run_bins(i)
                call link_binary(project_dir, obj_path, link_inputs, n_link_inputs, &
                    test_dep_objs, n_link_dep_objs, link_libs, n_link_libs, bin_path, &
                    log_local, run_exits(i), test_flags, c, link_base)
            end if
        end do
        !$omp end parallel do

        ! All cases sharing a dispatcher wait for its single compile and link.
        ! Compiling into the same object or replacing a running binary in
        ! parallel corrupts the build and can execute another case's main.
        do i = 1, n_run
            if (run_build_indices(i) == i) cycle
            run_exits(i) = run_exits(run_build_indices(i))
        end do
        !$omp parallel do if(n_run > 1) num_threads(team_size) schedule(dynamic) &
        !$omp& private(bin_path, log_local, clk0, clk1, clk_rate)
        do i = 1, n_run
            bin_path = run_bins(i)
            log_local = run_logs(i)
            if (run_exits(i) == 0 .and. .not. bonly) then
                ran(i) = .true.
                call system_clock(clk0, clk_rate)
                ! Run from the project root, not from whatever directory fo was
                ! invoked from, so a test that opens a project-relative path
                ! behaves the same under the CLI and under the MCP server.
                call run_test_binary(execution_cwd, bin_path, &
                    run_args(i), log_local, &
                    manifest_config, is_slow_name(run_names(i)), run_exits(i), &
                    run_cpu(i))
                call system_clock(clk1)
                if (clk_rate > 0) run_secs(i) = real(clk1 - clk0) / real(clk_rate)
            end if
            call progress_step()
        end do
        !$omp end parallel do
        call progress_end()
        if (len_trim(link_base) == 0 .and. len_trim(archive_path) > 0) &
            call remove_ephemeral_link_artifact(archive_path)

        ! Flaky-test diagnosis: a test that compiled and ran but failed may have
        ! lost a race with a concurrently-running test (shared /tmp path, shared
        ! global state). Re-run each such failure once, serially and in isolation.
        ! If it now passes it was flaky: report it as a likely parallel-race
        ! candidate and do not fail the build on it. A genuine failure fails both
        ! times and is kept.
        if (.not. bonly) then
            do i = 1, n_run
                if (.not. ran(i)) cycle
                if (run_exits(i) == 0 .or. run_exits(i) == 124) cycle
                tname = run_names(i)
                bin_path = run_bins(i)
                call make_tmpfile('fo_test_rerun', rerun_log)
                call run_test_binary(execution_cwd, bin_path, &
                    run_args(i), rerun_log, manifest_config, is_slow_name(tname), &
                    run_exits(i))
                call delete_tmpfile(rerun_log)
                if (run_exits(i) == 0) flaky(i) = .true.
            end do
        end if

        do i = 1, n_run
            tname = run_names(i)
            if (run_exits(i) /= 0) then
                call append_test_stdout_block(run_logs(i), log_file, tname)
                if (run_exits(i) == 124) then
                    call append_timeout_status(log_file, tname)
                else
                    call append_test_status(log_file, tname, run_exits(i))
                end if
                if (exitcode == 0) exitcode = run_exits(i)
            else
                call append_log_file(trim(run_logs(i)), log_file)
                if (cache_ierr == 0 .and. run_compiled(i)) then
                    call make_obj_path(filenames(run_nodes(i)), project_dir, &
                        obj_dir, obj_path)
                    call cache_store_action(c, run_keys(i), obj_path, mod_dir, &
                        '', output_key, cache_ierr)
                    if (present(n_compiled)) n_compiled = n_compiled + 1
                end if
            end if
            call delete_tmpfile(run_logs(i))
        end do

        do i = 1, n_run
            if (flaky(i)) then
                tname = run_names(i)
                call append_flaky_status(log_file, tname)
            end if
        end do

        ! Structured TEST_RESULT lines for downstream consumers (CLI, MCP, agents)
        if (.not. bonly) then
            do i = 1, n_run
                tname = run_names(i)
                if (flaky(i)) then
                    call write_test_result_line(log_file, tname, 'FLAKY', '-', &
                        run_secs(i))
                else if (ran(i) .and. run_exits(i) == 124) then
                    call write_test_result_line(log_file, tname, 'TIMEOUT', '124', &
                        run_secs(i))
                else if (ran(i) .and. run_exits(i) == 0) then
                    call write_test_result_line(log_file, tname, 'PASS', '-', &
                        run_secs(i))
                else if (ran(i)) then
                    block
                        character(len=8) :: exit_str
                        write (exit_str, '(i0)') run_exits(i)
                        call write_test_result_line(log_file, tname, 'FAIL', &
                            trim(exit_str), run_secs(i))
                    end block
                else if (run_exits(i) /= 0) then
                    block
                        character(len=8) :: exit_str
                        write (exit_str, '(i0)') run_exits(i)
                        call write_test_result_line(log_file, tname, 'FAIL', &
                            trim(exit_str), 0.0)
                    end block
                else if (run_compiled(i)) then
                    call write_test_result_line(log_file, tname, 'SKIP', '-', 0.0)
                end if
            end do
        end if

        if (.not. bonly) &
            call warn_slow_tests(filenames, run_nodes, run_exits, run_secs, run_cpu, &
            n_run, test_warn, log_file)
    end subroutine compile_and_run_tests

    subroutine run_test_binary(execution_cwd, bin_path, arg_lines, log_file, &
            config, slow, exitcode, cpu_seconds)
        !! Run one test binary under its CPU budget and wall-clock cap (see
        !! fo_test_budget). A timeout returns 124 and appends a line naming the
        !! limit that fired to the test's own log. cpu_seconds is the CPU time
        !! the test used, negative when unknown.
        character(len=*), intent(in) :: execution_cwd, bin_path, arg_lines, log_file
        type(fpm_config_t), intent(in) :: config
        logical, intent(in) :: slow
        integer, intent(out) :: exitcode
        real, intent(out), optional :: cpu_seconds

        character(len=:), allocatable :: packed
        character(len=512) :: test_tmp
        integer :: n_args, budget, wall_cap, kind, u, ios
        real :: cpu_s, wall_s

        packed = ''
        n_args = 0
        call argv_push(packed, n_args, bin_path)
        call argv_push_split_nl(packed, n_args, arg_lines)
        budget = test_budget_seconds(config, slow)
        wall_cap = test_wall_cap_seconds(config, budget)
        ! A private TMPDIR per run: scratch the test leaves behind, including
        ! logs nested fo commands keep for failures, is removed when the run
        ! passes and kept as evidence when it fails.
        call make_tmpfile('fo-test-tmp', test_tmp)
        call fs_make_dir(trim(test_tmp))
        call process_run_argv_logged(execution_cwd, packed, n_args, log_file, .true., &
            wall_cap, exitcode, env_extra='TMPDIR='//trim(test_tmp), &
            cpu_budget_s=budget, timeout_kind=kind, cpu_seconds=cpu_s, &
            wall_seconds=wall_s)
        if (exitcode == 0) call fs_remove_tree(trim(test_tmp))
        if (present(cpu_seconds)) cpu_seconds = cpu_s
        if (exitcode /= 124) return
        open (newunit=u, file=trim(log_file), position='append', &
            status='unknown', action='write', iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') 'fo: test killed: '// &
            timeout_detail(kind, budget, wall_cap, cpu_s, wall_s)
        close (u)
    end subroutine run_test_binary

    subroutine shared_library(project_dir, objects, n_objects, so_path, &
            log_file, exitcode, dep_objs, n_dep_objs, link_libs, n_link_libs, &
            flags, content_key)
        !! Fold the library objects into one shared object instead of a static
        !! archive, so ~500 test executables link against a single 20 MB `.so`
        !! rather than relinking a ~30 MB archive each - measured 0.109 s and
        !! 2.1 MB per test against the archive's 15.6 MB, and it runs.
        !! The name follows the project directory so nothing here hard-codes
        !! ffc. Objects must be position-independent; if they are not, the
        !! linker says so in the log and this returns an empty path.
        character(len=*), intent(in) :: project_dir, log_file
        character(len=512), intent(in) :: objects(:)
        integer, intent(in) :: n_objects
        character(len=*), intent(out) :: so_path
        integer, intent(out) :: exitcode
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs, n_link_libs
        character(len=128), intent(in) :: link_libs(*)
        character(len=*), intent(in) :: flags
        character(len=*), intent(in), optional :: content_key

        character(len=:), allocatable :: packed
        character(len=512) :: lib_dir, proj, install_name, member
        character(len=6) :: extension
        character(len=4096) :: shared_keys(7)
        character(len=512) :: compiler
        character(len=HASH_LEN) :: shared_key, shared_action
        character(len=1024) :: final_path, owner_dir, stage_so
        type(cache_t) :: shared_cache
        integer :: i, n_args, reserve_rc, rename_rc, cache_rc, store_rc
        logical :: exists, valid

        so_path = ''
        exitcode = 0
        if (n_objects < 1) return

        lib_dir = trim(project_dir)//'/build/fo/lib'
        call fs_make_dir(lib_dir)
        call file_basename(project_dir, proj)
        extension = '.so'
        if (is_macos()) extension = '.dylib'
        final_path = ''
        cache_rc = 1
        if (present(content_key)) then
            if (len_trim(content_key) > 0) then
                shared_key = content_key
                if (is_macos()) then
                    shared_keys(1) = content_key
                    shared_keys(2) = fc_executable_command()
                    shared_keys(3) = flags
                    shared_keys(4) = fc_link_policy_flags()
                    shared_keys(5) = 'darwin-dylib-v1'
                    shared_key = cache_digest(shared_keys, 5)
                end if
                shared_keys(1) = 'fo-shared-1'
                shared_keys(2) = shared_key
                shared_keys(3) = fc_executable_command()
                shared_keys(4) = flags
                shared_keys(5) = fc_link_policy_flags()
                shared_keys(6) = trim(proj)//trim(extension)
                call detect_compiler(compiler)
                shared_keys(7) = compiler
                shared_action = cache_digest(shared_keys, 7)
                call cache_init(shared_cache, cache_rc)
                final_path = trim(lib_dir)//'/lib'//trim(proj)//'_'// &
                    shared_key(1:min(32, len_trim(shared_key)))//trim(extension)
                inquire (file=trim(final_path), exist=exists)
                if (exists) then
                    valid = .false.
                    if (cache_rc == 0) call cache_binary_matches(shared_cache, &
                        shared_action, final_path, valid)
                    if (valid) call native_image_valid(final_path, valid)
                    if (valid) then
                        so_path = final_path
                        return
                    end if
                end if
            end if
        end if

        ! Keep each linker's output private until it is complete. The marker
        ! directory is exclusively reserved; the hidden sibling output keeps
        ! the library directory on the consumer's existing runtime search path.
        reserve_rc = 1
        do i = 1, 100
            call make_sibling_tmpfile(trim(lib_dir)//'/.fo-shared', owner_dir)
            reserve_rc = fs_mkdir_excl(trim(owner_dir))
            if (reserve_rc /= 0) then
                if (reserve_rc < 0) exit
                cycle
            end if
            stage_so = trim(owner_dir)//trim(extension)
            inquire (file=trim(stage_so), exist=exists)
            if (.not. exists) exit
            call fs_remove_tree(trim(owner_dir))
            reserve_rc = 1
        end do
        if (reserve_rc /= 0) then
            call append_artifact_error(log_file, &
                'fo: could not reserve shared-library staging path')
            exitcode = 1
            so_path = ''
            return
        end if

        n_args = 0
        packed = ''
        if (is_macos()) then
            ! Darwin requires dependency symbols to be resolved in the dylib.
            ! The Fortran driver also supplies the selected compiler runtime.
            if (len_trim(final_path) > 0) then
                call archive_member_name(final_path, member)
            else
                call archive_member_name(stage_so, member)
            end if
            install_name = '@rpath/'//trim(member)
            call make_link_argv(project_dir, objects(1), objects(2:n_objects), &
                n_objects - 1, dep_objs, n_dep_objs, link_libs, n_link_libs, &
                stage_so, trim(flags)//' -dynamiclib', .false., packed, n_args, &
                install_name)
        else
            call argv_push(packed, n_args, 'gcc')
            call argv_push(packed, n_args, '-shared')
            call argv_push(packed, n_args, '-Wl,--whole-archive')
            do i = 1, n_objects
                call argv_push(packed, n_args, objects(i))
            end do
            call argv_push(packed, n_args, '-Wl,--no-whole-archive')
            call argv_push(packed, n_args, '-o')
            call argv_push(packed, n_args, stage_so)
        end if
        call process_run_argv_logged(project_dir, packed, n_args, log_file, &
            .true., build_timeout_seconds(), exitcode)
        if (exitcode /= 0) then
            call remove_ephemeral_link_artifact(stage_so)
            so_path = ''
            return
        end if
        call native_image_valid(stage_so, valid)
        if (.not. valid) then
            call append_artifact_error(log_file, &
                'fo: linker output is not a valid shared library')
            call remove_ephemeral_link_artifact(stage_so)
            so_path = ''
            exitcode = 1
            return
        end if
        if (len_trim(final_path) > 0) then
            ! Reuse requires the complete bytes recorded from this private
            ! stage, not merely intact image headers on a mutable public path.
            if (cache_rc == 0) call cache_store_binary(shared_cache, &
                shared_action, stage_so, store_rc)
            rename_rc = fs_rename(trim(stage_so), trim(final_path))
            if (rename_rc /= 0) then
                call append_artifact_error(log_file, &
                    'fo: could not atomically publish shared library')
                call remove_ephemeral_link_artifact(stage_so)
                so_path = ''
                exitcode = 1
                return
            end if
            call fs_remove_tree(trim(owner_dir))
            so_path = final_path
        else
            ! The consumer removes this completed process-owned output after
            ! its links and runs finish.
            so_path = stage_so
        end if
    end subroutine shared_library

    subroutine native_image_valid(path, valid)
        character(len=*), intent(in) :: path
        logical, intent(out) :: valid

        integer(int64) :: file_size
        integer :: u, ios, image_class, image_data, header_bytes
        character(len=4) :: magic
        character(len=64) :: elf_header

        valid = .false.
        file_size = 0_int64
        inquire (file=trim(path), size=file_size, iostat=ios)
        if (ios /= 0) return
        if (file_size < 4_int64) return
        open (newunit=u, file=trim(path), access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (u, pos=1, iostat=ios) magic
        if (is_macos()) then
            close (u)
            call macho_image_valid(path, valid)
        else
            if (ios /= 0) then
                close (u)
                return
            end if
            if (magic /= achar(127)//'ELF') then
                close (u)
                return
            end if
            if (file_size < 16_int64) then
                close (u)
                return
            end if
            header_bytes = int(min(file_size, 64_int64))
            read (u, pos=1, iostat=ios) elf_header(1:header_bytes)
            close (u)
            if (ios /= 0) return
            image_class = iachar(elf_header(5:5))
            image_data = iachar(elf_header(6:6))
            if (image_data /= 1 .and. image_data /= 2) return
            if (image_class == 1) then
                valid = file_size >= 52_int64
            else if (image_class == 2) then
                valid = file_size >= 64_int64
            end if
            if (valid) valid = elf_section_table_fits(elf_header(1:header_bytes), &
                image_class, image_data == 1, file_size)
        end if
    end subroutine native_image_valid

    logical function elf_section_table_fits(header, image_class, little, file_size)
        !! A linked ELF image ends with its section header table. A file cut short
        !! by an interrupted or damaged write loses that table, so its intact ELF
        !! prefix does not make it a complete image.
        character(len=*), intent(in) :: header
        integer, intent(in) :: image_class
        logical, intent(in) :: little
        integer(int64), intent(in) :: file_size
        integer(int64) :: sh_off, sh_num, sh_ent

        if (image_class == 2) then
            sh_off = elf_field(header, 40, 8, little)
            sh_ent = elf_field(header, 58, 2, little)
            sh_num = elf_field(header, 60, 2, little)
        else
            sh_off = elf_field(header, 32, 4, little)
            sh_ent = elf_field(header, 46, 2, little)
            sh_num = elf_field(header, 48, 2, little)
        end if
        elf_section_table_fits = .false.
        if (sh_num < 1_int64 .or. sh_off < 0_int64) return
        elf_section_table_fits = sh_off <= file_size - sh_num*sh_ent
    end function elf_section_table_fits

    integer(int64) function elf_field(bytes, offset, width, little)
        !! Unsigned ELF header field of width bytes at a zero-based offset.
        character(len=*), intent(in) :: bytes
        integer, intent(in) :: offset, width
        logical, intent(in) :: little
        integer :: k, at

        elf_field = 0_int64
        do k = 1, width
            if (little) then
                at = offset + width - k
            else
                at = offset + k - 1
            end if
            elf_field = elf_field*256_int64 + int(iachar(bytes(at + 1:at + 1)), int64)
        end do
    end function elf_field

    subroutine archive_objects(project_dir, objects, n_objects, archive_path, &
            log_file, exitcode, content_key)
        character(len=*), intent(in) :: project_dir, log_file
        character(len=512), intent(in) :: objects(:)
        integer, intent(in) :: n_objects
        character(len=*), intent(out) :: archive_path
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: content_key

        character(len=:), allocatable :: packed
        character(len=512) :: archive_dir, final_path
        character(len=1024) :: stage_dir, stage_archive, ready_archive
        integer :: i, n_args, reserve_rc, rename_rc
        logical :: exists, members_ok

        archive_path = ''
        exitcode = 0
        if (n_objects < 1) return
        archive_dir = trim(project_dir)//'/build/fo/lib'
        call fs_make_dir(archive_dir)
        final_path = ''
        if (present(content_key)) then
            if (len_trim(content_key) > 0) then
                final_path = trim(archive_dir)//'/objects_'// &
                    content_key(1:min(32, len_trim(content_key)))//'.a'
                inquire (file=trim(final_path), exist=exists)
                if (exists) then
                    call archive_has_expected_members(project_dir, final_path, &
                        objects, n_objects, members_ok)
                    if (members_ok) then
                        archive_path = final_path
                        return
                    end if
                end if
            end if
        end if

        ! Reserve an exclusive sibling directory so concurrent builders write
        ! separate archives. The stage is on the destination filesystem, making
        ! publication by rename atomic.
        reserve_rc = 1
        do i = 1, 100
            call make_sibling_tmpfile(trim(archive_dir)//'/.fo-archive', stage_dir)
            reserve_rc = fs_mkdir_excl(trim(stage_dir))
            if (reserve_rc == 0) exit
            if (reserve_rc < 0) exit
        end do
        if (reserve_rc /= 0) then
            call append_artifact_error(log_file, &
                'fo: could not reserve archive staging directory')
            exitcode = 1
            return
        end if
        stage_archive = trim(stage_dir)//'/archive.tmp.a'
        ready_archive = trim(stage_dir)//'/archive.a'
        n_args = 0
        call argv_push(packed, n_args, 'ar')
        call argv_push(packed, n_args, 'rcs')
        call argv_push(packed, n_args, trim(stage_archive))
        do i = 1, n_objects
            call argv_push(packed, n_args, objects(i))
        end do
        call process_run_argv_logged(project_dir, packed, n_args, log_file, &
            .true., build_timeout_seconds(), exitcode)
        if (exitcode /= 0) then
            call fs_remove_tree(trim(stage_dir))
            archive_path = ''
            return
        end if
        call archive_has_expected_members(project_dir, stage_archive, objects, &
            n_objects, members_ok)
        if (.not. members_ok) then
            call append_artifact_error(log_file, &
                'fo: archiver output does not contain the expected object members')
            call fs_remove_tree(trim(stage_dir))
            archive_path = ''
            exitcode = 1
            return
        end if
        rename_rc = fs_rename(trim(stage_archive), trim(ready_archive))
        if (rename_rc /= 0) then
            call append_artifact_error(log_file, 'fo: could not publish staged archive')
            call fs_remove_tree(trim(stage_dir))
            archive_path = ''
            exitcode = 1
            return
        end if
        if (len_trim(final_path) > 0) then
            rename_rc = fs_rename(trim(ready_archive), trim(final_path))
            if (rename_rc /= 0) then
                call append_artifact_error(log_file, &
                    'fo: could not publish cached archive')
                call fs_remove_tree(trim(stage_dir))
                archive_path = ''
                exitcode = 1
                return
            end if
            call fs_remove_tree(trim(stage_dir))
            archive_path = final_path
        else
            ! The caller links this completed process-owned archive and removes
            ! its staging directory after the link finishes.
            archive_path = ready_archive
        end if
    end subroutine archive_objects

    subroutine archive_has_expected_members(project_dir, archive_path, objects, &
            n_objects, valid)
        character(len=*), intent(in) :: project_dir, archive_path
        character(len=512), intent(in) :: objects(:)
        integer, intent(in) :: n_objects
        logical, intent(out) :: valid

        character(len=:), allocatable :: packed
        character(len=1024) :: listing_file, member_line
        character(len=512) :: expected
        logical, allocatable :: matched(:)
        integer :: i, n_args, exitcode, u, ios, j
        logical :: member_found, member_content_ok

        valid = .false.
        if (n_objects < 1) return
        call make_tmpfile('fo_archive_members', listing_file)
        n_args = 0
        call argv_push(packed, n_args, 'ar')
        call argv_push(packed, n_args, 't')
        call argv_push(packed, n_args, archive_path)
        call process_run_argv_logged(project_dir, packed, n_args, listing_file, &
            .false., build_timeout_seconds(), exitcode)
        if (exitcode /= 0) then
            call delete_tmpfile(listing_file)
            return
        end if

        allocate (matched(n_objects))
        matched = .false.
        open (newunit=u, file=trim(listing_file), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) then
            call delete_tmpfile(listing_file)
            return
        end if
        valid = .true.
        do
            read (u, '(a)', iostat=ios) member_line
            if (ios /= 0) exit
            if (len_trim(member_line) == 0) cycle
            if (archive_member_is_metadata(member_line)) cycle
            member_found = .false.
            do i = 1, n_objects
                if (matched(i)) cycle
                call archive_member_name(objects(i), expected)
                if (trim(member_line) == trim(expected)) then
                    matched(i) = .true.
                    member_found = .true.
                    exit
                end if
            end do
            if (.not. member_found) then
                valid = .false.
                exit
            end if
        end do
        close (u)
        if (valid) then
            do j = 1, n_objects
                if (.not. matched(j)) then
                    valid = .false.
                    exit
                end if
            end do
        end if
        call delete_tmpfile(listing_file)
        if (.not. valid) return
        do j = 1, n_objects
            call archive_member_content_matches(project_dir, archive_path, &
                objects(j), member_content_ok)
            if (.not. member_content_ok) then
                valid = .false.
                return
            end if
        end do
    end subroutine archive_has_expected_members

    logical function archive_member_is_metadata(member)
        character(len=*), intent(in) :: member

        ! BSD/Apple ar may expose its symbol table as an ordinary-looking
        ! member in `ar -t`. These exact names are archive indexes, not object
        ! inputs. Keep this list narrow: unknown members must still fail the
        ! exact expected-object check above.
        select case (trim(member))
        case ('__.SYMDEF', '__.SYMDEF SORTED', '__.SYMDEF_64', &
                '__.SYMDEF_64 SORTED')
            archive_member_is_metadata = .true.
        case default
            archive_member_is_metadata = .false.
        end select
    end function archive_member_is_metadata

    subroutine archive_member_content_matches(project_dir, archive_path, &
            object_path, matches)
        character(len=*), intent(in) :: project_dir, archive_path, object_path
        logical, intent(out) :: matches

        character(len=:), allocatable :: packed
        character(len=1024) :: member_output
        character(len=512) :: member_name
        character(len=HASH_LEN) :: object_digest, member_digest
        integer :: n_args, exitcode

        matches = .false.
        call archive_member_name(object_path, member_name)
        if (len_trim(member_name) == 0) return
        call make_tmpfile('fo_archive_member', member_output)
        n_args = 0
        call argv_push(packed, n_args, 'ar')
        call argv_push(packed, n_args, 'p')
        call argv_push(packed, n_args, archive_path)
        call argv_push(packed, n_args, trim(member_name))
        call process_run_argv_logged(project_dir, packed, n_args, member_output, &
            .false., build_timeout_seconds(), exitcode)
        if (exitcode == 0) then
            call cache_file_digest(object_path, object_digest)
            call cache_file_digest(member_output, member_digest)
            matches = object_digest == member_digest
        end if
        call delete_tmpfile(member_output)
    end subroutine archive_member_content_matches

    subroutine archive_member_name(path, member)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: member

        integer :: i, n, slash

        member = ''
        n = len_trim(path)
        slash = 0
        do i = 1, n
            if (path(i:i) == '/') slash = i
        end do
        if (slash < n) then
            member = path(slash + 1:n)
        else if (slash == 0) then
            member = path(1:n)
        end if
    end subroutine archive_member_name

    subroutine append_artifact_error(log_file, message)
        character(len=*), intent(in) :: log_file, message
        integer :: u, ios

        if (len_trim(log_file) == 0) return
        open (newunit=u, file=trim(log_file), position='append', status='unknown', &
            action='write', iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') trim(message)
        close (u)
    end subroutine append_artifact_error

    subroutine remove_ephemeral_link_artifact(artifact_path)
        character(len=*), intent(in) :: artifact_path

        character(len=1024) :: parent_dir, stage_dir, base
        integer :: slash, name_start, n

        if (len_trim(artifact_path) == 0) return
        call fs_remove_file(artifact_path)
        slash = index(trim(artifact_path), '/', back=.true.)
        if (slash <= 1) return
        parent_dir = artifact_path(:slash - 1)
        if (index(trim(parent_dir), '/.fo-archive.tmp-') > 0) then
            call fs_remove_tree(trim(parent_dir))
            return
        end if
        name_start = slash + 1
        base = artifact_path(name_start:len_trim(artifact_path))
        if (index(trim(base), '.fo-shared.tmp-') /= 1) return
        n = len_trim(base)
        if (n > len('.dylib')) then
            if (base(n - len('.dylib') + 1:n) == '.dylib') then
                stage_dir = trim(parent_dir)//'/'//base(:n - len('.dylib'))
                call fs_remove_tree(trim(stage_dir))
                return
            end if
        end if
        if (n > len('.so')) then
            if (base(n - len('.so') + 1:n) == '.so') then
                stage_dir = trim(parent_dir)//'/'//base(:n - len('.so'))
                call fs_remove_tree(trim(stage_dir))
            end if
        end if
    end subroutine remove_ephemeral_link_artifact

    subroutine compile_test_helpers(project_dir, obj_dir, dag, filenames, is_prog, &
            topo_order, n_order, run_nodes, n_run, incl_flag, log_file, &
            helper_objs, n_helper_objs, exitcode, source_flags)
        !! Compile the module-only helper files a selected test program depends
        !! on, in dependency order, so their .mod files exist before the test
        !! programs compile and their objects can be linked in. Scoped to the
        !! dependency closure of the run set, exactly as fpm builds a test
        !! target: an unrelated (possibly broken) test module is never compiled.
        character(len=*), intent(in) :: project_dir, obj_dir, log_file
        type(dag_t), intent(in) :: dag
        character(len=MAX_PATH), intent(in) :: filenames(:)
        logical, intent(in) :: is_prog(:)
        integer, intent(in) :: topo_order(:), n_order, run_nodes(:), n_run
        character(len=*), intent(in) :: incl_flag
        character(len=*), intent(in) :: source_flags(:)
        character(len=512), intent(out) :: helper_objs(:)
        integer, intent(out) :: n_helper_objs
        integer, intent(out) :: exitcode

        integer :: i, node_id
        character(len=512) :: obj_path
        logical, allocatable :: needed(:)

        n_helper_objs = 0
        exitcode = 0
        allocate (needed(MAX_NODES))
        needed = .false.
        do i = 1, n_run
            call mark_reachable(dag, run_nodes(i), needed)
        end do

        do i = 1, n_order
            node_id = topo_order(i)
            if (len_trim(filenames(node_id)) == 0) cycle
            if (is_prog(node_id)) cycle
            if (.not. needed(node_id)) cycle
            ! A module beside a test program shares that program's object, which
            ! defines main; the program links it directly, never as a helper.
            if (file_defines_program(filenames(node_id))) cycle
            call make_obj_path(filenames(node_id), project_dir, obj_dir, obj_path)
            call compile_f90(project_dir, filenames(node_id), obj_path, &
                source_flags(node_id), log_file, exitcode)
            if (exitcode /= 0) return
            if (n_helper_objs >= size(helper_objs)) cycle
            n_helper_objs = n_helper_objs + 1
            helper_objs(n_helper_objs) = obj_path
        end do

    contains

        logical function file_defines_program(filename) result(found)
            character(len=*), intent(in) :: filename
            integer :: j

            found = .false.
            do j = 1, n_order
                if (.not. is_prog(topo_order(j))) cycle
                if (trim(filenames(topo_order(j))) /= trim(filename)) cycle
                found = .true.
                return
            end do
        end function file_defines_program

    end subroutine compile_test_helpers

    recursive subroutine mark_reachable(dag, node_id, needed)
        !! Mark every node reachable from node_id along dependency edges
        !! (edges point node -> dependency), so the caller can scope work to a
        !! test program's transitive dependency closure.
        type(dag_t), intent(in) :: dag
        integer, intent(in) :: node_id
        logical, intent(inout) :: needed(:)
        integer :: j, dep_id

        if (node_id <= 0 .or. node_id > dag%n_nodes) return
        do j = 1, dag%nodes(node_id)%n_edges
            dep_id = dag%nodes(node_id)%edges(j)
            if (dep_id <= 0 .or. dep_id > dag%n_nodes) cycle
            if (needed(dep_id)) cycle
            needed(dep_id) = .true.
            call mark_reachable(dag, dep_id, needed)
        end do
    end subroutine mark_reachable

    subroutine append_flaky_status(log_file, test_name)
        !! Report a test that failed under parallel execution but passed on an
        !! isolated rerun: a likely parallel-execution race. Named so the user
        !! can fix it (usually a shared /tmp path or global state between tests).
        character(len=*), intent(in) :: log_file, test_name
        integer :: u, ios

        open (newunit=u, file=trim(log_file), position='append', &
            status='old', iostat=ios)
        if (ios /= 0) return
        write (u, '(a,a,a)') 'fo: FLAKY test ', trim(test_name), &
            ': failed in parallel, passed on isolated rerun -- a '// &
            'test-parallelism race that MUST be fixed in code (a shared /tmp '// &
            'path or global state between tests). Make every path the test '// &
            'creates process-unique. Tests always run on all cores in parallel.'
        close (u)
    end subroutine append_flaky_status

    subroutine write_test_result_line(log_file, name, status, exit_str, secs)
        character(len=*), intent(in) :: log_file, name, status, exit_str
        real, intent(in) :: secs

        integer :: u, ios
        character(len=16) :: secs_str

        write (secs_str, '(F8.2)') secs
        open (newunit=u, file=trim(log_file), position='append', &
            status='old', iostat=ios)
        if (ios /= 0) return
        write (u, '(a,a,a,a,a,a,a,a)') 'TEST_RESULT ', trim(name), ' ', &
            trim(status), ' ', trim(exit_str), ' ', trim(secs_str)
        close (u)
    end subroutine write_test_result_line

    integer function build_timeout_seconds() result(t)
        !! Per-invocation wall clock for a single compile or link. A hung
        !! compiler is killed at this deadline (exit 124) instead of stalling
        !! the whole build; tune with FO_BUILD_TIMEOUT.
        character(len=32) :: buf
        integer :: status, iostat

        t = 600
        call get_environment_variable('FO_BUILD_TIMEOUT', buf, status=status)
        if (status /= 0 .or. len_trim(buf) == 0) return
        read (buf, *, iostat=iostat) t
        if (iostat /= 0 .or. t < 1) t = 600
    end function build_timeout_seconds

    subroutine append_build_hang_hint(log_file, what, timeout_s)
        !! Note a compile/link that hit the wall clock and was killed, so the
        !! 124 exit reads as a hang rather than a silent build failure.
        character(len=*), intent(in) :: log_file, what
        integer, intent(in) :: timeout_s
        integer :: u, ios
        character(len=32) :: tbuf

        write (tbuf, '(i0)') timeout_s
        open (newunit=u, file=trim(log_file), position='append', action='write', &
            status='unknown', iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') 'fo: '//trim(what)//' exceeded FO_BUILD_TIMEOUT='// &
            trim(tbuf)//'s and was killed (treated as a hang).'
        write (u, '(a)') 'fo: raise FO_BUILD_TIMEOUT if this step is legitimately slow.'
        close (u)
    end subroutine append_build_hang_hint

    integer function test_warn_seconds(timeout_s) result(w)
        integer, intent(in) :: timeout_s
        character(len=32) :: buf
        integer :: status, iostat

        w = 30
        call get_environment_variable('FO_TEST_WARN', buf, status=status)
        if (status == 0 .and. len_trim(buf) > 0) then
            read (buf, *, iostat=iostat) w
            if (iostat /= 0 .or. w < 1) w = 30
        end if
        if (w > timeout_s) w = timeout_s
    end function test_warn_seconds

    subroutine warn_slow_tests(filenames, run_nodes, run_exits, run_secs, run_cpu, &
            n_run, test_warn, log_file)
        !! Advise renaming fast-suite tests that approach their budget. CPU time
        !! is used where known (run_cpu >= 0): wall time also measures host load.
        character(len=MAX_PATH), intent(in) :: filenames(:)
        integer, intent(in) :: run_nodes(:), run_exits(:), n_run, test_warn
        real, intent(in) :: run_secs(:), run_cpu(:)
        character(len=*), intent(in) :: log_file

        integer :: i
        character(len=MAX_PATH) :: tname

        do i = 1, n_run
            if (run_exits(i) /= 0) cycle
            call file_basename(filenames(run_nodes(i)), tname)
            if (run_cpu(i) >= 0.0) then
                if (run_cpu(i) < real(test_warn)) cycle
                call warn_current_slow_test(tname, run_cpu(i), .true., log_file)
            else
                if (run_secs(i) < real(test_warn)) cycle
                call warn_current_slow_test(tname, run_secs(i), .false., log_file)
            end if
        end do
    end subroutine warn_slow_tests

    subroutine append_timeout_status(log_file, test_name)
        !! The test's own log already names the limit that fired
        !! (run_test_binary); this line says what to do about it.
        character(len=*), intent(in) :: log_file, test_name

        integer :: u, ios

        open (newunit=u, file=trim(log_file), position='append', &
            status='old', iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') 'fo: test target '//trim(test_name)// &
            ' timed out and was killed; fix the hang, name it *_slow, or raise '// &
            'the CPU budget (FO_TEST_TIMEOUT or [extra.fo] test-timeout)'
        close (u)
    end subroutine append_timeout_status

    subroutine append_test_status(log_file, test_name, status)
        character(len=*), intent(in) :: log_file, test_name
        integer, intent(in) :: status

        integer :: u, ios

        open (newunit=u, file=trim(log_file), position='append', &
            status='old', iostat=ios)
        if (ios /= 0) return
        ! Shell-style 128+signal exit codes mean the test crashed, not that it
        ! returned a normal failure. Name the signal so the report and hint can
        ! point at a memory bug / stack overflow instead of "make it faster".
        if (status > 128) then
            write (u, '(a,a,a,i0,a,a,a)') 'fo: test target ', trim(test_name), &
                ' returned exit code ', status, ' (crashed: ', &
                trim(signal_name(status - 128)), ')'
        else
            write (u, '(a,a,a,i0)') 'fo: test target ', trim(test_name), &
                ' returned exit code ', status
        end if
        close (u)
    end subroutine append_test_status

    pure function signal_name(sig) result(name)
        integer, intent(in) :: sig
        character(len=16) :: name
        select case (sig)
        case (11); name = 'SIGSEGV'
        case (6);  name = 'SIGABRT'
        case (8);  name = 'SIGFPE'
        case (4);  name = 'SIGILL'
        case (10); name = 'SIGBUS'
        case (7);  name = 'SIGBUS'
        case default
            write (name, '(a,i0)') 'signal ', sig
        end select
    end function signal_name

    logical function selected_test(name, selected_names, n_selected) result(found)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: selected_names(:)
        integer, intent(in) :: n_selected
        integer :: i

        found = .false.
        do i = 1, n_selected
            if (trim(name) == trim(selected_names(i))) then
                found = .true.
                return
            end if
        end do
    end function selected_test

    logical function is_slow_name(name) result(slow)
        character(len=*), intent(in) :: name
        character(len=MAX_NAME) :: lower_name
        integer :: i, n

        lower_name = name
        do i = 1, len_trim(lower_name)
            if (lower_name(i:i) >= 'A' .and. lower_name(i:i) <= 'Z') &
                lower_name(i:i) = achar(iachar(lower_name(i:i)) + 32)
        end do
        n = len_trim(lower_name)
        slow = n >= 5 .and. lower_name(n - 4:n) == '_slow'
        if (.not. slow) slow = index(trim(lower_name), '_slow_') > 0
    end function is_slow_name

    subroutine collect_current_lib_objs(project_dir, config, obj_dir, dep_includes, &
            n_dep_includes, dep_objs, n_dep_objs, lib_objs, n_lib_objs)
        character(len=*), intent(in) :: project_dir, obj_dir
        type(fpm_config_t), intent(in) :: config
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(in) :: n_dep_includes
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs
        character(len=512), intent(out) :: lib_objs(MAX_SRC_OBJS)
        integer, intent(out) :: n_lib_objs

        type(scan_unit_t), allocatable :: units(:), all_units(:)
        type(resolved_src_t) :: deps(MAX_RESOLVED)
        type(dag_t) :: dag
        character(len=MAX_PATH), allocatable :: filenames(:)
        logical, allocatable :: is_prog(:), is_test_arr(:)
        integer, allocatable :: topo_order(:)
        character(len=512), allocatable :: cfiles(:)
        integer :: n_units, n_all, ierr, i, node_id, n_order, n_deps
        integer :: ic, n_cfiles, d
        logical :: has_cycle
        character(len=512) :: obj_path, c_line

        n_lib_objs = 0
        lib_objs = ''
        n_deps = 0
        call scan_dir(trim(project_dir)//'/'//trim(config%source_dir), units, &
            n_units, ierr)
        n_all = 0
        allocate (all_units(n_units))
        if (ierr == 0) then
            do i = 1, n_units
                if (n_all >= MAX_UNITS) exit
                n_all = n_all + 1
                all_units(n_all) = units(i)
            end do
        end if
        call add_dep_sources(project_dir, all_units, n_all, deps, n_deps)

        if (n_all > 0) then
            allocate (filenames(MAX_NODES), is_prog(MAX_NODES), &
                is_test_arr(MAX_NODES))
            allocate (topo_order(MAX_NODES))
            call build_dag_from_units(all_units, n_all, dag, filenames, &
                is_test_arr, is_prog)
            call dag_topo_sort(dag, topo_order, n_order, has_cycle)
            do i = 1, n_order
                node_id = topo_order(i)
                if (len_trim(filenames(node_id)) == 0) cycle
                if (is_test_arr(node_id)) cycle
                if (is_prog(node_id)) cycle
                call make_obj_path(filenames(node_id), project_dir, obj_dir, &
                    obj_path)
                call append_current_lib_obj(obj_path, lib_objs, n_lib_objs)
            end do
            deallocate (filenames, is_prog, is_test_arr, topo_order)
        end if

        allocate (cfiles(MAX_SRC_OBJS))
        call collect_c_family(trim(project_dir)//'/'//trim(config%source_dir), &
            cfiles, n_cfiles)
        do ic = 1, n_cfiles
            c_line = cfiles(ic)
            if (len_trim(c_line) == 0) cycle
            call make_obj_path(trim(c_line), project_dir, obj_dir, obj_path)
            call append_current_lib_obj(obj_path, lib_objs, n_lib_objs)
        end do
        do d = 1, n_deps
            call collect_c_family(trim(deps(d)%src_dir), cfiles, n_cfiles)
            do ic = 1, n_cfiles
                c_line = cfiles(ic)
                if (len_trim(c_line) == 0) cycle
                call make_obj_path(trim(c_line), project_dir, obj_dir, obj_path)
                call append_current_lib_obj(obj_path, lib_objs, n_lib_objs)
            end do
        end do
        deallocate (cfiles, units, all_units)
    end subroutine collect_current_lib_objs

    subroutine append_current_lib_obj(obj_path, lib_objs, n_lib_objs)
        character(len=*), intent(in) :: obj_path
        character(len=512), intent(inout) :: lib_objs(MAX_SRC_OBJS)
        integer, intent(inout) :: n_lib_objs

        integer :: i

        if (len_trim(obj_path) == 0) return
        do i = 1, n_lib_objs
            if (trim(lib_objs(i)) == trim(obj_path)) return
        end do
        if (n_lib_objs < MAX_SRC_OBJS) then
            n_lib_objs = n_lib_objs + 1
            lib_objs(n_lib_objs) = trim(obj_path)
        end if
    end subroutine append_current_lib_obj

    logical function source_is_in_dir(source_path, project_dir, relative_dir)
        character(len=*), intent(in) :: source_path, project_dir, relative_dir
        character(len=MAX_PATH) :: prefix
        integer :: n

        prefix = trim(project_dir)//'/'//trim(relative_dir)//'/'
        n = len_trim(prefix)
        source_is_in_dir = len_trim(source_path) >= n .and. &
            source_path(1:n) == prefix(1:n)
    end function source_is_in_dir

    subroutine hash_lib_objs(lib_objs, n_lib_objs, combined)
        !! Combine per-object content hashes into one key that represents the
        !! linked library implementation a test binary depends on, so a cached
        !! test result is invalidated when any library object changes.
        character(len=512), intent(in) :: lib_objs(:)
        integer, intent(in) :: n_lib_objs
        character(len=HASH_LEN), intent(out) :: combined
        character(len=512) :: tmpfile
        character(len=HASH_LEN) :: h
        integer :: i, u, ios

        combined = ''
        if (n_lib_objs <= 0) return
        call make_tmpfile('fo_lib_hash', tmpfile)
        open (newunit=u, file=trim(tmpfile), status='replace', iostat=ios)
        if (ios /= 0) return
        do i = 1, n_lib_objs
            call hash_mod_file(lib_objs(i), h)
            write (u, '(a)') trim(h)//' '//trim(lib_objs(i))
        end do
        close (u)
        call hash_mod_file(tmpfile, combined)
        call delete_tmpfile(tmpfile)
    end subroutine hash_lib_objs

    recursive subroutine make_obj_path(source_path, project_dir, obj_dir, obj_path)
        character(len=*), intent(in) :: source_path, project_dir, obj_dir
        character(len=*), intent(out) :: obj_path

        character(len=512) :: rel
        integer :: i, plen
        logical :: in_project

        plen = len_trim(project_dir)
        in_project = .false.
        if (len_trim(source_path) > plen + 1) then
            if (source_path(1:plen) == project_dir) &
                in_project = source_path(plen + 1:plen + 1) == '/'
        end if
        if (in_project) then
            rel = source_path(plen + 2:)
        else
            ! External absolute paths can exceed a filesystem component's limit.
            ! Keep project-relative names decodable for app/example targets.
            rel = source_path
            if (fs_path_is_absolute(source_path)) &
                rel = 'external_'//cache_digest([source_path], 1)
        end if
        do i = 1, len_trim(rel)
            if (rel(i:i) == '/') rel(i:i) = '_'
        end do
        obj_path = trim(obj_dir)//'/'//trim(rel)//'.o'
    end subroutine make_obj_path

    recursive function fc_command() result(cmd)
        !! Fortran compiler: $FO_FC if set, else gfortran. Lets a host opt into
        !! a different compiler (e.g. flang to dodge a gfortran codegen bug)
        !! without editing the project.
        character(len=:), allocatable :: cmd
        cmd = trim(selected_compiler_command())
    end function fc_command

    function fc_executable_command() result(cmd)
        character(len=:), allocatable :: cmd

        cmd = fc_command()
        if (len_trim(detected_fc_path) > 0) cmd = trim(detected_fc_path)
    end function fc_executable_command

    subroutine resolve_fc_executable(command)
        character(len=*), intent(in) :: command
        logical :: found

        detected_fc_path = ''
        call fs_find_executable(command, detected_fc_path, found)
        detected_fc_is_executable = found
        if (.not. found) detected_fc_path = command
    end subroutine resolve_fc_executable

    subroutine append_fc_command(packed, n_args)
        character(len=:), allocatable, intent(inout) :: packed
        integer, intent(inout) :: n_args

        if (detected_fc_is_executable) then
            call argv_push(packed, n_args, trim(detected_fc_path))
        else
            call argv_push_split(packed, n_args, fc_executable_command())
        end if
    end subroutine append_fc_command

    recursive logical function fc_is_flang()
        !! True when the selected compiler is LLVM flang. Drives flag dialect:
        !! flang rejects gfortran-only flags and spells the module-output dir
        !! differently.
        type(compiler_dialect_t) :: dialect

        dialect = compiler_dialect(fc_command())
        fc_is_flang = dialect%is_flang()
    end function fc_is_flang

    recursive function fc_policy_flags() result(flags)
        !! Compiler-appropriate baseline compile flags. gfortran needs the long
        !! free-form line length; flang has no line limit and rejects that flag.
        !! -fopenmp is added per project via the fpm openmp metapackage (see
        !! config_flags_str), not here.
        character(len=:), allocatable :: flags

        type(compiler_dialect_t) :: dialect
        character(len=512) :: diet_flag, split_flags

        dialect = compiler_dialect(fc_command())
        flags = dialect%base_flags()
        ! Debug-info diet: the default build emits no DWARF, so 500 test
        ! binaries stop each carrying their own copy of it. A caller that asked
        ! for debug info (`--debug`, `--asan`, `-g`) keeps it.
        diet_flag = dialect%debug_info_diet_flag(build_request_flags)
        if (len_trim(diet_flag) > 0) flags = trim(flags)//' '//trim(diet_flag)
        ! Section splitting pairs with `--gc-sections` in fc_link_policy_flags.
        ! A sanitizer build skips both: its instrumentation lives in
        ! constructor tables that section garbage collection can collect.
        split_flags = dialect%sections_split_flags(build_request_flags)
        if (len_trim(split_flags) > 0) then
            if (compiler_supports_section_splitting(fc_command(), &
                trim(split_flags))) flags = trim(flags)//' '//trim(split_flags)
        end if
        call append_pipe_flag(fc_command(), flags)
    end function fc_policy_flags

    function fc_link_policy_flags() result(flags)
        !! Link-side half of the section-split policy; it reads the same module
        !! state as `fc_policy_flags`, so compile and link cannot disagree about
        !! whether garbage collection is on.
        character(len=:), allocatable :: flags

        type(compiler_dialect_t) :: dialect

        dialect = compiler_dialect(fc_command())
        flags = dialect%gc_sections_link_flag(build_request_flags)
        if (len_trim(flags) > 0) then
            if (.not. compiler_supports_section_splitting(fc_command(), &
                dialect%sections_split_flags(build_request_flags))) then
                flags = ''
            else if (is_macos()) then
                flags = '-Wl,-dead_strip'
            end if
        end if
    end function fc_link_policy_flags

    recursive function fc_base_flags() result(flags)
        character(len=:), allocatable :: flags
        type(compiler_dialect_t) :: dialect
        dialect = compiler_dialect(fc_command())
        flags = fc_policy_flags()
        ! Source form is a package choice. In particular, "default" must let
        ! the compiler choose by extension instead of retaining -Mfree/-free.
        call remove_manifest_flag(flags, dialect%translate_flag('-ffree-form'))
    end function fc_base_flags

    recursive function request_key_flags(flags) result(key_flags)
        character(len=*), intent(in) :: flags
        character(len=:), allocatable :: key_flags

        key_flags = 'manifest-policy:package-v3'//new_line('a')// &
            'compiler-policy:'//trim(fc_policy_flags())//new_line('a')// &
            'request-flags:'//trim(flags)
    end function request_key_flags

    recursive function compile_key_flags(flags) result(key_flags)
        character(len=*), intent(in) :: flags
        character(len=:), allocatable :: key_flags

        key_flags = 'manifest-policy:package-v3'//new_line('a')// &
            'compiler-baseline:'//trim(fc_base_flags())//new_line('a')// &
            'project-flags:'//trim(flags)
    end function compile_key_flags

    subroutine make_includes_flag(mod_dir, dep_includes, n_dep_includes, flag)
        character(len=*), intent(in) :: mod_dir
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(in) :: n_dep_includes
        character(len=*), intent(out) :: flag

        integer :: i
        type(compiler_dialect_t) :: dialect

        ! Newline-separated argv tokens with raw, unquoted paths. compile_f90
        ! splits this on newlines so each -I/-J path stays one whole token even
        ! when it contains spaces; shell quoting must not be applied, or quotes
        ! would land literally in the path and the .mod would not be found.
        dialect = compiler_dialect(fc_command())
        flag = dialect%module_flags(mod_dir)
        do i = 1, n_dep_includes
            flag = trim(flag)//char(10)//'-I'//char(10)//trim(dep_includes(i))
        end do
    end subroutine make_includes_flag

    recursive function with_user_flags(includes_nl, flags) result(combined)
        !! Append space-separated user flags onto the newline-separated include
        !! token list as their own newline tokens, so the whole string stays
        !! newline-delimited and -I/-J paths containing spaces survive the split
        !! in compile_f90.
        character(len=*), intent(in) :: includes_nl, flags
        character(len=:), allocatable :: combined
        integer :: i, start, n

        combined = trim(includes_nl)
        n = len_trim(flags)
        start = 0
        do i = 1, n
            if (flags(i:i) == ' ') then
                if (start > 0) then
                    combined = combined//char(10)//flags(start:i - 1)
                    start = 0
                end if
            else if (start == 0) then
                start = i
            end if
        end do
        if (start > 0) combined = combined//char(10)//flags(start:n)
    end function with_user_flags

    subroutine detect_compiler(compiler)
        character(len=*), intent(out) :: compiler

        character(len=256) :: line
        character(len=512) :: tmpfile, command
        character(len=:), allocatable :: packed
        integer :: u, iostat, n_args, exitcode
        logical :: memo_hit
        character(len=HASH_LEN) :: tool_keys(3), tool_key

        command = fc_command()
        call resolve_fc_executable(command)
        tool_keys(1) = compiler_tool_key(command)
        tool_keys(2) = compiler_tool_key('gcc')
        tool_keys(3) = compiler_tool_key('g++')
        tool_key = cache_digest(tool_keys, size(tool_keys))
        if (trim(command) == trim(detected_fc) .and. &
            len_trim(detected_compiler) > 0 .and. tool_key == detected_tool_key) then
            compiler = detected_compiler
            return
        end if
        compiler = command
        call compiler_memo_load(command, compiler, memo_hit)
        if (memo_hit) then
            compiler = 'tool-sha256:'//tool_key//' '//trim(compiler)
            detected_fc = command
            detected_compiler = compiler
            detected_tool_key = tool_key
            return
        end if
        call make_tmpfile('fo_compiler_version', tmpfile)
        n_args = 0
        call append_fc_command(packed, n_args)
        call argv_push(packed, n_args, '--version')
        call process_run_argv_logged('', packed, n_args, trim(tmpfile), &
            .false., 30, exitcode)

        open (newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat == 0) then
            read (u, '(a)', iostat=iostat) line
            if (iostat == 0 .and. len_trim(line) > 0) compiler = trim(line)
            close (u)
        end if
        call delete_tmpfile(tmpfile)
        call compiler_memo_save(command, compiler)
        compiler = 'tool-sha256:'//tool_key//' '//trim(compiler)
        detected_fc = command
        detected_compiler = compiler
        detected_tool_key = tool_key
    end subroutine detect_compiler

    function compiler_tool_key(command) result(key)
        !! Capture Fo's explicit compiler drivers, without recursing into host tools.
        character(len=*), intent(in) :: command
        character(len=HASH_LEN) :: key, executable_key
        character(len=512) :: executable, program, parts(3)
        integer :: space
        logical :: found

        program = adjustl(command)
        space = index(trim(program), ' ')
        if (space > 0) program = program(:space - 1)
        call fs_find_executable(trim(command), executable, found)
        if (.not. found) call fs_find_executable(trim(program), executable, found)
        executable_key = ''
        if (found) call cache_file_digest(trim(executable), executable_key)
        parts(1) = command
        parts(2) = executable
        parts(3) = executable_key
        key = cache_digest(parts, size(parts))
    end function compiler_tool_key

    recursive subroutine compile_f90(project_dir, source, objfile, includes_flag, log_file, &
            exitcode)
        character(len=*), intent(in) :: project_dir, source, objfile, includes_flag
        character(len=*), intent(in) :: log_file
        integer, intent(out) :: exitcode
        character(len=:), allocatable :: packed
        integer :: n_args, n_removed

        ! Build an argv vector and spawn via the async-signal-safe argv runner
        ! (fork+execve, no /bin/sh): it is safe inside the OpenMP parallel
        ! compile/test loops, where system()/fork from a multithreaded process
        ! corrupts libgomp, and it is quote-proof, unlike a shell command line.
        !$omp critical (fo_compile_command)
        n_args = 0
        call append_fc_command(packed, n_args)
        call argv_push(packed, n_args, '-c')
        call argv_push_split(packed, n_args, fc_base_flags())
        call argv_push_split_nl(packed, n_args, includes_flag)
        call argv_push(packed, n_args, '-o')
        call argv_push(packed, n_args, objfile)
        call argv_push(packed, n_args, source)
        !$omp end critical (fo_compile_command)

        ! A successful process must produce this action's output; an old object
        ! cannot establish that the compiler actually compiled the new source.
        call clear_compiler_object(objfile, log_file, exitcode)
        if (exitcode /= 0) return
        call process_run_argv_logged(project_dir, packed, n_args, log_file, &
            .true., build_timeout_seconds(), exitcode)
        if (exitcode == 124) call append_build_hang_hint(log_file, &
            'compile of '//trim(source), build_timeout_seconds())
        call validate_compiler_object(objfile, source, log_file, exitcode)
        if (exitcode == 0) return
        if (.not. looks_like_stale_mod_failure(log_file)) return

        call clean_root_build_artifacts(project_dir, n_removed)
        call append_stale_mod_hint(log_file, n_removed)

        call clear_compiler_object(objfile, log_file, exitcode)
        if (exitcode /= 0) return
        call process_run_argv_logged(project_dir, packed, n_args, log_file, &
            .true., build_timeout_seconds(), exitcode)
        if (exitcode == 124) call append_build_hang_hint(log_file, &
            'compile of '//trim(source), build_timeout_seconds())
        call validate_compiler_object(objfile, source, log_file, exitcode)
    end subroutine compile_f90

    subroutine clear_compiler_object(objfile, log_file, exitcode)
        character(len=*), intent(in) :: objfile, log_file
        integer, intent(out) :: exitcode
        integer :: unit, ios
        logical :: exists

        call fs_remove_file(objfile)
        inquire (file=trim(objfile), exist=exists)
        exitcode = 0
        if (.not. exists) return
        exitcode = 1
        open (newunit=unit, file=trim(log_file), position='append', &
            status='unknown', action='write', iostat=ios)
        if (ios /= 0) return
        write (unit, '(a)') 'fo: cannot remove previous compiler object: '//trim(objfile)
        close (unit)
    end subroutine clear_compiler_object

    subroutine validate_compiler_object(objfile, source, log_file, exitcode)
        character(len=*), intent(in) :: objfile, source, log_file
        integer, intent(inout) :: exitcode
        integer(int64) :: bytes
        integer :: unit, ios
        logical :: exists

        if (exitcode /= 0) return
        inquire (file=trim(objfile), exist=exists, size=bytes, iostat=ios)
        if (ios == 0) then
            if (exists) then
                if (bytes > 0) return
            end if
        end if
        exitcode = 1
        call fs_remove_file(objfile)
        open (newunit=unit, file=trim(log_file), position='append', &
            status='unknown', action='write', iostat=ios)
        if (ios /= 0) return
        write (unit, '(a)') 'fo: compiler produced no nonempty object: '//trim(objfile)
        write (unit, '(a)') 'fo: compiler: '//fc_executable_command()
        write (unit, '(a)') 'fo: source: '//trim(source)
        close (unit)
    end subroutine validate_compiler_object

    logical function looks_like_stale_mod_failure(log_file) result(matches)
        character(len=*), intent(in) :: log_file

        character(len=8192) :: text

        call read_text_file(log_file, text)
        matches = index(text, 'is not a GNU Fortran module file') > 0 .or. &
            index(text, 'created by a different version of GNU Fortran') > 0 .or. &
            index(text, 'Cannot open module file') > 0 .or. &
            index(text, 'Fatal Error: Cannot read module file') > 0
    end function looks_like_stale_mod_failure

    subroutine append_stale_mod_hint(log_file, n_removed)
        character(len=*), intent(in) :: log_file
        integer, intent(in) :: n_removed

        integer :: u, ios

        open (newunit=u, file=trim(log_file), status='old', position='append', &
            iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') 'fo: possible stale root build artifacts detected.'
        write (u, '(a)') 'fo: .mod/.smod/.o files in the project root can shadow build/fo/mod.'
        write (u, '(a)') 'fo: VS Code Modern Fortran linting can create these files; set ' // &
            'fortran.linter.modOutput outside the project root.'
        if (n_removed > 0) then
            write (u, '(a,i0,a)') 'fo: removed ', n_removed, &
                ' root build artifacts and retried once.'
            write (error_unit, '(a,i0,a)') 'fo: removed ', n_removed, &
                ' stale root .mod, .smod, and .o build artifacts; retried once'
        else
            write (u, '(a)') 'fo: no root build artifacts were found to clean.'
            write (error_unit, '(a)') &
                'fo: stale-module-like failure; root artifacts may already be clean'
        end if
        write (error_unit, '(a)') &
            'fo: VS Code Modern Fortran users: set fortran.linter.modOutput outside the project root'
        close (u)
    end subroutine append_stale_mod_hint

    subroutine collect_c_family(root, items, n_items)
        !! Collect C and C++ sources under root into one list. A project that
        !! binds a C++ library needs a shim compiled by a C++ compiler; keeping
        !! the two extensions in one list means every call site that already
        !! handled .c handles .cpp without a second loop.
        character(len=*), intent(in) :: root
        character(len=512), intent(out) :: items(:)
        integer, intent(out) :: n_items

        integer :: n_cxx

        call fs_collect_files(root, '', '.c', '', items, n_items)
        if (n_items >= size(items)) return
        call fs_collect_files(root, '', '.cpp', '', &
            items(n_items + 1:), n_cxx)
        n_items = n_items + n_cxx
    end subroutine collect_c_family

    logical function is_cxx_source(path) result(is_cxx)
        !! True for a C++ source, which must go to the C++ driver: g++ finds the
        !! C++ standard headers and links the C++ runtime, gcc does neither.
        character(len=*), intent(in) :: path
        integer :: n
        n = len_trim(path)
        is_cxx = .false.
        if (n >= 4) is_cxx = path(n - 3:n) == '.cpp'
    end function is_cxx_source

    subroutine compile_c_family(source, objfile, package_dir, log_file, exitcode)
        character(len=*), intent(in) :: source, objfile, package_dir, log_file
        integer, intent(out) :: exitcode
        type(fpm_config_t), allocatable :: config
        character(len=512) :: directories(MAX_DEP_DIRS), objects(MAX_DEP_OBJS)
        character(:), allocatable :: packed
        integer :: n_args, n_directories, n_objects, i

        call fpm_config_allocate(config)
        call fpm_config_parse(package_dir, config, exitcode)
        if (exitcode /= 0) return
        call find_dep_artifacts(package_dir, config, directories, n_directories, &
            objects, n_objects)
        n_args = 0
        if (is_cxx_source(source)) then
            call argv_push(packed, n_args, 'g++')
            call argv_push(packed, n_args, '-std=c++17')
            call argv_push(packed, n_args, '-fPIC')
        else
            call argv_push(packed, n_args, 'gcc')
        end if
        call argv_push(packed, n_args, '-c')
        do i = 1, n_directories
            call argv_push(packed, n_args, '-I')
            call argv_push(packed, n_args, trim(directories(i)))
        end do
        call argv_push(packed, n_args, '-o')
        call argv_push(packed, n_args, objfile)
        call argv_push(packed, n_args, source)
        call process_run_argv_logged('', packed, n_args, log_file, .true., &
            build_timeout_seconds(), exitcode)
    end subroutine compile_c_family

    subroutine link_binary(project_dir, prog_obj, lib_objs, n_lib_objs, dep_objs, n_dep_objs, &
            link_libs, n_link_libs, output, log_file, exitcode, flags, &
            cache, base_digest)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: prog_obj, output, log_file
        character(len=512), intent(in) :: lib_objs(:)
        integer, intent(in) :: n_lib_objs
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs
        character(len=128), intent(in) :: link_libs(*)
        integer, intent(in) :: n_link_libs
        integer, intent(out) :: exitcode
        ! user flags forwarded to linker (e.g. -fsanitize=address needs to be
        ! present at link time so the runtime library is linked in)
        character(len=*), intent(in), optional :: flags
        ! Link-cache handle and the precomputed digest of all shared link
        ! inputs (library + dep objects + resolved archives). When both are
        ! present, the link is an action keyed by (toolchain, flags, base_digest,
        ! this program object): a cache hit restores the binary instead of
        ! relinking. Linking is the dominant warm-build cost otherwise.
        type(cache_t), intent(in), optional :: cache
        character(len=*), intent(in), optional :: base_digest

        character(len=:), allocatable :: packed
        character(len=8) :: debug_links
        integer :: debug_status
        integer :: n_args
        logical :: do_cache, restored, use_lld
        character(len=HASH_LEN) :: action_id, prog_key
        character(len=512) :: key_parts(8), lld_log, compiler
        character(len=1024) :: stage_dir, staged_output, restore_bin
        character(len=:), allocatable :: flags_str
        integer :: store_ierr, reserve_rc, copy_rc, rename_rc
        logical :: valid_image

        flags_str = ''
        if (present(flags)) flags_str = trim(flags)
        exitcode = 1
        call select_linker(fc_command(), use_lld)

        do_cache = present(cache) .and. present(base_digest)
        if (do_cache) do_cache = len_trim(base_digest) > 0
        if (do_cache) then
            call cache_file_digest(prog_obj, prog_key)
            key_parts(1) = 'fo-link-3'
            key_parts(2) = trim(fc_command())
            key_parts(3) = 'default'
            if (use_lld) key_parts(3) = 'lld-with-default-fallback'
            key_parts(4) = flags_str
            key_parts(5) = trim(link_lib_flags(project_dir, link_libs, n_link_libs))
            key_parts(6) = trim(base_digest)
            key_parts(7) = trim(prog_key)
            call detect_compiler(compiler)
            key_parts(8) = compiler
            action_id = cache_digest(key_parts, 8)
            ! Fast path: the output is already the binary this action produces.
            ! Leave it untouched - no relink, no copy. This is what keeps warm
            ! builds cheap when outputs are large (2.4 GB of static binaries).
            call cache_binary_matches(cache, action_id, output, restored)
            if (restored) then
                call native_image_valid(output, valid_image)
                if (valid_image) then
                    exitcode = 0
                    return
                end if
            end if
        end if

        call reserve_link_stage(output, stage_dir, staged_output, reserve_rc)
        if (reserve_rc /= 0) then
            call append_artifact_error(log_file, &
                'fo: could not reserve executable staging directory')
            exitcode = 1
            return
        end if
        if (do_cache) then
            ! Restore into the owned stage, then publish it with the same atomic
            ! rename used for a fresh linker output.
            restore_bin = trim(stage_dir)//'/cached'
            call cache_restore_binary(cache, action_id, restore_bin, restored)
            if (restored) then
                copy_rc = fs_copy_exec(trim(restore_bin), trim(staged_output))
                call fs_remove_file(trim(restore_bin))
                if (copy_rc == 0) then
                    call native_image_valid(staged_output, valid_image)
                    if (valid_image) then
                        rename_rc = fs_rename(trim(staged_output), trim(output))
                        if (rename_rc == 0) then
                            call fs_remove_tree(trim(stage_dir))
                            exitcode = 0
                            return
                        end if
                    end if
                end if
            end if
            call fs_remove_tree(trim(stage_dir))
            call reserve_link_stage(output, stage_dir, staged_output, reserve_rc)
            if (reserve_rc /= 0) then
                call append_artifact_error(log_file, &
                    'fo: could not reserve executable staging directory')
                exitcode = 1
                return
            end if
        end if

        ! Build an argv vector (no /bin/sh): quote-proof and async-signal-safe,
        ! since link_binary runs inside the parallel test loop where a shell
        ! fork would corrupt libgomp.
        call get_environment_variable('FO_DEBUG_LINKS', debug_links, &
            status=debug_status)
        call make_link_argv(project_dir, prog_obj, lib_objs, n_lib_objs, dep_objs, &
            n_dep_objs, link_libs, n_link_libs, staged_output, flags_str, use_lld, &
            packed, n_args)
        if (debug_status == 0 .and. len_trim(debug_links) > 0) then
            write (error_unit, '(a)') 'fo link: '//argv_display(packed)
        end if
        if (use_lld) then
            call make_tmpfile('fo_lld_link', lld_log)
            call process_run_argv_logged('', packed, n_args, lld_log, .true., &
                build_timeout_seconds(), exitcode)
            call delete_tmpfile(lld_log)
        end if
        if (.not. use_lld .or. exitcode /= 0) then
            call fs_remove_file(trim(staged_output))
            call make_link_argv(project_dir, prog_obj, lib_objs, n_lib_objs, dep_objs, &
                n_dep_objs, link_libs, n_link_libs, staged_output, flags_str, .false., &
                packed, n_args)
            if (use_lld .and. debug_status == 0 .and. len_trim(debug_links) > 0) &
                write (error_unit, '(a)') 'fo link fallback: '//argv_display(packed)
            call process_run_argv_logged('', packed, n_args, log_file, .true., &
                build_timeout_seconds(), exitcode)
        end if
        if (exitcode == 124) call append_build_hang_hint(log_file, &
            'link of '//trim(output), build_timeout_seconds())

        if (exitcode /= 0) then
            call fs_remove_tree(trim(stage_dir))
            return
        end if
        call native_image_valid(staged_output, valid_image)
        if (.not. valid_image) then
            call append_artifact_error(log_file, &
                'fo: linker output is not a valid executable image')
            call fs_remove_tree(trim(stage_dir))
            exitcode = 1
            return
        end if
        ! Cache only this producer's private bytes. Once published, output may
        ! immediately be replaced by a different action's complete binary.
        if (do_cache) &
            call cache_store_binary(cache, action_id, staged_output, store_ierr)
        rename_rc = fs_rename(trim(staged_output), trim(output))
        if (rename_rc /= 0) then
            call append_artifact_error(log_file, &
                'fo: could not atomically publish executable')
            call fs_remove_tree(trim(stage_dir))
            exitcode = 1
            return
        end if
        call fs_remove_tree(trim(stage_dir))
    end subroutine link_binary

    subroutine reserve_link_stage(output, stage_dir, staged_output, reserve_rc)
        character(len=*), intent(in) :: output
        character(len=*), intent(out) :: stage_dir, staged_output
        integer, intent(out) :: reserve_rc

        character(len=1024) :: parent_dir
        integer :: i, slash
        logical :: exists

        slash = index(trim(output), '/', back=.true.)
        parent_dir = '.'
        if (slash == 1) then
            parent_dir = '/'
        else if (slash > 1) then
            parent_dir = output(:slash - 1)
        end if
        reserve_rc = 1
        do i = 1, 100
            call make_sibling_tmpfile(trim(parent_dir)//'/.fo-link', stage_dir)
            reserve_rc = fs_mkdir_excl(trim(stage_dir))
            if (reserve_rc /= 0) then
                if (reserve_rc < 0) return
                cycle
            end if
            staged_output = trim(stage_dir)//'/binary'
            inquire (file=trim(staged_output), exist=exists)
            if (.not. exists) return
            call fs_remove_tree(trim(stage_dir))
            reserve_rc = 1
        end do
    end subroutine reserve_link_stage

    subroutine make_link_argv(project_dir, prog_obj, lib_objs, n_lib_objs, dep_objs, &
            n_dep_objs, link_libs, n_link_libs, output, flags, use_lld, packed, n_args, &
            install_name)
        character(len=*), intent(in) :: project_dir, prog_obj, output, flags
        character(len=512), intent(in) :: lib_objs(:)
        integer, intent(in) :: n_lib_objs
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs
        character(len=128), intent(in) :: link_libs(*)
        integer, intent(in) :: n_link_libs
        logical, intent(in) :: use_lld
        character(len=:), allocatable, intent(out) :: packed
        integer, intent(out) :: n_args
        character(len=*), intent(in), optional :: install_name

        integer :: i
        character(len=512) :: policy_flag

        n_args = 0
        call append_fc_command(packed, n_args)
        if (use_lld) call argv_push(packed, n_args, '-fuse-ld=lld')
        call argv_push(packed, n_args, prog_obj)
        do i = 1, n_lib_objs
            call argv_push(packed, n_args, lib_objs(i))
        end do
        do i = 1, n_dep_objs
            call argv_push(packed, n_args, dep_objs(i))
        end do
        call argv_push_split_nl(packed, n_args, &
            link_lib_flags(project_dir, link_libs, n_link_libs))
        ! flang's driver does not add Homebrew's libomp to the link search, so
        ! -fopenmp links fail with "library 'omp' not found". Add it (harmless
        ! when the build does not use OpenMP) plus an rpath for runtime.
        if (fc_is_flang() .and. is_macos()) then
            call argv_push(packed, n_args, '-L/opt/homebrew/opt/libomp/lib')
            call argv_push(packed, n_args, '-Wl,-rpath,/opt/homebrew/opt/libomp/lib')
        end if
        if (len_trim(flags) > 0) call argv_push_split(packed, n_args, flags)
        policy_flag = fc_link_policy_flags()
        if (len_trim(policy_flag) > 0) call argv_push_split(packed, n_args, policy_flag)
        if (present(install_name)) then
            if (len_trim(install_name) > 0) &
                call argv_push(packed, n_args, &
                '-Wl,-install_name,'//trim(install_name))
        end if
        call argv_push(packed, n_args, '-o')
        call argv_push(packed, n_args, output)
    end subroutine make_link_argv

    function argv_display(packed) result(text)
        !! Render a packed NUL-separated argv buffer as a space-joined string
        !! for human-readable debug output only.
        character(len=*), intent(in) :: packed
        character(len=:), allocatable :: text
        integer :: i

        text = packed
        do i = 1, len(text)
            if (text(i:i) == char(0)) text(i:i) = ' '
        end do
        text = trim(text)
    end function argv_display

    function link_lib_flags(project_dir, link_libs, n_link_libs) result(flags)
        !! Resolve each `link` lib to a concrete archive on a search path and
        !! link it by absolute path, preferring a static .a so the binary does
        !! not depend on LIBRARY_PATH at link or run time. Libs found only via
        !! the system default search (libm, libstdc++, ...) keep -lname. Returns
        !! the space-prefixed flag string so the caller mutates its own cmd
        !! buffer (passing the deferred-length cmd through an assumed-length
        !! intent(inout) dummy dropped the appended tokens via copy-out).
        character(len=*), intent(in) :: project_dir
        character(len=128), intent(in) :: link_libs(*)
        integer, intent(in) :: n_link_libs
        character(len=:), allocatable :: flags

        character(len=512) :: dirs(2*MAX_DEP_DIRS)
        character(len=1024) :: token
        integer :: n_dirs, n_env, i

        flags = ''
        call build_lib_search_dirs(dirs, n_dirs, n_env)
        do i = 1, n_link_libs
            call resolve_link_token(project_dir, trim(link_libs(i)), dirs, n_dirs, n_env, &
                token)
            ! Newline-join so the caller can split with argv_push_split_nl,
            ! which preserves spaces in resolved library paths. Tokens are
            ! passed straight to the linker via argv (shell-free build), so
            ! they must NOT be shell-quoted.
            flags = flags//new_line('a')//trim(token)
        end do
    end function link_lib_flags

    subroutine resolve_link_token(project_dir, name, dirs, n_dirs, n_env, token)
        !! Resolve a linker token directly to a file when we can find it in the
        !! build-tree graph around the current project. This keeps local sibling
        !! dependencies resolvable even when callers do not configure LIBRARY_PATH.
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: name
        character(len=512), intent(in) :: dirs(:)
        integer, intent(in) :: n_dirs, n_env
        character(len=*), intent(out) :: token

        character(len=1024) :: cand
        character(len=8) :: ext1, ext2
        character(len=8) :: debug_links
        integer :: i, debug_status
        logical :: ex
        logical :: dbg
        logical :: use_local

        dbg = .false.
        call get_environment_variable('FO_DEBUG_LINKS', debug_links, status=debug_status)
        if (debug_status == 0 .and. len_trim(debug_links) > 0) dbg = .true.

        ! On macOS prefer the shared .dylib: its absolute install name pulls in
        ! transitive deps (CoreText, CoreGraphics, bz2, ...) that a static .a
        ! would leave unresolved at link time, and dylib install names are
        ! absolute so the binary needs no DYLD_LIBRARY_PATH at run time.
        !
        ! On Linux the preference is per directory. Env dirs (the first n_env,
        ! from LIBRARY_PATH/FO_LIBRARY_PATH) are non-default paths: prefer a
        ! static .a there so the binary does not need LIBRARY_PATH at run time.
        ! System dirs (/usr/lib, ...) prefer the shared .so: it is always on the
        ! default runtime search path, and a system static .a often needs a long
        ! chain of transitive deps (glib -> pcre2, ffi, sysprof, ...) that the
        ! project's `link` list does not enumerate, which would break the link.
        ! 1) Existing environment/system search: /path/lib{name}{.so|.a}
        do i = 1, n_dirs
            if (is_macos()) then
                ext1 = '.dylib'
                ext2 = '.a'
            else if (i <= n_env) then
                ext1 = '.a'
                ext2 = '.so'
            else
                ext1 = '.so'
                ext2 = '.a'
            end if
            cand = trim(dirs(i))//'/lib'//trim(name)//trim(ext1)
            inquire (file=trim(cand), exist=ex)
            if (ex) then
                token = trim(cand)
                if (dbg) write (error_unit, '(a,a)') 'fo link token found: ', trim(token)
                return
            end if
            cand = trim(dirs(i))//'/lib'//trim(name)//trim(ext2)
            inquire (file=trim(cand), exist=ex)
            if (ex) then
                token = trim(cand)
                if (dbg) write (error_unit, '(a,a)') 'fo link token found: ', trim(token)
                return
            end if
        end do

        ! 2) Local sibling projects (e.g. ../liric/build). This is common in
        ! lazy-fortran workspaces where LIRIC is built side-by-side, not in
        ! LIBRARY_PATH.
        call local_library_candidate(project_dir, trim(name), cand, use_local)
        if (use_local) then
            token = trim(cand)
            if (dbg) write (error_unit, '(a,a)') 'fo link token found: ', trim(token)
            return
        end if

        if (dbg) write (error_unit, '(a,a)') 'fo link token unresolved: ', trim(name)
        token = '-l'//trim(name)
    end subroutine resolve_link_token

    subroutine local_library_candidate(project_dir, name, candidate, found)
        !! Resolve a direct library artifact path from sibling checkout/build
        !! trees so local dependency builds do not require LIBRARY_PATH.
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: name
        character(len=1024), intent(out) :: candidate
        logical, intent(out) :: found

        character(len=1024) :: root_dir
        character(len=1024) :: local_root
        character(len=8) :: ext1, ext2
        integer :: slash, j
        logical :: exists

        found = .false.
        candidate = ''
        if (len_trim(project_dir) == 0) return

        call manifest_library_candidate(project_dir, name, candidate, found)
        if (found) return

        root_dir = trim(project_dir)
        if (root_dir(1:1) /= '/') then
            call get_environment_variable('PWD', root_dir)
            if (len_trim(root_dir) == 0) return
        end if

        if (len_trim(root_dir) > 1 .and. root_dir(len_trim(root_dir):len_trim(root_dir)) == '/') &
            root_dir = root_dir(1:len_trim(root_dir) - 1)
        slash = index(trim(root_dir), '/', back=.true.)
        if (slash > 0) root_dir = root_dir(1:slash - 1)

        if (is_macos()) then
            ext1 = '.dylib'
            ext2 = '.a'
        else
            ext1 = '.a'
            ext2 = '.so'
        end if

        do j = 1, 2
            if (j == 1) then
                local_root = trim(root_dir)//'/'//trim(name)//'/build'
            else
                local_root = trim(root_dir)//'/../'//trim(name)//'/build'
            end if

            candidate = trim(local_root)//'/lib'//trim(name)//trim(ext1)
            inquire (file=trim(candidate), exist=exists)
            if (exists) then
                found = .true.
                return
            end if

            candidate = trim(local_root)//'/'//trim(name)//trim(ext1)
            inquire (file=trim(candidate), exist=exists)
            if (exists) then
                found = .true.
                return
            end if

            candidate = trim(local_root)//'/lib'//trim(name)//trim(ext2)
            inquire (file=trim(candidate), exist=exists)
            if (exists) then
                found = .true.
                return
            end if

            candidate = trim(local_root)//'/'//trim(name)//trim(ext2)
            inquire (file=trim(candidate), exist=exists)
            if (exists) then
                found = .true.
                return
            end if
        end do
    end subroutine local_library_candidate

    subroutine manifest_library_candidate(project_dir, name, candidate, found)
        !! Resolve a link library from an explicit path dependency in the
        !! project's fpm.toml rather than guessing sibling directories. When a
        !! [dependencies] entry named `name` has a local path, its build tree is
        !! the authoritative artifact location.
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: name
        character(len=1024), intent(out) :: candidate
        logical, intent(out) :: found

        type(fpm_config_t), allocatable :: config
        character(len=512) :: dep_root
        character(len=8) :: ext1, ext2
        integer :: ierr, i

        found = .false.
        candidate = ''
        dep_root = ''

        call fpm_config_allocate(config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) return

        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) /= DEP_PATH) cycle
            if (trim(config%deps(i)%name) /= trim(name)) cycle
            call join_path(project_dir, trim(config%deps(i)%path), dep_root)
            exit
        end do
        if (len_trim(dep_root) == 0) return

        if (is_macos()) then
            ext1 = '.dylib'
            ext2 = '.a'
        else
            ext1 = '.a'
            ext2 = '.so'
        end if

        call probe_dep_artifact(dep_root, name, ext1, candidate, found)
        if (found) return
        call probe_dep_artifact(dep_root, name, ext2, candidate, found)
    end subroutine manifest_library_candidate

    subroutine probe_dep_artifact(dep_root, name, ext, candidate, found)
        character(len=*), intent(in) :: dep_root, name, ext
        character(len=1024), intent(out) :: candidate
        logical, intent(out) :: found

        found = .false.
        candidate = trim(dep_root)//'/build/lib'//trim(name)//trim(ext)
        inquire (file=trim(candidate), exist=found)
        if (found) return
        candidate = trim(dep_root)//'/build/'//trim(name)//trim(ext)
        inquire (file=trim(candidate), exist=found)
    end subroutine probe_dep_artifact

    logical function is_macos()
        !! True on macOS, detected by the always-present dynamic linker. Cached:
        !! the filesystem layout does not change within a run.
        logical, save :: checked = .false.
        logical, save :: mac = .false.

        if (.not. checked) then
            inquire (file='/usr/lib/dyld', exist=mac)
            checked = .true.
        end if
        is_macos = mac
    end function is_macos

    subroutine build_lib_search_dirs(dirs, n, n_env)
        !! Search dirs for external libs: LIBRARY_PATH and FO_LIBRARY_PATH
        !! (colon-separated, as understood by the toolchain) plus common system
        !! library locations. FO_LIBRARY_PATH lets a daemonized fo (MCP/LSP) that
        !! did not inherit LIBRARY_PATH still locate project-external archives.
        !! n_env returns how many leading dirs came from the environment; those
        !! are non-default paths where a static .a is preferred for runtime
        !! independence, while the trailing system dirs prefer the shared .so
        !! (always on the default runtime search path), avoiding broken links
        !! when a system .a needs transitive deps not listed in `link`.
        character(len=512), intent(out) :: dirs(:)
        integer, intent(out) :: n
        integer, intent(out) :: n_env

        n = 0
        call add_env_path_list('LIBRARY_PATH', dirs, n)
        call add_env_path_list('FO_LIBRARY_PATH', dirs, n)
        n_env = n
        if (is_macos()) call add_search_dir('/opt/homebrew/lib', dirs, n)
        call add_search_dir('/usr/local/lib', dirs, n)
        call add_search_dir('/usr/lib', dirs, n)
        call add_search_dir('/usr/lib64', dirs, n)
        call add_search_dir('/lib', dirs, n)
    end subroutine build_lib_search_dirs

    subroutine add_env_path_list(var, dirs, n)
        character(len=*), intent(in) :: var
        character(len=512), intent(inout) :: dirs(:)
        integer, intent(inout) :: n

        character(len=4096) :: val
        integer :: status, i, start

        call get_environment_variable(var, val, status=status)
        if (status /= 0 .or. len_trim(val) == 0) return
        start = 1
        do i = 1, len_trim(val) + 1
            if (i > len_trim(val) .or. val(i:i) == ':') then
                if (i > start) call add_search_dir(val(start:i - 1), dirs, n)
                start = i + 1
            end if
        end do
    end subroutine add_env_path_list

    subroutine add_search_dir(dir, dirs, n)
        character(len=*), intent(in) :: dir
        character(len=512), intent(inout) :: dirs(:)
        integer, intent(inout) :: n

        integer :: i

        if (len_trim(dir) == 0) return
        do i = 1, n
            if (trim(dirs(i)) == trim(dir)) return
        end do
        if (n >= size(dirs)) return
        n = n + 1
        dirs(n) = trim(dir)
    end subroutine add_search_dir

    subroutine file_basename(path, name)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: name

        character(len=512) :: base
        integer :: slash, dot, n

        n = len_trim(path)
        slash = index(path(1:n), '/', back=.true.)
        if (slash > 0) then
            base = path(slash + 1:n)
        else
            base = trim(path)
        end if
        dot = index(trim(base), '.', back=.true.)
        if (dot > 1) then
            name = base(1:dot - 1)
        else
            name = trim(base)
        end if
    end subroutine file_basename

    subroutine truncate_file(path)
        character(len=*), intent(in) :: path
        integer :: u, ios
        open (newunit=u, file=trim(path), status='replace', iostat=ios)
        if (ios == 0) close (u)
    end subroutine truncate_file

    subroutine package_source_flags(sources, flags, root_config, deps, n_deps, out)
        !! Manifest compile choices belong to the package defining a source.
        !! Profile/user flags stay shared; root manifest choices must not leak.
        character(len=*), intent(in) :: sources(:), flags
        type(fpm_config_t), intent(in) :: root_config
        type(resolved_src_t), intent(in) :: deps(:)
        integer, intent(in) :: n_deps
        character(len=*), intent(out) :: out(:)
        type(fpm_config_t), allocatable :: dep_config
        type(compiler_dialect_t) :: dialect
        character(len=:), allocatable :: shared, package_flags, prefix
        integer :: i, d, k, ierr, n
        integer, allocatable :: owner_lengths(:)

        allocate (owner_lengths(size(sources)))
        owner_lengths = 0
        out = flags
        shared = flags
        dialect = compiler_dialect(fc_command())
        package_flags = manifest_compile_flags(root_config, dialect)
        do while (len_trim(package_flags) > 0)
            k = index(package_flags, ' ')
            if (k == 0) k = len_trim(package_flags) + 1
            call remove_manifest_flag(shared, package_flags(:k - 1))
            package_flags = adjustl(package_flags(k:))
        end do
        call fpm_config_allocate(dep_config)
        do d = 1, n_deps
            call fpm_config_parse(trim(deps(d)%dir), dep_config, ierr)
            if (ierr /= 0) cycle
            package_flags = manifest_compile_flags(dep_config, dialect)
            prefix = trim(deps(d)%src_dir)//'/'
            n = len(prefix)
            do i = 1, size(sources)
                if (len_trim(sources(i)) < n) cycle
                if (sources(i)(:n) /= prefix) cycle
                if (n <= owner_lengths(i)) cycle
                owner_lengths(i) = n
                out(i) = trim(adjustl(package_flags))//' '//trim(shared)
            end do
        end do
    end subroutine package_source_flags

    function manifest_compile_flags(config, dialect) result(flags)
        type(fpm_config_t), intent(in) :: config
        type(compiler_dialect_t), intent(in) :: dialect
        character(len=:), allocatable :: flags, mapped
        integer :: i

        flags = ''
        if (config%implicit_typing) then
            mapped = dialect%translate_flag('-fno-implicit-none')
        else
            mapped = dialect%translate_flag('-fimplicit-none')
        end if
        if (len_trim(mapped) > 0) flags = mapped
        if (.not. config%implicit_external) then
            mapped = dialect%translate_flag('-Werror=implicit-interface')
            if (len_trim(mapped) > 0) flags = trim(flags)//' '//mapped
        end if
        if (config%source_form /= 'default') then
            mapped = dialect%translate_flag('-f'//trim(config%source_form)//'-form')
            if (len_trim(mapped) > 0) flags = trim(flags)//' '//mapped
        end if
        do i = 1, config%n_flags
            mapped = dialect%translate_flag(config%flags(i))
            if (len_trim(mapped) > 0) flags = trim(flags)//' '//mapped
        end do
        flags = trim(adjustl(flags))
    end function manifest_compile_flags

    subroutine remove_manifest_flag(flags, flag)
        !! Remove one manifest occurrence, retaining a repeated user override.
        character(len=:), allocatable, intent(inout) :: flags
        character(len=*), intent(in) :: flag
        character(len=:), allocatable :: padded, needle
        integer :: pos
        if (len_trim(flag) == 0) return
        padded = ' '//trim(flags)//' '
        needle = ' '//trim(flag)//' '
        pos = index(padded, needle)
        if (pos == 0) return
        flags = trim(adjustl(padded(:pos)//padded(pos + len(needle):)))
    end subroutine remove_manifest_flag

    subroutine merge_flags(config, flag_text)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(inout) :: flag_text
        character(len=1024) :: combined
        character(len=:), allocatable :: mapped
        type(compiler_dialect_t) :: dialect

        combined = ''
        dialect = compiler_dialect(fc_command())
        ! fpm openmp metapackage -> the selected compiler's OpenMP flag.
        if (config%openmp) combined = trim(dialect%openmp_flag())
        combined = trim(combined)//' '//manifest_compile_flags(config, dialect)
        if (len_trim(flag_text) > 0) then
            if (len_trim(combined) > 0) then
                combined = trim(combined)//' '//trim(flag_text)
            else
                combined = trim(flag_text)
            end if
        end if
        ! PIC last, and here rather than only in `config_flags_str`: this is
        ! the function the compile path actually calls (three call sites),
        ! while `config_flags_str` has no callers at all - the first time this
        ! flag was wired it went into the dead function and the linker kept
        ! refusing the archive with "recompile with -fPIC".
        mapped = dialect%pic_flag(config%pic)
        if (len_trim(mapped) > 0) then
            if (len_trim(combined) > 0) then
                combined = trim(combined)//' '//trim(mapped)
            else
                combined = trim(mapped)
            end if
        end if
        flag_text = combined
    end subroutine merge_flags

    function config_flags_str(config) result(s)
        type(fpm_config_t), intent(in) :: config
        character(len=1024) :: s
        character(len=:), allocatable :: mapped
        type(compiler_dialect_t) :: dialect
        integer :: i

        s = ''
        dialect = compiler_dialect(fc_command())
        if (config%openmp) s = trim(dialect%openmp_flag())
        do i = 1, config%n_flags
            mapped = dialect%translate_flag(config%flags(i))
            if (len_trim(mapped) == 0) cycle
            if (len_trim(s) > 0) then
                s = trim(s)//' '//trim(mapped)
            else
                s = trim(mapped)
            end if
        end do

        ! The manifest's debug-info budget goes last: a project that declares
        ! `debug-info = "g0"` means it, even against its own [build] flags.
        mapped = dialect%debug_info_flag(config%debug_info)
        if (len_trim(mapped) > 0) then
            if (len_trim(s) > 0) then
                s = trim(s)//' '//trim(mapped)
            else
                s = trim(mapped)
            end if
        end if

        ! PIC last, like the debug budget: the manifest states what this
        ! project needs to be able to link at all, so it must not be beaten by
        ! a stale [build] flag. Without it the linker refuses to fold the
        ! archive into a shared object at all:
        ! "relocation R_X86_64_PC32 ... can not be used when making a shared
        ! object; recompile with -fPIC".
        mapped = dialect%pic_flag(config%pic)
        if (len_trim(mapped) > 0) then
            if (len_trim(s) > 0) then
                s = trim(s)//' '//trim(mapped)
            else
                s = trim(mapped)
            end if
        end if
    end function config_flags_str

end module fo_gfortran_build
