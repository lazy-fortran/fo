module fo_gfortran_build
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, manifest_exe_name, &
        dep_kind, DEP_PATH
    use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS, MAX_NAME, MAX_PATH
    use fo_dag_bridge, only: build_dag_from_units
    use fo_dep_resolve, only: resolved_src_t, resolve_dep_srcs, &
        resolve_dev_dep_srcs, MAX_RESOLVED
    use fo_stat_memo, only: memo_save
    use fo_compdb, only: compdb_write
    use fx_dag, only: dag_t, dag_find_node, dag_topo_sort, dag_levels, MAX_NODES
    use fo_cache, only: cache_t, cache_init, cache_lookup, cache_key_for, &
        cache_restore_action, cache_store_action, hash_mod_file, &
        HASH_LEN, cache_digest, cache_file_digest, &
        cache_store_binary, cache_restore_binary, cache_binary_matches
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file, &
        clean_root_build_artifacts
    use fo_process, only: process_run_logged, &
        process_run_argv_logged, argv_push, argv_push_split, &
        argv_push_split_nl
    use fo_lock, only: lock_check
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_append_file, &
        fs_delete_suffix, fs_collect_files, fs_collect_mod_dirs, fs_copy_exec
    use fo_progress, only: progress_begin, progress_step, progress_end
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    integer, parameter :: MAX_DEP_DIRS = 64
    integer, parameter :: MAX_DEP_OBJS = 1024
    integer, parameter :: MAX_SRC_OBJS = 2048

    public :: gfortran_build, gfortran_test, gfortran_test_names
    public :: config_flags_str

contains

    subroutine gfortran_build(project_dir, log_file, exitcode, n_compiled, flags, &
            compiler_id, use_cache)
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode
        integer, intent(out), optional :: n_compiled
        character(len=*), intent(in), optional :: flags
        character(len=*), intent(in), optional :: compiler_id
        logical, intent(in), optional :: use_cache

        type(fpm_config_t) :: config
        integer :: ierr, n_dep_includes, n_dep_objs, n_src_objs, nc
        character(len=512) :: mod_dir, obj_dir, bin_dir
        character(len=512) :: dep_includes(MAX_DEP_DIRS)
        character(len=512) :: dep_objs(MAX_DEP_OBJS)
        character(len=512) :: src_objs(MAX_SRC_OBJS)
        logical :: is_prog_arr(MAX_SRC_OBJS)
        character(len=512) :: lf
        character(len=512) :: flag_text, compiler
        character(len=256) :: lock_message
        logical :: lock_ok

        lf = log_file
        if (len_trim(lf) == 0) lf = '/dev/null'
        flag_text = ''
        if (present(flags)) flag_text = flags
        if (present(compiler_id)) then
            compiler = compiler_id
        else
            call detect_compiler(compiler)
        end if

        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: no fpm.toml found in '//trim(project_dir)
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

        mod_dir = trim(project_dir)//'/build/fo/mod'
        obj_dir = trim(project_dir)//'/build/fo/obj'
        bin_dir = trim(project_dir)//'/build/fo/bin'
        call fs_make_dir(mod_dir)
        call fs_make_dir(obj_dir)
        call fs_make_dir(bin_dir)
        exitcode = 0
        if (exitcode /= 0) return

        call truncate_file(trim(lf))

        call guard_root_mod_shadow(project_dir, lf)

        call bootstrap_external_deps(project_dir, config, lf, exitcode)
        if (exitcode /= 0) return

        call find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs)

        nc = 0
        call compile_sources(project_dir, config%source_dir, config%app_dir, &
            mod_dir, obj_dir, dep_includes, n_dep_includes, lf, &
            src_objs, n_src_objs, is_prog_arr, exitcode, nc, &
            flag_text, compiler, use_cache)
        if (exitcode /= 0) return

        if (present(n_compiled)) n_compiled = nc

        call link_app_binaries(project_dir, config, bin_dir, src_objs, n_src_objs, &
            is_prog_arr, dep_objs, n_dep_objs, lf, exitcode, &
            flags=flag_text, use_cache=use_cache)
        call memo_save()
    end subroutine gfortran_build

    subroutine bootstrap_external_deps(project_dir, config, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, log_file
        type(fpm_config_t), intent(in) :: config
        integer, intent(out) :: exitcode

        character(len=512) :: dep_includes(MAX_DEP_DIRS)
        character(len=512) :: dep_objs(MAX_DEP_OBJS)
        integer :: n_dep_includes, n_dep_objs

        exitcode = 0
        if (.not. has_external_dep_closure(project_dir)) return

        call find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs)
        if (n_dep_objs > 0) return

        call run_fpm_bootstrap(project_dir, log_file, exitcode)
    end subroutine bootstrap_external_deps

    logical function has_external_dep_closure(project_dir) result(found)
        character(len=*), intent(in) :: project_dir
        type(resolved_src_t) :: deps(MAX_RESOLVED)
        integer :: n_deps, n_unres, ierr

        found = .false.
        call resolve_dep_srcs(project_dir, deps, n_deps, n_unres, ierr)
        if (ierr == 0 .and. n_unres > 0) found = .true.
    end function has_external_dep_closure

    subroutine run_fpm_bootstrap(project_dir, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode

        character(len=:), allocatable :: packed
        character(len=4096) :: bootstrap_env, lib_dir, lib_path, prior_paths
        integer :: n_args
        integer :: stat, cut
        logical :: has_local_liric

        bootstrap_env = ''
        call local_library_candidate(project_dir, 'liric', lib_path, has_local_liric)
        if (has_local_liric) then
            cut = index(trim(lib_path), '/', back=.true.)
            if (cut > 0) lib_dir = trim(lib_path(1:cut - 1))

            if (len_trim(lib_dir) > 0) then
                call get_environment_variable('LIBRARY_PATH', prior_paths, status=stat)
                if (stat == 0 .and. len_trim(prior_paths) > 0) then
                    bootstrap_env = 'LIBRARY_PATH='//trim(lib_dir)//':'//trim(prior_paths)
                else
                    bootstrap_env = 'LIBRARY_PATH='//trim(lib_dir)
                end if
            end if
        end if

        n_args = 0
        call argv_push(packed, n_args, 'fpm')
        call argv_push(packed, n_args, 'build')
        call process_run_argv_logged(project_dir, packed, n_args, log_file, &
            .true., build_timeout_seconds(), exitcode, &
            env_extra=trim(bootstrap_env))
        if (exitcode == 0) return

        write (error_unit, '(a)') 'fo: fpm bootstrap failed for git/registry dependencies'
        if (len_trim(log_file) > 0) then
            write (error_unit, '(a)') 'fo: see '//trim(log_file)
        end if
    end subroutine run_fpm_bootstrap

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

        type(fpm_config_t) :: config
        integer :: ierr, n_dep_includes, n_dep_objs, n_lib_objs
        character(len=512) :: mod_dir, obj_dir, bin_dir
        character(len=512) :: dep_includes(MAX_DEP_DIRS)
        character(len=512) :: dep_objs(MAX_DEP_OBJS)
        character(len=512) :: lib_objs(MAX_SRC_OBJS)
        character(len=512) :: lf, flag_text
        character(len=128) :: no_names(1)
        logical :: slow, bonly

        lf = log_file
        if (len_trim(lf) == 0) lf = '/dev/null'
        slow = .false.
        if (present(include_slow)) slow = include_slow
        flag_text = ''
        if (present(flags)) flag_text = flags
        bonly = .false.
        if (present(build_only)) bonly = build_only
        if (present(n_compiled)) n_compiled = 0

        call gfortran_build(project_dir, lf, exitcode, flags=flag_text, &
            use_cache=use_cache)
        if (exitcode /= 0) return

        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            exitcode = 1
            return
        end if
        call merge_flags(config, flag_text)

        mod_dir = trim(project_dir)//'/build/fo/mod'
        obj_dir = trim(project_dir)//'/build/fo/obj'
        bin_dir = trim(project_dir)//'/build/fo/bin'

        call find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs)
        call collect_lib_objs(obj_dir, lib_objs, n_lib_objs)

        call compile_and_run_tests(project_dir, config%test_dir, mod_dir, obj_dir, &
            bin_dir, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs, lib_objs, n_lib_objs, &
            config%link_libs, config%n_link_libs, lf, &
            no_names, 0, slow, exitcode, n_compiled, &
            flags=flag_text, &
            build_only=bonly, use_cache=use_cache)
        call memo_save()
    end subroutine gfortran_test

    subroutine gfortran_test_names(project_dir, names, n_names, log_file, &
            exitcode, include_slow, n_compiled, flags, use_cache)
        character(len=*), intent(in) :: project_dir, log_file
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        integer, intent(out), optional :: n_compiled
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache

        type(fpm_config_t) :: config
        integer :: ierr, n_dep_includes, n_dep_objs, n_lib_objs
        character(len=512) :: mod_dir, obj_dir, bin_dir
        character(len=512) :: dep_includes(MAX_DEP_DIRS)
        character(len=512) :: dep_objs(MAX_DEP_OBJS)
        character(len=512) :: lib_objs(MAX_SRC_OBJS)
        character(len=512) :: lf, flag_text
        logical :: slow

        lf = log_file
        if (len_trim(lf) == 0) lf = '/dev/null'
        slow = .false.
        if (present(include_slow)) slow = include_slow
        flag_text = ''
        if (present(flags)) flag_text = flags
        if (present(n_compiled)) n_compiled = 0

        call gfortran_build(project_dir, lf, exitcode, flags=flag_text, &
            use_cache=use_cache)
        if (exitcode /= 0) return

        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            exitcode = 1
            return
        end if
        call merge_flags(config, flag_text)

        mod_dir = trim(project_dir)//'/build/fo/mod'
        obj_dir = trim(project_dir)//'/build/fo/obj'
        bin_dir = trim(project_dir)//'/build/fo/bin'

        call find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs)
        call collect_lib_objs(obj_dir, lib_objs, n_lib_objs)

        call compile_and_run_tests(project_dir, config%test_dir, mod_dir, obj_dir, &
            bin_dir, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs, lib_objs, n_lib_objs, &
            config%link_libs, config%n_link_libs, lf, &
            names, n_names, slow, exitcode, n_compiled, &
            flags=flag_text, use_cache=use_cache)
    end subroutine gfortran_test_names

    subroutine find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
            dep_objs, n_dep_objs)
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(in) :: config
        character(len=512), intent(out) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(out) :: n_dep_includes
        character(len=512), intent(out) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(out) :: n_dep_objs

        character(len=512) :: line
        integer :: i, j
        integer :: n_obj_seen
        character(len=512) :: obj_basenames(MAX_DEP_OBJS)
        character(len=512) :: obj_key
        character(len=512) :: found(MAX_DEP_OBJS)
        integer :: n_found
        integer :: slash
        character(len=8) :: suffixes(2)

        n_dep_includes = 0
        n_dep_objs = 0
        n_obj_seen = 0
        if (config%n_deps == 0 .and. config%n_dev_deps == 0) return

        ! Every directory holding a .mod under build/ is an include candidate:
        ! the project's own gfortran_* profile dir and each dependency's mod
        ! dir. Replaces grep over compile_commands.json plus find -printf %h.
        call fs_collect_mod_dirs(trim(project_dir)//'/build', dep_includes, &
            n_dep_includes)

        suffixes(1) = '.f90.o'
        suffixes(2) = '.c.o'
        do i = 1, config%n_deps
            ! A path dependency is compiled natively (Fortran and C) into
            ! build/fo/obj, so its objects are already linked from src_objs.
            ! Harvesting the same modules from a coexisting fpm build/gfortran_*
            ! tree would link every dependency symbol twice. Only git/registry
            ! deps, bootstrapped through fpm, are collected from that tree.
            if (dep_kind(config%deps(i)) == DEP_PATH) cycle
            ! fpm names a dependency's compiled objects from the relative path to
            ! its source: git deps under build/dependencies become
            ! build_dependencies_<dep>_src_*, path deps (path = "../dep") become
            ! .._<dep>_src_*. Both share the _<dep>_src_ infix. Scan every
            ! gfortran_* profile dir directly and dedup by module identity.
            do j = 1, 2
                call fs_collect_files(trim(project_dir)//'/build', &
                    '_'//trim(config%deps(i)%name)//'_src_', &
                    trim(suffixes(j)), '/gfortran_', found, &
                    n_found)
                call add_dep_objs(found, n_found, dep_objs, n_dep_objs, &
                    obj_basenames, n_obj_seen)
            end do
        end do
    end subroutine find_dep_artifacts

    subroutine add_dep_objs(found, n_found, dep_objs, n_dep_objs, &
            obj_basenames, n_obj_seen)
        !! Append collected dependency library objects to dep_objs,
        !! deduplicating by module identity so the same module built under
        !! different prefixes or profiles links once. find_dep_artifacts only
        !! collects objects carrying the dependency's '_<dep>_src_' library
        !! marker, so fpm's app/test objects (named '_<dep>_app_' / '_<dep>_test_')
        !! never reach here and need no filtering.
        character(len=512), intent(in) :: found(:)
        integer, intent(in) :: n_found
        character(len=512), intent(inout) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(inout) :: n_dep_objs
        character(len=512), intent(inout) :: obj_basenames(MAX_DEP_OBJS)
        integer, intent(inout) :: n_obj_seen
        character(len=512) :: line, obj_key, base
        integer :: k, slash

        do k = 1, n_found
            line = found(k)
            if (len_trim(line) == 0) cycle
            slash = index(trim(line), '/', back=.true.)
            base = line(slash + 1:)
            obj_key = dep_object_module_key(base)
            if (any(obj_basenames(1:n_obj_seen) == obj_key)) cycle
            if (n_dep_objs < MAX_DEP_OBJS) then
                n_dep_objs = n_dep_objs + 1
                dep_objs(n_dep_objs) = trim(line)
                n_obj_seen = n_obj_seen + 1
                obj_basenames(n_obj_seen) = obj_key
            end if
        end do
    end subroutine add_dep_objs

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

    subroutine compile_sources(project_dir, src_dir, app_dir, mod_dir, obj_dir, &
            dep_includes, n_dep_includes, log_file, &
            src_objs, n_src_objs, is_prog_arr, exitcode, &
            n_compiled, flags, compiler, use_cache)
        character(len=*), intent(in) :: project_dir, src_dir, app_dir
        character(len=*), intent(in) :: mod_dir, obj_dir, log_file
        character(len=*), intent(in) :: flags, compiler
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(in) :: n_dep_includes
        character(len=512), intent(out) :: src_objs(MAX_SRC_OBJS)
        integer, intent(out) :: n_src_objs
        logical, intent(out) :: is_prog_arr(MAX_SRC_OBJS)
        integer, intent(out) :: exitcode, n_compiled
        logical, intent(in), optional :: use_cache

        type(scan_unit_t), allocatable :: units_a(:), units_b(:), all_units(:)
        integer :: na, nb, n_all, i, ii, ierr, node_id
        type(dag_t) :: dag
        character(len=MAX_PATH), allocatable :: filenames(:)
        logical, allocatable :: is_prog(:), is_test_arr(:)
        integer, allocatable :: topo_order(:), node_levels(:)
        integer :: n_order, n_levels, lvl, total_source
        logical :: has_cycle, restored
        logical :: allow_cache
        character(len=512) :: obj_path
        character(len=4096) :: includes_flag
        character(len=512) :: c_line
        character(len=512), allocatable :: cfiles(:)
        character(len=MAX_PATH), allocatable :: compdb_sources(:)
        character(len=512), allocatable :: compdb_objects(:)
        integer :: n_cfiles, ic
        integer :: n_compdb
        type(resolved_src_t) :: deps(MAX_RESOLVED)
        integer :: n_deps_resolved
        integer :: ii_failed

        type(cache_t) :: c
        integer :: cache_ierr
        character(len=HASH_LEN), allocatable :: old_mod_keys(:), new_mod_keys(:)
        character(len=HASH_LEN) :: dep_keys(64), source_key
        character(len=HASH_LEN) :: output_id
        integer :: n_dep
        integer, allocatable :: compile_nodes(:)
        character(len=HASH_LEN), allocatable :: compile_keys(:)
        integer, allocatable :: compile_exits(:)
        character(len=512), allocatable :: per_logs(:)
        integer :: n_compile
        character(len=MAX_PATH) :: fname_local
        character(len=512) :: per_log_local

        n_src_objs = 0
        is_prog_arr = .false.
        exitcode = 0
        n_compiled = 0

        allocate (units_a(MAX_UNITS), units_b(MAX_UNITS), all_units(MAX_UNITS))
        allocate (filenames(MAX_NODES), is_prog(MAX_NODES), is_test_arr(MAX_NODES))
        allocate (topo_order(MAX_NODES), node_levels(MAX_NODES))
        allocate (old_mod_keys(MAX_NODES), new_mod_keys(MAX_NODES))
        allocate (compile_nodes(MAX_NODES), compile_keys(MAX_NODES))
        allocate (compile_exits(MAX_NODES), per_logs(MAX_NODES))
        allocate (compdb_sources(MAX_NODES), compdb_objects(MAX_NODES))

        call scan_dir(trim(project_dir)//'/'//trim(src_dir), units_a, na, ierr)
        call scan_dir(trim(project_dir)//'/'//trim(app_dir), units_b, nb, ierr)

        n_all = na
        do i = 1, nb
            if (n_all < MAX_UNITS) then
                n_all = n_all + 1
                all_units(n_all) = units_b(i)
            end if
        end do
        do i = 1, na
            all_units(i) = units_a(i)
        end do

        ! Fold path-dependency library sources into the same unit set, so the
        ! module DAG spans packages and the existing content-addressed compile
        ! loop builds deps once and caches them like first-party modules. Only
        ! modules actually reached by a `use` edge get compiled, so an unused
        ! dep (e.g. a declared-but-unreferenced stdlib) costs nothing.
        call add_dep_sources(project_dir, all_units, n_all, deps, n_deps_resolved)

        call build_dag_from_units(all_units, n_all, dag, filenames, is_test_arr, is_prog)
        call dag_topo_sort(dag, topo_order, n_order, has_cycle)
        call dag_levels(dag, topo_order, n_order, node_levels, n_levels)
        call remove_shadow_mods(project_dir, dag)
        call make_includes_flag(mod_dir, dep_includes, n_dep_includes, includes_flag)

        old_mod_keys = ''
        new_mod_keys = ''
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
            total_source = total_source + 1
            n_compdb = n_compdb + 1
            compdb_sources(n_compdb) = filenames(node_id)
            call make_obj_path(filenames(node_id), project_dir, obj_dir, &
                compdb_objects(n_compdb))
        end do
        call progress_begin('build', total_source)

        if (cache_ierr == 0) then
            do lvl = 0, n_levels - 1
                n_compile = 0
                compile_exits = 0
                ii_failed = 0

                do i = 1, n_order
                    if (node_levels(i) /= lvl) cycle
                    node_id = topo_order(i)
                    if (is_test_arr(node_id)) cycle
                    if (len_trim(filenames(node_id)) == 0) cycle

                    call collect_dep_keys_source_order(all_units, n_all, dag, node_id, &
                        new_mod_keys, dep_includes, &
                        n_dep_includes, dep_keys, n_dep, &
                        restored)
                    if (.not. restored) then
                        n_compile = n_compile + 1
                        compile_nodes(n_compile) = node_id
                        compile_keys(n_compile) = ''
                        cycle
                    end if
                    source_key = cache_key_for(filenames(node_id), compiler, flags, &
                        dep_keys, n_dep)

                    call make_obj_path(filenames(node_id), project_dir, obj_dir, obj_path)
                    if (.not. source_may_emit_smod(filenames(node_id)) .and. &
                        cache_lookup(c, source_key)) then
                        call cache_restore_action(c, source_key, obj_path, mod_dir, &
                            restored)
                        if (restored) then
                            call get_mod_key(dag%nodes(node_id)%label, mod_dir, &
                                new_mod_keys(node_id))
                            call progress_step()
                            cycle
                        end if
                    end if
                    n_compile = n_compile + 1
                    compile_nodes(n_compile) = node_id
                    compile_keys(n_compile) = source_key
                end do

                do ii = 1, n_compile
                    call make_tmpfile('fo_compile', per_logs(ii))
                end do

                !$omp parallel do schedule(dynamic) &
                !$omp private(node_id, obj_path, fname_local, per_log_local)
                do ii = 1, n_compile
                    node_id = compile_nodes(ii)
                    fname_local = filenames(node_id)
                    per_log_local = per_logs(ii)
                    call make_obj_path(fname_local, project_dir, obj_dir, obj_path)
                    call compile_f90(project_dir, fname_local, obj_path, &
                        with_user_flags(includes_flag, flags), &
                        per_log_local, compile_exits(ii))
                    call progress_step()
                end do
                !$omp end parallel do

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
                        new_mod_keys(node_id))
                    if (len_trim(compile_keys(ii)) > 0) then
                        call cache_store_action(c, compile_keys(ii), obj_path, mod_dir, &
                            dag%nodes(node_id)%label, output_id, cache_ierr)
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
                call make_obj_path(filenames(node_id), project_dir, obj_dir, obj_path)
                call compile_f90(project_dir, filenames(node_id), obj_path, &
                    with_user_flags(includes_flag, flags), log_file, &
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
            fc_command(), fc_base_flags(), includes_flag, flags)

        do i = 1, n_order
            node_id = topo_order(i)
            if (len_trim(filenames(node_id)) == 0) cycle
            if (is_test_arr(node_id)) cycle
            call make_obj_path(filenames(node_id), project_dir, obj_dir, obj_path)
            if (n_src_objs < MAX_SRC_OBJS) then
                n_src_objs = n_src_objs + 1
                src_objs(n_src_objs) = obj_path
                is_prog_arr(n_src_objs) = is_prog(node_id)
            end if
        end do

        allocate (cfiles(MAX_SRC_OBJS))
        call fs_collect_files(trim(project_dir)//'/'//trim(src_dir), '', '.c', &
            '', cfiles, n_cfiles)
        do ic = 1, n_cfiles
            c_line = cfiles(ic)
            if (len_trim(c_line) == 0) cycle
            call make_obj_path(trim(c_line), project_dir, obj_dir, obj_path)
            call compile_c(trim(c_line), obj_path, log_file, exitcode)
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

    logical function source_may_emit_smod(path) result(may_emit)
        character(len=*), intent(in) :: path

        character(len=512) :: line, lower
        integer :: u, ios

        may_emit = .false.
        open (newunit=u, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            lower = adjustl(line)
            call lowercase_inplace(lower)
            if (starts_with_submodule(lower) .or. &
                index(lower, 'module subroutine') == 1 .or. &
                index(lower, 'module function') == 1) then
                may_emit = .true.
                exit
            end if
        end do
        close (u)
    end function source_may_emit_smod

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
        !! dep's C sources for linking.
        character(len=*), intent(in) :: project_dir
        type(scan_unit_t), intent(inout) :: all_units(MAX_UNITS)
        integer, intent(inout) :: n_all
        type(resolved_src_t), intent(out) :: deps(MAX_RESOLVED)
        integer, intent(out) :: n_deps

        type(scan_unit_t), allocatable :: ud(:)
        integer :: n_unres, ierr, d, j, nu

        n_deps = 0
        call resolve_dep_srcs(project_dir, deps, n_deps, n_unres, ierr)
        if (ierr /= 0 .or. n_deps == 0) return

        allocate (ud(MAX_UNITS))
        do d = 1, n_deps
            call scan_dir(trim(deps(d)%src_dir), ud, nu, ierr)
            if (ierr /= 0) cycle
            do j = 1, nu
                if (ud(j)%is_program) cycle
                if (n_all < MAX_UNITS) then
                    n_all = n_all + 1
                    all_units(n_all) = ud(j)
                end if
            end do
        end do
        deallocate (ud)
    end subroutine add_dep_sources

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
            call fs_collect_files(trim(deps(d)%src_dir), '', '.c', '', &
                cfiles, n_cfiles)
            do ic = 1, n_cfiles
                c_line = cfiles(ic)
                if (len_trim(c_line) == 0) cycle
                call make_obj_path(trim(c_line), project_dir, obj_dir, obj_path)
                call compile_c(trim(c_line), obj_path, log_file, exitcode)
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

    subroutine add_external_dep_keys(units, n_units, dag, node_id, &
            dep_includes, n_dep_includes, dep_keys, n_dep)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units, node_id, n_dep_includes
        type(dag_t), intent(in) :: dag
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        character(len=HASH_LEN), intent(inout) :: dep_keys(64)
        integer, intent(inout) :: n_dep

        integer :: i, j
        character(len=MAX_NAME) :: node_name
        character(len=512) :: modpath
        logical :: found

        node_name = dag%nodes(node_id)%label(1:MAX_NAME)
        do i = 1, n_units
            if (trim(units(i)%module_name) /= trim(node_name) .and. &
                trim(units(i)%program_name) /= trim(node_name)) cycle

            do j = 1, units(i)%n_deps
                if (dag_find_node(dag, units(i)%deps(j)) > 0) cycle
                if (n_dep >= 64) return
                call find_dep_mod_file(units(i)%deps(j), dep_includes, &
                    n_dep_includes, modpath, found)
                if (.not. found) cycle
                n_dep = n_dep + 1
                call hash_mod_file(modpath, dep_keys(n_dep))
            end do
            return
        end do
    end subroutine add_external_dep_keys

    subroutine collect_dep_keys_source_order(units, n_units, dag, node_id, &
            mod_keys, dep_includes, &
            n_dep_includes, dep_keys, n_dep, complete)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units, node_id, n_dep_includes
        type(dag_t), intent(in) :: dag
        character(len=HASH_LEN), intent(in) :: mod_keys(MAX_NODES)
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        character(len=HASH_LEN), intent(out) :: dep_keys(64)
        integer, intent(out) :: n_dep
        logical, intent(out) :: complete

        integer :: i, j, dep_id
        character(len=MAX_NAME) :: node_name
        character(len=512) :: modpath
        logical :: found

        n_dep = 0
        dep_keys = ''
        complete = .true.
        node_name = dag%nodes(node_id)%label(1:MAX_NAME)
        do i = 1, n_units
            if (trim(units(i)%module_name) /= trim(node_name) .and. &
                trim(units(i)%program_name) /= trim(node_name)) cycle

            do j = 1, units(i)%n_deps
                if (n_dep >= 64) return
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

    subroutine get_mod_key(label, mod_dir, key)
        character(len=*), intent(in) :: label, mod_dir
        character(len=HASH_LEN), intent(out) :: key

        character(len=MAX_NAME) :: lower_label
        character(len=512) :: modpath
        integer :: i

        lower_label = label
        do i = 1, len_trim(lower_label)
            if (lower_label(i:i) >= 'A' .and. lower_label(i:i) <= 'Z') &
                lower_label(i:i) = achar(iachar(lower_label(i:i)) + 32)
        end do

        modpath = trim(mod_dir)//'/'//trim(lower_label)//'.mod'
        call hash_mod_file(modpath, key)
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

    subroutine link_app_binaries(project_dir, config, bin_dir, src_objs, n_src_objs, &
            is_prog_arr, dep_objs, n_dep_objs, log_file, exitcode, &
            flags, use_cache)
        character(len=*), intent(in) :: project_dir, bin_dir, log_file
        type(fpm_config_t), intent(in) :: config
        character(len=512), intent(in) :: src_objs(MAX_SRC_OBJS)
        integer, intent(in) :: n_src_objs
        logical, intent(in) :: is_prog_arr(MAX_SRC_OBJS)
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache

        integer :: i, n_lib
        character(len=512) :: lib_objs(MAX_SRC_OBJS)
        character(len=512) :: prog_obj, bin_path
        character(len=128) :: prog_name, manifest_name
        character(len=512) :: link_flags
        type(cache_t) :: c
        integer :: cache_ierr
        character(len=HASH_LEN) :: base_digest
        logical :: allow_cache

        link_flags = ''
        if (present(flags)) link_flags = flags
        exitcode = 0
        n_lib = 0
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

        do i = 1, n_src_objs
            if (.not. is_prog_arr(i)) cycle
            prog_obj = src_objs(i)
            ! fpm naming: app/main.f90 takes the package name; every other app
            ! program takes its own source stem. Selecting by stem (not source
            ! order) keeps `main` mapped to the package binary even when another
            ! app source sorts ahead of it.
            call app_prog_stem(prog_obj, config%app_dir, prog_name)
            manifest_name = manifest_exe_name(config, config%app_dir, prog_name)
            if (len_trim(manifest_name) > 0) then
                prog_name = trim(manifest_name)
            else if (prog_name == 'main' .and. len_trim(config%name) > 0) then
                prog_name = trim(config%name)
            end if
            bin_path = trim(bin_dir)//'/'//trim(prog_name)
            call link_binary(project_dir, prog_obj, lib_objs, n_lib, dep_objs, n_dep_objs, &
                config%link_libs, config%n_link_libs, bin_path, &
                log_file, exitcode, link_flags, c, base_digest)
            if (exitcode /= 0) return
        end do
    end subroutine link_app_binaries

    subroutine link_base_digest(project_dir, lib_objs, n_lib, dep_objs, n_dep, link_libs, &
            n_link, digest)
        !! One digest standing for every shared link input: the library objects,
        !! the dependency objects, and the resolved external archives (hashed by
        !! content so a rebuilt liric.a relinks). Computed once per build; each
        !! per-program link folds in only its own object, so the link key is
        !! cheap and correct.
        character(len=*), intent(in) :: project_dir
        character(len=512), intent(in) :: lib_objs(MAX_SRC_OBJS)
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
        integer :: dot, plen

        call file_basename(obj_path, base) ! drops dir and trailing '.o'
        dot = index(trim(base), '.', back=.true.)
        if (dot > 1) base = base(1:dot - 1) ! drop .f90 / .F90
        prefix = trim(app_dir)//'_'
        plen = len_trim(prefix)
        if (len_trim(base) > plen .and. base(1:plen) == trim(prefix)) &
            base = base(plen + 1:)
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
        character(len=512), intent(in) :: lib_objs(MAX_SRC_OBJS)
        integer, intent(in) :: n_lib_objs
        character(len=128), intent(in) :: link_libs(*)
        integer, intent(in) :: n_link_libs
        character(len=128), intent(in) :: selected_names(:)
        integer, intent(in) :: n_selected
        logical, intent(in) :: include_slow
        integer, intent(out) :: exitcode
        integer, intent(out), optional :: n_compiled
        character(len=*), intent(in), optional :: flags
        ! build_only: compile and link the test binaries but do not run them.
        ! Used by `fo build` so build/fo/bin/test_* stay current with the
        ! sources, instead of going stale until the next `fo test`.
        logical, intent(in), optional :: build_only
        logical, intent(in), optional :: use_cache

        type(scan_unit_t), allocatable :: tunits(:)
        integer :: n_tests, i, ierr, node_id, n_run
        type(dag_t) :: dag
        character(len=MAX_PATH), allocatable :: filenames(:)
        logical, allocatable :: is_prog(:), is_test_arr(:)
        integer, allocatable :: topo_order(:)
        integer, allocatable :: run_nodes(:), run_exits(:)
        character(len=512), allocatable :: run_logs(:)
        character(len=HASH_LEN), allocatable :: run_keys(:)
        logical, allocatable :: run_compiled(:)
        logical, allocatable :: ran(:), flaky(:)
        real, allocatable :: run_secs(:)
        integer :: test_timeout, test_warn
        integer(8) :: clk0, clk1, clk_rate
        integer :: n_order
        logical :: has_cycle, restored, bonly
        logical :: allow_cache
        character(len=512) :: obj_path, bin_path
        character(len=4096) :: incl_flag
        character(len=128) :: tname
        character(len=MAX_PATH) :: fname_local
        character(len=512) :: log_local
        type(cache_t) :: c
        integer :: cache_ierr
        character(len=256) :: compiler
        character(len=HASH_LEN) :: dep_keys(64), output_key, lib_hash
        character(len=HASH_LEN) :: link_base
        integer :: n_dep, n_test_includes
        character(len=512) :: test_includes(MAX_DEP_DIRS)
        character(len=1024) :: test_flags
        character(len=512), allocatable :: helper_objs(:)
        character(len=512), allocatable :: all_lib_objs(:)
        integer :: n_helper_objs, n_all_lib
        logical :: in_lib
        type(resolved_src_t) :: devsrcs(MAX_RESOLVED)
        integer :: n_dev, d, k, nud
        type(scan_unit_t), allocatable :: udev(:)

        bonly = .false.
        if (present(build_only)) bonly = build_only
        test_flags = ''
        if (present(flags)) test_flags = flags
        exitcode = 0
        if (present(n_compiled)) n_compiled = 0
        allocate (tunits(MAX_UNITS))
        allocate (filenames(MAX_NODES), is_prog(MAX_NODES), is_test_arr(MAX_NODES))
        allocate (topo_order(MAX_NODES))
        allocate (run_nodes(MAX_NODES), run_exits(MAX_NODES), run_logs(MAX_NODES))
        allocate (run_keys(MAX_NODES))
        allocate (run_compiled(MAX_NODES), run_secs(MAX_NODES))
        allocate (ran(MAX_NODES), flaky(MAX_NODES))
        ran = .false.
        flaky = .false.
        run_secs = 0.0
        test_timeout = test_timeout_seconds()
        test_warn = test_warn_seconds(test_timeout)
        call scan_dir(trim(project_dir)//'/'//trim(test_dir), tunits, n_tests, ierr)
        if (n_tests == 0) return

        call resolve_dev_dep_srcs(project_dir, devsrcs, n_dev, ierr)
        if (ierr == 0 .and. n_dev > 0) then
            allocate (udev(MAX_UNITS))
            do d = 1, n_dev
                call scan_dir(trim(devsrcs(d)%src_dir), udev, nud, ierr)
                if (ierr /= 0) cycle
                do k = 1, nud
                    if (udev(k)%is_program) cycle
                    if (n_tests >= MAX_UNITS) exit
                    n_tests = n_tests + 1
                    tunits(n_tests) = udev(k)
                end do
            end do
            deallocate (udev)
        end if

        call build_dag_from_units(tunits, n_tests, dag, filenames, is_test_arr, is_prog)
        call dag_topo_sort(dag, topo_order, n_order, has_cycle)
        call make_includes_flag(mod_dir, dep_includes, n_dep_includes, incl_flag)
        if (present(flags) .and. len_trim(flags) > 0) then
            incl_flag = with_user_flags(incl_flag, flags)
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
        do i = 1, n_order
            node_id = topo_order(i)
            if (len_trim(filenames(node_id)) == 0) cycle
            if (.not. is_prog(node_id)) cycle
            call file_basename(filenames(node_id), tname)
            if (.not. include_slow .and. is_slow_name(tname)) cycle
            if (n_selected > 0 .and. .not. selected_test(tname, selected_names, &
                n_selected)) cycle
            n_run = n_run + 1
            run_nodes(n_run) = node_id
            call make_tmpfile('fo_test_case', run_logs(n_run))
        end do

        ! Module-only files under test/ are helper modules a test program uses
        ! (fpm compiles them and links their objects into the test executable).
        ! Compile only those reachable from a selected test, in dependency order,
        ! so their .mod files exist before the test programs compile, then fold
        ! their objects into the link line.
        allocate (helper_objs(MAX_SRC_OBJS))
        call compile_test_helpers(project_dir, obj_dir, dag, filenames, is_prog, &
            topo_order, n_order, run_nodes, n_run, incl_flag, log_file, &
            helper_objs, n_helper_objs, exitcode)
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
            dep_objs, n_dep_objs, link_libs, n_link_libs, link_base)

        do i = 1, n_run
            node_id = run_nodes(i)
            n_dep = 0
            call add_external_dep_keys(tunits, n_tests, dag, node_id, &
                test_includes, n_test_includes, &
                dep_keys, n_dep)
            if (len_trim(lib_hash) > 0 .and. n_dep < size(dep_keys)) then
                n_dep = n_dep + 1
                dep_keys(n_dep) = lib_hash
            end if
            run_keys(i) = cache_key_for(filenames(node_id), compiler, &
                test_flags, dep_keys, n_dep)
        end do

        run_exits = 0
        run_compiled = .false.
        if (bonly) then
            call progress_begin('build tests', n_run)
        else
            call progress_begin('test', n_run)
        end if
        !$omp parallel do schedule(dynamic) private(node_id, fname_local, &
        !$omp& obj_path, bin_path, tname, log_local, restored, clk0, clk1, &
        !$omp& clk_rate)
        do i = 1, n_run
            node_id = run_nodes(i)
            fname_local = filenames(node_id)
            log_local = run_logs(i)
            call make_obj_path(fname_local, project_dir, obj_dir, obj_path)
            restored = .false.
            if (cache_ierr == 0 .and. cache_lookup(c, run_keys(i))) then
                call cache_restore_action(c, run_keys(i), obj_path, mod_dir, &
                    restored)
            end if
            if (.not. restored) then
                call compile_f90(project_dir, fname_local, obj_path, incl_flag, &
                    log_local, run_exits(i))
                run_compiled(i) = run_exits(i) == 0
            end if
            if (run_exits(i) == 0) then
                call file_basename(fname_local, tname)
                bin_path = trim(bin_dir)//'/'//trim(tname)
                call link_binary(project_dir, obj_path, all_lib_objs, n_all_lib, dep_objs, &
                    n_dep_objs, link_libs, n_link_libs, bin_path, log_local, &
                    run_exits(i), test_flags, c, link_base)
            end if
            if (run_exits(i) == 0 .and. .not. bonly) then
                ran(i) = .true.
                call system_clock(clk0, clk_rate)
                call process_run_logged('', bin_path, log_local, .true., &
                    test_timeout, run_exits(i))
                call system_clock(clk1)
                if (clk_rate > 0) run_secs(i) = real(clk1 - clk0) / real(clk_rate)
            end if
            call progress_step()
        end do
        !$omp end parallel do
        call progress_end()

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
                call file_basename(filenames(run_nodes(i)), tname)
                bin_path = trim(bin_dir)//'/'//trim(tname)
                call process_run_logged('', bin_path, run_logs(i), .true., &
                    test_timeout, run_exits(i))
                if (run_exits(i) == 0) flaky(i) = .true.
            end do
        end if

        do i = 1, n_run
            call append_log_file(trim(run_logs(i)), log_file)
            if (run_exits(i) /= 0) then
                call file_basename(filenames(run_nodes(i)), tname)
                if (run_exits(i) == 124) then
                    call append_timeout_status(log_file, tname, test_timeout)
                else
                    call append_test_status(log_file, tname, run_exits(i))
                end if
                if (exitcode == 0) exitcode = run_exits(i)
            else if (cache_ierr == 0 .and. run_compiled(i)) then
                call make_obj_path(filenames(run_nodes(i)), project_dir, obj_dir, &
                    obj_path)
                call cache_store_action(c, run_keys(i), obj_path, mod_dir, '', &
                    output_key, cache_ierr)
                if (present(n_compiled)) n_compiled = n_compiled + 1
            end if
            call delete_tmpfile(run_logs(i))
        end do

        do i = 1, n_run
            if (flaky(i)) then
                call file_basename(filenames(run_nodes(i)), tname)
                call append_flaky_status(log_file, tname)
            end if
        end do

        ! Structured TEST_RESULT lines for downstream consumers (CLI, MCP, agents)
        if (.not. bonly) then
            do i = 1, n_run
                call file_basename(filenames(run_nodes(i)), tname)
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
                else if (run_compiled(i)) then
                    call write_test_result_line(log_file, tname, 'SKIP', '-', 0.0)
                end if
            end do
        end if

        if (.not. bonly) &
            call warn_slow_tests(filenames, run_nodes, run_exits, run_secs, n_run, &
            test_warn, log_file)
    end subroutine compile_and_run_tests

    subroutine compile_test_helpers(project_dir, obj_dir, dag, filenames, is_prog, &
            topo_order, n_order, run_nodes, n_run, incl_flag, log_file, &
            helper_objs, n_helper_objs, exitcode)
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
            call make_obj_path(filenames(node_id), project_dir, obj_dir, obj_path)
            call compile_f90(project_dir, filenames(node_id), obj_path, incl_flag, &
                log_file, exitcode)
            if (exitcode /= 0) return
            if (n_helper_objs >= size(helper_objs)) cycle
            n_helper_objs = n_helper_objs + 1
            helper_objs(n_helper_objs) = obj_path
        end do
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

    integer function test_timeout_seconds() result(t)
        character(len=32) :: buf
        integer :: status, iostat

        t = 120
        call get_environment_variable('FO_TEST_TIMEOUT', buf, status=status)
        if (status /= 0 .or. len_trim(buf) == 0) return
        read (buf, *, iostat=iostat) t
        if (iostat /= 0 .or. t < 1) t = 120
    end function test_timeout_seconds

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

    subroutine warn_slow_tests(filenames, run_nodes, run_exits, run_secs, n_run, &
            test_warn, log_file)
        character(len=MAX_PATH), intent(in) :: filenames(:)
        integer, intent(in) :: run_nodes(:), run_exits(:), n_run, test_warn
        real, intent(in) :: run_secs(:)
        character(len=*), intent(in) :: log_file

        integer :: i, u, ios
        character(len=128) :: tname

        do i = 1, n_run
            if (run_exits(i) /= 0) cycle
            if (run_secs(i) < real(test_warn)) cycle
            call file_basename(filenames(run_nodes(i)), tname)
            ! tests already named *_slow are an intentional opt-in (fo test --all);
            ! only nag fast-suite tests that are creeping toward the hard limit
            if (is_slow_name(tname)) cycle
            write (error_unit, '(a,f0.1,a)') 'fo: slow test '//trim(tname)// &
                ' took ', run_secs(i), 's; name it *_slow or speed it up'
            open (newunit=u, file=trim(log_file), position='append', &
                status='old', iostat=ios)
            if (ios /= 0) cycle
            write (u, '(a,f0.1,a)') 'fo: warning: slow test '//trim(tname)// &
                ' took ', run_secs(i), 's (threshold above is advisory)'
            close (u)
        end do
    end subroutine warn_slow_tests

    subroutine append_timeout_status(log_file, test_name, timeout_s)
        character(len=*), intent(in) :: log_file, test_name
        integer, intent(in) :: timeout_s

        integer :: u, ios

        open (newunit=u, file=trim(log_file), position='append', &
            status='old', iostat=ios)
        if (ios /= 0) return
        write (u, '(a,i0,a)') 'fo: test target '//trim(test_name)// &
            ' exceeded the ', timeout_s, &
            's hard timeout and was killed; fix the hang or name it *_slow'
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
        character(len=128), intent(in) :: selected_names(:)
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

    subroutine collect_lib_objs(obj_dir, lib_objs, n_lib_objs)
        character(len=*), intent(in) :: obj_dir
        character(len=512), intent(out) :: lib_objs(MAX_SRC_OBJS)
        integer, intent(out) :: n_lib_objs

        character(len=512) :: line, bname
        character(len=512), allocatable :: objs(:)
        integer :: n_objs, k, slash, n

        n_lib_objs = 0
        allocate (objs(MAX_SRC_OBJS))
        call fs_collect_files(trim(obj_dir), '', '.o', '', objs, n_objs)
        do k = 1, n_objs
            line = objs(k)
            if (len_trim(line) == 0) cycle
            n = len_trim(line)
            slash = index(line(1:n), '/', back=.true.)
            if (slash > 0) then
                bname = line(slash + 1:n)
            else
                bname = trim(line)
            end if
            if (bname(1:4) == 'app_') cycle
            if (bname(1:5) == 'test_') cycle
            if (n_lib_objs < MAX_SRC_OBJS) then
                n_lib_objs = n_lib_objs + 1
                lib_objs(n_lib_objs) = trim(line)
            end if
        end do
        deallocate (objs)
    end subroutine collect_lib_objs

    subroutine hash_lib_objs(lib_objs, n_lib_objs, combined)
        !! Combine per-object content hashes into one key that represents the
        !! linked library implementation a test binary depends on, so a cached
        !! test result is invalidated when any library object changes.
        character(len=512), intent(in) :: lib_objs(MAX_SRC_OBJS)
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

    subroutine make_obj_path(source_path, project_dir, obj_dir, obj_path)
        character(len=*), intent(in) :: source_path, project_dir, obj_dir
        character(len=*), intent(out) :: obj_path

        character(len=512) :: rel
        integer :: i, plen

        plen = len_trim(project_dir)
        if (len_trim(source_path) > plen .and. &
            source_path(1:plen) == project_dir) then
            rel = source_path(plen + 2:)
        else
            rel = trim(source_path)
        end if
        do i = 1, len_trim(rel)
            if (rel(i:i) == '/') rel(i:i) = '_'
        end do
        obj_path = trim(obj_dir)//'/'//trim(rel)//'.o'
    end subroutine make_obj_path

    function fc_command() result(cmd)
        !! Fortran compiler: $FO_FC if set, else gfortran. Lets a host opt into
        !! a different compiler (e.g. flang to dodge a gfortran codegen bug)
        !! without editing the project.
        character(len=:), allocatable :: cmd
        character(len=256) :: val
        integer :: st

        call get_environment_variable('FO_FC', val, status=st)
        if (st == 0 .and. len_trim(val) > 0) then
            cmd = trim(adjustl(val))
        else
            cmd = 'gfortran'
        end if
    end function fc_command

    logical function fc_is_flang()
        !! True when the selected compiler is LLVM flang. Drives flag dialect:
        !! flang rejects gfortran-only flags and spells the module-output dir
        !! differently.
        fc_is_flang = index(fc_command(), 'flang') > 0
    end function fc_is_flang

    function fc_base_flags() result(flags)
        !! Compiler-appropriate baseline compile flags. gfortran needs the long
        !! free-form line length; flang has no line limit and rejects that flag.
        !! -fopenmp is added per project via the fpm openmp metapackage (see
        !! config_flags_str), not here.
        character(len=:), allocatable :: flags

        if (fc_is_flang()) then
            flags = '-fimplicit-none'
        else
            flags = '-ffree-line-length-none -fimplicit-none'
        end if
    end function fc_base_flags

    subroutine make_includes_flag(mod_dir, dep_includes, n_dep_includes, flag)
        character(len=*), intent(in) :: mod_dir
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(in) :: n_dep_includes
        character(len=*), intent(out) :: flag

        integer :: i

        ! Newline-separated argv tokens with raw, unquoted paths. compile_f90
        ! splits this on newlines so each -I/-J path stays one whole token even
        ! when it contains spaces; shell quoting must not be applied, or quotes
        ! would land literally in the path and the .mod would not be found.
        ! flang spells the module-output dir -module-dir; gfortran uses -J.
        if (fc_is_flang()) then
            flag = '-module-dir'//char(10)//trim(mod_dir)//char(10)//'-I'// &
                char(10)//trim(mod_dir)
        else
            flag = '-J'//char(10)//trim(mod_dir)//char(10)//'-I'//char(10)// &
                trim(mod_dir)
        end if
        do i = 1, n_dep_includes
            flag = trim(flag)//char(10)//'-I'//char(10)//trim(dep_includes(i))
        end do
    end subroutine make_includes_flag

    function with_user_flags(includes_nl, flags) result(combined)
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
        character(len=512) :: tmpfile
        character(len=:), allocatable :: packed
        integer :: u, iostat, n_args, exitcode

        compiler = fc_command()
        call make_tmpfile('fo_compiler_version', tmpfile)
        n_args = 0
        call argv_push_split(packed, n_args, fc_command())
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
    end subroutine detect_compiler

    subroutine compile_f90(project_dir, source, objfile, includes_flag, log_file, &
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
        n_args = 0
        call argv_push_split(packed, n_args, fc_command())
        call argv_push(packed, n_args, '-c')
        call argv_push_split_nl(packed, n_args, includes_flag)
        call argv_push_split(packed, n_args, fc_base_flags())
        call argv_push(packed, n_args, '-o')
        call argv_push(packed, n_args, objfile)
        call argv_push(packed, n_args, source)

        call process_run_argv_logged(project_dir, packed, n_args, log_file, &
            .true., build_timeout_seconds(), exitcode)
        if (exitcode == 124) call append_build_hang_hint(log_file, &
            'compile of '//trim(source), build_timeout_seconds())
        if (exitcode == 0) return
        if (.not. looks_like_stale_mod_failure(log_file)) return

        call clean_root_build_artifacts(project_dir, n_removed)
        call append_stale_mod_hint(log_file, n_removed)

        call process_run_argv_logged(project_dir, packed, n_args, log_file, &
            .true., build_timeout_seconds(), exitcode)
        if (exitcode == 124) call append_build_hang_hint(log_file, &
            'compile of '//trim(source), build_timeout_seconds())
    end subroutine compile_f90

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
        write (u, '(a)') 'fo: VS Code Modern Fortran linting can create these files; set fortran.linter.modOutput outside the project root.'
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

    subroutine compile_c(source, objfile, log_file, exitcode)
        character(len=*), intent(in) :: source, objfile, log_file
        integer, intent(out) :: exitcode
        character(len=:), allocatable :: packed
        integer :: n_args

        n_args = 0
        call argv_push(packed, n_args, 'gcc')
        call argv_push(packed, n_args, '-c')
        call argv_push(packed, n_args, '-o')
        call argv_push(packed, n_args, objfile)
        call argv_push(packed, n_args, source)
        call process_run_argv_logged('', packed, n_args, log_file, .true., &
            build_timeout_seconds(), exitcode)
    end subroutine compile_c

    subroutine link_binary(project_dir, prog_obj, lib_objs, n_lib_objs, dep_objs, n_dep_objs, &
            link_libs, n_link_libs, output, log_file, exitcode, flags, &
            cache, base_digest)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: prog_obj, output, log_file
        character(len=512), intent(in) :: lib_objs(MAX_SRC_OBJS)
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
        integer :: i, n_args
        logical :: do_cache, restored
        character(len=HASH_LEN) :: action_id, prog_key
        character(len=512) :: tmp_bin, key_parts(6)
        character(len=:), allocatable :: flags_str
        integer :: store_ierr

        flags_str = ''
        if (present(flags)) flags_str = trim(flags)

        do_cache = present(cache) .and. present(base_digest)
        if (do_cache) do_cache = len_trim(base_digest) > 0
        if (do_cache) then
            call cache_file_digest(prog_obj, prog_key)
            key_parts(1) = 'fo-link-1'
            key_parts(2) = trim(fc_command())
            key_parts(3) = flags_str
            key_parts(4) = trim(link_lib_flags(project_dir, link_libs, n_link_libs))
            key_parts(5) = trim(base_digest)
            key_parts(6) = trim(prog_key)
            action_id = cache_digest(key_parts, 6)
            ! Fast path: the output is already the binary this action produces.
            ! Leave it untouched - no relink, no copy. This is what keeps warm
            ! builds cheap when outputs are large (2.4 GB of static binaries).
            call cache_binary_matches(cache, action_id, output, restored)
            if (restored) then
                exitcode = 0
                return
            end if
            ! Output missing or stale but the binary is in the CAS: restore it
            ! (to a temp, then copy with the execute bit) instead of relinking.
            tmp_bin = trim(output)//'.fo-link'
            call cache_restore_binary(cache, action_id, tmp_bin, restored)
            if (restored) then
                if (fs_copy_exec(trim(tmp_bin), trim(output)) == 0) then
                    call fs_remove_file(trim(tmp_bin))
                    call cache_store_binary(cache, action_id, output, store_ierr)
                    exitcode = 0
                    return
                end if
                call fs_remove_file(trim(tmp_bin))
            end if
        end if

        ! Build an argv vector (no /bin/sh): quote-proof and async-signal-safe,
        ! since link_binary runs inside the parallel test loop where a shell
        ! fork would corrupt libgomp.
        n_args = 0
        call argv_push_split(packed, n_args, fc_command())
        call argv_push(packed, n_args, prog_obj)
        do i = 1, n_lib_objs
            call argv_push(packed, n_args, lib_objs(i))
        end do
        do i = 1, n_dep_objs
            call argv_push(packed, n_args, dep_objs(i))
        end do
        call argv_push_split_nl(packed, n_args, link_lib_flags(project_dir, link_libs, n_link_libs))
        ! flang's driver does not add Homebrew's libomp to the link search, so
        ! -fopenmp links fail with "library 'omp' not found". Add it (harmless
        ! when the build does not use OpenMP) plus an rpath for runtime.
        if (fc_is_flang() .and. is_macos()) then
            call argv_push(packed, n_args, '-L/opt/homebrew/opt/libomp/lib')
            call argv_push(packed, n_args, '-Wl,-rpath,/opt/homebrew/opt/libomp/lib')
        end if
        if (present(flags) .and. len_trim(flags) > 0) then
            call argv_push_split(packed, n_args, flags)
        end if
        call argv_push(packed, n_args, '-o')
        call argv_push(packed, n_args, output)
        call get_environment_variable('FO_DEBUG_LINKS', debug_links, &
            status=debug_status)
        if (debug_status == 0 .and. len_trim(debug_links) > 0) then
            write (error_unit, '(a)') 'fo link: '//argv_display(packed)
        end if
        call process_run_argv_logged('', packed, n_args, log_file, .true., &
            build_timeout_seconds(), exitcode)
        if (exitcode == 124) call append_build_hang_hint(log_file, &
            'link of '//trim(output), build_timeout_seconds())

        if (do_cache .and. exitcode == 0) &
            call cache_store_binary(cache, action_id, output, store_ierr)
    end subroutine link_binary

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

    subroutine merge_flags(config, flag_text)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(inout) :: flag_text
        integer :: i
        character(len=1024) :: combined

        combined = ''
        ! fpm openmp metapackage -> -fopenmp on compile and link.
        if (config%openmp) combined = '-fopenmp'
        do i = 1, config%n_flags
            if (len_trim(combined) > 0) then
                combined = trim(combined)//' '//trim(config%flags(i))
            else
                combined = trim(config%flags(i))
            end if
        end do
        if (len_trim(flag_text) > 0) then
            if (len_trim(combined) > 0) then
                combined = trim(combined)//' '//trim(flag_text)
            else
                combined = trim(flag_text)
            end if
        end if
        flag_text = combined
    end subroutine merge_flags

    function config_flags_str(config) result(s)
        type(fpm_config_t), intent(in) :: config
        character(len=1024) :: s
        integer :: i

        s = ''
        if (config%openmp) s = '-fopenmp'
        do i = 1, config%n_flags
            if (len_trim(s) > 0) then
                s = trim(s)//' '//trim(config%flags(i))
            else
                s = trim(config%flags(i))
            end if
        end do
    end function config_flags_str

end module fo_gfortran_build
