module fo_gfortran_build
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS, MAX_NAME
    use fo_dag_bridge, only: build_dag_from_units
    use fx_dag, only: dag_t, dag_topo_sort, dag_levels, MAX_NODES
    use fo_cache, only: cache_t, cache_init, cache_lookup, cache_store, &
                        cache_key_for, HASH_LEN
    use fo_util, only: make_tmpfile, delete_tmpfile
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    integer, parameter :: MAX_DEP_DIRS = 64
    integer, parameter :: MAX_DEP_OBJS = 1024
    integer, parameter :: MAX_SRC_OBJS = 2048

    public :: gfortran_build, gfortran_test

contains

    subroutine gfortran_build(project_dir, log_file, exitcode, n_compiled)
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode
        integer, intent(out), optional :: n_compiled

        type(fpm_config_t) :: config
        integer :: ierr, n_dep_includes, n_dep_objs, n_src_objs, nc
        character(len=512) :: mod_dir, obj_dir, bin_dir
        character(len=512) :: dep_includes(MAX_DEP_DIRS)
        character(len=512) :: dep_objs(MAX_DEP_OBJS)
        character(len=512) :: src_objs(MAX_SRC_OBJS)
        logical :: is_prog_arr(MAX_SRC_OBJS)
        character(len=512) :: lf

        lf = log_file
        if (len_trim(lf) == 0) lf = '/dev/null'

        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: no fpm.toml found in '//trim(project_dir)
            exitcode = 1
            return
        end if

        mod_dir = trim(project_dir)//'/build/fo/mod'
        obj_dir = trim(project_dir)//'/build/fo/obj'
        bin_dir = trim(project_dir)//'/build/fo/bin'
        call execute_command_line('mkdir -p '//sq(mod_dir)//' '// &
                                  sq(obj_dir)//' '//sq(bin_dir), &
                                  wait=.true., exitstat=exitcode)
        if (exitcode /= 0) return

        call truncate_file(trim(lf))

        call find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
                                dep_objs, n_dep_objs)

        nc = 0
        call compile_sources(project_dir, config%source_dir, config%app_dir, &
                             mod_dir, obj_dir, dep_includes, n_dep_includes, lf, &
                             src_objs, n_src_objs, is_prog_arr, exitcode, nc)
        if (exitcode /= 0) return

        if (present(n_compiled)) n_compiled = nc

        call link_app_binaries(project_dir, config, bin_dir, src_objs, n_src_objs, &
                               is_prog_arr, dep_objs, n_dep_objs, lf, exitcode)
    end subroutine gfortran_build

    subroutine gfortran_test(project_dir, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode

        type(fpm_config_t) :: config
        integer :: ierr, n_dep_includes, n_dep_objs, n_lib_objs
        character(len=512) :: mod_dir, obj_dir, bin_dir
        character(len=512) :: dep_includes(MAX_DEP_DIRS)
        character(len=512) :: dep_objs(MAX_DEP_OBJS)
        character(len=512) :: lib_objs(MAX_SRC_OBJS)
        character(len=512) :: lf

        lf = log_file
        if (len_trim(lf) == 0) lf = '/dev/null'

        call gfortran_build(project_dir, lf, exitcode)
        if (exitcode /= 0) return

        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            exitcode = 1
            return
        end if

        mod_dir = trim(project_dir)//'/build/fo/mod'
        obj_dir = trim(project_dir)//'/build/fo/obj'
        bin_dir = trim(project_dir)//'/build/fo/bin'

        call find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
                                dep_objs, n_dep_objs)
        call collect_lib_objs(obj_dir, lib_objs, n_lib_objs)

        call compile_and_run_tests(project_dir, config%test_dir, mod_dir, obj_dir, &
                                   bin_dir, dep_includes, n_dep_includes, &
                                   dep_objs, n_dep_objs, lib_objs, n_lib_objs, &
                                   config%link_libs, config%n_link_libs, lf, exitcode)
    end subroutine gfortran_test

    subroutine find_dep_artifacts(project_dir, config, dep_includes, n_dep_includes, &
                                  dep_objs, n_dep_objs)
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(in) :: config
        character(len=512), intent(out) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(out) :: n_dep_includes
        character(len=512), intent(out) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(out) :: n_dep_objs

        character(len=512) :: tmpfile, line
        character(len=4096) :: cmd
        integer :: u, ios, i
        integer :: n_obj_seen
        character(len=512) :: obj_basenames(MAX_DEP_OBJS)
        integer :: slash

        n_dep_includes = 0
        n_dep_objs = 0
        n_obj_seen = 0
        if (config%n_deps == 0) return

        call make_tmpfile('fo_dep_dirs', tmpfile)
        cmd = 'find '//sq(trim(project_dir)//'/build')//' -maxdepth 1 '// &
              '-name "gfortran_*" -type d 2>/dev/null | sort > '//trim(tmpfile)
        call execute_command_line(trim(cmd), wait=.true.)

        open (newunit=u, file=tmpfile, status='old', iostat=ios)
        if (ios == 0) then
            do
                read (u, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (len_trim(line) == 0) cycle
                if (n_dep_includes < MAX_DEP_DIRS) then
                    n_dep_includes = n_dep_includes + 1
                    dep_includes(n_dep_includes) = trim(line)
                end if
            end do
            close (u)
        end if
        call delete_tmpfile(tmpfile)

        call make_tmpfile('fo_dep_objs', tmpfile)
        do i = 1, config%n_deps
            cmd = 'find '//sq(trim(project_dir)//'/build')//' -name '// &
                  '"build_dependencies_'//trim(config%deps(i)%name)//'_src_*.f90.o" '// &
                  '2>/dev/null >> '//trim(tmpfile)
            call execute_command_line(trim(cmd), wait=.true.)
            cmd = 'find '//sq(trim(project_dir)//'/build')//' -name '// &
                  '"build_dependencies_'//trim(config%deps(i)%name)//'_*.c.o" '// &
                  '2>/dev/null >> '//trim(tmpfile)
            call execute_command_line(trim(cmd), wait=.true.)
        end do

        open (newunit=u, file=tmpfile, status='old', iostat=ios)
        if (ios == 0) then
            do
                read (u, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (len_trim(line) == 0) cycle
                if (index(line, '_app_') > 0) cycle
                if (index(line, '_test_') > 0) cycle
                ! deduplicate: skip if basename already seen (multiple gfortran_* dirs)
                slash = index(trim(line), '/', back=.true.)
                if (any(obj_basenames(1:n_obj_seen) == line(slash + 1:))) cycle
                if (n_dep_objs < MAX_DEP_OBJS) then
                    n_dep_objs = n_dep_objs + 1
                    dep_objs(n_dep_objs) = trim(line)
                    n_obj_seen = n_obj_seen + 1
                    obj_basenames(n_obj_seen) = line(slash + 1:)
                end if
            end do
            close (u)
        end if
        call delete_tmpfile(tmpfile)
    end subroutine find_dep_artifacts

    subroutine compile_sources(project_dir, src_dir, app_dir, mod_dir, obj_dir, &
                               dep_includes, n_dep_includes, log_file, &
                               src_objs, n_src_objs, is_prog_arr, exitcode, n_compiled)
        character(len=*), intent(in) :: project_dir, src_dir, app_dir
        character(len=*), intent(in) :: mod_dir, obj_dir, log_file
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(in) :: n_dep_includes
        character(len=512), intent(out) :: src_objs(MAX_SRC_OBJS)
        integer, intent(out) :: n_src_objs
        logical, intent(out) :: is_prog_arr(MAX_SRC_OBJS)
        integer, intent(out) :: exitcode, n_compiled

        type(scan_unit_t), allocatable :: units_a(:), units_b(:), all_units(:)
        integer :: na, nb, n_all, i, ii, ierr, node_id
        type(dag_t) :: dag
        character(len=MAX_NAME), allocatable :: filenames(:)
        logical, allocatable :: is_prog(:), is_test_arr(:)
        integer, allocatable :: topo_order(:), node_levels(:)
        integer :: n_order, n_levels, lvl
        logical :: has_cycle, obj_exists
        character(len=512) :: obj_path
        character(len=4096) :: includes_flag
        character(len=512) :: c_list, c_line
        integer :: uc, cios

        type(cache_t) :: c
        integer :: cache_ierr
        character(len=HASH_LEN), allocatable :: old_mod_keys(:), new_mod_keys(:)
        character(len=HASH_LEN) :: dep_keys(64), source_key
        integer :: dep_id, j, n_dep
        integer, allocatable :: compile_nodes(:)
        character(len=HASH_LEN), allocatable :: compile_keys(:)
        integer, allocatable :: compile_exits(:)
        character(len=512), allocatable :: per_logs(:)
        integer :: n_compile
        character(len=MAX_NAME) :: fname_local
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

        call build_dag_from_units(all_units, n_all, dag, filenames, is_test_arr, is_prog)
        call dag_topo_sort(dag, topo_order, n_order, has_cycle)
        call dag_levels(dag, topo_order, n_order, node_levels, n_levels)
        call make_includes_flag(mod_dir, dep_includes, n_dep_includes, includes_flag)

        old_mod_keys = ''
        new_mod_keys = ''
        call load_mod_keys(mod_dir, dag, n_order, topo_order, old_mod_keys)
        call cache_init(c, cache_ierr)

        if (cache_ierr == 0) then
            do lvl = 0, n_levels - 1
                n_compile = 0
                compile_exits = 0

                do i = 1, n_order
                    if (node_levels(i) /= lvl) cycle
                    node_id = topo_order(i)
                    if (is_test_arr(node_id)) cycle
                    if (len_trim(filenames(node_id)) == 0) cycle

                    n_dep = 0
                    do j = 1, dag%nodes(node_id)%n_edges
                        dep_id = dag%nodes(node_id)%edges(j)
                        if (dep_id > 0 .and. len_trim(new_mod_keys(dep_id)) > 0) then
                            n_dep = n_dep + 1
                            if (n_dep <= 64) dep_keys(n_dep) = new_mod_keys(dep_id)
                        end if
                    end do
                    source_key = cache_key_for(filenames(node_id), 'gfortran', '', &
                                               dep_keys, n_dep)

                    call make_obj_path(filenames(node_id), project_dir, obj_dir, obj_path)
                    if (cache_lookup(c, dag%nodes(node_id)%label, source_key)) then
                        inquire (file=trim(obj_path), exist=obj_exists)
                        if (obj_exists) then
                            new_mod_keys(node_id) = old_mod_keys(node_id)
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
                    call compile_f90(fname_local, obj_path, includes_flag, &
                                     per_log_local, compile_exits(ii))
                end do
                !$omp end parallel do

                do ii = 1, n_compile
                    call append_log_file(trim(per_logs(ii)), log_file)
                end do
                do ii = 1, n_compile
                    if (compile_exits(ii) /= 0) then
                        exitcode = compile_exits(ii)
                        return
                    end if
                    node_id = compile_nodes(ii)
                    call get_mod_key(dag%nodes(node_id)%label, mod_dir, &
                                     new_mod_keys(node_id))
                    call cache_store(c, dag%nodes(node_id)%label, compile_keys(ii))
                end do
                n_compiled = n_compiled + n_compile
            end do
            call save_mod_keys(mod_dir, dag, n_order, topo_order, new_mod_keys)
        else
            do i = 1, n_order
                node_id = topo_order(i)
                if (len_trim(filenames(node_id)) == 0) cycle
                if (is_test_arr(node_id)) cycle
                call make_obj_path(filenames(node_id), project_dir, obj_dir, obj_path)
                call compile_f90(filenames(node_id), obj_path, includes_flag, &
                                 log_file, exitcode)
                if (exitcode /= 0) return
                n_compiled = n_compiled + 1
            end do
        end if

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

        call make_tmpfile('fo_c_files', c_list)
        call execute_command_line('find '//sq(trim(project_dir)//'/'//trim(src_dir))// &
            ' -name "*.c" 2>/dev/null | sort > '//trim(c_list), wait=.true.)
        open (newunit=uc, file=c_list, status='old', iostat=cios)
        if (cios == 0) then
            do
                read (uc, '(a)', iostat=cios) c_line
                if (cios /= 0) exit
                if (len_trim(c_line) == 0) cycle
                call make_obj_path(trim(c_line), project_dir, obj_dir, obj_path)
                call compile_c(trim(c_line), obj_path, log_file, exitcode)
                if (exitcode /= 0) then
                    close (uc)
                    call delete_tmpfile(c_list)
                    return
                end if
                if (n_src_objs < MAX_SRC_OBJS) then
                    n_src_objs = n_src_objs + 1
                    src_objs(n_src_objs) = obj_path
                    is_prog_arr(n_src_objs) = .false.
                end if
            end do
            close (uc)
        end if
        call delete_tmpfile(c_list)
    end subroutine compile_sources

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
        character(len=512) :: modpath, tmpfile, cmd
        character(len=HASH_LEN) :: empty_keys(0)
        integer :: exitcode, i

        lower_label = label
        do i = 1, len_trim(lower_label)
            if (lower_label(i:i) >= 'A' .and. lower_label(i:i) <= 'Z') &
                lower_label(i:i) = achar(iachar(lower_label(i:i)) + 32)
        end do

        modpath = trim(mod_dir)//'/'//trim(lower_label)//'.mod'
        call make_tmpfile('fo_mod_text', tmpfile)
        cmd = 'gzip -d -c '//sq(modpath)//' > '//trim(tmpfile)//' 2>/dev/null'
        call execute_command_line(trim(cmd), wait=.true., exitstat=exitcode)
        if (exitcode == 0) then
            key = cache_key_for(trim(tmpfile), '', '', empty_keys, 0)
        else
            key = cache_key_for(trim(modpath), '', '', empty_keys, 0)
        end if
        call delete_tmpfile(tmpfile)
    end subroutine get_mod_key

    subroutine append_log_file(src, dst)
        character(len=*), intent(in) :: src, dst
        call execute_command_line('cat '//sq(trim(src))//' >> '//sq(trim(dst))// &
                                  ' 2>/dev/null; rm -f '//sq(trim(src)), wait=.true.)
    end subroutine append_log_file

    subroutine link_app_binaries(project_dir, config, bin_dir, src_objs, n_src_objs, &
                                 is_prog_arr, dep_objs, n_dep_objs, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, bin_dir, log_file
        type(fpm_config_t), intent(in) :: config
        character(len=512), intent(in) :: src_objs(MAX_SRC_OBJS)
        integer, intent(in) :: n_src_objs
        logical, intent(in) :: is_prog_arr(MAX_SRC_OBJS)
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs
        integer, intent(out) :: exitcode

        integer :: i, j, n_lib
        character(len=512) :: lib_objs(MAX_SRC_OBJS)
        character(len=512) :: prog_obj, bin_path
        character(len=128) :: prog_name

        exitcode = 0
        n_lib = 0
        do i = 1, n_src_objs
            if (.not. is_prog_arr(i)) then
                n_lib = n_lib + 1
                lib_objs(n_lib) = src_objs(i)
            end if
        end do

        j = 0
        do i = 1, n_src_objs
            if (.not. is_prog_arr(i)) cycle
            prog_obj = src_objs(i)
            j = j + 1
            if (j == 1 .and. len_trim(config%name) > 0) then
                prog_name = trim(config%name)
            else
                call file_basename(prog_obj, prog_name)
            end if
            bin_path = trim(bin_dir)//'/'//trim(prog_name)
            call link_binary(prog_obj, lib_objs, n_lib, dep_objs, n_dep_objs, &
                             config%link_libs, config%n_link_libs, bin_path, &
                             log_file, exitcode)
            if (exitcode /= 0) return
        end do
    end subroutine link_app_binaries

    subroutine compile_and_run_tests(project_dir, test_dir, mod_dir, obj_dir, &
                                     bin_dir, dep_includes, n_dep_includes, &
                                     dep_objs, n_dep_objs, lib_objs, n_lib_objs, &
                                     link_libs, n_link_libs, log_file, exitcode)
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
        integer, intent(out) :: exitcode

        type(scan_unit_t), allocatable :: tunits(:)
        integer :: n_tests, i, ierr, node_id
        type(dag_t) :: dag
        character(len=MAX_NAME), allocatable :: filenames(:)
        logical, allocatable :: is_prog(:), is_test_arr(:)
        integer, allocatable :: topo_order(:)
        integer :: n_order
        logical :: has_cycle
        character(len=512) :: obj_path, bin_path
        character(len=4096) :: incl_flag
        character(len=128) :: tname
        integer :: run_exit

        exitcode = 0
        allocate (tunits(MAX_UNITS))
        allocate (filenames(MAX_NODES), is_prog(MAX_NODES), is_test_arr(MAX_NODES))
        allocate (topo_order(MAX_NODES))
        call scan_dir(trim(project_dir)//'/'//trim(test_dir), tunits, n_tests, ierr)
        if (n_tests == 0) return

        call build_dag_from_units(tunits, n_tests, dag, filenames, is_test_arr, is_prog)
        call dag_topo_sort(dag, topo_order, n_order, has_cycle)
        call make_includes_flag(mod_dir, dep_includes, n_dep_includes, incl_flag)

        do i = 1, n_order
            node_id = topo_order(i)
            if (len_trim(filenames(node_id)) == 0) cycle
            if (.not. is_prog(node_id)) cycle
            call make_obj_path(filenames(node_id), project_dir, obj_dir, obj_path)
            call compile_f90(filenames(node_id), obj_path, incl_flag, log_file, exitcode)
            if (exitcode /= 0) return

            call file_basename(filenames(node_id), tname)
            bin_path = trim(bin_dir)//'/'//trim(tname)
            call link_binary(obj_path, lib_objs, n_lib_objs, dep_objs, n_dep_objs, &
                             link_libs, n_link_libs, bin_path, log_file, exitcode)
            if (exitcode /= 0) return

            call execute_command_line(sq(bin_path)//" >> '"//trim(log_file)// &
                                      "' 2>&1", wait=.true., exitstat=run_exit)
            if (run_exit /= 0) exitcode = run_exit
        end do
    end subroutine compile_and_run_tests

    subroutine collect_lib_objs(obj_dir, lib_objs, n_lib_objs)
        character(len=*), intent(in) :: obj_dir
        character(len=512), intent(out) :: lib_objs(MAX_SRC_OBJS)
        integer, intent(out) :: n_lib_objs

        character(len=512) :: tmpfile, line, bname
        integer :: u, ios, slash, n

        n_lib_objs = 0
        call make_tmpfile('fo_lib_objs', tmpfile)
        call execute_command_line('find '//sq(obj_dir)// &
            ' -name "*.o" 2>/dev/null | sort > '//trim(tmpfile), wait=.true.)
        open (newunit=u, file=tmpfile, status='old', iostat=ios)
        if (ios == 0) then
            do
                read (u, '(a)', iostat=ios) line
                if (ios /= 0) exit
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
            close (u)
        end if
        call delete_tmpfile(tmpfile)
    end subroutine collect_lib_objs

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

    subroutine make_includes_flag(mod_dir, dep_includes, n_dep_includes, flag)
        character(len=*), intent(in) :: mod_dir
        character(len=512), intent(in) :: dep_includes(MAX_DEP_DIRS)
        integer, intent(in) :: n_dep_includes
        character(len=*), intent(out) :: flag

        integer :: i

        flag = '-J '//sq(mod_dir)//' -I '//sq(mod_dir)
        do i = 1, n_dep_includes
            flag = trim(flag)//' -I '//sq(dep_includes(i))
        end do
    end subroutine make_includes_flag

    pure function sq(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len_trim(s) + 2) :: r
        r = "'"//trim(s)//"'"
    end function sq

    subroutine compile_f90(source, objfile, includes_flag, log_file, exitcode)
        character(len=*), intent(in) :: source, objfile, includes_flag, log_file
        integer, intent(out) :: exitcode
        character(len=8192) :: cmd
        cmd = 'gfortran -c '//trim(includes_flag)// &
              ' -ffree-line-length-none -fimplicit-none'// &
              ' -o '//sq(objfile)//' '//sq(source)// &
              " >> '"//trim(log_file)//"' 2>&1"
        call execute_command_line(trim(cmd), wait=.true., exitstat=exitcode)
    end subroutine compile_f90

    subroutine compile_c(source, objfile, log_file, exitcode)
        character(len=*), intent(in) :: source, objfile, log_file
        integer, intent(out) :: exitcode
        character(len=4096) :: cmd
        cmd = 'gcc -c -o '//sq(objfile)//' '//sq(source)// &
              " >> '"//trim(log_file)//"' 2>&1"
        call execute_command_line(trim(cmd), wait=.true., exitstat=exitcode)
    end subroutine compile_c

    subroutine link_binary(prog_obj, lib_objs, n_lib_objs, dep_objs, n_dep_objs, &
                           link_libs, n_link_libs, output, log_file, exitcode)
        character(len=*), intent(in) :: prog_obj, output, log_file
        character(len=512), intent(in) :: lib_objs(MAX_SRC_OBJS)
        integer, intent(in) :: n_lib_objs
        character(len=512), intent(in) :: dep_objs(MAX_DEP_OBJS)
        integer, intent(in) :: n_dep_objs
        character(len=128), intent(in) :: link_libs(*)
        integer, intent(in) :: n_link_libs
        integer, intent(out) :: exitcode

        character(len=16384) :: cmd
        integer :: i

        cmd = 'gfortran '//sq(prog_obj)
        do i = 1, n_lib_objs
            cmd = trim(cmd)//' '//sq(lib_objs(i))
        end do
        do i = 1, n_dep_objs
            cmd = trim(cmd)//' '//sq(dep_objs(i))
        end do
        do i = 1, n_link_libs
            cmd = trim(cmd)//' -l'//trim(link_libs(i))
        end do
        cmd = trim(cmd)//' -o '//sq(output)//" >> '"//trim(log_file)//"' 2>&1"
        call execute_command_line(trim(cmd), wait=.true., exitstat=exitcode)
    end subroutine link_binary

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

end module fo_gfortran_build
