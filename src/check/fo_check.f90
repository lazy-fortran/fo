module fo_check
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_scan, only: scan_unit_t, scan_dir, MAX_NAME, MAX_UNITS
    use fx_dag, only: dag_t, dag_find_node, dag_topo_sort, dag_affected_set, MAX_NODES
    use fo_dag_bridge, only: build_dag_from_units
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE, &
                                BACKEND_FPM, BACKEND_CMAKE
    use fo_cache, only: cache_t, cache_init, cache_lookup, cache_store, &
                        cache_key_for, hash_mod_file, HASH_LEN
    use fo_artifact_cache, only: artifact_store, artifact_restore
    use fo_diagnostics, only: diagnostic_t, diagnostic_from_log, is_runner_crash
    implicit none
    private
    public :: check_result_t, test_result_t, fo_check_run, fo_changed_modules
    public :: MAX_TEST_RESULTS

    integer, parameter :: MAX_EXT_DEPS = 256
    integer, parameter :: MAX_TEST_RESULTS = 64

    type :: test_result_t
        character(len=128) :: name = ''
        integer :: n_pass = 0
        integer :: n_fail = 0
        character(len=8) :: status = ''
    end type test_result_t

    type :: check_result_t
        logical :: build_ok = .false.
        logical :: tests_ok = .false.
        integer :: n_modules = 0
        integer :: n_cached = 0
        integer :: n_changed = 0
        integer :: n_affected = 0
        integer :: n_ext_deps = 0
        integer :: n_in_cycle = 0
        real :: elapsed = 0.0
        character(len=512) :: error_msg = ''
        character(len=32) :: stage = 'done'
        character(len=128) :: target = ''
        character(len=512) :: summary = ''
        character(len=256) :: hint = ''
        character(len=256) :: rerun = ''
        character(len=512) :: log_path = ''
        character(len=256) :: diag_file = ''
        integer :: diag_line = 0
        integer :: diag_column = 0
        type(test_result_t) :: test_results(MAX_TEST_RESULTS)
        integer :: n_test_results = 0
    end type check_result_t

contains

    subroutine fo_changed_modules(dir, dag, changed_ids, n_changed, &
                                  affected_ids, n_affected, n_cached, ierr, &
                                  n_in_cycle, filenames, is_test_arr)
        character(len=*), intent(in) :: dir
        type(dag_t), intent(out) :: dag
        integer, intent(out) :: changed_ids(MAX_NODES), n_changed
        integer, intent(out) :: affected_ids(MAX_NODES), n_affected
        integer, intent(out) :: n_cached, ierr
        integer, intent(out), optional :: n_in_cycle
        character(len=MAX_NAME), optional, intent(out) :: filenames(MAX_NODES)
        logical, optional, intent(out) :: is_test_arr(MAX_NODES)

        type(scan_unit_t), allocatable :: units(:)
        type(cache_t) :: c
        type(backend_t) :: b
        integer :: n_units
        integer, allocatable :: order(:)
        integer :: n_order
        integer :: i, node_id, j, dep_id, n_dep_keys
        character(len=HASH_LEN), allocatable :: keys(:)
        character(len=HASH_LEN) :: dep_keys(64)
        character(len=256) :: compiler
        character(len=MAX_NAME) :: ext_names(MAX_EXT_DEPS)
        character(len=HASH_LEN) :: ext_keys(MAX_EXT_DEPS)
        integer :: n_ext
        character(len=MAX_NAME), allocatable :: local_filenames(:)
        logical :: has_cycle

        allocate (units(MAX_UNITS), order(MAX_NODES))
        allocate (keys(MAX_NODES), local_filenames(MAX_NODES))

        ierr = 0
        n_changed = 0
        n_affected = 0
        n_cached = 0
        n_ext = 0

        b = detect_backend(dir)
        if (b%kind == BACKEND_NONE) then
            ierr = 1
            return
        end if

        call scan_dir(trim(b%project_dir), units, n_units, ierr)
        if (ierr /= 0) return

        call build_dag_from_units(units, n_units, dag, local_filenames, is_test_arr)
        if (present(filenames)) filenames = local_filenames
        call dag_topo_sort(dag, order, n_order, has_cycle)
        if (present(n_in_cycle)) n_in_cycle = dag%n_nodes - n_order
        ierr = 0

        call detect_compiler(compiler)
        call cache_init(c, ierr)
        if (ierr /= 0) return

        ! collect and hash external deps (modules used but not in DAG)
        call collect_external_dep_hashes(units, n_units, dag, b, &
                                         ext_names, ext_keys, n_ext)

        keys = ''
        do i = 1, n_order
            node_id = order(i)

            ! collect in-DAG dep keys
            n_dep_keys = 0
            do j = 1, dag%nodes(node_id)%n_edges
                dep_id = dag%nodes(node_id)%edges(j)
                if (dep_id > 0 .and. len_trim(keys(dep_id)) > 0) then
                    n_dep_keys = n_dep_keys + 1
                    if (n_dep_keys <= 64) dep_keys(n_dep_keys) = keys(dep_id)
                end if
            end do

            ! add external dep hashes for any unresolved uses in this unit
            call add_ext_dep_keys(units, n_units, dag, node_id, &
                                  ext_names, ext_keys, n_ext, dep_keys, n_dep_keys)

            keys(node_id) = cache_key_for( &
                            local_filenames(node_id), compiler, '', &
                            dep_keys, n_dep_keys)

            if (cache_lookup(c, dag%nodes(node_id)%label, keys(node_id))) then
                n_cached = n_cached + 1
            else
                n_changed = n_changed + 1
                changed_ids(n_changed) = node_id
            end if
        end do

        if (n_changed > 0) then
            call dag_affected_set(dag, changed_ids, n_changed, &
                                  affected_ids, n_affected)
        end if

        do i = 1, n_order
            node_id = order(i)
            call cache_store(c, dag%nodes(node_id)%label, keys(node_id))
        end do
    end subroutine fo_changed_modules

    subroutine collect_external_dep_hashes(units, n_units, dag, b, &
                                           ext_names, ext_keys, n_ext)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units
        type(dag_t), intent(in) :: dag
        type(backend_t), intent(in) :: b
        character(len=MAX_NAME), intent(out) :: ext_names(MAX_EXT_DEPS)
        character(len=HASH_LEN), intent(out) :: ext_keys(MAX_EXT_DEPS)
        integer, intent(out) :: n_ext

        integer :: i, j, k
        character(len=MAX_NAME) :: dep_name
        character(len=512) :: modpath
        logical :: found, already

        n_ext = 0

        do i = 1, n_units
            do j = 1, units(i)%n_deps
                dep_name = units(i)%deps(j)
                if (dag_find_node(dag, dep_name) > 0) cycle

                ! skip if already collected
                already = .false.
                do k = 1, n_ext
                    if (trim(ext_names(k)) == trim(dep_name)) then
                        already = .true.
                        exit
                    end if
                end do
                if (already) cycle

                ! search for .mod file in build directories
                call find_mod_file(dep_name, b, modpath, found)
                if (found) then
                    if (n_ext < MAX_EXT_DEPS) then
                        n_ext = n_ext + 1
                        ext_names(n_ext) = dep_name
                        call hash_mod_file(modpath, ext_keys(n_ext))
                    end if
                end if
            end do
        end do
    end subroutine collect_external_dep_hashes

    subroutine find_mod_file(modname, b, modpath, found)
        character(len=*), intent(in) :: modname
        type(backend_t), intent(in) :: b
        character(len=*), intent(out) :: modpath
        logical, intent(out) :: found

        character(len=MAX_NAME) :: lower_name
        integer :: i

        found = .false.
        modpath = ''

        lower_name = modname
        do i = 1, len_trim(lower_name)
            if (iachar(lower_name(i:i)) >= iachar('A') .and. &
                iachar(lower_name(i:i)) <= iachar('Z')) then
                lower_name(i:i) = achar(iachar(lower_name(i:i)) + 32)
            end if
        end do

        ! fpm build tree: build/dependencies/*/*.mod and build/gfortran_*/*.mod
        ! cmake build tree: build/**/*.mod
        ! search with find for the .mod file
        block
            character(len=1024) :: cmd, tmpfile, line
            integer :: u, iostat

            call make_tmpfile('fo_find_mod', tmpfile)
            cmd = 'find '//trim(b%project_dir)//'/build'// &
                  " -name '"//trim(lower_name)//".mod'"// &
                  ' -type f 2>/dev/null | head -1 > '//trim(tmpfile)
            call execute_command_line(cmd, wait=.true.)

            open (newunit=u, file=tmpfile, status='old', iostat=iostat)
            if (iostat == 0) then
                read (u, '(a)', iostat=iostat) line
                if (iostat == 0 .and. len_trim(line) > 0) then
                    modpath = trim(line)
                    found = .true.
                end if
                close (u)
            end if
            call delete_tmpfile(tmpfile)
        end block
    end subroutine find_mod_file

    subroutine add_ext_dep_keys(units, n_units, dag, node_id, &
                                ext_names, ext_keys, n_ext, dep_keys, n_dep_keys)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units
        type(dag_t), intent(in) :: dag
        integer, intent(in) :: node_id
        character(len=MAX_NAME), intent(in) :: ext_names(MAX_EXT_DEPS)
        character(len=HASH_LEN), intent(in) :: ext_keys(MAX_EXT_DEPS)
        integer, intent(in) :: n_ext
        character(len=HASH_LEN), intent(inout) :: dep_keys(64)
        integer, intent(inout) :: n_dep_keys

        integer :: i, j, k
        character(len=MAX_NAME) :: node_name

        ! find the scan unit for this node
        node_name = dag%nodes(node_id)%label(1:MAX_NAME)
        do i = 1, n_units
            if (trim(units(i)%module_name) == trim(node_name) .or. &
                trim(units(i)%program_name) == trim(node_name)) then
                ! check each of this unit's deps
                do j = 1, units(i)%n_deps
                    if (dag_find_node(dag, units(i)%deps(j)) > 0) cycle
                    ! external dep: find its hash
                    do k = 1, n_ext
                        if (trim(ext_names(k)) == trim(units(i)%deps(j))) then
                            if (n_dep_keys < 64) then
                                n_dep_keys = n_dep_keys + 1
                                dep_keys(n_dep_keys) = ext_keys(k)
                            end if
                            exit
                        end if
                    end do
                end do
                return
            end if
        end do
    end subroutine add_ext_dep_keys

    subroutine fo_check_run(dir, res)
        character(len=*), intent(in) :: dir
        type(check_result_t), intent(out) :: res

        type(dag_t) :: dag
        type(backend_t) :: backend
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, exitcode
        real :: t0, t1
        character(len=512) :: build_log, test_log
        character(len=512) :: no_project

        call cpu_time(t0)

        backend = detect_backend(dir)
        if (backend%kind == BACKEND_NONE) then
            no_project = 'no fpm.toml or CMakeLists.txt found'
            no_project = trim(no_project)//' in directory or parents: '//trim(dir)
            call set_failure(res, 'backend', '', no_project, &
                             'run fo from a project directory', &
                             'fo check', '')
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        call fo_changed_modules(trim(backend%project_dir), dag, changed_ids, &
                                n_changed, affected_ids, n_affected, n_cached, &
                                ierr, res%n_in_cycle)
        if (ierr /= 0) then
            call set_failure(res, 'scan', '', 'scan or dag failed', &
                             'check source parsing and module cycles', &
                             'fo changed', '')
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        res%n_modules = dag%n_nodes
        res%n_cached = n_cached
        res%n_changed = n_changed
        res%n_affected = n_affected

        ! try restoring cached artifacts before build
        block
            integer :: n_restored, art_ierr
            call artifact_restore(trim(backend%project_dir)//'/build', &
                                  n_restored, art_ierr)
        end block

        call make_tmpfile('fo-build', build_log)
        call backend%build(exitcode, log_file=build_log)
        if (exitcode /= 0) then
            call summarize_backend_failure('build', build_log, 'fo build', res)
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if
        call delete_tmpfile(build_log)
        res%build_ok = .true.

        ! cache artifacts after successful build
        block
            integer :: art_ierr
            call artifact_store(trim(backend%project_dir)//'/build', art_ierr)
        end block

        call make_tmpfile('fo-test', test_log)
        call backend%test(exitcode, log_file=test_log)
        res%tests_ok = (exitcode == 0)
        call parse_test_log(test_log, res%test_results, res%n_test_results)
        if (.not. res%tests_ok) then
            call summarize_backend_failure('test', test_log, 'fo test', res)
        else
            call delete_tmpfile(test_log)
        end if

        call cpu_time(t1)
        res%elapsed = t1 - t0
    end subroutine fo_check_run

    subroutine summarize_backend_failure(stage, log_file, rerun, res)
        character(len=*), intent(in) :: stage, log_file, rerun
        type(check_result_t), intent(inout) :: res

        type(diagnostic_t) :: diag

        call diagnostic_from_log(stage, log_file, rerun, diag)
        if (trim(stage) == 'test' .and. is_runner_crash(diag%message)) then
            diag%hint = 'runner crash (not a test failure); check fpm/OpenMP'
        end if
        call set_failure(res, stage, diag%target, diag%message, &
                         diag%hint, diag%rerun, log_file)
        res%diag_file = diag%file
        res%diag_line = diag%line
        res%diag_column = diag%column
        if (trim(rerun) == 'fo test') then
            res%error_msg = trim(res%error_msg)// &
                            '; slow: make timed-out tests faster or name them *_slow'
            res%error_msg = trim(res%error_msg)// &
                            '; use fo test --all for the slow suite'
        end if
    end subroutine summarize_backend_failure

    subroutine set_failure(res, stage, target, summary, hint, rerun, log_path)
        type(check_result_t), intent(inout) :: res
        character(len=*), intent(in) :: stage, target, summary, hint
        character(len=*), intent(in) :: rerun, log_path

        res%stage = stage
        res%target = target
        res%summary = summary
        res%hint = hint
        res%rerun = rerun
        res%log_path = log_path
        res%error_msg = trim(stage)//' failed: '//trim(res%summary)
        if (len_trim(res%log_path) > 0) then
            res%error_msg = trim(res%error_msg)//'; log: '//trim(res%log_path)
        end if
        if (len_trim(res%rerun) > 0) then
            res%error_msg = trim(res%error_msg)//'; rerun: '//trim(res%rerun)
        end if
    end subroutine set_failure

    subroutine parse_test_log(log_file, results, n_results)
        character(len=*), intent(in) :: log_file
        type(test_result_t), intent(out) :: results(MAX_TEST_RESULTS)
        integer, intent(out) :: n_results

        character(len=512) :: line
        character(len=128) :: name
        integer :: u, iostat, io, colon_pos, pass_pos, fail_pos, comma_pos
        integer :: n_pass, n_fail

        n_results = 0
        open (newunit=u, file=log_file, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            colon_pos = index(line, ': ')
            if (colon_pos < 2) cycle
            name = line(1:colon_pos - 1)
            if (len_trim(name) == 0) cycle
            line = adjustl(line(colon_pos + 2:))
            pass_pos = index(line, ' pass')
            fail_pos = index(line, ' fail')
            comma_pos = index(line, ',')
            if (pass_pos < 2 .or. fail_pos < 2 .or. &
                comma_pos < 2 .or. comma_pos >= fail_pos) cycle
            read (line(1:pass_pos - 1), *, iostat=io) n_pass
            if (io /= 0) cycle
            read (line(comma_pos + 1:fail_pos - 1), *, iostat=io) n_fail
            if (io /= 0) cycle
            if (n_results < MAX_TEST_RESULTS) then
                n_results = n_results + 1
                results(n_results)%name = trim(name)
                results(n_results)%n_pass = n_pass
                results(n_results)%n_fail = n_fail
                if (n_fail == 0) then
                    results(n_results)%status = 'pass'
                else
                    results(n_results)%status = 'fail'
                end if
            end if
        end do
        close (u)
    end subroutine parse_test_log

    subroutine detect_compiler(compiler)
        character(len=*), intent(out) :: compiler

        character(len=256) :: line
        character(len=512) :: tmpfile, cmd
        integer :: u, iostat

        compiler = 'unknown'
        call make_tmpfile('fo_compiler_version', tmpfile)
        cmd = 'gfortran --version 2>/dev/null | head -1 > '//trim(tmpfile)
        call execute_command_line(cmd, wait=.true.)

        open (newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat == 0) then
            read (u, '(a)', iostat=iostat) line
            if (iostat == 0) compiler = trim(line)
            close (u)
        end if
        call delete_tmpfile(tmpfile)
    end subroutine detect_compiler

end module fo_check
