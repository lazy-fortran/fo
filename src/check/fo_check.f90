module fo_check
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_scan, only: scan_unit_t, scan_dir, MAX_NAME, MAX_UNITS, is_slow_test
    use fx_dag, only: dag_t, dag_find_node, dag_topo_sort, dag_affected_set, MAX_NODES
    use fo_dag_bridge, only: build_dag_from_units
    use fo_process, only: process_detect_nproc, process_fpm_build, &
                          process_fpm_test_list, process_fpm_test_all, &
                          process_fpm_test_names, process_cmake_build, &
                          process_ctest
    use fo_gfortran_build, only: gfortran_build, gfortran_test, &
                                 gfortran_test_names
    use fo_cache, only: cache_t, cache_init, cache_lookup, cache_key_for, &
                        cache_action_mod_key, hash_mod_file, HASH_LEN
    use fo_diagnostics, only: diagnostic_t, diagnostic_from_log, is_runner_crash
    implicit none
    private
    public :: check_result_t, test_result_t, fo_check_run, fo_changed_modules
    public :: MAX_TEST_RESULTS

    integer, parameter :: MAX_EXT_DEPS = 256
    integer, parameter :: MAX_TEST_RESULTS = 64
    integer, parameter :: MAX_TEST_TARGETS = 512
    integer, parameter :: BACKEND_NONE = 0
    integer, parameter :: BACKEND_FPM = 1
    integer, parameter :: BACKEND_CMAKE = 2
    integer, parameter :: BACKEND_GFORTRAN = 3

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

        call fo_changed_modules_impl(dir, dag, changed_ids, n_changed, &
                                     affected_ids, n_affected, n_cached, ierr, &
                                     n_in_cycle, filenames, is_test_arr)
    end subroutine fo_changed_modules

    subroutine fo_changed_modules_impl(dir, dag, changed_ids, n_changed, &
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
        integer :: n_units
        integer :: backend_kind
        character(len=512) :: project_dir
        integer, allocatable :: order(:)
        integer :: n_order
        integer :: i, node_id, n_dep_keys
        character(len=HASH_LEN), allocatable :: keys(:)
        character(len=HASH_LEN), allocatable :: mod_keys(:)
        character(len=HASH_LEN) :: dep_keys(64)
        character(len=256) :: compiler
        character(len=MAX_NAME) :: ext_names(MAX_EXT_DEPS)
        character(len=HASH_LEN) :: ext_keys(MAX_EXT_DEPS)
        integer :: n_ext
        character(len=MAX_NAME), allocatable :: local_filenames(:)
        logical, allocatable :: local_is_test_arr(:)
        logical :: has_cycle, found_mod_key

        allocate (units(MAX_UNITS), order(MAX_NODES))
        allocate (keys(MAX_NODES), mod_keys(MAX_NODES), local_filenames(MAX_NODES))
        allocate (local_is_test_arr(MAX_NODES))

        ierr = 0
        n_changed = 0
        n_affected = 0
        n_cached = 0
        n_ext = 0
        backend_kind = detect_backend_kind(dir, project_dir)
        if (backend_kind == BACKEND_NONE) then
            ierr = 1
            return
        end if

        call scan_dir(trim(project_dir), units, n_units, ierr)
        if (ierr /= 0) return

        call build_dag_from_units(units, n_units, dag, local_filenames, &
                                  local_is_test_arr)
        if (present(filenames)) filenames = local_filenames
        if (present(is_test_arr)) is_test_arr = local_is_test_arr
        call dag_topo_sort(dag, order, n_order, has_cycle)
        if (present(n_in_cycle)) n_in_cycle = dag%n_nodes - n_order
        ierr = 0

        call detect_compiler(compiler)
        call cache_init(c, ierr)
        if (ierr /= 0) return

        ! collect and hash external deps (modules used but not in DAG)
        call collect_external_dep_hashes(units, n_units, dag, project_dir, &
                                         ext_names, ext_keys, n_ext)

        keys = ''
        mod_keys = ''
        do i = 1, n_order
            node_id = order(i)

            call collect_dep_keys_source_order(units, n_units, dag, node_id, &
                                               mod_keys, ext_names, ext_keys, n_ext, &
                                               dep_keys, n_dep_keys)

            keys(node_id) = cache_key_for( &
                            local_filenames(node_id), compiler, '', &
                            dep_keys, n_dep_keys)

            if (cache_lookup(c, keys(node_id))) then
                n_cached = n_cached + 1
                call cache_action_mod_key(c, keys(node_id), mod_keys(node_id), &
                                          found_mod_key)
            else
                n_changed = n_changed + 1
                changed_ids(n_changed) = node_id
            end if
        end do

        if (n_changed > 0) then
            call dag_affected_set(dag, changed_ids, n_changed, &
                                  affected_ids, n_affected)
        end if

    end subroutine fo_changed_modules_impl

    subroutine collect_external_dep_hashes(units, n_units, dag, project_dir, &
                                           ext_names, ext_keys, n_ext)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units
        type(dag_t), intent(in) :: dag
        character(len=*), intent(in) :: project_dir
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
                call find_mod_file(dep_name, project_dir, modpath, found)
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

    subroutine find_mod_file(modname, project_dir, modpath, found)
        character(len=*), intent(in) :: modname
        character(len=*), intent(in) :: project_dir
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

        block
            character(len=2048) :: cmd
            character(len=512) :: tmpfile, line, cc_file, dep_dir, mod_file
            integer :: u, iostat

            call make_tmpfile('fo_find_mod', tmpfile)
            cc_file = trim(project_dir)//'/build/compile_commands.json'
            dep_dir = trim(project_dir)//'/build/dependencies'
            mod_file = trim(lower_name)//'.mod'
            cmd = '( test -d '//sq(trim(project_dir)//'/build/fo/mod')// &
                  ' && printf "%s\n" '//sq(trim(project_dir)//'/build/fo/mod')// &
                  '; if [ -f '//sq(trim(cc_file))// &
                  ' ]; then grep -o ''build/gfortran_[A-Za-z0-9_]*'' '// &
                  sq(trim(cc_file))//' | awk -v p='//sq(trim(project_dir))// &
                  ' ''{print p "/" $0}''; fi; '// &
                  'find '//sq(trim(dep_dir))// &
                  ' -type f -name "*.mod" -printf "%h\n" 2>/dev/null ) | '// &
                  'sort -u | while IFS= read -r d; do test -f "$d/'// &
                  trim(mod_file)//'" && printf "%s\n" "$d/'//trim(mod_file)// &
                  '"; done | head -1 > '//trim(tmpfile)
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

    subroutine collect_dep_keys_source_order(units, n_units, dag, node_id, &
                                             mod_keys, ext_names, ext_keys, n_ext, &
                                             dep_keys, n_dep_keys)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units, node_id, n_ext
        type(dag_t), intent(in) :: dag
        character(len=HASH_LEN), intent(in) :: mod_keys(MAX_NODES)
        character(len=MAX_NAME), intent(in) :: ext_names(MAX_EXT_DEPS)
        character(len=HASH_LEN), intent(in) :: ext_keys(MAX_EXT_DEPS)
        character(len=HASH_LEN), intent(out) :: dep_keys(64)
        integer, intent(out) :: n_dep_keys

        integer :: i, j, k, dep_id
        character(len=MAX_NAME) :: node_name

        dep_keys = ''
        n_dep_keys = 0
        node_name = dag%nodes(node_id)%label(1:MAX_NAME)
        do i = 1, n_units
            if (trim(units(i)%module_name) /= trim(node_name) .and. &
                trim(units(i)%program_name) /= trim(node_name)) cycle

            do j = 1, units(i)%n_deps
                if (n_dep_keys >= 64) return
                dep_id = dag_find_node(dag, units(i)%deps(j))
                if (dep_id > 0) then
                    if (len_trim(mod_keys(dep_id)) == 0) cycle
                    n_dep_keys = n_dep_keys + 1
                    dep_keys(n_dep_keys) = mod_keys(dep_id)
                else
                    do k = 1, n_ext
                        if (trim(ext_names(k)) == trim(units(i)%deps(j))) then
                            n_dep_keys = n_dep_keys + 1
                            dep_keys(n_dep_keys) = ext_keys(k)
                            exit
                        end if
                    end do
                end if
            end do
            return
        end do
    end subroutine collect_dep_keys_source_order

    subroutine fo_check_run(dir, res)
        character(len=*), intent(in) :: dir
        type(check_result_t), intent(out) :: res

        type(dag_t) :: dag
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, exitcode
        integer :: backend_kind
        real :: t0, t1
        character(len=512) :: build_log, test_log
        character(len=512) :: no_project
        character(len=512) :: project_dir
        character(len=128) :: test_names(MAX_NODES)
        integer :: i, n_test_names
        logical :: is_test_arr(MAX_NODES)

        call cpu_time(t0)

        backend_kind = detect_backend_kind(dir, project_dir)
        if (backend_kind == BACKEND_NONE) then
            no_project = 'no fpm.toml or CMakeLists.txt found'
            no_project = trim(no_project)//' in directory or parents: '//trim(dir)
            call set_failure(res, 'backend', '', no_project, &
                             'run fo from a project directory', &
                             'fo check', '')
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        call fo_changed_modules(trim(project_dir), dag, changed_ids, &
                                n_changed, affected_ids, n_affected, n_cached, &
                                ierr, res%n_in_cycle, &
                                is_test_arr=is_test_arr)
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

        if (n_changed == 0) then
            call make_tmpfile('fo-build', build_log)
            call run_backend_build(backend_kind, project_dir, build_log, exitcode)
            if (exitcode /= 0) then
                call summarize_backend_failure('build', build_log, 'fo build', res)
                call cpu_time(t1)
                res%elapsed = t1 - t0
                return
            end if
            call delete_tmpfile(build_log)
            res%build_ok = .true.
            res%tests_ok = .true.
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        call make_tmpfile('fo-build', build_log)
        call run_backend_build(backend_kind, project_dir, build_log, exitcode)
        if (exitcode /= 0) then
            call summarize_backend_failure('build', build_log, 'fo build', res)
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if
        call delete_tmpfile(build_log)
        res%build_ok = .true.

        n_test_names = 0
        do i = 1, n_affected
            if (is_test_arr(affected_ids(i))) then
                if (.not. is_slow_test(dag%nodes(affected_ids(i))%label)) then
                    n_test_names = n_test_names + 1
                    test_names(n_test_names) = dag%nodes(affected_ids(i))%label(1:128)
                end if
            end if
        end do

        if (n_test_names == 0) then
            res%tests_ok = .true.
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        call make_tmpfile('fo-test', test_log)
        call run_backend_tests(backend_kind, project_dir, test_names, n_test_names, &
                               test_log, exitcode)
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

    integer function detect_backend_kind(dir, project_dir) result(kind)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: project_dir

        character(len=512) :: current, parent
        logical :: has_fpm, has_cmake
        integer :: depth

        kind = BACKEND_NONE
        project_dir = ''
        current = absolute_dir(dir)

        do depth = 1, 64
            inquire (file=trim(current)//'/fpm.toml', exist=has_fpm)
            inquire (file=trim(current)//'/CMakeLists.txt', exist=has_cmake)
            if (has_cmake) then
                kind = BACKEND_CMAKE
                project_dir = current
                return
            else if (has_fpm) then
                kind = BACKEND_GFORTRAN
                project_dir = current
                return
            end if

            call parent_dir(current, parent)
            if (trim(parent) == trim(current)) exit
            current = parent
        end do
    end function detect_backend_kind

    function absolute_dir(dir) result(absdir)
        character(len=*), intent(in) :: dir
        character(len=512) :: absdir

        character(len=512) :: pwd

        if (len_trim(dir) == 0) then
            absdir = '.'
        else if (dir(1:1) == '/') then
            absdir = trim(dir)
        else
            call get_environment_variable('PWD', pwd)
            if (len_trim(pwd) > 0) then
                if (trim(dir) == '.') then
                    absdir = trim(pwd)
                else
                    absdir = trim(pwd)//'/'//trim(dir)
                end if
            else
                absdir = trim(dir)
            end if
        end if
    end function absolute_dir

    subroutine parent_dir(path, parent)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: parent

        character(len=512) :: clean
        integer :: n, last

        clean = trim(path)
        n = len_trim(clean)
        do while (n > 1 .and. clean(n:n) == '/')
            clean(n:n) = ' '
            n = n - 1
        end do

        if (trim(clean) == '/') then
            parent = '/'
            return
        end if

        last = index(trim(clean), '/', back=.true.)
        if (last <= 1) then
            parent = '/'
        else
            parent = clean(1:last - 1)
        end if
    end subroutine parent_dir

    integer function detect_jobs() result(jobs)
        character(len=32) :: buf
        integer :: status, iostat

        jobs = process_detect_nproc()
        call get_environment_variable('FO_JOBS', buf, status=status)
        if (status /= 0 .or. len_trim(buf) == 0) return

        read (buf, *, iostat=iostat) jobs
        if (iostat /= 0 .or. jobs < 1) jobs = process_detect_nproc()
    end function detect_jobs

    subroutine run_backend_build(kind, project_dir, log_file, exitcode)
        integer, intent(in) :: kind
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode

        integer :: jobs

        select case (kind)
        case (BACKEND_GFORTRAN)
            call gfortran_build(project_dir, log_file, exitcode)
        case (BACKEND_FPM)
            jobs = detect_jobs()
            call process_fpm_build(project_dir, '', jobs, log_file, exitcode)
            if (exitcode /= 0 .and. exitcode /= 124 .and. len_trim(log_file) > 0) then
                if (log_has_vtable_mismatch(log_file)) then
                    call clear_fpm_mod_cache(project_dir)
                    call process_fpm_build(project_dir, '', jobs, log_file, &
                                           exitcode)
                end if
            end if
        case (BACKEND_CMAKE)
            jobs = detect_jobs()
            call process_cmake_build(project_dir, '', jobs, log_file, exitcode)
        case default
            exitcode = 1
        end select
    end subroutine run_backend_build

    subroutine run_backend_tests(kind, project_dir, names, n_names, log_file, &
                                 exitcode)
        integer, intent(in) :: kind
        character(len=*), intent(in) :: project_dir, log_file
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode

        integer :: jobs
        character(len=1024) :: regex

        select case (kind)
        case (BACKEND_GFORTRAN)
            if (n_names > 0) then
                call gfortran_test_names(project_dir, names, n_names, log_file, &
                                         exitcode)
            else
                call gfortran_test(project_dir, log_file, exitcode)
            end if
        case (BACKEND_FPM)
            jobs = detect_jobs()
            if (n_names > 0) then
                call process_fpm_test_names(project_dir, names, n_names, jobs, &
                                            log_file, exitcode)
            else
                call run_fpm_tests(project_dir, log_file, exitcode)
            end if
        case (BACKEND_CMAKE)
            jobs = detect_jobs()
            if (n_names > 0) then
                call names_to_ctest_regex(names, n_names, regex)
                call process_ctest(project_dir, jobs, regex, .false., log_file, &
                                   exitcode)
            else
                call process_ctest(project_dir, jobs, '', .false., log_file, &
                                   exitcode)
            end if
        case default
            exitcode = 1
        end select
    end subroutine run_backend_tests

    subroutine run_fpm_tests(project_dir, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode

        character(len=128) :: names(MAX_TEST_TARGETS)
        integer :: n_names, list_ierr

        call fpm_list_tests(project_dir, names, n_names, list_ierr, log_file)
        if (list_ierr /= 0) then
            exitcode = 1
            return
        end if
        call filter_slow_tests(names, n_names)
        if (n_names == 0) then
            exitcode = 0
            return
        end if
        call process_fpm_test_names(project_dir, names, n_names, detect_jobs(), &
                                    log_file, exitcode)
    end subroutine run_fpm_tests

    subroutine fpm_list_tests(project_dir, names, n_names, exitcode, log_file)
        character(len=*), intent(in) :: project_dir
        character(len=128), intent(out) :: names(MAX_TEST_TARGETS)
        integer, intent(out) :: n_names, exitcode
        character(len=*), intent(in) :: log_file

        character(len=512) :: list_file

        names = ''
        n_names = 0
        call process_fpm_test_list(project_dir, log_file, exitcode)
        if (exitcode /= 0) return
        list_file = log_file
        call parse_fpm_test_list(list_file, names, n_names)
    end subroutine fpm_list_tests

    subroutine parse_fpm_test_list(path, names, n_names)
        character(len=*), intent(in) :: path
        character(len=128), intent(inout) :: names(MAX_TEST_TARGETS)
        integer, intent(inout) :: n_names

        character(len=512) :: line
        integer :: u, iostat, colon
        logical :: in_names

        in_names = .false.
        open (newunit=u, file=path, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            colon = index(line, 'Matched names:')
            if (colon > 0) then
                in_names = .true.
                line = line(colon + len('Matched names:'):)
            else if (.not. in_names) then
                cycle
            end if
            call parse_words(line, names, n_names)
        end do
        close (u)
    end subroutine parse_fpm_test_list

    subroutine parse_words(line, names, n_names)
        character(len=*), intent(in) :: line
        character(len=128), intent(inout) :: names(MAX_TEST_TARGETS)
        integer, intent(inout) :: n_names

        integer :: pos, start, finish, n

        n = len_trim(line)
        pos = 1
        do while (pos <= n)
            do while (pos <= n .and. line(pos:pos) == ' ')
                pos = pos + 1
            end do
            if (pos > n) exit

            start = pos
            do while (pos <= n .and. line(pos:pos) /= ' ')
                pos = pos + 1
            end do
            finish = pos - 1

            if (n_names < MAX_TEST_TARGETS) then
                n_names = n_names + 1
                names(n_names) = line(start:finish)
            end if
        end do
    end subroutine parse_words

    subroutine filter_slow_tests(names, n_names)
        character(len=128), intent(inout) :: names(MAX_TEST_TARGETS)
        integer, intent(inout) :: n_names

        character(len=128) :: fast_names(MAX_TEST_TARGETS)
        integer :: i, n_fast

        fast_names = ''
        n_fast = 0
        do i = 1, n_names
            if (is_slow_test(names(i))) cycle
            n_fast = n_fast + 1
            fast_names(n_fast) = names(i)
        end do

        names = fast_names
        n_names = n_fast
    end subroutine filter_slow_tests

    subroutine names_to_ctest_regex(names, n_names, regex)
        character(len=128), intent(in) :: names(MAX_TEST_TARGETS)
        integer, intent(in) :: n_names
        character(len=*), intent(out) :: regex

        integer :: i

        regex = '^('
        do i = 1, n_names
            if (i > 1) regex = trim(regex)//'|'
            call append_ctest_regex_name(regex, names(i))
        end do
        regex = trim(regex)//')$'
    end subroutine names_to_ctest_regex

    subroutine append_ctest_regex_name(regex, name)
        character(len=*), intent(inout) :: regex
        character(len=*), intent(in) :: name

        integer :: i
        character(len=1) :: ch

        do i = 1, len_trim(name)
            ch = name(i:i)
            select case (ch)
            case ('.', '^', '$', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|')
                regex = trim(regex)//achar(92)//ch
            case (achar(92))
                regex = trim(regex)//achar(92)//achar(92)
            case default
                regex = trim(regex)//ch
            end select
        end do
    end subroutine append_ctest_regex_name

    logical function log_has_vtable_mismatch(log_file)
        character(len=*), intent(in) :: log_file

        integer :: u, iostat
        character(len=512) :: line

        log_has_vtable_mismatch = .false.
        open (newunit=u, file=trim(log_file), status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, 'Mismatch in components of derived type') > 0 .or. &
                index(line, '__vtype_') > 0) then
                log_has_vtable_mismatch = .true.
                exit
            end if
        end do
        close (u)
    end function log_has_vtable_mismatch

    subroutine clear_fpm_mod_cache(project_dir)
        character(len=*), intent(in) :: project_dir

        character(len=1024) :: cmd
        integer :: ierr

        cmd = 'find '//trim(project_dir)//'/build' &
              //' -maxdepth 2 -name "*.mod"' &
              //' -not -path "*/dependencies/*"' &
              //' -delete 2>/dev/null'
        call execute_command_line(cmd, exitstat=ierr, wait=.true.)

        cmd = 'find '//trim(project_dir)//'/build' &
              //' -mindepth 3 -maxdepth 3 -name "src_*.o"' &
              //' -not -path "*/dependencies/*"' &
              //' -delete 2>/dev/null'
        call execute_command_line(cmd, exitstat=ierr, wait=.true.)
    end subroutine clear_fpm_mod_cache

    pure function sq(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len_trim(s) + 2) :: r

        r = "'"//trim(s)//"'"
    end function sq

end module fo_check
