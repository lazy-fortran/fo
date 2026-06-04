module fo_check
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_scan, only: scan_unit_t, scan_file, scan_dir, MAX_NAME, MAX_UNITS
    use fo_dag, only: dag_t, dag_build, dag_topo_order, dag_reverse_deps, MAX_NODES
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE, &
                                BACKEND_FPM, BACKEND_CMAKE
    use fo_cache, only: cache_t, cache_init, cache_lookup, cache_store, &
                        cache_key_for, hash_mod_file, HASH_LEN
    use fo_artifact_cache, only: artifact_store, artifact_restore
    implicit none
    private
    public :: check_result_t, fo_check_run, fo_changed_modules

    integer, parameter :: MAX_EXT_DEPS = 256

    type :: check_result_t
        logical :: build_ok = .false.
        logical :: tests_ok = .false.
        integer :: n_modules = 0
        integer :: n_cached = 0
        integer :: n_changed = 0
        integer :: n_affected = 0
        integer :: n_ext_deps = 0
        real :: elapsed = 0.0
        character(len=512) :: error_msg = ''
    end type check_result_t

contains

    subroutine fo_changed_modules(dir, dag, changed_ids, n_changed, &
                                  affected_ids, n_affected, n_cached, ierr)
        character(len=*), intent(in) :: dir
        type(dag_t), intent(out) :: dag
        integer, intent(out) :: changed_ids(MAX_NODES), n_changed
        integer, intent(out) :: affected_ids(MAX_NODES), n_affected
        integer, intent(out) :: n_cached, ierr

        type(scan_unit_t) :: units(MAX_UNITS)
        type(cache_t) :: c
        type(backend_t) :: b
        integer :: n_units
        integer :: order(MAX_NODES), n_order
        integer :: i, node_id, j, dep_id, n_dep_keys
        character(len=HASH_LEN) :: keys(MAX_NODES)
        character(len=HASH_LEN) :: dep_keys(64)
        character(len=256) :: compiler
        ! external dep hashes (global across all modules)
        character(len=HASH_LEN) :: ext_hash
        character(len=MAX_NAME) :: ext_names(MAX_EXT_DEPS)
        character(len=HASH_LEN) :: ext_keys(MAX_EXT_DEPS)
        integer :: n_ext

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

        call dag_build(units, n_units, dag)
        call dag_topo_order(dag, order, n_order, ierr)
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
            do j = 1, dag%nodes(node_id)%n_deps
                dep_id = dag%nodes(node_id)%dep_ids(j)
                if (dep_id > 0 .and. len_trim(keys(dep_id)) > 0) then
                    n_dep_keys = n_dep_keys + 1
                    if (n_dep_keys <= 64) dep_keys(n_dep_keys) = keys(dep_id)
                end if
            end do

            ! add external dep hashes for any unresolved uses in this unit
            call add_ext_dep_keys(units, n_units, dag, node_id, &
                                  ext_names, ext_keys, n_ext, dep_keys, n_dep_keys)

            keys(node_id) = cache_key_for( &
                            dag%nodes(node_id)%filename, compiler, '', &
                            dag, dep_keys, n_dep_keys)

            if (cache_lookup(c, dag%nodes(node_id)%name, keys(node_id))) then
                n_cached = n_cached + 1
            else
                n_changed = n_changed + 1
                changed_ids(n_changed) = node_id
            end if
        end do

        if (n_changed > 0) then
            call dag_reverse_deps(dag, changed_ids, n_changed, &
                                  affected_ids, n_affected)
        end if

        do i = 1, n_order
            node_id = order(i)
            call cache_store(c, dag%nodes(node_id)%name, keys(node_id))
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
                if (dag%find(dep_name) > 0) cycle

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

        character(len=512) :: candidate
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

            tmpfile = '/tmp/fo_find_mod.tmp'
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
            call execute_command_line('rm -f '//trim(tmpfile), wait=.true.)
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
        node_name = dag%nodes(node_id)%name
        do i = 1, n_units
            if (trim(units(i)%module_name) == trim(node_name) .or. &
                trim(units(i)%program_name) == trim(node_name)) then
                ! check each of this unit's deps
                do j = 1, units(i)%n_deps
                    if (dag%find(units(i)%deps(j)) > 0) cycle
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

        call cpu_time(t0)

        backend = detect_backend(dir)
        if (backend%kind == BACKEND_NONE) then
            res%error_msg = &
                'no fpm.toml or CMakeLists.txt found in directory or parents: '// &
                trim(dir)
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        call fo_changed_modules(trim(backend%project_dir), dag, changed_ids, &
                                n_changed, affected_ids, n_affected, n_cached, ierr)
        if (ierr /= 0) then
            res%error_msg = 'scan or dag failed'
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        res%n_modules = dag%n
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
            call summarize_backend_failure('build failed', build_log, &
                                           'fo build', res%error_msg)
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if
        call delete_file(build_log)
        res%build_ok = .true.

        ! cache artifacts after successful build
        block
            integer :: art_ierr
            call artifact_store(trim(backend%project_dir)//'/build', art_ierr)
        end block

        call make_tmpfile('fo-test', test_log)
        call backend%test(exitcode, log_file=test_log)
        res%tests_ok = (exitcode == 0)
        if (.not. res%tests_ok) then
            call summarize_backend_failure('tests failed', test_log, &
                                           'fo test', res%error_msg)
        else
            call delete_file(test_log)
        end if

        call cpu_time(t1)
        res%elapsed = t1 - t0
    end subroutine fo_check_run

    subroutine make_tmpfile(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path

        integer :: count

        call system_clock(count)
        write (path, '(a,a,a,i0,a)') '/tmp/', trim(prefix), '-', count, '.log'
    end subroutine make_tmpfile

    subroutine summarize_backend_failure(stage, log_file, rerun, message)
        character(len=*), intent(in) :: stage, log_file, rerun
        character(len=*), intent(out) :: message

        character(len=512) :: summary, fallback, line
        integer :: u, iostat, best_priority

        summary = ''
        fallback = ''
        best_priority = 0

        open (newunit=u, file=log_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (u, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                call consider_log_line(line, summary, fallback, best_priority)
            end do
            close (u)
        end if

        if (len_trim(summary) == 0) summary = fallback
        if (len_trim(summary) == 0) summary = 'backend returned nonzero status'

        message = trim(stage)//': '//trim(summary)//'; log: '// &
                  trim(log_file)//'; rerun: '//trim(rerun)
        if (trim(rerun) == 'fo test') then
            message = trim(message)//'; slow: make timed-out tests faster'
            message = trim(message)//' or name them *_slow'
            message = trim(message)//'; use fo test --all for the slow suite'
        end if
    end subroutine summarize_backend_failure

    subroutine consider_log_line(line, summary, fallback, best_priority)
        character(len=*), intent(in) :: line
        character(len=*), intent(inout) :: summary, fallback
        integer, intent(inout) :: best_priority

        character(len=512) :: clean
        integer :: priority

        clean = adjustl(line)
        if (len_trim(clean) == 0) return
        if (trim(clean) == 'STOP 1') return
        if (index(clean, 'Backtrace') > 0) return

        fallback = clean

        priority = 0
        if (index(clean, 'Fatal Error:') > 0 .or. &
            index(clean, 'Cannot open file') > 0) then
            priority = 5
        else if (index(clean, 'Error:') > 0 .or. &
                 index(clean, 'error:') > 0) then
            priority = 4
        else if (index(clean, 'FAIL:') > 0) then
            priority = 3
        else if (index(clean, 'returned exit code') > 0) then
            priority = 2
        else if (index(clean, '<ERROR>') > 0 .or. &
                 index(clean, 'FAIL') > 0) then
            priority = 1
        end if

        if (priority > 0 .and. priority >= best_priority) then
            summary = clean
            best_priority = priority
        end if
    end subroutine consider_log_line

    subroutine delete_file(path)
        character(len=*), intent(in) :: path

        call execute_command_line('rm -f '//trim(path), wait=.true.)
    end subroutine delete_file

    subroutine detect_compiler(compiler)
        character(len=*), intent(out) :: compiler

        character(len=256) :: line
        character(len=512) :: tmpfile, cmd
        integer :: u, iostat

        compiler = 'unknown'
        tmpfile = '/tmp/fo_compiler_version.tmp'
        cmd = 'gfortran --version 2>/dev/null | head -1 > '//trim(tmpfile)
        call execute_command_line(cmd, wait=.true.)

        open (newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat == 0) then
            read (u, '(a)', iostat=iostat) line
            if (iostat == 0) compiler = trim(line)
            close (u)
        end if
        call execute_command_line('rm -f '//trim(tmpfile), wait=.true.)
    end subroutine detect_compiler

end module fo_check
