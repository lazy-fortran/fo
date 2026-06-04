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
    public :: check_result_json, check_result_compact_json, check_result_full_json

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
        character(len=32) :: stage = 'done'
        character(len=128) :: target = ''
        character(len=512) :: summary = ''
        character(len=256) :: hint = ''
        character(len=256) :: rerun = ''
        character(len=512) :: log_path = ''
        character(len=256) :: diag_file = ''
        integer :: diag_line = 0
        integer :: diag_column = 0
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
                                n_changed, affected_ids, n_affected, n_cached, ierr)
        if (ierr /= 0) then
            call set_failure(res, 'scan', '', 'scan or dag failed', &
                             'check source parsing and module cycles', &
                             'fo changed', '')
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
            call summarize_backend_failure('build', build_log, 'fo build', res)
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
            call summarize_backend_failure('test', test_log, 'fo test', res)
        else
            call delete_file(test_log)
        end if

        call cpu_time(t1)
        res%elapsed = t1 - t0
    end subroutine fo_check_run

    function check_result_json(res) result(line)
        type(check_result_t), intent(in) :: res
        character(len=2048) :: line

        character(len=32) :: modules, cached, changed, affected, elapsed

        write (modules, '(i0)') res%n_modules
        write (cached, '(i0)') res%n_cached
        write (changed, '(i0)') res%n_changed
        write (affected, '(i0)') res%n_affected
        write (elapsed, '(f12.3)') res%elapsed

        line = '{'
        line = trim(line)//'"build_ok":'//trim(json_bool(res%build_ok))
        line = trim(line)//',"tests_ok":'//trim(json_bool(res%tests_ok))
        line = trim(line)//',"modules":'//trim(modules)
        line = trim(line)//',"cached":'//trim(cached)
        line = trim(line)//',"changed":'//trim(changed)
        line = trim(line)//',"affected":'//trim(affected)
        line = trim(line)//',"elapsed_s":'//trim(adjustl(elapsed))
        line = trim(line)//',"error":"'
        line = trim(line)//trim(json_escape(res%error_msg))//'"}'
    end function check_result_json

    function check_result_compact_json(res) result(line)
        type(check_result_t), intent(in) :: res
        character(len=2048) :: line

        line = make_agent_json(res, .false.)
    end function check_result_compact_json

    function check_result_full_json(res) result(line)
        type(check_result_t), intent(in) :: res
        character(len=4096) :: line

        character(len=2048) :: base

        base = check_result_json(res)
        line = base(1:len_trim(base) - 1)
        line = trim(line)//',"stage":"'//trim(json_escape(res%stage))//'"'
        line = trim(line)//',"target":"'//trim(json_escape(res%target))//'"'
        line = trim(line)//',"summary":"'//trim(json_escape(agent_summary(res)))//'"'
        line = trim(line)//',"hint":"'//trim(json_escape(res%hint))//'"'
        line = trim(line)//',"rerun":"'//trim(json_escape(res%rerun))//'"'
        line = trim(line)//',"log_path":"'//trim(json_escape(res%log_path))//'"'
        if (res%build_ok .and. res%tests_ok) then
            line = trim(line)//',"diagnostics":[]}'
        else
            line = trim(line)//',"diagnostics":[{"kind":"'// &
                   trim(json_escape(res%stage))//'"'
            if (len_trim(res%diag_file) > 0) then
                line = trim(line)//',"file":"'//trim(json_escape(res%diag_file))//'"'
            else
                line = trim(line)//',"file":""'
            end if
            line = trim(line)//',"line":'//trim(int_json(res%diag_line))
            line = trim(line)//',"column":'//trim(int_json(res%diag_column))
            line = trim(line)//',"target":"'//trim(json_escape(res%target))//'"'
            line = trim(line)//',"message":"'// &
                   trim(json_escape(agent_summary(res)))//'"'
            line = trim(line)//',"hint":"'//trim(json_escape(res%hint))//'"'
            line = trim(line)//',"rerun":"'//trim(json_escape(res%rerun))//'"}]}'
        end if
    end function check_result_full_json

    function make_agent_json(res, include_legacy) result(line)
        type(check_result_t), intent(in) :: res
        logical, intent(in) :: include_legacy
        character(len=2048) :: line

        character(len=32) :: elapsed
        logical :: ok

        ok = res%build_ok .and. res%tests_ok
        write (elapsed, '(f12.3)') res%elapsed

        line = '{'
        line = trim(line)//'"ok":'//trim(json_bool(ok))
        line = trim(line)//',"stage":"'//trim(json_escape(res%stage))//'"'
        line = trim(line)//',"target":"'//trim(json_escape(res%target))//'"'
        line = trim(line)//',"summary":"'//trim(json_escape(agent_summary(res)))//'"'
        line = trim(line)//',"hint":"'//trim(json_escape(res%hint))//'"'
        line = trim(line)//',"rerun":"'//trim(json_escape(res%rerun))//'"'
        line = trim(line)//',"log_path":"'//trim(json_escape(res%log_path))//'"'
        line = trim(line)//',"elapsed_s":'//trim(adjustl(elapsed))
        if (include_legacy) then
            line = trim(line)//',"legacy":'//trim(check_result_json(res))
        end if
        line = trim(line)//'}'
    end function make_agent_json

    function agent_summary(res) result(summary)
        type(check_result_t), intent(in) :: res
        character(len=512) :: summary

        if (len_trim(res%summary) > 0) then
            summary = res%summary
        else if (res%build_ok .and. res%tests_ok) then
            summary = 'build and tests passed'
        else if (len_trim(res%error_msg) > 0) then
            summary = res%error_msg
        else
            summary = 'fo check did not complete'
        end if
    end function agent_summary

    function json_bool(value) result(text)
        logical, intent(in) :: value
        character(len=5) :: text

        if (value) then
            text = 'true'
        else
            text = 'false'
        end if
    end function json_bool

    function int_json(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        write (text, '(i0)') value
    end function int_json

    function json_escape(input) result(output)
        character(len=*), intent(in) :: input
        character(len=1024) :: output

        integer :: i, n
        character(len=1) :: ch

        output = ''
        n = 0
        do i = 1, len_trim(input)
            ch = input(i:i)
            select case (ch)
            case ('"')
                call append_json_chars(output, n, achar(92)//achar(34))
            case (achar(92))
                call append_json_chars(output, n, achar(92)//achar(92))
            case (achar(9), achar(10), achar(13))
                call append_json_chars(output, n, ' ')
            case default
                if (iachar(ch) >= 32) call append_json_chars(output, n, ch)
            end select
        end do
    end function json_escape

    subroutine append_json_chars(output, n, text)
        character(len=*), intent(inout) :: output
        integer, intent(inout) :: n
        character(len=*), intent(in) :: text

        integer :: m

        m = len(text)
        if (m <= 0) return
        if (n + m > len(output)) return
        output(n + 1:n + m) = text(1:m)
        n = n + m
    end subroutine append_json_chars

    subroutine make_tmpfile(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a)') '/tmp/', trim(prefix), '-', &
            count, '-', serial, '.log'
    end subroutine make_tmpfile

    subroutine summarize_backend_failure(stage, log_file, rerun, res)
        character(len=*), intent(in) :: stage, log_file, rerun
        type(check_result_t), intent(inout) :: res

        character(len=512) :: summary, fallback, line
        character(len=128) :: target
        character(len=256) :: rerun_cmd
        character(len=256) :: current_file, selected_file, parsed_file
        integer :: current_line, current_column, selected_line, selected_column
        integer :: parsed_line, parsed_column
        integer :: u, iostat, best_priority
        logical :: has_location, selected

        summary = ''
        fallback = ''
        best_priority = 0
        current_file = ''
        selected_file = ''
        current_line = 0
        current_column = 0
        selected_line = 0
        selected_column = 0

        open (newunit=u, file=log_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (u, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                call parse_location_line(line, parsed_file, parsed_line, &
                                         parsed_column, has_location)
                if (has_location) then
                    current_file = parsed_file
                    current_line = parsed_line
                    current_column = parsed_column
                end if
                call consider_log_line(line, summary, fallback, best_priority, &
                                       selected)
                if (selected .and. len_trim(current_file) > 0) then
                    selected_file = current_file
                    selected_line = current_line
                    selected_column = current_column
                end if
            end do
            close (u)
        end if

        if (len_trim(summary) == 0) summary = fallback
        if (len_trim(summary) == 0) summary = 'backend returned nonzero status'

        target = infer_target(summary)
        rerun_cmd = rerun
        if (trim(stage) == 'test' .and. len_trim(target) > 0) then
            rerun_cmd = trim(rerun)//' '//trim(target)
        end if

        call set_failure(res, stage, target, summary, &
                         default_hint(stage), rerun_cmd, log_file)
        res%diag_file = ''
        res%diag_line = selected_line
        res%diag_column = selected_column
        if (trim(rerun) == 'fo test') then
            res%hint = 'make this test faster or mark it slow'
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

    function default_hint(stage) result(hint)
        character(len=*), intent(in) :: stage
        character(len=256) :: hint

        select case (trim(stage))
        case ('build')
            hint = 'fix the first compiler diagnostic, then rerun fo build'
        case ('test')
            hint = 'rerun the failing test, then fix or mark it slow'
        case default
            hint = 'rerun the reported fo command after fixing the input'
        end select
    end function default_hint

    function infer_target(summary) result(target)
        character(len=*), intent(in) :: summary
        character(len=128) :: target

        integer :: pos, start, finish

        target = ''
        pos = index(summary, 'test_')
        if (pos == 0) return

        start = pos
        finish = start
        do while (finish <= len_trim(summary))
            select case (summary(finish:finish))
            case (' ', ':', ';', ',', ')', '(')
                exit
            case default
                finish = finish + 1
            end select
        end do
        target = summary(start:finish - 1)
    end function infer_target

    subroutine parse_location_line(line, file, line_no, column, found)
        character(len=*), intent(in) :: line
        character(len=256), intent(out) :: file
        integer, intent(out) :: line_no, column
        logical, intent(out) :: found

        character(len=512) :: clean, number
        integer :: ext, start, colon1, colon2, iostat, file_len

        found = .false.
        file = ''
        line_no = 0
        column = 0

        clean = adjustl(line)
        ext = index(clean, '.f90:')
        if (ext == 0) ext = index(clean, '.F90:')
        if (ext == 0) return

        start = 1
        colon1 = ext + 4
        colon2 = index(clean(colon1 + 1:), ':')
        if (colon2 == 0) return
        colon2 = colon1 + colon2

        number = clean(colon1 + 1:colon2 - 1)
        read (number, *, iostat=iostat) line_no
        if (iostat /= 0) return

        number = ''
        if (colon2 + 1 <= len_trim(clean)) then
            number = clean(colon2 + 1:)
            if (index(number, ':') > 0) number = number(1:index(number, ':') - 1)
            read (number, *, iostat=iostat) column
            if (iostat /= 0) column = 0
        end if

        file_len = colon1 - start
        if (file_len <= 0) return
        if (file_len > len(file)) file_len = len(file)
        file(1:file_len) = clean(start:start + file_len - 1)
        found = .true.
    end subroutine parse_location_line

    subroutine consider_log_line(line, summary, fallback, best_priority, selected)
        character(len=*), intent(in) :: line
        character(len=*), intent(inout) :: summary, fallback
        integer, intent(inout) :: best_priority
        logical, intent(out) :: selected

        character(len=512) :: clean
        integer :: priority

        selected = .false.
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
            selected = .true.
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
        call make_tmpfile('fo_compiler_version', tmpfile)
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
