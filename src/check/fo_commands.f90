module fo_commands
    !! One routine per command shared by the CLI and the MCP server. Each
    !! returns its stdout text, its stderr text and an exit code; the CLI
    !! prints them and the MCP server wraps them. Nothing here writes to a
    !! stream, and every text is a deferred-length string, so output is never
    !! truncated by a buffer.
    use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS, MAX_PATH
    use fx_dag, only: dag_t, dag_to_dot, MAX_NODES
    use fo_dag_bridge, only: build_dag_from_units
    use fo_build_backend, only: backend_t, detect_backend, backend_clean, &
        BACKEND_NONE, BACKEND_NATIVE, BACKEND_CMAKE
    use fo_cache, only: cache_schema, cache_store_root
    use fo_capabilities, only: capabilities_t, detect_capabilities, &
        capabilities_text, capabilities_json
    use fo_check, only: check_result_t, fo_check_run, fo_changed_modules, &
        should_report_frontend_diagnostics
    use fo_check_output, only: check_result_compact_json, check_result_full_json
    use fo_diagnostics, only: diagnostic_t, frontend_diagnostics_from_file, &
        FO_DIAG_SEVERITY_ERROR
    use fo_install, only: install_native_executables
    use fo_lint, only: lint_finding_t, lint_warning_t, lint_files, lint_compiler, &
        lint_dedup_warnings, collect_fortran_sources, lint_all_json, lint_fix_dir, &
        MAX_FINDINGS, MAX_WARNINGS
    use fo_lint_deep, only: lint_deep_run
    use fo_progress, only: progress_suppress
    use fo_project_load, only: project_has_model, project_affected
    use fo_project_model, only: model_request_t, model_selection_t, project_model_t, &
        SELECT_OK
    use fo_util, only: strip_path_prefix_in_str
    use fx_path, only: path_normalize, path_is_absolute
    use fx_string, only: string_t, builder_t, builder_new, builder_append, &
        builder_to_string
    implicit none
    private
    public :: command_graph, command_info, command_changed, command_clean
    public :: command_install, command_check, command_lint, failed_tests_text

    character(len=1), parameter :: LF = achar(10)

contains

    subroutine add_line(b, line)
        type(builder_t), intent(inout) :: b
        character(len=*), intent(in) :: line

        call builder_append(b, line)
        call builder_append(b, LF)
    end subroutine add_line

    subroutine add_count(b, label, n, suffix)
        type(builder_t), intent(inout) :: b
        character(len=*), intent(in) :: label, suffix
        integer, intent(in) :: n
        character(len=16) :: digits

        write (digits, '(i0)') n
        call add_line(b, label//trim(digits)//suffix)
    end subroutine add_count

    function scan_root_of(dir, b) result(root)
        character(len=*), intent(in) :: dir
        type(backend_t), intent(in) :: b
        character(len=:), allocatable :: root

        root = trim(dir)
        if (b%kind /= BACKEND_NONE) root = trim(b%project_dir)
    end function scan_root_of

    subroutine strip_dir(text, dir)
        !! Report paths relative to the project directory, as the CLI does
        !! when it runs inside it.
        character(len=:), allocatable, intent(inout) :: text
        character(len=*), intent(in) :: dir

        if (trim(dir) == '.' .or. len_trim(dir) == 0) return
        call strip_path_prefix_in_str(text, trim(dir)//'/')
    end subroutine strip_dir

    subroutine command_graph(dir, dot, text, err_text, exitcode)
        character(len=*), intent(in) :: dir
        logical, intent(in) :: dot
        character(len=:), allocatable, intent(out) :: text, err_text
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        type(scan_unit_t), allocatable :: units(:)
        type(dag_t) :: dag
        type(builder_t) :: out
        character(len=:), allocatable :: dot_out
        integer :: n_units, ierr, i, j

        allocate (units(MAX_UNITS))
        err_text = ''
        b = detect_backend(trim(dir))
        call scan_dir(scan_root_of(dir, b), units, n_units, ierr)
        if (ierr /= 0) then
            text = ''
            err_text = 'fo: scan failed'//LF
            exitcode = 1
            return
        end if
        call build_dag_from_units(units, n_units, dag)
        exitcode = 0
        if (dot) then
            call dag_to_dot(dag, dot_out)
            text = trim(dot_out)//LF
            return
        end if
        out = builder_new(4096)
        do i = 1, dag%n_nodes
            if (dag%nodes(i)%n_edges == 0) then
                call add_line(out, trim(dag%nodes(i)%label))
            else
                do j = 1, dag%nodes(i)%n_edges
                    call add_line(out, trim(dag%nodes(i)%label)//' -> '// &
                        trim(dag%nodes(dag%nodes(i)%edges(j))%label))
                end do
            end if
        end do
        text = builder_to_string(out)
    end subroutine command_graph

    subroutine command_info(dir, capabilities, text, exitcode)
        character(len=*), intent(in) :: dir
        logical, intent(in) :: capabilities
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        type(scan_unit_t), allocatable :: units(:)
        type(dag_t) :: dag
        type(builder_t) :: out
        type(capabilities_t) :: cap
        character(len=2048) :: cap_text
        character(len=512) :: cache_text
        integer :: n_units, ierr

        exitcode = 0
        if (capabilities) then
            call detect_capabilities(cap)
            call capabilities_text(cap, cap_text)
            text = trim(cap_text)//LF
            return
        end if
        allocate (units(MAX_UNITS))
        b = detect_backend(trim(dir))
        out = builder_new(512)
        select case (b%kind)
        case (BACKEND_NATIVE)
            call add_line(out, 'backend: native')
        case (BACKEND_CMAKE)
            call add_line(out, 'backend: cmake')
        case default
            call add_line(out, 'backend: none')
        end select
        call cache_schema(cache_text)
        call add_line(out, 'cache-schema: '//trim(cache_text))
        call cache_store_root(cache_text)
        call add_line(out, 'cache-root: '//trim(cache_text))
        call add_line(out, 'cache-shards: 256')
        call scan_dir(scan_root_of(dir, b), units, n_units, ierr)
        if (ierr == 0) then
            call build_dag_from_units(units, n_units, dag)
            call add_count(out, 'files: ', n_units, '')
            call add_count(out, 'modules: ', dag%n_nodes, '')
        end if
        text = builder_to_string(out)
    end subroutine command_info

    subroutine command_changed(dir, text, err_text, exitcode)
        character(len=*), intent(in) :: dir
        character(len=:), allocatable, intent(out) :: text, err_text
        integer, intent(out) :: exitcode

        type(dag_t) :: dag
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, i
        character(len=MAX_PATH), allocatable :: filenames(:)
        type(backend_t) :: b
        type(model_request_t) :: request
        type(model_selection_t) :: selection
        type(project_model_t) :: model
        type(string_t), allocatable :: changed(:), widen_reasons(:)
        type(builder_t) :: out
        character(len=:), allocatable :: message

        allocate (filenames(MAX_NODES))
        err_text = ''
        call fo_changed_modules(trim(dir), dag, changed_ids, n_changed, &
            affected_ids, n_affected, n_cached, ierr, filenames=filenames)
        if (ierr /= 0) then
            text = ''
            err_text = 'fo: scan or dag failed'//LF
            exitcode = 1
            return
        end if
        exitcode = 0
        out = builder_new(1024)
        if (n_changed == 0) then
            call add_count(out, 'all ', n_cached, ' modules cached')
            text = builder_to_string(out)
            return
        end if
        call add_count(out, 'changed (', n_changed, '):')
        do i = 1, n_changed
            call add_line(out, '  '//trim(dag%nodes(changed_ids(i))%label)//'  '// &
                trim(filenames(changed_ids(i))))
        end do
        call add_count(out, 'affected (', n_affected, '):')
        do i = 1, n_affected
            call add_line(out, '  '//trim(dag%nodes(affected_ids(i))%label)//'  '// &
                trim(filenames(affected_ids(i))))
        end do
        ! Affected tests come from the project model, as in fo test
        ! --only-changed and fo check.
        b = detect_backend(trim(dir))
        if (project_has_model(b)) then
            allocate (changed(n_changed))
            do i = 1, n_changed
                changed(i)%s = trim(filenames(changed_ids(i)))
            end do
            allocate (request%names(0), request%include_labels(0), &
                request%exclude_labels(0), request%layers(0))
            call project_affected(b, '', request, changed, model, selection, &
                widen_reasons, ierr, message)
            if (ierr /= 0) then
                call add_line(out, 'affected tests unavailable: '//message)
            else if (selection%status /= SELECT_OK) then
                call add_line(out, 'affected tests (0):')
            else
                call add_count(out, 'affected tests (', size(selection%cases), '):')
                do i = 1, size(selection%cases)
                    call add_line(out, '  '//trim(model%cases(selection%cases(i))%name))
                end do
                do i = 1, size(widen_reasons)
                    call add_line(out, 'affected selection widened: '// &
                        widen_reasons(i)%s)
                end do
            end if
        end if
        text = builder_to_string(out)
    end subroutine command_changed

    subroutine command_clean(dir, purge_store, text, exitcode)
        character(len=*), intent(in) :: dir
        logical, intent(in) :: purge_store
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        type(builder_t) :: out
        character(len=512) :: store_root
        logical :: build_removed, store_removed

        call cache_store_root(store_root)
        b = detect_backend(trim(dir))
        call backend_clean(trim(b%project_dir), purge_store, build_removed, &
            store_removed)
        out = builder_new(256)
        if (build_removed) call add_line(out, 'build tree cleared: '// &
            trim(b%project_dir)//'/build')
        if (store_removed) then
            call add_line(out, 'cache cleared: '//trim(store_root))
        else
            call add_line(out, &
                'cache kept (shared store); use `fo clean --cache` to purge it')
        end if
        text = builder_to_string(out)
        exitcode = 0
    end subroutine command_clean

    subroutine command_install(dir, prefix, text, err_text, exitcode)
        !! An empty prefix selects $HOME/.local.
        character(len=*), intent(in) :: dir, prefix
        character(len=:), allocatable, intent(out) :: text, err_text
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=512) :: home
        character(len=2048) :: message
        character(len=:), allocatable :: target
        integer :: status

        text = ''
        err_text = ''
        target = trim(prefix)
        if (len_trim(prefix) == 0) then
            call get_environment_variable('HOME', home, status=status)
            if (status /= 0 .or. len_trim(home) == 0) home = '/usr/local'
            target = trim(home)//'/.local'
        end if
        b = detect_backend(trim(dir))
        if (b%kind == BACKEND_NONE) then
            err_text = 'fo: no fpm.toml or CMakeLists.txt found'//LF
            exitcode = 1
            return
        end if
        call install_native_executables(b, target, message, exitcode)
        if (exitcode /= 0) then
            err_text = trim(message)//LF
        else
            text = trim(message)//LF
        end if
    end subroutine command_install

    subroutine command_check(dir, mode, quiet, text, err_text, exitcode)
        !! mode 0 text summary, 1 compact JSON, 2 full JSON. Frontend parser
        !! diagnostics enrich a compiler build failure and are reported in
        !! err_text; a frontend error fails the check. `quiet` suppresses
        !! progress output on stdout (the MCP transport).
        character(len=*), intent(in) :: dir
        integer, intent(in) :: mode
        logical, intent(in) :: quiet
        character(len=:), allocatable, intent(out) :: text, err_text
        integer, intent(out) :: exitcode

        type(check_result_t) :: res
        type(capabilities_t) :: cap
        type(builder_t) :: out, err
        character(len=2048) :: cap_json
        character(len=16) :: digits
        character(len=96) :: elapsed
        logical :: frontend_had_error, ok
        integer :: i

        cap_json = ''
        if (mode == 2) then
            call detect_capabilities(cap)
            call capabilities_json(cap, cap_json)
        end if
        if (quiet) call progress_suppress(.true.)
        call fo_check_run(trim(dir), res)
        if (quiet) call progress_suppress(.false.)

        err = builder_new(256)
        frontend_had_error = .false.
        if (should_report_frontend_diagnostics(res)) &
            call frontend_report(trim(dir), err, frontend_had_error)
        ok = res%build_ok .and. res%tests_ok
        exitcode = 0
        if (.not. ok .or. frontend_had_error) exitcode = 1

        select case (mode)
        case (1)
            text = check_result_compact_json(res)//LF
        case (2)
            text = check_result_full_json(res, cap_json)//LF
        case default
            out = builder_new(256)
            if (frontend_had_error) then
                call add_line(out, 'Frontend: FAIL')
            else if (ok) then
                call add_summary(out, res)
                if (res%n_in_cycle > 0) call add_line(err, 'Warning: '// &
                    trim(cycle_text(res)))
            else if (.not. res%build_ok) then
                call add_line(out, 'Build: FAIL '//trim(res%error_msg))
            else
                write (elapsed, '(i0,a,i0,a,i0,a)') res%n_cached, ' cached, ', &
                    res%n_changed, ' changed, ', res%n_affected, ' affected'
                call add_line(out, 'Build: OK ('//trim(elapsed)//') Tests: FAIL '// &
                    trim(res%error_msg))
                call builder_append(err, failed_tests_text(res%failed_tests, &
                    res%n_failed_tests))
            end if
            text = builder_to_string(out)
        end select
        err_text = builder_to_string(err)
        call strip_dir(text, dir)
        call strip_dir(err_text, dir)
    end subroutine command_check

    function failed_tests_text(failed, n_failed) result(text)
        !! List every failing test, not just the first: a single reported
        !! failure hides the rest and pushes the user to run stale binaries.
        character(len=*), intent(in) :: failed(:)
        integer, intent(in) :: n_failed
        character(len=:), allocatable :: text
        character(len=16) :: digits
        integer :: i

        text = ''
        if (n_failed < 1) return
        write (digits, '(i0)') n_failed
        if (n_failed == 1) then
            text = 'fo: 1 test failed:'//LF
        else
            text = 'fo: '//trim(digits)//' tests failed:'//LF
        end if
        do i = 1, n_failed
            text = text//'  - '//trim(failed(i))//LF
        end do
        text = text//'fo: rerun one with: fo test <name>  '// &
            '(never run build/fo/bin/* directly; that can be stale)'//LF
    end function failed_tests_text

    subroutine add_summary(b, res)
        type(builder_t), intent(inout) :: b
        type(check_result_t), intent(in) :: res
        character(len=256) :: line
        character(:), allocatable :: tests

        if (res%n_tests_selected == 0) then
            tests = 'not run (0 affected; '
        else
            tests = 'pass ('
        end if
        write (line, '(a,i0,a,i0,a,i0,a,i0,a,a,f0.1,a)') &
            'Build: OK (', res%n_modules, ' modules, ', res%n_cached, ' cached, ', &
            res%n_changed, ' changed, ', res%n_affected, ' affected) Tests: ', &
            tests, res%elapsed, 's)'
        call add_line(b, trim(line))
        if (res%n_tests_selected == 0) call add_line(b, &
            'fo: run fo test for the default suite')
    end subroutine add_summary

    function cycle_text(res) result(line)
        type(check_result_t), intent(in) :: res
        character(len=128) :: line

        write (line, '(i0,a,i0,a)') res%n_in_cycle, ' of ', res%n_modules, &
            ' modules in possible cycle'
    end function cycle_text

    subroutine frontend_report(dir, err, had_error)
        !! FortFront parser diagnostics for every source after the compiler
        !! rejected the build; only parser errors are surfaced and they never
        !! override a successful build.
        character(len=*), intent(in) :: dir
        type(builder_t), intent(inout) :: err
        logical, intent(out) :: had_error

        type(scan_unit_t), allocatable :: units(:)
        type(diagnostic_t), allocatable :: diags(:)
        integer :: n_units, n_diags, i, j, ierr
        logical :: file_error
        character(len=64) :: loc
        character(len=16) :: kind

        had_error = .false.
        allocate (units(MAX_UNITS))
        call scan_dir(dir, units, n_units, ierr)
        if (ierr /= 0 .or. n_units < 1) return
        allocate (diags(64))
        do i = 1, n_units
            if (len_trim(units(i)%filename) == 0) cycle
            call frontend_diagnostics_from_file(trim(units(i)%filename), &
                diags, n_diags, file_error, with_semantics=.false.)
            if (file_error) had_error = .true.
            do j = 1, n_diags
                write (loc, '(i0,a,i0)') diags(j)%line, ':', diags(j)%column
                kind = 'warning'
                if (diags(j)%severity == FO_DIAG_SEVERITY_ERROR) kind = 'error'
                call add_line(err, 'fo: frontend '//trim(kind)//': '// &
                    trim(diags(j)%file)//':'//trim(loc)//': '//trim(diags(j)%message))
            end do
        end do
    end subroutine frontend_report

    subroutine command_lint(dir, sources, deep, fix, json, text, deep_report, tail, &
            err_text, exitcode)
        !! `sources` are absolute normalized paths; none selects the project.
        !! Human output is text, then the deep diagnostics in `deep_report`
        !! (rendered by fo_lint_deep, which writes to stdout), then tail.
        character(len=*), intent(in) :: dir
        character(len=*), intent(in) :: sources(:)
        logical, intent(in) :: deep, fix, json
        character(len=:), allocatable, intent(out) :: text, deep_report, tail, err_text
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        type(lint_finding_t), allocatable :: findings(:)
        type(lint_warning_t), allocatable :: warnings(:)
        character(len=512), allocatable :: files(:), selected(:)
        logical, allocatable :: wanted(:)
        character(len=:), allocatable :: root, deep_json, deep_error, candidate
        character(len=512) :: scratch
        type(builder_t) :: out
        integer :: n_findings, n_warnings, i, j, n_files, n_selected
        integer :: n_removed, n_remaining, deep_count, deep_status
        logical :: matched

        text = ''
        deep_report = ''
        tail = ''
        err_text = ''
        exitcode = 0
        b = detect_backend(trim(dir))
        root = scan_root_of(dir, b)

        if (fix) then
            call lint_fix_dir(root, n_removed, n_remaining)
            if (json) then
                write (scratch, '(a,i0,a,i0,a)') '{"removed":', n_removed, &
                    ',"remaining":', n_remaining, '}'
                text = trim(scratch)//LF
            else
                write (scratch, '(i0,a)') n_removed, ' unused import(s) removed'
                text = trim(scratch)//LF
                if (n_remaining > 0) then
                    write (scratch, '(i0,a)') n_remaining, &
                        ' unused import(s) remaining (not auto-removable)'
                    text = text//trim(scratch)//LF
                end if
            end if
            if (n_remaining > 0) exitcode = 1
            return
        end if

        allocate (findings(MAX_FINDINGS), warnings(MAX_WARNINGS))
        call collect_fortran_sources(root, files, n_files)
        allocate (wanted(n_files))
        wanted = size(sources) == 0
        do i = 1, size(sources)
            matched = .false.
            do j = 1, n_files
                candidate = trim(files(j))
                if (.not. path_is_absolute(candidate)) &
                    candidate = trim(current_dir())//'/'//candidate
                candidate = path_normalize(candidate)
                if (candidate == trim(sources(i))) then
                    wanted(j) = .true.
                    matched = .true.
                else if (index(candidate, trim(sources(i))//'/') == 1) then
                    wanted(j) = .true.
                    matched = .true.
                end if
            end do
            if (.not. matched) then
                err_text = 'fo lint: no supported source matches '// &
                    trim(sources(i))//LF
                exitcode = 2
                return
            end if
        end do
        selected = pack(files(:n_files), wanted)
        n_selected = size(selected)
        call lint_files(root, selected, n_selected, findings, n_findings)
        call lint_compiler(root, warnings, n_warnings, selected)
        call lint_dedup_warnings(warnings, n_warnings)
        deep_count = 0
        deep_status = 0
        deep_json = '[]'
        deep_error = ''
        if (deep) call lint_deep_run(root, selected, n_selected, deep_json, &
            deep_count, deep_status, deep_error)

        if (json) then
            if (deep) then
                text = lint_all_json(findings, n_findings, warnings, n_warnings, &
                    deep_json, deep_count, deep_error)//LF
            else
                text = lint_all_json(findings, n_findings, warnings, n_warnings)//LF
            end if
        else if (n_findings + n_warnings + deep_count == 0 .and. deep_status == 0) then
            text = 'no issues found'//LF
        else
            out = builder_new(1024)
            do i = 1, n_findings
                write (scratch, '(a,a,i0,a,a,a,a)') trim(findings(i)%file), ':', &
                    findings(i)%line, ': unused import ', trim(findings(i)%symbol), &
                    ' from ', trim(findings(i)%module_name)
                call add_line(out, trim(scratch))
            end do
            do i = 1, n_warnings
                write (scratch, '(a,a,i0,a,i0,a,a)') trim(warnings(i)%file), ':', &
                    warnings(i)%line, ':', warnings(i)%column, ': ', &
                    trim(warnings(i)%message)
                call add_line(out, trim(scratch))
            end do
            text = builder_to_string(out)
            if (deep_count > 0) deep_report = deep_json
            if (len(deep_error) > 0) err_text = deep_error//LF
            out = builder_new(256)
            if (any_unused_dummy(warnings, n_warnings)) call add_line(out, &
                'hint: an intentionally unused dummy argument (e.g. an interface'// &
                '-mandated callback parameter) can be silenced with'// &
                ' `associate (unused => arg); end associate`')
            write (scratch, '(i0,a,i0,a)') n_findings, ' unused import(s), ', &
                n_warnings, ' compiler warning(s)'
            call add_line(out, trim(scratch))
            if (deep) then
                write (scratch, '(i0,a)') deep_count, ' deep diagnostic(s)'
                call add_line(out, trim(scratch))
            end if
            tail = builder_to_string(out)
        end if
        if (deep_status > 1) then
            exitcode = 2
        else if (n_findings + n_warnings > 0 .or. deep_status == 1) then
            exitcode = 1
        end if
        call strip_dir(text, dir)
    end subroutine command_lint

    function current_dir() result(cwd)
        use fo_process, only: process_getcwd
        character(len=:), allocatable :: cwd
        character(len=4096) :: buffer
        integer :: status

        call process_getcwd(buffer, status)
        cwd = trim(buffer)
    end function current_dir

    logical function any_unused_dummy(warnings, n)
        type(lint_warning_t), intent(in) :: warnings(:)
        integer, intent(in) :: n
        integer :: i

        any_unused_dummy = .false.
        do i = 1, n
            if (index(warnings(i)%message, 'Unused dummy argument') > 0) then
                any_unused_dummy = .true.
                return
            end if
        end do
    end function any_unused_dummy

end module fo_commands
