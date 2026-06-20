program fo_main
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS, MAX_NAME, is_slow_test
    use fx_dag, only: dag_t, dag_topo_sort, dag_to_dot, MAX_NODES
    use fo_dag_bridge, only: build_dag_from_units
    use fo_build_backend, only: backend_t, detect_backend, backend_build, &
        backend_test, backend_test_names, BACKEND_NONE, &
        BACKEND_GFORTRAN, BACKEND_CMAKE
    use fo_check, only: check_result_t, fo_check_run, fo_changed_modules, &
        collect_failed_test_names, MAX_TEST_RESULTS
    use fo_diagnostics, only: diagnostic_t, diagnostic_from_log
    use fo_util, only: make_tmpfile, delete_tmpfile, clean_root_build_artifacts
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_collect_files, &
        fs_copy_exec, fs_rename
    use fo_check_output, only: check_result_json, check_result_compact_json, &
        check_result_full_json
    use fo_capabilities, only: capabilities_t, detect_capabilities, &
        capabilities_json
    use fo_fmt, only: fo_fmt_run, fo_fmt_check_run
    use fo_process, only: process_run_argv_logged, argv_push
    use fo_exec_target, only: resolve_exec_target
    implicit none

    character(len=256) :: action
    integer :: nargs

    nargs = command_argument_count()
    if (nargs == 0) then
        call cmd_run()
        stop
    end if

    call get_command_argument(1, action)

    select case (trim(action))
    case ('check')
        call cmd_check()
    case ('changed')
        call cmd_changed()
    case ('graph')
        call cmd_graph()
    case ('build')
        call cmd_build()
    case ('test')
        call cmd_test()
    case ('exec', 'run')
        call cmd_exec()
    case ('info')
        call cmd_info()
    case ('lint')
        call cmd_lint()
    case ('fmt', 'format')
        call cmd_fmt()
    case ('clean')
        call cmd_clean()
    case ('install')
        call cmd_install()
    case ('watch')
        call cmd_watch()
    case ('mcp-server')
        call cmd_mcp_server()
    case ('lsp')
        call cmd_lsp()
    case ('version', '--version')
        write (output_unit, '(a)') 'fo 0.1.0'
    case ('help', '--help', '-h')
        call print_usage()
    case default
        write (error_unit, '(a)') 'fo: unknown command: '//trim(action)
        call print_usage()
        stop 1
    end select

contains

    subroutine cmd_run()
        ! staged pipeline: static -> build -> test -> lint
        type(backend_t) :: b
        type(dag_t) :: dag
        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr, exitcode
        integer :: order(MAX_NODES), n_order
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, i, n_test_names
        real :: t0, t1
        character(len=128) :: test_names(MAX_NODES)
        character(len=512) :: build_log, test_log
        character(len=128) :: failed_tests(MAX_TEST_RESULTS)
        integer :: n_failed_tests
        logical :: is_test_arr(MAX_NODES), has_cycle

        allocate (units(MAX_UNITS))

        call cpu_time(t0)

        ! 0. detect backend
        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (output_unit, '(a)') 'fo: no Fortran project detected'
            return
        end if

        ! 1. static: scan + DAG cycle check
        call scan_dir(trim(b%project_dir), units, n_units, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'Static: FAIL scan error'
            stop 1
        end if

        ! graceful skip for non-Fortran directories
        if (n_units == 0) return

        call build_dag_from_units(units, n_units, dag)
        call dag_topo_sort(dag, order, n_order, has_cycle)
        if (has_cycle) then
            write (error_unit, '(a,i0,a,i0,a)') &
                'Static: warning: ', dag%n_nodes - n_order, ' of ', dag%n_nodes, &
                ' modules in possible cycle (continuing with build)'
        end if

        ! compute changed modules (rebuilds dag internally)
        call fo_changed_modules(trim(b%project_dir), dag, changed_ids, n_changed, &
            affected_ids, n_affected, n_cached, ierr, &
            is_test_arr=is_test_arr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'Static: FAIL scan or dag error'
            stop 1
        end if

        write (output_unit, '(a,i0,a,i0,a,i0,a)') &
            'Static: OK (', dag%n_nodes, ' modules, ', n_changed, &
            ' changed, ', n_affected, ' affected)'

        ! 2. build through the action/output store
        call make_tmpfile('fo-build', build_log)
        call backend_build(b, exitcode, log_file=build_log)
        if (exitcode /= 0) then
            write (error_unit, '(a)') 'Build: FAIL'
            call report_build_result(build_log)
            stop 1, quiet=.true.
        end if
        call delete_tmpfile(build_log)
        write (output_unit, '(a)') 'Build: OK'

        ! 3. test: skip if nothing changed, otherwise run affected tests only
        if (n_changed == 0) then
            call cpu_time(t1)
            write (output_unit, '(a,f0.1,a)') &
                'Tests: skipped, all cached (', t1 - t0, 's)'
        else
            ! collect affected test names (excluding slow)
            n_test_names = 0
            do i = 1, n_affected
                if (is_test_arr(affected_ids(i))) then
                    if (.not. is_slow_test(dag%nodes(affected_ids(i))%label)) then
                        n_test_names = n_test_names + 1
                        test_names(n_test_names) = &
                            dag%nodes(affected_ids(i))%label(1:128)
                    end if
                end if
            end do

            if (n_test_names == 0) then
                write (output_unit, '(a)') 'Tests: skipped, no affected tests'
            else
                call make_tmpfile('fo-test', test_log)
                call backend_test_names(b, test_names, n_test_names, exitcode, &
                    log_file=test_log)
                if (exitcode /= 0) then
                    call collect_failed_test_names(test_log, failed_tests, &
                        n_failed_tests)
                    write (error_unit, '(a)') 'Tests: FAIL'
                    call report_failed_tests(failed_tests, n_failed_tests)
                    stop 1, quiet=.true.
                end if
                call delete_tmpfile(test_log)
                write (output_unit, '(a)') 'Tests: OK'
            end if
        end if

        ! 4. lint: unused imports (skip compiler warnings in pipeline)
        block
            use fo_lint, only: lint_finding_t, lint_dir, MAX_FINDINGS
            type(lint_finding_t), allocatable :: findings(:)
            integer :: n_findings, li

            allocate (findings(MAX_FINDINGS))

            call lint_dir(trim(b%project_dir), findings, n_findings)
            if (n_findings > 0) then
                do li = 1, n_findings
                    write (error_unit, '(a,a,i0,a,a,a,a)') &
                        trim(findings(li)%file), ':', findings(li)%line, &
                        ': unused import ', trim(findings(li)%symbol), &
                        ' from ', trim(findings(li)%module_name)
                end do
                write (error_unit, '(a,i0,a)') &
                    'Lint: FAIL (', n_findings, ' unused imports)'
                stop 1
            end if
        end block
        write (output_unit, '(a)') 'Lint: OK'

        ! 5. fmt --check
        block
            integer :: fmt_exit
            character(len=8192) :: fmt_out
            call fo_fmt_check_run(trim(b%project_dir), fmt_out, fmt_exit)
            if (len_trim(fmt_out) > 0) write (error_unit, '(a)') trim(fmt_out)
            if (fmt_exit /= 0) stop 1
        end block

        call cpu_time(t1)
        write (output_unit, '(a,f0.1,a)') 'All stages passed (', t1 - t0, 's)'
    end subroutine cmd_run

    subroutine print_usage()
        write (output_unit, '(a)') 'fo - Fortran build driver'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'Run in or below fpm.toml or CMakeLists.txt.'
        write (output_unit, '(a)') 'Scans modules, builds the DAG, caches by hash.'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'usage: fo [command]'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') '  (none)     static -> build -> test -> lint -> fmt --check'
        write (output_unit, '(a)') '  build      build only (--flag "-O0")'
        write (output_unit, '(a)') '  test       run tests (--only-changed, --all)'
        write (output_unit, '(a)') &
            '  exec [--cwd <dir>] [--no-build] <t> [args]  build then run target'
        write (output_unit, '(a)') '  check      build + test, one-line status'
        write (output_unit, '(a)') '  check --json  build + test, JSON status'
        write (output_unit, '(a)') '  check --json=compact  bounded agent JSON'
        write (output_unit, '(a)') '  check --json=full  JSON status with diagnostics'
        write (output_unit, '(a)') '  check --agent  compact JSON for opencode/Qwen'
        write (output_unit, '(a)') '  changed    list changed and affected modules'
        write (output_unit, '(a)') '  graph      module dependency graph'
        write (output_unit, '(a)') '  graph --dot  graph in Graphviz DOT format'
        write (output_unit, '(a)') '  fmt        format sources (project fprettify config if present)'
        write (output_unit, '(a)') '  fmt --check  check formatting without modifying files'
        write (output_unit, '(a)') '  watch      rebuild on file change (inotify loop)'
        write (output_unit, '(a)') '  watch --fmt  auto-format changed files before rebuild'
        write (output_unit, '(a)') '  lint       unused imports + gfortran warnings'
        write (output_unit, '(a)') '  lint --json  lint results as JSON'
        write (output_unit, '(a)') '  clean      clear global cache and project build tree'
        write (output_unit, '(a)') '  install    install binary (fpm install --prefix ~/.local)'
        write (output_unit, '(a)') '  info       backend, file count, module count'
        write (output_unit, '(a)') '  info --capabilities  compiler and tooling limits'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'integration:'
        write (output_unit, '(a)') '  mcp-server  MCP JSON-RPC on stdin/stdout'
        write (output_unit, '(a)') '  lsp         LSP server (diagnostics on save)'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'fo version    print version'
    end subroutine print_usage

    subroutine print_test_usage(unit)
        integer, optional, intent(in) :: unit
        integer :: out

        out = output_unit
        if (present(unit)) out = unit
        write (out, '(a)') 'usage: fo test [--only-changed] [--all] [name ...]'
        write (out, '(a)') ''
        write (out, '(a)') 'Run project tests through the detected backend.'
        write (out, '(a)') ''
        write (out, '(a)') 'options:'
        write (out, '(a)') '  --only-changed  run tests affected by changed modules'
        write (out, '(a)') '  --all           include slow tests'
        write (out, '(a)') '  -h, --help      show this help'
        write (out, '(a)') ''
        write (out, '(a)') 'examples:'
        write (out, '(a)') '  fo test'
        write (out, '(a)') '  fo test test_cpp6d_vs_gc'
        write (out, '(a)') '  fo test --only-changed'
    end subroutine print_test_usage

    subroutine print_exec_usage(unit)
        integer, optional, intent(in) :: unit
        integer :: out

        out = output_unit
        if (present(unit)) out = unit
        write (out, '(a)') &
            'usage: fo exec [--cwd <dir>] [--no-build] <target> [args...]'
        write (out, '(a)') ''
        write (out, '(a)') 'Build incrementally, resolve a target, then run it.'
        write (out, '(a)') ''
        write (out, '(a)') 'options:'
        write (out, '(a)') '  --cwd <dir>     run the target in this directory'
        write (out, '(a)') '  --no-build      resolve and run without rebuilding'
        write (out, '(a)') '  -h, --help      show this help'
        write (out, '(a)') ''
        write (out, '(a)') 'examples:'
        write (out, '(a)') '  fo exec simple simple.in'
        write (out, '(a)') '  fo exec --cwd /tmp/run simple simple.in'
        write (out, '(a)') '  fo exec --no-build --cwd /tmp/run simple simple.in'
    end subroutine print_exec_usage

    subroutine cmd_check()
        type(check_result_t) :: res
        type(capabilities_t) :: cap
        character(len=2048) :: cap_json
        integer :: output_mode, mode_ierr

        call check_output_mode(output_mode, mode_ierr)
        if (mode_ierr /= 0) then
            write (error_unit, '(a)') &
                'fo: use --json, --json=compact, --json=full, or --agent'
            stop 1, quiet = .true.
        end if

        cap_json = ''
        if (output_mode == 3) then
            call detect_capabilities(cap)
            call capabilities_json(cap, cap_json)
        end if

        call fo_check_run('.', res)

        select case (output_mode)
        case (1)
            write (output_unit, '(a)') trim(check_result_json(res))
            if (.not. (res%build_ok .and. res%tests_ok)) stop 1, quiet = .true.
            return
        case (2)
            write (output_unit, '(a)') trim(check_result_compact_json(res))
            if (.not. (res%build_ok .and. res%tests_ok)) stop 1, quiet = .true.
            return
        case (3)
            write (output_unit, '(a)') trim(check_result_full_json(res, cap_json))
            if (.not. (res%build_ok .and. res%tests_ok)) stop 1, quiet = .true.
            return
        case (4)
            write (output_unit, '(a)') trim(check_result_compact_json(res))
            if (.not. (res%build_ok .and. res%tests_ok)) stop 1, quiet = .true.
            return
        end select

        if (res%build_ok .and. res%tests_ok) then
            write (output_unit, '(a,i0,a,i0,a,i0,a,i0,a,f0.1,a)') &
                'Build: OK (', res%n_modules, ' modules, ', &
                res%n_cached, ' cached, ', res%n_changed, &
                ' changed, ', res%n_affected, &
                ' affected) Tests: pass (', res%elapsed, 's)'
            if (res%n_in_cycle > 0) then
                write (error_unit, '(a,i0,a,i0,a)') &
                    'Warning: ', res%n_in_cycle, ' of ', res%n_modules, &
                    ' modules in possible cycle'
            end if
        else if (.not. res%build_ok) then
            write (output_unit, '(a,a)') 'Build: FAIL ', trim(res%error_msg)
            stop 1, quiet = .true.
        else
            write (output_unit, '(a,i0,a,i0,a,i0,a,a)') &
                'Build: OK (', res%n_cached, ' cached, ', res%n_changed, &
                ' changed, ', res%n_affected, &
                ' affected) Tests: FAIL ', trim(res%error_msg)
            call report_failed_tests(res%failed_tests, res%n_failed_tests)
            stop 1, quiet = .true.
        end if
    end subroutine cmd_check

    subroutine report_failed_tests(failed, n_failed)
        !! List every failing test, not just the first. A single reported
        !! failure hides the rest and pushes the user to run raw build/fo/bin
        !! binaries to find them -- which serves stale artifacts when sources
        !! changed since the last fo run. The full list keeps everything inside
        !! fo, where the content-addressed cache guarantees fresh binaries.
        character(len=128), intent(in) :: failed(:)
        integer, intent(in) :: n_failed
        integer :: i

        if (n_failed < 1) return
        if (n_failed == 1) then
            write (error_unit, '(a)') 'fo: 1 test failed:'
        else
            write (error_unit, '(a,i0,a)') 'fo: ', n_failed, ' tests failed:'
        end if
        do i = 1, n_failed
            write (error_unit, '(a,a)') '  - ', trim(failed(i))
        end do
        write (error_unit, '(a)') &
            'fo: rerun one with: fo test <name>  (never run build/fo/bin/* '// &
            'directly; that can be stale)'
    end subroutine report_failed_tests

    subroutine cmd_exec()
        !! Build incrementally, then exec build/fo/bin/<target> with the
        !! remaining args, inheriting the terminal. The sanctioned way to run a
        !! built binary (app or test): it can never be stale, because the build
        !! runs first and fo's cache is content-addressed. Running
        !! build/fo/bin/* by hand skips that and may execute an artifact older
        !! than the current sources.
        type(backend_t) :: b
        integer :: exitcode, i, target_index
        character(len=256) :: target, arg
        character(len=512) :: build_log, bin_path, run_cwd
        character(len=:), allocatable :: packed
        integer :: n_args
        logical :: exists, skip_build

        if (has_arg('--help') .or. has_arg('-h')) then
            call print_exec_usage()
            return
        end if
        if (command_argument_count() < 2) then
            call print_exec_usage(error_unit)
            stop 1
        end if
        run_cwd = ''
        skip_build = .false.
        target = ''
        target_index = 0
        i = 2
        do while (i <= command_argument_count())
            call get_command_argument(i, arg)
            if (trim(arg) == '--cwd') then
                if (i == command_argument_count()) then
                    write (error_unit, '(a)') 'fo exec: --cwd requires a directory'
                    stop 1
                end if
                i = i + 1
                call get_command_argument(i, run_cwd)
            else if (index(trim(arg), '--cwd=') == 1) then
                run_cwd = trim(arg(7:))
            else if (trim(arg) == '--no-build') then
                skip_build = .true.
            else
                target = arg
                target_index = i
                exit
            end if
            i = i + 1
        end do
        if (target_index == 0) then
            call print_exec_usage(error_unit)
            stop 1
        end if
        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if

        if (.not. skip_build) then
            call make_tmpfile('fo-exec-build', build_log)
            call backend_build(b, exitcode, log_file=build_log, with_tests=.true.)
            if (exitcode /= 0) then
                write (error_unit, '(a)') 'fo exec: build failed'
                call report_build_result(build_log)
                stop 1, quiet=.true.
            end if
            call delete_tmpfile(build_log)
        end if

        call resolve_exec_target(b, target, bin_path, exists)
        if (.not. exists) then
            write (error_unit, '(a)') 'fo exec: no such target: '//trim(target)
            stop 1, quiet=.true.
        end if

        n_args = 0
        call argv_push(packed, n_args, trim(bin_path))
        do i = target_index + 1, command_argument_count()
            call get_command_argument(i, arg)
            call argv_push(packed, n_args, trim(arg))
        end do
        ! Empty log_file makes the child inherit this terminal's stdout/stderr;
        ! timeout 0 means no limit (an interactive app may run arbitrarily long).
        call process_run_argv_logged(trim(run_cwd), packed, n_args, '', .false., &
            0, exitcode)
        if (exitcode /= 0) stop 1, quiet=.true.
    end subroutine cmd_exec

    subroutine check_output_mode(mode, ierr)
        integer, intent(out) :: mode, ierr

        character(len=256) :: arg
        integer :: i

        mode = 0
        ierr = 0
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            select case (trim(arg))
            case ('--json')
                mode = 1
            case ('--json=compact')
                mode = 2
            case ('--json=full')
                mode = 3
            case ('--agent')
                mode = 4
            case default
                if (index(trim(arg), '--json=') == 1 .or. &
                    index(trim(arg), '--agent=') == 1) then
                    ierr = 1
                    return
                end if
            end select
        end do
    end subroutine check_output_mode

    logical function has_arg(name)
        character(len=*), intent(in) :: name

        character(len=256) :: arg
        integer :: i

        has_arg = .false.
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == trim(name)) then
                has_arg = .true.
                return
            end if
        end do
    end function has_arg

    subroutine cmd_changed()
        type(dag_t) :: dag
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, i, n_tests
        character(len=MAX_NAME) :: filenames(MAX_NODES)
        logical :: is_test_arr(MAX_NODES)

        call fo_changed_modules('.', dag, changed_ids, n_changed, &
            affected_ids, n_affected, n_cached, ierr, &
            filenames=filenames, is_test_arr=is_test_arr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: scan or dag failed'
            stop 1
        end if

        if (n_changed == 0) then
            write (output_unit, '(a,i0,a)') 'all ', n_cached, ' modules cached'
            return
        end if

        write (output_unit, '(a,i0,a)') 'changed (', n_changed, '):'
        do i = 1, n_changed
            write (output_unit, '(a,a,a,a)') '  ', &
                trim(dag%nodes(changed_ids(i))%label), &
                '  ', trim(filenames(changed_ids(i)))
        end do

        write (output_unit, '(a,i0,a)') 'affected (', n_affected, '):'
        do i = 1, n_affected
            write (output_unit, '(a,a,a,a)') '  ', &
                trim(dag%nodes(affected_ids(i))%label), &
                '  ', trim(filenames(affected_ids(i)))
        end do

        n_tests = 0
        do i = 1, n_affected
            if (is_test_arr(affected_ids(i))) n_tests = n_tests + 1
        end do
        if (n_tests > 0) then
            write (output_unit, '(a,i0,a)') 'affected tests (', n_tests, '):'
            do i = 1, n_affected
                if (is_test_arr(affected_ids(i))) then
                    write (output_unit, '(a,a,a,a)') '  ', &
                        trim(dag%nodes(affected_ids(i))%label), &
                        '  ', trim(filenames(affected_ids(i)))
                end if
            end do
        end if
    end subroutine cmd_changed

    subroutine cmd_build()
        type(backend_t) :: b
        integer :: exitcode
        character(len=512) :: flags, build_log

        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if

        call get_flags_arg(flags)
        ! Capture the compiler output so a failed build shows the real diagnostic
        ! (file:line: Error: ...) instead of a bare "STOP 1".
        call make_tmpfile('fo-build', build_log)
        ! Build the test binaries too so build/fo/bin/test_* never go stale against
        ! the sources (running one directly after `fo build` used to give results
        ! from the previous `fo test`).
        if (len_trim(flags) > 0) then
            call backend_build(b, exitcode, flags, build_log, with_tests=.true.)
        else
            call backend_build(b, exitcode, log_file=build_log, with_tests=.true.)
        end if
        if (exitcode /= 0) then
            call report_build_result(build_log)
            stop 1, quiet=.true.
        end if
        call delete_tmpfile(build_log)
    end subroutine cmd_build

    subroutine report_build_result(build_log)
        !! Print the best compiler diagnostic from a failed build's log, with the
        !! source location and the log path for the full output.
        character(len=*), intent(in) :: build_log
        type(diagnostic_t) :: diag
        character(len=32) :: lnum

        call diagnostic_from_log('build', build_log, 'fo build', diag)
        write (error_unit, '(a,a)') 'fo: build failed: ', trim(diag%message)
        if (len_trim(diag%file) > 0) then
            write (lnum, '(i0)') diag%line
            write (error_unit, '(a,a,a,a)') 'fo: at: ', trim(diag%file), ':', &
                trim(lnum)
        end if
        if (len_trim(diag%hint) > 0) then
            write (error_unit, '(a,a)') 'fo: hint: ', trim(diag%hint)
        end if
        write (error_unit, '(a,a)') 'fo: full log: ', trim(build_log)
    end subroutine report_build_result

    subroutine get_flags_arg(flags)
        character(len=*), intent(out) :: flags
        character(len=256) :: arg
        integer :: i

        flags = ''
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--flag' .and. i < command_argument_count()) then
                call get_command_argument(i + 1, flags)
                return
            end if
        end do
    end subroutine get_flags_arg

    subroutine cmd_test()
        type(backend_t) :: b
        type(dag_t) :: dag
        integer :: exitcode
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, i, n_test_names, n_arg_names
        logical :: only_changed, include_all
        character(len=256) :: arg
        character(len=128) :: test_names(MAX_NODES)
        character(len=512) :: test_log
        logical :: is_test_arr(MAX_NODES)

        if (has_arg('--help') .or. has_arg('-h')) then
            call print_test_usage()
            return
        end if
        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if

        only_changed = .false.
        include_all = .false.
        n_arg_names = 0
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--only-changed') only_changed = .true.
            if (trim(arg) == '--all') include_all = .true.
            if (arg(1:1) /= '-') then
                n_arg_names = n_arg_names + 1
                test_names(n_arg_names) = arg(1:128)
            end if
        end do

        if (n_arg_names > 0) then
            call make_tmpfile('fo-test', test_log)
            call backend_test_names(b, test_names, n_arg_names, exitcode, &
                include_all, test_log)
            call report_test_result(exitcode, test_log)
            call delete_tmpfile(test_log)
        else if (only_changed) then
            call fo_changed_modules('.', dag, changed_ids, n_changed, &
                affected_ids, n_affected, n_cached, ierr, &
                is_test_arr=is_test_arr)
            if (ierr /= 0) then
                write (error_unit, '(a)') 'fo: scan or dag failed'
                stop 1
            end if

            if (n_changed == 0) then
                write (output_unit, '(a)') 'all cached, skipping tests'
                return
            end if

            ! collect affected test names
            n_test_names = 0
            do i = 1, n_affected
                if (is_test_arr(affected_ids(i))) then
                    n_test_names = n_test_names + 1
                    test_names(n_test_names) = dag%nodes(affected_ids(i))%label(1:128)
                end if
            end do

            if (n_test_names == 0) then
                write (output_unit, '(a)') 'no affected tests'
                return
            end if

            call make_tmpfile('fo-test', test_log)
            call backend_test_names(b, test_names, n_test_names, exitcode, &
                include_all, test_log)
            call report_test_result(exitcode, test_log)
            call delete_tmpfile(test_log)
        else
            call make_tmpfile('fo-test', test_log)
            call backend_test(b, exitcode, include_all, test_log)
            call report_test_result(exitcode, test_log)
            call delete_tmpfile(test_log)
        end if
    end subroutine cmd_test

    subroutine report_test_result(exitcode, test_log)
        integer, intent(in) :: exitcode
        character(len=*), intent(in) :: test_log

        type(diagnostic_t) :: diag
        character(len=128) :: failed_tests(MAX_TEST_RESULTS)
        integer :: n_failed_tests

        if (exitcode == 0) return
        call diagnostic_from_log('test', test_log, 'fo test', diag)
        write (error_unit, '(a,a)') 'fo: test failed: ', trim(diag%message)
        call collect_failed_test_names(test_log, failed_tests, n_failed_tests)
        if (n_failed_tests > 1) call report_failed_tests(failed_tests, n_failed_tests)
        if (len_trim(diag%target) > 0) then
            write (error_unit, '(a,a)') 'fo: target: ', trim(diag%target)
        end if
        if (len_trim(diag%hint) > 0) then
            write (error_unit, '(a,a)') 'fo: hint: ', trim(diag%hint)
        end if
        if (len_trim(diag%rerun) > 0) then
            write (error_unit, '(a,a)') 'fo: rerun: ', trim(diag%rerun)
        end if
        write (error_unit, '(a,a)') 'fo: log: ', trim(test_log)
        stop 1, quiet = .true.
    end subroutine report_test_result

    subroutine cmd_graph()
        type(scan_unit_t), allocatable :: units(:)
        type(dag_t) :: dag
        type(backend_t) :: b
        character(len=512) :: scan_root
        integer :: n_units, ierr, i, j
        logical :: dot_mode
        character(len=:), allocatable :: dot_output

        allocate (units(MAX_UNITS))
        b = detect_backend('.')
        scan_root = '.'
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        dot_mode = has_arg('--dot')

        call scan_dir(trim(scan_root), units, n_units, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: scan failed'
            stop 1
        end if

        call build_dag_from_units(units, n_units, dag)

        if (dot_mode) then
            call dag_to_dot(dag, dot_output)
            write (output_unit, '(a)') trim(dot_output)
        else
            do i = 1, dag%n_nodes
                if (dag%nodes(i)%n_edges == 0) then
                    write (output_unit, '(a)') trim(dag%nodes(i)%label)
                else
                    do j = 1, dag%nodes(i)%n_edges
                        write (output_unit, '(a,a,a)') &
                            trim(dag%nodes(i)%label), &
                            ' -> ', &
                            trim(dag%nodes(dag%nodes(i)%edges(j))%label)
                    end do
                end if
            end do
        end if
    end subroutine cmd_graph

    subroutine cmd_lint()
        use fo_lint, only: lint_finding_t, lint_warning_t, &
            lint_dir, lint_compiler, lint_dedup_warnings, &
            lint_all_json, MAX_FINDINGS, MAX_WARNINGS
        type(backend_t) :: b
        type(lint_finding_t), allocatable :: findings(:)
        type(lint_warning_t), allocatable :: warnings(:)
        integer :: n_findings, n_warnings, i, output_mode, mode_ierr
        character(len=512) :: scan_root

        allocate (findings(MAX_FINDINGS), warnings(MAX_WARNINGS))
        b = detect_backend('.')
        scan_root = '.'
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        call check_output_mode(output_mode, mode_ierr)

        call lint_dir(trim(scan_root), findings, n_findings)
        call lint_compiler(trim(scan_root), warnings, n_warnings)
        call lint_dedup_warnings(warnings, n_warnings)

        if (output_mode > 0) then
            write (output_unit, '(a)') &
                trim(lint_all_json(findings, n_findings, &
                warnings, n_warnings))
        else if (n_findings == 0 .and. n_warnings == 0) then
            write (output_unit, '(a)') 'no issues found'
        else
            do i = 1, n_findings
                write (output_unit, '(a,a,i0,a,a,a,a)') &
                    trim(findings(i)%file), ':', findings(i)%line, &
                    ': unused import ', trim(findings(i)%symbol), &
                    ' from ', trim(findings(i)%module_name)
            end do
            do i = 1, n_warnings
                write (output_unit, '(a,a,i0,a,i0,a,a)') &
                    trim(warnings(i)%file), ':', warnings(i)%line, &
                    ':', warnings(i)%column, ': ', &
                    trim(warnings(i)%message)
            end do
            write (output_unit, '(i0,a,i0,a)') &
                n_findings, ' unused import(s), ', &
                n_warnings, ' compiler warning(s)'
            stop 1, quiet = .true.
        end if
    end subroutine cmd_lint

    subroutine cmd_fmt()
        integer :: exitcode
        character(len=8192) :: fmt_output

        if (has_arg('--help') .or. has_arg('-h')) then
            write (output_unit, '(a)') 'usage: fo fmt [--check]'
            write (output_unit, '(a)') ''
            write (output_unit, '(a)') 'Formats project Fortran sources.'
            write (output_unit, '(a)') 'Uses .fprettify or .fprettify.rc at the project root when present.'
            write (output_unit, '(a)') 'Falls back to fo native formatting when no fprettify config exists.'
            return
        end if

        if (has_arg('--check')) then
            call fo_fmt_check_run('.', fmt_output, exitcode)
            if (len_trim(fmt_output) > 0) &
                write (error_unit, '(a)') trim(fmt_output)
            if (exitcode /= 0) stop 1, quiet = .true.
            return
        end if

        call fo_fmt_run('.', exitcode)
        if (exitcode /= 0) then
            write (error_unit, '(a)') 'fo fmt: formatting failed'
            stop 1, quiet = .true.
        end if
        write (output_unit, '(a)') 'formatted'
    end subroutine cmd_fmt

    subroutine cmd_install()
        type(backend_t) :: b
        character(len=256) :: prefix, arg
        character(len=512) :: home
        integer :: i, exitcode, status

        call get_environment_variable('HOME', home, status=status)
        if (status /= 0 .or. len_trim(home) == 0) home = '/usr/local'
        prefix = trim(home)//'/.local'

        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--prefix' .and. i < command_argument_count()) then
                call get_command_argument(i + 1, prefix)
            end if
        end do

        ! Native build, then copy the produced app binaries into prefix/bin.
        ! No fpm: the binaries come from the gfortran backend's build/fo/bin.
        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if
        call backend_build(b, exitcode)
        if (exitcode /= 0) stop 1

        call fs_make_dir(trim(prefix)//'/bin')
        ! Atomic per-binary replace: copy to a temp name then rename over the
        ! target. rename swaps the directory entry even when the existing binary
        ! is held open (e.g. a running fo MCP server), avoiding "text file busy".
        block
            character(len=512), allocatable :: bins(:)
            character(len=512) :: dst, slash_name
            integer :: nb, k, sl
            logical :: installed_any
            installed_any = .false.
            allocate (bins(256))
            call fs_collect_files(trim(b%project_dir)//'/build/fo/bin', '', '', '', &
                bins, nb, recursive=.false.)
            do k = 1, nb
                sl = index(trim(bins(k)), '/', back=.true.)
                slash_name = bins(k) (sl + 1:)
                dst = trim(prefix)//'/bin/'//trim(slash_name)
                if (fs_copy_exec(trim(bins(k)), trim(dst)//'.fo-new') /= 0) cycle
                if (fs_rename(trim(dst)//'.fo-new', trim(dst)) == 0) &
                    installed_any = .true.
            end do
            deallocate (bins)
            exitcode = merge(0, 1, installed_any)
        end block
        if (exitcode /= 0) then
            write (error_unit, '(a)') 'fo: install: no binaries found in build/fo/bin'
            stop 1
        end if
        write (output_unit, '(a,a)') 'installed: ', trim(prefix)//'/bin/'
    end subroutine cmd_install

    subroutine cmd_clean()
        use fo_cache, only: cache_root, cache_store_root
        type(backend_t) :: b
        character(len=512) :: root, store_root
        integer :: n_removed

        call cache_root(root)
        call cache_store_root(store_root)
        call fs_remove_tree(trim(root))
        b = detect_backend('.')
        if (b%kind /= BACKEND_NONE) then
            call fs_remove_tree(trim(b%project_dir)//'/build')
            call clean_root_build_artifacts(trim(b%project_dir), n_removed)
        end if
        write (output_unit, '(a,a)') 'cache cleared: ', trim(store_root)
        if (b%kind /= BACKEND_NONE) &
            write (output_unit, '(a,a)') 'build tree cleared: ', &
            trim(b%project_dir)//'/build'
    end subroutine cmd_clean

    subroutine cmd_info()
        use fo_capabilities, only: capabilities_t, detect_capabilities, &
            capabilities_text, capabilities_json
        use fo_cache, only: cache_schema, cache_store_root
        type(scan_unit_t), allocatable :: units(:)
        type(dag_t) :: dag
        type(backend_t) :: b
        character(len=512) :: scan_root
        integer :: n_units, ierr
        logical :: show_caps
        type(capabilities_t) :: cap
        character(len=512) :: cache_text

        allocate (units(MAX_UNITS))
        b = detect_backend('.')
        scan_root = '.'
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        show_caps = has_arg('--capabilities')

        if (show_caps) then
            call detect_capabilities(cap)
            block
                character(len=2048) :: text
                call capabilities_text(cap, text)
                write (output_unit, '(a)') trim(text)
            end block
            return
        end if

        select case (b%kind)
        case (BACKEND_GFORTRAN)
            write (output_unit, '(a)') 'backend: gfortran'
        case (BACKEND_CMAKE)
            write (output_unit, '(a)') 'backend: cmake'
        case default
            write (output_unit, '(a)') 'backend: none'
        end select
        call cache_schema(cache_text)
        write (output_unit, '(a,a)') 'cache-schema: ', trim(cache_text)
        call cache_store_root(cache_text)
        write (output_unit, '(a,a)') 'cache-root: ', trim(cache_text)
        write (output_unit, '(a)') 'cache-shards: 256'

        call scan_dir(trim(scan_root), units, n_units, ierr)
        if (ierr == 0) then
            call build_dag_from_units(units, n_units, dag)
            write (output_unit, '(a,i0)') 'files: ', n_units
            write (output_unit, '(a,i0)') 'modules: ', dag%n_nodes
        end if
    end subroutine cmd_info

    subroutine cmd_watch()
        use fo_watch, only: watch_loop
        character(len=256) :: arg
        logical :: fmt_mode
        integer :: i

        fmt_mode = .false.
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--fmt') fmt_mode = .true.
        end do
        call watch_loop('.', fmt_mode=fmt_mode)
    end subroutine cmd_watch

    subroutine cmd_mcp_server()
        use fo_mcp, only: mcp_serve
        call mcp_serve()
    end subroutine cmd_mcp_server

    subroutine cmd_lsp()
        use fo_lsp, only: lsp_serve
        call lsp_serve()
    end subroutine cmd_lsp

end program fo_main
