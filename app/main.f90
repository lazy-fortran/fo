program fo_main
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS, MAX_PATH, &
        is_slow_test
    use fx_dag, only: dag_t, dag_topo_sort, dag_to_dot, MAX_NODES
    use fo_dag_bridge, only: build_dag_from_units
    use fo_build_backend, only: backend_t, detect_backend, backend_build, &
        backend_test, backend_test_names, BACKEND_NONE, &
        BACKEND_NATIVE, BACKEND_CMAKE, profile_flags
    use fo_check, only: check_result_t, fo_check_run, fo_changed_modules, &
        collect_failed_test_names, MAX_TEST_RESULTS
    use fo_diagnostics, only: diagnostic_t, diagnostic_from_log
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_check_output, only: check_result_json, check_result_compact_json, &
        check_result_full_json
    use fo_test_results, only: test_result_entry_t, MAX_TEST_RESULTS_ENTRIES, &
        parse_test_results, format_test_results_human, format_test_results_json
    use fo_capabilities, only: capabilities_t, detect_capabilities, &
        capabilities_json
    use fo_fmt, only: fo_fmt_run, fo_fmt_files, fo_fmt_changed_run, &
        fo_fmt_check_run, fo_fmt_check_files, fo_fmt_check_changed_run, &
        fo_fmt_deep_run, fo_fmt_deep_files, fo_fmt_deep_changed_run, &
        fo_fmt_deep_check_run, fo_fmt_deep_check_files
    use fo_process, only: process_run_argv_logged, argv_push
    use fo_ffc_cli, only: ffc_cmd_build, ffc_cmd_run, ffc_native_requested
    use fo_exec_target, only: resolve_exec_target
    use fo_cover, only: fo_cover_run
    use fo_lock, only: lock_write
    use fo_scaffold, only: scaffold_project
    use fo_bench, only: bench_result_t, fo_bench_run
    use fo_doc, only: fo_doc_run
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
    case ('doc')
        call cmd_doc()
    case ('build')
        if (ffc_native_requested()) then
            call ffc_cmd_build()
        else
            call cmd_build()
        end if
    case ('test')
        call cmd_test()
    case ('bench')
        call cmd_bench()
    case ('cover')
        call fo_cover_run()
    case ('exec')
        call cmd_exec()
    case ('run')
        if (ffc_native_requested()) then
            call ffc_cmd_run()
        else
            call cmd_exec()
        end if
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
    case ('lock')
        call cmd_lock()
    case ('watch')
        call cmd_watch()
    case ('mcp-server')
        call cmd_mcp_server()
    case ('lsp')
        call cmd_lsp()
    case ('new')
        call cmd_new()
    case ('init')
        call cmd_init()
    case ('version', '--version')
        write (output_unit, '(a)') 'fo 0.2.0'
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
        character(len=MAX_PATH) :: filenames(MAX_NODES)
        character(len=MAX_PATH) :: changed_files(MAX_NODES)
        character(len=512) :: build_log, test_log
        character(len=128) :: failed_tests(MAX_TEST_RESULTS)
        integer :: n_failed_tests
        logical :: is_test_arr(MAX_NODES), has_cycle

        allocate (units(MAX_UNITS))

        call cpu_time(t0)

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
            filenames=filenames, is_test_arr=is_test_arr)
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
            do i = 1, n_changed
                changed_files(i) = filenames(changed_ids(i))
            end do
            call fo_fmt_check_changed_run(trim(b%project_dir), changed_files, &
                n_changed, fmt_out, fmt_exit)
            if (len_trim(fmt_out) > 0) write (error_unit, '(a)') trim(fmt_out)
            if (fmt_exit /= 0) write (error_unit, '(a)') 'Fmt: WARN'
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
        write (output_unit, '(a)') '  (none)     static -> build -> test -> lint -> fmt hint'
        write (output_unit, '(a)') '  build      build only (--flag "-O0")'
        write (output_unit, '(a)') &
            '  build --debug   add -g -O0 -fcheck=all -fbacktrace'
        write (output_unit, '(a)') &
            '  build --asan    debug flags plus -fsanitize=address,undefined'
        write (output_unit, '(a)') '  test       run tests (--only-changed, --all)'
        write (output_unit, '(a)') '  cover      run tests with coverage, then fortcov'
        write (output_unit, '(a)') &
            '  exec [--cwd <dir>] [--no-build] <t> [args]  build then run target'
        write (output_unit, '(a)') &
            '  run --native <source> [args]  compile with ffc, then run'
        write (output_unit, '(a)') &
            '  build --native [-o <exe>] <source>...  compile and link with ffc'
        write (output_unit, '(a)') '  check      build + test, one-line status'
        write (output_unit, '(a)') '  check --json  build + test, JSON status'
        write (output_unit, '(a)') '  check --json=compact  bounded agent JSON'
        write (output_unit, '(a)') '  check --json=full  JSON status with diagnostics'
        write (output_unit, '(a)') '  check --agent  compact JSON for opencode/Qwen'
        write (output_unit, '(a)') '  changed    list changed and affected modules'
        write (output_unit, '(a)') '  graph      module dependency graph'
        write (output_unit, '(a)') '  graph --dot  graph in Graphviz DOT format'
        write (output_unit, '(a)') '  fmt [paths...]  format sources (project fprettify config if present)'
        write (output_unit, '(a)') '  fmt --changed  format Git-dirty Fortran sources'
        write (output_unit, '(a)') '  fmt --check  check formatting without modifying files'
        write (output_unit, '(a)') '  watch      rebuild on file change (inotify loop)'
        write (output_unit, '(a)') '  watch --fmt  auto-format changed files before rebuild'
        write (output_unit, '(a)') '  lint       unused imports + gfortran warnings'
        write (output_unit, '(a)') '  lint --json  lint results as JSON'
        write (output_unit, '(a)') '  lint --fix   remove unused imports in place'
        write (output_unit, '(a)') '  clean      drop project build tree (--cache also purges shared store)'
        write (output_unit, '(a)') '  install    install binary (fpm install --prefix ~/.local)'
        write (output_unit, '(a)') '  lock       write fo.lock for current compiler, flags, and deps'
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
        write (out, '(a)') 'usage: fo test [--only-changed] [--all] [--verbose] [--json] [name ...]'
        write (out, '(a)') ''
        write (out, '(a)') 'Run project tests from fpm.toml.'
        write (out, '(a)') ''
        write (out, '(a)') 'options:'
        write (out, '(a)') '  --only-changed  run tests affected by changed modules'
        write (out, '(a)') '  --all           include slow tests'
        write (out, '(a)') '  --verbose       show all tests (default for named runs)'
        write (out, '(a)') '  --json          output results as JSON'
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
        character(len=MAX_PATH) :: filenames(MAX_NODES)
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
        character(len=64) :: profile
        character(len=1024) :: all_flags

        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if

        call get_flags_arg(flags)
        call get_profile_arg(profile)
        if (len_trim(profile) > 0 .and. len_trim(profile_flags(profile)) == 0) then
            write (error_unit, '(a)') 'fo: unknown build profile: '//trim(profile)
            stop 1
        end if
        all_flags = trim(profile_flags(profile))
        if (len_trim(flags) > 0) then
            if (len_trim(all_flags) > 0) then
                all_flags = trim(all_flags)//' '//trim(flags)
            else
                all_flags = trim(flags)
            end if
        end if
        call make_tmpfile('fo-build', build_log)
        if (len_trim(all_flags) > 0) then
            call backend_build(b, exitcode, all_flags, build_log, with_tests=.true.)
        else
            call backend_build(b, exitcode, log_file=build_log, with_tests=.true.)
        end if
        if (exitcode /= 0) then
            call report_build_result(build_log)
            stop 1, quiet=.true.
        end if
        call delete_tmpfile(build_log)
    end subroutine cmd_build

    subroutine cmd_lock()
        type(backend_t) :: b
        integer :: ierr
        character(len=512) :: flags
        character(len=64) :: profile
        character(len=1024) :: all_flags

        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if

        call get_flags_arg(flags)
        call get_profile_arg(profile)
        if (len_trim(profile) > 0 .and. len_trim(profile_flags(profile)) == 0) then
            write (error_unit, '(a)') 'fo: unknown build profile: '//trim(profile)
            stop 1
        end if
        all_flags = trim(profile_flags(profile))
        if (len_trim(flags) > 0) then
            if (len_trim(all_flags) > 0) then
                all_flags = trim(all_flags)//' '//trim(flags)
            else
                all_flags = trim(flags)
            end if
        end if

        call lock_write(trim(b%project_dir), all_flags, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: lock failed'
            stop 1
        end if
        write (output_unit, '(a)') 'wrote fo.lock'
    end subroutine cmd_lock

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

    subroutine report_install_result(install_log)
        character(len=*), intent(in) :: install_log
        type(diagnostic_t) :: diag
        character(len=32) :: lnum

        call diagnostic_from_log('install', install_log, 'fo install', diag)
        write (error_unit, '(a,a)') 'fo: install failed: ', trim(diag%message)
        if (len_trim(diag%file) > 0) then
            write (lnum, '(i0)') diag%line
            write (error_unit, '(a,a,a,a)') 'fo: at: ', trim(diag%file), ':', &
                trim(lnum)
        end if
        if (len_trim(diag%hint) > 0) then
            write (error_unit, '(a,a)') 'fo: hint: ', trim(diag%hint)
        end if
        write (error_unit, '(a,a)') 'fo: full log: ', trim(install_log)
    end subroutine report_install_result

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

    subroutine get_profile_arg(profile)
        character(len=*), intent(out) :: profile
        character(len=256) :: arg
        integer :: i

        profile = ''
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            select case (trim(arg))
            case ('--debug')
                profile = 'debug'
            case ('--asan')
                profile = 'asan'
            case ('--profile')
                if (i < command_argument_count()) &
                    call get_command_argument(i + 1, profile)
            end select
            if (index(trim(arg), '--profile=') == 1) profile = trim(arg(11:))
        end do
    end subroutine get_profile_arg

    subroutine cmd_test()
        type(backend_t) :: b
        type(dag_t) :: dag
        integer :: exitcode
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, i, n_test_names, n_arg_names
        logical :: only_changed, include_all, verbose, use_json
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
        verbose = .false.
        use_json = .false.
        n_arg_names = 0
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--only-changed') only_changed = .true.
            if (trim(arg) == '--all') include_all = .true.
            if (trim(arg) == '--verbose') verbose = .true.
            if (trim(arg) == '--json') use_json = .true.
            if (arg(1:1) /= '-') then
                n_arg_names = n_arg_names + 1
                test_names(n_arg_names) = arg(1:128)
            end if
        end do

        if (n_arg_names > 0) then
            call make_tmpfile('fo-test', test_log)
            call backend_test_names(b, test_names, n_arg_names, exitcode, &
                include_all, test_log)
            call report_test_result(exitcode, test_log, .false., use_json)
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
            call report_test_result(exitcode, test_log, .false., use_json)
            call delete_tmpfile(test_log)
        else
            call make_tmpfile('fo-test', test_log)
            call backend_test(b, exitcode, include_all, test_log)
            call report_test_result(exitcode, test_log, &
                (n_arg_names == 0 .and. .not. verbose), use_json)
            call delete_tmpfile(test_log)
        end if
    end subroutine cmd_test

    subroutine report_test_result(exitcode, test_log, summary_mode, use_json)
        integer, intent(in) :: exitcode
        character(len=*), intent(in) :: test_log
        logical, intent(in) :: summary_mode
        logical, intent(in) :: use_json

        type(test_result_entry_t) :: entries(MAX_TEST_RESULTS_ENTRIES)
        integer :: n_entries, parse_ierr
        character(len=16384) :: json_output, human_output
        type(diagnostic_t) :: diag
        character(len=128) :: failed_tests(MAX_TEST_RESULTS)
        integer :: n_failed_tests

        call parse_test_results(test_log, entries, n_entries, parse_ierr)

        if (n_entries > 0) then
            if (use_json) then
                call format_test_results_json(entries, n_entries, exitcode, json_output)
                write (output_unit, '(a)') trim(json_output)
            else
                call format_test_results_human(entries, n_entries, test_log, &
                    summary_mode, human_output)
                if (len_trim(human_output) > 0) then
                    write (output_unit, '(a)') trim(human_output)
                end if
            end if
        else if (exitcode == 0) then
            if (.not. summary_mode) return
            write (output_unit, '(a)') 'Tests: 0 passed (0.00s)'
            return
        else
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
        end if

        if (exitcode /= 0) stop 1, quiet = .true.
    end subroutine report_test_result

    subroutine cmd_bench()
        type(bench_result_t), allocatable :: results(:)
        logical :: use_json
        integer :: n_runs, n_results, exitcode, i
        character(len=256) :: arg

        n_runs = 5
        use_json = .false.

        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--json') then
                use_json = .true.
            else if (trim(arg) == '--runs' .and. i < command_argument_count()) then
                call get_command_argument(i + 1, arg)
                read (arg, *, iostat=exitcode) n_runs
                if (exitcode /= 0 .or. n_runs < 1 .or. n_runs > 1000) then
                    write (error_unit, '(a)') 'fo bench: invalid --runs value'
                    stop 1
                end if
            else if (index(trim(arg), '--runs=') == 1) then
                read (arg(8:), *, iostat=exitcode) n_runs
                if (exitcode /= 0 .or. n_runs < 1 .or. n_runs > 1000) then
                    write (error_unit, '(a)') 'fo bench: invalid --runs value'
                    stop 1
                end if
            end if
        end do

        allocate (results(128))
        call fo_bench_run('.', results, n_results, use_json, n_runs, exitcode)
        if (exitcode /= 0) stop 1, quiet=.true.
    end subroutine cmd_bench

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

    subroutine cmd_doc()
        type(backend_t) :: b
        character(len=512) :: scan_root
        integer :: exitcode

        b = detect_backend('.')
        scan_root = '.'
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        call fo_doc_run(trim(scan_root), has_arg('--json'), exitcode)
        if (exitcode /= 0) stop 1
    end subroutine cmd_doc

    subroutine cmd_lint()
        use fo_lint, only: lint_finding_t, lint_warning_t, &
            lint_dir, lint_compiler, lint_dedup_warnings, &
            lint_all_json, lint_fix_dir, MAX_FINDINGS, MAX_WARNINGS
        type(backend_t) :: b
        type(lint_finding_t), allocatable :: findings(:)
        type(lint_warning_t), allocatable :: warnings(:)
        integer :: n_findings, n_warnings, i, output_mode, mode_ierr
        integer :: n_removed, n_remaining
        character(len=512) :: scan_root

        allocate (findings(MAX_FINDINGS), warnings(MAX_WARNINGS))
        b = detect_backend('.')
        scan_root = '.'
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        call check_output_mode(output_mode, mode_ierr)

        if (has_arg('--fix')) then
            call lint_fix_dir(trim(scan_root), n_removed, n_remaining)
            write (output_unit, '(i0,a)') n_removed, ' unused import(s) removed'
            if (n_remaining > 0) write (output_unit, '(i0,a)') &
                n_remaining, ' unused import(s) remaining (not auto-removable)'
            if (n_remaining > 0) stop 1, quiet=.true.
            return
        end if

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
            if (any_unused_dummy(warnings, n_warnings)) &
                write (output_unit, '(a)') &
                'hint: an intentionally unused dummy argument (e.g. an interface'// &
                '-mandated callback parameter) can be silenced with'// &
                ' `associate (unused => arg); end associate`'
            write (output_unit, '(i0,a,i0,a)') &
                n_findings, ' unused import(s), ', &
                n_warnings, ' compiler warning(s)'
            stop 1, quiet = .true.
        end if
    end subroutine cmd_lint

    logical function any_unused_dummy(warnings, n)
        use fo_lint, only: lint_warning_t
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

    subroutine cmd_fmt()
        type(backend_t) :: b
        integer :: exitcode
        integer :: i, n_fmt_files
        character(len=MAX_PATH) :: arg, project_dir
        character(len=MAX_PATH) :: fmt_files(MAX_NODES)
        character(len=8192) :: fmt_output
        logical :: check_mode, changed_mode, deep_mode

        if (has_arg('--help') .or. has_arg('-h')) then
            write (output_unit, '(a)') 'usage: fo fmt [--check] [--changed] [--deep] [path ...]'
            write (output_unit, '(a)') ''
            write (output_unit, '(a)') 'Formats project Fortran sources.'
            write (output_unit, '(a)') 'With paths, formats only the listed Fortran sources.'
            write (output_unit, '(a)') 'With --changed, formats Git-dirty Fortran sources only.'
            write (output_unit, '(a)') 'With --check, checks formatting without modifying files.'
            write (output_unit, '(a)') 'With --deep, uses fluff AST formatter (requires fluff on PATH).'
            write (output_unit, '(a)') 'Reads compatible .fprettify settings with fo native formatting.'
            write (output_unit, '(a)') 'Falls back to fo native formatting when no fprettify config exists.'
            return
        end if

        check_mode = has_arg('--check')
        changed_mode = has_arg('--changed')
        deep_mode = has_arg('--deep')
        n_fmt_files = 0
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            select case (trim(arg))
            case ('--check', '--changed', '--deep')
                cycle
            case default
                if (len_trim(arg) > 0) then
                    if (arg(1:1) == '-') then
                        write (error_unit, '(a)') 'fo fmt: unknown option: '//trim(arg)
                        stop 1, quiet = .true.
                    end if
                end if
                if (n_fmt_files >= size(fmt_files)) then
                    write (error_unit, '(a)') 'fo fmt: too many paths'
                    stop 1, quiet = .true.
                end if
                n_fmt_files = n_fmt_files + 1
                fmt_files(n_fmt_files) = arg
            end select
        end do

        if (changed_mode) then
            if (check_mode) then
                write (error_unit, '(a)') 'fo fmt: --check and --changed cannot be combined'
                stop 1, quiet = .true.
            end if
            if (n_fmt_files > 0) then
                write (error_unit, '(a)') 'fo fmt: --changed does not accept paths'
                stop 1, quiet = .true.
            end if
        end if

        if (deep_mode) then
            if (check_mode) then
                if (n_fmt_files > 0) then
                    b = detect_backend('.')
                    project_dir = '.'
                    if (b%kind /= BACKEND_NONE) project_dir = b%project_dir
                    call fo_fmt_deep_check_files(trim(project_dir), fmt_files, &
                        n_fmt_files, fmt_output, exitcode)
                else
                    call fo_fmt_deep_check_run('.', fmt_output, exitcode)
                end if
                if (len_trim(fmt_output) > 0) &
                    write (error_unit, '(a)') trim(fmt_output)
                if (exitcode /= 0) stop 1, quiet = .true.
                return
            end if

            if (changed_mode) then
                call fo_fmt_deep_changed_run('.', exitcode)
                if (exitcode /= 0) then
                    write (error_unit, '(a)') 'fo fmt --deep --changed: no Git worktree'
                    stop 1, quiet = .true.
                end if
                write (output_unit, '(a)') 'formatted changed sources (deep)'
                return
            end if

            if (n_fmt_files > 0) then
                call fo_fmt_deep_files('.', fmt_files, n_fmt_files, exitcode)
            else
                call fo_fmt_deep_run('.', exitcode)
            end if
            if (exitcode /= 0) then
                write (error_unit, '(a)') 'fo fmt --deep: formatting failed'
                stop 1, quiet = .true.
            end if
            if (n_fmt_files > 0) then
                write (output_unit, '(a)') 'formatted selected sources (deep)'
            else
                write (output_unit, '(a)') 'formatted (deep)'
            end if
            return
        end if

        if (check_mode) then
            if (n_fmt_files > 0) then
                b = detect_backend('.')
                project_dir = '.'
                if (b%kind /= BACKEND_NONE) project_dir = b%project_dir
                call fo_fmt_check_files(trim(project_dir), fmt_files, n_fmt_files, &
                    fmt_output, exitcode)
            else
                call fo_fmt_check_run('.', fmt_output, exitcode)
            end if
            if (len_trim(fmt_output) > 0) &
                write (error_unit, '(a)') trim(fmt_output)
            if (exitcode /= 0) stop 1, quiet = .true.
            return
        end if

        if (changed_mode) then
            call fo_fmt_changed_run('.', exitcode)
            if (exitcode /= 0) then
                write (error_unit, '(a)') 'fo fmt --changed: no Git worktree'
                stop 1, quiet = .true.
            end if
            write (output_unit, '(a)') 'formatted changed sources'
            return
        end if

        if (n_fmt_files > 0) then
            call fo_fmt_files('.', fmt_files, n_fmt_files, exitcode)
        else
            call fo_fmt_run('.', exitcode)
        end if
        if (exitcode /= 0) then
            write (error_unit, '(a)') 'fo fmt: formatting failed'
            stop 1, quiet = .true.
        end if
        if (n_fmt_files > 0) then
            write (output_unit, '(a)') 'formatted selected sources'
        else
            write (output_unit, '(a)') 'formatted'
        end if
    end subroutine cmd_fmt

    subroutine cmd_install()
        type(backend_t) :: b
        character(len=256) :: prefix, arg
        character(len=512) :: home
        character(len=512) :: install_log
        character(len=:), allocatable :: packed
        integer :: i, exitcode, status
        integer :: n_args

        call get_environment_variable('HOME', home, status=status)
        if (status /= 0 .or. len_trim(home) == 0) home = '/usr/local'
        prefix = trim(home)//'/.local'

        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--prefix' .and. i < command_argument_count()) then
                call get_command_argument(i + 1, prefix)
            end if
        end do

        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1, quiet=.true.
        end if

        call make_tmpfile('fo-install', install_log)
        n_args = 0
        call argv_push(packed, n_args, 'fpm')
        call argv_push(packed, n_args, 'install')
        call argv_push(packed, n_args, '--prefix')
        call argv_push(packed, n_args, trim(prefix))
        call process_run_argv_logged('.', packed, n_args, install_log, &
            .false., 0, exitcode)
        if (exitcode /= 0) then
            call report_install_result(install_log)
            stop 1, quiet=.true.
        end if
        call delete_tmpfile(install_log)
        write (output_unit, '(a,a)') 'installed: ', trim(prefix)//'/bin/'
    end subroutine cmd_install

    subroutine cmd_clean()
        use fo_cache, only: cache_store_root
        use fo_build_backend, only: backend_clean
        type(backend_t) :: b
        character(len=512) :: store_root, arg
        integer :: i
        logical :: purge_store, build_removed, store_removed

        ! Default clean is project-scoped: drop only this project's build/ tree
        ! (a disposable view that fo regenerates from the cache). The store at
        ! ~/.cache/fo/store/v1 is the shared, content-addressed source of truth
        ! across all projects; wiping it on a per-project clean cold-starts every
        ! other project. Purge it only when explicitly asked.
        purge_store = .false.
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--cache' .or. trim(arg) == '--all') &
                purge_store = .true.
        end do

        call cache_store_root(store_root)
        b = detect_backend('.')
        call backend_clean(trim(b%project_dir), purge_store, build_removed, &
            store_removed)
        if (build_removed) write (output_unit, '(a,a)') 'build tree cleared: ', &
            trim(b%project_dir)//'/build'
        if (store_removed) then
            write (output_unit, '(a,a)') 'cache cleared: ', trim(store_root)
        else
            write (output_unit, '(a)') &
                'cache kept (shared store); use `fo clean --cache` to purge it'
        end if
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
        case (BACKEND_NATIVE)
            write (output_unit, '(a)') 'backend: native'
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

    subroutine cmd_new()
        character(len=256) :: target, proj
        logical :: is_library
        integer :: nargs, i, ierr, pos
        character(len=256) :: arg

        nargs = command_argument_count()
        if (nargs < 2) then
            write (error_unit, '(a)') 'usage: fo new [--lib] <name>'
            stop 1
        end if

        is_library = .false.
        target = ''
        do i = 2, nargs
            call get_command_argument(i, arg)
            if (trim(arg) == '--lib') then
                is_library = .true.
            else if (len_trim(target) == 0) then
                target = arg
            end if
        end do

        if (len_trim(target) == 0) then
            write (error_unit, '(a)') 'fo: project name required'
            stop 1
        end if

        proj = target
        pos = index(trim(target), '/', back=.true.)
        if (pos > 0) proj = target(pos + 1:)

        call scaffold_project(trim(target), trim(proj), is_library, ierr)
        if (ierr /= 0) stop 1
    end subroutine cmd_new

    subroutine cmd_init()
        character(len=256) :: cwd, name
        integer :: ierr, status, pos

        call get_environment_variable('PWD', cwd, status=status)
        if (status /= 0 .or. len_trim(cwd) == 0) then
            cwd = '.'
        end if

        name = cwd
        pos = index(trim(cwd), '/', back=.true.)
        if (pos > 0) then
            name = cwd(pos + 1:)
        end if

        call scaffold_project('.', trim(name), .false., ierr)
        if (ierr /= 0) stop 1
    end subroutine cmd_init

end program fo_main
