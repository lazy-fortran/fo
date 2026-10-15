program fo_main
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, real64
    use, intrinsic :: iso_c_binding, only: c_long_long
    use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS, MAX_PATH
    use fx_dag, only: dag_t, dag_topo_sort, MAX_NODES
    use fo_dag_bridge, only: build_dag_from_units
    use fo_build_backend, only: backend_t, detect_backend, backend_build, &
        backend_test, backend_test_names, BACKEND_NONE, &
        BACKEND_NATIVE, BACKEND_CMAKE, profile_flags, backend_profile
    use fo_check, only: fo_changed_modules, &
        collect_failed_test_names, should_report_frontend_diagnostics
    use fo_diagnostics, only: diagnostic_t, diagnostic_from_log, &
        array_temporary_warnings_from_log, frontend_diagnostics_from_file, &
        FO_DIAG_SEVERITY_ERROR
    use fo_util, only: make_tmpfile, delete_tmpfile, wall_time_seconds
    use fo_fs, only: fs_stat
    use fo_build_tree, only: native_output_dir
    use fo_test_results, only: test_result_entry_t, &
        parse_test_results, format_test_results_text, format_test_results_json
    use fo_test_random, only: select_random_tests
    use fx_string, only: string_t
    use fo_project_model, only: model_request_t, model_selection_t, &
        project_model_t, model_find_case, model_select, SELECT_EMPTY, SELECT_OK, &
        CASE_SELECTED
    use fo_project_load, only: project_has_model, &
        project_run_cases, project_list_text, project_list_json, &
        project_select, project_run_affected, &
        project_load, project_inventory_cases, project_build_cases
    use fo_fmt, only: fmt_sources, FMT_SELECT_ALL, FMT_SELECT_FILES, FMT_SELECT_CHANGED, &
        write_git_changed_source_list
    use fo_process, only: process_exit, process_run_argv_logged, argv_push, &
        process_configure_openmp, process_export_self_driver
    use fo_ffc_cli, only: ffc_cmd_build, ffc_cmd_run, ffc_native_requested
    use fo_gremlin_cli, only: gremlin_cli_run
    use fo_commands, only: command_graph, command_info, command_changed, &
        command_clean, command_install, command_check, command_lint, &
        failed_tests_text
    use fo_exec_target, only: resolve_exec_target, exec_target_is_app, &
        exec_args_t, parse_exec_args, exec_uses_cmake_model, resolve_cmake_exec
    use fo_build_tree, only: native_other_builds
    use fo_cover, only: fo_cover_run
    use fo_lock, only: lock_write
    use fo_scaffold, only: scaffold_project
    use fo_version_info, only: FO_VERSION
    implicit none

    character(len=256) :: action
    integer :: nargs

    nargs = command_argument_count()
    action = ''
    if (nargs > 0) call get_command_argument(1, action)
    ! Help is a parser result, not a command execution. Resolve it before the
    ! command dispatcher configures the process environment or any handler can
    ! acquire a project lock, build, refresh, or install.
    if (dispatch_help(nargs, trim(action))) stop
    ! Build/test pipelines benefit from sleeping compiler workers, but an
    ! executable launched by `fo exec` (or its `fo run` alias) must inherit
    ! the caller's OpenMP policy.  Injecting OMP_WAIT_POLICY=PASSIVE here
    ! otherwise changes the benchmark/runtime behavior of the target.
    if (nargs == 0 .or. (trim(action) /= 'exec' .and. trim(action) /= 'run')) then
        call process_configure_openmp()
    end if
    if (nargs == 0) then
        call gremlin_cli_run()
        stop
    end if

    select case (trim(action))
    case ('check')
        call cmd_check()
    case ('changed')
        call cmd_changed()
    case ('graph')
        call cmd_graph()
    case ('build')
        if (ffc_native_requested()) then
            call ffc_cmd_build()
        else
            call cmd_build()
        end if
    case ('test')
        call process_export_self_driver()
        call cmd_test()
    case ('verify')
        call cmd_verify()
    case ('gremlin')
        call gremlin_cli_run()
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
    case ('fmt')
        call cmd_fmt()
    case ('clean')
        call cmd_clean()
    case ('update')
        call cmd_update()
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
        write (output_unit, '(a)') 'fo '//FO_VERSION
    case ('help', '--help', '-h')
        call print_usage()
    case default
        write (error_unit, '(a)') 'fo: unknown command: '//trim(action)
        call print_usage()
        stop 1
    end select

contains

    subroutine cmd_verify()
        ! staged pipeline: static -> build -> test -> lint
        type(backend_t) :: b
        type(dag_t) :: dag
        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr, exitcode
        integer :: order(MAX_NODES), n_order
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, i, n_test_names
        real(real64) :: t0, t1
        character(len=MAX_PATH), allocatable :: filenames(:), changed_files(:)
        character(len=512) :: build_log, test_log
        character(len=:), allocatable :: failed_tests(:)
        integer :: n_failed_tests
        logical :: has_cycle

        allocate (units(MAX_UNITS), filenames(MAX_NODES), changed_files(MAX_NODES))

        t0 = wall_time_seconds()

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
            filenames=filenames)
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
            call process_exit(1)
        end if
        call report_array_temporary_warnings(build_log)
        call delete_tmpfile(build_log)
        write (output_unit, '(a)') 'Build: OK'

        ! 3. test: the cases the Git delta affects (the selection of
        ! `fo test --only-changed`; the default selection, slow excluded)
        call make_tmpfile('fo-test', test_log)
        call project_run_affected(b, '', .false., test_log, exitcode, &
            n_test_names)
        if (n_test_names == 0 .and. exitcode == 0) then
            call delete_tmpfile(test_log)
            t1 = wall_time_seconds()
            write (output_unit, '(a,f0.1,a)') &
                'Tests: skipped, no affected tests (', t1 - t0, 's)'
        else
            if (exitcode /= 0) then
                call collect_failed_test_names(test_log, failed_tests, &
                    n_failed_tests)
                write (error_unit, '(a)') 'Tests: FAIL'
                call report_failed_tests(failed_tests, n_failed_tests)
                call process_exit(1)
            end if
            call delete_tmpfile(test_log)
            write (output_unit, '(a)') 'Tests: OK'
        end if

        ! 4. lint: unused imports (skip compiler warnings in pipeline)
        block
            use fo_lint, only: lint_finding_t, lint_files, MAX_FINDINGS
            type(lint_finding_t), allocatable :: findings(:)
            character(len=MAX_PATH) :: lint_files_list(MAX_NODES), lint_line
            character(len=512) :: lint_list_file
            integer :: n_findings, li, lint_unit, lint_ios, n_lint_files
            logical :: git_ok

            allocate (findings(MAX_FINDINGS))

            call make_tmpfile('fo-lint-files', lint_list_file)
            call write_git_changed_source_list(trim(b%project_dir), &
                lint_list_file, git_ok, &
                n_lint_files)
            if (git_ok) then
                n_lint_files = 0
                open (newunit=lint_unit, file=trim(lint_list_file), &
                    status='old', iostat=lint_ios)
                if (lint_ios == 0) then
                    do
                        read (lint_unit, '(a)', iostat=lint_ios) lint_line
                        if (lint_ios /= 0) exit
                        if (n_lint_files == MAX_NODES) exit
                        n_lint_files = n_lint_files + 1
                        lint_files_list(n_lint_files) = trim(lint_line)
                    end do
                    close (lint_unit)
                end if
            else
                n_lint_files = n_changed
                do i = 1, n_changed
                    lint_files_list(i) = filenames(changed_ids(i))
                end do
            end if
            call delete_tmpfile(lint_list_file)

            call lint_files(trim(b%project_dir), lint_files_list, &
                n_lint_files, findings, n_findings)
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

            ! Test programs that cannot fail the build. Text-only, so it runs
            ! here rather than in lint_compiler: a rule reachable only through
            ! the compile-based deep lint would never run in this pipeline,
            ! which is the path the project mandates before every commit.
            block
                use fo_lint, only: lint_warning_t, lint_testfail_files, &
                    MAX_WARNINGS
                type(lint_warning_t), allocatable :: tf(:)
                integer :: n_tf, ti

                allocate (tf(MAX_WARNINGS))
                call lint_testfail_files(trim(b%project_dir), lint_files_list, &
                    n_lint_files, tf, n_tf)
                if (n_tf > 0) then
                    do ti = 1, n_tf
                        write (error_unit, '(a,a,i0,a,a)') &
                            trim(tf(ti)%file), ':', tf(ti)%line, ': ', &
                            trim(tf(ti)%message)
                    end do
                    write (error_unit, '(a,i0,a)') &
                        'Lint: FAIL (', n_tf, ' test programs cannot fail)'
                    stop 1
                end if
                deallocate (tf)
            end block
        end block
        write (output_unit, '(a)') 'Lint: OK'

        ! 5. fmt --check
        block
            integer :: fmt_exit
            character(len=8192) :: fmt_out
            do i = 1, n_changed
                changed_files(i) = filenames(changed_ids(i))
            end do
            call fmt_sources(trim(b%project_dir), FMT_SELECT_CHANGED, .true., .false., &
                changed_files, n_changed, fmt_out, fmt_exit)
            if (len_trim(fmt_out) > 0) write (error_unit, '(a)') trim(fmt_out)
            if (fmt_exit /= 0) write (error_unit, '(a)') 'Fmt: WARN'
        end block

        t1 = wall_time_seconds()
        write (output_unit, '(a,f0.1,a)') 'All stages passed (', t1 - t0, 's)'
    end subroutine cmd_verify

    subroutine print_usage()
        write (output_unit, '(a)') 'fo - Fortran build driver'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'Run in or below fpm.toml or CMakeLists.txt.'
        write (output_unit, '(a)') 'Scans modules, builds the DAG, caches by hash.'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'usage: fo [command]'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') '  (none)     start or attach Gremlin for this project'
        write (output_unit, '(a)') '  verify     static -> build -> test -> lint -> fmt hint'
        write (output_unit, '(a)') '  build      build only (--flag "-O0")'
        write (output_unit, '(a)') &
            '  build --tests   also build test programs (--case NAME: what NAME needs)'
        write (output_unit, '(a)') &
            '  build --debug   add -g -O0 -fcheck=all -fbacktrace'
        write (output_unit, '(a)') &
            '  build --asan    debug flags plus -fsanitize=address,undefined'
        write (output_unit, '(a)') '  test       run tests (--only-changed, --all)'
        write (output_unit, '(a)') '  gremlin    continuous generation testing'
        write (output_unit, '(a)') '  cover      run tests with coverage, then fortcov'
        write (output_unit, '(a)') &
            '  exec [--cwd <dir>] [--no-build] <t> [args]  build then run target'
        write (output_unit, '(a)') &
            '  run --native <source> [args]  compile with ffc, then run'
        write (output_unit, '(a)') &
            '  build --native [-o <exe>] <source>...  compile and link with ffc'
        write (output_unit, '(a)') '  check      build + test, one-line status'
        write (output_unit, '(a)') '  check --json=compact  bounded agent JSON'
        write (output_unit, '(a)') '  check --json=full  JSON status with diagnostics'
        write (output_unit, '(a)') '  changed    list changed and affected modules'
        write (output_unit, '(a)') '  graph      module dependency graph'
        write (output_unit, '(a)') '  graph --dot  graph in Graphviz DOT format'
        write (output_unit, '(a)') '  graph --layers [--json]  layering check (exit 1 on violation)'
        write (output_unit, '(a)') '  fmt [paths...]  format sources (project fprettify config if present)'
        write (output_unit, '(a)') '  fmt --changed  format Git-dirty Fortran sources'
        write (output_unit, '(a)') '  fmt --check  check formatting without modifying files'
        write (output_unit, '(a)') '  watch      rebuild on file changes (shared provider)'
        write (output_unit, '(a)') '  watch --fmt  auto-format changed files before rebuild'
        write (output_unit, '(a)') '  lint       unused imports + gfortran warnings'
        write (output_unit, '(a)') '  lint --json  lint results as JSON'
        write (output_unit, '(a)') '  lint --fix   remove unused imports in place'
        write (output_unit, '(a)') '  clean      drop project build tree (--cache also purges shared store)'
        write (output_unit, '(a)') '  clean --stale  drop unreferenced artifacts (--keep N rescues newest N)'
        write (output_unit, '(a)') '  update     re-fetch git/registry dependencies on the next build'
        write (output_unit, '(a)') &
            '  install    install release app executables into prefix/bin'
        write (output_unit, '(a)') '  lock       write fo.lock for current compiler, flags, and deps'
        write (output_unit, '(a)') '  info       backend, file count, module count'
        write (output_unit, '(a)') '  info --capabilities  compiler and tooling limits'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'integration:'
        write (output_unit, '(a)') '  mcp-server  MCP JSON-RPC on stdin/stdout'
        write (output_unit, '(a)') '  lsp         LSP server (debounced unsaved diagnostics)'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'fo version    print version'
    end subroutine print_usage

    logical function dispatch_help(argument_count, command) result(handled)
        integer, intent(in) :: argument_count
        character(len=*), intent(in) :: command
        character(len=256) :: arg
        integer :: i
        logical :: end_options

        handled = .false.
        if (command == 'help') then
            handled = .true.
            if (argument_count >= 2) then
                call get_command_argument(2, arg)
                if (trim(arg) == 'format') then
                    write (error_unit, '(a)') 'fo: unknown command: format'
                    call process_exit(1)
                end if
                call print_command_help(trim(arg))
            else
                call print_usage()
            end if
            return
        end if
        if (command == '--help' .or. command == '-h') then
            call print_usage()
            handled = .true.
            return
        end if
        if (command == 'format') return
        if (command == 'exec' .or. command == 'run') then
            i = 2
            do while (i <= argument_count)
                call get_command_argument(i, arg)
                if (trim(arg) == '--help' .or. trim(arg) == '-h') then
                    call print_command_help(command)
                    handled = .true.
                    return
                end if
                if (trim(arg) == '--') exit
                if (arg(1:1) /= '-') exit
                if (trim(arg) == '--cwd' .or. trim(arg) == '--flag' .or. &
                    trim(arg) == '--profile') i = i + 1
                i = i + 1
            end do
            return
        end if
        if (command == 'test') then
            end_options = .false.
            do i = 2, argument_count
                call get_command_argument(i, arg)
                if (.not. end_options .and. trim(arg) == '--') then
                    end_options = .true.
                    cycle
                end if
                if (end_options) cycle
                if (trim(arg) == '--help' .or. trim(arg) == '-h') then
                    call print_command_help(command)
                    handled = .true.
                    return
                end if
            end do
            return
        end if
        do i = 2, argument_count
            call get_command_argument(i, arg)
            if (trim(arg) == '--help' .or. trim(arg) == '-h') then
                call print_command_help(command)
                handled = .true.
                return
            end if
        end do
    end function dispatch_help

    subroutine print_command_help(command)
        character(len=*), intent(in) :: command

        select case (command)
        case ('install')
            call print_install_usage()
        case ('test')
            call print_test_usage()
        case ('verify')
            write (output_unit, '(a)') &
                'usage: fo verify  static checks, build, tests, lint and format hint'
        case ('exec', 'run')
            call print_exec_usage()
        case ('clean')
            write (output_unit, '(a)') 'usage: fo clean [--cache|--all|--stale] [--keep N]'
            write (output_unit, '(a)') '  --help, -h  show this help without deleting files'
        case ('check')
            write (output_unit, '(a)') &
                'usage: fo check [--json=compact|--json=full]'
        case ('fmt')
            write (output_unit, '(a)') 'usage: fo fmt [--changed] [--check] [paths...]'
            write (output_unit, '(a)') '  -h, --help  show this help without formatting files'
        case default
            write (output_unit, '(a)') 'usage: fo '//trim(command)//' [options]'
            write (output_unit, '(a)') &
                '  -h, --help  show this help without running the command'
        end select
    end subroutine print_command_help

    subroutine print_install_usage()
        write (output_unit, '(a)') 'usage: fo install [--prefix PATH]'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') &
            'Install the release binary into PREFIX/bin (default: $HOME/.local/bin).'
        write (output_unit, '(a)') 'options:'
        write (output_unit, '(a)') '  --prefix PATH  installation prefix'
        write (output_unit, '(a)') '  -h, --help     show this help without installing'
    end subroutine print_install_usage

    subroutine print_test_usage(unit)
        integer, optional, intent(in) :: unit
        integer :: out

        out = output_unit
        if (present(unit)) out = unit
        write (out, '(a)') &
            'usage: fo test [--profile NAME] [--flag FLAGS] [options] [name|glob ...]'
        write (out, '(a)') ''
        write (out, '(a)') 'Run project tests from fpm.toml or CTest.'
        write (out, '(a)') ''
        write (out, '(a)') 'options:'
        write (out, '(a)') '  --only-changed  cases affected by changes since Git HEAD'
        write (out, '(a)') '  --all           include slow tests'
        write (out, '(a)') '  --no-build      run the selected tests without building'
        write (out, '(a)') '  -L, --label RE  select cases with a label matching RE (repeat: AND)'
        write (out, '(a)') '  -LE, --exclude-label RE'
        write (out, '(a)') '                  drop cases whose labels match RE (repeat: AND)'
        write (out, '(a)') '  --layer NAME    select cases of layer NAME (repeat: union)'
        write (out, '(a)') '  --list          print the selection and build plan; no build'
        write (out, '(a)') '  --random N      sample N tests without replacement'
        write (out, '(a)') '  --seed S        replay a random sample with seed S'
        write (out, '(a)') '  --verbose       show all tests (default for named runs)'
        write (out, '(a)') '  --json          output results as JSON'
        write (out, '(a)') '  --profile NAME  use release, debug, or asan flags'
        write (out, '(a)') '  --flag FLAGS    append compiler flags'
        write (out, '(a)') '  -h, --help      show this help'
        write (out, '(a)') ''
        write (out, '(a)') 'examples:'
        write (out, '(a)') '  fo test'
        write (out, '(a)') '  fo test test_cpp6d_vs_gc'
        write (out, '(a)') '  fo test ''test_core_*'' -LE slow'
        write (out, '(a)') '  fo test --list --json -L unit'
        write (out, '(a)') '  fo test --only-changed'
        write (out, '(a)') '  fo test --random 12 --seed 1729'
    end subroutine print_test_usage

    subroutine print_exec_usage(unit)
        integer, optional, intent(in) :: unit
        integer :: out

        out = output_unit
        if (present(unit)) out = unit
        write (out, '(a)') &
            'usage: fo exec [options] <target> [args...]'
        write (out, '(a)') ''
        write (out, '(a)') 'Build incrementally, resolve a target, then run it.'
        write (out, '(a)') 'Options go before the target; later words go to the program.'
        write (out, '(a)') ''
        write (out, '(a)') 'options:'
        write (out, '(a)') '  --cwd <dir>       run the target in this directory'
        write (out, '(a)') '  --no-build        resolve and run without rebuilding'
        write (out, '(a)') '  --flag <flags>    build with extra compiler flags'
        write (out, '(a)') '  --profile <name>  build profile: debug, release, asan'
        write (out, '(a)') '  --release         same as --profile release'
        write (out, '(a)') '  -h, --help        show this help'
        write (out, '(a)') ''
        write (out, '(a)') 'Each flag set has its own build tree. --flag/--profile also'
        write (out, '(a)') 'select the tree under --no-build; a binary built with other'
        write (out, '(a)') 'flags is never run in its place.'
        write (out, '(a)') ''
        write (out, '(a)') 'examples:'
        write (out, '(a)') '  fo exec simple simple.in'
        write (out, '(a)') '  fo exec --cwd /tmp/run simple simple.in'
        write (out, '(a)') '  fo build --release && fo exec --release --no-build simple in'
    end subroutine print_exec_usage

    subroutine cmd_check()
        character(len=:), allocatable :: text, err_text
        integer :: output_mode, mode_ierr, exitcode

        if (has_arg('--help') .or. has_arg('-h')) then
            write (output_unit, '(a)') &
                'usage: fo check [--json=compact|--json=full]'
            return
        end if
        call check_output_mode(output_mode, mode_ierr)
        if (mode_ierr /= 0) then
            write (error_unit, '(a)') &
                'fo check: unknown option (use --help for supported options)'
            call process_exit(1)
        end if
        ! The configured compiler is authoritative. FortFront only enriches a
        ! compiler build failure; it never overrides a successful build.
        call command_check('.', output_mode, .false., text, err_text, exitcode)
        call emit_command(text, err_text, exitcode)
    end subroutine cmd_check

    subroutine report_failed_tests(failed, n_failed)
        character(len=*), intent(in) :: failed(:)
        integer, intent(in) :: n_failed

        write (error_unit, '(a)', advance='no') failed_tests_text(failed, n_failed)
    end subroutine report_failed_tests

    subroutine cmd_exec()
        !! Build incrementally, then exec the built <target> with the remaining
        !! args, inheriting the terminal. The sanctioned way to run a built
        !! binary (app or test): it can never be stale, because the build runs
        !! first and fo's cache is content-addressed. Running build/fo/bin
        !! executables by hand skips that and may execute an artifact older
        !! than the current sources. The requested flags (--flag, --profile,
        !! --release) select the build tree, also with --no-build, so a binary
        !! built with other flags is never run in its place.
        type(backend_t) :: b
        type(exec_args_t) :: parsed
        integer :: exitcode, i, n_words
        character(len=4096), allocatable :: words(:)
        character(len=512) :: build_log, configure_log, bin_path
        character(len=1024) :: all_flags
        character(len=:), allocatable :: packed, message
        integer :: n_args
        logical :: exists, configured, uses_model

        n_words = max(0, command_argument_count() - 1)
        allocate (words(n_words))
        do i = 1, n_words
            call get_command_argument(i + 1, words(i))
        end do
        call parse_exec_args(words, trim(action) == 'run', parsed)
        if (parsed%help) then
            call print_exec_usage()
            return
        end if
        if (len_trim(parsed%error) > 0) then
            write (error_unit, '(a)') 'fo '//trim(action)//': '//trim(parsed%error)
            stop 1
        end if
        if (parsed%target_index == 0) then
            call print_exec_usage(error_unit)
            stop 1
        end if
        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if

        if (len_trim(parsed%profile) > 0 .and. &
            len_trim(profile_flags(parsed%profile)) == 0) then
            write (error_unit, '(a)') 'fo: unknown run profile: '//trim(parsed%profile)
            stop 1
        end if
        call backend_profile(b, parsed%profile)
        all_flags = trim(profile_flags(parsed%profile))
        if (len_trim(parsed%flags) > 0) then
            if (len_trim(all_flags) > 0) then
                all_flags = trim(all_flags)//' '//trim(parsed%flags)
            else
                all_flags = trim(parsed%flags)
            end if
        end if

        uses_model = exec_uses_cmake_model(b)
        if (uses_model) then
            ! The target is validated against the configured project model
            ! before anything is built, so an unknown name is reported as such.
            call make_tmpfile('fo-exec-configure', configure_log)
            call resolve_cmake_exec(b%cmake, parsed%target, all_flags, &
                configure_log, bin_path, exists, configured, message)
            if (.not. configured) then
                write (error_unit, '(a)') 'fo exec: CMake configure failed'
                call report_build_result(configure_log)
                call process_exit(1)
            end if
            call delete_tmpfile(configure_log)
            if (.not. exists) then
                write (error_unit, '(a)') 'fo exec: '//message
                call process_exit(1)
            end if
            ! Build only the requested target; the preset and configuration
            ! selected above are kept.
            if (allocated(b%cmake%build_targets)) deallocate (b%cmake%build_targets)
            allocate (character(len=len_trim(parsed%target)) :: &
                b%cmake%build_targets(1))
            b%cmake%build_targets(1) = trim(parsed%target)
        end if

        if (.not. parsed%skip_build) then
            call make_tmpfile('fo-exec-build', build_log)
            if (trim(action) == 'run') then
                call backend_build(b, exitcode, flags=all_flags, log_file=build_log, &
                    configured=uses_model)
            else
                call backend_build(b, exitcode, flags=all_flags, log_file=build_log, &
                    with_tests=.not. exec_target_is_app(b, parsed%target), &
                    configured=uses_model)
            end if
            if (exitcode /= 0) then
                write (error_unit, '(a)') 'fo exec: build failed'
                call report_build_result(build_log)
                call process_exit(1)
            end if
            call report_array_temporary_warnings(build_log)
            call delete_tmpfile(build_log)
        end if

        if (uses_model) then
            inquire (file=trim(bin_path), exist=exists)
        else
            call resolve_exec_target(b, parsed%target, bin_path, exists, &
                flags=all_flags)
        end if
        if (b%kind == BACKEND_NATIVE .and. parsed%skip_build) &
            call report_other_builds(b%project_dir, all_flags, parsed%target, exists)
        if (.not. exists) then
            if (uses_model) then
                write (error_unit, '(a)') 'fo exec: executable not built: '// &
                    trim(bin_path)
            else
                write (error_unit, '(a)') &
                    'fo exec: no such target: '//trim(parsed%target)
            end if
            call process_exit(1)
        end if

        n_args = 0
        call argv_push(packed, n_args, trim(bin_path))
        do i = 1, n_words
            if (parsed%to_program(i)) call argv_push(packed, n_args, trim(words(i)))
        end do
        ! Empty log_file makes the child inherit this terminal's stdout/stderr;
        ! timeout 0 means no limit (an interactive app may run arbitrarily long).
        call process_run_argv_logged(trim(parsed%cwd), packed, n_args, '', .false., &
            0, exitcode)
        if (exitcode /= 0) call process_exit(1)
    end subroutine cmd_exec

    subroutine report_other_builds(project_dir, flags, target, found)
        !! `fo exec --no-build` runs only the build made with the requested
        !! flags. Say so when builds with other flags exist: fail with the flags
        !! that select them if the requested build is missing, and otherwise
        !! note that they were not used.
        character(len=*), intent(in) :: project_dir, flags, target
        logical, intent(in) :: found
        character(len=512) :: labels(16)
        integer :: i, n_labels

        call native_other_builds(project_dir, flags, target, labels, n_labels)
        if (n_labels == 0) return
        if (found) then
            write (error_unit, '(a)') 'fo exec: note: running the '// &
                flags_label(flags)//' build of '//trim(target)// &
                '; it is also built with:'
        else
            write (error_unit, '(a)') 'fo exec: '//trim(target)// &
                ' has no '//flags_label(flags)//' build; it is built with:'
        end if
        do i = 1, n_labels
            write (error_unit, '(a)') 'fo exec:   '//flags_label(labels(i))
        end do
        write (error_unit, '(a)') 'fo exec: pass the same --flag/--profile '// &
            'as the build to select one'
        flush (error_unit)
        if (.not. found) call process_exit(1)
    end subroutine report_other_builds

    function flags_label(flags) result(label)
        character(len=*), intent(in) :: flags
        character(len=:), allocatable :: label

        if (len_trim(flags) == 0) then
            label = 'default (no --flag)'
        else
            label = "--flag '"//trim(flags)//"'"
        end if
    end function flags_label

    subroutine check_output_mode(mode, ierr)
        integer, intent(out) :: mode, ierr

        character(len=256) :: arg
        integer :: i

        mode = 0
        ierr = 0
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            select case (trim(arg))
            case ('--json=compact')
                mode = 1
            case ('--json=full')
                mode = 2
            case default
                ierr = 1
                return
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
        character(len=:), allocatable :: text, err_text
        integer :: exitcode

        call command_changed('.', text, err_text, exitcode)
        call emit_command(text, err_text, exitcode)
    end subroutine cmd_changed

    subroutine cmd_build()
        type(backend_t) :: b
        type(project_model_t) :: model
        character(len=:), allocatable :: model_message
        integer :: exitcode, model_error
        integer, allocatable :: cases(:)
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
        call backend_profile(b, profile)
        all_flags = trim(profile_flags(profile))
        if (len_trim(flags) > 0) then
            if (len_trim(all_flags) > 0) then
                all_flags = trim(all_flags)//' '//trim(flags)
            else
                all_flags = trim(flags)
            end if
        end if
        call make_tmpfile('fo-build', build_log)
        ! --tests builds what every test case needs, slow ones included (fpm:
        ! all test programs; CMake: the plan targets of the model's cases), so
        ! `fo test --no-build` can run any of them afterwards.
        if (has_arg('--tests') .and. b%kind == BACKEND_CMAKE .and. &
            project_has_model(b)) then
            call project_load(b, trim(all_flags), model, model_error, model_message)
            if (model_error /= 0) then
                write (error_unit, '(a)') 'fo: '//model_message
                call process_exit(1)
            end if
            call build_case_indices(model, cases)
            call project_build_cases(b, model, cases, trim(all_flags), build_log, &
                exitcode)
        else
            call backend_build(b, exitcode, trim(all_flags), build_log, &
                with_tests=has_arg('--tests'), include_slow=.true.)
        end if
        if (exitcode /= 0) then
            call report_build_result(build_log)
            call process_exit(1)
        end if
        call report_array_temporary_warnings(build_log)
        call delete_tmpfile(build_log)
        call report_private_fo_driver(b, all_flags)
    end subroutine cmd_build

    subroutine build_case_indices(model, cases)
        !! Cases named by `fo build --tests --case NAME` (repeatable) or
        !! scoped by -L/-LE/--layer (the `fo test` semantics), else every
        !! inventory case. An unknown name or layer is an error.
        type(project_model_t), intent(in) :: model
        integer, allocatable, intent(out) :: cases(:)
        type(model_request_t) :: request
        type(model_selection_t) :: selection
        character(len=1024) :: arg
        integer :: i, k, n
        integer, allocatable :: named(:), inventory(:), picked(:)
        logical :: scoped

        allocate (named(command_argument_count()))
        allocate (request%names(0), request%include_labels(0), &
            request%exclude_labels(0), request%layers(0))
        n = 0
        do i = 2, command_argument_count() - 1
            call get_command_argument(i, arg)
            select case (trim(arg))
            case ('--case')
                call get_command_argument(i + 1, arg)
                k = model_find_case(model, trim(arg))
                if (k == 0) then
                    write (error_unit, '(a)') 'fo: unknown test case: '//trim(arg)
                    call process_exit(1)
                end if
                n = n + 1
                named(n) = k
                call append_request_string(request%names, arg)
            case ('-L', '--label')
                call get_command_argument(i + 1, arg)
                call append_request_string(request%include_labels, arg)
            case ('-LE', '--exclude-label')
                call get_command_argument(i + 1, arg)
                call append_request_string(request%exclude_labels, arg)
            case ('--layer')
                call get_command_argument(i + 1, arg)
                call append_request_string(request%layers, arg)
            end select
        end do
        scoped = size(request%include_labels) + size(request%exclude_labels) + &
            size(request%layers) > 0
        if (.not. scoped) then
            if (n == 0) then
                call project_inventory_cases(model, cases)
            else
                cases = named(:n)
            end if
            return
        end if
        call model_select(model, request, selection)
        if (selection%status /= SELECT_OK .and. selection%status /= SELECT_EMPTY) then
            write (error_unit, '(a)') 'fo: '//selection%message
            call process_exit(1)
        end if
        ! Cases the campaign may run: not preset-filtered, not explicit-only.
        call project_inventory_cases(model, inventory)
        allocate (picked(size(selection%cases)))
        n = 0
        do i = 1, size(selection%cases)
            if (.not. any(inventory == selection%cases(i))) cycle
            n = n + 1
            picked(n) = selection%cases(i)
        end do
        allocate (cases(n))
        cases = picked(:n)
        if (n == 0) then
            write (error_unit, '(a)') 'fo: the scope selects no test case'
            call process_exit(1)
        end if
    end subroutine build_case_indices

    subroutine report_private_fo_driver(b, request_flags)
        ! A self-build already creates its current driver inside this
        ! worktree. Advertise that exact path so callers can run it directly;
        ! only `fo install` publishes into an installation prefix.
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: request_flags
        character(len=8) :: disabled
        character(len=1024) :: private_driver
        integer(c_long_long) :: mtime_ns, file_size
        integer :: env_status
        logical :: here

        call get_environment_variable('FO_DISABLE_SELF_REFRESH', disabled, &
            status=env_status)
        if (env_status == 0 .and. trim(disabled) == '1') return
        if (b%kind /= BACKEND_NATIVE) return
        inquire (file=trim(b%project_dir)//'/src/build/fo_build_backend.f90', &
            exist=here)
        if (.not. here) return
        private_driver = native_output_dir(b%project_dir, request_flags, 'app')// &
            '/fo'
        call fs_stat(trim(private_driver), mtime_ns, file_size, here)
        if (here .and. file_size > 0) write (output_unit, '(a,a)') &
            'fo: worktree driver ready: ', trim(private_driver)
    end subroutine report_private_fo_driver

    subroutine report_array_temporary_warnings(build_log)
        character(len=*), intent(in) :: build_log
        type(diagnostic_t), allocatable :: warnings(:)
        integer, parameter :: MAX_BUILD_WARNINGS = 4096
        integer :: i, n_warnings
        logical :: exists

        inquire (file=trim(build_log), exist=exists)
        if (.not. exists) return
        allocate (warnings(MAX_BUILD_WARNINGS))
        call array_temporary_warnings_from_log(build_log, warnings, n_warnings)
        do i = 1, n_warnings
            if (len_trim(warnings(i)%file) > 0) then
                write (error_unit, '(a,a,a,i0,a,i0,a,a)') 'fo: warning: ', &
                    trim(warnings(i)%file), ':', warnings(i)%line, ':', &
                    warnings(i)%column, ': ', trim(warnings(i)%message)
            else
                write (error_unit, '(a,a)') 'fo: warning: ', &
                    trim(warnings(i)%message)
            end if
        end do
        if (n_warnings == MAX_BUILD_WARNINGS) write (error_unit, '(a)') &
            'fo: warning: additional array-temporary diagnostics were truncated'
    end subroutine report_array_temporary_warnings

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
        call backend_profile(b, profile)
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
            case ('--release')
                profile = 'release'
            case ('--profile')
                if (i < command_argument_count()) &
                    call get_command_argument(i + 1, profile)
            end select
            if (index(trim(arg), '--profile=') == 1) profile = trim(arg(11:))
        end do
    end subroutine get_profile_arg

    subroutine cmd_test()
        type(backend_t) :: b
        integer :: exitcode, ierr, i, n_arg_names
        integer :: random_count, random_seed, clock_count
        integer :: argument_status, eq
        logical :: only_changed, include_all, verbose, use_json, skip_next
        logical :: end_options, flags_seen, list_mode, filtered, no_build
        type(string_t), allocatable :: widen_reasons(:)
        type(model_request_t) :: request
        type(model_selection_t) :: selection
        type(project_model_t) :: model
        character(len=:), allocatable :: text, message
        character(len=1024) :: arg
        character(len=1024), allocatable :: test_names(:), random_candidates(:)
        character(len=512) :: test_log, flags
        character(len=64) :: profile
        character(len=1024) :: all_flags

        allocate (test_names(MAX_NODES), random_candidates(MAX_NODES))
        only_changed = .false.
        include_all = .false.
        verbose = .false.
        use_json = .false.
        flags = ''
        profile = ''
        flags_seen = .false.
        n_arg_names = 0
        random_count = 0
        random_seed = 0
        skip_next = .false.
        end_options = .false.
        list_mode = .false.
        no_build = .false.
        allocate (request%include_labels(0), request%exclude_labels(0), &
            request%layers(0))
        do i = 2, command_argument_count()
            call get_command_argument(i, arg, status=argument_status)
            if (argument_status /= 0) then
                write (error_unit, '(a)') &
                    'fo: test argument exceeds the 1024-character bound'
                call process_exit(2)
            end if
            if (skip_next) then
                skip_next = .false.
                cycle
            end if
            if (end_options) then
                n_arg_names = n_arg_names + 1
                test_names(n_arg_names) = trim(arg)
                cycle
            end if
            if (trim(arg) == '--') then
                end_options = .true.
                cycle
            end if
            if (index(trim(arg), '--profile=') == 1) then
                if (len_trim(arg) <= 10) call test_usage_error('--profile needs NAME')
                profile = arg(11:)
                cycle
            end if
            eq = index(arg, '=')
            if (eq > 2 .and. arg(1:2) == '--') then
                select case (arg(1:eq - 1))
                case ('--label')
                    if (len_trim(arg) <= eq) call test_usage_error('--label needs RE')
                    call append_request_string(request%include_labels, arg(eq + 1:))
                    cycle
                case ('--exclude-label')
                    if (len_trim(arg) <= eq) &
                        call test_usage_error('--exclude-label needs RE')
                    call append_request_string(request%exclude_labels, arg(eq + 1:))
                    cycle
                case ('--layer')
                    if (len_trim(arg) <= eq) call test_usage_error('--layer needs NAME')
                    call append_request_string(request%layers, arg(eq + 1:))
                    cycle
                end select
            end if
            select case (trim(arg))
            case ('--flag')
                if (i >= command_argument_count()) &
                    call test_usage_error('--flag needs FLAGS')
                if (.not. flags_seen) call get_command_argument(i + 1, flags)
                flags_seen = .true.
                skip_next = .true.
            case ('--profile')
                if (i >= command_argument_count()) &
                    call test_usage_error('--profile needs NAME')
                call get_command_argument(i + 1, profile)
                if (len_trim(profile) == 0) call test_usage_error('--profile needs NAME')
                skip_next = .true.
            case ('--random')
                if (i >= command_argument_count()) &
                    call test_usage_error('--random needs N')
                call get_command_argument(i + 1, arg)
                read (arg, *, iostat=ierr) random_count
                if (ierr /= 0 .or. random_count < 1) &
                    call test_usage_error('invalid --random N')
                skip_next = .true.
            case ('--seed')
                if (i >= command_argument_count()) &
                    call test_usage_error('--seed needs S')
                call get_command_argument(i + 1, arg)
                read (arg, *, iostat=ierr) random_seed
                if (ierr /= 0) call test_usage_error('invalid --seed S')
                skip_next = .true.
            case ('-L', '--label')
                call test_option_value(i, '--label needs RE', arg)
                call append_request_string(request%include_labels, arg)
                skip_next = .true.
            case ('-LE', '--exclude-label')
                call test_option_value(i, '--exclude-label needs RE', arg)
                call append_request_string(request%exclude_labels, arg)
                skip_next = .true.
            case ('--layer')
                call test_option_value(i, '--layer needs NAME', arg)
                call append_request_string(request%layers, arg)
                skip_next = .true.
            case ('--list')
                list_mode = .true.
            case ('--no-build')
                no_build = .true.
            case ('--only-changed')
                only_changed = .true.
            case ('--all')
                include_all = .true.
            case ('--verbose')
                verbose = .true.
            case ('--json')
                use_json = .true.
            case ('--debug')
                profile = 'debug'
            case ('--release')
                profile = 'release'
            case ('--asan')
                profile = 'asan'
            case ('-h', '--help')
                call print_test_usage()
                return
            case default
                if (arg(1:1) == '-') &
                    call test_usage_error('unknown option: '//trim(arg))
                n_arg_names = n_arg_names + 1
                test_names(n_arg_names) = trim(arg)
            end select
        end do

        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if
        if (len_trim(profile) > 0 .and. len_trim(profile_flags(profile)) == 0) then
            write (error_unit, '(a)') 'fo: unknown test profile: '//trim(profile)
            stop 1
        end if
        call backend_profile(b, profile)
        all_flags = trim(profile_flags(profile))
        if (len_trim(flags) > 0) then
            if (len_trim(all_flags) > 0) then
                all_flags = trim(all_flags)//' '//trim(flags)
            else
                all_flags = trim(flags)
            end if
        end if

        filtered = size(request%include_labels) > 0 .or. &
            size(request%exclude_labels) > 0 .or. size(request%layers) > 0
        if (random_count > 0) then
            if (filtered .or. list_mode .or. only_changed) then
                write (error_unit, '(a)') 'fo test: --random conflicts with '// &
                    '-L, -LE, --layer, --list and --only-changed'
                stop 1
            end if
            if (n_arg_names > 0) then
                write (error_unit, '(a)') &
                    'fo test: --random conflicts with names or --only-changed'
                stop 1
            end if
        end if

        if (.not. project_has_model(b)) then
            ! Frozen FO_CMAKE_NATIVE route: no project model, the CTest
            ! registry is authoritative and a no-match is rejected by the run.
            if (filtered .or. list_mode .or. only_changed .or. random_count > 0) then
                write (error_unit, '(a)') 'fo test: -L, -LE, --layer, --list, '// &
                    '--random and --only-changed need the project model '// &
                    '(unsupported with FO_CMAKE_NATIVE)'
                stop 1
            end if
            call make_tmpfile('fo-test', test_log)
            if (n_arg_names > 0) then
                call backend_test_names(b, test_names, n_arg_names, exitcode, &
                    include_all, test_log, flags=all_flags, no_build=no_build)
            else
                call backend_test(b, exitcode, include_all, test_log, &
                    flags=all_flags, no_build=no_build)
            end if
            call report_test_result(exitcode, test_log, &
                n_arg_names == 0 .and. .not. verbose, use_json, &
                require_results=n_arg_names > 0)
            call delete_tmpfile(test_log)
            return
        end if

        ! One selection path for every backend, CLI and MCP: load the project
        ! model (CMake is configured, nothing is built), select, run the exact
        ! cases. The model applies the default exclusions and --all.
        allocate (request%names(n_arg_names))
        do i = 1, n_arg_names
            request%names(i)%s = trim(test_names(i))
        end do
        request%all = include_all
        call project_select(b, all_flags, request, only_changed, model, selection, &
            widen_reasons, ierr, message)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: '//message
            stop 1
        end if
        do i = 1, size(widen_reasons)
            write (error_unit, '(a)') 'fo: affected selection widened: '// &
                widen_reasons(i)%s
        end do
        if (random_count > 0) then
            ! Candidates are the model's default selection.
            if (size(selection%cases) > size(random_candidates)) then
                deallocate (random_candidates)
                allocate (random_candidates(size(selection%cases)))
            end if
            do i = 1, size(selection%cases)
                random_candidates(i) = model%cases(selection%cases(i))%name
            end do
            if (random_seed == 0) then
                call system_clock(clock_count)
                random_seed = clock_count
            end if
            call select_random_tests(random_candidates, size(selection%cases), &
                random_count, random_seed, test_names, n_arg_names)
            if (n_arg_names == 0) then
                write (error_unit, '(a)') 'fo: no eligible tests for --random'
                if (.not. include_all) then
                    write (error_unit, '(a)') 'fo: use --all to include slow tests'
                end if
                stop 1
            end if
            write (error_unit, '(a,i0,a,i0,a)') &
                'fo: random seed ', random_seed, ' selected ', n_arg_names, &
                ' test(s)'
            selection%cases = [(model_find_case(model, trim(test_names(i))), &
                i=1, n_arg_names)]
        end if
        if (list_mode) then
            if (use_json) then
                call project_list_json(model, selection, text)
            else
                call project_list_text(model, selection, text)
            end if
            write (output_unit, '(a)') text
            return
        end if
        if (selection%status == SELECT_EMPTY) then
            if (only_changed) then
                write (output_unit, '(a)') 'no affected tests'
                return
            end if
            if (model%n_cases == 0 .and. n_arg_names == 0 .and. .not. filtered) then
                write (output_unit, '(a)') 'fo: project registers no tests'
                return
            end if
            write (error_unit, '(a)') 'fo: '//selection%message
            stop 1
        end if
        call make_tmpfile('fo-test', test_log)
        call project_run_cases(b, model, selection%cases, all_flags, no_build, &
            test_log, exitcode)
        call report_test_result(exitcode, test_log, &
            random_count == 0 .and. n_arg_names == 0 .and. .not. filtered .and. &
            .not. verbose .and. .not. only_changed, use_json, &
            require_results=.true.)
        call delete_tmpfile(test_log)
    end subroutine cmd_test

    subroutine test_option_value(i, message, value)
        integer, intent(in) :: i
        character(len=*), intent(in) :: message
        character(len=*), intent(out) :: value

        value = ''
        if (i >= command_argument_count()) call test_usage_error(message)
        call get_command_argument(i + 1, value)
        if (len_trim(value) == 0) call test_usage_error(message)
    end subroutine test_option_value

    subroutine append_request_string(list, value)
        type(string_t), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: value
        type(string_t), allocatable :: grown(:)
        integer :: n

        n = size(list)
        allocate (grown(n + 1))
        if (n > 0) grown(1:n) = list
        grown(n + 1)%s = trim(value)
        call move_alloc(grown, list)
    end subroutine append_request_string

    subroutine test_usage_error(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'fo test: '//trim(message)
        call print_test_usage(error_unit)
        stop 2
    end subroutine test_usage_error


    integer function index_of_last_char(text, ch) result(position)
        character(len=*), intent(in) :: text
        character(len=1), intent(in) :: ch
        integer :: i
        position = 0
        do i = 1, len_trim(text)
            if (text(i:i) == ch) position = i
        end do
    end function index_of_last_char

    integer function last_dot_before_f90(label) result(dot)
        ! Position just before a trailing .f90/.F90/.f95/.f03/.f08 or .c
        ! suffix; 0 when no such suffix closes the basename.
        character(len=*), intent(in) :: label
        integer :: n
        character(len=8) :: lower
        n = len_trim(label)
        lower = label
        call lowercase_in_place(lower)
        dot = 0
        if (n < 4) return
        if (lower(n-3:n) == '.f90' .or. lower(n-3:n) == '.f95' .or. &
            lower(n-3:n) == '.f03' .or. lower(n-3:n) == '.f08') then
            dot = n-3
        else if (n >= 3 .and. lower(n-1:n) == '.c') then
            dot = n-1
        else if (lower(n-2:n) == '.cu' .or. lower(n-2:n) == '.cc') then
            dot = n-2
        end if
    end function last_dot_before_f90

    subroutine lowercase_in_place(text)
        character(len=*), intent(inout) :: text
        integer :: i, k
        do i = 1, len_trim(text)
            k = index('ABCDEFGHIJKLMNOPQRSTUVWXYZ', text(i:i))
            if (k > 0) text(i:i) = achar(k + 32)
        end do
    end subroutine lowercase_in_place


    logical function env_flag(name) result(on)
        !! True when the environment variable is set to a truthy 1.
        character(len=*), intent(in) :: name

        character(len=64) :: value
        integer :: length

        call get_environment_variable(name, value, length)
        on = length > 0 .and. trim(value) == '1'
    end function env_flag

    subroutine report_test_result(exitcode, test_log, summary_mode, use_json, require_results)
        integer, intent(inout) :: exitcode
        character(len=*), intent(in) :: test_log
        logical, intent(in) :: summary_mode
        logical, intent(in) :: use_json
        logical, intent(in), optional :: require_results

        type(test_result_entry_t), allocatable :: entries(:)
        integer :: n_entries, parse_ierr
        character(len=:), allocatable :: json_output, human_output
        type(diagnostic_t) :: diag
        character(len=:), allocatable :: failed_tests(:)
        integer :: n_failed_tests, i
        character(len=8) :: exit_out
        character(len=16) :: secs_out

        call parse_test_results(test_log, entries, n_entries, parse_ierr)
        if (parse_ierr /= 0) then
            exitcode = 1
            if (use_json) then
                write (output_unit, '(a)') '{"tests":[],"summary":'// &
                    '{"passed":0,"failed":0,"skipped":0,'// &
                    '"total_seconds":0},"exit_code":1,'// &
                    '"error":"could not parse test results"}'
            else
                write (error_unit, '(a)') 'fo: could not parse test results'
            end if
            call process_exit(1)
            return
        end if

        if (present(require_results)) then
            if (require_results .and. n_entries == 0 .and. exitcode == 0) then
                if (use_json) then
                    write (output_unit, '(a)') '{"tests":[],"exit_code":1,'// &
                        '"error":"selected tests produced no results"}'
                else
                    write (error_unit, '(a)') 'fo: selected tests produced no results'
                end if
                call process_exit(1)
            end if
        end if

        ! `fo test` normally prints an aggregate count and the failures, and the
        ! per-test log is deleted. A CI that diffs the fail-name set against a
        ! baseline needs every name reported, pass included; FO_TEST_REPORT_NAMES=1
        ! prints the TEST_RESULT lines to stdout before the log goes away.
        if (env_flag('FO_TEST_REPORT_NAMES')) then
            do i = 1, n_entries
                write (secs_out, '(F8.2)') entries(i)%seconds
                write (exit_out, '(i0)') entries(i)%exit_code
                write (output_unit, '(a,a,a,a,a,a,a,a)') 'TEST_RESULT ', &
                    trim(entries(i)%name), ' ', trim(entries(i)%status), ' ', &
                    trim(exit_out), ' ', trim(secs_out)
            end do
        end if

        if (use_json) then
            call format_test_results_json(entries, n_entries, exitcode, &
                json_output, test_log)
            write (output_unit, '(a)') trim(json_output)
        else if (n_entries > 0) then
            call format_test_results_text(entries, n_entries, test_log, &
                summary_mode, human_output)
            if (len_trim(human_output) > 0) then
                write (output_unit, '(a)') trim(human_output)
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

        if (exitcode /= 0) call process_exit(1)
    end subroutine report_test_result

    subroutine cmd_graph()
        character(len=:), allocatable :: text, err_text
        integer :: exitcode

        if (has_arg('--layers')) then
            call graph_layers()
            return
        end if
        call command_graph('.', has_arg('--dot'), text, err_text, exitcode)
        call emit_command(text, err_text, exitcode)
    end subroutine cmd_graph

    subroutine graph_layers()
        !! `fo graph --layers [--json]`: layering check on the project model.
        !! Exit 0 clean, 1 on a violation, 2 when no model can be loaded.
        use fo_build_backend, only: backend_profile, profile_flags
        use fo_project_model, only: project_model_t, model_layering_t
        use fo_project_load, only: project_has_model, project_layering, &
            project_layering_text, project_layering_json
        type(backend_t) :: b
        type(project_model_t) :: model
        type(model_layering_t) :: report
        type(string_t), allocatable :: unscanned(:)
        character(len=:), allocatable :: message, text
        integer :: ierr

        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            call process_exit(2)
        end if
        if (.not. project_has_model(b)) then
            write (error_unit, '(a)') &
                'fo graph --layers: no project model with FO_CMAKE_NATIVE'
            call process_exit(2)
        end if
        call backend_profile(b, '')
        call project_layering(b, trim(profile_flags('')), model, report, &
            unscanned, ierr, message)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo graph --layers: '//message
            call process_exit(2)
        end if
        if (has_arg('--json')) then
            call project_layering_json(trim(b%project_dir), model, report, &
                unscanned, text)
        else
            call project_layering_text(trim(b%project_dir), model, report, &
                unscanned, text)
            text = text(1:len(text) - 1)
        end if
        write (output_unit, '(a)') text
        if (size(report%violations) > 0) call process_exit(1)
    end subroutine graph_layers

    subroutine cmd_lint()
        use fo_lint_deep, only: lint_deep_print
        use fo_process, only: process_getcwd
        use fo_fs, only: fs_realpath
        use fx_path, only: path_normalize, path_is_absolute
        character(len=512), allocatable :: selected(:)
        integer :: i, n_selected, arg_length, status
        character(len=512) :: argument
        character(len=4096) :: resolved, cwd
        character(len=:), allocatable :: text, deep_report, tail, err_text, normalized
        integer :: exitcode
        logical :: ok, deep, fix, json_mode

        deep = has_arg('--deep')
        fix = has_arg('--fix')
        json_mode = has_arg('--json') .or. has_arg('--json=compact') .or. &
            has_arg('--json=full')
        allocate (selected(max(0, nargs - 1)))
        call process_getcwd(cwd, status)
        if (status /= 0) then
            write (error_unit, '(a)') 'fo lint: cannot identify working directory'
            call process_exit(2)
        end if
        n_selected = 0
        do i = 2, nargs
            call get_command_argument(i, length=arg_length)
            if (arg_length > len(argument)) then
                write (error_unit, '(a)') &
                    'fo lint: source argument exceeds 512 characters'
                call process_exit(2)
            end if
            call get_command_argument(i, argument)
            select case (trim(argument))
            case ('--deep', '--fix', '--json', '--json=compact', '--json=full')
                cycle
            case default
                if (len_trim(argument) == 0) then
                    write (error_unit, '(a)') 'fo lint: empty source argument'
                    call process_exit(2)
                end if
                if (argument(1:1) == '-') then
                    write (error_unit, '(a)') 'fo lint: unknown option '//trim(argument)
                    call process_exit(2)
                end if
                n_selected = n_selected + 1
                if (path_is_absolute(trim(argument))) then
                    normalized = path_normalize(trim(argument))
                else
                    normalized = path_normalize(trim(cwd)//'/'//trim(argument))
                end if
                call fs_realpath(normalized, resolved, ok)
                if (ok) normalized = trim(resolved)
                if (len(normalized) > len(argument)) then
                    write (error_unit, '(a)') &
                        'fo lint: normalized source path exceeds 512 characters'
                    call process_exit(2)
                end if
                selected(n_selected) = normalized
            end select
        end do

        if (fix .and. (deep .or. n_selected > 0)) then
            write (error_unit, '(a)') &
                'fo lint: --fix cannot combine with --deep or selected sources'
            call process_exit(2)
        end if
        call command_lint('.', selected(:n_selected), deep, fix, json_mode, text, &
            deep_report, tail, err_text, exitcode)
        if (len(text) > 0) write (output_unit, '(a)', advance='no') text
        if (len(deep_report) > 0) call lint_deep_print(deep_report)
        call emit_command(tail, err_text, exitcode)
    end subroutine cmd_lint

    subroutine cmd_fmt()
        integer :: exitcode, selection
        integer :: i, n_fmt_files
        character(len=MAX_PATH) :: arg
        character(len=MAX_PATH) :: fmt_files(MAX_NODES)
        character(len=8192) :: fmt_output
        character(len=16) :: suffix, deep_flag
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
                        call process_exit(1)
                    end if
                end if
                if (n_fmt_files >= size(fmt_files)) then
                    write (error_unit, '(a)') 'fo fmt: too many paths'
                    call process_exit(1)
                end if
                n_fmt_files = n_fmt_files + 1
                fmt_files(n_fmt_files) = arg
            end select
        end do

        if (changed_mode) then
            if (check_mode) then
                write (error_unit, '(a)') 'fo fmt: --check and --changed cannot be combined'
                call process_exit(1)
            end if
            if (n_fmt_files > 0) then
                write (error_unit, '(a)') 'fo fmt: --changed does not accept paths'
                call process_exit(1)
            end if
        end if

        if (changed_mode) then
            selection = FMT_SELECT_CHANGED
        else if (n_fmt_files > 0) then
            selection = FMT_SELECT_FILES
        else
            selection = FMT_SELECT_ALL
        end if
        call fmt_sources('.', selection, check_mode, deep_mode, fmt_files, n_fmt_files, &
            fmt_output, exitcode)

        if (check_mode) then
            if (len_trim(fmt_output) > 0) &
                write (error_unit, '(a)') trim(fmt_output)
            if (exitcode /= 0) call process_exit(1)
            return
        end if

        suffix = ''
        deep_flag = ''
        if (deep_mode) then
            suffix = ' (deep)'
            deep_flag = ' --deep'
        end if
        if (exitcode /= 0) then
            if (changed_mode) then
                write (error_unit, '(a)') 'fo fmt'//trim(deep_flag)// &
                    ' --changed: no Git worktree'
            else
                write (error_unit, '(a)') 'fo fmt'//trim(deep_flag)// &
                    ': formatting failed'
            end if
            call process_exit(1)
        end if
        select case (selection)
        case (FMT_SELECT_CHANGED)
            write (output_unit, '(a)') 'formatted changed sources'//trim(suffix)
        case (FMT_SELECT_FILES)
            write (output_unit, '(a)') 'formatted selected sources'//trim(suffix)
        case default
            write (output_unit, '(a)') 'formatted'//trim(suffix)
        end select
    end subroutine cmd_fmt

    subroutine cmd_install()
        character(len=256) :: prefix
        character(len=512) :: error_message
        character(len=:), allocatable :: text, err_text
        integer :: exitcode, status

        call parse_install_args(prefix, error_message, status)
        if (status /= 0) then
            write (error_unit, '(a,a)') 'fo install: ', trim(error_message)
            write (error_unit, '(a)') 'fo install: use --help for usage'
            call process_exit(2)
        end if

        call command_install('.', trim(prefix), text, err_text, exitcode)
        call emit_command(text, err_text, exitcode)
    end subroutine cmd_install

    subroutine parse_install_args(prefix, error_message, exitcode)
        character(len=*), intent(out) :: prefix, error_message
        integer, intent(out) :: exitcode
        character(len=256) :: arg, value
        character(len=512) :: home
        integer :: i, status, equals_at

        call get_environment_variable('HOME', home, status=status)
        if (status /= 0 .or. len_trim(home) == 0) home = '/usr/local'
        prefix = trim(home)//'/.local'
        error_message = ''
        exitcode = 0

        i = 2
        do while (i <= command_argument_count())
            call get_command_argument(i, arg)
            if (trim(arg) == '--prefix') then
                if (i == command_argument_count()) then
                    error_message = 'missing value for --prefix'
                    exitcode = 2
                    return
                end if
                call get_command_argument(i + 1, prefix)
                if (len_trim(prefix) == 0) then
                    error_message = 'prefix must not be empty'
                    exitcode = 2
                    return
                end if
                if (prefix(1:1) == '-') then
                    error_message = 'invalid value for --prefix: '//trim(prefix)
                    exitcode = 2
                    return
                end if
                i = i + 2
                cycle
            else if (index(trim(arg), '--prefix=') == 1) then
                equals_at = index(trim(arg), '=')
                value = arg(equals_at + 1:)
                if (len_trim(value) == 0) then
                    error_message = 'prefix must not be empty'
                    exitcode = 2
                    return
                end if
                if (value(1:1) == '-') then
                    error_message = 'invalid value for --prefix: '//trim(value)
                    exitcode = 2
                    return
                end if
                prefix = value
                i = i + 1
            else
                error_message = 'unknown option: '//trim(arg)
                exitcode = 2
                return
            end if
        end do
    end subroutine parse_install_args

    subroutine cmd_clean()
        use fo_clean_stale, only: clean_stale_run, CLEAN_STALE_DEFAULT_KEEP
        type(backend_t) :: b
        character(len=512) :: arg
        integer :: i, n_removed, exitcode, keep
        integer(8) :: freed_bytes
        logical :: purge_store, stale_only
        character(len=:), allocatable :: text
        logical :: want_help

        ! `--help` must never reach the destructive path below. The flag loop
        ! only knows --cache/--all/--stale/--keep and ignores everything else, so
        ! `fo clean --help` used to fall through and wipe the build tree while
        ! printing no usage at all (#136). The flag a cautious user types to ask
        ! what a command does is exactly the one that must not act.
        want_help = .false.
        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--help' .or. trim(arg) == '-h') want_help = .true.
        end do
        if (want_help) then
            write (output_unit, '(a)') 'usage: fo clean [OPTIONS]'
            write (output_unit, '(a)') ''
            write (output_unit, '(a)') &
                '  (no flags)   drop this project''s build/ tree; the shared'// &
                ' cache is kept'
            write (output_unit, '(a)') &
                '  --stale      drop only unreferenced artifacts, keeping the'// &
                ' newest N archives'
            write (output_unit, '(a)') &
                '  --keep N     number of unreferenced archives to keep with'// &
                ' --stale'
            write (output_unit, '(a)') &
                '  --cache      also purge the shared content-addressed store'// &
                ' (cold-starts every project)'
            write (output_unit, '(a)') &
                '  --all        same as --cache'
            write (output_unit, '(a)') &
                '  -h, --help   print this text and exit without deleting'// &
                ' anything'
            return
        end if

        ! Default clean is project-scoped: drop only this project's build/ tree
        ! (a disposable view that fo regenerates). The content-addressed cache
        ! is shared across projects; purging it forces cold builds elsewhere.
        ! Purge it only when explicitly asked.
        purge_store = .false.
        stale_only = .false.
        keep = CLEAN_STALE_DEFAULT_KEEP
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--cache' .or. trim(arg) == '--all') &
                purge_store = .true.
            if (trim(arg) == '--stale') stale_only = .true.
            if (trim(arg) == '--keep') then
                call get_command_argument(i + 1, arg)
                read (arg, *, iostat=n_removed) keep
                if (n_removed /= 0) keep = CLEAN_STALE_DEFAULT_KEEP
            end if
        end do

        ! `--stale` is the opposite trade from a full clean: keep everything the
        ! last build still references, delete the generations a content-addressed
        ! tree can never revisit. ffc carried 6.5 GB of unreachable link archives
        ! and stale compiler profile dirs this way.
        if (stale_only) then
            b = detect_backend('.')
            call clean_stale_run(trim(b%project_dir), keep, n_removed, freed_bytes)
            write (output_unit, '(a,i0,a,f0.1,a,i0,a)') &
                'stale artifacts removed: ', n_removed, ' entries, ', &
                real(freed_bytes)/1024.0/1024.0, ' MiB (kept the newest ', keep, &
                ' unreferenced archives)'
            return
        end if

        call command_clean('.', purge_store, text, exitcode)
        call emit_command(text, '', exitcode)
    end subroutine cmd_clean

    subroutine cmd_update()
        use fo_dep_update, only: dep_update_run
        type(backend_t) :: b
        character(len=1024) :: update_error
        integer :: n_deps
        logical :: refreshed

        b = detect_backend('.')
        call dep_update_run(trim(b%project_dir), n_deps, refreshed, update_error)
        if (len_trim(update_error) > 0) then
            write (error_unit, '(a)') 'fo update: '//trim(update_error)
            call process_exit(1)
        end if
        if (.not. refreshed) then
            write (output_unit, '(a)') &
                'no git or registry dependencies to refresh'
            return
        end if
        write (output_unit, '(a,i0,a)') 'dropped cached sources and objects for ', &
            n_deps, ' external dependency(ies); the next build re-fetches them'
    end subroutine cmd_update

    subroutine cmd_info()
        character(len=:), allocatable :: text
        integer :: exitcode

        call command_info('.', has_arg('--capabilities'), text, exitcode)
        call emit_command(text, '', exitcode)
    end subroutine cmd_info

    subroutine emit_command(text, err_text, exitcode)
        !! Print a command's stdout and stderr texts and apply its exit code.
        character(len=*), intent(in) :: text, err_text
        integer, intent(in) :: exitcode

        if (len(text) > 0) write (output_unit, '(a)', advance='no') text
        if (len(err_text) > 0) write (error_unit, '(a)', advance='no') err_text
        if (exitcode /= 0) call process_exit(exitcode)
    end subroutine emit_command

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
