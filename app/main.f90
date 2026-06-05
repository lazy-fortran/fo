program fo_main
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS, MAX_NAME, is_slow_test
    use fx_dag, only: dag_t, dag_topo_sort, dag_to_dot, MAX_NODES
    use fo_dag_bridge, only: build_dag_from_units
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE, &
                                BACKEND_FPM, BACKEND_CMAKE
    use fo_check, only: check_result_t, fo_check_run, fo_changed_modules
    use fo_check_output, only: check_result_json, check_result_compact_json, &
                               check_result_full_json
    use fo_capabilities, only: capabilities_t, detect_capabilities, &
                               capabilities_json
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

        ! 2. build (restore cached artifacts first, store after)
        block
            use fo_artifact_cache, only: artifact_restore, artifact_store
            integer :: n_restored, art_ierr

            call artifact_restore(trim(b%project_dir)//'/build', n_restored, art_ierr)
            call b%build(exitcode)
            if (exitcode /= 0) then
                write (error_unit, '(a)') 'Build: FAIL'
                stop 1
            end if
            call artifact_store(trim(b%project_dir)//'/build', art_ierr)
        end block
        write (output_unit, '(a)') 'Build: OK'

        ! 3. test: skip if nothing changed, otherwise run affected tests only
        if (n_changed == 0) then
            call cpu_time(t1)
            write (output_unit, '(a,f0.1,a)') &
                'Tests: skipped, all cached (', t1 - t0, 's)'
            return
        end if

        ! collect affected test names (excluding slow)
        n_test_names = 0
        do i = 1, n_affected
            if (is_test_arr(affected_ids(i))) then
                if (.not. is_slow_test(dag%nodes(affected_ids(i))%label)) then
                    n_test_names = n_test_names + 1
                    test_names(n_test_names) = dag%nodes(affected_ids(i))%label
                end if
            end if
        end do

        if (n_test_names > 0) then
            call b%test_names(test_names, n_test_names, exitcode)
            if (exitcode /= 0) then
                write (error_unit, '(a)') 'Tests: FAIL'
                stop 1
            end if
        else
            ! no specific affected tests found; run all non-slow
            call b%test(exitcode)
            if (exitcode /= 0) then
                write (error_unit, '(a)') 'Tests: FAIL'
                stop 1
            end if
        end if

        write (output_unit, '(a)') 'Tests: OK'

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
            call fmt_check(trim(b%project_dir), fmt_exit)
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
        write (output_unit, '(a)') '  check      build + test, one-line status'
        write (output_unit, '(a)') '  check --json  build + test, JSON status'
        write (output_unit, '(a)') '  check --json=compact  bounded agent JSON'
        write (output_unit, '(a)') '  check --json=full  JSON status with diagnostics'
        write (output_unit, '(a)') '  check --agent  compact JSON for opencode/Qwen'
        write (output_unit, '(a)') '  changed    list changed and affected modules'
        write (output_unit, '(a)') '  graph      module dependency graph'
        write (output_unit, '(a)') '  graph --dot  graph in Graphviz DOT format'
      write (output_unit, '(a)') '  fmt        format sources (fprettify, 88 col, 4 sp)'
    write (output_unit, '(a)') '  fmt --check  check formatting without modifying files'
        write (output_unit, '(a)') '  watch      rebuild on file change (inotify loop)'
    write (output_unit, '(a)') '  watch --fmt  auto-format changed files before rebuild'
        write (output_unit, '(a)') '  lint       unused imports + gfortran warnings'
        write (output_unit, '(a)') '  lint --json  lint results as JSON'
        write (output_unit, '(a)') '  clean      clear global cache (~/.cache/fo)'
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
            stop 1, quiet = .true.
        end if
    end subroutine cmd_check

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
        character(len=512) :: flags

        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if

        call get_flags_arg(flags)
        if (len_trim(flags) > 0) then
            call b%build(exitcode, flags)
        else
            call b%build(exitcode)
        end if
        if (exitcode /= 0) stop 1
    end subroutine cmd_build

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
        integer :: n_cached, ierr, i, n_test_names
        logical :: only_changed, include_all
        character(len=256) :: arg
        character(len=128) :: test_names(MAX_NODES)
        logical :: is_test_arr(MAX_NODES)

        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if

        only_changed = .false.
        include_all = .false.
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--only-changed') only_changed = .true.
            if (trim(arg) == '--all') include_all = .true.
        end do

        if (only_changed) then
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
                    test_names(n_test_names) = dag%nodes(affected_ids(i))%label
                end if
            end do

            if (n_test_names == 0) then
                write (output_unit, '(a)') 'no affected tests'
                return
            end if

            call b%test_names(test_names, n_test_names, exitcode, include_all)
            if (exitcode /= 0) stop 1
        else
            call b%test(exitcode, include_all)
            if (exitcode /= 0) stop 1
        end if
    end subroutine cmd_test

    subroutine cmd_graph()
        type(scan_unit_t) :: units(MAX_UNITS)
        type(dag_t) :: dag
        type(backend_t) :: b
        character(len=512) :: scan_root
        integer :: n_units, ierr, i, j
        logical :: dot_mode
        character(len=:), allocatable :: dot_output

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
        use fo_util, only: make_tmpfile, delete_tmpfile
        type(backend_t) :: b
        character(len=512) :: scan_root
        character(len=4096) :: cmd
        integer :: exitcode
        logical :: check_mode

        b = detect_backend('.')
        scan_root = '.'
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        check_mode = has_arg('--check')
        if (check_mode) then
            call fmt_check(trim(scan_root), exitcode)
            if (exitcode /= 0) stop 1, quiet = .true.
            return
        end if

        cmd = 'find '//trim(scan_root)// &
              " -path '*/build' -prune -o"// &
              " -path '*/.git' -prune -o"// &
              " \( -name '*.f90' -o -name '*.F90' \) -print"// &
              " | xargs -r fprettify -i 4 -l 88 --strict-indent"
        call execute_command_line(cmd, exitstat=exitcode, wait=.true.)
        if (exitcode /= 0) then
            write (error_unit, '(a)') 'fo fmt: fprettify failed'
            stop 1, quiet = .true.
        end if
        write (output_unit, '(a)') 'formatted'
    end subroutine cmd_fmt

    subroutine fmt_check(scan_root, exitcode)
        use fo_util, only: make_tmpfile, delete_tmpfile
        character(len=*), intent(in) :: scan_root
        integer, intent(out) :: exitcode

        character(len=512) :: list_file, fpath, tmpf
        character(len=4096) :: cmd
        integer :: u, iostat, diff_exit, n_bad

        exitcode = 0
        n_bad = 0

        call make_tmpfile('fo_fmt_files', list_file)
        cmd = 'find '//trim(scan_root)// &
              " -path '*/build' -prune -o"// &
              " -path '*/.git' -prune -o"// &
              " \( -name '*.f90' -o -name '*.F90' \) -print 2>/dev/null"// &
              ' | sort > '//trim(list_file)
        call execute_command_line(cmd, wait=.true.)

        open (newunit=u, file=list_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            call delete_tmpfile(list_file)
            return
        end if

        do
            read (u, '(a)', iostat=iostat) fpath
            if (iostat /= 0) exit
            if (len_trim(fpath) == 0) cycle

            call make_tmpfile('fo_fmt_check', tmpf)
            cmd = 'cp '//trim(fpath)//' '//trim(tmpf)
            call execute_command_line(cmd, wait=.true.)
            cmd = 'fprettify -i 4 -l 88 --strict-indent '//trim(tmpf)// &
                  ' >/dev/null 2>&1'
            call execute_command_line(cmd, wait=.true.)
            cmd = 'diff -q '//trim(fpath)//' '//trim(tmpf)//' >/dev/null 2>&1'
            call execute_command_line(cmd, exitstat=diff_exit, wait=.true.)
            call delete_tmpfile(tmpf)

            if (diff_exit /= 0) then
                write (error_unit, '(a)') trim(fpath)//': needs formatting'
                n_bad = n_bad + 1
            end if
        end do
        close (u)
        call delete_tmpfile(list_file)

        if (n_bad > 0) then
            write (error_unit, '(a,i0,a)') &
                'Format: FAIL (', n_bad, ' files need formatting)'
            exitcode = 1
        else
            write (output_unit, '(a)') 'Format: OK'
        end if
    end subroutine fmt_check

    subroutine cmd_install()
        character(len=256) :: prefix, arg
        character(len=512) :: home
        character(len=4096) :: cmd
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

        cmd = 'fpm install --prefix '//trim(prefix)
        call execute_command_line(cmd, exitstat=exitcode, wait=.true.)
        if (exitcode /= 0) stop 1
        write (output_unit, '(a,a)') 'installed: ', trim(prefix)//'/bin/fo'
    end subroutine cmd_install

    subroutine cmd_clean()
        use fo_cache, only: cache_t, cache_init
        type(cache_t) :: c
        integer :: ierr

        call cache_init(c, ierr)
        if (ierr == 0) then
            call execute_command_line( &
                'rm -rf '//trim(c%root_dir), wait=.true.)
            write (output_unit, '(a,a)') 'cache cleared: ', trim(c%root_dir)
        end if
    end subroutine cmd_clean

    subroutine cmd_info()
        use fo_capabilities, only: capabilities_t, detect_capabilities, &
                                   capabilities_text, capabilities_json
        type(scan_unit_t) :: units(MAX_UNITS)
        type(dag_t) :: dag
        type(backend_t) :: b
        character(len=512) :: scan_root
        integer :: n_units, ierr
        logical :: show_caps
        type(capabilities_t) :: cap

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
        case (BACKEND_FPM)
            write (output_unit, '(a)') 'backend: fpm'
        case (BACKEND_CMAKE)
            write (output_unit, '(a)') 'backend: cmake'
        case default
            write (output_unit, '(a)') 'backend: none'
        end select

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
