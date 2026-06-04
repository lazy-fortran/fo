program fo_main
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS, is_slow_test
    use fo_dag, only: dag_t, dag_build, dag_topo_order, MAX_NODES
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE, &
                                BACKEND_FPM, BACKEND_CMAKE
    use fo_check, only: check_result_t, fo_check_run, fo_changed_modules
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
    case ('clean')
        call cmd_clean()
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
        ! staged pipeline: static -> build -> test
        ! stops at first failure, reports the failing stage
        use fo_dag, only: MAX_NODES
        type(backend_t) :: b
        type(dag_t) :: dag
        type(scan_unit_t) :: units(MAX_UNITS)
        integer :: n_units, ierr, exitcode
        integer :: order(MAX_NODES), n_order
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, i, n_test_names
        real :: t0, t1
        character(len=128) :: test_names(MAX_NODES)

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

        call dag_build(units, n_units, dag)
        call dag_topo_order(dag, order, n_order, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a,i0,a,i0,a)') &
                'Static: warning: ', dag%n - n_order, ' of ', dag%n, &
                ' modules in possible cycle (continuing with build)'
        end if

        ! compute changed modules
        call fo_changed_modules(trim(b%project_dir), dag, changed_ids, n_changed, &
                                affected_ids, n_affected, n_cached, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'Static: FAIL scan or dag error'
            stop 1
        end if

        write (output_unit, '(a,i0,a,i0,a,i0,a)') &
            'Static: OK (', dag%n, ' modules, ', n_changed, &
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
            if (dag%nodes(affected_ids(i))%is_test) then
                if (.not. is_slow_test(dag%nodes(affected_ids(i))%name)) then
                    n_test_names = n_test_names + 1
                    test_names(n_test_names) = dag%nodes(affected_ids(i))%name
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

        call cpu_time(t1)
        write (output_unit, '(a,f0.1,a)') 'Tests: OK (', t1 - t0, 's)'
    end subroutine cmd_run

    subroutine print_usage()
        write (output_unit, '(a)') 'fo - Fortran build driver'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'Run in or below fpm.toml or CMakeLists.txt.'
        write (output_unit, '(a)') 'Scans modules, builds the DAG, caches by hash.'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'usage: fo [command]'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') '  (none)     static -> build -> test (the default)'
        write (output_unit, '(a)') '  build      build only (--flag "-O0")'
        write (output_unit, '(a)') '  test       run tests (--only-changed, --all)'
        write (output_unit, '(a)') '  check      build + test, one-line status'
        write (output_unit, '(a)') '  changed    list changed and affected modules'
        write (output_unit, '(a)') '  graph      module dependency graph'
        write (output_unit, '(a)') '  watch      rebuild on file change (inotify loop)'
        write (output_unit, '(a)') '  clean      clear global cache (~/.cache/fo)'
        write (output_unit, '(a)') '  info       backend, file count, module count'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'integration:'
        write (output_unit, '(a)') '  mcp-server  MCP JSON-RPC on stdin/stdout'
        write (output_unit, '(a)') '  lsp         LSP server (diagnostics on save)'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'fo version    print version'
    end subroutine print_usage

    subroutine cmd_check()
        type(check_result_t) :: res

        call fo_check_run('.', res)

        if (res%build_ok .and. res%tests_ok) then
            write (output_unit, '(a,i0,a,i0,a,i0,a,i0,a,f0.1,a)') &
                'Build: OK (', res%n_modules, ' modules, ', &
                res%n_cached, ' cached, ', res%n_changed, &
                ' changed, ', res%n_affected, &
                ' affected) Tests: pass (', res%elapsed, 's)'
        else if (.not. res%build_ok) then
            write (output_unit, '(a,a)') 'Build: FAIL ', trim(res%error_msg)
            stop 1
        else
            write (output_unit, '(a,i0,a,i0,a,i0,a,a)') &
                'Build: OK (', res%n_cached, ' cached, ', res%n_changed, &
                ' changed, ', res%n_affected, &
                ' affected) Tests: FAIL ', trim(res%error_msg)
            stop 1
        end if
    end subroutine cmd_check

    subroutine cmd_changed()
        use fo_dag, only: MAX_NODES
        type(dag_t) :: dag
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, i, n_tests

        call fo_changed_modules('.', dag, changed_ids, n_changed, &
                                affected_ids, n_affected, n_cached, ierr)
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
                trim(dag%nodes(changed_ids(i))%name), &
                '  ', trim(dag%nodes(changed_ids(i))%filename)
        end do

        write (output_unit, '(a,i0,a)') 'affected (', n_affected, '):'
        do i = 1, n_affected
            write (output_unit, '(a,a,a,a)') '  ', &
                trim(dag%nodes(affected_ids(i))%name), &
                '  ', trim(dag%nodes(affected_ids(i))%filename)
        end do

        n_tests = 0
        do i = 1, n_affected
            if (dag%nodes(affected_ids(i))%is_test) n_tests = n_tests + 1
        end do
        if (n_tests > 0) then
            write (output_unit, '(a,i0,a)') 'affected tests (', n_tests, '):'
            do i = 1, n_affected
                if (dag%nodes(affected_ids(i))%is_test) then
                    write (output_unit, '(a,a,a,a)') '  ', &
                        trim(dag%nodes(affected_ids(i))%name), &
                        '  ', trim(dag%nodes(affected_ids(i))%filename)
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
        use fo_dag, only: MAX_NODES
        type(backend_t) :: b
        type(dag_t) :: dag
        integer :: exitcode
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, i, n_test_names
        logical :: only_changed, include_all
        character(len=256) :: arg
        character(len=128) :: test_names(MAX_NODES)

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
                                    affected_ids, n_affected, n_cached, ierr)
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
                if (dag%nodes(affected_ids(i))%is_test) then
                    n_test_names = n_test_names + 1
                    test_names(n_test_names) = dag%nodes(affected_ids(i))%name
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

        b = detect_backend('.')
        scan_root = '.'
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        call scan_dir(trim(scan_root), units, n_units, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: scan failed'
            stop 1
        end if

        call dag_build(units, n_units, dag)

        do i = 1, dag%n
            if (dag%nodes(i)%n_deps == 0) then
                write (output_unit, '(a)') trim(dag%nodes(i)%name)
            else
                do j = 1, dag%nodes(i)%n_deps
                    write (output_unit, '(a,a,a)') trim(dag%nodes(i)%name), &
                        ' -> ', trim(dag%nodes(dag%nodes(i)%dep_ids(j))%name)
                end do
            end if
        end do
    end subroutine cmd_graph

    subroutine cmd_clean()
        use fo_cache, only: cache_t, cache_init
        type(cache_t) :: c
        integer :: ierr

        call cache_init(c, ierr)
        if (ierr == 0) then
            call execute_command_line('rm -f '//trim(c%dir)//'/index', wait=.true.)
            write (output_unit, '(a,a)') 'cache cleared: ', trim(c%dir)
        end if
    end subroutine cmd_clean

    subroutine cmd_info()
        type(scan_unit_t) :: units(MAX_UNITS)
        type(dag_t) :: dag
        type(backend_t) :: b
        character(len=512) :: scan_root
        integer :: n_units, ierr

        b = detect_backend('.')
        scan_root = '.'
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

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
            call dag_build(units, n_units, dag)
            write (output_unit, '(a,i0)') 'files: ', n_units
            write (output_unit, '(a,i0)') 'modules: ', dag%n
        end if
    end subroutine cmd_info

    subroutine cmd_watch()
        use fo_watch, only: watch_loop
        call watch_loop('.')
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
