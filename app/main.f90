program fo_main
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS
    use fo_dag, only: dag_t, dag_build, dag_topo_order, MAX_NODES
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE, &
        BACKEND_FPM, BACKEND_CMAKE
    use fo_check, only: check_result_t, fo_check_run, fo_changed_modules
    implicit none

    character(len=256) :: action
    integer :: nargs

    nargs = command_argument_count()
    if (nargs == 0) then
        call print_usage()
        stop 1
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
    case ('version', '--version')
        write(output_unit, '(a)') 'fo 0.1.0'
    case ('help', '--help', '-h')
        call print_usage()
    case default
        write(error_unit, '(a)') 'fo: unknown command: '//trim(action)
        call print_usage()
        stop 1
    end select

contains

    subroutine print_usage()
        write(output_unit, '(a)') 'fo - Fortran build driver'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') 'usage: fo <command>'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '  check    build + test, compact delta'
        write(output_unit, '(a)') '  changed  list changed and affected modules'
        write(output_unit, '(a)') '  build    build only'
        write(output_unit, '(a)') '  test     run tests only'
        write(output_unit, '(a)') '  graph    module dependency graph'
        write(output_unit, '(a)') '  info     detected backend and module count'
        write(output_unit, '(a)') '  clean    clear global build cache'
        write(output_unit, '(a)') '  version  print version'
    end subroutine print_usage

    subroutine cmd_check()
        type(check_result_t) :: res

        call fo_check_run('.', res)

        if (res%build_ok .and. res%tests_ok) then
            write(output_unit, '(a,i0,a,i0,a,i0,a,i0,a,f0.1,a)') &
                'Build: OK (', res%n_modules, ' modules, ', &
                res%n_cached, ' cached, ', res%n_changed, &
                ' changed, ', res%n_affected, &
                ' affected) Tests: pass (', res%elapsed, 's)'
        else if (.not. res%build_ok) then
            write(output_unit, '(a,a)') 'Build: FAIL ', trim(res%error_msg)
            stop 1
        else
            write(output_unit, '(a,i0,a,i0,a,i0,a,a)') &
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
            write(error_unit, '(a)') 'fo: scan or dag failed'
            stop 1
        end if

        if (n_changed == 0) then
            write(output_unit, '(a,i0,a)') 'all ', n_cached, ' modules cached'
            return
        end if

        write(output_unit, '(a,i0,a)') 'changed (', n_changed, '):'
        do i = 1, n_changed
            write(output_unit, '(a,a,a,a)') '  ', &
                trim(dag%nodes(changed_ids(i))%name), &
                '  ', trim(dag%nodes(changed_ids(i))%filename)
        end do

        write(output_unit, '(a,i0,a)') 'affected (', n_affected, '):'
        do i = 1, n_affected
            write(output_unit, '(a,a,a,a)') '  ', &
                trim(dag%nodes(affected_ids(i))%name), &
                '  ', trim(dag%nodes(affected_ids(i))%filename)
        end do

        n_tests = 0
        do i = 1, n_affected
            if (dag%nodes(affected_ids(i))%is_test) n_tests = n_tests + 1
        end do
        if (n_tests > 0) then
            write(output_unit, '(a,i0,a)') 'affected tests (', n_tests, '):'
            do i = 1, n_affected
                if (dag%nodes(affected_ids(i))%is_test) then
                    write(output_unit, '(a,a,a,a)') '  ', &
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
            write(error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
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
        integer :: exitcode

        b = detect_backend('.')
        if (b%kind == BACKEND_NONE) then
            write(error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            stop 1
        end if
        call b%test(exitcode)
        if (exitcode /= 0) stop 1
    end subroutine cmd_test

    subroutine cmd_graph()
        type(scan_unit_t) :: units(MAX_UNITS)
        type(dag_t) :: dag
        integer :: n_units, ierr, i, j

        call scan_dir('.', units, n_units, ierr)
        if (ierr /= 0) then
            write(error_unit, '(a)') 'fo: scan failed'
            stop 1
        end if

        call dag_build(units, n_units, dag)

        do i = 1, dag%n
            if (dag%nodes(i)%n_deps == 0) then
                write(output_unit, '(a)') trim(dag%nodes(i)%name)
            else
                do j = 1, dag%nodes(i)%n_deps
                    write(output_unit, '(a,a,a)') trim(dag%nodes(i)%name), &
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
            write(output_unit, '(a,a)') 'cache cleared: ', trim(c%dir)
        end if
    end subroutine cmd_clean

    subroutine cmd_info()
        type(scan_unit_t) :: units(MAX_UNITS)
        type(dag_t) :: dag
        type(backend_t) :: b
        integer :: n_units, ierr

        b = detect_backend('.')

        select case (b%kind)
        case (BACKEND_FPM)
            write(output_unit, '(a)') 'backend: fpm'
        case (BACKEND_CMAKE)
            write(output_unit, '(a)') 'backend: cmake'
        case default
            write(output_unit, '(a)') 'backend: none'
        end select

        call scan_dir('.', units, n_units, ierr)
        if (ierr == 0) then
            call dag_build(units, n_units, dag)
            write(output_unit, '(a,i0)') 'files: ', n_units
            write(output_unit, '(a,i0)') 'modules: ', dag%n
        end if
    end subroutine cmd_info

end program fo_main
