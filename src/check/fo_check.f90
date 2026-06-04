module fo_check
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_scan, only: scan_unit_t, scan_file, scan_dir, MAX_NAME, MAX_UNITS
    use fo_dag, only: dag_t, dag_build, dag_topo_order, dag_reverse_deps, MAX_NODES
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
    use fo_cache, only: cache_t, cache_init, cache_lookup, cache_store, &
        cache_key_for
    implicit none
    private
    public :: check_result_t, fo_check_run

    integer, parameter :: HASH_LEN = 16

    type :: check_result_t
        logical :: build_ok = .false.
        logical :: tests_ok = .false.
        integer :: n_modules = 0
        integer :: n_cached = 0
        integer :: n_changed = 0
        real :: elapsed = 0.0
        character(len=512) :: error_msg = ''
    end type check_result_t

contains

    subroutine fo_check_run(dir, res)
        character(len=*), intent(in) :: dir
        type(check_result_t), intent(out) :: res

        type(scan_unit_t) :: units(MAX_UNITS)
        type(dag_t) :: dag
        type(backend_t) :: backend
        type(cache_t) :: c
        integer :: n_units, ierr, exitcode
        integer :: order(MAX_NODES), n_order
        integer :: i, node_id
        character(len=HASH_LEN) :: keys(MAX_NODES)
        character(len=HASH_LEN) :: dep_keys(64)
        integer :: n_dep_keys, j, dep_id
        character(len=256) :: compiler, flags
        logical :: any_changed
        real :: t0, t1

        call cpu_time(t0)

        backend = detect_backend(dir)
        if (backend%kind == BACKEND_NONE) then
            res%error_msg = 'no fpm.toml or CMakeLists.txt in '//trim(dir)
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        call scan_dir(dir, units, n_units, ierr)
        if (ierr /= 0) then
            res%error_msg = 'scan failed: '//trim(dir)
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        call dag_build(units, n_units, dag)
        res%n_modules = dag%n

        call dag_topo_order(dag, order, n_order, ierr)
        if (ierr /= 0) then
            res%error_msg = 'cycle in module dependency graph'
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        ! detect compiler
        call detect_compiler(compiler)
        flags = ''

        ! init global cache
        call cache_init(c, ierr)

        ! compute cache keys in topo order (deps before dependents)
        keys = ''
        any_changed = .false.
        do i = 1, n_order
            node_id = order(i)

            ! collect dep keys
            n_dep_keys = 0
            do j = 1, dag%nodes(node_id)%n_deps
                dep_id = dag%nodes(node_id)%dep_ids(j)
                if (dep_id > 0 .and. len_trim(keys(dep_id)) > 0) then
                    n_dep_keys = n_dep_keys + 1
                    dep_keys(n_dep_keys) = keys(dep_id)
                end if
            end do

            keys(node_id) = cache_key_for( &
                dag%nodes(node_id)%filename, compiler, flags, &
                dag, dep_keys, n_dep_keys)

            if (cache_lookup(c, dag%nodes(node_id)%name, keys(node_id))) then
                res%n_cached = res%n_cached + 1
            else
                res%n_changed = res%n_changed + 1
                any_changed = .true.
            end if
        end do

        ! build (always, since fpm/cmake have their own incremental logic)
        call backend%build(exitcode)
        if (exitcode /= 0) then
            res%error_msg = 'build failed'
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if
        res%build_ok = .true.

        ! update cache for all modules after successful build
        do i = 1, n_order
            node_id = order(i)
            call cache_store(c, dag%nodes(node_id)%name, keys(node_id))
        end do

        ! test
        call backend%test(exitcode)
        res%tests_ok = (exitcode == 0)
        if (.not. res%tests_ok) then
            res%error_msg = 'tests failed'
        end if

        call cpu_time(t1)
        res%elapsed = t1 - t0
    end subroutine fo_check_run

    subroutine detect_compiler(compiler)
        character(len=*), intent(out) :: compiler

        character(len=256) :: line
        character(len=512) :: tmpfile, cmd
        integer :: u, iostat

        compiler = 'unknown'
        tmpfile = '/tmp/fo_compiler_version.tmp'
        cmd = 'gfortran --version 2>/dev/null | head -1 > '//trim(tmpfile)
        call execute_command_line(cmd, wait=.true.)

        open(newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat == 0) then
            read(u, '(a)', iostat=iostat) line
            if (iostat == 0) compiler = trim(line)
            close(u)
        end if
        call execute_command_line('rm -f '//trim(tmpfile), wait=.true.)
    end subroutine detect_compiler

end module fo_check
