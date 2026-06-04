module fo_check
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_scan, only: scan_unit_t, scan_file, scan_dir, MAX_NAME, MAX_UNITS
    use fo_dag, only: dag_t, dag_build, dag_topo_order, dag_reverse_deps, MAX_NODES
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
    use fo_cache, only: cache_t, cache_init, cache_lookup, cache_store, &
        cache_key_for
    implicit none
    private
    public :: check_result_t, fo_check_run, fo_changed_modules

    integer, parameter :: HASH_LEN = 16

    type :: check_result_t
        logical :: build_ok = .false.
        logical :: tests_ok = .false.
        integer :: n_modules = 0
        integer :: n_cached = 0
        integer :: n_changed = 0
        integer :: n_affected = 0
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
        integer :: n_units
        integer :: order(MAX_NODES), n_order
        integer :: i, node_id, j, dep_id, n_dep_keys
        character(len=HASH_LEN) :: keys(MAX_NODES)
        character(len=HASH_LEN) :: dep_keys(64)
        character(len=256) :: compiler

        ierr = 0
        n_changed = 0
        n_affected = 0
        n_cached = 0

        call scan_dir(dir, units, n_units, ierr)
        if (ierr /= 0) return

        call dag_build(units, n_units, dag)
        call dag_topo_order(dag, order, n_order, ierr)
        if (ierr /= 0) return

        call detect_compiler(compiler)
        call cache_init(c, ierr)
        if (ierr /= 0) return

        keys = ''
        do i = 1, n_order
            node_id = order(i)

            n_dep_keys = 0
            do j = 1, dag%nodes(node_id)%n_deps
                dep_id = dag%nodes(node_id)%dep_ids(j)
                if (dep_id > 0 .and. len_trim(keys(dep_id)) > 0) then
                    n_dep_keys = n_dep_keys + 1
                    dep_keys(n_dep_keys) = keys(dep_id)
                end if
            end do

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

        ! compute reverse-dependency closure of changed modules
        if (n_changed > 0) then
            call dag_reverse_deps(dag, changed_ids, n_changed, &
                affected_ids, n_affected)
        end if

        ! update cache after analysis (caller decides whether to build)
        do i = 1, n_order
            node_id = order(i)
            call cache_store(c, dag%nodes(node_id)%name, keys(node_id))
        end do
    end subroutine fo_changed_modules

    subroutine fo_check_run(dir, res)
        character(len=*), intent(in) :: dir
        type(check_result_t), intent(out) :: res

        type(dag_t) :: dag
        type(backend_t) :: backend
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, exitcode
        real :: t0, t1

        call cpu_time(t0)

        backend = detect_backend(dir)
        if (backend%kind == BACKEND_NONE) then
            res%error_msg = 'no fpm.toml or CMakeLists.txt in '//trim(dir)
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        call fo_changed_modules(dir, dag, changed_ids, n_changed, &
            affected_ids, n_affected, n_cached, ierr)
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

        ! build
        call backend%build(exitcode)
        if (exitcode /= 0) then
            res%error_msg = 'build failed'
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if
        res%build_ok = .true.

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
