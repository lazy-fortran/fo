module fo_check
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_scan, only: scan_unit_t, scan_dir, MAX_NAME, MAX_UNITS
    use fo_dag, only: dag_t, dag_build, dag_topo_order, dag_reverse_deps, MAX_NODES
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
    implicit none
    private
    public :: check_result_t, fo_check_run

    type :: check_result_t
        logical :: build_ok = .false.
        logical :: tests_ok = .false.
        integer :: n_modules = 0
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
        integer :: n_units, ierr, exitcode
        integer :: order(MAX_NODES), n_order
        real :: t0, t1

        call cpu_time(t0)

        ! detect build system
        backend = detect_backend(dir)
        if (backend%kind == BACKEND_NONE) then
            res%error_msg = 'no fpm.toml or CMakeLists.txt in '//trim(dir)
            call cpu_time(t1)
            res%elapsed = t1 - t0
            return
        end if

        ! scan module graph
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

end module fo_check
