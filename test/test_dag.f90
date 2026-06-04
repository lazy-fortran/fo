program test_dag
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t, MAX_NAME
    use fo_dag, only: dag_t, dag_build, dag_topo_order, dag_reverse_deps, MAX_NODES
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_linear_chain()
    call test_diamond()
    call test_reverse_deps()

    write(output_unit, '(a,i0,a,i0,a)') 'dag: ', n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write(error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_linear_chain()
        ! a -> b -> c (a depends on b, b depends on c)
        type(scan_unit_t) :: units(3)
        type(dag_t) :: dag
        integer :: order(MAX_NODES), n_order, ierr

        units(1)%filename = 'a.f90'
        units(1)%module_name = 'a'
        units(1)%n_deps = 1
        units(1)%deps(1) = 'b'

        units(2)%filename = 'b.f90'
        units(2)%module_name = 'b'
        units(2)%n_deps = 1
        units(2)%deps(1) = 'c'

        units(3)%filename = 'c.f90'
        units(3)%module_name = 'c'
        units(3)%n_deps = 0

        call dag_build(units, 3, dag)
        call assert(dag%n == 3, 'linear: 3 nodes')

        call dag_topo_order(dag, order, n_order, ierr)
        call assert(ierr == 0, 'linear: no cycle')
        call assert(n_order == 3, 'linear: 3 in order')

        ! c must come before b, b before a
        call assert(position(order, n_order, dag%find('c')) < &
                    position(order, n_order, dag%find('b')), 'linear: c before b')
        call assert(position(order, n_order, dag%find('b')) < &
                    position(order, n_order, dag%find('a')), 'linear: b before a')
    end subroutine test_linear_chain

    subroutine test_diamond()
        ! d -> b, d -> c, b -> a, c -> a
        type(scan_unit_t) :: units(4)
        type(dag_t) :: dag
        integer :: order(MAX_NODES), n_order, ierr

        units(1)%filename = 'a.f90'
        units(1)%module_name = 'a'
        units(1)%n_deps = 0

        units(2)%filename = 'b.f90'
        units(2)%module_name = 'b'
        units(2)%n_deps = 1
        units(2)%deps(1) = 'a'

        units(3)%filename = 'c.f90'
        units(3)%module_name = 'c'
        units(3)%n_deps = 1
        units(3)%deps(1) = 'a'

        units(4)%filename = 'd.f90'
        units(4)%module_name = 'd'
        units(4)%n_deps = 2
        units(4)%deps(1) = 'b'
        units(4)%deps(2) = 'c'

        call dag_build(units, 4, dag)
        call assert(dag%n == 4, 'diamond: 4 nodes')

        call dag_topo_order(dag, order, n_order, ierr)
        call assert(ierr == 0, 'diamond: no cycle')
        call assert(n_order == 4, 'diamond: all 4 ordered')
        call assert(position(order, n_order, dag%find('a')) < &
                    position(order, n_order, dag%find('d')), 'diamond: a before d')
    end subroutine test_diamond

    subroutine test_reverse_deps()
        ! if c changes, both b and a are affected (a -> b -> c)
        type(scan_unit_t) :: units(3)
        type(dag_t) :: dag
        integer :: changed(1), affected(MAX_NODES), n_affected

        units(1)%filename = 'a.f90'
        units(1)%module_name = 'a'
        units(1)%n_deps = 1
        units(1)%deps(1) = 'b'

        units(2)%filename = 'b.f90'
        units(2)%module_name = 'b'
        units(2)%n_deps = 1
        units(2)%deps(1) = 'c'

        units(3)%filename = 'c.f90'
        units(3)%module_name = 'c'
        units(3)%n_deps = 0

        call dag_build(units, 3, dag)

        changed(1) = dag%find('c')
        call dag_reverse_deps(dag, changed, 1, affected, n_affected)
        call assert(n_affected == 3, 'rdeps: changing c affects all 3')
    end subroutine test_reverse_deps

    function position(order, n, idx) result(pos)
        integer, intent(in) :: order(:), n, idx
        integer :: pos, i

        pos = 0
        do i = 1, n
            if (order(i) == idx) then
                pos = i
                return
            end if
        end do
    end function position

end program test_dag
