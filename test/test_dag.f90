program test_dag
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t
    use fx_dag, only: dag_t, dag_find_node, dag_topo_sort, dag_affected_set, &
                      MAX_NODES
    use fo_dag_bridge, only: build_dag_from_units
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_linear_chain()
    call test_diamond()
    call test_reverse_deps()
    call test_affected_tests()

    write (output_unit, '(a,i0,a,i0,a)') 'dag: ', n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_linear_chain()
        ! a -> b -> c (a depends on b, b depends on c)
        type(scan_unit_t) :: units(3)
        type(dag_t) :: dag
        integer :: order(MAX_NODES), n_order
        logical :: has_cycle

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

        call build_dag_from_units(units, 3, dag)
        call assert(dag%n_nodes == 3, 'linear: 3 nodes')

        call dag_topo_sort(dag, order, n_order, has_cycle)
        call assert(.not. has_cycle, 'linear: no cycle')
        call assert(n_order == 3, 'linear: 3 in order')

        ! c must come before b, b before a
        call assert(position(order, n_order, dag_find_node(dag, 'c')) < &
                position(order, n_order, dag_find_node(dag, 'b')), 'linear: c before b')
        call assert(position(order, n_order, dag_find_node(dag, 'b')) < &
                position(order, n_order, dag_find_node(dag, 'a')), 'linear: b before a')
    end subroutine test_linear_chain

    subroutine test_diamond()
        ! d -> b, d -> c, b -> a, c -> a
        type(scan_unit_t) :: units(4)
        type(dag_t) :: dag
        integer :: order(MAX_NODES), n_order
        logical :: has_cycle

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

        call build_dag_from_units(units, 4, dag)
        call assert(dag%n_nodes == 4, 'diamond: 4 nodes')

        call dag_topo_sort(dag, order, n_order, has_cycle)
        call assert(.not. has_cycle, 'diamond: no cycle')
        call assert(n_order == 4, 'diamond: all 4 ordered')
        call assert(position(order, n_order, dag_find_node(dag, 'a')) < &
               position(order, n_order, dag_find_node(dag, 'd')), 'diamond: a before d')
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

        call build_dag_from_units(units, 3, dag)

        changed(1) = dag_find_node(dag, 'c')
        call dag_affected_set(dag, changed, 1, affected, n_affected)
        call assert(n_affected == 3, 'rdeps: changing c affects all 3')
    end subroutine test_reverse_deps

    subroutine test_affected_tests()
        ! lib_a (module), test_a (test program, depends on lib_a),
        ! test_b (test program, no deps), lib_b (module, no rdeps)
        ! changing lib_a should affect lib_a + test_a but not test_b or lib_b
        type(scan_unit_t) :: units(4)
        type(dag_t) :: dag
        integer :: changed(1), affected(MAX_NODES), n_affected
        integer :: i, n_test_affected
        logical :: is_test_arr(MAX_NODES)

        units(1)%filename = 'src/lib_a.f90'
        units(1)%module_name = 'lib_a'
        units(1)%n_deps = 0
        units(1)%is_test = .false.

        units(2)%filename = 'test/test_a.f90'
        units(2)%program_name = 'test_a'
        units(2)%is_program = .true.
        units(2)%is_test = .true.
        units(2)%n_deps = 1
        units(2)%deps(1) = 'lib_a'

        units(3)%filename = 'test/test_b.f90'
        units(3)%program_name = 'test_b'
        units(3)%is_program = .true.
        units(3)%is_test = .true.
        units(3)%n_deps = 0

        units(4)%filename = 'src/lib_b.f90'
        units(4)%module_name = 'lib_b'
        units(4)%n_deps = 0
        units(4)%is_test = .false.

        call build_dag_from_units(units, 4, dag, is_test_arr=is_test_arr)
        call assert(dag%n_nodes == 4, 'affected_tests: 4 nodes')

        ! change lib_a
        changed(1) = dag_find_node(dag, 'lib_a')
        call dag_affected_set(dag, changed, 1, affected, n_affected)

        call assert(n_affected == 2, 'affected_tests: 2 affected (lib_a + test_a)')

        ! count how many affected are tests
        n_test_affected = 0
        do i = 1, n_affected
            if (is_test_arr(affected(i))) n_test_affected = n_test_affected + 1
        end do
        call assert(n_test_affected == 1, 'affected_tests: 1 affected test')

        ! verify test_b is not affected
        do i = 1, n_affected
            call assert(trim(dag%nodes(affected(i))%label) /= 'test_b', &
                        'affected_tests: test_b not affected')
        end do
    end subroutine test_affected_tests

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
