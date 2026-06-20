module fo_dag_bridge
    use fx_dag, only: dag_t, dag_init, dag_add_node, dag_find_node, &
        dag_add_edge, MAX_NODES
    use fo_scan, only: scan_unit_t, MAX_PATH
    implicit none
    private
    public :: build_dag_from_units

contains

    subroutine build_dag_from_units(units, n_units, dag, filenames, &
            is_test_arr, is_prog_arr)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units
        type(dag_t), intent(out) :: dag
        character(len=MAX_PATH), optional, intent(out) :: filenames(MAX_NODES)
        logical, optional, intent(out) :: is_test_arr(MAX_NODES)
        logical, optional, intent(out) :: is_prog_arr(MAX_NODES)

        integer :: i, j, node_id, dep_id

        call dag_init(dag, MAX_NODES)
        if (present(filenames)) filenames = ''
        if (present(is_test_arr)) is_test_arr = .false.
        if (present(is_prog_arr)) is_prog_arr = .false.

        do i = 1, n_units
            if (len_trim(units(i)%module_name) > 0) then
                node_id = dag_add_node(dag, trim(units(i)%module_name))
                if (node_id > 0) then
                    if (present(filenames)) filenames(node_id) = units(i)%filename
                    if (present(is_test_arr)) is_test_arr(node_id) = units(i)%is_test
                end if
            end if
            if (units(i)%is_program) then
                node_id = dag_add_node(dag, trim(units(i)%program_name))
                if (node_id > 0) then
                    if (present(filenames)) filenames(node_id) = units(i)%filename
                    if (present(is_test_arr)) is_test_arr(node_id) = units(i)%is_test
                    if (present(is_prog_arr)) is_prog_arr(node_id) = .true.
                end if
            end if
        end do

        do i = 1, n_units
            if (len_trim(units(i)%module_name) > 0) then
                node_id = dag_find_node(dag, trim(units(i)%module_name))
            else if (units(i)%is_program) then
                node_id = dag_find_node(dag, trim(units(i)%program_name))
            else
                cycle
            end if
            if (node_id == 0) cycle

            do j = 1, units(i)%n_deps
                dep_id = dag_find_node(dag, trim(units(i)%deps(j)))
                if (dep_id == 0 .or. dep_id == node_id) cycle
                call dag_add_edge(dag, node_id, dep_id)
            end do
        end do
    end subroutine build_dag_from_units

end module fo_dag_bridge
