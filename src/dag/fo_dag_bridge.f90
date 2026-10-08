module fo_dag_bridge
    use fx_dag, only: dag_t, dag_init, dag_add_node, dag_find_node, &
        dag_add_edge, MAX_NODES
    use fo_scan, only: scan_unit_t, MAX_PATH, scan_provides_name
    implicit none
    private
    public :: build_dag_from_units, dag_find_source_provider

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
        integer :: unit_nodes(n_units)
        character(len=256) :: program_label
        character(len=32) :: suffix

        call dag_init(dag, MAX_NODES)
        if (present(filenames)) filenames = ''
        if (present(is_test_arr)) is_test_arr = .false.
        if (present(is_prog_arr)) is_prog_arr = .false.
        unit_nodes = 0

        do i = 1, n_units
            if (len_trim(units(i)%module_name) > 0) then
                node_id = dag_add_node(dag, trim(units(i)%module_name))
                if (node_id > 0) then
                    unit_nodes(i) = node_id
                    if (present(filenames)) filenames(node_id) = units(i)%filename
                    if (present(is_test_arr)) is_test_arr(node_id) = units(i)%is_test
                end if
            end if
            if (units(i)%is_program) then
                ! PROGRAM names are private to each executable; different
                ! sources may legally declare the same name. Their build
                ! identities must remain distinct, or the later source
                ! replaces the earlier one and leaves an unbuilt public test.
                program_label = units(i)%program_name
                node_id = dag_find_node(dag, trim(program_label))
                if (node_id > 0) then
                    write (suffix, '(i0)') i
                    program_label = trim(program_label)//'#program'//trim(suffix)
                end if
                node_id = dag_add_node(dag, trim(program_label))
                if (node_id > 0) then
                    unit_nodes(i) = node_id
                    if (present(filenames)) filenames(node_id) = units(i)%filename
                    if (present(is_test_arr)) is_test_arr(node_id) = units(i)%is_test
                    if (present(is_prog_arr)) is_prog_arr(node_id) = .true.
                end if
            end if
        end do

        do i = 1, n_units
            node_id = unit_nodes(i)
            if (node_id == 0) cycle

            do j = 1, units(i)%n_deps
                dep_id = dag_find_source_provider(units, n_units, dag, units(i)%deps(j))
                if (dep_id == 0 .or. dep_id == node_id) cycle
                call dag_add_edge(dag, node_id, dep_id)
            end do
        end do
    end subroutine build_dag_from_units

    integer function dag_find_source_provider(units, n_units, dag, name) result(node)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units
        type(dag_t), intent(in) :: dag
        character(len=*), intent(in) :: name
        integer :: i

        node = dag_find_node(dag, trim(name))
        if (node > 0) return
        do i = 1, n_units
            if (.not. scan_provides_name(units(i), name)) cycle
            node = dag_find_node(dag, trim(units(i)%module_name))
            return
        end do
    end function dag_find_source_provider

end module fo_dag_bridge
