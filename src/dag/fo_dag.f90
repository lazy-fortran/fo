module fo_dag
    use fo_scan, only: scan_unit_t, MAX_NAME
    implicit none
    private
    public :: dag_t, dag_build, dag_topo_order, dag_reverse_deps, MAX_NODES

    integer, parameter :: MAX_NODES = 2048
    integer, parameter :: MAX_EDGES = 64

    type :: dag_node_t
        character(len=MAX_NAME) :: name = ''
        character(len=MAX_NAME) :: filename = ''
        logical :: is_program = .false.
        logical :: is_test = .false.
        integer :: n_deps = 0
        integer :: dep_ids(MAX_EDGES)
        integer :: n_rdeps = 0
        integer :: rdep_ids(MAX_EDGES)
    end type dag_node_t

    type :: dag_t
        integer :: n = 0
        type(dag_node_t) :: nodes(MAX_NODES)
    contains
        procedure :: find => dag_find
    end type dag_t

contains

    subroutine dag_build(units, n_units, dag)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units
        type(dag_t), intent(out) :: dag

        integer :: i, j, dep_id

        dag%n = 0

        ! register all modules and programs as nodes
        do i = 1, n_units
            if (len_trim(units(i)%module_name) > 0) then
                call add_node(dag, units(i)%module_name, units(i)%filename, &
                    .false., units(i)%is_test)
            end if
            if (units(i)%is_program) then
                call add_node(dag, units(i)%program_name, units(i)%filename, &
                    .true., units(i)%is_test)
            end if
        end do

        ! wire edges from units to their dependencies
        do i = 1, n_units
            j = find_unit_node(dag, units(i))
            if (j == 0) cycle

            do dep_id = 1, units(i)%n_deps
                call add_edge(dag, j, trim(units(i)%deps(dep_id)))
            end do
        end do
    end subroutine dag_build

    function dag_find(self, name) result(idx)
        class(dag_t), intent(in) :: self
        character(len=*), intent(in) :: name
        integer :: idx, i

        idx = 0
        do i = 1, self%n
            if (trim(self%nodes(i)%name) == trim(name)) then
                idx = i
                return
            end if
        end do
    end function dag_find

    subroutine dag_topo_order(dag, order, n_order, ierr)
        type(dag_t), intent(in) :: dag
        integer, intent(out) :: order(MAX_NODES)
        integer, intent(out) :: n_order, ierr

        integer :: n_unsatisfied(MAX_NODES)
        integer :: queue(MAX_NODES)
        integer :: qhead, qtail, i, j, rdep

        ierr = 0
        n_order = 0

        ! n_unsatisfied(i) = number of i's dependencies not yet processed
        do i = 1, dag%n
            n_unsatisfied(i) = dag%nodes(i)%n_deps
        end do

        ! seed with nodes that have no dependencies (leaves)
        qhead = 1
        qtail = 0
        do i = 1, dag%n
            if (n_unsatisfied(i) == 0) then
                qtail = qtail + 1
                queue(qtail) = i
            end if
        end do

        do while (qhead <= qtail)
            i = queue(qhead)
            qhead = qhead + 1
            n_order = n_order + 1
            order(n_order) = i

            ! processing i satisfies one dependency for each of its rdeps
            do j = 1, dag%nodes(i)%n_rdeps
                rdep = dag%nodes(i)%rdep_ids(j)
                if (rdep > 0) then
                    n_unsatisfied(rdep) = n_unsatisfied(rdep) - 1
                    if (n_unsatisfied(rdep) == 0) then
                        qtail = qtail + 1
                        queue(qtail) = rdep
                    end if
                end if
            end do
        end do

        if (n_order /= dag%n) ierr = 1
    end subroutine dag_topo_order

    subroutine dag_reverse_deps(dag, changed_ids, n_changed, affected, n_affected)
        type(dag_t), intent(in) :: dag
        integer, intent(in) :: changed_ids(:)
        integer, intent(in) :: n_changed
        integer, intent(out) :: affected(MAX_NODES)
        integer, intent(out) :: n_affected

        logical :: visited(MAX_NODES)
        integer :: i

        visited = .false.
        n_affected = 0

        do i = 1, n_changed
            if (changed_ids(i) > 0 .and. changed_ids(i) <= dag%n) then
                call walk_rdeps(dag, changed_ids(i), visited, affected, n_affected)
            end if
        end do
    end subroutine dag_reverse_deps

    ! --- private helpers ---

    subroutine add_node(dag, name, filename, is_prog, is_test)
        type(dag_t), intent(inout) :: dag
        character(len=*), intent(in) :: name, filename
        logical, intent(in) :: is_prog, is_test

        integer :: idx

        idx = dag%find(name)
        if (idx > 0) return

        dag%n = dag%n + 1
        dag%nodes(dag%n)%name = name
        dag%nodes(dag%n)%filename = filename
        dag%nodes(dag%n)%is_program = is_prog
        dag%nodes(dag%n)%is_test = is_test
    end subroutine add_node

    subroutine add_edge(dag, from_id, dep_name)
        type(dag_t), intent(inout) :: dag
        integer, intent(in) :: from_id
        character(len=*), intent(in) :: dep_name

        integer :: dep_id

        dep_id = dag%find(dep_name)
        if (dep_id == 0) return
        if (dep_id == from_id) return

        ! forward edge: from_id depends on dep_id
        if (dag%nodes(from_id)%n_deps < MAX_EDGES) then
            dag%nodes(from_id)%n_deps = dag%nodes(from_id)%n_deps + 1
            dag%nodes(from_id)%dep_ids(dag%nodes(from_id)%n_deps) = dep_id
        end if

        ! reverse edge: dep_id is depended on by from_id
        if (dag%nodes(dep_id)%n_rdeps < MAX_EDGES) then
            dag%nodes(dep_id)%n_rdeps = dag%nodes(dep_id)%n_rdeps + 1
            dag%nodes(dep_id)%rdep_ids(dag%nodes(dep_id)%n_rdeps) = from_id
        end if
    end subroutine add_edge

    function find_unit_node(dag, unit) result(idx)
        type(dag_t), intent(in) :: dag
        type(scan_unit_t), intent(in) :: unit
        integer :: idx

        if (len_trim(unit%module_name) > 0) then
            idx = dag%find(unit%module_name)
        else if (unit%is_program) then
            idx = dag%find(unit%program_name)
        else
            idx = 0
        end if
    end function find_unit_node

    recursive subroutine walk_rdeps(dag, node_id, visited, affected, n_affected)
        type(dag_t), intent(in) :: dag
        integer, intent(in) :: node_id
        logical, intent(inout) :: visited(MAX_NODES)
        integer, intent(inout) :: affected(MAX_NODES)
        integer, intent(inout) :: n_affected

        integer :: i, rdep

        if (visited(node_id)) return
        visited(node_id) = .true.
        n_affected = n_affected + 1
        affected(n_affected) = node_id

        do i = 1, dag%nodes(node_id)%n_rdeps
            rdep = dag%nodes(node_id)%rdep_ids(i)
            if (rdep > 0) call walk_rdeps(dag, rdep, visited, affected, n_affected)
        end do
    end subroutine walk_rdeps

end module fo_dag
