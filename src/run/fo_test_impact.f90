module fo_test_impact
    !! Select affected public test cases from canonical execution input changes.
    use fo_input_inventory, only: input_inventory_t, input_entry_t
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_scan, only: MAX_PATH
    use fx_dag, only: dag_t, dag_affected_set, MAX_NODES
    implicit none
    private

    integer, parameter, public :: IMPACT_AFFECTED = 1
    integer, parameter, public :: IMPACT_WIDENED = 2

    type, public :: test_impact_case_t
        character(len=128) :: public_name = ''
        character(len=512) :: identity = ''
        integer :: node_id = 0
        logical :: eligible = .true.
        logical :: slow = .false.
        logical :: dependency_complete = .true.
    end type test_impact_case_t

    type, public :: test_impact_selected_t
        character(len=128) :: public_name = ''
        character(len=512) :: identity = ''
        character(len=256) :: reason = ''
        integer :: classification = 0
        logical :: slow = .false.
    end type test_impact_selected_t

    type, public :: test_impact_result_t
        type(test_impact_selected_t), allocatable :: required(:)
        character(len=256), allocatable :: widening_reasons(:)
        integer :: required_count = 0
        integer :: widening_count = 0
        logical :: complete = .false.
        logical :: model_complete = .false.
        logical :: widened = .false.
        character(len=HASH_LEN) :: model_identity = ''
    end type test_impact_result_t

    public :: test_impact_select

contains

    subroutine test_impact_select(baseline, candidate, dag, filenames, cases, &
            dependency_model_complete, result, ierr, message)
        type(input_inventory_t), intent(in) :: baseline, candidate
        type(dag_t), intent(in) :: dag
        character(len=MAX_PATH), intent(in) :: filenames(:)
        type(test_impact_case_t), intent(in) :: cases(:)
        logical, intent(in) :: dependency_model_complete
        type(test_impact_result_t), intent(out) :: result
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        integer, allocatable :: changed(:), affected(:)
        logical, allocatable :: selected(:)
        character(len=256), allocatable :: reasons(:)
        integer :: n_changed, n_affected, i, j, node_id, n_reason
        logical :: widen, model_complete

        result = test_impact_result_t()
        ierr = 1
        message = ''
        n_reason = 0
        allocate(reasons(8))
        reasons = ''
        allocate(changed(MAX_NODES), affected(MAX_NODES))
        allocate(selected(size(cases)))
        changed = 0
        affected = 0
        selected = .false.
        n_changed = 0
        n_affected = 0
        widen = .false.
        model_complete = .true.

        if (size(filenames) < dag%n_nodes) then
            message = 'DAG filename map is shorter than the dependency graph'
            return
        end if
        if (size(cases) == 0) then
            message = 'eligible case catalog is empty'
            return
        end if
        if (.not. baseline%complete) then
            call add_reason('baseline inventory is incomplete')
            model_complete = .false.
        end if
        if (.not. candidate%complete) then
            call add_reason('candidate inventory is incomplete')
            model_complete = .false.
        end if
        if (.not. dependency_model_complete) then
            call add_reason('unsupported or incomplete dependency edges')
            model_complete = .false.
        end if
        do i = 1, size(cases)
            if (.not. cases(i)%eligible) cycle
            if (len_trim(cases(i)%public_name) == 0 .or. &
                    len_trim(cases(i)%identity) == 0) then
                message = 'eligible case is missing its public or logical identity'
                return
            end if
            if (cases(i)%node_id < 1 .or. cases(i)%node_id > dag%n_nodes) then
                call add_reason('eligible case is not represented in dependency DAG')
            end if
            if (.not. cases(i)%dependency_complete) then
                call add_reason('case dependency closure is incomplete')
                model_complete = .false.
                exit
            end if
        end do

        call compare_inventories()
        if (widen) then
            do i = 1, size(cases)
                selected(i) = cases(i)%eligible
            end do
        else if (n_changed > 0) then
            call dag_affected_set(dag, changed(:n_changed), n_changed, &
                affected, n_affected)
            do i = 1, size(cases)
                if (.not. cases(i)%eligible) cycle
                do j = 1, n_affected
                    if (cases(i)%node_id == affected(j)) then
                        selected(i) = .true.
                        exit
                    end if
                end do
            end do
        end if

        allocate(result%required(count(selected)))
        result%required_count = 0
        do i = 1, size(cases)
            if (.not. selected(i)) cycle
            result%required_count = result%required_count + 1
            result%required(result%required_count)%public_name = &
                cases(i)%public_name
            result%required(result%required_count)%identity = cases(i)%identity
            result%required(result%required_count)%slow = cases(i)%slow
            if (widen) then
                result%required(result%required_count)%classification = &
                    IMPACT_WIDENED
                result%required(result%required_count)%reason = &
                    'conservative widening'
            else
                result%required(result%required_count)%classification = &
                    IMPACT_AFFECTED
                result%required(result%required_count)%reason = &
                    'changed represented dependency closure'
            end if
        end do
        allocate(result%widening_reasons(n_reason))
        if (n_reason > 0) result%widening_reasons = reasons(:n_reason)
        result%widening_count = n_reason
        result%widened = widen
        result%complete = .true.
        result%model_complete = model_complete
        call compute_model_identity()
        ierr = 0
        message = ''

    contains

        subroutine add_reason(reason)
            character(len=*), intent(in) :: reason
            if (n_reason < size(reasons)) then
                n_reason = n_reason + 1
                reasons(n_reason) = reason
            end if
            widen = .true.
        end subroutine add_reason

        subroutine compare_inventories()
            integer :: a, b, matches
            logical :: found

            if (.not. baseline%complete .or. .not. candidate%complete) return
            do a = 1, baseline%entry_count
                matches = 0
                b = 0
                do j = 1, candidate%entry_count
                    if (.not. same_path(baseline%entries(a), &
                            candidate%entries(j))) cycle
                    matches = matches + 1
                    b = j
                end do
                if (matches /= 1) then
                    call add_reason('input removed, renamed, or ambiguous')
                    cycle
                end if
                if (entry_equal(baseline%entries(a), candidate%entries(b))) cycle
                if (is_global_role(baseline%entries(a)%role) .or. &
                        is_global_role(candidate%entries(b)%role)) then
                    call add_reason('shared or global input changed')
                    cycle
                end if
                call changed_node(candidate, candidate%entries(b), node_id, found)
                if (.not. found) then
                    call add_reason('changed input is not represented in DAG')
                    model_complete = .false.
                    cycle
                end if
                call append_changed(node_id)
            end do
            do b = 1, candidate%entry_count
                found = .false.
                do a = 1, baseline%entry_count
                    if (same_path(baseline%entries(a), candidate%entries(b))) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) call add_reason('new input added')
            end do
        end subroutine compare_inventories

        subroutine changed_node(inventory, entry, id, found)
            type(input_inventory_t), intent(in) :: inventory
            type(input_entry_t), intent(in) :: entry
            integer, intent(out) :: id
            logical, intent(out) :: found
            character(len=4096) :: abs_path
            integer :: root, k

            id = 0
            found = .false.
            abs_path = ''
            do root = 1, inventory%root_count
                if (trim(inventory%roots(root)%canonical_alias) /= &
                        trim(entry%root_alias)) cycle
                abs_path = trim(inventory%roots(root)%physical_path)//'/'// &
                    trim(entry%relative_path)
                exit
            end do
            if (len_trim(abs_path) == 0) return
            do k = 1, min(dag%n_nodes, size(filenames))
                if (trim(filenames(k)) /= trim(abs_path)) cycle
                if (found) then
                    found = .false.
                    id = 0
                    return
                end if
                found = .true.
                id = k
            end do
        end subroutine changed_node

        subroutine append_changed(id)
            integer, intent(in) :: id
            integer :: k
            do k = 1, n_changed
                if (changed(k) == id) return
            end do
            if (id < 1 .or. id > dag%n_nodes) then
                call add_reason('changed input maps outside dependency graph')
                model_complete = .false.
                return
            end if
            if (n_changed < size(changed)) then
                n_changed = n_changed + 1
                changed(n_changed) = id
            else
                call add_reason('changed dependency graph exceeds selector capacity')
                model_complete = .false.
            end if
        end subroutine append_changed

        subroutine compute_model_identity()
            character(len=1024), allocatable :: parts(:)
            integer :: k, e, n

            n = 2 + dag%n_nodes + size(cases) + result%required_count
            allocate(parts(n))
            parts = ''
            parts(1) = 'fo-test-impact-v1:'//baseline%digest
            parts(2) = candidate%digest
            e = 2
            do k = 1, dag%n_nodes
                e = e + 1
                parts(e) = 'node:'//trim(dag%nodes(k)%label)
                do j = 1, dag%nodes(k)%n_edges
                    parts(e) = trim(parts(e))//'>'// &
                        trim(dag%nodes(dag%nodes(k)%edges(j))%label)
                end do
            end do
            do k = 1, size(cases)
                e = e + 1
                parts(e) = 'case:'//trim(cases(k)%identity)//':'// &
                    trim(cases(k)%public_name)//':'// &
                    logical_text(cases(k)%eligible)//':'// &
                    logical_text(cases(k)%slow)//':'// &
                    logical_text(cases(k)%dependency_complete)//':'// &
                    integer_text(cases(k)%node_id)
            end do
            do k = 1, result%required_count
                e = e + 1
                parts(e) = 'required:'//trim(result%required(k)%identity)//':'// &
                    trim(result%required(k)%reason)
            end do
            result%model_identity = cache_digest(parts, size(parts))
        end subroutine compute_model_identity

        function integer_text(value) result(text)
            integer, intent(in) :: value
            character(len=16) :: text
            write (text, '(i0)') value
        end function integer_text

    end subroutine test_impact_select

    logical function same_path(left, right)
        type(input_entry_t), intent(in) :: left, right
        same_path = trim(left%root_alias) == trim(right%root_alias) .and. &
            trim(left%relative_path) == trim(right%relative_path) .and. &
            trim(left%role) == trim(right%role)
    end function same_path

    logical function entry_equal(left, right)
        type(input_entry_t), intent(in) :: left, right
        entry_equal = trim(left%role) == trim(right%role) .and. &
            left%kind == right%kind .and. left%mode == right%mode .and. &
            (left%writable_at_execution .eqv. right%writable_at_execution)
        if (.not. entry_equal) return
        entry_equal = trim(left%link_target) == trim(right%link_target) .and. &
            trim(left%content_digest) == trim(right%content_digest)
    end function entry_equal

    logical function is_global_role(role)
        character(len=*), intent(in) :: role
        character(len=:), allocatable :: normalized
        normalized = trim(adjustl(role))
        is_global_role = index(normalized, 'manifest') > 0 .or. &
            index(normalized, 'include') > 0 .or. &
            index(normalized, 'harness') > 0 .or. &
            index(normalized, 'runtime') > 0 .or. &
            index(normalized, 'toolchain') > 0 .or. &
            index(normalized, 'environment') > 0 .or. &
            index(normalized, 'test-support') > 0 .or. &
            index(normalized, 'dependency-lock') > 0
    end function is_global_role

    function logical_text(value) result(text)
        logical, intent(in) :: value
        character(len=1) :: text
        text = '0'
        if (value) text = '1'
    end function logical_text

end module fo_test_impact
