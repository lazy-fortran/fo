module fo_gremlin_policy
    use fo_test_random, only: select_random_tests
    implicit none
    private

    integer, parameter, public :: GREMLIN_POLICY_OK = 0
    integer, parameter, public :: GREMLIN_POLICY_INVALID_COUNT = 1
    integer, parameter, public :: GREMLIN_POLICY_UNKNOWN_NAME = 2
    integer, parameter, public :: GREMLIN_POLICY_OUTPUT_TOO_SMALL = 3
    integer, parameter, public :: GREMLIN_POLICY_EMPTY_SELECTION = 4
    integer, parameter, public :: GREMLIN_POLICY_INVALID_INVENTORY = 5
    integer, parameter, public :: GREMLIN_NONMANDATORY_LIMIT = 32
    integer, parameter :: GREMLIN_DEBT_LIMIT = GREMLIN_NONMANDATORY_LIMIT/4

    public :: select_gremlin_tests
    public :: shuffle_gremlin_tests

contains

    subroutine select_gremlin_tests( &
            names, n_names, mandatory, n_mandatory, impacted, n_impacted, &
            history, n_history, debt, n_debt, random_count, seed, &
            selected, n_selected, status, debt_cursor, n_mandatory_selected)
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        character(len=*), intent(in) :: mandatory(:), impacted(:), history(:), debt(:)
        integer, intent(in) :: n_mandatory, n_impacted, n_history, n_debt
        integer, intent(in) :: random_count, seed
        character(len=*), intent(out) :: selected(:)
        integer, intent(out) :: n_selected, status
        ! One-based next debt index; updated to the position after the scan.
        integer, optional, intent(inout) :: debt_cursor
        ! Unique mandatory prefix length; zero on unsuccessful selection.
        integer, optional, intent(out) :: n_mandatory_selected

        character(len=len(names)), allocatable :: ordered(:), candidates(:), sample(:)
        character(len=len(names)), allocatable :: reserved_debt(:)
        integer :: n_reserved, n_candidates, n_sample, random_slots
        integer :: i, debt_index, debt_start, debt_scanned, cursor_after
        integer :: n_mandatory_unique, nonmandatory_count

        selected = ''
        n_selected = 0
        status = GREMLIN_POLICY_OK
        if (present(n_mandatory_selected)) n_mandatory_selected = 0
        if (n_names < 1 .or. n_names > size(names) .or. random_count < 0) then
            status = GREMLIN_POLICY_INVALID_COUNT
            return
        end if
        if (n_mandatory < 0 .or. n_mandatory > size(mandatory)) then
            status = GREMLIN_POLICY_INVALID_COUNT
            return
        end if
        if (n_impacted < 0 .or. n_impacted > size(impacted)) then
            status = GREMLIN_POLICY_INVALID_COUNT
            return
        end if
        if (n_history < 0 .or. n_history > size(history)) then
            status = GREMLIN_POLICY_INVALID_COUNT
            return
        end if
        if (n_debt < 0 .or. n_debt > size(debt)) then
            status = GREMLIN_POLICY_INVALID_COUNT
            return
        end if
        if (present(debt_cursor)) then
            if (debt_cursor < 1) then
                status = GREMLIN_POLICY_INVALID_COUNT
                return
            end if
        end if
        if (len(selected) < len(names)) then
            status = GREMLIN_POLICY_OUTPUT_TOO_SMALL
            return
        end if

        call validate_inventory(names, n_names, status)
        if (status /= GREMLIN_POLICY_OK) return
        call validate_names(names, n_names, mandatory, n_mandatory, status)
        if (status /= GREMLIN_POLICY_OK) return
        call validate_names(names, n_names, impacted, n_impacted, status)
        if (status /= GREMLIN_POLICY_OK) return
        call validate_names(names, n_names, history, n_history, status)
        if (status /= GREMLIN_POLICY_OK) return
        call validate_names(names, n_names, debt, n_debt, status)
        if (status /= GREMLIN_POLICY_OK) return

        allocate(ordered(n_names), candidates(n_names), sample(n_names))
        allocate(reserved_debt(min(GREMLIN_DEBT_LIMIT, n_debt)))
        ordered = ''
        candidates = ''
        reserved_debt = ''
        do i = 1, n_mandatory
            call append_unique(mandatory(i), ordered, n_selected)
        end do
        n_mandatory_unique = n_selected

        ! The caller orders debt from least recently completed to most recent.
        ! A session cursor moves the bounded window across that ordered list.
        n_reserved = 0
        debt_start = 1
        if (n_debt > 0) then
            if (present(debt_cursor)) then
                debt_start = modulo(debt_cursor - 1, n_debt) + 1
            end if
            debt_index = debt_start
            debt_scanned = 0
            do while (debt_scanned < n_debt .and. &
                    n_reserved < size(reserved_debt))
                if (.not. contains_name(ordered, n_selected, debt(debt_index)) .and. &
                    .not. contains_name( &
                    reserved_debt, n_reserved, debt(debt_index))) then
                    n_reserved = n_reserved + 1
                    reserved_debt(n_reserved) = debt(debt_index)
                end if
                debt_scanned = debt_scanned + 1
                debt_index = debt_index + 1
                if (debt_index > n_debt) debt_index = 1
            end do
            cursor_after = debt_index
        else
            cursor_after = 1
        end if

        ! Impacts precede historical failures; both precede rotating debt and
        ! the exploratory sample in the returned selection.
        nonmandatory_count = 0
        do i = 1, n_impacted
            if (nonmandatory_count >= &
                GREMLIN_NONMANDATORY_LIMIT - n_reserved) exit
            call append_unique(impacted(i), ordered, n_selected)
            nonmandatory_count = n_selected - n_mandatory_unique
        end do
        do i = 1, n_history
            if (nonmandatory_count >= &
                GREMLIN_NONMANDATORY_LIMIT - n_reserved) exit
            call append_unique(history(i), ordered, n_selected)
            nonmandatory_count = n_selected - n_mandatory_unique
        end do
        do i = 1, n_reserved
            if (nonmandatory_count >= GREMLIN_NONMANDATORY_LIMIT) exit
            call append_unique(reserved_debt(i), ordered, n_selected)
            nonmandatory_count = n_selected - n_mandatory_unique
        end do

        ! Sampling membership uses the supplied seed and does not reorder the
        ! mandatory, impacted, history, or debt prefixes.
        random_slots = min(random_count, &
            max(0, GREMLIN_NONMANDATORY_LIMIT - nonmandatory_count))
        n_candidates = 0
        do i = 1, n_names
            if (contains_name(ordered, n_selected, names(i))) cycle
            if (contains_name(candidates, n_candidates, names(i))) cycle
            n_candidates = n_candidates + 1
            candidates(n_candidates) = names(i)
        end do
        n_sample = 0
        if (random_slots > 0 .and. n_candidates > 0) then
            call select_random_tests( &
                candidates, n_candidates, random_slots, seed, sample, n_sample)
            do i = 1, n_sample
                call append_unique(sample(i), ordered, n_selected)
            end do
        end if

        if (n_selected == 0) then
            status = GREMLIN_POLICY_EMPTY_SELECTION
            return
        end if
        if (n_selected > size(selected)) then
            n_selected = 0
            status = GREMLIN_POLICY_OUTPUT_TOO_SMALL
            return
        end if
        selected(:n_selected) = ordered(:n_selected)
        if (present(debt_cursor)) debt_cursor = cursor_after
        if (present(n_mandatory_selected)) then
            n_mandatory_selected = n_mandatory_unique
        end if
    end subroutine select_gremlin_tests

    subroutine shuffle_gremlin_tests(selected, n_mandatory, n_selected, seed, status)
        character(len=*), intent(inout) :: selected(:)
        integer, intent(in) :: n_mandatory, n_selected, seed
        integer, intent(out) :: status

        character(len=len(selected)), allocatable :: shuffled(:)
        integer :: n_shuffled, n_exploratory

        status = GREMLIN_POLICY_OK
        if (n_selected < 0 .or. n_selected > size(selected)) then
            status = GREMLIN_POLICY_INVALID_COUNT
            return
        end if
        if (n_mandatory < 0 .or. n_mandatory > n_selected) then
            status = GREMLIN_POLICY_INVALID_COUNT
            return
        end if
        n_exploratory = n_selected - n_mandatory
        if (n_exploratory < 2) return
        allocate(shuffled(n_exploratory))
        call select_random_tests( &
            selected(n_mandatory + 1:n_selected), n_exploratory, &
            n_exploratory, seed, shuffled, n_shuffled)
        selected(n_mandatory + 1:n_selected) = shuffled(:n_shuffled)
    end subroutine shuffle_gremlin_tests

    subroutine validate_inventory(names, n_names, status)
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: status

        integer :: i

        status = GREMLIN_POLICY_OK
        do i = 1, n_names
            if (len_trim(names(i)) == 0) then
                status = GREMLIN_POLICY_INVALID_INVENTORY
                return
            end if
            if (i == 1) cycle
            if (any(names(:i - 1) == names(i))) then
                status = GREMLIN_POLICY_INVALID_INVENTORY
                return
            end if
        end do
    end subroutine validate_inventory

    subroutine validate_names(names, n_names, requested, n_requested, status)
        character(len=*), intent(in) :: names(:), requested(:)
        integer, intent(in) :: n_names, n_requested
        integer, intent(out) :: status

        integer :: i

        status = GREMLIN_POLICY_OK
        do i = 1, n_requested
            if (.not. any(names(:n_names) == requested(i))) then
                status = GREMLIN_POLICY_UNKNOWN_NAME
                return
            end if
        end do
    end subroutine validate_names

    subroutine append_unique(name, names, n_names)
        character(len=*), intent(in) :: name
        character(len=*), intent(inout) :: names(:)
        integer, intent(inout) :: n_names

        if (contains_name(names, n_names, name)) return
        if (n_names >= size(names)) return
        n_names = n_names + 1
        names(n_names) = name
    end subroutine append_unique

    logical function contains_name(names, n_names, name)
        character(len=*), intent(in) :: names(:), name
        integer, intent(in) :: n_names

        contains_name = .false.
        if (n_names < 1) return
        contains_name = any(names(:n_names) == name)
    end function contains_name

end module fo_gremlin_policy
