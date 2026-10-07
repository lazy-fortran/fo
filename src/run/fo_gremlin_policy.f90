module fo_gremlin_policy
    use fo_test_random, only: select_random_tests
    implicit none
    private

    integer, parameter, public :: GREMLIN_POLICY_OK = 0
    integer, parameter, public :: GREMLIN_POLICY_INVALID_COUNT = 1
    integer, parameter, public :: GREMLIN_NONMANDATORY_LIMIT = 32
    public :: shuffle_gremlin_tests

contains

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

end module fo_gremlin_policy
