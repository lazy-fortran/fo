module fo_gremlin_coverage_view
    implicit none
    private

    type, public :: gremlin_coverage_view_t
        character(len=64) :: generation_id = ''
        character(len=64) :: inventory_digest = ''
        integer :: epoch = 0
        integer :: seed = 0
        integer :: cursor = 0
        integer :: eligible_count = 0
        integer :: ordinary_eligible_count = 0
        integer :: completed_count = 0
        integer :: pass_count = 0
        integer :: ordinary_pass_count = 0
        integer :: fail_count = 0
        integer :: ordinary_failure_count = 0
        integer :: timeout_count = 0
        integer :: ordinary_timeout_count = 0
        integer :: flaky_count = 0
        integer :: ordinary_flaky_count = 0
        integer :: infra_count = 0
        integer :: ordinary_infra_count = 0
        integer :: running_count = 0
        integer :: cancelled_count = 0
        integer :: remaining_count = 0
        integer :: unknown_count = 0
        integer :: other_count = 0
        logical :: ordinary_complete = .false.
        logical :: full_coverage = .false.
        logical :: green = .false.
    end type gremlin_coverage_view_t
end module fo_gremlin_coverage_view
