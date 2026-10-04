module fo_gremlin_readiness
    use fo_cache, only: cache_digest, HASH_LEN
    use fo_util, only: json_int
    implicit none
    private

    integer, parameter, public :: GREMLIN_PHASE_STARTING = 1
    integer, parameter, public :: GREMLIN_PHASE_BUILDING = 2
    integer, parameter, public :: GREMLIN_PHASE_TESTING = 3
    integer, parameter, public :: GREMLIN_PHASE_QUIESCENT = 4
    integer, parameter, public :: GREMLIN_PHASE_STOPPED = 5
    integer, parameter, public :: GREMLIN_PHASE_FAILED = 6

    integer, parameter, public :: GREMLIN_HEALTH_UNKNOWN = 0
    integer, parameter, public :: GREMLIN_HEALTH_HEALTHY = 1
    integer, parameter, public :: GREMLIN_HEALTH_DEGRADED = 2
    integer, parameter, public :: GREMLIN_HEALTH_FAILURE = 3

    integer, parameter, public :: GREMLIN_VERIFY_NONE = 0
    integer, parameter, public :: GREMLIN_VERIFY_GATE = 1
    integer, parameter, public :: GREMLIN_VERIFY_ORDINARY = 2
    integer, parameter, public :: GREMLIN_VERIFY_FULL = 3

    integer, parameter, public :: GREMLIN_WAIT_LOCAL_GATE_GREEN = 1
    integer, parameter, public :: GREMLIN_WAIT_ORDINARY_VERIFIED = 2
    integer, parameter, public :: GREMLIN_WAIT_FULLY_VERIFIED = 3
    integer, parameter, public :: GREMLIN_WAIT_QUIESCENT = 4
    integer, parameter, public :: GREMLIN_WAIT_FAILURE = 5

    type, public :: gremlin_readiness_input_t
        character(len=128) :: owner_session = ''
        character(len=HASH_LEN) :: generation = ''
        character(len=HASH_LEN) :: inventory_digest = ''
        character(len=HASH_LEN) :: requirement_digest = ''
        integer :: event_epoch = 0
        integer :: phase = GREMLIN_PHASE_STARTING
        logical :: exact_active_generation = .false.
        logical :: capture_fresh = .false.
        logical :: build_passed = .false.
        logical :: dirty = .false.
        logical :: gate_failure = .false.
        logical :: failure_observed = .false.
        integer :: gate_required = 0
        integer :: gate_passed = 0
        integer :: ordinary_required = 0
        integer :: ordinary_passed = 0
        integer :: ordinary_failures = 0
        integer :: full_required = 0
        integer :: full_passed = 0
        integer :: full_failures = 0
    end type gremlin_readiness_input_t

    type, public :: gremlin_readiness_t
        character(len=128) :: owner_session = ''
        character(len=HASH_LEN) :: generation = ''
        character(len=HASH_LEN) :: inventory_digest = ''
        character(len=HASH_LEN) :: requirement_digest = ''
        integer :: event_epoch = 0
        integer :: phase = GREMLIN_PHASE_STARTING
        integer :: health = GREMLIN_HEALTH_UNKNOWN
        integer :: verification_level = GREMLIN_VERIFY_NONE
        logical :: dirty = .false.
        logical :: gate_failure = .false.
        logical :: failure_observed = .false.
        character(len=HASH_LEN) :: gate_token = ''
        logical :: local_gate_green = .false.
        logical :: fully_verified = .false.
        integer :: gate_required = 0
        integer :: gate_passed = 0
        integer :: ordinary_required = 0
        integer :: ordinary_passed = 0
        integer :: ordinary_failures = 0
        integer :: full_required = 0
        integer :: full_passed = 0
        integer :: full_failures = 0
    end type gremlin_readiness_t

    public :: gremlin_readiness_compute, gremlin_wait_satisfied
    public :: gremlin_phase_name, gremlin_health_name, gremlin_verification_name
    public :: gremlin_wait_name, gremlin_wait_code

contains

    subroutine gremlin_readiness_compute(input, readiness)
        type(gremlin_readiness_input_t), intent(in) :: input
        type(gremlin_readiness_t), intent(out) :: readiness
        logical :: ordinary_verified, full_verified, fresh
        character(len=1024) :: token_parts(1)

        readiness = gremlin_readiness_t()
        readiness%owner_session = input%owner_session
        readiness%generation = input%generation
        readiness%inventory_digest = input%inventory_digest
        readiness%requirement_digest = input%requirement_digest
        readiness%event_epoch = input%event_epoch
        readiness%phase = input%phase
        readiness%dirty = input%dirty
        readiness%gate_required = max(0, input%gate_required)
        readiness%gate_passed = max(0, input%gate_passed)
        readiness%ordinary_required = max(0, input%ordinary_required)
        readiness%ordinary_passed = max(0, input%ordinary_passed)
        readiness%ordinary_failures = max(0, input%ordinary_failures)
        readiness%full_required = max(0, input%full_required)
        readiness%full_passed = max(0, input%full_passed)
        readiness%full_failures = max(0, input%full_failures)

        readiness%gate_failure = input%gate_failure
        readiness%failure_observed = input%failure_observed .or. input%gate_failure
        fresh = input%exact_active_generation .and. input%capture_fresh .and. &
            input%build_passed .and. .not. input%dirty .and. &
            input%phase /= GREMLIN_PHASE_STOPPED .and. &
            input%phase /= GREMLIN_PHASE_FAILED .and. &
            len_trim(input%owner_session) > 0 .and. &
            len_trim(input%generation) == HASH_LEN .and. &
            len_trim(input%inventory_digest) == HASH_LEN .and. &
            len_trim(input%requirement_digest) == HASH_LEN .and. input%event_epoch >= 0
        readiness%local_gate_green = fresh .and. &
            readiness%gate_required > 0 .and. &
            readiness%gate_passed == readiness%gate_required .and. &
            .not. input%gate_failure .and. .not. input%failure_observed .and. &
            readiness%ordinary_failures == 0 .and. readiness%full_failures == 0

        ordinary_verified = readiness%local_gate_green .and. &
            .not. readiness%failure_observed .and. &
            readiness%full_required > 0 .and. &
            readiness%ordinary_passed == readiness%ordinary_required .and. &
            readiness%ordinary_failures == 0
        full_verified = ordinary_verified .and. readiness%full_required > 0 .and. &
            readiness%full_passed == readiness%full_required .and. &
            readiness%full_failures == 0
        readiness%fully_verified = full_verified
        if (full_verified) then
            readiness%verification_level = GREMLIN_VERIFY_FULL
        else if (ordinary_verified) then
            readiness%verification_level = GREMLIN_VERIFY_ORDINARY
        else if (readiness%local_gate_green) then
            readiness%verification_level = GREMLIN_VERIFY_GATE
        end if
        if (readiness%local_gate_green) then
            token_parts(1) = 'gate-v1|'//trim(input%owner_session)//'|'// &
                input%generation//'|'// &
                input%inventory_digest//'|'//input%requirement_digest//'|'// &
                trim(json_int(input%event_epoch))
            readiness%gate_token = cache_digest(token_parts, 1)
        end if

        if (input%phase == GREMLIN_PHASE_FAILED .or. readiness%failure_observed .or. &
            readiness%ordinary_failures > 0 .or. readiness%full_failures > 0) then
            readiness%health = GREMLIN_HEALTH_FAILURE
        else if (readiness%local_gate_green) then
            readiness%health = GREMLIN_HEALTH_HEALTHY
        else if (input%dirty) then
            readiness%health = GREMLIN_HEALTH_DEGRADED
        else
            readiness%health = GREMLIN_HEALTH_UNKNOWN
        end if
    end subroutine gremlin_readiness_compute

    logical function gremlin_wait_satisfied(until_kind, readiness)
        integer, intent(in) :: until_kind
        type(gremlin_readiness_t), intent(in) :: readiness

        select case (until_kind)
        case (GREMLIN_WAIT_LOCAL_GATE_GREEN)
            gremlin_wait_satisfied = readiness%local_gate_green
        case (GREMLIN_WAIT_ORDINARY_VERIFIED)
            gremlin_wait_satisfied = readiness%verification_level >= GREMLIN_VERIFY_ORDINARY
        case (GREMLIN_WAIT_FULLY_VERIFIED)
            gremlin_wait_satisfied = readiness%fully_verified
        case (GREMLIN_WAIT_QUIESCENT)
            gremlin_wait_satisfied = readiness%phase == GREMLIN_PHASE_QUIESCENT .and. &
                .not. readiness%dirty
        case (GREMLIN_WAIT_FAILURE)
            gremlin_wait_satisfied = readiness%health == GREMLIN_HEALTH_FAILURE
        case default
            gremlin_wait_satisfied = .false.
        end select
    end function gremlin_wait_satisfied

    integer function gremlin_wait_code(name) result(code)
        character(len=*), intent(in) :: name

        select case (trim(name))
        case ('local-gate-green')
            code = GREMLIN_WAIT_LOCAL_GATE_GREEN
        case ('ordinary-verified')
            code = GREMLIN_WAIT_ORDINARY_VERIFIED
        case ('fully-verified')
            code = GREMLIN_WAIT_FULLY_VERIFIED
        case ('quiescent')
            code = GREMLIN_WAIT_QUIESCENT
        case ('failure')
            code = GREMLIN_WAIT_FAILURE
        case default
            code = 0
        end select
    end function gremlin_wait_code

    function gremlin_phase_name(phase) result(name)
        integer, intent(in) :: phase
        character(len=16) :: name

        select case (phase)
        case (GREMLIN_PHASE_STARTING)
            name = 'starting'
        case (GREMLIN_PHASE_BUILDING)
            name = 'building'
        case (GREMLIN_PHASE_TESTING)
            name = 'testing'
        case (GREMLIN_PHASE_QUIESCENT)
            name = 'quiescent'
        case (GREMLIN_PHASE_STOPPED)
            name = 'stopped'
        case (GREMLIN_PHASE_FAILED)
            name = 'failed'
        case default
            name = 'unknown'
        end select
    end function gremlin_phase_name

    function gremlin_health_name(health) result(name)
        integer, intent(in) :: health
        character(len=16) :: name

        select case (health)
        case (GREMLIN_HEALTH_HEALTHY)
            name = 'healthy'
        case (GREMLIN_HEALTH_DEGRADED)
            name = 'degraded'
        case (GREMLIN_HEALTH_FAILURE)
            name = 'failure'
        case default
            name = 'unknown'
        end select
    end function gremlin_health_name

    function gremlin_verification_name(level) result(name)
        integer, intent(in) :: level
        character(len=16) :: name

        select case (level)
        case (GREMLIN_VERIFY_ORDINARY)
            name = 'ordinary'
        case (GREMLIN_VERIFY_FULL)
            name = 'full'
        case (GREMLIN_VERIFY_GATE)
            name = 'gate'
        case default
            name = 'none'
        end select
    end function gremlin_verification_name

    function gremlin_wait_name(until_kind) result(name)
        integer, intent(in) :: until_kind
        character(len=32) :: name

        select case (until_kind)
        case (GREMLIN_WAIT_LOCAL_GATE_GREEN)
            name = 'local-gate-green'
        case (GREMLIN_WAIT_ORDINARY_VERIFIED)
            name = 'ordinary-verified'
        case (GREMLIN_WAIT_FULLY_VERIFIED)
            name = 'fully-verified'
        case (GREMLIN_WAIT_QUIESCENT)
            name = 'quiescent'
        case (GREMLIN_WAIT_FAILURE)
            name = 'failure'
        case default
            name = 'unknown'
        end select
    end function gremlin_wait_name

end module fo_gremlin_readiness
