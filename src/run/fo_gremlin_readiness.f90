module fo_gremlin_readiness
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

    integer, parameter, public :: GREMLIN_VERIFY_GATE = 1
    integer, parameter, public :: GREMLIN_VERIFY_ORDINARY = 2
    integer, parameter, public :: GREMLIN_VERIFY_FULL = 3

    integer, parameter, public :: GREMLIN_WAIT_PROMOTION_READY = 1
    integer, parameter, public :: GREMLIN_WAIT_ORDINARY_VERIFIED = 2
    integer, parameter, public :: GREMLIN_WAIT_FULLY_VERIFIED = 3
    integer, parameter, public :: GREMLIN_WAIT_QUIESCENT = 4
    integer, parameter, public :: GREMLIN_WAIT_FAILURE = 5

    type, public :: gremlin_gate_evidence_t
        logical :: capture_fresh = .false.
        logical :: build_passed = .false.
        logical :: regression_passed = .false.
        logical :: affected_passed = .false.
        logical :: mandatory_passed = .false.
        logical :: ordinary_complete = .false.
        logical :: full_complete = .false.
        logical :: gate_failure = .false.
        logical :: exact_active_generation = .false.
    end type gremlin_gate_evidence_t

    type, public :: gremlin_readiness_t
        integer :: phase = GREMLIN_PHASE_STARTING
        integer :: health = GREMLIN_HEALTH_UNKNOWN
        integer :: verification_level = GREMLIN_VERIFY_GATE
        logical :: promotion_ready = .false.
        type(gremlin_gate_evidence_t) :: gates
    end type gremlin_readiness_t

    public :: gremlin_readiness_compute, gremlin_wait_satisfied
    public :: gremlin_phase_name, gremlin_health_name, gremlin_verification_name
    public :: gremlin_wait_name, gremlin_dirty_event_relevant

contains

    subroutine gremlin_readiness_compute(phase, evidence, readiness)
        integer, intent(in) :: phase
        type(gremlin_gate_evidence_t), intent(in) :: evidence
        type(gremlin_readiness_t), intent(out) :: readiness

        readiness%phase = phase
        readiness%gates = evidence
        readiness%promotion_ready = evidence%exact_active_generation .and. &
            evidence%capture_fresh .and. evidence%build_passed .and. &
            evidence%regression_passed .and. evidence%affected_passed .and. &
            evidence%mandatory_passed .and. .not. evidence%gate_failure
        if (phase == GREMLIN_PHASE_FAILED .or. evidence%gate_failure) then
            readiness%health = GREMLIN_HEALTH_FAILURE
        else if (readiness%promotion_ready) then
            readiness%health = GREMLIN_HEALTH_HEALTHY
        else
            readiness%health = GREMLIN_HEALTH_UNKNOWN
        end if
        if (evidence%full_complete) then
            readiness%verification_level = GREMLIN_VERIFY_FULL
        else if (evidence%ordinary_complete) then
            readiness%verification_level = GREMLIN_VERIFY_ORDINARY
        else
            readiness%verification_level = GREMLIN_VERIFY_GATE
        end if
    end subroutine gremlin_readiness_compute

    logical function gremlin_wait_satisfied(until_kind, readiness)
        integer, intent(in) :: until_kind
        type(gremlin_readiness_t), intent(in) :: readiness

        select case (until_kind)
        case (GREMLIN_WAIT_PROMOTION_READY)
            gremlin_wait_satisfied = readiness%promotion_ready
        case (GREMLIN_WAIT_ORDINARY_VERIFIED)
            gremlin_wait_satisfied = &
                readiness%verification_level >= GREMLIN_VERIFY_ORDINARY
        case (GREMLIN_WAIT_FULLY_VERIFIED)
            gremlin_wait_satisfied = &
                readiness%verification_level >= GREMLIN_VERIFY_FULL
        case (GREMLIN_WAIT_QUIESCENT)
            gremlin_wait_satisfied = readiness%phase == GREMLIN_PHASE_QUIESCENT
        case (GREMLIN_WAIT_FAILURE)
            gremlin_wait_satisfied = readiness%health == GREMLIN_HEALTH_FAILURE
        case default
            gremlin_wait_satisfied = .false.
        end select
    end function gremlin_wait_satisfied

    logical function gremlin_dirty_event_relevant(path)
        character(len=*), intent(in) :: path
        integer :: n

        n = len_trim(path)
        gremlin_dirty_event_relevant = .false.
        if (n == 0) return
        if (is_suffix(path, '.f90') .or. is_suffix(path, '.F90') .or. &
            is_suffix(path, '.f') .or. is_suffix(path, '.F') .or. &
            is_suffix(path, '.inc') .or. is_suffix(path, '.h') .or. &
            is_suffix(path, '.c') .or. is_suffix(path, '.toml')) then
            gremlin_dirty_event_relevant = .true.
        else if (path(:n) == 'Makefile' .or. path(:n) == 'makefile' .or. &
            path(:n) == 'CMakeLists.txt') then
            gremlin_dirty_event_relevant = .true.
        end if
    end function gremlin_dirty_event_relevant

    logical function is_suffix(path, suffix)
        character(len=*), intent(in) :: path, suffix
        integer :: n, m

        n = len_trim(path)
        m = len(suffix)
        is_suffix = .false.
        if (n < m) return
        is_suffix = path(n - m + 1:n) == suffix
    end function is_suffix

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
        case default
            name = 'gate'
        end select
    end function gremlin_verification_name

    function gremlin_wait_name(until_kind) result(name)
        integer, intent(in) :: until_kind
        character(len=32) :: name

        select case (until_kind)
        case (GREMLIN_WAIT_PROMOTION_READY)
            name = 'promotion-ready'
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
