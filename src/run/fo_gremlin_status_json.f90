module fo_gremlin_status_json
    !! The public Gremlin JSON documents, emitted through the Fx JSON builder:
    !! the published lane state, its readiness/coverage enrichment, the wait
    !! decoration, the compact summary, event pages and plain responses.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_generation, only: generation_t
    use fo_gremlin_readiness, only: gremlin_readiness_t, gremlin_phase_name, &
        gremlin_health_name, gremlin_verification_name
    use fo_gremlin_request, only: gremlin_request_t, gremlin_json_field
    use fo_gremlin_state, only: gremlin_session_t
    use fo_json_object, only: json_value_raw
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_array_start, json_array_end, json_key, &
        json_value_string, json_key_string, json_key_int, json_key_bool, &
        json_to_string, json_escape_string
    implicit none
    private

    public :: gremlin_status_json, gremlin_status_enrich, gremlin_wait_decorate
    public :: gremlin_summary_json, gremlin_events_json, gremlin_failure_json
    public :: gremlin_simple_response, gremlin_error_response, json_merge

    integer, parameter :: RUNNING_CASES_LIMIT = 2048
    integer, parameter :: SUMMARY_DIAGNOSTIC_LIMIT = 1024
    character(len=*), parameter :: SUMMARY_TRUNCATION = '...[truncated]'

contains

    function gremlin_status_json(session, request, policy_key, state, active, &
            candidate, active_model, current_case, running, completed, selected, &
            seed, last_outcome, last_exitcode, retention, diagnostic) result(text)
        !! The lane state an owner publishes. running: the in-flight cases.
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        character(len=*), intent(in) :: policy_key, state, active_model, current_case
        type(generation_t), intent(in) :: active, candidate
        character(len=*), intent(in) :: running(:)
        integer, intent(in) :: completed, selected, seed, last_exitcode
        character(len=*), intent(in) :: last_outcome, retention, diagnostic
        character(len=:), allocatable :: text
        type(json_builder_t) :: jb
        integer :: i, used

        jb = json_new()
        call json_object_start(jb)
        call json_key_int(jb, 'protocol', 1)
        call json_key_string(jb, 'session_id', trim(session%session_id))
        call json_key_string(jb, 'lane_id', trim(request%lane_id))
        call json_key_string(jb, 'lifetime', trim(request%lifetime))
        call json_key_string(jb, 'policy_key', policy_key)
        call json_key_string(jb, 'state', trim(state))
        call json_key_string(jb, 'driver_path', trim(session%driver_path))
        call json_key_string(jb, 'driver_digest', trim(session%driver_digest))
        call key_int64(jb, 'driver_size', session%driver_size)
        call json_key_string(jb, 'active_driver_digest', trim(active%driver_digest))
        call key_int64(jb, 'active_driver_size', active%driver_size)
        call json_key_string(jb, 'candidate_driver_digest', trim(candidate%driver_digest))
        call key_int64(jb, 'candidate_driver_size', candidate%driver_size)
        call json_key_string(jb, 'active_generation', trim(active%identity))
        call json_key_string(jb, 'active_model', trim(active_model))
        call json_key_string(jb, 'active_model_generation', trim(active%model_generation))
        call json_key_string(jb, 'candidate_generation', trim(candidate%identity))
        call json_key_string(jb, 'active_project', project_of(active))
        call json_key_string(jb, 'candidate_project', project_of(candidate))
        call json_key_string(jb, 'current_test', trim(current_case))
        call json_key_int(jb, 'running', size(running))
        call json_key(jb, 'running_cases')
        call json_array_start(jb)
        used = 0
        do i = 1, size(running)
            if (used + len_trim(running(i)) + 6 > RUNNING_CASES_LIMIT) exit
            call json_value_string(jb, trim(running(i)))
            used = used + len(json_escape_string(trim(running(i)))) + 2
            if (i > 1) used = used + 1
        end do
        call json_array_end(jb)
        call json_key_int(jb, 'completed', completed)
        call json_key_int(jb, 'selected', selected)
        call json_key_int(jb, 'seed', seed)
        call json_key_string(jb, 'last_outcome', trim(last_outcome))
        call json_key_int(jb, 'last_exitcode', last_exitcode)
        call json_key_int(jb, 'gate_required', request%gate_required_count)
        call json_key_string(jb, 'requirement_digest', request%requirement_digest)
        call json_key_int(jb, 'event_epoch', request%event_epoch)
        call json_key_bool(jb, 'input_changed', request%input_changed)
        call json_key(jb, 'retention')
        call json_value_raw(jb, retention)
        call json_key_string(jb, 'diagnostic', trim(diagnostic))
        call json_object_end(jb)
        text = json_to_string(jb)
    end function gremlin_status_json

    function gremlin_status_enrich(status_text, view, readiness, have_coverage) &
            result(enriched)
        !! The published state with the readiness facts and, when the active
        !! generation has coverage, its ordinary/full verification counts.
        character(len=*), intent(in) :: status_text
        type(gremlin_coverage_view_t), intent(in) :: view
        type(gremlin_readiness_t), intent(in) :: readiness
        logical, intent(in) :: have_coverage
        character(len=:), allocatable :: enriched
        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call readiness_members(jb, readiness)
        if (have_coverage) then
            call json_key(jb, 'coverage')
            call json_object_start(jb)
            call json_key(jb, 'ordinary')
            call verification_counts(jb, view%ordinary_eligible_count, &
                view%ordinary_pass_count, view%ordinary_failure_count, &
                view%ordinary_timeout_count, view%ordinary_infra_count, &
                view%ordinary_flaky_count)
            call json_key(jb, 'full')
            call verification_counts(jb, view%eligible_count, view%pass_count, &
                readiness%full_failures, view%timeout_count, view%infra_count, &
                view%flaky_count)
            call json_key_string(jb, 'generation_id', trim(view%generation_id))
            call json_key_string(jb, 'inventory_digest', trim(view%inventory_digest))
            call json_key_int(jb, 'epoch', view%epoch)
            call json_key_int(jb, 'seed', view%seed)
            call json_key_int(jb, 'cursor', view%cursor)
            call json_key_int(jb, 'eligible', view%eligible_count)
            call json_key_int(jb, 'completed', view%completed_count)
            call json_key_int(jb, 'pass', view%pass_count)
            call json_key_int(jb, 'fail', view%fail_count)
            call json_key_int(jb, 'timeout', view%timeout_count)
            call json_key_int(jb, 'flaky', view%flaky_count)
            call json_key_int(jb, 'infra', view%infra_count)
            call json_key_int(jb, 'running', view%running_count)
            call json_key_int(jb, 'cancelled', view%cancelled_count)
            call json_key_int(jb, 'unknown', view%unknown_count)
            call json_key_int(jb, 'remaining', view%remaining_count)
            call json_key_int(jb, 'ordinary_required', readiness%ordinary_required)
            call json_key_int(jb, 'ordinary_passed', readiness%ordinary_passed)
            call json_key_bool(jb, 'ordinary_verified', readiness%verification_level >= 2)
            call json_key_int(jb, 'full_required', readiness%full_required)
            call json_key_int(jb, 'full_passed', readiness%full_passed)
            call json_key_bool(jb, 'fully_verified', readiness%fully_verified)
            call json_key_bool(jb, 'ordinary_complete', view%ordinary_complete)
            call json_key_bool(jb, 'full_coverage', view%full_coverage)
            call json_key_bool(jb, 'green', view%green)
            call json_object_end(jb)
        end if
        call json_object_end(jb)
        enriched = json_merge(trim(status_text), json_to_string(jb))
    end function gremlin_status_enrich

    subroutine readiness_members(jb, readiness)
        type(json_builder_t), intent(inout) :: jb
        type(gremlin_readiness_t), intent(in) :: readiness

        call json_key_string(jb, 'phase', trim(gremlin_phase_name(readiness%phase)))
        call json_key_string(jb, 'health', trim(gremlin_health_name(readiness%health)))
        call json_key_bool(jb, 'dirty', readiness%dirty)
        call json_key_bool(jb, 'local_gate_green', readiness%local_gate_green)
        call json_key_string(jb, 'verification_level', &
            trim(gremlin_verification_name(readiness%verification_level)))
        call json_key_bool(jb, 'fully_verified', readiness%fully_verified)
        call json_key_int(jb, 'gate_token_version', 1)
        call json_key_string(jb, 'gate_token', trim(readiness%gate_token))
        call json_key_string(jb, 'requirement_digest', trim(readiness%requirement_digest))
        call json_key_int(jb, 'event_epoch', readiness%event_epoch)
        call json_key_int(jb, 'gate_required', readiness%gate_required)
        call json_key_int(jb, 'gate_passed', readiness%gate_passed)
        call json_key_int(jb, 'ordinary_required', readiness%ordinary_required)
        call json_key_int(jb, 'ordinary_passed', readiness%ordinary_passed)
        call json_key_int(jb, 'ordinary_failures', readiness%ordinary_failures)
        call json_key_int(jb, 'full_required', readiness%full_required)
        call json_key_int(jb, 'full_passed', readiness%full_passed)
        call json_key_int(jb, 'full_failures', readiness%full_failures)
    end subroutine readiness_members

    subroutine verification_counts(jb, eligible, passed, failures, timeouts, infra, &
            flaky)
        type(json_builder_t), intent(inout) :: jb
        integer, intent(in) :: eligible, passed, failures, timeouts, infra, flaky
        integer :: remaining

        remaining = max(0, eligible - passed - failures)
        call json_object_start(jb)
        call json_key_int(jb, 'eligible', eligible)
        call json_key_int(jb, 'pass', passed)
        call json_key_int(jb, 'fail', max(0, failures - timeouts - infra - flaky))
        call json_key_int(jb, 'timeout', timeouts)
        call json_key_int(jb, 'infra_error', infra)
        call json_key_int(jb, 'flaky', flaky)
        call json_key_int(jb, 'unknown', remaining)
        call json_key_int(jb, 'remaining', remaining)
        call json_object_end(jb)
    end subroutine verification_counts

    function gremlin_wait_decorate(status_json, wait_name, satisfied, timed_out, &
            terminal, failed, failure_report) result(decorated)
        !! A status document with the wait outcome; failure_report (a failures
        !! response) is attached when it is not empty.
        character(len=*), intent(in) :: status_json, wait_name, failure_report
        logical, intent(in) :: satisfied, timed_out, terminal, failed
        character(len=:), allocatable :: decorated
        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'wait_until', trim(wait_name))
        call json_key_bool(jb, 'wait_satisfied', satisfied)
        call json_key_bool(jb, 'wait_timed_out', timed_out)
        call json_key_bool(jb, 'wait_terminal', terminal)
        call json_key_bool(jb, 'failure_observed', failed)
        if (len(failure_report) > 1) then
            call json_key(jb, 'durable_failure_report')
            call json_value_raw(jb, failure_report)
        end if
        call json_object_end(jb)
        decorated = json_merge(status_json, json_to_string(jb))
    end function gremlin_wait_decorate

    function gremlin_summary_json(status_text, action, latest_failure) result(text)
        !! The compact summary of a status or wait document; latest_failure is
        !! the newest failure receipt object or empty.
        character(len=*), intent(in) :: status_text, action, latest_failure
        character(len=:), allocatable :: text
        character(len=32), parameter :: strings(*) = [character(len=32) :: &
            'session_id', 'lane_id', 'lifetime', 'state', 'phase', 'health', &
            'current_test', &
            'active_generation', 'candidate_generation', 'last_outcome', &
            'diagnostic', 'verification_level', 'requirement_digest', 'wait_until']
        character(len=32), parameter :: scalars(*) = [character(len=32) :: &
            'input_changed', 'dirty', 'local_gate_green', 'fully_verified', &
            'completed', 'selected', 'seed', 'last_exitcode', 'event_epoch', &
            'gate_required', 'gate_passed', 'ordinary_required', 'ordinary_passed', &
            'ordinary_failures', 'full_required', 'full_passed', 'full_failures', &
            'wait_satisfied', 'wait_timed_out', 'wait_terminal', 'failure_observed']
        character(len=4096) :: value
        type(json_builder_t) :: jb
        integer :: i

        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'action', trim(action))
        call json_key_string(jb, 'detail', 'summary')
        do i = 1, size(strings)
            call gremlin_json_field(status_text, trim(strings(i)), value, .true.)
            if (len_trim(value) == 0) cycle
            if (trim(strings(i)) == 'diagnostic' .and. &
                    len_trim(value) > SUMMARY_DIAGNOSTIC_LIMIT) &
                value = value(:SUMMARY_DIAGNOSTIC_LIMIT - len(SUMMARY_TRUNCATION))// &
                    SUMMARY_TRUNCATION
            call json_key_string(jb, trim(strings(i)), trim(value))
        end do
        do i = 1, size(scalars)
            call gremlin_json_field(status_text, trim(scalars(i)), value, .true.)
            if (len_trim(value) == 0) cycle
            call json_key(jb, trim(scalars(i)))
            call json_value_raw(jb, trim(value))
        end do
        if (len(latest_failure) > 0) then
            call json_key(jb, 'latest_failure')
            call json_value_raw(jb, latest_failure)
        end if
        call json_key_string(jb, 'retrieval_hint', 'Use detail=full for complete '// &
            'status or action=events/failures with cursors for event pages.')
        call json_object_end(jb)
        text = json_to_string(jb)
    end function gremlin_summary_json

    function gremlin_failure_json(generation, case_id, status, log_path) result(text)
        character(len=*), intent(in) :: generation, case_id, status, log_path
        character(len=:), allocatable :: text
        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'generation', trim(generation))
        call json_key_string(jb, 'case_id', trim(case_id))
        call json_key_string(jb, 'status', trim(status))
        call json_key_string(jb, 'log_path', trim(log_path))
        call json_object_end(jb)
        text = json_to_string(jb)
    end function gremlin_failure_json

    function gremlin_events_json(action, status_text, session_id, lane_id, events, &
            next_cursor, has_more, lifecycle, next_lifecycle_cursor, &
            lifecycle_has_more) result(text)
        !! An event page: the action, the status members, the receipt and
        !! lifecycle event arrays with their cursors.
        character(len=*), intent(in) :: action, status_text, session_id, lane_id
        type(json_builder_t), intent(in) :: events, lifecycle
        integer(int64), intent(in) :: next_cursor, next_lifecycle_cursor
        logical, intent(in) :: has_more, lifecycle_has_more
        character(len=:), allocatable :: text
        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'action', trim(action))
        call json_object_end(jb)
        text = json_merge(json_to_string(jb), trim(status_text))
        if (len_trim(status_text) < 2) then
            ! No published state: an unknown lane state of this identity.
            jb = json_new()
            call json_object_start(jb)
            call json_key_int(jb, 'protocol', 1)
            call json_key_string(jb, 'session_id', trim(session_id))
            call json_key_string(jb, 'lane_id', trim(lane_id))
            call json_key_string(jb, 'state', 'unknown')
            call json_key_string(jb, 'active_generation', '')
            call json_key_string(jb, 'candidate_generation', '')
            call json_key_int(jb, 'completed', 0)
            call json_key_int(jb, 'selected', 0)
            call json_key_int(jb, 'seed', 0)
            call json_key_string(jb, 'last_outcome', 'NONE')
            call json_key_int(jb, 'last_exitcode', 0)
            call json_object_end(jb)
            text = json_merge(text, json_to_string(jb))
        end if
        jb = json_new()
        call json_object_start(jb)
        call json_key(jb, 'events')
        call json_value_raw(jb, json_to_string(events))
        call key_int64(jb, 'next_cursor', next_cursor)
        call json_key_bool(jb, 'has_more', has_more)
        call json_key(jb, 'lifecycle_events')
        call json_value_raw(jb, json_to_string(lifecycle))
        call key_int64(jb, 'next_lifecycle_cursor', next_lifecycle_cursor)
        call json_key_bool(jb, 'lifecycle_has_more', lifecycle_has_more)
        call json_object_end(jb)
        text = json_merge(text, json_to_string(jb))
    end function gremlin_events_json

    subroutine gremlin_simple_response(action, lane_id, session_id, state, response)
        character(len=*), intent(in) :: action, lane_id, session_id, state
        character(len=:), allocatable, intent(out) :: response
        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'action', trim(action))
        call json_key_string(jb, 'lane_id', trim(lane_id))
        call json_key_string(jb, 'session_id', trim(session_id))
        call json_key_string(jb, 'state', trim(state))
        call json_object_end(jb)
        response = json_to_string(jb)
    end subroutine gremlin_simple_response

    subroutine gremlin_error_response(action, message, response)
        character(len=*), intent(in) :: action, message
        character(len=:), allocatable, intent(out) :: response
        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'action', trim(action))
        call json_key_string(jb, 'error', trim(message))
        call json_object_end(jb)
        response = json_to_string(jb)
    end subroutine gremlin_error_response

    function json_merge(first, second) result(merged)
        !! The members of object `first` followed by those of object `second`.
        character(len=*), intent(in) :: first, second
        character(len=:), allocatable :: merged

        if (len(second) <= 2) then
            merged = first
        else if (len(first) <= 2) then
            merged = second
        else
            merged = first(:len(first) - 1)//','//second(2:)
        end if
    end function json_merge

    function project_of(generation) result(project)
        type(generation_t), intent(in) :: generation
        character(len=:), allocatable :: project

        project = ''
        if (len_trim(generation%identity) > 0) project = trim(generation%project_root)
    end function project_of

    subroutine key_int64(jb, key, value)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: key
        integer(int64), intent(in) :: value
        character(len=32) :: text

        write (text, '(i0)') value
        call json_key(jb, key)
        call json_value_raw(jb, trim(text))
    end subroutine key_int64

end module fo_gremlin_status_json
