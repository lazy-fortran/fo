program test_gremlin_status_json
    !! The status/wait documents of a fixed lane state, parsed independently,
    !! equal the documents of the previous hand-concatenated emitter: the same
    !! members in the same order (duplicates included) with the same values.
    use, intrinsic :: iso_fortran_env, only: int64, output_unit
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_generation, only: generation_t
    use fo_gremlin_readiness, only: gremlin_readiness_t, GREMLIN_PHASE_TESTING, &
        GREMLIN_HEALTH_FAILURE, GREMLIN_VERIFY_ORDINARY
    use fo_gremlin_request, only: gremlin_request_t
    use fo_gremlin_state, only: gremlin_session_t
    use fo_gremlin_status_json, only: gremlin_status_json, gremlin_status_enrich, &
        gremlin_wait_decorate
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_json, only: json_value_t, json_parse, json_object, json_array
    implicit none

    type(gremlin_session_t) :: session
    type(gremlin_request_t), allocatable :: request
    type(generation_t) :: active, candidate
    type(gremlin_readiness_t) :: readiness
    type(gremlin_coverage_view_t) :: view
    character(len=:), allocatable :: status, enriched, bare, waited
    character(len=1024) :: names(2)
    !! Previous emitter output for this state: the published state, the
    !! readiness tail, the coverage member and the wait decoration.
    character(len=*), parameter :: GOLDEN_1 = &
        '{"protocol":1,"session_id":"owner-1","lane_id":"lane-a","lifetime":"persistent","policy_key":"99'// &
        '99999999999999999999999999999999999999999999999999999999999999","state":'// &
        '"testing","driver_path":"/opt/fo \"x\"\\bin/fo","driver_digest":"aaaaaaa'// &
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","driver_size"'// &
        ':123456789012,"active_driver_digest":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'// &
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","active_driver_size":42,"candidate_drive'// &
        'r_digest":"fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'// &
        'fff","candidate_driver_size":43,"active_generation":"ccccccccccccccccccc'// &
        'ccccccccccccccccccccccccccccccccccccccccccccc","active_model":"ddddddddd'// &
        'ddddddddddddddddddddddddddddddddddddddddddddddddddddddd","active_model_g'// &
        'eneration":"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd'// &
        'dddd","candidate_generation":"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee'// &
        'eeeeeeeeeeeeeeeeeeeeee","active_project":"/gen/c/project","candidate_pro'// &
        'ject":"/gen/e/project","current_test":"test_a","running":2,"running_case'// &
        's":["test_a","te\"st_b"],"completed":11,"selected":20,"seed":1729,"last_'// &
        'outcome":"FAIL","last_exitcode":1,"gate_required":3,"requirement_digest"'// &
        ':"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","eve'// &
        'nt_epoch":7,"input_changed":true,"retention":{"generations":2,"bytes":10'// &
        ',"pinned":1,"owners":1,"views":0,"view_bytes":0,"view_bytes_exact":true}'// &
        ',"diagnostic":"line one\ntwo"}'
    character(len=*), parameter :: GOLDEN_2 = &
        ',"phase":"testing","health":"failure","dirty":true,"local_gate_green":fa'// &
        'lse,"verification_level":"ordinary","fully_verified":false,"gate_token_v'// &
        'ersion":1,"gate_token":"111111111111111111111111111111111111111111111111'// &
        '1111111111111111","requirement_digest":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'// &
        'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","event_epoch":7,"gate_required":3,"gat'// &
        'e_passed":2,"ordinary_required":5,"ordinary_passed":4,"ordinary_failures'// &
        '":1,"full_required":6,"full_passed":4,"full_failures":2'
    character(len=*), parameter :: GOLDEN_3 = &
        ',"coverage":{"ordinary":{"eligible":5,"pass":4,"fail":1,"timeout":0,"inf'// &
        'ra_error":0,"flaky":0,"unknown":0,"remaining":0},"full":{"eligible":6,"p'// &
        'ass":4,"fail":1,"timeout":1,"infra_error":0,"flaky":0,"unknown":0,"remai'// &
        'ning":0},"generation_id":"cccccccccccccccccccccccccccccccccccccccccccccc'// &
        'cccccccccccccccccc","inventory_digest":"22222222222222222222222222222222'// &
        '22222222222222222222222222222222","epoch":3,"seed":1729,"cursor":5,"elig'// &
        'ible":6,"completed":6,"pass":4,"fail":1,"timeout":1,"flaky":0,"infra":0,'// &
        '"running":0,"cancelled":0,"unknown":0,"remaining":0,"ordinary_required":'// &
        '5,"ordinary_passed":4,"ordinary_verified":true,"full_required":6,"full_p'// &
        'assed":4,"fully_verified":false,"ordinary_complete":true,"full_coverage"'// &
        ':false,"green":false}'
    character(len=*), parameter :: GOLDEN_4 = &
        ',"wait_until":"local_gate_green","wait_satisfied":false,"wait_timed_out"'// &
        ':true,"wait_terminal":false,"failure_observed":true,"durable_failure_rep'// &
        'ort":{"action":"failures","events":[]}'

    allocate (request)
    session%session_id = 'owner-1'
    session%driver_path = '/opt/fo "x"\bin/fo'
    session%driver_digest = repeat('a', 64)
    session%driver_size = 123456789012_int64
    request%lane_id = 'lane-a'
    request%gate_required_count = 3
    request%requirement_digest = repeat('b', 64)
    request%event_epoch = 7
    request%input_changed = .true.
    active%identity = repeat('c', 64)
    active%project_root = '/gen/c/project'
    active%driver_digest = repeat('a', 64)
    active%driver_size = 42_int64
    active%model_generation = repeat('d', 64)
    candidate%identity = repeat('e', 64)
    candidate%project_root = '/gen/e/project'
    candidate%driver_digest = repeat('f', 64)
    candidate%driver_size = 43_int64
    names(1) = 'test_a'
    names(2) = 'te"st_b'
    status = gremlin_status_json(session, request, repeat('9', 64), 'testing', &
        active, candidate, repeat('d', 64), 'test_a', names, 11, 20, 1729, 'FAIL', &
        1, '{"generations":2,"bytes":10,"pinned":1,"owners":1,"views":0,'// &
        '"view_bytes":0,"view_bytes_exact":true}', 'line one'//achar(10)//'two')
    readiness%phase = GREMLIN_PHASE_TESTING
    readiness%health = GREMLIN_HEALTH_FAILURE
    readiness%verification_level = GREMLIN_VERIFY_ORDINARY
    readiness%dirty = .true.
    readiness%gate_token = repeat('1', 64)
    readiness%requirement_digest = repeat('b', 64)
    readiness%event_epoch = 7
    readiness%gate_required = 3
    readiness%gate_passed = 2
    readiness%ordinary_required = 5
    readiness%ordinary_passed = 4
    readiness%ordinary_failures = 1
    readiness%full_required = 6
    readiness%full_passed = 4
    readiness%full_failures = 2
    view%generation_id = repeat('c', 64)
    view%inventory_digest = repeat('2', 64)
    view%epoch = 3
    view%seed = 1729
    view%cursor = 5
    view%eligible_count = 6
    view%ordinary_eligible_count = 5
    view%completed_count = 6
    view%pass_count = 4
    view%ordinary_pass_count = 4
    view%fail_count = 1
    view%ordinary_failure_count = 1
    view%timeout_count = 1
    view%running_count = 0
    view%remaining_count = 0
    view%ordinary_complete = .true.
    enriched = gremlin_status_enrich(status, view, readiness, .true.)
    bare = gremlin_status_enrich(status, view, readiness, .false.)
    waited = gremlin_wait_decorate(bare, 'local_gate_green', .false., .true., &
        .false., .true., '{"action":"failures","events":[]}')
    call assert_same(status, GOLDEN_1, 'published lane state')
    call assert_same(enriched, GOLDEN_1(:len(GOLDEN_1) - 1)//GOLDEN_2//GOLDEN_3//'}', &
        'status enriched with readiness and coverage')
    call assert_same(waited, GOLDEN_1(:len(GOLDEN_1) - 1)//GOLDEN_2//GOLDEN_4//'}', &
        'wait decoration with a durable failure report')
    call finish_assertions()

contains

    subroutine assert_same(actual, expected, label)
        character(len=*), intent(in) :: actual, expected, label
        type(json_value_t) :: left, right
        character(len=:), allocatable :: message
        logical :: valid_left, valid_right

        call json_parse(actual, left, valid_left, message)
        call json_parse(expected, right, valid_right, message)
        call assert_true(valid_left .and. valid_right, label//' parses independently')
        call assert_true(same_members(left, right), &
            label//' keeps the previous members, order and values')
        if (.not. same_members(left, right)) write (output_unit, '(a)') actual
    end subroutine assert_same

    recursive logical function same_members(left, right) result(equal)
        type(json_value_t), intent(in) :: left, right
        integer :: i

        equal = left%kind == right%kind
        if (.not. equal) return
        select case (left%kind)
        case (json_object, json_array)
            equal = allocated(left%children) .eqv. allocated(right%children)
            if (.not. equal .or. .not. allocated(left%children)) return
            equal = size(left%children) == size(right%children)
            do i = 1, size(left%children)
                if (.not. equal) return
                if (left%kind == json_object) &
                    equal = left%children(i)%name == right%children(i)%name
                if (equal) equal = same_members(left%children(i), right%children(i))
            end do
        case default
            equal = (allocated(left%text) .eqv. allocated(right%text)) .and. &
                (left%boolean .eqv. right%boolean)
            if (equal .and. allocated(left%text)) equal = left%text == right%text
        end select
    end function same_members

end program test_gremlin_status_json
