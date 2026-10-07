program test_gremlin_readiness
    use, intrinsic :: iso_fortran_env, only: output_unit, int64
    use fo_gremlin_readiness
    use fo_gremlin_lifecycle
    use fo_gremlin_coverage, only: coverage_epoch_t, coverage_open, &
        coverage_record, coverage_view, COVERAGE_OK
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_request, only: gremlin_request_t, parse_request
    use fo_cache, only: HASH_LEN
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0
    call test_partial_gate_and_verification_levels()
    call test_failure_and_generation_freshness()
    call test_gate_token_binding()
    call test_wait_request_parser()
    call test_ordinary_and_full_inventory_counts()
    call test_lifecycle_idempotence_pagination_and_partial_tail()
    write (output_unit, '(a,i0,a,i0,a)') 'Gremlin readiness: ', passed, &
        ' passed, ', failed, ' failed'
    if (failed > 0) stop 1

contains

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label

        if (condition) then
            passed = passed + 1
        else
            failed = failed + 1
            write (output_unit, '(a,a)') 'FAIL: ', label
        end if
    end subroutine check

    subroutine test_partial_gate_and_verification_levels()
        type(gremlin_readiness_input_t) :: input
        type(gremlin_readiness_t) :: readiness

        call exact_gate(input)
        input%ordinary_required = 38
        input%ordinary_passed = 6
        input%full_required = 40
        input%full_passed = 6
        call gremlin_readiness_compute(input, readiness)
        call check(readiness%local_gate_green, &
            'local gate can be green while ordinary coverage is partial')
        call check(readiness%full_required == 40 .and. readiness%full_passed == 6, &
            'exact immutable generation is gate green with only six of forty cases covered')
        call check(readiness%verification_level == GREMLIN_VERIFY_GATE, &
            'partial ordinary coverage remains at gate verification')

        input%ordinary_passed = input%ordinary_required
        input%full_passed = input%ordinary_passed
        call gremlin_readiness_compute(input, readiness)
        call check(readiness%verification_level == GREMLIN_VERIFY_ORDINARY, &
            'ordinary completion advances independently of slow inventory')
        call check(gremlin_wait_satisfied(GREMLIN_WAIT_ORDINARY_VERIFIED, readiness), &
            'ordinary semantic wait observes complete ordinary facts')
        call check(.not. readiness%fully_verified, &
            'ordinary completion does not imply full verification')

        input%full_passed = input%full_required
        call gremlin_readiness_compute(input, readiness)
        call check(readiness%fully_verified .and. &
            readiness%verification_level == GREMLIN_VERIFY_FULL, &
            'complete supported inventory advances to full verification')
        call check(gremlin_wait_satisfied(GREMLIN_WAIT_FULLY_VERIFIED, readiness), &
            'full semantic wait observes full verification')
    end subroutine test_partial_gate_and_verification_levels

    subroutine test_failure_and_generation_freshness()
        type(gremlin_readiness_input_t) :: input
        type(gremlin_readiness_t) :: readiness

        call exact_gate(input)
        input%gate_failure = .true.
        input%failure_observed = .true.
        call gremlin_readiness_compute(input, readiness)
        call check(.not. readiness%local_gate_green, &
            'durable current-generation failure prevents a green gate')
        call check(readiness%health == GREMLIN_HEALTH_FAILURE, &
            'durable failure is visible independently of phase')
        call check(gremlin_wait_satisfied(GREMLIN_WAIT_FAILURE, readiness), &
            'failure wait accepts durable failure state')

        call exact_gate(input)
        input%exact_active_generation = .false.
        input%capture_fresh = .false.
        input%dirty = .true.
        call gremlin_readiness_compute(input, readiness)
        call check(.not. readiness%local_gate_green, &
            'stale generation or dirty capture cannot retain gate green')

        call exact_gate(input)
        input%gate_required = 0
        input%gate_passed = 0
        call gremlin_readiness_compute(input, readiness)
        call check(.not. readiness%local_gate_green, &
            'empty selected gate evidence cannot produce green')

        call exact_gate(input)
        input%phase = GREMLIN_PHASE_QUIESCENT
        input%failure_observed = .true.
        call gremlin_readiness_compute(input, readiness)
        call check(readiness%phase == GREMLIN_PHASE_QUIESCENT .and. &
            readiness%health == GREMLIN_HEALTH_FAILURE, &
            'failure health does not overwrite quiescent phase')
        input%dirty = .true.
        call gremlin_readiness_compute(input, readiness)
        call check(.not. gremlin_wait_satisfied(GREMLIN_WAIT_QUIESCENT, readiness), &
            'unobserved input changes cannot satisfy a historical quiescent wait')

        call exact_gate(input)
        input%full_failures = 1
        call gremlin_readiness_compute(input, readiness)
        call check(.not. readiness%local_gate_green, &
            'a current failure outside required cases clears the local gate')

        call exact_gate(input)
        input%ordinary_required = 0
        input%ordinary_passed = 0
        input%full_required = 6
        input%full_passed = 6
        call gremlin_readiness_compute(input, readiness)
        call check(readiness%fully_verified, &
            'an inventory containing only slow cases can become fully verified')
    end subroutine test_failure_and_generation_freshness

    subroutine test_gate_token_binding()
        type(gremlin_readiness_input_t) :: input
        type(gremlin_readiness_t) :: readiness
        character(len=HASH_LEN) :: original
        integer :: i

        call exact_gate(input)
        call gremlin_readiness_compute(input, readiness)
        original = readiness%gate_token
        call check(len_trim(original) == HASH_LEN, 'green gate provides a versioned token')
        do i = 1, 5
            call exact_gate(input)
            select case (i)
            case (1)
                input%generation = repeat('d', HASH_LEN)
            case (2)
                input%inventory_digest = repeat('d', HASH_LEN)
            case (3)
                input%requirement_digest = repeat('d', HASH_LEN)
            case (4)
                input%event_epoch = 1
            case (5)
                input%owner_session = 'restarted-owner'
            end select
            call gremlin_readiness_compute(input, readiness)
            call check(readiness%gate_token /= original, &
                'each changed token input invalidates the previous identity')
        end do
        input%ordinary_required = 40
        input%ordinary_passed = 40
        input%full_required = 44
        input%full_passed = 44
        input%dirty = .true.
        call gremlin_readiness_compute(input, readiness)
        call check(readiness%verification_level == GREMLIN_VERIFY_NONE .and. &
            .not. readiness%fully_verified .and. len_trim(readiness%gate_token) == 0, &
            'stale full receipts cannot retain verification or a usable gate token')
        call check(.not. gremlin_wait_satisfied(GREMLIN_WAIT_FULLY_VERIFIED, readiness), &
            'full typed wait cannot succeed on stale full receipts')
    end subroutine test_gate_token_binding

    subroutine test_wait_request_parser()
        type(gremlin_request_t) :: request
        character(len=256) :: message
        integer :: ierr

        call parse_request('wait', '{"wait_until":"local-gate-green",'// &
            '"wait_ms":0,"lifecycle_cursor":37}', request, ierr, message)
        call check(ierr == 0 .and. request%wait_until == 'local-gate-green' .and. &
            request%lifecycle_cursor == 37_int64 .and. request%wait_ms == 0, &
            'shared request parser accepts typed wait and lifecycle cursor')
        call parse_request('wait', '{"wait_until":"unknown-condition"}', &
            request, ierr, message)
        call check(ierr /= 0, 'request parser rejects governance language')
        call parse_request('wait', '{"fail_on_failure":true}', request, ierr, message)
        call check(ierr /= 0, 'request parser rejects removed failure alias')
    end subroutine test_wait_request_parser

    subroutine test_ordinary_and_full_inventory_counts()
        type(coverage_epoch_t) :: state
        type(gremlin_coverage_view_t) :: view
        character(len=64) :: generation
        character(len=128) :: names(2)
        character(len=256) :: path, message
        integer :: status, unit

        write (path, '(a,i0)') '/var/tmp/fo-gremlin-coverage-', clock_value()
        generation = repeat('b', 64)
        names = ''
        names(1) = 'test_fast'
        names(2) = 'test_example_slow'
        call coverage_open(trim(path), generation, names, 2, 17, state, status, message)
        call check(status == COVERAGE_OK, 'coverage inventory opens with ordinary and slow')
        if (status /= COVERAGE_OK) return
        call coverage_record(state, 'test_fast', 'PASS', status, message)
        call check(status == COVERAGE_OK, 'ordinary case receipt updates coverage')
        call coverage_view(state, view)
        call check(view%ordinary_eligible_count == 1 .and. &
            view%ordinary_pass_count == 1 .and. view%ordinary_complete, &
            'ordinary counts complete before slow coverage')
        call check(view%eligible_count == 2 .and. .not. view%full_coverage, &
            'full inventory remains incomplete while slow case is unknown')
        call coverage_record(state, 'test_example_slow', 'PASS', status, message)
        call coverage_view(state, view)
        call check(status == COVERAGE_OK .and. view%full_coverage .and. view%green, &
            'full inventory completes only after the slow case passes')
        call remove_file(trim(path))
        call remove_file(trim(path)//'.lock')
    end subroutine test_ordinary_and_full_inventory_counts

    subroutine test_lifecycle_idempotence_pagination_and_partial_tail()
        type(gremlin_lifecycle_event_t) :: event, restarted, conflicting
        type(gremlin_lifecycle_event_t), allocatable :: page(:)
        character(len=256) :: path, message
        character(len=HASH_LEN) :: active, candidate
        character(len=:), allocatable :: json
        integer(int64) :: cursor, next_cursor, tail_cursor
        integer :: ierr, n_events, unit
        logical :: has_more

        write (path, '(a,i0,a)') '/var/tmp/fo-gremlin-lifecycle-', &
            clock_value(), '.jsonl'
        active = repeat('a', 64)
        candidate = repeat('c', 64)
        event%event_type = 'generation_activated'
        event%generation = active
        event%active_generation = active
        event%phase = 'testing'
        event%health = 'healthy'
        event%verification_level = 'gate'
        event%event_id = gremlin_lifecycle_make_id('session-fixture', event)
        call gremlin_lifecycle_encode(event, json, ierr, message)
        call check(ierr == LIFECYCLE_OK, 'typed lifecycle envelope validates')
        call gremlin_lifecycle_append(trim(path), event, ierr, message)
        call check(ierr == LIFECYCLE_OK, 'lifecycle event is durably appended')
        restarted = event
        restarted%event_id = gremlin_lifecycle_make_id('session-fixture', restarted)
        call check(restarted%event_id == event%event_id, &
            'transition IDs remain deterministic after reconstructing state')
        call gremlin_lifecycle_append(trim(path), restarted, ierr, message)
        call check(ierr == LIFECYCLE_OK, 'duplicate transition append is idempotent')

        event%event_type = 'candidate_build_started'
        event%generation = candidate
        event%active_generation = active
        event%candidate_generation = candidate
        event%phase = 'building'
        event%health = 'degraded'
        event%dirty = .true.
        event%event_id = gremlin_lifecycle_make_id('session-fixture', event)
        call gremlin_lifecycle_append(trim(path), event, ierr, message)
        call check(ierr == LIFECYCLE_OK, 'candidate start event is appended')
        call gremlin_lifecycle_read_page(trim(path), 0_int64, 1, 4096_int64, &
            page, n_events, next_cursor, has_more, ierr, message)
        call check(ierr == LIFECYCLE_OK .and. n_events == 1 .and. has_more .and. &
            next_cursor > 0_int64, 'lifecycle page cursor preserves later events')
        cursor = next_cursor
        call gremlin_lifecycle_read_page(trim(path), cursor, 1, 4096_int64, &
            page, n_events, next_cursor, has_more, ierr, message)
        call check(ierr == LIFECYCLE_OK .and. n_events == 1 .and. .not. has_more, &
            'second page returns the next semantic transition')
        if (n_events == 1) then
            call check(page(1)%generation == candidate, &
                'candidate transition names the candidate generation')
            call check(page(1)%candidate_generation == candidate, &
                'candidate transition names its candidate generation')
        end if
        tail_cursor = next_cursor

        open(newunit=unit, file=trim(path), access='stream', form='unformatted', &
            position='append', status='old', action='write', iostat=ierr)
        if (ierr == 0) then
            write (unit, iostat=ierr) '{"partial":'
            close(unit)
        end if
        call check(ierr == 0, 'test writes an interrupted partial journal tail')
        call gremlin_lifecycle_read_page(trim(path), tail_cursor, 8, 4096_int64, &
            page, n_events, next_cursor, has_more, ierr, message)
        call check(ierr == LIFECYCLE_OK .and. n_events == 0 .and. .not. has_more .and. &
            next_cursor == tail_cursor, 'partial tail does not create a phantom event')

        event%event_type = 'local_gate_green'
        event%generation = active
        event%candidate_generation = ''
        event%phase = 'testing'
        event%health = 'healthy'
        event%dirty = .false.
        event%local_gate_green = .true.
        event%gate_required = 1
        event%gate_passed = 1
        event%event_id = gremlin_lifecycle_make_id('session-fixture', event)
        call gremlin_lifecycle_encode(event, json, ierr, message)
        call gremlin_lifecycle_append(trim(path), event, ierr, message)
        call check(ierr == LIFECYCLE_OK, 'append truncates a partial tail before publication')
        call gremlin_lifecycle_read_page(trim(path), tail_cursor, 8, 4096_int64, &
            page, n_events, next_cursor, has_more, ierr, message)
        call check(ierr == LIFECYCLE_OK .and. n_events == 1, &
            'pagination resumes at the next complete lifecycle event')
        if (n_events == 1) call check(page(1)%event_type == 'local_gate_green', &
            'pagination restores the resumed event type')

        conflicting = event
        conflicting%phase = 'quiescent'
        call gremlin_lifecycle_append(trim(path), conflicting, ierr, message)
        call check(ierr == LIFECYCLE_CONFLICT, &
            'a transition ID cannot name different event facts')
        call gremlin_lifecycle_read_page(trim(path), 0_int64, 8, 1_int64, &
            page, n_events, next_cursor, has_more, ierr, message)
        call check(ierr == LIFECYCLE_TOO_LARGE, &
            'oversized first event fails instead of stalling pagination')
        call remove_file(trim(path))
    end subroutine test_lifecycle_idempotence_pagination_and_partial_tail

    subroutine exact_gate(input)
        type(gremlin_readiness_input_t), intent(out) :: input

        input = gremlin_readiness_input_t()
        input%phase = GREMLIN_PHASE_TESTING
        input%exact_active_generation = .true.
        input%capture_fresh = .true.
        input%build_passed = .true.
        input%owner_session = 'fixture-owner'
        input%generation = repeat('a', HASH_LEN)
        input%inventory_digest = repeat('b', HASH_LEN)
        input%requirement_digest = repeat('c', HASH_LEN)
        input%gate_required = 6
        input%gate_passed = 6
    end subroutine exact_gate

    subroutine remove_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios

        open(newunit=unit, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) return
        close(unit, status='delete', iostat=ios)
    end subroutine remove_file

    function clock_value() result(value)
        integer(int64) :: value
        call system_clock(value)
    end function clock_value

end program test_gremlin_readiness
