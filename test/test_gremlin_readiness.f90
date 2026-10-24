program test_gremlin_readiness
    use fo_util, only: temporary_root, make_tmpfile
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: output_unit, int64
    use fo_gremlin_readiness
    use fo_gremlin_lifecycle
    use fo_gremlin_coverage, only: coverage_epoch_t, coverage_open, &
        coverage_record, coverage_view, COVERAGE_OK
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_request, only: gremlin_request_t, parse_request
    use fo_gremlin_receipts, only: compute_readiness
    use fo_gremlin_journal, only: journal_append, JOURNAL_OK
    use fo_gremlin_session, only: gremlin_get_session_journal_path
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_release
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_cache, only: HASH_LEN
    implicit none

    interface
        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
        integer(c_int) function c_unsetenv(name) bind(C, name='unsetenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
        end function c_unsetenv
    end interface

    integer :: passed, failed

    passed = 0
    failed = 0
    call test_partial_gate_and_verification_levels()
    call test_failure_and_generation_freshness()
    call test_native_skip_readiness()
    call test_mixed_session_skip_dispositions()
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

    subroutine test_native_skip_readiness()
        type(gremlin_readiness_input_t) :: input
        type(gremlin_readiness_t) :: readiness
        type(gremlin_lifecycle_event_t) :: event
        character(len=:), allocatable :: json
        character(len=256) :: message
        integer :: ierr, at

        call exact_gate(input)
        input%gate_required = 43
        input%gate_passed = 41
        input%gate_skipped = 2
        input%ordinary_required = 43
        input%ordinary_passed = 41
        input%ordinary_skipped = 2
        input%full_required = 43
        input%full_passed = 41
        input%full_skipped = 2
        call gremlin_readiness_compute(input, readiness)
        call check(readiness%fully_verified .and. readiness%local_gate_green .and. &
            readiness%gate_passed == 41 .and. readiness%gate_skipped == 2, &
            'native-parity readiness completes skips without calling them passes')
        input%gate_passed = 0
        input%gate_skipped = 43
        call gremlin_readiness_compute(input, readiness)
        call check(.not. readiness%local_gate_green .and. .not. readiness%fully_verified, &
            'a gate of only native skips is not green')
        input%gate_passed = 41
        input%gate_skipped = 2
        input%full_passed = 0
        input%full_skipped = 43
        input%ordinary_passed = 0
        input%ordinary_skipped = 43
        call gremlin_readiness_compute(input, readiness)
        call check(readiness%local_gate_green .and. .not. readiness%fully_verified, &
            'an inventory of only native skips is not fully verified')
        input%full_passed = 41
        input%full_skipped = 2
        input%ordinary_passed = 41
        input%ordinary_skipped = 2
        input%gate_skipped = 1
        call gremlin_readiness_compute(input, readiness)
        call check(.not. readiness%local_gate_green, &
            'one outstanding gate case remains an obligation')
        input%gate_skipped = 2
        input%failure_observed = .true.
        call gremlin_readiness_compute(input, readiness)
        call check(.not. readiness%local_gate_green .and. .not. readiness%fully_verified, &
            'a skip does not resolve previously observed failure evidence')
        event%event_type = 'coverage_updated'
        event%gate_required = 43
        event%gate_passed = 41
        event%gate_skipped = 2
        event%ordinary_skipped = 2
        event%full_skipped = 2
        event%event_id = gremlin_lifecycle_make_id('skip-fixture', event)
        call gremlin_lifecycle_encode(event, json, ierr, message)
        call check(ierr == LIFECYCLE_OK .and. index(json, '"gate_skipped":2') > 0, &
            'lifecycle events retain distinct native skip counts')
        at = index(json, ',"gate_skipped":')
        call check(at > 0, 'new lifecycle event carries additive skip counters')
        if (at > 0) then
            json = json(:at - 1)//'}'
            call check(gremlin_lifecycle_validate(json) == LIFECYCLE_OK, &
                'historical lifecycle events without skip fields remain valid')
            json = json(:len(json) - 1)//',"gate_skipped":""}'
            call check(gremlin_lifecycle_validate(json) == LIFECYCLE_INVALID, &
                'a present malformed skip count is not treated as absent')
        end if
        event%full_skipped = -1
        call gremlin_lifecycle_encode(event, json, ierr, message)
        call check(ierr == LIFECYCLE_INVALID, 'negative lifecycle skip counts are invalid')
    end subroutine test_native_skip_readiness

    subroutine test_mixed_session_skip_dispositions()
        type(gremlin_session_t) :: session
        type(coverage_epoch_t) :: epoch
        type(gremlin_coverage_view_t) :: view
        type(gremlin_readiness_t) :: readiness
        character(len=*), parameter :: generation = repeat('a', HASH_LEN)
        character(len=*), parameter :: requirement = repeat('c', HASH_LEN)
        character(len=16), parameter :: names(1) = ['test_mixed_gate ']
        character(len=512) :: project, journal, ledger, coverage_path
        character(len=256) :: message
        character(len=32) :: lane
        character(len=4) :: current, previous, current_outcome, previous_outcome
        character(len=:), allocatable :: status_text, shared_fields, record, old_state
        integer :: trial, ierr, release_error, env_status, env_length
        logical :: all_released

        call make_tmpfile('fo-readiness-mixed-session', project)
        call fs_make_dir(trim(project), ierr)
        call check(ierr == 0, 'creates an isolated mixed-session project')
        if (ierr /= 0) return
        call get_environment_variable('FO_GREMLIN_STATE_DIR', &
            length=env_length, status=env_status)
        if (env_status == 0) then
            allocate(character(len=env_length) :: old_state)
            call get_environment_variable('FO_GREMLIN_STATE_DIR', old_state)
        end if
        ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(project)//'/state'//c_null_char, 1_c_int)
        call check(ierr == 0, 'isolates all mixed-session state and journals')
        if (ierr /= 0) then
            call fs_remove_tree(trim(project))
            return
        end if
        all_released = .true.
        do trial = 1, 2
            write(lane, '(a,i1)') 'mixed-skip-', trial
            call gremlin_session_acquire(trim(project), trim(lane), session, &
                ierr, message)
            call check(ierr == 0 .and. session%owner, &
                'acquires the isolated mixed-session fixture lane')
            if (ierr /= 0 .or. .not. session%owner) then
                all_released = .false.
                cycle
            end if
            fixture: block
                call gremlin_get_session_journal_path(trim(project), trim(lane), &
                    session%session_id, journal, ierr, message)
                call check(ierr == 0, 'resolves the current-session journal')
                if (ierr /= 0) exit fixture
                ledger = session%state_dir//'/campaign-journal.jsonl'
                coverage_path = session%state_dir//'/coverage-'//generation//'.state'
                current = 'PASS'
                previous = 'SKIP'
                current_outcome = 'pass'
                previous_outcome = 'skip'
                if (trial == 2) then
                    current = 'SKIP'
                    previous = 'PASS'
                    current_outcome = 'skip'
                    previous_outcome = 'pass'
                end if
                call coverage_open(trim(coverage_path), generation, names, 1, &
                    17, epoch, ierr, message)
                call check(ierr == COVERAGE_OK, 'opens mixed-session coverage')
                if (ierr /= COVERAGE_OK) exit fixture
                call coverage_record(epoch, trim(names(1)), current, ierr, message)
                call check(ierr == COVERAGE_OK, 'records the current disposition')
                if (ierr /= COVERAGE_OK) exit fixture
                call coverage_view(epoch, view)
                status_text = '{"state":"quiescent","active_generation":"'// &
                    generation//'","candidate_generation":"'//generation// &
                    '","requirement_digest":"'//requirement// &
                    '","event_epoch":0,"gate_required":1,"input_changed":false}'
                shared_fields = ',"generation":"'//generation// &
                    '","case_id":"'//trim(names(1))// &
                    '","gate_required":true,"requirement_digest":"'//requirement//'"'
                record = '{"completion_id":"build","generation":"'// &
                    generation//'","case_id":"<build>",'// &
                    '"status":"BUILD_PASS","outcome":"pass"}'
                call journal_append(trim(journal), 'build', record, ierr, message)
                call check(ierr == JOURNAL_OK, 'records a current build pass')
                if (ierr /= JOURNAL_OK) exit fixture
                record = '{"completion_id":"older-gate",'// &
                    '"session_id":"older-owner"'//shared_fields// &
                    ',"status":"'//previous//'","outcome":"'//previous_outcome//'"}'
                call journal_append(trim(ledger), 'older-gate', record, ierr, message)
                call check(ierr == JOURNAL_OK, 'records the older gate disposition')
                if (ierr /= JOURNAL_OK) exit fixture
                record = '{"completion_id":"current-gate","session_id":"'// &
                    session%session_id//'"'//shared_fields// &
                    ',"status":"'//current//'","outcome":"'//current_outcome//'"}'
                call journal_append(trim(journal), 'current-gate', record, &
                    ierr, message)
                call check(ierr == JOURNAL_OK, 'records the current gate disposition')
                if (ierr /= JOURNAL_OK) exit fixture
                call compute_readiness(session, status_text, view, .true., &
                    readiness, ierr, message, freshness_verified=.true.)
                ! With one gate case, a current SKIP leaves no pass and is not green.
                call check(ierr == 0 .and. &
                    (readiness%fully_verified .eqv. trial == 1) .and. &
                    readiness%gate_required == 1 .and. &
                    readiness%gate_passed == 2 - trial .and. &
                    readiness%gate_skipped == trial - 1, &
                    'current '//current//' overrides older '//previous// &
                    ' for one deduplicated gate case')
                record = '{"completion_id":"older-failure",'// &
                    '"session_id":"older-owner"'//shared_fields// &
                    ',"status":"FAIL","outcome":"fail"}'
                call journal_append(trim(ledger), 'older-failure', record, &
                    ierr, message)
                call check(ierr == JOURNAL_OK, &
                    'records a genuine same-generation failure')
                if (ierr /= JOURNAL_OK) exit fixture
                call compute_readiness(session, status_text, view, .true., &
                    readiness, ierr, message, freshness_verified=.true.)
                call check(ierr == 0 .and. readiness%failure_observed .and. &
                    .not. readiness%local_gate_green .and. &
                    .not. readiness%fully_verified, &
                    'current '//current//' cannot clear the older generation failure')
            end block fixture
            call gremlin_session_release(session, release_error, message)
            call check(release_error == 0, 'releases the mixed-session fixture lane')
            all_released = all_released .and. release_error == 0
        end do
        if (allocated(old_state)) then
            ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
                old_state//c_null_char, 1_c_int)
        else
            ierr = c_unsetenv('FO_GREMLIN_STATE_DIR'//c_null_char)
        end if
        call check(ierr == 0, 'restores the preceding Gremlin state directory')
        if (all_released) call fs_remove_tree(trim(project))
    end subroutine test_mixed_session_skip_dispositions

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

        write (path, '(a,i0)') temporary_root()//'/fo-gremlin-coverage-', clock_value()
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

        write (path, '(a,i0,a)') temporary_root()//'/fo-gremlin-lifecycle-', &
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
