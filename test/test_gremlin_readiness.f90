program test_gremlin_readiness
    use, intrinsic :: iso_fortran_env, only: output_unit, int64
    use fo_gremlin_readiness
    use fo_gremlin_lifecycle
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0
    call test_promotion_gate_matrix()
    call test_phase_health_orthogonality()
    call test_verification_waits()
    call test_dirty_event_filter()
    call test_typed_lifecycle_journal()
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

    subroutine test_promotion_gate_matrix()
        type(gremlin_gate_evidence_t) :: gates
        type(gremlin_readiness_t) :: status

        gates%exact_active_generation = .true.
        gates%capture_fresh = .true.
        gates%build_passed = .true.
        gates%regression_passed = .true.
        gates%affected_passed = .true.
        gates%mandatory_passed = .true.
        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(status%promotion_ready, 'all exact active generation gates enable readiness')
        call check(gremlin_wait_satisfied(GREMLIN_WAIT_PROMOTION_READY, status), &
            'promotion-ready wait sees all required gates')

        gates%exact_active_generation = .false.
        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(.not. status%promotion_ready, 'stale generation cannot become ready')
        gates%exact_active_generation = .true.
        gates%capture_fresh = .false.
        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(.not. status%promotion_ready, 'stale capture cannot become ready')
        gates%capture_fresh = .true.
        gates%build_passed = .false.
        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(.not. status%promotion_ready, 'missing build pass cannot become ready')
        gates%build_passed = .true.
        gates%regression_passed = .false.
        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(.not. status%promotion_ready, 'regression failure blocks readiness')
        gates%regression_passed = .true.
        gates%affected_passed = .false.
        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(.not. status%promotion_ready, 'affected gate blocks readiness')
        gates%affected_passed = .true.
        gates%mandatory_passed = .false.
        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(.not. status%promotion_ready, 'mandatory gate blocks readiness')
        gates%mandatory_passed = .true.
        gates%gate_failure = .true.
        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(.not. status%promotion_ready, 'gate failure blocks readiness')
        call check(status%health == GREMLIN_HEALTH_FAILURE, &
            'gate failure has failure health')
    end subroutine test_promotion_gate_matrix

    subroutine test_phase_health_orthogonality()
        type(gremlin_gate_evidence_t) :: gates
        type(gremlin_readiness_t) :: status

        gates%exact_active_generation = .true.
        gates%capture_fresh = .true.
        gates%build_passed = .true.
        gates%regression_passed = .true.
        gates%affected_passed = .true.
        gates%mandatory_passed = .true.
        call gremlin_readiness_compute(GREMLIN_PHASE_QUIESCENT, gates, status)
        call check(status%phase == GREMLIN_PHASE_QUIESCENT .and. &
            status%health == GREMLIN_HEALTH_HEALTHY .and. status%promotion_ready, &
            'quiescent phase can retain healthy promotion readiness')
        call check(gremlin_wait_satisfied(GREMLIN_WAIT_QUIESCENT, status), &
            'quiescent wait reads phase independently')
        gates%gate_failure = .true.
        call gremlin_readiness_compute(GREMLIN_PHASE_QUIESCENT, gates, status)
        call check(status%phase == GREMLIN_PHASE_QUIESCENT .and. &
            status%health == GREMLIN_HEALTH_FAILURE, &
            'failure health does not overwrite quiescent phase')
        call check(gremlin_wait_satisfied(GREMLIN_WAIT_FAILURE, status), &
            'failure wait reads health independently')
    end subroutine test_phase_health_orthogonality

    subroutine test_verification_waits()
        type(gremlin_gate_evidence_t) :: gates
        type(gremlin_readiness_t) :: status

        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(status%verification_level == GREMLIN_VERIFY_GATE, &
            'incomplete evidence remains at gate verification')
        call check(.not. gremlin_wait_satisfied(GREMLIN_WAIT_ORDINARY_VERIFIED, status), &
            'ordinary wait rejects an incomplete gate run')
        gates%ordinary_complete = .true.
        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(status%verification_level == GREMLIN_VERIFY_ORDINARY, &
            'ordinary completion advances verification level')
        call check(gremlin_wait_satisfied(GREMLIN_WAIT_ORDINARY_VERIFIED, status), &
            'ordinary wait accepts ordinary completion')
        call check(.not. gremlin_wait_satisfied(GREMLIN_WAIT_FULLY_VERIFIED, status), &
            'full wait does not accept ordinary completion')
        gates%full_complete = .true.
        call gremlin_readiness_compute(GREMLIN_PHASE_TESTING, gates, status)
        call check(status%verification_level == GREMLIN_VERIFY_FULL, &
            'complete inventory advances verification level to full')
        call check(gremlin_wait_satisfied(GREMLIN_WAIT_FULLY_VERIFIED, status), &
            'full wait accepts full inventory completion')
    end subroutine test_verification_waits

    subroutine test_dirty_event_filter()
        call check(gremlin_dirty_event_relevant('src/a.f90'), &
            'Fortran source dirties a generation')
        call check(gremlin_dirty_event_relevant('include/config.inc'), &
            'include dirties a generation')
        call check(gremlin_dirty_event_relevant('fpm.toml'), &
            'manifest dirties a generation')
        call check(.not. gremlin_dirty_event_relevant('README.md'), &
            'unrelated prose does not dirty a generation')
        call check(.not. gremlin_dirty_event_relevant('build/cache.bin'), &
            'generated cache files do not dirty a generation')
    end subroutine test_dirty_event_filter

    subroutine test_typed_lifecycle_journal()
        type(gremlin_lifecycle_event_t) :: event, conflicting
        type(gremlin_lifecycle_event_t), allocatable :: page(:)
        character(len=256) :: path, message
        character(len=:), allocatable :: json
        integer(int64) :: cursor, next_cursor
        integer :: ierr, n_events, unit
        logical :: has_more

        write(path, '(a,i0)') '/var/tmp/fo-gremlin-lifecycle-', clock_value()
        event%event_id = 'session:event:1'
        event%event_type = 'generation_activated'
        event%generation = repeat('a', 64)
        event%phase = 'testing'
        event%health = 'healthy'
        event%verification_level = 'ordinary'
        event%promotion_ready = .true.
        call gremlin_lifecycle_encode(event, json, ierr, message)
        call check(ierr == 0 .and. gremlin_lifecycle_validate(json) == 0, &
            'typed lifecycle envelope encodes and validates')
        call check(gremlin_lifecycle_validate('{"event_version":2}') /= 0, &
            'lifecycle validator rejects unknown envelope version')
        call gremlin_lifecycle_append(trim(path), event, ierr, message)
        call check(ierr == 0, 'typed lifecycle event is durably appended')
        call gremlin_lifecycle_append(trim(path), event, ierr, message)
        call check(ierr == 0, 'identical lifecycle event append is idempotent')
        call gremlin_lifecycle_read_page(trim(path), 0_int64, 8, 4096_int64, &
            page, n_events, next_cursor, has_more, ierr, message)
        call check(ierr == 0 .and. n_events == 1 .and. .not. has_more, &
            'lifecycle page returns one event without duplicating its receipt')
        if (n_events == 1) then
            call check(page(1)%event_type == 'generation_activated' .and. &
                page(1)%promotion_ready, 'lifecycle page restores typed event fields')
        end if
        cursor = next_cursor
        call gremlin_lifecycle_read_page(trim(path), cursor, 8, 4096_int64, &
            page, n_events, next_cursor, has_more, ierr, message)
        call check(ierr == 0 .and. n_events == 0, &
            'lifecycle cursor resumes after the delivered event')
        conflicting = event
        conflicting%phase = 'quiescent'
        call gremlin_lifecycle_append(trim(path), conflicting, ierr, message)
        call check(ierr /= 0, 'event ID cannot be reused for different lifecycle data')
        open(newunit=unit, file=trim(path), status='old', iostat=ierr)
        if (ierr == 0) close(unit, status='delete')
    end subroutine test_typed_lifecycle_journal

    function clock_value() result(value)
        integer(int64) :: value
        call system_clock(value)
    end function clock_value

end program test_gremlin_readiness
