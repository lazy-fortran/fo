module fo_gremlin_supervisor
    !! The Gremlin request adapter shared by the CLI and MCP: it starts or
    !! stops the lane owner (fo_gremlin_owner), answers status, wait, events
    !! and failures from the published state and receipts, and runs one-off
    !! reproductions of retained generations.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_build_backend, only: BACKEND_CMAKE, backend_t, detect_backend, cmake_configure
    use fo_cache, only: HASH_LEN
    use fo_driver, only: driver_pin_t, driver_pin_current, driver_pin_existing
    use fo_exec_service, only: exec_job_t, exec_start, exec_poll, exec_cancel, &
        exec_clock_ms, EXEC_UNCANCELLED
    use fo_fs, only: fs_make_dir, fs_sleep_ms
    use fo_gremlin_campaign, only: discover_campaign, case_wall_timeout, &
        build_command, case_command, append_execution_provenance, &
        acquire_heavy_work_slot, release_heavy_work_slot, release_generation_lease, &
        request_policy_key, push_scope_args, log_path, gremlin_attach_response
    use fo_gremlin_context, only: generation_inventory_restore, common_generation_cas_root
    use fo_gremlin_coverage, only: coverage_read_view_path, COVERAGE_OK, &
        COVERAGE_NOT_FOUND
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release
    use fo_gremlin_generation, only: generation_t, generation_driver_identity
    use fo_gremlin_journal, only: journal_read_page, journal_read_all, &
        journal_record_t, JOURNAL_OK
    use fo_gremlin_lifecycle, only: gremlin_freshness_check, gremlin_lifecycle_event_t, &
        gremlin_lifecycle_read_page, LIFECYCLE_OK
    use fo_gremlin_log_text, only: gremlin_retain_build_log, gremlin_log_tail
    use fo_gremlin_owner, only: run_owner
    use fo_gremlin_readiness, only: gremlin_readiness_t, gremlin_wait_satisfied, &
        gremlin_wait_code, GREMLIN_PHASE_STARTING, GREMLIN_PHASE_BUILDING, &
        GREMLIN_PHASE_TESTING, GREMLIN_PHASE_QUIESCENT, GREMLIN_PHASE_STOPPED, &
        GREMLIN_PHASE_FAILED, GREMLIN_HEALTH_FAILURE, GREMLIN_VERIFY_ORDINARY, &
        GREMLIN_VERIFY_FULL
    use fo_gremlin_receipts, only: NAME_LEN, PATH_LEN, record_receipt, &
        compute_readiness, lifecycle_path_for_session, status_is_failure, &
        runner_case_outcome
    use fo_gremlin_request, only: gremlin_request_t, parse_request, is_hex_digest, &
        gremlin_json_field
    use fo_gremlin_session, only: gremlin_resolve_read_session, &
        gremlin_load_terminal_session, gremlin_stage_recovery_journal
    use fo_gremlin_state, only: gremlin_session_t, gremlin_lease_t, &
        gremlin_lease_group_t, gremlin_session_acquire, gremlin_session_read, &
        gremlin_session_release, gremlin_session_state_dir, &
        gremlin_session_request_stop, gremlin_session_stop_requested, &
        gremlin_generation_lease_acquire_at, gremlin_generation_pin_at, &
        GREMLIN_STATE_TEXT_MAX
    use fo_gremlin_status_json, only: gremlin_status_enrich, gremlin_wait_decorate, &
        gremlin_summary_json, gremlin_events_json, gremlin_failure_json, &
        simple_response => gremlin_simple_response, &
        error_response => gremlin_error_response
    use fo_json_object, only: json_value_raw
    use fo_process, only: argv_push, process_poll_pid, process_scope_t, &
        process_scope_begin, process_scope_end
    use fo_util, only: int_text
    use fx_dag, only: MAX_NODES
    use fx_json_build, only: json_builder_t, json_new, json_escape_string, &
        array_open => json_array_start, array_close => json_array_end
    implicit none
    private

    public :: gremlin_handle, runner_case_outcome

contains

    subroutine gremlin_handle(action, project_dir, request_json, response_json, exitcode)
        character(len=*), intent(in) :: action, project_dir, request_json
        character(len=:), allocatable, intent(out) :: response_json
        integer, intent(out) :: exitcode

        type(gremlin_request_t), allocatable :: request
        type(gremlin_session_t) :: session
        character(len=PATH_LEN) :: error_message, owner_start
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=128) :: session_id
        integer :: ierr, owner_pid

        response_json = ''
        exitcode = 0
        allocate (request)
        call parse_request(action, request_json, request, ierr, error_message)
        if (ierr /= 0) then
            call error_response(action, trim(error_message), response_json)
            exitcode = 2
            return
        end if
        if (len_trim(project_dir) == 0) then
            call error_response(action, 'project directory is required', response_json)
            exitcode = 2
            return
        end if

        select case (trim(action))
        case ('start', 'gremlin_start')
            call handle_start(project_dir, request, response_json, exitcode)
        case ('run')
            call run_owner(project_dir, request, response_json, exitcode)
        case ('status')
            call handle_status(project_dir, request, response_json, exitcode)
        case ('events')
            call handle_events(project_dir, request, response_json, exitcode)
        case ('wait')
            call handle_wait(project_dir, request, response_json, exitcode)
        case ('failures')
            call handle_failures(project_dir, request, response_json, exitcode)
        case ('reproduce')
            call handle_reproduce(project_dir, request, response_json, exitcode)
        case ('stop')
            call gremlin_session_read(project_dir, trim(request%lane_id), &
                session_id, owner_pid, owner_start, status_text, ierr, error_message)
            if (len_trim(request%session_id) == 0 .and. ierr == 0) then
                request%session_id = trim(session_id)
            end if
            if (len_trim(request%session_id) > 0 .and. ierr == 0) then
                if (trim(request%session_id) == trim(session_id)) then
                    call gremlin_session_request_stop(project_dir, &
                        trim(request%lane_id), trim(request%session_id), &
                        ierr, error_message)
                else
                    call gremlin_load_terminal_session(project_dir, request%lane_id, &
                        trim(request%session_id), session, status_text, ierr, error_message)
                    if (ierr == 0) then
                        call simple_response(action, request%lane_id, &
                            request%session_id, 'stopped', response_json)
                        return
                    end if
                end if
            else if (len_trim(request%session_id) > 0) then
                call gremlin_load_terminal_session(project_dir, request%lane_id, &
                    trim(request%session_id), session, status_text, ierr, error_message)
                if (ierr == 0) then
                    call simple_response(action, request%lane_id, &
                        request%session_id, 'stopped', response_json)
                    return
                end if
            end if
            if (ierr /= 0) then
                call error_response(action, trim(error_message), response_json)
                exitcode = 2
            else
                call simple_response(action, request%lane_id, request%session_id, &
                    'stopping', response_json)
            end if
        case default
            call error_response(action, 'unknown Gremlin action', response_json)
            exitcode = 2
        end select
    end subroutine gremlin_handle


    subroutine handle_start(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        type(driver_pin_t) :: driver_pin
        type(exec_job_t) :: launcher
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=PATH_LEN) :: message, owner_start, log_file
        character(len=128) :: session_id
        character(len=HASH_LEN) :: stored_policy
        integer :: ierr, owner_pid, spawn_exit, i, n_args
        integer, parameter :: READY_WAIT_POLLS = 1200
        logical :: ready, owner_exited
        character(len=:), allocatable :: packed

        exitcode = 0
        call gremlin_session_acquire(project_dir, trim(request%lane_id), &
            session, ierr, message)
        if (ierr /= 0) then
            call error_response('start', trim(message), response)
            exitcode = 2
            return
        end if
        if (session%attached) then
            call gremlin_attach_response('start', project_dir, request, response, &
                exitcode)
            return
        end if
        if (allocated(session%recovered_session_id)) then
            if (len_trim(session%recovered_session_id) > 0) then
                call gremlin_stage_recovery_journal(project_dir, session, ierr, message)
                if (ierr /= 0) then
                    call gremlin_session_release(session, spawn_exit, owner_start)
                    call error_response('start', &
                        'cannot preserve receipts from the previous Gremlin owner: '// &
                        trim(message), response)
                    exitcode = 2
                    return
                end if
            end if
        end if
        call driver_pin_current(session%state_dir, driver_pin, ierr, message)
        if (ierr /= 0) then
            call gremlin_session_release(session, spawn_exit, owner_start)
            call error_response('start', 'cannot pin the running fo driver: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        call gremlin_session_release(session, ierr, message)
        if (ierr /= 0) then
            call error_response('start', &
                'cannot reserve Gremlin owner: '//trim(message), response)
            exitcode = 2
            return
        end if
        call fs_make_dir(trim(session%state_dir)//'/logs')
        call log_path(session, 'launcher.log', log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(driver_pin%path))
        call argv_push(packed, n_args, 'gremlin')
        call argv_push(packed, n_args, 'run')
        call argv_push(packed, n_args, '--dir')
        call argv_push(packed, n_args, trim(project_dir))
        call argv_push(packed, n_args, '--lane')
        call argv_push(packed, n_args, trim(request%lane_id))
        call argv_push(packed, n_args, '--random')
        call argv_push(packed, n_args, int_text(request%random_count))
        call argv_push(packed, n_args, '--seed')
        call argv_push(packed, n_args, int_text(request%seed))
        call argv_push(packed, n_args, '--timeout-seconds')
        call argv_push(packed, n_args, int_text(request%timeout_seconds))
        call argv_push(packed, n_args, '--campaign-seconds')
        call argv_push(packed, n_args, int_text(request%campaign_seconds))
        if (request%jobs > 0) then
            call argv_push(packed, n_args, '--jobs')
            call argv_push(packed, n_args, int_text(request%jobs))
        end if
        if (request%only_changed) call argv_push(packed, n_args, '--only-changed')
        if (request%shuffle) call argv_push(packed, n_args, '--shuffle')
        do i = 1, request%n_targets
            call argv_push(packed, n_args, '--target')
            call argv_push(packed, n_args, trim(request%targets(i)))
        end do
        call push_scope_args(packed, n_args, request)
        call exec_start(launcher, project_dir, packed, n_args, log_file, spawn_exit, &
            'FO_DISABLE_SELF_REFRESH=1;FO_SELF_REFRESH=0')
        if (spawn_exit /= 0) then
            call error_response('start', 'cannot launch the Gremlin owner '// &
                '(process setup error '//int_text(spawn_exit)//')', response)
            exitcode = 2
            return
        end if
        ! The child obtains the lane lock. Wait only for its ready publication;
        ! campaign progress never depends on this caller staying connected. A
        ! loaded host can need several seconds before the owner publishes, so
        ! give up only when the owner exited or after READY_WAIT_POLLS.
        ready = .false.
        owner_exited = .false.
        do i = 1, READY_WAIT_POLLS
            call fs_sleep_ms(50)
            call gremlin_session_read(project_dir, trim(request%lane_id), session_id, &
                owner_pid, owner_start, status_text, ierr, message)
            if (ierr /= 0 .or. len_trim(session_id) == 0 .or. &
                len_trim(status_text) == 0) then
                call exec_poll(launcher, owner_exited, spawn_exit)
                if (owner_exited) exit
                cycle
            end if
            call gremlin_json_field(status_text, 'policy_key', stored_policy)
            if (trim(stored_policy) /= request_policy_key(request)) then
                call exec_cancel(launcher, spawn_exit)
                if (spawn_exit /= 0) then
                    call error_response('start', 'conflicting owner; failed to release '// &
                        'this launcher process', response)
                    exitcode = 2
                    return
                end if
                call error_response('start', &
                    'a concurrent Gremlin start selected an incompatible policy', response)
                exitcode = 2
                return
            end if
            ready = .true.
            exit
        end do
        if (.not. ready) then
            spawn_exit = 0
            if (.not. owner_exited) call exec_cancel(launcher, spawn_exit)
            if (owner_exited) then
                message = 'Gremlin owner exited before publishing ready state'
            else if (spawn_exit == 0) then
                message = 'Gremlin owner did not publish ready state'
            else
                message = 'Gremlin owner did not publish ready state; '// &
                    'launcher cancellation failed'
            end if
            call error_response('start', trim(message)//'; owner log '// &
                trim(log_file)//': '//gremlin_log_tail(log_file, 1500), response)
            exitcode = 2
            return
        end if
        call simple_response('start', request%lane_id, trim(session_id), 'running', response)
    end subroutine handle_start

    subroutine handle_status(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        character(len=65536) :: status_text
        character(len=PATH_LEN) :: message
        type(gremlin_session_t) :: session
        type(gremlin_coverage_view_t) :: coverage_view
        type(gremlin_readiness_t) :: readiness
        character(len=HASH_LEN) :: generation_id
        character(len=PATH_LEN) :: coverage_path, coverage_state_dir
        character(len=:), allocatable :: enriched_status
        integer :: owner_pid, ierr
        logical :: is_live, have_coverage, freshness_verified
        character(len=PATH_LEN) :: freshness_path, owner_start
        character(len=65536) :: fresh_status
        character(len=128) :: fresh_session

        exitcode = 0
        call gremlin_resolve_read_session(project_dir, request, session, status_text, &
            owner_pid, is_live, ierr, message)
        if (ierr /= 0) then
            call error_response('status', trim(message), response)
            exitcode = 2
            return
        end if
        if (is_live) call poll_launcher(owner_pid)
        generation_id = ''
        coverage_view = gremlin_coverage_view_t()
        have_coverage = .false.
        call gremlin_json_field(status_text, 'active_generation', generation_id)
        if (len_trim(generation_id) == HASH_LEN) then
            call gremlin_session_state_dir(project_dir, request%lane_id, &
                coverage_state_dir, ierr, message)
            if (ierr /= 0) then
                call error_response('status', trim(message), response)
                exitcode = 2
                return
            end if
            coverage_path = trim(coverage_state_dir)//'/coverage-'// &
                generation_id//'.state'
            call coverage_read_view_path(trim(coverage_path), generation_id, &
                coverage_view, ierr, message)
            if (ierr == COVERAGE_OK) then
                have_coverage = .true.
            else if (ierr /= COVERAGE_NOT_FOUND) then
                call error_response('status', trim(message), response)
                exitcode = 2
                return
            end if
        end if
        call compute_readiness(session, trim(status_text), coverage_view, &
            have_coverage, readiness, ierr, message)
        if (ierr /= 0) then
            call error_response('status', trim(message), response)
            exitcode = 2
            return
        end if
        if (is_live .and. (readiness%local_gate_green .or. &
            (readiness%phase == GREMLIN_PHASE_QUIESCENT .and. &
            .not. readiness%dirty))) then
            freshness_path = trim(coverage_state_dir)//'/freshness-'// &
                trim(session%session_id)
            call gremlin_freshness_check(trim(freshness_path), freshness_verified)
            if (freshness_verified) then
                call gremlin_session_read(project_dir, request%lane_id, fresh_session, &
                    owner_pid, owner_start, fresh_status, ierr, message)
                freshness_verified = ierr == 0 .and. &
                    trim(fresh_session) == trim(session%session_id)
                if (freshness_verified) then
                    status_text = fresh_status
                    ! The freshness handshake can advance the owner from testing
                    ! to quiescence. Its refreshed status needs refreshed coverage,
                    ! including the new generation if activation occurred meanwhile.
                    generation_id = ''
                    coverage_view = gremlin_coverage_view_t()
                    have_coverage = .false.
                    call gremlin_json_field(status_text, 'active_generation', &
                        generation_id)
                    if (len_trim(generation_id) == HASH_LEN) then
                        coverage_path = trim(coverage_state_dir)//'/coverage-'// &
                            generation_id//'.state'
                        call coverage_read_view_path(trim(coverage_path), generation_id, &
                            coverage_view, ierr, message)
                        if (ierr == COVERAGE_OK) then
                            have_coverage = .true.
                        else if (ierr /= COVERAGE_NOT_FOUND) then
                            call error_response('status', trim(message), response)
                            exitcode = 2
                            return
                        end if
                    end if
                end if
            end if
            call compute_readiness(session, trim(status_text), coverage_view, &
                have_coverage, readiness, ierr, message, freshness_verified)
            if (ierr /= 0) then
                call error_response('status', trim(message), response)
                exitcode = 2
                return
            end if
        end if
        enriched_status = gremlin_status_enrich(trim(status_text), coverage_view, &
            readiness, have_coverage)
        if (trim(request%detail) == 'summary') then
            call compact_summary(enriched_status, session, 'status', &
                response, ierr, message)
        else
            call response_with_events('status', enriched_status, session, request, &
                .false., response, ierr, message)
        end if
        if (ierr /= 0) then
            call error_response('status', trim(message), response)
            exitcode = 2
            return
        end if
        exitcode = 0
    end subroutine handle_status

    subroutine compact_summary(status_text, session, action, response, &
            ierr, message)
        !! The summary document; with failure evidence it names this session's
        !! newest failure receipt of the reported generation.
        character(len=*), intent(in) :: status_text, action
        type(gremlin_session_t), intent(in) :: session
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        character(len=4096) :: value
        character(len=PATH_LEN) :: journal_state_dir, log_file
        character(len=HASH_LEN) :: generation
        character(len=NAME_LEN) :: field, case_id
        character(len=32) :: health, last_outcome
        integer :: failure_count, ios, i
        logical :: failure_evidence
        character(len=:), allocatable :: latest

        ierr = 0
        message = ''
        generation = ''
        health = ''
        last_outcome = ''
        call gremlin_json_field(status_text, 'active_generation', generation, .true.)
        if (len_trim(generation) == 0) &
            call gremlin_json_field(status_text, 'candidate_generation', generation, &
                .true.)
        call gremlin_json_field(status_text, 'health', health, .true.)
        call gremlin_json_field(status_text, 'last_outcome', last_outcome, .true.)
        failure_evidence = trim(health) == 'failure' .or. &
            status_is_failure(trim(last_outcome))
        call gremlin_json_field(status_text, 'ordinary_failures', value, .true.)
        read(value, *, iostat=ios) failure_count
        if (ios == 0) failure_evidence = failure_evidence .or. failure_count > 0
        call gremlin_json_field(status_text, 'full_failures', value, .true.)
        read(value, *, iostat=ios) failure_count
        if (ios == 0) failure_evidence = failure_evidence .or. failure_count > 0
        latest = ''
        if (failure_evidence) then
            call gremlin_session_state_dir(trim(session%project_key), &
                trim(session%lane_id), journal_state_dir, ierr, message)
            if (ierr /= 0) return
            if (len_trim(generation) == HASH_LEN) then
                call journal_read_all(trim(journal_state_dir)// &
                    '/campaign-journal.jsonl', records, ierr, message)
                if (ierr /= JOURNAL_OK) return
                do i = 1, size(records)
                    call gremlin_json_field(records(i)%json, 'session_id', field)
                    if (trim(field) /= trim(session%session_id)) cycle
                    call gremlin_json_field(records(i)%json, 'generation', field)
                    if (trim(field) /= trim(generation)) cycle
                    call gremlin_json_field(records(i)%json, 'status', field)
                    if (.not. status_is_failure(trim(field))) cycle
                    call gremlin_json_field(records(i)%json, 'case_id', case_id)
                    call gremlin_json_field(records(i)%json, 'log_path', log_file)
                    latest = gremlin_failure_json(generation, case_id, field, log_file)
                end do
            end if
        end if
        response = gremlin_summary_json(status_text, action, latest)
    end subroutine compact_summary


    subroutine handle_events(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        call handle_status(project_dir, request, response, exitcode)
        if (exitcode == 0) call replace_action(response, 'events')
    end subroutine handle_events

    subroutine handle_wait(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        character(len=PATH_LEN) :: message
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=32) :: state_name, health, phase_name, verification, fact
        character(len=:), allocatable :: status_json, wait_name, decorated
        character(len=:), allocatable :: durable_failure_report
        type(gremlin_readiness_t) :: readiness
        type(gremlin_request_t) :: poll_request, failure_request
        type(gremlin_session_t) :: session
        integer :: ierr, until_kind, elapsed, sleep_ms, failure_exitcode
        integer :: owner_pid
        integer(int64) :: started_ms, now_ms, next_cursor, next_lifecycle_cursor
        logical :: satisfied, failed, timed_out, terminal, has_events
        logical :: is_live

        poll_request = request
        poll_request%detail = 'full'
        until_kind = gremlin_wait_code(request%wait_until)
        wait_name = trim(request%wait_until)
        if (until_kind == 0) wait_name = 'events'
        satisfied = .false.
        failed = .false.
        timed_out = .false.
        terminal = .false.
        elapsed = 0
        started_ms = exec_clock_ms()
        do
            call handle_status(project_dir, poll_request, status_json, ierr)
            if (ierr /= 0) then
                response = status_json
                exitcode = ierr
                return
            end if
            call gremlin_json_field(status_json, 'health', health)
            call gremlin_json_field(status_json, 'phase', phase_name)
            call gremlin_json_field(status_json, 'state', state_name)
            readiness = gremlin_readiness_t()
            if (trim(health) == 'failure') readiness%health = GREMLIN_HEALTH_FAILURE
            select case (trim(phase_name))
            case ('starting')
                readiness%phase = GREMLIN_PHASE_STARTING
            case ('building')
                readiness%phase = GREMLIN_PHASE_BUILDING
            case ('testing')
                readiness%phase = GREMLIN_PHASE_TESTING
            case ('quiescent')
                readiness%phase = GREMLIN_PHASE_QUIESCENT
            case ('stopped')
                readiness%phase = GREMLIN_PHASE_STOPPED
            case ('failed')
                readiness%phase = GREMLIN_PHASE_FAILED
            end select
            call gremlin_json_field(status_json, 'local_gate_green', fact)
            readiness%local_gate_green = trim(fact) == 'true'
            call gremlin_json_field(status_json, 'fully_verified', fact)
            readiness%fully_verified = trim(fact) == 'true'
            call gremlin_json_field(status_json, 'dirty', fact)
            readiness%dirty = trim(fact) == 'true'
            call gremlin_json_field(status_json, 'verification_level', verification)
            select case (trim(verification))
            case ('ordinary')
                readiness%verification_level = GREMLIN_VERIFY_ORDINARY
            case ('full')
                readiness%verification_level = GREMLIN_VERIFY_FULL
            end select
            failed = readiness%health == GREMLIN_HEALTH_FAILURE
            call read_json_cursor(status_json, 'next_cursor', next_cursor, ierr)
            call read_json_cursor(status_json, 'next_lifecycle_cursor', &
                next_lifecycle_cursor, ierr)
            if (ierr /= 0) then
                response = '{"error":"malformed cursor in Gremlin status"}'
                exitcode = 1
                return
            end if
            has_events = next_cursor > poll_request%cursor .or. &
                next_lifecycle_cursor > poll_request%lifecycle_cursor

            if (until_kind > 0) then
                satisfied = gremlin_wait_satisfied(until_kind, readiness)
            else
                satisfied = has_events
            end if
            terminal = trim(state_name) == 'stopped' .or. trim(state_name) == 'error'
            if (satisfied .or. terminal) exit

            now_ms = exec_clock_ms()
            elapsed = int(max(0_int64, now_ms - started_ms))
            if (elapsed >= request%wait_ms) then
                timed_out = .true.
                exit
            end if
            sleep_ms = min(100, request%wait_ms - elapsed)
            if (sleep_ms > 0) call fs_sleep_ms(sleep_ms)
        end do

        call replace_action(status_json, 'wait')
        if (len(status_json) < 2) then
            call error_response('wait', 'status query returned an empty response', response)
            exitcode = 2
            return
        end if
        durable_failure_report = ''
        if (failed .and. trim(request%detail) == 'full') then
            failure_request = request
            failure_request%cursor = 0_int64
            failure_request%lifecycle_cursor = 0_int64
            failure_request%max_records = 128
            call handle_failures(project_dir, failure_request, &
                durable_failure_report, failure_exitcode)
            if (failure_exitcode /= 0) durable_failure_report = ''
        end if
        decorated = gremlin_wait_decorate(status_json, wait_name, satisfied, &
            timed_out, terminal, failed, durable_failure_report)
        if (trim(request%detail) == 'summary') then
            call gremlin_resolve_read_session(project_dir, request, session, &
                status_text, owner_pid, is_live, ierr, message)
            if (ierr /= 0) then
                call error_response('wait', trim(message), response)
                exitcode = 2
                return
            end if
            call compact_summary(decorated, session, 'wait', response, &
                ierr, message)
            if (ierr /= 0) then
                call error_response('wait', trim(message), response)
                exitcode = 2
                return
            end if
        else
            response = decorated
        end if
        exitcode = 0
    end subroutine handle_wait

    subroutine read_json_cursor(json, name, value, ierr)
        !! An absent cursor is 0; a malformed, negative or oversized one sets ierr.
        character(len=*), intent(in) :: json, name
        integer(int64), intent(out) :: value
        integer, intent(inout) :: ierr
        character(len=64) :: raw
        integer :: ios, status

        value = 0_int64
        call gremlin_json_field(json, name, raw, status=status)
        if (status == 1) return
        read(raw, *, iostat=ios) value
        if (status /= 0 .or. ios /= 0 .or. value < 0_int64) then
            value = 0_int64
            ierr = 1
        end if
    end subroutine read_json_cursor

    subroutine handle_failures(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        character(len=PATH_LEN) :: message
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        integer :: owner_pid, ierr
        logical :: is_live

        exitcode = 0
        call gremlin_resolve_read_session(project_dir, request, session, status_text, &
            owner_pid, is_live, ierr, message)
        if (ierr /= 0) then
            call error_response('failures', trim(message), response)
            exitcode = 2
            return
        end if
        if (is_live) call poll_launcher(owner_pid)
        call response_with_events('failures', status_text, session, request, .true., &
            response, ierr, message)
        if (ierr /= 0) then
            call error_response('failures', trim(message), response)
            exitcode = 2
        end if
    end subroutine handle_failures

    subroutine handle_reproduce(project_dir, request, response, exitcode)
        !! Build a retained generation in a private view and run one of its
        !! cases again; the reproduction receipt never credits coverage.
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        type(gremlin_lease_t) :: lease
        type(gremlin_lease_group_t) :: work_slot
        type(process_scope_t) :: scope
        type(generation_t) :: generation
        type(execution_view_t) :: case_view, build_view
        type(driver_pin_t) :: pin
        type(gremlin_request_t) :: selection_request
        type(backend_t) :: backend
        type(exec_job_t) :: job
        character(len=PATH_LEN) :: message, log_file, build_log, driver_state_dir, detail
        character(len=NAME_LEN) :: selected(MAX_NODES)
        character(len=HASH_LEN) :: identity
        character(len=16) :: outcome
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=128) :: view_owner
        character(len=:), allocatable :: packed, env
        integer :: owner_pid, ierr, n_selected, mandatory_count, seed, n_args, status
        integer :: sequence, timeout, test_exit
        integer(int64) :: clock_count
        logical :: have_lease, have_slot, have_scope, stop_requested, stopped
        logical :: exists, is_live, spawned, slot_busy
        integer(int64) :: slot_wait_started

        exitcode = 2
        have_lease = .false.
        have_slot = .false.
        have_scope = .false.
        test_exit = 0
        call gremlin_resolve_read_session(project_dir, request, session, status_text, &
            owner_pid, is_live, ierr, message)
        if (ierr == 0 .and. is_live) call gremlin_session_acquire(project_dir, &
            trim(request%lane_id), session, ierr, message)
        if (ierr /= 0) then
            call error_response('reproduce', trim(message), response)
            return
        end if
        ! Retired sessions keep their exact journal namespace. Driver pins are
        ! shared by that lane and remain validated against the generation key.
        call gremlin_session_state_dir(project_dir, request%lane_id, driver_state_dir, &
            ierr, message)
        if (ierr /= 0) call finish(trim(message))
        if (ierr /= 0) return
        call fs_make_dir(session%state_dir//'/logs')
        call gremlin_json_field(status_text, 'active_generation', identity)
        if (len_trim(request%generation_id) > 0) identity = request%generation_id
        if (.not. is_hex_digest(identity)) then
            call finish('there is no compatible successful generation')
            return
        end if
        call common_generation_cas_root(generation%root, ierr, message)
        if (ierr /= 0) then
            call finish('cannot resolve requested generation: '//trim(message))
            return
        end if
        generation%identity = identity
        generation%root = trim(generation%root)//'/gremlin/generations-v2/'// &
            trim(identity)
        generation%project_root = trim(generation%root)//'/bundle/project'
        call generation_inventory_restore(generation, ierr, message)
        call gremlin_generation_lease_acquire_at(generation%root, lease, ierr, message)
        have_lease = ierr == 0
        if (ierr /= 0) message = 'cannot lease requested generation: '//trim(message)
        if (ierr == 0) then
            call generation_driver_identity(generation%root, generation%driver_digest, &
                generation%driver_size, ierr, message)
            if (ierr /= 0) message = 'cannot read generation driver identity: '// &
                trim(message)
        end if
        if (ierr == 0) then
            call driver_pin_existing(driver_state_dir, generation%driver_digest, &
                generation%driver_size, pin, ierr, message)
            if (ierr /= 0) message = 'cannot validate the pinned generation driver: '// &
                trim(message)
        end if
        if (ierr /= 0) then
            call finish(trim(message))
            return
        end if
        generation%driver_path = pin%path
        inquire (file=trim(generation%project_root)//'/fpm.toml', exist=exists)
        if (.not. exists) inquire (file=trim(generation%project_root)// &
            '/CMakeLists.txt', exist=exists)
        if (.not. exists) then
            call finish('requested generation snapshot is unavailable')
            return
        end if
        ! The completion sequence is the reproduction execution ID. Allocate it
        ! before launch so its output path and durable receipt identify the same
        ! invocation, including concurrent reproductions in one live session.
        call system_clock(count=clock_count)
        sequence = int(clock_count)
        call log_path(session, 'reproduce-'//int_text(sequence)//'.log', log_file)
        call log_path(session, 'reproduce-'//int_text(sequence)//'-build.log', build_log)
        timeout = case_wall_timeout(generation%project_root, request%case_id, &
            request%timeout_seconds)
        write (view_owner, '(a,"-reproduce-",i0)') session%session_id, sequence
        call execution_view_create(session%state_dir//'/views', generation%identity, &
            trim(view_owner)//'-build', 'build', generation%input_inventory, &
            generation%input_inventory_ready, generation%input_inventory_complete, &
            build_view, ierr, message, &
            candidate_bundle_root=trim(generation%root)//'/bundle')
        if (ierr /= 0) then
            call finish('cannot create private build view: '//trim(message))
            return
        end if
        call execution_view_create(session%state_dir//'/views', generation%identity, &
            trim(view_owner), request%case_id, generation%input_inventory, &
            generation%input_inventory_ready, generation%input_inventory_complete, &
            case_view, ierr, message)
        if (ierr /= 0) then
            call finish('cannot create private execution view: '//trim(message))
            return
        end if
        call process_scope_begin(session%state_dir, scope, ierr)
        have_scope = ierr == 0
        if (ierr == 0) then
            ! The case wall timeout also bounds the wait for a host work slot,
            ! so a reproduction never blocks silently behind other lanes.
            slot_wait_started = exec_clock_ms()
            do
                call acquire_heavy_work_slot(session, 1, work_slot, have_slot, &
                    .false., stop_requested, ierr, message, slot_busy)
                if (ierr /= 0 .or. .not. slot_busy) exit
                if (exec_clock_ms() - slot_wait_started >= &
                        int(timeout, int64)*1000_int64) then
                    ierr = 1
                    write (message, '(a,i0,a)') 'TIMEOUT: no host Gremlin work '// &
                        'slot became free within ', timeout, ' seconds'
                    exit
                end if
                call fs_sleep_ms(100)
            end do
        else
            message = 'cannot claim reproduction process ownership: '//int_text(ierr)
        end if
        if (ierr /= 0) then
            call finish(trim(message))
            return
        end if
        ! Captured generations contain authored inputs rather than generated
        ! CTest files. Query the frozen configuration in the owned writable view.
        generation%build_project_root = build_view%cwd
        backend = detect_backend(build_view%cwd)
        if (backend%kind == BACKEND_CMAKE) then
            call cmake_configure(backend%cmake, '', log_file, ierr)
            if (ierr /= 0) then
                call finish('cannot configure frozen CMake test inventory; '// &
                    'diagnostic: '//trim(log_file))
                return
            end if
        end if
        selection_request = request
        selection_request%targets = ''
        selection_request%targets(1) = request%case_id
        selection_request%n_targets = 1
        selection_request%random_count = 0
        call discover_campaign(generation, session, selection_request, selected, &
            n_selected, mandatory_count, seed, ierr, message, bypass_coverage=.true.)
        if (ierr == 0) then
            if (n_selected /= 1) ierr = 1
            if (ierr == 0 .and. trim(selected(1)) /= trim(request%case_id)) ierr = 1
            if (ierr /= 0) message = 'case_id is not present in that generation'
        end if
        if (ierr /= 0) then
            call finish(trim(message))
            return
        end if
        ! Build the generation once here; the case child never builds, so
        ! a build failure is reported once and no case runs.
        call build_command(pin%path, build_view, packed, n_args, env)
        call argv_push(packed, n_args, '--case')
        call argv_push(packed, n_args, trim(request%case_id))
        call exec_start(job, trim(build_view%cwd), packed, n_args, trim(build_log), &
            ierr, env, timeout)
        if (ierr /= 0) then
            call finish('cannot start the generation build')
            return
        end if
        call wait_job(job, stopped, session)
        if (job%exitcode /= 0) then
            call gremlin_retain_build_log(build_log)
            exitcode = 1
            if (job%timed_out) then
                write (message, '(a,i0,a,a)') 'TIMEOUT: generation build exceeded ', &
                    timeout, ' seconds and was cancelled; diagnostic: ', trim(build_log)
            else if (stopped) then
                message = 'CANCELLED: generation build cancelled by a stop request'
            else
                message = 'generation build failed; diagnostic: '//trim(build_log)
            end if
            call finish(trim(message))
            return
        end if
        call case_command(pin%path, generation, request%case_id, case_view, &
            build_view%cwd, packed, n_args, env)
        call exec_start(job, trim(build_view%cwd), packed, n_args, trim(log_file), &
            test_exit, env, timeout)
        spawned = test_exit == 0
        if (.not. spawned) then
            outcome = 'INFRA_ERROR'
        else
            call wait_job(job, stopped)
            test_exit = job%exitcode
            if (job%timed_out .and. test_exit /= EXEC_UNCANCELLED) then
                outcome = 'TIMEOUT'
            else if (test_exit == EXEC_UNCANCELLED) then
                outcome = 'INFRA_ERROR'
                call gremlin_generation_pin_at(generation%root, .true., status, detail)
            else
                outcome = runner_case_outcome(log_file, request%case_id)
            end if
            call append_execution_provenance(log_file, case_view)
        end if
        call record_receipt(session, request, generation, request%case_id, test_exit, &
            trim(outcome), sequence, 1, seed, trim(log_file), ierr, message, &
            credit_coverage=.false., gate_required=.true.)
        if (ierr /= JOURNAL_OK) then
            call finish(trim(message), spawned .and. test_exit /= 0)
            return
        end if
        call finish('', spawned .and. test_exit /= 0)
        if (exitcode /= 2) call simple_response('reproduce', request%lane_id, &
            session%session_id, trim(outcome), response)

    contains

        subroutine finish(error_text, retain_views)
            !! Release everything this reproduction holds, in reverse order;
            !! error_text (or the first release failure) is the response.
            character(len=*), intent(in) :: error_text
            logical, intent(in), optional :: retain_views
            character(len=PATH_LEN) :: failure
            logical :: retain

            retain = .false.
            if (present(retain_views)) retain = retain_views
            failure = error_text
            if (have_slot) then
                call release_heavy_work_slot(work_slot, have_slot, status, detail, &
                    drain=.true.)
                if (status /= 0 .and. len_trim(failure) == 0) failure = &
                    'cannot release Gremlin host work slot: '//trim(detail)
            end if
            if (have_scope) then
                call process_scope_end(scope, status)
                if (status /= 0 .and. len_trim(failure) == 0) failure = &
                    'cannot release Gremlin host work slot: '// &
                    'cannot restore caller process ownership'
            end if
            if (case_view%active) call execution_view_release(case_view, retain, &
                status, detail)
            if (build_view%active) call execution_view_release(build_view, retain, &
                status, detail)
            call release_generation_lease(lease, have_lease, status, detail)
            if (status /= 0 .and. len_trim(failure) == 0) failure = &
                'cannot release generation lease: '//trim(detail)
            call release_if_owner(session, status, detail)
            if (status /= 0 .and. len_trim(failure) == 0) failure = &
                'cannot release Gremlin ownership: '//trim(detail)
            if (len_trim(failure) > 0) then
                call error_response('reproduce', trim(failure), response)
                if (exitcode /= 1 .or. len_trim(error_text) == 0) exitcode = 2
            else if (test_exit /= 0) then
                exitcode = 1
            else
                exitcode = 0
            end if
        end subroutine finish

    end subroutine handle_reproduce

    subroutine wait_job(job, stopped, session)
        !! Wait for the job under its wall deadline. With a session, a stop
        !! request cancels it (EXEC_CANCELLED, or EXEC_UNCANCELLED on failure).
        type(exec_job_t), intent(inout) :: job
        logical, intent(out) :: stopped
        type(gremlin_session_t), intent(in), optional :: session
        integer :: polls, status
        logical :: done, stop_now
        character(len=PATH_LEN) :: detail

        stopped = .false.
        polls = 0
        do
            call exec_poll(job, done, status)
            if (done) return
            polls = polls + 1
            if (present(session) .and. mod(polls, 4) == 0) then
                call gremlin_session_stop_requested(session, stop_now, status, detail)
                if (status == 0 .and. stop_now) then
                    call exec_cancel(job, status)
                    if (status /= 0) job%exitcode = EXEC_UNCANCELLED
                    stopped = .true.
                    return
                end if
            end if
            call fs_sleep_ms(50)
        end do
    end subroutine wait_job

    subroutine response_with_events(action, status_text, session, request, failures_only, &
            response, ierr, message)
        character(len=*), intent(in) :: action, status_text
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        logical, intent(in) :: failures_only
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        type(gremlin_lifecycle_event_t), allocatable :: lifecycle_events(:)
        type(json_builder_t) :: events, lifecycle
        integer(int64) :: next_cursor, file_size, next_lifecycle_cursor
        integer :: i, n_lifecycle
        logical :: exists, has_more, lifecycle_has_more
        character(len=16) :: row_status
        character(len=PATH_LEN) :: lifecycle_path

        call journal_read_page(session%state_dir//'/journal.jsonl', request%cursor, &
            request%max_records, request%max_bytes, records, next_cursor, ierr, message)
        if (ierr /= JOURNAL_OK) return
        file_size = 0_int64
        inquire (file=session%state_dir//'/journal.jsonl', exist=exists, &
            size=file_size, iostat=i)
        if (i /= 0) then
            ierr = 2
            message = 'cannot inspect Gremlin journal after reading its page'
            return
        end if
        has_more = .false.
        if (exists) has_more = next_cursor < file_size
        events = json_new()
        call array_open(events)
        do i = 1, size(records)
            if (failures_only) then
                call gremlin_json_field(records(i)%json, 'status', row_status)
                if (.not. status_is_failure(trim(row_status))) cycle
            end if
            call json_value_raw(events, records(i)%json)
        end do
        call array_close(events)
        call lifecycle_path_for_session(session, request, lifecycle_path, ierr, message)
        if (ierr /= 0) return
        call gremlin_lifecycle_read_page(trim(lifecycle_path), request%lifecycle_cursor, &
            request%max_records, request%max_bytes, lifecycle_events, n_lifecycle, &
            next_lifecycle_cursor, lifecycle_has_more, ierr, message)
        if (ierr /= LIFECYCLE_OK) return
        lifecycle = json_new()
        call array_open(lifecycle)
        do i = 1, n_lifecycle
            call json_value_raw(lifecycle, lifecycle_events(i)%json)
        end do
        call array_close(lifecycle)
        response = gremlin_events_json(action, status_text, session%session_id, &
            request%lane_id, events, next_cursor, has_more, lifecycle, &
            next_lifecycle_cursor, lifecycle_has_more)
        ierr = 0
        message = ''
    end subroutine response_with_events

    subroutine replace_action(response, action)
        character(len=:), allocatable, intent(inout) :: response
        character(len=*), intent(in) :: action

        character(len=:), allocatable :: needle
        integer :: position

        needle = '"action":"status"'
        position = index(response, needle)
        if (position == 0) return
        response = response(:position - 1)//'"action":"'// &
            trim(json_escape_string(action))//'"'//response(position + len(needle):)
    end subroutine replace_action

    subroutine release_if_owner(session, ierr, message)
        type(gremlin_session_t), intent(inout) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        if (session%owner) then
            call gremlin_session_release(session, ierr, message)
        else
            ierr = 0
            message = ''
        end if
    end subroutine release_if_owner

    subroutine poll_launcher(pid)
        !! Reap this process's owner launcher once it exited.
        integer, intent(in) :: pid
        integer :: ignored_exit
        logical :: done

        if (pid > 0) call process_poll_pid(pid, done, ignored_exit)
    end subroutine poll_launcher

end module fo_gremlin_supervisor
