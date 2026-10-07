module fo_gremlin_supervisor
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use fo_build_backend, only: BACKEND_NATIVE, backend_t, detect_backend
    use fo_cache, only: HASH_LEN, cache_digest, cache_store_root
    use fx_action_result_store, only: action_result_store_t, &
        action_result_store_init, action_result_maintenance_tick, ACTION_RESULT_OK
    use fx_immutable_root_compact, only: immutable_store_compact_generation_roots
    use fx_immutable_store, only: IMMUTABLE_OK
    use fo_check, only: fo_changed_modules
    use fo_gremlin_context, only: capture_candidate, generation_inventory_restore, &
        common_generation_cas_root
    use fo_gremlin_request, only: gremlin_request_t, parse_request, is_hex_digest, &
        gremlin_json_field
    use fo_test_impact, only: test_impact_case_t, test_impact_result_t, &
        test_impact_select
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
        json_parser_next, JSON_OBJECT_START, JSON_OBJECT_END, JSON_ARRAY_START, &
        JSON_ARRAY_END, JSON_KEY, JSON_STRING, JSON_ERROR, JSON_END_OF_INPUT
    use fo_gremlin_generation, only: generation_t, generation_driver_identity
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release
    use fo_driver, only: driver_pin_t, driver_pin_current, driver_pin_existing
    use fo_change_watch, only: change_watch_t, change_watch_init, &
        change_watch_poll, change_watch_close
    use fo_gremlin_journal, only: journal_append, journal_read_page, &
        journal_compact_tail, journal_record_t, JOURNAL_OK, JOURNAL_INVALID, &
        JOURNAL_MAX_RECORD_BYTES
    use fo_gremlin_coverage, only: coverage_epoch_t, coverage_open, &
        coverage_next_chunk, coverage_record, coverage_record_path, &
        coverage_recover_running, COVERAGE_OK
    use fo_gremlin_coverage, only: coverage_read_view_path, COVERAGE_NOT_FOUND
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_readiness, only: gremlin_readiness_input_t, gremlin_readiness_t, &
        gremlin_readiness_compute, gremlin_wait_satisfied, gremlin_wait_code, &
        gremlin_phase_name, gremlin_health_name, gremlin_verification_name, &
        GREMLIN_PHASE_STARTING, GREMLIN_PHASE_BUILDING, GREMLIN_PHASE_TESTING, &
        GREMLIN_PHASE_QUIESCENT, GREMLIN_PHASE_STOPPED, GREMLIN_PHASE_FAILED, &
        GREMLIN_WAIT_FAILURE, GREMLIN_HEALTH_FAILURE, GREMLIN_VERIFY_ORDINARY, &
        GREMLIN_VERIFY_FULL
    use fo_gremlin_lifecycle, only: gremlin_freshness_update, &
        gremlin_freshness_check, gremlin_lifecycle_event_t, &
        gremlin_lifecycle_append, gremlin_lifecycle_read_page, &
        gremlin_lifecycle_make_id, LIFECYCLE_OK, LIFECYCLE_INVALID, &
        LIFECYCLE_MAX_EVENT
    use fo_gremlin_policy, only: shuffle_gremlin_tests, GREMLIN_POLICY_OK, &
        GREMLIN_NONMANDATORY_LIMIT
    use fo_gremlin_state, only: gremlin_session_t, gremlin_lease_t, &
        gremlin_session_acquire, &
        gremlin_session_publish, gremlin_session_read, gremlin_session_release, &
        gremlin_session_recovery_complete, gremlin_session_state_dir, &
        gremlin_session_request_stop, gremlin_session_stop_requested, &
        gremlin_lease_release, &
        gremlin_generation_lease_acquire_at, gremlin_generation_pin_at, &
        gremlin_generation_prune_inactive, &
        GREMLIN_STATE_TEXT_MAX
    use fo_gfortran_build, only: gfortran_selected_test_names
    use fo_scan, only: is_slow_test
    use fo_test_budget, only: test_budget_seconds, test_wall_cap_seconds
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    use fo_process, only: argv_push, process_cancel_pid, &
        process_poll_pid, process_reap_adopted_scope_zombies, &
        process_start_argv_logged, process_set_async_scope
    use fo_scan_types, only: MAX_PATH
    use fo_util, only: json_bool, json_int
    use fx_dag, only: dag_t, MAX_NODES
    use fx_json_build, only: json_escape_string
    use fo_fs, only: fs_make_dir, fs_sleep_ms, fs_remove_tree
    use fo_gremlin_session, only: gremlin_resolve_read_session, &
        gremlin_load_terminal_session, gremlin_get_session_journal_path, &
        gremlin_stage_recovery_journal, gremlin_recover_owner_journal, &
        gremlin_publish_terminal_snapshot, gremlin_release_stopped_session
    implicit none
    private

    integer, parameter :: NAME_LEN = 128, PATH_LEN = 4096
    integer, parameter :: SUMMARY_DIAGNOSTIC_LIMIT = 1024
    character(len=*), parameter :: SUMMARY_TRUNCATION = '...[truncated]'
    integer, parameter :: GREMLIN_HISTORY_LIMIT = 128
    integer, parameter :: GREMLIN_POLICY_RECEIPT_LIMIT = 512


    type :: child_t
        integer :: pid = 0
        integer :: campaign = 0
        integer :: case_index = 0
        integer :: seed = 0
        character(len=PATH_LEN) :: log_file = ''
        character(len=NAME_LEN) :: case_name = ''
        type(execution_view_t) :: execution_view
        logical :: gate_required = .false.
        real :: started_at = 0.0
    end type child_t

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
        character(len=PATH_LEN) :: message, status_text, owner_start
        character(len=128) :: session_id
        character(len=HASH_LEN) :: stored_policy
        integer :: ierr, owner_pid, pid, spawn_exit, i, n_args
        character(len=:), allocatable :: packed
        character(len=PATH_LEN) :: executable, log_file

        exitcode = 0
        call gremlin_session_acquire(project_dir, trim(request%lane_id), &
            session, ierr, message)
        if (ierr /= 0) then
            call error_response('start', trim(message), response)
            exitcode = 2
            return
        end if
        if (session%attached) then
            do i = 1, 60
                call gremlin_session_read(project_dir, trim(request%lane_id), session_id, &
                    owner_pid, owner_start, status_text, ierr, message)
                if (ierr == 0 .and. len_trim(status_text) > 0) exit
                call fs_sleep_ms(50)
            end do
            if (ierr /= 0) then
                call error_response('start', trim(message), response)
                exitcode = 2
                return
            end if
            if (len_trim(status_text) == 0) then
                call error_response('start', 'active Gremlin owner has not published state', &
                    response)
                exitcode = 2
                return
            end if
            call gremlin_json_field(status_text, 'policy_key', stored_policy)
            if (trim(stored_policy) /= request_policy_key(request)) then
                call error_response('start', &
                    'an active Gremlin session has an incompatible policy', response)
                exitcode = 2
                return
            end if
            call simple_response('start', request%lane_id, trim(session_id), &
                'attached', response)
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
        executable = driver_pin%path
        call fs_make_dir(trim(session%state_dir)//'/logs')
        call log_path(session, 'launcher.log', log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
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
        call argv_push(packed, n_args, '--jobs')
        call argv_push(packed, n_args, int_text(request%jobs))
        if (request%only_changed) call argv_push(packed, n_args, '--only-changed')
        if (request%shuffle) call argv_push(packed, n_args, '--shuffle')
        do i = 1, request%n_targets
            call argv_push(packed, n_args, '--target')
            call argv_push(packed, n_args, trim(request%targets(i)))
        end do
        call process_start_argv_logged(project_dir, packed, n_args, log_file, &
            pid, spawn_exit, 'FO_DISABLE_SELF_REFRESH=1;FO_SELF_REFRESH=0')
        if (spawn_exit /= 0) then
            call error_response('start', 'cannot launch the Gremlin owner '// &
                '(process setup error '//int_text(spawn_exit)//')', response)
            exitcode = 2
            return
        end if
        ! The child obtains the lane lock. Wait only for its ready publication;
        ! campaign progress never depends on this caller staying connected.
        do i = 1, 60
            call fs_sleep_ms(50)
            call gremlin_session_read(project_dir, trim(request%lane_id), session_id, &
                owner_pid, owner_start, status_text, ierr, message)
            if (ierr /= 0 .or. len_trim(session_id) == 0 .or. &
                len_trim(status_text) == 0) cycle
            call gremlin_json_field(status_text, 'policy_key', stored_policy)
            if (trim(stored_policy) /= request_policy_key(request)) then
                call cancel_launched_owner(pid, spawn_exit)
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
            exit
        end do
        if (i > 60 .or. len_trim(session_id) == 0 .or. len_trim(status_text) == 0) then
            call cancel_launched_owner(pid, spawn_exit)
            if (spawn_exit == 0) then
                call error_response('start', 'Gremlin owner did not publish ready state', &
                    response)
            else
                call error_response('start', 'Gremlin owner did not publish ready state; '// &
                    'launcher cancellation failed', response)
            end if
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
                if (freshness_verified) status_text = fresh_status
            end if
            call compute_readiness(session, trim(status_text), coverage_view, &
                have_coverage, readiness, ierr, message, freshness_verified)
            if (ierr /= 0) then
                call error_response('status', trim(message), response)
                exitcode = 2
                return
            end if
        end if
        enriched_status = status_with_coverage(trim(status_text), coverage_view, &
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

    subroutine compute_readiness(session, status_text, coverage, &
            have_coverage, readiness, ierr, message, freshness_verified)
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: status_text
        type(gremlin_coverage_view_t), intent(in) :: coverage
        logical, intent(in) :: have_coverage
        type(gremlin_readiness_t), intent(out) :: readiness
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        logical, intent(in), optional :: freshness_verified
        type(gremlin_readiness_input_t) :: input
        type(journal_record_t), allocatable :: records(:)
        character(len=HASH_LEN) :: active_id, candidate_id, receipt_generation
        character(len=HASH_LEN) :: receipt_requirement
        character(len=128) :: status_name, case_id, raw
        character(len=PATH_LEN) :: journal_path
        character(len=NAME_LEN) :: gate_cases(MAX_NODES)
        character(len=NAME_LEN) :: gate_pass_cases(MAX_NODES)
        integer(int64) :: cursor, next_cursor
        integer :: journal_status, i, ios, gate_count, gate_passed
        logical :: build_passed, gate_failure, gate_required, passed

        ierr = 0
        message = ''
        input = gremlin_readiness_input_t()
        active_id = ''
        candidate_id = ''
        status_name = ''
        call gremlin_json_field(status_text, 'active_generation', active_id)
        call gremlin_json_field(status_text, 'candidate_generation', candidate_id)
        call gremlin_json_field(status_text, 'state', status_name)
        call gremlin_json_field(status_text, 'gate_required', raw)
        read(raw, *, iostat=ios) input%gate_required
        if (ios /= 0) input%gate_required = -1
        call gremlin_json_field(status_text, 'input_changed', raw)
        input%dirty = trim(raw) == 'true'
        if (present(freshness_verified)) &
            input%dirty = input%dirty .or. .not. freshness_verified
        input%owner_session = session%session_id
        input%generation = active_id
        input%inventory_digest = coverage%inventory_digest
        call gremlin_json_field(status_text, 'requirement_digest', &
            input%requirement_digest)
        call gremlin_json_field(status_text, 'event_epoch', raw)
        read (raw, *, iostat=ios) input%event_epoch
        if (ios /= 0) input%event_epoch = -1
        input%dirty = input%dirty .or. len_trim(active_id) /= HASH_LEN .or. &
            (len_trim(candidate_id) > 0 .and. trim(candidate_id) /= trim(active_id))
        if (trim(status_name) == 'building' .or. trim(status_name) == 'build_failed' .or. &
            trim(status_name) == 'capture_failed' .or. &
            trim(status_name) == 'capture_pending') input%dirty = .true.
        if (trim(status_name) == 'build_failed' .or. &
            trim(status_name) == 'capture_failed' .or. trim(status_name) == 'error') &
            input%failure_observed = .true.
        input%exact_active_generation = len_trim(active_id) == HASH_LEN .and. &
            input%gate_required >= 0 .and. input%event_epoch >= 0 .and. &
            len_trim(input%requirement_digest) == HASH_LEN .and. have_coverage .and. &
            coverage%generation_id == active_id .and. &
            len_trim(coverage%inventory_digest) == HASH_LEN
        input%capture_fresh = input%exact_active_generation .and. .not. input%dirty
        if (input%gate_required < 0) input%gate_required = 0
        if (have_coverage .and. trim(coverage%generation_id) == trim(active_id)) then
            input%ordinary_required = coverage%ordinary_eligible_count
            input%ordinary_passed = coverage%ordinary_pass_count
            input%ordinary_failures = coverage%ordinary_failure_count
            input%full_required = coverage%eligible_count
            input%full_passed = coverage%pass_count
            input%full_failures = coverage%fail_count + coverage%timeout_count + &
                coverage%flaky_count + coverage%infra_count
        end if
        select case (trim(status_name))
        case ('starting')
            input%phase = GREMLIN_PHASE_STARTING
        case ('building')
            input%phase = GREMLIN_PHASE_BUILDING
        case ('quiescent')
            input%phase = GREMLIN_PHASE_QUIESCENT
        case ('stopped')
            input%phase = GREMLIN_PHASE_STOPPED
        case ('capture_pending')
            if (input%exact_active_generation) then
                input%phase = GREMLIN_PHASE_TESTING
            else
                input%phase = GREMLIN_PHASE_STARTING
            end if
        case ('error', 'inventory_failed', 'test_launch_failed', 'build_failed', &
                'capture_failed')
            input%phase = GREMLIN_PHASE_FAILED
        case default
            if (input%exact_active_generation) then
                input%phase = GREMLIN_PHASE_TESTING
            else
                input%phase = GREMLIN_PHASE_FAILED
            end if
        end select

        build_passed = .false.
        gate_failure = .false.
        gate_cases = ''
        gate_pass_cases = ''
        gate_count = 0
        gate_passed = 0
        cursor = 0_int64
        if (.not. allocated(session%project_key) .or. &
            .not. allocated(session%lane_id) .or. &
            .not. allocated(session%session_id)) then
            ierr = 1
            message = 'Gremlin session has no receipt journal identity'
            return
        end if
        call gremlin_get_session_journal_path(trim(session%project_key), &
            trim(session%lane_id), trim(session%session_id), journal_path, ierr, message)
        if (ierr /= 0) return
        do
            call journal_read_page(trim(journal_path), cursor, 64, &
                int(JOURNAL_MAX_RECORD_BYTES, int64)*64_int64, records, next_cursor, &
                journal_status, message)
            if (journal_status /= JOURNAL_OK) then
                ierr = journal_status
                return
            end if
            if (size(records) == 0) exit
            do i = 1, size(records)
                receipt_generation = ''
                case_id = ''
                status_name = ''
                raw = ''
                call gremlin_json_field(records(i)%json, 'generation', receipt_generation)
                call gremlin_json_field(records(i)%json, 'case_id', case_id)
                call gremlin_json_field(records(i)%json, 'status', status_name)
                if (trim(receipt_generation) == trim(candidate_id) .and. &
                    trim(case_id) == '<build>' .and. &
                    status_is_failure(trim(status_name))) input%failure_observed = .true.
                if (trim(receipt_generation) /= trim(active_id)) cycle
                if (trim(case_id) == '<build>' .and. trim(status_name) == 'BUILD_PASS') &
                    build_passed = .true.
                if (status_is_failure(trim(status_name))) then
                    gate_failure = .true.
                    input%failure_observed = .true.
                end if
                call gremlin_json_field(records(i)%json, 'gate_required', raw)
                receipt_requirement = ''
                call gremlin_json_field(records(i)%json, 'requirement_digest', &
                    receipt_requirement)
                gate_required = trim(raw) == 'true' .and. &
                    receipt_requirement == input%requirement_digest
                passed = trim(status_name) == 'PASS'
                if (gate_required .and. len_trim(case_id) > 0) then
                    if (.not. any(gate_cases(:gate_count) == trim(case_id))) then
                        if (gate_count < size(gate_cases)) then
                            gate_count = gate_count + 1
                            gate_cases(gate_count) = trim(case_id)
                        end if
                    end if
                    if (passed .and. .not. any( &
                        gate_pass_cases(:gate_passed) == trim(case_id))) then
                        if (gate_passed < size(gate_cases)) then
                            gate_passed = gate_passed + 1
                            gate_pass_cases(gate_passed) = trim(case_id)
                        end if
                    end if
                end if
            end do
            if (next_cursor <= cursor) then
                ierr = JOURNAL_INVALID
                message = 'Gremlin receipt cursor did not advance while computing readiness'
                return
            end if
            cursor = next_cursor
        end do
        input%exact_active_generation = input%exact_active_generation .and. build_passed
        input%capture_fresh = input%exact_active_generation .and. .not. input%dirty
        input%build_passed = build_passed
        input%gate_required = max(input%gate_required, gate_count)
        input%gate_passed = min(input%gate_required, gate_passed)
        input%gate_failure = gate_failure
        call gremlin_readiness_compute(input, readiness)
    end subroutine compute_readiness

    function status_with_coverage(status_text, view, readiness, have_coverage) &
            result(enriched)
        character(len=*), intent(in) :: status_text
        type(gremlin_coverage_view_t), intent(in) :: view
        type(gremlin_readiness_t), intent(in) :: readiness
        logical, intent(in) :: have_coverage
        character(len=:), allocatable :: enriched
        character(len=:), allocatable :: coverage_json
        character(len=16) :: phase, health, verification

        phase = gremlin_phase_name(readiness%phase)
        health = gremlin_health_name(readiness%health)
        verification = gremlin_verification_name(readiness%verification_level)
        enriched = status_text(:len_trim(status_text) - 1)// &
            ',"phase":"'//trim(phase)//'","health":"'//trim(health)// &
            '","dirty":'//trim(json_bool(readiness%dirty))// &
            ',"local_gate_green":'//trim(json_bool(readiness%local_gate_green))// &
            ',"verification_level":"'//trim(verification)// &
            '","fully_verified":'//trim(json_bool(readiness%fully_verified))// &
            ',"gate_token_version":1,"gate_token":"'// &
            trim(readiness%gate_token)//'","requirement_digest":"'// &
            trim(readiness%requirement_digest)//'","event_epoch":'// &
            trim(json_int(readiness%event_epoch))// &
            ',"gate_required":'//trim(json_int(readiness%gate_required))// &
            ',"gate_passed":'//trim(json_int(readiness%gate_passed))// &
            ',"ordinary_required":'//trim(json_int(readiness%ordinary_required))// &
            ',"ordinary_passed":'//trim(json_int(readiness%ordinary_passed))// &
            ',"ordinary_failures":'//trim(json_int(readiness%ordinary_failures))// &
            ',"full_required":'//trim(json_int(readiness%full_required))// &
            ',"full_passed":'//trim(json_int(readiness%full_passed))// &
            ',"full_failures":'//trim(json_int(readiness%full_failures))
        if (have_coverage) then
            coverage_json = '{"ordinary":'// &
                verification_counts(view%ordinary_eligible_count, &
                view%ordinary_pass_count, view%ordinary_failure_count, &
                view%ordinary_timeout_count, view%ordinary_infra_count, &
                view%ordinary_flaky_count)//',"full":'// &
                verification_counts(view%eligible_count, view%pass_count, &
                readiness%full_failures, view%timeout_count, view%infra_count, &
                view%flaky_count)//',"generation_id":"'//trim(view%generation_id)// &
                '","inventory_digest":"'//trim(view%inventory_digest)// &
                '","epoch":'//trim(json_int(view%epoch))// &
                ',"seed":'//trim(json_int(view%seed))// &
                ',"cursor":'//trim(json_int(view%cursor))// &
                ',"eligible":'//trim(json_int(view%eligible_count))// &
                ',"completed":'//trim(json_int(view%completed_count))// &
                ',"pass":'//trim(json_int(view%pass_count))// &
                ',"fail":'//trim(json_int(view%fail_count))// &
                ',"timeout":'//trim(json_int(view%timeout_count))// &
                ',"flaky":'//trim(json_int(view%flaky_count))// &
                ',"infra":'//trim(json_int(view%infra_count))// &
                ',"running":'//trim(json_int(view%running_count))// &
                ',"cancelled":'//trim(json_int(view%cancelled_count))// &
                ',"unknown":'//trim(json_int(view%unknown_count))// &
                ',"remaining":'//trim(json_int(view%remaining_count))// &
                ',"ordinary_required":'//trim(json_int(readiness%ordinary_required))// &
                ',"ordinary_passed":'//trim(json_int(readiness%ordinary_passed))// &
                ',"ordinary_verified":'//trim(json_bool( &
                readiness%verification_level >= 2))// &
                ',"full_required":'//trim(json_int(readiness%full_required))// &
                ',"full_passed":'//trim(json_int(readiness%full_passed))// &
                ',"fully_verified":'//trim(json_bool(readiness%fully_verified))// &
                ',"ordinary_complete":'//trim(json_bool(view%ordinary_complete))// &
                ',"full_coverage":'//trim(json_bool(view%full_coverage))// &
                ',"green":'//trim(json_bool(view%green))//'}'
            enriched = enriched//',"coverage":'//coverage_json
        end if
        enriched = enriched//'}'
    end function status_with_coverage

    function verification_counts(eligible, passed, failures, timeouts, infra, flaky) &
            result(json)
        integer, intent(in) :: eligible, passed, failures, timeouts, infra, flaky
        character(len=:), allocatable :: json
        integer :: remaining

        remaining = max(0, eligible - passed - failures)
        json = '{"eligible":'//trim(json_int(eligible))// &
            ',"pass":'//trim(json_int(passed))// &
            ',"fail":'//trim(json_int(max(0, failures - timeouts - infra - flaky)))// &
            ',"timeout":'//trim(json_int(timeouts))// &
            ',"infra_error":'//trim(json_int(infra))// &
            ',"flaky":'//trim(json_int(flaky))// &
            ',"unknown":'//trim(json_int(remaining))// &
            ',"remaining":'//trim(json_int(remaining))//'}'
    end function verification_counts

    subroutine compact_summary(status_text, session, action, response, &
            ierr, message)
        character(len=*), intent(in) :: status_text, action
        type(gremlin_session_t), intent(in) :: session
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=4096) :: value
        character(len=PATH_LEN) :: journal_state_dir
        character(len=HASH_LEN) :: generation
        character(len=32) :: health, last_outcome
        integer :: failure_count, ios
        logical :: failure_evidence
        character(len=:), allocatable :: compact, latest
        character(len=32), parameter :: strings(*) = [character(len=32) :: &
            'session_id', 'lane_id', 'state', 'phase', 'health', 'current_test', &
            'active_generation', 'candidate_generation', 'last_outcome', &
            'diagnostic', 'verification_level', 'requirement_digest', 'wait_until']
        character(len=32), parameter :: scalars(*) = [character(len=32) :: &
            'input_changed', 'dirty', 'local_gate_green', 'fully_verified', &
            'completed', 'selected', 'seed', 'last_exitcode', 'event_epoch', &
            'gate_required', 'gate_passed', 'ordinary_required', 'ordinary_passed', &
            'ordinary_failures', 'full_required', 'full_passed', 'full_failures', &
            'wait_satisfied', 'wait_timed_out', 'wait_terminal', 'failure_observed']
        integer :: i

        ierr = 0
        message = ''
        compact = '{"action":"'//trim(action)//'","detail":"summary"'
        do i = 1, size(strings)
            call gremlin_json_field(status_text, trim(strings(i)), value, .true.)
            if (len_trim(value) == 0) cycle
            if (trim(strings(i)) == 'diagnostic' .and. &
                    len_trim(value) > SUMMARY_DIAGNOSTIC_LIMIT) &
                value = value(:SUMMARY_DIAGNOSTIC_LIMIT - len(SUMMARY_TRUNCATION))// &
                    SUMMARY_TRUNCATION
            compact = compact//',"'//trim(strings(i))//'":"'// &
                trim(json_escape_string(trim(value)))//'"'
        end do
        do i = 1, size(scalars)
            call gremlin_json_field(status_text, trim(scalars(i)), value, .true.)
            if (len_trim(value) == 0) cycle
            compact = compact//',"'//trim(scalars(i))//'":'//trim(value)
        end do
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
            call latest_generation_failure(trim(journal_state_dir), &
                trim(session%session_id), trim(generation), latest, ierr, message)
            if (ierr /= 0) return
        end if
        if (len(latest) > 0) compact = compact//',"latest_failure":'//latest
        compact = compact//',"retrieval_hint":"Use detail=full for complete status or '
        compact = compact//'action=events/failures with cursors for event pages."}'
        response = compact
    end subroutine compact_summary

    subroutine latest_generation_failure(state_dir, session_id, generation, result, &
            ierr, message)
        character(len=*), intent(in) :: state_dir, session_id, generation
        character(len=:), allocatable, intent(out) :: result
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        character(len=128) :: record_session_id, record_generation, status, case_id
        character(len=PATH_LEN) :: log_path
        integer(int64) :: cursor, next_cursor, page_bytes
        integer :: i, journal_status, n_read, n_page

        result = ''
        ierr = JOURNAL_OK
        message = ''
        if (len_trim(generation) /= HASH_LEN) return
        cursor = 0_int64
        n_read = 0
        do while (n_read < GREMLIN_POLICY_RECEIPT_LIMIT)
            n_page = min(64, GREMLIN_POLICY_RECEIPT_LIMIT - n_read)
            page_bytes = int(n_page, int64) * int(JOURNAL_MAX_RECORD_BYTES, int64)
            call journal_read_page(trim(state_dir)//'/campaign-journal.jsonl', &
                cursor, n_page, page_bytes, records, next_cursor, journal_status, &
                message)
            if (journal_status /= JOURNAL_OK) then
                ierr = journal_status
                return
            end if
            do i = 1, size(records)
                record_session_id = ''
                record_generation = ''
                status = ''
                call gremlin_json_field(records(i)%json, 'session_id', &
                    record_session_id)
                call gremlin_json_field(records(i)%json, 'generation', &
                    record_generation)
                call gremlin_json_field(records(i)%json, 'status', status)
                if (trim(record_session_id) /= trim(session_id)) cycle
                if (trim(record_generation) /= trim(generation)) cycle
                if (.not. status_is_failure(trim(status))) cycle
                case_id = ''
                log_path = ''
                call gremlin_json_field(records(i)%json, 'case_id', case_id)
                call gremlin_json_field(records(i)%json, 'log_path', log_path)
                result = '{"generation":"'// &
                    trim(json_escape_string(trim(generation)))// &
                    '","case_id":"'//trim(json_escape_string(trim(case_id)))// &
                    '","status":"'//trim(json_escape_string(trim(status)))// &
                    '","log_path":"'//trim(json_escape_string(trim(log_path)))//'"}'
            end do
            n_read = n_read + size(records)
            if (next_cursor <= cursor .or. size(records) < n_page) exit
            cursor = next_cursor
        end do
    end subroutine latest_generation_failure

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
        logical :: legacy_failure_wait
        logical :: is_live

        poll_request = request
        poll_request%detail = 'full'
        until_kind = gremlin_wait_code(request%wait_until)
        wait_name = trim(request%wait_until)
        legacy_failure_wait = until_kind == 0 .and. request%fail_on_failure
        if (until_kind == 0) then
            if (legacy_failure_wait) then
                until_kind = GREMLIN_WAIT_FAILURE
                wait_name = 'failure'
            else
                wait_name = 'events'
            end if
        end if
        satisfied = .false.
        failed = .false.
        timed_out = .false.
        terminal = .false.
        elapsed = 0
        call clock_milliseconds(started_ms)
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
            call read_json_cursor(status_json, 'next_cursor', next_cursor)
            call read_json_cursor(status_json, 'next_lifecycle_cursor', &
                next_lifecycle_cursor)
            has_events = next_cursor > poll_request%cursor .or. &
                next_lifecycle_cursor > poll_request%lifecycle_cursor

            if (until_kind > 0) then
                satisfied = gremlin_wait_satisfied(until_kind, readiness)
            else
                satisfied = has_events
            end if
            terminal = trim(state_name) == 'stopped' .or. trim(state_name) == 'error'
            if (satisfied .or. terminal) exit

            call clock_milliseconds(now_ms)
            elapsed = int(max(0_int64, now_ms - started_ms))
            if (elapsed >= request%wait_ms) then
                timed_out = .true.
                exit
            end if
            if (legacy_failure_wait) then
                if (next_cursor > poll_request%cursor) &
                    poll_request%cursor = next_cursor
                if (next_lifecycle_cursor > poll_request%lifecycle_cursor) &
                    poll_request%lifecycle_cursor = next_lifecycle_cursor
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
        decorated = status_json(:len(status_json) - 1)// &
            ',"wait_until":"'//trim(json_escape_string(wait_name))//'",'// &
            '"wait_satisfied":'//trim(json_bool(satisfied))//','// &
            '"wait_timed_out":'//trim(json_bool(timed_out))//','// &
            '"wait_terminal":'//trim(json_bool(terminal))//','// &
            '"failure_observed":'//trim(json_bool(failed))
        if (len(durable_failure_report) > 1) decorated = decorated// &
            ',"durable_failure_report":'//durable_failure_report
        decorated = decorated//'}'
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
        if (legacy_failure_wait .and. failed) exitcode = 1
    end subroutine handle_wait

    subroutine read_json_cursor(json, name, value)
        character(len=*), intent(in) :: json, name
        integer(int64), intent(out) :: value
        character(len=64) :: raw
        integer :: ios

        value = 0_int64
        call gremlin_json_field(json, name, raw)
        read(raw, *, iostat=ios) value
        if (ios /= 0) value = 0_int64
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
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        type(gremlin_lease_t) :: reproduction_lease
        type(generation_t) :: generation
        type(execution_view_t) :: execution_view, build_view
        type(driver_pin_t) :: reproduction_pin
        type(gremlin_request_t) :: selection_request
        character(len=PATH_LEN) :: message, active_project, log_file, executable
        character(len=PATH_LEN) :: cleanup_message
        character(len=PATH_LEN) :: generation_root
        character(len=32) :: reproduction_id
        character(len=NAME_LEN) :: selected(MAX_NODES)
        character(len=HASH_LEN) :: active_identity
        character(len=16) :: outcome
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=128) :: session_id, owner_start
        character(len=128) :: build_view_owner
        character(len=:), allocatable :: packed, execution_env
        integer :: owner_pid, ierr, n_selected, mandatory_count, seed
        integer :: n_args, spawn_exit, test_exit, sequence, release_error
        integer :: reproduction_timeout
        character(len=128) :: view_owner
        logical :: retain_view
        logical :: have_reproduction_lease, executable_ok

        exitcode = 0
        have_reproduction_lease = .false.
        call gremlin_session_read(project_dir, trim(request%lane_id), session_id, &
            owner_pid, owner_start, status_text, ierr, message)
        if (ierr /= 0) then
            call error_response('reproduce', trim(message), response)
            exitcode = 2
            return
        end if
        if (len_trim(request%session_id) > 0 .and. &
            trim(request%session_id) /= trim(session_id)) then
            call error_response('reproduce', 'session_id does not match the lane owner', response)
            exitcode = 2
            return
        end if
        call gremlin_session_acquire(project_dir, trim(request%lane_id), session, &
            ierr, message)
        if (ierr /= 0) then
            call error_response('reproduce', trim(message), response)
            exitcode = 2
            return
        end if
        call gremlin_json_field(status_text, 'active_generation', active_identity)
        if (len_trim(request%generation_id) > 0) then
            active_identity = request%generation_id
        end if
        if (.not. is_hex_digest(active_identity)) then
            call release_if_owner(session, ierr, message)
            call error_response('reproduce', 'there is no compatible successful generation', &
                response)
            exitcode = 2
            return
        end if
        call common_generation_cas_root(generation_root, ierr, message)
        if (ierr /= 0) then
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', 'cannot resolve requested generation: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        generation_root = trim(generation_root)//'/gremlin/generations-v2/'// &
            trim(active_identity)
        active_project = trim(generation_root)//'/bundle/project'
        generation%identity = active_identity
        generation%root = generation_root
        generation%project_root = active_project
        call generation_inventory_restore(generation, ierr, message)
        call gremlin_generation_lease_acquire_at(generation%root, reproduction_lease, &
            ierr, message)
        if (ierr /= 0) then
            call error_response('reproduce', 'cannot lease requested generation: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        have_reproduction_lease = .true.
        call generation_driver_identity(generation%root, generation%driver_digest, &
            generation%driver_size, ierr, message)
        if (ierr /= 0) then
            call release_generation_lease(reproduction_lease, have_reproduction_lease, &
                release_error, cleanup_message)
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', &
                'cannot read generation driver identity: '//trim(message), response)
            exitcode = 2
            return
        end if
        call driver_pin_existing(session%state_dir, generation%driver_digest, &
            generation%driver_size, reproduction_pin, ierr, message)
        if (ierr /= 0) then
            call release_generation_lease(reproduction_lease, have_reproduction_lease, &
                release_error, cleanup_message)
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', &
                'cannot validate the pinned generation driver: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        generation%driver_path = reproduction_pin%path
        inquire (file=trim(active_project)//'/fpm.toml', exist=executable_ok)
        if (.not. executable_ok) then
            call release_generation_lease(reproduction_lease, have_reproduction_lease, &
                release_error, cleanup_message)
            call release_if_owner(session, ierr, message)
            call error_response('reproduce', 'requested generation snapshot is unavailable', &
                response)
            exitcode = 2
            return
        end if
        selection_request = request
        selection_request%targets = ''
        selection_request%targets(1) = request%case_id
        selection_request%n_targets = 1
        selection_request%random_count = 0
        call discover_campaign(trim(active_project), generation%identity, session, &
            selection_request, selected, n_selected, mandatory_count, seed, ierr, message, &
            bypass_coverage=.true.)
        if (ierr /= 0 .or. n_selected == 0) then
            call release_generation_lease(reproduction_lease, have_reproduction_lease, &
                release_error, cleanup_message)
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', 'case_id is not present in that generation', &
                response)
            exitcode = 2
            return
        end if
        if (trim(selected(1)) /= trim(request%case_id)) then
            call release_generation_lease(reproduction_lease, have_reproduction_lease, &
                release_error, cleanup_message)
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', 'case_id is not present in that generation', &
                response)
            exitcode = 2
            return
        end if
        executable = reproduction_pin%path
        ! The completion sequence is the reproduction execution ID. Allocate it
        ! before launch so its output path and durable receipt identify the same
        ! invocation, including concurrent reproductions in one live session.
        sequence = int(system_clock_count())
        write (reproduction_id, '(i0)') sequence
        call log_path(session, 'reproduce-'//trim(reproduction_id)//'.log', log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'test')
        call argv_push(packed, n_args, '--json')
        if (is_slow_test(trim(request%case_id))) then
            call argv_push(packed, n_args, '--all')
        end if
        call argv_push(packed, n_args, trim(request%case_id))
        reproduction_timeout = case_wall_timeout(active_project, request%case_id, &
            request%timeout_seconds)
        write(view_owner, '(a,"-reproduce-",i0)') trim(session%session_id), sequence
        write(build_view_owner, '(a,"-build")') trim(view_owner)
        call execution_view_create(trim(session%state_dir)//'/views', &
            generation%identity, trim(build_view_owner), 'build', &
            generation%input_inventory, generation%input_inventory_ready, &
            generation%input_inventory_complete, build_view, ierr, message, &
            candidate_bundle_root=trim(generation%root)//'/bundle')
        if (ierr /= 0) then
            call release_generation_lease(reproduction_lease, &
                have_reproduction_lease, release_error, cleanup_message)
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', 'cannot create private build view: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        call execution_view_create(trim(session%state_dir)//'/views', &
            generation%identity, trim(view_owner), request%case_id, &
            generation%input_inventory, generation%input_inventory_ready, &
            generation%input_inventory_complete, execution_view, ierr, message)
        if (ierr /= 0) then
            call execution_view_release(build_view, .false., release_error, &
                cleanup_message)
            call release_generation_lease(reproduction_lease, &
                have_reproduction_lease, release_error, cleanup_message)
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', 'cannot create private execution view: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        execution_env = 'FO_BIN='//trim(executable)//';FO='//trim(executable)//';'// &
            'FO_DISABLE_SELF_REFRESH=1;FO_SELF_REFRESH=0;'// &
            'FO_GREMLIN_EXECUTION_CWD='//trim(execution_view%cwd)//';'// &
            'TMPDIR='//trim(execution_view%tmpdir)
        call process_start_argv_logged(trim(build_view%cwd), packed, n_args, &
            trim(log_file), owner_pid, spawn_exit, &
            trim(execution_env))
        if (spawn_exit == 0) then
            call process_wait_bounded(owner_pid, reproduction_timeout, test_exit)
            if (test_exit == 124) then
                outcome = 'TIMEOUT'
                test_exit = 124
            else if (test_exit == 125) then
                outcome = 'INFRA_ERROR'
                call gremlin_generation_pin_at(generation%root, .true., &
                    release_error, cleanup_message)
            else
                outcome = runner_case_outcome(log_file, request%case_id)
            end if
        else
            test_exit = spawn_exit
            outcome = 'INFRA_ERROR'
        end if
        if (spawn_exit == 0) call append_execution_provenance(log_file, execution_view)
        retain_view = spawn_exit == 0 .and. test_exit /= 0
        call execution_view_release(execution_view, retain_view, release_error, &
            cleanup_message)
        call execution_view_release(build_view, retain_view, release_error, &
            cleanup_message)
        call record_immediate_case(session, request, generation, request%case_id, &
            test_exit, trim(outcome), sequence, 1, seed, trim(log_file), ierr, message, &
            credit_coverage=.false., gate_required=.true.)
        call release_generation_lease(reproduction_lease, have_reproduction_lease, &
            release_error, cleanup_message)
        if (release_error /= 0) then
            call error_response('reproduce', 'cannot release generation lease: '// &
                trim(cleanup_message), response)
            exitcode = 2
            return
        end if
        call release_if_owner(session, release_error, cleanup_message)
        if (release_error /= 0) then
            call error_response('reproduce', 'cannot release Gremlin ownership: '// &
                trim(cleanup_message), response)
            exitcode = 2
            return
        end if
        if (ierr /= JOURNAL_OK) then
            call error_response('reproduce', trim(message), response)
            exitcode = 2
            return
        end if
        if (test_exit /= 0) exitcode = 1
        call simple_response('reproduce', request%lane_id, session_id, trim(outcome), response)
    end subroutine handle_reproduce

    subroutine run_owner(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        type(driver_pin_t) :: driver_pin
        type(gremlin_request_t), allocatable :: owner_request
        type(gremlin_lease_t) :: active_lease, candidate_lease
        type(generation_t) :: active_generation, candidate_generation
        type(change_watch_t) :: change_watch
        type(child_t) :: build_child, test_child
        character(len=PATH_LEN) :: message, state_name, last_failed_identity
        character(len=PATH_LEN) :: capture_diagnostic
        character(len=PATH_LEN) :: fatal_message, state_message
        character(len=PATH_LEN) :: build_view_cleanup_message
        character(len=16) :: observed_outcome
        character(len=PATH_LEN) :: owner_start
        character(len=NAME_LEN), allocatable :: selected(:)
        character(len=HASH_LEN) :: stored_policy
        character(len=65536) :: status_text
        character(len=128) :: stored_session_id
        integer :: ierr, build_exit, test_exit, launch_error, completed, selected_count
        integer :: cancel_error, release_error, capture_error, state_error
        integer :: registration_error
        integer :: build_view_cleanup_status
        integer :: owner_pid, i, cache_cursor, cache_status
        integer :: sequence, campaign_seed, campaign_number, test_index
        integer(int64) :: capture_debounce_ms, campaign_started_ms, freshness_ticket
        integer(int64) :: last_idle_reap_ms, last_cache_maintenance_ms, now_ms
        character(len=PATH_LEN) :: freshness_path
        real :: test_timeout
        logical :: have_active, have_candidate, stop_requested, capture_ok
        logical :: capture_failed, provider_quiet
        logical :: have_active_lease, have_candidate_lease
        logical :: active_pinned, candidate_pinned
        logical :: build_done, test_done, fatal_error, cache_warning_reported

        exitcode = 0
        allocate (owner_request, selected(MAX_NODES))
        active_generation = generation_t()
        candidate_generation = generation_t()
        owner_request = request
        build_child = child_t()
        test_child = child_t()
        completed = 0
        selected_count = 0
        sequence = 0
        campaign_number = 0
        test_index = 0
        capture_debounce_ms = 0_int64
        campaign_started_ms = 0_int64
        last_idle_reap_ms = 0_int64
        last_cache_maintenance_ms = 0_int64
        cache_cursor = 0
        cache_warning_reported = .false.
        have_active = .false.
        have_candidate = .false.
        have_active_lease = .false.
        have_candidate_lease = .false.
        active_pinned = .false.
        candidate_pinned = .false.
        last_failed_identity = ''
        capture_diagnostic = ''
        state_name = 'starting'
        campaign_seed = request%seed
        if (campaign_seed == 0) then
            call system_clock(campaign_seed)
            if (campaign_seed == 0) campaign_seed = 1
        end if
        fatal_error = .false.
        fatal_message = ''
        call gremlin_session_acquire(project_dir, trim(request%lane_id), &
            session, ierr, message)
        if (ierr /= 0) then
            call error_response('run', trim(message), response)
            exitcode = 2
            return
        end if
        if (session%attached) then
            do i = 1, 60
                call gremlin_session_read(project_dir, trim(request%lane_id), &
                    stored_session_id, owner_pid, owner_start, status_text, ierr, message)
                if (ierr == 0 .and. len_trim(status_text) > 0) exit
                call fs_sleep_ms(50)
            end do
            if (ierr /= 0) then
                call error_response('run', trim(message), response)
                exitcode = 2
                return
            end if
            if (len_trim(status_text) == 0) then
                call error_response('run', 'active Gremlin owner has not published state', &
                    response)
                exitcode = 2
                return
            end if
            call gremlin_json_field(status_text, 'policy_key', stored_policy)
            if (trim(stored_policy) /= request_policy_key(request)) then
                call error_response('run', &
                    'an active Gremlin session has an incompatible policy', response)
                exitcode = 2
                return
            end if
            call simple_response('run', request%lane_id, trim(stored_session_id), &
                'attached', response)
            return
        end if
        call process_set_async_scope(session%state_dir, session%owner_pid, &
            session%owner_start, ierr)
        if (ierr /= 0) then
            call gremlin_session_release(session, release_error, state_message)
            if (release_error == 0) message = 'cannot configure exact process ownership: '// &
                trim(int_text(ierr))
            if (release_error /= 0) message = 'cannot release owner after process ownership '// &
                'setup failure: '//trim(state_message)
            call error_response('run', trim(message), response)
            exitcode = 2
            return
        end if
        call maintain_fo_cache(cache_cursor, cache_status)
        if (cache_status /= 0) then
            write (error_unit, '(a,i0)') 'Gremlin cache maintenance: ', cache_status
            cache_warning_reported = .true.
        end if
        call clock_milliseconds(last_cache_maintenance_ms)
        call gremlin_recover_owner_journal(project_dir, session, ierr, message)
        if (ierr /= 0) then
            call release_if_owner(session, state_error, state_message)
            call error_response('run', 'cannot recover prior Gremlin receipts: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        call driver_pin_current(session%state_dir, driver_pin, ierr, message)
        if (ierr /= 0) then
            call release_if_owner(session, state_error, state_message)
            call error_response('run', 'cannot pin the running fo driver: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        session%driver_path = driver_pin%path
        session%driver_digest = driver_pin%digest
        session%driver_size = driver_pin%size
        call fs_make_dir(session%state_dir//'/logs')
        call publish_state(session, owner_request, 'starting', active_generation, &
            candidate_generation, '', completed, selected_count, campaign_seed, 'NONE', 0, &
            ierr, message)
        if (ierr /= 0) then
            fatal_message = trim(message)
            call gremlin_session_release(session, release_error, state_message)
            if (release_error /= 0) fatal_message = trim(state_message)
            call error_response('run', 'cannot publish initial Gremlin state: '// &
                trim(fatal_message), response)
            exitcode = 2
            return
        end if
        call change_watch_init(change_watch, project_dir, ierr, message)
        if (ierr /= 0) then
            call change_watch_close(change_watch)
            call release_if_owner(session, state_error, state_message)
            call error_response('run', 'cannot start Gremlin change watcher: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        call capture_candidate(project_dir, driver_pin, candidate_generation, &
            candidate_lease, &
            capture_ok, registration_error, message, change_watch=change_watch)
        if (registration_error /= 0) then
            fatal_error = .true.
            fatal_message = 'cannot register captured generation: '//trim(message)
        end if
        if (capture_ok .and. registration_error == 0) then
            have_candidate = .true.
            have_candidate_lease = .true.
            if (.not. fatal_error) then
                call start_build(session, candidate_generation, build_child, ierr, message)
                if (ierr /= 0) then
                    launch_error = ierr
                    fatal_message = 'cannot start initial candidate build '// &
                        '(process setup error '//int_text(ierr)//')'
                    call publish_state(session, owner_request, 'build_failed', active_generation, &
                        candidate_generation, '', completed, selected_count, campaign_seed, &
                        'INFRA_ERROR', launch_error, state_error, state_message)
                    if (state_error == 0) then
                        sequence = sequence + 1
                        call record_immediate_case(session, request, candidate_generation, &
                            '<build>', launch_error, 'INFRA_ERROR', sequence, 0, &
                            campaign_seed, build_child%log_file, ierr, message, &
                            credit_coverage=.false.)
                    else
                        ierr = state_error
                        message = state_message
                    end if
                    if (ierr /= 0) fatal_message = trim(message)
                    fatal_error = .true.
                    last_failed_identity = candidate_generation%identity
                    have_candidate = .false.
                    if (have_candidate_lease) then
                        call gremlin_lease_release(candidate_lease, release_error, &
                            state_message)
                        if (release_error == 0) have_candidate_lease = .false.
                        if (release_error /= 0) fatal_message = &
                            'cannot release failed candidate lease: '//trim(state_message)
                    end if
                else
                    call publish_state(session, owner_request, 'building', active_generation, &
                        candidate_generation, '', completed, selected_count, campaign_seed, &
                        'NONE', 0, state_error, state_message)
                    if (state_error /= 0) then
                        fatal_error = .true.
                        fatal_message = trim(state_message)
                    end if
                    state_name = 'building'
                end if
            end if
        else if (.not. fatal_error) then
            capture_diagnostic = trim(message)
            call publish_state(session, owner_request, 'capture_failed', active_generation, &
                candidate_generation, '', completed, selected_count, 0, 'NONE', 0, &
                ierr, message, diagnostic=trim(capture_diagnostic))
            if (ierr /= 0) fatal_error = .true.
        end if

        do
            if (fatal_error) exit
            if (build_child%pid <= 0 .and. test_child%pid <= 0) then
                call clock_milliseconds(now_ms)
                if (now_ms == 0_int64 .or. &
                        now_ms - last_idle_reap_ms >= 1000_int64) then
                    call process_reap_adopted_scope_zombies(ierr)
                    if (ierr /= 0) then
                        message = 'cannot reap adopted Gremlin children ('// &
                            trim(int_text(ierr))//')'
                        fatal_error = .true.
                        exit
                    end if
                    last_idle_reap_ms = now_ms
                end if
                if (now_ms - last_cache_maintenance_ms >= 5000_int64) then
                    call maintain_fo_cache(cache_cursor, cache_status)
                    if (cache_status /= 0 .and. .not. cache_warning_reported) &
                        write (error_unit, '(a,i0)') &
                            'Gremlin cache maintenance: ', cache_status
                    cache_warning_reported = cache_status /= 0
                    last_cache_maintenance_ms = now_ms
                end if
            end if
            call gremlin_session_stop_requested(session, stop_requested, ierr, message)
            if (ierr /= 0) then
                fatal_error = .true.
                exit
            end if
            if (stop_requested) exit

            if (build_child%pid > 0) then
                call process_poll_pid(build_child%pid, build_done, build_exit)
                if (build_done) then
                    call complete_build(session, owner_request, candidate_generation, &
                        build_child, build_exit, active_generation, have_active, &
                        test_child, active_lease, candidate_lease, &
                        have_active_lease, have_candidate_lease, active_pinned, &
                        candidate_pinned, selected, selected_count, campaign_seed, &
                        completed, state_name, last_failed_identity, sequence, ierr, message)
                    if (ierr /= 0) then
                        build_child%pid = 0
                        if (build_child%execution_view%active) then
                            call execution_view_release( &
                                build_child%execution_view, .false., &
                                build_view_cleanup_status, &
                                build_view_cleanup_message)
                        end if
                        if (candidate_generation%build_project_root == &
                                build_child%execution_view%cwd) &
                            candidate_generation%build_project_root = ''
                        fatal_error = .true.
                        exit
                    end if
                    if (build_exit == 0 .and. have_active .and. selected_count > 0) then
                        campaign_number = campaign_number + 1
                        test_index = 1
                        call clock_milliseconds(campaign_started_ms)
                        call launch_selected_case(session, owner_request, active_generation, &
                            selected, selected_count, test_index, campaign_seed, &
                            campaign_number, test_child, ierr, message, completed, sequence)
                        if (ierr /= 0) then
                            fatal_error = .true.
                            exit
                        end if
                    end if
                    have_candidate = .false.
                end if
            end if
            if (fatal_error) exit

            if (test_child%pid > 0) then
                test_timeout = real(case_wall_timeout(active_generation%project_root, &
                    test_child%case_name, request%timeout_seconds))
                call process_poll_pid(test_child%pid, test_done, test_exit)
                if (test_done) then
                    state_name = 'testing'
                    observed_outcome = runner_case_outcome(test_child%log_file, &
                        test_child%case_name)
                    call append_execution_provenance(test_child%log_file, &
                        test_child%execution_view)
                    call record_case(session, owner_request, active_generation, &
                        test_child, test_exit, observed_outcome, sequence, campaign_seed, &
                        ierr, message)
                    call execution_view_release(test_child%execution_view, &
                        test_exit /= 0 .or. trim(observed_outcome) /= 'PASS', &
                        release_error, state_message)
                    test_child%pid = 0
                    completed = completed + 1
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                    call publish_state(session, owner_request, state_name, &
                        active_generation, candidate_generation, test_child%case_name, &
                        completed, selected_count, campaign_seed, observed_outcome, &
                        test_exit, ierr, message)
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                    call advance_campaign(project_dir, session, owner_request, &
                        active_generation, selected, &
                        selected_count, test_index, campaign_seed, campaign_number, &
                        campaign_started_ms, test_child, ierr, message, completed, &
                        state_name, sequence)
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                else if (elapsed_seconds(test_child%started_at) >= test_timeout) then
                    call cancel_owned_process(test_child%pid, test_exit)
                    if (test_exit /= 0) then
                        message = 'cannot cancel timed-out Gremlin test process'
                        fatal_error = .true.
                        exit
                    end if
                    call append_execution_provenance(test_child%log_file, &
                        test_child%execution_view)
                    call execution_view_release(test_child%execution_view, .true., &
                        release_error, state_message)
                    call record_case(session, owner_request, active_generation, &
                        test_child, 124, 'TIMEOUT', sequence, campaign_seed, ierr, message)
                    test_child%pid = 0
                    completed = completed + 1
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                    call publish_state(session, owner_request, state_name, &
                        active_generation, candidate_generation, test_child%case_name, &
                        completed, selected_count, campaign_seed, 'TIMEOUT', 124, &
                        ierr, message)
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                    call advance_campaign(project_dir, session, owner_request, &
                        active_generation, selected, &
                        selected_count, test_index, campaign_seed, campaign_number, &
                        campaign_started_ms, test_child, ierr, message, completed, &
                        state_name, sequence)
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                end if
            end if
            if (fatal_error) exit

            ! Snapshot consumers before the provider poll: a request arriving
            ! after this point must wait for the next poll, never an older one.
            freshness_path = trim(session%state_dir)//'/freshness-'// &
                trim(session%session_id)
            freshness_ticket = 0_int64
            call gremlin_freshness_update(trim(freshness_path), 0, &
                freshness_ticket, state_error)
            if (state_error /= 0) freshness_ticket = 0_int64
            call maybe_capture_latest(project_dir, session, driver_pin, owner_request, &
                active_generation, &
                candidate_generation, have_active, have_candidate, last_failed_identity, &
                build_child, candidate_lease, have_candidate_lease, capture_debounce_ms, &
                change_watch, sequence, completed, selected_count, campaign_seed, &
                current_test_name(selected, selected_count), capture_failed, ierr, message, &
                provider_quiet)
            if (ierr /= 0) then
                fatal_message = trim(message)
                fatal_error = .true.
                exit
            end if
            if (capture_failed) then
                state_name = 'capture_failed'
                capture_error = 1
                capture_diagnostic = trim(message)
                call publish_state(session, owner_request, state_name, active_generation, &
                    candidate_generation, current_test_name(selected, selected_count), &
                    completed, selected_count, campaign_seed, 'NONE', capture_error, &
                    state_error, state_message, diagnostic=trim(capture_diagnostic))
                if (state_error /= 0) then
                    fatal_message = 'cannot publish capture failure: '//trim(state_message)
                    fatal_error = .true.
                    exit
                end if
                ierr = 0
            end if
            if (freshness_ticket > 0_int64 .and. &
                (owner_request%input_changed .or. provider_quiet)) &
                call gremlin_freshness_update(trim(freshness_path), 2, &
                freshness_ticket, state_error)
            call fs_sleep_ms(100)
        end do
        call change_watch_close(change_watch)

        if (fatal_error) then
            if (len_trim(fatal_message) == 0) fatal_message = trim(message)
            if (len_trim(fatal_message) == 0) &
                fatal_message = 'Gremlin stopped after an evidence or process error'
            cancel_error = 0
            if (test_child%pid > 0) then
                call cancel_test_case(session, active_generation, test_child, &
                test_exit, state_message)
                if (test_exit /= 0) cancel_error = test_exit
                if (test_exit == 0) test_child%pid = 0
            end if
            if (build_child%pid > 0) then
                call cancel_owned_process(build_child%pid, build_exit)
                if (build_exit /= 0 .and. cancel_error == 0) cancel_error = build_exit
                if (build_exit == 0) then
                    build_child%pid = 0
                    if (build_child%execution_view%active) then
                        call execution_view_release( &
                            build_child%execution_view, .false., &
                            build_view_cleanup_status, build_view_cleanup_message)
                    end if
                    candidate_generation%build_project_root = ''
                end if
            end if
            if (cancel_error == 0) then
                if (candidate_pinned) then
                    call gremlin_generation_pin_at(candidate_generation%root, .false., &
                        state_error, state_message)
                    if (state_error == 0) candidate_pinned = .false.
                    if (state_error /= 0) fatal_message = &
                        'cannot release failed candidate pin: '//trim(state_message)
                end if
                call release_generation_lease(candidate_lease, have_candidate_lease, &
                    state_error, state_message)
                if (state_error /= 0) fatal_message = trim(state_message)
                call release_generation_lease(active_lease, have_active_lease, &
                    state_error, state_message)
                if (state_error /= 0) fatal_message = trim(state_message)
            else if (build_child%pid > 0 .and. len_trim(candidate_generation%root) > 0) then
                call gremlin_generation_pin_at(candidate_generation%root, .true., &
                    state_error, state_message)
            end if
            call publish_state(session, owner_request, 'error', active_generation, &
                candidate_generation, current_test_name(selected, selected_count), &
                completed, selected_count, campaign_seed, 'INFRA_ERROR', 1, &
                state_error, state_message, diagnostic=trim(fatal_message))
            if (state_error /= 0) fatal_message = trim(state_message)
            if (cancel_error /= 0) fatal_message = &
                'process cancellation failed: '//trim(int_text(cancel_error))
            if (cancel_error == 0 .and. state_error == 0) then
                call gremlin_publish_terminal_snapshot(project_dir, session, release_error, &
                    state_message)
                if (release_error == 0) then
                    call gremlin_session_release(session, release_error, state_message)
                end if
                if (release_error /= 0) fatal_message = &
                    'cannot preserve terminal Gremlin ownership record: '//trim(state_message)
            end if
            call error_response('run', trim(fatal_message), response)
            exitcode = 2
            return
        end if
        stop_requested = test_child%pid > 0 .or. build_child%pid > 0
        cancel_error = 0
        if (test_child%pid > 0) then
            call cancel_test_case(session, active_generation, test_child, &
                test_exit, state_message)
            if (test_exit /= 0) cancel_error = test_exit
            if (test_exit == 0) test_child%pid = 0
        end if
        if (build_child%pid > 0) then
            call cancel_owned_process(build_child%pid, build_exit)
            if (build_exit /= 0 .and. cancel_error == 0) cancel_error = build_exit
            if (build_exit == 0) then
                build_child%pid = 0
                if (build_child%execution_view%active) &
                    call execution_view_release(build_child%execution_view, .false., &
                        release_error, state_message)
                candidate_generation%build_project_root = ''
            end if
        end if
        if (cancel_error /= 0) then
            call publish_state(session, owner_request, 'error', active_generation, &
                candidate_generation, current_test_name(selected, selected_count), &
                completed, selected_count, campaign_seed, 'INFRA_ERROR', cancel_error, &
                state_error, state_message)
            if (build_child%pid > 0 .and. len_trim(candidate_generation%root) > 0) then
                call gremlin_generation_pin_at(candidate_generation%root, .true., &
                    release_error, message)
            end if
            call error_response('run', 'cannot cancel owned Gremlin work: '// &
                trim(int_text(cancel_error))//'; '//trim(state_message), response)
            exitcode = 2
            return
        end if
        call release_generation_lease(candidate_lease, have_candidate_lease, &
            state_error, state_message)
        if (state_error == 0) call release_generation_lease(active_lease, &
            have_active_lease, state_error, state_message)
        if (state_error /= 0) then
            call publish_state(session, owner_request, 'error', active_generation, &
                candidate_generation, '', completed, selected_count, campaign_seed, &
                'INFRA_ERROR', state_error, release_error, message)
            if (release_error == 0) then
                call gremlin_publish_terminal_snapshot(project_dir, session, release_error, message)
                if (release_error == 0) then
                    call gremlin_session_release(session, release_error, message)
                end if
            end if
            call error_response('run', 'cannot release generation lease: '// &
                trim(state_message), response)
            exitcode = 2
            return
        end if
        state_name = 'NONE'
        if (stop_requested) state_name = 'CANCELLED'
        call publish_state(session, owner_request, 'stopped', active_generation, &
            candidate_generation, '', completed, selected_count, campaign_seed, &
            state_name, 0, ierr, message, status_text_out=status_text)
        if (ierr /= 0) then
            state_error = ierr
            call gremlin_release_stopped_session(project_dir, session, &
                trim(status_text), release_error, state_message)
            if (release_error /= 0) then
                call error_response('run', 'cannot durably terminalize stopped Gremlin '// &
                    'state after state publication error ('//trim(int_text(state_error))// &
                    '): '//trim(state_message), response)
            else
                call error_response('run', 'cannot publish stopped Gremlin state ('// &
                    trim(int_text(state_error))//'); stored the constructed terminal state', &
                    response)
            end if
            exitcode = 2
            return
        end if
        call gremlin_release_stopped_session(project_dir, session, trim(status_text), &
            ierr, message)
        if (ierr /= 0) then
            call error_response('run', trim(message), response)
            exitcode = 2
            return
        end if
        if (len_trim(active_generation%build_project_root) > 0) then
            call fs_remove_tree(trim(active_generation%build_project_root))
            active_generation%build_project_root = ''
        end if
        if (len_trim(candidate_generation%root) > 0) then
            call gremlin_generation_prune_inactive(candidate_generation%root, &
                state_error, state_message)
        else if (len_trim(active_generation%root) > 0) then
            call gremlin_generation_prune_inactive(active_generation%root, &
                state_error, state_message)
        end if
        if (state_error /= 0) write (error_unit, '(a)') &
            'Gremlin generation cleanup: '//trim(state_message)
        call maintain_fo_cache(cache_cursor, cache_status)
        if (cache_status /= 0) write (error_unit, '(a,i0)') &
            'Gremlin cache maintenance at stop: ', cache_status
        call simple_response('run', request%lane_id, session%session_id, 'stopped', response)
    end subroutine run_owner

    subroutine maintain_fo_cache(scan_cursor, ierr)
        integer, intent(inout) :: scan_cursor
        integer, intent(out) :: ierr
        type(action_result_store_t) :: store
        character(len=PATH_LEN) :: root
        integer :: scanned, retired, deleted, scanned_rows, compacted
        integer :: action_status, compact_status

        call cache_store_root(root)
        call action_result_store_init(store, trim(root), ierr)
        if (ierr /= ACTION_RESULT_OK) return
        call action_result_maintenance_tick(store, scanned, retired, deleted, &
            action_status)
        call immutable_store_compact_generation_roots(store%objects, &
            4096, 1, 4096, scanned_rows, compacted, compact_status, scan_cursor)
        ierr = action_status
        if (ierr == ACTION_RESULT_OK .and. compact_status /= IMMUTABLE_OK) &
            ierr = compact_status
    end subroutine maintain_fo_cache

    subroutine maybe_capture_latest(project_dir, session, driver_pin, request, &
            active, candidate, &
            have_active, have_candidate, last_failed, build_child, candidate_lease, &
            have_candidate_lease, capture_debounce_ms, change_watch, sequence, &
            completed, selected_count, seed, current_case, capture_failed, ierr, message, &
            provider_quiet)
        character(len=*), intent(in) :: project_dir
        type(gremlin_session_t), intent(in) :: session
        type(driver_pin_t), intent(in) :: driver_pin
        type(gremlin_request_t), intent(inout) :: request
        type(generation_t), intent(in) :: active
        type(generation_t), intent(inout) :: candidate
        logical, intent(in) :: have_active
        logical, intent(inout) :: have_candidate
        character(len=*), intent(inout) :: last_failed
        type(child_t), intent(inout) :: build_child
        type(gremlin_lease_t), intent(inout) :: candidate_lease
        logical, intent(inout) :: have_candidate_lease
        integer(int64), intent(inout) :: capture_debounce_ms
        type(change_watch_t), intent(inout) :: change_watch
        integer, intent(inout) :: sequence
        integer, intent(in) :: completed, selected_count, seed
        character(len=*), intent(in) :: current_case
        logical, intent(out) :: capture_failed, provider_quiet
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(generation_t) :: current
        type(gremlin_lease_t) :: current_lease
        character(len=PATH_LEN) :: changed_path
        logical :: ok, got_event
        integer(int64) :: now_ms
        integer :: spawn_error, journal_error, release_error, event_type, watch_error
        integer :: state_error
        character(len=PATH_LEN) :: state_message

        ierr = 0
        message = ''
        capture_failed = .false.
        ! A zero-timeout provider poll deliberately cannot finish a pending
        ! structural reconciliation. Give the watcher a bounded tick so its
        ! quiet deadline can expire while the owner loop remains responsive.
        call change_watch_poll(change_watch, 1, changed_path, event_type, &
            got_event, watch_error, provider_quiet)
        if (watch_error /= 0) then
            ierr = watch_error
            message = 'Gremlin change watcher reconciliation failed'
            return
        end if
        call clock_milliseconds(now_ms)
        if (got_event) then
            request%event_epoch = request%event_epoch + 1
            request%input_changed = .true.
            call publish_state(session, request, 'capture_pending', active, candidate, &
                current_case, completed, selected_count, seed, 'NONE', 0, ierr, message)
            if (ierr /= 0) return
            capture_debounce_ms = now_ms + 250_int64
            return
        end if
        if (capture_debounce_ms <= 0_int64 .or. now_ms < capture_debounce_ms) return
        capture_debounce_ms = 0_int64
        call capture_candidate(project_dir, driver_pin, current, current_lease, ok, &
            release_error, message, change_watch)
        if (release_error /= 0) then
            ierr = release_error
            message = 'cannot register captured generation: '//trim(message)
            return
        end if
        if (.not. ok) then
            capture_failed = .true.
            return
        end if
        if (have_candidate) then
            if (current%identity == candidate%identity) then
                call gremlin_lease_release(current_lease, release_error, message)
                if (release_error /= 0) ierr = release_error
                return
            end if
            if (build_child%pid > 0) then
                call cancel_owned_process(build_child%pid, ierr)
                if (ierr /= 0) then
                    message = 'cannot cancel superseded candidate build process'
                    return
                end if
                build_child%pid = 0
            end if
            if (have_candidate_lease) then
                call gremlin_lease_release(candidate_lease, release_error, message)
                if (release_error /= 0) then
                    ierr = release_error
                    message = 'cannot release superseded candidate generation lease'
                    return
                end if
                have_candidate_lease = .false.
            end if
            have_candidate = .false.
        end if
        if (have_active) then
            if (current%identity == active%identity) then
                call gremlin_lease_release(current_lease, release_error, message)
                if (release_error /= 0) then
                    ierr = release_error
                    return
                end if
                request%input_changed = .false.
                call publish_state(session, request, 'testing', active, current, &
                    current_case, completed, selected_count, seed, 'NONE', 0, ierr, message)
                return
            end if
        end if
        if (current%identity == last_failed) then
            call gremlin_lease_release(current_lease, release_error, message)
            if (release_error /= 0) ierr = release_error
            return
        end if
        candidate = current
        have_candidate = .true.
        candidate_lease = current_lease
        current_lease%lock_fd = -1
        have_candidate_lease = .true.
        call start_build(session, candidate, build_child, spawn_error, message)
        if (spawn_error /= 0) then
            have_candidate = .false.
            last_failed = candidate%identity
            sequence = sequence + 1
            call record_immediate_case(session, request, candidate, '<build>', &
                spawn_error, 'INFRA_ERROR', sequence, 0, request%seed, &
                build_child%log_file, journal_error, message)
            call gremlin_lease_release(candidate_lease, release_error, message)
            if (release_error == 0) have_candidate_lease = .false.
            if (journal_error /= JOURNAL_OK) then
                ierr = journal_error
                return
            end if
            if (release_error /= 0) then
                ierr = release_error
                message = 'cannot release failed candidate generation lease'
                return
            end if
            ierr = spawn_error
            message = 'cannot start candidate build'
        else
            call publish_state(session, request, 'building', active, candidate, &
                current_case, completed, selected_count, seed, 'NONE', 0, &
                state_error, state_message)
            if (state_error /= 0) then
                ierr = state_error
                message = 'cannot publish candidate build start: '//trim(state_message)
            end if
        end if
    end subroutine maybe_capture_latest

    subroutine start_build(session, generation, child, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(generation_t), intent(inout) :: generation
        type(child_t), intent(out) :: child
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: executable, view_message
        character(len=:), allocatable :: packed
        integer :: n_args, spawn_exit, view_status
        logical :: exists

        child = child_t()
        if (generation%driver_digest /= session%driver_digest .or. &
            generation%driver_size /= session%driver_size .or. &
            len_trim(session%driver_path) == 0) then
            ierr = 1
            message = 'candidate generation does not match the pinned fo driver'
            return
        end if
        inquire (file=trim(session%driver_path), exist=exists)
        if (.not. exists) then
            ierr = 1
            message = 'pinned fo driver is missing; refusing to launch candidate build'
            return
        end if
        executable = session%driver_path
        generation%build_project_root = ''
        call execution_view_create(trim(session%state_dir)//'/views', &
            generation%identity, trim(session%session_id)//'-build', 'build', &
            generation%input_inventory, generation%input_inventory_ready, &
            generation%input_inventory_complete, child%execution_view, ierr, message, &
            candidate_bundle_root=trim(generation%root)//'/bundle')
        if (ierr /= 0) then
            message = 'cannot create candidate build view: '//trim(message)
            return
        end if
        generation%build_project_root = child%execution_view%cwd
        call log_path(session, 'build-'//generation%identity(1:16), child%log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'build')
        call process_start_argv_logged(trim(child%execution_view%cwd), packed, &
            n_args, trim(child%log_file), child%pid, spawn_exit, &
            'FO_DISABLE_SELF_REFRESH=1;FO_SELF_REFRESH=0;TMPDIR='// &
            trim(child%execution_view%tmpdir))
        ierr = spawn_exit
        if (ierr /= 0) then
            message = 'cannot start candidate build'
            child%pid = 0
            call execution_view_release(child%execution_view, .false., view_status, &
                view_message)
            generation%build_project_root = ''
            return
        end if
        call clock_seconds(child%started_at)
        message = ''
    end subroutine start_build

    subroutine complete_build(session, request, candidate, build_child, build_exit, &
            active, have_active, test_child, active_lease, candidate_lease, &
            have_active_lease, have_candidate_lease, active_pinned, candidate_pinned, &
            selected, selected_count, seed, completed, state_name, last_failed, &
            sequence, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(inout) :: request
        type(generation_t), intent(inout) :: candidate, active
        type(child_t), intent(inout) :: build_child, test_child
        type(gremlin_lease_t), intent(inout) :: active_lease, candidate_lease
        integer, intent(in) :: build_exit
        logical, intent(inout) :: have_active
        logical, intent(inout) :: have_active_lease, have_candidate_lease
        logical, intent(inout) :: active_pinned, candidate_pinned
        character(len=*), intent(out) :: selected(:)
        integer, intent(out) :: selected_count
        integer, intent(inout) :: seed
        integer, intent(in) :: completed
        character(len=*), intent(out) :: state_name, last_failed
        integer, intent(inout) :: sequence
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(gremlin_lease_t) :: new_active_lease
        type(gremlin_request_t), allocatable :: selection_request
        integer :: inventory_status, cancel_exit, mandatory_count, state_status
        integer :: release_status, pin_status
        character(len=PATH_LEN) :: state_message, release_message
        character(len=PATH_LEN) :: coverage_path, discovery_diagnostic
        type(gremlin_coverage_view_t) :: coverage_view
        character(len=NAME_LEN) :: active_case
        logical :: was_active

        ierr = 0
        message = ''
        allocate (selection_request)
        was_active = have_active
        call record_build(session, request, candidate, build_exit, build_child%log_file, &
            sequence, ierr, message)
        if (ierr /= 0) return
        build_child%pid = 0
        if (build_exit /= 0) then
            if (build_child%execution_view%active) &
                call execution_view_release(build_child%execution_view, .false., &
                    release_status, release_message)
            candidate%build_project_root = ''
            last_failed = candidate%identity
            state_name = 'build_failed'
            active_case = ''
            if (test_child%pid > 0) then
                active_case = test_child%case_name
            end if
            call publish_state(session, request, state_name, active, candidate, &
                trim(active_case), &
                completed, selected_count, seed, 'BUILD_FAIL', build_exit, ierr, message)
            if (ierr /= 0) return
            if (have_candidate_lease) then
                call gremlin_lease_release(candidate_lease, release_status, release_message)
                if (release_status /= 0) then
                    ierr = release_status
                    message = 'cannot release failed candidate generation lease: '// &
                        trim(release_message)
                    return
                end if
                have_candidate_lease = .false.
            end if
            return
        end if
        if (allocated(request%impact_cases)) deallocate(request%impact_cases)
        request%n_impact_cases = 0
        request%impact_all = .false.
        if (was_active) call compute_generation_impact(active, candidate, request)
        call gremlin_generation_lease_acquire_at(candidate%root, new_active_lease, &
            ierr, message)
        if (ierr /= 0) then
            message = 'cannot lease successfully built generation: '//trim(message)
            return
        end if
        call gremlin_generation_pin_at(candidate%root, .true., pin_status, message)
        if (pin_status /= 0) then
            ierr = pin_status
            message = 'cannot preserve successfully built generation: '//trim(message)
            call gremlin_lease_release(new_active_lease, release_status, release_message)
            return
        end if
        candidate_pinned = .true.
        if (test_child%pid > 0) then
            call cancel_test_case(session, active, test_child, cancel_exit, message)
            if (cancel_exit /= 0) then
                ierr = cancel_exit
                message = 'cannot cancel obsolete generation test process'
                call gremlin_generation_pin_at(candidate%root, .false., &
                    pin_status, release_message)
                if (pin_status == 0) candidate_pinned = .false.
                call gremlin_lease_release(new_active_lease, release_status, release_message)
                return
            end if
            test_child%pid = 0
        end if

        if (active_pinned) then
            call gremlin_generation_pin_at(active%root, .false., pin_status, message)
            if (pin_status /= 0) then
                ierr = pin_status
                message = 'cannot release previous generation pin: '//trim(message)
                call gremlin_generation_pin_at(candidate%root, .false., &
                    state_status, release_message)
                if (state_status == 0) candidate_pinned = .false.
                call gremlin_lease_release(new_active_lease, release_status, release_message)
                return
            end if
            active_pinned = .false.
        end if
        if (have_active_lease) then
            call gremlin_lease_release(active_lease, release_status, release_message)
            if (release_status /= 0) then
                call gremlin_generation_pin_at(active%root, .true., state_status, &
                    release_message)
                if (state_status == 0) active_pinned = .true.
                call gremlin_generation_pin_at(candidate%root, .false., &
                    state_status, release_message)
                if (state_status == 0) candidate_pinned = .false.
                call gremlin_lease_release(new_active_lease, state_status, release_message)
                ierr = release_status
                message = 'cannot release previous generation lease: '//trim(release_message)
                return
            end if
            have_active_lease = .false.
        end if

        active_lease%resource = new_active_lease%resource
        active_lease%slot = new_active_lease%slot
        active_lease%lock_fd = new_active_lease%lock_fd
        new_active_lease%lock_fd = -1
        new_active_lease%slot = -1
        have_active_lease = .true.
        candidate%build_project_root = build_child%execution_view%cwd
        if (was_active .and. len_trim(active%build_project_root) > 0) &
            call fs_remove_tree(trim(active%build_project_root))
        request%has_previous_generation = was_active
        request%requirement_digest = ''
        request%gate_cases = ''
        active = candidate
        build_child%execution_view%active = .false.
        have_active = .true.
        request%input_changed = .false.
        request%gate_required_count = 0
        active_pinned = .true.
        candidate_pinned = .false.
        if (have_candidate_lease) then
            call gremlin_lease_release(candidate_lease, release_status, release_message)
            if (release_status /= 0) then
                ierr = release_status
                message = 'cannot release candidate generation lease: '//trim(release_message)
                return
            end if
            have_candidate_lease = .false.
        end if
        last_failed = ''
        if (was_active) seed = next_campaign_seed(seed)
        selection_request = request
        selection_request%seed = seed
        call discover_campaign(active%project_root, active%identity, session, &
            selection_request, selected, selected_count, mandatory_count, seed, &
            inventory_status, message)
        request%requirement_digest = selection_request%requirement_digest
        request%gate_cases = selection_request%gate_cases
        request%gate_required_count = selection_request%gate_required_count
        if (inventory_status /= 0) then
            state_name = 'inventory_failed'
            selected_count = 0
            discovery_diagnostic = trim(message)
            if (len_trim(discovery_diagnostic) == 0) &
                discovery_diagnostic = 'discovery returned an unspecified error'
            call publish_state(session, request, state_name, active, candidate, '', &
                completed, selected_count, seed, 'INFRA_ERROR', inventory_status, &
                ierr, message, diagnostic=trim(discovery_diagnostic))
            if (ierr == 0) then
                ierr = inventory_status
                message = 'cannot discover the frozen test inventory: '// &
                    trim(discovery_diagnostic)
            end if
            return
        end if
        state_name = 'testing'
        if (selected_count == 0) then
            state_name = 'idle'
            coverage_path = trim(session%state_dir)//'/coverage-'// &
                trim(active%identity)//'.state'
            call coverage_read_view_path(trim(coverage_path), active%identity, &
                coverage_view, inventory_status, message)
            if (inventory_status == COVERAGE_OK) then
                if (coverage_view%green .and. request%gate_required_count > 0) &
                    state_name = 'quiescent'
            else if (inventory_status /= COVERAGE_NOT_FOUND) then
                ierr = inventory_status
                return
            end if
        end if
        call publish_state(session, request, state_name, active, candidate, &
            current_test_name(selected, selected_count), completed, selected_count, &
            seed, 'NONE', 0, ierr, message)
        if (ierr == 0 .and. was_active) then
            call gremlin_generation_prune_inactive(active%root, state_status, &
                release_message)
            if (state_status /= 0) write (error_unit, '(a)') &
                'Gremlin generation cleanup: '//trim(release_message)
        end if
    end subroutine complete_build

    subroutine compute_generation_impact(baseline, candidate, request)
        type(generation_t), intent(inout) :: baseline, candidate
        type(gremlin_request_t), intent(inout) :: request

        type(dag_t) :: dag
        type(test_impact_case_t), allocatable :: cases(:)
        type(test_impact_result_t) :: impact
        integer, allocatable :: changed_ids(:), affected_ids(:)
        integer, allocatable :: candidate_ids(:), selected_node_ids(:)
        integer :: n_changed, n_affected, n_cached, n_all, ierr, i
        character(len=MAX_PATH), allocatable :: filenames(:)
        character(len=NAME_LEN), allocatable :: all_names(:)
        character(len=PATH_LEN) :: message
        logical, allocatable :: is_test_arr(:)
        logical :: dependency_model_complete

        allocate (changed_ids(MAX_NODES), affected_ids(MAX_NODES), &
            candidate_ids(MAX_NODES), selected_node_ids(MAX_NODES), &
            filenames(MAX_NODES), all_names(MAX_NODES), is_test_arr(MAX_NODES))
        if (allocated(request%impact_cases)) deallocate(request%impact_cases)
        request%n_impact_cases = 0
        request%impact_all = .false.
        if (.not. baseline%input_inventory_ready) then
            call generation_inventory_restore(baseline, ierr, message)
            if (ierr /= 0) then
                request%impact_all = .true.
                return
            end if
        end if
        if (.not. candidate%input_inventory_ready) then
            call generation_inventory_restore(candidate, ierr, message)
            if (ierr /= 0) then
                request%impact_all = .true.
                return
            end if
        end if
        call fo_changed_modules(candidate%project_root, dag, changed_ids, &
            n_changed, affected_ids, n_affected, n_cached, ierr, &
            filenames=filenames, is_test_arr=is_test_arr)
        if (ierr /= 0) then
            request%impact_all = .true.
            return
        end if
        do i = 1, dag%n_nodes
            candidate_ids(i) = i
        end do
        call gfortran_selected_test_names(candidate%project_root, filenames, &
            candidate_ids, dag%n_nodes, .true., all_names, n_all, &
            selected_node_ids)
        if (n_all == 0) then
            request%impact_all = .true.
            return
        end if
        allocate(cases(n_all))
        do i = 1, n_all
            cases(i)%public_name = all_names(i)
            cases(i)%identity = 'gfortran-test:'//trim(all_names(i))
            cases(i)%node_id = selected_node_ids(i)
            cases(i)%eligible = .not. is_slow_test(all_names(i))
            cases(i)%slow = is_slow_test(all_names(i))
            cases(i)%dependency_complete = .true.
        end do
        dependency_model_complete = &
            baseline%driver_digest == candidate%driver_digest .and. &
            baseline%driver_size == candidate%driver_size
        call test_impact_select(baseline%input_inventory, &
            candidate%input_inventory, dag, filenames, cases, &
            dependency_model_complete, impact, ierr, message)
        if (ierr /= 0) then
            request%impact_all = .true.
            return
        end if
        request%n_impact_cases = min(impact%required_count, MAX_NODES)
        allocate(request%impact_cases(request%n_impact_cases))
        do i = 1, request%n_impact_cases
            request%impact_cases(i) = impact%required(i)%public_name
        end do
    end subroutine compute_generation_impact

    subroutine discover_campaign(project_dir, generation_id, session, request, &
            selected, n_selected, &
            n_mandatory_selected, seed, ierr, message, bypass_coverage)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: generation_id
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(inout) :: request
        character(len=*), intent(out) :: selected(:)
        integer, intent(out) :: n_selected, n_mandatory_selected, seed, ierr
        character(len=*), intent(out) :: message
        logical, intent(in), optional :: bypass_coverage

        type(backend_t) :: backend
        type(dag_t) :: dag
        integer, allocatable :: changed_ids(:), affected_ids(:)
        integer :: n_changed, n_affected, n_cached, i, n_all, n_impacted
        integer :: n_history, n_debt, cursor_seed, n_priorities, limit
        integer :: n_selected_priorities
        integer, allocatable :: candidate_ids(:)
        integer :: shuffle_status, coverage_status
        character(len=MAX_PATH), allocatable :: filenames(:)
        character(len=NAME_LEN), allocatable :: all_names(:), impacted(:)
        character(len=NAME_LEN), allocatable :: history(:), debt(:), priorities(:)
        type(coverage_epoch_t) :: coverage
        character(len=PATH_LEN) :: coverage_path
        logical, allocatable :: is_test_arr(:)
        logical :: reproduce_only

        allocate (changed_ids(MAX_NODES), affected_ids(MAX_NODES), &
            candidate_ids(MAX_NODES), filenames(MAX_NODES), &
            all_names(MAX_NODES), impacted(MAX_NODES), history(MAX_NODES), &
            debt(MAX_NODES), priorities(MAX_NODES), is_test_arr(MAX_NODES))
        selected = ''
        impacted = ''
        n_selected = 0
        n_mandatory_selected = 0
        seed = request%seed
        if (seed == 0) then
            call system_clock(seed)
            if (seed == 0) seed = 1
        end if
        backend = detect_backend(project_dir)
        ierr = 0
        message = ''
        if (backend%kind /= BACKEND_NATIVE) then
            ierr = 1
            message = 'Gremlin currently requires the native fpm test backend'
            return
        end if
        call fo_changed_modules(project_dir, dag, changed_ids, n_changed, &
            affected_ids, n_affected, n_cached, ierr, filenames=filenames, &
            is_test_arr=is_test_arr)
        if (ierr /= 0) then
            message = 'cannot scan frozen project test inventory'
            return
        end if
        do i = 1, dag%n_nodes
            candidate_ids(i) = i
        end do
        call gfortran_selected_test_names(project_dir, filenames, candidate_ids, &
            dag%n_nodes, .true., all_names, n_all)
        call read_campaign_history(session, all_names, n_all, history, n_history, &
            debt, n_debt, cursor_seed, ierr, message)
        if (ierr /= 0) return
        n_impacted = 0
        if (request%has_previous_generation) then
            if (request%impact_all) then
                do i = 1, n_all
                    if (is_slow_test(all_names(i))) cycle
                    call append_priority_names(all_names(i:i), 1, impacted, &
                        n_impacted)
                end do
            else
                if (request%n_impact_cases > 0) &
                    call append_priority_names(request%impact_cases, &
                        request%n_impact_cases, impacted, n_impacted)
            end if
        else if (request%only_changed) then
            call gfortran_selected_test_names(project_dir, filenames, affected_ids, &
                n_affected, .false., impacted, n_impacted)
        end if
        n_priorities = 0
        if (request%only_changed .or. request%has_previous_generation .or. &
            request%n_targets > 0 .or. n_history > 0) then
            call append_priority_names(request%gate_cases, request%gate_required_count, &
                priorities, n_priorities)
        end if
        call append_priority_names(request%targets, request%n_targets, priorities, &
            n_priorities)
        call append_priority_names(impacted, n_impacted, priorities, n_priorities)
        call append_priority_names(history, n_history, priorities, n_priorities)
        do i = 1, n_priorities
            if (.not. any(all_names(:n_all) == priorities(i))) then
                ierr = 2
                message = 'requested or prioritized case is outside the eligible inventory'
                return
            end if
        end do
        reproduce_only = .false.
        if (present(bypass_coverage)) reproduce_only = bypass_coverage
        if (reproduce_only) then
            n_selected = 1
            n_mandatory_selected = 1
            selected(1) = request%targets(1)
            ierr = GREMLIN_POLICY_OK
            message = ''
            return
        end if
        n_mandatory_selected = 0
        limit = n_priorities + min(GREMLIN_NONMANDATORY_LIMIT, request%random_count)
        if (limit == 0) then
            ierr = GREMLIN_POLICY_OK
            message = ''
            return
        end if
        if (len_trim(generation_id) /= HASH_LEN) then
            ierr = 1
            message = 'coverage requires an exact generation identity'
            return
        end if
        coverage_path = trim(session%state_dir)//'/coverage-'// &
            generation_id//'.state'
        call coverage_open(trim(coverage_path), generation_id, all_names, n_all, &
            seed, coverage, coverage_status, message)
        if (coverage_status /= COVERAGE_OK) then
            ierr = coverage_status
            message = 'cannot recover current-generation coverage: '//trim(message)
            return
        end if
        ! Discovery runs after the prior owned child has completed/cancelled.
        ! Reset launch intents from a stopped or crashed owner before selection.
        call coverage_recover_running(coverage, coverage_status, message)
        if (coverage_status /= COVERAGE_OK) then
            ierr = coverage_status
            return
        end if
        seed = coverage%seed
        call reconcile_coverage(session, coverage, message, ierr)
        if (ierr /= 0) return
        call coverage_next_chunk(coverage, priorities, n_priorities, min(limit, &
            size(selected)), selected, n_selected, coverage_status, &
            n_selected_priorities)
        if (coverage_status /= COVERAGE_OK) then
            ierr = coverage_status
            message = 'coverage scheduler rejected eligible inventory or priorities'
            return
        end if
        if (n_priorities == 0) then
            if (n_selected > 0) then
                priorities(1) = selected(1)
            else
                priorities(1) = all_names(1)
            end if
            n_priorities = 1
        end if
        if (len_trim(request%requirement_digest) /= HASH_LEN) then
            request%gate_cases = ''
            request%gate_cases(:n_priorities) = priorities(:n_priorities)
            request%gate_required_count = n_priorities
            request%requirement_digest = cache_digest(priorities, n_priorities)
        end if
        n_mandatory_selected = n_selected_priorities
        call select_missing_gate_cases(session, generation_id, request, &
            min(limit, size(selected)), selected, n_selected, &
            n_mandatory_selected, ierr, message)
        if (ierr /= 0) return
        if (request%shuffle) then
            call shuffle_gremlin_tests(selected, n_mandatory_selected, n_selected, &
                seed, shuffle_status)
            if (shuffle_status /= GREMLIN_POLICY_OK) then
                ierr = shuffle_status
                message = 'Gremlin could not shuffle selected tests'
            end if
        end if
    end subroutine discover_campaign

    subroutine select_missing_gate_cases(session, generation, request, limit, &
            selected, n_selected, n_mandatory, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: generation
        type(gremlin_request_t), intent(in) :: request
        integer, intent(in) :: limit
        character(len=*), intent(inout) :: selected(:)
        integer, intent(inout) :: n_selected, n_mandatory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        character(len=NAME_LEN) :: pending(MAX_NODES)
        character(len=HASH_LEN) :: receipt_generation, requirement
        character(len=NAME_LEN) :: case_name
        character(len=PATH_LEN) :: journal_path
        character(len=16) :: outcome, gate_required
        logical :: attempted(MAX_NODES)
        integer(int64) :: cursor, next_cursor
        integer :: i, j, n_pending

        ! Coverage may already be complete when a frozen generation is reused
        ! with a different gate. Only actual receipts can discharge that gate.
        attempted = .false.
        cursor = 0_int64
        ierr = 0
        message = ''
        call gremlin_get_session_journal_path(session%project_key, request%lane_id, &
            session%session_id, journal_path, ierr, message)
        if (ierr /= 0) return
        do
            call journal_read_page(trim(journal_path), cursor, 64, &
                int(JOURNAL_MAX_RECORD_BYTES, int64)*64_int64, records, next_cursor, &
                ierr, message)
            if (ierr /= JOURNAL_OK) return
            if (size(records) == 0) exit
            do i = 1, size(records)
                call gremlin_json_field(records(i)%json, 'generation', &
                    receipt_generation)
                if (trim(receipt_generation) /= trim(generation)) cycle
                call gremlin_json_field(records(i)%json, 'requirement_digest', &
                    requirement)
                if (requirement /= request%requirement_digest) cycle
                call gremlin_json_field(records(i)%json, 'gate_required', gate_required)
                if (trim(gate_required) /= 'true') cycle
                call gremlin_json_field(records(i)%json, 'status', outcome)
                if (outcome /= 'PASS' .and. .not. status_is_failure(outcome)) cycle
                call gremlin_json_field(records(i)%json, 'case_id', case_name)
                do j = 1, request%gate_required_count
                    if (case_name == request%gate_cases(j)) attempted(j) = .true.
                end do
            end do
            if (next_cursor <= cursor) then
                ierr = JOURNAL_INVALID
                message = 'Gremlin receipt cursor did not advance during gate selection'
                return
            end if
            cursor = next_cursor
        end do
        pending = ''
        n_pending = 0
        do i = 1, request%gate_required_count
            if (attempted(i)) cycle
            n_pending = n_pending + 1
            pending(n_pending) = request%gate_cases(i)
        end do
        call append_priority_names(selected, n_mandatory, pending, n_pending)
        n_mandatory = min(n_pending, limit)
        call append_priority_names(selected, n_selected, pending, n_pending)
        n_selected = min(n_pending, limit)
        selected = ''
        selected(:n_selected) = pending(:n_selected)
    end subroutine select_missing_gate_cases

    subroutine append_priority_names(source, n_source, destination, n_destination)
        character(len=*), intent(in) :: source(:)
        integer, intent(in) :: n_source
        character(len=*), intent(inout) :: destination(:)
        integer, intent(inout) :: n_destination
        integer :: i

        do i = 1, n_source
            if (n_destination >= size(destination)) exit
            if (any(destination(:n_destination) == source(i))) cycle
            n_destination = n_destination + 1
            destination(n_destination) = source(i)
        end do
    end subroutine append_priority_names

    subroutine reconcile_coverage(session, coverage, message, ierr)
        type(gremlin_session_t), intent(in) :: session
        type(coverage_epoch_t), intent(inout) :: coverage
        character(len=*), intent(out) :: message
        integer, intent(out) :: ierr

        type(journal_record_t), allocatable :: records(:)
        character(len=NAME_LEN) :: case_name
        character(len=HASH_LEN) :: generation
        character(len=16) :: outcome, seed_text
        character(len=HASH_LEN) :: inventory_digest
        character(len=16) :: epoch_text
        character(len=PATH_LEN) :: path
        integer(int64) :: cursor, next_cursor
        integer :: journal_status, i, status, receipt_seed, receipt_epoch, ios
        logical :: exists

        ierr = 0
        message = ''
        path = trim(session%state_dir)//'/campaign-journal.jsonl'
        inquire(file=trim(path), exist=exists)
        if (.not. exists) return
        cursor = 0_int64
        do
            call journal_read_page(trim(path), cursor, 64, &
                int(JOURNAL_MAX_RECORD_BYTES, int64)*64_int64, records, &
                next_cursor, journal_status, message)
            if (journal_status /= JOURNAL_OK) then
                ierr = journal_status
                return
            end if
            if (size(records) == 0) exit
            do i = 1, size(records)
                case_name = ''
                generation = ''
                outcome = ''
                seed_text = ''
                inventory_digest = ''
                epoch_text = ''
                call gremlin_json_field(records(i)%json, 'case_id', case_name)
                call gremlin_json_field(records(i)%json, 'generation', generation)
                call gremlin_json_field(records(i)%json, 'status', outcome)
                call gremlin_json_field(records(i)%json, 'seed', seed_text)
                call gremlin_json_field(records(i)%json, 'inventory_digest', &
                    inventory_digest)
                call gremlin_json_field(records(i)%json, 'coverage_epoch', epoch_text)
                if (trim(generation) /= trim(coverage%generation)) cycle
                read(seed_text, *, iostat=ios) receipt_seed
                if (ios /= 0) cycle
                if (receipt_seed /= coverage%seed) cycle
                read(epoch_text, *, iostat=ios) receipt_epoch
                if (ios /= 0) cycle
                if (receipt_epoch /= coverage%epoch .or. &
                    trim(inventory_digest) /= trim(coverage%inventory_digest)) cycle
                call coverage_record(coverage, case_name, outcome, status, message)
                if (status /= COVERAGE_OK) then
                    ierr = status
                    return
                end if
            end do
            if (next_cursor <= cursor) then
                ierr = JOURNAL_INVALID
                message = 'campaign journal cursor did not advance during coverage recovery'
                return
            end if
            cursor = next_cursor
        end do
    end subroutine reconcile_coverage

    subroutine read_campaign_history(session, inventory, n_inventory, history, &
            n_history, debt, n_debt, cursor_seed, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: inventory(:)
        integer, intent(in) :: n_inventory
        character(len=*), intent(out) :: history(:), debt(:)
        integer, intent(out) :: n_history, n_debt, cursor_seed, ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        character(len=NAME_LEN) :: case_name
        character(len=16) :: status_name, seed_text
        character(len=PATH_LEN) :: path
        integer(int64) :: cursor, next_cursor
        integer :: journal_status, i, seed_value, ios
        logical :: exists

        history = ''
        debt = ''
        n_history = 0
        n_debt = 0
        cursor_seed = 0
        ierr = 0
        message = ''
        path = trim(session%state_dir)//'/campaign-journal.jsonl'
        inquire(file=trim(path), exist=exists)
        if (.not. exists) return
        cursor = 0_int64
        do
            call journal_read_page(trim(path), cursor, 64, &
                int(JOURNAL_MAX_RECORD_BYTES, int64)*64_int64, records, &
                next_cursor, journal_status, message)
            if (journal_status /= JOURNAL_OK) then
                ierr = journal_status
                return
            end if
            if (size(records) == 0) exit
            do i = 1, size(records)
                case_name = ''
                status_name = ''
                seed_text = ''
                call gremlin_json_field(records(i)%json, 'case_id', case_name)
                call gremlin_json_field(records(i)%json, 'status', status_name)
                call gremlin_json_field(records(i)%json, 'seed', seed_text)
                if (.not. any(inventory(:n_inventory) == case_name)) cycle
                if (status_name /= 'PASS' .and. status_name /= 'FAIL' .and. &
                    status_name /= 'TIMEOUT') cycle
                read(seed_text, *, iostat=ios) seed_value
                if (ios == 0) cursor_seed = seed_value
                call move_to_recent(case_name, debt, n_debt)
                if (status_name == 'FAIL' .or. status_name == 'TIMEOUT') then
                    call move_to_priority(case_name, history, n_history)
                end if
            end do
            if (next_cursor <= cursor) then
                ierr = JOURNAL_INVALID
                message = 'campaign journal cursor did not advance'
                return
            end if
            cursor = next_cursor
        end do
    end subroutine read_campaign_history

    subroutine move_to_recent(name, values, count_values)
        character(len=*), intent(in) :: name
        character(len=*), intent(inout) :: values(:)
        integer, intent(inout) :: count_values
        integer :: i, found

        found = 0
        do i = 1, count_values
            if (values(i) == name) then
                found = i
                exit
            end if
        end do
        if (found > 0) then
            do i = found, count_values - 1
                values(i) = values(i + 1)
            end do
            count_values = count_values - 1
        else if (count_values == size(values)) then
            values(:count_values - 1) = values(2:count_values)
            count_values = count_values - 1
        end if
        if (count_values < size(values)) then
            count_values = count_values + 1
            values(count_values) = name
        end if
    end subroutine move_to_recent

    subroutine move_to_priority(name, values, count_values)
        character(len=*), intent(in) :: name
        character(len=*), intent(inout) :: values(:)
        integer, intent(inout) :: count_values
        integer :: i

        do i = 1, count_values
            if (values(i) == name) then
                if (i > 1) values(2:i) = values(1:i - 1)
                values(1) = name
                return
            end if
        end do
        if (count_values == min(size(values), GREMLIN_HISTORY_LIMIT)) then
            values(count_values) = ''
            count_values = count_values - 1
        end if
        count_values = count_values + 1
        if (count_values > 1) values(2:count_values) = values(1:count_values - 1)
        values(1) = name
    end subroutine move_to_priority

    subroutine cancel_test_case(session, generation, child, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(generation_t), intent(in) :: generation
        type(child_t), intent(inout) :: child
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: coverage_path

        call cancel_owned_process(child%pid, ierr)
        message = 'cannot cancel owned test process'
        if (ierr /= 0) return
        child%pid = 0
        call append_execution_provenance(child%log_file, child%execution_view)
        call execution_view_release(child%execution_view, .false., ierr, message)
        if (ierr /= 0) return
        coverage_path = trim(session%state_dir)//'/coverage-'// &
            generation%identity//'.state'
        call coverage_record_path(trim(coverage_path), generation%identity, &
            child%case_name, 'CANCELLED', ierr, message)
    end subroutine cancel_test_case

    subroutine append_execution_provenance(log_file, view)
        character(len=*), intent(in) :: log_file
        type(execution_view_t), intent(in) :: view

        integer :: unit, ios
        character(len=8) :: completeness

        completeness = 'complete'
        if (.not. view%complete) completeness = 'incomplete'
        open (newunit=unit, file=trim(log_file), status='unknown', &
            position='append', action='write', iostat=ios)
        if (ios /= 0) return
        write (unit, '(a)', iostat=ios) 'fo: execution cwd: '//trim(view%cwd)
        if (ios == 0) write (unit, '(a)', iostat=ios) &
            'fo: execution generation: '//trim(view%generation_id)
        if (ios == 0) write (unit, '(a)', iostat=ios) &
            'fo: execution input inventory: '//trim(view%inventory_digest)// &
            ' ('//trim(completeness)//')'
        close (unit)
    end subroutine append_execution_provenance

    subroutine launch_selected_case(session, request, generation, selected, n_selected, &
            index_case, seed, campaign, child, ierr, message, completed, sequence)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: selected(:)
        integer, intent(in) :: n_selected, index_case, seed, campaign, completed
        type(child_t), intent(out) :: child
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer, intent(inout) :: sequence

        character(len=PATH_LEN) :: executable, log_name, test_project
        character(len=PATH_LEN) :: journal_message
        character(len=PATH_LEN) :: view_owner, view_message
        character(len=:), allocatable :: packed, execution_env
        integer :: n_args, spawn_exit, journal_status, coverage_status
        integer :: view_status
        logical :: exists
        character(len=PATH_LEN) :: coverage_path

        child = child_t()
        ierr = 0
        message = ''
        if (index_case < 1 .or. index_case > n_selected) return
        if (len_trim(generation%build_project_root) == 0) then
            ierr = 1
            message = 'active generation has no candidate build view'
            return
        end if
        test_project = generation%build_project_root
        child%gate_required = any(request%gate_cases(:request%gate_required_count) == &
            selected(index_case))
        if (generation%driver_digest /= session%driver_digest .or. &
            generation%driver_size /= session%driver_size .or. &
            len_trim(session%driver_path) == 0) then
            ierr = 1
            message = 'test generation does not match the pinned fo driver'
            return
        end if
        inquire (file=trim(session%driver_path), exist=exists)
        if (.not. exists) then
            ierr = 1
            message = 'pinned fo driver is missing; refusing to launch test case'
            return
        end if
        executable = session%driver_path
        write (log_name, '(a,i0,a,i0,a)') 'case-', campaign, '-', index_case, '.log'
        call log_path(session, trim(log_name), child%log_file)
        write(view_owner, '(a,"-case-",i0,"-",i0)') trim(session%session_id), &
            campaign, index_case
        call execution_view_create(trim(session%state_dir)//'/views', &
            generation%identity, trim(view_owner), selected(index_case), &
            generation%input_inventory, generation%input_inventory_ready, &
            generation%input_inventory_complete, child%execution_view, ierr, message)
        if (ierr /= 0) return
        execution_env = 'FO_BIN='//trim(executable)//';FO='//trim(executable)//';'// &
            'FO_DISABLE_SELF_REFRESH=1;FO_SELF_REFRESH=0;'// &
            'FO_GREMLIN_EXECUTION_CWD='//trim(child%execution_view%cwd)//';'// &
            'TMPDIR='//trim(child%execution_view%tmpdir)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'test')
        call argv_push(packed, n_args, '--json')
        if (is_slow_test(trim(selected(index_case)))) then
            call argv_push(packed, n_args, '--all')
        end if
        call argv_push(packed, n_args, trim(selected(index_case)))
        call process_start_argv_logged(trim(test_project), packed, &
            n_args, trim(child%log_file), child%pid, spawn_exit, &
            trim(execution_env))
        ierr = spawn_exit
        if (spawn_exit /= 0) then
            child%pid = 0
            call execution_view_release(child%execution_view, .false., view_status, &
                view_message)
            journal_message = 'cannot start selected test '//trim(selected(index_case))
            sequence = sequence + 1
            call record_immediate_case(session, request, generation, &
                selected(index_case), spawn_exit, 'INFRA_ERROR', sequence, &
                index_case, seed, child%log_file, journal_status, message, &
                gate_required=child%gate_required)
            if (journal_status /= JOURNAL_OK) then
                ierr = journal_status
            else
                ierr = spawn_exit
                message = trim(journal_message)
            end if
            return
        end if
        child%case_name = selected(index_case)
        child%campaign = campaign
        child%case_index = index_case
        child%seed = seed
        coverage_path = trim(session%state_dir)//'/coverage-'// &
            generation%identity//'.state'
        call coverage_record_path(trim(coverage_path), generation%identity, &
            trim(selected(index_case)), 'RUNNING', coverage_status, message)
        if (coverage_status /= COVERAGE_OK) then
            ierr = coverage_status
            return
        end if
        call clock_seconds(child%started_at)
        call publish_state(session, request, 'testing', generation, generation, &
            child%case_name, completed, n_selected, seed, 'RUNNING', 0, ierr, message)
    end subroutine launch_selected_case

    subroutine advance_campaign(project_dir, session, request, generation, selected, &
            n_selected, index_case, seed, campaign, campaign_started_ms, child, ierr, message, &
            completed, state_name, sequence)
        character(len=*), intent(in) :: project_dir
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(inout) :: request
        type(generation_t), intent(in) :: generation
        character(len=*), intent(inout) :: selected(:)
        integer, intent(inout) :: n_selected, index_case, seed, campaign
        integer(int64), intent(inout) :: campaign_started_ms
        type(child_t), intent(inout) :: child
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message, state_name
        integer, intent(in) :: completed
        integer, intent(inout) :: sequence

        integer(int64) :: now_ms
        integer :: status, mandatory_count, launch_error
        integer :: owner_pid
        type(gremlin_request_t) :: next_request
        type(gremlin_coverage_view_t) :: coverage_view
        type(gremlin_readiness_t) :: readiness
        character(len=PATH_LEN) :: launch_message, state_message
        character(len=PATH_LEN) :: coverage_path
        character(len=128) :: active_session, owner_start
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text

        ierr = 0
        message = ''
        call clock_milliseconds(now_ms)
        if (request%random_count == 0) then
            call gremlin_session_read(project_dir, request%lane_id, &
                active_session, owner_pid, owner_start, status_text, status, message)
            if (status /= 0) then
                ierr = status
                return
            end if
            if (trim(active_session) /= trim(session%session_id)) then
                ierr = 1
                message = 'Gremlin owner identity changed before gate completion'
                return
            end if
            coverage_path = trim(session%state_dir)//'/coverage-'// &
                generation%identity//'.state'
            call coverage_read_view_path(trim(coverage_path), generation%identity, &
                coverage_view, status, message)
            if (status == COVERAGE_OK) then
                call compute_readiness(session, trim(status_text), coverage_view, &
                    .true., readiness, ierr, message)
                if (ierr /= 0) return
                if (readiness%local_gate_green) then
                    selected = ''
                    n_selected = 0
                    state_name = 'quiescent'
                    call publish_state(session, request, state_name, generation, &
                        generation, '', completed, n_selected, seed, 'NONE', 0, &
                        ierr, message)
                    return
                end if
            else if (status /= COVERAGE_NOT_FOUND) then
                ierr = status
                return
            end if
        end if
        if (index_case < n_selected .and. &
            now_ms - campaign_started_ms < &
            int(request%campaign_seconds, int64)*1000_int64) then
            index_case = index_case + 1
        else
            seed = next_campaign_seed(seed)
            next_request = request
            next_request%seed = seed
            call discover_campaign(generation%project_root, generation%identity, &
                session, next_request, selected, n_selected, mandatory_count, seed, &
                ierr, message)
            request%requirement_digest = next_request%requirement_digest
            request%gate_cases = next_request%gate_cases
            request%gate_required_count = next_request%gate_required_count
            if (ierr /= 0) then
                state_name = 'inventory_failed'
                call publish_state(session, request, state_name, generation, generation, '', &
                    completed, n_selected, seed, 'NONE', ierr)
                return
            end if
            campaign = campaign + 1
            index_case = 1
            call clock_milliseconds(campaign_started_ms)
        end if
        if (n_selected == 0) then
            state_name = 'idle'
            coverage_path = trim(session%state_dir)//'/coverage-'// &
                trim(generation%identity)//'.state'
            call coverage_read_view_path(trim(coverage_path), generation%identity, &
                coverage_view, status, message)
            if (status == COVERAGE_OK) then
                if (coverage_view%green .and. request%gate_required_count > 0) &
                    state_name = 'quiescent'
            else if (status /= COVERAGE_NOT_FOUND) then
                ierr = status
                return
            end if
            call publish_state(session, request, state_name, generation, generation, '', &
                completed, n_selected, seed, 'NONE', 0, ierr, message)
            return
        end if
        state_name = 'testing'
        call launch_selected_case(session, request, generation, selected, n_selected, &
            index_case, seed, campaign, child, ierr, message, completed, sequence)
        if (ierr /= 0) then
            launch_error = ierr
            launch_message = message
            call publish_state(session, request, 'test_launch_failed', generation, &
                generation, current_test_name(selected, n_selected), completed, &
                n_selected, seed, 'INFRA_ERROR', launch_error, status, state_message)
            if (status == 0) then
                ierr = launch_error
                message = trim(launch_message)
            else
                ierr = status
                message = trim(state_message)
            end if
        end if
    end subroutine advance_campaign

    integer function next_campaign_seed(seed) result(next_seed)
        integer, intent(in) :: seed
        integer(int64) :: state

        ! Park-Miller progression gives a reproducible positive 31-bit seed.
        state = modulo(int(seed, int64), 2147483646_int64) + 1_int64
        state = modulo(state*48271_int64, 2147483647_int64)
        next_seed = int(state)
    end function next_campaign_seed

    subroutine record_case(session, request, generation, child, exitcode, outcome, &
            sequence, seed, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        type(child_t), intent(in) :: child
        integer, intent(in) :: exitcode, seed
        character(len=*), intent(in) :: outcome
        integer, intent(inout) :: sequence
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        sequence = sequence + 1
        call record_immediate_case(session, request, generation, child%case_name, &
            exitcode, outcome, sequence, child%case_index, seed, child%log_file, ierr, &
            message, gate_required=child%gate_required)
    end subroutine record_case

    subroutine record_immediate_case(session, request, generation, case_name, exitcode, &
            outcome, sequence, case_index, seed, log_file, ierr, message, &
            credit_coverage, gate_required)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: case_name, outcome, log_file
        integer, intent(in) :: exitcode, sequence, case_index, seed
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        logical, intent(in), optional :: credit_coverage
        logical, intent(in), optional :: gate_required
        logical :: credit, is_gate
        character(len=160) :: gate_identity
        character(len=256) :: completion_id
        character(len=32768) :: record
        character(len=16) :: journal_outcome
        character(len=PATH_LEN) :: journal_path
        character(len=PATH_LEN) :: coverage_path
        character(len=HASH_LEN) :: inventory_digest
        character(len=96) :: receipt_epoch
        character(len=512) :: receipt_identity
        type(gremlin_coverage_view_t) :: coverage_view
        integer :: status, coverage_status

        write (completion_id, '(a,a,a,a,i0,a,i0)') trim(session%session_id), '-', &
            generation%identity(1:12), '-', sequence, '-', case_index
        journal_outcome = 'fail'
        if (outcome == 'PASS' .or. outcome == 'BUILD_PASS') journal_outcome = 'pass'
        credit = .true.
        if (present(credit_coverage)) credit = credit_coverage
        is_gate = .false.
        if (present(gate_required)) is_gate = gate_required
        gate_identity = ''
        if (is_gate) gate_identity = ',"gate_required":true,"requirement_digest":"'// &
            request%requirement_digest//'"'
        receipt_identity = ',"evidence_kind":"reproduction"'
        if (credit) receipt_identity = ',"evidence_kind":"campaign"'
        if (credit .and. coverage_outcome(outcome)) then
            coverage_path = trim(session%state_dir)//'/coverage-'// &
                generation%identity//'.state'
            call coverage_read_view_path(trim(coverage_path), generation%identity, &
                coverage_view, coverage_status, message)
            if (coverage_status /= COVERAGE_OK) then
                ierr = coverage_status
                return
            end if
            inventory_digest = coverage_view%inventory_digest
            write(receipt_epoch, '(i0)') coverage_view%epoch
            receipt_identity = ',"evidence_kind":"campaign","coverage_epoch":'// &
                trim(receipt_epoch)// &
                ',"inventory_digest":"'//trim(inventory_digest)//'"'
        end if
        write (record, '(a)') '{"completion_id":"'//trim(completion_id)// &
            '","session_id":"'//trim(json_escape_string(session%session_id))// &
            '","lane_id":"'//trim(json_escape_string(request%lane_id))// &
            '","generation":"'//generation%identity// &
            '","driver_path":"'// &
            trim(json_escape_string(trim(generation%driver_path)))// &
            '","driver_digest":"'//generation%driver_digest// &
            '","driver_size":'//trim(int64_text(generation%driver_size))// &
            ',"case_id":"'//trim(json_escape_string(case_name))// &
            '","outcome":"'//trim(journal_outcome)//'","status":"'// &
            trim(outcome)//'","exitcode":'// &
            trim(json_int(exitcode))//',"seed":'//trim(json_int(seed))// &
            trim(receipt_identity)//trim(gate_identity)// &
            ',"order":'//trim(json_int(case_index))//',"log_path":"'// &
            trim(json_escape_string(log_file))//'"}'
        call gremlin_get_session_journal_path(session%project_key, request%lane_id, &
            session%session_id, journal_path, ierr, message)
        if (ierr /= 0) return
        ! The lane ledger is the selection source of truth and is written first.
        ! A restart can therefore never advance coverage from an unreceipted case.
        if (credit .and. coverage_outcome(outcome)) then
            call journal_append(trim(session%state_dir)//'/campaign-journal.jsonl', &
                trim(completion_id), trim(record), status, message)
            if (status /= JOURNAL_OK) then
                ierr = status
                return
            end if
            coverage_path = trim(session%state_dir)//'/coverage-'// &
                generation%identity//'.state'
            call coverage_record_path(trim(coverage_path), generation%identity, &
                case_name, outcome, coverage_status, message)
            if (coverage_status /= COVERAGE_OK) then
                ierr = coverage_status
                return
            end if
            call journal_compact_tail(trim(session%state_dir)// &
                '/campaign-journal.jsonl', GREMLIN_POLICY_RECEIPT_LIMIT, status, message)
            if (status /= JOURNAL_OK) then
                ierr = status
                return
            end if
        end if
        call journal_append(trim(journal_path), trim(completion_id), trim(record), &
            status, message)
        ierr = status
    end subroutine record_immediate_case

    subroutine record_build(session, request, generation, exitcode, log_file, sequence, &
            ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        integer, intent(in) :: exitcode
        character(len=*), intent(in) :: log_file
        integer, intent(inout) :: sequence
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=16) :: outcome

        if (exitcode == 0) then
            outcome = 'BUILD_PASS'
        else
            outcome = 'BUILD_FAIL'
        end if
        sequence = sequence + 1
        call record_immediate_case(session, request, generation, '<build>', exitcode, &
            trim(outcome), sequence, 0, request%seed, log_file, ierr, message)
    end subroutine record_build

    subroutine publish_state(session, request, state, active, candidate, current_case, &
            completed, selected_count, seed, last_outcome, last_exitcode, ierr, message, &
            status_text_out, diagnostic)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        character(len=*), intent(in) :: state, current_case, last_outcome
        type(generation_t), intent(in) :: active, candidate
        integer, intent(in) :: completed, selected_count, seed, last_exitcode
        integer, intent(out), optional :: ierr
        character(len=*), intent(out), optional :: message
        character(len=*), intent(out), optional :: status_text_out
        character(len=*), intent(in), optional :: diagnostic

        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        type(gremlin_session_t) :: read_session
        type(gremlin_coverage_view_t) :: coverage_view
        type(gremlin_readiness_t) :: readiness
        character(len=PATH_LEN) :: local_message, lifecycle_path, event_message
        character(len=PATH_LEN) :: diagnostic_text
        character(len=PATH_LEN) :: coverage_path
        integer :: status
        character(len=PATH_LEN) :: active_project, candidate_project
        logical :: have_coverage

        active_project = ''
        candidate_project = ''
        diagnostic_text = ''
        if (present(diagnostic)) diagnostic_text = diagnostic
        if (len_trim(active%identity) > 0) active_project = trim(active%project_root)
        if (len_trim(candidate%identity) > 0) candidate_project = trim(candidate%project_root)
        status_text = '{"protocol":1,"session_id":"'// &
            trim(json_escape_string(session%session_id))//'","lane_id":"'// &
            trim(json_escape_string(request%lane_id))//'","policy_key":"'// &
            request_policy_key(request)//'","state":"'//trim(state)// &
            '","driver_path":"'// &
            trim(json_escape_string(trim(session%driver_path)))// &
            '","driver_digest":"'//trim(session%driver_digest)// &
            '","driver_size":'//trim(int64_text(session%driver_size))// &
            ',"active_driver_digest":"'//trim(active%driver_digest)// &
            '","active_driver_size":'//trim(int64_text(active%driver_size))// &
            ',"candidate_driver_digest":"'//trim(candidate%driver_digest)// &
            '","candidate_driver_size":'// &
            trim(int64_text(candidate%driver_size))// &
            ',"active_generation":"'//trim(active%identity)// &
            '","candidate_generation":"'//trim(candidate%identity)// &
            '","active_project":"'//trim(json_escape_string(active_project))// &
            '","candidate_project":"'//trim(json_escape_string(candidate_project))// &
            '","current_test":"'//trim(json_escape_string(current_case))// &
            '","completed":'//trim(json_int(completed))// &
            ',"selected":'//trim(json_int(selected_count))// &
            ',"seed":'//trim(json_int(seed))//',"last_outcome":"'// &
            trim(last_outcome)//'","last_exitcode":'// &
            trim(json_int(last_exitcode))//',"gate_required":'// &
            trim(json_int(request%gate_required_count))// &
            ',"requirement_digest":"'//request%requirement_digest// &
            '","event_epoch":'//trim(json_int(request%event_epoch))// &
            ',"input_changed":'//trim(json_bool(request%input_changed))// &
            ',"diagnostic":"'//trim(json_escape_string(trim(diagnostic_text)))//'"}'
        call gremlin_session_publish(session, trim(status_text), status, local_message)
        if (status == 0) then
            call lifecycle_path_for_session(session, request, lifecycle_path, &
                status, event_message)
            if (status == 0) then
                read_session = session
                read_session%state_dir = lifecycle_path(: &
                    len_trim(lifecycle_path) - len('/lifecycle.jsonl'))
                coverage_view = gremlin_coverage_view_t()
                have_coverage = .false.
                if (len_trim(active%identity) == HASH_LEN) then
                    coverage_path = trim(session%state_dir)//'/coverage-'// &
                        trim(active%identity)//'.state'
                    call coverage_read_view_path(trim(coverage_path), &
                        trim(active%identity), coverage_view, status, event_message)
                    if (status == COVERAGE_OK) then
                        have_coverage = .true.
                    else if (status == COVERAGE_NOT_FOUND) then
                        status = 0
                        event_message = ''
                    end if
                end if
            end if
            if (status == 0) then
                call compute_readiness(read_session, trim(status_text), coverage_view, &
                    have_coverage, readiness, status, event_message)
            end if
            if (status == 0) then
                call publish_lifecycle_transition(read_session, request, &
                    trim(lifecycle_path), state, active, candidate, current_case, last_outcome, &
                    readiness, coverage_view, have_coverage, trim(diagnostic_text), &
                    status, event_message)
            end if
            if (status /= 0) then
                local_message = 'cannot publish Gremlin lifecycle transition: '// &
                    trim(event_message)
            end if
        end if
        if (present(ierr)) ierr = status
        if (present(message)) message = local_message
        if (present(status_text_out)) status_text_out = trim(status_text)
    end subroutine publish_state

    subroutine publish_lifecycle_transition(session, request, path, state, active, &
            candidate, current_case, last_outcome, readiness, coverage, have_coverage, &
            diagnostic, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        character(len=*), intent(in) :: path, state, current_case, last_outcome
        type(generation_t), intent(in) :: active, candidate
        type(gremlin_readiness_t), intent(in) :: readiness
        type(gremlin_coverage_view_t), intent(in) :: coverage
        logical, intent(in) :: have_coverage
        character(len=*), intent(in) :: diagnostic
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(gremlin_lifecycle_event_t) :: previous
        logical :: found, changed_generation, coverage_changed, previous_pass
        character(len=32) :: failure_event
        integer :: status
        character(len=HASH_LEN) :: previous_generation

        ierr = 0
        message = ''
        call read_last_lifecycle_event(path, previous, found, ierr, message)
        if (ierr /= 0) return
        previous_generation = ''
        if (found) previous_generation = previous%active_generation
        changed_generation = len_trim(active%identity) == HASH_LEN .and. &
            trim(active%identity) /= trim(previous_generation)
        if (trim(state) == 'starting') then
            call append_lifecycle_event(session, request, path, 'session_started', &
                active, candidate, active%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message)
        else if (trim(state) == 'building') then
            call append_lifecycle_event(session, request, path, &
                'generation_started', active, candidate, candidate%identity, &
                previous_generation, readiness, coverage, have_coverage, ierr, message)
        else if (trim(state) == 'build_failed' .or. trim(last_outcome) == 'BUILD_FAIL') then
            call append_lifecycle_event(session, request, path, &
                'build_failed', active, candidate, candidate%identity, &
                previous_generation, readiness, coverage, have_coverage, ierr, message)
        else if (trim(state) == 'capture_failed') then
            call append_lifecycle_event(session, request, path, 'capture_failed', &
                active, candidate, candidate%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message, diagnostic)
        else if (trim(state) == 'error') then
            call append_lifecycle_event(session, request, path, 'session_failed', &
                active, candidate, active%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message)
        else if (trim(state) == 'stopped') then
            call append_lifecycle_event(session, request, path, 'session_stopped', &
                active, candidate, active%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message)
        end if
        if (ierr /= 0) return

        if (changed_generation) then
            call append_lifecycle_event(session, request, path, &
                'build_passed', active, candidate, candidate%identity, &
                previous_generation, readiness, coverage, have_coverage, ierr, message)
            if (ierr /= 0) return
            if (len_trim(previous_generation) > 0) then
                call append_lifecycle_event(session, request, path, &
                    'generation_superseded', active, candidate, candidate%identity, &
                    previous_generation, readiness, coverage, have_coverage, ierr, message)
            else
                call append_lifecycle_event(session, request, path, 'generation_activated', &
                    active, candidate, active%identity, previous_generation, readiness, &
                    coverage, have_coverage, ierr, message)
            end if
            if (ierr /= 0) return
        end if

        if (found .and. (readiness%local_gate_green .neqv. &
            previous%local_gate_green)) then
            if (readiness%local_gate_green) then
                call append_lifecycle_event(session, request, path, 'local_gate_green', &
                    active, candidate, active%identity, previous_generation, readiness, &
                    coverage, have_coverage, ierr, message)
            else
                call append_lifecycle_event(session, request, path, 'local_gate_changed', &
                    active, candidate, active%identity, previous_generation, readiness, &
                    coverage, have_coverage, ierr, message)
            end if
            if (ierr /= 0) return
        else if (.not. found .and. readiness%local_gate_green) then
            call append_lifecycle_event(session, request, path, 'local_gate_green', &
                active, candidate, active%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message)
            if (ierr /= 0) return
        end if

        ! Normal FAIL is retained only after the normal runner's repeat attempt.
        ! A previous-generation PASS distinguishes a confirmed regression from
        ! a newly observed failure. Flaky, timeout and infra are factual outcomes.
        failure_event = ''
        select case (trim(last_outcome))
        case ('FAIL')
            call prior_generation_case_passed(session, active%identity, current_case, &
                previous_pass, ierr, message)
            if (ierr /= 0) return
            failure_event = 'test_failed'
            if (previous_pass) failure_event = 'regression_confirmed'
        case ('FLAKY')
            failure_event = 'test_flaky'
        case ('TIMEOUT')
            failure_event = 'test_timeout'
        case ('INFRA_ERROR', 'INFRA')
            if (len_trim(current_case) > 0 .and. trim(current_case) /= '<build>') &
                failure_event = 'test_infra_error'
        end select
        if (len_trim(failure_event) > 0 .and. len_trim(active%identity) == HASH_LEN) then
            call append_lifecycle_event(session, request, path, failure_event, &
                active, candidate, active%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message)
            if (ierr /= 0) return
        end if
        if (readiness%verification_level >= GREMLIN_VERIFY_ORDINARY .and. &
            (.not. found .or. (previous%verification_level /= 'ordinary' .and. &
            previous%verification_level /= 'full'))) then
            call append_lifecycle_event(session, request, path, 'ordinary_coverage_complete', &
                active, candidate, active%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message)
            if (ierr /= 0) return
        end if
        if (readiness%fully_verified .and. &
            (.not. found .or. previous%verification_level /= 'full')) then
            call append_lifecycle_event(session, request, path, 'full_verification_complete', &
                active, candidate, active%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message)
            if (ierr /= 0) return
        end if
        if (readiness%phase == GREMLIN_PHASE_QUIESCENT .and. &
            (.not. found .or. previous%phase /= 'quiescent')) then
            call append_lifecycle_event(session, request, path, 'session_quiescent', &
                active, candidate, active%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message)
            if (ierr /= 0) return
        else if (found .and. previous%phase == 'quiescent' .and. &
                readiness%phase /= GREMLIN_PHASE_QUIESCENT) then
            call append_lifecycle_event(session, request, path, 'session_wake', &
                active, candidate, active%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message)
            if (ierr /= 0) return
        end if

        coverage_changed = have_coverage
        if (found .and. have_coverage) then
            coverage_changed = coverage_changed .and. &
                (coverage%epoch /= previous%epoch .or. &
                coverage%cursor /= previous%epoch_cursor .or. &
                readiness%ordinary_passed /= previous%ordinary_passed .or. &
                readiness%ordinary_required /= previous%ordinary_required .or. &
                readiness%ordinary_failures /= previous%ordinary_failures)
        end if
        if (coverage_changed) then
            call append_lifecycle_event(session, request, path, 'coverage_updated', &
                active, candidate, active%identity, previous_generation, readiness, &
                coverage, have_coverage, ierr, message)
            if (ierr /= 0) return
        end if
    end subroutine publish_lifecycle_transition

    subroutine prior_generation_case_passed(session, generation, case_name, passed, &
            ierr, message)
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: generation, case_name
        logical, intent(out) :: passed
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(journal_record_t), allocatable :: records(:)
        character(len=HASH_LEN) :: receipt_generation
        character(len=NAME_LEN) :: receipt_case
        character(len=32) :: verdict
        character(len=PATH_LEN) :: journal_path
        integer(int64) :: cursor, next_cursor
        integer :: i

        passed = .false.
        cursor = 0_int64
        if (.not. allocated(session%project_key) .or. &
            .not. allocated(session%lane_id) .or. &
            .not. allocated(session%session_id)) then
            ierr = 1
            message = 'Gremlin session has no receipt journal identity'
            return
        end if
        call gremlin_get_session_journal_path(trim(session%project_key), &
            trim(session%lane_id), trim(session%session_id), journal_path, ierr, message)
        if (ierr /= 0) return
        do
            call journal_read_page(trim(journal_path), cursor, 64, &
                int(JOURNAL_MAX_RECORD_BYTES, int64)*64_int64, records, next_cursor, &
                ierr, message)
            if (ierr /= JOURNAL_OK) return
            if (size(records) == 0) return
            do i = 1, size(records)
                call gremlin_json_field(records(i)%json, 'generation', receipt_generation)
                call gremlin_json_field(records(i)%json, 'case_id', receipt_case)
                call gremlin_json_field(records(i)%json, 'status', verdict)
                if (len_trim(receipt_generation) /= HASH_LEN) cycle
                if (receipt_generation == generation) cycle
                if (trim(receipt_case) /= trim(case_name)) cycle
                if (trim(verdict) /= 'PASS') cycle
                passed = .true.
                return
            end do
            if (next_cursor <= cursor) then
                ierr = JOURNAL_INVALID
                message = 'journal cursor did not advance during regression confirmation'
                return
            end if
            cursor = next_cursor
        end do
    end subroutine prior_generation_case_passed

    subroutine read_last_lifecycle_event(path, event, found, ierr, message)
        character(len=*), intent(in) :: path
        type(gremlin_lifecycle_event_t), intent(out) :: event
        logical, intent(out) :: found
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(gremlin_lifecycle_event_t), allocatable :: page(:)
        integer(int64) :: cursor, next_cursor
        integer :: n_events
        logical :: has_more

        event = gremlin_lifecycle_event_t()
        found = .false.
        cursor = 0_int64
        ierr = LIFECYCLE_OK
        message = ''
        do
            call gremlin_lifecycle_read_page(path, cursor, 128, &
                int(LIFECYCLE_MAX_EVENT, int64), page, n_events, next_cursor, &
                has_more, ierr, message)
            if (ierr /= LIFECYCLE_OK) return
            if (n_events > 0) then
                event = page(n_events)
                found = .true.
            end if
            if (.not. has_more) exit
            if (next_cursor <= cursor) then
                ierr = LIFECYCLE_INVALID
                message = 'lifecycle event cursor did not advance'
                return
            end if
            cursor = next_cursor
        end do
    end subroutine read_last_lifecycle_event

    subroutine append_lifecycle_event(session, request, path, event_type, active, &
            candidate, subject_generation, previous_generation, readiness, coverage, &
            have_coverage, ierr, message, diagnostic)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        character(len=*), intent(in) :: path, event_type, subject_generation
        character(len=*), intent(in) :: previous_generation
        type(generation_t), intent(in) :: active, candidate
        type(gremlin_readiness_t), intent(in) :: readiness
        type(gremlin_coverage_view_t), intent(in) :: coverage
        logical, intent(in) :: have_coverage
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: diagnostic
        type(gremlin_lifecycle_event_t) :: event

        event%event_type = trim(event_type)
        event%diagnostic = ''
        if (present(diagnostic)) event%diagnostic = diagnostic
        event%generation = trim(subject_generation)
        event%active_generation = trim(active%identity)
        event%candidate_generation = trim(candidate%identity)
        event%previous_generation = trim(previous_generation)
        event%phase = gremlin_phase_name(readiness%phase)
        event%health = gremlin_health_name(readiness%health)
        event%event_epoch = readiness%event_epoch
        event%requirement_digest = readiness%requirement_digest
        event%gate_token = readiness%gate_token
        event%dirty = readiness%dirty
        event%local_gate_green = readiness%local_gate_green
        event%verification_level = gremlin_verification_name(readiness%verification_level)
        event%fully_verified = readiness%fully_verified
        event%gate_required = readiness%gate_required
        event%gate_passed = readiness%gate_passed
        event%ordinary_required = readiness%ordinary_required
        event%ordinary_passed = readiness%ordinary_passed
        event%ordinary_failures = readiness%ordinary_failures
        event%full_required = readiness%full_required
        event%full_passed = readiness%full_passed
        event%full_failures = readiness%full_failures
        if (have_coverage) then
            event%epoch = coverage%epoch
            event%epoch_cursor = coverage%cursor
        end if
        event%event_id = gremlin_lifecycle_make_id(session%session_id, event)
        call gremlin_lifecycle_append(path, event, ierr, message)
    end subroutine append_lifecycle_event

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
        integer(int64) :: next_cursor, file_size
        integer(int64) :: next_lifecycle_cursor
        integer :: journal_status, i, emitted, n_lifecycle, lifecycle_status
        logical :: exists, has_more
        logical :: lifecycle_has_more
        character(len=16) :: row_status
        character(len=PATH_LEN) :: lifecycle_path, lifecycle_message
        character(len=:), allocatable :: base

        call journal_read_page(session%state_dir//'/journal.jsonl', request%cursor, &
            request%max_records, request%max_bytes, records, next_cursor, &
            journal_status, message)
        ierr = journal_status
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
        base = trim(status_text)
        if (len(base) < 2) then
            base = '{"protocol":1,"session_id":"'// &
                trim(json_escape_string(session%session_id))//'","lane_id":"'// &
                trim(json_escape_string(request%lane_id))//'","state":"unknown",'// &
                '"active_generation":"","candidate_generation":"","completed":0,'// &
                '"selected":0,"seed":0,"last_outcome":"NONE","last_exitcode":0}'
        end if
        response = '{"action":"'//trim(action)//'",'//base(2:len(base) - 1)// &
            ',"events":['
        emitted = 0
        do i = 1, size(records)
            if (failures_only) then
                call gremlin_json_field(records(i)%json, 'status', row_status)
                if (.not. status_is_failure(trim(row_status))) cycle
            end if
            if (emitted > 0) response = response//','
            response = response//records(i)%json
            emitted = emitted + 1
        end do
        response = response//'],"next_cursor":'//trim(int64_text(next_cursor))// &
            ',"has_more":'//trim(json_bool(has_more))
        call lifecycle_path_for_session(session, request, lifecycle_path, &
            lifecycle_status, lifecycle_message)
        if (lifecycle_status /= 0) then
            ierr = lifecycle_status
            message = trim(lifecycle_message)
            return
        end if
        call gremlin_lifecycle_read_page(trim(lifecycle_path), request%lifecycle_cursor, &
            request%max_records, request%max_bytes, lifecycle_events, n_lifecycle, &
            next_lifecycle_cursor, lifecycle_has_more, lifecycle_status, message)
        if (lifecycle_status /= LIFECYCLE_OK) then
            ierr = lifecycle_status
            return
        end if
        response = response//',"lifecycle_events":['
        do i = 1, n_lifecycle
            if (i > 1) response = response//','
            response = response//lifecycle_events(i)%json
        end do
        response = response//'],"next_lifecycle_cursor":'// &
            trim(int64_text(next_lifecycle_cursor))// &
            ',"lifecycle_has_more":'//trim(json_bool(lifecycle_has_more))//'}'
        ierr = 0
        message = ''
    end subroutine response_with_events

    subroutine lifecycle_path_for_session(session, request, path, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        character(len=*), intent(out) :: path, message
        integer, intent(out) :: ierr
        character(len=PATH_LEN) :: journal_path
        integer :: suffix_length

        path = ''
        if (.not. allocated(session%project_key) .or. &
            .not. allocated(session%session_id)) then
            ierr = 1
            message = 'Gremlin session has no lifecycle journal identity'
            return
        end if
        call gremlin_get_session_journal_path(trim(session%project_key), &
            request%lane_id, trim(session%session_id), journal_path, ierr, message)
        if (ierr /= 0) return
        suffix_length = len('/journal.jsonl')
        if (len_trim(journal_path) <= suffix_length .or. &
            journal_path(len_trim(journal_path) - suffix_length + 1:) /= '/journal.jsonl') then
            ierr = 1
            message = 'Gremlin session receipt journal path is invalid'
            return
        end if
        path = journal_path(:len_trim(journal_path) - suffix_length)//'/lifecycle.jsonl'
        ierr = 0
        message = ''
    end subroutine lifecycle_path_for_session

    subroutine release_generation_lease(lease, held, ierr, message)
        type(gremlin_lease_t), intent(inout) :: lease
        logical, intent(inout) :: held
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        if (.not. held) then
            ierr = 0
            message = ''
            return
        end if
        call gremlin_lease_release(lease, ierr, message)
        if (ierr == 0) held = .false.
    end subroutine release_generation_lease

    logical function status_is_failure(status)
        character(len=*), intent(in) :: status

        status_is_failure = status == 'FAIL' .or. status == 'BUILD_FAIL' .or. &
            status == 'TIMEOUT' .or. status == 'FLAKY' .or. status == 'INFRA_ERROR'
    end function status_is_failure

    logical function coverage_outcome(status)
        character(len=*), intent(in) :: status
        coverage_outcome = status == 'PASS' .or. status == 'FAIL' .or. &
            status == 'TIMEOUT' .or. status == 'FLAKY' .or. &
            status == 'INFRA' .or. status == 'INFRA_ERROR' .or. &
            status == 'CANCELLED'
    end function coverage_outcome

    function request_policy_key(request) result(key)
        type(gremlin_request_t), intent(in) :: request
        character(len=HASH_LEN) :: key

        character(len=32) :: number
        character(len=:), allocatable :: canonical
        integer :: i

        canonical = 'v1|r='//trim(int_text(request%random_count))// &
            '|s='//trim(int_text(request%seed))//'|c='// &
            trim(int_text(request%campaign_seconds))//'|t='// &
            trim(int_text(request%timeout_seconds))//'|j='// &
            trim(int_text(request%jobs))//'|changed='// &
            trim(json_bool(request%only_changed))//'|shuffle='// &
            trim(json_bool(request%shuffle))
        do i = 1, request%n_targets
            write (number, '(i0)') len_trim(request%targets(i))
            canonical = canonical//'|target:'//trim(number)//':'// &
                trim(request%targets(i))
        end do
        key = text_digest(canonical)
    end function request_policy_key

    function text_digest(text) result(digest)
        character(len=*), intent(in) :: text
        character(len=HASH_LEN) :: digest

        character(len=:), allocatable :: parts(:)

        allocate (character(len=max(1, len(text))) :: parts(1))
        parts(1) = text
        digest = cache_digest(parts, 1)
    end function text_digest

    subroutine simple_response(action, lane_id, session_id, state, response)
        character(len=*), intent(in) :: action, lane_id, session_id, state
        character(len=:), allocatable, intent(out) :: response

        response = '{"action":"'//trim(json_escape_string(action))// &
            '","lane_id":"'//trim(json_escape_string(lane_id))// &
            '","session_id":"'//trim(json_escape_string(session_id))// &
            '","state":"'//trim(json_escape_string(state))//'"}'
    end subroutine simple_response

    subroutine error_response(action, message, response)
        character(len=*), intent(in) :: action, message
        character(len=:), allocatable, intent(out) :: response

        response = '{"action":"'//trim(json_escape_string(action))// &
            '","error":"'//trim(json_escape_string(message))//'"}'
    end subroutine error_response

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

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
    end function int_text

    function int64_text(value) result(text)
        integer(int64), intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
    end function int64_text

    function current_test_name(selected, n_selected) result(name)
        character(len=*), intent(in) :: selected(:)
        integer, intent(in) :: n_selected
        character(len=NAME_LEN) :: name

        name = ''
        if (n_selected <= 0) return
        name = selected(1)
    end function current_test_name

    subroutine log_path(session, filename, path)
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: path

        path = trim(session%state_dir)//'/logs/'//trim(session%session_id)// &
            '-'//trim(filename)
    end subroutine log_path

    subroutine poll_launcher(pid)
        integer, intent(in) :: pid

        integer :: ignored_exit
        logical :: done

        if (pid <= 0) return
        call process_poll_pid(pid, done, ignored_exit)
    end subroutine poll_launcher

    subroutine cancel_launched_owner(pid, ierr)
        integer, intent(in) :: pid
        integer, intent(out) :: ierr

        call cancel_owned_process(pid, ierr)
    end subroutine cancel_launched_owner

    subroutine cancel_owned_process(pid, ierr)
        integer, intent(in) :: pid
        integer, intent(out) :: ierr

        integer :: attempt, exitcode
        logical :: done

        ierr = 0
        if (pid <= 0) return
        do attempt = 1, 3
            call process_cancel_pid(pid, ierr)
            if (ierr == 0) return
            call process_poll_pid(pid, done, exitcode)
            if (done) then
                ierr = 0
                return
            end if
            call fs_sleep_ms(50)
        end do
    end subroutine cancel_owned_process


    subroutine clock_milliseconds(milliseconds)
        integer(int64), intent(out) :: milliseconds
        integer(int64) :: count, rate

        call system_clock(count=count, count_rate=rate)
        milliseconds = 0_int64
        if (rate <= 0_int64) return
        milliseconds = (count/rate)*1000_int64 + &
            modulo(count, rate)*1000_int64/rate
    end subroutine clock_milliseconds

    subroutine clock_seconds(seconds)
        real, intent(out) :: seconds
        integer(int64) :: count, rate

        call system_clock(count=count, count_rate=rate)
        seconds = 0.0
        if (rate > 0_int64) seconds = real(count)/real(rate)
    end subroutine clock_seconds

    real function elapsed_seconds(started)
        real, intent(in) :: started
        real :: now

        call clock_seconds(now)
        elapsed_seconds = max(0.0, now - started)
    end function elapsed_seconds

    integer(int64) function system_clock_count()
        integer(int64) :: count
        call system_clock(count=count)
        system_clock_count = count
    end function system_clock_count

    function runner_case_outcome(log_file, case_name) result(outcome)
        character(len=*), intent(in) :: log_file, case_name
        character(len=16) :: outcome
        character(len=8192) :: line
        character(len=NAME_LEN) :: result_name
        character(len=32) :: result_status, verdict
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        integer :: unit, ios, depth, matches, tests_count, name_count, status_count
        logical :: tests_array, test_object, want_tests, want_name, want_status
        logical :: malformed

        outcome = 'INFRA_ERROR'
        open (newunit=unit, file=log_file, status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(adjustl(line), '{') /= 1) cycle
            outcome = 'INFRA_ERROR'
            depth = 0
            matches = 0
            tests_count = 0
            name_count = 0
            status_count = 0
            malformed = .false.
            verdict = ''
            tests_array = .false.
            test_object = .false.
            want_tests = .false.
            want_name = .false.
            want_status = .false.
            call json_parser_init_strict(parser, trim(line))
            do
                call json_parser_next(parser, event)
                select case (event%event_type)
                case (JSON_OBJECT_START)
                    if (want_name .or. want_status) malformed = .true.
                    depth = depth + 1
                    if (depth == 3 .and. tests_array) then
                        test_object = .true.
                        result_name = ''
                        result_status = ''
                        name_count = 0
                        status_count = 0
                    end if
                    want_tests = .false.
                    want_name = .false.
                    want_status = .false.
                case (JSON_ARRAY_START)
                    if (want_name .or. want_status) malformed = .true.
                    if (tests_array .and. depth == 2) malformed = .true.
                    depth = depth + 1
                    if (depth == 2 .and. want_tests) tests_array = .true.
                    want_tests = .false.
                    want_name = .false.
                    want_status = .false.
                case (JSON_OBJECT_END)
                    if (depth == 3 .and. test_object) then
                        if (name_count /= 1 .or. status_count /= 1) malformed = .true.
                        if (name_count == 1 .and. status_count == 1 .and. &
                            trim(result_name) == trim(case_name)) then
                            matches = matches + 1
                            verdict = result_status
                        end if
                        test_object = .false.
                    end if
                    depth = depth - 1
                case (JSON_ARRAY_END)
                    if (depth == 2) tests_array = .false.
                    depth = depth - 1
                case (JSON_KEY)
                    if (.not. allocated(event%string_val)) cycle
                    if (depth == 1) then
                        want_tests = event%string_val == 'tests'
                        if (want_tests) tests_count = tests_count + 1
                    else if (depth == 3 .and. test_object) then
                        want_name = event%string_val == 'name'
                        want_status = event%string_val == 'status'
                        if (want_name) name_count = name_count + 1
                        if (want_status) status_count = status_count + 1
                        if (name_count > 1 .or. status_count > 1) malformed = .true.
                    end if
                case (JSON_STRING)
                    if (tests_array .and. depth == 2) malformed = .true.
                    if (depth /= 3 .or. .not. test_object) cycle
                    if (.not. allocated(event%string_val)) cycle
                    if (want_name) then
                        if (len(event%string_val) > len(result_name)) malformed = .true.
                        result_name = event%string_val
                    end if
                    if (want_status) then
                        if (len(event%string_val) > len(result_status)) malformed = .true.
                        result_status = event%string_val
                    end if
                    want_name = .false.
                    want_status = .false.
                case (JSON_ERROR)
                    exit
                case (JSON_END_OF_INPUT)
                    if (tests_count == 1 .and. matches == 1 .and. .not. malformed) then
                        select case (trim(verdict))
                        case ('pass')
                            outcome = 'PASS'
                        case ('fail')
                            outcome = 'FAIL'
                        case ('timeout')
                            outcome = 'TIMEOUT'
                        case ('flaky')
                            outcome = 'FLAKY'
                        case default
                            outcome = 'INFRA_ERROR'
                        end select
                    end if
                    exit
                case default
                    if (want_name .or. want_status) malformed = .true.
                    if (tests_array .and. depth == 2) malformed = .true.
                    want_tests = .false.
                    want_name = .false.
                    want_status = .false.
                end select
            end do
        end do
        close (unit)
    end function runner_case_outcome

    integer function case_wall_timeout(project_dir, case_name, override) result(seconds)
        character(len=*), intent(in) :: project_dir, case_name
        integer, intent(in) :: override
        type(fpm_config_t), allocatable :: config
        integer :: ierr, budget

        seconds = override
        if (seconds > 0) return
        allocate (config)
        call fpm_config_parse(project_dir, config, ierr)
        budget = test_budget_seconds(config, is_slow_test(case_name))
        seconds = test_wall_cap_seconds(config, budget) + 5
    end function case_wall_timeout

    subroutine process_wait_bounded(pid, timeout_seconds, exitcode)
        integer, intent(in) :: pid, timeout_seconds
        integer, intent(out) :: exitcode

        integer :: cancel_status
        logical :: done
        real :: started

        call clock_seconds(started)
        do
            call process_poll_pid(pid, done, exitcode)
            if (done) return
            if (elapsed_seconds(started) >= real(timeout_seconds)) then
                call cancel_owned_process(pid, cancel_status)
                if (cancel_status == 0) then
                    exitcode = 124
                else
                    exitcode = 125
                end if
                return
            end if
            call fs_sleep_ms(50)
        end do
    end subroutine process_wait_bounded

end module fo_gremlin_supervisor
