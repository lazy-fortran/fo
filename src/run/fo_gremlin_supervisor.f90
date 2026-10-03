module fo_gremlin_supervisor
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_build_backend, only: BACKEND_NATIVE, backend_t, detect_backend
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_check, only: fo_changed_modules
    use fo_gremlin_context, only: capture_candidate
    use fo_gremlin_request, only: gremlin_request_t, parse_request, is_hex_digest
    use fo_gremlin_generation, only: generation_t
    use fo_change_watch, only: change_watch_t, change_watch_init, &
        change_watch_poll, change_watch_close
    use fo_gremlin_journal, only: journal_append, journal_read_page, &
        journal_compact_tail, journal_record_t, JOURNAL_OK, JOURNAL_INVALID, &
        JOURNAL_MAX_RECORD_BYTES
    use fo_gremlin_coverage, only: coverage_epoch_t, coverage_open, &
        coverage_next_chunk, coverage_record, coverage_record_path, COVERAGE_OK
    use fo_gremlin_coverage, only: coverage_read_view_path, COVERAGE_NOT_FOUND
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_policy, only: shuffle_gremlin_tests, GREMLIN_POLICY_OK, &
        GREMLIN_NONMANDATORY_LIMIT
    use fo_gremlin_state, only: gremlin_session_t, gremlin_lease_t, &
        gremlin_session_acquire, &
        gremlin_session_publish, gremlin_session_read, gremlin_session_release, &
        gremlin_session_recovery_complete, gremlin_session_state_dir, &
        gremlin_session_request_stop, gremlin_session_stop_requested, &
        gremlin_lease_release, &
        gremlin_generation_lease_acquire_at, gremlin_generation_pin_at, &
        gremlin_generation_root, GREMLIN_STATE_TEXT_MAX
    use fo_gfortran_build, only: gfortran_selected_test_names
    use fo_process, only: argv_push, process_cancel_pid, &
        process_poll_pid, process_start_argv_logged
    use fo_scan_types, only: MAX_PATH
    use fo_util, only: extract_json_field, json_bool, json_int, make_tmpfile
    use fx_dag, only: dag_t, MAX_NODES
    use fx_json_build, only: json_escape_string
    use fo_fs, only: fs_find_executable, fs_make_dir, fs_sleep_ms
    use fo_gremlin_session, only: gremlin_resolve_read_session, &
        gremlin_load_terminal_session, gremlin_get_session_journal_path, &
        gremlin_stage_recovery_journal, gremlin_recover_owner_journal, &
        gremlin_publish_terminal_snapshot, gremlin_release_stopped_session
    implicit none
    private

    integer, parameter :: NAME_LEN = 128, PATH_LEN = 4096
    integer, parameter :: GREMLIN_HISTORY_LIMIT = 128
    integer, parameter :: GREMLIN_POLICY_RECEIPT_LIMIT = 512


    type :: child_t
        integer :: pid = 0
        integer :: campaign = 0
        integer :: case_index = 0
        integer :: seed = 0
        character(len=PATH_LEN) :: log_file = ''
        character(len=NAME_LEN) :: case_name = ''
        real :: started_at = 0.0
    end type child_t

    public :: gremlin_handle

contains

    subroutine gremlin_handle(action, project_dir, request_json, response_json, exitcode)
        character(len=*), intent(in) :: action, project_dir, request_json
        character(len=:), allocatable, intent(out) :: response_json
        integer, intent(out) :: exitcode

        type(gremlin_request_t) :: request
        type(gremlin_session_t) :: session
        character(len=PATH_LEN) :: error_message, owner_start
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=128) :: session_id
        integer :: ierr, owner_pid

        response_json = ''
        exitcode = 0
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
        character(len=PATH_LEN) :: message, status_text, owner_start
        character(len=128) :: session_id
        character(len=HASH_LEN) :: stored_policy
        integer :: ierr, owner_pid, pid, spawn_exit, i, n_args
        character(len=:), allocatable :: packed
        character(len=PATH_LEN) :: executable, log_file
        logical :: executable_ok

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
            call extract_json_field(status_text, 'policy_key', stored_policy)
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
        call gremlin_session_release(session, ierr, message)
        if (ierr /= 0) then
            call error_response('start', 'cannot reserve Gremlin owner: '//trim(message), &
                response)
            exitcode = 2
            return
        end if
        call find_self_executable(executable, executable_ok)
        if (.not. executable_ok) then
            call error_response('start', 'cannot locate the current fo executable', response)
            exitcode = 2
            return
        end if
        call make_tmpfile('fo-gremlin-launch', log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'gremlin')
        call argv_push(packed, n_args, 'run')
        call argv_push(packed, n_args, '--dir')
        call argv_push(packed, n_args, trim(project_dir))
        call argv_push(packed, n_args, '--lane-id')
        call argv_push(packed, n_args, trim(request%lane_id))
        call argv_push(packed, n_args, '--random-count')
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
            pid, spawn_exit)
        if (spawn_exit /= 0) then
            call error_response('start', 'cannot launch the Gremlin owner', response)
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
            call extract_json_field(status_text, 'policy_key', stored_policy)
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
        character(len=HASH_LEN) :: generation_id
        character(len=PATH_LEN) :: coverage_path, coverage_state_dir
        character(len=:), allocatable :: enriched_status
        integer :: owner_pid, ierr
        logical :: is_live

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
        call extract_json_field(status_text, 'active_generation', generation_id)
        enriched_status = trim(status_text)
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
                enriched_status = status_with_coverage(trim(status_text), coverage_view)
            else if (ierr /= COVERAGE_NOT_FOUND) then
                call error_response('status', trim(message), response)
                exitcode = 2
                return
            end if
        end if
        call response_with_events('status', enriched_status, session, request, .false., &
            response, ierr, message)
        if (ierr /= 0) then
            call error_response('status', trim(message), response)
            exitcode = 2
            return
        end if
        exitcode = 0
    end subroutine handle_status

    function status_with_coverage(status_text, view) result(enriched)
        character(len=*), intent(in) :: status_text
        type(gremlin_coverage_view_t), intent(in) :: view
        character(len=:), allocatable :: enriched
        character(len=:), allocatable :: object

        object = '{"generation_id":"'//trim(view%generation_id)// &
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
            ',"ordinary_complete":'//trim(json_bool(view%ordinary_complete))// &
            ',"full_coverage":'//trim(json_bool(view%full_coverage))// &
            ',"green":'//trim(json_bool(view%green))//'}'
        enriched = status_text(:len_trim(status_text) - 1)// &
            ',"coverage":'//object//'}'
    end function status_with_coverage

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

        character(len=65536) :: status_text
        character(len=PATH_LEN) :: message
        type(gremlin_request_t) :: poll_request
        type(gremlin_session_t) :: session
        integer :: owner_pid, ierr, i
        integer(int64) :: next_cursor
        logical :: failed, has_events, is_live

        failed = .false.
        poll_request = request
        do i = 1, max(1, (request%wait_ms + 99)/100)
            call gremlin_resolve_read_session(project_dir, poll_request, session, status_text, &
                owner_pid, is_live, ierr, message)
            if (ierr /= 0) then
                call error_response('wait', trim(message), response)
                exitcode = 2
                return
            end if
            if (is_live) call poll_launcher(owner_pid)
            call event_page_has_failure(session, poll_request, failed, has_events, &
                next_cursor, ierr, message)
            if (ierr /= 0) then
                call error_response('wait', trim(message), response)
                exitcode = 2
                return
            end if
            if ((has_events .and. .not. request%fail_on_failure) .or. &
                (has_events .and. failed) .or. &
                index(status_text, '"state":"stopped"') > 0) exit
            if (.not. is_live) exit
            if (has_events .and. request%fail_on_failure .and. &
                next_cursor > poll_request%cursor) poll_request%cursor = next_cursor
            if (i < max(1, (request%wait_ms + 99)/100)) call fs_sleep_ms(100)
        end do
        call response_with_events('wait', status_text, session, poll_request, .false., &
            response, ierr, message)
        if (ierr /= 0) then
            call error_response('wait', trim(message), response)
            exitcode = 2
            return
        end if
        exitcode = 0
        if (request%fail_on_failure .and. failed) exitcode = 1
    end subroutine handle_wait

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
        character(len=:), allocatable :: packed
        integer :: owner_pid, ierr, n_selected, mandatory_count, seed
        integer :: n_args, spawn_exit, test_exit, sequence, release_error
        logical :: executable_ok
        logical :: have_reproduction_lease

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
        call extract_json_field(status_text, 'active_generation', active_identity)
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
        call gremlin_generation_root(trim(active_identity), generation_root, ierr, message)
        if (ierr /= 0) then
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', 'cannot resolve requested generation: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        active_project = trim(generation_root)//'/bundle/project'
        generation%identity = active_identity
        generation%root = generation_root
        generation%project_root = active_project
        call gremlin_generation_lease_acquire_at(generation%root, reproduction_lease, &
            ierr, message)
        if (ierr /= 0) then
            call error_response('reproduce', 'cannot lease requested generation: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        have_reproduction_lease = .true.
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
        call find_self_executable(executable, executable_ok)
        if (.not. executable_ok) then
            call release_generation_lease(reproduction_lease, have_reproduction_lease, &
                release_error, cleanup_message)
            call release_if_owner(session, ierr, message)
            call error_response('reproduce', 'cannot locate the current fo executable', &
                response)
            exitcode = 2
            return
        end if
        ! The completion sequence is the reproduction execution ID. Allocate it
        ! before launch so its output path and durable receipt identify the same
        ! invocation, including concurrent reproductions in one live session.
        sequence = int(system_clock_count())
        write (reproduction_id, '(i0)') sequence
        call log_path(session, 'reproduce-'//trim(reproduction_id)//'.log', log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'test')
        call argv_push(packed, n_args, trim(request%case_id))
        call process_start_argv_logged(trim(active_project), packed, n_args, &
            trim(log_file), owner_pid, spawn_exit, 'FO_JOBS=1')
        if (spawn_exit == 0) then
            call process_wait_bounded(owner_pid, request%timeout_seconds, test_exit)
            if (test_exit == 124) then
                outcome = 'TIMEOUT'
                test_exit = 124
            else if (test_exit == 125) then
                outcome = 'INFRA_ERROR'
                call gremlin_generation_pin_at(generation%root, .true., &
                    release_error, cleanup_message)
            else if (test_exit == 0) then
                outcome = 'PASS'
            else
                outcome = 'FAIL'
            end if
        else
            test_exit = spawn_exit
            outcome = 'INFRA_ERROR'
        end if
        call record_immediate_case(session, request, generation, request%case_id, &
            test_exit, trim(outcome), sequence, 1, seed, trim(log_file), ierr, message, &
            credit_coverage=.false.)
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
        type(gremlin_lease_t) :: active_lease, candidate_lease
        type(generation_t) :: active_generation, candidate_generation
        type(change_watch_t) :: change_watch
        type(child_t) :: build_child, test_child
        character(len=PATH_LEN) :: message, state_name, last_failed_identity
        character(len=PATH_LEN) :: fatal_message, state_message
        character(len=PATH_LEN) :: owner_start
        character(len=NAME_LEN) :: selected(MAX_NODES)
        character(len=HASH_LEN) :: stored_policy
        character(len=65536) :: status_text
        character(len=128) :: stored_session_id
        integer :: ierr, build_exit, test_exit, launch_error, completed, selected_count
        integer :: cancel_error, release_error, capture_error, state_error
        integer :: registration_error
        integer :: owner_pid, i
        integer :: sequence, campaign_seed, campaign_number, test_index
        integer(int64) :: capture_debounce_ms, campaign_started_ms
        logical :: have_active, have_candidate, stop_requested, capture_ok
        logical :: capture_failed
        logical :: have_active_lease, have_candidate_lease
        logical :: active_pinned, candidate_pinned
        logical :: build_done, test_done, fatal_error

        exitcode = 0
        active_generation = generation_t()
        candidate_generation = generation_t()
        build_child = child_t()
        test_child = child_t()
        completed = 0
        selected_count = 0
        sequence = 0
        campaign_number = 0
        test_index = 0
        capture_debounce_ms = 0_int64
        campaign_started_ms = 0_int64
        have_active = .false.
        have_candidate = .false.
        have_active_lease = .false.
        have_candidate_lease = .false.
        active_pinned = .false.
        candidate_pinned = .false.
        last_failed_identity = ''
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
            call extract_json_field(status_text, 'policy_key', stored_policy)
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
        call gremlin_recover_owner_journal(project_dir, session, ierr, message)
        if (ierr /= 0) then
            call release_if_owner(session, state_error, state_message)
            call error_response('run', 'cannot recover prior Gremlin receipts: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        call fs_make_dir(session%state_dir//'/logs')
        call publish_state(session, request, 'starting', active_generation, &
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
        call change_watch_init(change_watch, project_dir, ierr)
        if (ierr /= 0) then
            call change_watch_close(change_watch)
            call release_if_owner(session, state_error, state_message)
            call error_response('run', 'cannot start Gremlin change watcher', response)
            exitcode = 2
            return
        end if
        call capture_candidate(project_dir, candidate_generation, &
            capture_ok, registration_error, message, change_watch=change_watch)
        if (registration_error /= 0) then
            fatal_error = .true.
            fatal_message = 'cannot register captured generation: '//trim(message)
        end if
        if (capture_ok .and. registration_error == 0) then
            have_candidate = .true.
            call gremlin_generation_lease_acquire_at(candidate_generation%root, &
                candidate_lease, ierr, message)
            if (ierr /= 0) then
                fatal_error = .true.
                fatal_message = 'cannot lease captured generation: '//trim(message)
            else
                have_candidate_lease = .true.
            end if
            if (.not. fatal_error) then
                call start_build(session, candidate_generation, build_child, ierr, message)
                if (ierr /= 0) then
                    launch_error = ierr
                    fatal_message = 'cannot start initial candidate build'
                    call publish_state(session, request, 'build_failed', active_generation, &
                        candidate_generation, '', completed, selected_count, campaign_seed, &
                        'INFRA_ERROR', launch_error, state_error, state_message)
                    if (state_error == 0) then
                        sequence = sequence + 1
                        call record_immediate_case(session, request, candidate_generation, &
                            '<build>', launch_error, 'INFRA_ERROR', sequence, 0, &
                            campaign_seed, build_child%log_file, ierr, message)
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
                    state_name = 'building'
                end if
            end if
        else if (.not. fatal_error) then
            call publish_state(session, request, 'capture_failed', active_generation, &
                candidate_generation, '', completed, selected_count, 0, 'NONE', 0, &
                ierr, message)
            if (ierr /= 0) fatal_error = .true.
        end if

        do
            if (fatal_error) exit
            call gremlin_session_stop_requested(session, stop_requested, ierr, message)
            if (ierr /= 0) then
                fatal_error = .true.
                exit
            end if
            if (stop_requested) exit

            if (build_child%pid > 0) then
                call process_poll_pid(build_child%pid, build_done, build_exit)
                if (build_done) then
                    call complete_build(session, request, candidate_generation, &
                        build_child, build_exit, active_generation, have_active, &
                        test_child, active_lease, candidate_lease, &
                        have_active_lease, have_candidate_lease, active_pinned, &
                        candidate_pinned, selected, selected_count, campaign_seed, &
                        completed, state_name, last_failed_identity, sequence, ierr, message)
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                    if (build_exit == 0 .and. have_active .and. selected_count > 0) then
                        campaign_number = campaign_number + 1
                        test_index = 1
                        call clock_milliseconds(campaign_started_ms)
                        call launch_selected_case(session, request, active_generation, &
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
                call process_poll_pid(test_child%pid, test_done, test_exit)
                if (test_done) then
                    if (test_exit == 0) then
                        state_name = 'testing'
                        call record_case(session, request, active_generation, &
                            test_child, test_exit, 'PASS', sequence, campaign_seed, &
                            ierr, message)
                    else
                        call record_case(session, request, active_generation, &
                            test_child, test_exit, 'FAIL', sequence, campaign_seed, &
                            ierr, message)
                    end if
                    test_child%pid = 0
                    completed = completed + 1
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                    call advance_campaign(session, request, active_generation, selected, &
                        selected_count, test_index, campaign_seed, campaign_number, &
                        campaign_started_ms, test_child, ierr, message, completed, &
                        state_name, sequence)
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                else if (elapsed_seconds(test_child%started_at) >= &
                        real(request%timeout_seconds)) then
                    call cancel_owned_process(test_child%pid, test_exit)
                    if (test_exit /= 0) then
                        message = 'cannot cancel timed-out Gremlin test process'
                        fatal_error = .true.
                        exit
                    end if
                    call record_case(session, request, active_generation, &
                        test_child, 124, 'TIMEOUT', sequence, campaign_seed, ierr, message)
                    test_child%pid = 0
                    completed = completed + 1
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                    call advance_campaign(session, request, active_generation, selected, &
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

            call maybe_capture_latest(project_dir, session, request, active_generation, &
                candidate_generation, have_active, have_candidate, last_failed_identity, &
                build_child, candidate_lease, have_candidate_lease, capture_debounce_ms, &
                change_watch, sequence, capture_failed, ierr, message)
            if (ierr /= 0) then
                fatal_message = trim(message)
                fatal_error = .true.
                exit
            end if
            if (capture_failed) then
                state_name = 'capture_failed'
                capture_error = 1
                call publish_state(session, request, state_name, active_generation, &
                    candidate_generation, current_test_name(selected, selected_count), &
                    completed, selected_count, campaign_seed, 'NONE', capture_error, &
                    state_error, state_message)
                if (state_error /= 0) then
                    fatal_message = 'cannot publish capture failure: '//trim(state_message)
                    fatal_error = .true.
                    exit
                end if
                ierr = 0
            end if
            call fs_sleep_ms(100)
        end do
        call change_watch_close(change_watch)

        if (fatal_error) then
            if (len_trim(fatal_message) == 0) fatal_message = trim(message)
            if (len_trim(fatal_message) == 0) &
                fatal_message = 'Gremlin stopped after an evidence or process error'
            cancel_error = 0
            if (test_child%pid > 0) then
                call cancel_owned_process(test_child%pid, test_exit)
                if (test_exit /= 0) cancel_error = test_exit
                if (test_exit == 0) test_child%pid = 0
            end if
            if (build_child%pid > 0) then
                call cancel_owned_process(build_child%pid, build_exit)
                if (build_exit /= 0 .and. cancel_error == 0) cancel_error = build_exit
                if (build_exit == 0) build_child%pid = 0
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
            call publish_state(session, request, 'error', active_generation, &
                candidate_generation, current_test_name(selected, selected_count), &
                completed, selected_count, campaign_seed, 'INFRA_ERROR', 1, &
                state_error, state_message)
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
            call cancel_owned_process(test_child%pid, test_exit)
            if (test_exit /= 0) cancel_error = test_exit
            if (test_exit == 0) test_child%pid = 0
        end if
        if (build_child%pid > 0) then
            call cancel_owned_process(build_child%pid, build_exit)
            if (build_exit /= 0 .and. cancel_error == 0) cancel_error = build_exit
            if (build_exit == 0) build_child%pid = 0
        end if
        if (cancel_error /= 0) then
            call publish_state(session, request, 'error', active_generation, &
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
            call publish_state(session, request, 'error', active_generation, &
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
        call publish_state(session, request, 'stopped', active_generation, &
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
        call simple_response('run', request%lane_id, session%session_id, 'stopped', response)
    end subroutine run_owner

    subroutine maybe_capture_latest(project_dir, session, request, active, candidate, &
            have_active, have_candidate, last_failed, build_child, candidate_lease, &
            have_candidate_lease, capture_debounce_ms, change_watch, sequence, &
            capture_failed, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
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
        logical, intent(out) :: capture_failed
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(generation_t) :: current
        character(len=PATH_LEN) :: changed_path
        logical :: ok, got_event
        integer(int64) :: now_ms
        integer :: spawn_error, journal_error, release_error, event_type, watch_error

        ierr = 0
        message = ''
        capture_failed = .false.
        call change_watch_poll(change_watch, 0, changed_path, event_type, &
            got_event, watch_error)
        if (watch_error /= 0) then
            ierr = watch_error
            message = 'Gremlin change watcher reconciliation failed'
            return
        end if
        call clock_milliseconds(now_ms)
        if (got_event) then
            capture_debounce_ms = now_ms + 250_int64
            return
        end if
        if (capture_debounce_ms <= 0_int64 .or. now_ms < capture_debounce_ms) return
        capture_debounce_ms = 0_int64
        call capture_candidate(project_dir, current, ok, &
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
            if (current%identity == candidate%identity) return
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
            if (current%identity == active%identity) return
        end if
        if (current%identity == last_failed) return
        candidate = current
        have_candidate = .true.
        call gremlin_generation_lease_acquire_at(candidate%root, candidate_lease, &
            ierr, message)
        if (ierr /= 0) then
            have_candidate = .false.
            message = 'cannot lease candidate generation: '//trim(message)
            return
        end if
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
        end if
    end subroutine maybe_capture_latest

    subroutine start_build(session, generation, child, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(generation_t), intent(in) :: generation
        type(child_t), intent(out) :: child
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: executable
        character(len=:), allocatable :: packed
        integer :: n_args, spawn_exit
        logical :: executable_ok

        child = child_t()
        call find_self_executable(executable, executable_ok)
        if (.not. executable_ok) then
            ierr = 1
            message = 'cannot locate the current fo executable'
            return
        end if
        call log_path(session, 'build-'//generation%identity(1:16), child%log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'build')
        call process_start_argv_logged(trim(generation%project_root), packed, &
            n_args, trim(child%log_file), child%pid, spawn_exit, &
            'FO_JOBS=1;FO_DISABLE_SELF_REFRESH=1')
        ierr = spawn_exit
        if (ierr /= 0) then
            message = 'cannot start candidate build'
            child%pid = 0
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
        type(gremlin_request_t), intent(in) :: request
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
        type(gremlin_request_t) :: selection_request
        integer :: inventory_status, cancel_exit, mandatory_count, state_status
        integer :: release_status, pin_status
        character(len=PATH_LEN) :: state_message, release_message
        character(len=NAME_LEN) :: active_case
        logical :: was_active

        ierr = 0
        message = ''
        was_active = have_active
        call record_build(session, request, candidate, build_exit, build_child%log_file, &
            sequence, ierr, message)
        if (ierr /= 0) return
        build_child%pid = 0
        if (build_exit /= 0) then
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
            call cancel_owned_process(test_child%pid, cancel_exit)
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
        active = candidate
        have_active = .true.
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
        if (inventory_status /= 0) then
            state_name = 'inventory_failed'
            selected_count = 0
            call publish_state(session, request, state_name, active, candidate, '', &
                completed, selected_count, seed, 'INFRA_ERROR', inventory_status, &
                ierr, message)
            if (ierr == 0) then
                ierr = inventory_status
                message = 'cannot discover the frozen test inventory'
            end if
            return
        end if
        state_name = 'testing'
        if (selected_count == 0) state_name = 'idle'
        call publish_state(session, request, state_name, active, candidate, &
            current_test_name(selected, selected_count), completed, selected_count, &
            seed, 'NONE', 0, ierr, message)
    end subroutine complete_build

    subroutine discover_campaign(project_dir, generation_id, session, request, &
            selected, n_selected, &
            n_mandatory_selected, seed, ierr, message, bypass_coverage)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: generation_id
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        character(len=*), intent(out) :: selected(:)
        integer, intent(out) :: n_selected, n_mandatory_selected, seed, ierr
        character(len=*), intent(out) :: message
        logical, intent(in), optional :: bypass_coverage

        type(backend_t) :: backend
        type(dag_t) :: dag
        integer :: changed_ids(MAX_NODES), affected_ids(MAX_NODES)
        integer :: n_changed, n_affected, n_cached, i, n_all, n_impacted
        integer :: n_history, n_debt, cursor_seed, n_priorities, limit
        integer :: n_selected_priorities
        integer :: candidate_ids(MAX_NODES), shuffle_status, coverage_status
        character(len=MAX_PATH) :: filenames(MAX_NODES)
        character(len=NAME_LEN) :: all_names(MAX_NODES), impacted(MAX_NODES)
        character(len=NAME_LEN) :: history(MAX_NODES), debt(MAX_NODES)
        character(len=NAME_LEN) :: priorities(MAX_NODES)
        type(coverage_epoch_t) :: coverage
        character(len=PATH_LEN) :: coverage_path
        logical :: is_test_arr(MAX_NODES)
        logical :: reproduce_only

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
            dag%n_nodes, .false., all_names, n_all)
        call read_campaign_history(session, all_names, n_all, history, n_history, &
            debt, n_debt, cursor_seed, ierr, message)
        if (ierr /= 0) return
        if (request%only_changed) then
            n_impacted = 0
            call gfortran_selected_test_names(project_dir, filenames, affected_ids, &
                n_affected, .false., impacted, n_impacted)
        else
            n_impacted = 0
        end if
        n_priorities = 0
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
        n_mandatory_selected = n_selected_priorities
        if (request%shuffle) then
            call shuffle_gremlin_tests(selected, n_mandatory_selected, n_selected, &
                seed, shuffle_status)
            if (shuffle_status /= GREMLIN_POLICY_OK) then
                ierr = shuffle_status
                message = 'Gremlin could not shuffle selected tests'
            end if
        end if
    end subroutine discover_campaign

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
                call extract_json_field(records(i)%json, 'case_id', case_name)
                call extract_json_field(records(i)%json, 'generation', generation)
                call extract_json_field(records(i)%json, 'status', outcome)
                call extract_json_field(records(i)%json, 'seed', seed_text)
                call extract_json_field(records(i)%json, 'inventory_digest', &
                    inventory_digest)
                call extract_json_field(records(i)%json, 'coverage_epoch', epoch_text)
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
                call extract_json_field(records(i)%json, 'case_id', case_name)
                call extract_json_field(records(i)%json, 'status', status_name)
                call extract_json_field(records(i)%json, 'seed', seed_text)
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

        character(len=PATH_LEN) :: executable, log_name
        character(len=PATH_LEN) :: journal_message
        character(len=:), allocatable :: packed
        integer :: n_args, spawn_exit, journal_status, coverage_status
        logical :: executable_ok
        character(len=PATH_LEN) :: coverage_path

        child = child_t()
        ierr = 0
        message = ''
        if (index_case < 1 .or. index_case > n_selected) return
        call find_self_executable(executable, executable_ok)
        if (.not. executable_ok) then
            ierr = 1
            message = 'cannot locate the current fo executable'
            return
        end if
        write (log_name, '(a,i0,a,i0,a)') 'case-', campaign, '-', index_case, '.log'
        call log_path(session, trim(log_name), child%log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'test')
        call argv_push(packed, n_args, trim(selected(index_case)))
        coverage_path = trim(session%state_dir)//'/coverage-'// &
            generation%identity//'.state'
        call coverage_record_path(trim(coverage_path), generation%identity, &
            trim(selected(index_case)), 'RUNNING', coverage_status, message)
        if (coverage_status /= COVERAGE_OK) then
            ierr = coverage_status
            return
        end if
        call process_start_argv_logged(trim(generation%project_root), packed, &
            n_args, trim(child%log_file), child%pid, spawn_exit, 'FO_JOBS=1')
        ierr = spawn_exit
        if (spawn_exit /= 0) then
            child%pid = 0
            journal_message = 'cannot start selected test '//trim(selected(index_case))
            sequence = sequence + 1
            call record_immediate_case(session, request, generation, &
                selected(index_case), spawn_exit, 'INFRA_ERROR', sequence, &
                index_case, seed, child%log_file, journal_status, message)
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
        call clock_seconds(child%started_at)
        call publish_state(session, request, 'testing', generation, generation, &
            child%case_name, completed, n_selected, seed, 'RUNNING', 0, ierr, message)
    end subroutine launch_selected_case

    subroutine advance_campaign(session, request, generation, selected, n_selected, &
            index_case, seed, campaign, campaign_started_ms, child, ierr, message, &
            completed, state_name, sequence)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
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
        type(gremlin_request_t) :: next_request
        character(len=PATH_LEN) :: launch_message, state_message

        ierr = 0
        message = ''
        call clock_milliseconds(now_ms)
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
            message)
    end subroutine record_case

    subroutine record_immediate_case(session, request, generation, case_name, exitcode, &
            outcome, sequence, case_index, seed, log_file, ierr, message, credit_coverage)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: case_name, outcome, log_file
        integer, intent(in) :: exitcode, sequence, case_index, seed
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        logical, intent(in), optional :: credit_coverage
        logical :: credit
        character(len=256) :: completion_id
        character(len=8192) :: record
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
            '","case_id":"'//trim(json_escape_string(case_name))// &
            '","outcome":"'//trim(journal_outcome)//'","status":"'// &
            trim(outcome)//'","exitcode":'// &
            trim(json_int(exitcode))//',"seed":'//trim(json_int(seed))// &
            trim(receipt_identity)// &
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
            status_text_out)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        character(len=*), intent(in) :: state, current_case, last_outcome
        type(generation_t), intent(in) :: active, candidate
        integer, intent(in) :: completed, selected_count, seed, last_exitcode
        integer, intent(out), optional :: ierr
        character(len=*), intent(out), optional :: message
        character(len=*), intent(out), optional :: status_text_out

        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=PATH_LEN) :: local_message
        integer :: status
        character(len=PATH_LEN) :: active_project, candidate_project

        active_project = ''
        candidate_project = ''
        if (len_trim(active%identity) > 0) active_project = trim(active%project_root)
        if (len_trim(candidate%identity) > 0) candidate_project = trim(candidate%project_root)
        status_text = '{"protocol":1,"session_id":"'// &
            trim(json_escape_string(session%session_id))//'","lane_id":"'// &
            trim(json_escape_string(request%lane_id))//'","policy_key":"'// &
            request_policy_key(request)//'","state":"'//trim(state)// &
            '","active_generation":"'//trim(active%identity)// &
            '","candidate_generation":"'//trim(candidate%identity)// &
            '","active_project":"'//trim(json_escape_string(active_project))// &
            '","candidate_project":"'//trim(json_escape_string(candidate_project))// &
            '","current_test":"'//trim(json_escape_string(current_case))// &
            '","completed":'//trim(json_int(completed))// &
            ',"selected":'//trim(json_int(selected_count))// &
            ',"seed":'//trim(json_int(seed))//',"last_outcome":"'// &
            trim(last_outcome)//'","last_exitcode":'// &
            trim(json_int(last_exitcode))//'}'
        call gremlin_session_publish(session, trim(status_text), status, local_message)
        if (present(ierr)) ierr = status
        if (present(message)) message = local_message
        if (present(status_text_out)) status_text_out = trim(status_text)
    end subroutine publish_state

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
        integer(int64) :: next_cursor, file_size
        integer :: journal_status, i, emitted
        logical :: exists, has_more
        character(len=16) :: row_status
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
                call extract_json_field(records(i)%json, 'status', row_status)
                if (.not. status_is_failure(trim(row_status))) cycle
            end if
            if (emitted > 0) response = response//','
            response = response//records(i)%json
            emitted = emitted + 1
        end do
        response = response//'],"next_cursor":'//trim(int64_text(next_cursor))// &
            ',"has_more":'//trim(json_bool(has_more))//'}'
        ierr = 0
        message = ''
    end subroutine response_with_events

    subroutine event_page_has_failure(session, request, failed, has_events, &
            next_cursor, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        logical, intent(out) :: failed, has_events
        integer(int64), intent(out) :: next_cursor
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        integer :: journal_status, i
        character(len=16) :: row_status

        failed = .false.
        has_events = .false.
        next_cursor = request%cursor
        call journal_read_page(session%state_dir//'/journal.jsonl', request%cursor, &
            request%max_records, request%max_bytes, records, next_cursor, &
            journal_status, message)
        ierr = journal_status
        if (ierr == JOURNAL_OK) then
            has_events = size(records) > 0
            do i = 1, size(records)
                call extract_json_field(records(i)%json, 'status', row_status)
                if (status_is_failure(trim(row_status))) failed = .true.
            end do
        end if
    end subroutine event_page_has_failure

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
            status == 'TIMEOUT' .or. status == 'INFRA_ERROR'
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

    subroutine find_self_executable(path, ok)
        character(len=*), intent(out) :: path
        logical, intent(out) :: ok

        character(len=PATH_LEN) :: argument
        integer :: status, length

        argument = ''
        call get_command_argument(0, argument, length, status)
        ok = .false.
        if (status == 0 .and. length > 0 .and. length <= len(argument)) then
            call fs_find_executable(trim(argument), path, ok)
            if (ok) return
        end if
        call fs_find_executable('fo', path, ok)
    end subroutine find_self_executable

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
