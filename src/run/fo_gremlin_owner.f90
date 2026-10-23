module fo_gremlin_owner
    !! The Gremlin lane owner: one process that captures the watched project
    !! into immutable generations, builds each candidate once, runs campaign
    !! cases of the active generation in parallel children, journals their
    !! receipts and publishes the lane state. All owner state is one owner_t;
    !! run_owner is its event loop over the build child, the case children
    !! and the input watcher.
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use fo_cache, only: HASH_LEN, cache_store_root
    use fo_change_watch, only: change_watch_t, change_watch_init, change_watch_poll, &
        change_watch_close
    use fo_driver, only: driver_pin_t, driver_pin_current
    use fo_exec_service, only: exec_job_t, exec_start, exec_poll, exec_cancel, &
        exec_clock_ms
    use fo_fs, only: fs_make_dir, fs_sleep_ms, fs_remove_tree
    use fo_gremlin_campaign, only: discover_campaign, compute_generation_impact, &
        generation_model_adopt, generation_model_expand, active_model_identity, &
        next_campaign_seed, case_wall_timeout, build_command, case_command, &
        append_execution_provenance, acquire_heavy_work_slot, release_heavy_work_slot, &
        gremlin_host_jobs, gremlin_lane_jobs, GREMLIN_MAX_LANE_JOBS, &
        release_generation_lease, request_policy_key, push_scope_args, log_path, &
        gremlin_attach_response
    use fo_gremlin_context, only: capture_candidate
    use fo_gremlin_coverage, only: coverage_read_view_path, coverage_record_path, &
        COVERAGE_OK, COVERAGE_NOT_FOUND
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release
    use fo_gremlin_generation, only: generation_t
    use fo_gremlin_journal, only: JOURNAL_OK
    use fo_gremlin_lifecycle, only: gremlin_freshness_update, &
        gremlin_lifecycle_event_t, gremlin_lifecycle_append, &
        gremlin_lifecycle_read_page, gremlin_lifecycle_make_id, LIFECYCLE_OK, &
        LIFECYCLE_INVALID, LIFECYCLE_MAX_EVENT
    use fo_gremlin_log_text, only: gremlin_retain_build_log
    use fo_gremlin_readiness, only: gremlin_readiness_t, gremlin_phase_name, &
        gremlin_health_name, gremlin_verification_name, GREMLIN_PHASE_QUIESCENT, &
        GREMLIN_VERIFY_ORDINARY
    use fo_gremlin_receipts, only: NAME_LEN, PATH_LEN, record_receipt, &
        case_reuse_source, prior_generation_case_passed, compute_readiness, &
        lifecycle_path_for_session, coverage_state_path, runner_case_outcome
    use fo_gremlin_request, only: gremlin_request_t
    use fo_gremlin_retention, only: gremlin_retention_json, gremlin_trim_cache
    use fo_gremlin_session, only: gremlin_recover_owner_journal, &
        gremlin_publish_terminal_snapshot, gremlin_release_stopped_session
    use fo_gremlin_state, only: gremlin_session_t, gremlin_lease_t, &
        gremlin_lease_group_t, gremlin_session_acquire, gremlin_session_publish, &
        gremlin_session_read, gremlin_session_release, gremlin_session_stop_requested, &
        gremlin_session_bind_task, &
        gremlin_lease_release, gremlin_generation_lease_acquire_at, &
        gremlin_generation_pin_at, gremlin_generation_prune_inactive, &
        GREMLIN_STATE_TEXT_MAX
    use fo_gremlin_status_json, only: gremlin_status_json, gremlin_simple_response, &
        gremlin_error_response
    use fo_process, only: process_reap_adopted_scope_zombies, process_set_async_scope
    use fo_util, only: int_text
    use fx_action_result_store, only: action_result_store_t, &
        action_result_store_init, action_result_maintenance_tick, ACTION_RESULT_OK
    use fx_dag, only: MAX_NODES
    use fx_immutable_root_compact, only: immutable_store_compact_generation_roots
    use fx_immutable_store, only: IMMUTABLE_OK
    implicit none
    private

    public :: run_owner

    type :: child_t
        !! One owned child process tree: the candidate build or a campaign case.
        type(exec_job_t) :: job = exec_job_t()
        type(execution_view_t) :: execution_view = execution_view_t()
        character(len=NAME_LEN) :: case_name = ''
        integer :: case_index = 0
        logical :: gate_required = .false.
        !! Not launched: an earlier generation's PASS receipt with the same
        !! case input closure digest is reused (see case_reuse_source).
        logical :: reused = .false.
    end type child_t

    type :: owner_t
        character(len=:), allocatable :: project_dir
        type(gremlin_session_t) :: session
        type(driver_pin_t) :: pin
        type(gremlin_request_t), allocatable :: request
        type(change_watch_t) :: watch
        !! The active generation under test and the newest captured candidate,
        !! each with its generation lease and retention pin.
        type(generation_t) :: active, candidate
        type(gremlin_lease_t) :: active_lease, candidate_lease
        logical :: have_active = .false., have_candidate = .false.
        logical :: have_active_lease = .false., have_candidate_lease = .false.
        logical :: active_pinned = .false., candidate_pinned = .false.
        !! Identity of the newest candidate that failed to build or launch.
        character(len=PATH_LEN) :: last_failed = ''
        !! Host work-slot admission of the lane and its case fan-out.
        type(gremlin_lease_group_t) :: heavy
        logical :: have_heavy = .false.
        integer :: lane_jobs = 1
        !! In-flight children.
        type(child_t) :: build
        type(child_t) :: tests(GREMLIN_MAX_LANE_JOBS)
        !! Campaign cursor: the selection, its position, seed and number, the
        !! completed cases and the receipt sequence of this owner.
        character(len=NAME_LEN), allocatable :: selected(:)
        integer :: n_selected = 0, index = 0, seed = 0, number = 0
        integer :: completed = 0, sequence = 0
        integer(int64) :: campaign_started_ms = 0_int64
        !! Loop state: the lane state name, capture debounce and maintenance.
        character(len=32) :: state = 'starting'
        integer(int64) :: debounce_ms = 0_int64, reaped_ms = 0_int64
        integer(int64) :: maintained_ms = 0_int64
        integer :: cache_cursor = 0
        logical :: cache_warned = .false., stop_requested = .false.
        logical :: failed = .false.
        character(len=PATH_LEN) :: message = '', fatal_message = ''
    end type owner_t

contains

    subroutine run_owner(project_dir, request, response, exitcode)
        !! Own the lane until a stop request or an evidence/process error.
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode
        type(owner_t), allocatable :: o
        integer :: ierr

        allocate (o)
        call owner_open(o, project_dir, request, response, exitcode)
        if (allocated(response)) return
        call capture_first(o)
        do while (.not. o%failed)
            call maintain_idle(o)
            if (o%failed) exit
            call gremlin_session_stop_requested(o%session, o%stop_requested, ierr, &
                o%message)
            if (ierr /= 0) call fail(o, o%message)
            if (o%failed .or. o%stop_requested) exit
            call poll_build(o)
            if (.not. o%failed) call poll_cases(o)
            if (.not. o%failed) call poll_inputs(o)
            if (.not. o%failed) call fs_sleep_ms(100)
        end do
        call change_watch_close(o%watch)
        if (o%failed) then
            call close_failed(o, response, exitcode)
        else
            call close_stopped(o, response, exitcode)
        end if
    end subroutine run_owner

    subroutine fail(o, message)
        type(owner_t), intent(inout) :: o
        character(len=*), intent(in) :: message

        o%failed = .true.
        o%fatal_message = message
    end subroutine fail

    subroutine owner_open(o, project_dir, request, response, exitcode)
        !! Acquire the lane (or attach to its live owner), take exact process
        !! ownership, recover prior receipts, pin the driver and publish the
        !! starting state. A response is set when the owner does not run.
        type(owner_t), intent(inout) :: o
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode
        integer :: ierr, status
        character(len=PATH_LEN) :: detail

        exitcode = 0
        o%project_dir = project_dir
        o%request = request
        allocate (o%selected(MAX_NODES))
        o%selected = ''
        o%lane_jobs = gremlin_lane_jobs(request)
        o%seed = request%seed
        if (o%seed == 0) then
            call system_clock(o%seed)
            if (o%seed == 0) o%seed = 1
        end if
        call gremlin_session_acquire(project_dir, trim(request%lane_id), o%session, &
            ierr, o%message)
        if (ierr /= 0) then
            call refuse(trim(o%message))
            return
        end if
        if (o%session%attached) then
            call gremlin_attach_response('run', project_dir, request, response, exitcode)
            return
        end if
        call process_set_async_scope(o%session%state_dir, o%session%owner_pid, &
            o%session%owner_start, ierr)
        if (ierr /= 0) then
            call gremlin_session_release(o%session, status, detail)
            if (status == 0) call refuse('cannot configure exact process ownership: '// &
                int_text(ierr))
            if (status /= 0) call refuse('cannot release owner after process '// &
                'ownership setup failure: '//trim(detail))
            return
        end if
        if (trim(request%lifetime) == 'task') then
            call bind_task_client(o%session, ierr, o%message)
            if (ierr /= 0) then
                call release_if_owner(o%session)
                call refuse('cannot bind the task-owned lane to its client: '// &
                    trim(o%message))
                return
            end if
        end if
        call maintain_fo_cache(o%cache_cursor, status)
        if (status /= 0) then
            write (error_unit, '(a,i0)') 'Gremlin cache maintenance: ', status
            o%cache_warned = .true.
        end if
        o%maintained_ms = exec_clock_ms()
        call gremlin_recover_owner_journal(project_dir, o%session, ierr, o%message)
        if (ierr /= 0) then
            call release_if_owner(o%session)
            call refuse('cannot recover prior Gremlin receipts: '//trim(o%message))
            return
        end if
        call driver_pin_current(o%session%state_dir, o%pin, ierr, o%message)
        if (ierr /= 0) then
            call release_if_owner(o%session)
            call refuse('cannot pin the running fo driver: '//trim(o%message))
            return
        end if
        o%session%driver_path = o%pin%path
        o%session%driver_digest = o%pin%digest
        o%session%driver_size = o%pin%size
        call fs_make_dir(o%session%state_dir//'/logs')
        call publish(o, 'starting', '', 'NONE', 0, ierr, o%message)
        if (ierr /= 0) then
            detail = o%message
            call gremlin_session_release(o%session, status, o%message)
            if (status /= 0) detail = o%message
            call refuse('cannot publish initial Gremlin state: '//trim(detail))
            return
        end if
        call change_watch_init(o%watch, project_dir, ierr, o%message)
        if (ierr /= 0) then
            call change_watch_close(o%watch)
            call release_if_owner(o%session)
            call refuse('cannot start Gremlin change watcher: '//trim(o%message))
        end if

    contains

        subroutine refuse(text)
            character(len=*), intent(in) :: text

            call gremlin_error_response('run', text, response)
            exitcode = 2
        end subroutine refuse

    end subroutine owner_open

    subroutine bind_task_client(session, ierr, message)
        !! The launcher names the creating client in the environment; the lane
        !! stops (see gremlin_session_stop_requested) once that process is gone.
        type(gremlin_session_t), intent(in) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=64) :: pid_text, start
        integer :: pid, ios

        pid_text = ''
        start = ''
        call get_environment_variable('FO_GREMLIN_TASK_PID', pid_text)
        call get_environment_variable('FO_GREMLIN_TASK_START', start)
        read (pid_text, *, iostat=ios) pid
        if (ios /= 0 .or. len_trim(start) == 0) then
            ierr = 1
            message = 'the launcher did not identify the client process'
            return
        end if
        call gremlin_session_bind_task(session, pid, trim(start), ierr, message)
    end subroutine bind_task_client

    subroutine release_if_owner(session)
        type(gremlin_session_t), intent(inout) :: session
        integer :: status
        character(len=PATH_LEN) :: detail

        if (session%owner) call gremlin_session_release(session, status, detail)
    end subroutine release_if_owner

    subroutine capture_first(o)
        !! Capture the first candidate and start its build.
        type(owner_t), intent(inout) :: o
        integer :: registration_error, ierr
        logical :: ok

        call capture_candidate(o%project_dir, o%pin, o%candidate, o%candidate_lease, &
            ok, registration_error, o%message, change_watch=o%watch)
        if (registration_error /= 0) then
            call fail(o, 'cannot register captured generation: '//trim(o%message))
        else if (.not. ok) then
            call publish(o, 'capture_failed', '', 'NONE', 0, ierr, o%message, &
                diagnostic=trim(o%message), seed=0)
            if (ierr /= 0) call fail(o, o%message)
        else
            o%have_candidate = .true.
            o%have_candidate_lease = .true.
            call admit(o, 'starting', o%candidate, ierr)
            if (ierr /= 0) then
                call fail(o, o%message)
            else if (.not. o%stop_requested) then
                call launch_build(o)
            end if
        end if
    end subroutine capture_first

    subroutine admit(o, waiting_state, candidate, ierr)
        !! Take the lane's host work slot, publishing waiting_state while other
        !! lanes hold it; a stop request ends the wait (o%stop_requested).
        type(owner_t), intent(inout) :: o
        character(len=*), intent(in) :: waiting_state
        type(generation_t), intent(in) :: candidate
        integer, intent(out) :: ierr
        logical :: busy

        call acquire_heavy_work_slot(o%session, o%lane_jobs, o%heavy, o%have_heavy, &
            .true., o%stop_requested, ierr, o%message, busy)
        if (ierr /= 0 .or. .not. busy) return
        call publish(o, waiting_state, current_case(o), 'NONE', 0, &
            diagnostic='waiting for a host Gremlin work slot held by other lanes', &
            candidate=candidate)
        call acquire_heavy_work_slot(o%session, o%lane_jobs, o%heavy, o%have_heavy, &
            .true., o%stop_requested, ierr, o%message)
    end subroutine admit

    subroutine launch_build(o)
        !! Start the candidate's build; a launch failure is an INFRA_ERROR
        !! build receipt and ends the owner.
        type(owner_t), intent(inout) :: o
        integer :: ierr, status

        call start_build(o, ierr)
        if (ierr == 0) then
            call publish(o, 'building', current_case(o), 'NONE', 0, ierr, o%message)
            if (ierr /= 0) call fail(o, o%message)
            if (ierr == 0) o%state = 'building'
            return
        end if
        call fail(o, 'cannot start initial candidate build (process setup error '// &
            int_text(ierr)//')')
        o%last_failed = o%candidate%identity
        o%have_candidate = .false.
        call publish(o, 'build_failed', current_case(o), 'INFRA_ERROR', ierr, status, &
            o%message)
        if (status == 0) then
            o%sequence = o%sequence + 1
            call record_receipt(o%session, o%request, o%candidate, '<build>', ierr, &
                'INFRA_ERROR', o%sequence, 0, o%seed, o%build%job%log_file, status, &
                o%message, credit_coverage=.false.)
        end if
        if (status /= 0) o%fatal_message = o%message
        call release_generation_lease(o%candidate_lease, o%have_candidate_lease, &
            status, o%message)
        if (status /= 0) o%fatal_message = 'cannot release failed candidate lease: '// &
            trim(o%message)
    end subroutine launch_build

    logical function driver_matches(o, generation)
        !! The generation was captured with the pinned driver, which exists.
        type(owner_t), intent(in) :: o
        type(generation_t), intent(in) :: generation

        driver_matches = generation%driver_digest == o%session%driver_digest .and. &
            generation%driver_size == o%session%driver_size .and. &
            len_trim(o%session%driver_path) > 0
    end function driver_matches

    subroutine start_build(o, ierr)
        !! Build the candidate once for its campaigns: a focused campaign
        !! (explicit targets, no random cases) builds what its cases need,
        !! any other campaign what every inventory case needs.
        type(owner_t), intent(inout) :: o
        integer, intent(out) :: ierr
        character(len=PATH_LEN) :: log_file, detail
        character(len=:), allocatable :: packed, env
        integer :: n_args, status
        logical :: exists

        o%build = child_t()
        ierr = 1
        if (.not. driver_matches(o, o%candidate)) then
            o%message = 'candidate generation does not match the pinned fo driver'
            return
        end if
        inquire (file=trim(o%session%driver_path), exist=exists)
        if (.not. exists) then
            o%message = 'pinned fo driver is missing; refusing to launch candidate build'
            return
        end if
        o%candidate%build_project_root = ''
        call execution_view_create(o%session%state_dir//'/views', &
            o%candidate%identity, o%session%session_id//'-build', 'build', &
            o%candidate%input_inventory, o%candidate%input_inventory_ready, &
            o%candidate%input_inventory_complete, o%build%execution_view, ierr, &
            o%message, candidate_bundle_root=trim(o%candidate%root)//'/bundle')
        if (ierr /= 0) then
            o%message = 'cannot create candidate build view: '//trim(o%message)
            return
        end if
        o%candidate%build_project_root = o%build%execution_view%cwd
        call log_path(o%session, 'build-'//o%candidate%identity(1:16), log_file)
        if (o%request%random_count == 0) then
            call build_command(o%session%driver_path, o%build%execution_view, packed, &
                n_args, env, o%request%targets(:o%request%n_targets))
        else
            call build_command(o%session%driver_path, o%build%execution_view, packed, &
                n_args, env)
        end if
        ! A scoped lane builds the plan targets of its scope only.
        call push_scope_args(packed, n_args, o%request)
        call exec_start(o%build%job, trim(o%build%execution_view%cwd), packed, n_args, &
            trim(log_file), ierr, env)
        if (ierr /= 0) then
            o%message = 'cannot start candidate build'
            call execution_view_release(o%build%execution_view, .false., status, detail)
            o%candidate%build_project_root = ''
        end if
    end subroutine start_build

    subroutine maintain_idle(o)
        !! With no child running: reap adopted zombies and tick the cache.
        type(owner_t), intent(inout) :: o
        integer(int64) :: now_ms
        integer :: status

        if (o%build%job%pid > 0 .or. running_count(o) > 0) return
        now_ms = exec_clock_ms()
        if (now_ms == 0_int64 .or. now_ms - o%reaped_ms >= 1000_int64) then
            call process_reap_adopted_scope_zombies(status)
            if (status /= 0) then
                call fail(o, 'cannot reap adopted Gremlin children ('// &
                    int_text(status)//')')
                return
            end if
            o%reaped_ms = now_ms
        end if
        if (o%state /= 'quiescent' .and. now_ms - o%maintained_ms >= 5000_int64) then
            call maintain_fo_cache(o%cache_cursor, status)
            if (status /= 0 .and. .not. o%cache_warned) &
                write (error_unit, '(a,i0)') 'Gremlin cache maintenance: ', status
            o%cache_warned = status /= 0
            o%maintained_ms = now_ms
        end if
    end subroutine maintain_idle

    subroutine poll_build(o)
        !! A finished candidate build completes; a successful one starts the
        !! new generation's campaign.
        type(owner_t), intent(inout) :: o
        integer :: ierr, status, build_exit
        logical :: done
        character(len=PATH_LEN) :: detail

        if (o%build%job%pid <= 0) return
        call exec_poll(o%build%job, done, ierr)
        if (.not. done) return
        build_exit = o%build%job%exitcode
        call complete_build(o, build_exit, ierr)
        if (ierr /= 0) then
            if (o%build%execution_view%active) call execution_view_release( &
                o%build%execution_view, .false., status, detail)
            if (o%candidate%build_project_root == o%build%execution_view%cwd) &
                o%candidate%build_project_root = ''
            call fail(o, o%message)
            return
        end if
        if (build_exit == 0 .and. o%have_active .and. o%n_selected > 0) then
            o%number = o%number + 1
            o%index = 0
            o%campaign_started_ms = exec_clock_ms()
            call advance_campaign(o, .true., ierr)
            if (ierr /= 0) call fail(o, o%message)
        end if
        o%have_candidate = .false.
    end subroutine poll_build

    subroutine poll_cases(o)
        !! Each finished (or timed-out) case gets its receipt, in completion
        !! order, and frees its slot for the next dispatch.
        type(owner_t), intent(inout) :: o
        integer :: slot, ierr, exitcode, status
        logical :: done
        character(len=16) :: outcome
        character(len=NAME_LEN) :: finished
        character(len=PATH_LEN) :: detail

        do slot = 1, GREMLIN_MAX_LANE_JOBS
            if (o%tests(slot)%job%pid <= 0) cycle
            call exec_poll(o%tests(slot)%job, done, ierr)
            if (ierr /= 0) then
                call fail(o, 'cannot cancel timed-out Gremlin test process')
                return
            end if
            if (.not. done) cycle
            exitcode = o%tests(slot)%job%exitcode
            call append_execution_provenance(o%tests(slot)%job%log_file, &
                o%tests(slot)%execution_view)
            if (o%tests(slot)%job%timed_out) then
                outcome = 'TIMEOUT'
            else
                o%state = 'testing'
                outcome = runner_case_outcome(o%tests(slot)%job%log_file, &
                    o%tests(slot)%case_name)
            end if
            if (outcome == 'SKIP' .and. exitcode /= 0) outcome = 'INFRA_ERROR'
            call execution_view_release(o%tests(slot)%execution_view, &
                exitcode /= 0 .or. (trim(outcome) /= 'PASS' .and. &
                trim(outcome) /= 'SKIP'), status, detail)
            o%sequence = o%sequence + 1
            call record_receipt(o%session, o%request, o%active, &
                o%tests(slot)%case_name, exitcode, trim(outcome), o%sequence, &
                o%tests(slot)%case_index, o%seed, o%tests(slot)%job%log_file, ierr, &
                o%message, gate_required=o%tests(slot)%gate_required)
            finished = o%tests(slot)%case_name
            o%completed = o%completed + 1
            if (ierr == 0) call publish(o, o%state, finished, outcome, exitcode, ierr, &
                o%message)
            if (ierr == 0) call advance_campaign(o, .false., ierr)
            if (ierr /= 0) then
                call fail(o, o%message)
                return
            end if
        end do
    end subroutine poll_cases

    subroutine poll_inputs(o)
        !! Watch the project, capture a changed one into a new candidate and
        !! answer pending freshness requests once the provider is quiet.
        type(owner_t), intent(inout) :: o
        character(len=PATH_LEN) :: freshness_path, detail
        integer(int64) :: ticket
        integer :: ierr, status
        logical :: capture_failed, quiet

        ! Snapshot consumers before the provider poll: a request arriving
        ! after this point must wait for the next poll, never an older one.
        freshness_path = o%session%state_dir//'/freshness-'//o%session%session_id
        ticket = 0_int64
        call gremlin_freshness_update(trim(freshness_path), 0, ticket, status)
        if (status /= 0) ticket = 0_int64
        call capture_latest(o, capture_failed, quiet, ierr)
        if (ierr /= 0) then
            call fail(o, o%message)
            return
        end if
        if (capture_failed) then
            o%state = 'capture_failed'
            call publish(o, 'capture_failed', current_case(o), 'NONE', 1, status, &
                detail, diagnostic=trim(o%message))
            if (status /= 0) then
                call fail(o, 'cannot publish capture failure: '//trim(detail))
                return
            end if
        end if
        if (o%build%job%pid <= 0 .and. running_count(o) == 0 .and. o%have_heavy) then
            call release_heavy_work_slot(o%heavy, o%have_heavy, ierr, o%message)
            if (ierr /= 0) then
                call fail(o, o%message)
                return
            end if
        end if
        if (ticket > 0_int64 .and. (o%request%input_changed .or. quiet)) &
            call gremlin_freshness_update(trim(freshness_path), 2, ticket, status)
    end subroutine poll_inputs

    subroutine capture_latest(o, capture_failed, provider_quiet, ierr)
        !! A changed project is debounced, captured and, when it is a new
        !! generation, supersedes the candidate (cancelling its build) and
        !! starts its build. An unchanged capture resumes the lane state.
        type(owner_t), intent(inout) :: o
        logical, intent(out) :: capture_failed, provider_quiet
        integer, intent(out) :: ierr
        type(generation_t) :: current
        type(gremlin_lease_t) :: current_lease
        character(len=PATH_LEN) :: changed_path, detail
        character(len=16) :: resumed
        logical :: ok, got_event
        integer(int64) :: now_ms
        integer :: status, event_type

        ierr = 0
        o%message = ''
        capture_failed = .false.
        ! A zero-timeout provider poll deliberately cannot finish a pending
        ! structural reconciliation. Give the watcher a bounded tick so its
        ! quiet deadline can expire while the owner loop remains responsive.
        call change_watch_poll(o%watch, 1, changed_path, event_type, got_event, ierr, &
            provider_quiet)
        if (ierr /= 0) then
            o%message = 'Gremlin change watcher reconciliation failed'
            return
        end if
        now_ms = exec_clock_ms()
        if (got_event) then
            o%request%event_epoch = o%request%event_epoch + 1
            o%request%input_changed = .true.
            call publish(o, 'capture_pending', current_case(o), 'NONE', 0, ierr, &
                o%message)
            o%debounce_ms = now_ms + 250_int64
            return
        end if
        if (o%debounce_ms <= 0_int64 .or. now_ms < o%debounce_ms) return
        o%debounce_ms = 0_int64
        if (o%have_candidate) then
            call capture_candidate(o%project_dir, o%pin, current, current_lease, ok, &
                ierr, o%message, o%watch, o%candidate%input_inventory)
        else if (o%have_active) then
            call capture_candidate(o%project_dir, o%pin, current, current_lease, ok, &
                ierr, o%message, o%watch, o%active%input_inventory)
        else
            call capture_candidate(o%project_dir, o%pin, current, current_lease, ok, &
                ierr, o%message, o%watch)
        end if
        if (ierr /= 0) then
            o%message = 'cannot register captured generation: '//trim(o%message)
            return
        end if
        if (.not. ok) then
            capture_failed = .true.
            return
        end if
        if (o%have_candidate) then
            if (current%identity == o%candidate%identity) then
                call gremlin_lease_release(current_lease, ierr, o%message)
                return
            end if
            if (o%build%job%pid > 0) then
                call exec_cancel(o%build%job, ierr)
                if (ierr /= 0) then
                    o%message = 'cannot cancel superseded candidate build process'
                    return
                end if
                call execution_view_release(o%build%execution_view, .false., status, &
                    detail)
            end if
            call release_generation_lease(o%candidate_lease, o%have_candidate_lease, &
                ierr, o%message)
            if (ierr /= 0) then
                o%message = 'cannot release superseded candidate generation lease'
                return
            end if
            o%have_candidate = .false.
        end if
        if (o%have_active .and. current%identity == o%active%identity) then
            call gremlin_lease_release(current_lease, ierr, o%message)
            if (ierr /= 0) return
            o%request%input_changed = .false.
            ! The edit left the generation unchanged: resume the state the
            ! lane slept in instead of waking it into an endless testing phase.
            resumed = 'testing'
            if (trim(o%state) == 'quiescent') resumed = 'quiescent'
            call publish(o, trim(resumed), current_case(o), 'NONE', 0, ierr, o%message, &
                candidate=current)
            return
        end if
        if (current%identity == o%last_failed) then
            call gremlin_lease_release(current_lease, ierr, o%message)
            return
        end if
        call admit(o, 'capture_pending', current, ierr)
        if (ierr /= 0 .or. o%stop_requested) then
            call gremlin_lease_release(current_lease, status, detail)
            if (ierr == 0) ierr = status
            return
        end if
        o%candidate = current
        o%have_candidate = .true.
        o%candidate_lease = current_lease
        o%have_candidate_lease = .true.
        call start_build(o, status)
        if (status == 0) then
            call publish(o, 'building', current_case(o), 'NONE', 0, ierr, detail)
            if (ierr /= 0) o%message = 'cannot publish candidate build start: '// &
                trim(detail)
            return
        end if
        o%have_candidate = .false.
        o%last_failed = o%candidate%identity
        o%sequence = o%sequence + 1
        call record_receipt(o%session, o%request, o%candidate, '<build>', status, &
            'INFRA_ERROR', o%sequence, 0, o%request%seed, o%build%job%log_file, ierr, &
            o%message)
        if (ierr /= JOURNAL_OK) return
        call release_generation_lease(o%candidate_lease, o%have_candidate_lease, ierr, &
            o%message)
        if (ierr /= 0) then
            o%message = 'cannot release failed candidate generation lease'
            return
        end if
        ierr = status
        o%message = 'cannot start candidate build'
    end subroutine capture_latest

    subroutine complete_build(o, build_exit, ierr)
        !! A finished candidate build: a failure keeps the active generation
        !! testing; a success supersedes it (cancelling its in-flight cases)
        !! and selects the new generation's campaign.
        type(owner_t), intent(inout) :: o
        integer, intent(in) :: build_exit
        integer, intent(out) :: ierr
        type(gremlin_lease_t) :: new_lease
        type(gremlin_request_t), allocatable :: selection
        character(len=PATH_LEN) :: detail
        integer :: status, mandatory_count
        logical :: was_active

        ierr = 0
        o%message = ''
        was_active = o%have_active
        o%sequence = o%sequence + 1
        if (build_exit == 0) then
            call record_receipt(o%session, o%request, o%candidate, '<build>', build_exit, &
                'BUILD_PASS', o%sequence, 0, o%request%seed, o%build%job%log_file, ierr, &
                o%message)
        else
            call record_receipt(o%session, o%request, o%candidate, '<build>', build_exit, &
                'BUILD_FAIL', o%sequence, 0, o%request%seed, o%build%job%log_file, ierr, &
                o%message)
        end if
        if (ierr /= 0) return
        if (build_exit /= 0) then
            call gremlin_retain_build_log(o%build%job%log_file)
            if (o%build%execution_view%active) call execution_view_release( &
                o%build%execution_view, .false., status, detail)
            o%candidate%build_project_root = ''
            o%last_failed = o%candidate%identity
            o%state = 'build_failed'
            call publish(o, 'build_failed', first_running_case(o), 'BUILD_FAIL', &
                build_exit, ierr, o%message)
            if (ierr /= 0) return
            call release_generation_lease(o%candidate_lease, o%have_candidate_lease, &
                ierr, detail)
            if (ierr /= 0) o%message = 'cannot release failed candidate generation '// &
                'lease: '//trim(detail)
            return
        end if
        ! The completed build keeps its CWD for campaign artifacts, but no
        ! process still owns its disposable scratch after successful completion.
        call execution_view_release(o%build%execution_view, .true., ierr, detail)
        if (ierr /= 0) then
            o%message = 'cannot release completed build scratch: '//trim(detail)
            return
        end if
        if (allocated(o%request%impact_cases)) deallocate (o%request%impact_cases)
        o%request%n_impact_cases = 0
        o%request%impact_all = .false.
        call gremlin_generation_lease_acquire_at(o%candidate%root, new_lease, ierr, &
            o%message)
        if (ierr /= 0) then
            o%message = 'cannot lease successfully built generation: '//trim(o%message)
            return
        end if
        call gremlin_generation_pin_at(o%candidate%root, .true., ierr, o%message)
        if (ierr /= 0) then
            o%message = 'cannot preserve successfully built generation: '// &
                trim(o%message)
            call gremlin_lease_release(new_lease, status, detail)
            return
        end if
        o%candidate_pinned = .true.
        ! Supersession: every in-flight child of the old generation goes.
        if (running_count(o) > 0) call cancel_cases(o, ierr, o%message)
        if (ierr /= 0) then
            o%message = 'cannot cancel obsolete generation test process'
        else if (o%active_pinned) then
            call gremlin_generation_pin_at(o%active%root, .false., ierr, o%message)
            if (ierr /= 0) then
                o%message = 'cannot release previous generation pin: '//trim(o%message)
            else
                o%active_pinned = .false.
            end if
        end if
        if (ierr == 0 .and. o%have_active_lease) then
            call gremlin_lease_release(o%active_lease, ierr, detail)
            if (ierr /= 0) then
                o%message = 'cannot release previous generation lease: '//trim(detail)
                call gremlin_generation_pin_at(o%active%root, .true., status, detail)
                if (status == 0) o%active_pinned = .true.
            else
                o%have_active_lease = .false.
            end if
        end if
        if (ierr /= 0) then
            call gremlin_generation_pin_at(o%candidate%root, .false., status, detail)
            if (status == 0) o%candidate_pinned = .false.
            call gremlin_lease_release(new_lease, status, detail)
            return
        end if
        o%active_lease = new_lease
        o%have_active_lease = .true.
        o%candidate%build_project_root = o%build%execution_view%cwd
        if (was_active .and. len_trim(o%active%build_project_root) > 0) &
            call fs_remove_tree(trim(o%active%build_project_root))
        o%request%has_previous_generation = was_active
        o%request%requirement_digest = ''
        o%request%gate_cases = ''
        if (was_active) then
            call generation_model_adopt(o%active, o%candidate)
            call generation_model_expand(o%candidate)
            ! Impact on the candidate's model and its built view.
            call compute_generation_impact(o%active, o%candidate, o%request)
        else if (allocated(o%candidate%model)) then
            deallocate (o%candidate%model)
        end if
        o%active = o%candidate
        o%have_active = .true.
        o%request%input_changed = .false.
        o%request%gate_required_count = 0
        o%active_pinned = .true.
        o%candidate_pinned = .false.
        call release_generation_lease(o%candidate_lease, o%have_candidate_lease, ierr, &
            detail)
        if (ierr /= 0) then
            o%message = 'cannot release candidate generation lease: '//trim(detail)
            return
        end if
        o%last_failed = ''
        if (was_active) o%seed = next_campaign_seed(o%seed)
        allocate (selection)
        selection = o%request
        selection%seed = o%seed
        call discover_campaign(o%active, o%session, selection, o%selected, o%n_selected, &
            mandatory_count, o%seed, status, o%message, live_dir=o%project_dir)
        o%request%requirement_digest = selection%requirement_digest
        o%request%gate_cases = selection%gate_cases
        o%request%gate_required_count = selection%gate_required_count
        if (status /= 0) then
            o%state = 'inventory_failed'
            o%n_selected = 0
            detail = o%message
            if (len_trim(detail) == 0) detail = 'discovery returned an unspecified error'
            call publish(o, 'inventory_failed', '', 'INFRA_ERROR', status, ierr, &
                o%message, diagnostic=trim(detail))
            if (ierr == 0) then
                ierr = status
                o%message = 'cannot discover the frozen test inventory: '//trim(detail)
            end if
            return
        end if
        o%state = 'testing'
        if (o%n_selected == 0) call idle_state(o, ierr)
        if (ierr /= 0) return
        call publish(o, o%state, current_case(o), 'NONE', 0, ierr, o%message)
        if (ierr == 0 .and. was_active) then
            call gremlin_generation_prune_inactive(o%active%root, status, detail)
            if (status /= 0) write (error_unit, '(a)') &
                'Gremlin generation cleanup: '//trim(detail)
        end if
    end subroutine complete_build

    subroutine idle_state(o, ierr)
        !! The state of a lane with nothing selected: quiescent once its gate
        !! is required (and green under random cases), idle otherwise.
        type(owner_t), intent(inout) :: o
        integer, intent(out) :: ierr
        type(gremlin_coverage_view_t) :: view

        ierr = 0
        o%state = 'idle'
        if (o%request%random_count == 0 .and. o%request%gate_required_count > 0) &
            o%state = 'quiescent'
        call coverage_read_view_path(coverage_state_path(o%session, o%active%identity), &
            o%active%identity, view, ierr, o%message)
        if (ierr == COVERAGE_OK) then
            if (view%green .and. o%request%gate_required_count > 0) o%state = 'quiescent'
        else if (ierr == COVERAGE_NOT_FOUND) then
            ierr = 0
        end if
    end subroutine idle_state

    subroutine advance_campaign(o, first, ierr)
        !! Dispatch cases in selection order until lane_jobs children run or
        !! nothing is left to dispatch now; a reused case completes at once.
        !! first: the campaign's first dispatch (no gate check, no rotation).
        type(owner_t), intent(inout) :: o
        logical, intent(in) :: first
        integer, intent(out) :: ierr
        logical :: started, reused, first_dispatch

        first_dispatch = first
        do
            call advance_step(o, first_dispatch, started, reused, ierr)
            first_dispatch = .false.
            if (ierr /= 0) return
            if (reused) then
                o%completed = o%completed + 1
            else if (.not. started .or. running_count(o) >= o%lane_jobs) then
                return
            end if
        end do
    end subroutine advance_campaign

    subroutine advance_step(o, first, started, reused, ierr)
        !! One dispatch: quiesce a green zero-random gate, advance or rotate
        !! the campaign, then launch (or reuse) the next case.
        type(owner_t), intent(inout) :: o
        logical, intent(in) :: first
        logical, intent(out) :: started, reused
        integer, intent(out) :: ierr
        type(gremlin_request_t) :: next_request
        type(gremlin_coverage_view_t) :: view
        type(gremlin_readiness_t) :: readiness
        character(len=PATH_LEN) :: launch_message, detail
        character(len=128) :: active_session, owner_start
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        integer :: slot, in_flight, status, mandatory_count, owner_pid

        ierr = 0
        o%message = ''
        started = .false.
        reused = .false.
        in_flight = running_count(o)
        if (in_flight >= o%lane_jobs) return
        if (first) then
            o%index = 1
        else
            if (o%request%random_count == 0) then
                call gremlin_session_read(o%project_dir, o%request%lane_id, &
                    active_session, owner_pid, owner_start, status_text, ierr, o%message)
                if (ierr /= 0) return
                if (trim(active_session) /= trim(o%session%session_id)) then
                    ierr = 1
                    o%message = 'Gremlin owner identity changed before gate completion'
                    return
                end if
                call coverage_read_view_path(coverage_state_path(o%session, &
                    o%active%identity), o%active%identity, view, status, o%message)
                if (status == COVERAGE_OK) then
                    call compute_readiness(o%session, trim(status_text), view, .true., &
                        readiness, ierr, o%message)
                    if (ierr /= 0) return
                    ! Quiescence only when nothing is in flight: drain first.
                    if (readiness%local_gate_green .and. in_flight > 0) return
                    if (readiness%local_gate_green) then
                        o%selected = ''
                        o%n_selected = 0
                        o%state = 'quiescent'
                        call publish(o, 'quiescent', '', 'NONE', 0, ierr, o%message, &
                            candidate=o%active)
                        return
                    end if
                else if (status /= COVERAGE_NOT_FOUND) then
                    ierr = status
                    return
                end if
            end if
            if (o%index < o%n_selected .and. exec_clock_ms() - o%campaign_started_ms < &
                    int(o%request%campaign_seconds, int64)*1000_int64) then
                o%index = o%index + 1
            else if (in_flight > 0) then
                ! Rotate the campaign only when no child is in flight.
                return
            else
                o%seed = next_campaign_seed(o%seed)
                next_request = o%request
                next_request%seed = o%seed
                call discover_campaign(o%active, o%session, next_request, o%selected, &
                    o%n_selected, mandatory_count, o%seed, ierr, o%message)
                o%request%requirement_digest = next_request%requirement_digest
                o%request%gate_cases = next_request%gate_cases
                o%request%gate_required_count = next_request%gate_required_count
                if (ierr /= 0) then
                    o%state = 'inventory_failed'
                    call publish(o, 'inventory_failed', '', 'NONE', ierr, &
                        candidate=o%active)
                    return
                end if
                o%number = o%number + 1
                o%index = 1
                o%campaign_started_ms = exec_clock_ms()
            end if
        end if
        if (o%n_selected == 0) then
            call idle_state(o, ierr)
            if (ierr == 0) call publish(o, o%state, '', 'NONE', 0, ierr, o%message, &
                candidate=o%active)
            return
        end if
        o%state = 'testing'
        slot = 1
        do while (o%tests(slot)%job%pid > 0)
            slot = slot + 1
        end do
        call launch_case(o, slot, ierr)
        reused = o%tests(slot)%reused
        if (ierr == 0 .and. o%tests(slot)%job%pid > 0) then
            started = .true.
            call publish(o, 'testing', first_running_case(o), 'RUNNING', 0, ierr, &
                o%message, candidate=o%active)
        end if
        if (ierr /= 0) then
            launch_message = o%message
            call publish(o, 'test_launch_failed', current_case(o), 'INFRA_ERROR', ierr, &
                status, detail, candidate=o%active)
            o%message = launch_message
            if (status /= 0) then
                ierr = status
                o%message = detail
            end if
        end if
    end subroutine advance_step

    subroutine launch_case(o, slot, ierr)
        !! Run the selected case o%index in slot, or reuse an earlier PASS of
        !! the same input closure (an immediate receipt, no child).
        type(owner_t), intent(inout) :: o
        integer, intent(in) :: slot
        integer, intent(out) :: ierr
        character(len=NAME_LEN) :: name
        character(len=PATH_LEN) :: log_name, log_file, view_owner, detail
        character(len=HASH_LEN) :: reused_from
        character(len=:), allocatable :: packed, env
        integer :: n_args, spawn_error, status
        logical :: exists

        o%tests(slot) = child_t()
        ierr = 0
        o%message = ''
        if (o%index < 1 .or. o%index > o%n_selected) return
        name = o%selected(o%index)
        ierr = 1
        if (len_trim(o%active%build_project_root) == 0) then
            o%message = 'active generation has no candidate build view'
            return
        end if
        o%tests(slot)%gate_required = any(o%request%gate_cases(: &
            o%request%gate_required_count) == name)
        if (.not. driver_matches(o, o%active)) then
            o%message = 'test generation does not match the pinned fo driver'
            return
        end if
        inquire (file=trim(o%session%driver_path), exist=exists)
        if (.not. exists) then
            o%message = 'pinned fo driver is missing; refusing to launch test case'
            return
        end if
        call case_reuse_source(o%session, o%active, name, reused_from, ierr, o%message)
        if (ierr /= 0) return
        o%tests(slot)%case_name = name
        o%tests(slot)%case_index = o%index
        if (len_trim(reused_from) > 0) then
            ! Same input closure as a PASS of an earlier generation: no run.
            o%tests(slot)%reused = .true.
            o%sequence = o%sequence + 1
            call record_receipt(o%session, o%request, o%active, name, 0, 'PASS', &
                o%sequence, o%index, o%seed, '', ierr, o%message, &
                gate_required=o%tests(slot)%gate_required, reused_from=reused_from)
            return
        end if
        write (log_name, '(a,i0,a,i0,a)') 'case-', o%number, '-', o%index, '.log'
        call log_path(o%session, trim(log_name), log_file)
        write (view_owner, '(a,"-case-",i0,"-",i0)') o%session%session_id, o%number, &
            o%index
        call execution_view_create(o%session%state_dir//'/views', o%active%identity, &
            trim(view_owner), name, o%active%input_inventory, &
            o%active%input_inventory_ready, o%active%input_inventory_complete, &
            o%tests(slot)%execution_view, ierr, o%message)
        if (ierr /= 0) return
        call case_command(o%session%driver_path, o%active, name, &
            o%tests(slot)%execution_view, o%active%build_project_root, packed, n_args, env)
        ! Parallel cases split the host job budget; a serial lane keeps the
        ! children's own OpenMP default.
        if (o%lane_jobs > 1) env = env//';OMP_NUM_THREADS='// &
            int_text(max(1, gremlin_host_jobs()/o%lane_jobs))
        call exec_start(o%tests(slot)%job, trim(o%active%build_project_root), packed, &
            n_args, trim(log_file), spawn_error, env, case_wall_timeout( &
            o%active%project_root, name, o%request%timeout_seconds))
        if (spawn_error /= 0) then
            call execution_view_release(o%tests(slot)%execution_view, .false., status, &
                detail)
            o%sequence = o%sequence + 1
            call record_receipt(o%session, o%request, o%active, name, spawn_error, &
                'INFRA_ERROR', o%sequence, o%index, o%seed, log_file, ierr, o%message, &
                gate_required=o%tests(slot)%gate_required)
            if (ierr == JOURNAL_OK) then
                ierr = spawn_error
                o%message = 'cannot start selected test '//trim(name)
            end if
            return
        end if
        call coverage_record_path(coverage_state_path(o%session, o%active%identity), &
            o%active%identity, trim(name), 'RUNNING', ierr, o%message)
    end subroutine launch_case

    subroutine cancel_cases(o, ierr, message)
        !! Cancel every in-flight case; ierr is the first failure, the others
        !! are still attempted so no child outlives the generation.
        type(owner_t), intent(inout) :: o
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: i, status
        character(len=PATH_LEN) :: detail

        ierr = 0
        message = ''
        do i = 1, size(o%tests)
            if (o%tests(i)%job%pid <= 0) cycle
            call exec_cancel(o%tests(i)%job, status)
            detail = 'cannot cancel owned test process'
            if (status == 0) then
                call append_execution_provenance(o%tests(i)%job%log_file, &
                    o%tests(i)%execution_view)
                call execution_view_release(o%tests(i)%execution_view, .false., status, &
                    detail)
            end if
            if (status == 0) call coverage_record_path(coverage_state_path(o%session, &
                o%active%identity), o%active%identity, o%tests(i)%case_name, &
                'CANCELLED', status, detail)
            if (status /= 0 .and. ierr == 0) then
                ierr = status
                message = detail
            end if
        end do
    end subroutine cancel_cases

    subroutine cancel_children(o, ierr, message)
        !! Cancel the cases and the candidate build of this owner.
        type(owner_t), intent(inout) :: o
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: status
        character(len=PATH_LEN) :: detail

        call cancel_cases(o, ierr, message)
        if (o%build%job%pid <= 0) return
        call exec_cancel(o%build%job, status)
        if (status /= 0 .and. ierr == 0) ierr = status
        if (status /= 0) return
        if (o%build%execution_view%active) call execution_view_release( &
            o%build%execution_view, .false., status, detail)
        o%candidate%build_project_root = ''
    end subroutine cancel_children

    subroutine close_failed(o, response, exitcode)
        !! Evidence or process error: cancel and release everything, publish
        !! the error state and keep a terminal ownership record.
        type(owner_t), intent(inout) :: o
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode
        integer :: cancel_error, status, publish_error
        character(len=PATH_LEN) :: detail

        if (len_trim(o%fatal_message) == 0) o%fatal_message = o%message
        if (len_trim(o%fatal_message) == 0) &
            o%fatal_message = 'Gremlin stopped after an evidence or process error'
        call cancel_children(o, cancel_error, detail)
        if (cancel_error == 0) then
            if (o%candidate_pinned) then
                call gremlin_generation_pin_at(o%candidate%root, .false., status, detail)
                if (status == 0) o%candidate_pinned = .false.
                if (status /= 0) o%fatal_message = &
                    'cannot release failed candidate pin: '//trim(detail)
            end if
            call release_all_leases(o)
        else if (o%build%job%pid > 0 .and. len_trim(o%candidate%root) > 0) then
            call gremlin_generation_pin_at(o%candidate%root, .true., status, detail)
        end if
        call publish(o, 'error', current_case(o), 'INFRA_ERROR', 1, publish_error, &
            detail, diagnostic=trim(o%fatal_message))
        if (publish_error /= 0) o%fatal_message = detail
        if (cancel_error /= 0) o%fatal_message = 'process cancellation failed: '// &
            int_text(cancel_error)
        if (cancel_error == 0 .and. publish_error == 0) then
            call gremlin_publish_terminal_snapshot(o%project_dir, o%session, status, &
                detail)
            if (status == 0) call gremlin_session_release(o%session, status, detail)
            if (status /= 0) o%fatal_message = &
                'cannot preserve terminal Gremlin ownership record: '//trim(detail)
        end if
        call gremlin_error_response('run', trim(o%fatal_message), response)
        exitcode = 2
    end subroutine close_failed

    subroutine release_all_leases(o)
        !! Release the candidate, active and host work leases; the last failure
        !! becomes the fatal message.
        type(owner_t), intent(inout) :: o
        integer :: status
        character(len=PATH_LEN) :: detail

        call release_generation_lease(o%candidate_lease, o%have_candidate_lease, &
            status, detail)
        if (status /= 0) o%fatal_message = detail
        call release_generation_lease(o%active_lease, o%have_active_lease, status, detail)
        if (status /= 0) o%fatal_message = detail
        call release_heavy_work_slot(o%heavy, o%have_heavy, status, detail, drain=.true.)
        if (status /= 0) o%fatal_message = detail
    end subroutine release_all_leases

    subroutine close_stopped(o, response, exitcode)
        !! Stop request: cancel the children, release the leases and publish
        !! the stopped state; the generations stay for terminal reproduction.
        type(owner_t), intent(inout) :: o
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=PATH_LEN) :: detail
        character(len=16) :: outcome
        integer :: cancel_error, status, publish_error

        exitcode = 2
        o%stop_requested = o%stop_requested .or. running_count(o) > 0 .or. &
            o%build%job%pid > 0
        call cancel_children(o, cancel_error, detail)
        if (cancel_error /= 0) then
            call publish(o, 'error', current_case(o), 'INFRA_ERROR', cancel_error, &
                status, o%message)
            if (o%build%job%pid > 0 .and. len_trim(o%candidate%root) > 0) &
                call gremlin_generation_pin_at(o%candidate%root, .true., status, &
                    o%message)
            call gremlin_error_response('run', 'cannot cancel owned Gremlin work: '// &
                int_text(cancel_error)//'; '//trim(detail), response)
            return
        end if
        o%fatal_message = ''
        call release_all_leases(o)
        if (len_trim(o%fatal_message) > 0) then
            call publish(o, 'error', '', 'INFRA_ERROR', 1, status, o%message)
            if (status == 0) then
                call gremlin_publish_terminal_snapshot(o%project_dir, o%session, &
                    status, o%message)
                if (status == 0) call gremlin_session_release(o%session, status, &
                    o%message)
            end if
            call gremlin_error_response('run', 'cannot release generation lease: '// &
                trim(o%fatal_message), response)
            return
        end if
        ! Finish potentially slow filesystem work before exposing stopped or
        ! releasing the owner; observers use that terminal state as a barrier.
        if (len_trim(o%active%build_project_root) > 0) then
            call fs_remove_tree(trim(o%active%build_project_root))
            o%active%build_project_root = ''
        end if
        call maintain_fo_cache(o%cache_cursor, status)
        if (status /= 0) write (error_unit, '(a,i0)') &
            'Gremlin cache maintenance at stop: ', status
        outcome = 'NONE'
        if (o%stop_requested) outcome = 'CANCELLED'
        call publish(o, 'stopped', '', outcome, 0, publish_error, o%message, &
            status_text_out=status_text)
        call gremlin_release_stopped_session(o%project_dir, o%session, &
            trim(status_text), status, detail)
        if (publish_error /= 0 .and. status /= 0) then
            call gremlin_error_response('run', 'cannot durably terminalize stopped '// &
                'Gremlin state after state publication error ('// &
                int_text(publish_error)//'): '//trim(detail), response)
            return
        else if (publish_error /= 0) then
            call gremlin_error_response('run', 'cannot publish stopped Gremlin state ('// &
                int_text(publish_error)//'); stored the constructed terminal state', &
                response)
            return
        else if (status /= 0) then
            call gremlin_error_response('run', trim(detail), response)
            return
        end if
        ! Stopping keeps this session's generations: terminal reproduction replays
        ! them after the owner exits. Later builds prune inactive generations.
        exitcode = 0
        call gremlin_simple_response('run', o%request%lane_id, o%session%session_id, &
            'stopped', response)
    end subroutine close_stopped

    integer function running_count(o) result(n)
        type(owner_t), intent(in) :: o

        n = count(o%tests%job%pid > 0)
    end function running_count

    function first_running_case(o) result(name)
        type(owner_t), intent(in) :: o
        character(len=NAME_LEN) :: name
        integer :: i

        name = ''
        do i = 1, size(o%tests)
            if (o%tests(i)%job%pid <= 0) cycle
            name = o%tests(i)%case_name
            return
        end do
    end function first_running_case

    function current_case(o) result(name)
        !! The head of the current selection.
        type(owner_t), intent(in) :: o
        character(len=NAME_LEN) :: name

        name = ''
        if (o%n_selected > 0) name = o%selected(1)
    end function current_case

    subroutine publish(o, state, case_name, last_outcome, last_exitcode, ierr, message, &
            status_text_out, diagnostic, candidate, seed)
        !! Publish the lane state (and first its lifecycle transition). The
        !! candidate defaults to the owner's; seed to the campaign seed.
        type(owner_t), intent(in) :: o
        character(len=*), intent(in) :: state, case_name, last_outcome
        integer, intent(in) :: last_exitcode
        integer, intent(out), optional :: ierr
        character(len=*), intent(out), optional :: message, status_text_out
        character(len=*), intent(in), optional :: diagnostic
        type(generation_t), intent(in), optional :: candidate
        integer, intent(in), optional :: seed

        if (present(candidate)) then
            call publish_state(o, candidate, state, case_name, last_outcome, &
                last_exitcode, ierr, message, status_text_out, diagnostic, seed)
        else
            call publish_state(o, o%candidate, state, case_name, last_outcome, &
                last_exitcode, ierr, message, status_text_out, diagnostic, seed)
        end if
    end subroutine publish

    subroutine publish_state(o, candidate, state, case_name, last_outcome, &
            last_exitcode, ierr, message, status_text_out, diagnostic, seed)
        type(owner_t), intent(in) :: o
        type(generation_t), intent(in) :: candidate
        character(len=*), intent(in) :: state, case_name, last_outcome
        integer, intent(in) :: last_exitcode
        integer, intent(out), optional :: ierr
        character(len=*), intent(out), optional :: message, status_text_out
        character(len=*), intent(in), optional :: diagnostic
        integer, intent(in), optional :: seed

        character(len=:), allocatable :: status_text
        character(len=NAME_LEN) :: running(GREMLIN_MAX_LANE_JOBS)
        type(gremlin_session_t) :: read_session
        type(gremlin_coverage_view_t) :: coverage_view
        type(gremlin_readiness_t) :: readiness
        character(len=PATH_LEN) :: local_message, lifecycle_path, event_message
        character(len=PATH_LEN) :: diagnostic_text, retention_root
        integer :: status, n_running, i, published_seed, trimmed
        logical :: have_coverage

        diagnostic_text = ''
        if (present(diagnostic)) diagnostic_text = diagnostic
        published_seed = o%seed
        if (present(seed)) published_seed = seed
        n_running = 0
        do i = 1, size(o%tests)
            if (o%tests(i)%job%pid <= 0) cycle
            n_running = n_running + 1
            running(n_running) = o%tests(i)%case_name
        end do
        retention_root = o%active%root
        if (len_trim(retention_root) == 0) retention_root = candidate%root
        status_text = gremlin_status_json(o%session, o%request, &
            request_policy_key(o%request), state, o%active, candidate, &
            active_model_identity(o%active), case_name, running(:n_running), &
            o%completed, o%n_selected, published_seed, last_outcome, last_exitcode, &
            gremlin_retention_json(retention_root, o%session%state_dir//'/views'), &
            diagnostic_text)
        ! Record the lifecycle transition before publishing the state, so any
        ! reader that observes a state also finds its lifecycle event.
        local_message = ''
        call lifecycle_path_for_session(o%session, o%request, lifecycle_path, status, &
            event_message)
        if (status == 0) then
            read_session = o%session
            read_session%state_dir = lifecycle_path(: &
                len_trim(lifecycle_path) - len('/lifecycle.jsonl'))
            coverage_view = gremlin_coverage_view_t()
            have_coverage = .false.
            if (len_trim(o%active%identity) == HASH_LEN) then
                call coverage_read_view_path(coverage_state_path(o%session, &
                    o%active%identity), trim(o%active%identity), coverage_view, &
                    status, event_message)
                have_coverage = status == COVERAGE_OK
                if (status == COVERAGE_NOT_FOUND) status = 0
            end if
        end if
        if (status == 0) call compute_readiness(read_session, status_text, &
            coverage_view, have_coverage, readiness, status, event_message)
        ! Quiescence is write-free once its lifecycle/state becomes visible.
        if (status == 0 .and. state == 'quiescent') then
            trimmed = gremlin_trim_cache(retention_root, o%session%state_dir)
            if (trimmed < 0) write(error_unit, '(a,i0)') &
                'Gremlin retained-view eviction: ', trimmed
        end if
        if (status == 0) call publish_lifecycle_transition(read_session, &
            trim(lifecycle_path), state, o%active, candidate, case_name, last_outcome, &
            readiness, coverage_view, have_coverage, trim(diagnostic_text), status, &
            event_message)
        if (status /= 0) then
            local_message = 'cannot publish Gremlin lifecycle transition: '// &
                trim(event_message)
        else
            call gremlin_session_publish(o%session, status_text, status, local_message)
        end if
        if (present(ierr)) ierr = status
        if (present(message)) message = local_message
        if (present(status_text_out)) status_text_out = status_text
    end subroutine publish_state

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

    subroutine publish_lifecycle_transition(session, path, state, active, candidate, &
            current_case, last_outcome, readiness, coverage, have_coverage, diagnostic, &
            ierr, message)
        !! Append the lifecycle events that the newly published state implies
        !! against the last recorded event, in their fixed order.
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: path, state, current_case, last_outcome
        type(generation_t), intent(in) :: active, candidate
        type(gremlin_readiness_t), intent(in) :: readiness
        type(gremlin_coverage_view_t), intent(in) :: coverage
        logical, intent(in) :: have_coverage
        character(len=*), intent(in) :: diagnostic
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(gremlin_lifecycle_event_t) :: previous
        logical :: found, coverage_changed, previous_pass
        character(len=32) :: failure_event
        character(len=HASH_LEN) :: previous_generation

        ierr = 0
        message = ''
        call read_last_lifecycle_event(path, previous, found, ierr, message)
        if (ierr /= 0) return
        previous_generation = ''
        if (found) previous_generation = previous%active_generation
        if (trim(state) == 'starting') then
            call emit('session_started', active%identity)
        else if (trim(state) == 'building') then
            call emit('generation_started', candidate%identity)
        else if (trim(state) == 'build_failed' .or. trim(last_outcome) == 'BUILD_FAIL') then
            call emit('build_failed', candidate%identity)
        else if (trim(state) == 'capture_failed') then
            call emit('capture_failed', candidate%identity, diagnostic)
        else if (trim(state) == 'error') then
            call emit('session_failed', active%identity)
        else if (trim(state) == 'stopped') then
            call emit('session_stopped', active%identity)
        end if
        if (len_trim(active%identity) == HASH_LEN .and. &
            trim(active%identity) /= trim(previous_generation)) then
            call emit('build_passed', candidate%identity)
            if (len_trim(previous_generation) > 0) then
                call emit('generation_superseded', candidate%identity)
            else
                call emit('generation_activated', active%identity)
            end if
        end if
        if (readiness%local_gate_green .and. (.not. found .or. &
            .not. previous%local_gate_green)) then
            call emit('local_gate_green', active%identity)
        else if (found .and. previous%local_gate_green .and. &
                .not. readiness%local_gate_green) then
            call emit('local_gate_changed', active%identity)
        end if
        ! Normal FAIL is retained only after the normal runner's repeat attempt.
        ! A previous-generation PASS distinguishes a confirmed regression from
        ! a newly observed failure. Flaky, timeout and infra are factual outcomes.
        failure_event = ''
        select case (trim(last_outcome))
        case ('FAIL')
            if (ierr == 0) call prior_generation_case_passed(session, active, &
                current_case, previous_pass, ierr, message)
            failure_event = 'test_failed'
            if (ierr == 0 .and. previous_pass) failure_event = 'regression_confirmed'
        case ('FLAKY')
            failure_event = 'test_flaky'
        case ('TIMEOUT')
            failure_event = 'test_timeout'
        case ('INFRA_ERROR', 'INFRA')
            if (len_trim(current_case) > 0 .and. trim(current_case) /= '<build>') &
                failure_event = 'test_infra_error'
        end select
        if (len_trim(failure_event) > 0 .and. len_trim(active%identity) == HASH_LEN) &
            call emit(failure_event, active%identity)
        if (readiness%verification_level >= GREMLIN_VERIFY_ORDINARY .and. &
            (.not. found .or. (previous%verification_level /= 'ordinary' .and. &
            previous%verification_level /= 'full'))) &
            call emit('ordinary_coverage_complete', active%identity)
        if (readiness%fully_verified .and. &
            (.not. found .or. previous%verification_level /= 'full')) &
            call emit('full_verification_complete', active%identity)
        if (readiness%phase == GREMLIN_PHASE_QUIESCENT .and. &
            (.not. found .or. previous%phase /= 'quiescent')) then
            call emit('session_quiescent', active%identity)
        else if (found .and. previous%phase == 'quiescent' .and. &
                readiness%phase /= GREMLIN_PHASE_QUIESCENT) then
            call emit('session_wake', active%identity)
        end if
        coverage_changed = have_coverage
        if (found .and. have_coverage) coverage_changed = &
            coverage%epoch /= previous%epoch .or. &
            coverage%cursor /= previous%epoch_cursor .or. &
            readiness%ordinary_passed /= previous%ordinary_passed .or. &
            readiness%ordinary_skipped /= previous%ordinary_skipped .or. &
            readiness%ordinary_required /= previous%ordinary_required .or. &
            readiness%ordinary_failures /= previous%ordinary_failures
        if (coverage_changed) call emit('coverage_updated', active%identity)

    contains

        subroutine emit(event_type, subject_generation, event_diagnostic)
            !! Append one event; after a failure no further event is written.
            character(len=*), intent(in) :: event_type, subject_generation
            character(len=*), intent(in), optional :: event_diagnostic
            type(gremlin_lifecycle_event_t) :: event

            if (ierr /= 0) return
            event%event_type = trim(event_type)
            event%diagnostic = ''
            if (present(event_diagnostic)) event%diagnostic = event_diagnostic
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
            event%verification_level = &
                gremlin_verification_name(readiness%verification_level)
            event%fully_verified = readiness%fully_verified
            event%gate_required = readiness%gate_required
            event%gate_passed = readiness%gate_passed
            event%gate_skipped = readiness%gate_skipped
            event%ordinary_required = readiness%ordinary_required
            event%ordinary_passed = readiness%ordinary_passed
            event%ordinary_skipped = readiness%ordinary_skipped
            event%ordinary_failures = readiness%ordinary_failures
            event%full_required = readiness%full_required
            event%full_passed = readiness%full_passed
            event%full_skipped = readiness%full_skipped
            event%full_failures = readiness%full_failures
            if (have_coverage) then
                event%epoch = coverage%epoch
                event%epoch_cursor = coverage%cursor
            end if
            event%event_id = gremlin_lifecycle_make_id(session%session_id, event)
            call gremlin_lifecycle_append(path, event, ierr, message)
        end subroutine emit

    end subroutine publish_lifecycle_transition

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


end module fo_gremlin_owner
