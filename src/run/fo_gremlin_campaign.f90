module fo_gremlin_campaign
    !! Gremlin campaign policy shared by the lane owner and reproductions:
    !! the generation model and its impact, campaign selection over coverage
    !! and receipts, the child commands of a frozen generation, host work-slot
    !! admission and the lane policy key.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_build_backend, only: BACKEND_NATIVE, BACKEND_CMAKE, backend_t, detect_backend
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_ctest_run, only: ctest_fixture_support_case, ctest_run_timeout_seconds
    use fo_cmake_generation, only: cmake_registered_names
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    use fo_fs, only: fs_sleep_ms
    use fo_gremlin_case_inputs, only: generation_model_roots, generation_units_ensure, &
        generation_case_digests_ensure, generation_inventory_changes, &
        generation_oracle_digests_ensure, generation_oracle_digest
    use fo_gremlin_context, only: generation_inventory_restore
    use fo_gremlin_coverage, only: coverage_epoch_t, coverage_open, coverage_next_chunk, &
        coverage_record, coverage_recover_running, COVERAGE_OK
    use fo_gremlin_execution_view, only: execution_view_t
    use fo_gremlin_generation, only: generation_t, generation_model_key
    use fo_gremlin_journal, only: journal_read_all, journal_record_t, JOURNAL_OK
    use fo_gremlin_policy, only: shuffle_gremlin_tests, GREMLIN_POLICY_OK, &
        GREMLIN_NONMANDATORY_LIMIT
    use fo_gremlin_receipts, only: NAME_LEN, PATH_LEN, gate_receipt_matches, &
        coverage_state_path, campaign_failures, campaign_failure_t
    use fo_gremlin_request, only: gremlin_request_t, gremlin_json_field
    use fo_gremlin_session, only: gremlin_get_session_journal_path
    use fo_gremlin_state, only: gremlin_session_t, gremlin_lease_t, &
        gremlin_lease_group_t, gremlin_lease_release, gremlin_session_state_dir, &
        gremlin_session_stop_requested, gremlin_host_lease_acquire_weighted, &
        gremlin_host_lease_release, gremlin_session_read, GREMLIN_STATE_TEXT_MAX, &
        LEASE_GROUP_MAX
    use fo_gremlin_status_json, only: gremlin_simple_response, gremlin_error_response
    use fo_process, only: argv_push, process_detect_nproc
    use fo_project_load, only: project_has_model, project_load_configured, &
        project_inventory_cases, project_affected_cases, project_changed_files, &
        project_expand_cases
    use fo_project_model, only: model_find_case, model_select, model_request_t, &
        model_selection_t, SELECT_OK, SELECT_EMPTY
    use fo_scan, only: is_slow_test
    use fo_test_budget, only: test_budget_seconds, test_wall_cap_seconds, &
        ctest_default_timeout_seconds
    use fo_util, only: json_bool, int_text
    use fx_dag, only: MAX_NODES
    use fx_string, only: string_t
    implicit none
    private

    public :: discover_campaign, compute_generation_impact, generation_model_adopt
    public :: generation_model_expand, active_model_identity, next_campaign_seed
    public :: case_wall_timeout, build_command, case_command, append_execution_provenance
    public :: acquire_heavy_work_slot, release_heavy_work_slot, gremlin_host_jobs
    public :: gremlin_lane_jobs, gremlin_host_slots, GREMLIN_MAX_LANE_JOBS
    public :: GREMLIN_HOST_WORK_LEASE
    public :: release_generation_lease, request_policy_key, push_scope_args, log_path
    public :: gremlin_attach_response

    integer, parameter :: GREMLIN_HISTORY_LIMIT = 128
    !! Case children of a lane whose request names no job count.
    integer, parameter :: GREMLIN_DEFAULT_LANE_JOBS = 2
    !! Upper bound of concurrently running case children in one lane.
    integer, parameter :: GREMLIN_MAX_LANE_JOBS = 16
    !! Host work-slot pool shared by every lane of this user. Each slot is one
    !! case child or build job; FO_GREMLIN_HOST_SLOTS sets the pool size
    !! (default: nproc).
    !! The name differs from the former two-slot 'gremlin-heavy-work' pool,
    !! whose capacity file would otherwise make old and new drivers refuse
    !! each other while both are active.
    character(len=*), parameter :: GREMLIN_HOST_WORK_LEASE = 'gremlin-host-work'

contains

    subroutine gremlin_attach_response(action, project_dir, request, response, exitcode)
        !! Attach to the lane's live owner once it has published its state; a
        !! different lane policy is refused.
        character(len=*), intent(in) :: action, project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=PATH_LEN) :: message, owner_start
        character(len=HASH_LEN) :: stored_policy
        character(len=128) :: session_id
        integer :: ierr, owner_pid, i

        exitcode = 2
        do i = 1, 60
            call gremlin_session_read(project_dir, trim(request%lane_id), session_id, &
                owner_pid, owner_start, status_text, ierr, message)
            if (ierr == 0 .and. len_trim(status_text) > 0) exit
            call fs_sleep_ms(50)
        end do
        if (ierr /= 0) then
            call gremlin_error_response(action, trim(message), response)
        else if (len_trim(status_text) == 0) then
            call gremlin_error_response(action, &
                'active Gremlin owner has not published state', response)
        else
            call gremlin_json_field(status_text, 'policy_key', stored_policy)
            if (trim(stored_policy) /= request_policy_key(request)) then
                call gremlin_error_response(action, &
                    'an active Gremlin session has an incompatible policy', response)
            else
                exitcode = 0
                call gremlin_simple_response(action, request%lane_id, trim(session_id), &
                    'attached', response)
            end if
        end if
    end subroutine gremlin_attach_response

    subroutine build_command(driver, view, packed, n_args, env, cases)
        !! `fo build --tests` of a frozen generation in its writable view, for
        !! the given cases only (absent: every case).
        character(len=*), intent(in) :: driver
        type(execution_view_t), intent(in) :: view
        character(len=:), allocatable, intent(out) :: packed, env
        integer, intent(out) :: n_args
        character(len=*), intent(in), optional :: cases(:)
        integer :: i

        n_args = 0
        call argv_push(packed, n_args, trim(driver))
        call argv_push(packed, n_args, 'build')
        call argv_push(packed, n_args, '--tests')
        if (present(cases)) then
            ! Explicit cases already bound the build scope, including slow cases.
            if (size(cases) > 0) call argv_push(packed, n_args, '--all')
            do i = 1, size(cases)
                call argv_push(packed, n_args, '--case')
                call argv_push(packed, n_args, trim(cases(i)))
            end do
        end if
        env = 'FO_DISABLE_SELF_REFRESH=1;FO_SELF_REFRESH=0;FO_GREMLIN_FROZEN_ROOT='// &
            trim(view%cwd)//';TMPDIR='//trim(view%tmpdir)
    end subroutine build_command

    subroutine case_command(driver, generation, case_name, view, frozen_root, packed, &
            n_args, env)
        !! One case of a built generation (never a build), run in its private
        !! execution view against the frozen build at frozen_root.
        character(len=*), intent(in) :: driver, case_name, frozen_root
        type(generation_t), intent(in) :: generation
        type(execution_view_t), intent(in) :: view
        character(len=:), allocatable, intent(out) :: packed, env
        integer, intent(out) :: n_args

        n_args = 0
        call argv_push(packed, n_args, trim(driver))
        call argv_push(packed, n_args, 'test')
        call argv_push(packed, n_args, '--no-build')
        call argv_push(packed, n_args, '--no-cache')
        call argv_push(packed, n_args, '--json=full')
        if (case_needs_all(generation, case_name)) call argv_push(packed, n_args, '--all')
        call argv_push(packed, n_args, trim(case_name))
        env = 'FO_BIN='//trim(driver)//';FO='//trim(driver)//';'// &
            'FO_DISABLE_SELF_REFRESH=1;FO_SELF_REFRESH=0;'// &
            'FO_GREMLIN_EXECUTION_CWD='//trim(view%cwd)//';'// &
            'FO_GREMLIN_FROZEN_ROOT='//trim(frozen_root)//';'// &
            'TMPDIR='//trim(view%tmpdir)
    end subroutine case_command

    subroutine acquire_heavy_work_slot(session, lane_jobs, lease, held, honor_stop, &
            stop_requested, ierr, message, would_wait)
        !! Admission weight out of the host pool gremlin_host_slots(): the
        !! larger of the lane's concurrent case children and its FO_JOBS build
        !! budget, clamped to the pool. Lanes with explicit budgets share the
        !! host in proportion to them; a lane without FO_JOBS (host default)
        !! still takes the whole pool.
        type(gremlin_session_t), intent(in) :: session
        integer, intent(in) :: lane_jobs
        type(gremlin_lease_group_t), intent(inout) :: lease
        logical, intent(inout) :: held
        logical, intent(in) :: honor_stop
        logical, intent(out) :: stop_requested
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        !! When present, return at once with would_wait = .true. instead of
        !! waiting for a slot that other lanes hold, so callers can publish why.
        logical, intent(out), optional :: would_wait

        logical :: busy
        integer :: weight, capacity

        ierr = 0
        message = ''
        stop_requested = .false.
        if (present(would_wait)) would_wait = .false.
        if (held) then
            if (.not. lease%closing) return
        end if
        capacity = gremlin_host_slots()
        weight = max(1, min(capacity, max(lane_jobs, gremlin_host_jobs())))
        do
            if (held) then
                call release_heavy_work_slot(lease, held, ierr, message)
                if (ierr /= 0) return
                busy = held
            else
                call gremlin_host_lease_acquire_weighted(GREMLIN_HOST_WORK_LEASE, &
                    capacity, weight, lease, busy, ierr, message)
            end if
            if (ierr == 0) then
                if (.not. busy) then
                    if (lease%count == 0) cycle
                    held = .true.
                    return
                end if
            end if
            if (.not. busy) then
                message = 'cannot acquire Gremlin host work slot: '//trim(message)
                return
            end if
            if (present(would_wait)) then
                ! A busy slot reports EAGAIN; that is a wait, not a failure.
                ierr = 0
                message = ''
                would_wait = .true.
                return
            end if
            if (honor_stop) then
                call gremlin_session_stop_requested(session, stop_requested, &
                    ierr, message)
                if (ierr /= 0) then
                    message = 'cannot check Gremlin stop while waiting for host '// &
                        'work slot: '//trim(message)
                    return
                end if
                if (stop_requested) return
            end if
            call fs_sleep_ms(100)
        end do
    end subroutine acquire_heavy_work_slot

    subroutine release_heavy_work_slot(lease, held, ierr, message, drain)
        type(gremlin_lease_group_t), intent(inout) :: lease
        logical, intent(inout) :: held
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        logical, intent(in), optional :: drain
        logical :: wait_for_children

        wait_for_children = .false.
        if (present(drain)) wait_for_children = drain
        ierr = 0
        message = ''
        if (.not. held) return
        do
            call gremlin_host_lease_release(lease, ierr, message)
            if (ierr /= 0 .or. lease%count == 0) exit
            if (.not. wait_for_children) exit
            call fs_sleep_ms(20)
        end do
        if (ierr == 0 .and. lease%count == 0) held = .false.
        if (ierr /= 0) message = &
            'cannot release Gremlin host work slot: '//trim(message)
    end subroutine release_heavy_work_slot

    integer function gremlin_host_jobs() result(jobs)
        !! Host job budget; matches native_jobs(): unset/invalid FO_JOBS keeps
        !! the host default.
        character(len=32) :: value
        integer :: status, parse_status

        jobs = process_detect_nproc()
        if (jobs < 1) jobs = 1
        call get_environment_variable('FO_JOBS', value, status=status)
        if (status == 0 .and. len_trim(value) > 0) then
            read (value, *, iostat=parse_status) jobs
            if (parse_status /= 0 .or. jobs < 1) then
                jobs = process_detect_nproc()
                if (jobs < 1) jobs = 1
            end if
        end if
    end function gremlin_host_jobs

    integer function gremlin_host_slots() result(slots)
        !! Host work-slot pool: FO_GREMLIN_HOST_SLOTS when it is a positive
        !! integer, else the processor count, clamped to LEASE_GROUP_MAX.
        character(len=32) :: value
        integer :: status, parse_status

        slots = 0
        call get_environment_variable('FO_GREMLIN_HOST_SLOTS', value, status=status)
        if (status == 0 .and. len_trim(value) > 0) then
            read (value, *, iostat=parse_status) slots
            if (parse_status /= 0) slots = 0
        end if
        if (slots < 1) slots = process_detect_nproc()
        slots = max(1, min(LEASE_GROUP_MAX, slots))
    end function gremlin_host_slots

    integer function gremlin_lane_jobs(request) result(k)
        !! Case children one lane runs at once: the request's jobs, else the
        !! host heavy-work weight (1 for FO_JOBS=1).
        type(gremlin_request_t), intent(in) :: request

        k = request%jobs
        if (k < 1) k = min(GREMLIN_DEFAULT_LANE_JOBS, gremlin_host_jobs())
        k = max(1, min(GREMLIN_MAX_LANE_JOBS, k))
    end function gremlin_lane_jobs

    subroutine compute_generation_impact(baseline, candidate, request)
        !! Cases of the candidate affected by its input changes against the
        !! previous generation: model_affected on the candidate's model
        !! (fpm and CMake alike) with the changed project files of the two
        !! inventories. Changed inputs outside the project files (global
        !! roles, dependency trees, CMake context), an incomplete inventory
        !! or another fo driver widen to every case.
        type(generation_t), intent(inout) :: baseline, candidate
        type(gremlin_request_t), intent(inout) :: request

        type(backend_t) :: backend
        type(string_t), allocatable :: changed(:), reasons(:), roots(:)
        type(string_t), allocatable :: widen_reasons(:)
        integer, allocatable :: cases(:), affected(:)
        character(len=PATH_LEN) :: message, project
        integer :: ierr, i

        if (allocated(request%impact_cases)) deallocate(request%impact_cases)
        request%n_impact_cases = 0
        request%impact_all = .true.
        project = campaign_project(candidate)
        backend = detect_backend(trim(project))
        ! The frozen FO_CMAKE_NATIVE route has no model: every case.
        if (.not. project_has_model(backend)) return
        if (.not. baseline%input_inventory_ready) then
            call generation_inventory_restore(baseline, ierr, message)
            if (ierr /= 0) return
        end if
        if (.not. candidate%input_inventory_ready) then
            call generation_inventory_restore(candidate, ierr, message)
            if (ierr /= 0) return
        end if
        call generation_model_ensure(candidate, ierr, message)
        if (ierr /= 0 .or. .not. allocated(candidate%model)) return
        call generation_units_ensure(candidate, trim(project))
        call generation_inventory_changes(baseline%input_inventory, &
            candidate%input_inventory, changed, reasons)
        if (baseline%driver_digest /= candidate%driver_digest .or. &
            baseline%driver_size /= candidate%driver_size) &
            call push_reason('fo driver changed')
        call generation_model_roots(candidate, trim(project), roots)
        call project_inventory_cases(candidate%model, cases)
        if (size(reasons) > 0) then
            call project_affected_cases(candidate%model, candidate%units, roots, &
                cases, changed, affected, widen_reasons, force_reason=reasons(1)%s)
        else
            call project_affected_cases(candidate%model, candidate%units, roots, &
                cases, changed, affected, widen_reasons)
        end if
        request%impact_all = .false.
        request%n_impact_cases = size(affected)
        allocate (request%impact_cases(size(affected)))
        do i = 1, size(affected)
            request%impact_cases(i) = candidate%model%cases(affected(i))%name
        end do

    contains

        subroutine push_reason(text)
            character(len=*), intent(in) :: text
            type(string_t), allocatable :: grown(:)

            allocate (grown(size(reasons) + 1))
            grown(1:size(reasons)) = reasons
            grown(size(grown))%s = text
            call move_alloc(grown, reasons)
        end subroutine push_reason

    end subroutine compute_generation_impact

    function campaign_project(generation) result(project)
        type(generation_t), intent(in) :: generation
        character(len=PATH_LEN) :: project
        type(backend_t) :: backend
        backend = detect_backend(generation%project_root)
        project = generation%project_root
        if (backend%kind == BACKEND_CMAKE .and. &
            len_trim(generation%build_project_root) > 0) &
            project = generation%build_project_root
    end function campaign_project

    subroutine generation_model_ensure(generation, ierr, message)
        !! Load the generation's project model once, from its built view
        !! (CMake: the configure of the generation build, not repeated).
        !! A model reused from an earlier generation is kept as is. The
        !! frozen FO_CMAKE_NATIVE route has no model and loads nothing.
        type(generation_t), intent(inout) :: generation
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(backend_t) :: backend
        character(len=:), allocatable :: detail
        integer :: restore_status

        ierr = 0
        message = ''
        if (allocated(generation%model)) return
        backend = detect_backend(campaign_project(generation))
        if (.not. project_has_model(backend)) return
        ! Without its inventory the model has no reuse key; it still loads.
        if (.not. generation%input_inventory_ready) &
            call generation_inventory_restore(generation, restore_status, message)
        message = ''
        allocate (generation%model)
        call project_load_configured(backend, generation%model, ierr, detail, &
            scanned=generation%model_extra)
        if (ierr /= 0) then
            deallocate (generation%model)
            message = 'cannot load the generation project model: '//detail
            return
        end if
        generation%model_generation = generation%identity
        generation%model_root = campaign_project(generation)
        ! fpm source layers follow module use, so every source counts.
        generation%model_full_key = backend%kind == BACKEND_NATIVE .and. &
            generation%model%n_layers > 0
        generation%model_key = ''
        if (generation%input_inventory_ready) generation%model_key = &
            generation_model_key(generation%input_inventory, &
            generation%driver_digest, generation%model_full_key)
        call generation_model_expand(generation)
    end subroutine generation_model_ensure

    subroutine generation_model_expand(generation)
        !! Split the executables of the built generation that list their
        !! cases into model cases `exe::case` (#217), so each case has its
        !! own verdict, receipt and input closure digest. A model kept from an
        !! earlier generation is expanded again from the new executables.
        type(generation_t), intent(inout) :: generation
        type(backend_t) :: backend
        character(len=:), allocatable :: detail
        integer :: n, ierr

        if (.not. allocated(generation%model)) return
        ! The executables are in the built view, where the cases run.
        if (len_trim(generation%build_project_root) > 0) then
            backend = detect_backend(trim(generation%build_project_root))
        else
            backend = detect_backend(campaign_project(generation))
        end if
        call project_expand_cases(backend, '', generation%model, n, ierr, detail, &
            trim(generation%model_root), trim(generation%build_project_root))
        if (allocated(generation%case_digests)) deallocate (generation%case_digests)
        if (allocated(generation%oracle_digests)) deallocate (generation%oracle_digests)
    end subroutine generation_model_expand

    function active_model_identity(generation) result(identity)
        type(generation_t), intent(in) :: generation
        character(len=HASH_LEN) :: identity

        identity = ''
        if (allocated(generation%model)) identity = generation%model%identity
    end function active_model_identity

    subroutine generation_model_adopt(previous, candidate)
        !! Reuse the previous generation's model when the candidate's model
        !! key (manifest, CMake script, preset and cache content, input
        !! paths) is the same; otherwise the candidate loads its own.
        type(generation_t), intent(in) :: previous
        type(generation_t), intent(inout) :: candidate
        character(len=HASH_LEN) :: key
        character(len=PATH_LEN) :: message
        integer :: ierr

        if (allocated(candidate%model)) deallocate (candidate%model)
        candidate%model_key = ''
        candidate%model_generation = ''
        if (.not. allocated(previous%model)) return
        if (len_trim(previous%model_key) /= HASH_LEN) return
        if (.not. candidate%input_inventory_ready) then
            call generation_inventory_restore(candidate, ierr, message)
            if (ierr /= 0) return
        end if
        key = generation_model_key(candidate%input_inventory, &
            candidate%driver_digest, previous%model_full_key)
        if (key /= previous%model_key) return
        candidate%model = previous%model
        candidate%model_key = key
        candidate%model_generation = previous%model_generation
        candidate%model_full_key = previous%model_full_key
        candidate%model_root = previous%model_root
        if (allocated(previous%model_extra)) &
            candidate%model_extra = previous%model_extra
    end subroutine generation_model_adopt

    logical function case_needs_all(generation, name) result(needs)
        !! The case is excluded by default through a label (slow), so its
        !! run passes --all. Without a model: the slow name rule.
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: name
        integer :: k, j, d

        needs = is_slow_test(trim(name))
        if (.not. allocated(generation%model)) return
        k = model_find_case(generation%model, trim(name))
        if (k == 0) return
        needs = .false.
        associate (c => generation%model%cases(k))
            do j = 1, size(c%labels)
                do d = 1, size(generation%model%default_exclude)
                    if (c%labels(j)%s == generation%model%default_exclude(d)%s) &
                        needs = .true.
                end do
            end do
        end associate
    end function case_needs_all

    subroutine discover_campaign(generation, session, request, &
            selected, n_selected, &
            n_mandatory_selected, seed, ierr, message, bypass_coverage, live_dir)
        !! Select a campaign from the cases of the generation's project model
        !! (loaded once per generation, see generation_model_ensure).
        !! live_dir: the watched project directory, whose working-tree delta
        !! an only-changed first generation owes (absent: no baseline).
        type(generation_t), intent(inout) :: generation
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(inout) :: request
        character(len=*), intent(out) :: selected(:)
        integer, intent(out) :: n_selected, n_mandatory_selected, seed, ierr
        character(len=*), intent(out) :: message
        logical, intent(in), optional :: bypass_coverage
        character(len=*), intent(in), optional :: live_dir

        type(backend_t) :: backend
        integer :: i, n_all, n_impacted
        integer :: n_history, n_debt, cursor_seed, n_priorities, limit
        integer :: n_selected_priorities
        integer :: shuffle_status, coverage_status
        character(len=NAME_LEN), allocatable :: all_names(:), impacted(:)
        character(len=NAME_LEN), allocatable :: history(:), debt(:), priorities(:)
        type(coverage_epoch_t) :: coverage
        character(len=PATH_LEN) :: project_dir, live
        character(len=HASH_LEN) :: generation_id
        logical :: reproduce_only, focused_targets
        integer, allocatable :: model_cases(:)
        integer :: capacity, kept, n_model
        type(model_request_t) :: scope_request
        type(model_selection_t) :: scope_selection

        project_dir = campaign_project(generation)
        generation_id = generation%identity
        ierr = 0
        message = ''
        call generation_model_ensure(generation, ierr, message)
        if (ierr /= 0) return
        call generation_case_digests_ensure(generation, trim(project_dir))
        call generation_oracle_digests_ensure(generation, trim(project_dir))
        capacity = MAX_NODES
        if (allocated(generation%model)) then
            call project_inventory_cases(generation%model, model_cases)
            capacity = max(capacity, size(model_cases))
        end if
        allocate (all_names(capacity), impacted(capacity), history(capacity), &
            debt(capacity), priorities(capacity))
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
        if (backend%kind /= BACKEND_CMAKE .and. backend%kind /= BACKEND_NATIVE) then
            ierr = 1
            message = 'Gremlin currently requires the native fpm test backend'
            return
        end if
        n_all = 0
        if (allocated(generation%model)) then
            n_model = size(model_cases)
            if (request_scoped(request)) then
                ! Lane scope: the model's `fo test` selection of -L/-LE/--layer.
                call scope_model_request(request, scope_request)
                call model_select(generation%model, scope_request, scope_selection)
                if (scope_selection%status /= SELECT_OK .and. &
                    scope_selection%status /= SELECT_EMPTY) then
                    ierr = 2
                    message = 'invalid lane scope: '//scope_selection%message
                    return
                end if
                kept = 0
                do i = 1, n_model
                    if (.not. any(scope_selection%cases == model_cases(i))) cycle
                    kept = kept + 1
                    model_cases(kept) = model_cases(i)
                end do
                n_model = kept
                if (kept == 0) then
                    ierr = 2
                    message = 'the lane scope selects no test case'
                    return
                end if
            end if
            do i = 1, n_model
                if (.not. any(request%targets(:request%n_targets) == &
                    generation%model%cases(model_cases(i))%name) .and. &
                    .not. any(request%gate_cases(:request%gate_required_count) == &
                    generation%model%cases(model_cases(i))%name)) then
                    if (ctest_fixture_support_case(generation%model, model_cases(i), &
                        model_cases(:n_model))) cycle
                end if
                associate (name => generation%model%cases(model_cases(i))%name)
                    if (len(name) > len(all_names)) then
                        ierr = 1
                        message = 'test case name exceeds supported length: '//name
                        return
                    end if
                    n_all = n_all + 1
                    all_names(n_all) = name
                end associate
            end do
        else
            ! The frozen FO_CMAKE_NATIVE route has no project model.
            call cmake_registered_names(backend, all_names, n_all, ierr, message)
            if (ierr /= 0) return
        end if
        call read_campaign_history(session, generation, all_names, n_all, &
            history, n_history, &
            debt, n_debt, cursor_seed, ierr, message)
        if (ierr /= 0) return
        n_impacted = 0
        if (request%has_previous_generation) then
            if (request%impact_all) then
                call append_priority_names(all_names, n_all, impacted, n_impacted)
            else
                if (request%n_impact_cases > 0) &
                    call append_priority_names(request%impact_cases, &
                    request%n_impact_cases, impacted, n_impacted)
            end if
        else if (request%only_changed) then
            ! First generation: the cases the working-tree delta affects on
            ! the generation's model, as `fo test --only-changed`; without a
            ! model (frozen FO_CMAKE_NATIVE) every case.
            if (allocated(generation%model)) then
                live = ''
                if (present(live_dir)) live = live_dir
                call first_generation_impact(generation, project_dir, trim(live), &
                    model_cases(:n_model), impacted, n_impacted)
            else
                call append_priority_names(all_names, n_all, impacted, n_impacted)
            end if
        end if
        ! Impact is computed over the complete generation model. A scoped
        ! lane owes only the affected cases within its eligible inventory.
        kept = 0
        do i = 1, n_impacted
            if (.not. any(all_names(:n_all) == impacted(i))) cycle
            kept = kept + 1
            impacted(kept) = impacted(i)
        end do
        n_impacted = kept
        n_priorities = 0
        ! Zero-random campaigns keep their explicit gate across source edits.
        focused_targets = request%n_targets > 0 .and. request%random_count == 0
        ! Recheck durable failures before new affected work without dropping either.
        if (.not. focused_targets) &
            call append_priority_names(history, n_history, priorities, n_priorities)
        if (request%only_changed .or. request%has_previous_generation .or. &
            request%n_targets > 0 .or. n_history > 0) then
            call append_priority_names(request%gate_cases, request%gate_required_count, &
                priorities, n_priorities)
        end if
        call append_priority_names(request%targets, request%n_targets, priorities, &
            n_priorities)
        if (.not. focused_targets) then
            call append_priority_names(impacted, n_impacted, priorities, n_priorities)
        end if
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
        ! An empty eligible inventory has no coverage debt, even when the
        ! request permits background cases. Coverage epochs require members.
        if (limit == 0 .or. n_all == 0) then
            ierr = GREMLIN_POLICY_OK
            message = ''
            return
        end if
        if (len_trim(generation_id) /= HASH_LEN) then
            ierr = 1
            message = 'coverage requires an exact generation identity'
            return
        end if
        call coverage_open(coverage_state_path(session, generation_id), generation_id, &
            all_names, n_all, &
            seed, coverage, coverage_status, message, &
            timeout_seconds=request%timeout_seconds)
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
        if (request%random_count == 0) then
            selected(n_selected_priorities + 1:) = ''
            n_selected = n_selected_priorities
        end if
        if (n_priorities == 0) then
            ! Keep the default gate stable when an owner resumes this epoch.
            priorities(1) = coverage%order(1)
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

    subroutine first_generation_impact(generation, project_dir, live_dir, cases, &
            impacted, n_impacted)
        !! Names of the cases (model indices) that the working-tree delta of
        !! live_dir affects: the selection of `fo test --only-changed` on the
        !! generation's model (project_dir spells its sources). Without a
        !! delta baseline every case.
        type(generation_t), intent(inout) :: generation
        character(len=*), intent(in) :: project_dir, live_dir
        integer, intent(in) :: cases(:)
        character(len=NAME_LEN), intent(inout) :: impacted(:)
        integer, intent(inout) :: n_impacted
        type(string_t), allocatable :: changed(:), roots(:), widen_reasons(:)
        integer, allocatable :: affected(:)
        character(len=:), allocatable :: reason
        logical :: baseline
        integer :: i

        call generation_units_ensure(generation, trim(project_dir))
        call generation_model_roots(generation, trim(project_dir), roots)
        if (len(live_dir) > 0) then
            call project_changed_files(live_dir, changed, baseline, reason)
        else
            allocate (changed(0))
            baseline = .false.
            reason = 'no working tree to compare against'
        end if
        if (baseline) then
            call project_affected_cases(generation%model, generation%units, roots, &
                cases, changed, affected, widen_reasons)
        else
            call project_affected_cases(generation%model, generation%units, roots, &
                cases, changed, affected, widen_reasons, force_reason=reason)
        end if
        do i = 1, size(affected)
            n_impacted = n_impacted + 1
            impacted(n_impacted) = generation%model%cases(affected(i))%name
        end do
    end subroutine first_generation_impact

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

        character(len=NAME_LEN) :: pending(MAX_NODES)
        character(len=PATH_LEN) :: journal_path, campaign_state_dir
        logical :: attempted(MAX_NODES)
        integer :: i, n_pending

        ! Coverage may already be complete when a frozen generation is reused
        ! with a different gate. Durable campaign receipts precede lane receipts,
        ! so both ledgers can discharge a matching gate after owner loss.
        attempted = .false.
        ierr = 0
        message = ''
        call gremlin_get_session_journal_path(session%project_key, request%lane_id, &
            session%session_id, journal_path, ierr, message)
        if (ierr /= 0) return
        call mark_gate_receipts(journal_path, generation, request, attempted, ierr, &
            message)
        if (ierr /= JOURNAL_OK) return
        call gremlin_session_state_dir(trim(session%project_key), &
            trim(request%lane_id), campaign_state_dir, ierr, message)
        if (ierr /= 0) return
        journal_path = trim(campaign_state_dir)//'/campaign-journal.jsonl'
        call mark_gate_receipts(journal_path, generation, request, attempted, ierr, &
            message)
        if (ierr /= JOURNAL_OK) return
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

    subroutine mark_gate_receipts(path, generation, request, attempted, ierr, message)
        character(len=*), intent(in) :: path, generation
        type(gremlin_request_t), intent(in) :: request
        logical, intent(inout) :: attempted(:)
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        character(len=NAME_LEN) :: case_name
        character(len=16) :: outcome
        integer :: i, j

        call journal_read_all(trim(path), records, ierr, message)
        if (ierr /= JOURNAL_OK) return
        do i = 1, size(records)
            if (.not. gate_receipt_matches(records(i)%json, generation, &
                request%requirement_digest, case_name, outcome)) cycle
            do j = 1, request%gate_required_count
                if (case_name == request%gate_cases(j)) attempted(j) = .true.
            end do
        end do
    end subroutine mark_gate_receipts

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
        !! Credit the coverage epoch with the campaign receipts of its
        !! generation, seed, epoch and inventory.
        type(gremlin_session_t), intent(in) :: session
        type(coverage_epoch_t), intent(inout) :: coverage
        character(len=*), intent(out) :: message
        integer, intent(out) :: ierr

        type(journal_record_t), allocatable :: records(:)
        character(len=NAME_LEN) :: case_name
        character(len=HASH_LEN) :: generation, inventory_digest
        character(len=16) :: outcome, seed_text, epoch_text
        integer :: i, receipt_seed, receipt_epoch, ios

        call journal_read_all(trim(session%state_dir)//'/campaign-journal.jsonl', &
            records, ierr, message)
        if (ierr /= JOURNAL_OK) return
        do i = 1, size(records)
            call gremlin_json_field(records(i)%json, 'case_id', case_name)
            call gremlin_json_field(records(i)%json, 'generation', generation)
            call gremlin_json_field(records(i)%json, 'status', outcome)
            call gremlin_json_field(records(i)%json, 'seed', seed_text)
            call gremlin_json_field(records(i)%json, 'inventory_digest', inventory_digest)
            call gremlin_json_field(records(i)%json, 'coverage_epoch', epoch_text)
            if (trim(generation) /= trim(coverage%generation)) cycle
            read(seed_text, *, iostat=ios) receipt_seed
            if (ios /= 0) cycle
            if (receipt_seed /= coverage%seed) cycle
            read(epoch_text, *, iostat=ios) receipt_epoch
            if (ios /= 0) cycle
            if (receipt_epoch /= coverage%epoch .or. &
                trim(inventory_digest) /= trim(coverage%inventory_digest)) cycle
            call coverage_record(coverage, case_name, outcome, ierr, message)
            if (ierr /= COVERAGE_OK) return
        end do
    end subroutine reconcile_coverage

    subroutine read_campaign_history(session, generation, inventory, &
            n_inventory, history, &
            n_history, debt, n_debt, cursor_seed, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: inventory(:)
        integer, intent(in) :: n_inventory
        character(len=*), intent(out) :: history(:), debt(:)
        integer, intent(out) :: n_history, n_debt, cursor_seed, ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        type(campaign_failure_t), allocatable :: failures(:)
        character(len=HASH_LEN) :: identity
        character(len=NAME_LEN) :: case_name
        character(len=16) :: status_name, seed_text
        integer :: i, j, seed_value, ios

        history = ''
        debt = ''
        n_history = 0
        n_debt = 0
        cursor_seed = 0
        call journal_read_all(trim(session%state_dir)//'/campaign-journal.jsonl', &
            records, ierr, message)
        if (ierr /= JOURNAL_OK) return
        call campaign_failures(session, failures, ierr, message)
        if (ierr /= JOURNAL_OK) return
        do i = 1, size(records)
            call gremlin_json_field(records(i)%json, 'case_id', case_name)
            call gremlin_json_field(records(i)%json, 'status', status_name)
            call gremlin_json_field(records(i)%json, 'seed', seed_text)
            if (.not. any(inventory(:n_inventory) == case_name)) cycle
            if (status_name /= 'PASS' .and. status_name /= 'FAIL' .and. &
                status_name /= 'TIMEOUT') cycle
            read(seed_text, *, iostat=ios) seed_value
            if (ios == 0) cursor_seed = seed_value
            call move_to_recent(case_name, debt, n_debt)
            do j = 1, size(failures)
                if (.not. failures(j)%pending .or. failures(j)%order /= i) cycle
                identity = generation_oracle_digest(generation, case_name)
                ! Replaced oracles keep their historical failure evidence but
                ! cannot block this generation's different test contract.
                if (len_trim(identity) == HASH_LEN .and. &
                    len_trim(failures(j)%identity) == HASH_LEN) then
                    if (identity /= failures(j)%identity) cycle
                end if
                call move_to_priority(case_name, history, n_history)
            end do
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

    subroutine append_execution_provenance(log_file, view)
        character(len=*), intent(in) :: log_file
        type(execution_view_t), intent(in) :: view

        integer :: unit, ios
        character(len=len('incomplete')) :: completeness

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

    integer function next_campaign_seed(seed) result(next_seed)
        integer, intent(in) :: seed
        integer(int64) :: state

        ! Park-Miller progression gives a reproducible positive 31-bit seed.
        state = modulo(int(seed, int64), 2147483646_int64) + 1_int64
        state = modulo(state*48271_int64, 2147483647_int64)
        next_seed = int(state)
    end function next_campaign_seed

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

    function request_policy_key(request) result(key)
        type(gremlin_request_t), intent(in) :: request
        character(len=HASH_LEN) :: key

        character(len=32) :: number
        character(len=:), allocatable :: canonical
        integer :: i

        ! jobs 0 (host-derived default) keys like 1: only an explicit fan-out
        ! makes a lane incompatible with a default start.
        canonical = 'v1|r='//trim(int_text(request%random_count))// &
            '|s='//trim(int_text(request%seed))//'|c='// &
            trim(int_text(request%campaign_seconds))//'|t='// &
            trim(int_text(request%timeout_seconds))//'|j='// &
            trim(int_text(max(1, request%jobs)))//'|changed='// &
            trim(json_bool(request%only_changed))//'|shuffle='// &
            trim(json_bool(request%shuffle))
        do i = 1, request%n_targets
            write (number, '(i0)') len_trim(request%targets(i))
            canonical = canonical//'|target:'//trim(number)//':'// &
                trim(request%targets(i))
        end do
        do i = 1, request%n_labels
            canonical = canonical//'|label:'// &
                trim(int_text(len_trim(request%labels(i))))//':'// &
                trim(request%labels(i))
        end do
        do i = 1, request%n_exclude_labels
            canonical = canonical//'|exclude:'// &
                trim(int_text(len_trim(request%exclude_labels(i))))//':'// &
                trim(request%exclude_labels(i))
        end do
        do i = 1, request%n_layers
            canonical = canonical//'|layer:'// &
                trim(int_text(len_trim(request%layers(i))))//':'// &
                trim(request%layers(i))
        end do
        if (len_trim(request%filter) > 0) canonical = canonical//'|filter:'// &
            trim(int_text(len_trim(request%filter)))//':'//trim(request%filter)
        key = text_digest(canonical)
    end function request_policy_key

    logical function request_scoped(request)
        type(gremlin_request_t), intent(in) :: request

        request_scoped = request%n_labels + request%n_exclude_labels + &
            request%n_layers > 0 .or. len_trim(request%filter) > 0
    end function request_scoped

    subroutine push_scope_args(packed, n_args, request)
        !! -L/-LE/--layer arguments of the lane scope, in `fo test` spelling.
        character(len=:), allocatable, intent(inout) :: packed
        integer, intent(inout) :: n_args
        type(gremlin_request_t), intent(in) :: request
        integer :: i

        do i = 1, request%n_labels
            call argv_push(packed, n_args, '-L')
            call argv_push(packed, n_args, trim(request%labels(i)))
        end do
        do i = 1, request%n_exclude_labels
            call argv_push(packed, n_args, '-LE')
            call argv_push(packed, n_args, trim(request%exclude_labels(i)))
        end do
        do i = 1, request%n_layers
            call argv_push(packed, n_args, '--layer')
            call argv_push(packed, n_args, trim(request%layers(i)))
        end do
        if (len_trim(request%filter) > 0) then
            call argv_push(packed, n_args, '-E')
            call argv_push(packed, n_args, trim(request%filter))
        end if
    end subroutine push_scope_args

    subroutine scope_model_request(request, model_request)
        type(gremlin_request_t), intent(in) :: request
        type(model_request_t), intent(out) :: model_request
        integer :: i

        allocate (model_request%names(0))
        allocate (model_request%include_labels(request%n_labels))
        allocate (model_request%exclude_labels(request%n_exclude_labels))
        allocate (model_request%layers(request%n_layers))
        model_request%filter = trim(request%filter)
        ! An explicit required case may carry an otherwise excluded slow label.
        model_request%all = request%n_targets > 0
        do i = 1, request%n_labels
            model_request%include_labels(i)%s = trim(request%labels(i))
        end do
        do i = 1, request%n_exclude_labels
            model_request%exclude_labels(i)%s = trim(request%exclude_labels(i))
        end do
        do i = 1, request%n_layers
            model_request%layers(i)%s = trim(request%layers(i))
        end do
    end subroutine scope_model_request

    function text_digest(text) result(digest)
        character(len=*), intent(in) :: text
        character(len=HASH_LEN) :: digest

        character(len=:), allocatable :: parts(:)

        allocate (character(len=max(1, len(text))) :: parts(1))
        parts(1) = text
        digest = cache_digest(parts, 1)
    end function text_digest

    subroutine log_path(session, filename, path)
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: path

        path = trim(session%state_dir)//'/logs/'//trim(session%session_id)// &
            '-'//trim(filename)
    end subroutine log_path

    integer function case_wall_timeout(generation, case_name, override) result(seconds)
        !! Gremlin's backstop for one case process. It must not fire before
        !! the runner's own limit: CTest cases get their TIMEOUT properties
        !! (fixtures included) or ctest's default, fpm tests their wall cap.
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: case_name
        integer, intent(in) :: override
        type(fpm_config_t), allocatable :: config
        type(backend_t) :: backend
        integer :: ierr, budget, k
        logical :: fpm

        seconds = override
        if (seconds > 0) return
        if (allocated(generation%model)) then
            fpm = generation%model%backend == 'fpm'
            k = model_find_case(generation%model, trim(case_name))
            if (.not. fpm .and. k > 0) then
                seconds = ctest_run_timeout_seconds(generation%model, k) + 5
                return
            end if
        else
            backend = detect_backend(generation%project_root)
            fpm = backend%kind /= BACKEND_CMAKE
        end if
        if (.not. fpm) then
            seconds = ctest_default_timeout_seconds() + 5
            return
        end if
        allocate (config)
        call fpm_config_parse(trim(generation%project_root), config, ierr)
        budget = test_budget_seconds(config, is_slow_test(case_name))
        seconds = test_wall_cap_seconds(config, budget) + 5
    end function case_wall_timeout

end module fo_gremlin_campaign
