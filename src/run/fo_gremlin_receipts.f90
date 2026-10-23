module fo_gremlin_receipts
    !! Gremlin evidence: campaign and reproduction receipts, the per-case
    !! receipt index that decides PASS reuse, the runner verdict of a case
    !! log, and the readiness of a published lane state from its receipts.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_fs, only: fs_make_dir
    use fo_gremlin_case_inputs, only: generation_case_digest, generation_oracle_digest
    use fo_gremlin_coverage, only: coverage_read_view_path, coverage_record_path, &
        COVERAGE_OK
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_generation, only: generation_t
    use fo_gremlin_journal, only: journal_append, journal_read_all, &
        journal_compact_tail, journal_record_t, JOURNAL_OK
    use fo_gremlin_lifecycle, only: gremlin_lifecycle_event_t, &
        gremlin_lifecycle_read_page, LIFECYCLE_OK, LIFECYCLE_MAX_EVENT
    use fo_gremlin_readiness, only: gremlin_readiness_input_t, gremlin_readiness_t, &
        gremlin_readiness_compute, GREMLIN_PHASE_STARTING, GREMLIN_PHASE_BUILDING, &
        GREMLIN_PHASE_TESTING, GREMLIN_PHASE_QUIESCENT, GREMLIN_PHASE_STOPPED, &
        GREMLIN_PHASE_FAILED
    use fo_gremlin_request, only: gremlin_request_t, gremlin_json_field
    use fo_gremlin_session, only: gremlin_get_session_journal_path
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_state_dir
    use fo_project_model, only: model_find_case
    use fo_json_object, only: json_value_raw, json_object_member
    use fx_dag, only: MAX_NODES
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_key, json_value_string, json_key_string, json_key_int, &
        json_key_bool, json_to_string, array_open => json_array_start, &
        array_close => json_array_end
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
        json_parser_next, EV_OBJECT_START => JSON_OBJECT_START, &
        EV_OBJECT_END => JSON_OBJECT_END, EV_ARRAY_START => JSON_ARRAY_START, &
        EV_ARRAY_END => JSON_ARRAY_END, EV_KEY => JSON_KEY, JSON_STRING, JSON_ERROR, &
        JSON_END_OF_INPUT, JSON_INTEGER
    implicit none
    private

    public :: NAME_LEN, PATH_LEN
    public :: record_receipt, case_reuse_source, prior_generation_case_passed
    public :: compute_readiness, lifecycle_path_for_session, coverage_state_path
    public :: gate_receipt_matches, status_is_failure, receipt_limit
    public :: runner_case_outcome, campaign_failures, campaign_failure_resolution

    type, public :: campaign_failure_t
        character(len=1024) :: name = ''
        character(len=HASH_LEN) :: identity = '', generation = ''
        character(len=256) :: completion = ''
        logical :: pending = .false.
        integer :: order = 0
    end type campaign_failure_t

    integer, parameter :: NAME_LEN = 1024, PATH_LEN = 4096
    integer, parameter :: GREMLIN_POLICY_RECEIPT_LIMIT = 512
    integer, parameter :: GREMLIN_MIN_RECEIPT_LIMIT = 16

    type :: case_index_entry_t
        !! Latest campaign receipt and latest PASS of one case.
        character(len=HASH_LEN) :: last_digest = '', last_generation = ''
        character(len=16) :: last_status = ''
        character(len=HASH_LEN) :: pass_digest = '', pass_generation = ''
    end type case_index_entry_t

    interface
        integer(c_int) function c_publish_file(path, data, data_len) &
                bind(C, name='fo_c_gremlin_coverage_publish')
            import :: c_char, c_int, c_size_t
            character(kind=c_char), intent(in) :: path(*), data(*)
            integer(c_size_t), value :: data_len
        end function c_publish_file
    end interface

contains

    function coverage_state_path(session, identity) result(path)
        !! The coverage state file of one generation of the lane.
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: identity
        character(len=:), allocatable :: path

        path = trim(session%state_dir)//'/coverage-'//trim(identity)//'.state'
    end function coverage_state_path

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
        character(len=NAME_LEN) :: status_name, case_id, raw, receipt_session
        character(len=PATH_LEN) :: journal_path, session_journal_path, campaign_state_dir
        character(len=NAME_LEN) :: gate_cases(MAX_NODES)
        character(len=NAME_LEN) :: gate_success_cases(MAX_NODES)
        character(len=16) :: gate_success_status(MAX_NODES)
        integer :: i, j, ios, gate_count, gate_success_count, journal_index, at
        logical :: build_passed, gate_failure

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
            input%ordinary_skipped = coverage%ordinary_skip_count
            input%ordinary_failures = coverage%ordinary_failure_count
            input%full_required = coverage%eligible_count
            input%full_passed = coverage%pass_count
            input%full_skipped = coverage%skip_count
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
        gate_success_cases = ''
        gate_success_status = ''
        gate_count = 0
        gate_success_count = 0
        if (.not. allocated(session%project_key) .or. &
            .not. allocated(session%lane_id) .or. &
            .not. allocated(session%session_id)) then
            ierr = 1
            message = 'Gremlin session has no receipt journal identity'
            return
        end if
        call gremlin_get_session_journal_path(trim(session%project_key), &
            trim(session%lane_id), trim(session%session_id), session_journal_path, &
            ierr, message)
        if (ierr /= 0) return
        ! Current-session receipts supersede older campaign dispositions.
        do journal_index = 2, 1, -1
            if (journal_index == 2) then
                call gremlin_session_state_dir(trim(session%project_key), &
                    trim(session%lane_id), campaign_state_dir, ierr, message)
                if (ierr /= 0) return
                journal_path = trim(campaign_state_dir)//'/campaign-journal.jsonl'
            else
                journal_path = session_journal_path
            end if
            call journal_read_all(trim(journal_path), records, ierr, message)
            if (ierr /= JOURNAL_OK) return
            do i = 1, size(records)
                ! This session's ledger entries precede their coverage update;
                ! the session journal receives each one after coverage, so
                ! counting them here would report gates ahead of coverage.
                if (journal_index == 2) then
                    call gremlin_json_field(records(i)%json, 'session_id', &
                        receipt_session)
                    if (trim(receipt_session) == trim(session%session_id)) cycle
                end if
                call gremlin_json_field(records(i)%json, 'generation', &
                    receipt_generation)
                call gremlin_json_field(records(i)%json, 'case_id', case_id)
                call gremlin_json_field(records(i)%json, 'status', status_name)
                if (trim(receipt_generation) == trim(candidate_id) .and. &
                    trim(case_id) == '<build>' .and. &
                    status_is_failure(trim(status_name))) &
                    input%failure_observed = .true.
                if (trim(receipt_generation) /= trim(active_id)) cycle
                if (trim(case_id) == '<build>' .and. &
                    trim(status_name) == 'BUILD_PASS') build_passed = .true.
                if (status_is_failure(trim(status_name))) then
                    gate_failure = .true.
                    input%failure_observed = .true.
                end if
                if (.not. gate_receipt_matches(records(i)%json, active_id, &
                    input%requirement_digest, case_id, status_name)) cycle
                if (len_trim(case_id) == 0) cycle
                if (.not. any(gate_cases(:gate_count) == trim(case_id)) .and. &
                    gate_count < size(gate_cases)) then
                    gate_count = gate_count + 1
                    gate_cases(gate_count) = trim(case_id)
                end if
                if (trim(status_name) /= 'PASS' .and. &
                    trim(status_name) /= 'SKIP') cycle
                at = 0
                do j = 1, gate_success_count
                    if (gate_success_cases(j) == trim(case_id)) at = j
                end do
                if (at == 0) then
                    if (gate_success_count == size(gate_cases)) cycle
                    gate_success_count = gate_success_count + 1
                    at = gate_success_count
                    gate_success_cases(at) = trim(case_id)
                end if
                gate_success_status(at) = trim(status_name)
            end do
        end do
        input%exact_active_generation = input%exact_active_generation .and. build_passed
        input%capture_fresh = input%exact_active_generation .and. .not. input%dirty
        input%build_passed = build_passed
        input%gate_required = max(input%gate_required, gate_count)
        input%gate_passed = count(gate_success_status(:gate_success_count) == 'PASS')
        input%gate_skipped = count(gate_success_status(:gate_success_count) == 'SKIP')
        input%gate_failure = gate_failure
        call gremlin_readiness_compute(input, readiness)
    end subroutine compute_readiness

    integer function receipt_limit() result(limit)
        !! Campaign journal retention: FO_GREMLIN_RECEIPT_LIMIT clamped to
        !! [GREMLIN_MIN_RECEIPT_LIMIT, GREMLIN_POLICY_RECEIPT_LIMIT].
        character(len=32) :: value
        integer :: status, parsed

        limit = GREMLIN_POLICY_RECEIPT_LIMIT
        call get_environment_variable('FO_GREMLIN_RECEIPT_LIMIT', value, status=status)
        if (status /= 0) return
        read (value, *, iostat=status) parsed
        if (status /= 0) return
        limit = max(GREMLIN_MIN_RECEIPT_LIMIT, min(GREMLIN_POLICY_RECEIPT_LIMIT, parsed))
    end function receipt_limit


    logical function gate_receipt_matches(record, generation, requirement, case_name, &
            outcome)
        character(len=*), intent(in) :: record, generation, requirement
        character(len=*), intent(out) :: case_name, outcome
        character(len=HASH_LEN) :: receipt_generation, receipt_requirement
        character(len=16) :: gate_required

        case_name = ''
        outcome = ''
        call gremlin_json_field(record, 'generation', receipt_generation)
        call gremlin_json_field(record, 'requirement_digest', receipt_requirement)
        call gremlin_json_field(record, 'gate_required', gate_required)
        call gremlin_json_field(record, 'case_id', case_name)
        call gremlin_json_field(record, 'status', outcome)
        gate_receipt_matches = trim(receipt_generation) == trim(generation) .and. &
            trim(receipt_requirement) == trim(requirement) .and. &
            trim(gate_required) == 'true' .and. len_trim(case_name) > 0 .and. &
            (trim(outcome) == 'PASS' .or. trim(outcome) == 'SKIP' .or. &
            status_is_failure(trim(outcome)))
    end function gate_receipt_matches

    subroutine record_receipt(session, request, generation, case_name, exitcode, &
            outcome, sequence, case_index, seed, log_file, ierr, message, &
            credit_coverage, gate_required, reused_from)
        !! Journal one case (or `<build>`) receipt. A campaign receipt (the
        !! default) is credited to coverage: it goes to the lane's campaign
        !! ledger and the case index first, then to the session journal.
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: case_name, outcome, log_file
        integer, intent(in) :: exitcode, sequence, case_index, seed
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        logical, intent(in), optional :: credit_coverage, gate_required
        !! The receipt reuses this earlier generation's PASS (no run).
        character(len=*), intent(in), optional :: reused_from

        type(json_builder_t) :: jb
        type(gremlin_coverage_view_t) :: coverage_view
        character(len=HASH_LEN) :: closure, oracle
        character(len=256) :: resolves
        character(len=256) :: completion_id
        character(len=PATH_LEN) :: journal_path
        character(len=32) :: size_text
        character(len=:), allocatable :: record
        logical :: campaign, credit
        integer :: j, k

        write (completion_id, '(a,a,a,a,i0,a,i0)') trim(session%session_id), '-', &
            generation%identity(1:12), '-', sequence, '-', case_index
        campaign = .true.
        if (present(credit_coverage)) campaign = credit_coverage
        credit = campaign .and. coverage_outcome(outcome)
        if (credit) then
            call coverage_read_view_path(coverage_state_path(session, &
                generation%identity), generation%identity, coverage_view, ierr, message)
            if (ierr /= COVERAGE_OK) return
        end if
        ! Case receipts carry the case input closure digest (reuse key) and
        ! whether the case ran or reused an earlier PASS.
        closure = ''
        if (case_name /= '<build>') closure = generation_case_digest(generation, case_name)
        oracle = generation_oracle_digest(generation, case_name)
        resolves = ''
        if (campaign .and. outcome == 'PASS' .and. len_trim(oracle) == HASH_LEN) then
            call campaign_failure_resolution(session, generation%identity, &
                case_name, oracle, resolves, ierr, message)
            if (ierr /= JOURNAL_OK) return
        end if
        write (size_text, '(i0)') generation%driver_size
        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'completion_id', trim(completion_id))
        call json_key_string(jb, 'session_id', trim(session%session_id))
        call json_key_string(jb, 'lane_id', trim(request%lane_id))
        call json_key_string(jb, 'generation', generation%identity)
        call json_key_string(jb, 'driver_path', trim(generation%driver_path))
        call json_key_string(jb, 'driver_digest', generation%driver_digest)
        call json_key(jb, 'driver_size')
        call json_value_raw(jb, trim(size_text))
        call json_key_string(jb, 'case_id', trim(case_name))
        k = 0
        if (allocated(generation%model)) k = model_find_case(generation%model, trim(case_name))
        if (k > 0) then
            associate (c => generation%model%cases(k))
                call json_key(jb, 'labels')
                call array_open(jb)
                do j = 1, size(c%labels)
                    call json_value_string(jb, c%labels(j)%s)
                end do
                call array_close(jb)
                if (c%layer > 0) then
                    call json_key_string(jb, 'layer', generation%model%layers(c%layer)%name)
                else
                    call json_key_string(jb, 'layer', '')
                end if
            end associate
        end if
        if (case_name /= '<build>') then
            call json_key_string(jb, 'oracle_identity', trim(oracle))
            if (len_trim(resolves) > 0) &
                call json_key_string(jb, 'resolves_completion_id', trim(resolves))
            if (len_trim(closure) > 0 .or. present(reused_from)) &
                call json_key_string(jb, 'closure_digest', trim(closure))
            if (present(reused_from)) then
                call json_key_string(jb, 'disposition', 'reused')
                call json_key_string(jb, 'reused_from', trim(reused_from))
            else
                call json_key_string(jb, 'disposition', 'run')
            end if
        end if
        if (outcome == 'PASS' .or. outcome == 'BUILD_PASS') then
            call json_key_string(jb, 'outcome', 'pass')
        else if (outcome == 'SKIP') then
            call json_key_string(jb, 'outcome', 'skip')
        else
            call json_key_string(jb, 'outcome', 'fail')
        end if
        call json_key_string(jb, 'status', trim(outcome))
        call json_key_int(jb, 'exitcode', exitcode)
        call json_key_int(jb, 'seed', seed)
        if (campaign) then
            call json_key_string(jb, 'evidence_kind', 'campaign')
        else
            call json_key_string(jb, 'evidence_kind', 'reproduction')
        end if
        if (credit) then
            call json_key_int(jb, 'coverage_epoch', coverage_view%epoch)
            call json_key_string(jb, 'inventory_digest', trim(coverage_view%inventory_digest))
        end if
        if (present(gate_required)) then
            if (gate_required) then
                call json_key_bool(jb, 'gate_required', .true.)
                call json_key_string(jb, 'requirement_digest', request%requirement_digest)
            end if
        end if
        call json_key_int(jb, 'order', case_index)
        call json_key_string(jb, 'log_path', trim(log_file))
        call json_object_end(jb)
        record = json_to_string(jb)
        call gremlin_get_session_journal_path(session%project_key, request%lane_id, &
            session%session_id, journal_path, ierr, message)
        if (ierr /= 0) return
        ! The lane ledger is the selection source of truth and is written first.
        ! A restart can therefore never advance coverage from an unreceipted case.
        if (credit) then
            ! A non-PASS receipt is indexed first (a crash then only loses a
            ! reuse); a PASS only once it is in the journal.
            if (trim(outcome) /= 'PASS') then
                call case_index_record(session, case_name, generation%identity, &
                    closure, trim(outcome), ierr, message)
                if (ierr /= 0) return
            end if
            call journal_append(trim(session%state_dir)//'/campaign-journal.jsonl', &
                trim(completion_id), record, ierr, message)
            if (ierr /= JOURNAL_OK) return
            if (trim(outcome) == 'PASS') then
                call case_index_record(session, case_name, generation%identity, &
                    closure, trim(outcome), ierr, message)
                if (ierr /= 0) return
            end if
            call coverage_record_path(coverage_state_path(session, generation%identity), &
                generation%identity, case_name, outcome, ierr, message)
            if (ierr /= COVERAGE_OK) return
            call journal_compact_tail(trim(session%state_dir)// &
                '/campaign-journal.jsonl', receipt_limit(), ierr, message)
            if (ierr /= JOURNAL_OK) return
        end if
        call journal_append(trim(journal_path), trim(completion_id), record, ierr, message)
    end subroutine record_receipt

    subroutine case_history(journal_path, case_name, generation, digest, in_generation, &
            other_pass, match_verdict, match_generation, ierr, message)
        !! One pass over a receipt journal for a case: whether it has a receipt
        !! in `generation`, whether another generation holds a PASS, and the
        !! latest verdict and generation among receipts of closure `digest`
        !! outside `generation`.
        character(len=*), intent(in) :: journal_path, case_name, generation, digest
        logical, intent(out) :: in_generation, other_pass
        character(len=*), intent(out) :: match_verdict
        character(len=HASH_LEN), intent(out) :: match_generation
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(journal_record_t), allocatable :: records(:)
        character(len=HASH_LEN) :: receipt_generation, receipt_digest
        character(len=NAME_LEN) :: receipt_case
        character(len=32) :: verdict
        integer :: i

        in_generation = .false.
        other_pass = .false.
        match_verdict = ''
        match_generation = ''
        call journal_read_all(journal_path, records, ierr, message)
        if (ierr /= JOURNAL_OK) return
        do i = 1, size(records)
            call gremlin_json_field(records(i)%json, 'case_id', receipt_case)
            if (trim(receipt_case) /= trim(case_name)) cycle
            call gremlin_json_field(records(i)%json, 'generation', receipt_generation)
            if (receipt_generation == generation) then
                in_generation = .true.
                cycle
            end if
            call gremlin_json_field(records(i)%json, 'status', verdict)
            if (len_trim(receipt_generation) == HASH_LEN .and. &
                trim(verdict) == 'PASS') other_pass = .true.
            call gremlin_json_field(records(i)%json, 'closure_digest', receipt_digest)
            if (receipt_digest /= digest) cycle
            match_verdict = verdict
            match_generation = receipt_generation
        end do
    end subroutine case_history

    subroutine case_index_path(session, case_name, path)
        !! One small file per case, so a receipt costs one atomic publish
        !! whatever the number of cases; the set is bounded by the cases.
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: case_name
        character(len=*), intent(out) :: path
        character(len=max(1, len_trim(case_name))), allocatable :: parts(:)

        allocate (parts(1))
        parts(1) = trim(case_name)
        path = trim(session%state_dir)//'/case-index/'//cache_digest(parts, 1)
    end subroutine case_index_path

    subroutine case_index_read(session, case_name, entry, found)
        !! entry: latest receipt (digest, status, generation) and latest PASS
        !! (digest, generation) of the case, independent of journal compaction.
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: case_name
        type(case_index_entry_t), intent(out) :: entry
        logical, intent(out) :: found
        character(len=PATH_LEN) :: path
        character(len=NAME_LEN + 8*HASH_LEN) :: line
        integer :: unit, ios, bar(5), k, at

        found = .false.
        call case_index_path(session, case_name, path)
        open (newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) line
        close (unit)
        if (ios /= 0) return
        at = 0
        do k = 1, 5
            bar(k) = index(line(at + 1:), '|')
            if (bar(k) == 0) return
            bar(k) = bar(k) + at
            at = bar(k)
        end do
        if (line(bar(5) + 1:len_trim(line)) /= trim(case_name)) return
        entry%last_digest = line(1:bar(1) - 1)
        entry%last_status = line(bar(1) + 1:bar(2) - 1)
        entry%last_generation = line(bar(2) + 1:bar(3) - 1)
        entry%pass_digest = line(bar(3) + 1:bar(4) - 1)
        entry%pass_generation = line(bar(4) + 1:bar(5) - 1)
        found = .true.
    end subroutine case_index_read

    subroutine case_index_record(session, case_name, generation, digest, status, ierr, &
            message)
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: case_name, generation, digest, status
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(case_index_entry_t) :: entry
        character(len=PATH_LEN) :: path
        character(len=:), allocatable :: text
        character(kind=c_char), allocatable :: bytes(:)
        integer :: k
        logical :: found

        ierr = 0
        message = ''
        if (len_trim(digest) /= HASH_LEN) return
        call case_index_read(session, case_name, entry, found)
        if (.not. found) entry = case_index_entry_t()
        entry%last_digest = digest
        entry%last_status = status
        entry%last_generation = generation
        if (trim(status) == 'PASS') then
            entry%pass_digest = digest
            entry%pass_generation = generation
        end if
        text = trim(entry%last_digest)//'|'//trim(entry%last_status)//'|'// &
            trim(entry%last_generation)//'|'//trim(entry%pass_digest)//'|'// &
            trim(entry%pass_generation)//'|'//trim(case_name)//achar(10)
        allocate (bytes(len(text)))
        do k = 1, len(text)
            bytes(k) = text(k:k)
        end do
        call fs_make_dir(trim(session%state_dir)//'/case-index')
        call case_index_path(session, case_name, path)
        if (c_publish_file(trim(path)//c_null_char, bytes, &
                int(size(bytes), c_size_t)) /= 0) then
            ierr = 1
            message = 'cannot publish the case receipt index'
        end if
    end subroutine case_index_record

    subroutine case_reuse_source(session, generation, case_name, source, ierr, &
            message)
        !! The earlier generation whose PASS this case may reuse: the latest
        !! campaign receipt of the case with the same input closure digest is
        !! a PASS, and the case has no receipt in this generation yet (a later
        !! campaign epoch runs it again). '' = run the case. The case index
        !! answers without reading the journal; a case missing from it (state
        !! of an older lane) falls back to the retained journal tail.
        type(gremlin_session_t), intent(in) :: session
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: case_name
        character(len=HASH_LEN), intent(out) :: source
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(case_index_entry_t) :: entry
        character(len=HASH_LEN) :: digest, match_generation
        character(len=32) :: match_verdict
        logical :: found, in_generation, other_pass

        source = ''
        ierr = 0
        message = ''
        digest = generation_case_digest(generation, case_name)
        if (len_trim(digest) /= HASH_LEN) return
        call case_index_read(session, case_name, entry, found)
        if (found) then
            if (entry%last_generation == generation%identity) return
            if (entry%pass_digest /= digest) return
            if (entry%last_digest == digest .and. trim(entry%last_status) /= 'PASS') &
                return
            source = entry%pass_generation
            return
        end if
        call case_history(trim(session%state_dir)//'/campaign-journal.jsonl', &
            case_name, generation%identity, digest, in_generation, other_pass, &
            match_verdict, match_generation, ierr, message)
        if (ierr /= JOURNAL_OK .or. in_generation) return
        ierr = 0
        if (trim(match_verdict) == 'PASS') source = match_generation
    end subroutine case_reuse_source

    subroutine campaign_failures(session, failures, ierr, message)
        !! A same-generation PASS or a changed oracle cannot repair a failure.
        !! Keep latest generation evidence even after resolution so a delayed
        !! older receipt cannot resurrect a resolved reproducer.
        type(gremlin_session_t), intent(in) :: session
        type(campaign_failure_t), allocatable, intent(out) :: failures(:)
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(journal_record_t), allocatable :: records(:)
        type(campaign_failure_t), allocatable :: entries(:)
        character(len=HASH_LEN), allocatable :: children(:), parents(:)
        character(len=HASH_LEN) :: generation, identity
        character(len=NAME_LEN) :: name
        character(len=256) :: completion
        character(len=16) :: verdict
        character(len=PATH_LEN) :: journal_path
        integer :: i, j, n

        allocate (failures(0))
        call campaign_journal_path(session, journal_path, ierr, message)
        if (ierr /= JOURNAL_OK) return
        call journal_read_all(trim(journal_path), records, ierr, message)
        if (ierr /= JOURNAL_OK) return
        if (size(records) == 0) return
        call generation_lineage(session, children, parents, ierr, message)
        if (ierr /= LIFECYCLE_OK) return
        allocate (entries(size(records)))
        n = 0
        do i = 1, size(records)
            call gremlin_json_field(records(i)%json, 'case_id', name)
            call gremlin_json_field(records(i)%json, 'status', verdict)
            call gremlin_json_field(records(i)%json, 'generation', generation)
            call gremlin_json_field(records(i)%json, 'oracle_identity', identity)
            call gremlin_json_field(records(i)%json, 'completion_id', completion)
            if (verdict /= 'PASS' .and. verdict /= 'FAIL' .and. &
                verdict /= 'TIMEOUT') cycle
            j = 1
            do while (j <= n)
                if (entries(j)%name == name) exit
                j = j + 1
            end do
            if (j > n) then
                n = n + 1
                entries(n)%name = name
            else if (strict_descendant(entries(j)%generation, generation, &
                    children, parents)) then
                cycle
            end if
            if (verdict == 'PASS') then
                if (entries(j)%pending .and. len_trim(identity) == HASH_LEN) then
                    if (entries(j)%identity == identity .and. &
                        strict_descendant(generation, entries(j)%generation, &
                            children, parents)) entries(j)%pending = .false.
                end if
                if (entries(j)%pending) cycle
            else
                entries(j)%pending = .true.
                entries(j)%completion = completion
                entries(j)%order = i
            end if
            entries(j)%generation = generation
            entries(j)%identity = identity
        end do
        failures = entries(:n)
    end subroutine campaign_failures

    subroutine campaign_failure_resolution(session, generation, name, identity, &
            completion, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: generation, name, identity
        character(len=*), intent(out) :: completion, message
        integer, intent(out) :: ierr
        type(campaign_failure_t), allocatable :: failures(:)
        character(len=HASH_LEN), allocatable :: children(:), parents(:)
        integer :: i

        completion = ''
        call campaign_failures(session, failures, ierr, message)
        if (ierr /= JOURNAL_OK) return
        if (len_trim(identity) /= HASH_LEN) return
        call generation_lineage(session, children, parents, ierr, message)
        if (ierr /= LIFECYCLE_OK) return
        do i = 1, size(failures)
            if (.not. failures(i)%pending .or. failures(i)%name /= name) cycle
            if (failures(i)%identity /= identity) cycle
            if (strict_descendant(generation, failures(i)%generation, &
                    children, parents)) completion = failures(i)%completion
            return
        end do
    end subroutine campaign_failure_resolution

    subroutine generation_lineage(session, children, parents, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        character(len=HASH_LEN), allocatable, intent(out) :: children(:), parents(:)
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(gremlin_lifecycle_event_t), allocatable :: events(:)
        character(len=HASH_LEN), allocatable :: grown(:)
        character(len=PATH_LEN) :: lifecycle_path
        type(gremlin_request_t) :: request
        integer(int64) :: cursor, next_cursor
        integer :: i, j, n, n_events
        logical :: has_more

        allocate (children(0), parents(0))
        if (.not. allocated(session%lane_id)) then
            ierr = 1
            message = 'Gremlin session has no lifecycle lane identity'
            return
        end if
        request%lane_id = session%lane_id
        call lifecycle_path_for_session(session, request, lifecycle_path, ierr, message)
        if (ierr /= LIFECYCLE_OK) return
        cursor = 0_int64
        do
            call gremlin_lifecycle_read_page(trim(lifecycle_path), cursor, 128, &
                int(LIFECYCLE_MAX_EVENT, int64), &
                events, n_events, next_cursor, has_more, ierr, message)
            if (ierr /= LIFECYCLE_OK) return
            do i = 1, n_events
                if (events(i)%event_type /= 'generation_superseded') cycle
                if (len_trim(events(i)%generation) /= HASH_LEN .or. &
                    len_trim(events(i)%previous_generation) /= HASH_LEN) cycle
                n = size(children)
                j = 1
                do while (j <= n)
                    if (children(j) == events(i)%generation) exit
                    j = j + 1
                end do
                if (j <= n) then
                    if (parents(j) /= events(i)%previous_generation) parents(j) = ''
                    cycle
                end if
                allocate (grown(n + 1))
                grown(:n) = children
                grown(n + 1) = events(i)%generation
                call move_alloc(grown, children)
                allocate (grown(n + 1))
                grown(:n) = parents
                grown(n + 1) = events(i)%previous_generation
                call move_alloc(grown, parents)
            end do
            if (.not. has_more) exit
            if (next_cursor <= cursor) then
                ierr = 1
                message = 'lifecycle cursor did not advance during failure resolution'
                return
            end if
            cursor = next_cursor
        end do
    end subroutine generation_lineage

    logical function strict_descendant(generation, ancestor, children, parents) &
            result(descends)
        character(len=*), intent(in) :: generation, ancestor
        character(len=HASH_LEN), intent(in) :: children(:), parents(:)
        character(len=HASH_LEN) :: current
        integer :: step, j

        descends = .false.
        if (len_trim(generation) /= HASH_LEN .or. len_trim(ancestor) /= HASH_LEN) return
        if (generation == ancestor) return
        current = generation
        do step = 1, size(children)
            do j = 1, size(children)
                if (children(j) == current) exit
            end do
            if (j > size(children)) return
            current = parents(j)
            if (len_trim(current) /= HASH_LEN) return
            if (current == ancestor) then
                descends = .true.
                return
            end if
        end do
    end function strict_descendant

    subroutine prior_generation_case_passed(session, generation, case_name, passed, &
            ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: case_name
        logical, intent(out) :: passed
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(journal_record_t), allocatable :: records(:)
        character(len=HASH_LEN), allocatable :: children(:), parents(:)
        character(len=HASH_LEN) :: identity, receipt_identity, receipt_generation
        character(len=NAME_LEN) :: name
        character(len=16) :: verdict
        character(len=PATH_LEN) :: journal_path
        integer :: i

        passed = .false.
        ierr = JOURNAL_OK
        message = ''
        identity = generation_oracle_digest(generation, case_name)
        if (len_trim(identity) /= HASH_LEN) return
        call campaign_journal_path(session, journal_path, ierr, message)
        if (ierr /= JOURNAL_OK) return
        call journal_read_all(trim(journal_path), records, ierr, message)
        if (ierr /= JOURNAL_OK) return
        call generation_lineage(session, children, parents, ierr, message)
        if (ierr /= LIFECYCLE_OK) return
        do i = 1, size(records)
            call gremlin_json_field(records(i)%json, 'case_id', name)
            call gremlin_json_field(records(i)%json, 'status', verdict)
            call gremlin_json_field(records(i)%json, 'generation', receipt_generation)
            call gremlin_json_field(records(i)%json, 'oracle_identity', &
                receipt_identity)
            if (name /= case_name .or. verdict /= 'PASS') cycle
            if (identity /= receipt_identity) cycle
            if (strict_descendant(generation%identity, receipt_generation, &
                    children, parents)) then
                passed = .true.
                return
            end if
        end do
    end subroutine prior_generation_case_passed

    subroutine campaign_journal_path(session, path, ierr, message)
        !! Read and owner sessions use different state_dir views. The lane
        !! campaign ledger is addressed by project and lane identity in both.
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(out) :: path, message
        integer, intent(out) :: ierr
        character(len=PATH_LEN) :: state_dir

        path = ''
        if (.not. allocated(session%project_key) .or. &
            .not. allocated(session%lane_id)) then
            ierr = 1
            message = 'Gremlin session has no campaign journal identity'
            return
        end if
        call gremlin_session_state_dir(session%project_key, session%lane_id, &
            state_dir, ierr, message)
        if (ierr /= 0) return
        path = trim(state_dir)//'/campaign-journal.jsonl'
    end subroutine campaign_journal_path

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

    logical function status_is_failure(status)
        character(len=*), intent(in) :: status

        status_is_failure = status == 'FAIL' .or. status == 'BUILD_FAIL' .or. &
            status == 'TIMEOUT' .or. status == 'FLAKY' .or. status == 'INFRA_ERROR'
    end function status_is_failure

    logical function coverage_outcome(status)
        character(len=*), intent(in) :: status
        coverage_outcome = status == 'PASS' .or. status == 'SKIP' .or. &
            status == 'FAIL' .or. &
            status == 'TIMEOUT' .or. status == 'FLAKY' .or. &
            status == 'INFRA' .or. status == 'INFRA_ERROR' .or. &
            status == 'CANCELLED'
    end function coverage_outcome

    function runner_case_outcome(log_file, case_name, exitcode) result(outcome)
        character(len=*), intent(in) :: log_file, case_name
        integer, intent(in), optional :: exitcode
        character(len=16) :: outcome
        character(len=8192) :: line
        character(len=:), allocatable :: result_name
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
                case (EV_OBJECT_START)
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
                case (EV_ARRAY_START)
                    if (want_name .or. want_status) malformed = .true.
                    if (tests_array .and. depth == 2) malformed = .true.
                    depth = depth + 1
                    if (depth == 2 .and. want_tests) tests_array = .true.
                    want_tests = .false.
                    want_name = .false.
                    want_status = .false.
                case (EV_OBJECT_END)
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
                case (EV_ARRAY_END)
                    if (depth == 2) tests_array = .false.
                    depth = depth - 1
                case (EV_KEY)
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
                        case ('skip')
                            if (runner_skip_metadata_valid(trim(line))) outcome = 'SKIP'
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
        if (present(exitcode)) then
            if (exitcode /= 0 .and. outcome == 'SKIP') outcome = 'INFRA_ERROR'
        end if
    end function runner_case_outcome

    logical function runner_skip_metadata_valid(report) result(valid)
        character(len=*), intent(in) :: report
        character(len=9), parameter :: fields(3) = &
            [character(len=9) :: 'exit_code', 'failed', 'skipped']
        character(len=:), allocatable :: summary, raw
        type(json_event_t) :: event
        integer :: i, count, ierr, value

        valid = .false.
        call json_object_member(report, 'summary', summary, count, ierr, event)
        if (ierr /= 0) return
        if (count /= 1) return
        if (event%event_type /= EV_OBJECT_START) return
        do i = 1, size(fields)
            if (i == 1) then
                call json_object_member(report, trim(fields(i)), raw, &
                    count, ierr, event)
            else
                call json_object_member(summary, trim(fields(i)), raw, &
                    count, ierr, event)
            end if
            if (ierr /= 0) return
            if (count /= 1) return
            if (event%event_type /= JSON_INTEGER) return
            read(raw, *, iostat=ierr) value
            if (ierr /= 0) return
            if (i < 3) then
                if (value /= 0) return
            else
                if (value < 1) return
            end if
        end do
        valid = .true.
    end function runner_skip_metadata_valid

end module fo_gremlin_receipts
