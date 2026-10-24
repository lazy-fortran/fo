module fo_gremlin_report
    !! The agent-facing answer of Gremlin status and wait (#218): a one-line
    !! verdict first, the counts, the failing cases with a bounded log excerpt
    !! and a log handle, and, for `since`, only what changed since a
    !! generation or an event cursor (newly failing, fixed, still failing).
    !! Cases are judged by the latest receipt of this session's journal.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_gremlin_journal, only: journal_record_t
    use fo_gremlin_log_text, only: gremlin_log_tail
    use fo_gremlin_receipts, only: status_is_failure
    use fo_gremlin_request, only: gremlin_json_field, is_hex_digest
    use fo_json_object, only: json_value_raw
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_array_start, json_array_end, json_key, &
        json_value_string, json_key_string, json_key_int, json_to_string
    implicit none
    private

    public :: gremlin_report_json

    integer, parameter :: MAX_FAILING_SHOWN = 5, MAX_DELTA_IDS = 20
    integer, parameter :: EXCERPT_BYTES = 400

    type :: case_state_t
        character(len=:), allocatable :: id, status, generation, log
    end type case_state_t

contains

    function gremlin_report_json(records, generation, since, state, health, &
            gate_green) result(report)
        !! The report object. records: the session journal in order;
        !! generation: the lane's current generation ('' for any); since: ''
        !! or a 64-digit generation or a decimal journal byte cursor.
        type(journal_record_t), intent(in) :: records(:)
        character(len=*), intent(in) :: generation, since, state, health
        logical, intent(in) :: gate_green
        character(len=:), allocatable :: report
        type(case_state_t), allocatable :: before(:), after(:)
        type(json_builder_t) :: jb
        character(len=:), allocatable :: ids_new, ids_fixed, ids_still, verdict
        integer :: n_failing, n_pass, n_new, n_fixed, n_still, shown, i, j
        integer(int64) :: cursor
        logical :: have_since, by_generation, was_failing

        by_generation = is_hex_digest(since)
        have_since = len_trim(since) > 0
        cursor = -1_int64
        i = 0
        if (have_since .and. .not. by_generation) read (since, *, iostat=i) cursor
        if (have_since .and. .not. by_generation .and. i /= 0) have_since = .false.
        after = case_states(records, generation, -1_int64)
        if (have_since) then
            if (by_generation) then
                before = case_states(records, trim(since), -1_int64)
            else
                before = case_states(records, '', cursor)
                after = case_states(records, '', -1_int64)
            end if
        else
            allocate (before(0))
        end if

        n_failing = 0
        n_pass = 0
        do i = 1, size(after)
            if (status_is_failure(after(i)%status)) n_failing = n_failing + 1
            if (after(i)%status == 'PASS') n_pass = n_pass + 1
        end do
        n_new = 0
        n_fixed = 0
        n_still = 0
        ids_new = ''
        ids_fixed = ''
        ids_still = ''
        if (have_since) then
            do i = 1, size(after)
                was_failing = .false.
                do j = 1, size(before)
                    if (before(j)%id /= after(i)%id) cycle
                    was_failing = status_is_failure(before(j)%status)
                    exit
                end do
                if (status_is_failure(after(i)%status)) then
                    if (was_failing) then
                        call push(ids_still, n_still, after(i)%id)
                    else
                        call push(ids_new, n_new, after(i)%id)
                    end if
                else if (after(i)%status == 'PASS' .and. was_failing) then
                    call push(ids_fixed, n_fixed, after(i)%id)
                end if
            end do
        end if

        verdict = verdict_text(n_failing, n_pass, state, health, gate_green)
        if (have_since) verdict = verdict//'; since '//trim(since(:min(len_trim(since), 12)))// &
            ': '//itoa(n_new)//' new, '//itoa(n_fixed)//' fixed, '//itoa(n_still)// &
            ' still failing'

        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'verdict', verdict)
        call json_key(jb, 'counts')
        call json_object_start(jb)
        call json_key_int(jb, 'cases', size(after))
        call json_key_int(jb, 'pass', n_pass)
        call json_key_int(jb, 'failing', n_failing)
        call json_object_end(jb)
        if (have_since) then
            call json_key_string(jb, 'since', trim(since))
            call json_key(jb, 'delta')
            call json_object_start(jb)
            call json_key(jb, 'newly_failing')
            call json_value_raw(jb, '['//ids_new//']')
            call json_key(jb, 'fixed')
            call json_value_raw(jb, '['//ids_fixed//']')
            call json_key(jb, 'still_failing')
            call json_value_raw(jb, '['//ids_still//']')
            call json_key_int(jb, 'newly_failing_count', n_new)
            call json_key_int(jb, 'fixed_count', n_fixed)
            call json_key_int(jb, 'still_failing_count', n_still)
            call json_object_end(jb)
        end if
        call json_key(jb, 'failing')
        call json_array_start(jb)
        shown = 0
        do i = 1, size(after)
            if (.not. status_is_failure(after(i)%status)) cycle
            if (shown >= MAX_FAILING_SHOWN) exit
            shown = shown + 1
            call json_object_start(jb)
            call json_key_string(jb, 'case_id', after(i)%id)
            call json_key_string(jb, 'status', after(i)%status)
            call json_key_string(jb, 'generation', after(i)%generation)
            call json_key_string(jb, 'log_path', after(i)%log)
            call json_key_string(jb, 'excerpt', excerpt_of(after(i)%log))
            call json_object_end(jb)
        end do
        call json_array_end(jb)
        call json_key_int(jb, 'failing_omitted', max(0, n_failing - shown))
        call json_object_end(jb)
        report = json_to_string(jb)
    end function gremlin_report_json

    function verdict_text(n_failing, n_pass, state, health, gate_green) result(text)
        integer, intent(in) :: n_failing, n_pass
        character(len=*), intent(in) :: state, health
        logical, intent(in) :: gate_green
        character(len=:), allocatable :: text
        character(len=7) :: head

        if (n_failing > 0 .or. trim(health) == 'failure') then
            head = 'FAIL'
        else if (gate_green .or. n_pass > 0) then
            head = 'PASS'
        else
            head = 'PENDING'
        end if
        text = trim(head)//': '//itoa(n_failing)//' failing, '//itoa(n_pass)// &
            ' passed, lane '//trim(state)
        if (gate_green) text = text//', gate green'
    end function verdict_text

    function case_states(records, generation, upto) result(states)
        !! The latest campaign receipt of each case, optionally of one
        !! generation and only among the records ending at or before the byte
        !! cursor `upto` (negative: all).
        type(journal_record_t), intent(in) :: records(:)
        character(len=*), intent(in) :: generation
        integer(int64), intent(in) :: upto
        type(case_state_t), allocatable :: states(:), grown(:)
        character(len=1024) :: id
        character(len=64) :: record_generation
        character(len=32) :: status, kind
        character(len=4096) :: log
        integer(int64) :: end_offset
        integer :: i, j, n

        allocate (states(0))
        end_offset = 0_int64
        do i = 1, size(records)
            end_offset = end_offset + int(len(records(i)%json), int64) + 1_int64
            if (upto >= 0_int64 .and. end_offset > upto) exit
            call gremlin_json_field(records(i)%json, 'evidence_kind', kind)
            if (trim(kind) == 'reproduction') cycle
            call gremlin_json_field(records(i)%json, 'case_id', id)
            call gremlin_json_field(records(i)%json, 'status', status)
            call gremlin_json_field(records(i)%json, 'generation', record_generation)
            if (len_trim(id) == 0 .or. len_trim(status) == 0) cycle
            if (len_trim(generation) > 0 .and. trim(id) /= '<build>' .and. &
                trim(record_generation) /= trim(generation)) cycle
            call gremlin_json_field(records(i)%json, 'log_path', log)
            n = size(states)
            do j = 1, n
                if (states(j)%id /= trim(id)) cycle
                states(j)%status = trim(status)
                states(j)%generation = trim(record_generation)
                states(j)%log = trim(log)
                exit
            end do
            if (j <= n) cycle
            allocate (grown(n + 1))
            grown(:n) = states
            ! Assign components directly: GNU Fortran 12 -O2 builds
            ! case_state_t(trim(id), ...) with wrong component lengths.
            grown(n + 1)%id = trim(id)
            grown(n + 1)%status = trim(status)
            grown(n + 1)%generation = trim(record_generation)
            grown(n + 1)%log = trim(log)
            call move_alloc(grown, states)
        end do
        ! A passing build is not a case.
        n = 0
        do j = 1, size(states)
            if (states(j)%status == 'BUILD_PASS') cycle
            n = n + 1
            if (n /= j) call move_case(states(j), states(n))
        end do
        allocate (grown(n))
        do j = 1, n
            call move_case(states(j), grown(j))
        end do
        call move_alloc(grown, states)
    end function case_states

    function excerpt_of(log) result(text)
        !! The last EXCERPT_BYTES of the case output, without the execution
        !! provenance lines that the runner appends after it.
        character(len=*), intent(in) :: log
        character(len=:), allocatable :: text
        integer :: cut

        text = gremlin_log_tail(log, 4096)
        cut = index(text, 'fo: execution cwd:')
        if (cut > 0) text = trim(text(:cut - 1))
        text = text(max(1, len(text) - EXCERPT_BYTES + 1):)
    end function excerpt_of

    subroutine move_case(from, to)
        type(case_state_t), intent(inout) :: from, to

        to%id = from%id
        to%status = from%status
        to%generation = from%generation
        to%log = from%log
    end subroutine move_case

    subroutine push(list, count, id)
        !! Append a JSON-quoted id to a comma-separated list, within the bound.
        character(len=:), allocatable, intent(inout) :: list
        integer, intent(inout) :: count
        character(len=*), intent(in) :: id
        type(json_builder_t) :: jb

        count = count + 1
        if (count > MAX_DELTA_IDS) return
        jb = json_new()
        call json_value_string(jb, id)
        if (len(list) > 0) list = list//','
        list = list//json_to_string(jb)
    end subroutine push

    function itoa(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=16) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function itoa

end module fo_gremlin_report
