program test_gremlin_coverage_restart
    use, intrinsic :: iso_c_binding, only: c_int64_t
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, file_exists, remove_path
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_gate_create, gremlin_gate_release
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_owner_pid
    use fo_test_gremlin_oracle, only: gremlin_process_running
    use fo_test_gremlin_oracle, only: gremlin_write_case
    use fo_test_process_identity, only: mcp_process_start_time, mcp_kill_owned_tree
    use fo_test_process_identity, only: mcp_process_identity_running
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_number_value, json_boolean_value, json_string_value
    use fo_test_json, only: json_invalid
    use fo_test_gremlin_oracle, only: gremlin_gate_read_source, gremlin_pid_binding
    use fo_gremlin_state, only: gremlin_session_state_dir
    implicit none

    integer, parameter :: n_cases = 40, interrupted_slot = 18
    integer, parameter :: seed = 1729
    character(len=32) :: names(n_cases), expected(n_cases)
    character(:), allocatable :: driver, scratch, project, cache, state, lane
    character(:), allocatable :: gate, ready, gate_after_marker, ready_after_marker
    character(:), allocatable :: marker, owner, replacement, session
    character(:), allocatable :: marker_text, child_text
    character(:), allocatable :: coverage_before_terminal
    character(:), allocatable :: generation, coverage_path, fail_flag
    character(:), allocatable :: reproduction_flag, reproduction_marker
    character(len=4096) :: state_dir
    character(len=256) :: state_message
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: report, coverage, field, events, event, event_case, event_status
    integer :: owner_pid, child_pid, signal_status, attempt, i
    integer(c_int64_t) :: owner_start, child_start
    logical :: ready_seen, interrupted_pass

    call gremlin_setup(driver, scratch, project, cache, state)
    write (*, '(a)') 'coverage restart fixture: '//scratch
    lane = 'coverage-restart-native'
    gate = scratch//'/interrupted.fifo'
    ready = scratch//'/interrupted.pid'
    gate_after_marker = scratch//'/final.fifo'
    ready_after_marker = scratch//'/final.pid'
    marker = scratch//'/cases.log'
    fail_flag = scratch//'/reproduction-fails'
    reproduction_flag = scratch//'/reproduction-active'
    reproduction_marker = scratch//'/reproductions.log'
    call write_text(reproduction_marker, '')
    call write_text(project//'/fpm.toml', 'name = "coverage_restart_probe"'//new_line('a')// &
        '[extra.fo]'//new_line('a')//'test-retries = 0'//new_line('a'))
    call gremlin_gate_create(gate)
    call gremlin_gate_create(gate_after_marker)
    do i = 1, n_cases
        write(names(i), '(a,i2.2)') 'test_case_', i
    end do
    expected = names
    call shuffle(expected, seed)
    call write_cases()

    call start_campaign()
    if (process%exit_code /= 0) call finish_assertions()
    session = gremlin_field(report, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts the public finite-coverage campaign')
    call wait_for_progress(17, ready, 60000, ready_seen, session)
    call assert_true(ready_seen, 'exact eighteenth permutation case blocks after 17 completions')
    if (.not. ready_seen) call finish_assertions()
    marker_text = read_text(marker)
    call assert_equal_integer(line_count(marker_text), 17, &
        'the interrupted epoch has exactly seventeen independent case markers')
    call assert_marker_prefix(marker_text, expected, 17)
    call query_status(session, report)
    generation = gremlin_field(report, 'active_generation')
    call gremlin_session_state_dir(project, lane, state_dir, signal_status, state_message)
    call assert_equal_integer(signal_status, 0, 'resolves the lane coverage state')
    if (signal_status /= 0) call finish_assertions()
    coverage_path = trim(state_dir)//'/coverage-'//generation//'.state'
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'running')
    call assert_equal_integer(int(json_number_value(field)), 1, &
        'status reports one in-flight obligation')
    field = json_member(coverage, 'unknown')
    call assert_equal_integer(int(json_number_value(field)), 23, &
        'status keeps the remaining obligations unknown')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), 17, &
        'only completed behavior counts as PASS')
    call reproduce_without_credit(trim(expected(19)), session, 0)

    owner = session
    owner_pid = -1
    signal_status = -1
    attempt = index(owner, '-')
    if (attempt > 1) read(owner(:attempt - 1), *, iostat=signal_status) owner_pid
    call assert_true(signal_status == 0 .and. owner_pid > 0, &
        'session encodes the exact owner PID')
    owner_start = mcp_process_start_time(owner_pid)
    call assert_true(owner_start > 0_c_int64_t, 'owner start identity is captured')
    child_pid = -1
    child_text = read_text(ready)
    read(child_text, *, iostat=signal_status) child_pid
    call assert_true(signal_status == 0 .and. child_pid > 0, &
        'blocked case publishes its real PID')
    child_start = mcp_process_start_time(child_pid)
    call assert_true(child_start > 0_c_int64_t, 'blocked case start identity is captured')

    call gremlin_stop_lane(driver, project, cache, state, lane, session)
    call wait_gone(child_pid, child_start, 5000)
    call query_status(session, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'cancelled')
    call assert_equal_integer(int(json_number_value(field)), 1, &
        'graceful stop durably records the interrupted launch as cancelled')
    field = json_member(coverage, 'unknown')
    call assert_equal_integer(int(json_number_value(field)), 23, &
        'graceful stop leaves the uncompleted coverage obligations unknown')
    call remove_path(ready)
    call start_campaign()
    if (process%exit_code /= 0) call finish_assertions()
    replacement = gremlin_field(report, 'session_id')
    call assert_true(len(replacement) > 0, 'restarts the stopped coverage lane')
    call wait_for_progress(17, ready, 60000, ready_seen, replacement)
    call assert_true(ready_seen, 'replacement owner reaches the interrupted slot')
    if (.not. ready_seen) call finish_assertions()
    child_text = read_text(ready)
    read(child_text, *, iostat=signal_status) child_pid
    call assert_true(signal_status == 0 .and. child_pid > 0, &
        'replacement blocked case publishes its real PID')
    child_start = mcp_process_start_time(child_pid)
    owner = replacement
    owner_pid = -1
    signal_status = -1
    attempt = index(owner, '-')
    if (attempt > 1) read(owner(:attempt - 1), *, iostat=signal_status) owner_pid
    call assert_true(signal_status == 0 .and. owner_pid > 0, &
        'replacement session encodes its owner PID')
    owner_start = mcp_process_start_time(owner_pid)
    call mcp_kill_owned_tree(owner_pid, owner_start, signal_status)
    call assert_true(signal_status == 0, 'kills exact replacement owner tree')
    call wait_gone(owner_pid, owner_start, 5000)
    call wait_gone(child_pid, child_start, 5000)
    call remove_path(ready)
    call inject_stale_receipt()
    call start_campaign()
    if (process%exit_code /= 0) call finish_assertions()
    replacement = gremlin_field(report, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(replacement) > 0, &
        'same-lane recovery starts a replacement session')
    call assert_true(replacement /= session, 'recovery does not reuse the dead owner identity')
    call wait_for_progress(17, ready, 60000, ready_seen, replacement)
    call assert_true(ready_seen, 'restarted epoch reaches the interrupted permutation slot')
    if (.not. ready_seen) call finish_assertions()
    call query_status(replacement, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), 17, &
        'a stale-generation journal PASS cannot satisfy the interrupted obligation')
    call gremlin_gate_release(gate)
    call remove_path(ready)
    call wait_for_progress(n_cases, ready_after_marker, 180000, ready_seen, replacement)
    call assert_true(ready_seen, 'all markers are present while the final child is blocked')
    if (.not. ready_seen) call finish_assertions()
    call query_status(replacement, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), n_cases - 1, &
        '40 markers precede the final PASS receipt')
    field = json_member(coverage, 'running')
    call assert_equal_integer(int(json_number_value(field)), 1, &
        'the final child remains RUNNING behind the marker')
    field = json_member(coverage, 'full_coverage')
    call assert_true(.not. json_boolean_value(field), &
        'markers alone do not satisfy the 40-case oracle')
    coverage_before_terminal = read_text(coverage_path)
    call assert_true(index(coverage_before_terminal, &
        trim(expected(n_cases))//'|RUNNING') > 0, &
        'the saved ledger captures the final child before its terminal receipt')
    call reproduce_without_credit(trim(expected(1)), replacement, 1)
    child_text = read_text(ready_after_marker)
    read(child_text, *, iostat=signal_status) child_pid
    call assert_true(signal_status == 0 .and. child_pid > 0, &
        'final marker fixture publishes its blocked child')
    child_start = mcp_process_start_time(child_pid)
    call gremlin_gate_release(gate_after_marker)
    call wait_gone(child_pid, child_start, 5000)
    call remove_path(ready_after_marker)
    call wait_for_progress(n_cases, '', 180000, ready_seen, replacement)
    call assert_true(ready_seen, 'resumed finite epoch finishes all 40 cases')
    marker_text = read_text(marker)
    call assert_equal_integer(line_count(marker_text), n_cases, &
        'completed epoch has exactly forty case executions')
    call assert_marker_prefix(marker_text, expected, n_cases)
    call query_status(replacement, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'eligible')
    call assert_equal_integer(int(json_number_value(field)), n_cases, &
        'recovered coverage retains the original eligible inventory')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), n_cases, &
        'all forty distinct behavioral obligations pass')
    field = json_member(coverage, 'unknown')
    call assert_equal_integer(int(json_number_value(field)), 0, &
        'completed epoch has no unknown obligations')
    field = json_member(coverage, 'full_coverage')
    call assert_true(json_boolean_value(field), 'status marks exact full coverage')
    args = string_list_t()
    call list_add(args, 'gremlin')
    call list_add(args, 'events')
    call list_add(args, '--dir')
    call list_add(args, project)
    call list_add(args, '--lane')
    call list_add(args, lane)
    call list_add(args, '--session')
    call list_add(args, replacement)
    call list_add(args, '--cursor')
    call list_add(args, '0')
    call list_add(args, '--max-records')
    call list_add(args, '128')
    call list_add(args, '--max-bytes')
    call list_add(args, '262144')
    call gremlin_json(driver, project, cache, state, args, report, process, 10000)
    events = json_member(report, 'events')
    interrupted_pass = .false.
    do i = 1, json_size(events)
        event = json_element(events, i)
        event_case = json_member(event, 'case_id')
        event_status = json_member(event, 'status')
        if (json_string_value(event_case) == trim(expected(interrupted_slot)) .and. &
            json_string_value(event_status) == 'PASS') interrupted_pass = .true.
    end do
    write (*, '(a)') 'interrupted case '//trim(expected(interrupted_slot))// &
        ' has a PASS receipt: '//logical_text(interrupted_pass)
    call assert_true(interrupted_pass, 'recovered interrupted case has a PASS receipt')

    call reproduce_without_credit(trim(expected(1)), replacement, 1)
    call wait_for_quiescence(replacement)
    call query_status(replacement, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'full_coverage')
    call assert_true(json_boolean_value(field), &
        'a reproduction failure does not reopen the campaign epoch')

    call gremlin_stop_lane(driver, project, cache, state, lane, replacement)
    call write_text(coverage_path, coverage_before_terminal)
    call start_campaign()
    replacement = gremlin_field(report, 'session_id')
    if (process%exit_code /= 0) call finish_assertions()
    call wait_for_progress(n_cases, '', 60000, ready_seen, replacement)
    call assert_true(ready_seen, 'restart repairs coverage from durable campaign receipts')
    call assert_equal_integer(line_count(read_text(marker)), n_cases, &
        'receipt-before-coverage recovery never reruns the receipted final case')
    call gremlin_stop_lane(driver, project, cache, state, lane, replacement)
    call finish_assertions()

contains

    subroutine wait_for_quiescence(owner_id)
        character(len=*), intent(in) :: owner_id
        type(json_value_t) :: response, satisfied

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'wait')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, owner_id)
        call list_add(args, '--until')
        call list_add(args, 'quiescent')
        call list_add(args, '--wait-ms')
        call list_add(args, '30000')
        call gremlin_json(driver, project, cache, state, args, response, process, 35000)
        satisfied = json_member(response, 'wait_satisfied')
        call assert_true(process%exit_code == 0 .and. json_boolean_value(satisfied), &
            'the completed epoch stays quiescent after the differing reproduction')
    end subroutine wait_for_quiescence

    subroutine reproduce_without_credit(case_name, owner_id, expected_exit)
        character(len=*), intent(in) :: case_name, owner_id
        integer, intent(in) :: expected_exit
        character(:), allocatable :: before, expected_status
        type(json_value_t) :: response, records, receipt, value
        integer :: record, markers_before
        logical :: found

        before = read_text(coverage_path)
        markers_before = line_count(read_text(reproduction_marker))
        expected_status = 'PASS'
        if (expected_exit /= 0) then
            expected_status = 'FAIL'
            call write_text(fail_flag, 'fail')
        end if
        call write_text(reproduction_flag, 'reproduce')
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'reproduce')
        call list_add(args, case_name)
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, owner_id)
        call gremlin_json(driver, project, cache, state, args, response, process, 120000)
        call assert_equal_integer(process%exit_code, expected_exit, &
            'independent reproduction returns its controlled process result')
        value = json_member(response, 'state')
        call assert_equal_string(json_string_value(value), expected_status, &
            'reproduction publishes its own result')
        call remove_path(reproduction_flag)
        call remove_path(fail_flag)
        call assert_equal_integer(line_count(read_text(reproduction_marker)), &
            markers_before + 1, 'independent marker proves the reproduction executed')
        call assert_equal_string(read_text(coverage_path), before, &
            'reproduction leaves the exact campaign coverage ledger unchanged')
        call query_events(owner_id, records)
        found = .false.
        do record = 1, json_size(records)
            receipt = json_element(records, record)
            value = json_member(receipt, 'evidence_kind')
            if (json_string_value(value) /= 'reproduction') cycle
            value = json_member(receipt, 'case_id')
            if (json_string_value(value) /= case_name) cycle
            value = json_member(receipt, 'status')
            if (json_string_value(value) /= expected_status) cycle
            found = .true.
            value = json_member(receipt, 'generation')
            call assert_equal_string(json_string_value(value), generation, &
                'the reproduction receipt binds the captured generation')
            value = json_member(receipt, 'coverage_epoch')
            call assert_equal_integer(value%kind, json_invalid, &
                'a reproduction receipt has no campaign epoch')
            value = json_member(receipt, 'inventory_digest')
            call assert_equal_integer(value%kind, json_invalid, &
                'a reproduction receipt has no campaign inventory credit')
        end do
        call assert_true(found, 'the independent reproduction has its own receipt')
    end subroutine reproduce_without_credit

    subroutine inject_stale_receipt()
        character(:), allocatable :: text, digest, record, journal_path
        integer :: first, last, unit, status

        text = read_text(coverage_path)
        first = index(text, '|') + 1
        last = index(text, new_line('a')) - 1
        call assert_true(first > 1 .and. last >= first, &
            'reads the coverage inventory identity independently')
        if (first <= 1 .or. last < first) call finish_assertions()
        digest = text(first:last)
        record = '{"completion_id":"stale-generation-pass","session_id":"'// &
            session//'","lane_id":"'//lane//'","generation":"'// &
            repeat('f', 64)//'","case_id":"'//trim(expected(interrupted_slot))// &
            '","outcome":"pass","status":"PASS","exitcode":0,"seed":1729,'// &
            '"coverage_epoch":1,"inventory_digest":"'//digest// &
            '","order":18,"log_path":"stale"}'
        call assert_true(generation /= repeat('f', 64), &
            'the injected receipt belongs to another generation')
        journal_path = trim(state_dir)//'/campaign-journal.jsonl'
        open(newunit=unit, file=journal_path, status='old', position='append', &
            action='write', iostat=status)
        call assert_equal_integer(status, 0, 'opens the stopped-owner campaign journal')
        if (status /= 0) call finish_assertions()
        write(unit, '(a)', iostat=status) record
        call assert_equal_integer(status, 0, 'writes the stale-generation receipt')
        close(unit)
    end subroutine inject_stale_receipt

    subroutine query_events(owner_id, records)
        character(len=*), intent(in) :: owner_id
        type(json_value_t), intent(out) :: records
        type(json_value_t) :: response

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'events')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, owner_id)
        call list_add(args, '--cursor')
        call list_add(args, '0')
        call list_add(args, '--max-records')
        call list_add(args, '128')
        call list_add(args, '--max-bytes')
        call list_add(args, '262144')
        call gremlin_json(driver, project, cache, state, args, response, process, 10000)
        call assert_equal_integer(process%exit_code, 0, 'reads the selected owner receipts')
        records = json_member(response, 'events')
    end subroutine query_events

    function logical_text(value) result(text)
        logical, intent(in) :: value
        character(len=5) :: text
        text = 'false'
        if (value) text = 'true '
    end function logical_text

    subroutine start_campaign()
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'start')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--random')
        call list_add(args, '4')
        call list_add(args, '--seed')
        call list_add(args, '1729')
        call list_add(args, '--campaign-seconds')
        call list_add(args, '60')
        call list_add(args, '--timeout-seconds')
        call list_add(args, '180')
        call gremlin_json(driver, project, cache, state, args, report, process, 30000)
        call assert_true(process%exit_code == 0, 'campaign start command returns success')
    end subroutine start_campaign

    subroutine query_status(owner_id, document)
        character(len=*), intent(in) :: owner_id
        type(json_value_t), intent(out) :: document

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--detail')
        call list_add(args, 'full')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, owner_id)
        call gremlin_json(driver, project, cache, state, args, document, process, 10000)
        call assert_true(process%exit_code == 0, 'status reads the selected owner session')
    end subroutine query_status

    subroutine wait_for_progress(markers, pid_path, stall_ms, reached, owner_id)
        !! Wait until `markers` cases have run and the blocked child published
        !! pid_path (or, without one, the epoch reports full coverage). The
        !! wait follows the lane rather than a wall-clock guess: it fails once
        !! the owner exits, or when neither the marker count nor the published
        !! lane state changes for stall_ms. A starting or building lane is
        !! progressing: its cold build of every case scales with host load and
        !! is bounded by the test's wall timeout instead.
        integer, intent(in) :: markers, stall_ms
        character(len=*), intent(in) :: pid_path
        logical, intent(out) :: reached
        character(len=*), intent(in) :: owner_id
        integer :: owner_pid, count, seen_count
        integer(c_int64_t) :: now, rate, last_progress, last_status
        type(json_value_t) :: snapshot, current_coverage, current_completed
        character(:), allocatable :: lane_state, seen_state

        reached = .false.
        owner_pid = gremlin_owner_pid(owner_id)
        seen_count = -1
        seen_state = ''
        call system_clock(last_progress, rate)
        last_status = last_progress - 2_c_int64_t*rate
        do
            count = 0
            if (file_exists(marker)) count = line_count(read_text(marker))
            if (count >= markers) then
                if (len_trim(pid_path) > 0) then
                    if (file_exists(pid_path)) reached = .true.
                else if (markers < n_cases) then
                    reached = .true.
                end if
                if (reached) return
            end if
            call system_clock(now)
            if (count /= seen_count) then
                seen_count = count
                last_progress = now
            end if
            if (now - last_status >= 2_c_int64_t*rate) then
                last_status = now
                call query_status(owner_id, snapshot)
                if (count >= markers .and. len_trim(pid_path) == 0) then
                    current_coverage = json_member(snapshot, 'coverage')
                    current_completed = json_member(current_coverage, 'full_coverage')
                    reached = json_boolean_value(current_completed)
                    if (reached) return
                end if
                lane_state = gremlin_field(snapshot, 'state')
                if (lane_state /= seen_state .or. lane_state == 'starting' .or. &
                    lane_state == 'building') last_progress = now
                seen_state = lane_state
            end if
            if (.not. gremlin_process_running(owner_pid)) return
            if ((now - last_progress)*1000_c_int64_t > int(stall_ms, c_int64_t)*rate) &
                return
            call gremlin_wait_ms(50)
        end do
    end subroutine wait_for_progress

    subroutine wait_gone(pid, expected_start, timeout_ms)
        integer, intent(in) :: pid, timeout_ms
        integer(c_int64_t), intent(in) :: expected_start
        integer :: elapsed

        elapsed = 0
        do while (elapsed < timeout_ms)
            if (.not. mcp_process_identity_running(pid, expected_start)) return
            call gremlin_wait_ms(20)
            elapsed = elapsed + 20
        end do
        call assert_true(.not. mcp_process_identity_running(pid, expected_start), &
            'recorded process identity exits within its bounded wait')
    end subroutine wait_gone

    subroutine write_cases()
        integer :: index_case
        character(:), allocatable :: body

        do index_case = 1, n_cases
            if (names(index_case) == expected(interrupted_slot)) then
                body = blocked_body(trim(names(index_case)))
            else if (names(index_case) == expected(n_cases)) then
                body = marker_then_blocked_body(trim(names(index_case)))
            else
                body = marker_body(trim(names(index_case)))
            end if
            call gremlin_write_case(project, trim(names(index_case)), trim(body))
        end do
    end subroutine write_cases

    function marker_body(name) result(body)
        character(len=*), intent(in) :: name
        character(:), allocatable :: body

        body = 'integer :: unit'//new_line('a')
        if (name == trim(expected(1)) .or. name == trim(expected(19))) then
            body = body//'logical :: replay, fail'//new_line('a')// &
                'inquire(file="'//reproduction_flag//'", exist=replay)'//new_line('a')// &
                'if (replay) then'//new_line('a')// &
                'open(newunit=unit, file="'//reproduction_marker// &
                '", status="unknown", position="append")'//new_line('a')// &
                'else'//new_line('a')
        end if
        body = body//'open(newunit=unit, file="'//marker// &
            '", status="unknown", position="append")'//new_line('a')
        if (name == trim(expected(1)) .or. name == trim(expected(19))) &
            body = body//'end if'//new_line('a')
        body = body// &
            'write(unit, "(a)") "'//name//'"'//new_line('a')//'close(unit)'
        if (name == trim(expected(1)) .or. name == trim(expected(19))) &
            body = body//new_line('a')// &
            'inquire(file="'//fail_flag//'", exist=fail)'//new_line('a')// &
            'if (fail) stop 1'
    end function marker_body

    function blocked_body(name) result(body)
        character(len=*), intent(in) :: name
        character(:), allocatable :: body

        body = 'integer :: unit, gate_unit, mark_unit'//new_line('a')//'character :: token'// &
            new_line('a')//'interface'//new_line('a')// &
            'integer function getpid() bind(C, name="'//gremlin_pid_binding()//'")'//new_line('a')// &
            'end function getpid'//new_line('a')// &
            'end interface'//new_line('a')// &
            'open(newunit=unit, file="'//ready//'", status="replace")'//new_line('a')// &
            'write(unit, "(i0)") getpid()'//new_line('a')//'close(unit)'//new_line('a')// &
            gremlin_gate_read_source(gate)//new_line('a')// &
            'open(newunit=mark_unit, file="'//marker// &
            '", status="unknown", position="append")'//new_line('a')// &
            'write(mark_unit, "(a)") "'//name//'"'//new_line('a')//'close(mark_unit)'
    end function blocked_body

    function marker_then_blocked_body(name) result(body)
        character(len=*), intent(in) :: name
        character(:), allocatable :: body

        body = 'integer :: unit, gate_unit'//new_line('a')//'character :: token'// &
            new_line('a')//'interface'//new_line('a')// &
            'integer function getpid() bind(C, name="'//gremlin_pid_binding()//'")'//new_line('a')// &
            'end function getpid'//new_line('a')//'end interface'//new_line('a')// &
            new_line('a')//'open(newunit=unit, file="'//marker// &
            '", status="unknown", position="append")'//new_line('a')// &
            'write(unit, "(a)") "'//name//'"'//new_line('a')//'close(unit)'// &
            new_line('a')//'open(newunit=unit, file="'//ready_after_marker// &
            '", status="replace")'//new_line('a')//'write(unit, "(i0)") getpid()'// &
            new_line('a')//'close(unit)'//new_line('a')// &
            gremlin_gate_read_source(gate_after_marker)
    end function marker_then_blocked_body

    subroutine shuffle(values, initial_seed)
        character(len=*), intent(inout) :: values(:)
        integer, intent(in) :: initial_seed
        integer(c_int64_t) :: state, quotient
        integer :: index_value, swap_index
        character(len=len(values(1))) :: temporary

        state = modulo(int(initial_seed, c_int64_t), 2147483646_c_int64_t) + 1
        do index_value = size(values), 2, -1
            quotient = state / 127773_c_int64_t
            state = 16807_c_int64_t*(state - quotient*127773_c_int64_t) - &
                2836_c_int64_t*quotient
            if (state <= 0_c_int64_t) state = state + 2147483647_c_int64_t
            swap_index = int(modulo(state, int(index_value, c_int64_t))) + 1
            temporary = values(index_value)
            values(index_value) = values(swap_index)
            values(swap_index) = temporary
        end do
    end subroutine shuffle

    integer function line_count(text) result(count)
        character(len=*), intent(in) :: text
        integer :: i

        count = 0
        do i = 1, len(text)
            if (text(i:i) == new_line('a')) count = count + 1
        end do
    end function line_count

    subroutine assert_marker_prefix(text, order, count)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: order(:)
        integer, intent(in) :: count
        integer :: i, first, last, record

        first = 1
        record = 0
        do i = 1, len(text)
            if (text(i:i) /= new_line('a')) cycle
            record = record + 1
            last = i - 1
            if (record <= count) then
                call assert_equal_string(text(first:last), trim(order(record)), &
                    'coverage marker follows the original seeded permutation')
            end if
            first = i + 1
        end do
        call assert_equal_integer(record, count, 'marker count matches expected obligations')
    end subroutine assert_marker_prefix

end program test_gremlin_coverage_restart
