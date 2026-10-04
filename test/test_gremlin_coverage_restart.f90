program test_gremlin_coverage_restart
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, file_exists, remove_path
    use fo_test_harness, only: make_directory
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_fifo, gremlin_release_fifo
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_wait_file
    use fo_test_gremlin_oracle, only: gremlin_spawn, gremlin_poll_child
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_gremlin_oracle, only: gremlin_field
    use fo_test_gremlin_oracle, only: gremlin_write_case
    use fo_test_process_identity, only: mcp_process_start_time, mcp_kill_owned_tree
    use fo_test_process_identity, only: mcp_process_identity_running
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_number_value, json_boolean_value, json_string_value
    use fo_test_json, only: json_parse, json_invalid
    implicit none

    interface
        integer(c_int) function lock_coverage(path) &
                bind(C, name='fo_c_gremlin_coverage_lock')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function lock_coverage
        subroutine unlock_coverage(descriptor) &
                bind(C, name='fo_c_gremlin_coverage_unlock')
            import :: c_int
            integer(c_int), value :: descriptor
        end subroutine unlock_coverage
    end interface

    integer, parameter :: n_cases = 40, interrupted_slot = 18
    integer, parameter :: seed = 1729
    character(len=32) :: names(n_cases), expected(n_cases)
    character(:), allocatable :: driver, scratch, project, cache, state, lane
    character(:), allocatable :: gate, ready, gate_after_marker, ready_after_marker
    character(:), allocatable :: replay_gate, replay_ready, replay_flag, fail_flag
    character(:), allocatable :: marker, owner, replacement, session
    character(:), allocatable :: marker_text, child_text, generation, state_dir
    character(:), allocatable :: coverage_path, replay_stdout, replay_stderr
    character(:), allocatable :: reproduced_case, unseen_case, replay_output
    character(:), allocatable :: campaign_path, coverage_text, coverage_header
    character(:), allocatable :: inventory_digest, stale_record
    character(:), allocatable :: coverage_before_reproduce, coverage_before_replay
    character(:), allocatable :: priority_lane, priority_session
    character(len=256) :: local_message
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: report, coverage, field, events, event, event_case, event_status
    type(json_value_t) :: reproduction_report, event_identity
    integer :: owner_pid, child_pid, signal_status, attempt, i
    integer :: replay_child, replay_exit, replay_elapsed, markers_before
    integer :: header_end, separator, journal_unit
    integer(c_int64_t) :: owner_start, child_start
    integer(c_int64_t) :: replay_start
    logical :: ready_seen, interrupted_pass
    logical :: replay_done
    logical :: replay_failed_receipt, unseen_pass_receipt, final_pass_receipt
    logical :: replay_receipt_has_no_epoch
    logical :: has_proc

    inquire(file='/proc/self/stat', exist=has_proc)
    if (.not. has_proc) then
        write (*, '(a)') 'coverage restart: skipped (requires Linux /proc)'
        stop
    end if
    call gremlin_setup(driver, scratch, project, cache, state)
    replay_child = 0
    replay_start = 0_c_int64_t
    child_pid = -1
    child_start = 0_c_int64_t
    write (*, '(a)') 'coverage restart fixture: '//scratch
    lane = 'coverage-restart-native'
    gate = scratch//'/interrupted.fifo'
    ready = scratch//'/interrupted.pid'
    gate_after_marker = scratch//'/final.fifo'
    ready_after_marker = scratch//'/final.pid'
    replay_gate = scratch//'/replay.fifo'
    replay_ready = scratch//'/replay.ready'
    replay_flag = scratch//'/replay-'
    fail_flag = scratch//'/fail-'
    marker = scratch//'/cases.log'
    call write_text(project//'/fpm.toml', 'name = "coverage_restart_probe"'//new_line('a'))
    call gremlin_fifo(gate)
    call gremlin_fifo(gate_after_marker)
    call gremlin_fifo(replay_gate)
    do i = 1, n_cases
        write(names(i), '(a,i2.2)') 'test_case_', i
    end do
    expected = names
    call shuffle(expected, seed)
    unseen_case = trim(expected(19))
    reproduced_case = trim(expected(1))
    call write_cases()

    call run_receipt_barrier()
    call run_marker_crash_barrier()

    call start_campaign()
    session = gremlin_field(report, 'session_id')
    call capture_owner(session, owner_pid, owner_start)
    if (process%exit_code /= 0) call fail_fixture()
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts the public finite-coverage campaign')
    call wait_for_progress(17, ready, 60000, ready_seen, session)
    call assert_true(ready_seen, 'exact eighteenth permutation case blocks after 17 completions')
    if (.not. ready_seen) call fail_fixture()
    marker_text = read_text(marker)
    call assert_equal_integer(line_count(marker_text), 17, &
        'the interrupted epoch has exactly seventeen independent case markers')
    call assert_marker_prefix(marker_text, expected, 17)
    call query_status(session, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'running')
    call assert_equal_integer(int(json_number_value(field)), 1, &
        'status reports one in-flight obligation')
    field = json_member(coverage, 'unknown')
    call assert_equal_integer(int(json_number_value(field)), 23, &
        'status keeps the remaining obligations unknown')
    field = json_member(coverage, 'remaining')
    call assert_equal_integer(int(json_number_value(field)), 23, &
        'status reports the exact remaining obligation count')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), 17, &
        'only completed behavior counts as PASS')

    generation = gremlin_field(report, 'active_generation')
    call gremlin_session_state_dir(project, lane, state_dir, signal_status, &
        local_message)
    call assert_equal_integer(signal_status, 0, &
        'locates the exact lane state directory')
    coverage_path = trim(state_dir)//'/coverage-'//generation//'.state'
    coverage_before_reproduce = read_text(coverage_path)
    call reproduce_case(unseen_case, '')
    call assert_true(read_text(coverage_path) == coverage_before_reproduce, &
        'unseen reproduction leaves the exact coverage ledger unchanged')
    call query_status(session, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'running')
    call assert_equal_integer(int(json_number_value(field)), 1, &
        'an unseen reproduction leaves the blocked epoch launch intact')
    field = json_member(coverage, 'unknown')
    call assert_equal_integer(int(json_number_value(field)), 23, &
        'an unseen reproduction cannot satisfy an epoch obligation')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), 17, &
        'a reproduction receipt does not add epoch PASS credit')
    marker_text = read_text(marker)
    call assert_equal_integer(line_count(marker_text), 18, &
        'the independent reproduction emits its own real case marker')
    call query_events(session, events)
    unseen_pass_receipt = .false.
    do i = 1, json_size(events)
        event = json_element(events, i)
        event_case = json_member(event, 'case_id')
        event_status = json_member(event, 'evidence_kind')
        if (json_string_value(event_case) /= unseen_case .or. &
                json_string_value(event_status) /= 'reproduction') cycle
        event_status = json_member(event, 'status')
        if (json_string_value(event_status) == 'PASS') unseen_pass_receipt = .true.
    end do
    call assert_true(unseen_pass_receipt, &
        'unseen public reproduction has its own PASS receipt')

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
    field = json_member(coverage, 'running')
    call assert_equal_integer(int(json_number_value(field)), 0, &
        'cancelled launch is no longer running')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), 17, &
        'graceful stop retains the seventeen completed cases')
    field = json_member(coverage, 'remaining')
    call assert_equal_integer(int(json_number_value(field)), 23, &
        'graceful stop preserves all twenty-three remaining obligations')
    call assert_true(index(read_text(coverage_path), &
        trim(expected(interrupted_slot))//'|CANCELLED') > 0, &
        'coverage ledger records the interrupted launch as CANCELLED')
    call remove_path(ready)
    owner_pid = -1
    owner_start = 0_c_int64_t
    call start_campaign()
    replacement = gremlin_field(report, 'session_id')
    call capture_owner(replacement, owner_pid, owner_start)
    if (process%exit_code /= 0) call fail_fixture()
    call assert_true(len(replacement) > 0, 'restarts the stopped coverage lane')
    call wait_for_progress(17, ready, 60000, ready_seen, replacement)
    call assert_true(ready_seen, 'replacement owner reaches the interrupted slot')
    if (.not. ready_seen) call fail_fixture()
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
    call gremlin_session_state_dir(project, lane, state_dir, signal_status, &
        local_message)
    call assert_equal_integer(signal_status, 0, &
        'resolves campaign state for stale-generation receipt injection')
    if (signal_status /= 0) call fail_fixture()
    coverage_path = trim(state_dir)//'/coverage-'//generation//'.state'
    campaign_path = trim(state_dir)//'/campaign-journal.jsonl'
    coverage_text = read_text(coverage_path)
    header_end = index(coverage_text, new_line('a'))
    if (header_end > 0) then
        coverage_header = coverage_text(:header_end - 1)
        separator = index(coverage_header, '|')
    else
        separator = 0
    end if
    call assert_true(separator > 0, &
        'coverage header exposes its independent inventory digest')
    if (separator <= 0) call fail_fixture()
    inventory_digest = coverage_header(separator + 1:)
    call assert_equal_integer(len(inventory_digest), 64, &
        'stale receipt uses the exact generation inventory identity')
    stale_record = '{"completion_id":"stale-generation-pass","session_id":"'// &
        session//'","lane_id":"'//lane//'","generation":"'// &
        repeat('f', 64)//'","case_id":"'//trim(expected(interrupted_slot))// &
        '","outcome":"pass","status":"PASS","exitcode":0,"seed":1729,'// &
        '"coverage_epoch":1,"inventory_digest":"'//inventory_digest// &
        '","order":18,"log_path":"stale"}'
    open(newunit=journal_unit, file=campaign_path, status='unknown', &
        position='append', action='write', iostat=signal_status)
    call assert_equal_integer(signal_status, 0, &
        'opens journal for a stale-generation receipt')
    if (signal_status /= 0) call fail_fixture()
    write(journal_unit, '(a)', iostat=signal_status) trim(stale_record)
    call assert_equal_integer(signal_status, 0, &
        'writes the stale-generation receipt')
    close(journal_unit, iostat=signal_status)
    call assert_equal_integer(signal_status, 0, &
        'closes the stale-generation receipt journal')
    if (signal_status /= 0) call fail_fixture()
    call assert_true(repeat('f', 64) /= generation, &
        'stale receipt generation differs from the active generation')
    call assert_true(index(read_text(campaign_path), &
        'stale-generation-pass') > 0, &
        'stale PASS exists in the recovery journal before restart')
    owner_pid = -1
    owner_start = 0_c_int64_t
    call start_campaign()
    replacement = gremlin_field(report, 'session_id')
    call capture_owner(replacement, owner_pid, owner_start)
    if (process%exit_code /= 0) call fail_fixture()
    call assert_true(process%exit_code == 0 .and. len(replacement) > 0, &
        'same-lane recovery starts a replacement session')
    call assert_true(replacement /= session, 'recovery does not reuse the dead owner identity')
    call wait_for_progress(17, ready, 60000, ready_seen, replacement)
    call assert_true(ready_seen, 'restarted epoch reaches the interrupted permutation slot')
    if (.not. ready_seen) call fail_fixture()
    call query_status(replacement, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), 17, &
        'stale-generation PASS cannot satisfy the current epoch obligation')
    call write_text(replay_flag//reproduced_case, 'replay')
    call reproduction_args(reproduced_case, generation)
    replay_stdout = scratch//'/replay.stdout'
    replay_stderr = scratch//'/replay.stderr'
    call gremlin_spawn(driver, project, args, replay_stdout, replay_stderr, &
        replay_child)
    replay_start = mcp_process_start_time(replay_child)
    call assert_true(replay_start > 0_c_int64_t, &
        'captures the concurrent reproduction process identity')
    call gremlin_wait_file(replay_ready, 30000, ready_seen)
    call assert_true(ready_seen, 'same-generation reproduction reaches its FIFO')
    if (.not. ready_seen) call fail_fixture()
    call gremlin_release_fifo(gate)
    call remove_path(ready)
    call wait_for_pass_count(replacement, 18, 60000, ready_seen)
    call assert_true(ready_seen, &
        'campaign records the interrupted case while replay waits')
    call wait_for_progress(n_cases, ready_after_marker, 180000, ready_seen, replacement)
    call assert_true(ready_seen, 'all markers are present while the final child is blocked')
    if (.not. ready_seen) call fail_fixture()
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
    coverage_before_replay = read_text(coverage_path)
    call assert_true(index(coverage_before_replay, &
        trim(reproduced_case)//'|PASS') > 0, &
        'epoch ledger contains the replayed case PASS before reproduction')
    call write_text(fail_flag//reproduced_case, 'fail')
    call remove_path(replay_flag//reproduced_case)
    call gremlin_release_fifo(replay_gate)
    replay_done = .false.
    replay_elapsed = 0
    do while (.not. replay_done .and. replay_elapsed < 120000)
        call gremlin_poll_child(replay_child, replay_done, replay_exit)
        if (.not. replay_done) call gremlin_wait_ms(100)
        replay_elapsed = replay_elapsed + 100
    end do
    if (.not. replay_done) call fail_fixture()
    replay_child = 0
    replay_output = read_text(replay_stdout)//read_text(replay_stderr)
    call assert_equal_integer(replay_exit, 1, &
        'concurrent reproduction reports its controlled failure')
    call assert_true(index(replay_output, '"state":"FAIL"') > 0, &
        'concurrent reproduction publishes a separate FAIL result')
    call assert_true(read_text(coverage_path) == coverage_before_replay, &
        'concurrent reproduction leaves the exact campaign ledger unchanged')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), n_cases - 1, &
        'reproduction failure does not overwrite campaign PASS receipts')
    field = json_member(coverage, 'unknown')
    call assert_equal_integer(int(json_number_value(field)), 1, &
        'reproduction failure leaves the final epoch obligation outstanding')
    call query_status(replacement, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), n_cases - 1, &
        'fresh status preserves campaign PASS while replay fails')
    field = json_member(coverage, 'unknown')
    call assert_equal_integer(int(json_number_value(field)), 1, &
        'fresh status leaves only the blocked campaign case outstanding')
    call remove_path(fail_flag//reproduced_case)
    child_text = read_text(ready_after_marker)
    read(child_text, *, iostat=signal_status) child_pid
    call assert_true(signal_status == 0 .and. child_pid > 0, &
        'final marker fixture publishes its blocked child')
    child_start = mcp_process_start_time(child_pid)
    call gremlin_release_fifo(gate_after_marker)
    call wait_gone(child_pid, child_start, 5000)
    call remove_path(ready_after_marker)
    call wait_for_progress(n_cases, '', 180000, ready_seen, replacement)
    call assert_true(ready_seen, 'resumed finite epoch finishes all 40 cases')
    marker_text = read_text(marker)
    call assert_equal_integer(line_count(marker_text), n_cases + 3, &
        'unseen and retried reproduction executions retain independent markers')
    call assert_epoch_marker_order(marker_text)
    call query_status(replacement, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'eligible')
    call assert_equal_integer(int(json_number_value(field)), n_cases, &
        'recovered coverage retains the original eligible inventory')
    field = json_member(coverage, 'timeout')
    i = int(json_number_value(json_member(coverage, 'pass')))
    j = int(json_number_value(field))
    call assert_equal_integer(i + j, n_cases, &
        'terminal outcomes account for all forty current epoch obligations')
    field = json_member(coverage, 'unknown')
    call assert_equal_integer(int(json_number_value(field)), 0, &
        'completed epoch has no unknown obligations')
    field = json_member(coverage, 'full_coverage')
    call assert_true(json_boolean_value(field), 'status marks exact full coverage')
    call assert_epoch_receipts(session, replacement, coverage)
    call query_events(replacement, events)
    interrupted_pass = .false.
    replay_failed_receipt = .false.
    replay_receipt_has_no_epoch = .false.
    final_pass_receipt = .false.
    do i = 1, json_size(events)
        event = json_element(events, i)
        event_case = json_member(event, 'case_id')
        event_status = json_member(event, 'status')
        if (json_string_value(event_case) == trim(expected(interrupted_slot)) .and. &
            json_string_value(event_status) == 'PASS') interrupted_pass = .true.
        event_status = json_member(event, 'evidence_kind')
        if (json_string_value(event_case) == trim(expected(n_cases)) .and. &
            json_string_value(event_status) == 'campaign') then
            event_status = json_member(event, 'status')
            if (json_string_value(event_status) == 'PASS') final_pass_receipt = .true.
        end if
        if (json_string_value(event_case) == reproduced_case .and. &
            json_string_value(event_status) == 'reproduction') then
            event_status = json_member(event, 'status')
            if (json_string_value(event_status) == 'FAIL') then
                replay_failed_receipt = .true.
                event_identity = json_member(event, 'coverage_epoch')
                if (event_identity%kind == json_invalid) then
                    event_identity = json_member(event, 'inventory_digest')
                    if (event_identity%kind == json_invalid) &
                        replay_receipt_has_no_epoch = .true.
                end if
            end if
        end if
    end do
    write (*, '(a)') 'interrupted case '//trim(expected(interrupted_slot))// &
        ' has a PASS receipt: '//logical_text(interrupted_pass)
    call assert_true(interrupted_pass, 'recovered interrupted case has a PASS receipt')
    call assert_true(replay_failed_receipt, &
        'concurrent failing reproduction remains separately receipted')
    call assert_true(replay_receipt_has_no_epoch, &
        'reproduction failure receipt has no campaign epoch or input identity')
    call assert_true(final_pass_receipt, &
        'final case has a durable campaign receipt before coverage repair')

    coverage_before_replay = read_text(coverage_path)
    call write_text(fail_flag//reproduced_case, 'fail')
    call reproduction_args(reproduced_case, '')
    call gremlin_json(driver, project, cache, state, args, reproduction_report, &
        process, 120000)
    call assert_equal_integer(process%exit_code, 1, &
        'completed campaign PASS can be independently reproduced as FAIL')
    field = json_member(reproduction_report, 'state')
    call assert_true(json_string_value(field) == 'FAIL', &
        'post-completion reproduction reports its differing failure')
    call remove_path(fail_flag//reproduced_case)
    call assert_true(read_text(coverage_path) == coverage_before_replay, &
        'post-completion reproduction leaves the exact epoch ledger unchanged')
    call query_status(replacement, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'full_coverage')
    call assert_true(json_boolean_value(field), &
        'post-completion reproduction does not reopen the completed epoch')
    marker_text = read_text(marker)
    markers_before = line_count(marker_text)
    call gremlin_wait_ms(1500)
    call assert_equal_integer(line_count(read_text(marker)), markers_before, &
        'a completed epoch remains quiescent after a differing reproduction')
    call gremlin_stop_lane(driver, project, cache, state, lane, replacement)
    call query_status(replacement, report)
    generation = gremlin_field(report, 'active_generation')
    call gremlin_session_state_dir(project, lane, state_dir, signal_status, &
        local_message)
    call assert_equal_integer(signal_status, 0, 'resolves state for receipt repair')
    coverage_path = trim(state_dir)//'/coverage-'//generation//'.state'
    call set_coverage_unknown(coverage_path, trim(expected(n_cases)))
    owner_pid = -1
    owner_start = 0_c_int64_t
    call start_campaign()
    replacement = gremlin_field(report, 'session_id')
    call capture_owner(replacement, owner_pid, owner_start)
    if (process%exit_code /= 0) call fail_fixture()
    call assert_true(len(replacement) > 0, 'reopens completed epoch state')
    call query_status(replacement, report)
    coverage = json_member(report, 'coverage')
    field = json_member(coverage, 'pass')
    call assert_equal_integer(int(json_number_value(field)), n_cases, &
        'restart repairs coverage from its durable campaign receipt')
    field = json_member(coverage, 'unknown')
    call assert_equal_integer(int(json_number_value(field)), 0, &
        'receipt-before-coverage recovery restores the exact complete epoch')
    call assert_equal_integer(line_count(read_text(marker)), markers_before, &
        'reconciliation does not rerun a receipted case')
    call gremlin_wait_ms(1500)
    call assert_equal_integer(line_count(read_text(marker)), markers_before, &
        'repaired completed epoch remains quiescent without a repeat')
    call gremlin_stop_lane(driver, project, cache, state, lane, replacement)
    call run_priority_jump()
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine run_priority_jump()
        type(process_result_t) :: start_result
        type(json_value_t) :: priority_report, priority_coverage, priority_events
        type(json_value_t) :: priority_event, value
        character(:), allocatable :: target, active_generation, status
        character(:), allocatable :: marker_text
        character(len=32) :: actual(n_cases)
        logical :: timed_out(n_cases), reached
        integer :: i, j, target_slot, timeout_count, actual_count, markers_before
        integer :: case_position

        priority_lane = 'coverage-priority-jump'
        target = trim(names(1))
        target_slot = 0
        do i = 1, n_cases
            if (trim(expected(i)) == target) target_slot = i
        end do
        call assert_true(target_slot >= 5, &
            'priority target is later than the initial four-case chunk')
        if (target_slot < 5) call finish_assertions(retain_failed_scratch=.true.)

        call write_text(marker, '')
        call gremlin_write_case(project, trim(expected(interrupted_slot)), &
            trim(marker_body(trim(expected(interrupted_slot)))))
        call gremlin_write_case(project, trim(expected(n_cases)), &
            trim(marker_body(trim(expected(n_cases)))))
        child_pid = -1
        child_start = 0_c_int64_t
        call start_priority_campaign(target, priority_report, start_result)
        priority_session = gremlin_field(priority_report, 'session_id')
        call capture_owner(priority_session, owner_pid, owner_start)
        if (start_result%exit_code /= 0 .or. len(priority_session) == 0) &
            call fail_fixture()
        call wait_for_priority_coverage(priority_session, reached, priority_report)
        call assert_true(reached, 'priority epoch records all forty terminal outcomes')
        if (.not. reached) call fail_fixture()

        priority_coverage = json_member(priority_report, 'coverage')
        active_generation = gremlin_field(priority_report, 'active_generation')
        call assert_epoch_receipts(priority_session, priority_session, &
            active_generation, priority_coverage, priority_lane)
        value = json_member(priority_coverage, 'eligible')
        call assert_equal_integer(int(json_number_value(value)), n_cases, &
            'priority status retains all forty eligible cases')
        value = json_member(priority_coverage, 'pass')
        i = int(json_number_value(value))
        value = json_member(priority_coverage, 'timeout')
        j = int(json_number_value(value))
        call assert_equal_integer(i + j, n_cases, &
            'priority status distinguishes terminal PASS and TIMEOUT outcomes')
        value = json_member(priority_coverage, 'unknown')
        call assert_equal_integer(int(json_number_value(value)), 0, &
            'priority epoch has no unknown obligations')
        value = json_member(priority_coverage, 'remaining')
        call assert_equal_integer(int(json_number_value(value)), 0, &
            'priority epoch has no remaining obligations')
        value = json_member(priority_coverage, 'full_coverage')
        call assert_true(json_boolean_value(value), &
            'priority epoch reaches full terminal coverage')

        call query_events(priority_session, priority_events, priority_lane)
        timed_out = .false.
        timeout_count = 0
        do i = 1, json_size(priority_events)
            priority_event = json_element(priority_events, i)
            if (.not. same_epoch_receipt(priority_event, active_generation, &
                    priority_coverage)) cycle
            value = json_member(priority_event, 'case_id')
            target = json_string_value(value)
            value = json_member(priority_event, 'status')
            status = json_string_value(value)
            if (status /= 'TIMEOUT') cycle
            do j = 1, n_cases
                if (target == trim(names(j))) then
                    if (.not. timed_out(j)) timeout_count = timeout_count + 1
                    timed_out(j) = .true.
                end if
            end do
        end do
        value = json_member(priority_coverage, 'green')
        call assert_true(json_boolean_value(value) .eqv. (timeout_count == 0), &
            'any non-PASS terminal outcome prevents a green status')

        marker_text = read_text(marker)
        actual_count = collect_marker_names(marker_text, actual)
        call assert_equal_integer(actual_count, n_cases - timeout_count, &
            'priority marker count excludes terminal timeout cases')
        if (actual_count > 0) then
            call assert_equal_string(trim(actual(1)), trim(names(1)), &
                'configured priority target runs before its shuffled slot')
        end if
        j = 0
        do i = 1, n_cases
            if (trim(expected(i)) == trim(names(1))) cycle
            case_position = case_index(trim(expected(i)))
            if (case_position > 0) then
                if (timed_out(case_position)) cycle
            end if
            j = j + 1
            if (j + 1 <= actual_count) call assert_equal_string(trim(actual(j + 1)), &
                trim(expected(i)), 'priority execution preserves remaining permutation')
        end do
        call assert_unique_marker_names(actual, actual_count)
        markers_before = actual_count
        call gremlin_wait_ms(1500)
        call assert_equal_integer(line_count(read_text(marker)), markers_before, &
            'priority epoch quiesces without repeating a completed case')
        call gremlin_stop_lane(driver, project, cache, state, priority_lane, &
            priority_session)
    end subroutine run_priority_jump

    subroutine start_priority_campaign(target, document, result)
        character(len=*), intent(in) :: target
        type(json_value_t), intent(out) :: document
        type(process_result_t), intent(out) :: result

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'start')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, priority_lane)
        call list_add(args, '--random-count')
        call list_add(args, '4')
        call list_add(args, '--seed')
        call list_add(args, '1729')
        call list_add(args, '--campaign-seconds')
        call list_add(args, '60')
        call list_add(args, '--timeout-seconds')
        call list_add(args, '5')
        call list_add(args, '--target')
        call list_add(args, target)
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, document, result, &
            30000)
    end subroutine start_priority_campaign

    subroutine wait_for_priority_coverage(owner_id, reached, document)
        character(len=*), intent(in) :: owner_id
        logical, intent(out) :: reached
        type(json_value_t), intent(out) :: document
        type(json_value_t) :: current_coverage, passes, timeouts, eligible
        integer :: elapsed, pass_count, timeout_count, eligible_count

        reached = .false.
        elapsed = 0
        do while (elapsed < 180000)
            call query_status(owner_id, document, priority_lane)
            current_coverage = json_member(document, 'coverage')
            passes = json_member(current_coverage, 'pass')
            timeouts = json_member(current_coverage, 'timeout')
            eligible = json_member(current_coverage, 'eligible')
            pass_count = int(json_number_value(passes))
            timeout_count = int(json_number_value(timeouts))
            eligible_count = int(json_number_value(eligible))
            if (eligible_count == n_cases .and. &
                    pass_count + timeout_count == n_cases) then
                reached = .true.
                return
            end if
            call gremlin_wait_ms(100)
            elapsed = elapsed + 100
        end do
    end subroutine wait_for_priority_coverage

    logical function same_epoch_receipt(event, generation_id, status_coverage)
        type(json_value_t), intent(in) :: event, status_coverage
        character(len=*), intent(in) :: generation_id
        type(json_value_t) :: value

        same_epoch_receipt = .false.
        value = json_member(event, 'evidence_kind')
        if (json_string_value(value) /= 'campaign') return
        value = json_member(event, 'generation')
        if (json_string_value(value) /= generation_id) return
        value = json_member(event, 'coverage_epoch')
        if (int(json_number_value(value)) /= &
                int(json_number_value(json_member(status_coverage, 'epoch')))) return
        value = json_member(event, 'inventory_digest')
        if (json_string_value(value) /= &
                json_string_value(json_member(status_coverage, &
                    'inventory_digest'))) return
        value = json_member(event, 'seed')
        if (int(json_number_value(value)) /= &
                int(json_number_value(json_member(status_coverage, 'seed')))) return
        same_epoch_receipt = .true.
    end function same_epoch_receipt

    integer function collect_marker_names(text, values) result(count)
        character(len=*), intent(in) :: text
        character(len=32), intent(out) :: values(:)
        integer :: i, first

        values = ''
        count = 0
        first = 1
        do i = 1, len(text)
            if (text(i:i) /= new_line('a')) cycle
            if (i <= first) then
                first = i + 1
                cycle
            end if
            if (count < size(values)) then
                count = count + 1
                values(count) = text(first:i - 1)
            end if
            first = i + 1
        end do
    end function collect_marker_names

    subroutine assert_unique_marker_names(values, count)
        character(len=32), intent(in) :: values(:)
        integer, intent(in) :: count
        integer :: i, j

        do i = 1, min(count, size(values))
            do j = i + 1, min(count, size(values))
                call assert_true(trim(values(i)) /= trim(values(j)), &
                    'priority epoch does not repeat an executed case')
            end do
        end do
    end subroutine assert_unique_marker_names

    integer function case_index(case_name) result(index_case)
        character(len=*), intent(in) :: case_name
        integer :: i

        index_case = 0
        do i = 1, n_cases
            if (case_name == trim(names(i))) index_case = i
        end do
    end function case_index

    subroutine fail_fixture()
        integer :: local_status

        if (replay_child > 0 .and. replay_start > 0_c_int64_t) then
            if (mcp_process_identity_running(replay_child, replay_start)) &
                call mcp_kill_owned_tree(replay_child, replay_start, local_status)
        end if
        if (owner_pid > 0 .and. owner_start > 0_c_int64_t) then
            if (mcp_process_identity_running(owner_pid, owner_start)) &
                call mcp_kill_owned_tree(owner_pid, owner_start, local_status)
        end if
        if (child_pid > 0 .and. child_start > 0_c_int64_t) then
            if (mcp_process_identity_running(child_pid, child_start)) &
                call mcp_kill_owned_tree(child_pid, child_start, local_status)
        end if
        call finish_assertions(retain_failed_scratch=.true.)
    end subroutine fail_fixture

    subroutine capture_owner(owner_id, pid, start_time)
        character(len=*), intent(in) :: owner_id
        integer, intent(out) :: pid
        integer(c_int64_t), intent(out) :: start_time
        integer :: dash, status

        pid = -1
        start_time = 0_c_int64_t
        dash = index(owner_id, '-')
        if (dash <= 1) return
        read(owner_id(:dash - 1), *, iostat=status) pid
        if (status /= 0 .or. pid <= 0) return
        start_time = mcp_process_start_time(pid)
    end subroutine capture_owner

    subroutine run_receipt_barrier()
        character(:), allocatable :: receipt_project, receipt_lane, receipt_marker
        character(:), allocatable :: terminal_ready, followup_ready, terminal_gate
        character(:), allocatable :: followup_gate, receipt_session, replacement
        character(:), allocatable :: generation, coverage_path, campaign_path, state_dir
        character(:), allocatable :: journal_text, coverage_text, terminal_text
        character(len=512) :: state_message
        character(len=4096) :: state_dir_buffer
        type(string_list_t) :: start_args
        type(process_result_t) :: start_result
        type(json_value_t) :: status_reply, status_coverage, status_value
        integer :: status_code, owner_pid, child_pid, followup_pid, attempt, ierr
        integer(c_int) :: lock_fd
        integer(c_int64_t) :: owner_start, child_start, followup_start
        logical :: ready_seen

        receipt_project = scratch//'/receipt-project'
        owner_pid = -1
        child_pid = -1
        followup_pid = -1
        owner_start = 0_c_int64_t
        child_start = 0_c_int64_t
        followup_start = 0_c_int64_t
        receipt_lane = 'receipt-commit-barrier'
        receipt_marker = scratch//'/receipt-markers.log'
        terminal_ready = scratch//'/terminal-ready.pid'
        followup_ready = scratch//'/followup-ready.pid'
        terminal_gate = scratch//'/terminal-release.fifo'
        followup_gate = scratch//'/followup-release.fifo'
        call make_directory(receipt_project)
        call write_text(receipt_project//'/fpm.toml', &
            'name = "gremlin_receipt_barrier_probe"'//new_line('a'))
        call gremlin_fifo(terminal_gate)
        call gremlin_fifo(followup_gate)
        call gremlin_write_case(receipt_project, 'test_receipt_terminal', &
            blocked_pass_body(terminal_ready, terminal_gate, receipt_marker, &
                'test_receipt_terminal'))
        call gremlin_write_case(receipt_project, 'test_receipt_followup', &
            blocked_pass_body(followup_ready, followup_gate, receipt_marker, &
                'test_receipt_followup'))

        call start_target_owner(start_args, receipt_project, receipt_lane, &
            'test_receipt_terminal', status_reply, start_result)
        receipt_session = gremlin_field(status_reply, 'session_id')
        call capture_owner(receipt_session, owner_pid, owner_start)
        call assert_true(start_result%exit_code == 0 .and. len(receipt_session) > 0, &
            'starts the durable receipt barrier lane')
        if (start_result%exit_code /= 0 .or. len(receipt_session) == 0) &
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        call gremlin_wait_file(terminal_ready, 60000, ready_seen)
        call assert_true(ready_seen, 'terminal test blocks immediately before exit')
        if (.not. ready_seen) then
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        if (owner_pid <= 0) then
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        call assert_true(owner_start > 0_c_int64_t, &
            'receipt barrier captures owner PID start identity')
        if (owner_start <= 0_c_int64_t) then
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        terminal_text = read_text(terminal_ready)
        child_pid = -1
        read(terminal_text, *, iostat=status_code) child_pid
        call assert_true(status_code == 0 .and. child_pid > 0, &
            'blocked terminal test publishes its PID')
        if (status_code /= 0 .or. child_pid <= 0) then
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        child_start = mcp_process_start_time(child_pid)
        call assert_true(child_start > 0_c_int64_t, &
            'receipt barrier captures child PID start identity')
        if (child_start <= 0_c_int64_t) then
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if

        call query_receipt_status(receipt_project, receipt_lane, receipt_session, &
            status_reply)
        status_value = json_member(status_reply, 'active_generation')
        generation = json_string_value(status_value)
        call assert_true(len(generation) == 64, &
            'barrier status identifies the exact coverage generation')
        if (len(generation) /= 64) then
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        call gremlin_session_state_dir(receipt_project, receipt_lane, &
            state_dir_buffer, ierr, state_message)
        call assert_true(ierr == 0, 'resolves exact public lane state directory')
        if (ierr /= 0) then
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        state_dir = trim(state_dir_buffer)
        coverage_path = state_dir//'/coverage-'//generation//'.state'
        campaign_path = state_dir//'/campaign-journal.jsonl'
        lock_fd = lock_coverage(coverage_path//c_null_char)
        call assert_true(lock_fd >= 0, &
            'holds coverage publication lock as an external barrier')
        if (lock_fd < 0) then
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if

        call gremlin_release_fifo(terminal_gate)
        attempt = 0
        do while (attempt < 1200)
            if (file_exists(campaign_path)) then
                journal_text = read_text(campaign_path)
                if (receipt_pass_count(journal_text, 'test_receipt_terminal', &
                        generation) == 1) exit
            end if
            call gremlin_wait_ms(25)
            attempt = attempt + 1
        end do
        call assert_true(attempt < 1200, &
            'observes one durable PASS receipt before coverage publication')
        if (attempt >= 1200) then
            call unlock_coverage(lock_fd)
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        coverage_text = read_text(coverage_path)
        call assert_true(index(coverage_text, &
            'test_receipt_terminal|RUNNING') > 0 .and. &
            index(coverage_text, 'test_receipt_terminal|PASS') == 0, &
            'durable terminal receipt is not yet credited while publication is locked')
        call wait_gone(child_pid, child_start, 5000)
        call mcp_kill_owned_tree(owner_pid, owner_start, status_code)
        call assert_true(status_code == 0, 'kills only the receipt-blocked owner tree')
        call unlock_coverage(lock_fd)
        if (status_code /= 0) then
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if

        call remove_path(terminal_gate)
        call remove_path(terminal_ready)
        call start_target_owner(start_args, receipt_project, receipt_lane, &
            'test_receipt_terminal', status_reply, start_result)
        replacement = gremlin_field(status_reply, 'session_id')
        call capture_owner(replacement, owner_pid, owner_start)
        call assert_true(start_result%exit_code == 0 .and. len(replacement) > 0, &
            'restarts coverage after the receipt-before-update owner death')
        if (start_result%exit_code /= 0 .or. len(replacement) == 0) &
            call fail_barrier(receipt_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        call gremlin_wait_file(followup_ready, 60000, ready_seen)
        call assert_true(ready_seen, 'recovery starts only the still-unknown test')
        if (.not. ready_seen) then
            call fail_barrier(replacement, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        call read_pid_file(followup_ready, followup_pid, ierr)
        if (ierr == 0 .and. followup_pid > 0) &
            followup_start = mcp_process_start_time(followup_pid)
        call query_receipt_status(receipt_project, receipt_lane, replacement, &
            status_reply)
        status_coverage = json_member(status_reply, 'coverage')
        status_value = json_member(status_coverage, 'pass')
        call assert_equal_integer(int(json_number_value(status_value)), 1, &
            'replayed durable receipt credits the terminal case exactly once')
        status_value = json_member(status_coverage, 'running')
        call assert_equal_integer(int(json_number_value(status_value)), 1, &
            'the distinct follow-up remains blocked during recovery inspection')
        journal_text = read_text(campaign_path)
        call assert_true(receipt_pass_count(journal_text, 'test_receipt_terminal', &
            generation) == 1, 'receipt recovery does not append a duplicate completion')
        call assert_equal_integer(line_count(read_text(receipt_marker)), 1, &
            'the terminal behavior marker appears once across owner recovery')
        call gremlin_stop_lane(driver, receipt_project, cache, state, receipt_lane, &
            replacement)
        call remove_path(followup_gate)
        call remove_path(followup_ready)
    end subroutine run_receipt_barrier

    subroutine run_marker_crash_barrier()
        character(:), allocatable :: marker_project, marker_lane, marker_file
        character(:), allocatable :: marker_ready, followup_ready
        character(:), allocatable :: marker_gate, followup_gate
        character(:), allocatable :: marker_session, replacement, generation
        character(:), allocatable :: marker_state_dir, coverage_path, campaign_path
        character(:), allocatable :: journal_text
        character(len=4096) :: state_dir_buffer
        character(len=512) :: state_message
        type(string_list_t) :: start_args
        type(process_result_t) :: start_result
        type(json_value_t) :: reply, coverage_view, field
        integer :: owner_pid, child_pid, followup_pid, owner_status, ierr
        integer(c_int) :: kill_status
        integer(c_int64_t) :: owner_start, child_start, followup_start
        logical :: ready_seen

        marker_project = scratch//'/marker-project'
        owner_pid = -1
        child_pid = -1
        followup_pid = -1
        owner_start = 0_c_int64_t
        child_start = 0_c_int64_t
        followup_start = 0_c_int64_t
        marker_lane = 'marker-before-receipt'
        marker_file = scratch//'/pre-receipt-markers.log'
        marker_ready = scratch//'/pre-receipt-ready.pid'
        followup_ready = scratch//'/pre-receipt-followup.pid'
        marker_gate = scratch//'/pre-receipt-release.fifo'
        followup_gate = scratch//'/pre-receipt-followup.fifo'
        call make_directory(marker_project)
        call write_text(marker_project//'/fpm.toml', &
            'name = "gremlin_pre_receipt_barrier_probe"'//new_line('a'))
        call gremlin_fifo(marker_gate)
        call gremlin_fifo(followup_gate)
        call gremlin_write_case(marker_project, 'test_marker_before_receipt', &
            marked_blocked_body(marker_ready, marker_gate, marker_file, &
                'test_marker_before_receipt'))
        call gremlin_write_case(marker_project, 'test_marker_followup', &
            blocked_pass_body(followup_ready, followup_gate, marker_file, &
                'test_marker_followup'))

        call start_target_owner(start_args, marker_project, marker_lane, &
            'test_marker_before_receipt', reply, start_result)
        marker_session = gremlin_field(reply, 'session_id')
        call capture_owner(marker_session, owner_pid, owner_start)
        call assert_true(start_result%exit_code == 0 .and. len(marker_session) > 0, &
            'starts the marker-before-receipt lane')
        if (start_result%exit_code /= 0 .or. len(marker_session) == 0) &
            call fail_barrier(marker_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        call gremlin_wait_file(marker_ready, 60000, ready_seen)
        call assert_true(ready_seen, 'case publishes marker then blocks before exit')
        if (.not. ready_seen) then
            call fail_barrier(marker_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        call assert_equal_integer(line_count(read_text(marker_file)), 1, &
            'independent marker exists before any terminal receipt')
        if (owner_pid <= 0) then
            call fail_barrier(marker_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        call assert_true(owner_start > 0_c_int64_t, &
            'marker barrier captures owner PID start identity')
        if (owner_start <= 0_c_int64_t) then
            call fail_barrier(marker_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        call read_pid_file(marker_ready, child_pid, owner_status)
        call assert_true(owner_status == 0, 'reads blocked marker child PID')
        if (owner_status /= 0 .or. child_pid <= 0) then
            call fail_barrier(marker_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        child_start = mcp_process_start_time(child_pid)
        call assert_true(child_start > 0_c_int64_t, &
            'marker barrier captures child PID start identity')
        if (child_start <= 0_c_int64_t) then
            call fail_barrier(marker_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        call query_receipt_status(marker_project, marker_lane, marker_session, reply)
        call assert_true(gremlin_field(reply, 'active_generation') /= '', &
            'marker test status names its current generation')
        generation = gremlin_field(reply, 'active_generation')
        coverage_view = json_member(reply, 'coverage')
        field = json_member(coverage_view, 'pass')
        call assert_equal_integer(int(json_number_value(field)), 0, &
            'marker alone does not create a PASS receipt')
        field = json_member(coverage_view, 'running')
        call assert_equal_integer(int(json_number_value(field)), 1, &
            'marked case remains RUNNING before its terminal receipt')

        call mcp_kill_owned_tree(owner_pid, owner_start, kill_status)
        call assert_true(kill_status == 0, &
            'kills exact owner tree before terminal receipt')
        if (kill_status /= 0) then
            call fail_barrier(marker_session, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        call wait_gone(owner_pid, owner_start, 5000)
        call wait_gone(child_pid, child_start, 5000)
        call query_receipt_status(marker_project, marker_lane, marker_session, reply)
        coverage_view = json_member(reply, 'coverage')
        field = json_member(coverage_view, 'pass')
        call assert_equal_integer(int(json_number_value(field)), 0, &
            'marker-only crash has no durable PASS')
        field = json_member(coverage_view, 'running')
        call assert_equal_integer(int(json_number_value(field)), 0, &
            'crashed marker-only launch is no longer running')
        field = json_member(coverage_view, 'unknown')
        call assert_equal_integer(int(json_number_value(field)), 1, &
            'marker-before-receipt crash leaves the obligation UNKNOWN')
        field = json_member(coverage_view, 'remaining')
        call assert_equal_integer(int(json_number_value(field)), 1, &
            'marker-before-receipt crash preserves one remaining obligation')
        call remove_path(marker_ready)
        call start_target_owner(start_args, marker_project, marker_lane, &
            'test_marker_before_receipt', reply, start_result)
        replacement = gremlin_field(reply, 'session_id')
        call capture_owner(replacement, owner_pid, owner_start)
        call assert_true(start_result%exit_code == 0 .and. len(replacement) > 0, &
            'restarts the lane with its unreceipted case outstanding')
        if (start_result%exit_code /= 0 .or. len(replacement) == 0) &
            call fail_barrier(replacement, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        call gremlin_wait_file(marker_ready, 60000, ready_seen)
        call assert_true(ready_seen, 'recovery reruns the case that had only a marker')
        if (.not. ready_seen) then
            call fail_barrier(replacement, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        call assert_equal_integer(line_count(read_text(marker_file)), 2, &
            'the unreceipted behavior is executed again after owner death')
        call query_receipt_status(marker_project, marker_lane, replacement, reply)
        coverage_view = json_member(reply, 'coverage')
        field = json_member(coverage_view, 'pass')
        call assert_equal_integer(int(json_number_value(field)), 0, &
            'restart does not infer PASS from either pre-receipt marker')
        field = json_member(coverage_view, 'running')
        call assert_equal_integer(int(json_number_value(field)), 1, &
            'rerun stays RUNNING until the terminal receipt is durable')
        call gremlin_release_fifo(marker_gate)
        call gremlin_wait_file(followup_ready, 60000, ready_seen)
        call assert_true(ready_seen, &
            'follow-up blocks after the retried terminal receipt')
        if (.not. ready_seen) then
            call fail_barrier(replacement, owner_start, child_pid, &
                child_start, followup_pid, followup_start)
        end if
        call read_pid_file(followup_ready, followup_pid, ierr)
        if (ierr == 0 .and. followup_pid > 0) &
            followup_start = mcp_process_start_time(followup_pid)
        call query_receipt_status(marker_project, marker_lane, replacement, reply)
        coverage_view = json_member(reply, 'coverage')
        field = json_member(coverage_view, 'pass')
        call assert_equal_integer(int(json_number_value(field)), 1, &
            'only the completed rerun earns one PASS credit')
        call assert_equal_integer(line_count(read_text(marker_file)), 2, &
            'retry has two markers but only one completed PASS')
        call gremlin_session_state_dir(marker_project, marker_lane, state_dir_buffer, &
            ierr, state_message)
        call assert_true(ierr == 0, 'resolves marker test campaign journal directory')
        marker_state_dir = trim(state_dir_buffer)
        campaign_path = marker_state_dir//'/campaign-journal.jsonl'
        journal_text = read_text(campaign_path)
        call assert_true(receipt_pass_count(journal_text, &
            'test_marker_before_receipt', generation) == 1, &
            'marker case has one terminal PASS receipt after retry')
        call gremlin_stop_lane(driver, marker_project, cache, state, marker_lane, &
            replacement)
        call remove_path(marker_gate)
        call remove_path(followup_gate)
        call remove_path(marker_ready)
        call remove_path(followup_ready)
    end subroutine run_marker_crash_barrier

    subroutine fail_barrier(owner_id, owner_identity, child_one, child_one_start, &
            child_two, child_two_start)
        character(len=*), intent(in) :: owner_id
        integer(c_int64_t), intent(in) :: owner_identity
        integer, intent(in) :: child_one, child_two
        integer(c_int64_t), intent(in) :: child_one_start, child_two_start
        integer :: pid, dash, ios, kill_status

        pid = -1
        dash = index(owner_id, '-')
        if (dash > 1) read(owner_id(:dash - 1), *, iostat=ios) pid
        if (child_one > 0 .and. child_one_start > 0_c_int64_t) then
            if (mcp_process_identity_running(child_one, child_one_start)) &
                call mcp_kill_owned_tree(child_one, child_one_start, kill_status)
        end if
        if (child_two > 0 .and. child_two_start > 0_c_int64_t) then
            if (mcp_process_identity_running(child_two, child_two_start)) &
                call mcp_kill_owned_tree(child_two, child_two_start, kill_status)
        end if
        if (pid > 0 .and. owner_identity > 0_c_int64_t) then
            if (mcp_process_identity_running(pid, owner_identity)) &
                call mcp_kill_owned_tree(pid, owner_identity, kill_status)
        end if
        call finish_assertions(retain_failed_scratch=.true.)
    end subroutine fail_barrier

    function marked_blocked_body(ready_path, gate_path, marker_path, case_name) &
            result(body)
        character(len=*), intent(in) :: ready_path, gate_path, marker_path, case_name
        character(len=2048) :: body

        body = 'integer :: unit, gate_unit'//new_line('a')//'character :: token'// &
            new_line('a')//'interface'//new_line('a')// &
            'integer function getpid() bind(C, name="getpid")'//new_line('a')// &
            'end function getpid'//new_line('a')//'end interface'//new_line('a')// &
            'open(newunit=unit, file="'//marker_path//'", status="unknown", '// &
            'position="append")'//new_line('a')//'write(unit, "(a)") "'// &
            case_name//'"'//new_line('a')//'close(unit)'//new_line('a')// &
            'open(newunit=unit, file="'//ready_path//'", status="replace")'// &
            new_line('a')//'write(unit, "(i0)") getpid()'//new_line('a')// &
            'close(unit)'//new_line('a')//'open(newunit=gate_unit, file="'// &
            gate_path//'", status="old", access="stream", form="unformatted", '// &
            'action="read")'//new_line('a')//'read(gate_unit) token'//new_line('a')// &
            'close(gate_unit)'
    end function marked_blocked_body

    subroutine read_pid_file(path, pid, status)
        character(len=*), intent(in) :: path
        integer, intent(out) :: pid, status
        character(:), allocatable :: text

        pid = -1
        text = read_text(path)
        read(text, *, iostat=status) pid
    end subroutine read_pid_file

    subroutine start_target_owner(start_args, receipt_project, receipt_lane, target, &
            reply, result)
        type(string_list_t), intent(out) :: start_args
        character(len=*), intent(in) :: receipt_project, receipt_lane, target
        type(json_value_t), intent(out) :: reply
        type(process_result_t), intent(out) :: result

        call gremlin_start_args(start_args, receipt_project, receipt_lane, target)
        call list_add(start_args, '--random-count')
        call list_add(start_args, '1')
        call list_add(start_args, '--seed')
        call list_add(start_args, '7331')
        call list_add(start_args, '--json')
        call gremlin_json(driver, receipt_project, cache, state, start_args, reply, &
            result, 30000)
    end subroutine start_target_owner

    function blocked_pass_body(ready_path, gate_path, marker_path, case_name) &
            result(body)
        character(len=*), intent(in) :: ready_path, gate_path, marker_path, case_name
        character(len=2048) :: body

        body = 'integer :: unit, gate_unit'//new_line('a')//'character :: token'// &
            new_line('a')//'interface'//new_line('a')// &
            'integer function getpid() bind(C, name="getpid")'//new_line('a')// &
            'end function getpid'//new_line('a')//'end interface'//new_line('a')// &
            'open(newunit=unit, file="'//ready_path//'", status="replace")'// &
            new_line('a')//'write(unit, "(i0)") getpid()'//new_line('a')// &
            'close(unit)'//new_line('a')//'open(newunit=gate_unit, file="'// &
            gate_path//'", status="old", access="stream", form="unformatted", '// &
            'action="read")'//new_line('a')//'read(gate_unit) token'//new_line('a')// &
            'close(gate_unit)'//new_line('a')//'open(newunit=unit, file="'// &
            marker_path//'", status="unknown", position="append")'//new_line('a')// &
            'write(unit, "(a)") "'//case_name//'"'//new_line('a')//'close(unit)'
    end function blocked_pass_body

    subroutine query_receipt_status(project_dir, lane_id, owner_id, document)
        character(len=*), intent(in) :: project_dir, lane_id, owner_id
        type(json_value_t), intent(out) :: document
        type(string_list_t) :: status_args
        type(process_result_t) :: status_process

        call list_add(status_args, 'gremlin')
        call list_add(status_args, 'status')
        call list_add(status_args, '--dir')
        call list_add(status_args, project_dir)
        call list_add(status_args, '--lane')
        call list_add(status_args, lane_id)
        call list_add(status_args, '--session')
        call list_add(status_args, owner_id)
        call list_add(status_args, '--json')
        call gremlin_json(driver, project_dir, cache, state, status_args, document, &
            status_process, 10000)
        call assert_true(status_process%exit_code == 0, &
            'public status inspects the exact receipt-barrier owner')
    end subroutine query_receipt_status

    integer function receipt_pass_count(text, case_name, generation) result(count)
        character(len=*), intent(in) :: text, case_name, generation
        type(json_value_t) :: receipt, field
        character(:), allocatable :: json, message
        integer :: first, position
        logical :: valid

        count = 0
        first = 1
        do position = 1, len(text)
            if (text(position:position) /= new_line('a')) cycle
            if (position <= first) then
                first = position + 1
                cycle
            end if
            json = text(first:position - 1)
            call json_parse(json, receipt, valid, message)
            call assert_true(valid, 'durable journal record parses independently')
            if (valid) then
                field = json_member(receipt, 'case_id')
                if (json_string_value(field) /= case_name) then
                    first = position + 1
                    cycle
                end if
                field = json_member(receipt, 'generation')
                if (json_string_value(field) /= generation) then
                    first = position + 1
                    cycle
                end if
                field = json_member(receipt, 'status')
                if (json_string_value(field) == 'PASS') count = count + 1
            end if
            first = position + 1
        end do
    end function receipt_pass_count

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
        call list_add(args, '--random-count')
        call list_add(args, '4')
        call list_add(args, '--seed')
        call list_add(args, '1729')
        call list_add(args, '--campaign-seconds')
        call list_add(args, '60')
        call list_add(args, '--timeout-seconds')
        call list_add(args, '5')
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, report, process, 30000)
        call assert_true(process%exit_code == 0, 'campaign start command returns success')
    end subroutine start_campaign

    subroutine reproduce_case(case_name, generation_id)
        character(len=*), intent(in) :: case_name, generation_id
        type(json_value_t) :: response

        call reproduction_args(case_name, generation_id)
        call gremlin_json(driver, project, cache, state, args, response, &
            process, 120000)
        call assert_true(process%exit_code == 0, &
            'public reproduction completes independently of epoch credit')
    end subroutine reproduce_case

    subroutine reproduction_args(case_name, generation_id)
        character(len=*), intent(in) :: case_name, generation_id

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'reproduce')
        call list_add(args, trim(case_name))
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        if (len_trim(generation_id) > 0) then
            call list_add(args, '--generation')
            call list_add(args, trim(generation_id))
        end if
        call list_add(args, '--json')
    end subroutine reproduction_args

    subroutine assert_epoch_receipts(first_owner, resumed_owner, generation_id, &
            status_coverage, lane_id)
        character(len=*), intent(in) :: first_owner, resumed_owner, generation_id
        type(json_value_t), intent(in) :: status_coverage
        character(len=*), intent(in), optional :: lane_id
        type(json_value_t) :: documents, event, value
        character(:), allocatable :: digest, evidence, case_name, completion_id
        character(:), allocatable :: outcome
        character(len=128) :: completion_ids(128)
        logical :: case_seen(n_cases), duplicate
        integer :: epoch, seed_value, event_count, source, i, j, case_index

        value = json_member(status_coverage, 'epoch')
        epoch = int(json_number_value(value))
        value = json_member(status_coverage, 'seed')
        seed_value = int(json_number_value(value))
        value = json_member(status_coverage, 'inventory_digest')
        digest = json_string_value(value)
        case_seen = .false.
        completion_ids = ''
        event_count = 0

        do source = 1, 2
            if (source == 1) then
                call query_events(first_owner, documents, lane_id)
            else
                call query_events(resumed_owner, documents, lane_id)
            end if
            do i = 1, json_size(documents)
                event = json_element(documents, i)
                value = json_member(event, 'evidence_kind')
                evidence = json_string_value(value)
                if (evidence /= 'campaign') cycle
                value = json_member(event, 'generation')
                if (json_string_value(value) /= generation_id) cycle
                value = json_member(event, 'coverage_epoch')
                if (int(json_number_value(value)) /= epoch) cycle
                value = json_member(event, 'inventory_digest')
                if (json_string_value(value) /= digest) cycle
                value = json_member(event, 'seed')
                if (int(json_number_value(value)) /= seed_value) cycle

                value = json_member(event, 'completion_id')
                completion_id = json_string_value(value)
                duplicate = .false.
                do j = 1, event_count
                    if (trim(completion_ids(j)) == completion_id) duplicate = .true.
                end do
                if (duplicate) cycle
                call assert_true(len(completion_id) > 0, &
                    'current epoch receipt has a completion identity')
                if (event_count < size(completion_ids)) then
                    event_count = event_count + 1
                    completion_ids(event_count) = completion_id
                end if

                value = json_member(event, 'case_id')
                case_name = json_string_value(value)
                case_index = 0
                do j = 1, n_cases
                    if (case_name == trim(names(j))) case_index = j
                end do
                call assert_true(case_index > 0, &
                    'current epoch receipt belongs to an eligible case')
                if (case_index <= 0) cycle
                call assert_true(.not. case_seen(case_index), &
                    'current epoch has at most one receipt per case')
                case_seen(case_index) = .true.
                value = json_member(event, 'status')
                outcome = json_string_value(value)
                call assert_true(outcome == 'PASS' .or. outcome == 'TIMEOUT', &
                    'current epoch receipt preserves a terminal behavior outcome')
            end do
        end do

        call assert_equal_integer(event_count, n_cases, &
            'receipts cover exactly forty current generation/epoch/input/seed cases')
        do i = 1, n_cases
            call assert_true(case_seen(i), &
                'every declared case has one current epoch receipt')
        end do
    end subroutine assert_epoch_receipts

    subroutine query_status(owner_id, document, lane_id)
        character(len=*), intent(in) :: owner_id
        type(json_value_t), intent(out) :: document
        character(len=*), intent(in), optional :: lane_id
        character(:), allocatable :: selected_lane

        selected_lane = lane
        if (present(lane_id)) selected_lane = trim(lane_id)

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, selected_lane)
        call list_add(args, '--session')
        call list_add(args, owner_id)
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, document, process, 10000)
        call assert_true(process%exit_code == 0, 'status reads the selected owner session')
    end subroutine query_status

    subroutine query_events(owner_id, document, lane_id)
        character(len=*), intent(in) :: owner_id
        type(json_value_t), intent(out) :: document
        character(len=*), intent(in), optional :: lane_id
        character(:), allocatable :: selected_lane

        selected_lane = lane
        if (present(lane_id)) selected_lane = trim(lane_id)

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'events')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, selected_lane)
        call list_add(args, '--session')
        call list_add(args, owner_id)
        call list_add(args, '--cursor')
        call list_add(args, '0')
        call list_add(args, '--max-records')
        call list_add(args, '128')
        call list_add(args, '--max-bytes')
        call list_add(args, '262144')
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, report, process, 10000)
        call assert_equal_integer(process%exit_code, 0, &
            'reads public completion receipts for the selected owner')
        document = json_member(report, 'events')
    end subroutine query_events

    subroutine wait_for_pass_count(owner_id, wanted, timeout_ms, reached)
        character(len=*), intent(in) :: owner_id
        integer, intent(in) :: wanted, timeout_ms
        logical, intent(out) :: reached
        type(json_value_t) :: snapshot, current_coverage, current_pass
        integer :: elapsed

        reached = .false.
        elapsed = 0
        do while (elapsed < timeout_ms)
            call query_status(owner_id, snapshot)
            current_coverage = json_member(snapshot, 'coverage')
            current_pass = json_member(current_coverage, 'pass')
            if (int(json_number_value(current_pass)) >= wanted) then
                reached = .true.
                return
            end if
            call gremlin_wait_ms(50)
            elapsed = elapsed + 50
        end do
    end subroutine wait_for_pass_count

    subroutine set_coverage_unknown(path, case_name)
        character(len=*), intent(in) :: path, case_name
        character(:), allocatable :: text, target, changed
        integer :: at, after

        text = read_text(path)
        target = trim(case_name)//'|PASS'
        at = index(text, target)
        call assert_true(at > 0, &
            'finds durable PASS to simulate interrupted publication')
        if (at <= 0) return
        after = at + len(target)
        changed = text(:at - 1)//trim(case_name)//'|UNKNOWN'//text(after:)
        call write_text(path, changed)
        call assert_true(index(read_text(path), trim(case_name)//'|UNKNOWN') > 0, &
            'leaves the original receipt beside an UNKNOWN coverage slot')
    end subroutine set_coverage_unknown

    subroutine wait_for_progress(markers, pid_path, timeout_ms, reached, owner_id)
        integer, intent(in) :: markers, timeout_ms
        character(len=*), intent(in) :: pid_path
        logical, intent(out) :: reached
        character(len=*), intent(in) :: owner_id
        integer :: elapsed
        type(json_value_t) :: snapshot, current_coverage, current_completed
        logical :: full_coverage

        reached = .false.
        elapsed = 0
        do while (elapsed < timeout_ms)
            if (file_exists(marker)) then
                if (line_count(read_text(marker)) >= markers) then
                    if (len_trim(pid_path) == 0) then
                        if (markers < n_cases) then
                            reached = .true.
                            return
                        end if
                        if (mod(elapsed, 2000) == 0) then
                            call query_status(owner_id, snapshot)
                            current_coverage = json_member(snapshot, 'coverage')
                            current_completed = json_member(current_coverage, 'full_coverage')
                            full_coverage = json_boolean_value(current_completed)
                            if (full_coverage) then
                                reached = .true.
                                return
                            end if
                        end if
                    end if
                    if (file_exists(pid_path)) then
                        reached = .true.
                        return
                    end if
                end if
            end if
            call gremlin_wait_ms(50)
            elapsed = elapsed + 50
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
        character(len=2048) :: body

        do index_case = 1, n_cases
            if (names(index_case) == expected(interrupted_slot)) then
                body = blocked_body(trim(names(index_case)))
            else if (names(index_case) == expected(n_cases)) then
                body = marker_then_blocked_body(trim(names(index_case)))
            else if (names(index_case) == reproduced_case) then
                body = replay_marker_body(trim(names(index_case)))
            else
                body = marker_body(trim(names(index_case)))
            end if
            call gremlin_write_case(project, trim(names(index_case)), trim(body))
        end do
    end subroutine write_cases

    function marker_body(name) result(body)
        character(len=*), intent(in) :: name
        character(len=2048) :: body

        body = 'integer :: unit'//new_line('a')//'open(newunit=unit, file="'//marker// &
            '", status="unknown", position="append")'//new_line('a')// &
            'write(unit, "(a)") "'//name//'"'//new_line('a')//'close(unit)'
    end function marker_body

    function replay_marker_body(name) result(body)
        character(len=*), intent(in) :: name
        character(len=2048) :: body

        body = 'integer :: unit, replay_unit'//new_line('a')// &
            'character :: token'//new_line('a')//'logical :: flagged'//new_line('a')// &
            'inquire(file="'//replay_flag//name// '", exist=flagged)'//new_line('a')// &
            'if (flagged) then'//new_line('a')// &
            'open(newunit=unit, file="'//replay_ready// &
            '", status="replace")'//new_line('a')//'close(unit)'//new_line('a')// &
            'open(newunit=replay_unit, file="'//replay_gate// &
            '", status="old", access="stream", form="unformatted", '// &
            'action="read")'//new_line('a')// &
            'read(replay_unit) token'//new_line('a')// &
            'close(replay_unit)'//new_line('a')//'end if'//new_line('a')// &
            'open(newunit=unit, file="'//marker// &
            '", status="unknown", position="append")'//new_line('a')// &
            'write(unit, "(a)") "'//name//'"'//new_line('a')//'close(unit)'// &
            new_line('a')//'inquire(file="'//fail_flag//name// &
            '", exist=flagged)'//new_line('a')// &
            'if (flagged) error stop 1'
    end function replay_marker_body

    function blocked_body(name) result(body)
        character(len=*), intent(in) :: name
        character(len=2048) :: body

        body = 'integer :: unit, gate_unit, mark_unit'//new_line('a')//'character :: token'// &
            new_line('a')//'interface'//new_line('a')// &
            'integer function getpid() bind(C, name="getpid")'//new_line('a')// &
            'end function getpid'//new_line('a')// &
            'end interface'//new_line('a')// &
            'open(newunit=unit, file="'//ready//'", status="replace")'//new_line('a')// &
            'write(unit, "(i0)") getpid()'//new_line('a')//'close(unit)'//new_line('a')// &
            'open(newunit=gate_unit, file="'//gate// &
            '", status="old", access="stream", form="unformatted", action="read")'// &
            new_line('a')//'read(gate_unit) token'//new_line('a')//'close(gate_unit)'// &
            new_line('a')// &
            'open(newunit=mark_unit, file="'//marker// &
            '", status="unknown", position="append")'//new_line('a')// &
            'write(mark_unit, "(a)") "'//name//'"'//new_line('a')//'close(mark_unit)'
    end function blocked_body

    function marker_then_blocked_body(name) result(body)
        character(len=*), intent(in) :: name
        character(len=2048) :: body

        body = 'integer :: unit, gate_unit'//new_line('a')//'character :: token'// &
            new_line('a')//'interface'//new_line('a')// &
            'integer function getpid() bind(C, name="getpid")'//new_line('a')// &
            'end function getpid'//new_line('a')//'end interface'//new_line('a')// &
            new_line('a')//'open(newunit=unit, file="'//marker// &
            '", status="unknown", position="append")'//new_line('a')// &
            'write(unit, "(a)") "'//name//'"'//new_line('a')//'close(unit)'// &
            new_line('a')//'open(newunit=unit, file="'//ready_after_marker// &
            '", status="replace")'//new_line('a')//'write(unit, "(i0)") getpid()'// &
            new_line('a')//'close(unit)'//new_line('a')// &
            'open(newunit=gate_unit, file="'//gate_after_marker// &
            '", status="old", access="stream", form="unformatted", action="read")'// &
            new_line('a')//'read(gate_unit) token'//new_line('a')//'close(gate_unit)'
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

    subroutine assert_epoch_marker_order(text)
        character(len=*), intent(in) :: text
        character(len=32) :: actual(n_cases + 2)
        integer :: i, first, record, n_actual, replay_seen
        logical :: removed_unseen

        actual = ''
        first = 1
        record = 0
        n_actual = 0
        replay_seen = 0
        removed_unseen = .false.
        do i = 1, len(text)
            if (text(i:i) /= new_line('a')) cycle
            record = record + 1
            if (record > 17 .and. .not. removed_unseen .and. &
                    text(first:i - 1) == unseen_case) then
                removed_unseen = .true.
                first = i + 1
                cycle
            end if
            if (text(first:i - 1) == reproduced_case) then
                replay_seen = replay_seen + 1
                if (replay_seen > 1) then
                    first = i + 1
                    cycle
                end if
            end if
            n_actual = n_actual + 1
            actual(n_actual) = text(first:i - 1)
            first = i + 1
        end do
        call assert_equal_integer(n_actual, n_cases, &
            'removing isolated reproduction markers leaves forty campaign runs')
        do i = 1, min(n_actual, n_cases)
            call assert_equal_string(trim(actual(i)), trim(expected(i)), &
                'restart preserves the exact seeded campaign permutation')
        end do
    end subroutine assert_epoch_marker_order

end program test_gremlin_coverage_restart
