program test_gremlin_crash_receipts
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: assert_true, assert_equal_integer, write_text
    use fo_test_harness, only: read_text, remove_path, finish_assertions, file_exists
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_field
    use fo_test_gremlin_oracle, only: gremlin_fifo, gremlin_release_fifo
    use fo_test_gremlin_oracle, only: gremlin_wait_file, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_process_identity, only: mcp_process_start_time, mcp_kill_owned_tree
    use fo_test_process_identity, only: mcp_process_identity_running
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_number_value, json_boolean_value, json_parse
    use fo_gremlin_state, only: gremlin_session_state_dir
    use fo_gremlin_session, only: gremlin_get_session_journal_path
    use fo_gremlin_journal, only: journal_append, journal_read_page, journal_record_t
    use fo_gremlin_journal, only: JOURNAL_OK, JOURNAL_MAX_RECORD_BYTES
    use fo_gremlin_coverage, only: coverage_read_view_path, COVERAGE_OK
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, owner
    character(:), allocatable :: marker, ready, gate, released
    character(:), allocatable :: old_generation, generation
    character(:), allocatable :: old_receipt, injected_receipt
    character(len=4096) :: lane_state, journal_path
    character(len=256) :: message
    type(string_list_t) :: arguments
    type(process_result_t) :: process
    type(json_value_t) :: document, coverage
    type(gremlin_coverage_view_t) :: coverage_view
    integer :: status, child_pid
    integer(c_int64_t) :: child_start
    logical :: has_proc
    character(len=*), parameter :: lane = 'crash-receipts'
    character(len=*), parameter :: case_name = 'test_crash_case'

    interface
        integer(c_int) function signal_identity(pid, start, signal) &
                bind(C, name='fo_test_signal_identity')
            import :: c_int, c_int64_t
            integer(c_int), value :: pid, signal
            integer(c_int64_t), value :: start
        end function signal_identity
    end interface

    inquire(file='/proc/self/stat', exist=has_proc)
    if (.not. has_proc) then
        write (*, '(a)') 'crash receipts: skipped (requires Linux /proc)'
        stop
    end if
    call gremlin_setup(driver, scratch, project, cache, state)
    marker = scratch//'/executions.log'
    ready = scratch//'/blocked.pid'
    gate = scratch//'/completion.fifo'
    released = scratch//'/failure-released'
    call gremlin_fifo(gate)
    call write_text(project//'/fpm.toml', 'name = "crash_receipts"'//new_line('a'))
    call write_case(.false.)

    call start_owner()
    call wait_at_marker(1)
    old_generation = gremlin_field(document, 'active_generation')
    call assert_uncredited('marker before terminal completion')
    call crash_owner()
    call assert_persisted_unknown(old_generation)
    call start_owner()
    call wait_at_marker(2)
    call assert_true(gremlin_field(document, 'active_generation') == old_generation, &
        'crash recovery retries the exact unchanged generation')
    call assert_uncredited('retry remains unknown before terminal completion')
    call gremlin_release_fifo(gate)
    call wait_until('quiescent')
    call assert_true(boolean_field(document, 'local_gate_green'), &
        'only the real retry terminal PASS discharges the gate')
    coverage = json_member(document, 'coverage')
    call assert_equal_integer(number_field(coverage, 'pass'), 1, &
        'one terminal completion credits exactly one obligation')
    call assert_equal_integer(number_field(coverage, 'unknown'), 0, &
        'completed retry leaves no stranded unknown case')
    call assert_equal_integer(line_count(read_text(marker)), 2, &
        'the interrupted marker required one real retry')
    call capture_terminal_pass()
    call gremlin_stop_lane(driver, project, cache, state, lane, owner)
    call remove_path(ready)

    call write_case(.true.)
    call start_owner()
    call wait_at_marker(3)
    generation = gremlin_field(document, 'active_generation')
    call assert_true(len(generation) == 64 .and. generation /= old_generation, &
        'changed behavior establishes a distinct current generation')
    call inject_stale_pass()
    call query_status()
    call assert_stale_visible()
    call assert_uncredited('injected old-generation PASS cannot satisfy current gate')
    call crash_owner()
    call assert_persisted_unknown(generation)
    call start_owner()
    call wait_at_marker(4)
    call assert_true(gremlin_field(document, 'active_generation') == generation, &
        'restart preserves the current generation despite stale PASS injection')
    call assert_uncredited('recovery ignores stale PASS in both durable ledgers')
    call assert_stale_visible()
    ! Native Fo diagnoses a failing test with another execution. Once this
    ! controlled retry is released, those diagnostic executions may finish too.
    call write_text(released, 'released'//new_line('a'))
    call gremlin_release_fifo(gate)
    call wait_until('failure')
    call wait_for_failure_coverage()
    call assert_true(gremlin_field(document, 'health') == 'failure' .and. &
        .not. boolean_field(document, 'local_gate_green'), &
        'real current failure keeps the gate revoked despite an old PASS')
    coverage = json_member(document, 'coverage')
    call assert_equal_integer(number_field(coverage, 'fail'), 1, &
        'current terminal failure has independent coverage evidence')
    call assert_equal_integer(number_field(coverage, 'pass'), 0, &
        'old-generation PASS never credits current coverage')
    call gremlin_stop_lane(driver, project, cache, state, lane, owner)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine write_case(failing)
        logical, intent(in) :: failing
        character(:), allocatable :: source

        source = 'program '//case_name//new_line('a')// &
            'implicit none'//new_line('a')//'integer :: unit, gate_unit'// &
            new_line('a')//'logical :: released'// &
            new_line('a')//'character :: token'//new_line('a')// &
            'interface'//new_line('a')// &
            'integer function getpid() bind(C, name="getpid")'//new_line('a')// &
            'end function getpid'//new_line('a')//'end interface'//new_line('a')// &
            'open(newunit=unit, file="'//marker// &
            '", status="unknown", position="append")'//new_line('a')// &
            'write(unit, "(a)") "MARKER"'//new_line('a')//'close(unit)'// &
            new_line('a')//'open(newunit=unit, file="'//ready// &
            '", status="replace")'//new_line('a')// &
            'write(unit, "(i0)") getpid()'//new_line('a')//'close(unit)'// &
            new_line('a')//'inquire(file="'//released// &
            '", exist=released)'//new_line('a')// &
            'if (.not. released) then'//new_line('a')// &
            'open(newunit=gate_unit, file="'//gate// &
            '", status="old", access="stream", form="unformatted", action="read")'// &
            new_line('a')//'read(gate_unit) token'//new_line('a')// &
            'close(gate_unit)'//new_line('a')//'end if'//new_line('a')
        if (failing) source = source//'error stop 19'//new_line('a')
        source = source//'end program '//case_name//new_line('a')
        call write_text(project//'/test/'//case_name//'.f90', source)
    end subroutine write_case

    subroutine start_owner()
        arguments = string_list_t()
        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'start')
        call list_add(arguments, '--dir')
        call list_add(arguments, project)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--target')
        call list_add(arguments, case_name)
        call list_add(arguments, '--random')
        call list_add(arguments, '0')
        call list_add(arguments, '--seed')
        call list_add(arguments, '17')
        call list_add(arguments, '--timeout-seconds')
        call list_add(arguments, '60')
        call gremlin_json(driver, project, cache, state, arguments, document, &
            process, 30000)
        owner = gremlin_field(document, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(owner) > 0, &
            'starts an exact public Gremlin owner')
        if (process%exit_code /= 0 .or. len(owner) == 0) &
            call finish_assertions(retain_failed_scratch=.true.)
    end subroutine start_owner

    subroutine query_status()
        call read_request('status')
        call gremlin_json(driver, project, cache, state, arguments, document, &
            process, 10000)
        call assert_true(process%exit_code == 0, 'reads exact owner public status')
    end subroutine query_status

    subroutine read_request(action)
        character(len=*), intent(in) :: action

        arguments = string_list_t()
        call list_add(arguments, 'gremlin')
        call list_add(arguments, action)
        call list_add(arguments, '--dir')
        call list_add(arguments, project)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--session')
        call list_add(arguments, owner)
        call list_add(arguments, '--detail')
        call list_add(arguments, 'full')
    end subroutine read_request

    subroutine wait_until(condition)
        character(len=*), intent(in) :: condition

        call read_request('wait')
        call list_add(arguments, '--until')
        call list_add(arguments, condition)
        call list_add(arguments, '--wait-ms')
        call list_add(arguments, '30000')
        call gremlin_json(driver, project, cache, state, arguments, document, &
            process, 35000)
        call assert_true(process%exit_code == 0 .and. &
            boolean_field(document, 'wait_satisfied'), &
            'public owner satisfies '//condition)
    end subroutine wait_until

    subroutine wait_for_failure_coverage()
        integer :: attempt

        ! Readiness and coverage are separate durable reads. A receipt can
        ! arrive between them in the poll that first observes failure.
        do attempt = 1, 100
            coverage = json_member(document, 'coverage')
            if (number_field(coverage, 'fail') == 1) return
            call gremlin_wait_ms(50)
            call query_status()
        end do
    end subroutine wait_for_failure_coverage

    subroutine wait_at_marker(executions)
        integer, intent(in) :: executions
        logical :: found
        character(:), allocatable :: child_text

        call gremlin_wait_file(ready, 90000, found)
        call assert_true(found, 'native case reaches its marker/completion barrier')
        if (.not. found) then
            call gremlin_stop_lane(driver, project, cache, state, lane, owner)
            call finish_assertions(retain_failed_scratch=.true.)
        end if
        child_text = read_text(ready)
        child_pid = -1
        read(child_text, *, iostat=status) child_pid
        call assert_true(status == 0 .and. child_pid > 0, &
            'blocked native case publishes its exact PID')
        child_start = mcp_process_start_time(child_pid)
        call assert_true(child_start > 0_c_int64_t, 'captures blocked child identity')
        call assert_equal_integer(line_count(read_text(marker)), executions, &
            'independent marker counts actual executions before process completion')
        call query_status()
    end subroutine wait_at_marker

    subroutine crash_owner()
        integer :: owner_pid, split, signal_status, attempt, unit, ios
        integer(c_int64_t) :: owner_start
        character(len=32) :: pid_text
        character(len=4096) :: proc_status
        logical :: stopped

        split = index(owner, '-')
        owner_pid = -1
        status = -1
        if (split > 1) read(owner(:split - 1), *, iostat=status) owner_pid
        call assert_true(status == 0 .and. owner_pid > 0, &
            'session identifies the exact crash owner')
        owner_start = mcp_process_start_time(owner_pid)
        call assert_true(owner_start > 0_c_int64_t, 'captures owner start identity')
        ! Freeze receipt publication before killing descendants. Their death
        ! cannot race a synthetic failure receipt into the interrupted journal.
        signal_status = signal_identity(int(owner_pid, c_int), owner_start, 19_c_int)
        call assert_true(signal_status == 0, 'freezes only the exact owner identity')
        write(pid_text, '(i0)') owner_pid
        stopped = .false.
        do attempt = 1, 250
            open(newunit=unit, file='/proc/'//trim(pid_text)//'/stat', &
                status='old', action='read', iostat=ios)
            if (ios == 0) then
                read(unit, '(a)', iostat=ios) proc_status
                close(unit)
                if (ios == 0) then
                    split = index(proc_status, ')', back=.true.)
                    if (split > 0) then
                        if (split + 2 <= len_trim(proc_status)) then
                            stopped = proc_status(split + 2:split + 2) == 'T' .or. &
                                proc_status(split + 2:split + 2) == 't'
                        end if
                    end if
                end if
            end if
            if (stopped) exit
            call gremlin_wait_ms(20)
        end do
        call assert_true(stopped, &
            'kernel confirms owner stopped before descendant death')
        call mcp_kill_owned_tree(owner_pid, owner_start, signal_status)
        call assert_true(signal_status == 0, 'crashes the frozen owned process tree')
        do attempt = 1, 250
            if (.not. mcp_process_identity_running(child_pid, child_start)) exit
            call gremlin_wait_ms(20)
        end do
        call assert_true(.not. mcp_process_identity_running(child_pid, child_start), &
            'crash leaves no live blocked child')
        call remove_path(ready)
    end subroutine crash_owner

    subroutine assert_uncredited(context)
        character(len=*), intent(in) :: context
        type(json_value_t) :: events, event
        integer :: i, terminal_passes

        coverage = json_member(document, 'coverage')
        call assert_equal_integer(number_field(coverage, 'pass'), 0, context)
        call assert_equal_integer(number_field(coverage, 'unknown'), 1, &
            'unfinished current obligation remains UNKNOWN: '//context)
        call assert_equal_integer(number_field(document, 'gate_passed'), 0, &
            'gate has no current terminal PASS: '//context)
        call assert_true(.not. boolean_field(document, 'local_gate_green'), &
            'gate remains red before real current completion: '//context)
        events = json_member(document, 'events')
        terminal_passes = 0
        do i = 1, json_size(events)
            event = json_element(events, i)
            if (gremlin_field(event, 'case_id') /= case_name) cycle
            if (gremlin_field(event, 'generation') /= &
                gremlin_field(document, 'active_generation')) cycle
            if (gremlin_field(event, 'status') == 'PASS') &
                terminal_passes = terminal_passes + 1
        end do
        call assert_equal_integer(terminal_passes, 0, &
            'marker never substitutes for a real terminal receipt: '//context)
    end subroutine assert_uncredited

    subroutine assert_persisted_unknown(exact_generation)
        character(len=*), intent(in) :: exact_generation

        call gremlin_session_state_dir(project, lane, lane_state, status, message)
        call assert_true(status == 0, 'resolves exact durable lane state')
        call coverage_read_view_path(trim(lane_state)//'/coverage-'// &
            exact_generation//'.state', exact_generation, coverage_view, status, &
            message)
        call assert_true(status == COVERAGE_OK .and. &
            coverage_view%pass_count == 0 .and. &
            coverage_view%unknown_count == 1, &
            'owner crash preserves persisted UNKNOWN despite the execution marker')
    end subroutine assert_persisted_unknown

    subroutine capture_terminal_pass()
        type(journal_record_t), allocatable :: records(:)
        type(json_value_t) :: receipt
        integer(int64) :: next_cursor
        integer :: i, passes
        logical :: valid
        character(:), allocatable :: parse_message

        call gremlin_session_state_dir(project, lane, lane_state, status, message)
        call assert_true(status == 0, 'locates real campaign terminal evidence')
        call journal_read_page(trim(lane_state)//'/campaign-journal.jsonl', &
            0_int64, 64, int(JOURNAL_MAX_RECORD_BYTES, int64)*64_int64, records, &
            next_cursor, status, message)
        call assert_true(status == JOURNAL_OK, 'reads durable terminal receipt ledger')
        old_receipt = ''
        passes = 0
        do i = 1, size(records)
            call json_parse(records(i)%json, receipt, valid, parse_message)
            if (.not. valid) cycle
            if (gremlin_field(receipt, 'case_id') /= case_name) cycle
            if (gremlin_field(receipt, 'generation') /= old_generation) cycle
            if (gremlin_field(receipt, 'status') /= 'PASS') cycle
            passes = passes + 1
            old_receipt = records(i)%json
        end do
        call assert_equal_integer(passes, 1, &
            'crashed marker has no PASS receipt; only the completed retry has one')
        call write_text(scratch//'/real-old-terminal.jsonl', old_receipt//new_line('a'))
    end subroutine capture_terminal_pass

    subroutine inject_stale_pass()
        type(json_value_t) :: receipt
        character(:), allocatable :: requirement, inventory, old_log, parse_message
        character(len=32) :: epoch_text, seed_text
        logical :: valid

        call json_parse(old_receipt, receipt, valid, parse_message)
        call assert_true(valid, 'stale injection is based on a real terminal PASS')
        old_log = gremlin_field(receipt, 'log_path')
        call assert_true(file_exists(old_log), &
            'real old terminal PASS retains its output artifact before injection')
        requirement = gremlin_field(document, 'requirement_digest')
        coverage = json_member(document, 'coverage')
        inventory = gremlin_field(coverage, 'inventory_digest')
        write(epoch_text, '(i0)') number_field(coverage, 'epoch')
        write(seed_text, '(i0)') number_field(coverage, 'seed')
        ! Every current gate/coverage discriminator matches except generation.
        ! The old PASS and its log come from the completed native retry above.
        injected_receipt = old_receipt
        call replace_receipt_value('completion_id', '"stale-generation-pass"')
        call replace_receipt_value('session_id', '"'//owner//'"')
        call replace_receipt_value('requirement_digest', '"'//requirement//'"')
        call replace_receipt_value('inventory_digest', '"'//inventory//'"')
        call replace_receipt_value('coverage_epoch', trim(epoch_text))
        call replace_receipt_value('seed', trim(seed_text))
        injected_receipt = '{"metadata":{"completion_id":"nested-shadow"},'// &
            injected_receipt(2:)
        call gremlin_get_session_journal_path(project, lane, owner, journal_path, &
            status, message)
        call assert_true(status == 0, 'locates current session injection ledger')
        call journal_append(trim(journal_path), 'stale-generation-pass', &
            injected_receipt, status, message)
        call assert_true(status == JOURNAL_OK, 'durably injects old PASS into session')
        call journal_append(trim(lane_state)//'/campaign-journal.jsonl', &
            'stale-generation-pass', injected_receipt, status, message)
        call assert_true(status == JOURNAL_OK, 'durably injects old PASS into campaign')
    end subroutine inject_stale_pass

    subroutine replace_receipt_value(key, value)
        character(len=*), intent(in) :: key, value
        integer :: at, last, delimiter

        ! The copied native receipt uses compact JSON; these fields contain
        ! simple IDs/digests or numbers, with no escaped quote in their values.
        at = index(injected_receipt, '"'//key//'":')
        call assert_true(at > 0, 'real terminal receipt contains '//key)
        if (at == 0) return
        at = at + len(key) + 3
        if (injected_receipt(at:at) == '"') then
            delimiter = index(injected_receipt(at + 1:), '"')
            last = at + delimiter
        else
            delimiter = scan(injected_receipt(at:), ',}')
            last = at + delimiter - 2
        end if
        call assert_true(delimiter > 0, 'real receipt field has a value boundary')
        if (delimiter == 0) return
        injected_receipt = injected_receipt(:at - 1)//value// &
            injected_receipt(last + 1:)
    end subroutine replace_receipt_value

    subroutine assert_stale_visible()
        type(json_value_t) :: events, event
        integer :: i
        logical :: found

        events = json_member(document, 'events')
        found = .false.
        do i = 1, json_size(events)
            event = json_element(events, i)
            if (gremlin_field(event, 'completion_id') /= 'stale-generation-pass') cycle
            if (gremlin_field(event, 'generation') /= old_generation) cycle
            if (gremlin_field(event, 'status') == 'PASS') found = .true.
        end do
        call assert_true(found, &
            'public journal retains injected stale terminal evidence')
    end subroutine assert_stale_visible

    logical function boolean_field(value, key)
        type(json_value_t), intent(in) :: value
        character(len=*), intent(in) :: key

        boolean_field = json_boolean_value(json_member(value, key))
    end function boolean_field

    integer function number_field(value, key)
        type(json_value_t), intent(in) :: value
        character(len=*), intent(in) :: key

        number_field = int(json_number_value(json_member(value, key)))
    end function number_field

    integer function line_count(text)
        character(len=*), intent(in) :: text
        integer :: i

        line_count = 0
        do i = 1, len(text)
            if (text(i:i) == new_line('a')) line_count = line_count + 1
        end do
    end function line_count

end program test_gremlin_crash_receipts
