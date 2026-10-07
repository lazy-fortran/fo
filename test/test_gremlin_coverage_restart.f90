program test_gremlin_coverage_restart
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, file_exists, remove_path
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_fifo, gremlin_release_fifo
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_gremlin_oracle, only: gremlin_field
    use fo_test_gremlin_oracle, only: gremlin_write_case
    use fo_test_process_identity, only: mcp_process_start_time, mcp_kill_owned_tree
    use fo_test_process_identity, only: mcp_process_identity_running
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_number_value, json_boolean_value, json_string_value
    implicit none

    integer, parameter :: n_cases = 40, interrupted_slot = 18
    integer, parameter :: seed = 1729
    character(len=32) :: names(n_cases), expected(n_cases)
    character(:), allocatable :: driver, scratch, project, cache, state, lane
    character(:), allocatable :: gate, ready, gate_after_marker, ready_after_marker
    character(:), allocatable :: marker, owner, replacement, session
    character(:), allocatable :: marker_text, child_text
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: report, coverage, field, events, event, event_case, event_status
    integer :: owner_pid, child_pid, signal_status, attempt, i
    integer(c_int64_t) :: owner_start, child_start
    logical :: ready_seen, interrupted_pass
    logical :: has_proc

    inquire(file='/proc/self/stat', exist=has_proc)
    if (.not. has_proc) then
        write (*, '(a)') 'coverage restart: skipped (requires Linux /proc)'
        stop
    end if
    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text('/var/tmp/fo161-coverage-current', scratch)
    write (*, '(a)') 'coverage restart fixture: '//scratch
    lane = 'coverage-restart-native'
    gate = scratch//'/interrupted.fifo'
    ready = scratch//'/interrupted.pid'
    gate_after_marker = scratch//'/final.fifo'
    ready_after_marker = scratch//'/final.pid'
    marker = scratch//'/cases.log'
    call write_text(project//'/fpm.toml', 'name = "coverage_restart_probe"'//new_line('a'))
    call gremlin_fifo(gate)
    call gremlin_fifo(gate_after_marker)
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
    call start_campaign()
    if (process%exit_code /= 0) call finish_assertions()
    replacement = gremlin_field(report, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(replacement) > 0, &
        'same-lane recovery starts a replacement session')
    call assert_true(replacement /= session, 'recovery does not reuse the dead owner identity')
    call wait_for_progress(17, ready, 60000, ready_seen, replacement)
    call assert_true(ready_seen, 'restarted epoch reaches the interrupted permutation slot')
    if (.not. ready_seen) call finish_assertions()
    call gremlin_release_fifo(gate)
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

    call gremlin_stop_lane(driver, project, cache, state, lane, replacement)
    call finish_assertions()

contains

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
        call list_add(args, '10')
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

end program test_gremlin_coverage_restart
