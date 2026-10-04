program test_gremlin_journal_recovery
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, file_exists, remove_path
    use fo_test_harness, only: run_process, environment_value, current_directory
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_field
    use fo_test_gremlin_oracle, only: gremlin_fifo
    use fo_test_gremlin_oracle, only: gremlin_wait_file, gremlin_stop_lane
    use fo_test_gremlin_oracle, only: gremlin_wait_ms, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_spawn, gremlin_wait_child
    use fo_test_process_identity, only: mcp_process_start_time, mcp_kill_owned_tree
    use fo_test_process_identity, only: mcp_process_identity_running
    use fo_gremlin_session, only: gremlin_get_session_journal_path
    use fo_gremlin_state, only: gremlin_session_read, gremlin_session_state_dir
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_number_value, json_parse
    implicit none

    interface
        integer(c_int) function c_setenv(name, value, overwrite) &
                bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
        integer(c_int) function c_unsetenv(name) bind(C, name='unsetenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
        end function c_unsetenv
    end interface

    character(:), allocatable :: driver, scratch, project, cache, state, lane
    character(:), allocatable :: first_gate, first_pid, first_done
    character(:), allocatable :: second_gate, second_pid, second_done
    character(:), allocatable :: first_session, second_session, journal_path
    character(:), allocatable :: pass_log, fail_log, pass_snapshot, fail_snapshot
    character(:), allocatable :: barrier_library
    character(len=512) :: message
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: report
    integer :: owner_pid, child_pid, parse_status, ierr
    integer(c_int64_t) :: owner_start, child_start
    logical :: ready_seen

    inquire(file='/proc/self/stat', exist=ready_seen)
    if (.not. ready_seen) then
        write (*, '(a)') 'Gremlin journal recovery: skipped (requires Linux /proc)'
        stop
    end if
    call gremlin_setup(driver, scratch, project, cache, state)
    barrier_library = build_barrier_library()
    if (.not. file_exists(barrier_library)) &
        call finish_assertions(retain_failed_scratch=.true.)
    lane = 'journal-recovery-native'
    first_gate = scratch//'/blocked-first.fifo'
    first_pid = scratch//'/blocked-first.pid'
    first_done = scratch//'/blocked-first.done'
    second_gate = scratch//'/blocked-second.fifo'
    second_pid = scratch//'/blocked-second.pid'
    second_done = scratch//'/blocked-second.done'
    call write_text(project//'/fpm.toml', &
        'name = "gremlin_journal_recovery_probe"'//new_line('a'))
    call gremlin_fifo(first_gate)
    call gremlin_fifo(second_gate)
    call write_pass_case()
    call write_fail_case()
    call write_blocked_case(first_gate, first_pid, first_done)

    call start_cases('test_fail', 'test_pass', 'test_blocked', report, process)
    first_session = gremlin_field(report, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(first_session) > 0, &
        'starts owner with independent PASS, FAIL and blocked cases')
    if (process%exit_code /= 0 .or. len(first_session) == 0) &
        call finish_assertions(retain_failed_scratch=.true.)
    call owner_pid_from_session(first_session, owner_pid)
    owner_start = mcp_process_start_time(owner_pid)
    call assert_true(owner_start > 0_c_int64_t, &
        'captures exact first owner PID and start identity')
    if (owner_start <= 0_c_int64_t) then
        call gremlin_stop_lane(driver, project, cache, state, lane, first_session)
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    call gremlin_wait_file(first_pid, 60000, ready_seen)
    call assert_true(ready_seen, 'third case waits at its native FIFO barrier')
    if (.not. ready_seen) then
        call gremlin_stop_lane(driver, project, cache, state, lane, first_session)
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    call read_pid_file(first_pid, child_pid, parse_status)
    call assert_true(parse_status == 0 .and. child_pid > 0, &
        'blocked fixture publishes its exact child PID')
    if (parse_status /= 0 .or. child_pid <= 0) then
        call gremlin_stop_lane(driver, project, cache, state, lane, first_session)
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    child_start = mcp_process_start_time(child_pid)
    call assert_true(child_start > 0_c_int64_t, 'captures blocked child start identity')
    if (child_start <= 0_c_int64_t) then
        call gremlin_stop_lane(driver, project, cache, state, lane, first_session)
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    call wait_for_completed_prefix(first_session, report)
    call check_receipt_set(report, first_session, pass_log, fail_log)
    pass_snapshot = read_text(pass_log)
    fail_snapshot = read_text(fail_log)
    call assert_true(index(fail_snapshot, 'journal-recovery-fail-token-4e812a') > 0, &
        'failure log keeps its independent unique output token')
    call gremlin_get_session_journal_path(project, lane, first_session, journal_path, &
        ierr, message)
    call assert_true(ierr == 0, 'resolves first owner durable journal path')
    if (ierr /= 0) then
        call gremlin_stop_lane(driver, project, cache, state, lane, first_session)
        call finish_assertions(retain_failed_scratch=.true.)
    end if

    call mcp_kill_owned_tree(owner_pid, owner_start, parse_status)
    call assert_true(parse_status == 0, 'kills only the exact owner and child tree')
    call wait_gone(owner_pid, owner_start, 5000)
    call wait_gone(child_pid, child_start, 5000)
    call assert_true(.not. file_exists(first_done), &
        'interrupted case never reaches its post-gate completion marker')
    call append_truncated_tail(journal_path)

    call remove_path(project//'/test/test_pass.f90')
    call remove_path(project//'/test/test_fail.f90')
    call write_blocked_case(second_gate, second_pid, second_done)
    call interrupt_restart('owner', barrier_library, first_session)
    call interrupt_restart('import', barrier_library, first_session)
    call start_cases('', '', 'test_blocked', report, process)
    second_session = gremlin_field(report, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(second_session) > 0, &
        'restarts the lane from durable receipts after owner death')
    if (process%exit_code /= 0 .or. len(second_session) == 0) &
        call finish_assertions(retain_failed_scratch=.true.)
    call assert_true(second_session /= first_session, &
        'recovery installs a distinct live owner session')
    call gremlin_wait_file(second_pid, 60000, ready_seen)
    call assert_true(ready_seen, 'restarted owner reaches its sole blocked case')
    if (.not. ready_seen) then
        call gremlin_stop_lane(driver, project, cache, state, lane, second_session)
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    call read_pid_file(second_pid, child_pid, parse_status)
    call assert_true(parse_status == 0 .and. child_pid > 0, &
        'restarted blocked test publishes its child identity')
    if (parse_status /= 0 .or. child_pid <= 0) then
        call gremlin_stop_lane(driver, project, cache, state, lane, second_session)
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    child_start = mcp_process_start_time(child_pid)
    if (child_start <= 0_c_int64_t) then
        call gremlin_stop_lane(driver, project, cache, state, lane, second_session)
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    call wait_for_current_block(second_session, report)
    call assert_equal_string(gremlin_field(report, 'current_test'), 'test_blocked', &
        'recovered owner is blocked on the interrupted case')
    call check_receipt_set(report, first_session, pass_log, fail_log)
    call assert_equal_string(read_text(pass_log), pass_snapshot, &
        'recovery leaves the original PASS log byte-for-byte unchanged')
    call assert_equal_string(read_text(fail_log), fail_snapshot, &
        'recovery leaves the original FAIL log byte-for-byte unchanged')
    call assert_equal_integer(&
        int(json_number_value(json_member(report, 'completed'))), 0, &
        'the restarted case has no terminal completion receipt')
    call assert_true(.not. file_exists(first_done) .and. &
        .not. file_exists(second_done), &
        'neither interrupted execution publishes its completion marker')
    call gremlin_stop_lane(driver, project, cache, state, lane, second_session)
    call wait_gone(child_pid, child_start, 5000)
    call assert_true(.not. mcp_process_identity_running(owner_pid, owner_start), &
        'the killed first owner remains absent after restart cleanup')
    call finish_assertions(retain_failed_scratch=.true.)

contains

    function build_barrier_library() result(library)
        character(:), allocatable :: library
        type(string_list_t) :: compile_args, compile_env
        type(process_result_t) :: compile
        character(:), allocatable :: source_root

        source_root = current_directory()
        library = scratch//'/gremlin-journal-recovery-barrier.so'
        call list_add(compile_args, 'cc')
        call list_add(compile_args, '-shared')
        call list_add(compile_args, '-fPIC')
        call list_add(compile_args, source_root// &
            '/test-fixtures/c/gremlin_journal_recovery_barrier.c')
        call list_add(compile_args, '-ldl')
        call list_add(compile_args, '-o')
        call list_add(compile_args, library)
        call list_add(compile_env, 'PATH='//environment_value('PATH'))
        call run_process(compile_args, scratch, compile, compile_env, timeout_ms=30000)
        call assert_true(compile%exit_code == 0, &
            'builds the scoped journal durability barrier: '//trim(compile%stderr))
    end function build_barrier_library

    subroutine interrupt_restart(mode, shim, original_session)
        character(len=*), intent(in) :: mode, shim, original_session
        type(string_list_t) :: restart_args
        character(:), allocatable :: ready, gate, owner_file, stdout_path, stderr_path
        character(:), allocatable :: imported_journal, imported_text, parse_message
        character(len=4096) :: state_dir
        character(len=128) :: published_session, published_start
        character(len=65536) :: owner_status
        character(len=32) :: expected_start
        character(len=512) :: local_message
        type(json_value_t) :: first_import
        integer(c_int64_t) :: start_time
        integer :: process_id, owner_pid, ierr, exit_code, read_pid_status
        integer(c_int) :: env_status
        logical :: ready_seen, valid

        ready = scratch//'/restart-'//trim(mode)//'.ready'
        gate = scratch//'/restart-'//trim(mode)//'.fifo'
        stdout_path = scratch//'/restart-'//trim(mode)//'.out'
        stderr_path = scratch//'/restart-'//trim(mode)//'.err'
        call gremlin_fifo(gate)
        call gremlin_session_state_dir(project, lane, state_dir, ierr, local_message)
        call assert_true(ierr == 0, 'resolves the exact owner-record path')
        if (ierr /= 0) then
            call remove_path(gate)
            return
        end if
        owner_file = trim(state_dir)//'/owner'

        if (trim(mode) == 'owner') then
            call list_add(restart_args, 'gremlin')
            call list_add(restart_args, 'start')
        else
            call list_add(restart_args, 'gremlin')
            call list_add(restart_args, 'run')
        end if
        call list_add(restart_args, '--dir')
        call list_add(restart_args, project)
        call list_add(restart_args, '--lane')
        call list_add(restart_args, lane)
        call list_add(restart_args, '--random-count')
        call list_add(restart_args, '32')
        call list_add(restart_args, '--seed')
        call list_add(restart_args, '1729')
        call list_add(restart_args, '--target')
        call list_add(restart_args, 'test_blocked')

        call set_barrier_env('LD_PRELOAD', shim, env_status)
        call assert_true(env_status == 0, 'arms the test-only durability shim')
        if (env_status /= 0) then
            call clear_barrier_env()
            call remove_path(gate)
            return
        end if
        call set_barrier_env('RECOVERY_BARRIER_OWNER', owner_file, env_status)
        call assert_true(env_status == 0, 'sets the exact owner-record path')
        if (env_status /= 0) then
            call clear_barrier_env()
            call remove_path(gate)
            return
        end if
        call set_barrier_env('RECOVERY_BARRIER_MODE', trim(mode), env_status)
        call assert_true(env_status == 0, 'sets the selected durability barrier')
        if (env_status /= 0) then
            call clear_barrier_env()
            call remove_path(gate)
            return
        end if
        call set_barrier_env('RECOVERY_BARRIER_READY', ready, env_status)
        call assert_true(env_status == 0, 'sets the barrier readiness marker')
        if (env_status /= 0) then
            call clear_barrier_env()
            call remove_path(gate)
            return
        end if
        call set_barrier_env('RECOVERY_BARRIER_GATE', gate, env_status)
        call assert_true(env_status == 0, 'sets the barrier FIFO path')
        if (env_status /= 0) then
            call clear_barrier_env()
            call remove_path(gate)
            return
        end if
        call gremlin_spawn(driver, project, restart_args, stdout_path, stderr_path, &
            process_id)
        start_time = mcp_process_start_time(process_id)
        write(expected_start, '(i0)') start_time
        call clear_barrier_env()

        call gremlin_wait_file(ready, 30000, ready_seen)
        call assert_true(ready_seen, trim(mode)// &
            ' restart reaches its scoped post-durability FIFO')
        if (ready_seen) then
            call read_pid_file(ready, owner_pid, read_pid_status)
            call assert_true(read_pid_status == 0 .and. owner_pid == process_id, &
                trim(mode)//' barrier belongs to the exact spawned owner PID')
            call assert_true(mcp_process_identity_running(process_id, start_time), &
                trim(mode)//' barrier retains the exact process start identity')
            call gremlin_session_read(project, lane, published_session, owner_pid, &
                published_start, owner_status, ierr, local_message)
            call assert_true(ierr == 0, trim(mode)// &
                ' owner pointer is readable after its durable publication')
            call assert_true(len_trim(published_session) > 0 .and. &
                trim(published_session) /= trim(original_session), &
                trim(mode)//' publishes a replacement session for this lane')
            call assert_true(owner_pid == process_id .and. &
                trim(published_start) == trim(expected_start), &
                trim(mode)//' owner record matches exact PID and start identity')
            if (trim(mode) == 'import' .and. ierr == 0) then
                call gremlin_get_session_journal_path(project, lane, &
                    trim(published_session), imported_journal, ierr, local_message)
                call assert_true(ierr == 0, &
                    'resolves the imported-receipt destination journal')
                if (ierr == 0) then
                    imported_text = read_text(imported_journal)
                    call assert_equal_integer(text_line_count(imported_text), 1, &
                        'import barrier follows exactly one durable receipt')
                    call json_parse(trim(imported_text), first_import, valid, &
                        parse_message)
                    call assert_true(valid, &
                        'the first imported durable receipt is complete JSON')
                    if (valid) then
                        call assert_equal_string(member(first_import, 'session_id'), &
                            original_session, &
                            'first imported receipt retains its original session')
                        call assert_true(&
                            len(member(first_import, 'completion_id')) > 0, &
                            'first imported durable receipt retains its completion ID')
                    end if
                end if
            end if
        end if
        call kill_barrier_child(process_id, start_time, exit_code)
        call remove_path(ready)
        call remove_path(gate)
    end subroutine interrupt_restart

    subroutine set_barrier_env(name, value, status)
        character(len=*), intent(in) :: name, value
        integer(c_int), intent(out) :: status

        status = c_setenv(trim(name)//c_null_char, trim(value)//c_null_char, 1_c_int)
    end subroutine set_barrier_env

    subroutine clear_barrier_env()
        integer(c_int) :: status
        status = c_unsetenv('LD_PRELOAD'//c_null_char)
        status = c_unsetenv('RECOVERY_BARRIER_OWNER'//c_null_char)
        status = c_unsetenv('RECOVERY_BARRIER_MODE'//c_null_char)
        status = c_unsetenv('RECOVERY_BARRIER_READY'//c_null_char)
        status = c_unsetenv('RECOVERY_BARRIER_GATE'//c_null_char)
    end subroutine clear_barrier_env

    subroutine kill_barrier_child(process_id, start_time, exit_code)
        integer, intent(in) :: process_id
        integer(c_int64_t), intent(in) :: start_time
        integer, intent(out) :: exit_code
        integer(c_int) :: kill_status

        if (process_id <= 0) return
        if (mcp_process_identity_running(process_id, start_time)) then
            call mcp_kill_owned_tree(process_id, start_time, kill_status)
            call assert_true(kill_status == 0, &
                'kills only the barrier owner and its owned child tree')
        end if
        call gremlin_wait_child(process_id, exit_code)
        call wait_gone(process_id, start_time, 5000)
    end subroutine kill_barrier_child

    integer function text_line_count(text) result(count)
        character(len=*), intent(in) :: text
        integer :: i

        count = 0
        do i = 1, len_trim(text)
            if (text(i:i) == new_line('a')) count = count + 1
        end do
    end function text_line_count

    subroutine start_cases(first_target, second_target, third_target, reply, result)
        character(len=*), intent(in) :: first_target, second_target, third_target
        type(json_value_t), intent(out) :: reply
        type(process_result_t), intent(out) :: result

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'start')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--random-count')
        call list_add(args, '32')
        call list_add(args, '--seed')
        call list_add(args, '1729')
        call list_add(args, '--campaign-seconds')
        call list_add(args, '60')
        call list_add(args, '--timeout-seconds')
        call list_add(args, '5')
        if (len_trim(first_target) > 0) call add_target(first_target)
        if (len_trim(second_target) > 0) call add_target(second_target)
        if (len_trim(third_target) > 0) call add_target(third_target)
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, reply, result, 30000)
    end subroutine start_cases

    subroutine add_target(target)
        character(len=*), intent(in) :: target

        call list_add(args, '--target')
        call list_add(args, target)
    end subroutine add_target

    subroutine wait_for_completed_prefix(owner, document)
        character(len=*), intent(in) :: owner
        type(json_value_t), intent(out) :: document
        integer :: attempt

        do attempt = 1, 600
            call query_status(owner, document)
            if (gremlin_field(document, 'current_test') == 'test_blocked') then
                if (event_case_count(document, 'test_pass') == 1 .and. &
                    event_case_count(document, 'test_fail') == 1) return
            end if
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'owner completes PASS and FAIL before blocking')
    end subroutine wait_for_completed_prefix

    subroutine wait_for_current_block(owner, document)
        character(len=*), intent(in) :: owner
        type(json_value_t), intent(out) :: document
        integer :: attempt

        do attempt = 1, 600
            call query_status(owner, document)
            if (gremlin_field(document, 'current_test') == 'test_blocked') return
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'restarted owner reaches the blocked current test')
    end subroutine wait_for_current_block

    subroutine query_status(owner, document)
        character(len=*), intent(in) :: owner
        type(json_value_t), intent(out) :: document
        type(string_list_t) :: status_args
        type(process_result_t) :: status_process

        call list_add(status_args, 'gremlin')
        call list_add(status_args, 'status')
        call list_add(status_args, '--dir')
        call list_add(status_args, project)
        call list_add(status_args, '--lane')
        call list_add(status_args, lane)
        call list_add(status_args, '--session')
        call list_add(status_args, owner)
        call list_add(status_args, '--json')
        call gremlin_json(driver, project, cache, state, status_args, document, &
            status_process, 10000)
        call assert_true(status_process%exit_code == 0, &
            'reads public Gremlin owner status')
    end subroutine query_status

    integer function event_case_count(document, case_name) result(count)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: case_name
        type(json_value_t) :: events, event, field
        integer :: i

        count = 0
        events = json_member(document, 'events')
        do i = 1, json_size(events)
            event = json_element(events, i)
            field = json_member(event, 'case_id')
            if (json_string_value(field) == case_name) count = count + 1
        end do
    end function event_case_count

    subroutine check_receipt_set(document, owner, pass_path, fail_path)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: owner
        character(:), allocatable, intent(inout) :: pass_path, fail_path
        type(json_value_t) :: events, event, field
        character(len=256) :: completion_ids(128)
        character(:), allocatable :: case_name, status_name, log_path, session_id, id
        integer :: i, j, count, pass_count, fail_count

        events = json_member(document, 'events')
        count = 0
        pass_count = 0
        fail_count = 0
        do i = 1, json_size(events)
            event = json_element(events, i)
            case_name = member(event, 'case_id')
            if (case_name == 'test_blocked') then
                call assert_true(.false., 'blocked test has no terminal receipt')
                cycle
            end if
            if (case_name /= 'test_pass' .and. case_name /= 'test_fail') cycle
            status_name = member(event, 'status')
            session_id = member(event, 'session_id')
            id = member(event, 'completion_id')
            log_path = member(event, 'log_path')
            call assert_equal_string(session_id, owner, &
                'recovered completion retains original owner session')
            call assert_true(len(id) > 0, 'completion receipt has a unique identity')
            call assert_true(len(log_path) > 0, &
                'completion receipt references its output log')
            if (len(log_path) > 0) then
                call assert_true(log_path(1:1) == '/', &
                    'log reference is an absolute path')
                call assert_true(file_exists(log_path), &
                    'recovered output artifact still exists')
            end if
            if (count >= size(completion_ids)) then
                call assert_true(.false., 'recovery receipt count fits bounded oracle')
                cycle
            end if
            do j = 1, count
                call assert_true(trim(completion_ids(j)) /= id, &
                    'journal recovery does not duplicate completion IDs')
            end do
            count = count + 1
            completion_ids(count) = id
            if (case_name == 'test_pass') then
                pass_count = pass_count + 1
                call assert_equal_string(status_name, 'PASS', &
                    'pass behavior has PASS receipt')
                pass_path = log_path
            else
                fail_count = fail_count + 1
                call assert_equal_string(status_name, 'FAIL', &
                    'failing behavior has FAIL receipt')
                fail_path = log_path
            end if
        end do
        call assert_equal_integer(count, 2, &
            'exactly two completed receipts survive recovery')
        call assert_equal_integer(pass_count, 1, 'one PASS receipt survives recovery')
        call assert_equal_integer(fail_count, 1, 'one FAIL receipt survives recovery')
    end subroutine check_receipt_set

    function member(document, name) result(value)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: name
        character(:), allocatable :: value
        type(json_value_t) :: field

        field = json_member(document, name)
        value = json_string_value(field)
    end function member

    subroutine owner_pid_from_session(owner, pid)
        character(len=*), intent(in) :: owner
        integer, intent(out) :: pid
        integer :: dash, ios

        pid = -1
        dash = index(owner, '-')
        if (dash <= 1) then
            call assert_true(.false., 'owner session identifies its PID prefix')
            return
        end if
        read(owner(:dash - 1), *, iostat=ios) pid
        call assert_true(ios == 0 .and. pid > 0, 'parses exact owner PID')
    end subroutine owner_pid_from_session

    subroutine read_pid_file(path, pid, status)
        character(len=*), intent(in) :: path
        integer, intent(out) :: pid, status
        character(:), allocatable :: text

        pid = -1
        text = read_text(path)
        read(text, *, iostat=status) pid
    end subroutine read_pid_file

    subroutine append_truncated_tail(path)
        character(len=*), intent(in) :: path
        integer :: unit, status

        open(newunit=unit, file=trim(path), status='old', action='write', &
            access='stream', form='formatted', position='append', iostat=status)
        call assert_true(status == 0, &
            'opens recovered receipt journal for tail mutation')
        if (status /= 0) return
        write(unit, '(a)', advance='no', iostat=status) '{"completion_id":"partial'
        call assert_true(status == 0, 'writes a truncated unterminated journal tail')
        close(unit)
    end subroutine append_truncated_tail

    subroutine write_pass_case()
        call gremlin_write_case(project, 'test_pass', &
            'integer :: unit'//new_line('a')// &
            'open(newunit=unit, file="'//scratch//'/pass.done", status="replace")'// &
            new_line('a')//'write(unit, "(a)") "done"'//new_line('a')//'close(unit)')
    end subroutine write_pass_case

    subroutine write_fail_case()
        call gremlin_write_case(project, 'test_fail', &
            'integer :: unit'//new_line('a')// &
            'print "(a)", "journal-recovery-fail-token-4e812a"'//new_line('a')// &
            'open(newunit=unit, file="'//scratch//'/fail.started", '// &
            'status="replace")'//new_line('a')//'write(unit, "(a)") "started"'// &
            new_line('a')//'close(unit)'//new_line('a')//'error stop 7')
    end subroutine write_fail_case

    subroutine write_blocked_case(gate_path, pid_path, done_path)
        character(len=*), intent(in) :: gate_path, pid_path, done_path
        character(len=2048) :: body

        body = 'integer :: unit, gate_unit'//new_line('a')//'character :: token'// &
            new_line('a')//'interface'//new_line('a')// &
            'integer function getpid() bind(C, name="getpid")'//new_line('a')// &
            'end function getpid'//new_line('a')//'end interface'//new_line('a')// &
            'open(newunit=unit, file="'//pid_path// &
            '", status="replace")'//new_line('a')// &
            'write(unit, "(i0)") getpid()'//new_line('a')// &
            'close(unit)'//new_line('a')// &
            'open(newunit=gate_unit, file="'//gate_path// &
            '", status="old", access="stream", form="unformatted", action="read")'// &
            new_line('a')//'read(gate_unit) token'//new_line('a')// &
            'close(gate_unit)'// &
            new_line('a')//'open(newunit=unit, file="'//done_path// &
            '", status="replace")'//new_line('a')// &
            'write(unit, "(a)") "done"'//new_line('a')//'close(unit)'
        call gremlin_write_case(project, 'test_blocked', trim(body))
    end subroutine write_blocked_case

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
            'exact recorded process identity exits within its bound')
    end subroutine wait_gone

end program test_gremlin_journal_recovery
