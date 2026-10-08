program test_gremlin_journal_recovery
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_char, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add, run_process
    use fo_test_harness, only: write_text, read_text, file_exists, remove_path
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_contains, finish_assertions, current_directory
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_field
    use fo_test_gremlin_oracle, only: gremlin_fifo, gremlin_wait_file, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_spawn, gremlin_wait_child, gremlin_stop_lane
    use fo_test_process_identity, only: mcp_process_start_time
    use fo_test_process_identity, only: mcp_process_identity_running, mcp_kill_owned_tree
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_number_value, json_parse
    use fo_gremlin_state, only: gremlin_session_state_dir, gremlin_session_read
    use fo_gremlin_session, only: gremlin_get_session_journal_path
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, cwd
    character(:), allocatable :: owner, first_owner, library, first_gate, second_gate
    character(:), allocatable :: first_pid_file, second_pid_file, first_done, second_done
    character(:), allocatable :: pass_path, fail_path, pass_bytes, fail_bytes
    character(len=4096) :: lane_state, journal_path, owner_start
    character(len=65536) :: owner_status
    character(len=128) :: observed_owner
    character(len=256) :: message
    type(string_list_t) :: arguments
    type(process_result_t) :: process
    type(json_value_t) :: document
    integer :: status, owner_pid, workers(2), i
    integer(c_int64_t) :: births(2)
    logical :: linux
    character(len=*), parameter :: lane = 'journal-recovery'

    interface
        integer(c_int) function signal_identity(pid, birth, signal_number) &
                bind(C, name='fo_test_signal_identity')
            import :: c_int, c_int64_t
            integer(c_int), value :: pid, signal_number
            integer(c_int64_t), value :: birth
        end function signal_identity
        integer(c_int) function setenv(name, value, overwrite) bind(C, name='fo_test_setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function setenv
        integer(c_int) function regular_file(path) bind(C, name='fo_test_is_regular_file')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function regular_file
        integer(c_int) function chdir(path) bind(C, name='fo_test_chdir')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function chdir
    end interface

    inquire(file='/proc/self/stat', exist=linux)
    if (.not. linux) then
        print '(a)', 'journal recovery durability barriers: skipped (requires Linux /proc)'
        stop
    end if
    call current_directory(cwd)
    call gremlin_setup(driver, scratch, project, cache, state)
    workers = 0
    births = 0_c_int64_t
    library = scratch//'/recovery_barrier.so'
    call list_add(arguments, 'cc')
    call list_add(arguments, '-shared')
    call list_add(arguments, '-fPIC')
    call list_add(arguments, cwd//'/test-fixtures/c/recovery_barrier.c')
    call list_add(arguments, '-ldl')
    call list_add(arguments, '-o')
    call list_add(arguments, library)
    call run_process(arguments, project, process, timeout_ms=30000)
    call assert_equal_integer(process%exit_code, 0, 'builds native durability OS barrier')
    if (process%exit_code /= 0) call finish_assertions(retain_failed_scratch=.true.)
    first_gate = scratch//'/before-crash.fifo'
    second_gate = scratch//'/after-recovery.fifo'
    first_pid_file = scratch//'/before-crash.pid'
    second_pid_file = scratch//'/after-recovery.pid'
    first_done = scratch//'/before-crash.done'
    second_done = scratch//'/after-recovery.done'
    call gremlin_fifo(first_gate)
    call gremlin_fifo(second_gate)
    call write_text(project//'/fpm.toml', 'name="journal_recovery"'//new_line('a'))
    call write_text(project//'/test/test_pass.f90', &
        'program test_pass'//new_line('a')//'end program'//new_line('a'))
    call write_text(project//'/test/test_fail.f90', &
        'program test_fail'//new_line('a')// &
        'print "(a)", "journal-recovery-fail-token-4e812a"'//new_line('a')// &
        'error stop 7'//new_line('a')//'end program'//new_line('a'))
    call write_blocked(first_gate, first_pid_file, first_done)
    call start(.true.)
    first_owner = owner
    call wait_blocked(first_pid_file, workers(1), births(1))
    call assert_receipts(document, .true.)
    call assert_equal_integer(number_field(document, 'completed'), 2, &
        'only terminal pass and failure complete before the crash')
    call crash_recorded_owner()
    call assert_true(.not. file_exists(first_done), &
        'interrupted execution has no completion marker')
    call remove_path(project//'/test/test_pass.f90')
    call remove_path(project//'/test/test_fail.f90')
    call write_blocked(second_gate, second_pid_file, second_done)
    call interrupt_restart('owner')
    call interrupt_restart('import')
    call start(.false.)
    call assert_true(owner /= first_owner, 'recovery publishes a new exact owner session')
    call wait_blocked(second_pid_file, workers(2), births(2))
    call gremlin_session_read(project, lane, observed_owner, owner_pid, &
        owner_start, owner_status, status, message)
    call assert_true(status == 0 .and. trim(observed_owner) == owner, &
        'recovered public session matches the exact live owner pointer')
    call assert_owner_metadata(.true.)
    call assert_receipts(document, .false.)
    call assert_equal_integer(number_field(document, 'completed'), 0, &
        'recovered interrupted case remains uncompleted')
    call assert_true(.not. file_exists(first_done) .and. .not. file_exists(second_done), &
        'neither blocked execution acquires a completion marker')
    call gremlin_stop_lane(driver, project, cache, state, lane, owner)
    do i = 1, size(workers)
        if (workers(i) <= 0 .or. births(i) <= 0_c_int64_t) cycle
        if (mcp_process_identity_running(workers(i), births(i))) &
            call mcp_kill_owned_tree(workers(i), births(i), status)
        call assert_true(.not. mcp_process_identity_running(workers(i), births(i)), &
            'owned interrupted worker is absent before fixture cleanup')
    end do
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine write_blocked(gate, pid_file, done_file)
        character(len=*), intent(in) :: gate, pid_file, done_file

        call write_text(project//'/test/test_blocked.f90', &
            'program test_blocked'//new_line('a')// &
            'integer :: unit'//new_line('a')//'character :: token'//new_line('a')// &
            'interface'//new_line('a')// &
            'integer function getpid() bind(C,name="getpid")'//new_line('a')// &
            'end function'//new_line('a')//'end interface'//new_line('a')// &
            'open(newunit=unit,file="'//pid_file//'",status="replace")'//new_line('a')// &
            'write(unit,"(i0)") getpid()'//new_line('a')//'close(unit)'//new_line('a')// &
            'open(newunit=unit,file="'//gate// &
            '",access="stream",form="unformatted",action="read")'//new_line('a')// &
            'read(unit) token'//new_line('a')//'close(unit)'//new_line('a')// &
            'open(newunit=unit,file="'//done_file//'",status="replace")'//new_line('a')// &
            'write(unit,"(a)") "done"'//new_line('a')//'close(unit)'//new_line('a')// &
            'end program'//new_line('a'))
    end subroutine write_blocked

    subroutine start(initial)
        logical, intent(in) :: initial

        call start_arguments('start', initial)
        call gremlin_json(driver, project, cache, state, arguments, document, &
            process, 30000)
        owner = gremlin_field(document, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(owner) > 0, &
            'starts an independently reconnectable public owner')
        if (process%exit_code /= 0 .or. len(owner) == 0) &
            call finish_assertions(retain_failed_scratch=.true.)
    end subroutine start

    subroutine start_arguments(action, initial)
        character(len=*), intent(in) :: action
        logical, intent(in) :: initial

        arguments = string_list_t()
        call list_add(arguments, 'gremlin')
        call list_add(arguments, action)
        call list_add(arguments, '--dir')
        call list_add(arguments, project)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--random')
        call list_add(arguments, '32')
        call list_add(arguments, '--seed')
        call list_add(arguments, '1729')
        call list_add(arguments, '--campaign-seconds')
        call list_add(arguments, '60')
        call list_add(arguments, '--timeout-seconds')
        call list_add(arguments, '5')
        if (initial) then
            call list_add(arguments, '--target')
            call list_add(arguments, 'test_fail')
            call list_add(arguments, '--target')
            call list_add(arguments, 'test_pass')
        end if
        call list_add(arguments, '--target')
        call list_add(arguments, 'test_blocked')
    end subroutine start_arguments

    subroutine query()
        arguments = string_list_t()
        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'status')
        call list_add(arguments, '--dir')
        call list_add(arguments, project)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--session')
        call list_add(arguments, owner)
        call list_add(arguments, '--detail')
        call list_add(arguments, 'full')
        call gremlin_json(driver, project, cache, state, arguments, document, &
            process, 10000)
        call assert_equal_integer(process%exit_code, 0, 'reads the exact public owner')
    end subroutine query

    subroutine wait_blocked(pid_file, worker, birth)
        character(len=*), intent(in) :: pid_file
        integer, intent(out) :: worker
        integer(c_int64_t), intent(out) :: birth
        integer :: unit, attempt, io_status
        logical :: found

        worker = 0
        birth = 0_c_int64_t
        call gremlin_wait_file(pid_file, 30000, found)
        call assert_true(found, 'blocked native case publishes its PID barrier')
        if (.not. found) call finish_assertions(retain_failed_scratch=.true.)
        open(newunit=unit, file=pid_file, status='old', action='read')
        read(unit, *, iostat=io_status) worker
        close(unit)
        call assert_true(io_status == 0 .and. worker > 0, 'reads exact blocked worker PID')
        birth = mcp_process_start_time(worker)
        call assert_true(birth > 0_c_int64_t, 'records independent blocked-worker birth')
        do attempt = 1, 300
            call query()
            if (gremlin_field(document, 'current_test') == 'test_blocked' .and. &
                terminal_receipts_present(document)) exit
            call gremlin_wait_ms(100)
        end do
        call assert_equal_string(gremlin_field(document, 'current_test'), &
            'test_blocked', 'owner remains on the interrupted case')
    end subroutine wait_blocked

    logical function terminal_receipts_present(value)
        type(json_value_t), intent(in) :: value
        type(json_value_t) :: events, event
        integer :: index, passes, failures

        passes = 0
        failures = 0
        events = json_member(value, 'events')
        do index = 1, json_size(events)
            event = json_element(events, index)
            if (gremlin_field(event, 'case_id') == 'test_pass') passes = passes + 1
            if (gremlin_field(event, 'case_id') == 'test_fail') failures = failures + 1
        end do
        terminal_receipts_present = passes == 1 .and. failures == 1
    end function terminal_receipts_present

    subroutine crash_recorded_owner()
        integer(c_int64_t) :: birth

        call gremlin_session_state_dir(project, lane, lane_state, status, message)
        call assert_equal_integer(status, 0, 'locates this exact project/lane state')
        call gremlin_session_read(project, lane, observed_owner, owner_pid, &
            owner_start, owner_status, status, message)
        call assert_true(status == 0 .and. owner_pid > 0, &
            'reads the live owned supervisor identity before crash')
        call assert_equal_string(trim(observed_owner), owner, &
            'owner pointer matches the reconnectable public session')
        call assert_owner_metadata(.true.)
        birth = mcp_process_start_time(owner_pid)
        call assert_true(birth > 0_c_int64_t, 'independent OS birth identifies the owner')
        status = int(signal_identity(int(owner_pid, c_int), birth, 9_c_int))
        call assert_equal_integer(status, 0, 'crashes only the exact owned supervisor')
        call wait_gone(owner_pid, birth)
    end subroutine crash_recorded_owner

    subroutine wait_gone(pid, birth)
        integer, intent(in) :: pid
        integer(c_int64_t), intent(in) :: birth
        integer :: attempt

        do attempt = 1, 500
            if (.not. mcp_process_identity_running(pid, birth)) return
            call gremlin_wait_ms(20)
        end do
        call assert_true(.false., 'exact owned process exits within its safety bound')
    end subroutine wait_gone

    subroutine interrupt_restart(mode)
        character(len=*), intent(in) :: mode
        character(:), allocatable :: ready, gate, imported, parse_message
        type(json_value_t) :: receipt
        integer :: child, barrier_pid, unit, io_status, exit_code, line_end
        integer(c_int64_t) :: birth
        logical :: found, valid

        ready = scratch//'/restart-'//mode//'.ready'
        gate = scratch//'/restart-'//mode//'.fifo'
        call gremlin_fifo(gate)
        call environment('RECOVERY_BARRIER_OWNER', trim(lane_state)//'/owner')
        call environment('RECOVERY_BARRIER_MODE', mode)
        call environment('RECOVERY_BARRIER_READY', ready)
        call environment('RECOVERY_BARRIER_GATE', gate)
        call environment('LD_PRELOAD', library)
        if (mode == 'owner') then
            call start_arguments('start', .false.)
        else
            call start_arguments('run', .false.)
        end if
        call gremlin_spawn(driver, project, arguments, scratch//'/restart-'//mode//'.out', &
            scratch//'/restart-'//mode//'.err', child)
        call environment('LD_PRELOAD', '')
        birth = mcp_process_start_time(child)
        call gremlin_wait_file(ready, 30000, found)
        call assert_true(found, 'replacement reaches the exact '//mode//' durability barrier')
        if (found) then
            open(newunit=unit, file=ready, status='old', action='read')
            read(unit, *, iostat=io_status) barrier_pid
            close(unit)
            call assert_true(io_status == 0 .and. barrier_pid == child, &
                'durability barrier belongs to the exact restarting process')
            call gremlin_session_read(project, lane, observed_owner, owner_pid, &
                owner_start, owner_status, status, message)
            call assert_true(status == 0 .and. owner_pid == child, &
                'replacement owner pointer is already published')
            call assert_owner_metadata(.false.)
            if (mode == 'import') then
                call gremlin_get_session_journal_path(project, lane, &
                    trim(observed_owner), journal_path, status, message)
                call assert_equal_integer(status, 0, 'locates interrupted import destination')
                imported = read_text(trim(journal_path))
                call assert_equal_integer(line_count(imported), 1, &
                    'recovery import is interrupted after its first durable receipt')
                line_end = index(imported, new_line('a'))
                if (line_end > 0) then
                    call json_parse(imported(:line_end - 1), receipt, valid, parse_message)
                    call assert_true(valid, 'imported receipt parses independently')
                    call assert_equal_string(gremlin_field(receipt, 'session_id'), &
                        first_owner, 'partial import preserves original receipt owner')
                end if
            end if
        end if
        if (mcp_process_identity_running(child, birth)) then
            status = int(signal_identity(int(child, c_int), birth, 9_c_int))
            call assert_equal_integer(status, 0, 'crashes the recorded durability-barrier owner')
        end if
        call gremlin_wait_child(child, exit_code)
        call wait_gone(child, birth)
    end subroutine interrupt_restart

    subroutine assert_owner_metadata(expect_run)
        logical, intent(in) :: expect_run
        character(len=4096) :: stored_project, stored_lane, stored_id, command_line
        character(:), allocatable :: physical_project, pid_text
        character :: byte
        integer :: unit, io_status, pid, position
        integer(c_int64_t) :: birth

        open(newunit=unit, file=trim(lane_state)//'/owner', status='old', action='read')
        read(unit, '(a)') stored_id
        read(unit, *) pid
        read(unit, *) birth
        read(unit, '(a)') stored_project
        read(unit, '(a)') stored_lane
        close(unit)
        call assert_equal_string(trim(stored_id), trim(observed_owner), &
            'raw owner pointer retains the independently observed session identity')
        call assert_equal_integer(pid, owner_pid, 'raw owner pointer names this exact process')
        call assert_true(birth == mcp_process_start_time(pid), &
            'raw owner birth matches independently observed OS identity')
        status = int(chdir(project//c_null_char))
        call assert_equal_integer(status, 0, 'enters the real fixture project')
        call current_directory(physical_project)
        status = int(chdir(cwd//c_null_char))
        call assert_equal_integer(status, 0, 'restores the native fixture working directory')
        call assert_equal_string(trim(stored_project), physical_project, &
            'raw owner record names this physical project')
        call assert_equal_string(trim(stored_lane), lane, 'raw owner record names this lane')
        if (.not. expect_run) return
        write(stored_id, '(i0)') pid
        pid_text = trim(stored_id)
        command_line = ''
        open(newunit=unit, file='/proc/'//pid_text//'/cmdline', &
            access='stream', form='unformatted', status='old', action='read')
        do position = 1, len(command_line)
            read(unit, iostat=io_status) byte
            if (io_status /= 0) exit
            command_line(position:position) = byte
        end do
        close(unit)
        call assert_contains(command_line, achar(0)//'gremlin'//achar(0)//'run'//achar(0), &
            'recorded process is the real Gremlin supervisor')
        call assert_contains(command_line, '--dir'//achar(0)//physical_project//achar(0), &
            'supervisor argv names this physical project')
        call assert_contains(command_line, '--lane'//achar(0)//lane//achar(0), &
            'supervisor argv names this exact lane')
    end subroutine assert_owner_metadata

    subroutine environment(name, value)
        character(len=*), intent(in) :: name, value

        status = int(setenv(trim(name)//c_null_char, trim(value)//c_null_char, 1_c_int))
        call assert_equal_integer(status, 0, 'sets native recovery barrier environment')
    end subroutine environment

    integer function number_field(value, name)
        type(json_value_t), intent(in) :: value
        character(len=*), intent(in) :: name

        number_field = int(json_number_value(json_member(value, name)))
    end function number_field

    integer function line_count(text)
        character(len=*), intent(in) :: text
        integer :: position

        line_count = 0
        do position = 1, len(text)
            if (text(position:position) == new_line('a')) line_count = line_count + 1
        end do
    end function line_count

    subroutine assert_receipts(value, capture)
        type(json_value_t), intent(in) :: value
        logical, intent(in) :: capture
        type(json_value_t) :: events, event, previous
        character(:), allocatable :: name, path, completion, recovered_bytes
        integer :: index, prior, passes, failures, blocked

        events = json_member(value, 'events')
        passes = 0
        failures = 0
        blocked = 0
        do index = 1, json_size(events)
            event = json_element(events, index)
            completion = gremlin_field(event, 'completion_id')
            call assert_true(len(completion) > 0, 'receipt keeps a unique completion identity')
            do prior = 1, index - 1
                previous = json_element(events, prior)
                call assert_true(gremlin_field(previous, 'completion_id') /= completion, &
                    'recovered journal has no duplicate completion IDs')
            end do
            name = gremlin_field(event, 'case_id')
            if (name == 'test_blocked') blocked = blocked + 1
            if (name /= 'test_pass' .and. name /= 'test_fail') cycle
            call assert_equal_string(gremlin_field(event, 'session_id'), first_owner, &
                'recovered receipt retains its original owner identity')
            path = gremlin_field(event, 'log_path')
            call assert_true(len(path) > 0, 'completed receipt references its output artifact')
            if (len(path) == 0) cycle
            call assert_true(path(1:1) == '/', 'completed artifact has an absolute reference')
            call assert_true(regular_file(path//c_null_char) == 1_c_int, &
                'referenced completed artifact survives recovery as a regular file')
            if (.not. file_exists(path)) cycle
            if (name == 'test_pass') then
                passes = passes + 1
                call assert_equal_string(gremlin_field(event, 'status'), 'PASS', &
                    'completed passing receipt retains its actual verdict')
                if (capture) then
                    pass_path = path
                    pass_bytes = read_text(path)
                else if (allocated(pass_path)) then
                    call assert_equal_string(path, pass_path, &
                        'passing receipt retains its original artifact reference')
                    recovered_bytes = read_text(path)
                    call assert_true(len(recovered_bytes) == len(pass_bytes) .and. &
                        recovered_bytes == pass_bytes, &
                        'passing artifact bytes survive repeated interrupted recovery')
                end if
            else
                failures = failures + 1
                call assert_equal_string(gremlin_field(event, 'status'), 'FAIL', &
                    'completed failing receipt retains its actual verdict')
                if (capture) then
                    fail_path = path
                    fail_bytes = read_text(path)
                    call assert_contains(fail_bytes, 'journal-recovery-fail-token-4e812a', &
                        'completed failing artifact contains its independent output token')
                else if (allocated(fail_path)) then
                    call assert_equal_string(path, fail_path, &
                        'failing receipt retains its original artifact reference')
                    recovered_bytes = read_text(path)
                    call assert_true(len(recovered_bytes) == len(fail_bytes) .and. &
                        recovered_bytes == fail_bytes, &
                        'failing artifact bytes survive repeated interrupted recovery')
                end if
            end if
        end do
        call assert_equal_integer(passes, 1, 'completed passing receipt appears exactly once')
        call assert_equal_integer(failures, 1, 'completed failing receipt appears exactly once')
        call assert_equal_integer(blocked, 0, 'interrupted case has no completion receipt')
        if (capture) call assert_true(allocated(pass_path) .and. allocated(fail_path), &
            'both completed artifacts are independently snapshotted before the crash')
    end subroutine assert_receipts

end program test_gremlin_journal_recovery
