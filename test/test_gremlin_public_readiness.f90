program test_gremlin_public_readiness
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none

    character(len=4096) :: root, fixture, prefix, cli_file, rpc_file, rpc_input, driver_path
    character(len=4096) :: primary_fixture, state_directory
    character(len=65536) :: cli_json, rpc_json, output
    character(len=32) :: conditions(5)
    character(len=64) :: previous_generation, receipt_cursor, event_cursor
    character(len=128) :: event_session
    integer :: unit, ios, status, failures, i
    integer(int64) :: clock
    logical :: driver_exists
    integer(c_int) :: stop_signal, continue_signal

    interface
        function signal_process(pid, signal) bind(C, name='kill') result(rc)
            import :: c_int
            integer(c_int), value :: pid, signal
            integer(c_int) :: rc
        end function signal_process
        function pause_microseconds(duration) bind(C, name='usleep') result(rc)
            import :: c_int
            integer(c_int), value :: duration
            integer(c_int) :: rc
        end function pause_microseconds
    end interface

    failures = 0
    call get_environment_variable('FO_BIN', driver_path, status=ios)
    driver_exists = ios == 0 .and. len_trim(driver_path) > 0
    if (driver_exists) inquire(file=trim(driver_path), exist=driver_exists)
    call check(driver_exists, 'test runner provides the assigned pinned FO_BIN')
    if (.not. driver_exists) stop 1
    call get_environment_variable('PWD', root, status=ios)
    call check(ios == 0, 'project working directory is available')
    if (ios /= 0) stop 1
    call system_clock(clock)
    write (fixture, '(a,i0)') '/var/tmp/fo-public-readiness-', clock
    primary_fixture = fixture
    state_directory = trim(fixture)//'-state'
    prefix = 'cd '//quote(trim(root))//' && FO_DISABLE_SELF_REFRESH=1 '// &
        'FO_GREMLIN_STATE_DIR='//quote(trim(state_directory))//' '// &
        quote(trim(driver_path))//' '
    cli_file = trim(fixture)//'-cli.json'
    rpc_file = trim(fixture)//'-rpc.json'
    rpc_input = trim(fixture)//'-rpc-input.jsonl'
    stop_signal = 19_c_int
    continue_signal = 18_c_int
    call command('uname -s > '//quote(trim(cli_file)), status)
    call read_output(cli_file, output)
    if (trim(output) == 'Darwin') then
        stop_signal = 17_c_int
        continue_signal = 19_c_int
    end if
    call command('mkdir -p '//quote(trim(fixture)//'/test')//' '// &
        quote(trim(fixture)//'/src'), status)
    if (status /= 0) stop 1
    open (newunit=unit, file=trim(fixture)//'/fpm.toml', status='replace')
    write (unit, '(a)') 'name = "public_readiness_fixture"'
    write (unit, '(a)') '[extra.fo]'
    write (unit, '(a)') 'test-timeout = 10'
    write (unit, '(a)') 'slow-test-timeout = 20'
    close (unit)
    call write_provider(1, 0)
    do i = 1, 2
        open (newunit=unit, file=trim(fixture)//'/test/test_case_'// &
            achar(iachar('a') + i - 1)//'.f90', status='replace')
        write (unit, '(a)') 'program fixture_case_'//achar(iachar('a') + i - 1)
        if (i == 1) write (unit, '(a)') 'use fixture_probe, only: fixture_value'
        write (unit, '(a)') 'implicit none'
        if (i == 1) write (unit, '(a)') 'if (fixture_value() /= 1) error stop 9'
        write (unit, '(a)') 'end program fixture_case_'//achar(iachar('a') + i - 1)
        close (unit)
    end do
    open (newunit=unit, file=trim(fixture)//'/test/test_case_slow.f90', status='replace')
    write (unit, '(a)') 'program fixture_slow'
    write (unit, '(a)') 'use, intrinsic :: iso_c_binding, only: c_int'
    write (unit, '(a)') 'implicit none'
    write (unit, '(a)') 'interface'
    write (unit, '(a)') 'function sleep_seconds(seconds) bind(C, name="sleep") result(left)'
    write (unit, '(a)') 'import :: c_int'
    write (unit, '(a)') 'integer(c_int), value :: seconds'
    write (unit, '(a)') 'integer(c_int) :: left'
    write (unit, '(a)') 'end function sleep_seconds'
    write (unit, '(a)') 'end interface'
    write (unit, '(a)') 'if (sleep_seconds(6_c_int) /= 0) error stop 1'
    write (unit, '(a)') 'end program fixture_slow'
    close (unit)

    call cli('start --seed 17 --random 2', cli_json, status)
    call check(status == 0, 'real CLI starts an owned fixture campaign')
    if (status == 0) then
        call cli('wait --until fully-verified --wait-ms 30000', cli_json, status)
        call check(status == 0, 'real CLI reads full verification')
        call check(field(cli_json, 'wait_satisfied') == 'true', &
            'slow case longer than five seconds passes with its normal budget')
        call cli('wait --until quiescent --wait-ms 30000', cli_json, status)
        call check(status == 0, 'fully covered fixture enters quiescence')
        call check(field(cli_json, 'wait_satisfied') == 'true', &
            'quiescent wait observes the completed fixture')
        call cli('status', cli_json, status)
        call mcp('status', '', rpc_json, status)
        call check(trim(cli_json) == trim(rpc_json), &
            'independent CLI and MCP processes report identical exact readiness facts')
        call check(field(cli_json, 'local_gate_green') == 'true', &
            'public gate is green for the verified current generation')
        call check(field(cli_json, 'fully_verified') == 'true', &
            'public full verification includes the slow inventory')
        call check(field(cli_json, 'gate_token_version') == '1', &
            'public gate token is explicitly versioned')
        conditions = [character(len=32) :: 'local-gate-green', 'ordinary-verified', &
            'fully-verified', 'quiescent', 'failure']
        do i = 1, size(conditions)
            call cli('wait --until '//trim(conditions(i))//' --wait-ms 0', &
                cli_json, status)
            call check(status == 0, 'CLI typed wait returns a factual result')
            call mcp('wait', trim(conditions(i)), rpc_json, status)
            call check(status == 0, 'MCP typed wait returns a factual result')
            call check(trim(cli_json) == trim(rpc_json), &
                trim(conditions(i))//' returns identical state and outcome across processes')
            if (i < 5) then
                call check(field(cli_json, 'wait_satisfied') == 'true', &
                    trim(conditions(i))//' is independently expected true')
            else
                call check(field(cli_json, 'wait_satisfied') == 'false', &
                    'failure wait cannot be satisfied by an all-pass fixture')
                call check(field(cli_json, 'wait_timed_out') == 'true', &
                    'false zero-duration failure wait reports its timeout')
            end if
        end do
        call exercise_token_lifecycle()
        call remember_cursors()
        open (newunit=unit, file=trim(fixture)//'/test/test_case_flaky.f90', &
            status='replace')
        write (unit, '(a)') 'program fixture_flaky'
        write (unit, '(a)') 'implicit none'
        write (unit, '(a)') 'logical :: seen'
        write (unit, '(a)') 'integer :: unit'
        write (unit, '(a)') "inquire(file='"//trim(fixture)//"-once', exist=seen)"
        write (unit, '(a)') 'if (.not. seen) then'
        write (unit, '(a)') "open(newunit=unit, file='"//trim(fixture)// &
            "-once', status='replace')"
        write (unit, '(a)') 'close(unit)'
        write (unit, '(a)') 'error stop 7'
        write (unit, '(a)') 'end if'
        write (unit, '(a)') 'end program fixture_flaky'
        close (unit)
        call await_event('test_flaky')
        call check(index(cli_json, '"status":"FLAKY"') > 0, &
            'Gremlin retains FLAKY rather than converting runner exit zero to PASS')
        call check(field(cli_json, 'local_gate_green') == 'false' .and. &
            field(cli_json, 'fully_verified') == 'false', &
            'a flaky current-generation case prevents both gate and full green')
        call remember_cursors()
        call write_provider(2, 0)
        call await_event('regression_confirmed')
        call check(index(cli_json, '"status":"FAIL"') > 0, &
            'unchanged oracle detects repeatable failure after an earlier PASS')
        call check(field(cli_json, 'local_gate_green') == 'false', &
            'confirmed current regression clears the gate')
        call remember_cursors()
        open (newunit=unit, file=trim(fixture)//'/fpm.toml', status='replace')
        write (unit, '(a)') 'name = "public_readiness_fixture"'
        write (unit, '(a)') '[extra.fo]'
        write (unit, '(a)') 'test-timeout = 1'
        write (unit, '(a)') 'test-wall-timeout = 1'
        write (unit, '(a)') 'slow-test-timeout = 20'
        close (unit)
        call write_provider(1, 2)
        call await_event('test_timeout')
        call check(index(cli_json, '"status":"TIMEOUT"') > 0, &
            'normal runner timeout is retained as TIMEOUT')
        call check(field(cli_json, 'local_gate_green') == 'false', &
            'timeout clears the gate without asserting regression')
        call remember_cursors()
        open (newunit=unit, file=trim(fixture)//'/test/test_case_new_fail.f90', &
            status='replace')
        write (unit, '(a)') 'program fixture_new_failure'
        write (unit, '(a)') 'implicit none'
        write (unit, '(a)') 'error stop 11'
        write (unit, '(a)') 'end program'
        close (unit)
        call await_event('test_failed')
        call check(index(cli_json, '"status":"FAIL"') > 0, &
            'new repeatable failure without an earlier PASS is test_failed')

    end if
    call stop_fixture()
    call check(status == 0, 'owned fixture campaign stops through the public CLI')
    if (status == 0) call exercise_infra_failure()
    if (status == 0 .and. failures == 0) then
        call command('rm -rf '//quote(trim(primary_fixture))//' '// &
            quote(trim(fixture))//' '//quote(trim(state_directory))//' '// &
            quote(trim(cli_file))//' '//quote(trim(rpc_file))//' '// &
            quote(trim(rpc_input))//' '//quote(trim(primary_fixture)//'-once'), status)
    else
        write (error_unit, '(a)') 'Preserving failed public-readiness fixture:'
        write (error_unit, '(a)') 'fixture='//trim(primary_fixture)
        write (error_unit, '(a)') 'state='//trim(state_directory)
        write (error_unit, '(a)') 'cli='//trim(cli_file)
        write (error_unit, '(a)') 'mcp='//trim(rpc_file)
        write (error_unit, '(a)') 'mcp_input='//trim(rpc_input)
    end if
    if (failures > 0) stop 1
    print '(a)', 'Public readiness: typed waits, token invalidation/restart and exact verdict events PASS'

contains

    subroutine stop_fixture()
        character(len=64) :: session
        integer :: attempt, exitcode
        session = field(cli_json, 'session_id')
        call cli('stop --session '//trim(session), cli_json, status)
        if (status /= 0) return
        do attempt = 1, 200
            call cli('status', cli_json, exitcode)
            if (exitcode /= 0) return
            exitcode = pause_microseconds(50000_c_int)
        end do
        status = 1
        call check(.false., 'owned owner releases before generated fixture cleanup')
    end subroutine stop_fixture

    logical function has_invalidation_event() result(found)
        character(len=65536) :: event_json
        integer :: first, last, offset
        found = .false.
        offset = 1
        do
            first = index(cli_json(offset:), '"event_type":')
            if (first == 0) exit
            first = first + offset - 1
            last = index(cli_json(first:), '}')
            if (last == 0) exit
            last = last + first - 1
            event_json = cli_json(first:last)
            if (field(event_json, 'event_type') == 'local_gate_changed' .and. &
                field(event_json, 'active_generation') == previous_generation .and. &
                field(event_json, 'candidate_generation') == previous_generation .and. &
                field(event_json, 'dirty') == 'true' .and. &
                field(event_json, 'local_gate_green') == 'false' .and. &
                field(event_json, 'gate_token') == '') found = .true.
            offset = last + 1
        end do
    end function has_invalidation_event

    subroutine exercise_token_lifecycle()
        character(len=64) :: held_token, owner_id, pid_text
        integer :: source_unit, attempt, exitcode, owner_pid, split, parse_status, condition_index
        logical :: pending, consumer_allowed, paused
        held_token = field(cli_json, 'gate_token')
        call check(len_trim(held_token) == 64, 'blocked consumer retains a green token')
        call remember_cursors()
        owner_id = field(cli_json, 'session_id')
        split = index(owner_id, '-')
        owner_pid = 0
        if (split > 1) read (owner_id(:split - 1), *, iostat=parse_status) owner_pid
        call check(owner_pid > 0, 'green public session identifies its owned process')
        if (owner_pid <= 0) return
        exitcode = signal_process(int(owner_pid, c_int), stop_signal)
        call check(exitcode == 0, 'pauses owned change processing before consumer release')
        write (pid_text, '(i0)') owner_pid
        paused = .false.
        do attempt = 1, 20
            call command('ps -o stat= -p '//trim(pid_text)//' > '// &
                quote(trim(cli_file)), exitcode)
            call read_output(cli_file, output)
            output = adjustl(output)
            paused = output(1:1) == 'T'
            if (paused) exit
            exitcode = pause_microseconds(5000_c_int)
        end do
        call check(paused, 'independent process table confirms owner is stopped')

        open (newunit=source_unit, file=trim(fixture)//'/src/fixture_probe.f90', &
            status='old', position='append')
        write (source_unit, '(a)') '! relevant source closure changes while consumer is blocked'
        close (source_unit)
        call cli('status', cli_json, exitcode)
        consumer_allowed = field(cli_json, 'gate_token') == held_token .and. &
            field(cli_json, 'local_gate_green') == 'true'
        call check(exitcode == 0 .and. .not. consumer_allowed .and. &
            field(cli_json, 'gate_token') == '' .and. &
            field(cli_json, 'local_gate_green') == 'false' .and. &
            field(cli_json, 'active_generation') == previous_generation .and. &
            field(cli_json, 'candidate_generation') == previous_generation, &
            'paused-owner status rejects old token before provider poll or replacement')
        call cli('wait --until local-gate-green --wait-ms 0', cli_json, exitcode)
        call check(exitcode == 0 .and. field(cli_json, 'wait_satisfied') == 'false' .and. &
            field(cli_json, 'local_gate_green') == 'false' .and. &
            index(cli_json, '"event_type":"local_gate_green"') > 0, &
            'paused-owner typed wait rejects historical lifecycle green events')
        do condition_index = 2, 4
            call cli('wait --until '//trim(conditions(condition_index))//' --wait-ms 0', &
                cli_json, exitcode)
            call check(exitcode == 0 .and. field(cli_json, 'wait_satisfied') == 'false', &
                'paused owner rejects historical '//trim(conditions(condition_index)))
        end do
        call mcp('wait', 'local-gate-green', rpc_json, exitcode)
        call check(exitcode == 0 .and. field(rpc_json, 'wait_satisfied') == 'false' .and. &
            field(rpc_json, 'gate_token') == '', &
            'independent MCP consumer also rejects green before owner processing resumes')
        exitcode = signal_process(int(owner_pid, c_int), continue_signal)
        call check(exitcode == 0, 'resumes only the owned fixture change provider')
        pending = .false.
        do attempt = 1, 30
            call cli('wait --wait-ms 1000 --cursor '//trim(receipt_cursor)// &
                ' --lifecycle-cursor '//trim(event_cursor), cli_json, exitcode)
            if (exitcode /= 0) exit
            if (has_invalidation_event()) then
                pending = .true.
                exit
            end if
            receipt_cursor = field(cli_json, 'next_cursor')
            event_cursor = field(cli_json, 'next_lifecycle_cursor')
        end do
        call check(pending, &
            'watch invalidates token before replacement generation exists')
        consumer_allowed = field(cli_json, 'gate_token') == held_token .and. &
            field(cli_json, 'local_gate_green') == 'true'
        call check(.not. consumer_allowed, 'blocked consumer rejects old token before reuse')
        call cli('wait --until local-gate-green --wait-ms 0', cli_json, exitcode)
        call check(field(cli_json, 'wait_satisfied') /= 'true' .or. &
            field(cli_json, 'gate_token') /= held_token, &
            'typed wait never returns historical green after input mutation')
        call cli('wait --until fully-verified --wait-ms 30000', cli_json, exitcode)
        call check(exitcode == 0 .and. field(cli_json, 'wait_satisfied') == 'true', &
            'replacement closure completes before restart fixture')
        call cli('wait --until quiescent --wait-ms 30000', cli_json, exitcode)
        held_token = field(cli_json, 'gate_token')
        owner_id = field(cli_json, 'session_id')
        split = index(owner_id, '-')
        owner_pid = 0
        if (split > 1) read (owner_id(:split - 1), *, iostat=parse_status) owner_pid
        call check(owner_pid > 0, 'public owned session identifies restart fixture process')
        if (owner_pid <= 0) return
        exitcode = signal_process(int(owner_pid, c_int), 9_c_int)
        call check(exitcode == 0, 'abruptly terminates only the quiescent fixture owner')
        exitcode = pause_microseconds(100000_c_int)
        call write_provider(2, 0)
        call cli('wait --until local-gate-green --wait-ms 0', cli_json, exitcode)
        call check(exitcode /= 0 .or. field(cli_json, 'wait_satisfied') == 'false', &
            'dead owner cannot restore historical green from persisted receipts')
        call cli('start --target test_case_a --seed 17 --random 2', cli_json, exitcode)
        call check(exitcode == 0, 'restarts an owner against the changed source closure')
        call cli('wait --until local-gate-green --wait-ms 0', cli_json, exitcode)
        call check(field(cli_json, 'wait_satisfied') == 'false' .and. &
            field(cli_json, 'gate_token') /= held_token, &
            'restarted owner has no persisted green before fresh closure validation')
        call cli('wait --until failure --wait-ms 30000', cli_json, exitcode)
        call check(exitcode == 0 .and. field(cli_json, 'wait_satisfied') == 'true' .and. &
            field(cli_json, 'local_gate_green') == 'false', &
            'restarted watcher validates changed closure and observes current failure')
        call write_provider(1, 0)
        call cli('wait --until fully-verified --wait-ms 30000', cli_json, exitcode)
        if (exitcode /= 0 .or. field(cli_json, 'wait_satisfied') /= 'true' .or. &
            field(cli_json, 'gate_token') == held_token) then
            write (error_unit, '(a)') 'Restarted readiness diagnostic:'
            write (error_unit, '(a)') 'held_token='//trim(held_token)
            write (error_unit, '(a)') 'new_token='//field(cli_json, 'gate_token')
            write (error_unit, '(a)') 'active_generation='// &
                field(cli_json, 'active_generation')
            write (error_unit, '(a)') 'candidate_generation='// &
                field(cli_json, 'candidate_generation')
            write (error_unit, '(a)') 'gate_required='//field(cli_json, 'gate_required')// &
                ' gate_passed='//field(cli_json, 'gate_passed')
            write (error_unit, '(a)') 'ordinary_required='// &
                field(cli_json, 'ordinary_required')//' ordinary_passed='// &
                field(cli_json, 'ordinary_passed')
            write (error_unit, '(a)') 'full_required='// &
                field(cli_json, 'full_required')//' full_passed='// &
                field(cli_json, 'full_passed')
            write (error_unit, '(a)') 'requirement_digest='// &
                field(cli_json, 'requirement_digest')
            write (error_unit, '(a)') 'last_outcome='//field(cli_json, 'last_outcome')// &
                ' diagnostic='//field(cli_json, 'diagnostic')
            write (error_unit, '(a)') 'raw cli_json='//trim(cli_json)
        end if
        call check(exitcode == 0 .and. field(cli_json, 'wait_satisfied') == 'true' .and. &
            field(cli_json, 'gate_token') /= held_token, &
            'armed restart watcher verifies replacement and issues a fresh token')
        call cli('wait --until quiescent --wait-ms 30000', cli_json, exitcode)
        call check(exitcode == 0 .and. field(cli_json, 'wait_satisfied') == 'true', &
            'restarted fixture finishes its current generation')
    end subroutine exercise_token_lifecycle

    subroutine exercise_infra_failure()
        character(len=4096) :: logs
        character(len=65536) :: log_file
        integer :: source_unit, attempt, exitcode, split
        logical :: started
        fixture = trim(primary_fixture)//'-infra'
        call command('mkdir -p '//quote(trim(fixture)//'/test'), exitcode)
        call check(exitcode == 0, 'creates isolated launch-failure fixture')
        open (newunit=source_unit, file=trim(fixture)//'/fpm.toml', status='replace')
        write (source_unit, '(a)') 'name = "public_infra_fixture"'
        close (source_unit)
        open (newunit=source_unit, file=trim(fixture)//'/test/test_infra_slow.f90', &
            status='replace')
        write (source_unit, '(a)') 'program fixture_slow'
        write (source_unit, '(a)') 'use, intrinsic :: iso_c_binding, only: c_int'
        write (source_unit, '(a)') 'implicit none'
        write (source_unit, '(a)') 'integer :: unit'
        write (source_unit, '(a)') 'interface'
        write (source_unit, '(a)') 'function sleep_seconds(n) bind(C,name="sleep") result(left)'
        write (source_unit, '(a)') 'import :: c_int'
        write (source_unit, '(a)') 'integer(c_int), value :: n'
        write (source_unit, '(a)') 'integer(c_int) :: left'
        write (source_unit, '(a)') 'end function'
        write (source_unit, '(a)') 'end interface'
        write (source_unit, '(a)') "open(newunit=unit, file='"//trim(fixture)// &
            "/started', status='replace')"
        write (source_unit, '(a)') 'close(unit)'
        write (source_unit, '(a)') 'if (sleep_seconds(3_c_int) /= 0) error stop 1'
        write (source_unit, '(a)') 'end program'
        close (source_unit)
        open (newunit=source_unit, file=trim(fixture)//'/test/test_infra_fast.f90', &
            status='replace')
        write (source_unit, '(a)') 'program fixture_fast'
        write (source_unit, '(a)') 'implicit none'
        write (source_unit, '(a)') 'end program'
        close (source_unit)
        call cli('start --target test_infra_slow --target test_infra_fast --random 1', &
            cli_json, exitcode)
        call check(exitcode == 0, 'starts isolated launch-failure campaign')
        started = .false.
        do attempt = 1, 200
            inquire (file=trim(fixture)//'/started', exist=started)
            if (started) exit
            exitcode = pause_microseconds(50000_c_int)
        end do
        call check(started, 'first child signals its actual process start')
        if (started) then
            call cli('status', cli_json, exitcode)
            log_file = field(cli_json, 'log_path')
            split = scan(trim(log_file), '/', back=.true.)
            call check(split > 0, 'public build receipt identifies owned log directory')
            if (split > 0) then
                logs = log_file(:split - 1)
                call remember_cursors()
                previous_generation = ''
                call command('chmod 000 '//quote(trim(logs)), exitcode)
                call check(exitcode == 0, 'denies next launch its isolated log output')
                call await_event('test_infra_error')
                call check(field(cli_json, 'local_gate_green') == 'false', &
                    'infrastructure launch failure clears the gate')
                call command('chmod 700 '//quote(trim(logs)), exitcode)
                call check(exitcode == 0, 'restores owned log directory permissions')
            end if
        end if
        call stop_fixture()
        call check(status == 0, 'stops isolated infrastructure fixture')
    end subroutine exercise_infra_failure

    subroutine write_provider(value, delay)
        integer, intent(in) :: value, delay
        integer :: source_unit
        open (newunit=source_unit, file=trim(fixture)//'/src/fixture_probe.f90', &
            status='replace')
        write (source_unit, '(a)') 'module fixture_probe'
        write (source_unit, '(a)') 'use, intrinsic :: iso_c_binding, only: c_int'
        write (source_unit, '(a)') 'implicit none'
        write (source_unit, '(a)') 'interface'
        write (source_unit, '(a)') 'function delay_seconds(n) bind(C,name="sleep") result(left)'
        write (source_unit, '(a)') 'import :: c_int'
        write (source_unit, '(a)') 'integer(c_int), value :: n'
        write (source_unit, '(a)') 'integer(c_int) :: left'
        write (source_unit, '(a)') 'end function'
        write (source_unit, '(a)') 'end interface'
        write (source_unit, '(a)') 'contains'
        write (source_unit, '(a)') 'integer function fixture_value() result(value)'
        write (source_unit, '(a,i0,a)') 'if (delay_seconds(', delay, '_c_int) /= 0) error stop 2'
        write (source_unit, '(a,i0)') 'value = ', value
        write (source_unit, '(a)') 'end function'
        write (source_unit, '(a)') 'end module'
        close (source_unit)
    end subroutine write_provider

    subroutine remember_cursors()
        previous_generation = field(cli_json, 'active_generation')
        event_session = field(cli_json, 'session_id')
        receipt_cursor = field(cli_json, 'next_cursor')
        event_cursor = field(cli_json, 'next_lifecycle_cursor')
    end subroutine remember_cursors

    subroutine await_event(expected)
        character(len=*), intent(in) :: expected
        integer :: attempt, exitcode
        logical :: observed
        observed = .false.
        do attempt = 1, 40
            call cli('wait --session '//trim(event_session)//' --wait-ms 1000 --cursor '// &
                trim(receipt_cursor)// &
                ' --lifecycle-cursor '//trim(event_cursor), cli_json, exitcode)
            if (exitcode /= 0) exit
            if (field(cli_json, 'active_generation') /= previous_generation .and. &
                has_current_event(expected)) then
                observed = .true.
                exit
            end if
            receipt_cursor = field(cli_json, 'next_cursor')
            event_cursor = field(cli_json, 'next_lifecycle_cursor')
        end do
        call check(observed, 'public lifecycle emits exact '//expected//' verdict event')
    end subroutine await_event

    logical function has_current_event(expected) result(found)
        character(len=*), intent(in) :: expected
        character(len=65536) :: event_json
        character(len=64) :: active
        integer :: first, last, offset
        found = .false.
        active = field(cli_json, 'active_generation')
        if (active == previous_generation) return
        offset = 1
        do
            first = index(cli_json(offset:), '"event_type":')
            if (first == 0) exit
            first = first + offset - 1
            last = index(cli_json(first:), '}')
            if (last == 0) exit
            last = last + first - 1
            event_json = cli_json(first:last)
            if (field(event_json, 'generation') == active) then
                call check(field(event_json, 'event_type') /= 'regression_confirmed' &
                    .or. expected == 'regression_confirmed', &
                    trim(expected)//' must never be labeled regression_confirmed')
                if (field(event_json, 'event_type') == expected) found = .true.
            end if
            offset = last + 1
        end do
    end function has_current_event

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label
        if (condition) return
        failures = failures + 1
        write (error_unit, '(a)') 'FAIL: '//label
    end subroutine check

    subroutine command(text, exitcode)
        character(len=*), intent(in) :: text
        integer, intent(out) :: exitcode
        integer :: launch_error
        call execute_command_line(text, exitstat=exitcode, cmdstat=launch_error)
        if (launch_error /= 0) exitcode = launch_error
    end subroutine command

    subroutine cli(arguments, json, exitcode)
        character(len=*), intent(in) :: arguments
        character(len=*), intent(out) :: json
        integer, intent(out) :: exitcode
        character(len=4096) :: full_arguments
        full_arguments = trim(arguments)
        if (index(trim(arguments), 'status') == 1 .or. &
                index(trim(arguments), 'wait') == 1) &
            full_arguments = trim(arguments)//' --detail full'
        call command(trim(prefix)//' gremlin '//trim(full_arguments)//' --dir '// &
            quote(trim(fixture))//' --lane public-readiness > '// &
            quote(trim(cli_file)), exitcode)
        call read_output(cli_file, json)
    end subroutine cli

    subroutine mcp(action, condition, json, exitcode)
        character(len=*), intent(in) :: action, condition
        character(len=*), intent(out) :: json
        integer, intent(out) :: exitcode
        integer :: rpc_unit
        character(len=:), allocatable :: parameters
        parameters = '"action":"gremlin_'//action//'","dir":"'//trim(fixture)// &
            '","lane_id":"public-readiness"'
        if (action == 'status' .or. action == 'wait') parameters = parameters// &
            ',"detail":"full"'
        if (len_trim(condition) > 0) parameters = parameters// &
            ',"wait_until":"'//condition//'","wait_ms":0'
        open (newunit=rpc_unit, file=trim(rpc_input), status='replace')
        write (rpc_unit, '(a)') '{"jsonrpc":"2.0","id":1,"method":"initialize",'// &
            '"params":{"protocolVersion":"2024-11-05","capabilities":{},'// &
            '"clientInfo":{"name":"independent-fortran-oracle","version":"1"}}}'
        write (rpc_unit, '(a)') '{"jsonrpc":"2.0","method":"notifications/initialized"}'
        write (rpc_unit, '(a)') '{"jsonrpc":"2.0","id":2,"method":"tools/call",'// &
            '"params":{"name":"fo","arguments":{'//parameters//'}}}'
        write (rpc_unit, '(a)') '{"jsonrpc":"2.0","id":3,"method":"shutdown"}'
        write (rpc_unit, '(a)') '{"jsonrpc":"2.0","method":"exit"}'
        close (rpc_unit)
        call command(trim(prefix)//' mcp-server < '//quote(trim(rpc_input))// &
            ' > '//quote(trim(rpc_file)), exitcode)
        call read_output(rpc_file, output)
        json = field(output, 'text')
    end subroutine mcp

    subroutine read_output(filename, text)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: text
        integer :: file_unit, read_status, used
        character(len=65536) :: line
        text = ''
        used = 0
        open (newunit=file_unit, file=trim(filename), status='old', iostat=read_status)
        if (read_status /= 0) return
        do
            read (file_unit, '(a)', iostat=read_status) line
            if (read_status /= 0) exit
            if (used + len_trim(line) > len(text)) exit
            text(used + 1:used + len_trim(line)) = trim(line)
            used = used + len_trim(line)
        end do
        close (file_unit)
    end subroutine read_output

    function field(json, key) result(value)
        character(len=*), intent(in) :: json, key
        character(len=65536) :: value
        integer :: at, cursor, used, n
        logical :: quoted
        value = ''
        at = index(json, '"'//key//'":')
        if (at == 0) return
        cursor = at + len(key) + 3
        n = len_trim(json)
        do while (cursor <= n)
            if (json(cursor:cursor) /= ' ') exit
            cursor = cursor + 1
        end do
        if (cursor > n) return
        quoted = json(cursor:cursor) == '"'
        if (quoted) cursor = cursor + 1
        used = 0
        do while (cursor <= n)
            if (quoted) then
                if (json(cursor:cursor) == '"') exit
                if (json(cursor:cursor) == achar(92)) then
                    cursor = cursor + 1
                    if (cursor > n) return
                end if
            else
                if (index(',}'//achar(10), json(cursor:cursor)) > 0) exit
            end if
            used = used + 1
            value(used:used) = json(cursor:cursor)
            cursor = cursor + 1
        end do
    end function field

    function quote(text) result(quoted)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: quoted
        integer :: i
        quoted = "'"
        do i = 1, len(text)
            if (text(i:i) == "'") then
                quoted = quoted//"'"//'"'//"'"//'"'//"'"
            else
                quoted = quoted//text(i:i)
            end if
        end do
        quoted = quoted//"'"
    end function quote
end program test_gremlin_public_readiness
