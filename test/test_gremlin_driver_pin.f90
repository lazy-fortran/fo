program test_gremlin_driver_pin
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use, intrinsic :: iso_c_binding, only: c_int
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, join_path, write_text, write_lines, read_text, file_exists, &
        current_directory, list_extend, &
        move_path, spawn_process, poll_process, terminate_process_group, &
        assert_true, assert_contains, assert_equal_string, assert_process_ok, &
        finish_assertions, remove_tree
    use fo_test_cli, only: resolve_driver, run_fo, run_external, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, &
        json_string_value, json_number_value, json_size
    implicit none

    character(:), allocatable :: driver, scratch, project, public, cache, state
    character(:), allocatable :: platform, digest_a, digest_b, size_a, session, root
    character(:), allocatable :: generation, head, new_head, old_pins
    type(string_list_t) :: arguments, environment, gated_environment
    type(process_result_t) :: result
    type(json_value_t) :: report, receipt
    character(len=128) :: sessions(3) = ""
    character(len=8192) :: debug_lines(8)
    character(len=80) :: source_lines(3)
    integer :: debugger, attempt, status, rc
    integer(int64) :: deadline
    logical :: reaped
    interface
        integer(c_int) function signal_owned(pid, signal) bind(C, name='kill')
            import :: c_int
            integer(c_int), value :: pid, signal
        end function signal_owned
        integer(c_int) function current_uid() bind(C, name='getuid')
            import :: c_int
        end function current_uid
        integer(c_int) function pause_us(duration) bind(C, name='usleep')
            import :: c_int
            integer(c_int), value :: duration
        end function pause_us
    end interface

    call resolve_driver(driver)
    call make_scratch('fo-driver-pin', scratch)
    project = join_path(scratch, 'project')
    public = join_path(scratch, 'public/fo')
    cache = join_path(scratch, 'cache')
    state = join_path(scratch, 'state')
    call list_add(environment, 'FO_DISABLE_SELF_REFRESH=1')
    call list_add(environment, 'FO_SELF_REFRESH=0')
    call list_add(environment, 'FO_CACHE_DIR='//cache)
    call list_add(environment, 'FO_GREMLIN_STATE_DIR='//state)
    call list_add(environment, 'FO_JOBS=1')
    call list_add(environment, 'FO_GREMLIN_TEST_CAPTURE_COUNTER='//scratch//'/capture.json')
    call list_add(environment, 'FO_PREFIX='//scratch//'/prefix')
    call write_text(project//'/fpm.toml', 'name = "driver_pin_fixture"'//new_line('a'))
    source_lines = ['program test_probe    ', 'implicit none         ', &
        'end program test_probe']
    call write_lines(project//'/test/test_probe.f90', source_lines)
    arguments = string_list_t()
    call run_external('uname', arguments_with('-s'), scratch, result)
    call assert_process_ok(result, 'discover native platform')
    platform = result%stdout(:index(result%stdout, new_line('a')) - 1)
    call native_image(public, 'A')
    digest_a = digest(public)
    inquire (file=public, size=status)
    size_a = integer_text(status)
    call native_image(scratch//'/image-b', 'B')
    digest_b = digest(scratch//'/image-b')
    call assert_true(digest_a /= digest_b, 'native A and B have different bytes')
    call run_external('mkfifo', arguments_with(scratch//'/before-main'), &
        scratch, result)
    call assert_process_ok(result, 'create deterministic before-pin gate')
    call list_extend(gated_environment, environment)
    if (platform == 'Darwin') then
        ! LLDB needs interactive host authorization. A native OS interposer
        ! gates the second exec of private A before its first image pin.
        call current_directory(root)
        arguments = arguments_with('-dynamiclib', root// &
            '/test/fixtures/gremlin_image_gate.c', '-o', scratch//'/image-gate.dylib')
        call run_external('cc', arguments, scratch, result)
        call assert_process_ok(result, 'compile the small Darwin image gate shim')
        call list_add(gated_environment, 'DYLD_INSERT_LIBRARIES='// &
            scratch//'/image-gate.dylib')
        call list_add(gated_environment, 'FO_DRIVER_PIN_GATE='//scratch//'/before-main')
        arguments = arguments_with(public, 'gremlin', 'start', '--dir', project)
        call list_add(arguments, '--lane')
        call list_add(arguments, 'before-pin')
        call list_add(arguments, '--target')
        call list_add(arguments, 'test_probe')
    else
        debug_lines = [character(len=8192) :: &
            'set pagination off', 'set confirm off', 'set startup-with-shell off', &
            'break main', 'run gremlin start --dir '//project// &
            ' --lane before-pin --target test_probe --campaign-seconds 60', &
            'shell touch '//scratch//'/before-main.started', &
            'shell cat '//scratch//'/before-main >/dev/null', 'continue']
        call write_lines(scratch//'/debugger', debug_lines)
        arguments = arguments_with('/bin/sh', '-c', 'exec gdb --batch -x '// &
            scratch//'/debugger '//public//' > '//scratch//'/debugger.log 2>&1')
    end if
    call spawn_process(arguments, project, debugger, gated_environment)
    call wait_file(scratch//'/before-main.started')
    ! Native A is already executing and its first pin has not happened.
    call move_path(scratch//'/image-b', public)
    call assert_equal_string(digest(public), digest_b, 'public path now contains B')
    call run_external('/bin/sh', arguments_with('-c', &
        'printf x > '//scratch//'/before-main'), scratch, result, timeout_ms=5000)
    call assert_process_ok(result, 'release debugger after atomic replacement')
    do attempt = 1, 300
        call poll_process(debugger, status)
        if (status /= 0) exit
        rc = pause_us(100000_c_int)
    end do
    if (status == 0) call terminate_process_group(debugger, status, reaped)
    call assert_true(status /= 0, 'native A returns from its gated start')
    call await_pass('before-pin', digest_a, 'A', receipt)
    generation = text(receipt, 'generation')
    call assert_equal_string(number(receipt, 'driver_size'), size_a, &
        'receipt size independently matches native image A')
    call command('before-pin', 'status', report)
    session = text(report, 'session_id')
    call assert_equal_string(text(report, 'driver_digest'), digest_a, &
        'status identifies the actual running A after pre-pin replacement')
    call assert_equal_string(digest(text(report, 'active_project')// &
        '/build/fo-gremlin-driver/fo'), digest_a, 'generation owns exactly native A')
    call command('before-pin', 'reproduce', report, 'test_probe')
    call assert_contains(read_text(text(report, 'log_path')), 'AAAAAA', &
        'reproduction launched through A although the public command is B')
    call command('before-pin', 'stop', report)
    call start_lane('after-pin')
    call await_pass('after-pin', digest_b, 'B', receipt)
    generation = text(receipt, 'generation')
    ! Replace public B by A after the immutable generation is captured.
    call native_image(scratch//'/image-next', 'A')
    call move_path(scratch//'/image-next', public)
    call command('after-pin', 'reproduce', report, 'test_probe')
    call assert_contains(read_text(text(report, 'log_path')), 'BBBBBB', &
        'reproduction retains native B after public path replacement')
    call command('after-pin', 'stop', report)
    call start_lane('after-pin')
    call await_pass('after-pin', digest_a, 'A', receipt)
    call assert_true(text(receipt, 'generation') /= generation, &
        'new session observes A with a new execution identity')
    call command('after-pin', 'stop', report)

    call git('init -q')
    call git('config user.name DriverOracle')
    call git('config user.email driver@example.invalid')
    call git('add fpm.toml test')
    call git('commit -qm initial')
    call git('rev-parse HEAD')
    head = result%stdout(:index(result%stdout, new_line('a')) - 1)
    call start_lane('metadata')
    call await_pass('metadata', digest_a, 'A', receipt)
    generation = text(receipt, 'generation')
    call assert_equal_string(text(receipt, 'base_commit'), head, &
        'Git provenance comes from independent Git')
    call read_counter(deadline)
    call git('commit --allow-empty -qm metadata')
    call git('rev-parse HEAD')
    new_head = result%stdout(:index(result%stdout, new_line('a')) - 1)
    call assert_true(new_head /= head, 'independent Git observes metadata-only commit')
    call run_external('touch', arguments_with(project), scratch, result)
    call assert_process_ok(result, 'wake capture for Git metadata provenance')
    call await_capture(deadline, generation, .true.)
    call command('metadata', 'status', report)
    call assert_equal_string(text(report, 'active_generation'), generation, &
        'Git metadata commit preserves execution generation')
    call assert_equal_string(text(report, 'base_commit'), new_head, &
        'same generation refreshes live Git provenance')
    call command('metadata', 'reproduce', report, 'test_probe')
    call assert_equal_string(text(report, 'generation'), generation, &
        'metadata commit retains reproducible generation')
    call assert_equal_string(text(report, 'base_commit'), new_head, &
        'reproduction reports refreshed provenance independently of driver identity')
    call write_text(project//'/test/test_probe.f90', &
        'program test_probe'//new_line('a')//'implicit none'//new_line('a')// &
        '! relevant execution input changed'//new_line('a')// &
        'end program test_probe'//new_line('a'))
    call await_new_pass('metadata', generation)
    call command('metadata', 'stop', report)
    if (platform == 'Darwin') call force_exit_retention()
    call run_external('find', arguments_with(state, '-name', 'fo'), scratch, result)
    old_pins = result%stdout
    call assert_contains(old_pins, '/pinned-driver/', 'session retention is visible')
    call run_external('find', arguments_with(state, '-path', '*/launch-driver/*', &
        '-name', 'fo'), scratch, result)
    call assert_equal_string(trim(result%stdout), '', 'launch copies are collected')
    call run_external('chmod', arguments_with('-R', 'u+w', scratch), scratch, result)
    call assert_process_ok(result, 'release stopped read-only fixture copies')
    call remove_tree(scratch)
    call finish_assertions()
contains
    integer function lane_index(lane) result(index)
        character(len=*), intent(in) :: lane
        select case(lane)
        case("before-pin")
            index = 1
        case("after-pin")
            index = 2
        case default
            index = 3
        end select
    end function lane_index

    subroutine force_exit_retention()
        type(json_value_t) :: snapshot, completed
        type(process_result_t) :: listing
        type(string_list_t) :: args
        character(:), allocatable :: owner, images
        integer :: pid, split, ios, i, status_signal
        call start_lane('forced-exit')
        call await_pass('forced-exit', digest_a, 'A', completed)
        call command('forced-exit', 'status', snapshot)
        owner = text(snapshot, 'session_id')
        split = index(owner, '-')
        pid = 0
        if (split > 1) read(owner(:split - 1), *, iostat=ios) pid
        call assert_true(pid > 0, 'forced-exit fixture identifies its exact owner')
        if (pid <= 0) return
        status_signal = signal_owned(int(pid, c_int), 9_c_int)
        call assert_true(status_signal == 0, 'kill only the owned bootstrap fixture')
        status_signal = pause_us(200000_c_int)
        call start_lane('forced-exit')
        call await_pass('forced-exit', digest_a, 'A', completed)
        call command('forced-exit', 'stop', snapshot)
        images = '/private/var/tmp/fo-gremlin-images-'//integer_text(int(current_uid()))
        args = arguments_with(images, '-maxdepth', '1', '-name', 'image-*')
        do i = 1, 100
            call run_external('find', args, scratch, listing)
            call assert_process_ok(listing, 'inspect owned bootstrap retention')
            if (len_trim(listing%stdout) == 0) exit
            status_signal = pause_us(50000_c_int)
        end do
        call assert_equal_string(listing%stdout, '', &
            'subsequent starts collect forced-exit bootstrap files without active-pin deletion')
    end subroutine force_exit_retention

    function arguments_with(first, second, third, fourth, fifth) result(values)
        character(len=*), intent(in) :: first
        character(len=*), intent(in), optional :: second, third, fourth, fifth
        type(string_list_t) :: values
        call list_add(values, first)
        if (present(second)) call list_add(values, second)
        if (present(third)) call list_add(values, third)
        if (present(fourth)) call list_add(values, fourth)
        if (present(fifth)) call list_add(values, fifth)
    end function arguments_with

    subroutine native_image(path, tag)
        character(len=*), intent(in) :: path, tag
        character(:), allocatable :: bytes
        integer :: unit, count, pos, found
        type(string_list_t) :: signing
        inquire (file=driver, size=count)
        allocate(character(len=count) :: bytes)
        open(newunit=unit, file=driver, access='stream', form='unformatted', &
            status='old', action='read')
        read(unit) bytes
        close(unit)
        pos = 1
        found = 0
        do
            count = index(bytes(pos:), 'total_seconds')
            if (count == 0) exit
            count = pos + count - 1
            bytes(count:count + 12) = repeat(tag, 6)//'seconds'
            pos = count + 13
            found = found + 1
        end do
        call assert_true(found > 0, 'native image has distinguishable JSON output')
        ! write_text creates only text files; retain exact native bytes here.
        call write_text(path, '')
        open(newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='write')
        write(unit) bytes
        close(unit)
        call run_external('chmod', arguments_with('755', path), scratch, result)
        call assert_process_ok(result, 'make real native driver executable')
        if (platform == 'Darwin') then
            signing = arguments_with('--force', '--sign', '-', '--identifier', &
                'fo-driver-pin-native')
            call list_add(signing, path)
            call run_external('codesign', signing, scratch, result)
            call assert_process_ok(result, 'sign the native Mach-O fixture')
        end if
    end subroutine native_image

    function digest(path) result(value)
        character(len=*), intent(in) :: path
        character(:), allocatable :: value
        type(process_result_t) :: hash_result
        call run_external('shasum', arguments_with('-a', '256', path), scratch, hash_result)
        call assert_process_ok(hash_result, 'independent executable SHA256')
        value = ''
        if (len(hash_result%stdout) >= 64) value = hash_result%stdout(:64)
    end function digest

    function text(object, name) result(value)
        type(json_value_t), intent(in) :: object
        character(len=*), intent(in) :: name
        character(:), allocatable :: value
        type(json_value_t) :: member
        member = json_member(object, name)
        value = json_string_value(member)
    end function text

    function number(object, name) result(value)
        type(json_value_t), intent(in) :: object
        character(len=*), intent(in) :: name
        character(:), allocatable :: value
        type(json_value_t) :: member
        member = json_member(object, name)
        value = integer_text(int(json_number_value(member)))
    end function number

    function integer_text(value) result(output)
        integer, intent(in) :: value
        character(:), allocatable :: output
        character(len=32) :: buffer
        write(buffer, '(i0)') value
        output = trim(buffer)
    end function integer_text

    recursive subroutine command(lane, action, output, case_name)
        character(len=*), intent(in) :: lane, action
        character(len=*), intent(in), optional :: case_name
        type(json_value_t), intent(out) :: output
        type(string_list_t) :: args
        type(process_result_t) :: child
        type(json_value_t) :: snapshot, events, item
        integer :: i, rc_wait, retry
        args = arguments_with('gremlin', action, '--dir', project, '--lane')
        call list_add(args, lane)
        call list_add(args, '--json')
        if (len_trim(sessions(lane_index(lane))) > 0) then
            call list_add(args, '--session')
            call list_add(args, trim(sessions(lane_index(lane))))
        end if
        if (present(case_name)) then
            call list_add(args, '--case-id')
            call list_add(args, case_name)
        end if
        do retry = 1, 100
            call run_fo(public, args, project, cache, child, environment, timeout_ms=30000)
            if (action /= "status") exit
            if (child%exit_code /= 2) exit
            if (index(child%stdout, "Gremlin state provider error: 2") == 0) exit
            rc_wait = pause_us(50000_c_int)
        end do
        if (child%exit_code /= 0) write(error_unit, '(a)') &
            'public '//action//': '//child%stdout//child%stderr
        call assert_process_ok(child, 'public Gremlin '//action)
        call parse_json_report(child, output, 'public Gremlin '//action)
        if (len_trim(text(output, 'session_id')) > 0) &
            sessions(lane_index(lane)) = text(output, 'session_id')
        if (action == 'stop') then
            do i = 1, 300
                call command(lane, 'status', snapshot)
                if (text(snapshot, 'state') == 'stopped') return
                rc_wait = pause_us(100000_c_int)
            end do
            call assert_true(.false., 'owner stops before lane reuse')
        end if
        if (action /= 'reproduce') return
        call command(lane, 'status', snapshot)
        events = json_member(snapshot, 'events')
        do i = json_size(events), 1, -1
            item = json_element(events, i)
            if (text(item, 'case_id') /= 'test_probe') cycle
            output = item
            return
        end do
        call assert_true(.false., 'reproduction publishes an independent receipt')
    end subroutine command

    subroutine start_lane(lane)
        character(len=*), intent(in) :: lane
        type(process_result_t) :: child
        type(string_list_t) :: args
        type(json_value_t) :: started
        args = arguments_with('gremlin', 'start', '--dir', project, '--lane')
        call list_add(args, lane)
        call list_add(args, '--target')
        call list_add(args, 'test_probe')
        call run_fo(public, args, project, cache, child, environment, timeout_ms=30000)
        call assert_process_ok(child, 'start native driver generation')
        call parse_json_report(child, started, 'start generation')
        sessions(lane_index(lane)) = text(started, 'session_id')
    end subroutine start_lane

    subroutine wait_file(path)
        character(len=*), intent(in) :: path
        integer :: i, rc_pause
        do i = 1, 300
            if (file_exists(path)) return
            rc_pause = pause_us(100000_c_int)
        end do
        if (file_exists(scratch//'/debugger.log')) &
            write(error_unit, '(a)') read_text(scratch//'/debugger.log')
        call assert_true(.false., 'deterministic gate was reached: '//path)
        call finish_assertions()
    end subroutine wait_file

    subroutine await_pass(lane, expected_digest, tag, found)
        character(len=*), intent(in) :: lane, expected_digest, tag
        type(json_value_t), intent(out) :: found
        type(json_value_t) :: snapshot, events, item
        integer :: i, j, rc_pause
        do i = 1, 300
            call command(lane, 'status', snapshot)
            events = json_member(snapshot, 'events')
            do j = 1, json_size(events)
                item = json_element(events, j)
                if (text(item, 'case_id') /= 'test_probe') cycle
                if (text(item, 'status') /= 'PASS') cycle
                call assert_equal_string(text(item, 'driver_digest'), expected_digest, &
                    'receipt identifies executed native '//tag)
                call assert_contains(read_text(text(item, 'log_path')), repeat(tag, 6), &
                    'case was actually executed through native '//tag)
                found = item
                return
            end do
            rc_pause = pause_us(100000_c_int)
        end do
        write(error_unit, '(a)') 'last status: '//text(snapshot, 'state')// &
            ' outcome='//text(snapshot, 'last_outcome')
        write(error_unit, '(a)') 'fixture: '//scratch
        call assert_true(.false., 'native campaign produces a PASS receipt')
        call finish_assertions()
    end subroutine await_pass

    subroutine git(command_line)
        character(len=*), intent(in) :: command_line
        call run_external('/bin/sh', arguments_with('-c', 'git '//command_line), &
            project, result)
        call assert_process_ok(result, 'independent Git '//command_line)
    end subroutine git

    subroutine read_counter(value)
        integer(int64), intent(out) :: value
        type(json_value_t) :: parsed, member
        type(process_result_t) :: child
        child%stdout = read_text(scratch//'/capture.json')
        call parse_json_report(child, parsed, 'capture observer')
        member = json_member(parsed, 'count')
        value = int(json_number_value(member), int64)
    end subroutine read_counter

    subroutine await_capture(previous, expected, same)
        integer(int64), intent(in) :: previous
        character(len=*), intent(in) :: expected
        logical, intent(in) :: same
        integer(int64) :: current
        type(process_result_t) :: child
        type(json_value_t) :: parsed
        integer :: i, rc_pause
        do i = 1, 300
            call read_counter(current)
            if (current > previous) then
                child%stdout = read_text(scratch//'/capture.json')
                call parse_json_report(child, parsed, 'fresh capture observer')
                call assert_true((text(parsed, 'generation') == expected) .eqv. same, &
                    'metadata-only capture preserves execution identity')
                return
            end if
            rc_pause = pause_us(100000_c_int)
        end do
        call assert_true(.false., 'metadata event triggers a fresh capture')
    end subroutine await_capture

    subroutine await_new_pass(lane, old)
        character(len=*), intent(in) :: lane, old
        type(json_value_t) :: snapshot, events, item
        integer :: i, j, rc_pause
        do i = 1, 300
            call command(lane, 'status', snapshot)
            events = json_member(snapshot, 'events')
            do j = 1, json_size(events)
                item = json_element(events, j)
                if (text(item, 'case_id') /= 'test_probe') cycle
                if (text(item, 'status') /= 'PASS') cycle
                if (text(item, 'generation') == old) cycle
                call assert_equal_string(text(item, 'driver_digest'), digest_a, &
                    'relevant edit retains owner driver A')
                call assert_equal_string(text(item, 'base_commit'), new_head, &
                    'source generation records latest Git provenance')
                return
            end do
            rc_pause = pause_us(100000_c_int)
        end do
        call assert_true(.false., 'source edit creates a new tested generation')
    end subroutine await_new_pass
end program test_gremlin_driver_pin
