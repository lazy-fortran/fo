program test_gremlin_bootstrap
    use, intrinsic :: iso_c_binding, only: c_int
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, write_text, read_text, process_alive
    use fo_test_harness, only: run_process
    use fo_test_harness, only: file_exists
    use fo_test_harness, only: assert_true, assert_equal_string, assert_equal_integer
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_wait_file, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_spawn, gremlin_wait_child
    use fo_test_gremlin_oracle, only: gremlin_fifo, gremlin_release_fifo
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_parse
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, lane
    character(:), allocatable :: marker, generation_done, obsolete_marker, obsolete_read
    character(:), allocatable :: old_gate, new_gate, lane_gate, old_pid_file
    character(:), allocatable :: session, lane_session, generation, new_generation
    character(:), allocatable :: out_one, err_one, out_two, err_two
    character(:), allocatable :: output_one, output_two, message
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: report, status, event, parsed_one, parsed_two
    integer :: start_one, start_two, exit_one, exit_two, old_pid, attempt
    logical :: found, valid
    integer(c_int) :: sleep_result

    call gremlin_setup(driver, scratch, project, cache, state)
    lane = 'bootstrap-replacement'
    marker = scratch//'/old.events'
    generation_done = scratch//'/generation.done'
    obsolete_marker = scratch//'/old.obsolete.events'
    obsolete_read = scratch//'/old.obsolete.version'
    old_pid_file = scratch//'/old.obsolete.pid'
    old_gate = scratch//'/old-generation.fifo'
    new_gate = scratch//'/new-generation.fifo'
    lane_gate = scratch//'/lane-b.fifo'
    call make_directory(project//'/test')
    call write_text(project//'/fpm.toml', &
        'name = "gremlin_bootstrap_probe"'//new_line('a'))
    call gremlin_fifo(old_gate)
    call gremlin_fifo(new_gate)
    call write_generation('old')
    out_one = scratch//'/start-one.out'
    err_one = scratch//'/start-one.err'
    out_two = scratch//'/start-two.out'
    err_two = scratch//'/start-two.err'
    call start_args(args)
    call gremlin_spawn(driver, project, args, out_one, err_one, start_one)
    call gremlin_spawn(driver, project, args, out_two, err_two, start_two)
    call gremlin_wait_child(start_one, exit_one)
    call gremlin_wait_child(start_two, exit_two)
    call assert_equal_integer(exit_one, 0, 'first concurrent start succeeds')
    call assert_equal_integer(exit_two, 0, 'second concurrent start attaches successfully')
    output_one = read_text(out_one)
    output_two = read_text(out_two)
    call json_parse(output_one, parsed_one, valid, message)
    call assert_true(valid, 'first concurrent start returns JSON: '//message)
    call json_parse(output_two, parsed_two, valid, message)
    call assert_true(valid, 'second concurrent start returns JSON: '//message)
    session = field(parsed_one, 'session_id')
    call assert_true(len(session) > 0, 'start returns its owner id before tests finish')
    call assert_equal_string(field(parsed_two, 'session_id'), session, &
        'same-place concurrent starts atomically attach to one owner')
    call assert_true(field(parsed_one, 'state') == 'running' .or. &
        field(parsed_one, 'state') == 'attached', 'first start reports its shared owner state')
    call assert_true(field(parsed_two, 'state') == 'running' .or. &
        field(parsed_two, 'state') == 'attached', 'second start reports its shared owner state')
    call gremlin_wait_file(marker, 30000, found)
    call assert_true(found, 'first generation test starts once')
    call status_now(lane, session, status)
    generation = field(status, 'active_generation')
    call assert_true(len(generation) == 64, 'status publishes an immutable generation')

    ! A failed candidate must not displace the exact running generation or child.
    call write_generation('invalid', .true.)
    call wait_build_failure(lane, session, generation, status)
    call assert_true(.not. marker_has(marker, 'done'), &
        'last-compilable test remains blocked when replacement build fails')
    call assert_equal_string(field(status, 'active_generation'), generation, &
        'failed build retains the last-compilable generation')
    call assert_equal_integer(marker_count(marker, 'started'), 1, &
        'concurrent start executes the first case exactly once')
    call gremlin_release_fifo(old_gate)
    call wait_marker(generation_done, 'done', 30000, found)
    call assert_true(found, 'last-compilable test completes after gate release')
    call wait_receipt(lane, session, 'test_generation', generation, 'PASS', event)
    call assert_equal_string(field(event, 'status'), 'PASS', 'completed old case has one receipt')
    call status_now(lane, session, status)
    call assert_equal_integer(receipt_count(status, 'test_generation', generation, 'PASS'), 1, &
        'concurrent start produces exactly one old-case completion receipt')

    ! Next case reads frozen runtime input, then is preempted only by a good build.
    call gremlin_wait_file(obsolete_read, 30000, found)
    call assert_true(found, 'second old-generation test reads its runtime snapshot')
    call assert_equal_string(read_text(obsolete_read), 'old'//new_line('a'), &
        'runtime read came from the captured old generation')
    call assert_equal_string(read_text(project//'/test/obsolete.version'), &
        'invalid'//new_line('a'), &
        'editable runtime input differs from the captured old generation')
    call gremlin_wait_file(old_pid_file, 5000, found)
    call assert_true(found, 'obsolete child publishes its PID after the runtime read')
    old_pid = read_integer(old_pid_file)
    call assert_true(old_pid > 0 .and. process_alive(old_pid), &
        'old-generation child is blocked in its own gate')
    call write_generation('new')
    call wait_generation_change(lane, session, generation, new_generation, status)
    call assert_true(new_generation /= generation, 'new successful build publishes a new identity')
    call wait_marker(marker, 'new-started', 30000, found)
    call assert_true(found, 'new generation starts its selected test')
    call assert_true(.not. process_alive(old_pid), 'successful replacement terminates obsolete child')
    call assert_true(.not. marker_has(obsolete_marker, 'done'), &
        'obsolete child receives no fabricated completion')
    call assert_true(.not. has_receipt(status, 'test_obsolete', generation, 'PASS'), &
        'preempted old case remains unclassified')
    call gremlin_release_fifo(new_gate)
    call wait_receipt(lane, session, 'test_generation', new_generation, 'PASS', event)
    call status_now(lane, session, status)
    call assert_true(has_receipt(status, 'test_generation', generation, 'PASS'), &
        'old generation PASS receipt survives later candidate failure and replacement')

    ! A second lane owns an independent child; stopping the first lane leaves it live.
    call make_directory(scratch//'/other/test')
    call write_text(scratch//'/other/fpm.toml', &
        'name = "gremlin_bootstrap_lane_b"'//new_line('a'))
    call gremlin_fifo(lane_gate)
    call write_gate_case(scratch//'/other', 'test_lane_b', lane_gate, &
        scratch//'/lane-b.started', scratch//'/lane-b.done')
    call start_other_lane(scratch//'/other', lane_session)
    call gremlin_wait_file(scratch//'/lane-b.started', 30000, found)
    call assert_true(found, 'independent lane B enters its blocking test')
    call status_now(lane, session, status)
    call stop_lane(lane, session)
    call assert_true(.not. marker_has(scratch//'/lane-b.done', 'done'), &
        'stopping lane A does not complete or kill lane B')
    call gremlin_release_fifo(lane_gate)
    call wait_marker(scratch//'/lane-b.done', 'done', 30000, found)
    call assert_true(found, 'lane B completes after its own gate is released')
    call wait_receipt_for_project(scratch//'/other', 'lane-b', lane_session, &
        'test_lane_b', 'PASS', event)
    call stop_other_lane(scratch//'/other', lane_session)
    call finish_assertions()

contains

    function field(document, key) result(value)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: key
        character(:), allocatable :: value
        type(json_value_t) :: item
        item = json_member(document, key)
        value = json_string_value(item)
    end function field

    subroutine start_args(values)
        type(string_list_t), intent(out) :: values
        call list_add(values, 'gremlin')
        call list_add(values, 'start')
        call list_add(values, '--dir')
        call list_add(values, project)
        call list_add(values, '--lane')
        call list_add(values, lane)
        call list_add(values, '--target')
        call list_add(values, 'test_generation')
        call list_add(values, '--target')
        call list_add(values, 'test_obsolete')
        call list_add(values, '--seed')
        call list_add(values, '1729')
        call list_add(values, '--campaign-seconds')
        call list_add(values, '60')
        call list_add(values, '--timeout-seconds')
        call list_add(values, '10')
        call list_add(values, '--json')
    end subroutine start_args

    subroutine write_generation(version, invalid)
        character(len=*), intent(in) :: version
        logical, optional, intent(in) :: invalid
        logical :: broken
        broken = .false.
        if (present(invalid)) broken = invalid
        call write_gate_case(project, 'test_generation', &
            merge(new_gate, old_gate, version == 'new'), marker, generation_done)
        call write_obsolete_case(version)
        call write_text(project//'/test/obsolete.version', version//new_line('a'))
        if (broken) then
            call make_directory(project//'/src')
            call write_text(project//'/src/invalid.f90', &
                'module invalid_candidate'//new_line('a')//'integer :: broken = "x"'//new_line('a')// &
                'end module invalid_candidate'//new_line('a'))
        else if (version /= 'invalid') then
            call remove_file(project//'/src/invalid.f90')
        end if
        if (version == 'new') then
            call write_text(project//'/test/test_generation.f90', &
                'program test_generation'//new_line('a')//'implicit none'//new_line('a')// &
                'integer :: unit, gate_unit'//new_line('a')//'character :: token'//new_line('a')// &
                "open(newunit=unit,file='"//marker//"',status='unknown',position='append')"//new_line('a')// &
                "write(unit,'(a)') 'new-started'"//new_line('a')//'close(unit)'//new_line('a')// &
                "open(newunit=gate_unit,file='"//new_gate//"',status='old',access='stream', &"//new_line('a')// &
                "    form='unformatted',action='read')"//new_line('a')// &
                'read(gate_unit) token'//new_line('a')//'close(gate_unit)'//new_line('a')// &
                "open(newunit=unit,file='"//scratch//"/generation.done',status='replace')"//new_line('a')// &
                "write(unit,'(a)') 'done'"//new_line('a')//'close(unit)'//new_line('a')// &
                'end program test_generation'//new_line('a'))
        end if
    end subroutine write_generation

    subroutine write_gate_case(root, name, gate_path, started_path, done_path)
        character(len=*), intent(in) :: root, name, gate_path, started_path, done_path
        character(:), allocatable :: source
        call make_directory(root//'/test')
        source = 'program '//name//new_line('a')//'implicit none'//new_line('a')// &
            'integer :: unit, gate_unit'//new_line('a')//'character :: token'//new_line('a')// &
            "open(newunit=unit,file='"//started_path// &
            "',status='unknown',position='append')"//new_line('a')// &
            "write(unit,'(a)') 'started'"//new_line('a')//'close(unit)'//new_line('a')// &
            "open(newunit=gate_unit,file='"//gate_path//"',status='old',access='stream', &"//new_line('a')// &
            "    form='unformatted',action='read')"//new_line('a')// &
            'read(gate_unit) token'//new_line('a')//'close(gate_unit)'//new_line('a')// &
            "open(newunit=unit,file='"//done_path//"',status='replace')"//new_line('a')// &
            "write(unit,'(a)') 'done'"//new_line('a')//'close(unit)'//new_line('a')// &
            'end program '//name//new_line('a')
        call write_text(root//'/test/'//name//'.f90', source)
    end subroutine write_gate_case

    subroutine write_obsolete_case(version)
        character(len=*), intent(in) :: version
        character(:), allocatable :: source
        source = 'program test_obsolete'//new_line('a')// &
            'use, intrinsic :: iso_c_binding, only: c_int'//new_line('a')//'implicit none'//new_line('a')// &
            'interface'//new_line('a')//'integer(c_int) function c_getpid() bind(C,name="getpid")'//new_line('a')// &
            'import :: c_int'//new_line('a')//'end function c_getpid'//new_line('a')//'end interface'//new_line('a')// &
            'integer :: unit, gate_unit'//new_line('a')//'character :: token'//new_line('a')// &
            'character(len=32) :: value'//new_line('a')// &
            "open(newunit=unit,file='test/obsolete.version',status='old')"//new_line('a')// &
            "read(unit,'(a)') value"//new_line('a')//'close(unit)'//new_line('a')// &
            "open(newunit=unit,file='"//scratch//"/old.obsolete.version',status='replace')"//new_line('a')// &
            "write(unit,'(a)') trim(value)"//new_line('a')//'close(unit)'//new_line('a')// &
            "open(newunit=unit,file='"//old_pid_file//"',status='replace')"//new_line('a')// &
            "write(unit,'(i0)') c_getpid()"//new_line('a')//'close(unit)'//new_line('a')// &
            "open(newunit=gate_unit,file='"//old_gate//"',status='old',access='stream', &"//new_line('a')// &
            "    form='unformatted',action='read')"//new_line('a')// &
            'read(gate_unit) token'//new_line('a')//'close(gate_unit)'//new_line('a')// &
            "open(newunit=unit,file='"//obsolete_marker//"',status='replace')"//new_line('a')// &
            "write(unit,'(a)') 'done'"//new_line('a')//'close(unit)'//new_line('a')// &
            'end program test_obsolete'//new_line('a')
        call write_text(project//'/test/test_obsolete.f90', source)
    end subroutine write_obsolete_case

    subroutine status_now(lane_id, owner, value)
        character(len=*), intent(in) :: lane_id, owner
        type(json_value_t), intent(out) :: value
        type(string_list_t) :: values
        type(process_result_t) :: result
        call list_add(values, 'gremlin')
        call list_add(values, 'status')
        call list_add(values, '--dir')
        call list_add(values, project)
        call list_add(values, '--lane')
        call list_add(values, lane_id)
        call list_add(values, '--session')
        call list_add(values, owner)
        call list_add(values, '--cursor')
        call list_add(values, '0')
        call list_add(values, '--json')
        call gremlin_json(driver, project, cache, state, values, value, result, 30000)
    end subroutine status_now

    subroutine wait_build_failure(lane_id, owner, expected_generation, value)
        character(len=*), intent(in) :: lane_id, owner, expected_generation
        type(json_value_t), intent(out) :: value
        type(json_value_t) :: events, item
        integer :: j, pass
        do pass = 1, 300
            call status_now(lane_id, owner, value)
            events = json_member(value, 'events')
            do j = 1, json_size(events)
                item = json_element(events, j)
                if (field(item, 'case_id') == '<build>' .and. &
                    field(item, 'status') == 'BUILD_FAIL') then
                    if (field(value, 'state') /= 'build_failed') cycle
                    call assert_equal_string(field(value, 'active_generation'), expected_generation, &
                        'failed candidate leaves last-compilable active generation')
                    return
                end if
            end do
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'failed replacement build produces a BUILD_FAIL event')
    end subroutine wait_build_failure

    subroutine wait_generation_change(lane_id, owner, old_generation, new_value, status_value)
        character(len=*), intent(in) :: lane_id, owner, old_generation
        character(:), allocatable, intent(out) :: new_value
        type(json_value_t), intent(out) :: status_value
        integer :: pass
        do pass = 1, 300
            call status_now(lane_id, owner, status_value)
            new_value = field(status_value, 'active_generation')
            if (len(new_value) == 64 .and. new_value /= old_generation) return
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'successful candidate publishes a new generation')
    end subroutine wait_generation_change

    subroutine wait_receipt(lane_id, owner, case_id, gen, verdict, found_event)
        character(len=*), intent(in) :: lane_id, owner, case_id, gen, verdict
        type(json_value_t), intent(out) :: found_event
        type(json_value_t) :: value, events, item
        integer :: pass, j
        do pass = 1, 300
            call status_now(lane_id, owner, value)
            events = json_member(value, 'events')
            do j = 1, json_size(events)
                item = json_element(events, j)
                if (field(item, 'case_id') == case_id .and. &
                    field(item, 'generation') == gen .and. field(item, 'status') == verdict) then
                    found_event = item
                    return
                end if
            end do
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'waits for '//case_id//' '//verdict//' receipt')
    end subroutine wait_receipt

    logical function has_receipt(value, case_id, gen, verdict)
        type(json_value_t), intent(in) :: value
        character(len=*), intent(in) :: case_id, gen, verdict
        type(json_value_t) :: events, item
        integer :: j
        has_receipt = .false.
        events = json_member(value, 'events')
        do j = 1, json_size(events)
            item = json_element(events, j)
            if (field(item, 'case_id') /= case_id) cycle
            if (field(item, 'generation') /= gen) cycle
            if (field(item, 'status') /= verdict) cycle
            has_receipt = .true.
            return
        end do
    end function has_receipt

    integer function receipt_count(value, case_id, gen, verdict)
        type(json_value_t), intent(in) :: value
        character(len=*), intent(in) :: case_id, gen, verdict
        type(json_value_t) :: events, item
        integer :: j
        receipt_count = 0
        events = json_member(value, 'events')
        do j = 1, json_size(events)
            item = json_element(events, j)
            if (field(item, 'case_id') /= case_id) cycle
            if (field(item, 'generation') /= gen) cycle
            if (field(item, 'status') /= verdict) cycle
            receipt_count = receipt_count + 1
        end do
    end function receipt_count

    subroutine wait_marker(path, expected, timeout, found_value)
        character(len=*), intent(in) :: path, expected
        integer, intent(in) :: timeout
        logical, intent(out) :: found_value
        integer :: pass, limit
        limit = timeout / 20
        found_value = .false.
        do pass = 1, limit
            if (marker_has(path, expected)) then
                found_value = .true.
                return
            end if
            call gremlin_wait_ms(20)
        end do
    end subroutine wait_marker

    logical function marker_has(path, marker_text)
        character(len=*), intent(in) :: path, marker_text
        marker_has = .false.
        if (.not. file_exists(path)) return
        marker_has = index(read_text(path), marker_text) > 0
    end function marker_has

    integer function marker_count(path, marker_text) result(count)
        character(len=*), intent(in) :: path, marker_text
        character(:), allocatable :: text
        integer :: first, next
        text = read_text(path)
        count = 0
        first = 1
        do while (first <= len(text))
            next = index(text(first:), marker_text//new_line('a'))
            if (next == 0) exit
            count = count + 1
            first = first + next + len(marker_text)
        end do
    end function marker_count

    integer function read_integer(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        read_integer = -1
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(unit, *, iostat=ios) read_integer
        close(unit)
        if (ios /= 0) read_integer = -1
    end function read_integer

    subroutine start_other_lane(root, owner)
        character(len=*), intent(in) :: root
        character(:), allocatable, intent(out) :: owner
        type(string_list_t) :: values
        type(process_result_t) :: result
        type(json_value_t) :: value
        logical :: ok
        character(:), allocatable :: error
        call list_add(values, 'gremlin')
        call list_add(values, 'start')
        call list_add(values, '--dir')
        call list_add(values, root)
        call list_add(values, '--lane')
        call list_add(values, 'lane-b')
        call list_add(values, '--target')
        call list_add(values, 'test_lane_b')
        call list_add(values, '--campaign-seconds')
        call list_add(values, '60')
        call list_add(values, '--json')
        call gremlin_run(driver, root, cache, state, values, result, timeout=30000)
        call json_parse(result%stdout, value, ok, error)
        call assert_true(ok, 'lane B start returns JSON: '//error)
        owner = field(value, 'session_id')
    end subroutine start_other_lane

    subroutine stop_other_lane(root, owner)
        character(len=*), intent(in) :: root, owner
        call gremlin_stop_lane(driver, root, cache, state, "lane-b", owner)
    end subroutine stop_other_lane

    subroutine wait_receipt_for_project(root, lane_id, owner, case_id, verdict, found_event)
        character(len=*), intent(in) :: root, lane_id, owner, case_id, verdict
        type(json_value_t), intent(out) :: found_event
        type(json_value_t) :: value, events, item
        type(string_list_t) :: values
        type(process_result_t) :: result
        integer :: pass, j
        do pass = 1, 300
            call list_add(values, 'gremlin')
            call list_add(values, 'status')
            call list_add(values, '--dir')
            call list_add(values, root)
            call list_add(values, '--lane')
            call list_add(values, lane_id)
            call list_add(values, '--session')
            call list_add(values, owner)
            call list_add(values, '--cursor')
            call list_add(values, '0')
            call list_add(values, '--json')
            call gremlin_json(driver, root, cache, state, values, value, result, 30000)
            events = json_member(value, 'events')
            do j = 1, json_size(events)
                item = json_element(events, j)
                if (field(item, 'case_id') == case_id .and. field(item, 'status') == verdict) then
                    found_event = item
                    return
                end if
            end do
            call gremlin_wait_ms(100)
            values = string_list_t()
        end do
        call assert_true(.false., 'other lane reaches its PASS receipt')
    end subroutine wait_receipt_for_project

    subroutine stop_lane(lane_id, owner)
        character(len=*), intent(in) :: lane_id, owner
        call gremlin_stop_lane(driver, project, cache, state, lane_id, owner)
    end subroutine stop_lane

    subroutine remove_file(path)
        character(len=*), intent(in) :: path
        type(string_list_t) :: values
        type(process_result_t) :: result
        call list_add(values, 'rm')
        call list_add(values, '-f')
        call list_add(values, path)
        call run_process(values, scratch, result, timeout_ms=10000)
    end subroutine remove_file

end program test_gremlin_bootstrap
