program test_continuous_preemption
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, write_text, read_text, file_exists
    use fo_test_harness, only: process_alive, assert_true, assert_equal_string
    use fo_test_harness, only: assert_equal_integer, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_wait_file, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_fifo, gremlin_release_fifo
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_number_value
    implicit none

    character(:), allocatable :: driver, scratch, project, other, cache, state
    character(:), allocatable :: gate_a, gate_b, other_gate, token_file, source_file
    character(:), allocatable :: start_a, pid_a, result_a, start_b, result_b
    character(:), allocatable :: other_start, other_pid, session_a, session_other
    character(:), allocatable :: generation_a, generation_b, lane_a, lane_other
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: report, status, old_failure, item, events
    integer :: pid_blocked, pid_other
    logical :: found

    call gremlin_setup(driver, scratch, project, cache, state)
    other = scratch//'/other'
    lane_a = 'preemption-a'
    lane_other = 'preemption-other'
    gate_a = scratch//'/a-blocked.fifo'
    gate_b = scratch//'/b-capture.fifo'
    other_gate = scratch//'/other.fifo'
    token_file = project//'/token.txt'
    source_file = project//'/src/probe.f90'
    start_a = scratch//'/a-started'
    pid_a = scratch//'/a.pid'
    result_a = scratch//'/a-result'
    start_b = scratch//'/b-started'
    result_b = scratch//'/b-result'
    other_start = scratch//'/other-started'
    other_pid = scratch//'/other.pid'
    call make_project(project, 'A')
    call make_project(other, 'O')
    call gremlin_fifo(gate_a)
    call gremlin_fifo(gate_b)
    call gremlin_fifo(other_gate)
    call write_text(project//'/test/test_fail.f90', &
        'program test_fail'//new_line('a')//'implicit none'//new_line('a')// &
        "print '(a)', 'CURRENT_GENERATION_FAILURE'"//new_line('a')//'error stop 7'//new_line('a')// &
        'end program test_fail'//new_line('a'))
    call write_blocked_test('test_blocked', gate_a, start_a, pid_a, token_file, result_a, 'A')
    call write_blocked_test('test_capture', gate_b, start_b, '', token_file, result_b, 'B')

    call start_main_lane(session_a)
    call wait_file(start_a, 30000, found)
    call assert_true(found, 'A test reaches its real FIFO and records process identity')
    pid_blocked = read_integer(pid_a)
    call assert_true(pid_blocked > 0, 'blocked process publishes its pid')
    call wait_failure_and_blocked(lane_a, session_a, status, old_failure)
    generation_a = field(status, 'active_generation')
    call assert_true(len(generation_a) == 64, 'A test runs on a frozen generation')

    ! A compiler error in the editable tree does not interrupt the compiled A child.
    call write_text(source_file, &
        'module probe'//new_line('a')//'this is not Fortran'//new_line('a')// &
        'end module probe'//new_line('a'))
    call write_text(token_file, 'BROKEN'//new_line('a'))
    call wait_build_failure(lane_a, session_a, status)
    call assert_equal_string(field(status, 'active_generation'), generation_a, &
        'failed candidate keeps exact last-compilable generation active')
    call assert_true(process_alive(pid_blocked), 'old test child remains alive during failed build')
    call gremlin_release_fifo(gate_a)
    call wait_file(result_a, 10000, found)
    call assert_true(found, 'old test completes from its frozen runtime view')
    call assert_equal_string(trim(read_text(result_a)), 'A:A', &
        'old test sees its compiled module and matching runtime input')

    call start_other_lane(session_other)
    call wait_file(other_start, 30000, found)
    call assert_true(found, 'unrelated lane owns a blocked process')
    pid_other = read_integer(other_pid)
    call assert_true(pid_other > 0 .and. process_alive(pid_other), &
        'unrelated process is alive before A replacement')

    call write_text(source_file, &
        'module probe'//new_line('a')// &
        'character(len=*), parameter :: probe_value = "B"'//new_line('a')// &
        'end module probe'//new_line('a'))
    call write_text(token_file, 'B'//new_line('a'))
    call wait_generation_b(lane_a, session_a, generation_a, generation_b, status)
    call wait_file(start_b, 30000, found)
    call assert_true(found, 'successful B generation starts its own capture child')
    call assert_true(generation_b /= generation_a, 'successful build switches immutable generation')
    call wait_dead(pid_blocked)
    call assert_true(.not. file_exists(result_b), 'B child stays behind its own gate')
    call assert_true(process_alive(pid_other), 'other lane child survives A preemption')
    call status_now(lane_a, session_a, status)
    call assert_true(has_same_receipt(status, old_failure), &
        'A generation failure receipt survives successful replacement byte-for-field')
    call assert_true(.not. has_receipt(status, 'test_capture', generation_b, 'PASS'), &
        'blocked B case has no premature receipt')
    call assert_true(.not. has_receipt(status, 'test_capture', generation_a, 'TIMEOUT'), &
        'preempted A case stays unclassified instead of timing out')

    call gremlin_release_fifo(gate_b)
    call wait_file(result_b, 10000, found)
    call assert_true(found, 'new B test completes after its gate release')
    call assert_equal_string(trim(read_text(result_b)), 'B:B', &
        'new test observes matching compiled and runtime B inputs')
    call wait_receipt(lane_a, session_a, 'test_capture', generation_b, 'PASS', item)
    call status_now(lane_a, session_a, status)
    call assert_true(has_same_receipt(status, old_failure), 'old receipt remains in current event history')

    ! Explicit stop is bounded and scoped to lane A. Lane Other stays in its own gate.
    call stop_lane(project, lane_a, session_a)
    call assert_true(process_alive(pid_other), 'stopping lane A leaves unrelated lane live')
    call gremlin_release_fifo(other_gate)
    call wait_file(scratch//'/other.done', 10000, found)
    call assert_true(found, 'unrelated lane finishes after its own FIFO release')
    call stop_lane(other, lane_other, session_other)
    call finish_assertions()

contains

    function field(value, name) result(text)
        type(json_value_t), intent(in) :: value
        character(len=*), intent(in) :: name
        character(:), allocatable :: text
        type(json_value_t) :: item
        item = json_member(value, name)
        text = json_string_value(item)
    end function field

    subroutine make_project(root, value)
        character(len=*), intent(in) :: root, value
        call make_directory(root//'/test')
        call make_directory(root//'/src')
        call write_text(root//'/fpm.toml', 'name = "preemption_probe"'//new_line('a'))
        call write_text(root//'/src/probe.f90', &
            'module probe'//new_line('a')// &
            'character(len=*), parameter :: probe_value = "'//value//'"'//new_line('a')// &
            'end module probe'//new_line('a'))
        call write_text(root//'/token.txt', value//new_line('a'))
    end subroutine make_project

    subroutine write_blocked_test(name, fifo, started_path, pid_path, token_path, result_path, tag)
        character(len=*), intent(in) :: name, fifo, started_path, pid_path, token_path, result_path, tag
        character(:), allocatable :: source, pid_declarations, pid_body, pid_use
        pid_declarations = ''
        pid_body = ''
        pid_use = ''
        if (len(pid_path) > 0) then
            pid_use = 'use, intrinsic :: iso_c_binding, only: c_int'//new_line('a')
            pid_declarations = 'interface'//new_line('a')// &
                'integer(c_int) function c_getpid() bind(C,name="getpid")'//new_line('a')// &
                'import :: c_int'//new_line('a')//'end function c_getpid'//new_line('a')//'end interface'//new_line('a')
            pid_body = "open(newunit=unit,file='"//pid_path//"',status='replace')"//new_line('a')// &
                "write(unit,'(i0)') c_getpid()"//new_line('a')//'close(unit)'//new_line('a')
        end if
        source = 'program '//name//new_line('a')//pid_use//'use probe, only: probe_value'//new_line('a')// &
            'implicit none'//new_line('a')//pid_declarations// &
            'integer :: unit, gate_unit'//new_line('a')//'character :: gate_token'//new_line('a')// &
            'character(len=32) :: runtime_token'//new_line('a')// &
            "open(newunit=unit,file='"//started_path//"',status='replace')"//new_line('a')// &
            "write(unit,'(a)') 'started'"//new_line('a')//'close(unit)'//new_line('a')//pid_body// &
            "open(newunit=gate_unit,file='"//fifo//"',status='old',access='stream', &"//new_line('a')// &
            "    form='unformatted',action='read')"//new_line('a')// &
            'read(gate_unit) gate_token'//new_line('a')//'close(gate_unit)'//new_line('a')// &
            "open(newunit=unit,file='"//token_path//"',status='old')"//new_line('a')// &
            "read(unit,'(a)') runtime_token"//new_line('a')//'close(unit)'//new_line('a')// &
            "open(newunit=unit,file='"//result_path//"',status='replace')"//new_line('a')// &
            "write(unit,'(a,a,a)') probe_value,':',trim(runtime_token)"//new_line('a')// &
            'close(unit)'//new_line('a')
        if (tag == 'B') source = source//'! replacement generation'//new_line('a')
        source = source//'end program '//name//new_line('a')
        call write_text(project//'/test/'//name//'.f90', source)
    end subroutine write_blocked_test

    subroutine start_main_lane(owner)
        character(:), allocatable, intent(out) :: owner
        type(string_list_t) :: values
        type(process_result_t) :: result
        type(json_value_t) :: response
        logical :: valid
        character(:), allocatable :: message
        call list_add(values, 'gremlin')
        call list_add(values, 'start')
        call list_add(values, '--dir')
        call list_add(values, project)
        call list_add(values, '--lane')
        call list_add(values, lane_a)
        call list_add(values, '--target')
        call list_add(values, 'test_fail')
        call list_add(values, '--target')
        call list_add(values, 'test_blocked')
        call list_add(values, '--target')
        call list_add(values, 'test_capture')
        call list_add(values, '--seed')
        call list_add(values, '141')
        call list_add(values, '--campaign-seconds')
        call list_add(values, '60')
        call list_add(values, '--timeout-seconds')
        call list_add(values, '30')
        call list_add(values, '--json')
        call gremlin_run(driver, project, cache, state, values, result, timeout=30000)
        call json_parse(result%stdout, response, valid, message)
        call assert_true(result%exit_code == 0 .and. valid, 'starts lane A: '//message)
        owner = field(response, 'session_id')
    end subroutine start_main_lane

    subroutine start_other_lane(owner)
        character(:), allocatable, intent(out) :: owner
        type(string_list_t) :: values
        type(process_result_t) :: result
        type(json_value_t) :: response
        logical :: valid
        character(:), allocatable :: message
        call write_blocked_other()
        call list_add(values, 'gremlin')
        call list_add(values, 'start')
        call list_add(values, '--dir')
        call list_add(values, other)
        call list_add(values, '--lane')
        call list_add(values, lane_other)
        call list_add(values, '--target')
        call list_add(values, 'test_other')
        call list_add(values, '--campaign-seconds')
        call list_add(values, '60')
        call list_add(values, '--timeout-seconds')
        call list_add(values, '30')
        call list_add(values, '--json')
        call gremlin_run(driver, other, cache, state, values, result, timeout=30000)
        call json_parse(result%stdout, response, valid, message)
        call assert_true(result%exit_code == 0 .and. valid, 'starts unrelated lane: '//message)
        owner = field(response, 'session_id')
    end subroutine start_other_lane

    subroutine write_blocked_other()
        character(:), allocatable :: source
        source = 'program test_other'//new_line('a')//'use, intrinsic :: iso_c_binding, only: c_int'// &
            new_line('a')//'implicit none'//new_line('a')// &
            'interface'//new_line('a')//' integer(c_int) function getpid() bind(C, name="getpid")'// &
            new_line('a')//'  import :: c_int'//new_line('a')//' end function getpid'//new_line('a')// &
            'end interface'//new_line('a')// &
            'integer :: unit, gate_unit'//new_line('a')//'character :: gate_token'//new_line('a')// &
            "open(newunit=unit,file='"//other_start//"',status='replace')"//new_line('a')// &
            "write(unit,'(a)') 'started'"//new_line('a')//'close(unit)'//new_line('a')// &
            "open(newunit=unit,file='"//other_pid//"',status='replace')"//new_line('a')// &
            "write(unit,'(i0)') getpid()"//new_line('a')//'close(unit)'//new_line('a')// &
            "open(newunit=gate_unit,file='"//other_gate//"',status='old',access='stream', &"//new_line('a')// &
            "    form='unformatted',action='read')"//new_line('a')// &
            'read(gate_unit) gate_token'//new_line('a')//'close(gate_unit)'//new_line('a')// &
            "open(newunit=unit,file='"//scratch//"/other.done',status='replace')"//new_line('a')// &
            "write(unit,'(a)') 'done'"//new_line('a')//'close(unit)'//new_line('a')// &
            'end program test_other'//new_line('a')
        call write_text(other//'/test/test_other.f90', source)
    end subroutine write_blocked_other

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

    subroutine wait_failure_and_blocked(lane_id, owner, value, failure)
        character(len=*), intent(in) :: lane_id, owner
        type(json_value_t), intent(out) :: value, failure
        type(json_value_t) :: events, item
        integer :: attempt, j
        do attempt = 1, 300
            call status_now(lane_id, owner, value)
            events = json_member(value, 'events')
            do j = 1, json_size(events)
                item = json_element(events, j)
                if (field(item, 'case_id') == 'test_fail' .and. field(item, 'status') == 'FAIL') then
                    failure = item
                    if (field(value, 'current_test') == 'test_blocked') return
                end if
            end do
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'A failure is receipted while the next case remains blocked')
    end subroutine wait_failure_and_blocked

    subroutine wait_build_failure(lane_id, owner, value)
        character(len=*), intent(in) :: lane_id, owner
        type(json_value_t), intent(out) :: value
        type(json_value_t) :: events, item
        integer :: attempt, j
        do attempt = 1, 300
            call status_now(lane_id, owner, value)
            events = json_member(value, 'events')
            do j = 1, json_size(events)
                item = json_element(events, j)
                if (field(item, 'case_id') == '<build>' .and. field(item, 'status') == 'BUILD_FAIL') return
            end do
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'invalid candidate produces a build failure event')
    end subroutine wait_build_failure

    subroutine wait_generation_b(lane_id, owner, old_gen, new_gen, value)
        character(len=*), intent(in) :: lane_id, owner, old_gen
        character(:), allocatable, intent(out) :: new_gen
        type(json_value_t), intent(out) :: value
        integer :: attempt
        do attempt = 1, 300
            call status_now(lane_id, owner, value)
            new_gen = field(value, 'active_generation')
            if (len(new_gen) == 64 .and. new_gen /= old_gen) return
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'valid B candidate becomes active')
    end subroutine wait_generation_b

    subroutine wait_receipt(lane_id, owner, case_id, gen, verdict, found_event)
        character(len=*), intent(in) :: lane_id, owner, case_id, gen, verdict
        type(json_value_t), intent(out) :: found_event
        type(json_value_t) :: value, events, item
        integer :: attempt, j
        do attempt = 1, 300
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
        call assert_true(.false., 'waits for current generation PASS receipt')
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

    logical function has_same_receipt(value, expected)
        type(json_value_t), intent(in) :: value, expected
        type(json_value_t) :: events, item
        integer :: j
        has_same_receipt = .false.
        events = json_member(value, 'events')
        do j = 1, json_size(events)
            item = json_element(events, j)
            if (field(item, 'case_id') /= field(expected, 'case_id')) cycle
            if (field(item, 'generation') /= field(expected, 'generation')) cycle
            if (field(item, 'completion_id') /= field(expected, 'completion_id')) cycle
            if (field(item, 'status') /= field(expected, 'status')) cycle
            has_same_receipt = .true.
            return
        end do
    end function has_same_receipt

    subroutine wait_dead(pid)
        integer, intent(in) :: pid
        integer :: attempt
        do attempt = 1, 100
            if (.not. process_alive(pid)) return
            call gremlin_wait_ms(50)
        end do
        call assert_true(.not. process_alive(pid), 'superseded test process exits in cancellation bound')
    end subroutine wait_dead

    subroutine wait_file(path, timeout_ms, found_value)
        character(len=*), intent(in) :: path
        integer, intent(in) :: timeout_ms
        logical, intent(out) :: found_value
        call gremlin_wait_file(path, timeout_ms, found_value)
    end subroutine wait_file

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

    subroutine stop_lane(root, lane_id, owner)
        character(len=*), intent(in) :: root, lane_id, owner
        type(string_list_t) :: values
        type(process_result_t) :: result
        call list_add(values, 'gremlin')
        call list_add(values, 'stop')
        call list_add(values, '--dir')
        call list_add(values, root)
        call list_add(values, '--lane')
        call list_add(values, lane_id)
        call list_add(values, '--session')
        call list_add(values, owner)
        call list_add(values, '--json')
        call gremlin_run(driver, root, cache, state, values, result, timeout=30000)
        call assert_equal_integer(result%exit_code, 0, 'stops requested lane within bound')
    end subroutine stop_lane

end program test_continuous_preemption
