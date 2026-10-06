program test_gremlin_reproduce_concurrent_logs
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, write_text, read_text, file_exists
    use fo_test_harness, only: assert_true, assert_equal_string
    use fo_test_harness, only: assert_equal_integer, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_wait_ms, gremlin_wait_file
    use fo_test_gremlin_oracle, only: gremlin_spawn, gremlin_wait_child, gremlin_poll_child
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_boolean_value, json_boolean
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, lane
    character(:), allocatable :: session, generation, gate_one, gate_two
    character(:), allocatable :: entered_one, entered_two, out_one, err_one
    character(:), allocatable :: out_two, err_two, log_one, log_two
    character(:), allocatable :: first_text, second_text, first_cwd, second_cwd
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: report, response
    integer :: child_one, child_two, exit_one, exit_two
    logical :: found, valid

    call gremlin_setup(driver, scratch, project, cache, state)
    lane = 'reproduce-concurrent-log-oracle'
    call write_text(project//'/fpm.toml', &
        'name = "gremlin_reproduce_concurrent_probe"'//new_line('a'))
    call write_case('test_reproduce_anchor', '', 'FO_REPRODUCE_ANCHOR_OUTPUT')
    call setup_gate('test_reproduce_gate_first', 'FO_REPRODUCE_GATE_FIRST_48ef31', &
        gate_one, entered_one)
    call setup_gate('test_reproduce_gate_second', 'FO_REPRODUCE_GATE_SECOND_0ba742', &
        gate_two, entered_two)
    call gremlin_start_args(args, project, lane, 'test_reproduce_anchor')
    call list_add(args, '--seed')
    call list_add(args, '1729')
    call list_add(args, '--json')
    call gremlin_json(driver, project, cache, state, args, report, process, 120000)
    session = field(report, 'session_id')
    call assert_true(len(session) > 0, 'start returns an owner session')
    call wait_for_anchor(session, generation)

    out_one = scratch//'/first.stdout'
    err_one = scratch//'/first.stderr'
    out_two = scratch//'/second.stdout'
    err_two = scratch//'/second.stderr'
    call reproduction_args('test_reproduce_gate_first', args)
    call gremlin_spawn(driver, project, args, out_one, err_one, child_one)
    call gremlin_wait_file(entered_one, 30000, found)
    call assert_true(found, 'first reproduction blocks at its real FIFO')
    call reproduction_args('test_reproduce_gate_second', args)
    call gremlin_spawn(driver, project, args, out_two, err_two, child_two)

    call write_text(gate_one, 'release'//new_line('a'))
    call gremlin_wait_child(child_one, exit_one)
    call assert_equal_integer(exit_one, 1, 'first reproduction retains its failing status')
    call gremlin_wait_file(entered_two, 15000, found)
    if (.not. found) then
        call gremlin_poll_child(child_two, valid, exit_two)
        call assert_true(valid, 'a rejected overlapping request is explicitly completed')
        call reproduction_args('test_reproduce_gate_second', args)
        call gremlin_spawn(driver, project, args, out_two, err_two, child_two)
        call gremlin_wait_file(entered_two, 30000, found)
    end if
    call assert_true(found, 'second overlapping or retried request reaches its own barrier')
    call write_text(gate_two, 'release'//new_line('a'))
    call gremlin_wait_child(child_two, exit_two)
    call assert_equal_integer(exit_two, 1, 'second reproduction retains its failing status')

    call wait_for_logs(session, generation, log_one, log_two)
    call assert_true(log_one /= log_two, 'concurrent receipts use unique log paths')
    first_text = read_text(log_one)
    second_text = read_text(log_two)
    call assert_true(index(first_text, 'FO_REPRODUCE_GATE_FIRST_48ef31') > 0, &
        'first receipt log retains the first process output')
    call assert_true(index(first_text, 'FO_REPRODUCE_GATE_SECOND_0ba742') == 0, &
        'first receipt log excludes the second process output')
    call assert_true(index(second_text, 'FO_REPRODUCE_GATE_SECOND_0ba742') > 0, &
        'second receipt log retains the second process output')
    call assert_true(index(second_text, 'FO_REPRODUCE_GATE_FIRST_48ef31') == 0, &
        'second receipt log excludes the first process output')
    first_cwd = execution_cwd(first_text)
    second_cwd = execution_cwd(second_text)
    call assert_true(len(first_cwd) > 0 .and. len(second_cwd) > 0, &
        'both reproductions report their private execution directories')
    call assert_true(first_cwd /= second_cwd, &
        'overlapping reproductions own distinct execution directories')
    if (len(first_cwd) > 0) then
        call assert_true(file_exists(first_cwd//'/private-output.txt'), &
            'failed first reproduction retains its relative output')
        if (file_exists(first_cwd//'/private-output.txt')) &
            call assert_equal_string(read_text(first_cwd//'/private-output.txt'), &
                'FO_REPRODUCE_GATE_FIRST_48ef31'//new_line('a'), &
                'first retained output belongs to the first reproduction')
    end if
    if (len(second_cwd) > 0) then
        call assert_true(file_exists(second_cwd//'/private-output.txt'), &
            'failed second reproduction retains its relative output')
        if (file_exists(second_cwd//'/private-output.txt')) &
            call assert_equal_string(read_text(second_cwd//'/private-output.txt'), &
                'FO_REPRODUCE_GATE_SECOND_0ba742'//new_line('a'), &
                'second retained output belongs to the second reproduction')
    end if
    call assert_true(.not. file_exists(project//'/private-output.txt'), &
        'relative reproduction output never enters the editable project')
    call status_args(session, args)
    call gremlin_json(driver, project, cache, state, args, report, process, 30000)
    call assert_equal_integer(process%exit_code, 0, &
        'status reads source state after private reproduction writes')
    call assert_equal_string(field(report, 'active_generation'), generation, &
        'private reproduction writes leave the active generation unchanged')
    response = json_member(report, 'dirty')
    call assert_true(response%kind == json_boolean, 'status reports source dirtiness')
    call assert_true(.not. json_boolean_value(response), &
        'private reproduction writes do not dirty the source generation')
    response = json_member(report, 'input_changed')
    call assert_true(response%kind == json_boolean, &
        'status reports source input events')
    call assert_true(.not. json_boolean_value(response), &
        'private reproduction writes do not create source input events')
    call assert_true(.not. file_exists(field(report, 'active_project')// &
        '/private-output.txt'), &
        'relative reproduction output never enters the captured project')
    call stop_lane(session)
    call finish_assertions()

contains

    function execution_cwd(text) result(cwd)
        character(len=*), intent(in) :: text
        character(:), allocatable :: cwd, tail
        integer :: start, finish

        cwd = ''
        start = index(text, 'fo: execution cwd: ')
        if (start == 0) return
        tail = text(start + len('fo: execution cwd: '):)
        finish = index(tail, new_line('a'))
        if (finish == 0) return
        cwd = trim(tail(:finish - 1))
    end function execution_cwd

    function field(document, key) result(value)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: key
        character(:), allocatable :: value
        type(json_value_t) :: item
        item = json_member(document, key)
        value = json_string_value(item)
    end function field

    subroutine write_case(name, gate, token)
        character(len=*), intent(in) :: name, gate, token
        character(:), allocatable :: source
        call make_directory(project//'/test')
        source = 'program '//name//new_line('a')//'implicit none'//new_line('a')// &
            "print '(a)', '"//token//"'"//new_line('a')//'end program '//name//new_line('a')
        call write_text(project//'/test/'//name//'.f90', source)
    end subroutine write_case

    subroutine setup_gate(name, token, fifo, entered)
        character(len=*), intent(in) :: name, token
        character(:), allocatable, intent(out) :: fifo, entered
        character(:), allocatable :: source
        fifo = scratch//'/'//name//'.fifo'
        entered = scratch//'/'//name//'.entered'
        ! A durable release wakes campaign and reproduction readers independently.
        source = 'program '//name//new_line('a')// &
            'use, intrinsic :: iso_c_binding, only: c_int'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'interface'//new_line('a')// &
            'integer(c_int) function c_usleep(us) bind(C,name="usleep")'//new_line('a')// &
            'import :: c_int'//new_line('a')//'integer(c_int), value :: us'//new_line('a')// &
            'end function c_usleep'//new_line('a')//'end interface'//new_line('a')// &
            'integer :: unit, rc'//new_line('a')//'logical :: released'//new_line('a')// &
            "open(newunit=unit,file='"//entered//"',status='replace')"//new_line('a')// &
            "write(unit,'(a)') 'entered'"//new_line('a')//'close(unit)'//new_line('a')// &
            'do'//new_line('a')// &
            "inquire(file='"//fifo//"',exist=released)"//new_line('a')// &
            'if (released) exit'//new_line('a')// &
            'rc = c_usleep(20000_c_int)'//new_line('a')//'end do'//new_line('a')// &
            "open(newunit=unit,file='private-output.txt',"// &
            "status='replace')"//new_line('a')// &
            "write(unit,'(a)') '"//token//"'"//new_line('a')// &
            'close(unit)'//new_line('a')// &
            "print '(a)', '"//token//"'"//new_line('a')//'error stop 7'//new_line('a')// &
            'end program '//name//new_line('a')
        call write_text(project//'/test/'//name//'.f90', source)
    end subroutine setup_gate

    subroutine reproduction_args(case_id, values)
        character(len=*), intent(in) :: case_id
        type(string_list_t), intent(out) :: values
        call list_add(values, 'gremlin')
        call list_add(values, 'reproduce')
        call list_add(values, case_id)
        call list_add(values, '--dir')
        call list_add(values, project)
        call list_add(values, '--lane')
        call list_add(values, lane)
        call list_add(values, '--session')
        call list_add(values, session)
        call list_add(values, '--generation')
        call list_add(values, generation)
        call list_add(values, '--json')
    end subroutine reproduction_args

    subroutine wait_for_anchor(owner, active_generation)
        character(len=*), intent(in) :: owner
        character(:), allocatable, intent(out) :: active_generation
        type(json_value_t) :: status, events, event
        type(string_list_t) :: values
        type(process_result_t) :: result
        integer :: attempt, j
        do attempt = 1, 1200
            call status_args(owner, values)
            call gremlin_json(driver, project, cache, state, values, status, result, 30000)
            active_generation = field(status, 'active_generation')
            events = json_member(status, 'events')
            do j = 1, json_size(events)
                event = json_element(events, j)
                if (field(event, 'case_id') == 'test_reproduce_anchor' .and. &
                    field(event, 'status') == 'PASS' .and. &
                    field(event, 'generation') == active_generation) return
            end do
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'anchor receipt proves the generation is usable')
    end subroutine wait_for_anchor

    subroutine status_args(owner, values)
        character(len=*), intent(in) :: owner
        type(string_list_t), intent(out) :: values
        call list_add(values, 'gremlin')
        call list_add(values, 'status')
        call list_add(values, '--detail')
        call list_add(values, 'full')
        call list_add(values, '--dir')
        call list_add(values, project)
        call list_add(values, '--lane')
        call list_add(values, lane)
        call list_add(values, '--session')
        call list_add(values, owner)
        call list_add(values, '--cursor')
        call list_add(values, '0')
        call list_add(values, '--json')
    end subroutine status_args

    subroutine wait_for_logs(owner, active_generation, first, second)
        character(len=*), intent(in) :: owner, active_generation
        character(:), allocatable, intent(out) :: first, second
        type(json_value_t) :: status, events, event
        type(string_list_t) :: values
        type(process_result_t) :: result
        integer :: attempt, j
        first = ''
        second = ''
        do attempt = 1, 300
            call status_args(owner, values)
            call gremlin_json(driver, project, cache, state, values, status, result, 30000)
            events = json_member(status, 'events')
            do j = 1, json_size(events)
                event = json_element(events, j)
                if (field(event, 'generation') /= active_generation .or. &
                    field(event, 'status') /= 'FAIL') cycle
                if (index(field(event, 'log_path'), '-reproduce-') == 0) cycle
                if (field(event, 'case_id') == 'test_reproduce_gate_first') &
                    first = field(event, 'log_path')
                if (field(event, 'case_id') == 'test_reproduce_gate_second') &
                    second = field(event, 'log_path')
            end do
            if (len(first) > 0 .and. len(second) > 0) return
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'durable events retain both concurrent log paths')
    end subroutine wait_for_logs

    subroutine stop_lane(owner)
        character(len=*), intent(in) :: owner
        call gremlin_stop_lane(driver, project, cache, state, lane, owner)
    end subroutine stop_lane

end program test_gremlin_reproduce_concurrent_logs
