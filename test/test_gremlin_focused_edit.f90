program test_gremlin_focused_edit
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, make_directory, assert_true
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_gremlin_oracle, only: gremlin_gate_create, gremlin_gate_release
    use fo_test_gremlin_oracle, only: gremlin_gate_read_source, gremlin_wait_file
    use fo_test_json, only: json_value_t, json_member, json_number_value
    use fo_test_json, only: json_boolean_value, json_element, json_size
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: initial_generation, current_generation
    character(*), parameter :: lane = 'focused-edit-oracle'
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply
    logical :: ready

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/fpm.toml', 'name = "focused_edit_fixture"'// &
        new_line('a'))
    call write_provider(1)
    call write_case('test_target')
    call write_case('test_other')

    call arguments('start', args)
    call list_add(args, '--target')
    call list_add(args, 'test_target')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_json(driver, project, cache, state, args, reply, process, 30000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts a target-only public Gremlin campaign')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)

    call wait_for_gate('', initial_generation, ready)
    call assert_true(ready, 'first generation verifies the requested target')
    call write_provider(2)
    call wait_for_gate(initial_generation, current_generation, ready)
    call assert_true(ready, 'edited shared source reaches a new green generation')

    call gremlin_stop_lane(driver, project, cache, state, lane, session)
    if (.not. ready) call finish_assertions(retain_failed_scratch=.true.)
    call affected_slow_and_reproducer()
    call finish_assertions()
    print '(a)', 'Gremlin focused edit oracle PASS'

contains

    subroutine arguments(action, values)
        character(len=*), intent(in) :: action
        type(string_list_t), intent(out) :: values

        call list_add(values, 'gremlin')
        call list_add(values, action)
        call list_add(values, '--dir')
        call list_add(values, project)
        call list_add(values, '--lane')
        call list_add(values, lane)
    end subroutine arguments

    subroutine wait_for_gate(previous, generation, found)
        character(len=*), intent(in) :: previous
        character(:), allocatable, intent(out) :: generation
        logical, intent(out) :: found
        type(string_list_t) :: status_args
        type(json_value_t) :: status_reply, value
        type(process_result_t) :: status_process
        integer :: poll
        character(len=32) :: observed

        found = .false.
        generation = ''
        call arguments('status', status_args)
        do poll = 1, 4800
            call gremlin_json(driver, project, cache, state, status_args, &
                status_reply, status_process, 10000)
            if (status_process%exit_code /= 0) exit
            generation = gremlin_field(status_reply, 'active_generation')
            if (len(generation) == 64 .and. generation /= previous) then
                value = json_member(status_reply, 'local_gate_green')
                if (json_boolean_value(value)) then
                    value = json_member(status_reply, 'gate_required')
                    call assert_true(json_number_value(value) == 1, &
                        'edited generation keeps exactly one required target')
                    value = json_member(status_reply, 'ordinary_passed')
                    write (observed, '(f0.0)') json_number_value(value)
                    call assert_true(json_number_value(value) == 1, &
                        'target-only campaign runs only the requested case; '// &
                        'ordinary_passed='//trim(observed))
                    found = .true.
                    return
                end if
            end if
            call gremlin_wait_ms(50)
        end do
    end subroutine wait_for_gate

    subroutine write_provider(version)
        integer, intent(in) :: version
        character(len=16) :: number

        write (number, '(i0)') version
        call write_text(project//'/src/shared.f90', 'module shared'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, parameter :: value = '//trim(number)//new_line('a')// &
            'end module shared'//new_line('a'))
    end subroutine write_provider

    subroutine write_case(name)
        character(len=*), intent(in) :: name

        call write_text(project//'/test/'//name//'.f90', &
            'program '//name//new_line('a')// &
            'use shared, only: value'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'if (value < 1) error stop 1'//new_line('a')// &
            'end program '//name//new_line('a'))
    end subroutine write_case

    subroutine affected_slow_and_reproducer()
        character(:), allocatable :: marker, armed, entered, gate, old_generation
        character(:), allocatable :: source, observed
        type(json_value_t) :: coverage, events, event
        integer :: i
        logical :: found, required_pass
        character(len=1), parameter :: nl = new_line('a')

        project = scratch//'/affected-project'
        marker = scratch//'/affected-executions'
        armed = scratch//'/affected-armed'
        entered = scratch//'/affected-entered'
        gate = scratch//'/affected.fifo'
        call make_directory(project//'/src')
        call make_directory(project//'/test')
        call gremlin_gate_create(gate)
        call write_text(project//'/fpm.toml', 'name = "affected_slow_fixture"'//nl)
        call write_affected_provider(1)
        source = 'program test_a_affected_slow'//nl// &
            'use affected_provider, only: provider_value'//nl// &
            'implicit none'//nl//'integer :: unit'//nl// &
            'if (provider_value() == 2) then'//nl// &
            'open(newunit=unit,file="'//marker// &
            '",status="unknown",position="append")'//nl// &
            'write(unit,"(a)") "slow"'//nl//'close(unit)'//nl// &
            'open(newunit=unit,file="'//entered//'",status="replace")'//nl// &
            'write(unit,"(a)") "entered"'//nl//'close(unit)'//nl// &
            gremlin_gate_read_source(gate)//nl//'end if'//nl// &
            'if (provider_value() < 1) error stop 1'//nl//'end program'//nl
        call write_text(project//'/test/test_a_affected_slow.f90', source)
        source = 'program test_z_reproducer'//nl//'implicit none'//nl// &
            'integer :: unit'//nl//'logical :: armed'//nl// &
            'inquire(file="'//armed//'",exist=armed)'//nl//'if (armed) then'//nl// &
            'open(newunit=unit,file="'//marker// &
            '",status="unknown",position="append")'//nl// &
            'write(unit,"(a)") "reproducer"'//nl//'close(unit)'//nl// &
            'end if'//nl//'error stop 7'//nl//'end program'//nl
        call write_text(project//'/test/test_z_reproducer.f90', source)
        args = string_list_t()
        call arguments('start', args)
        call list_add(args, '--random')
        call list_add(args, '1')
        call list_add(args, '--seed')
        call list_add(args, '1729')
        call list_add(args, '--timeout-seconds')
        call list_add(args, '30')
        call gremlin_json(driver, project, cache, state, args, reply, process, 30000)
        session = gremlin_field(reply, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(session) > 0, &
            'starts the two-family affected-test owner')
        if (len(session) == 0) return
        call wait_for_complete_coverage('', found)
        call assert_true(found, 'initial finite coverage observes both real cases')
        if (.not. found) then
            call gremlin_stop_lane(driver, project, cache, state, lane, session)
            return
        end if
        old_generation = gremlin_field(reply, 'active_generation')
        coverage = json_member(reply, 'coverage')
        call assert_equal_integer(number_field(coverage, 'fail'), 1, &
            'independent reproducer fails before the source edit')
        call write_text(armed, 'armed'//nl)
        call write_affected_provider(2)
        call gremlin_wait_file(entered, 60000, found)
        call assert_true(found, &
            'affected slow execution reaches its completion barrier')
        if (found) then
            call query_impact_owner()
            current_generation = gremlin_field(reply, 'active_generation')
            call assert_true(current_generation /= old_generation, &
                'the changed implementation establishes a fresh execution generation')
            call assert_equal_integer(number_field(reply, 'gate_required'), 2, &
                'both the current reproducer and affected slow case remain required')
            observed = read_text(marker)
            i = index(observed, nl)
            if (i > 0) observed = observed(:i - 1)
            call assert_equal_string(observed, 'reproducer', &
                'real reproducer execution precedes the affected slow case')
            call gremlin_gate_release(gate)
            call wait_for_complete_coverage(old_generation, found)
            call assert_true(found, &
                'released affected slow case completes finite coverage')
            required_pass = .false.
            events = json_member(reply, 'events')
            do i = 1, json_size(events)
                event = json_element(events, i)
                if (gremlin_field(event, 'generation') /= current_generation) cycle
                if (gremlin_field(event, 'case_id') /= 'test_a_affected_slow') cycle
                if (gremlin_field(event, 'status') /= 'PASS') cycle
                required_pass = json_boolean_value(json_member(event, 'gate_required'))
            end do
            call assert_true(required_pass, &
                'actual slow PASS receipt belongs to the current required gate')
            call assert_equal_integer(number_field(reply, 'gate_passed'), 1, &
                'only the actual passing slow requirement receives gate credit')
            call assert_true(.not. json_boolean_value(json_member(reply, &
                'local_gate_green')), &
                'current reproducer failure prevents a green gate')
        end if
        call gremlin_stop_lane(driver, project, cache, state, lane, session)
    end subroutine affected_slow_and_reproducer

    subroutine write_affected_provider(version)
        integer, intent(in) :: version
        character :: number
        character(len=1), parameter :: nl = new_line('a')

        write(number, '(i1)') version
        call write_text(project//'/src/affected_provider.f90', &
            'module affected_provider'//nl//'contains'//nl// &
            'integer function provider_value()'//nl//'provider_value = '//number//nl// &
            'end function'//nl//'end module'//nl)
    end subroutine write_affected_provider

    subroutine query_impact_owner()
        args = string_list_t()
        call arguments('status', args)
        call list_add(args, '--session')
        call list_add(args, session)
        call list_add(args, '--detail')
        call list_add(args, 'full')
        call gremlin_json(driver, project, cache, state, args, reply, process, 10000)
    end subroutine query_impact_owner

    subroutine wait_for_complete_coverage(previous, found)
        character(len=*), intent(in) :: previous
        logical, intent(out) :: found
        type(json_value_t) :: coverage
        integer :: poll

        found = .false.
        do poll = 1, 600
            call query_impact_owner()
            if (process%exit_code /= 0) return
            if (gremlin_field(reply, 'active_generation') /= previous) then
                coverage = json_member(reply, 'coverage')
                if (json_boolean_value(json_member(coverage, 'full_coverage')) .and. &
                    gremlin_field(reply, 'current_test') == '') then
                    found = .true.
                    return
                end if
            end if
            call gremlin_wait_ms(100)
        end do
    end subroutine wait_for_complete_coverage

    integer function number_field(value, name)
        type(json_value_t), intent(in) :: value
        character(len=*), intent(in) :: name

        number_field = int(json_number_value(json_member(value, name)))
    end function number_field

end program test_gremlin_focused_edit
