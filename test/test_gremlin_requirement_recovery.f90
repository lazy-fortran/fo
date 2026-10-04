program test_gremlin_requirement_recovery
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: assert_true, assert_equal_integer, write_text
    use fo_test_harness, only: read_text, finish_assertions, make_directory
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args
    use fo_test_gremlin_oracle, only: gremlin_json, gremlin_field
    use fo_test_gremlin_oracle, only: gremlin_stop_lane, gremlin_wait_ms
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_boolean_value, json_number_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, owner
    character(:), allocatable :: generation, old_requirement, old_token, requirement
    character(:), allocatable :: counter_a, counter_b
    type(string_list_t) :: arguments
    type(process_result_t) :: process
    type(json_value_t) :: document, events, event
    integer :: i
    logical :: matching_pass
    character(len=*), parameter :: lane = 'requirement-recovery'

    call gremlin_setup(driver, scratch, project, cache, state)
    counter_a = scratch//'/case-a-runs'
    counter_b = scratch//'/case-b-runs'
    call make_directory(project//'/test')
    call write_text(project//'/fpm.toml', &
        'name = "requirement_recovery"'//new_line('a'))
    call write_counting_case('test_requirement_a', counter_a)
    call write_counting_case('test_requirement_b', counter_b)
    call start_owner('test_requirement_b')
    call wait_until('quiescent')
    call assert_true(boolean_field(document, 'fully_verified'), &
        'initial exact generation has full verification')
    generation = gremlin_field(document, 'active_generation')
    old_requirement = gremlin_field(document, 'requirement_digest')
    old_token = gremlin_field(document, 'gate_token')
    call assert_true(len(generation) == 64 .and. len(old_token) == 64, &
        'initial closure has an exact generation and green token')
    call assert_equal_integer(run_count(counter_a), 1, &
        'ordinary case executes once in the first coverage epoch')
    call assert_equal_integer(run_count(counter_b), 1, &
        'first required case executes once')
    call gremlin_stop_lane(driver, project, cache, state, lane, owner)

    ! The source bytes and driver are unchanged. The new owner's explicit
    ! requirement must be proved even though ordinary coverage is complete.
    call start_owner('test_requirement_a')
    call wait_until('fully-verified')
    call assert_true(boolean_field(document, 'local_gate_green'), &
        'restored coverage discharges the new explicit gate requirement')
    call assert_true(gremlin_field(document, 'active_generation') == generation, &
        'changed requirements reuse the exact unchanged source generation')
    requirement = gremlin_field(document, 'requirement_digest')
    call assert_true(requirement /= old_requirement .and. len(requirement) == 64, &
        'the new case establishes a distinct requirement identity')
    call assert_true(gremlin_field(document, 'gate_token') /= old_token .and. &
        len(gremlin_field(document, 'gate_token')) == 64, &
        'new requirement yields a fresh verified token')
    call assert_equal_integer(number_field(document, 'gate_required'), 1, &
        'only the new explicit case is required')
    call assert_equal_integer(number_field(document, 'gate_passed'), 1, &
        'the required case has actual passing evidence')
    call assert_equal_integer(run_count(counter_a), 2, &
        'previously ordinary case really runs for the new requirement')
    call assert_equal_integer(run_count(counter_b), 1, &
        'unrelated completed case is not rerun')

    events = json_member(document, 'events')
    matching_pass = .false.
    do i = 1, json_size(events)
        event = json_element(events, i)
        if (gremlin_field(event, 'case_id') /= 'test_requirement_a') cycle
        if (gremlin_field(event, 'generation') /= generation) cycle
        if (gremlin_field(event, 'requirement_digest') /= requirement) cycle
        if (.not. boolean_field(event, 'gate_required')) cycle
        if (gremlin_field(event, 'status') == 'PASS') matching_pass = .true.
    end do
    call assert_true(matching_pass, &
        'fresh token is backed by a real PASS receipt for the new requirement')
    call wait_until('quiescent')
    call gremlin_wait_ms(350)
    call assert_equal_integer(run_count(counter_a), 2, &
        'completed gate obligation does not repeat during quiescence')
    call assert_equal_integer(run_count(counter_b), 1, &
        'ordinary completed evidence remains reusable')
    call gremlin_stop_lane(driver, project, cache, state, lane, owner)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine write_counting_case(name, counter)
        character(len=*), intent(in) :: name, counter

        call write_text(project//'/test/'//name//'.f90', &
            'program '//name//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer :: unit'//new_line('a')// &
            "open(newunit=unit, file='"//counter// &
            "', status='unknown', position='append')"//new_line('a')// &
            "write(unit, '(a)') 'EXECUTED'"//new_line('a')// &
            'close(unit)'//new_line('a')// &
            'end program '//name//new_line('a'))
    end subroutine write_counting_case

    subroutine start_owner(target)
        character(len=*), intent(in) :: target

        call gremlin_start_args(arguments, project, lane, target)
        call list_add(arguments, '--random')
        call list_add(arguments, '1')
        call list_add(arguments, '--seed')
        call list_add(arguments, '17')
        call list_add(arguments, '--json')
        call gremlin_json(driver, project, cache, state, arguments, document, &
            process, 30000)
        owner = gremlin_field(document, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(owner) > 0, &
            'starts an owned exact-generation requirement fixture')
        if (len(owner) == 0) call finish_assertions(retain_failed_scratch=.true.)
    end subroutine start_owner

    subroutine wait_until(condition)
        character(len=*), intent(in) :: condition

        arguments = string_list_t()
        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'wait')
        call list_add(arguments, '--dir')
        call list_add(arguments, project)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--session')
        call list_add(arguments, owner)
        call list_add(arguments, '--until')
        call list_add(arguments, condition)
        call list_add(arguments, '--wait-ms')
        call list_add(arguments, '30000')
        call list_add(arguments, '--json')
        call gremlin_json(driver, project, cache, state, arguments, document, &
            process, 35000)
        call assert_true(process%exit_code == 0 .and. &
            boolean_field(document, 'wait_satisfied'), &
            'owned requirement fixture reaches '//condition)
    end subroutine wait_until

    logical function boolean_field(value, key)
        type(json_value_t), intent(in) :: value
        character(len=*), intent(in) :: key
        type(json_value_t) :: member

        member = json_member(value, key)
        boolean_field = json_boolean_value(member)
    end function boolean_field

    integer function number_field(value, key)
        type(json_value_t), intent(in) :: value
        character(len=*), intent(in) :: key
        type(json_value_t) :: member

        member = json_member(value, key)
        number_field = int(json_number_value(member))
    end function number_field

    integer function run_count(path)
        character(len=*), intent(in) :: path
        character(:), allocatable :: text
        integer :: at

        text = read_text(path)
        run_count = 0
        do at = 1, len(text)
            if (text(at:at) == new_line('a')) run_count = run_count + 1
        end do
    end function run_count

end program test_gremlin_requirement_recovery
