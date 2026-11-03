program test_gremlin_cmake_label_scope
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, assert_true, assert_equal_integer, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_field, &
        gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, &
        json_size, json_element
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(*), parameter :: lane = 'cmake-label-scope'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply, events, receipt
    logical :: verified, first_passed, second_passed
    integer :: attempt, i, receipts

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(label_scope LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'add_executable(probe probe.f90)'//nl// &
        'add_test(NAME first_case COMMAND probe)'//nl// &
        'add_test(NAME second_case COMMAND probe)'//nl// &
        'set_tests_properties(first_case second_case PROPERTIES LABELS "rm2")'//nl// &
        'add_test(NAME other_case COMMAND probe)'//nl// &
        'set_tests_properties(other_case PROPERTIES LABELS "other")'//nl)
    call write_text(project//'/probe.f90', &
        'program probe'//nl//'implicit none'//nl// &
        'print *, "probe passed"'//nl//'end program probe'//nl)

    ! `--label rm2 --random 0` without --target runs every rm2 case, as
    ! `fo test -L rm2` does, instead of a lane that selects nothing.
    call list_add(args, 'gremlin')
    call list_add(args, 'start')
    call list_add(args, '--dir')
    call list_add(args, project)
    call list_add(args, '--lane')
    call list_add(args, lane)
    call list_add(args, '--label')
    call list_add(args, 'rm2')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts the label-scoped lane without random cases')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)

    args = string_list_t()
    call list_add(args, 'gremlin')
    call list_add(args, 'status')
    call list_add(args, '--dir')
    call list_add(args, project)
    call list_add(args, '--lane')
    call list_add(args, lane)
    call list_add(args, '--session')
    call list_add(args, session)
    call list_add(args, '--detail')
    call list_add(args, 'full')
    verified = .false.
    do attempt = 1, 1200
        call gremlin_json(driver, project, cache, state, args, reply, process, 10000)
        if (process%exit_code /= 0) exit
        if (gremlin_field(reply, 'state') == 'error' .or. &
            gremlin_field(reply, 'state') == 'build_failed' .or. &
            gremlin_field(reply, 'state') == 'quiescent') exit
        verified = json_boolean_value(json_member(reply, 'local_gate_green'))
        if (verified) exit
        call gremlin_wait_ms(50)
    end do
    verified = json_boolean_value(json_member(reply, 'local_gate_green'))
    call assert_true(verified, 'the label scope is the verified gate')
    events = json_member(reply, 'events')
    first_passed = .false.
    second_passed = .false.
    receipts = 0
    do i = 1, json_size(events)
        receipt = json_element(events, i)
        if (gremlin_field(receipt, 'case_id') == '<build>') cycle
        receipts = receipts + 1
        if (gremlin_field(receipt, 'status') /= 'PASS') cycle
        if (gremlin_field(receipt, 'case_id') == 'first_case') first_passed = .true.
        if (gremlin_field(receipt, 'case_id') == 'second_case') second_passed = .true.
    end do
    call assert_true(first_passed .and. second_passed, 'every rm2 case passes')
    call assert_equal_integer(receipts, 2, 'no case outside the label runs')
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)
end program test_gremlin_cmake_label_scope
