program test_gremlin_cmake_explicit_selection
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, assert_true, assert_equal_integer, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, &
        gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, &
        json_size, json_element
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(*), parameter :: lane = 'cmake-explicit-slow'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply, events, receipt
    logical :: verified, passed
    integer :: attempt, i, receipts

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(explicit_slow LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'add_executable(probe EXCLUDE_FROM_ALL probe.f90)'//nl// &
        'add_test(NAME selected_case COMMAND probe)'//nl// &
        'set_tests_properties(selected_case PROPERTIES LABELS "probe;slow")'//nl// &
        'add_executable(unselected EXCLUDE_FROM_ALL broken.f90)'//nl// &
        'add_test(NAME unselected_case COMMAND unselected)'//nl// &
        'set_tests_properties(unselected_case PROPERTIES LABELS "probe;slow")'//nl)
    call write_text(project//'/probe.f90', &
        'program probe'//nl//'implicit none'//nl// &
        'print *, "explicit slow case passed"'//nl//'end program probe'//nl)
    call write_text(project//'/broken.f90', 'unselected target is invalid Fortran')

    ! A label, rather than a slow name suffix, excludes this CTest case by default.
    ! The other slow case must remain unbuilt even when the explicit one is lifted.
    call gremlin_start_args(args, project, lane, 'selected_case')
    call list_add(args, '--random')
    call list_add(args, '0')
    call list_add(args, '-L')
    call list_add(args, 'probe')
    call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts the explicitly selected slow CTest campaign')
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
            gremlin_field(reply, 'state') == 'build_failed') exit
        verified = json_boolean_value(json_member(reply, 'local_gate_green'))
        if (verified) exit
        call gremlin_wait_ms(50)
    end do
    call assert_true(verified, 'the explicitly selected slow CTest case is verified')
    events = json_member(reply, 'events')
    passed = .false.
    receipts = 0
    do i = 1, json_size(events)
        receipt = json_element(events, i)
        if (gremlin_field(receipt, 'case_id') == '<build>') cycle
        receipts = receipts + 1
        if (gremlin_field(receipt, 'case_id') /= 'selected_case') cycle
        passed = gremlin_field(receipt, 'status') == 'PASS' .and. &
            gremlin_field(receipt, 'generation') == &
            gremlin_field(reply, 'active_generation')
    end do
    call assert_true(passed, 'native CTest reports PASS for the active generation')
    call assert_equal_integer(receipts, 1, 'no unselected slow case runs')
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)
end program test_gremlin_cmake_explicit_selection
