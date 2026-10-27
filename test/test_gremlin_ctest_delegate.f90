program test_gremlin_ctest_delegate
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, assert_true, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, &
        gremlin_run, gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_cli, only: parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, json_number_value
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, session
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args, env
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer :: attempt
    logical :: green

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(delegate LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'add_executable(probe probe.f90)'//nl// &
        'add_test(NAME probe COMMAND probe)'//nl)
    call write_text(project//'/probe.f90', &
        'program probe'//nl// &
        'character(8) :: value'//nl// &
        'call get_environment_variable("FO_CTEST_DELEGATE", value)'//nl// &
        'if(trim(value) /= "1") error stop "delegation environment lost"'//nl// &
        'end program probe'//nl)
    call check_mode('0', .false.)
    call check_mode('1', .true.)
    call finish_assertions(retain_failed_scratch=.true.)
contains
    subroutine check_mode(mode, expected_green)
        character(*), intent(in) :: mode
        logical, intent(in) :: expected_green

        args = string_list_t()
        env = string_list_t()
        call gremlin_start_args(args, project, 'delegate', 'probe')
        call list_add(args, '--random')
        call list_add(args, '0')
        call list_add(env, 'FO_CTEST_DELEGATE='//mode)
        call gremlin_run(driver, project, cache, state, args, process, &
            environment=env, timeout=20000)
        call parse_json_report(process, reply, 'delegation lane starts')
        session = gremlin_field(reply, 'session_id')
        call assert_true(len(session) > 0, 'delegation lane has an owner')
        if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, 'delegate')
        call list_add(args, '--session')
        call list_add(args, session)
        green = .false.
        do attempt = 1, 500
            call gremlin_json(driver, project, cache, state, args, reply, process, 10000)
            if (gremlin_field(reply, 'state') == 'error' .or. &
                gremlin_field(reply, 'state') == 'build_failed') exit
            green = json_boolean_value(json_member(reply, 'local_gate_green'))
            if (expected_green .and. green) exit
            if (.not. expected_green .and. &
                json_number_value(json_member(reply, 'completed')) > 0) exit
            call gremlin_wait_ms(50)
        end do
        if (expected_green) then
            call assert_true(green, 'changing CTest delegation reruns the frozen case successfully')
        else
            call assert_true(.not. green .and. &
                json_number_value(json_member(reply, 'completed')) > 0, &
                'native baseline executes and fails the environment-sensitive case')
        end if
        call gremlin_stop_lane(driver, project, cache, state, 'delegate', session, &
            allow_terminal_error=.true.)
    end subroutine check_mode
end program test_gremlin_ctest_delegate
