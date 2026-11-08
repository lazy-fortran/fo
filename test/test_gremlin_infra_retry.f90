program test_gremlin_infra_retry
    !! A case whose environment was missing ends as INFRA_ERROR. Once the
    !! environment is back and the lane restarts on the unchanged generation,
    !! the case runs again and passes instead of keeping the lane red (#352).
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, assert_true, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, &
        gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, &
        json_size, json_element
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: marker, first_generation, outcome
    character(*), parameter :: lane = 'infra-retry'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply
    logical :: green

    call gremlin_setup(driver, scratch, project, cache, state)
    marker = scratch//'/environment-ready'
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(infra_retry LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'add_executable(probe probe.f90)'//nl// &
        'add_test(NAME environment_case COMMAND probe)'//nl// &
        'set_tests_properties(environment_case PROPERTIES REQUIRED_FILES "'// &
        marker//'")'//nl)
    call write_text(project//'/probe.f90', &
        'program probe'//nl//'implicit none'//nl//'end program probe'//nl)

    call start()
    call wait_outcome(outcome, green)
    call assert_true(outcome == 'INFRA_ERROR', &
        'a missing environment file is an infrastructure error: '//outcome)
    first_generation = gremlin_field(reply, 'active_generation')
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)

    call write_text(marker, 'ready'//nl)
    call start()
    call wait_outcome(outcome, green, 'PASS')
    call assert_true(gremlin_field(reply, 'active_generation') == first_generation, &
        'the restarted lane keeps the unchanged generation')
    call assert_true(outcome == 'PASS' .and. green, &
        'the restarted lane retries the infrastructure error and turns green: '// &
        outcome//' state='//gremlin_field(reply, 'state')//' health='// &
        gremlin_field(reply, 'health'))
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine start()
        call gremlin_start_args(args, project, lane, 'environment_case')
        call list_add(args, '--random')
        call list_add(args, '0')
        call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
        session = gremlin_field(reply, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(session) > 0, &
            'starts the lane')
        if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)
    end subroutine start

    subroutine wait_outcome(last, gate_green, wanted)
        !! The newest receipt of environment_case for the active generation once
        !! the lane rests (or wanted appears), with the lane's gate verdict.
        character(:), allocatable, intent(out) :: last
        logical, intent(out) :: gate_green
        character(len=*), intent(in), optional :: wanted
        type(string_list_t) :: status_args
        type(json_value_t) :: events, receipt
        integer :: attempt, i

        call list_add(status_args, 'gremlin')
        call list_add(status_args, 'status')
        call list_add(status_args, '--dir')
        call list_add(status_args, project)
        call list_add(status_args, '--lane')
        call list_add(status_args, lane)
        call list_add(status_args, '--session')
        call list_add(status_args, session)
        call list_add(status_args, '--detail')
        call list_add(status_args, 'full')
        last = ''
        gate_green = .false.
        do attempt = 1, 1200
            call gremlin_json(driver, project, cache, state, status_args, reply, &
                process, 10000)
            if (process%exit_code /= 0) exit
            if (gremlin_field(reply, 'state') == 'build_failed') exit
            gate_green = json_boolean_value(json_member(reply, 'local_gate_green'))
            events = json_member(reply, 'events')
            last = ''
            do i = 1, json_size(events)
                receipt = json_element(events, i)
                if (gremlin_field(receipt, 'case_id') /= 'environment_case') cycle
                if (gremlin_field(receipt, 'generation') /= &
                    gremlin_field(reply, 'active_generation')) cycle
                if (len(gremlin_field(receipt, 'status')) == 0) cycle
                last = gremlin_field(receipt, 'status')
            end do
            if (present(wanted)) then
                if (last == wanted .and. gate_green) exit
            end if
            if (len(last) > 0 .and. gremlin_field(reply, 'state') == 'quiescent') exit
            call gremlin_wait_ms(50)
        end do
    end subroutine wait_outcome

end program test_gremlin_infra_retry
