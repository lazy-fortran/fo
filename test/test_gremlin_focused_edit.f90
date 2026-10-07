program test_gremlin_focused_edit
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, assert_true
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_number_value
    use fo_test_json, only: json_boolean_value
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

        found = .false.
        generation = ''
        call arguments('status', status_args)
        do poll = 1, 1200
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
                    call assert_true(json_number_value(value) == 1, &
                        'target-only campaign runs only the requested case')
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

end program test_gremlin_focused_edit
