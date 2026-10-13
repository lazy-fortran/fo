program test_gremlin_cmake_impact
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, assert_true, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: first, second, restored
    character(*), parameter :: lane = 'cmake-impact-oracle'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply
    logical :: found

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/fpm.toml', 'name = "cmake_impact_probe"'//nl)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(impact_probe LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'add_executable(marker test/marker.f90)'//nl// &
        'add_test(NAME registered_marker COMMAND marker)'//nl)
    call write_marker(.false.)
    ! FPM sees these program names; CMake registers only registered_marker.
    call write_text(project//'/test/unregistered.f90', &
        'program unregistered'//nl//'implicit none'//nl// &
        'error stop "not a registered CTest case"'//nl// &
        'end program unregistered'//nl)
    call arguments('start', args)
    call gremlin_json(driver, project, cache, state, args, reply, process, 30000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts a mixed-manifest CMake campaign')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)

    call wait_result('', .false., first, found)
    call assert_true(found, 'initial CTest case passes')
    if (found) then
        call write_marker(.true.)
        call wait_result(first, .true., second, found)
        call assert_true(found, 'edited generation runs the registered CTest failure')
    end if
    if (found) then
        call write_marker(.false.)
        call wait_result(second, .false., restored, found)
        call assert_true(found, &
            'restored generation verifies the registered CTest case')
    end if
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)

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

    subroutine write_marker(fail)
        logical, intent(in) :: fail
        character(:), allocatable :: action

        action = 'print *, "registered marker passed"'
        if (fail) action = 'error stop "registered marker failed"'
        call write_text(project//'/test/marker.f90', &
            'program native_marker'//nl//'implicit none'//nl// &
            action//nl//'end program native_marker'//nl)
    end subroutine write_marker

    subroutine wait_result(previous, expect_failure, generation, found)
        character(len=*), intent(in) :: previous
        logical, intent(in) :: expect_failure
        character(:), allocatable, intent(out) :: generation
        logical, intent(out) :: found
        type(string_list_t) :: status_args
        type(process_result_t) :: status_process
        type(json_value_t) :: status, value, failure
        integer :: poll

        call arguments('status', status_args)
        call list_add(status_args, '--session')
        call list_add(status_args, session)
        found = .false.
        generation = ''
        do poll = 1, 1200
            call gremlin_json(driver, project, cache, state, status_args, &
                status, status_process, 10000)
            if (status_process%exit_code /= 0) return
            if (gremlin_field(status, 'state') == 'error') return
            generation = gremlin_field(status, 'active_generation')
            if (len(generation) == 64 .and. generation /= previous) then
                if (expect_failure) then
                    failure = json_member(status, 'latest_failure')
                    found = gremlin_field(failure, 'generation') == generation .and. &
                        gremlin_field(failure, 'status') == 'FAIL' .and. &
                        gremlin_field(failure, 'case_id') == 'registered_marker'
                else
                    value = json_member(status, 'fully_verified')
                    found = json_boolean_value(value)
                end if
                if (found) return
            end if
            call gremlin_wait_ms(50)
        end do
    end subroutine wait_result

end program test_gremlin_cmake_impact
