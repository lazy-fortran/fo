program test_gremlin_native_skip
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_directory, write_text, assert_true, assert_equal_integer, &
        assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, &
        gremlin_start_args, gremlin_wait_ms, gremlin_field, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_element, json_size, &
        json_number_value, json_boolean_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, owner
    character(:), allocatable :: first_generation
    character(len=*), parameter :: lane = 'native-skips'
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply, coverage, events, event
    integer :: rc, i, skip_receipts
    logical :: complete, skip_lifecycle

    interface
        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
    end interface

    call gremlin_setup(driver, scratch, project, cache, state)
    rc = c_setenv('FO_BACKEND'//c_null_char, 'cmake'//c_null_char, 1_c_int)
    call assert_true(rc == 0, 'selects the native CTest backend')
    call write_fixture(.false.)
    call start_owner(.false.)
    call await_complete(complete)
    call assert_true(complete, 'native pass and three authored skips complete coverage')
    if (complete) then
        call assert_counts()
        first_generation = gremlin_field(reply, 'active_generation')
        call arguments('events')
        call gremlin_json(driver, project, cache, state, args, reply, process)
        events = json_member(reply, 'events')
        skip_receipts = 0
        do i = 1, json_size(events)
            event = json_element(events, i)
            if (gremlin_field(event, 'status') /= 'SKIP') cycle
            skip_receipts = skip_receipts + 1
            call assert_equal_string(gremlin_field(event, 'outcome'), 'skip', &
                'public receipt preserves the native skip disposition')
            call assert_equal_integer(number(event, 'exitcode'), 0, &
                'native runner reports a successful skip interpretation')
        end do
        call assert_equal_integer(skip_receipts, 3, &
            'return code, skip regex and disabled verdicts remain distinct from PASS')
        events = json_member(reply, 'lifecycle_events')
        skip_lifecycle = .false.
        do i = 1, json_size(events)
            event = json_element(events, i)
            if (.not. json_boolean_value(json_member(event, 'fully_verified'))) cycle
            if (number(event, 'gate_skipped') /= 3) cycle
            skip_lifecycle = .true.
            call assert_equal_integer(number(event, 'gate_passed'), 1, &
                'durable lifecycle reports only the actual passing case')
        end do
        call assert_true(skip_lifecycle, 'durable lifecycle retains native skip counts')
    end if
    call gremlin_stop_lane(driver, project, cache, state, lane, owner)
    if (.not. complete) call finish_assertions(retain_failed_scratch=.true.)

    ! Reopening the same generation must restore completed skips as skips.
    call start_owner(.false.)
    call await_complete(complete)
    call assert_true(complete, 'restart restores complete native-parity readiness')
    if (complete) then
        call assert_counts()
        call assert_equal_string(gremlin_field(reply, 'active_generation'), &
            first_generation, 'restart reads the same immutable generation')
    end if
    call gremlin_stop_lane(driver, project, cache, state, lane, owner)

    ! A real failed case alongside the same skips remains a failed generation.
    project = scratch//'/failed-project'
    call make_directory(project)
    call write_fixture(.true.)
    call start_owner(.true.)
    call await_complete(complete)
    call assert_true(complete, 'a native failure is terminal coverage, not a skip')
    if (complete) then
        coverage = json_member(reply, 'coverage')
        call assert_equal_integer(number(coverage, 'pass'), 1, 'one actual pass')
        call assert_equal_integer(number(coverage, 'skip'), 3, 'three actual skips')
        call assert_equal_integer(number(coverage, 'fail'), 1, 'true failure survives')
        call assert_true(.not. json_boolean_value(json_member(reply, &
            'local_gate_green')), 'native skips cannot clear a failed gate')
        call assert_true(.not. json_boolean_value(json_member(reply, &
            'fully_verified')), 'native skips cannot verify a failing generation')
    end if
    call gremlin_stop_lane(driver, project, cache, state, lane, owner)
    call finish_assertions()

contains

    subroutine write_fixture(failing)
        logical, intent(in) :: failing
        character(:), allocatable :: source

        source = 'cmake_minimum_required(VERSION 3.22)'//nl// &
            'project(native_skips NONE)'//nl//'enable_testing()'//nl// &
            'add_test(NAME test_pass COMMAND sh -c "exit 0")'//nl// &
            'add_test(NAME test_skip_code COMMAND sh -c "exit 77")'//nl// &
            'set_tests_properties(test_skip_code PROPERTIES '// &
            'SKIP_RETURN_CODE 77)'//nl// &
            'add_test(NAME test_skip_regex COMMAND sh -c "echo unavailable")'//nl// &
            'set_tests_properties(test_skip_regex PROPERTIES '// &
            'SKIP_REGULAR_EXPRESSION "unavailable")'//nl// &
            'add_test(NAME test_disabled COMMAND sh -c "exit 19")'//nl// &
            'set_tests_properties(test_disabled PROPERTIES DISABLED TRUE)'//nl
        if (failing) source = source// &
            'add_test(NAME test_failure COMMAND sh -c "exit 9")'//nl
        call write_text(project//'/CMakeLists.txt', source)
    end subroutine write_fixture

    subroutine start_owner(failing)
        logical, intent(in) :: failing

        args = string_list_t()
        call gremlin_start_args(args, project, lane, 'test_pass')
        call target('test_skip_code')
        call target('test_skip_regex')
        call target('test_disabled')
        if (failing) call target('test_failure')
        call list_add(args, '--random')
        call list_add(args, '0')
        call gremlin_json(driver, project, cache, state, args, reply, process, 30000)
        owner = gremlin_field(reply, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(owner) > 0, &
            'starts an isolated native CTest resident owner')
        if (len(owner) == 0) call finish_assertions(retain_failed_scratch=.true.)
    end subroutine start_owner

    subroutine target(name)
        character(len=*), intent(in) :: name

        call list_add(args, '--target')
        call list_add(args, name)
    end subroutine target

    subroutine arguments(action)
        character(len=*), intent(in) :: action

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, action)
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
    end subroutine arguments

    subroutine await_complete(found)
        logical, intent(out) :: found
        integer :: poll

        found = .false.
        ! The default status document is a summary; coverage needs full detail.
        call arguments('status')
        call list_add(args, '--detail')
        call list_add(args, 'full')
        do poll = 1, 1200
            call gremlin_json(driver, project, cache, state, args, reply, process)
            if (process%exit_code /= 0) return
            coverage = json_member(reply, 'coverage')
            if (json_boolean_value(json_member(coverage, 'full_coverage'))) then
                if (gremlin_field(reply, 'phase') == 'quiescent' .and. &
                    .not. json_boolean_value(json_member(reply, 'dirty'))) then
                    found = .true.
                    return
                end if
            end if
            call gremlin_wait_ms(50)
        end do
    end subroutine await_complete

    subroutine assert_counts()
        type(json_value_t) :: ordinary, full

        coverage = json_member(reply, 'coverage')
        ordinary = json_member(coverage, 'ordinary')
        full = json_member(coverage, 'full')
        call assert_equal_integer(number(coverage, 'eligible'), 4, 'four native cases')
        call assert_equal_integer(number(coverage, 'completed'), 4, 'four completions')
        call assert_equal_integer(number(coverage, 'pass'), 1, 'only one case passed')
        call assert_equal_integer(number(coverage, 'skip'), 3, 'three cases skipped')
        call assert_equal_integer(number(coverage, 'remaining'), 0, &
            'no obligations left')
        call assert_equal_integer(number(ordinary, 'skip'), 3, 'ordinary skips exposed')
        call assert_equal_integer(number(full, 'skip'), 3, 'full skips exposed')
        call assert_equal_integer(number(reply, 'gate_passed'), 1, 'one gate pass')
        call assert_equal_integer(number(reply, 'gate_skipped'), 3, 'three gate skips')
        call assert_equal_integer(number(reply, 'full_passed'), 1, 'one full pass')
        call assert_equal_integer(number(reply, 'full_skipped'), 3, 'three full skips')
        call assert_true(json_boolean_value(json_member(reply, 'fully_verified')), &
            'native-parity completion is fresh, complete and failure-free')
    end subroutine assert_counts

    integer function number(document, key)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: key

        number = nint(json_number_value(json_member(document, key)))
    end function number

end program test_gremlin_native_skip
