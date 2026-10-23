program test_gremlin_cmake_skip
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, assert_true, assert_contains, assert_process_ok, &
        assert_equal_integer, finish_assertions, run_process
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, &
        gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, &
        json_number_value, json_size, json_element
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: generation
    character(*), parameter :: lane = 'native-cmake-skip'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply, events, receipt
    integer :: rc, attempt, i, passed, skipped
    logical :: verified, failed

    interface
        integer(c_int) function set_env(name, value, overwrite) &
                bind(C, name='fo_test_setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function set_env
    end interface

    call gremlin_setup(driver, scratch, project, cache, state)
    rc = set_env('FO_CMAKE_DELEGATE_BUILD'//c_null_char, '1'//c_null_char, 1_c_int)
    call assert_equal_integer(rc, 0, 'selects the native CMake executor')
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(native_skip LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'add_executable(probe probe.f90)'//nl// &
        'add_executable(skipped skipped.f90)'//nl// &
        'add_test(NAME native_pass COMMAND probe)'//nl// &
        'add_test(NAME native_skip COMMAND skipped)'//nl// &
        'set_tests_properties(native_skip PROPERTIES SKIP_RETURN_CODE 77)'//nl)
    call write_text(project//'/probe.f90', 'program probe'//nl//'end program'//nl)
    call write_text(project//'/skipped.f90', &
        'program skipped'//nl//'stop 77'//nl//'end program'//nl)

    ! CMake/CTest independently defines this mixed result as successful.
    call list_add(args, 'cmake')
    call list_add(args, '-S')
    call list_add(args, project)
    call list_add(args, '-B')
    call list_add(args, scratch//'/reference-build')
    call run_process(args, scratch, process)
    call assert_process_ok(process, 'native reference configure')
    args = string_list_t()
    call list_add(args, 'cmake')
    call list_add(args, '--build')
    call list_add(args, scratch//'/reference-build')
    call run_process(args, scratch, process)
    call assert_process_ok(process, 'native reference build')
    args = string_list_t()
    call list_add(args, 'ctest')
    call list_add(args, '--test-dir')
    call list_add(args, scratch//'/reference-build')
    call list_add(args, '--output-on-failure')
    call run_process(args, scratch, process)
    call assert_process_ok(process, 'native PASS plus declared SKIP succeeds')
    call assert_contains(process%stdout, 'Skipped', 'CTest actually observed the skip')

    call gremlin_start_args(args, project, lane, 'native_pass')
    call list_add(args, '--target')
    call list_add(args, 'native_skip')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts the mixed native CTest campaign')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)
    verified = .false.
    do attempt = 1, 300
        call status_now()
        verified = json_boolean_value(json_member(reply, 'fully_verified'))
        if (verified) exit
        if (gremlin_field(reply, 'last_outcome') == 'INFRA_ERROR') exit
        call gremlin_wait_ms(100)
    end do
    call assert_true(verified, 'native PASS and SKIP reach full verification')
    call assert_equal_integer(int(json_number_value(json_member(reply, 'gate_passed'))), &
        1, 'only the successful test is counted as PASS')
    call assert_equal_integer(int(json_number_value(json_member(reply, 'gate_skipped'))), &
        1, 'the declared skip has its own count')
    generation = gremlin_field(reply, 'active_generation')
    events = json_member(reply, 'events')
    passed = 0
    skipped = 0
    do i = 1, json_size(events)
        receipt = json_element(events, i)
        if (gremlin_field(receipt, 'generation') /= generation) cycle
        if (gremlin_field(receipt, 'case_id') == 'native_pass' .and. &
            gremlin_field(receipt, 'status') == 'PASS') passed = passed + 1
        if (gremlin_field(receipt, 'case_id') == 'native_skip' .and. &
            gremlin_field(receipt, 'status') == 'SKIP' .and. &
            gremlin_field(receipt, 'outcome') == 'skip') skipped = skipped + 1
    end do
    call assert_equal_integer(passed, 1, 'one genuine PASS receipt')
    call assert_equal_integer(skipped, 1, 'one genuine SKIP receipt, never a PASS')
    if (verified) then
        call write_text(project//'/probe.f90', &
            'program probe'//nl//'stop 4'//nl//'end program'//nl)
        failed = .false.
        do attempt = 1, 300
            call status_now()
            events = json_member(reply, 'events')
            do i = 1, json_size(events)
                receipt = json_element(events, i)
                if (gremlin_field(receipt, 'generation') == generation) cycle
                if (gremlin_field(receipt, 'case_id') == 'native_pass' .and. &
                    gremlin_field(receipt, 'status') == 'FAIL') failed = .true.
            end do
            if (failed) exit
            call gremlin_wait_ms(100)
        end do
        call assert_true(failed, 'a changed native test still produces FAIL')
        call assert_true(.not. json_boolean_value(json_member(reply, 'local_gate_green')), &
            'a legitimate skip cannot hide the new failure')
    end if
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine status_now()
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
        call gremlin_json(driver, project, cache, state, args, reply, process, 10000)
    end subroutine status_now
end program test_gremlin_cmake_skip
