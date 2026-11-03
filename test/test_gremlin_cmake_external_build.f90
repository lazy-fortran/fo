program test_gremlin_cmake_external_build
    !! A resident generation captured from an absolute CMake build tree outside
    !! the project builds in its own view and leaves the caller's tree alone.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, read_text, assert_true, assert_contains, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, &
        gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, &
        json_size, json_element
    use fo_fs, only: fs_canonical_path
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: external
    character(*), parameter :: lane = 'cmake-external-build'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply, events, receipt
    logical :: verified, passed, inside_project
    integer :: attempt, i
    interface
        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
    end interface

    call gremlin_setup(driver, scratch, project, cache, state)
    external = scratch//'/external-build'
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(external_build LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'add_executable(probe probe.f90)'//nl// &
        'add_test(NAME external_case COMMAND probe)'//nl)
    call write_text(project//'/probe.f90', &
        'program probe'//nl//'implicit none'//nl// &
        'print *, "external build case passed"'//nl//'end program probe'//nl)
    call assert_true(c_setenv('FO_CMAKE_BUILD_DIR'//c_null_char, &
        external//c_null_char, 1_c_int) == 0, 'selects an absolute external build tree')

    call gremlin_start_args(args, project, lane, 'external_case')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts a lane on an absolute external build tree')
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
    call assert_true(verified, 'the generation of an external build tree is green: '// &
        gremlin_field(reply, 'diagnostic'))
    events = json_member(reply, 'events')
    passed = .false.
    do i = 1, json_size(events)
        receipt = json_element(events, i)
        if (gremlin_field(receipt, 'case_id') /= 'external_case') cycle
        passed = gremlin_field(receipt, 'status') == 'PASS' .and. &
            gremlin_field(receipt, 'generation') == &
            gremlin_field(reply, 'active_generation')
    end do
    call assert_true(passed, 'native CTest reports PASS for the active generation')
    ! A view configured into the caller's tree would rewrite its source home.
    call assert_contains(read_text(external//'/CMakeCache.txt'), &
        'CMAKE_HOME_DIRECTORY:INTERNAL='//fs_canonical_path(project)//nl, &
        'the external build tree still belongs to the authored project')
    inquire(file=project//'/build', exist=inside_project)
    call assert_true(.not. inside_project, 'the generation never builds in the project')
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)
end program test_gremlin_cmake_external_build
