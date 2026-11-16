program test_gremlin_cmake_external_build
    !! A resident generation captured from an absolute CMake build tree outside
    !! the project builds in its own view and leaves the caller's tree alone.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, read_text, assert_true, assert_contains, finish_assertions, assert_equal_string
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, &
        gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, &
        json_size, json_element
    use fo_test_cli, only: run_external
    use fo_fs, only: fs_canonical_path
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: external, before_cache, before_registration
    character(:), allocatable :: active_lane, original_generation, absent, before_count
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
    active_lane = lane
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(external_build LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'include("${CACHED_INPUT}")'//nl// &
        'if(NOT INPUT_MARKER EQUAL 17)'//nl// &
        '  message(FATAL_ERROR "cached dependency input was lost")'//nl// &
        'endif()'//nl// &
        'option(SEED_OPTION "Retained caller option" OFF)'//nl// &
        'option(OVERRIDE_OPTION "Private override" OFF)'//nl// &
        'file(APPEND "${CMAKE_BINARY_DIR}/configure-count" "configured\n")'//nl// &
        'if(NOT SEED_OPTION)'//nl// &
        '  message(FATAL_ERROR "caller cache option was lost")'//nl// &
        'endif()'//nl// &
        'add_executable(probe probe.f90)'//nl// &
        'if(OVERRIDE_OPTION)'//nl// &
        '  add_test(NAME external_case COMMAND probe)'//nl// &
        'endif()'//nl)
    call write_text(project//'/probe.f90', &
        'program probe'//nl//'implicit none'//nl// &
        'print *, "external build case passed"'//nl//'end program probe'//nl)
    call write_text(external//'/_deps/fixture-src/settings.cmake', &
        'set(INPUT_MARKER 17)'//nl)
    args = string_list_t()
    call list_add(args, '-S')
    call list_add(args, project)
    call list_add(args, '-B')
    call list_add(args, external)
    call list_add(args, '-DFIXTURE_SOURCE_DIR:PATH='//external//'/_deps/fixture-src')
    call list_add(args, '-DCACHED_INPUT:FILEPATH='//external//'/_deps/fixture-src/settings.cmake')
    call list_add(args, '-DSEED_OPTION=ON')
    call list_add(args, '-DOVERRIDE_OPTION=OFF')
    call run_external('cmake', args, project, process, timeout_ms=30000)
    call assert_true(process%exit_code == 0, 'configures caller options independently')
    before_cache = read_text(external//'/CMakeCache.txt')
    before_registration = read_text(external//'/CTestTestfile.cmake')
    call assert_true(c_setenv('FO_CMAKE_ARGS'//c_null_char, &
        '-DOVERRIDE_OPTION=ON'//c_null_char, 1_c_int) == 0, &
        'requests a conflicting option only in the private configure')
    call assert_true(c_setenv('FO_CMAKE_BUILD_DIR'//c_null_char, &
        external//c_null_char, 1_c_int) == 0, 'selects an absolute external build tree')

    call gremlin_start_args(args, project, active_lane, 'external_case')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts a lane on an absolute external build tree')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)

    call wait_green('')
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
    call assert_equal_string(read_text(external//'/CMakeCache.txt'), before_cache, &
        'capture preserves every byte of the caller cache')
    call assert_equal_string(read_text(external//'/CTestTestfile.cmake'), &
        before_registration, 'capture preserves the caller test registration')
    call assert_equal_string(read_text(external//'/configure-count'), &
        'configured'//nl, 'capture never configures the caller tree')
    inquire(file=project//'/build', exist=inside_project)
    call assert_true(.not. inside_project, 'the generation never builds in the project')
    original_generation = gremlin_field(reply, 'active_generation')
    args = string_list_t()
    call list_add(args, '-S')
    call list_add(args, project)
    call list_add(args, '-B')
    call list_add(args, external)
    call list_add(args, '-DSEED_REVISION=second')
    call run_external('cmake', args, project, process, timeout_ms=30000)
    call assert_true(process%exit_code == 0, 'updates the native caller seed independently')
    before_cache = read_text(external//'/CMakeCache.txt')
    before_count = read_text(external//'/configure-count')
    call write_text(project//'/probe.f90', &
        'program probe'//nl//'implicit none'//nl// &
        'print *, "updated external build case passed"'//nl//'end program probe'//nl)
    call wait_green(original_generation)
    call assert_contains(read_text(gremlin_field(reply, 'active_project')// &
        '/.fo-cmake/cache.cmake'), 'set(SEED_REVISION "second" CACHE STRING', &
        'a changed seed invalidates the warm private configure')
    call assert_equal_string(read_text(external//'/CMakeCache.txt'), before_cache, &
        'recapture preserves the updated caller cache')
    call assert_equal_string(read_text(external//'/configure-count'), before_count, &
        'source edits never reconfigure the caller tree')
    call gremlin_stop_lane(driver, project, cache, state, active_lane, session, &
        allow_terminal_error=.true.)

    absent = scratch//'/absent-build'
    active_lane = 'cmake-absent-build'
    call assert_true(c_setenv('FO_CMAKE_BUILD_DIR'//c_null_char, &
        absent//c_null_char, 1_c_int) == 0, 'selects an absent caller tree')
    call assert_true(c_setenv('FO_CMAKE_ARGS'//c_null_char, &
        ('-DSEED_OPTION=ON -DOVERRIDE_OPTION=ON -DFIXTURE_SOURCE_DIR:PATH='// &
        external//'/_deps/fixture-src -DCACHED_INPUT:FILEPATH='// &
        external//'/_deps/fixture-src/settings.cmake')//c_null_char, 1_c_int) == 0, &
        'supplies the native inputs of an unconfigured tree')
    call gremlin_start_args(args, project, active_lane, 'external_case')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts with an absent explicit caller tree')
    if (len(session) > 0) then
        call wait_green('')
        inquire(file=absent, exist=inside_project)
        call assert_true(.not. inside_project, 'capture never creates the absent caller tree')
        call gremlin_stop_lane(driver, project, cache, state, active_lane, session, &
            allow_terminal_error=.true.)
    end if
    call finish_assertions(retain_failed_scratch=.true.)
contains
    subroutine wait_green(previous)
        character(len=*), intent(in) :: previous
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, active_lane)
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
            verified = json_boolean_value(json_member(reply, 'local_gate_green')) .and. &
                gremlin_field(reply, 'active_generation') /= previous
            if (verified) exit
            call gremlin_wait_ms(50)
        end do
        call assert_true(verified, 'the generation of an external build tree is green: '// &
            gremlin_field(reply, 'diagnostic'))
    end subroutine wait_green
end program test_gremlin_cmake_external_build
