program test_gremlin_cmake_configure_events
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, read_text, assert_true, assert_equal_integer, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, &
        gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, &
        json_parse, json_number_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, counter
    character(:), allocatable :: session, generation, cmake_source, configure_log
    character(*), parameter :: lane = 'cmake-configure-events'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer :: rc, before, configurations, attempt
    logical :: recaptured, rejected

    interface
        integer(c_int) function set_env(name, value, overwrite) &
                bind(C, name='fo_test_setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function set_env
        integer(c_int) function touch_directory(path) &
                bind(C, name='fo_test_touch_directory')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function touch_directory
    end interface

    call gremlin_setup(driver, scratch, project, cache, state, counter)
    rc = set_env('FO_CMAKE_DELEGATE_BUILD'//c_null_char, '1'//c_null_char, 1_c_int)
    call assert_equal_integer(rc, 0, 'selects the native CMake executor')
    configure_log = scratch//'/configurations.log'
    cmake_source = 'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(configure_events LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'file(APPEND "'//configure_log//'" "configured\n")'//nl// &
        'file(CREATE_LINK "${CMAKE_CURRENT_SOURCE_DIR}/payload" '// &
        '"${CMAKE_CURRENT_SOURCE_DIR}/generated-link" SYMBOLIC)'//nl// &
        'add_executable(probe probe.f90)'//nl// &
        'add_test(NAME run_probe COMMAND probe)'//nl
    call write_text(project//'/CMakeLists.txt', cmake_source)
    call write_text(project//'/payload', 'synthetic payload'//nl)
    call write_text(project//'/probe.f90', &
        'program probe'//nl//'implicit none'//nl//'print *, "passed"'//nl// &
        'end program probe'//nl)
    call gremlin_start_args(args, project, lane, 'run_probe')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts a project whose configure recreates a source symlink')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)
    call wait_green()
    generation = gremlin_field(reply, 'active_generation')
    configurations = line_count(read_text(configure_log))
    before = capture_count()
    rc = touch_directory(project//c_null_char)
    call assert_equal_integer(rc, 0, 'emits a real metadata-only input event')
    recaptured = .false.
    do attempt = 1, 300
        if (capture_count() > before) then
            recaptured = .true.
            exit
        end if
        call gremlin_wait_ms(50)
    end do
    call assert_true(recaptured, 'owner completes the metadata-event recapture')
    call wait_green()
    call assert_true(gremlin_field(reply, 'active_generation') == generation, &
        'metadata-only input event retains the executable generation')
    call assert_equal_integer(line_count(read_text(configure_log)), configurations, &
        'unchanged captured inputs do not reconfigure and recreate another event')

    ! A real configuration edit must still invoke CMake and revoke the old PASS.
    call write_text(project//'/CMakeLists.txt', &
        cmake_source//'message(FATAL_ERROR "synthetic configuration edit")'//nl)
    rejected = .false.
    do attempt = 1, 300
        call status_now()
        if (gremlin_field(reply, 'state') == 'capture_failed' .or. &
                gremlin_field(reply, 'state') == 'error') then
            rejected = index(gremlin_field(reply, 'diagnostic'), &
                'CMake configuration failed') > 0
            exit
        end if
        call gremlin_wait_ms(50)
    end do
    call assert_true(rejected, 'changed configuration reaches the native CMake oracle')
    call assert_true(.not. json_boolean_value(json_member(reply, 'local_gate_green')), &
        'a failed changed configuration cannot reuse the previous green gate')
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
        call gremlin_json(driver, project, cache, state, args, reply, process, 10000)
    end subroutine status_now

    subroutine wait_green()
        integer :: poll
        do poll = 1, 300
            call status_now()
            if (json_boolean_value(json_member(reply, 'local_gate_green'))) return
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'the current input generation has a green gate')
    end subroutine wait_green

    integer function capture_count() result(count)
        type(json_value_t) :: value
        character(:), allocatable :: message
        logical :: valid
        call json_parse(read_text(counter), value, valid, message)
        call assert_true(valid, 'capture observer publishes complete JSON')
        count = int(json_number_value(json_member(value, 'count')))
    end function capture_count

    integer function line_count(text) result(count)
        character(len=*), intent(in) :: text
        integer :: i
        count = 0
        do i = 1, len(text)
            if (text(i:i) == nl) count = count + 1
        end do
    end function line_count
end program test_gremlin_cmake_configure_events
