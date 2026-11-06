program test_gremlin_cmake_lane_build
    !! Two lanes on one CMake checkout configure private build trees, so
    !! neither reconfigures the other's tree nor the caller's `build`.
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, read_text, assert_true, assert_process_ok, assert_file_absent, &
        finish_assertions
    use fo_test_cli, only: parse_json_report
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, gremlin_run, &
        gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, log
    character(:), allocatable :: sessions(:), text
    character(len=*), parameter :: nl = new_line('a')
    character(len=8), parameter :: lanes(2) = [character(len=8) :: 'lane-a', 'lane-b']
    type(string_list_t) :: args, environment
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer :: i, line_a, line_b

    call gremlin_setup(driver, scratch, project, cache, state)
    log = scratch//'/configured.log'
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(lane_build LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'file(APPEND "'//log//'" "${MODE} ${CMAKE_BINARY_DIR}\n")'//nl// &
        'add_executable(probe probe.f90)'//nl//'add_test(NAME probe COMMAND probe)'//nl)
    call write_text(project//'/probe.f90', 'program probe'//nl//'end program probe'//nl)
    allocate (character(len=64) :: sessions(2))
    do i = 1, 2
        environment = string_list_t()
        call list_add(environment, 'FO_BACKEND=cmake')
        call list_add(environment, 'FO_CMAKE_CONFIGURE_PRESET=')
        call list_add(environment, 'FO_CMAKE_ARGS=-DMODE='//trim(lanes(i)))
        call gremlin_start_args(args, project, trim(lanes(i)), 'probe')
        call list_add(args, '--random')
        call list_add(args, '0')
        call gremlin_run(driver, project, cache, state, args, process, environment, &
            20000)
        call parse_json_report(process, reply, 'lane start')
        call assert_process_ok(process, 'starts '//trim(lanes(i)))
        sessions(i) = gremlin_field(reply, 'session_id')
    end do
    do i = 1, 2
        call wait_green(trim(lanes(i)), trim(sessions(i)))
    end do
    text = read_text(log)
    line_a = index(text, 'lane-a ')
    line_b = index(text, 'lane-b ')
    call assert_true(line_a > 0 .and. line_b > 0, 'each lane configures with its own arguments')
    if (line_a > 0 .and. line_b > 0) call assert_true( &
        binary_dir(text, line_a) /= binary_dir(text, line_b), &
        'each lane configures its own build tree')
    call assert_file_absent(project//'/build/CMakeCache.txt', &
        'lanes leave the checkout build tree to the user')
    do i = 1, 2
        call gremlin_stop_lane(driver, project, cache, state, trim(lanes(i)), &
            trim(sessions(i)))
    end do
    call finish_assertions(retain_failed_scratch=.true.)

contains

    function binary_dir(text, start) result(path)
        character(len=*), intent(in) :: text
        integer, intent(in) :: start
        character(:), allocatable :: path
        integer :: space, finish

        space = start + index(text(start:), ' ') - 1
        finish = space + index(text(space:), nl) - 1
        path = text(space + 1:finish - 1)
    end function binary_dir

    subroutine wait_green(lane, session)
        character(len=*), intent(in) :: lane, session
        integer :: poll

        do poll = 1, 600
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
            if (json_boolean_value(json_member(reply, 'local_gate_green'))) return
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., lane//' reaches a green gate')
    end subroutine wait_green

end program test_gremlin_cmake_lane_build
