program test_gremlin_ctest_fixture_lifecycle
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_lines, assert_true, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, &
        gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, &
        json_size, json_element
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(*), parameter :: lane = 'fixture-lifecycle'
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply, events, receipt
    logical :: green, first, second, helper
    integer :: attempt, i

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_lines(project//'/CMakeLists.txt', [character(180) :: &
        'cmake_minimum_required(VERSION 3.22)', &
        'project(fixture_lifecycle LANGUAGES Fortran)', &
        'enable_testing()', &
        'add_executable(probe probe.f90)', &
        'foreach(n setup first second cleanup)', &
        '  add_test(NAME ${n} COMMAND probe ${n})', &
        'endforeach()', &
        'set_tests_properties(setup PROPERTIES FIXTURES_SETUP service)', &
        'set_tests_properties(first second PROPERTIES FIXTURES_REQUIRED service)', &
        'set_tests_properties(cleanup PROPERTIES FIXTURES_CLEANUP service)'])
    call write_lines(project//'/probe.f90', [character(180) :: &
        'program probe', &
        'use iso_fortran_env, only: int64', &
        'implicit none', &
        'character(16) :: mode', &
        'integer :: u, ios', &
        'integer(int64) :: start, now, rate', &
        'call get_command_argument(1, mode)', &
        'select case(trim(mode))', &
        'case("setup")', &
        '  open(newunit=u, file="active", status="new", iostat=ios)', &
        '  if(ios /= 0) error stop "overlapping fixture setups"', &
        '  close(u)', &
        'case("cleanup")', &
        '  open(newunit=u, file="active", status="old", iostat=ios)', &
        '  if(ios /= 0) error stop "cleanup without setup"', &
        '  close(u, status="delete")', &
        'case default', &
        '  call system_clock(start, rate)', &
        '  do', &
        '    call system_clock(now)', &
        '    if(real(now-start)/real(rate) >= 0.4) exit', &
        '  end do', &
        '  open(newunit=u, file="active", status="old", iostat=ios)', &
        '  if(ios /= 0) error stop "fixture cleaned before requirer"', &
        '  close(u)', &
        'end select', &
        'end program probe'])
    call gremlin_start_args(args, project, lane, 'first')
    call list_add(args, '--jobs')
    call list_add(args, '2')
    call list_add(args, '--random')
    call list_add(args, '8')
    call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, 'fixture lane starts')
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
    green = .false.
    first = .false.
    second = .false.
    helper = .false.
    do attempt = 1, 600
        call gremlin_json(driver, project, cache, state, args, reply, process, 10000)
        if (process%exit_code /= 0) exit
        if (gremlin_field(reply, 'state') == 'error' .or. &
            gremlin_field(reply, 'state') == 'build_failed') exit
        green = json_boolean_value(json_member(reply, 'local_gate_green'))
        events = json_member(reply, 'events')
        do i = 1, json_size(events)
            receipt = json_element(events, i)
            select case(gremlin_field(receipt, 'case_id'))
            case('first')
                first = gremlin_field(receipt, 'status') == 'PASS'
            case('second')
                second = gremlin_field(receipt, 'status') == 'PASS'
            case('setup', 'cleanup')
                helper = .true.
            end select
        end do
        if (green .and. first .and. second) exit
        call gremlin_wait_ms(50)
    end do
    call assert_true(green .and. first .and. second, &
        'concurrent requirers each complete an exclusive fixture lifecycle')
    call assert_true(.not. helper, 'fixture helpers run only inside requirer closures')
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)
end program test_gremlin_ctest_fixture_lifecycle
