program test_gremlin_timeout_requalification
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, make_directory, assert_true
    use fo_test_harness, only: assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_field
    use fo_test_gremlin_oracle, only: gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_number_value
    use fo_test_json, only: json_boolean_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: generation, initial_markers
    character(len=*), parameter :: lane = 'timeout-requalification'
    character, parameter :: nl = new_line('a')
    type(process_result_t) :: process
    type(json_value_t) :: reply, coverage, value
    integer :: count

    call gremlin_setup(driver, scratch, project, cache, state)
    call make_directory(project//'/test')
    call write_text(project//'/fpm.toml', 'name = "timeout_requalification"'//nl// &
        '[extra.fo]'//nl//'test-retries = 0'//nl)
    call write_text(scratch//'/markers', '')
    call write_text(project//'/test/test_pass.f90', &
        'program test_pass'//nl//'implicit none'//nl//'integer :: unit'//nl// &
        "open(newunit=unit,file='"//scratch// &
        "/markers',status='old',position='append')"//nl// &
        "write(unit,'(a)') 'pass'"//nl//'close(unit)'//nl//'end program'//nl)
    call write_text(project//'/test/test_timeout.f90', &
        'program test_timeout'//nl//'implicit none'//nl// &
        'integer :: unit'//nl//'logical :: released'//nl// &
        "open(newunit=unit,file='"//scratch// &
        "/markers',status='old',position='append')"//nl// &
        "write(unit,'(a)') 'timeout'"//nl//'close(unit)'//nl// &
        'do'//nl//"inquire(file='"//scratch//"/release',exist=released)"//nl// &
        'if (released) exit'//nl//'end do'//nl//'end program'//nl)

    call start_lane('1')
    call await_settled()
    generation = gremlin_field(reply, 'active_generation')
    call assert_true(len(generation) == 64, 'first campaign has an exact generation')
    call assert_count('pass', 1)
    call assert_count('timeout', 1)
    call assert_true(.not. json_boolean_value(json_member(reply, 'local_gate_green')), &
        'a timed out case does not establish green')
    initial_markers = read_text(scratch//'/markers')
    call stop_lane()

    call start_lane('1')
    call await_settled()
    call assert_equal_string(read_text(scratch//'/markers'), initial_markers, &
        'unchanged budget resumes without rerunning terminal cases')
    call assert_count('timeout', 1)
    call stop_lane()

    ! Release lives outside project inputs; the frozen source generation is unchanged.
    call write_text(scratch//'/release', '')
    call start_lane('10')
    call await_settled()
    call assert_equal_string(gremlin_field(reply, 'active_generation'), generation, &
        'explicit deadline change requalifies the same frozen source generation')
    call assert_count('pass', 2)
    call assert_count('timeout', 0)
    call assert_true(json_boolean_value(json_member(reply, 'local_gate_green')), &
        'actual successful execution discharges the renewed gate')
    initial_markers = read_text(scratch//'/markers')
    count = occurrences(initial_markers, 'pass'//nl)
    call assert_true(count == 1, 'the previously passing case executes only once')
    count = occurrences(initial_markers, 'timeout'//nl)
    call assert_true(count == 2, 'only the timed out case executes again')
    call stop_lane()
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine start_lane(seconds)
        character(len=*), intent(in) :: seconds
        type(string_list_t) :: args

        call list_add(args, 'gremlin')
        call list_add(args, 'start')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--target')
        call list_add(args, 'test_pass')
        call list_add(args, '--target')
        call list_add(args, 'test_timeout')
        call list_add(args, '--random')
        call list_add(args, '0')
        call list_add(args, '--timeout-seconds')
        call list_add(args, seconds)
        call gremlin_json(driver, project, cache, state, args, reply, process, 30000)
        session = gremlin_field(reply, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(session) > 0, &
            'starts the public campaign')
        if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)
    end subroutine start_lane

    subroutine await_settled()
        type(string_list_t) :: args
        integer :: poll
        logical :: settled

        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--detail')
        call list_add(args, 'full')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, session)
        settled = .false.
        do poll = 1, 600
            call gremlin_json(driver, project, cache, state, args, reply, process, &
                10000)
            if (gremlin_field(reply, 'state') == 'quiescent') then
                settled = .true.
                exit
            end if
            if (gremlin_field(reply, 'state') == 'error') exit
            call gremlin_wait_ms(100)
        end do
        call assert_true(settled, 'campaign settles within its safety deadline')
        if (.not. settled) then
            call stop_lane()
            call finish_assertions(retain_failed_scratch=.true.)
        end if
    end subroutine await_settled

    subroutine assert_count(name, expected)
        character(len=*), intent(in) :: name
        integer, intent(in) :: expected

        coverage = json_member(reply, 'coverage')
        value = json_member(coverage, name)
        call assert_true(int(json_number_value(value)) == expected, &
            'coverage '//name//' count reflects executed behavior')
    end subroutine assert_count

    subroutine stop_lane()
        call gremlin_stop_lane(driver, project, cache, state, lane, session)
    end subroutine stop_lane

    integer function occurrences(text, token) result(total)
        character(len=*), intent(in) :: text, token
        integer :: offset, at

        total = 0
        offset = 1
        do
            at = index(text(offset:), token)
            if (at == 0) exit
            total = total + 1
            offset = offset + at + len(token) - 1
        end do
    end function occurrences

end program test_gremlin_timeout_requalification
