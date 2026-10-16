program test_gremlin_listed_cases
    !! Cases inside a test executable are Gremlin cases (#217): the cases
    !! test_multi::one and test_multi::two of one listed executable each get
    !! a receipt, and after an edit of the module only case two uses, case
    !! one reuses the PASS of the previous generation while case two runs
    !! again. The same on fpm and CMake.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, assert_true
    use fo_test_harness, only: assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    use fo_test_json, only: json_string_value, json_size, json_element
    use fo_test_json, only: json_number_value
    use fo_test_cases_fixture, only: cases_fixture_write
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character, parameter :: nl = new_line('a')

    call gremlin_setup(driver, scratch, project, cache, state)
    call scenario('fpm', scratch//'/fpm-project')
    call scenario('cmake', scratch//'/cmake-project')
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine scenario(lane, dir)
        character(len=*), intent(in) :: lane, dir
        type(string_list_t) :: args
        type(process_result_t) :: process
        type(json_value_t) :: reply
        character(:), allocatable :: session, first, second
        logical :: ready

        call cases_fixture_write(dir, lane, scheduling=.false.)
        call arguments(lane, dir, 'start', args)
        call list_add(args, '--target')
        call list_add(args, 'test_multi::one')
        call list_add(args, '--target')
        call list_add(args, 'test_multi::two')
        call list_add(args, '--random')
        call list_add(args, '0')
        call gremlin_json(driver, dir, cache, state, args, reply, process, 30000)
        session = gremlin_field(reply, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(session) > 0, &
            lane//': starts its gate campaign on the listed cases: '// &
            process%stdout//process%stderr)
        if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)

        call wait_for_gate(lane, dir, session, '', first, ready)
        call assert_true(ready, lane//': first generation passes the listed cases')
        if (ready) then
            call expect_receipt(lane, dir, session, first, 'test_multi::one', 'run', '')
            call expect_receipt(lane, dir, session, first, 'test_multi::two', 'run', '')
            ! Used by case two only.
            call write_text(dir//'/src/tc_two.f90', 'module tc_two'//nl// &
                'implicit none'//nl//'contains'//nl// &
                'integer function two_value()'//nl//'two_value = 2'//nl// &
                'end function'//nl//'integer function two_extra()'//nl// &
                'two_extra = 3'//nl//'end function'//nl//'end module'//nl)
            call wait_for_gate(lane, dir, session, first, second, ready)
            call assert_true(ready, lane//': edited generation passes the cases')
        end if
        if (ready) then
            call expect_receipt(lane, dir, session, second, 'test_multi::one', &
                'reused', first)
            call expect_receipt(lane, dir, session, second, 'test_multi::two', &
                'run', '')
        end if
        call gremlin_stop_lane(driver, dir, cache, state, lane, session, &
            allow_terminal_error=.true.)
    end subroutine scenario

    subroutine arguments(lane, dir, action, values)
        character(len=*), intent(in) :: lane, dir, action
        type(string_list_t), intent(out) :: values

        call list_add(values, 'gremlin')
        call list_add(values, action)
        call list_add(values, '--dir')
        call list_add(values, dir)
        call list_add(values, '--lane')
        call list_add(values, lane)
    end subroutine arguments

    subroutine wait_for_gate(lane, dir, session, previous, generation, found)
        character(len=*), intent(in) :: lane, dir, session, previous
        character(:), allocatable, intent(out) :: generation
        logical, intent(out) :: found
        type(string_list_t) :: status_args
        type(json_value_t) :: status
        type(process_result_t) :: observed
        integer :: poll

        found = .false.
        generation = ''
        call arguments(lane, dir, 'status', status_args)
        call list_add(status_args, '--session')
        call list_add(status_args, session)
        do poll = 1, 4800
            call gremlin_json(driver, dir, cache, state, status_args, status, &
                observed, 10000)
            if (observed%exit_code /= 0) return
            if (gremlin_field(status, 'state') == 'error') return
            generation = gremlin_field(status, 'active_generation')
            if (len(generation) == 64 .and. generation /= previous) then
                found = json_boolean_value(json_member(status, 'local_gate_green'))
                if (found) return
            end if
            call gremlin_wait_ms(50)
        end do
    end subroutine wait_for_gate

    subroutine expect_receipt(lane, dir, session, generation, case_id, &
            disposition, reused_from)
        !! The case's receipt in the generation passed with this disposition
        !! (reused: from that earlier generation).
        character(len=*), intent(in) :: lane, dir, session, generation, case_id
        character(len=*), intent(in) :: disposition, reused_from
        type(string_list_t) :: query
        type(json_value_t) :: status, events, receipt
        type(process_result_t) :: observed
        character(:), allocatable :: cursor, got_status, got_disposition, got_from
        character(len=32) :: number
        integer :: k, page
        logical :: found

        found = .false.
        cursor = '0'
        do page = 1, 1000
            call arguments(lane, dir, 'status', query)
            call list_add(query, '--detail')
            call list_add(query, 'full')
            call list_add(query, '--session')
            call list_add(query, session)
            call list_add(query, '--cursor')
            call list_add(query, cursor)
            call list_add(query, '--max-records')
            call list_add(query, '128')
            call gremlin_json(driver, dir, cache, state, query, status, observed, &
                10000)
            events = json_member(status, 'events')
            do k = 1, json_size(events)
                receipt = json_element(events, k)
                if (json_string_value(json_member(receipt, 'generation')) /= &
                    generation) cycle
                if (json_string_value(json_member(receipt, 'case_id')) /= case_id) &
                    cycle
                found = .true.
                got_status = json_string_value(json_member(receipt, 'status'))
                got_disposition = json_string_value(json_member(receipt, &
                    'disposition'))
                got_from = json_string_value(json_member(receipt, 'reused_from'))
            end do
            if (.not. json_boolean_value(json_member(status, 'has_more'))) exit
            write (number, '(i0)') int(json_number_value(json_member(status, &
                'next_cursor')), int64)
            cursor = trim(number)
        end do
        call assert_true(found, lane//': '//case_id//' has a receipt')
        if (.not. found) return
        call assert_equal_string(got_status, 'PASS', lane//': '//case_id//' passes')
        call assert_equal_string(got_disposition, disposition, &
            lane//': '//case_id//' disposition')
        call assert_equal_string(got_from, reused_from, &
            lane//': '//case_id//' reused receipt source')
    end subroutine expect_receipt

end program test_gremlin_listed_cases
