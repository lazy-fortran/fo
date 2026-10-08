program test_gremlin_terminal_receipt_recovery
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: assert_true, assert_equal_integer, write_text
    use fo_test_harness, only: read_text, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup
    use fo_test_gremlin_oracle, only: gremlin_json, gremlin_stop_lane
    use fo_test_gremlin_oracle, only: gremlin_field
    use fo_gremlin_coverage, only: coverage_record_path, coverage_read_view_path
    use fo_gremlin_coverage, only: COVERAGE_OK
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_state, only: gremlin_session_state_dir
    use fo_test_json, only: json_value_t, json_member, json_number_value
    use fo_test_json, only: json_boolean_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: marker, generation, owner, coverage_path
    character(:), allocatable :: receipt_text
    character(len=4096) :: lane_state_dir
    type(string_list_t) :: arguments
    type(process_result_t) :: process
    type(json_value_t) :: document, coverage
    type(gremlin_coverage_view_t) :: coverage_view
    character(len=256) :: message
    integer :: status
    character(len=*), parameter :: lane = 'terminal-receipt-recovery'
    character(len=*), parameter :: case_name = 'test_receipt_case'

    call gremlin_setup(driver, scratch, project, cache, state)
    marker = scratch//'/executions.log'
    call write_text(project//'/fpm.toml', 'name = "receipt_recovery"'//new_line('a'))
    call write_counting_case()

    call start_owner()
    owner = gremlin_field(document, 'session_id')
    call wait_until_quiescent(owner)
    call query_status(owner)
    generation = gremlin_field(document, 'active_generation')
    coverage = json_member(document, 'coverage')
    call assert_true(process%exit_code == 0 .and. len(generation) == 64, &
        'first public run completes an exact generation')
    call assert_equal_integer(int(json_number_value( &
        json_member(coverage, 'pass'))), 1, &
        'first run records one passing obligation')
    call assert_equal_integer(line_count(read_text(marker)), 1, &
        'independent marker confirms one execution before simulated crash cut')
    call gremlin_stop_lane(driver, project, cache, state, lane, owner)

    ! Keep the fsynced campaign receipt but roll coverage back to the exact
    ! state that would exist if the owner died between journal append and save.
    call gremlin_session_state_dir(project, lane, lane_state_dir, status, message)
    call assert_true(status == 0, 'locates the public lane state directory')
    receipt_text = read_text(trim(lane_state_dir)//'/campaign-journal.jsonl')
    call assert_true(index(receipt_text, '"case_id":"'//case_name//'"') > 0 .and. &
        index(receipt_text, '"status":"PASS"') > 0, &
        'campaign journal retains the real terminal PASS receipt')
    coverage_path = trim(lane_state_dir)//'/coverage-'//generation//'.state'
    call coverage_record_path(coverage_path, generation, case_name, 'UNKNOWN', &
        status, message)
    call assert_true(status == COVERAGE_OK, &
        'fixture preserves the durable PASS receipt before coverage credit')
    call coverage_read_view_path(coverage_path, generation, coverage_view, status, &
        message)
    call assert_true(status == COVERAGE_OK .and. coverage_view%pass_count == 0 .and. &
        coverage_view%unknown_count == 1, &
        'crash cut leaves the receipt durable with exactly one UNKNOWN obligation')

    call start_owner()
    owner = gremlin_field(document, 'session_id')
    call wait_until_quiescent(owner)
    call query_status(owner)
    coverage = json_member(document, 'coverage')
    call assert_true(json_boolean_value(json_member(document, &
        'local_gate_green')) .and. int(json_number_value(json_member(document, &
        'gate_passed'))) == 1, &
        'restart credits the exact terminal receipt to public gate readiness')
    call assert_equal_integer(int(json_number_value( &
        json_member(coverage, 'pass'))), 1, &
        'restart replays the terminal receipt into coverage')
    call assert_equal_integer(int(json_number_value( &
        json_member(coverage, 'unknown'))), 0, &
        'replayed receipt discharges the only unknown obligation')
    call assert_equal_integer(line_count(read_text(marker)), 1, &
        'recovery credits the receipt without executing the case again')
    call gremlin_stop_lane(driver, project, cache, state, lane, owner)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine write_counting_case()
        call write_text(project//'/test/'//case_name//'.f90', &
            'program '//case_name//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer :: unit'//new_line('a')// &
            "open(newunit=unit, file='"//marker// &
            "', status='unknown', position='append')"//new_line('a')// &
            "write(unit, '(a)') 'EXECUTED'"//new_line('a')// &
            'close(unit)'//new_line('a')// &
            'end program '//case_name//new_line('a'))
    end subroutine write_counting_case

    subroutine start_owner()
        arguments = string_list_t()
        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'start')
        call list_add(arguments, '--dir')
        call list_add(arguments, project)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--random')
        call list_add(arguments, '1')
        call list_add(arguments, '--seed')
        call list_add(arguments, '17')
        call gremlin_json(driver, project, cache, state, arguments, document, &
            process, 30000)
        call assert_true(process%exit_code == 0 .and. &
            len(gremlin_field(document, 'session_id')) > 0, &
            'starts the public Gremlin owner')
    end subroutine start_owner

    subroutine wait_until_quiescent(session)
        character(len=*), intent(in) :: session
        integer :: attempt

        arguments = string_list_t()
        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'wait')
        call list_add(arguments, '--dir')
        call list_add(arguments, project)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--session')
        call list_add(arguments, session)
        call list_add(arguments, '--until')
        call list_add(arguments, 'quiescent')
        call list_add(arguments, '--wait-ms')
        call list_add(arguments, '30000')
        ! A loaded host can need several bounded public waits.
        do attempt = 1, 4
            call gremlin_json(driver, project, cache, state, arguments, document, &
                process, 35000)
            if (process%exit_code /= 0) exit
            if (json_boolean_value(json_member(document, 'wait_satisfied'))) exit
        end do
        call assert_true(process%exit_code == 0 .and. &
            json_boolean_value(json_member(document, 'wait_satisfied')), &
            'public owner reaches quiescence')
    end subroutine wait_until_quiescent

    subroutine query_status(session)
        character(len=*), intent(in) :: session

        arguments = string_list_t()
        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'status')
        call list_add(arguments, '--detail')
        call list_add(arguments, 'full')
        call list_add(arguments, '--dir')
        call list_add(arguments, project)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--session')
        call list_add(arguments, session)
        call gremlin_json(driver, project, cache, state, arguments, document, &
            process, 10000)
        call assert_true(process%exit_code == 0, &
            'public status reads coverage for the selected owner')
    end subroutine query_status

    integer function line_count(text)
        character(len=*), intent(in) :: text
        integer :: i

        line_count = 0
        do i = 1, len(text)
            if (text(i:i) == new_line('a')) line_count = line_count + 1
        end do
    end function line_count

end program test_gremlin_terminal_receipt_recovery
