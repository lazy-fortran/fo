program test_gremlin_reproduce_logs
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, assert_true
    use fo_test_harness, only: assert_equal_integer, assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_wait_ms
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_parse
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: session, generation, first_log, second_log
    character(:), allocatable :: first_text, second_text, lane
    type(string_list_t) :: arguments
    type(process_result_t) :: process
    type(json_value_t) :: document, started, event, events, parsed
    logical :: valid
    character(:), allocatable :: parse_error

    call gremlin_setup(driver, scratch, project, cache, state)
    lane = 'reproduce-log-oracle'
    call write_text(project//'/fpm.toml', 'name = "gremlin_reproduce_log_probe"'//new_line('a'))
    call gremlin_write_case(project, 'test_reproduce_anchor', &
        "print '(a)', 'FO_REPRODUCE_ANCHOR_OUTPUT'")
    call gremlin_write_case(project, 'test_reproduce_first', &
        "print '(a)', 'FO_REPRODUCE_FIRST_OUTPUT_61c8c1'"//new_line('a')// &
        "print '(a)', '"//achar(34)//'quoted'//achar(34)//achar(92)//"path'"// &
        new_line('a')//"print '(a)', repeat('q',4096)//'LONG_OUTPUT_END'"// &
        new_line('a')//'error stop 7')
    call gremlin_write_case(project, 'test_reproduce_second', &
        "print '(a)', 'FO_REPRODUCE_SECOND_OUTPUT_9d09d2'"//new_line('a')//'error stop 7')
    call gremlin_start_args(arguments, project, lane, 'test_reproduce_anchor')
    call list_add(arguments, '--seed')
    call list_add(arguments, '1729')
    call list_add(arguments, '--json')
    call gremlin_json(driver, project, cache, state, arguments, started, process, 120000)
    session = member_text(started, 'session_id')
    call assert_true(len(session) > 0, 'start returns the owner session id')
    call wait_for_anchor(session, generation)

    call reproduce('test_reproduce_first', first_log)
    call reproduce('test_reproduce_second', second_log)
    call assert_true(first_log /= second_log, 'sequential receipts use distinct reproduction logs')
    first_text = read_text(first_log)
    second_text = read_text(second_log)
    call assert_true(index(first_text, 'FO_REPRODUCE_FIRST_OUTPUT_61c8c1') > 0, &
        'first receipt log contains its independent test output')
    call assert_true(index(first_text, 'FO_REPRODUCE_SECOND_OUTPUT_9d09d2') == 0, &
        'second test cannot overwrite the first receipt log')
    call assert_true(index(second_text, 'FO_REPRODUCE_SECOND_OUTPUT_9d09d2') > 0, &
        'second receipt log contains its independent test output')
    call assert_true(index(second_text, 'FO_REPRODUCE_FIRST_OUTPUT_61c8c1') == 0, &
        'second receipt log excludes first test output')
    call assert_output_json(first_text)

    call stop_lane(session)
    call finish_assertions()

contains

    subroutine assert_output_json(text)
        character(len=*), intent(in) :: text
        type(json_value_t) :: report, tests, entry
        character(:), allocatable :: message, output
        logical :: valid
        integer :: first

        first = index(text, '{"tests":[')
        call assert_true(first > 0, 'reproduction log preserves the runner JSON report')
        if (first == 0) return
        call json_parse(text(first:), report, valid, message)
        call assert_true(valid, 'captured output remains valid JSON: '//message)
        tests = json_member(report, 'tests')
        entry = json_element(tests, 1)
        output = member_text(entry, 'output')
        call assert_true(index(output, achar(34)//'quoted'//achar(34)//achar(92)//'path') > 0, &
            'JSON preserves independently generated quotes and backslashes in test output')
        call assert_true(index(output, repeat('q', 4096)//'LONG_OUTPUT_END') > 0, &
            'JSON preserves a complete captured line longer than the old fixed buffer')
    end subroutine assert_output_json

    function member_text(object, key) result(value)
        type(json_value_t), intent(in) :: object
        character(len=*), intent(in) :: key
        character(:), allocatable :: value
        type(json_value_t) :: item

        item = json_member(object, key)
        value = json_string_value(item)
    end function member_text

    subroutine status_now(owner, value)
        character(len=*), intent(in) :: owner
        type(json_value_t), intent(out) :: value
        type(process_result_t) :: result
        type(string_list_t) :: args

        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, owner)
        call list_add(args, '--cursor')
        call list_add(args, '0')
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, value, result, 30000)
    end subroutine status_now

    subroutine wait_for_anchor(owner, active_generation)
        character(len=*), intent(in) :: owner
        character(:), allocatable, intent(out) :: active_generation
        type(json_value_t) :: status, list, receipt
        integer :: attempt, j

        active_generation = ''
        do attempt = 1, 1200
            call status_now(owner, status)
            active_generation = member_text(status, 'active_generation')
            list = json_member(status, 'events')
            do j = 1, json_size(list)
                receipt = json_element(list, j)
                if (member_text(receipt, 'case_id') == 'test_reproduce_anchor' .and. &
                    member_text(receipt, 'status') == 'PASS' .and. &
                    member_text(receipt, 'generation') == active_generation) return
            end do
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'anchor completes on the active immutable generation')
    end subroutine wait_for_anchor

    subroutine reproduce(case_id, log_path)
        character(len=*), intent(in) :: case_id
        character(:), allocatable, intent(out) :: log_path
        type(string_list_t) :: args
        type(process_result_t) :: result
        type(json_value_t) :: response
        integer :: j, attempt

        call list_add(args, 'gremlin')
        call list_add(args, 'reproduce')
        call list_add(args, case_id)
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, session)
        call list_add(args, '--generation')
        call list_add(args, generation)
        call list_add(args, '--json')
        call gremlin_run(driver, project, cache, state, args, result, timeout=120000)
        call assert_equal_integer(result%exit_code, 1, case_id//' returns its deliberate failure')
        call json_parse(result%stdout, response, valid, parse_error)
        call assert_true(valid, case_id//' reproduction result is JSON')
        call assert_equal_string(member_text(response, 'state'), 'FAIL', &
            case_id//' result records the test failure')
        do attempt = 1, 100
            call status_now(session, document)
            events = json_member(document, 'events')
            do j = 1, json_size(events)
                event = json_element(events, j)
                if (member_text(event, 'case_id') == case_id .and. &
                    member_text(event, 'generation') == generation .and. &
                    member_text(event, 'status') == 'FAIL' .and. &
                    index(member_text(event, 'log_path'), '-reproduce-') > 0) then
                    log_path = member_text(event, 'log_path')
                    if (len(log_path) > 0) return
                end if
            end do
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., case_id//' durable receipt exposes its reproduction log')
        log_path = ''
    end subroutine reproduce

    subroutine stop_lane(owner)
        character(len=*), intent(in) :: owner
        call gremlin_stop_lane(driver, project, cache, state, lane, owner)
    end subroutine stop_lane

end program test_gremlin_reproduce_logs
