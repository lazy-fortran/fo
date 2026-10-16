program test_gremlin_agent_report
    !! Agent-facing Gremlin output (#218): status leads with a one-line
    !! verdict, counts and the failing cases with a log excerpt and handle,
    !! and `--since` (a generation or an event cursor) answers only with the
    !! change: newly failing, fixed and still failing cases. Full detail keeps
    !! its existing members behind the verdict.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, assert_true, assert_equal_string
    use fo_test_harness, only: assert_equal_integer, assert_contains, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms, gremlin_write_case
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_number_value, json_string_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: generation, cursor
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: report, counts, failing, delta
    integer :: i
    logical :: ready
    character(len=*), parameter :: lane = 'agent-report'
    character(len=*), parameter :: token = 'agent-report-failure-token'

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/fpm.toml', 'name = "agent_report"'//new_line('a'))
    call gremlin_write_case(project, 'test_ok', "print '(a)', 'ok'")
    call gremlin_write_case(project, 'test_bad', &
        "print '(a)', '"//token//"'"//new_line('a')//'error stop 3')
    call list_add(args, 'gremlin')
    call list_add(args, 'start')
    call list_add(args, '--dir')
    call list_add(args, project)
    call list_add(args, '--lane')
    call list_add(args, lane)
    call list_add(args, '--target')
    call list_add(args, 'test_ok')
    call list_add(args, '--target')
    call list_add(args, 'test_bad')
    call list_add(args, '--random')
    call list_add(args, '0')
    call list_add(args, '--timeout-seconds')
    call list_add(args, '20')
    call gremlin_json(driver, project, cache, state, args, report, process, 60000)
    call assert_equal_integer(process%exit_code, 0, 'lane starts')

    call await_counts(1, 1, ready)
    call assert_true(ready, 'one passing and one failing case are judged')
    if (.not. ready) call finish_assertions(retain_failed_scratch=.true.)

    ! The default answer is compact and leads with the verdict.
    call status('', report)
    call assert_true(index(process%stdout, '{"verdict":"FAIL: 1 failing') == 1, &
        'status leads with a one-line verdict')
    counts = json_member(report, 'counts')
    call assert_equal_integer(int(json_number_value(json_member(counts, 'failing'))), 1, &
        'counts name the failing cases')
    failing = json_member(report, 'failing')
    call assert_equal_integer(json_size(failing), 1, 'one failing case is listed')
    call assert_equal_string(gremlin_field(json_element(failing, 1), 'case_id'), &
        'test_bad', 'the failing case is named')
    call assert_equal_string(gremlin_field(json_element(failing, 1), 'status'), 'FAIL', &
        'the failing case carries its status')
    call assert_true(len(gremlin_field(json_element(failing, 1), 'log_path')) > 0, &
        'the failing case carries a log handle')
    call assert_contains(gremlin_field(json_element(failing, 1), 'excerpt'), token, &
        'the failing case carries a log excerpt')
    generation = gremlin_field(report, 'active_generation')
    call assert_true(len(generation) == 64, 'summary names the generation')

    ! Since the start of the journal, the failure is new.
    call status('0', report)
    delta = json_member(report, 'delta')
    call assert_equal_string(json_string_value(json_element( &
        json_member(delta, 'newly_failing'), 1)), 'test_bad', &
        'the newly failing case is named')
    call assert_equal_integer(int(json_number_value( &
        json_member(delta, 'newly_failing_count'))), 1, &
        'since the journal start the failure is newly failing')
    call assert_equal_integer(int(json_number_value(json_member(delta, 'fixed_count'))), &
        0, 'nothing is fixed yet')

    ! Since the current generation the failure persists.
    call status(generation, report)
    delta = json_member(report, 'delta')
    call assert_equal_integer(int(json_number_value( &
        json_member(delta, 'still_failing_count'))), 1, &
        'since the current generation the failure still fails')
    call assert_equal_integer(int(json_number_value( &
        json_member(delta, 'newly_failing_count'))), 0, 'no new failure appears')

    ! Event cursor of everything observed so far.
    call event_cursor(cursor)
    call assert_true(len(cursor) > 0, 'events expose a cursor')

    ! The failing case is repaired.
    call gremlin_write_case(project, 'test_bad', "print '(a)', 'repaired'")
    call await_counts(0, 2, ready)
    call assert_true(ready, 'the repaired case passes in the new generation')
    call status(cursor, report)
    delta = json_member(report, 'delta')
    call assert_equal_integer(int(json_number_value(json_member(delta, 'fixed_count'))), &
        1, 'since the cursor the repaired case is fixed')
    call assert_true(index(gremlin_field(report, 'verdict'), 'PASS') == 1, &
        'the verdict turns to PASS')
    call status(generation, report)
    delta = json_member(report, 'delta')
    call assert_equal_integer(int(json_number_value(json_member(delta, 'fixed_count'))), &
        1, 'since the old generation the repaired case is fixed')

    ! Full detail keeps its members behind the verdict.
    call status_full(report)
    call assert_true(index(process%stdout, '{"verdict":"') == 1 .and. &
        index(process%stdout, '"events":') > 0 .and. &
        index(process%stdout, '"next_cursor":') > 0 .and. &
        index(process%stdout, '"protocol":1') > 0, &
        'full detail keeps the existing members behind the verdict')
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine lane_args(extra_name, extra_value)
        character(len=*), intent(in) :: extra_name, extra_value

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        if (len(extra_name) > 0) then
            call list_add(args, extra_name)
            call list_add(args, extra_value)
        end if
    end subroutine lane_args

    subroutine status(since, document)
        character(len=*), intent(in) :: since
        type(json_value_t), intent(out) :: document
        character(:), allocatable :: message
        logical :: valid

        if (len(since) > 0) then
            call lane_args('--since', since)
        else
            call lane_args('', '')
        end if
        call gremlin_run(driver, project, cache, state, args, process, timeout=30000)
        call assert_equal_integer(process%exit_code, 0, 'status succeeds')
        call json_parse(process%stdout, document, valid, message)
        call assert_true(valid, 'status is parseable JSON')
    end subroutine status

    subroutine status_full(document)
        type(json_value_t), intent(out) :: document
        character(:), allocatable :: message
        logical :: valid

        call lane_args('--detail', 'full')
        call gremlin_run(driver, project, cache, state, args, process, timeout=30000)
        call assert_equal_integer(process%exit_code, 0, 'full status succeeds')
        call json_parse(process%stdout, document, valid, message)
        call assert_true(valid, 'full status is parseable JSON')
    end subroutine status_full

    subroutine event_cursor(value)
        character(len=32) :: digits
        character(:), allocatable, intent(out) :: value
        type(json_value_t) :: page
        character(:), allocatable :: message
        logical :: valid

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'events')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--max-records')
        call list_add(args, '128')
        call gremlin_run(driver, project, cache, state, args, process, timeout=30000)
        call json_parse(process%stdout, page, valid, message)
        value = ''
        if (.not. valid) return
        write (digits, '(i0)') nint(json_number_value(json_member(page, 'next_cursor')))
        value = trim(digits)
    end subroutine event_cursor

    subroutine await_counts(want_failing, want_pass, found)
        integer, intent(in) :: want_failing, want_pass
        logical, intent(out) :: found
        type(json_value_t) :: view, summary
        character(:), allocatable :: message
        logical :: valid

        found = .false.
        do i = 1, 1200
            call lane_args('', '')
            call gremlin_run(driver, project, cache, state, args, process, timeout=30000)
            call json_parse(process%stdout, view, valid, message)
            if (valid) then
                summary = json_member(view, 'counts')
                if (int(json_number_value(json_member(summary, 'failing'))) == &
                        want_failing .and. &
                    int(json_number_value(json_member(summary, 'pass'))) == &
                        want_pass) then
                    found = .true.
                    return
                end if
            end if
            call gremlin_wait_ms(100)
        end do
    end subroutine await_counts

end program test_gremlin_agent_report
