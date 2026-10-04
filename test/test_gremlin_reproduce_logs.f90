program test_gremlin_reproduce_logs
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, write_text, read_text, file_exists
    use fo_test_harness, only: remove_path, assert_true
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_wait_ms
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_parse
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    implicit none

    character(:), allocatable :: driver, scratch, project, dependency, cache, state
    character(:), allocatable :: relocated_cache
    character(:), allocatable :: session, generation, first_log, second_log
    character(:), allocatable :: first_text, second_text, lane
    character(:), allocatable :: startup_error
    type(string_list_t) :: arguments
    type(process_result_t) :: process
    type(json_value_t) :: document, started, event, events, parsed
    logical :: valid, anchor_ready
    character(:), allocatable :: parse_error

    call gremlin_setup(driver, scratch, project, cache, state)
    lane = 'reproduce-log-oracle'
    dependency = scratch//'/fixture-dep'
    relocated_cache = scratch//'/cache-relocated'
    call make_directory(dependency//'/src')
    call make_directory(relocated_cache)
    call write_text(project//'/fpm.toml', &
        'name = "gremlin_reproduce_log_probe"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'fixture_dep = { path = "../fixture-dep" }'//new_line('a')// &
        '[[extra.fo.inputs]]'//new_line('a')// &
        'path = "token.txt"'//new_line('a')// &
        'role = "test-fixture"'//new_line('a'))
    call write_text(dependency//'/fpm.toml', 'name = "fixture_dep"'//new_line('a'))
    call write_dependency('FROZEN_DEPENDENCY_TOKEN')
    call write_text(project//'/token.txt', 'FROZEN_REPRODUCE_TOKEN'//new_line('a'))
    call gremlin_write_case(project, 'test_reproduce_anchor', &
        "print '(a)', 'FO_REPRODUCE_ANCHOR_OUTPUT'")
    call write_reproduction_case('test_reproduce_first')
    call write_reproduction_case('test_reproduce_second')
    call gremlin_start_args(arguments, project, lane, 'test_reproduce_anchor')
    call list_add(arguments, '--seed')
    call list_add(arguments, '1729')
    call list_add(arguments, '--json')
    call gremlin_json(driver, project, cache, state, arguments, started, process, &
        30000)
    session = member_text(started, 'session_id')
    if (len(session) == 0 .or. process%exit_code /= 0 .or. &
            process%runner_failed .or. process%timed_out) then
        startup_error = start_failure(process)
        if (len(session) > 0) call stop_lane(session)
        call assert_true(.false., 'initial owner start failed: '//startup_error)
        call finish_assertions(retain_failed_scratch=.true.)
    end if
    call wait_for_anchor(session, generation, anchor_ready, startup_error)
    if (.not. anchor_ready) then
        call stop_lane(session)
        call assert_true(.false., 'initial anchor did not become ready: '//startup_error)
        call finish_assertions(retain_failed_scratch=.true.)
    end if

    call write_text(project//'/token.txt', 'EDITED_REPRODUCE_TOKEN'//new_line('a'))
    call write_dependency('EDITED_DEPENDENCY_TOKEN')
    call reproduce('test_reproduce_first', first_log)
    call assert_true(index(read_text(first_log), 'FROZEN_DEPENDENCY_TOKEN') > 0, &
        'reproduction builds from the captured dependency alias')
    call remove_path(project//'/token.txt')
    call remove_path(dependency//'/src/fixture_dep.f90')
    call assert_true(.not. file_exists(project//'/token.txt'), &
        'live declared project input is deleted before manifest recovery')
    call assert_true(.not. file_exists(dependency//'/src/fixture_dep.f90'), &
        'live path-dependency source is deleted before manifest recovery')
    call reproduce('test_reproduce_second', second_log, relocated_cache)
    call assert_true(first_log /= second_log, &
        'sequential receipts use distinct reproduction logs')
    first_text = read_text(first_log)
    second_text = read_text(second_log)
    call assert_true(index(first_text, 'FO_REPRODUCE_FIRST_OUTPUT_61c8c1') > 0, &
        'first receipt log contains its independent test output')
    call assert_true(index(first_text, 'FROZEN_REPRODUCE_TOKEN') > 0, &
        'reproduced test reads the declared fixture from its captured generation')
    call assert_true(index(first_text, 'EDITED_REPRODUCE_TOKEN') == 0, &
        'reproduced test does not read later editable fixture bytes')
    call assert_true(index(first_text, 'EDITED_DEPENDENCY_TOKEN') == 0, &
        'reproduced test excludes later editable dependency source bytes')
    call assert_true(index(first_text, 'FO_REPRODUCE_SECOND_OUTPUT_9d09d2') == 0, &
        'second test cannot overwrite the first receipt log')
    call assert_true(index(second_text, 'FO_REPRODUCE_SECOND_OUTPUT_9d09d2') > 0, &
        'second receipt log contains its independent test output')
    call assert_true(index(second_text, 'FROZEN_REPRODUCE_TOKEN') > 0, &
        'reopened private view restores captured input after source deletion')
    call assert_true(index(second_text, 'EDITED_REPRODUCE_TOKEN') == 0, &
        'reopened private view excludes edited live input bytes')
    call assert_true(index(second_text, 'FROZEN_DEPENDENCY_TOKEN') > 0, &
        'relocated cache still restores the captured dependency alias')
    call assert_true(index(second_text, 'EDITED_DEPENDENCY_TOKEN') == 0, &
        'relocated cache excludes edited live dependency bytes')
    call assert_true(index(second_text, 'FO_REPRODUCE_FIRST_OUTPUT_61c8c1') == 0, &
        'second receipt log excludes first test output')
    call assert_output_json(first_text)

    call stop_lane(session)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine write_dependency(token)
        character(len=*), intent(in) :: token

        call write_text(dependency//'/src/fixture_dep.f90', &
            'module fixture_dep'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(len=*), parameter :: fixture_token = "'//trim(token)//'"'// &
            new_line('a')//'end module fixture_dep'//new_line('a'))
    end subroutine write_dependency

    subroutine write_reproduction_case(case_id)
        character(len=*), intent(in) :: case_id
        character(:), allocatable :: source

        source = 'program '//trim(case_id)//new_line('a')// &
            'use fixture_dep, only: fixture_token'//new_line('a')// &
            'implicit none'//new_line('a')//'integer :: unit'//new_line('a')// &
            'character(len=64) :: token'//new_line('a')// &
            "open(newunit=unit,file='token.txt',status='old')"//new_line('a')// &
            "read(unit,'(a)') token"//new_line('a')//'close(unit)'//new_line('a')// &
            "print '(a)', trim(token)"//new_line('a')// &
            "print '(a)', trim(fixture_token)"//new_line('a')
        if (trim(case_id) == 'test_reproduce_first') then
            source = source//"print '(a)', 'FO_REPRODUCE_FIRST_OUTPUT_61c8c1'"// &
                new_line('a')//"print '(a)', '"//achar(34)//'quoted'// &
                achar(34)//achar(92)//"path'"//new_line('a')// &
                "print '(a)', repeat('q',4096)//'LONG_OUTPUT_END'"//new_line('a')
        else
            source = source// &
                "print '(a)', 'FO_REPRODUCE_SECOND_OUTPUT_9d09d2'"//new_line('a')
        end if
        source = source//'error stop 7'//new_line('a')// &
            'end program '//trim(case_id)//new_line('a')
        call write_text(project//'/test/'//trim(case_id)//'.f90', source)
    end subroutine write_reproduction_case

    subroutine assert_output_json(text)
        character(len=*), intent(in) :: text
        type(json_value_t) :: report, tests, entry
        character(:), allocatable :: message, output, report_line
        logical :: valid
        integer :: first, line_end

        first = index(text, '{"tests":[')
        call assert_true(first > 0, 'reproduction log preserves the runner JSON report')
        if (first == 0) return
        line_end = index(text(first:), new_line('a'))
        call assert_true(line_end > 0, 'runner JSON is a complete log record')
        if (line_end == 0) return
        report_line = text(first:first + line_end - 2)
        call json_parse(report_line, report, valid, message)
        call assert_true(valid, 'captured output remains valid JSON: '//message)
        if (.not. valid) return
        call assert_true(index(text(first + line_end:), 'fo: execution cwd: ') > 0, &
            'reproduction log also records the private execution view')
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

    subroutine wait_for_anchor(owner, active_generation, ready, failure)
        character(len=*), intent(in) :: owner
        character(:), allocatable, intent(out) :: active_generation
        logical, intent(out) :: ready
        character(:), allocatable, intent(out) :: failure
        type(json_value_t) :: status, list, receipt
        character(:), allocatable :: owner_state, diagnostic
        integer :: attempt, j

        active_generation = ''
        failure = ''
        ready = .false.
        do attempt = 1, 300
            call status_now(owner, status)
            active_generation = member_text(status, 'active_generation')
            owner_state = member_text(status, 'state')
            list = json_member(status, 'events')
            do j = 1, json_size(list)
                receipt = json_element(list, j)
                if (member_text(receipt, 'case_id') == 'test_reproduce_anchor' .and. &
                    member_text(receipt, 'status') == 'PASS' .and. &
                    member_text(receipt, 'generation') == active_generation) then
                    ready = .true.
                    return
                end if
            end do
            if (owner_state == 'build_failed' .or. owner_state == 'capture_failed' .or. &
                    owner_state == 'inventory_failed' .or. &
                    owner_state == 'test_launch_failed' .or. owner_state == 'error' .or. &
                    owner_state == 'stopped') then
                diagnostic = member_text(status, 'diagnostic')
                failure = 'state='//owner_state
                if (len_trim(diagnostic) > 0) then
                    failure = failure//'; diagnostic='//trim(diagnostic)
                else
                    diagnostic = member_text(status, 'last_outcome')
                    if (len_trim(diagnostic) > 0) &
                        failure = failure//'; last_outcome='//trim(diagnostic)
                end if
                return
            end if
            call gremlin_wait_ms(100)
        end do
        failure = 'no committed PASS receipt for test_reproduce_anchor within 30 seconds; '// &
            'last state='//owner_state
    end subroutine wait_for_anchor

    function start_failure(result) result(detail)
        type(process_result_t), intent(in) :: result
        character(:), allocatable :: detail

        detail = 'exit_code='//integer_text(result%exit_code)
        if (result%runner_failed) detail = detail//'; process runner failed'
        if (result%timed_out) detail = detail//'; command timed out'
        if (allocated(result%runner_error)) then
            if (len_trim(result%runner_error) > 0) &
                detail = detail//'; runner='//trim(result%runner_error)
        end if
        if (allocated(result%stderr)) then
            if (len_trim(result%stderr) > 0) detail = detail//'; stderr='//trim(result%stderr)
        end if
        if (allocated(result%stdout)) then
            if (len_trim(result%stdout) > 0) detail = detail//'; stdout='//trim(result%stdout)
        end if
    end function start_failure

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(32) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

    subroutine reproduce(case_id, log_path, cache_root)
        character(len=*), intent(in) :: case_id
        character(:), allocatable, intent(out) :: log_path
        character(len=*), intent(in), optional :: cache_root
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
        if (present(cache_root)) then
            call gremlin_run(driver, project, cache_root, state, args, result, &
                timeout=120000)
        else
            call gremlin_run(driver, project, cache, state, args, result, &
                timeout=120000)
        end if
        call assert_equal_integer(result%exit_code, 1, &
            case_id//' returns its deliberate failure')
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
