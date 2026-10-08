program test_gremlin_discovery_diagnostic
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, remove_tree
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_gremlin_state, only: gremlin_session_state_dir
    use fo_test_json, only: json_value_t
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: session, lane, diagnostic, launcher_log
    character(len=4096) :: session_state, state_message
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: started, status
    integer :: attempt, state_error
    logical :: terminal

    call gremlin_setup(driver, scratch, project, cache, state)
    lane = 'discovery-diagnostic-oracle'
    call write_text(project//'/fpm.toml', 'name = "discovery_diagnostic_probe"'// &
        new_line('a'))
    call write_valid_test()

    call gremlin_start_args(args, project, lane, 'test_missing_from_inventory')
    call gremlin_json(driver, project, cache, state, args, started, process, 30000)
    session = gremlin_field(started, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts a campaign with a requested case absent from the compiled inventory')
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
    terminal = .false.
    diagnostic = ''
    do attempt = 1, 1200
        call gremlin_json(driver, project, cache, state, args, status, process, 10000)
        diagnostic = gremlin_field(status, 'diagnostic')
        if (gremlin_field(status, 'state') == 'error' .or. &
                gremlin_field(status, 'state') == 'inventory_failed') then
            terminal = .true.
            exit
        end if
        call gremlin_wait_ms(50)
    end do

    call assert_true(terminal, 'invalid inventory priority reaches discovery failure')
    call gremlin_stop_lane(driver, project, cache, state, lane, session, .true.)
    call assert_true(index(diagnostic, &
        'requested or prioritized case is outside the eligible inventory') > 0, &
        'terminal status preserves the concrete discover_campaign diagnostic')
    call gremlin_session_state_dir(project, lane, session_state, &
        state_error, state_message)
    launcher_log = trim(session_state)//'/logs/launcher.log'
    if (terminal .and. state_error == 0) then
        call gremlin_wait_ms(100)
        call assert_true(index(read_text(launcher_log), &
            'requested or prioritized case is outside the eligible inventory') > 0, &
            'owner launcher error preserves the concrete discovery diagnostic')
    else
        call assert_true(.false., 'resolves failed campaign launcher log')
    end if
    if (.not. terminal .or. len(diagnostic) == 0 .or. state_error /= 0) &
        call finish_assertions(retain_failed_scratch=.true.)
    call remove_tree(scratch)
    print '(a)', 'Gremlin discovery diagnostic oracle PASS'

contains

    subroutine write_valid_test()
        call write_text(project//'/test/test_present.f90', &
            'program test_present'//new_line('a')//'implicit none'//new_line('a')// &
            "print '(a)', 'inventory fixture built'"//new_line('a')// &
            'end program test_present'//new_line('a'))
    end subroutine write_valid_test

end program test_gremlin_discovery_diagnostic
