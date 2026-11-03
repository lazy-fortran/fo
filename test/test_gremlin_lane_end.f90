program test_gremlin_lane_end
    !! A lane stops cleanly when its project directory is deleted, its state
    !! staying reachable by the deleted path for status and stop, and when its
    !! owner receives SIGTERM.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, make_directory, remove_tree, assert_true
    use fo_test_harness, only: finish_assertions, run_process
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_json, only: json_value_t
    use fo_fs, only: fs_is_windows
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: lane
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply

    call gremlin_setup(driver, scratch, project, cache, state)

    lane = 'lost-project'
    call start_settled_lane()
    call remove_tree(project)
    call assert_true(stops(), 'the lane of a deleted project stops cleanly')
    call command_view('stop', reply, process)
    call assert_true(process%exit_code == 0 .and. &
        gremlin_field(reply, 'state') == 'stopped', &
        'stop finds the lane of the deleted project')
    if (fs_is_windows()) call finish_assertions(retain_failed_scratch=.true.)

    project = scratch//'/terminated'
    lane = 'terminated'
    call start_settled_lane()
    args = string_list_t()
    call list_add(args, 'kill')
    call list_add(args, '-TERM')
    call list_add(args, session(:index(session, '-') - 1))
    call run_process(args, scratch, process)
    call assert_true(process%exit_code == 0, 'signals the lane owner')
    call assert_true(stops(), 'SIGTERM stops the lane cleanly')
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine start_settled_lane()
        integer :: poll
        logical :: settled

        call make_directory(project)
        call make_directory(project//'/test')
        call write_text(project//'/fpm.toml', 'name = "lane-end"'//nl)
        call write_text(project//'/test/test_alpha.f90', &
            'program test_alpha'//nl//'implicit none'//nl// &
            'if (1 < 0) error stop 1'//nl//'end program test_alpha'//nl)
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'start')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--random')
        call list_add(args, '1')
        call list_add(args, '--campaign-seconds')
        call list_add(args, '60')
        call gremlin_json(driver, project, cache, state, args, reply, process, 30000)
        session = gremlin_field(reply, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(session) > 0, &
            'lane '//lane//' starts')
        if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)
        settled = .false.
        do poll = 1, 300
            call command_view('status', reply)
            select case (gremlin_field(reply, 'state'))
            case ('quiescent', 'idle')
                settled = .true.
                exit
            end select
            call gremlin_wait_ms(100)
        end do
        call assert_true(settled, 'lane '//lane//' settles after its first campaign')
    end subroutine start_settled_lane

    logical function stops()
        integer :: poll

        stops = .false.
        do poll = 1, 300
            call command_view('status', reply)
            stops = gremlin_field(reply, 'state') == 'stopped'
            if (stops) return
            call gremlin_wait_ms(100)
        end do
    end function stops

    subroutine command_view(action, view, observed)
        character(len=*), intent(in) :: action
        type(json_value_t), intent(out) :: view
        type(process_result_t), intent(out), optional :: observed
        type(string_list_t) :: query
        type(process_result_t) :: result

        call list_add(query, 'gremlin')
        call list_add(query, action)
        call list_add(query, '--dir')
        call list_add(query, project)
        call list_add(query, '--lane')
        call list_add(query, lane)
        call list_add(query, '--session')
        call list_add(query, session)
        call gremlin_json(driver, scratch, cache, state, query, view, result, 10000)
        if (present(observed)) observed = result
    end subroutine command_view

end program test_gremlin_lane_end
