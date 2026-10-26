program test_gremlin_zero_selection
    !! A lane that can select no case sleeps with its reason (#271): with
    !! --random 0, no --target and no changed-file impact it ends quiescent
    !! and names the cause instead of reporting a failure with no message.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, make_directory, assert_true
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_json, only: json_value_t
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: phase, diagnostic
    character(len=*), parameter :: lane = 'zero-selection'
    character(len=*), parameter :: cause = 'no cases selected'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer :: poll
    logical :: settled

    call gremlin_setup(driver, scratch, project, cache, state)
    call make_directory(project//'/test')
    call write_text(project//'/fpm.toml', 'name = "zero-selection"'//nl)
    call write_text(project//'/test/test_alpha.f90', &
        'program test_alpha'//nl//'implicit none'//nl//'if (1 < 0) error stop 1'//nl// &
        'end program test_alpha'//nl)

    call list_add(args, 'gremlin')
    call list_add(args, 'start')
    call list_add(args, '--dir')
    call list_add(args, project)
    call list_add(args, '--lane')
    call list_add(args, lane)
    call list_add(args, '--random')
    call list_add(args, '0')
    call list_add(args, '--campaign-seconds')
    call list_add(args, '60')
    call list_add(args, '--timeout-seconds')
    call list_add(args, '10')
    call gremlin_json(driver, project, cache, state, args, reply, process, 30000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, 'lane starts')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)

    ! Settled: the first generation built and its campaign selection finished,
    ! so the lane either sleeps or reports why it cannot proceed.
    settled = .false.
    do poll = 1, 300
        call status_view(reply)
        select case (gremlin_field(reply, 'state'))
        case ('quiescent', 'idle', 'error', 'stopped')
            settled = .true.
            exit
        end select
        call gremlin_wait_ms(100)
    end do
    call assert_true(settled, 'a lane with nothing to run settles after its build')
    if (.not. settled) call stop_and_finish()

    phase = gremlin_field(reply, 'phase')
    diagnostic = gremlin_field(reply, 'diagnostic')
    call assert_true(phase /= 'failed', 'a zero-case lane is not reported failed')
    call assert_true(index(diagnostic, cause) > 0, &
        'the status names the cause of the empty selection')

    call stop_and_finish()

contains

    subroutine status_view(view)
        type(json_value_t), intent(out) :: view
        type(string_list_t) :: query
        type(process_result_t) :: observed

        call list_add(query, 'gremlin')
        call list_add(query, 'status')
        call list_add(query, '--dir')
        call list_add(query, project)
        call list_add(query, '--lane')
        call list_add(query, lane)
        call list_add(query, '--session')
        call list_add(query, session)
        call gremlin_json(driver, project, cache, state, query, view, observed, 10000)
    end subroutine status_view

    subroutine stop_and_finish()
        call gremlin_stop_lane(driver, project, cache, state, lane, session, &
            allow_terminal_error=.true.)
        call finish_assertions(retain_failed_scratch=.true.)
    end subroutine stop_and_finish

end program test_gremlin_zero_selection
