program test_gremlin_quiescence
    !! A fully verified lane with nothing to do stays asleep (#155, #139): its
    !! freshness barrier file is not rewritten while idle, a documentation edit
    !! does not wake it, and a relevant source edit wakes it and reruns the
    !! affected case in a new generation.
    use, intrinsic :: iso_c_binding, only: c_long_long
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, make_directory, assert_true
    use fo_test_harness, only: assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    use fo_test_json, only: json_string_value, json_size, json_element
    use fo_fs, only: fs_stat
    use fo_gremlin_state, only: gremlin_session_state_dir
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: first, second
    character(len=4096) :: state_dir, state_message
    character(len=*), parameter :: lane = 'quiescence'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer(c_long_long) :: mtime_first, mtime_now, bytes_first, bytes_now
    integer :: state_error, sample, events_before, events_after
    logical :: ok, stat_ok

    call gremlin_setup(driver, scratch, project, cache, state)
    call make_directory(project//'/src')
    call make_directory(project//'/test')
    call write_text(project//'/fpm.toml', 'name = "quiescence"'//nl)
    call write_module(1)
    call write_text(project//'/test/test_alpha.f90', &
        'program test_alpha'//nl//'use alpha, only: alpha_value'//nl// &
        'implicit none'//nl//'if (alpha_value < 1) error stop 1'//nl// &
        'end program test_alpha'//nl)
    call gremlin_start_args(args, project, lane, 'test_alpha')
    call gremlin_json(driver, project, cache, state, args, reply, process, 30000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, 'lane starts')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)

    call wait_quiescent('', first, ok)
    call assert_true(ok, 'lane reaches full verification and sleeps')
    if (.not. ok) call stop_and_finish()

    ! The status call above holds a freshness ticket: idle polling must not
    ! rewrite the barrier file.
    call gremlin_session_state_dir(project, lane, state_dir, state_error, &
        state_message)
    call assert_true(state_error == 0, 'resolves lane state directory')
    call fs_stat(trim(state_dir)//'/freshness-'//session, mtime_first, bytes_first, &
        stat_ok)
    call assert_true(stat_ok, 'a wait ticket created the freshness file')
    do sample = 1, 15
        call gremlin_wait_ms(100)
        call fs_stat(trim(state_dir)//'/freshness-'//session, mtime_now, bytes_now, &
            stat_ok)
        if (mtime_now /= mtime_first .or. bytes_now /= bytes_first) exit
    end do
    call assert_true(mtime_now == mtime_first .and. bytes_now == bytes_first, &
        'idle quiescent lane does not rewrite its freshness file')

    ! Documentation is exempt from impact: the lane stays asleep.
    events_before = event_count()
    call write_text(project//'/NOTES.md', 'documentation edit'//nl)
    call wait_quiescent(first, second, ok, expect_same=.true.)
    call assert_true(ok, 'a documentation edit leaves the lane quiescent')
    events_after = event_count()
    call assert_true(events_after == events_before, &
        'a documentation edit starts no case')

    call write_module(2)
    call wait_quiescent(first, second, ok)
    call assert_true(ok, 'a source edit wakes the lane and it verifies again')
    if (ok) call assert_true(count_receipts(second, 'test_alpha') >= 1, &
        'the affected case runs in the new generation')
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine stop_and_finish()
        call gremlin_stop_lane(driver, project, cache, state, lane, session, &
            allow_terminal_error=.true.)
        call finish_assertions(retain_failed_scratch=.true.)
    end subroutine stop_and_finish

    subroutine write_module(version)
        integer, intent(in) :: version
        character(len=8) :: number

        write (number, '(i0)') version
        call write_text(project//'/src/alpha.f90', 'module alpha'//nl// &
            'implicit none'//nl//'integer, parameter :: alpha_value = '// &
            trim(number)//nl//'end module alpha'//nl)
    end subroutine write_module

    subroutine status_view(view, with_events)
        type(json_value_t), intent(out) :: view
        logical, intent(in), optional :: with_events
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
        if (present(with_events)) then
            call list_add(query, '--detail')
            call list_add(query, 'full')
            call list_add(query, '--cursor')
            call list_add(query, '0')
            call list_add(query, '--max-records')
            call list_add(query, '128')
        end if
        call gremlin_json(driver, project, cache, state, query, view, observed, 10000)
    end subroutine status_view

    subroutine wait_quiescent(previous, generation, found, expect_same)
        !! Wait for a fully verified quiescent lane. Default: a generation
        !! other than previous. expect_same: the one generation `previous`,
        !! confirmed by a status handshake after the edit.
        character(len=*), intent(in) :: previous
        character(:), allocatable, intent(out) :: generation
        logical, intent(out) :: found
        logical, intent(in), optional :: expect_same
        type(json_value_t) :: view
        integer :: poll
        logical :: same

        same = .false.
        if (present(expect_same)) same = expect_same
        found = .false.
        generation = ''
        do poll = 1, 500
            call status_view(view)
            generation = gremlin_field(view, 'active_generation')
            if (gremlin_field(view, 'state') == 'error') return
            if (len(generation) == 64 .and. (generation /= previous .or. same) .and. &
                    gremlin_field(view, 'phase') == 'quiescent' .and. &
                    json_boolean_value(json_member(view, 'fully_verified'))) then
                found = .true.
                if (.not. same) return
                if (poll >= 3) return
            end if
            if (same .and. generation /= previous) then
                found = .false.
                return
            end if
            call gremlin_wait_ms(100)
        end do
    end subroutine wait_quiescent

    integer function event_count()
        type(json_value_t) :: view

        call status_view(view, .true.)
        event_count = json_size(json_member(view, 'events'))
    end function event_count

    integer function count_receipts(generation, case_id)
        character(len=*), intent(in) :: generation, case_id
        type(json_value_t) :: view, events, receipt
        integer :: k

        count_receipts = 0
        call status_view(view, .true.)
        events = json_member(view, 'events')
        do k = 1, json_size(events)
            receipt = json_element(events, k)
            if (json_string_value(json_member(receipt, 'generation')) /= generation) &
                cycle
            if (json_string_value(json_member(receipt, 'case_id')) /= case_id) cycle
            if (json_string_value(json_member(receipt, 'status')) /= 'PASS') cycle
            count_receipts = count_receipts + 1
        end do
    end function count_receipts

end program test_gremlin_quiescence
