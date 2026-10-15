program test_gremlin_case_reuse_retention
    !! PASS reuse does not depend on the campaign journal tail (#140): a CMake
    !! project with more cases than the journal retains reuses every unchanged
    !! case after an edit that only affects another target.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, make_directory, assert_true
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_run
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    use fo_test_json, only: json_string_value, json_size, json_element
    use fo_test_json, only: json_number_value
    use fo_test_json, only: json_parse
    implicit none

    integer, parameter :: n_cases = 40
    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: first, second
    character(len=*), parameter :: lane = 'reuse-retention'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(string_list_t) :: environment
    character(:), allocatable :: parse_message
    logical :: valid
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer :: reused, ran
    logical :: ok

    call gremlin_setup(driver, scratch, project, cache, state)
    call make_directory(project//'/test')
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(retention LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'add_executable(probe test/probe.f90)'//nl// &
        'add_executable(other test/other.f90)'//nl// &
        'foreach(i RANGE 1 '//number(n_cases)//')'//nl// &
        '  add_test(NAME case_${i} COMMAND probe)'//nl//'endforeach()'//nl// &
        'add_test(NAME test_other COMMAND other)'//nl)
    call write_text(project//'/test/probe.f90', 'program probe'//nl// &
        'end program probe'//nl)
    call write_other(1)
    call gremlin_start_args(args, project, lane, 'test_other')
    ! The lane owner keeps only 16 receipts: far fewer than the cases.
    call list_add(environment, 'FO_GREMLIN_RECEIPT_LIMIT=16')
    call gremlin_run(driver, project, cache, state, args, process, environment, 60000)
    call json_parse(process%stdout, reply, valid, parse_message)
    call assert_true(valid, 'start reply is JSON')
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, 'lane starts')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)

    call wait_verified('', first, ok)
    call assert_true(ok, 'first generation verifies every case')
    if (ok) then
        call write_other(2)
        call wait_verified(first, second, ok)
        call assert_true(ok, 'edited generation verifies again')
    end if
    if (ok) then
        call count_receipts(second, reused, ran)
        call assert_true(reused >= n_cases, &
            'every unchanged case beyond the journal tail is reused')
        call assert_true(ran >= 1, 'the edited target runs again')
    end if
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    function number(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=16) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function number

    subroutine write_other(version)
        integer, intent(in) :: version

        call write_text(project//'/test/other.f90', 'program other'//nl// &
            'implicit none'//nl//'integer, parameter :: version = '// &
            number(version)//nl//'if (version < 1) error stop 1'//nl// &
            'end program other'//nl)
    end subroutine write_other

    subroutine status_view(view, cursor)
        type(json_value_t), intent(out) :: view
        character(len=*), intent(in), optional :: cursor
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
        if (present(cursor)) then
            call list_add(query, '--detail')
            call list_add(query, 'full')
            call list_add(query, '--cursor')
            call list_add(query, cursor)
            call list_add(query, '--max-records')
            call list_add(query, '128')
        end if
        call gremlin_json(driver, project, cache, state, query, view, observed, 20000)
    end subroutine status_view

    subroutine wait_verified(previous, generation, found)
        character(len=*), intent(in) :: previous
        character(:), allocatable, intent(out) :: generation
        logical, intent(out) :: found
        type(json_value_t) :: view
        integer :: poll

        found = .false.
        generation = ''
        do poll = 1, 6000
            call status_view(view)
            if (gremlin_field(view, 'state') == 'error') return
            generation = gremlin_field(view, 'active_generation')
            if (len(generation) == 64 .and. generation /= previous .and. &
                    gremlin_field(view, 'phase') == 'quiescent' .and. &
                    json_boolean_value(json_member(view, 'fully_verified'))) then
                found = .true.
                return
            end if
            call gremlin_wait_ms(200)
        end do
    end subroutine wait_verified

    subroutine count_receipts(generation, reused, ran)
        character(len=*), intent(in) :: generation
        integer, intent(out) :: reused, ran
        type(json_value_t) :: view, events, receipt
        character(len=32) :: cursor
        character(:), allocatable :: disposition
        integer :: k, page

        reused = 0
        ran = 0
        cursor = '0'
        do page = 1, 1000
            call status_view(view, trim(cursor))
            events = json_member(view, 'events')
            do k = 1, json_size(events)
                receipt = json_element(events, k)
                if (json_string_value(json_member(receipt, 'generation')) /= &
                    generation) cycle
                if (json_string_value(json_member(receipt, 'status')) /= 'PASS') cycle
                disposition = json_string_value(json_member(receipt, 'disposition'))
                if (disposition == 'reused') reused = reused + 1
                if (disposition == 'run') ran = ran + 1
            end do
            if (.not. json_boolean_value(json_member(view, 'has_more'))) exit
            write (cursor, '(i0)') int(json_number_value(json_member(view, &
                'next_cursor')), int64)
        end do
    end subroutine count_receipts

end program test_gremlin_case_reuse_retention
