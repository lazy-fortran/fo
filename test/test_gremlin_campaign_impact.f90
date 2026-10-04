program test_gremlin_campaign_impact
    use, intrinsic :: iso_c_binding, only: c_int
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, write_text, read_text
    use fo_test_harness, only: assert_true, assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_number_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: marker, sentinel, lane, session, baseline, candidate
    character(:), allocatable :: marker_text
    character(len=32), parameter :: regression_case = 'test_regression'
    character(len=32), parameter :: affected_case = 'test_affected'
    character(len=32), parameter :: slow_affected_case = 'test_slow_affected'
    character(len=32), parameter :: background_case = 'test_background'
    type(process_result_t) :: process
    type(json_value_t) :: response, events, event
    integer :: attempt, baseline_count, candidate_count
    integer :: regression_order, affected_order, regression_seed, affected_seed
    logical :: found

    interface
        integer(c_int) function test_sleep(microseconds) &
                bind(C, name='usleep')
            import :: c_int
            integer(c_int), value :: microseconds
        end function test_sleep
    end interface

    call gremlin_setup(driver, scratch, project, cache, state)
    call make_directory(project//'/src')
    call make_directory(project//'/test')
    marker = scratch//'/case-markers.log'
    sentinel = scratch//'/regression-seen'
    lane = 'campaign-impact-oracle'
    call write_text(project//'/fpm.toml', &
        'name = "gremlin_campaign_impact_probe"'//new_line('a'))
    call write_shared_value(1)
    call write_probe_cases()
    call start_campaign(session)

    baseline = ''
    baseline_count = 0
    found = .false.
    do attempt = 1, 1200
        call read_events(response)
        events = json_member(response, 'events')
        call count_generation_receipts(events, '', baseline, baseline_count, found)
        if (found .and. baseline_count >= 4) exit
        call gremlin_wait_ms(50)
    end do
    call assert_true(found .and. baseline_count == 4, &
        'the baseline generation executes each public fixture case')
    if (.not. found .or. baseline_count < 4) then
        call gremlin_stop_lane(driver, project, cache, state, lane, session)
        call finish_assertions()
    end if
    events = json_member(response, 'events')
    call assert_receipt(events, baseline, regression_case, 'FAIL', &
        'the initial regression is observed as a real failing execution')

    call write_shared_value(2)
    candidate = ''
    candidate_count = 0
    found = .false.
    do attempt = 1, 1200
        call read_events(response)
        events = json_member(response, 'events')
        call count_generation_receipts(events, baseline, candidate, &
            candidate_count, found)
        if (found .and. candidate_count >= 3) exit
        call gremlin_wait_ms(50)
    end do
    call assert_true(found .and. candidate_count == 4, &
        'the candidate executes its pending failure, affected case, and finite background case')
    if (.not. found .or. candidate_count < 4) then
        call gremlin_stop_lane(driver, project, cache, state, lane, session)
        call finish_assertions()
    end if

    events = json_member(response, 'events')
    call assert_receipt(events, candidate, regression_case, 'PASS', &
        'the old failure is rerun and passes in the candidate generation')
    call assert_receipt(events, candidate, affected_case, 'PASS', &
        'the test depending on the changed module executes and passes')
    call assert_receipt(events, candidate, slow_affected_case, 'PASS', &
        'the slow affected test remains a required campaign case')
    call assert_receipt(events, candidate, background_case, 'PASS', &
        'the existing finite background queue still executes a case')
    regression_order = receipt_index(events, candidate, regression_case)
    affected_order = receipt_index(events, candidate, affected_case)
    call assert_true(regression_order > 0 .and. affected_order > regression_order, &
        'the unresolved failure is reproduced before the affected case')
    regression_seed = receipt_seed(events, candidate, regression_case)
    affected_seed = receipt_seed(events, candidate, affected_case)
    call assert_true(regression_seed /= 0 .and. affected_seed /= 0 .and. &
        regression_seed /= affected_seed, &
        'required cases continue across public campaign chunks')

    marker_text = read_text(marker)
    call assert_true(index(marker_text, regression_case) > 0 .and. &
        index(marker_text, affected_case) > 0 .and. &
        index(marker_text, slow_affected_case) > 0 .and. &
        index(marker_text, background_case) > 0, &
        'case markers confirm that executables ran and emitted output')
    call gremlin_stop_lane(driver, project, cache, state, lane, session)
    call finish_assertions()

contains

    subroutine start_campaign(owner)
        character(:), allocatable, intent(out) :: owner
        type(string_list_t) :: arguments

        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'start')
        call list_add(arguments, '--dir')
        call list_add(arguments, project)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--target')
        call list_add(arguments, regression_case)
        call list_add(arguments, '--random-count')
        call list_add(arguments, '1')
        call list_add(arguments, '--seed')
        call list_add(arguments, '1729')
        call list_add(arguments, '--campaign-seconds')
        call list_add(arguments, '1')
        call list_add(arguments, '--timeout-seconds')
        call list_add(arguments, '10')
        call list_add(arguments, '--json')
        call gremlin_json(driver, project, cache, state, arguments, response, &
            process, 30000)
        call assert_true(process%exit_code == 0, 'starts the public campaign')
        owner = gremlin_field(response, 'session_id')
        call assert_true(len(owner) > 0, 'campaign returns its session ID')
    end subroutine start_campaign

    subroutine read_events(document)
        type(json_value_t), intent(out) :: document
        type(string_list_t) :: arguments
        type(process_result_t) :: result

        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'events')
        call list_add(arguments, '--dir')
        call list_add(arguments, project)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--session')
        call list_add(arguments, session)
        call list_add(arguments, '--cursor')
        call list_add(arguments, '0')
        call list_add(arguments, '--max-records')
        call list_add(arguments, '128')
        call list_add(arguments, '--max-bytes')
        call list_add(arguments, '262144')
        call list_add(arguments, '--json')
        call gremlin_json(driver, project, cache, state, arguments, document, &
            result, 30000)
        call assert_true(result%exit_code == 0, 'reads public campaign receipts')
    end subroutine read_events

    subroutine count_generation_receipts(records, excluded_generation, &
            generation, count, generation_found)
        type(json_value_t), intent(in) :: records
        character(len=*), intent(in) :: excluded_generation
        character(:), allocatable, intent(inout) :: generation
        integer, intent(out) :: count
        logical, intent(out) :: generation_found
        character(:), allocatable :: event_generation, case_id
        integer :: i

        count = 0
        generation_found = .false.
        if (len(generation) == 0) then
            do i = 1, json_size(records)
                event = json_element(records, i)
                case_id = gremlin_field(event, 'case_id')
                if (.not. is_fixture_case(case_id)) cycle
                event_generation = gremlin_field(event, 'generation')
                if (len(event_generation) /= 64 .or. &
                        event_generation == excluded_generation) cycle
                generation = event_generation
                exit
            end do
        end if
        if (len(generation) /= 64 .or. generation == excluded_generation) return
        generation_found = .true.
        do i = 1, json_size(records)
            event = json_element(records, i)
            event_generation = gremlin_field(event, 'generation')
            case_id = gremlin_field(event, 'case_id')
            if (event_generation /= generation .or. .not. is_fixture_case(case_id)) cycle
            if (gremlin_field(event, 'status') == 'PASS' .or. &
                    gremlin_field(event, 'status') == 'FAIL') count = count + 1
        end do
    end subroutine count_generation_receipts

    subroutine assert_receipt(records, generation, case_id, expected_status, message)
        type(json_value_t), intent(in) :: records
        character(len=*), intent(in) :: generation, case_id, expected_status, message
        integer :: i

        do i = 1, json_size(records)
            event = json_element(records, i)
            if (gremlin_field(event, 'generation') /= generation) cycle
            if (gremlin_field(event, 'case_id') /= case_id) cycle
            if (gremlin_field(event, 'status') == expected_status) then
                call assert_true(.true., message)
                return
            end if
        end do
        call assert_true(.false., message)
    end subroutine assert_receipt

    integer function receipt_index(records, generation, case_id)
        type(json_value_t), intent(in) :: records
        character(len=*), intent(in) :: generation, case_id
        integer :: i

        receipt_index = 0
        do i = 1, json_size(records)
            event = json_element(records, i)
            if (gremlin_field(event, 'generation') /= generation) cycle
            if (gremlin_field(event, 'case_id') /= case_id) cycle
            if (gremlin_field(event, 'status') /= 'PASS' .and. &
                    gremlin_field(event, 'status') /= 'FAIL') cycle
            receipt_index = i
            return
        end do
    end function receipt_index

    integer function receipt_seed(records, generation, case_id)
        type(json_value_t), intent(in) :: records
        character(len=*), intent(in) :: generation, case_id
        type(json_value_t) :: seed_value
        integer :: i

        receipt_seed = 0
        do i = 1, json_size(records)
            event = json_element(records, i)
            if (gremlin_field(event, 'generation') /= generation) cycle
            if (gremlin_field(event, 'case_id') /= case_id) cycle
            seed_value = json_member(event, 'seed')
            receipt_seed = int(json_number_value(seed_value))
            return
        end do
    end function receipt_seed

    logical function is_fixture_case(case_id)
        character(len=*), intent(in) :: case_id

        is_fixture_case = case_id == regression_case .or. &
            case_id == affected_case .or. case_id == slow_affected_case .or. &
            case_id == background_case
    end function is_fixture_case

    subroutine write_shared_value(value)
        integer, intent(in) :: value
        character(:), allocatable :: source

        source = 'module shared_mod'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, parameter :: shared_value = '//integer_text(value)// &
            new_line('a')//'end module shared_mod'//new_line('a')
        call write_text(project//'/src/shared_mod.f90', source)
    end subroutine write_shared_value

    subroutine write_probe_cases()
        character(:), allocatable :: regression, affected, slow_affected, background
        character(:), allocatable :: regression_body, affected_body, slow_body
        character(:), allocatable :: background_body

        regression_body = 'integer :: unit, rc, ios'//new_line('a')// &
            'logical :: exists'//new_line('a')// &
            'open(newunit=unit,file='//fortran_quote(marker)// &
            ',status="unknown",position="append")'//new_line('a')// &
            'write(unit,"(a)") "'//regression_case//'"'//new_line('a')// &
            'close(unit)'//new_line('a')// &
            'rc = test_sleep(1500000_c_int)'//new_line('a')// &
            'inquire(file='//fortran_quote(sentinel)//',exist=exists)'// &
            new_line('a')//'if (.not. exists) then'//new_line('a')// &
            '  open(newunit=unit,file='//fortran_quote(sentinel)// &
            ',status="new",iostat=ios)'//new_line('a')// &
            '  if (ios == 0) then'//new_line('a')// &
            '    close(unit)'//new_line('a')//'    error stop 9'//new_line('a')// &
            '  end if'//new_line('a')//'end if'
        affected_body = 'integer :: unit, rc'//new_line('a')// &
            'open(newunit=unit,file='//fortran_quote(marker)// &
            ',status="unknown",position="append")'//new_line('a')// &
            'write(unit,"(a)") "'//affected_case//'"'//new_line('a')// &
            'close(unit)'//new_line('a')// &
            'rc = test_sleep(1500000_c_int)'//new_line('a')// &
            'if (shared_value < 1) error stop 7'
        slow_body = 'integer :: unit, rc'//new_line('a')// &
            'open(newunit=unit,file='//fortran_quote(marker)// &
            ',status="unknown",position="append")'//new_line('a')// &
            'write(unit,"(a)") "'//slow_affected_case//'"'//new_line('a')// &
            'close(unit)'//new_line('a')// &
            'rc = test_sleep(1500000_c_int)'//new_line('a')// &
            'if (shared_value < 1) error stop 8'
        background_body = 'integer :: unit, rc'//new_line('a')// &
            'open(newunit=unit,file='//fortran_quote(marker)// &
            ',status="unknown",position="append")'//new_line('a')// &
            'write(unit,"(a)") "'//background_case//'"'//new_line('a')// &
            'close(unit)'//new_line('a')// &
            'rc = test_sleep(1500000_c_int)'
        regression = test_source(regression_case, regression_body)
        affected = test_source(affected_case, affected_body, &
            'use shared_mod, only: shared_value'//new_line('a'))
        slow_affected = test_source(slow_affected_case, slow_body, &
            'use shared_mod, only: shared_value'//new_line('a'))
        background = test_source(background_case, background_body)
        call write_text(project//'/test/'//regression_case//'.f90', regression)
        call write_text(project//'/test/'//affected_case//'.f90', affected)
        call write_text(project//'/test/'//slow_affected_case//'.f90', slow_affected)
        call write_text(project//'/test/'//background_case//'.f90', background)
    end subroutine write_probe_cases

    function test_source(name, body, extra_use) result(source)
        character(len=*), intent(in) :: name, body
        character(len=*), intent(in), optional :: extra_use
        character(:), allocatable :: source, prelude

        prelude = ''
        if (present(extra_use)) prelude = trim(extra_use)//new_line('a')
        prelude = prelude//'use, intrinsic :: iso_c_binding, only: c_int'// &
            new_line('a')
        source = 'program '//trim(name)//new_line('a')//prelude// &
            'implicit none'//new_line('a')//'interface'//new_line('a')// &
            '  integer(c_int) function test_sleep(microseconds) '// &
            'bind(C,name="usleep")'//new_line('a')// &
            '    import :: c_int'//new_line('a')// &
            '    integer(c_int), value :: microseconds'//new_line('a')// &
            '  end function test_sleep'//new_line('a')//'end interface'// &
            new_line('a')//trim(body)//new_line('a')// &
            'end program '//trim(name)//new_line('a')
    end function test_source

    function fortran_quote(value) result(text)
        character(len=*), intent(in) :: value
        character(:), allocatable :: text

        text = "'"
        block
            integer :: i
            do i = 1, len_trim(value)
                text = text//value(i:i)
                if (value(i:i) == "'") text = text//"'"
            end do
        end block
        text = text//"'"
    end function fortran_quote

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(len=16) :: text

        write(text, '(i0)') value
    end function integer_text

end program test_gremlin_campaign_impact
