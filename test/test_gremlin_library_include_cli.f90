program test_gremlin_library_include_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, assert_true
    use fo_test_harness, only: assert_contains, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: session, generation, changed, log_path
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: response, document
    character(len=1), parameter :: nl = new_line('a')
    character(len=*), parameter :: lane = 'declared-library-headers'
    logical :: ready

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/fpm.toml', &
        'name = "header_probe"'//nl//'[build]'//nl//'auto-tests = false'//nl// &
        '[library]'//nl//'include-dir = ["headers"]'//nl// &
        '[dependencies]'//nl//'provider = { path = "../provider" }'//nl// &
        '[dev-dependencies]'//nl// &
        'test_provider = { path = "../test_provider" }'//nl// &
        '[[test]]'//nl//'name = "test_header_payload"'//nl// &
        'source-dir = "checks"'//nl//'main = "main.f90"'//nl)
    call write_package(project, 'root_value', '13')
    call write_package(scratch//'/provider', 'provider_value', '211')
    call write_package(scratch//'/test_provider', 'test_value', '31')
    call write_text(scratch//'/provider/fpm.toml', &
        'name = "provider"'//nl//'[library]'//nl// &
        'include-dir = "headers"'//nl)
    call write_text(scratch//'/test_provider/fpm.toml', &
        'name = "test_provider"'//nl//'[library]'//nl// &
        'include-dir = ["headers"]'//nl)
    call write_text(project//'/checks/main.f90', &
        'program test_header_payload'//nl//'use iso_c_binding, only: c_int'//nl// &
        'implicit none'//nl//'integer :: total'//nl//'interface'//nl// &
        c_interface('root_value')//c_interface('provider_value')// &
        c_interface('test_value')//'end interface'//nl// &
        'total = root_value()+provider_value()+test_value()'//nl// &
        "print '(a,i0)', 'HEADER_PAYLOAD=', total"//nl// &
        'if (total /= 255) error stop 1'//nl// &
        'end program test_header_payload'//nl)
    call gremlin_start_args(args, project, lane, 'test_header_payload')
    call gremlin_json(driver, project, cache, state, args, response, process, 30000)
    call assert_true(process%exit_code == 0, 'starts declared header resident lane')
    session = gremlin_field(response, 'session_id')
    call wait_case('', 'PASS', generation, log_path, ready)
    if (ready) then
        call assert_contains(read_text(log_path), 'HEADER_PAYLOAD=255'//nl, &
            'resident runtime consumes captured project, dependency and dev headers')
        call write_text(project//'/headers/value.h', '#define VALUE 17'//nl)
        call wait_case(generation, 'FAIL', changed, log_path, ready)
        if (ready) call assert_contains(read_text(log_path), &
            'HEADER_PAYLOAD=259'//nl, 'declared root header edit changes warm runtime')
        call gremlin_stop_lane(driver, project, cache, state, lane, session, &
            allow_terminal_error=.true.)
        call frozen_replay(generation)
    else
        call gremlin_stop_lane(driver, project, cache, state, lane, session, &
            allow_terminal_error=.true.)
    end if
    call finish_assertions(retain_failed_scratch=.true.)

contains

    function c_interface(name) result(source)
        character(len=*), intent(in) :: name
        character(:), allocatable :: source
        source = 'function '//name//'() bind(c) result(value)'//nl// &
            'import c_int'//nl//'integer(c_int) :: value'//nl// &
            'end function'//nl
    end function c_interface

    subroutine write_package(directory, name, value)
        character(len=*), intent(in) :: directory, name, value
        call write_text(directory//'/headers/value.h', '#define VALUE '//value//nl)
        call write_text(directory//'/src/'//name//'.c', &
            '#include "value.h"'//nl// &
            'int '//name//'(void) { return VALUE; }'//nl)
    end subroutine write_package

    subroutine read_status()
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--detail')
        call list_add(args, 'full')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, session)
        call gremlin_json(driver, project, cache, state, args, document, process, 30000)
    end subroutine read_status

    subroutine wait_case(previous, wanted, current, receipt_log, found)
        character(len=*), intent(in) :: previous, wanted
        character(:), allocatable, intent(out) :: current, receipt_log
        logical, intent(out) :: found
        type(json_value_t) :: events, event
        integer :: attempt, i

        found = .false.
        current = ''
        receipt_log = ''
        do attempt = 1, 600
            call read_status()
            events = json_member(document, 'events')
            do i = 1, json_size(events)
                event = json_element(events, i)
                if (gremlin_field(event, 'case_id') /= 'test_header_payload') cycle
                if (gremlin_field(event, 'status') /= wanted) cycle
                current = gremlin_field(event, 'generation')
                if (current == previous) cycle
                receipt_log = gremlin_field(event, 'log_path')
                if (len(receipt_log) == 0) cycle
                found = .true.
                return
            end do
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'resident declared-header '//wanted// &
            ' receipt arrives; last state='//gremlin_field(document, 'state'))
    end subroutine wait_case

    subroutine frozen_replay(frozen)
        character(len=*), intent(in) :: frozen
        type(json_value_t) :: events, event
        character(:), allocatable :: receipt_log
        integer :: i

        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'reproduce')
        call list_add(args, 'test_header_payload')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, session)
        call list_add(args, '--generation')
        call list_add(args, frozen)
        call gremlin_json(driver, project, cache, state, args, response, process, 120000)
        call assert_true(process%exit_code == 0, 'frozen header generation replays')
        call assert_true(gremlin_field(response, 'state') == 'PASS', &
            'frozen replay excludes edited live declared header')
        call read_status()
        events = json_member(document, 'events')
        receipt_log = ''
        do i = 1, json_size(events)
            event = json_element(events, i)
            if (gremlin_field(event, 'generation') /= frozen) cycle
            if (gremlin_field(event, 'case_id') /= 'test_header_payload') cycle
            if (index(gremlin_field(event, 'log_path'), '-reproduce-') == 0) cycle
            receipt_log = gremlin_field(event, 'log_path')
        end do
        call assert_true(len(receipt_log) > 0, 'frozen replay exposes its own log')
        if (len(receipt_log) > 0) call assert_contains(read_text(receipt_log), &
            'HEADER_PAYLOAD=255'//nl, 'frozen replay executes original header bytes')
    end subroutine frozen_replay

end program test_gremlin_library_include_cli
