program test_gremlin_library_include_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text, assert_true
    use fo_test_harness, only: assert_equal_integer, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_parse, json_string_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: session, generation, changed, log_path
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: response, document
    character(len=1), parameter :: nl = new_line('a')
    character(:), allocatable :: lane
    logical :: ready

    call run_header_case(.false.)
    call run_header_case(.true.)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine run_header_case(absolute)
        logical, intent(in) :: absolute
        character(:), allocatable :: provider, test_provider, leaf_interface, leaf_call
        character(:), allocatable :: expected, edit_header, edit_value
        integer :: edited_payload

        call gremlin_setup(driver, scratch, project, cache, state)
        provider = '../provider'
        test_provider = '../test_provider'
        leaf_interface = ''
        leaf_call = ''
        expected = '255'
        edited_payload = 259
        edit_header = project//'/headers/value.h'
        edit_value = '17'
        lane = 'declared-library-headers'
        if (absolute) then
            lane = 'absolute-dependency-headers'
            provider = scratch//'/provider'
            test_provider = scratch//'/test_provider'
            leaf_interface = c_interface('leaf_value')
            leaf_call = '+leaf_value()'
            expected = '262'
            edited_payload = 274
            edit_header = scratch//'/leaf/headers/value.h'
            edit_value = '19'
            call write_package(scratch//'/leaf', 'leaf_value', '7')
            call write_text(scratch//'/leaf/fpm.toml', &
                'name = "leaf"'//nl//'[library]'//nl//'include-dir = "headers"'//nl)
        end if
        call write_text(project//'/fpm.toml', &
            'name = "header_probe"'//nl//'[build]'//nl//'auto-tests = false'//nl// &
            '[library]'//nl//'include-dir = ["headers"]'//nl// &
            '[dependencies]'//nl//'provider = { path = "'//provider//'" }'//nl// &
            '[dev-dependencies]'//nl// &
            'test_provider = { path = "'//test_provider//'" }'//nl// &
            '[[test]]'//nl//'name = "test_header_payload"'//nl// &
            'source-dir = "checks"'//nl//'main = "main.f90"'//nl)
        call write_package(project, 'root_value', '13')
        call write_package(scratch//'/provider', 'provider_value', '211')
        call write_package(scratch//'/test_provider', 'test_value', '31')
        call write_text(scratch//'/provider/fpm.toml', &
            'name = "provider"'//nl//'[library]'//nl// &
            'include-dir = "headers"'//nl)
        if (absolute) call write_text(scratch//'/provider/fpm.toml', &
            'name = "provider"'//nl//'[library]'//nl//'include-dir = "headers"'//nl// &
            '[dependencies]'//nl//'leaf = { path = "'//scratch//'/leaf" }'//nl)
        call write_text(scratch//'/test_provider/fpm.toml', &
            'name = "test_provider"'//nl//'[library]'//nl// &
            'include-dir = ["headers"]'//nl)
        call write_text(project//'/checks/main.f90', &
            'program test_header_payload'//nl//'use iso_c_binding, only: c_int'//nl// &
            'implicit none'//nl//'integer :: total'//nl//'interface'//nl// &
            c_interface('root_value')//c_interface('provider_value')// &
            c_interface('test_value')//leaf_interface//'end interface'//nl// &
            'total = root_value()+provider_value()+test_value()'//leaf_call//nl// &
            "print '(a,i0)', 'HEADER_PAYLOAD=', total"//nl// &
            'if (total /= '//expected//') error stop 1'//nl// &
            'end program test_header_payload'//nl)
        call gremlin_start_args(args, project, lane, 'test_header_payload')
        call gremlin_json(driver, project, cache, state, args, response, process, 30000)
        call assert_true(process%exit_code == 0, 'starts declared header resident lane')
        session = gremlin_field(response, 'session_id')
        call wait_case('', 'PASS', generation, log_path, ready)
        if (ready) then
            ! The child checks the original sum independently of Fo receipts.
            call write_text(edit_header, '#define VALUE '//edit_value//nl)
            call wait_case(generation, 'FAIL', changed, log_path, ready)
            if (ready) call assert_equal_integer( &
                receipt_payload(log_path), edited_payload, &
                'declared root or transitive absolute header edit changes warm runtime')
            call gremlin_stop_lane(driver, project, cache, state, lane, session, &
                allow_terminal_error=.true.)
            call frozen_replay(generation)
            if (absolute) call reject_escaping_input()
        else
            call gremlin_stop_lane(driver, project, cache, state, lane, session, &
                allow_terminal_error=.true.)
        end if
    end subroutine run_header_case

    subroutine reject_escaping_input()
        character(:), allocatable :: manifest, sentinel, sentinel_bytes
        type(json_value_t) :: result
        type(process_result_t) :: command
        integer :: attempt

        sentinel = scratch//'/escape'
        call write_text(sentinel, 'outside sentinel')
        sentinel_bytes = read_text(sentinel)
        manifest = read_text(project//'/fpm.toml')
        call write_text(project//'/fpm.toml', manifest//nl// &
            '[[extra.fo.inputs]]'//nl//'path = "../escape"'//nl// &
            'role = "test-fixture"'//nl)
        lane = 'absolute-invalid-materialization'
        call gremlin_start_args(args, project, lane, 'test_header_payload')
        call gremlin_json(driver, project, cache, state, args, result, command, 30000)
        session = gremlin_field(result, 'session_id')
        if (len(session) > 0) then
            do attempt = 1, 200
                call read_status()
                if (gremlin_field(document, 'state') == 'capture_failed') exit
                if (gremlin_field(document, 'state') == 'error') exit
                call gremlin_wait_ms(50)
            end do
            call assert_true(gremlin_field(document, 'state') == 'capture_failed' .or. &
                gremlin_field(document, 'state') == 'error', &
                'capture refuses an input materialization path escaping its root')
            call assert_true(index(gremlin_field(document, 'diagnostic'), &
                'parent traversal') > 0, 'refusal identifies invalid input path')
        else
            call assert_true(command%exit_code /= 0, &
                'invalid input refuses resident launch')
        end if
        call assert_true(read_text(sentinel) == sentinel_bytes, &
            'refused materialization preserves external sentinel bytes')
        if (len(session) > 0) call gremlin_stop_lane(driver, project, cache, state, &
            lane, session, allow_terminal_error=.true.)
    end subroutine reject_escaping_input

    integer function receipt_payload(path) result(payload)
        character(len=*), intent(in) :: path
        character(:), allocatable :: log, message, output
        type(json_value_t) :: report, rows, row, field
        integer :: first, last, position, status
        logical :: valid

        payload = -huge(payload)
        log = read_text(path)
        first = index(log, '{"tests":')
        if (first == 0) return
        last = index(log(first:), nl)
        if (last == 0) last = len(log) - first + 2
        call json_parse(log(first:first + last - 2), report, valid, message)
        if (.not. valid) return
        rows = json_member(report, 'tests')
        row = json_element(rows, 1)
        field = json_member(row, 'output')
        output = json_string_value(field)
        position = index(output, 'HEADER_PAYLOAD=')
        if (position == 0) return
        read(output(position + len('HEADER_PAYLOAD='):), *, iostat=status) payload
        if (status /= 0) payload = -huge(payload)
    end function receipt_payload

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
            if (gremlin_field(document, 'state') == 'capture_failed') exit
            if (gremlin_field(document, 'state') == 'error') exit
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'resident declared-header '//wanted// &
            ' receipt arrives; last state='//gremlin_field(document, 'state')// &
            '; diagnostic='//gremlin_field(document, 'diagnostic'))
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
    end subroutine frozen_replay

end program test_gremlin_library_include_cli
