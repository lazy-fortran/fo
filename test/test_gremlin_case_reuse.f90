program test_gremlin_case_reuse
    !! A Gremlin PASS receipt is reused for a case while the digest of its
    !! input closure is unchanged (#189, #140): after an edit of beta.f90,
    !! test_alpha (closure: alpha.f90 and its test) reuses the PASS of the
    !! previous generation without running, and test_beta (closure contains
    !! beta.f90) runs again. The same on fpm and CMake, including a
    !! scoped lane whose excluded case alone is affected by the edit.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, make_directory, assert_true
    use fo_test_harness, only: assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    use fo_test_json, only: json_string_value, json_size, json_element
    use fo_test_json, only: json_number_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character, parameter :: nl = new_line('a')

    call gremlin_setup(driver, scratch, project, cache, state)
    call scenario('fpm', scratch//'/fpm-project', .false., .false., .false.)
    call scenario('cmake', scratch//'/cmake-project', .true., .false., .false.)
    call scenario('scope', scratch//'/scoped-project', .true., .true., .false.)
    call scenario('data', scratch//'/data-project', .true., .false., .true.)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine scenario(lane, dir, cmake, scoped, data_edit)
        !! data_edit: the edit is of an undeclared data file the tests may
        !! read, which no source closure names: every case runs again.
        character(len=*), intent(in) :: lane, dir
        logical, intent(in) :: cmake, scoped, data_edit
        type(string_list_t) :: args
        type(process_result_t) :: process
        type(json_value_t) :: reply
        character(:), allocatable :: session, first, second
        logical :: ready

        call make_directory(dir)
        call make_directory(dir//'/src')
        call make_directory(dir//'/test')
        if (cmake) then
            call write_text(dir//'/CMakeLists.txt', &
                'cmake_minimum_required(VERSION 3.22)'//nl// &
                'project(case_reuse LANGUAGES Fortran)'//nl// &
                'enable_testing()'//nl// &
                'set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)'//nl// &
                'add_library(core src/alpha.f90 src/beta.f90)'//nl// &
                'add_executable(test_alpha test/test_alpha.f90)'//nl// &
                'target_link_libraries(test_alpha PRIVATE core)'//nl// &
                'add_test(NAME test_alpha COMMAND test_alpha)'//nl// &
                'add_executable(test_beta test/test_beta.f90)'//nl// &
                'target_link_libraries(test_beta PRIVATE core)'//nl// &
                'add_test(NAME test_beta COMMAND test_beta)'//nl// &
                'set_tests_properties(test_alpha PROPERTIES LABELS focused)'//nl)
        else
            call write_text(dir//'/fpm.toml', 'name = "case_reuse"'//nl)
        end if
        call write_module(dir, 'alpha', 1)
        call write_module(dir, 'beta', 1)
        call write_case(dir, 'alpha')
        call write_case(dir, 'beta')
        if (data_edit) call write_text(dir//'/test/ref.dat', 'reference 1'//nl)

        call arguments(lane, dir, 'start', args)
        call list_add(args, '--target')
        call list_add(args, 'test_alpha')
        if (scoped) then
            call list_add(args, '-L')
            call list_add(args, 'focused')
        else
            call list_add(args, '--target')
            call list_add(args, 'test_beta')
        end if
        call list_add(args, '--random')
        if (scoped) then
            call list_add(args, '1')
        else
            call list_add(args, '0')
        end if
        call gremlin_json(driver, dir, cache, state, args, reply, process, 30000)
        session = gremlin_field(reply, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(session) > 0, &
            lane//': starts its selected gate campaign')
        if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)

        call wait_for_gate(lane, dir, session, '', first, ready)
        call assert_true(ready, lane//': first generation passes selected cases')
        if (ready) then
            call expect_receipt(lane, dir, session, first, 'test_alpha', 'run', '')
            call expect_receipt(lane, dir, session, first, 'test_beta', &
                'run', '', absent=scoped)
            if (data_edit) then
                call write_text(dir//'/test/ref.dat', 'reference 2'//nl)
            else
                ! Outside test_alpha's closure, inside test_beta's.
                call write_module(dir, 'beta', 2)
            end if
            call wait_for_gate(lane, dir, session, first, second, ready)
            call assert_true(ready, lane//': edited generation passes selected cases')
        end if
        if (ready) then
            if (data_edit) then
                call expect_receipt(lane, dir, session, second, 'test_alpha', &
                    'run', '')
            else
                call expect_receipt(lane, dir, session, second, 'test_alpha', &
                    'reused', first)
            end if
            call expect_receipt(lane, dir, session, second, 'test_beta', &
                'run', '', absent=scoped)
        end if
        call gremlin_stop_lane(driver, dir, cache, state, lane, session, &
            allow_terminal_error=.true.)
    end subroutine scenario

    subroutine arguments(lane, dir, action, values)
        character(len=*), intent(in) :: lane, dir, action
        type(string_list_t), intent(out) :: values

        call list_add(values, 'gremlin')
        call list_add(values, action)
        call list_add(values, '--dir')
        call list_add(values, dir)
        call list_add(values, '--lane')
        call list_add(values, lane)
    end subroutine arguments

    subroutine write_module(dir, name, version)
        character(len=*), intent(in) :: dir, name
        integer, intent(in) :: version
        character(len=16) :: number

        write (number, '(i0)') version
        call write_text(dir//'/src/'//name//'.f90', 'module '//name//nl// &
            'implicit none'//nl// &
            'integer, parameter :: '//name//'_value = '//trim(number)//nl// &
            'end module '//name//nl)
    end subroutine write_module

    subroutine write_case(dir, name)
        character(len=*), intent(in) :: dir, name

        call write_text(dir//'/test/test_'//name//'.f90', &
            'program test_'//name//nl// &
            'use '//name//', only: '//name//'_value'//nl// &
            'implicit none'//nl// &
            'if ('//name//'_value < 1) error stop 1'//nl// &
            'print *, "'//name//' passed"'//nl// &
            'end program test_'//name//nl)
    end subroutine write_case

    subroutine wait_for_gate(lane, dir, session, previous, generation, found)
        character(len=*), intent(in) :: lane, dir, session, previous
        character(:), allocatable, intent(out) :: generation
        logical, intent(out) :: found
        type(string_list_t) :: status_args
        type(json_value_t) :: status
        type(process_result_t) :: observed
        integer :: poll

        found = .false.
        generation = ''
        call arguments(lane, dir, 'status', status_args)
        call list_add(status_args, '--session')
        call list_add(status_args, session)
        do poll = 1, 4800
            call gremlin_json(driver, dir, cache, state, status_args, status, &
                observed, 10000)
            if (observed%exit_code /= 0) return
            if (gremlin_field(status, 'state') == 'error') return
            generation = gremlin_field(status, 'active_generation')
            if (len(generation) == 64 .and. generation /= previous) then
                found = json_boolean_value(json_member(status, 'local_gate_green'))
                if (found) return
            end if
            call gremlin_wait_ms(50)
        end do
    end subroutine wait_for_gate

    subroutine expect_receipt(lane, dir, session, generation, case_id, &
            disposition, reused_from, absent)
        !! The case's receipt in the generation passed with this disposition
        !! (reused: from that earlier generation).
        character(len=*), intent(in) :: lane, dir, session, generation, case_id
        character(len=*), intent(in) :: disposition, reused_from
        logical, intent(in), optional :: absent
        type(string_list_t) :: query
        type(json_value_t) :: status, events, receipt
        type(process_result_t) :: observed
        character(:), allocatable :: cursor, got_status, got_disposition, got_from
        character(len=32) :: number
        integer :: k, page
        logical :: found

        found = .false.
        cursor = '0'
        do page = 1, 1000
            call arguments(lane, dir, 'status', query)
            call list_add(query, '--detail')
            call list_add(query, 'full')
            call list_add(query, '--session')
            call list_add(query, session)
            call list_add(query, '--cursor')
            call list_add(query, cursor)
            call list_add(query, '--max-records')
            call list_add(query, '128')
            call gremlin_json(driver, dir, cache, state, query, status, observed, &
                10000)
            events = json_member(status, 'events')
            do k = 1, json_size(events)
                receipt = json_element(events, k)
                if (json_string_value(json_member(receipt, 'generation')) /= &
                    generation) cycle
                if (json_string_value(json_member(receipt, 'case_id')) /= case_id) &
                    cycle
                found = .true.
                got_status = json_string_value(json_member(receipt, 'status'))
                got_disposition = json_string_value(json_member(receipt, &
                    'disposition'))
                got_from = json_string_value(json_member(receipt, 'reused_from'))
            end do
            if (.not. json_boolean_value(json_member(status, 'has_more'))) exit
            write (number, '(i0)') int(json_number_value(json_member(status, &
                'next_cursor')), int64)
            cursor = trim(number)
        end do
        if (present(absent)) then
            if (absent) then
                call assert_true(.not. found, lane//': out-of-scope '//case_id// &
                    ' has no receipt')
                return
            end if
        end if
        call assert_true(found, lane//': '//case_id//' has a receipt')
        if (.not. found) return
        call assert_equal_string(got_status, 'PASS', lane//': '//case_id//' passes')
        call assert_equal_string(got_disposition, disposition, &
            lane//': '//case_id//' disposition')
        call assert_equal_string(got_from, reused_from, &
            lane//': '//case_id//' reused receipt source')
    end subroutine expect_receipt

end program test_gremlin_case_reuse
