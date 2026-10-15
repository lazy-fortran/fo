program test_gremlin_parallel
    !! A Gremlin lane with --jobs k runs up to k required cases at once (#216):
    !! (a) six CPU-bound cases overlap (at least 2, at most 3 with jobs=3) and
    !! each has one PASS receipt; (b) an edit during a running parallel
    !! generation cancels every in-flight child and credits no old-generation
    !! receipt; (c) a failing required case is dispatched before the
    !! background cases.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, make_directory, assert_true
    use fo_test_harness, only: read_text, file_exists, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane, gremlin_process_running
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    use fo_test_json, only: json_string_value, json_size, json_element
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state
    character, parameter :: nl = new_line('a')
    character(len=*), parameter :: overlap_names(6) = &
        [character(len=2) :: 'p1', 'p2', 'p3', 'p4', 'p5', 'p6']
    character(len=*), parameter :: block_names(4) = &
        [character(len=2) :: 'b1', 'b2', 'b3', 'b4']

    call gremlin_setup(driver, scratch, project, cache, state)
    call scenario_overlap()
    call scenario_supersession()
    call scenario_order()
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine scenario_overlap()
        character(len=*), parameter :: lane = 'par-overlap'
        character(:), allocatable :: dir, marks, session, generation, text
        type(string_list_t) :: args
        type(process_result_t) :: process
        type(json_value_t) :: reply
        integer :: i, peak, overall, unit, ios
        logical :: ready

        dir = scratch//'/overlap'
        marks = scratch//'/overlap-marks'
        call make_directory(marks)
        call make_project(dir, 1)
        do i = 1, size(overlap_names)
            call write_case(dir, overlap_names(i), marks, overlap_names, .false.)
        end do
        call start_args(args, dir, lane, 3)
        do i = 1, size(overlap_names)
            call list_add(args, '--target')
            call list_add(args, 'test_'//overlap_names(i))
        end do
        call list_add(args, '--random')
        call list_add(args, '0')
        call gremlin_json(driver, dir, cache, state, args, reply, process, 30000)
        session = gremlin_field(reply, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(session) > 0, &
            'overlap: lane starts with jobs=3')
        if (len(session) == 0) return
        call wait_gate(dir, lane, session, '', generation, ready)
        call assert_true(ready, 'overlap: all six required cases pass')
        overall = 0
        if (ready) then
            do i = 1, size(overlap_names)
                call assert_true(count_receipts(dir, lane, session, generation, &
                    'test_'//overlap_names(i), 'PASS') == 1, &
                    'overlap: exactly one PASS receipt for '//overlap_names(i))
                text = read_text(marks//'/'//overlap_names(i)//'.peak')
                peak = 0
                open (newunit=unit, status='scratch')
                write (unit, '(a)') text
                rewind (unit)
                read (unit, *, iostat=ios) peak
                close (unit)
                overall = max(overall, peak)
            end do
            call assert_true(overall >= 2, 'overlap: at least two cases ran at once')
            call assert_true(overall <= 3, 'overlap: never more than jobs=3 at once')
        end if
        call gremlin_stop_lane(driver, dir, cache, state, lane, session, &
            allow_terminal_error=.true.)
    end subroutine scenario_overlap

    subroutine scenario_supersession()
        character(len=*), parameter :: lane = 'par-supersede'
        character(:), allocatable :: dir, marks, session, first, second
        character(len=32) :: pids(size(block_names))
        type(string_list_t) :: args
        type(process_result_t) :: process
        type(json_value_t) :: reply
        integer :: i, started, pid, waited, ios, unit
        logical :: ready, any_alive

        dir = scratch//'/supersede'
        marks = scratch//'/supersede-marks'
        call make_directory(marks)
        call make_project(dir, 1)
        do i = 1, size(block_names)
            call write_case(dir, block_names(i), marks, block_names, .true.)
        end do
        call start_args(args, dir, lane, 3)
        do i = 1, size(block_names)
            call list_add(args, '--target')
            call list_add(args, 'test_'//block_names(i))
        end do
        call list_add(args, '--random')
        call list_add(args, '0')
        call gremlin_json(driver, dir, cache, state, args, reply, process, 30000)
        session = gremlin_field(reply, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(session) > 0, &
            'supersede: lane starts with jobs=3')
        if (len(session) == 0) return
        ! Three children block on the release file.
        started = 0
        do waited = 1, 2400
            started = 0
            do i = 1, size(block_names)
                if (file_exists(marks//'/'//block_names(i)//'.start')) started = started + 1
            end do
            if (started >= 3) exit
            call gremlin_wait_ms(100)
        end do
        call assert_true(started == 3, 'supersede: exactly three children are in flight')
        pids = ''
        do i = 1, size(block_names)
            if (.not. file_exists(marks//'/'//block_names(i)//'.start')) cycle
            pids(i) = trim(adjustl(read_text(marks//'/'//block_names(i)//'.start')))
        end do
        first = current_generation(dir, lane, session)
        call make_project(dir, 2)
        second = first
        do waited = 1, 2400
            second = current_generation(dir, lane, session)
            if (len(second) == 64 .and. second /= first) exit
            call gremlin_wait_ms(100)
        end do
        call assert_true(second /= first, 'supersede: the edit activates a new generation')
        any_alive = .false.
        do i = 1, size(block_names)
            if (len_trim(pids(i)) == 0) cycle
            read (pids(i), *, iostat=ios) pid
            if (ios /= 0) cycle
            if (gremlin_process_running(pid)) then
                call gremlin_wait_ms(500)
                if (gremlin_process_running(pid)) any_alive = .true.
            end if
        end do
        call assert_true(.not. any_alive, &
            'supersede: every old-generation child is cancelled')
        do i = 1, size(block_names)
            call assert_true(count_receipts(dir, lane, session, first, &
                'test_'//block_names(i), 'PASS') == 0, &
                'supersede: no old-generation PASS receipt for '//block_names(i))
        end do
        open (newunit=unit, file=marks//'/release', status='replace')
        write (unit, '(a)') 'go'
        close (unit)
        call wait_gate(dir, lane, session, first, second, ready)
        call assert_true(ready, 'supersede: the new generation passes its cases')
        if (ready) then
            do i = 1, size(block_names)
                call assert_true(count_receipts(dir, lane, session, second, &
                    'test_'//block_names(i), 'PASS') == 1, &
                    'supersede: one new-generation PASS receipt for '//block_names(i))
                call assert_true(count_receipts(dir, lane, session, first, &
                    'test_'//block_names(i), 'PASS') == 0, &
                    'supersede: old generation stays without PASS for '//block_names(i))
            end do
        end if
        call gremlin_stop_lane(driver, dir, cache, state, lane, session, &
            allow_terminal_error=.true.)
    end subroutine scenario_supersession

    subroutine scenario_order()
        character(len=*), parameter :: lane = 'par-order'
        character(len=*), parameter :: names(4) = &
            [character(len=7) :: 'c_fail', 'c_bg1', 'c_bg2', 'c_bg3']
        character(:), allocatable :: dir, marks, session, text
        type(string_list_t) :: args
        type(process_result_t) :: process
        type(json_value_t) :: reply
        integer :: i, waited, lines

        dir = scratch//'/order'
        marks = scratch//'/order-marks'
        call make_directory(marks)
        call make_project(dir, 1)
        call write_text(marks//'/order.log', '')
        do i = 1, size(names)
            call write_order_case(dir, trim(names(i)), marks, i == 1)
        end do
        call start_args(args, dir, lane, 2)
        call list_add(args, '--target')
        call list_add(args, 'test_c_fail')
        call list_add(args, '--random')
        call list_add(args, '3')
        call gremlin_json(driver, dir, cache, state, args, reply, process, 30000)
        session = gremlin_field(reply, 'session_id')
        call assert_true(process%exit_code == 0 .and. len(session) > 0, &
            'order: lane starts with jobs=2')
        if (len(session) == 0) return
        lines = 0
        do waited = 1, 2400
            text = read_text(marks//'/order.log')
            lines = count([(text(i:i) == nl, i=1, len(text))])
            if (lines >= 3) exit
            call gremlin_wait_ms(100)
        end do
        call assert_true(lines >= 3, 'order: background cases run after the required one')
        call assert_true(index(text, 'test_c_fail'//nl) == 1, &
            'order: the failing required case is dispatched first')
        call gremlin_stop_lane(driver, dir, cache, state, lane, session, &
            allow_terminal_error=.true.)
    end subroutine scenario_order

    subroutine make_project(dir, version)
        character(len=*), intent(in) :: dir
        integer, intent(in) :: version
        character(len=8) :: number

        call make_directory(dir)
        call make_directory(dir//'/src')
        call make_directory(dir//'/test')
        call write_text(dir//'/fpm.toml', 'name = "parallel"'//nl)
        write (number, '(i0)') version
        call write_text(dir//'/src/alpha.f90', 'module alpha'//nl// &
            'implicit none'//nl//'integer, parameter :: alpha_value = '// &
            trim(number)//nl//'end module alpha'//nl)
    end subroutine make_project

    subroutine write_case(dir, name, marks, names, block)
        !! Marks its start with its pid, then either blocks until marks/release
        !! exists (block) or spins until it has seen three cases running at
        !! once (or a deadline), recording the peak it saw.
        character(len=*), intent(in) :: dir, name, marks, names(:)
        logical, intent(in) :: block
        character(:), allocatable :: list, wait_condition
        integer :: i

        list = ''
        do i = 1, size(names)
            if (i > 1) list = list//', '
            list = list//"'"//trim(names(i))//"'"
        end do
        if (block) then
            wait_condition = '        inquire (file=dir//"/release", exist=e1)'//nl// &
                '        if (e1) exit'//nl
        else
            wait_condition = '        n = 0'//nl// &
                '        do i = 1, size(names)'//nl// &
                '            inquire (file=dir//"/"//trim(names(i))//".start", exist=e1)'//nl// &
                '            inquire (file=dir//"/"//trim(names(i))//".end", exist=e2)'//nl// &
                '            if (e1 .and. .not. e2) n = n + 1'//nl// &
                '        end do'//nl// &
                '        peak = max(peak, n)'//nl// &
                '        if (peak >= 3) exit'//nl
        end if
        call write_text(dir//'/test/test_'//trim(name)//'.f90', &
            'program test_'//trim(name)//nl// &
            'use alpha, only: alpha_value'//nl// &
            'implicit none'//nl// &
            'character(len=*), parameter :: dir = "'//marks//'"'//nl// &
            'character(len=2), parameter :: names('//itoa(size(names))//') = [character(len=2) :: '// &
            list//']'//nl// &
            'integer :: i, n, peak, u'//nl// &
            'integer(8) :: c0, c1, rate'//nl// &
            'logical :: e1, e2'//nl// &
            'integer :: getpid'//nl// &
            'peak = 0'//nl// &
            'if (alpha_value < 1) error stop 1'//nl// &
            'open (newunit=u, file=dir//"/'//trim(name)//'.start", status="replace")'//nl// &
            'write (u, "(i0)") getpid()'//nl// &
            'close (u)'//nl// &
            'call system_clock(c0, rate)'//nl// &
            'do'//nl// &
            wait_condition// &
            '        call system_clock(c1)'//nl// &
            '        if (c1 - c0 > 4*rate) exit'//nl// &
            'end do'//nl// &
            'open (newunit=u, file=dir//"/'//trim(name)//'.peak", status="replace")'//nl// &
            'write (u, "(i0)") peak'//nl// &
            'close (u)'//nl// &
            'open (newunit=u, file=dir//"/'//trim(name)//'.end", status="replace")'//nl// &
            'write (u, "(a)") "end"'//nl// &
            'close (u)'//nl// &
            'end program test_'//trim(name)//nl)
    end subroutine write_case

    subroutine write_order_case(dir, name, marks, failing)
        character(len=*), intent(in) :: dir, name, marks
        logical, intent(in) :: failing
        character(:), allocatable :: tail

        tail = 'end program test_'//name//nl
        if (failing) tail = 'error stop 3'//nl//tail
        call write_text(dir//'/test/test_'//name//'.f90', &
            'program test_'//name//nl// &
            'use alpha, only: alpha_value'//nl// &
            'implicit none'//nl// &
            'integer :: u'//nl// &
            'open (newunit=u, file="'//marks//'/order.log", position="append", '// &
            'action="write")'//nl// &
            'write (u, "(a)") "test_'//name//'"'//nl// &
            'close (u)'//nl// &
            'if (alpha_value < 1) error stop 1'//nl// &
            tail)
    end subroutine write_order_case

    function itoa(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=16) :: number

        write (number, '(i0)') value
        text = trim(number)
    end function itoa

    subroutine start_args(args, dir, lane, jobs)
        type(string_list_t), intent(out) :: args
        character(len=*), intent(in) :: dir, lane
        integer, intent(in) :: jobs

        call list_add(args, 'gremlin')
        call list_add(args, 'start')
        call list_add(args, '--dir')
        call list_add(args, dir)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--jobs')
        call list_add(args, itoa(jobs))
        call list_add(args, '--campaign-seconds')
        call list_add(args, '60')
        call list_add(args, '--timeout-seconds')
        call list_add(args, '120')
    end subroutine start_args

    subroutine status_args(args, dir, lane, session)
        type(string_list_t), intent(out) :: args
        character(len=*), intent(in) :: dir, lane, session

        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--dir')
        call list_add(args, dir)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, session)
    end subroutine status_args

    function current_generation(dir, lane, session) result(generation)
        character(len=*), intent(in) :: dir, lane, session
        character(:), allocatable :: generation
        type(string_list_t) :: args
        type(json_value_t) :: view
        type(process_result_t) :: observed

        call status_args(args, dir, lane, session)
        call gremlin_json(driver, dir, cache, state, args, view, observed, 10000)
        generation = gremlin_field(view, 'active_generation')
    end function current_generation

    subroutine wait_gate(dir, lane, session, previous, generation, found)
        character(len=*), intent(in) :: dir, lane, session, previous
        character(:), allocatable, intent(out) :: generation
        logical, intent(out) :: found
        type(string_list_t) :: args
        type(json_value_t) :: view
        type(process_result_t) :: observed
        integer :: poll

        found = .false.
        generation = ''
        call status_args(args, dir, lane, session)
        do poll = 1, 4800
            call gremlin_json(driver, dir, cache, state, args, view, observed, 10000)
            if (observed%exit_code /= 0) return
            if (gremlin_field(view, 'state') == 'error') return
            generation = gremlin_field(view, 'active_generation')
            if (len(generation) == 64 .and. generation /= previous) then
                found = json_boolean_value(json_member(view, 'local_gate_green'))
                if (found) return
            end if
            call gremlin_wait_ms(50)
        end do
    end subroutine wait_gate

    integer function count_receipts(dir, lane, session, generation, case_id, status) &
            result(n)
        character(len=*), intent(in) :: dir, lane, session, generation, case_id, status
        type(string_list_t) :: args
        type(json_value_t) :: view, events, receipt
        type(process_result_t) :: observed
        integer :: k

        n = 0
        call status_args(args, dir, lane, session)
        call list_add(args, '--detail')
        call list_add(args, 'full')
        call list_add(args, '--cursor')
        call list_add(args, '0')
        call list_add(args, '--max-records')
        call list_add(args, '128')
        call gremlin_json(driver, dir, cache, state, args, view, observed, 10000)
        events = json_member(view, 'events')
        do k = 1, json_size(events)
            receipt = json_element(events, k)
            if (json_string_value(json_member(receipt, 'generation')) /= generation) cycle
            if (json_string_value(json_member(receipt, 'case_id')) /= case_id) cycle
            if (json_string_value(json_member(receipt, 'status')) /= status) cycle
            n = n + 1
        end do
    end function count_receipts

end program test_gremlin_parallel
