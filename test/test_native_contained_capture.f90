program test_native_contained_capture
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, &
        c_funptr, c_funloc
    use, intrinsic :: iso_fortran_env, only: input_unit, error_unit, output_unit
    use fo_fs, only: fs_sleep_ms
    use fo_process, only: argv_push, process_getcwd, process_getpid, &
        process_poll_pid, process_start_argv_logged, process_set_async_scope, &
        process_cancel_pid, process_exit
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_release
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, make_directory, write_text, read_text, file_exists, &
        environment_value, run_process, process_alive, poll_process, stop_sentinel, assert_true, &
        assert_equal_integer, assert_equal_string, finish_assertions
    implicit none

    interface
        integer(c_int) function c_fork() bind(C, name='fork')
            import :: c_int
        end function c_fork

        integer(c_int) function c_kill(pid, signal_number) bind(C, name='kill')
            import :: c_int
            integer(c_int), value :: pid, signal_number
        end function c_kill

        function c_signal(number, handler) bind(C, name='signal') result(previous)
            import :: c_int, c_funptr
            integer(c_int), value :: number
            type(c_funptr), value :: handler
            type(c_funptr) :: previous
        end function c_signal

        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv

        integer(c_int) function c_spawn_same_group_sentinel() &
                bind(C, name='fo_test_spawn_same_group_sentinel')
            import :: c_int
        end function c_spawn_same_group_sentinel

        integer(c_int) function c_containment_required() &
                bind(C, name='fo_c_process_containment_required')
            import :: c_int
        end function c_containment_required

    end interface

    character(len=4096) :: executable, cwd, mode, scratch_buffer
    character(:), allocatable :: scratch
    integer :: status
    integer(c_int) :: containment_required

    call get_command_argument(0, executable)
    call process_getcwd(cwd, status)
    if (index(trim(executable), '/') == 0) executable = trim(cwd)//'/'//trim(executable)
    call get_command_argument(1, mode)
    select case (trim(mode))
    case ('--io-helper')
        call io_helper()
    case ('--signal-helper')
        call signal_helper()
    case ('--timeout-helper')
        call timeout_helper()
    case ('--contained')
        call get_command_argument(2, scratch_buffer)
        call contained_mode(trim(scratch_buffer))
    case ('--outer')
        call get_command_argument(2, scratch_buffer)
        call outer_mode(trim(scratch_buffer))
    case default
        call make_scratch('fo-native-contained-capture', scratch)
        containment_required = c_containment_required()
        call assert_true(containment_required >= 0, &
            'checks whether inherited containment is active')
        if (containment_required < 0) then
            call finish_assertions(retain_failed_scratch=.true.)
        else if (containment_required == 1) then
            call contained_mode(trim(scratch))
        else
            call run_oracle(trim(scratch))
            call finish_assertions(retain_failed_scratch=.true.)
        end if
    end select

contains

    subroutine run_oracle(root)
        character(len=*), intent(in) :: root
        character(:), allocatable :: packed
        integer :: count, outer_pid, start_error, outer_exit, attempt
        logical :: done
        character(:), allocatable :: driver
        character(len=4096) :: state_root
        character(len=32) :: pid_text
        integer(c_int) :: env_status

        driver = environment_value('FO_BIN')
        call assert_true(len(driver) > 1, 'receives a pinned FO_BIN')
        if (len(driver) > 1) then
            call assert_true(driver(1:1) == '/', 'FO_BIN is absolute')
            call assert_true(file_exists(driver), 'pinned FO_BIN exists')
        end if
        state_root = trim(root)//'/gremlin-state'
        env_status = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(state_root)//c_null_char, 1_c_int)
        call assert_equal_integer(int(env_status), 0, 'isolates async owner state')
        call make_directory(trim(root)//'/project')

        packed = ''
        count = 0
        outer_pid = -1
        start_error = -1
        outer_exit = -1
        call argv_push(packed, count, trim(executable))
        call argv_push(packed, count, '--outer')
        call argv_push(packed, count, trim(root))
        call process_start_argv_logged(trim(cwd), packed, count, &
            trim(root)//'/outer.log', outer_pid, start_error)
        call assert_true(start_error == 0 .and. outer_pid > 0, &
            'starts the uncontained async owner')
        done = .false.
        if (start_error == 0 .and. outer_pid > 0) then
            do attempt = 1, 2000
                call process_poll_pid(outer_pid, done, outer_exit)
                if (done) exit
                call fs_sleep_ms(10)
            end do
        end if
        if (.not. done) then
            call process_cancel_pid(outer_pid, status)
            call assert_true(.false., &
                'bounded outer owner exits after contained capture')
            do attempt = 1, 500
                call process_poll_pid(outer_pid, done, outer_exit)
                if (done) exit
                call fs_sleep_ms(10)
            end do
        end if
        call assert_true(done, 'reaps outer async owner')
        if (done) call assert_equal_integer(outer_exit, 0, &
            'outer owner reports all contained capture checks passed')
        if (outer_pid > 0) then
            call assert_true(.not. process_alive(outer_pid), &
                'outer owner is absent after asynchronous teardown')
        end if

        call assert_pid_absent(trim(root)//'/contained.pid', &
            'contained async child exits before outer teardown')
        call assert_pid_absent(trim(root)//'/io-monitor.pid', &
            'normal capture monitor exits before outer teardown')
        call assert_pid_absent(trim(root)//'/signal-monitor.pid', &
            'signal capture monitor exits before outer teardown')
        call assert_pid_absent(trim(root)//'/timeout-monitor.pid', &
            'timeout capture monitor exits before outer teardown')
        call assert_pid_absent(trim(root)//'/target.pid', &
            'timed out target exits before outer teardown')
        call assert_pid_absent(trim(root)//'/descendant.pid', &
            'TERM-resistant descendant is gone after outer teardown')
        call assert_pid_absent(trim(root)//'/sentinel.pid', &
            'same-group sentinel is gone after outer teardown')
    end subroutine run_oracle

    subroutine outer_mode(root)
        character(len=*), intent(in) :: root
        type(gremlin_session_t) :: session
        character(:), allocatable :: packed
        integer :: ierr, count, child_pid, child_exit, start_error, attempt
        logical :: done
        character(len=256) :: message
        character(len=32) :: pid_text
        integer :: sentinel_pid, descendant_pid, target_pid, monitor_pid

        call make_directory(trim(root)//'/project')
        call gremlin_session_acquire(trim(root)//'/project', 'native-capture', &
            session, ierr, message)
        if (ierr /= 0 .or. .not. session%owner) then
            write(error_unit, '(a)') &
                'async scope owner acquisition failed: '//trim(message)
            call process_exit(70)
        end if
        write(pid_text, '(i0)') process_getpid()
        call write_text(trim(root)//'/outer.pid', trim(pid_text))
        call process_set_async_scope(session%state_dir, session%owner_pid, &
            session%owner_start, ierr)
        if (ierr /= 0) then
            call gremlin_session_release(session, status, message)
            call process_exit(71)
        end if

        packed = ''
        count = 0
        child_pid = -1
        child_exit = -1
        start_error = -1
        call argv_push(packed, count, trim(executable))
        call argv_push(packed, count, '--contained')
        call argv_push(packed, count, trim(root))
        call process_start_argv_logged(trim(cwd), packed, count, &
            trim(root)//'/contained.log', child_pid, start_error)
        done = .false.
        if (start_error == 0 .and. child_pid > 0) then
            do attempt = 1, 2000
                call process_poll_pid(child_pid, done, child_exit)
                if (done) exit
                call fs_sleep_ms(10)
            end do
        end if
        if (.not. done) then
            if (child_pid > 0) call process_cancel_pid(child_pid, status)
            child_exit = 72
            if (child_pid > 0) then
                do attempt = 1, 500
                    call process_poll_pid(child_pid, done, child_exit)
                    if (done) exit
                    call fs_sleep_ms(10)
                end do
            end if
        end if

        sentinel_pid = read_pid(trim(root)//'/sentinel.pid')
        descendant_pid = read_pid(trim(root)//'/descendant.pid')
        target_pid = read_pid(trim(root)//'/target.pid')
        monitor_pid = read_pid(trim(root)//'/timeout-monitor.pid')
        call stop_exact(sentinel_pid)
        call stop_exact(descendant_pid)
        call stop_exact(target_pid)
        call stop_exact(monitor_pid)
        call stop_exact(read_pid(trim(root)//'/io-monitor.pid'))
        call stop_exact(read_pid(trim(root)//'/signal-monitor.pid'))
        call gremlin_session_release(session, ierr, message)
        call assert_equal_integer(ierr, 0, 'releases the async owner session')
        call assert_true(done, 'reaps contained capture child')
        call assert_equal_integer(child_exit, 0, 'contained capture checks pass')
        call assert_pid_absent(trim(root)//'/sentinel.pid', &
            'outer teardown removes the unrelated sentinel')
        call assert_pid_absent(trim(root)//'/descendant.pid', &
            'outer teardown removes the resistant descendant')
        call finish_assertions(retain_failed_scratch=.true.)
        call process_exit(0)
    end subroutine outer_mode

    subroutine contained_mode(root)
        character(len=*), intent(in) :: root
        type(string_list_t) :: args
        type(process_result_t) :: result
        integer :: sentinel_pid, descendant_pid, sentinel_status
        integer :: contained_pid, monitor_pid
        character(len=32) :: pid_text
        logical :: sentinel_running

        contained_pid = process_getpid()
        write(pid_text, '(i0)') contained_pid
        call write_text(trim(root)//'/contained.pid', trim(pid_text))
        sentinel_pid = int(c_spawn_same_group_sentinel())
        call assert_true(sentinel_pid > 0, 'starts unrelated same-group sentinel')
        sentinel_running = .false.
        if (sentinel_pid > 0) then
            call poll_process(sentinel_pid, sentinel_status)
            sentinel_running = sentinel_status == 999
        end if
        call assert_true(sentinel_running, 'same-group sentinel is initially running')
        write(pid_text, '(i0)') sentinel_pid
        call write_text(trim(root)//'/sentinel.pid', trim(pid_text))

        args = string_list_t()
        call list_add(args, trim(executable))
        call list_add(args, '--io-helper')
        call run_process(args, trim(root), result, &
            input='capture-input'//new_line('a'), &
            timeout_ms=5000)
        call assert_true(.not. result%runner_failed, &
            'contained capture starts through its pinned Fo monitor')
        call assert_true(result%reaped .and. .not. result%timed_out, &
            'normal capture completes and reaps its target')
        call assert_equal_integer(result%exit_code, 7, 'preserves target exit status')
        call assert_equal_integer(result%term_signal, 0, 'normal exit is not a signal')
        call assert_equal_string(result%stdout, 'stdout:capture-input'//new_line('a'), &
            'captures child stdout separately')
        call assert_equal_string(result%stderr, 'stderr:capture-input'//new_line('a'), &
            'captures child stderr separately')
        monitor_pid = result%process_id
        write(pid_text, '(i0)') monitor_pid
        call write_text(trim(root)//'/io-monitor.pid', trim(pid_text))

        args = string_list_t()
        call list_add(args, trim(executable))
        call list_add(args, '--signal-helper')
        call run_process(args, trim(root), result, timeout_ms=5000)
        call assert_true(.not. result%runner_failed .and. result%reaped, &
            'signal helper starts and is reaped')
        call assert_equal_integer(result%term_signal, 15, &
            'preserves target termination signal')
        monitor_pid = result%process_id
        write(pid_text, '(i0)') monitor_pid
        call write_text(trim(root)//'/signal-monitor.pid', trim(pid_text))

        args = string_list_t()
        call list_add(args, trim(executable))
        call list_add(args, '--timeout-helper')
        call list_add(args, trim(root)//'/target.pid')
        call list_add(args, trim(root)//'/descendant.pid')
        call list_add(args, trim(root)//'/descendant.ready')
        call list_add(args, trim(root)//'/descendant.heartbeat')
        call run_process(args, trim(root), result, timeout_ms=1000)
        call assert_true(.not. result%runner_failed, &
            'starts timeout helper under containment')
        call assert_true(result%timed_out .and. result%reaped, &
            'timeout cleans and reaps the capture monitor')
        monitor_pid = result%process_id
        write(pid_text, '(i0)') monitor_pid
        call write_text(trim(root)//'/timeout-monitor.pid', trim(pid_text))
        call assert_true(file_exists(trim(root)//'/descendant.ready'), &
            'TERM-resistant descendant reached its ready marker')
        call assert_true(file_exists(trim(root)//'/descendant.heartbeat'), &
            'TERM-resistant descendant produced heartbeat evidence')
        if (file_exists(trim(root)//'/descendant.heartbeat')) then
            call assert_true(index(read_text(trim(root)//'/descendant.heartbeat'), &
                'beat') > 0, 'descendant heartbeat advanced before timeout')
        end if
        descendant_pid = read_pid(trim(root)//'/descendant.pid')
        call assert_true(descendant_pid > 0, 'records the exact descendant identity')
        call assert_true(.not. process_alive(descendant_pid), &
            'timeout cleanup removes the TERM-resistant descendant')
        if (sentinel_running) then
            call poll_process(sentinel_pid, sentinel_status)
            sentinel_running = sentinel_status == 999
        end if
        call assert_true(sentinel_running, &
            'inner timeout cleanup preserves a running same-group sentinel')
        if (sentinel_running) call stop_sentinel(sentinel_pid)
        call assert_pid_absent(trim(root)//'/sentinel.pid', &
            'contained test stops and reaps its sentinel before owner teardown')
        call assert_pid_absent(trim(root)//'/descendant.pid', &
            'contained test leaves no descendant')
        call assert_pid_absent(trim(root)//'/target.pid', &
            'contained test leaves no target process')
        call assert_pid_absent(trim(root)//'/io-monitor.pid', &
            'contained test leaves no normal monitor')
        call assert_pid_absent(trim(root)//'/signal-monitor.pid', &
            'contained test leaves no signal monitor')
        call assert_pid_absent(trim(root)//'/timeout-monitor.pid', &
            'contained test leaves no timeout monitor')
        call finish_assertions(retain_failed_scratch=.true.)
        call process_exit(0)
    end subroutine contained_mode

    subroutine io_helper()
        character(len=256) :: input_line
        integer :: io_status

        read(input_unit, '(a)', iostat=io_status) input_line
        if (io_status /= 0) call process_exit(80)
        write(output_unit, '(a)') 'stdout:'//trim(input_line)
        write(error_unit, '(a)') 'stderr:'//trim(input_line)
        call process_exit(7)
    end subroutine io_helper

    subroutine signal_helper()
        integer(c_int) :: rc
        rc = c_kill(int(process_getpid(), c_int), 15_c_int)
        if (rc /= 0) call process_exit(81)
        call process_exit(82)
    end subroutine signal_helper

    subroutine timeout_helper()
        character(len=4096) :: target_path, descendant_path, ready_path, heartbeat_path
        integer(c_int) :: first, second
        integer :: pid, attempt
        character(len=32) :: pid_text
        type(c_funptr) :: previous

        call get_command_argument(2, target_path)
        call get_command_argument(3, descendant_path)
        call get_command_argument(4, ready_path)
        call get_command_argument(5, heartbeat_path)
        pid = process_getpid()
        write(pid_text, '(i0)') pid
        call write_text(trim(target_path), trim(pid_text))
        first = c_fork()
        if (first < 0_c_int) call process_exit(83)
        if (first == 0_c_int) then
            second = c_fork()
            if (second < 0_c_int) call process_exit(84)
            if (second > 0_c_int) call process_exit(0)
            previous = c_signal(15_c_int, c_funloc(ignore_term))
            pid = process_getpid()
            write(pid_text, '(i0)') pid
            call write_text(trim(descendant_path), trim(pid_text))
            call write_text(trim(ready_path), 'ready')
            call write_text(trim(heartbeat_path), 'started'//new_line('a'))
            do
                call append_text(trim(heartbeat_path), 'beat'//new_line('a'))
                call fs_sleep_ms(20)
            end do
        end if
        do attempt = 1, 200
            if (file_exists(trim(ready_path))) exit
            call fs_sleep_ms(10)
        end do
        if (.not. file_exists(trim(ready_path))) call process_exit(85)
        do
            call fs_sleep_ms(100)
        end do
    end subroutine timeout_helper

    subroutine stop_exact(pid)
        integer, intent(in) :: pid
        integer(c_int) :: rc
        integer :: attempt
        if (pid <= 0) return
        if (.not. process_alive(pid)) return
        rc = c_kill(int(pid, c_int), 15_c_int)
        do attempt = 1, 100
            if (.not. process_alive(pid)) return
            call fs_sleep_ms(10)
        end do
        rc = c_kill(int(pid, c_int), 9_c_int)
        do attempt = 1, 100
            if (.not. process_alive(pid)) return
            call fs_sleep_ms(10)
        end do
    end subroutine stop_exact

    subroutine assert_pid_absent(path, label)
        character(len=*), intent(in) :: path, label
        integer :: pid
        pid = read_pid(path)
        call assert_true(pid > 0, label//' (PID was recorded)')
        if (pid > 0) then
            call assert_true(.not. process_alive(pid), label)
        end if
    end subroutine assert_pid_absent

    integer function read_pid(path) result(pid)
        character(len=*), intent(in) :: path
        integer :: unit, io_status
        pid = -1
        if (.not. file_exists(path)) return
        open(newunit=unit, file=path, status='old', action='read', iostat=io_status)
        if (io_status /= 0) return
        read(unit, *, iostat=io_status) pid
        close(unit)
        if (io_status /= 0) pid = -1
    end function read_pid

    subroutine append_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: unit, io_status
        open(newunit=unit, file=path, access='stream', form='unformatted', &
            status='old', position='append', action='write', iostat=io_status)
        if (io_status /= 0) call process_exit(86)
        write(unit, iostat=io_status) text
        close(unit)
        if (io_status /= 0) call process_exit(87)
    end subroutine append_text

    subroutine ignore_term(signal_number) bind(C)
        integer(c_int), value :: signal_number
    end subroutine ignore_term

end program test_native_contained_capture
