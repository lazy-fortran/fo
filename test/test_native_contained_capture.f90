program test_native_contained_capture
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_null_char, &
        c_funptr
    use, intrinsic :: iso_fortran_env, only: input_unit, error_unit, output_unit
    use fo_fs, only: fs_sleep_ms, fs_is_windows, fs_path_is_absolute, fs_realpath
    use fo_process, only: argv_push, process_getcwd, process_getpid, &
        process_poll_pid, process_start_argv_logged, process_set_async_scope, &
        process_cancel_pid, process_exit
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_release
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, make_directory, write_text, read_text, file_exists, &
        environment_value, run_process, process_alive, poll_process, stop_sentinel, assert_true, &
        assert_equal_integer, assert_equal_string, finish_assertions, spawn_unowned_process
    use fo_test_os_link, only: test_os_initialize
    use fo_test_json, only: json_value_t, json_parse, json_member, json_string_value
    use fo_test_process_identity, only: mcp_process_start_time, &
        mcp_process_identity_running, mcp_kill_owned_tree
    implicit none

    interface
        integer(c_int) function c_kill(pid, signal_number) bind(C, name='fo_test_signal')
            import :: c_int
            integer(c_int), value :: pid, signal_number
        end function c_kill
        integer(c_int) function c_ignore_term() bind(C, name='fo_test_ignore_term')
            import :: c_int
        end function c_ignore_term
        integer(c_int) function c_terminate_self() bind(C, name='fo_test_terminate_self')
            import :: c_int
        end function c_terminate_self

        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='fo_test_setenv')
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

        integer(c_int) function c_host_is_linux() bind(C, name='fo_test_host_is_linux')
            import :: c_int
        end function c_host_is_linux

    end interface

    character(len=4096) :: executable, cwd, mode, scratch_buffer
    character(:), allocatable :: scratch
    integer :: status
    integer(c_int) :: containment_required

    status = test_os_initialize()
    if (status /= 162) error stop 'cannot initialize native binary test IO'
    call get_command_argument(0, executable)
    call process_getcwd(cwd, status)
    if (.not. fs_path_is_absolute(trim(executable))) executable = trim(cwd)//'/'//trim(executable)
    call get_command_argument(1, mode)
    select case (trim(mode))
    case ('--io-helper')
        call io_helper()
    case ('--signal-helper')
        call signal_helper()
    case ('--orphan-intermediate')
        call orphan_intermediate()
    case ('--orphan-leaf')
        call orphan_leaf()
    case ('--timeout-helper')
        call timeout_helper()
    case ('--async-exit-helper')
        call async_exit_helper()
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
            call assert_true(fs_path_is_absolute(driver), 'FO_BIN is absolute')
            call assert_true(file_exists(driver), 'pinned FO_BIN exists')
        end if
        state_root = trim(root)//'/gremlin-state'
        env_status = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(state_root)//c_null_char, 1_c_int)
        call assert_equal_integer(int(env_status), 0, 'isolates async owner state')
        ! One job keeps the fixture lane at host admission weight 1.
        env_status = c_setenv('FO_JOBS'//c_null_char, '1'//c_null_char, 1_c_int)
        call assert_equal_integer(int(env_status), 0, &
            'fixture lane uses one host work slot')
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
        call assert_pid_absent(trim(root)//'/async-target.pid', &
            'contained async target exits before outer teardown')
        call assert_pid_absent(trim(root)//'/async-descendant.pid', &
            'contained async descendant exits before outer teardown')
        call assert_pid_absent(trim(root)//'/public-owner.pid', &
            'publicly stopped resident owner exits before outer teardown')
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
        call write_identity(trim(root)//'/outer.pid', process_getpid())
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
        call stop_exact(trim(root)//'/sentinel.pid')
        call stop_exact(trim(root)//'/descendant.pid')
        call stop_exact(trim(root)//'/target.pid')
        call stop_exact(trim(root)//'/timeout-monitor.pid')
        call stop_exact(trim(root)//'/io-monitor.pid')
        call stop_exact(trim(root)//'/signal-monitor.pid')
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
        integer :: contained_pid, monitor_pid, async_pid, async_start_error
        integer :: async_exit, async_attempt, async_cancel_error
        integer :: async_target_pid, async_descendant_pid
        integer :: async_count
        integer :: cli_owner_pid, cli_status, cli_attempt, separator
        integer(c_int64_t) :: cli_owner_start
        character(len=32) :: pid_text
        character(:), allocatable :: driver, session_id, json_error
        logical :: sentinel_running
        logical :: async_done
        logical :: json_valid, cli_owner_live, cli_stopped
        character(:), allocatable :: async_args
        type(json_value_t) :: cli_json

        contained_pid = process_getpid()
        call write_identity(trim(root)//'/contained.pid', contained_pid)
        sentinel_pid = int(c_spawn_same_group_sentinel())
        call assert_true(sentinel_pid > 0, 'starts unrelated same-group sentinel')
        sentinel_running = .false.
        if (sentinel_pid > 0) then
            call poll_process(sentinel_pid, sentinel_status)
            sentinel_running = sentinel_status == 999
        end if
        call assert_true(sentinel_running, 'same-group sentinel is initially running')
        call write_identity(trim(root)//'/sentinel.pid', sentinel_pid)

        containment_required = c_containment_required()
        if (c_host_is_linux() == 1) then
            ! Since descendants may create native sessions, a nested launch runs
            ! under the inherited async filter without the strict group mode.
            call assert_equal_integer(seccomp_mode(), 2, &
                'nested async launch runs under the inherited async filter')
            call assert_equal_integer(int(containment_required), 0, &
                'nested async launch keeps native process groups available')
        else
            call assert_equal_integer(int(containment_required), 0, &
                'nested launch uses the available standard process boundary')
        end if
        async_args = ''
        async_count = 0
        call argv_push(async_args, async_count, trim(executable))
        call argv_push(async_args, async_count, '--async-exit-helper')
        async_pid = -1
        async_start_error = -1
        call process_start_argv_logged(trim(cwd), async_args, async_count, &
            trim(root)//'/async-exit.log', async_pid, async_start_error)
        call assert_true(async_start_error == 0 .and. async_pid > 0, &
            'starts an async command through the contained monitor')
        call write_identity(trim(root)//'/async-monitor.pid', async_pid)
        async_done = .false.
        async_exit = -1
        if (async_start_error == 0 .and. async_pid > 0) then
            do async_attempt = 1, 2000
                call process_poll_pid(async_pid, async_done, async_exit)
                if (async_done) exit
                call fs_sleep_ms(10)
            end do
        end if
        call assert_true(async_done, 'polls the contained async command to completion')
        if (.not. async_done .and. async_pid > 0) &
            call process_cancel_pid(async_pid, async_cancel_error)
        call assert_equal_integer(async_exit, 7, &
            'contained async monitor preserves the target exit status')
        if (file_exists(trim(root)//'/async-exit.log')) then
            call assert_true(index(read_text(trim(root)//'/async-exit.log'), &
                'async target output') > 0, 'contained async command captures output')
        else
            call assert_true(.false., 'contained async command creates its log')
        end if
        if (sentinel_running) then
            call poll_process(sentinel_pid, sentinel_status)
            sentinel_running = sentinel_status == 999
        end if
        call assert_true(sentinel_running, &
            'normal contained async completion preserves same-group sentinel')

        async_args = ''
        async_count = 0
        call argv_push(async_args, async_count, trim(executable))
        call argv_push(async_args, async_count, '--timeout-helper')
        call argv_push(async_args, async_count, trim(root)//'/async-target.pid')
        call argv_push(async_args, async_count, trim(root)//'/async-descendant.pid')
        call argv_push(async_args, async_count, trim(root)//'/async-descendant.ready')
        call argv_push(async_args, async_count, &
            trim(root)//'/async-descendant.heartbeat')
        async_pid = -1
        async_start_error = -1
        call process_start_argv_logged(trim(cwd), async_args, async_count, &
            trim(root)//'/async-timeout.log', async_pid, async_start_error)
        call assert_true(async_start_error == 0 .and. async_pid > 0, &
            'starts a contained async tree for cancellation')
        call write_identity(trim(root)//'/async-cancel-monitor.pid', async_pid)
        do async_attempt = 1, 200
            if (file_exists(trim(root)//'/async-descendant.ready')) exit
            call fs_sleep_ms(10)
        end do
        call assert_true(file_exists(trim(root)//'/async-descendant.ready'), &
            'contained async descendant reaches its ready barrier')
        call process_cancel_pid(async_pid, async_cancel_error)
        call assert_equal_integer(async_cancel_error, 0, &
            'cancels the exact contained async monitor tree')
        async_target_pid = read_pid(trim(root)//'/async-target.pid')
        async_descendant_pid = read_pid(trim(root)//'/async-descendant.pid')
        call assert_true(async_target_pid > 0 .and. async_descendant_pid > 0, &
            'records both contained async process identities')
        call assert_true(.not. process_alive(async_target_pid), &
            'cancellation reaps contained async target')
        call assert_true(.not. process_alive(async_descendant_pid), &
            'cancellation reaps TERM-resistant async descendant')
        if (sentinel_running) then
            call poll_process(sentinel_pid, sentinel_status)
            sentinel_running = sentinel_status == 999
        end if
        call assert_true(sentinel_running, &
            'contained async cancellation preserves same-group sentinel')

        driver = environment_value('FO_BIN')
        call make_directory(trim(root)//'/project/src')
        call write_text(trim(root)//'/project/fpm.toml', &
            'name = "contained_capture_owner"'//new_line('a'))
        call write_text(trim(root)//'/project/src/probe.f90', &
            'module contained_capture_probe'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'end module contained_capture_probe'//new_line('a'))
        args = string_list_t()
        call list_add(args, driver)
        call list_add(args, 'gremlin')
        call list_add(args, 'start')
        call list_add(args, '--dir')
        call list_add(args, trim(root)//'/project')
        call list_add(args, '--lane')
        call list_add(args, 'capture-owner')
        call list_add(args, '--random')
        call list_add(args, '1')
        call run_process(args, trim(root), result, timeout_ms=15000)
        call write_text(trim(root)//'/public-start.stdout', result%stdout)
        call write_text(trim(root)//'/public-start.stderr', result%stderr)
        call assert_true(.not. result%runner_failed .and. result%reaped .and. &
            result%exit_code == 0, 'public Gremlin start completes in capture monitor')
        call json_parse(result%stdout, cli_json, json_valid, json_error)
        call assert_true(json_valid, 'public start returns valid JSON: '//json_error)
        session_id = json_string_value(json_member(cli_json, 'session_id'))
        call assert_true(len(session_id) > 0, 'public start returns its owner session')
        separator = index(session_id, '-')
        cli_owner_pid = -1
        if (separator > 1) then
            read(session_id(:separator - 1), *, iostat=cli_status) cli_owner_pid
            if (cli_status /= 0) cli_owner_pid = -1
        end if
        call assert_true(cli_owner_pid > 0, 'public session identifies its owner process')
        cli_owner_start = 0_c_int64_t
        if (cli_owner_pid > 0) cli_owner_start = mcp_process_start_time(cli_owner_pid)
        call assert_true(cli_owner_start > 0_c_int64_t, &
            'public session owner has a recorded process birth identity')
        call write_identity(trim(root)//'/public-owner.pid', cli_owner_pid)
        cli_owner_live = cli_owner_pid > 0 .and. cli_owner_start > 0_c_int64_t
        if (cli_owner_live) cli_owner_live = &
            mcp_process_identity_running(cli_owner_pid, cli_owner_start)
        call assert_true(cli_owner_live, &
            'resident owner survives the completed public start capture')

        args = string_list_t()
        call list_add(args, driver)
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--detail')
        call list_add(args, 'full')
        call list_add(args, '--dir')
        call list_add(args, trim(root)//'/project')
        call list_add(args, '--lane')
        call list_add(args, 'capture-owner')
        call list_add(args, '--session')
        call list_add(args, session_id)
        call run_process(args, trim(root), result, timeout_ms=10000)
        call write_text(trim(root)//'/public-status.stdout', result%stdout)
        call write_text(trim(root)//'/public-status.stderr', result%stderr)
        call assert_true(.not. result%runner_failed .and. result%reaped .and. &
            result%exit_code == 0, 'public status attaches after start capture exits')
        cli_owner_live = cli_owner_pid > 0 .and. cli_owner_start > 0_c_int64_t
        if (cli_owner_live) cli_owner_live = &
            mcp_process_identity_running(cli_owner_pid, cli_owner_start)
        call assert_true(cli_owner_live, 'status observes the resident owner alive')

        args = string_list_t()
        call list_add(args, driver)
        call list_add(args, 'gremlin')
        call list_add(args, 'stop')
        call list_add(args, '--dir')
        call list_add(args, trim(root)//'/project')
        call list_add(args, '--lane')
        call list_add(args, 'capture-owner')
        call list_add(args, '--session')
        call list_add(args, session_id)
        call run_process(args, trim(root), result, timeout_ms=10000)
        call write_text(trim(root)//'/public-stop.stdout', result%stdout)
        call write_text(trim(root)//'/public-stop.stderr', result%stderr)
        call assert_true(.not. result%runner_failed .and. result%reaped .and. &
            result%exit_code == 0, 'public stop requests resident owner teardown')
        cli_stopped = .false.
        do cli_attempt = 1, 100
            cli_owner_live = cli_owner_pid > 0 .and. cli_owner_start > 0_c_int64_t
            if (cli_owner_live) cli_owner_live = &
                mcp_process_identity_running(cli_owner_pid, cli_owner_start)
            if (.not. cli_owner_live) exit
            call fs_sleep_ms(20)
        end do
        args = string_list_t()
        call list_add(args, driver)
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--detail')
        call list_add(args, 'full')
        call list_add(args, '--dir')
        call list_add(args, trim(root)//'/project')
        call list_add(args, '--lane')
        call list_add(args, 'capture-owner')
        call list_add(args, '--session')
        call list_add(args, session_id)
        call run_process(args, trim(root), result, timeout_ms=10000)
        call write_text(trim(root)//'/public-stopped-status.stdout', result%stdout)
        call write_text(trim(root)//'/public-stopped-status.stderr', result%stderr)
        if (.not. result%runner_failed .and. result%exit_code == 0) then
            call json_parse(result%stdout, cli_json, json_valid, json_error)
            cli_stopped = json_valid .and. &
                json_string_value(json_member(cli_json, 'state')) == 'stopped'
        end if
        call assert_true(cli_stopped, 'public status observes completed owner stop')
        if (cli_owner_pid > 0 .and. cli_owner_start > 0_c_int64_t) &
            call assert_true(.not. mcp_process_identity_running(cli_owner_pid, &
                cli_owner_start), 'publicly stopped owner identity is no longer running')

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
        call write_identity(trim(root)//'/io-monitor.pid', monitor_pid, result%process_start_time)

        args = string_list_t()
        call list_add(args, trim(executable))
        call list_add(args, '--signal-helper')
        call run_process(args, trim(root), result, timeout_ms=5000)
        call assert_true(.not. result%runner_failed .and. result%reaped, &
            'signal helper starts and is reaped')
        if (fs_is_windows()) then
            call assert_equal_integer(result%exit_code, 143, 'preserves native forced process exit code')
            call assert_equal_integer(result%term_signal, 0, 'native exit does not invent a POSIX signal')
        else
            call assert_equal_integer(result%term_signal, 15, 'preserves target termination signal')
        end if
        monitor_pid = result%process_id
        call write_identity(trim(root)//'/signal-monitor.pid', monitor_pid, result%process_start_time)

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
        call write_identity(trim(root)//'/timeout-monitor.pid', monitor_pid, result%process_start_time)
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
        call assert_pid_absent(trim(root)//'/async-monitor.pid', &
            'contained test leaves no completed async monitor')
        call assert_pid_absent(trim(root)//'/async-cancel-monitor.pid', &
            'contained test leaves no cancelled async monitor')
        call assert_pid_absent(trim(root)//'/async-target.pid', &
            'contained test leaves no async target')
        call assert_pid_absent(trim(root)//'/async-descendant.pid', &
            'contained test leaves no async descendant')
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

    subroutine async_exit_helper()
        write(output_unit, '(a)') 'async target output'
        call process_exit(7)
    end subroutine async_exit_helper

    subroutine signal_helper()
        integer(c_int) :: rc
        rc = c_terminate_self()
        if (rc /= 0) call process_exit(81)
        call process_exit(82)
    end subroutine signal_helper

    integer function seccomp_mode() result(mode)
        !! The Seccomp field of /proc/self/status, or -1 when unavailable.
        character(len=256) :: line
        integer :: unit, ios

        mode = -1
        open (newunit=unit, file='/proc/self/status', status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'Seccomp:') /= 1) cycle
            read (line(9:), *, iostat=ios) mode
            if (ios /= 0) mode = -1
            exit
        end do
        close (unit)
    end function seccomp_mode

    subroutine timeout_helper()
        character(len=4096) :: target_path, ready_path, descendant_path, heartbeat_path
        character(len=32) :: pid_text
        integer :: child, attempt
        type(string_list_t) :: args
        call get_command_argument(2, target_path)
        call get_command_argument(3, descendant_path)
        call get_command_argument(4, ready_path)
        call get_command_argument(5, heartbeat_path)
        call write_identity(trim(target_path), process_getpid())
        call list_add(args, trim(executable))
        call list_add(args, '--orphan-intermediate')
        call list_add(args, trim(descendant_path))
        call list_add(args, trim(ready_path))
        call list_add(args, trim(heartbeat_path))
        call spawn_unowned_process(args, trim(cwd), child)
        if (child <= 0) call process_exit(83)
        do attempt = 1, 200
            if (file_exists(trim(ready_path))) exit
            call fs_sleep_ms(10)
        end do
        if (.not. file_exists(trim(ready_path))) call process_exit(85)
        do
            call fs_sleep_ms(100)
        end do
    end subroutine timeout_helper

    subroutine orphan_intermediate()
        character(len=4096) :: value
        type(string_list_t) :: args
        integer :: child, i
        call list_add(args, trim(executable))
        call list_add(args, '--orphan-leaf')
        do i = 2, 4
            call get_command_argument(i, value)
            call list_add(args, trim(value))
        end do
        call spawn_unowned_process(args, trim(cwd), child)
        if (child <= 0) call process_exit(84)
        call process_exit(0)
    end subroutine orphan_intermediate

    subroutine orphan_leaf()
        character(len=4096) :: descendant_path, ready_path, heartbeat_path
        character(len=32) :: pid_text
        integer(c_int) :: rc
        call get_command_argument(2, descendant_path)
        call get_command_argument(3, ready_path)
        call get_command_argument(4, heartbeat_path)
        rc = c_ignore_term()
        if (rc /= 0) call process_exit(84)
        call write_identity(trim(descendant_path), process_getpid())
        call write_text(trim(heartbeat_path), 'started'//new_line('a'))
        call write_text(trim(ready_path), 'ready')
        do
            call append_text(trim(heartbeat_path), 'beat'//new_line('a'))
            call fs_sleep_ms(20)
        end do
    end subroutine orphan_leaf

    subroutine write_identity(path, pid, observed_birth)
        character(len=*), intent(in) :: path
        integer, intent(in) :: pid
        integer(c_int64_t), optional, intent(in) :: observed_birth
        character(len=96) :: text
        integer(c_int64_t) :: birth
        birth = 0
        if (present(observed_birth)) then
            birth = observed_birth
        else if (pid > 0) then
            birth = mcp_process_start_time(pid)
        end if
        write(text, '(i0,1x,i0)') pid, birth
        call write_text(path, trim(text))
    end subroutine write_identity

    subroutine read_identity(path, pid, birth)
        character(len=*), intent(in) :: path
        integer, intent(out) :: pid
        integer(c_int64_t), intent(out) :: birth
        integer :: unit, ios
        pid = -1
        birth = 0
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(unit, *, iostat=ios) pid, birth
        close(unit)
        if (ios /= 0) then
            pid = -1
            birth = 0
        end if
    end subroutine read_identity

    subroutine stop_exact(path)
        character(len=*), intent(in) :: path
        integer :: pid, status
        integer(c_int64_t) :: birth
        call read_identity(path, pid, birth)
        if (pid <= 0 .or. birth <= 0) return
        if (.not. mcp_process_identity_running(pid, birth)) return
        call mcp_kill_owned_tree(pid, birth, status)
    end subroutine stop_exact

    subroutine assert_pid_absent(path, label)
        character(len=*), intent(in) :: path, label
        integer :: pid
        integer(c_int64_t) :: birth
        call read_identity(path, pid, birth)
        call assert_true(pid > 0 .and. birth > 0, label//' (exact process identity was recorded)')
        if (pid > 0 .and. birth > 0) &
            call assert_true(.not. mcp_process_identity_running(pid, birth), label)
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


end program test_native_contained_capture
