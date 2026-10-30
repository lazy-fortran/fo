program test_gremlin_task_lane
    !! Task-owned lanes (#142): a lane started with `--owner task` (the
    !! MCP default) stops when its creating client ends, the CLI's
    !! parent process or the MCP connection; persistent lanes are unaffected
    !! and status reports the lifetime. This program doubles as the CLI's
    !! parent process when its first argument is --task-parent.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, read_text
    use fo_test_harness, only: assert_true, assert_equal_string, finish_assertions
    use fo_test_harness, only: resolve_test_executable, sleep_ms
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms, gremlin_write_case
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_wait_file
    use fo_test_gremlin_oracle, only: gremlin_spawn, gremlin_stop_child, gremlin_stop_lane
    use fo_test_gremlin_oracle, only: gremlin_owner_pid, gremlin_wait_exit
    use fo_test_gremlin_oracle, only: gremlin_process_running
    use fo_test_mcp, only: mcp_quote
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start, mcp_session_call
    use fo_test_mcp_session, only: mcp_session_request, mcp_session_notify
    use fo_test_mcp_session, only: mcp_session_shutdown
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_process, only: argv_push, process_run_argv_logged
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, self, marker
    character(:), allocatable :: text, task_session, persistent_session
    character(len=64) :: mode
    type(string_list_t) :: args
    type(json_value_t) :: document
    type(process_result_t) :: process
    integer :: helper, exit_code, task_owner, persistent_owner, newline
    logical :: ok, found

    mode = ''
    if (command_argument_count() >= 6) call get_command_argument(1, mode)
    if (trim(mode) == '--task-parent' .or. trim(mode) == '--sync-task-parent') &
        call run_as_parent()

    call gremlin_setup(driver, scratch, project, cache, state)
    call write_text(project//'/fpm.toml', 'name = "task_lane"'//new_line('a'))
    call gremlin_write_case(project, 'test_task', "print '(a)', 'task'")

    ! CLI: the lane follows its parent process.
    call resolve_test_executable(self, ok)
    call assert_true(ok, 'resolves the parent-process fixture')
    marker = scratch//'/sessions.txt'
    call list_add(args, '--task-parent')
    call list_add(args, driver)
    call list_add(args, project)
    call list_add(args, cache)
    call list_add(args, state)
    call list_add(args, marker)
    call gremlin_spawn(self, project, args, scratch//'/parent.out', &
        scratch//'/parent.err', helper)
    call gremlin_wait_file(marker, 60000, found)
    call assert_true(found, 'the parent fixture started a task and a persistent lane')
    if (.not. found) call finish_assertions(retain_failed_scratch=.true.)
    text = read_text(marker)
    newline = index(text, new_line('a'))
    task_session = text(:newline - 1)
    persistent_session = text(newline + 1:len(text) - 1)
    task_owner = gremlin_owner_pid(task_session)
    persistent_owner = gremlin_owner_pid(persistent_session)
    call status_lifetime('cli-task', 'task', 'CLI status reports the task lifetime')
    call status_lifetime('cli-persistent', 'persistent', &
        'CLI status reports the persistent lifetime')
    call gremlin_wait_ms(1500)
    call assert_true(gremlin_process_running(task_owner), &
        'task lane keeps running while its parent lives')
    call gremlin_stop_child(helper, exit_code)
    call assert_true(gremlin_wait_exit(task_owner, 20000), &
        'task lane owner exits after its parent process ends')
    call assert_true(gremlin_process_running(persistent_owner), &
        'persistent lane survives its parent process')
    call gremlin_stop_lane(driver, project, cache, state, 'cli-persistent', &
        persistent_session)

    call synchronous_cli()
    call mcp_connection()
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine synchronous_cli()
        type(string_list_t) :: sync_args
        character(:), allocatable :: sessions
        integer :: sync_parent, task_pid, persistent_pid, split, code
        logical :: ready

        marker = scratch//'/sync-sessions.txt'
        call list_add(sync_args, '--sync-task-parent')
        call list_add(sync_args, driver)
        call list_add(sync_args, project)
        call list_add(sync_args, cache)
        call list_add(sync_args, state)
        call list_add(sync_args, marker)
        call gremlin_spawn(self, project, sync_args, scratch//'/sync-parent.out', &
            scratch//'/sync-parent.err', sync_parent)
        call gremlin_wait_file(marker, 60000, ready)
        call assert_true(ready, 'synchronous parent starts its lanes')
        if (.not. ready) call finish_assertions(retain_failed_scratch=.true.)
        sessions = read_text(marker)
        split = index(sessions, new_line('a'))
        task_pid = gremlin_owner_pid(sessions(:split - 1))
        persistent_pid = gremlin_owner_pid(sessions(split + 1:len(sessions) - 1))
        call gremlin_wait_ms(1500)
        call assert_true(gremlin_process_running(task_pid), &
            'synchronous CLI task lane follows its live caller')
        call gremlin_stop_child(sync_parent, code)
        call assert_true(gremlin_wait_exit(task_pid, 20000), &
            'synchronous CLI task lane exits when its caller ends')
        call assert_true(gremlin_process_running(persistent_pid), &
            'synchronous CLI persistent lane survives its caller')
        call gremlin_stop_lane(driver, project, cache, state, 'cli-persistent', &
            sessions(split + 1:len(sessions) - 1))
    end subroutine synchronous_cli

    subroutine run_as_parent()
        !! The CLI's parent: start both lanes, publish their sessions, wait.
        character(len=4096) :: values(5)
        character(:), allocatable :: first, second
        integer :: i

        do i = 1, 5
            call get_command_argument(i + 1, values(i))
        end do
        call start_lane(values, 'cli-task', .true., first)
        call start_lane(values, 'cli-persistent', .false., second)
        call write_text(trim(values(5)), first//new_line('a')//second//new_line('a'))
        ! Self-bounded so a failed oracle cannot leave an immortal process.
        do i = 1, 1200
            call sleep_ms(100)
        end do
        stop 3
    end subroutine run_as_parent

    subroutine start_lane(v, lane, task, session)
        character(len=*), intent(in) :: v(:), lane
        logical, intent(in) :: task
        character(:), allocatable, intent(out) :: session
        type(string_list_t) :: start
        type(process_result_t) :: started
        type(json_value_t) :: reply
        logical :: valid
        character(:), allocatable :: message, packed, log_file
        integer :: i, count

        call gremlin_start_args(start, trim(v(2)), lane, 'test_task')
        if (task) then
            call list_add(start, '--owner')
            call list_add(start, 'task')
        end if
        if (trim(mode) == '--sync-task-parent') then
            packed = ''
            count = 0
            call argv_push(packed, count, trim(v(1)))
            do i = 1, size(start%items)
                call argv_push(packed, count, start%items(i)%value)
            end do
            log_file = trim(v(2))//'/'//lane//'.log'
            call process_run_argv_logged(trim(v(2)), packed, count, log_file, &
                .false., 60, started%exit_code, env_extra= &
                'FO_CACHE_DIR='//trim(v(3))//';FO_GREMLIN_STATE_DIR='//trim(v(4))// &
                ';FO_DISABLE_SELF_REFRESH=1;FO_SELF_REFRESH=0;FO_JOBS=1')
            started%stdout = read_text(log_file)
        else
            call gremlin_run(trim(v(1)), trim(v(2)), trim(v(3)), trim(v(4)), start, started)
        end if
        call json_parse(started%stdout, reply, valid, message)
        session = gremlin_field(reply, 'session_id')
        if (started%exit_code /= 0 .or. len(session) == 0) stop 4
    end subroutine start_lane

    subroutine status_lifetime(lane, expected, label)
        character(len=*), intent(in) :: lane, expected, label
        type(string_list_t) :: status_args

        call list_add(status_args, 'gremlin')
        call list_add(status_args, 'status')
        call list_add(status_args, '--dir')
        call list_add(status_args, project)
        call list_add(status_args, '--lane')
        call list_add(status_args, lane)
        call gremlin_json(driver, project, cache, state, status_args, document, process, &
            30000)
        call assert_equal_string(gremlin_field(document, 'lifetime'), expected, label)
    end subroutine status_lifetime

    subroutine mcp_connection()
        !! MCP: the lane follows the connection of the server that created it.
        type(mcp_session_t) :: server
        type(json_value_t) :: response
        character(:), allocatable :: task_id, default_id, persistent_id
        integer :: task_pid, default_pid, persistent_pid, code
        logical :: default_stopped

        call mcp_session_start(server, driver, project, cache, state, &
            scratch//'/mcp.stderr')
        call mcp_session_request(server, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}', response)
        call mcp_session_notify(server, 'notifications/initialized')
        call mcp_start(server, 'mcp-task', ',"lifetime":"task"', task_id)
        call mcp_start(server, 'mcp-default', '', default_id)
        call mcp_start(server, 'mcp-persistent', ',"lifetime":"persistent"', &
            persistent_id)
        call mcp_status(server, 'mcp-task', 'task', 'MCP status reports the task lifetime')
        call mcp_status(server, 'mcp-default', 'task', &
            'MCP status reports task ownership when lifetime is omitted')
        call mcp_status(server, 'mcp-persistent', 'persistent', &
            'MCP status reports the persistent lifetime')
        task_pid = gremlin_owner_pid(task_id)
        default_pid = gremlin_owner_pid(default_id)
        persistent_pid = gremlin_owner_pid(persistent_id)
        call gremlin_wait_ms(1500)
        call assert_true(gremlin_process_running(task_pid), &
            'MCP task lane keeps running while its connection lives')
        call assert_true(gremlin_process_running(default_pid), &
            'MCP default lane keeps running while its connection lives')
        call mcp_session_shutdown(server, code)
        call assert_true(gremlin_wait_exit(task_pid, 20000), &
            'MCP task lane owner exits after the connection ends')
        default_stopped = gremlin_wait_exit(default_pid, 20000)
        call assert_true(default_stopped, &
            'MCP default lane owner exits after the connection ends')
        if (.not. default_stopped) &
            call gremlin_stop_lane(driver, project, cache, state, 'mcp-default', &
            default_id)
        call assert_true(gremlin_process_running(persistent_pid), &
            'MCP persistent lane survives the connection')
        call gremlin_stop_lane(driver, project, cache, state, 'mcp-persistent', &
            persistent_id)
    end subroutine mcp_connection

    subroutine mcp_start(server, lane, extra, session)
        type(mcp_session_t), intent(inout) :: server
        character(len=*), intent(in) :: lane, extra
        character(:), allocatable, intent(out) :: session
        type(json_value_t) :: response, body
        logical :: valid
        character(:), allocatable :: message

        call mcp_session_call(server, '{"action":"gremlin_start","dir":'// &
            mcp_quote(project)//',"lane_id":"'//lane//'","targets":["test_task"]'// &
            ',"timeout_seconds":10'//extra//'}', response, timeout_ms=60000)
        call parse_tool_body(response, body, valid)
        session = gremlin_field(body, 'session_id')
        call assert_true(valid .and. len(session) > 0, 'MCP starts lane '//lane)
        if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)
    end subroutine mcp_start

    subroutine mcp_status(server, lane, expected, label)
        type(mcp_session_t), intent(inout) :: server
        character(len=*), intent(in) :: lane, expected, label
        type(json_value_t) :: response, body
        logical :: valid
        character(:), allocatable :: message

        call mcp_session_call(server, '{"action":"gremlin_status","dir":'// &
            mcp_quote(project)//',"lane_id":"'//lane//'"}', response, timeout_ms=30000)
        call parse_tool_body(response, body, valid)
        call assert_equal_string(gremlin_field(body, 'lifetime'), expected, label)
    end subroutine mcp_status

    subroutine parse_tool_body(response, body, valid)
        type(json_value_t), intent(in) :: response
        type(json_value_t), intent(out) :: body
        logical, intent(out) :: valid
        type(json_value_t) :: content
        character(:), allocatable :: message

        content = json_element(json_member(json_member(response, 'result'), &
            'content'), 1)
        call json_parse(gremlin_field(content, 'text'), body, valid, message)
    end subroutine parse_tool_body

end program test_gremlin_task_lane
