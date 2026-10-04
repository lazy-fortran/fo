program test_gremlin_process_boundary_slow
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_fs, only: fs_make_dir, fs_sleep_ms
    use fo_process, only: process_getpid
    use fo_gremlin_state, only: gremlin_session_read
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        list_with_first, run_process, make_scratch, join_path, make_directory, &
        current_directory, write_text, read_text, file_exists, environment_value, &
        assert_true, assert_contains, assert_equal_integer, finish_assertions, &
        start_sentinel, stop_sentinel, process_alive
    use fo_test_json, only: json_value_t, json_parse, json_member, json_string_value
    implicit none

    interface
        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
        integer(c_int) function c_kill(pid, signal_number) bind(C, name='kill')
            import :: c_int
            integer(c_int), value :: pid, signal_number
        end function c_kill
        integer(c_int) function c_process_matches(pid, start) &
                bind(C, name='fo_gremlin_process_matches')
            import :: c_char, c_int
            integer(c_int), value :: pid
            character(kind=c_char), intent(in) :: start(*)
        end function c_process_matches
        integer(c_int) function c_remove_frozen_tree(path) &
                bind(C, name='fo_c_generation_remove_stage')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_remove_frozen_tree
    end interface

    character(:), allocatable :: scratch, root, project, state_dir, cache_dir
    character(:), allocatable :: tmp_dir, driver, marker_stop, marker_crash
    character(:), allocatable :: marker_recovered, fixture_manifest, fixture_source
    character(:), allocatable :: fixture_signal
    character(:), allocatable :: lane_stop, lane_crash, session_stop, session_crash
    character(:), allocatable :: session_recovered
    character(len=128) :: owner_start, owner_session
    character(len=65536) :: status_text
    integer :: owner_pid, sentinel_pid, ierr, rc, pids(3), attempt
    type(process_result_t) :: result
    type(json_value_t) :: document, field
    logical :: markers_ready

    call current_directory(root)
    driver = environment_value('FO_BIN')
    call assert_true(len_trim(driver) > 0 .and. file_exists(driver), &
        'FO_BIN names the exact worktree-built Fo candidate')
    if (len_trim(driver) == 0 .or. .not. file_exists(driver)) goto 900

    call make_scratch('fo-gremlin-process-boundary', scratch)
    project = join_path(scratch, 'project')
    state_dir = join_path(scratch, 'state')
    cache_dir = join_path(scratch, 'cache')
    tmp_dir = join_path(scratch, 'tmp')
    marker_stop = join_path(scratch, 'normal-stop')
    marker_crash = join_path(scratch, 'crash')
    marker_recovered = join_path(scratch, 'recovered')
    lane_stop = 'process-stop-'//integer_text(process_getpid())
    lane_crash = 'process-crash-'//integer_text(process_getpid())
    call make_directory(project)
    call make_directory(join_path(project, 'test'))
    call make_directory(state_dir)
    call make_directory(cache_dir)
    call make_directory(tmp_dir)
    fixture_manifest = read_text(join_path(root, &
        'test-fixtures/gremlin_process_boundary/fpm.toml'))
    fixture_source = read_text(join_path(root, &
        'test-fixtures/gremlin_process_boundary/test_process_boundary.f90'))
    fixture_signal = read_text(join_path(root, &
        'test-fixtures/gremlin_process_boundary/fo_boundary_signal.f90'))
    call assert_true(len_trim(fixture_manifest) > 0 .and. len_trim(fixture_source) > 0 &
        .and. len_trim(fixture_signal) > 0, &
        'Fortran lifecycle fixture sources are available')
    call write_text(join_path(project, 'fpm.toml'), fixture_manifest)
    call write_text(join_path(project, 'test/test_process_boundary.f90'), fixture_source)
    call write_text(join_path(project, 'test/fo_boundary_signal.f90'), fixture_signal)
    call initialize_fixture_repository(project)
    rc = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, state_dir//c_null_char, 1_c_int)
    call assert_equal_integer(rc, 0, 'test state is isolated under its scratch root')
    call start_sentinel(sentinel_pid)

    call run_cli('start', project, lane_stop, '', marker_stop, driver, &
        state_dir, cache_dir, tmp_dir, result)
    call check_process(result, 'starts the public Gremlin owner')
    session_stop = response_session(result%stdout)
    call assert_true(len_trim(session_stop) > 0, 'start returns a session identity')
    if (len_trim(session_stop) == 0) goto 800
    markers_ready = wait_for_process_markers(marker_stop, pids)
    call assert_true(markers_ready, 'public owner launches its test descendants')
    if (.not. markers_ready) goto 800

    call run_cli('status', project, lane_stop, session_stop, marker_stop, driver, &
        state_dir, cache_dir, tmp_dir, result)
    call check_process(result, 'public status reads the exact running session')
    call assert_contains(result%stdout, session_stop, 'status preserves the session ID')
    call read_owner(project, lane_stop, owner_session, owner_pid, owner_start, ierr, &
        status_text)
    call assert_equal_integer(ierr, 0, 'reads exact owner PID/start identity')
    call assert_true(owner_session == session_stop .and. &
        c_process_matches(int(owner_pid, c_int), trim(owner_start)//c_null_char) /= 0, &
        'public owner start identity matches before stop')
    call assert_true(process_alive(sentinel_pid), 'unrelated sentinel runs before stop')
    call stop_and_check(project, lane_stop, session_stop, marker_stop, driver, &
        state_dir, cache_dir, tmp_dir, sentinel_pid)

    call run_cli('start', project, lane_crash, '', marker_crash, driver, &
        state_dir, cache_dir, tmp_dir, result)
    call check_process(result, 'starts a second public owner for crash recovery')
    session_crash = response_session(result%stdout)
    call assert_true(len_trim(session_crash) > 0, 'second start returns a session ID')
    if (len_trim(session_crash) == 0) goto 800
    markers_ready = wait_for_process_markers(marker_crash, pids)
    call assert_true(markers_ready, 'second owner launches its exact test session')
    if (.not. markers_ready) goto 800
    call read_owner(project, lane_crash, owner_session, owner_pid, owner_start, ierr, &
        status_text)
    call assert_equal_integer(ierr, 0, 'reads crash-owner PID/start identity')
    rc = c_process_matches(int(owner_pid, c_int), trim(owner_start)//c_null_char)
    call assert_true(owner_session == session_crash .and. rc /= 0, &
        'crash owner identity is exact before SIGKILL')
    if (rc == 0 .or. owner_pid <= 0) goto 800
    rc = c_kill(int(owner_pid, c_int), 9_c_int)
    call assert_equal_integer(rc, 0, 'abruptly kills only the verified owner PID')
    do attempt = 1, 100
        if (c_process_matches(int(owner_pid, c_int), &
            trim(owner_start)//c_null_char) == 0) exit
        call fs_sleep_ms(20)
    end do
    call assert_true(attempt <= 100, 'abrupt owner no longer matches its start identity')

    call run_cli('start', project, lane_crash, '', marker_recovered, driver, &
        state_dir, cache_dir, tmp_dir, result)
    call check_process(result, 'public start recovers the stale owner')
    session_recovered = response_session(result%stdout)
    call assert_true(len_trim(session_recovered) > 0 .and. &
        session_recovered /= session_crash, 'recovery publishes a new owner session')
    call assert_quiet(marker_crash, 'stale recovery stops old nested descendants')
    call assert_true(process_alive(sentinel_pid), &
        'unrelated sentinel survives stale-owner recovery')
    markers_ready = wait_for_process_markers(marker_recovered, pids)
    call assert_true(markers_ready, 'recovered owner launches its new test session')
    if (markers_ready) then
        call run_cli('status', project, lane_crash, session_recovered, marker_recovered, &
            driver, state_dir, cache_dir, tmp_dir, result)
        call check_process(result, 'status reads the recovered public session')
        call stop_and_check(project, lane_crash, session_recovered, marker_recovered, &
            driver, state_dir, cache_dir, tmp_dir, sentinel_pid)
    end if

800 if (allocated(session_stop)) call cleanup_public_owner(project, lane_stop, &
        session_stop, marker_stop, driver, state_dir, cache_dir, tmp_dir)
    if (allocated(session_recovered)) call cleanup_public_owner(project, lane_crash, &
        session_recovered, marker_recovered, driver, state_dir, cache_dir, tmp_dir)
    if (allocated(session_crash)) call cleanup_public_owner(project, lane_crash, &
        session_crash, marker_crash, driver, state_dir, cache_dir, tmp_dir)
    call stop_sentinel(sentinel_pid)
    call assert_true(.not. process_alive(sentinel_pid), 'sentinel cleanup is complete')
    if (allocated(scratch)) then
        rc = c_remove_frozen_tree(trim(scratch)//c_null_char)
        call assert_equal_integer(rc, 0, &
            'removes the isolated frozen project, state, and cache tree')
    end if
900 call finish_assertions()

contains

    subroutine cleanup_public_owner(project_dir, lane, known_session, marker, &
            executable, state_path, cache_path, temp_path)
        character(len=*), intent(in) :: project_dir, lane, known_session, marker
        character(len=*), intent(in) :: executable, state_path, cache_path, temp_path
        character(len=128) :: session, owner_start, owner_status
        character(len=512) :: message
        integer :: owner, ierr
        character(:), allocatable :: state
        type(process_result_t) :: child

        session = trim(known_session)
        if (len_trim(session) == 0) then
            call gremlin_session_read(project_dir, lane, session, owner, owner_start, &
                owner_status, ierr, message)
            if (ierr /= 0) return
        end if
        if (len_trim(session) == 0) return
        call run_cli('status', project_dir, lane, trim(session), marker, executable, &
            state_path, cache_path, temp_path, child)
        if (child%exit_code == 0) then
            state = response_state(child%stdout)
            if (state == 'stopped') return
        end if
        call run_cli('stop', project_dir, lane, trim(session), marker, executable, &
            state_path, cache_path, temp_path, child)
        if (child%exit_code /= 0 .and. allocated(child%stderr)) &
            write (*, '(a)') trim(child%stderr)
    end subroutine cleanup_public_owner

    subroutine initialize_fixture_repository(project_dir)
        character(len=*), intent(in) :: project_dir
        type(string_list_t) :: command

        command = string_list_t()
        call list_add(command, 'git')
        call list_add(command, '-C')
        call list_add(command, project_dir)
        call list_add(command, 'init')
        call list_add(command, '--quiet')
        call run_fixture_git(command, 'initializes a scratch source repository')

        command = string_list_t()
        call list_add(command, 'git')
        call list_add(command, '-C')
        call list_add(command, project_dir)
        call list_add(command, 'add')
        call list_add(command, '--')
        call list_add(command, 'fpm.toml')
        call list_add(command, 'test/test_process_boundary.f90')
        call list_add(command, 'test/fo_boundary_signal.f90')
        call run_fixture_git(command, 'stages the process fixture sources')

        command = string_list_t()
        call list_add(command, 'git')
        call list_add(command, '-C')
        call list_add(command, project_dir)
        call list_add(command, '-c')
        call list_add(command, 'user.name=Fo Boundary Test')
        call list_add(command, '-c')
        call list_add(command, 'user.email=fo-boundary@example.invalid')
        call list_add(command, 'commit')
        call list_add(command, '--quiet')
        call list_add(command, '-m')
        call list_add(command, 'isolated process boundary fixture')
        call run_fixture_git(command, 'commits the fixture for verified snapshotting')
    end subroutine initialize_fixture_repository

    subroutine run_fixture_git(command, context)
        type(string_list_t), intent(in) :: command
        character(len=*), intent(in) :: context
        type(process_result_t) :: child
        call run_process(command, root, child, timeout_ms=30000)
        call assert_equal_integer(child%exit_code, 0, context)
        if (child%exit_code /= 0) write (*, '(a)') trim(child%stderr)
    end subroutine run_fixture_git

    subroutine run_cli(action, project_dir, lane, session, marker, executable, &
            state_path, cache_path, temp_path, child)
        character(len=*), intent(in) :: action, project_dir, lane, session, marker
        character(len=*), intent(in) :: executable, state_path, cache_path, temp_path
        type(process_result_t), intent(out) :: child
        type(string_list_t) :: cli_args, cli_command, cli_env

        cli_args = string_list_t()
        call list_add(cli_args, 'gremlin')
        call list_add(cli_args, action)
        call list_add(cli_args, '--dir')
        call list_add(cli_args, project_dir)
        call list_add(cli_args, '--lane-id')
        call list_add(cli_args, lane)
        if (len_trim(session) > 0) then
            call list_add(cli_args, '--session-id')
            call list_add(cli_args, session)
        end if
        if (action == 'start') then
            call list_add(cli_args, '--target')
            call list_add(cli_args, 'test_process_boundary')
            call list_add(cli_args, '--random-count')
            call list_add(cli_args, '1')
            call list_add(cli_args, '--seed')
            call list_add(cli_args, '172')
            call list_add(cli_args, '--campaign-seconds')
            call list_add(cli_args, '60')
            call list_add(cli_args, '--timeout-seconds')
            call list_add(cli_args, '600')
            call list_add(cli_args, '--jobs')
            call list_add(cli_args, '1')
        end if
        call list_with_first(executable, cli_args, cli_command)
        call list_add(cli_env, 'FO_DISABLE_SELF_REFRESH=1')
        call list_add(cli_env, 'FO_SELF_REFRESH=0')
        call list_add(cli_env, 'FO_GREMLIN_STATE_DIR='//state_path)
        call list_add(cli_env, 'FO_CACHE_DIR='//cache_path)
        call list_add(cli_env, 'TMPDIR='//temp_path)
        call list_add(cli_env, 'FO_PROCESS_BOUNDARY_PREFIX='//marker)
        call run_process(cli_command, root, child, cli_env, timeout_ms=180000)
    end subroutine run_cli

    subroutine check_process(child, context)
        type(process_result_t), intent(in) :: child
        character(len=*), intent(in) :: context
        call assert_equal_integer(child%exit_code, 0, context//' exits successfully')
        if (child%exit_code /= 0) then
            call assert_contains(child%stderr, 'error', context//' reports its failure')
            write (*, '(a)') trim(child%stdout)
            write (*, '(a)') trim(child%stderr)
        end if
    end subroutine check_process

    function response_session(output) result(session)
        character(len=*), intent(in) :: output
        character(:), allocatable :: session, parse_error
        type(json_value_t) :: response, value
        logical :: parsed
        call json_parse(trim(output), response, parsed, parse_error)
        call assert_true(parsed, 'Gremlin command returns parseable JSON')
        session = ''
        if (.not. parsed) return
        value = json_member(response, 'session_id')
        session = json_string_value(value)
    end function response_session

    subroutine read_owner(project_dir, lane, session, pid, start, ierr, status)
        character(len=*), intent(in) :: project_dir, lane
        character(len=*), intent(out) :: session, start, status
        integer, intent(out) :: pid, ierr
        character(len=512) :: message
        call gremlin_session_read(project_dir, lane, session, pid, start, status, &
            ierr, message)
        if (ierr /= 0) call assert_true(.false., &
            'Gremlin owner state read: '//trim(message))
    end subroutine read_owner

    logical function wait_for_process_markers(prefix, process_ids)
        character(len=*), intent(in) :: prefix
        integer, intent(out) :: process_ids(3)
        integer :: attempt
        wait_for_process_markers = .false.
        process_ids = 0
        do attempt = 1, 1200
            process_ids(1) = read_pid(prefix//'.owner.pid')
            process_ids(2) = read_pid(prefix//'.nested.pid')
            process_ids(3) = read_pid(prefix//'.deep.pid')
            if (all(process_ids > 0) .and. &
                file_exists(prefix//'.deep.heartbeat')) then
                wait_for_process_markers = .true.
                return
            end if
            call fs_sleep_ms(100)
        end do
    end function wait_for_process_markers

    integer function read_pid(path)
        character(len=*), intent(in) :: path
        character(:), allocatable :: contents
        integer :: io_status
        read_pid = 0
        if (.not. file_exists(path)) return
        contents = read_text(path)
        read(contents, *, iostat=io_status) read_pid
        if (io_status /= 0) read_pid = 0
    end function read_pid

    subroutine assert_quiet(prefix, context)
        character(len=*), intent(in) :: prefix, context
        integer :: before(3), after(3)
        before(1) = count_lines(prefix//'.owner.heartbeat')
        before(2) = count_lines(prefix//'.nested.heartbeat')
        before(3) = count_lines(prefix//'.deep.heartbeat')
        call fs_sleep_ms(350)
        after(1) = count_lines(prefix//'.owner.heartbeat')
        after(2) = count_lines(prefix//'.nested.heartbeat')
        after(3) = count_lines(prefix//'.deep.heartbeat')
        call assert_true(all(before == after) .and. all(before > 0), context)
    end subroutine assert_quiet

    subroutine stop_and_check(project_dir, lane, session, marker, executable, &
            state_path, cache_path, temp_path, unrelated_pid)
        character(len=*), intent(in) :: project_dir, lane, session, marker
        character(len=*), intent(in) :: executable, state_path, cache_path, temp_path
        integer, intent(in) :: unrelated_pid
        integer :: owner, ierr, attempt, match
        character(len=128) :: owner_id, owner_birth
        character(len=65536) :: owner_status
        type(process_result_t) :: child
        character(:), allocatable :: state

        call read_owner(project_dir, lane, owner_id, owner, owner_birth, ierr, &
            owner_status)
        call assert_equal_integer(ierr, 0, 'reads owner before public stop')
        match = c_process_matches(int(owner, c_int), trim(owner_birth)//c_null_char)
        call assert_true(match /= 0 .and. owner_id == session, &
            'stop targets the exact current owner session')
        call run_cli('stop', project_dir, lane, session, marker, executable, &
            state_path, cache_path, temp_path, child)
        call check_process(child, 'public stop requests bounded process cleanup')
        call assert_contains(child%stdout, 'stopp', 'stop reports stopping state')

        do attempt = 1, 40
            call run_cli('status', project_dir, lane, session, marker, executable, &
                state_path, cache_path, temp_path, child)
            if (child%exit_code == 0) then
                state = response_state(child%stdout)
                if (state == 'stopped') exit
            end if
            call fs_sleep_ms(100)
        end do
        call assert_true(attempt <= 40 .and. child%exit_code == 0 .and. &
            response_state(child%stdout) == 'stopped', &
            'status observes the durable stopped session')
        call assert_quiet(marker, 'normal stop ends every nested process heartbeat')
        call assert_true(c_process_matches(int(owner, c_int), &
            trim(owner_birth)//c_null_char) == 0, 'normal stop releases its owner identity')
        call assert_true(process_alive(unrelated_pid), 'normal stop preserves unrelated sentinel')
    end subroutine stop_and_check

    function response_state(output) result(state)
        character(len=*), intent(in) :: output
        character(:), allocatable :: state
        type(json_value_t) :: response, value
        character(:), allocatable :: parse_error
        logical :: parsed
        state = ''
        call json_parse(trim(output), response, parsed, parse_error)
        if (.not. parsed) return
        value = json_member(response, 'state')
        state = json_string_value(value)
    end function response_state

    integer function count_lines(path)
        character(len=*), intent(in) :: path
        character(:), allocatable :: contents
        integer :: i
        count_lines = 0
        if (.not. file_exists(path)) return
        contents = read_text(path)
        do i = 1, len(contents)
            if (contents(i:i) == achar(10)) count_lines = count_lines + 1
        end do
    end function count_lines

    function integer_text(value) result(text_value)
        integer, intent(in) :: value
        character(:), allocatable :: text_value
        character(len=32) :: buffer
        write (buffer, '(i0)') value
        text_value = trim(buffer)
    end function integer_text
end program test_gremlin_process_boundary_slow
