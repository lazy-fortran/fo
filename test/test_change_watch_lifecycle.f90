program test_change_watch_lifecycle
    !! Verify the real provider, then exercise the public CLI lifecycle boundary.
    use fo_change_watch, only: change_watch_t, change_watch_init, change_watch_close
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_write_text, fs_sleep_ms
    use fo_util, only: make_tmpfile
    use fo_test_json, only: json_value_t, json_parse, json_member, json_string_value
    use fo_test_harness, only: string_list_t, list_add, add_words, run_list
    implicit none

    type(change_watch_t) :: watch
    character(len=4096) :: scratch, fixture, executable, message
    character(len=65536) :: output
    character(len=32) :: platform
    character(len=128) :: session_id
    character(len=:), allocatable :: uname_text
    type(string_list_t) :: lifecycle_environment
    character(len=:), allocatable :: parse_message
    type(json_value_t) :: response_document
    integer :: status, ios, attempt
    logical :: valid_json

    call get_environment_variable('FO', executable, status=ios)
    if (ios /= 0 .or. len_trim(executable) == 0) &
        error stop 'FO must identify the exact worktree CLI under test'
    call make_tmpfile('fo-change-lifecycle', scratch)
    fixture = trim(scratch)//'/project'
    call fs_make_dir(trim(fixture)//'/test')
    call fs_write_text(trim(fixture)//'/fpm.toml', 'name = "watch_lifecycle"')
    call fs_write_text(trim(fixture)//'/test/test_smoke.f90', &
        'program smoke'//new_line('a')//'implicit none'//new_line('a')// &
        'if (2 + 2 /= 4) error stop 1'//new_line('a')//'end program smoke')
    call run_uname(uname_text, status)
    call require(status == 0, 'cannot identify lifecycle platform')
    platform = uname_text(:max(0, index(uname_text, new_line('a')) - 1))

    call change_watch_init(watch, trim(fixture), status, message)
    call require(status == 0, 'lifecycle provider setup: '//trim(message))
    call change_watch_close(watch)
    print '(a)', trim(platform)//' lifecycle: shared watcher initialization passed'
    ! One job keeps the fixture lane at host admission weight 1, like the other
    ! Gremlin fixtures, so it does not need every host work slot.
    call list_add(lifecycle_environment, 'FO_JOBS=1')
    call list_add(lifecycle_environment, 'FO_DISABLE_SELF_REFRESH=1')
    call list_add(lifecycle_environment, 'FO_SELF_REFRESH=0')
    call list_add(lifecycle_environment, 'FO_GREMLIN_STATE_DIR='//trim(scratch)//'/state')
    call cli('start --random 1 --campaign-seconds 2', status, output)
    print '(a,i0,2a)', 'public start exit=', status, ' ', trim(output)
    call require(status == 0, 'public start failed')
    call json_parse(output, response_document, valid_json, parse_message)
    call require(valid_json, 'start returned valid JSON: '//parse_message)
    session_id = json_string_value(json_member(response_document, 'session_id'))
    call require(len_trim(session_id) > 0, 'start published an empty session')
    call cli('status', status, output)
    print '(a,i0,2a)', 'public status exit=', status, ' ', trim(output)
    call require(status == 0, 'public ready state was not readable')
    ! Safety bound only: a vanished owner fails at once. The first build may
    ! wait for a host work slot held by other lanes or run slowly under load.
    do attempt = 1, 6000
        call cli('status', status, output)
        call require(status == 0, 'owner disappeared after watcher setup')
        if (index(output, '"state":"starting"') == 0) exit
        call fs_sleep_ms(50)
    end do
    call require(attempt <= 6000, &
        'lifecycle did not advance beyond watcher setup; last status: '// &
        trim(output))
    call require(index(output, '"health":"error"') == 0, &
        'lifecycle hit an infrastructure error after watcher setup')
    call require(index(output, '"last_outcome":"INFRA_ERROR"') == 0, &
        'lifecycle hit a process setup error')
    call cli('stop', status, output)
    print '(a,i0,2a)', 'public stop exit=', status, ' ', trim(output)
    call require(status == 0, 'public stop failed')
    do attempt = 1, 100
        call cli('status --session '//trim(session_id), status, output)
        call require(status == 0, 'stopped state was not readable')
        if (index(output, '"state":"stopped"') > 0) exit
        call fs_sleep_ms(50)
    end do
    call require(attempt <= 100, 'public stop did not terminate the owner')
    print '(a)', trim(platform)//' public lifecycle start/status/stop passed'
    call fs_remove_tree(trim(scratch))

contains

    subroutine require(ok, reason)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: reason
        if (.not. ok) then
            print '(a)', 'preserved lifecycle evidence: '//trim(scratch)
            error stop reason
        end if
    end subroutine require

    subroutine cli(arguments, exitcode, text, lane)
        character(len=*), intent(in) :: arguments
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: text
        character(len=*), intent(in), optional :: lane
        type(string_list_t) :: command
        character(len=:), allocatable :: out, err
        character(len=32) :: selected_lane

        selected_lane = 'watcher-174'
        if (present(lane)) selected_lane = lane
        call list_add(command, trim(executable))
        call list_add(command, 'gremlin')
        call add_words(command, arguments)
        call list_add(command, '--dir')
        call list_add(command, trim(fixture))
        call list_add(command, '--lane')
        call list_add(command, trim(selected_lane))
        call run_list(command, exitcode, out, err, environment=lifecycle_environment, &
            timeout_ms=-1)
        call require(exitcode >= 0, 'cannot execute the worktree CLI')
        if (len(out) + len(err) > len(text)) &
            error stop 'lifecycle CLI output exceeded oracle capacity'
        text = out // err
    end subroutine cli

    subroutine run_uname(text, status)
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        type(string_list_t) :: command

        call list_add(command, 'uname')
        call list_add(command, '-s')
        call run_list(command, status, text)
    end subroutine run_uname
end program test_change_watch_lifecycle
