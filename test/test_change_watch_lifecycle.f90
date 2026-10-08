program test_change_watch_lifecycle
    !! Verify the real provider, then exercise the public CLI lifecycle boundary.
    use fo_change_watch, only: change_watch_t, change_watch_init, change_watch_close
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_write_text, fs_sleep_ms
    use fo_util, only: make_tmpfile
    use fo_test_json, only: json_value_t, json_parse, json_member, json_string_value
    implicit none

    type(change_watch_t) :: watch
    character(len=4096) :: scratch, fixture, executable, message, log_file
    character(len=65536) :: output
    character(len=32) :: platform
    character(len=128) :: session_id
    character(len=:), allocatable :: prefix
    character(len=:), allocatable :: parse_message
    type(json_value_t) :: response_document
    integer :: status, unit, ios, attempt
    logical :: valid_json

    call get_environment_variable('FO', executable, status=ios)
    if (ios /= 0 .or. len_trim(executable) == 0) &
        error stop 'FO must identify the exact worktree CLI under test'
    call make_tmpfile('fo-change-lifecycle', scratch)
    fixture = trim(scratch)//'/project'
    log_file = trim(scratch)//'/public.log'
    call fs_make_dir(trim(fixture)//'/test')
    call fs_write_text(trim(fixture)//'/fpm.toml', 'name = "watch_lifecycle"')
    call fs_write_text(trim(fixture)//'/test/test_smoke.f90', &
        'program smoke'//new_line('a')//'implicit none'//new_line('a')// &
        'if (2 + 2 /= 4) error stop 1'//new_line('a')//'end program smoke')
    call execute_command_line('uname -s > '//quote(trim(log_file)), exitstat=status)
    call require(status == 0, 'cannot identify lifecycle platform')
    open (newunit=unit, file=trim(log_file), status='old')
    read (unit, '(a)') platform
    close (unit)

    call change_watch_init(watch, trim(fixture), status, message)
    call require(status == 0, 'lifecycle provider setup: '//trim(message))
    call change_watch_close(watch)
    print '(a)', trim(platform)//' lifecycle: shared watcher initialization passed'
    ! One job keeps the fixture lane at host admission weight 1, like the other
    ! Gremlin fixtures, so it does not need every host work slot.
    prefix = 'FO_JOBS=1 FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 '// &
        'FO_GREMLIN_STATE_DIR='// &
        quote(trim(scratch)//'/state')//' '//quote(trim(executable))//' gremlin '
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
        integer :: file_unit, read_status, launch_error, used
        character(len=65536) :: line
        character(len=32) :: selected_lane

        selected_lane = 'watcher-174'
        if (present(lane)) selected_lane = lane
        call execute_command_line(prefix//arguments//' --dir '//quote(trim(fixture))// &
            ' --lane '//trim(selected_lane)//' > '//quote(trim(log_file))//' 2>&1', &
            exitstat=exitcode, cmdstat=launch_error)
        call require(launch_error == 0, 'cannot execute the worktree CLI')
        text = ''
        used = 0
        open (newunit=file_unit, file=trim(log_file), status='old')
        do
            read (file_unit, '(a)', iostat=read_status) line
            if (read_status /= 0) exit
            if (used + len_trim(line) + 1 > len(text)) &
                error stop 'lifecycle CLI output exceeded oracle capacity'
            text(used + 1:used + len_trim(line)) = trim(line)
            used = used + len_trim(line) + 1
            text(used:used) = new_line('a')
        end do
        close (file_unit)
    end subroutine cli

    function quote(text) result(quoted)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: quoted
        integer :: i
        quoted = "'"
        do i = 1, len_trim(text)
            if (text(i:i) == "'") then
                quoted = quoted//"'"//'"'//"'"//'"'//"'"
            else
                quoted = quoted//text(i:i)
            end if
        end do
        quoted = quoted//"'"
    end function quote
end program test_change_watch_lifecycle
