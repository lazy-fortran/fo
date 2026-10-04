program test_change_watch_lifecycle
    !! Verify the real provider, then exercise the public CLI lifecycle boundary.
    use fo_change_watch, only: change_watch_t, change_watch_init, change_watch_close
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_write_text, fs_sleep_ms
    use fo_util, only: make_tmpfile, extract_json_field
    implicit none

    type(change_watch_t) :: watch
    character(len=4096) :: scratch, fixture, executable, message, log_file
    character(len=65536) :: output
    character(len=32) :: platform
    character(len=128) :: session_id
    character(len=:), allocatable :: prefix
    integer :: status, unit, ios, attempt
    logical :: darwin

    call get_environment_variable('FO', executable, status=ios)
    if (ios /= 0 .or. len_trim(executable) == 0) &
        error stop 'FO must identify the exact worktree CLI under test'
    call make_tmpfile('fo-change-lifecycle', scratch)
    scratch = '/var/tmp/'//scratch(index(trim(scratch), '/', back=.true.) + 1:)
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
    darwin = trim(platform) == 'Darwin'

    call change_watch_init(watch, trim(fixture), status, message)
    call require(status == 0, 'lifecycle provider setup: '//trim(message))
    call change_watch_close(watch)
    print '(a)', trim(platform)//' lifecycle: shared watcher initialization passed'
    prefix = 'FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 FO_GREMLIN_STATE_DIR='// &
        quote(trim(scratch)//'/state')//' '//quote(trim(executable))//' gremlin '
    if (darwin) then
        ! Foreground run avoids the detached-owner launch barrier. Its first
        ! candidate build reaches containment only after real watcher setup and
        ! generation capture, so this observes the public boundary directly.
        call cli('run --random-count 1 --campaign-seconds 2', status, output, &
            'foreground-174')
        print '(a,i0,2a)', 'public foreground exit=', status, ' ', trim(output)
        call require(status /= 0, 'Darwin foreground containment blocker disappeared')
        call require(index(output, 'initial candidate build') > 0, &
            'Darwin foreground lifecycle failed before candidate build')
        call require(index(output, 'process setup error 45') > 0, &
            'Darwin foreground lifecycle lost the ENOTSUP containment boundary')
        call require(index(output, 'change watcher') == 0, &
            'Darwin foreground lifecycle failed during watcher setup')
    end if
    call cli('start --random-count 1 --campaign-seconds 2', status, output)
    print '(a,i0,2a)', 'public start exit=', status, ' ', trim(output)
    if (darwin) then
        call require(status /= 0, 'Darwin containment blocker unexpectedly disappeared')
        call require(index(output, 'process setup error 45') > 0, &
            'Darwin start failed outside the known ENOTSUP containment boundary')
        call require(index(output, 'watcher') == 0, &
            'Darwin public start still failed during watcher setup')
        call cli('status', status, output)
        print '(a,i0,2a)', 'public status exit=', status, ' ', trim(output)
        call require(status /= 0, 'unlaunched Darwin owner invented ready state')
        call cli('stop', status, output)
        print '(a,i0,2a)', 'public stop exit=', status, ' ', trim(output)
        call require(status /= 0, 'unlaunched Darwin owner invented stoppable state')
        print '(a)', 'Darwin lifecycle reached separate #172 process containment boundary'
    else
        call require(status == 0, 'Linux public start failed')
        call require(index(output, '"session_id":"') > 0, &
            'Linux public start did not publish an owner session')
        call extract_json_field(output, 'session_id', session_id)
        call require(len_trim(session_id) > 0, 'Linux start published an empty session')
        call cli('status', status, output)
        print '(a,i0,2a)', 'public status exit=', status, ' ', trim(output)
        call require(status == 0, 'Linux public ready state was not readable')
        do attempt = 1, 100
            call cli('status', status, output)
            call require(status == 0, 'Linux owner disappeared after watcher setup')
            if (index(output, '"state":"starting"') == 0) exit
            call fs_sleep_ms(50)
        end do
        call require(attempt <= 100, 'Linux lifecycle did not advance beyond watcher setup')
        call require(index(output, '"health":"error"') == 0, &
            'Linux lifecycle hit an infrastructure error after watcher setup')
        call require(index(output, '"last_outcome":"INFRA_ERROR"') == 0, &
            'Linux lifecycle hit a process setup error')
        call cli('stop', status, output)
        print '(a,i0,2a)', 'public stop exit=', status, ' ', trim(output)
        call require(status == 0, 'Linux public stop failed')
        do attempt = 1, 100
            call cli('status --session '//trim(session_id), status, output)
            call require(status == 0, 'Linux stopped state was not readable')
            if (index(output, '"state":"stopped"') > 0) exit
            call fs_sleep_ms(50)
        end do
        call require(attempt <= 100, 'Linux public stop did not terminate the owner')
        print '(a)', 'Linux public lifecycle start/status/stop passed'
    end if
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
            ' --lane '//trim(selected_lane)//' --json > '//quote(trim(log_file))//' 2>&1', &
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
