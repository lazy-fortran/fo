program test_mcp_session_cleanup
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, current_directory, write_text
    use fo_test_harness, only: run_process, file_exists, assert_true
    use fo_test_harness, only: assert_contains, assert_equal_integer
    use fo_test_harness, only: finish_assertions
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start
    use fo_test_mcp_session, only: mcp_session_shutdown
    use fo_test_process_identity, only: mcp_process_identity_running
    use fo_test_process_identity, only: mcp_kill_owned_tree, mcp_process_start_time
    implicit none

    character(len=4096) :: argument, executable_buffer, directory_buffer
    character(:), allocatable :: executable, scratch, cwd
    character(len=128) :: identity
    type(string_list_t) :: args
    type(process_result_t) :: result
    type(mcp_session_t) :: session
    integer :: unit, status, pid, exit_code, child_pid
    integer(c_int64_t) :: start_time, child_start

    interface
        integer(c_int) function c_fork() bind(C, name='fork')
            import :: c_int
        end function c_fork
        integer(c_int) function c_sleep(seconds) bind(C, name='sleep')
            import :: c_int
            integer(c_int), value :: seconds
        end function c_sleep
    end interface

    call get_command_argument(1, argument)
    if (trim(argument) == 'mcp-server') then
        child_pid = int(c_fork())
        if (child_pid < 0) error stop 'cannot create owned peer descendant'
        if (child_pid == 0) then
            do
                status = int(c_sleep(1_c_int))
            end do
        end if
        child_start = mcp_process_start_time(child_pid)
        write(identity, '(i0,1x,i0)') child_pid, child_start
        call write_text('peer.child.identity', trim(identity)//new_line('a'))
        ! Deliberately accept no response and remain owned until terminated.
        do
            read(*, '(a)', iostat=status) argument
            ! EOF alone must not hide a forgotten live peer in the outer oracle.
            if (status /= 0) status = int(c_sleep(1_c_int))
        end do
    end if
    call get_command_argument(0, executable_buffer)
    executable = trim(executable_buffer)
    if (len(executable) == 0) error stop 'missing native test executable'
    if (executable(1:1) /= '/') then
        call current_directory(cwd)
        executable = cwd//'/'//executable
    end if
    if (trim(argument) == '--shutdown-probe') then
        call get_command_argument(2, directory_buffer)
        scratch = trim(directory_buffer)
        call mcp_session_start(session, executable, scratch, scratch//'/cache', &
            scratch//'/state', scratch//'/peer.stderr')
        call assert_true(mcp_process_identity_running(session%pid, &
            session%start_time), 'unresponsive native peer is initially live')
        write(identity, '(i0,1x,i0)') session%pid, session%start_time
        call write_text(scratch//'/peer.identity', trim(identity)//new_line('a'))
        call mcp_session_shutdown(session, exit_code)
        call assert_equal_integer(session%handle, 0, &
            'failed graceful shutdown still closes a reaped owned session')
        call write_text(scratch//'/probe.returned', 'returned'//new_line('a'))
        ! The response/wait timeout remains a real assertion failure.
        call finish_assertions()
        error stop 'unresponsive peer unexpectedly passed'
    end if

    call make_scratch('fo-mcp-shutdown-cleanup', scratch)
    call list_add(args, executable)
    call list_add(args, '--shutdown-probe')
    call list_add(args, scratch)
    call run_process(args, scratch, result, timeout_ms=20000)
    call assert_true(.not. result%runner_failed .and. result%reaped, &
        'shutdown probe returns and is reaped within its own bound')
    call assert_true(result%exit_code /= 0, &
        'unresponsive peer keeps shutdown timeout as a failing result')
    call assert_contains(result%stderr, &
        'MCP server exits and is reaped after shutdown', &
        'probe reports the actual shutdown wait failure')
    call assert_true(file_exists(scratch//'/probe.returned'), &
        'shutdown helper returns after scoped cleanup')
    pid = 0
    start_time = 0_c_int64_t
    open(newunit=unit, file=scratch//'/peer.identity', status='old', &
        action='read', iostat=status)
    if (status == 0) then
        read(unit, *, iostat=status) pid, start_time
        close(unit)
    end if
    call assert_true(status == 0 .and. pid > 0 .and. start_time > 0_c_int64_t, &
        'probe publishes the independently observable exact peer identity')
    if (status == 0 .and. pid > 0 .and. start_time > 0_c_int64_t) then
        call assert_true(.not. mcp_process_identity_running(pid, start_time), &
            'shutdown failure leaves no live owned peer')
        if (mcp_process_identity_running(pid, start_time)) then
            call mcp_kill_owned_tree(pid, start_time, status)
            call assert_equal_integer(status, 0, &
                'negative oracle cleans its exact leaked peer after recording failure')
        end if
    end if
    child_pid = 0
    child_start = 0_c_int64_t
    open(newunit=unit, file=scratch//'/peer.child.identity', status='old', &
        action='read', iostat=status)
    if (status == 0) then
        read(unit, *, iostat=status) child_pid, child_start
        close(unit)
    end if
    call assert_true(status == 0 .and. child_pid > 0 .and. child_start > 0_c_int64_t, &
        'peer publishes its independently observable exact descendant identity')
    if (status == 0 .and. child_pid > 0 .and. child_start > 0_c_int64_t) then
        call assert_true(.not. mcp_process_identity_running(child_pid, child_start), &
            'shutdown failure leaves no live owned descendant')
        if (mcp_process_identity_running(child_pid, child_start)) then
            call mcp_kill_owned_tree(child_pid, child_start, status)
            call assert_equal_integer(status, 0, &
                'negative oracle cleans its exact leaked descendant after failure')
        end if
    end if
    call finish_assertions(retain_failed_scratch=.true.)
end program test_mcp_session_cleanup
