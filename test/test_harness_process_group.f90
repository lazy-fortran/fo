program test_harness_process_group
    use fo_test_harness, only: string_list_t, process_result_t, list_add, run_process
    use fo_test_harness, only: make_scratch, join_path, read_text, file_exists
    use fo_test_harness, only: assert_process_ok, assert_true, assert_equal_string
    use fo_test_harness, only: finish_assertions, write_text, append_text, sleep_ms
    use fo_test_harness, only: spawn_unowned_process, start_sentinel, stop_sentinel, process_alive
    use fo_fs, only: fs_is_windows
    use fo_test_harness, only: resolve_test_executable
    use fo_process, only: process_getpid, process_exit
    use fo_test_process_identity, only: mcp_process_start_time, mcp_process_identity_running, mcp_kill_owned_tree
    use fo_test_os_link, only: test_os_initialize
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t
    use, intrinsic :: iso_fortran_env, only: output_unit

    implicit none

    type(string_list_t) :: command
    type(process_result_t) :: result
    character(:), allocatable :: scratch, heartbeat, before, host
    integer :: i, pid, sentinel, status, unit
    integer(c_int64_t) :: birth
    character(len=4096) :: mode, value
    character(len=96) :: identity
    character(:), allocatable :: executable
    logical :: resolved
    interface
        integer(c_int) function c_in_job() bind(C, name='fo_test_in_job')
            import :: c_int
        end function c_in_job
        integer(c_int) function c_ignore_term() bind(C, name='fo_test_ignore_term')
            import :: c_int
        end function c_ignore_term
    end interface

    call get_command_argument(1, mode)
    call resolve_test_executable(executable, resolved)
    if (.not. resolved) error stop 'cannot resolve native boundary oracle'
    select case (trim(mode))
    case ('--native-job-leaf')
        status = test_os_initialize()
        if (status /= 162 .or. c_in_job() /= 1) call process_exit(125)
        write(output_unit, '(i0,1x,i0)') process_getpid(), mcp_process_start_time(process_getpid())
        flush(output_unit)
        call process_exit(0)
    case ('--native-tree')
        call get_command_argument(2, value)
        call list_add(command, executable)
        call list_add(command, '--native-heartbeat')
        call list_add(command, trim(value))
        call spawn_unowned_process(command, '.', pid)
        do
            call sleep_ms(100)
        end do
    case ('--native-heartbeat')
        call get_command_argument(2, value)
        status = c_ignore_term()
        if (status /= 0) call process_exit(125)
        write(identity, '(i0,1x,i0)') process_getpid(), mcp_process_start_time(process_getpid())
        call write_text(trim(value)//'.identity', trim(identity))
        call write_text(trim(value), 'started')
        do
            call append_text(trim(value), 'beat')
            call sleep_ms(20)
        end do
    end select

    call make_scratch('fo-harness-process-group', scratch)
    if (fs_is_windows()) then
        call start_sentinel(sentinel)
        do i = 1, 32
            command = string_list_t()
            call list_add(command, executable)
            call list_add(command, '--native-job-leaf')
            call run_process(command, scratch, result)
            call assert_process_ok(result, 'native child executes in real Windows Job')
            read(result%stdout, *, iostat=status) pid, birth
            call assert_true(status == 0 .and. pid == result%process_id .and. &
                birth == result%process_start_time .and. birth > 0, &
                'child and independent parent observe exact native PID/FILETIME')
            call assert_true(.not. mcp_process_identity_running(result%process_id, &
                result%process_start_time), 'normal child completion consumes its exact identity')
            call assert_true(process_alive(sentinel), 'native launch preserves independent live peer')
        end do
        heartbeat = join_path(scratch, 'grandchild-heartbeat')
        command = string_list_t()
        call list_add(command, executable)
        call list_add(command, '--native-tree')
        call list_add(command, heartbeat)
        call run_process(command, scratch, result, timeout_ms=500)
        call assert_true(result%timed_out .and. result%reaped .and. .not. result%runner_failed, &
            'native timeout drains TERM-resistant launcher and grandchild')
        call assert_true(file_exists(heartbeat), 'native grandchild ran before timeout')
        before = read_text(heartbeat)
        call sleep_ms(200)
        call assert_equal_string(read_text(heartbeat), before, 'native drainage stops grandchild heartbeat')
        pid = -1
        birth = 0
        open(newunit=unit, file=heartbeat//'.identity', status='old', action='read', iostat=status)
        if (status == 0) then
            read(unit, *, iostat=status) pid, birth
            close(unit)
        end if
        call assert_true(status == 0 .and. pid > 0 .and. birth > 0, &
            'native grandchild records its independently observable identity')
        if (pid > 0 .and. birth > 0) then
            call assert_true(.not. mcp_process_identity_running(pid, birth), &
                'native drainage removes the exact grandchild identity')
            if (mcp_process_identity_running(pid, birth)) call mcp_kill_owned_tree(pid, birth, status)
        end if
        call assert_true(process_alive(sentinel), 'native tree cancellation preserves independent peer')
        call stop_sentinel(sentinel)
        call finish_assertions()
        stop
    end if
    command = string_list_t()
    call list_add(command, '/usr/bin/uname')
    call list_add(command, '-s')
    call run_process(command, scratch, result)
    call assert_process_ok(result, 'identify process-group platform')
    host = trim(result%stdout)

    do i = 1, 32
        command = string_list_t()
        call list_add(command, '/bin/sh')
        call list_add(command, '-c')
        call list_add(command, 'set -- $(/bin/ps -o pgid= -p $$); test "$1" -eq "$$"')
        call run_process(command, scratch, result)
        call assert_process_ok(result, 'launched child owns its process group')
    end do

    if (host == 'Darwin') then
        command = string_list_t()
        call list_add(command, '/bin/sh')
        call list_add(command, '-c')
        call list_add(command, 'echo should-not-execute')
        call run_process(command, scratch, result, timeout_ms=100, child_start_delay_ms=2000)
        call assert_true(result%timed_out .and. result%reaped, &
            'timeout reaps a child before it establishes its group')
        call assert_true(result%term_signal == 15 .and. result%elapsed_ms < 1000, &
            'pre-group child receives direct TERM promptly')
        call assert_equal_string(result%stdout, '', 'pre-group timeout prevents execution')
    end if

    heartbeat = join_path(scratch, 'grandchild-heartbeat')
    command = string_list_t()
    call list_add(command, '/bin/sh')
    call list_add(command, '-c')
    call list_add(command, 'trap "" TERM; /bin/sh -c ' // &
        '''while :; do echo tick >> "$1"; sleep 0.02; done'' child "$1" & wait')
    call list_add(command, 'launcher')
    call list_add(command, heartbeat)
    call run_process(command, scratch, result, timeout_ms=200)
    call assert_true(result%timed_out .and. result%reaped .and. .not. result%runner_failed, &
        'timeout kills and reaps a TERM-ignoring launcher')
    call assert_true(file_exists(heartbeat), 'grandchild ran before timeout')
    before = read_text(heartbeat)
    command = string_list_t()
    call list_add(command, '/bin/sleep')
    call list_add(command, '0.2')
    call run_process(command, scratch, result)
    call assert_process_ok(result, 'observe grandchild after launcher cleanup')
    call assert_equal_string(read_text(heartbeat), before, &
        'group cleanup stops the launcher grandchild heartbeat')
    call finish_assertions()
end program test_harness_process_group
