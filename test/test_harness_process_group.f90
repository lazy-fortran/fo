program test_harness_process_group
    use fo_test_harness, only: string_list_t, process_result_t, list_add, run_process
    use fo_test_harness, only: make_scratch, join_path, read_text, file_exists
    use fo_test_harness, only: assert_process_ok, assert_true, assert_equal_string
    use fo_test_harness, only: finish_assertions
    implicit none

    type(string_list_t) :: command
    type(process_result_t) :: result
    character(:), allocatable :: scratch, heartbeat, before, host
    integer :: i

    call make_scratch('fo-harness-process-group', scratch)
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
