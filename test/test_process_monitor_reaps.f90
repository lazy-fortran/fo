program test_process_monitor_reaps
    !! The capture monitor is a Linux subreaper: a double-forked descendant that
    !! exits while the target still runs is adopted by the monitor and must be
    !! reaped there. A zombie would keep answering kill(pid, 0) as alive, so the
    !! still-running target probes the descendant through the public signal-0 test.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_null_ptr, &
        c_ptr, c_loc
    use fo_test_harness, only: make_scratch, join_path, remove_tree, write_text, &
        read_text, file_exists, environment_value, assert_true, assert_equal_string, &
        finish_assertions, sleep_ms
    implicit none

    interface
        integer(c_int) function c_start(monitor, cwd, arguments, environment, in_fd, &
                out_fd, err_fd) bind(C, name='fo_c_start_capture_monitor')
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: monitor(*), cwd(*)
            type(c_ptr), intent(in) :: arguments(*), environment(*)
            integer(c_int), value :: in_fd, out_fd, err_fd
        end function c_start
    end interface

    character(:), allocatable :: scratch, driver, script, result_path, pid_path
    character(kind=c_char), target :: sh(3), dash_c(3), script_bytes(1024)
    character(kind=c_char), allocatable, target :: driver_bytes(:)
    character(kind=c_char), target :: cwd(2)
    type(c_ptr) :: arguments(4), environment(1)
    integer(c_int) :: monitor
    integer :: i

    driver = environment_value('FO_BIN')
    call make_scratch('fo-monitor-reaps', scratch)
    result_path = join_path(scratch, 'verdict')
    pid_path = join_path(scratch, 'orphan.pid')
    script = '( sh -c "echo \$\$ > '//pid_path//'" & ) ; sleep 0.5; ' // &
        'if kill -0 $(cat '//pid_path//') 2>/dev/null; then echo alive; ' // &
        'else echo gone; fi > '//result_path
    sh = ['s', 'h', c_null_char]
    dash_c = ['-', 'c', c_null_char]
    cwd = ['/', c_null_char]
    script_bytes = c_null_char
    do i = 1, len(script)
        script_bytes(i) = script(i:i)
    end do
    allocate (driver_bytes(len(driver) + 1))
    do i = 1, len(driver)
        driver_bytes(i) = driver(i:i)
    end do
    driver_bytes(len(driver) + 1) = c_null_char
    arguments = [c_loc(sh), c_loc(dash_c), c_loc(script_bytes), c_null_ptr]
    environment = [c_null_ptr]

    monitor = c_start(driver_bytes, cwd, arguments, environment, 0_c_int, 1_c_int, 2_c_int)
    if (monitor <= 0) then
        print '(a)', 'SKIP: no Linux subreaper capture monitor on this platform'
        call remove_tree(scratch)
        stop
    end if
    do i = 1, 200
        if (file_exists(result_path)) exit
        call sleep_ms(50)
    end do
    call assert_true(file_exists(result_path), 'target reports a verdict')
    if (file_exists(result_path)) call assert_equal_string(read_text(result_path), &
        'gone'//new_line('a'), 'exited orphan adopted by the monitor is reaped, not a zombie')
    call remove_tree(scratch)
    call finish_assertions()
end program test_process_monitor_reaps
