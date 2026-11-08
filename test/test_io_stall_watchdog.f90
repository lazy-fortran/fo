program test_io_stall_watchdog
    !! A CMake configure whose child blocks in uninterruptible sleep, as a git
    !! clone does on a hung file system, is stopped after FO_IO_STALL_TIMEOUT
    !! with a diagnosis instead of after the whole FO_BUILD_TIMEOUT.
    use, intrinsic :: iso_c_binding, only: c_int
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, remove_tree, assert_true, assert_contains, &
        finish_assertions, environment_value
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none
    interface
        integer(c_int) function c_block() bind(C, name='fo_test_block_uninterruptibly')
            import :: c_int
        end function c_block
        integer(c_int) function c_host_is_linux() bind(C, name='fo_test_host_is_linux')
            import :: c_int
        end function c_host_is_linux
    end interface
    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project
    character(len=4096) :: self
    type(string_list_t) :: args, env
    type(process_result_t) :: result
    integer :: started, finished, rate

    if (environment_value('FO_TEST_BLOCK') == '1') then
        if (c_block() /= 0) error stop 'cannot block'
        stop
    end if
    if (c_host_is_linux() == 0) then
        print '(a)', 'SKIP: uninterruptible-sleep fixture and /proc watchdog are Linux-only'
        stop
    end if
    call resolve_driver(driver)
    call get_command_argument(0, self)
    call make_scratch('fo-io-stall', scratch)
    project = scratch//'/project'
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(stall LANGUAGES Fortran)'//nl// &
        'execute_process(COMMAND "${CMAKE_COMMAND}" -E env FO_TEST_BLOCK=1 "'// &
        trim(self)//'")'//nl// &
        'add_executable(app app.f90)'//nl)
    call write_text(project//'/app.f90', 'program app'//nl//'end program app'//nl)
    call list_add(env, 'FO_BACKEND=cmake')
    call list_add(env, 'FO_CMAKE_BUILD_DIR=build')
    call list_add(env, 'FO_CMAKE_CONFIGURE_PRESET=')
    call list_add(env, 'FO_IO_STALL_TIMEOUT=2')
    call list_add(env, 'FO_BUILD_TIMEOUT=90')
    call list_add(args, 'build')
    call system_clock(started, rate)
    call run_fo(driver, args, project, scratch//'/cache', result, env, &
        timeout_ms=150000)
    call system_clock(finished)
    call assert_true(result%exit_code /= 0, 'a stalled configure fails')
    call assert_contains(result%stdout//result%stderr, 'fo: STALLED:', &
        'the failure names the blocked process')
    call assert_true(real(finished - started) / real(rate) < 45.0, &
        'the stall is reported long before FO_BUILD_TIMEOUT')
    call remove_tree(scratch)
    call finish_assertions()
end program test_io_stall_watchdog
