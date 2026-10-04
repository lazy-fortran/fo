module fo_test_mcp_session
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_size_t, &
        c_null_char
    use fo_test_harness, only: assert_true
    use fo_test_json, only: json_value_t, json_parse
    use fo_test_mcp, only: mcp_request, mcp_quote
    use fo_test_process_identity, only: mcp_process_start_time, mcp_kill_owned_tree
    use fo_test_process_identity, only: mcp_process_identity_running
    implicit none
    private

    public :: mcp_session_t, mcp_session_start, mcp_session_request
    public :: mcp_session_call, mcp_session_notify, mcp_session_shutdown
    public :: mcp_session_terminate

    type :: mcp_session_t
        integer :: handle = 0
        integer :: pid = 0
        integer :: next_id = 1
        integer(c_int64_t) :: start_time = 0_c_int64_t
    end type mcp_session_t

    interface
        integer(c_int) function c_mcp_start(driver, directory, cache, state, &
                stderr_path) &
                bind(C, name='fo_test_mcp_start')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: driver(*), directory(*), cache(*)
            character(kind=c_char), intent(in) :: state(*), stderr_path(*)
        end function c_mcp_start
        integer(c_int) function c_mcp_pid(handle) bind(C, name='fo_test_mcp_pid')
            import :: c_int
            integer(c_int), value :: handle
        end function c_mcp_pid
        integer(c_int) function c_mcp_write(handle, message, length) &
                bind(C, name='fo_test_mcp_write')
            import :: c_char, c_int, c_size_t
            integer(c_int), value :: handle
            character(kind=c_char), intent(in) :: message(*)
            integer(c_size_t), value :: length
        end function c_mcp_write
        integer(c_int) function c_mcp_read_line(handle, buffer, capacity, timeout_ms) &
                bind(C, name='fo_test_mcp_read_line')
            import :: c_char, c_int, c_size_t
            integer(c_int), value :: handle
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: capacity
            integer(c_int), value :: timeout_ms
        end function c_mcp_read_line
        integer(c_int) function c_mcp_wait(handle, timeout_ms, exit_code) &
                bind(C, name='fo_test_mcp_wait')
            import :: c_int
            integer(c_int), value :: handle, timeout_ms
            integer(c_int), intent(out) :: exit_code
        end function c_mcp_wait
        integer(c_int) function c_mcp_close(handle) bind(C, name='fo_test_mcp_close')
            import :: c_int
            integer(c_int), value :: handle
        end function c_mcp_close
    end interface

contains

    subroutine mcp_session_start(session, driver, directory, cache, state, stderr_path)
        type(mcp_session_t), intent(out) :: session
        character(len=*), intent(in) :: driver, directory, cache, state, stderr_path

        session%handle = int(c_mcp_start(trim(driver)//c_null_char, &
            trim(directory)//c_null_char, trim(cache)//c_null_char, &
            trim(state)//c_null_char, trim(stderr_path)//c_null_char))
        call assert_true(session%handle > 0, 'starts an interactive owned MCP process')
        if (session%handle <= 0) return
        session%pid = int(c_mcp_pid(int(session%handle, c_int)))
        session%start_time = mcp_process_start_time(session%pid)
        call assert_true(session%pid > 0 .and. session%start_time > 0_c_int64_t, &
            'captures exact MCP server PID and start identity')
        session%next_id = 1
    end subroutine mcp_session_start

    subroutine mcp_session_request(session, method, params, response, timeout_ms)
        type(mcp_session_t), intent(inout) :: session
        character(len=*), intent(in) :: method, params
        type(json_value_t), intent(out) :: response
        integer, intent(in), optional :: timeout_ms
        character(:), allocatable :: request, raw, message
        character(kind=c_char), allocatable :: bytes(:), output(:)
        integer(c_int) :: result
        integer :: limit, length, i
        logical :: valid

        limit = 15000
        if (present(timeout_ms)) limit = timeout_ms
        if (len_trim(params) > 0) then
            request = mcp_request(session%next_id, trim(method), trim(params))
        else
            request = mcp_request(session%next_id, trim(method))
        end if
        request = request//new_line('a')
        allocate(bytes(len(request)))
        do i = 1, len(request)
            bytes(i) = request(i:i)
        end do
        result = c_mcp_write(int(session%handle, c_int), bytes, &
            int(len(request), c_size_t))
        call assert_true(result == 0, 'writes one bounded MCP JSON-RPC request')
        if (result /= 0) return
        allocate(output(1048577))
        result = c_mcp_read_line(int(session%handle, c_int), output, &
            int(size(output), c_size_t), int(limit, c_int))
        call assert_true(result >= 0, &
            'reads one complete MCP response before its bound')
        if (result < 0) return
        length = int(result)
        allocate(character(len=length) :: raw)
        do i = 1, length
            raw(i:i) = output(i)
        end do
        call json_parse(raw, response, valid, message)
        call assert_true(valid, 'independent Fortran JSON parser accepts MCP response')
        if (.not. valid) return
        session%next_id = session%next_id + 1
    end subroutine mcp_session_request

    subroutine mcp_session_call(session, arguments, response, timeout_ms)
        type(mcp_session_t), intent(inout) :: session
        character(len=*), intent(in) :: arguments
        type(json_value_t), intent(out) :: response
        integer, intent(in), optional :: timeout_ms

        call mcp_session_request(session, 'tools/call', &
            '{"name":"fo","arguments":'//trim(arguments)//'}', response, timeout_ms)
    end subroutine mcp_session_call

    subroutine mcp_session_notify(session, method, params)
        type(mcp_session_t), intent(inout) :: session
        character(len=*), intent(in) :: method
        character(len=*), intent(in), optional :: params
        character(:), allocatable :: request
        character(kind=c_char), allocatable :: bytes(:)
        integer(c_int) :: result
        integer :: i

        request = '{"jsonrpc":"2.0","method":'//mcp_quote(method)
        if (present(params)) then
            if (len_trim(params) > 0) request = request//',"params":'//trim(params)
        end if
        request = request//'}'//new_line('a')
        allocate(bytes(len(request)))
        do i = 1, len(request)
            bytes(i) = request(i:i)
        end do
        result = c_mcp_write(int(session%handle, c_int), bytes, &
            int(len(request), c_size_t))
        call assert_true(result == 0, 'writes one bounded MCP notification')
    end subroutine mcp_session_notify

    subroutine mcp_session_shutdown(session, exit_code)
        type(mcp_session_t), intent(inout) :: session
        integer, intent(out) :: exit_code
        type(json_value_t) :: response
        integer(c_int) :: status, c_exit_code

        c_exit_code = -999_c_int
        call mcp_session_request(session, 'shutdown', '', response, 5000)
        status = c_mcp_wait(int(session%handle, c_int), 5000_c_int, c_exit_code)
        exit_code = int(c_exit_code)
        call assert_true(status == 0, 'MCP server exits and is reaped after shutdown')
        call close_session(session)
    end subroutine mcp_session_shutdown

    subroutine mcp_session_terminate(session)
        type(mcp_session_t), intent(inout) :: session
        integer :: signal_status
        integer(c_int) :: wait_status, exit_code

        if (session%handle <= 0) return
        if (mcp_process_identity_running(session%pid, session%start_time)) then
            call mcp_kill_owned_tree(session%pid, session%start_time, signal_status)
            call assert_true(signal_status == 0, &
                'terminates exact MCP-owned process tree')
        end if
        wait_status = c_mcp_wait(int(session%handle, c_int), 5000_c_int, exit_code)
        call assert_true(wait_status == 0, 'reaps terminated interactive MCP process')
        call close_session(session)
    end subroutine mcp_session_terminate

    subroutine close_session(session)
        type(mcp_session_t), intent(inout) :: session
        integer(c_int) :: status

        if (session%handle <= 0) return
        status = c_mcp_close(int(session%handle, c_int))
        call assert_true(status == 0, 'closes only reaped MCP session pipes')
        session%handle = 0
        session%pid = 0
        session%start_time = 0_c_int64_t
    end subroutine close_session

end module fo_test_mcp_session
