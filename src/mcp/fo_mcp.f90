module fo_mcp
    use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
    use fo_check, only: check_result_t, fo_check_run, &
                        check_result_compact_json, check_result_full_json
    use fo_process, only: process_start_fo_check, process_poll_pid, &
                          process_cancel_pid
    use fo_run_queue, only: run_queue_t, RUN_IDLE, RUN_RUNNING, &
                            RUN_RERUN_PENDING
    implicit none
    private
    public :: mcp_serve

    integer, parameter :: MAX_LINE = 8192

    type :: mcp_async_state_t
        type(run_queue_t) :: queue
        integer :: active_pid = 0
        integer :: active_run_id = 0
        integer :: pending_run_id = 0
        integer :: last_run_id = 0
        integer :: last_exitcode = 0
        integer :: next_run_id = 0
        character(len=512) :: active_output = ''
        character(len=512) :: last_output = ''
    end type mcp_async_state_t

contains

    subroutine mcp_serve()
        character(len=MAX_LINE) :: line, response
        character(len=256) :: method, id_str
        integer :: iostat
        type(mcp_async_state_t) :: async_state

        do
            read (input_unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle

            call extract_field(line, '"method"', method)
            call extract_field(line, '"id"', id_str)
            call async_poll(async_state)

            select case (trim(method))
            case ('initialize')
                call make_initialize_response(id_str, response)
                call send_response(response)
            case ('initialized')
                ! notification, no response needed
                cycle
            case ('tools/list')
                call make_tools_list_response(id_str, response)
                call send_response(response)
            case ('tools/call')
                call handle_tools_call(line, id_str, response, async_state)
                call send_response(response)
            case ('resources/list')
                call make_resources_list_response(id_str, response)
                call send_response(response)
            case ('resources/read')
                call handle_resources_read(line, id_str, response, async_state)
                call send_response(response)
            case ('shutdown')
                call async_cancel_all(async_state)
                call make_result_null(id_str, response)
                call send_response(response)
                exit
            case default
                if (len_trim(id_str) > 0) then
                    call make_error_response(id_str, -32601, &
                                             'method not found', response)
                    call send_response(response)
                end if
            end select
        end do
        call async_cancel_all(async_state)
    end subroutine mcp_serve

    subroutine send_response(response)
        character(len=*), intent(in) :: response

        character(len=16) :: len_str
        integer :: n

        n = len_trim(response)
        write (len_str, '(i0)') n
        write (output_unit, '(a,a,a,a,a)', advance='no') &
            'Content-Length: ', trim(len_str), char(13)//char(10), &
            char(13)//char(10), trim(response)
        flush (output_unit)
    end subroutine send_response

    subroutine make_initialize_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"protocolVersion":"2024-11-05",'// &
                   '"capabilities":{"tools":{},"resources":{}},'// &
                   '"serverInfo":{"name":"fo","version":"0.1.0"}}}'
    end subroutine make_initialize_response

    subroutine make_tools_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"tools":[{"name":"fo",'// &
                   '"description":"Fortran build driver",'// &
                   '"inputSchema":{"type":"object","properties":{'// &
                   '"action":{"type":"string",'// &
                   '"enum":["check","status","diagnostics","cancel",'// &
                   '"build","test","graph","info","changed","clean"],'// &
                   '"description":"Action to run"}},'// &
                   '"required":["action"]}}]}}'
    end subroutine make_tools_list_response

    subroutine make_resources_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"resources":[{"uri":"fo://diagnostics",'// &
                   '"name":"diagnostics",'// &
                   '"description":"Current fo check diagnostics",'// &
                   '"mimeType":"text/plain"}]}}'
    end subroutine make_resources_list_response

    subroutine handle_tools_call(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=64) :: action, mode
        character(len=4096) :: output_text
        integer :: exitcode, cmdstat
        character(len=512) :: tmpfile, cmd, buf
        integer :: u, iostat, n
        type(check_result_t) :: check_res

        call extract_action(line, action)
        call make_tmpfile('fo_mcp_output', tmpfile)

        select case (trim(action))
        case ('check')
            call extract_field(line, '"mode"', mode)
            if (trim(mode) == 'start') then
                call handle_async_start(line, id_str, response, async_state)
                return
            else
                call fo_check_run('.', check_res)
                if (index(line, '"full"') > 0 .or. &
                    index(line, '"json":"full"') > 0) then
                    output_text = check_result_full_json(check_res)
                else
                    output_text = check_result_compact_json(check_res)
                end if
                exitcode = 0
                if (.not. (check_res%build_ok .and. check_res%tests_ok)) exitcode = 1
                call make_tool_text_response(id_str, output_text, exitcode, response)
            end if
        case ('status')
            call handle_async_status(line, id_str, response, async_state)
            return
        case ('diagnostics')
            call handle_async_diagnostics(line, id_str, response, async_state)
            return
        case ('cancel')
            call handle_async_cancel(line, id_str, response, async_state)
            return
        case ('build', 'test', 'graph', 'info', 'changed', 'clean')
            cmd = 'fo '//trim(action)//' > '//trim(tmpfile)//' 2>&1'
            call execute_command_line(cmd, exitstat=exitcode, &
                                      cmdstat=cmdstat, wait=.true.)
            if (cmdstat /= 0) exitcode = 1

            output_text = ''
            n = 0
            open (newunit=u, file=tmpfile, status='old', iostat=iostat)
            if (iostat == 0) then
                do
                    read (u, '(a)', iostat=iostat) buf
                    if (iostat /= 0) exit
                    if (n + len_trim(buf) + 1 > len(output_text)) exit
                    output_text(n + 1:n + len_trim(buf)) = trim(buf)
                    n = n + len_trim(buf)
                    n = n + 1
                    output_text(n:n) = char(10)
                end do
                close (u)
            end if
            call execute_command_line('rm -f '//trim(tmpfile), wait=.true.)

            call make_tool_text_response(id_str, output_text, exitcode, response)
        case default
            call make_error_response(id_str, -32602, &
                                     'unknown action: '//trim(action), response)
        end select
    end subroutine handle_tools_call

    subroutine handle_resources_read(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=256) :: uri
        character(len=4096) :: output_text
        integer :: exitcode
        type(check_result_t) :: check_res

        call extract_field(line, '"uri"', uri)
        call async_poll(async_state)

        if (trim(uri) == 'fo://diagnostics' .or. &
            index(line, 'fo://diagnostics') > 0) then
            if (len_trim(async_state%last_output) > 0) then
                call read_text_file(async_state%last_output, output_text)
                exitcode = async_state%last_exitcode
            else
                call fo_check_run('.', check_res)
                output_text = check_result_compact_json(check_res)
                exitcode = 0
                if (.not. (check_res%build_ok .and. check_res%tests_ok)) exitcode = 1
            end if

            call escape_json(output_text)
            response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                       '"result":{"contents":[{"uri":"fo://diagnostics",'// &
                       '"mimeType":"text/plain",'// &
                       '"text":"'//trim(output_text)//'"}]}}'
        else
            call make_error_response(id_str, -32602, &
                                     'unknown resource', response)
        end if
    end subroutine handle_resources_read

    subroutine handle_async_start(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=512) :: root
        character(len=32) :: output_mode
        integer :: ierr, run_id, started_before
        logical :: pending

        call extract_field(line, '"root"', root)
        if (len_trim(root) == 0) root = '.'
        output_mode = 'agent'
        if (index(line, '"json":"full"') > 0 .or. index(line, '"full"') > 0) then
            output_mode = 'full'
        end if

        started_before = async_state%queue%started
        call async_state%queue%request(root, output_mode, ierr)
        if (ierr /= 0) then
            call make_error_response(id_str, -32602, 'invalid root', response)
            return
        end if

        pending = async_state%queue%started == started_before
        if (pending) then
            async_state%next_run_id = async_state%next_run_id + 1
            async_state%pending_run_id = async_state%next_run_id
            run_id = async_state%pending_run_id
        else
            call async_start_current(async_state, 0, ierr)
            if (ierr /= 0) then
                call make_error_response(id_str, -32603, &
                                         'could not start check', response)
                return
            end if
            run_id = async_state%active_run_id
        end if

        call make_run_start_response(id_str, run_id, pending, response)
    end subroutine handle_async_start

    subroutine handle_async_status(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        integer :: run_id, ierr

        call async_poll(async_state)
        call requested_run_id(line, async_state, run_id, ierr)
        if (ierr /= 0) then
            call make_error_response(id_str, -32602, 'unknown run_id', response)
            return
        end if

        call make_status_response(id_str, run_id, async_state, response)
    end subroutine handle_async_status

    subroutine handle_async_diagnostics(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=4096) :: output_text
        integer :: run_id, ierr
        logical :: stale

        call async_poll(async_state)
        call requested_run_id(line, async_state, run_id, ierr)
        if (ierr /= 0) then
            call make_error_response(id_str, -32602, 'unknown run_id', response)
            return
        end if

        output_text = ''
        if (run_id == async_state%last_run_id .and. &
            len_trim(async_state%last_output) > 0) then
            call read_text_file(async_state%last_output, output_text)
        end if
        stale = async_state%active_pid > 0 .or. async_state%pending_run_id > 0
        call make_diagnostics_response(id_str, run_id, output_text, stale, &
                                       async_state%last_exitcode, response)
    end subroutine handle_async_diagnostics

    subroutine handle_async_cancel(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        integer :: run_id, ierr, exitcode

        call requested_run_id(line, async_state, run_id, ierr)
        if (ierr /= 0) then
            call make_error_response(id_str, -32602, 'unknown run_id', response)
            return
        end if
        if (run_id /= async_state%active_run_id .or. async_state%active_pid <= 0) then
            call make_error_response(id_str, -32602, 'run is not active', response)
            return
        end if

        call process_cancel_pid(async_state%active_pid, exitcode)
        async_state%active_pid = 0
        call async_state%queue%finish(130)
        async_state%last_run_id = run_id
        async_state%last_exitcode = 130
        async_state%last_output = async_state%active_output
        async_state%active_output = ''
        async_state%active_run_id = 0
        call async_start_pending_if_ready(async_state)

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"cancelled":true,"run_id":'//trim(int_text(run_id))//'}}'
    end subroutine handle_async_cancel

    subroutine async_poll(async_state)
        type(mcp_async_state_t), intent(inout) :: async_state

        logical :: done
        integer :: exitcode

        if (async_state%active_pid <= 0) then
            call async_start_pending_if_ready(async_state)
            return
        end if

        call process_poll_pid(async_state%active_pid, done, exitcode)
        if (.not. done) return

        async_state%last_run_id = async_state%active_run_id
        async_state%last_exitcode = exitcode
        async_state%last_output = async_state%active_output
        async_state%active_pid = 0
        async_state%active_run_id = 0
        async_state%active_output = ''
        call async_state%queue%finish(exitcode)
        call async_start_pending_if_ready(async_state)
    end subroutine async_poll

    subroutine async_start_pending_if_ready(async_state)
        type(mcp_async_state_t), intent(inout) :: async_state

        integer :: ierr, run_id

        if (async_state%active_pid > 0) return
        if (async_state%queue%state /= RUN_RUNNING) return

        run_id = async_state%pending_run_id
        async_state%pending_run_id = 0
        call async_start_current(async_state, run_id, ierr)
    end subroutine async_start_pending_if_ready

    subroutine async_start_current(async_state, requested_id, ierr)
        type(mcp_async_state_t), intent(inout) :: async_state
        integer, intent(in) :: requested_id
        integer, intent(out) :: ierr

        integer :: pid, exitcode, run_id
        character(len=512) :: output_file

        ierr = 0
        run_id = requested_id
        if (run_id <= 0) then
            async_state%next_run_id = async_state%next_run_id + 1
            run_id = async_state%next_run_id
        end if

        call make_tmpfile('fo_mcp_async', output_file)
        call process_start_fo_check(async_state%queue%current_root, &
                                    async_state%queue%current_mode, output_file, &
                                    pid, exitcode)
        if (exitcode /= 0 .or. pid <= 0) then
            ierr = 1
            return
        end if

        async_state%active_pid = pid
        async_state%active_run_id = run_id
        async_state%active_output = output_file
    end subroutine async_start_current

    subroutine async_cancel_all(async_state)
        type(mcp_async_state_t), intent(inout) :: async_state

        integer :: exitcode

        if (async_state%active_pid > 0) then
            call process_cancel_pid(async_state%active_pid, exitcode)
            async_state%active_pid = 0
            async_state%last_run_id = async_state%active_run_id
            async_state%last_exitcode = 130
            async_state%last_output = async_state%active_output
        end if
        async_state%active_run_id = 0
        async_state%pending_run_id = 0
        async_state%active_output = ''
        async_state%queue%state = RUN_IDLE
        async_state%queue%rerun_pending = .false.
        async_state%queue%current_root = ''
        async_state%queue%current_mode = ''
        async_state%queue%pending_root = ''
        async_state%queue%pending_mode = ''
    end subroutine async_cancel_all

    subroutine requested_run_id(line, async_state, run_id, ierr)
        character(len=*), intent(in) :: line
        type(mcp_async_state_t), intent(in) :: async_state
        integer, intent(out) :: run_id, ierr

        character(len=64) :: run_text
        integer :: iostat

        ierr = 0
        run_id = async_state%last_run_id
        if (index(line, '"latest"') > 0 .or. index(line, '"run_id"') == 0) then
            if (async_state%active_run_id > 0) run_id = async_state%active_run_id
            if (async_state%pending_run_id > 0) run_id = async_state%pending_run_id
            return
        end if

        call extract_field(line, '"run_id"', run_text)
        read (run_text, *, iostat=iostat) run_id
        if (iostat /= 0) then
            ierr = 1
            return
        end if
        if (run_id == async_state%active_run_id) return
        if (run_id == async_state%pending_run_id) return
        if (run_id == async_state%last_run_id) return
        ierr = 1
    end subroutine requested_run_id

    subroutine make_run_start_response(id_str, run_id, pending, response)
        character(len=*), intent(in) :: id_str
        integer, intent(in) :: run_id
        logical, intent(in) :: pending
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"run_id":'//trim(int_text(run_id))// &
                   ',"state":"'
        if (pending) then
            response = trim(response)//'rerun-pending"'
        else
            response = trim(response)//'running"'
        end if
        response = trim(response)//',"pending":'//trim(json_bool(pending))//'}}'
    end subroutine make_run_start_response

    subroutine make_status_response(id_str, run_id, async_state, response)
        character(len=*), intent(in) :: id_str
        integer, intent(in) :: run_id
        type(mcp_async_state_t), intent(in) :: async_state
        character(len=*), intent(out) :: response

        character(len=32) :: state

        if (run_id == async_state%active_run_id .and. async_state%active_pid > 0) then
            state = 'running'
        else if (run_id == async_state%pending_run_id) then
            state = 'rerun-pending'
        else if (run_id == async_state%last_run_id) then
            state = 'finished'
        else
            state = 'unknown'
        end if

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"run_id":'//trim(int_text(run_id))// &
                   ',"state":"'//trim(state)// &
                   '","active":'// &
                   trim(json_bool(run_id == async_state%active_run_id .and. &
                                  async_state%active_pid > 0))// &
                   ',"pending":'// &
                   trim(json_bool(run_id == async_state%pending_run_id))// &
                   ',"last_exitcode":'// &
                   trim(int_text(async_state%last_exitcode))//'}}'
    end subroutine make_status_response

    subroutine make_diagnostics_response(id_str, run_id, output_text, stale, &
                                         exitcode, response)
        character(len=*), intent(in) :: id_str, output_text
        integer, intent(in) :: run_id, exitcode
        logical, intent(in) :: stale
        character(len=*), intent(out) :: response

        character(len=4096) :: escaped

        escaped = output_text
        call escape_json(escaped)
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"run_id":'//trim(int_text(run_id))// &
                   ',"stale":'//trim(json_bool(stale))// &
                   ',"exitcode":'//trim(int_text(exitcode))// &
                   ',"diagnostics":"'//trim(escaped)//'"}}'
    end subroutine make_diagnostics_response

    subroutine make_tool_text_response(id_str, output_text, exitcode, response)
        character(len=*), intent(in) :: id_str, output_text
        integer, intent(in) :: exitcode
        character(len=*), intent(out) :: response

        character(len=4096) :: escaped

        escaped = output_text
        call escape_json(escaped)
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"content":[{"type":"text",'// &
                   '"text":"'//trim(escaped)//'"}],"isError":'// &
                   trim(json_bool(exitcode /= 0))//'}}'
    end subroutine make_tool_text_response

    subroutine read_text_file(path, text)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: text

        character(len=512) :: buf
        integer :: u, iostat, n

        text = ''
        n = 0
        open (newunit=u, file=trim(path), status='old', iostat=iostat)
        if (iostat /= 0) return
        do
            read (u, '(a)', iostat=iostat) buf
            if (iostat /= 0) exit
            if (n + len_trim(buf) + 1 > len(text)) exit
            text(n + 1:n + len_trim(buf)) = trim(buf)
            n = n + len_trim(buf)
            n = n + 1
            text(n:n) = char(10)
        end do
        close (u)
    end subroutine read_text_file

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        write (text, '(i0)') value
    end function int_text

    function json_bool(value) result(text)
        logical, intent(in) :: value
        character(len=5) :: text

        if (value) then
            text = 'true'
        else
            text = 'false'
        end if
    end function json_bool

    subroutine make_result_null(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',"result":null}'
    end subroutine make_result_null

    subroutine make_error_response(id_str, code, msg, response)
        character(len=*), intent(in) :: id_str, msg
        integer, intent(in) :: code
        character(len=*), intent(out) :: response

        character(len=16) :: code_str

        write (code_str, '(i0)') code
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"error":{"code":'//trim(code_str)//','// &
                   '"message":"'//trim(msg)//'"}}'
    end subroutine make_error_response

    subroutine extract_field(line, key, val)
        character(len=*), intent(in) :: line, key
        character(len=*), intent(out) :: val

        integer :: pos, start, fin, i
        character(len=1) :: ch

        val = ''
        pos = index(line, trim(key))
        if (pos == 0) return

        ! find the colon after the key
        pos = pos + len_trim(key)
        do while (pos <= len_trim(line))
            if (line(pos:pos) == ':') exit
            pos = pos + 1
        end do
        pos = pos + 1

        ! skip whitespace
        do while (pos <= len_trim(line) .and. line(pos:pos) == ' ')
            pos = pos + 1
        end do

        if (pos > len_trim(line)) return

        ch = line(pos:pos)
        if (ch == '"') then
            ! string value
            start = pos + 1
            fin = start
            do while (fin <= len_trim(line))
                if (line(fin:fin) == '"' .and. line(fin - 1:fin - 1) /= '\') exit
                fin = fin + 1
            end do
            val = line(start:fin - 1)
        else
            ! number or other value
            start = pos
            fin = pos
            do while (fin <= len_trim(line))
                ch = line(fin:fin)
                if (ch == ',' .or. ch == '}' .or. ch == ' ') exit
                fin = fin + 1
            end do
            val = line(start:fin - 1)
        end if
    end subroutine extract_field

    subroutine extract_action(line, action)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: action

        character(len=256) :: arguments_str

        ! look for "action" inside the arguments/params
        action = ''
        call extract_field(line, '"action"', action)
    end subroutine extract_action

    subroutine escape_json(str)
        character(len=*), intent(inout) :: str

        character(len=len(str)) :: buf
        integer :: i, j, n

        n = len_trim(str)
        j = 0
        buf = ''

        do i = 1, n
            select case (str(i:i))
            case ('"')
                if (j + 2 > len(buf)) exit
                j = j + 1; buf(j:j) = '\'
                j = j + 1; buf(j:j) = '"'
            case ('\')
                if (j + 2 > len(buf)) exit
                j = j + 1; buf(j:j) = '\'
                j = j + 1; buf(j:j) = '\'
            case (char(10))
                if (j + 2 > len(buf)) exit
                j = j + 1; buf(j:j) = '\'
                j = j + 1; buf(j:j) = 'n'
            case (char(13))
                if (j + 2 > len(buf)) exit
                j = j + 1; buf(j:j) = '\'
                j = j + 1; buf(j:j) = 'r'
            case (char(9))
                if (j + 2 > len(buf)) exit
                j = j + 1; buf(j:j) = '\'
                j = j + 1; buf(j:j) = 't'
            case default
                if (j + 1 > len(buf)) exit
                j = j + 1; buf(j:j) = str(i:i)
            end select
        end do

        str = buf(1:j)
    end subroutine escape_json

    subroutine make_tmpfile(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a)') '/tmp/', trim(prefix), '-', &
            count, '-', serial, '.tmp'
    end subroutine make_tmpfile

end module fo_mcp
