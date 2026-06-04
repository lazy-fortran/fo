module fo_mcp
    use fo_json, only: json_int, json_escape, extract_json_field, &
                       make_tmpfile, delete_tmpfile, read_text_file, &
                       send_jsonrpc, jsonrpc_error, jsonrpc_null
    use fo_mcp_response, only: make_initialize_response, &
                               make_tools_list_response, &
                               make_resources_list_response, &
                               make_run_start_response, &
                               make_tool_text_response
    use fo_check, only: check_result_t, fo_check_run
    use fo_check_output, only: check_result_compact_json, &
                               check_result_full_json
    use fo_capabilities, only: capabilities_t, detect_capabilities, &
                               capabilities_json
    use fo_process, only: process_start_fo_check, process_poll_pid, &
                          process_cancel_pid, process_read_jsonrpc_message
    use fo_run_queue, only: run_queue_t, RUN_IDLE, RUN_RUNNING, &
                            RUN_RERUN_PENDING
    implicit none
    private
    public :: mcp_serve

    integer, parameter :: MAX_LINE = 32768

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
            call read_jsonrpc_message(line, iostat)
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle

            call extract_json_field(line, '"method"', method)
            call extract_json_field(line, '"id"', id_str)
            call async_poll(async_state)

            select case (trim(method))
            case ('initialize')
                call make_initialize_response(id_str, line, response)
                call send_jsonrpc(response)
            case ('initialized')
                ! notification, no response needed
                cycle
            case ('tools/list')
                call make_tools_list_response(id_str, response)
                call send_jsonrpc(response)
            case ('tools/call')
                call handle_tools_call(line, id_str, response, async_state)
                call send_jsonrpc(response)
            case ('resources/list')
                call make_resources_list_response(id_str, response)
                call send_jsonrpc(response)
            case ('resources/read')
                call handle_resources_read(line, id_str, response, async_state)
                call send_jsonrpc(response)
            case ('shutdown')
                call async_cancel_all(async_state)
                call jsonrpc_null(id_str, response)
                call send_jsonrpc(response)
                exit
            case default
                if (len_trim(id_str) > 0) then
                    call jsonrpc_error(id_str, -32601, &
                                             'method not found', response)
                    call send_jsonrpc(response)
                end if
            end select
        end do
        call async_cancel_all(async_state)
    end subroutine mcp_serve

    subroutine read_jsonrpc_message(body, iostat)
        character(len=*), intent(out) :: body
        integer, intent(out) :: iostat

        integer :: nread

        body = ''
        iostat = 0
        call process_read_jsonrpc_message(body, nread)
        if (nread <= 0) iostat = -1
    end subroutine read_jsonrpc_message

    subroutine handle_tools_call(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=64) :: action, mode
        character(len=8192) :: output_text
        integer :: exitcode, cmdstat
        character(len=512) :: tmpfile, cmd
        type(check_result_t) :: check_res

        call extract_json_field(line, '"action"', action)
        call make_tmpfile('fo_mcp_output', tmpfile)

        select case (trim(action))
        case ('check')
            call extract_json_field(line, '"mode"', mode)
            if (trim(mode) == 'start') then
                call handle_async_start(line, id_str, response, async_state)
                return
            else
                block
                logical :: want_full
                type(capabilities_t) :: cap
                character(len=2048) :: cap_json

                want_full = (index(line, '"full"') > 0 .or. &
                             index(line, '"json":"full"') > 0)
                cap_json = ''
                if (want_full) then
                    call detect_capabilities(cap)
                    call capabilities_json(cap, cap_json)
                end if
                call fo_check_run('.', check_res)
                if (want_full) then
                    output_text = check_result_full_json(check_res, cap_json)
                else
                    output_text = check_result_compact_json(check_res)
                end if
                exitcode = 0
                if (.not. (check_res%build_ok .and. check_res%tests_ok)) exitcode = 1
                call make_tool_text_response(id_str, output_text, exitcode, response)
                end block
            end if
        case ('status')
            call handle_async_status(id_str, response, async_state)
            return
        case ('diagnostics')
            call handle_async_diagnostics(line, id_str, response, async_state)
            return
        case ('cancel')
            call handle_async_cancel(line, id_str, response, async_state)
            return
        case ('lint')
            block
                use fo_lint, only: lint_finding_t, lint_warning_t, &
                                   lint_dir, lint_compiler, &
                                   lint_all_json, &
                                   MAX_FINDINGS, MAX_WARNINGS
                type(lint_finding_t) :: findings(MAX_FINDINGS)
                type(lint_warning_t) :: warnings(MAX_WARNINGS)
                integer :: n_findings, n_warnings
                character(len=16384) :: lint_output

                call lint_dir('.', findings, n_findings)
                call lint_compiler('.', warnings, n_warnings)
                lint_output = lint_all_json(findings, n_findings, &
                                            warnings, n_warnings)
                exitcode = 0
                if (n_findings > 0 .or. n_warnings > 0) exitcode = 1
                call make_tool_text_response(id_str, lint_output, &
                                             exitcode, response)
            end block
            call delete_tmpfile(tmpfile)
            return
        case ('build', 'test', 'graph', 'info', 'changed', 'clean')
            cmd = 'fo '//trim(action)//' > '//trim(tmpfile)//' 2>&1'
            call execute_command_line(cmd, exitstat=exitcode, &
                                      cmdstat=cmdstat, wait=.true.)
            if (cmdstat /= 0) exitcode = 1

            call read_text_file(tmpfile, output_text)
            call delete_tmpfile(tmpfile)

            call make_tool_text_response(id_str, output_text, exitcode, response)
        case default
            call jsonrpc_error(id_str, -32602, &
                                     'unknown action: '//trim(action), response)
        end select
    end subroutine handle_tools_call

    subroutine handle_resources_read(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=256) :: uri
        character(len=8192) :: output_text
        integer :: exitcode
        type(check_result_t) :: check_res

        call extract_json_field(line, '"uri"', uri)
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

            call json_escape(output_text)
            response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                       '"result":{"contents":[{"uri":"fo://diagnostics",'// &
                       '"mimeType":"text/plain",'// &
                       '"text":"'//trim(output_text)//'"}]}}'
        else
            call jsonrpc_error(id_str, -32602, &
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

        call extract_json_field(line, '"root"', root)
        if (len_trim(root) == 0) root = '.'
        output_mode = 'agent'
        if (index(line, '"json":"full"') > 0 .or. index(line, '"full"') > 0) then
            output_mode = 'full'
        end if

        started_before = async_state%queue%started
        call async_state%queue%request(root, output_mode, ierr)
        if (ierr /= 0) then
            call jsonrpc_error(id_str, -32602, 'invalid root', response)
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
                call jsonrpc_error(id_str, -32603, &
                                         'could not start check', response)
                return
            end if
            run_id = async_state%active_run_id
        end if

        call make_run_start_response(id_str, run_id, pending, response)
    end subroutine handle_async_start

    subroutine handle_async_status(id_str, response, async_state)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=1024) :: status_text

        call async_poll(async_state)

        if (async_state%active_pid > 0) then
            status_text = '{"state":"running"'// &
                          ',"run_id":'//trim(json_int(async_state%active_run_id))//'}'
        else if (async_state%pending_run_id > 0) then
            status_text = '{"state":"rerun-pending"'// &
                          ',"run_id":'//trim(json_int(async_state%pending_run_id))//'}'
        else if (async_state%last_run_id > 0) then
            status_text = '{"state":"finished"'// &
                          ',"run_id":'//trim(json_int(async_state%last_run_id))// &
                          ',"exitcode":'// &
                          trim(json_int(async_state%last_exitcode))//'}'
        else
            status_text = '{"state":"idle"}'
        end if

        call make_tool_text_response(id_str, status_text, 0, response)
    end subroutine handle_async_status

    subroutine handle_async_diagnostics(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=8192) :: output_text
        integer :: run_id, ierr

        call async_poll(async_state)
        call requested_run_id(line, async_state, run_id, ierr)
        if (ierr /= 0) then
            call jsonrpc_error(id_str, -32602, 'unknown run_id', response)
            return
        end if

        output_text = ''
        if (run_id > 0 .and. run_id == async_state%last_run_id .and. &
            len_trim(async_state%last_output) > 0) then
            call read_text_file(async_state%last_output, output_text)
        end if

        if (len_trim(output_text) == 0) then
            output_text = '{"state":"idle","diagnostics":""}'
        end if

        call make_tool_text_response(id_str, output_text, 0, response)
    end subroutine handle_async_diagnostics

    subroutine handle_async_cancel(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        integer :: run_id, ierr, exitcode

        call requested_run_id(line, async_state, run_id, ierr)
        if (ierr /= 0) then
            call jsonrpc_error(id_str, -32602, 'unknown run_id', response)
            return
        end if
        if (run_id /= async_state%active_run_id .or. async_state%active_pid <= 0) then
            call jsonrpc_error(id_str, -32602, 'run is not active', response)
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

        block
            character(len=256) :: cancel_text
            cancel_text = '{"cancelled":true,"run_id":'// &
                          trim(json_int(run_id))//'}'
            call make_tool_text_response(id_str, cancel_text, 0, response)
        end block
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

        call extract_json_field(line, '"run_id"', run_text)
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

end module fo_mcp
