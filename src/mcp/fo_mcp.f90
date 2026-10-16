module fo_mcp
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file_alloc
    use fo_json_object, only: json_object_member, json_value_raw, &
        json_text_has_control, json_skip_value
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_array_start, json_array_end, json_key, &
        json_value_string, json_value_bool, &
        json_key_string, json_key_int, json_key_bool, json_to_string
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
        json_parser_next, EV_OBJECT_START => JSON_OBJECT_START, &
        EV_OBJECT_END => JSON_OBJECT_END, EV_ARRAY_START => JSON_ARRAY_START, &
        EV_ARRAY_END => JSON_ARRAY_END, EV_KEY => JSON_KEY, &
        JSON_STRING, JSON_INTEGER, JSON_REAL, JSON_BOOL, &
        JSON_NULL_VAL, JSON_ERROR, JSON_END_OF_INPUT
    use fx_mcp, only: mcp_read_message, mcp_send_response, MCP_FRAME_UNKNOWN
    use fo_check, only: check_result_t, fo_check_run
    use fo_check_output, only: check_result_compact_json
    use fo_commands, only: command_graph, command_info, command_changed, &
        command_clean, command_install, command_check, command_lint
    use fo_fmt, only: fmt_sources, FMT_SELECT_ALL
    use fo_gremlin_request, only: GREMLIN_OPTIONS, N_GREMLIN_OPTIONS, &
        gremlin_apply_adapter_defaults
    use fo_process, only: process_start_fo_check, process_poll_pid, &
        process_cancel_pid, process_suppress_heartbeats
    use fo_version_info, only: FO_VERSION
    use fo_run_queue, only: run_queue_t, RUN_IDLE, RUN_RUNNING
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
    use fo_fs, only: fs_sleep_ms, fs_canonical_path
    use fo_gremlin_supervisor, only: gremlin_handle
    implicit none
    private
    public :: mcp_serve

    integer, parameter :: MAX_LINE = 32768
    integer, parameter :: CANCEL_RETRY_INITIAL_DELAY_MS = 100
    integer, parameter :: CANCEL_RETRY_MAX_DELAY_MS = 5000

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
        character(len=MAX_LINE) :: line
        character(len=:), allocatable :: request, response, raw_value, params_json
        character(len=:), allocatable :: id_str
        character(len=256) :: method
        integer :: framing, read_status, parse_status, property_count
        integer :: cancel_retry_delay_ms
        logical :: eof_flag, valid_version, request_has_id, stop_requested
        type(json_event_t) :: envelope_event
        type(mcp_async_state_t) :: async_state

        call process_suppress_heartbeats(.true.)
        framing = MCP_FRAME_UNKNOWN
        do
            call async_poll(async_state)
            if (async_state%active_pid > 0) then
                call mcp_read_message(line, MAX_LINE, framing, eof_flag, read_status, &
                    timeout_ms=100)
            else
                call mcp_read_message(line, MAX_LINE, framing, eof_flag, read_status)
            end if
            if (eof_flag) exit
            if (read_status /= 0) cycle
            if (len_trim(line) == 0) cycle
            request = trim(line)

            call json_object_member(request, 'jsonrpc', raw_value, property_count, &
                parse_status, envelope_event)
            valid_version = .false.
            if (parse_status == 0 .and. property_count == 1) then
                if (envelope_event%event_type == JSON_STRING) then
                    if (allocated(envelope_event%string_val)) then
                        valid_version = envelope_event%string_val == '2.0' .and. &
                            len(envelope_event%string_val) == 3
                    end if
                end if
            end if
            if (.not. valid_version) then
                call rpc_error('null', -32600, 'invalid JSON-RPC request', response)
                call mcp_send_response(response, framing)
                cycle
            end if
            call extract_json_string_member(request, 'method', method, &
                property_count, parse_status)
            if (parse_status /= 0 .or. property_count /= 1) then
                call rpc_error('null', -32600, 'invalid JSON-RPC request', response)
                call mcp_send_response(response, framing)
                cycle
            end if
            call json_object_member(request, 'id', id_str, property_count, parse_status, &
                envelope_event)
            if (parse_status /= 0 .or. property_count > 1) then
                call rpc_error('null', -32600, 'invalid JSON-RPC id', response)
                call mcp_send_response(response, framing)
                cycle
            end if
            request_has_id = property_count == 1
            if (request_has_id) then
                select case (envelope_event%event_type)
                case (JSON_STRING, JSON_INTEGER, JSON_REAL, JSON_NULL_VAL)
                    ! Preserve the exact legal ID token without numeric conversion.
                case default
                    call rpc_error('null', -32600, 'invalid JSON-RPC id', response)
                    call mcp_send_response(response, framing)
                    cycle
                end select
            else
                id_str = 'null'
            end if
            params_json = '{}'
            call json_object_member(request, 'params', raw_value, property_count, &
                parse_status)
            if (parse_status == 0 .and. property_count == 1) then
                if (len(raw_value) > 0) then
                    if (raw_value(1:1) == '{') params_json = raw_value
                end if
            end if
            call async_poll(async_state)
            stop_requested = .false.

            select case (trim(method))
            case ('initialize')
                call make_initialize_response(id_str, params_json, response)
            case ('initialized', 'notifications/initialized')
                ! notification, no response needed
                cycle
            case ('tools/list')
                call make_tools_list_response(id_str, response)
            case ('tools/call')
                call handle_tools_call(params_json, id_str, response, async_state)
            case ('resources/list')
                call make_resources_list_response(id_str, response)
            case ('resources/read')
                call handle_resources_read(params_json, id_str, response, async_state)
            case ('shutdown')
                call async_cancel_all(async_state, read_status)
                if (read_status /= 0) then
                    call rpc_error(id_str, -32603, &
                        'failed to cancel active run; shutdown remains active', response)
                else
                    call rpc_result(id_str, 'null', response)
                    stop_requested = .true.
                end if
            case default
                if (request_has_id) then
                    call rpc_error(id_str, -32601, 'method not found', response)
                end if
            end select
            if (request_has_id) call mcp_send_response(response, framing)
            if (stop_requested) exit
        end do
        cancel_retry_delay_ms = CANCEL_RETRY_INITIAL_DELAY_MS
        do
            call async_cancel_all(async_state, read_status)
            if (read_status == 0) exit
            call fs_sleep_ms(cancel_retry_delay_ms)
            if (cancel_retry_delay_ms < CANCEL_RETRY_MAX_DELAY_MS) then
                if (cancel_retry_delay_ms > CANCEL_RETRY_MAX_DELAY_MS/2) then
                    cancel_retry_delay_ms = CANCEL_RETRY_MAX_DELAY_MS
                else
                    cancel_retry_delay_ms = 2*cancel_retry_delay_ms
                end if
            end if
        end do
        call process_suppress_heartbeats(.false.)
    end subroutine mcp_serve

    ! ---- JSON-RPC envelopes, built so every response is valid by construction

    subroutine rpc_open(jb, id_str)
        type(json_builder_t), intent(out) :: jb
        character(len=*), intent(in) :: id_str

        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'jsonrpc', '2.0')
        call json_key(jb, 'id')
        call json_value_raw(jb, trim(id_str))
    end subroutine rpc_open

    subroutine rpc_result(id_str, result_json, response)
        character(len=*), intent(in) :: id_str, result_json
        character(len=:), allocatable, intent(out) :: response
        type(json_builder_t) :: jb

        call rpc_open(jb, id_str)
        call json_key(jb, 'result')
        call json_value_raw(jb, result_json)
        call json_object_end(jb)
        response = json_to_string(jb)
    end subroutine rpc_result

    subroutine rpc_error(id_str, code, message, response)
        character(len=*), intent(in) :: id_str, message
        integer, intent(in) :: code
        character(len=:), allocatable, intent(out) :: response
        type(json_builder_t) :: jb

        call rpc_open(jb, id_str)
        call json_key(jb, 'error')
        call json_object_start(jb)
        call json_key_int(jb, 'code', code)
        call json_key_string(jb, 'message', message)
        call json_object_end(jb)
        call json_object_end(jb)
        response = json_to_string(jb)
    end subroutine rpc_error

    subroutine command_response(id_str, text, err_text, exitcode, response)
        !! A tool result: the stdout text, then the stderr text as a second
        !! block when the command wrote any. isError mirrors the exit code.
        character(len=*), intent(in) :: id_str, text, err_text
        integer, intent(in) :: exitcode
        character(len=:), allocatable, intent(out) :: response
        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call json_key(jb, 'content')
        call json_array_start(jb)
        if (len(text) > 0 .or. len(err_text) == 0) call text_block(jb, text)
        if (len(err_text) > 0) call text_block(jb, err_text)
        call json_array_end(jb)
        call json_key_bool(jb, 'isError', exitcode /= 0)
        call json_object_end(jb)
        call rpc_result(id_str, json_to_string(jb), response)
    end subroutine command_response

    subroutine text_block(jb, text)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: text

        call json_object_start(jb)
        call json_key_string(jb, 'type', 'text')
        call json_key_string(jb, 'text', text)
        call json_object_end(jb)
    end subroutine text_block

    subroutine make_tool_text_response(id_str, output_text, exitcode, response)
        character(len=*), intent(in) :: id_str, output_text
        integer, intent(in) :: exitcode
        character(len=:), allocatable, intent(out) :: response

        call command_response(id_str, output_text, '', exitcode, response)
    end subroutine make_tool_text_response

    subroutine handle_tools_call(params_json, id_str, response, async_state)
        character(len=*), intent(in) :: params_json, id_str
        character(len=:), allocatable, intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=64) :: action, mode
        character(len=:), allocatable :: arguments, raw_value, dir, text, err_text
        character(len=:), allocatable :: message
        character(len=512) :: tmpfile, fmt_output
        integer :: exitcode, property_count, parse_status
        type(json_event_t) :: dir_event

        call json_object_member(params_json, 'arguments', arguments, &
            property_count, parse_status)
        if (parse_status /= 0 .or. property_count /= 1 .or. len(arguments) == 0) then
            call rpc_error(id_str, -32602, 'missing MCP arguments object', response)
            return
        end if
        if (arguments(1:1) /= '{') then
            call rpc_error(id_str, -32602, 'MCP arguments must be an object', response)
            return
        end if
        call extract_json_string_member(arguments, 'action', action, &
            property_count, parse_status)
        if (parse_status /= 0 .or. property_count /= 1) then
            call rpc_error(id_str, -32602, 'arguments need one string action', response)
            return
        end if
        select case (trim(action))
        case ('gremlin_start', 'gremlin_status', 'gremlin_wait', &
                'gremlin_events', 'gremlin_failures', 'gremlin_reproduce', &
                'gremlin_stop')
            call handle_gremlin_action(arguments, id_str, response)
            return
        end select
        call json_object_member(arguments, 'dir', raw_value, property_count, &
            parse_status, dir_event)
        dir = '.'
        if (parse_status /= 0 .or. property_count > 1) then
            call rpc_error(id_str, -32602, 'dir must occur at most once', response)
            return
        end if
        if (property_count == 1) then
            if (dir_event%event_type /= JSON_STRING .or. &
                .not. allocated(dir_event%string_val)) then
                call rpc_error(id_str, -32602, 'dir must be a valid string', response)
                return
            end if
            if (len_trim(dir_event%string_val) > 0) &
                dir = fs_canonical_path(trim(dir_event%string_val))
        end if
        select case (trim(action))
        case ('lint')
            message = first_bad_member(arguments, ['fix ', 'deep'], 'b')
        case ('graph')
            message = first_bad_member(arguments, ['dot'], 'b')
        case ('clean')
            message = first_bad_member(arguments, ['cache'], 'b')
        case ('install')
            message = first_bad_member(arguments, ['prefix'], 's')
        case default
            message = ''
        end select
        if (len(message) > 0) then
            call rpc_error(id_str, -32602, message, response)
            return
        end if
        call make_tmpfile('fo_mcp_output', tmpfile)

        text = ''
        err_text = ''
        select case (trim(action))
        case ('check')
            call extract_json_string_member(arguments, 'mode', mode, &
                property_count, parse_status)
            if (trim(mode) == 'start') then
                call delete_tmpfile(tmpfile)
                call handle_async_start(arguments, id_str, response, async_state)
                return
            end if
            call handle_check(arguments, id_str, dir, response)
        case ('status')
            call handle_async_status(id_str, response, async_state)
        case ('diagnostics')
            call handle_async_diagnostics(arguments, id_str, response, async_state)
        case ('cancel')
            call handle_async_cancel(arguments, id_str, response, async_state)
        case ('lint')
            call handle_lint(arguments, id_str, dir, response)
        case ('fmt')
            call fmt_sources(trim(dir), FMT_SELECT_ALL, .false., .false., &
                [character(len=1) ::], 0, fmt_output, exitcode)
            text = 'formatted'
            if (exitcode /= 0) text = 'fo fmt: formatting failed'
            call make_tool_text_response(id_str, text, exitcode, response)
        case ('build')
            call handle_backend_build(id_str, dir, tmpfile, response)
        case ('test')
            call handle_backend_test_named(arguments, id_str, dir, tmpfile, response)
        case ('graph')
            call handle_graph(arguments, id_str, dir, response)
        case ('info')
            call command_info(dir, .false., text, exitcode)
            call command_response(id_str, text, '', exitcode, response)
        case ('changed')
            call command_changed(dir, text, err_text, exitcode)
            call command_response(id_str, text, err_text, exitcode, response)
        case ('clean')
            call handle_clean(arguments, id_str, dir, response)
        case ('install')
            call handle_install(arguments, id_str, dir, response)
        case default
            call rpc_error(id_str, -32602, 'unknown action: '//trim(action), response)
        end select
        call delete_tmpfile(tmpfile)
    end subroutine handle_tools_call

    subroutine handle_gremlin_action(arguments, id_str, response)
        character(len=*), intent(in) :: arguments, id_str
        character(len=:), allocatable, intent(out) :: response

        character(len=4096) :: project_dir
        character(len=64) :: public_action, core_action
        character(len=256) :: message
        character(len=:), allocatable :: request_json, result_json
        type(json_builder_t) :: jb
        integer :: ierr, exitcode

        call normalize_gremlin_arguments(arguments, public_action, core_action, &
            project_dir, request_json, ierr, message)
        if (ierr /= 0) then
            jb = json_new()
            call json_object_start(jb)
            call json_key_string(jb, 'error', trim(message))
            call json_object_end(jb)
            call make_tool_text_response(id_str, json_to_string(jb), 2, response)
            return
        end if

        call gremlin_handle(trim(core_action), fs_canonical_path(trim(project_dir)), &
            trim(request_json), result_json, exitcode)
        call make_tool_text_response(id_str, result_json, exitcode, response)
    end subroutine handle_gremlin_action

    subroutine extract_json_string_member(object_json, property, value, count, ierr)
        character(len=*), intent(in) :: object_json, property
        character(len=*), intent(out) :: value
        integer, intent(out) :: count, ierr

        character(len=:), allocatable :: raw_value
        type(json_event_t) :: event

        value = ''
        call json_object_member(object_json, property, raw_value, count, ierr, event)
        if (ierr /= 0 .or. count /= 1) return
        if (event%event_type /= JSON_STRING .or. .not. allocated(event%string_val)) then
            ierr = 1
            return
        end if
        if (len_trim(event%string_val) > len(value)) then
            ierr = 1
            return
        end if
        value = trim(event%string_val)
    end subroutine extract_json_string_member

    subroutine extract_json_bool_member(object_json, property, value, count, ierr)
        character(len=*), intent(in) :: object_json, property
        logical, intent(out) :: value
        integer, intent(out) :: count, ierr

        character(len=:), allocatable :: raw_value
        type(json_event_t) :: event

        value = .false.
        call json_object_member(object_json, property, raw_value, count, ierr, event)
        if (ierr /= 0 .or. count /= 1) return
        if (event%event_type /= JSON_BOOL) then
            ierr = 1
            return
        end if
        value = event%bool_val
    end subroutine extract_json_bool_member

    logical function bool_option(arguments, name) result(on)
        !! A boolean tool argument; absent is false (first_bad_member has
        !! already rejected duplicates and other types).
        character(len=*), intent(in) :: arguments, name
        integer :: count, ierr

        call extract_json_bool_member(arguments, name, on, count, ierr)
        if (ierr /= 0 .or. count /= 1) on = .false.
    end function bool_option

    function first_bad_member(arguments, names, kind) result(message)
        !! An option must occur at most once and have the declared type
        !! ('b' boolean, 's' string); a duplicate or mistyped one is an error
        !! instead of being read as the default.
        character(len=*), intent(in) :: arguments, names(:), kind
        character(len=:), allocatable :: message
        character(len=:), allocatable :: raw_value
        type(json_event_t) :: event
        integer :: i, count, ierr

        message = ''
        do i = 1, size(names)
            call json_object_member(arguments, trim(names(i)), raw_value, count, ierr, &
                event)
            if (ierr /= 0 .or. count > 1) then
                message = trim(names(i))//' must occur at most once'
            else if (count == 1) then
                if (kind == 'b' .and. event%event_type /= JSON_BOOL) &
                    message = trim(names(i))//' must be a boolean'
                if (kind == 's' .and. event%event_type /= JSON_STRING) &
                    message = trim(names(i))//' must be a string'
            end if
            if (len(message) > 0) return
        end do
    end function first_bad_member

    subroutine normalize_gremlin_arguments(arguments, public_action, core_action, &
            project_dir, request_json, ierr, message)
        !! Split the MCP arguments into the action, the project directory and
        !! the request object that the shared Gremlin core validates.
        character(len=*), intent(in) :: arguments
        character(len=*), intent(out) :: public_action, core_action, project_dir
        character(len=:), allocatable, intent(out) :: request_json
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(json_parser_t) :: parser
        type(json_event_t) :: event, value_event
        type(json_builder_t) :: jb
        character(len=:), allocatable :: key
        integer :: action_count, dir_count, skip_error, value_end

        public_action = ''
        project_dir = '.'
        core_action = ''
        request_json = '{}'
        message = 'malformed Gremlin request JSON'
        ierr = 1
        action_count = 0
        dir_count = 0
        jb = json_new()
        call json_object_start(jb)
        call json_parser_init_strict(parser, arguments)
        call json_parser_next(parser, event)
        if (event%event_type /= EV_OBJECT_START) return
        do
            call json_parser_next(parser, event)
            if (event%event_type == EV_OBJECT_END) exit
            if (event%event_type /= EV_KEY .or. .not. allocated(event%string_val)) return
            key = event%string_val
            call json_parser_next(parser, value_event)
            if (value_event%event_type == JSON_ERROR) return
            if (len(key) == 6 .and. key == 'action') then
                action_count = action_count + 1
                if (value_event%event_type /= JSON_STRING .or. &
                    .not. allocated(value_event%string_val)) then
                    message = 'Gremlin action must be a string'
                    return
                end if
                if (len(value_event%string_val) > len(public_action) .or. &
                    len(value_event%string_val) /= &
                    len_trim(value_event%string_val)) then
                    message = 'Gremlin action must be an exact public name'
                    return
                end if
                public_action = value_event%string_val
            else if (len(key) == 3 .and. key == 'dir') then
                dir_count = dir_count + 1
                if (value_event%event_type /= JSON_STRING .or. &
                    .not. allocated(value_event%string_val)) then
                    message = 'Gremlin dir must be a string'
                    return
                end if
                if (len(value_event%string_val) > len(project_dir)) then
                    message = 'Gremlin dir is too long'
                    return
                end if
                if (len_trim(value_event%string_val) == 0) then
                    message = 'Gremlin dir must be nonempty'
                    return
                end if
                if (json_text_has_control(value_event%string_val)) then
                    message = 'Gremlin dir contains a control character'
                    return
                end if
                project_dir = value_event%string_val
            else
                value_end = value_event%raw_end
                call json_skip_value(parser, value_event, skip_error)
                if (skip_error /= 0) then
                    message = 'malformed Gremlin request JSON'
                    return
                end if
                if (value_event%event_type == EV_ARRAY_START .or. &
                    value_event%event_type == EV_OBJECT_START) value_end = parser%pos - 1
                call json_key(jb, key)
                call json_value_raw(jb, arguments(value_event%raw_start:value_end))
            end if
        end do
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_END_OF_INPUT) return
        if (action_count /= 1 .or. dir_count > 1) then
            message = 'Gremlin arguments need one action and at most one dir'
            return
        end if
        if (index(public_action, 'gremlin_') /= 1) then
            message = 'unknown Gremlin MCP action: '//trim(public_action)
            return
        end if
        core_action = trim(public_action(9:))
        select case (trim(core_action))
        case ('start', 'status', 'wait', 'events', 'failures', 'reproduce', 'stop')
        case default
            message = 'unknown Gremlin MCP action: '//trim(public_action)
            return
        end select
        call json_object_end(jb)
        request_json = json_to_string(jb)
        call gremlin_apply_adapter_defaults(core_action, request_json)
        ierr = 0
        message = ''
    end subroutine normalize_gremlin_arguments

    subroutine handle_check(line, id_str, dir, response)
        character(len=*), intent(in) :: line, id_str, dir
        character(len=:), allocatable, intent(out) :: response

        character(len=:), allocatable :: text, err_text
        character(len=16) :: json_mode
        integer :: option_count, option_error, mode, exitcode

        call extract_json_string_member(line, 'json', json_mode, &
            option_count, option_error)
        if (option_count > 1 .or. (option_count == 1 .and. &
            (option_error /= 0 .or. &
            (trim(json_mode) /= 'compact' .and. trim(json_mode) /= 'full')))) then
            call rpc_error(id_str, -32602, 'invalid json mode', response)
            return
        end if
        mode = 1
        if (option_count == 1 .and. trim(json_mode) == 'full') mode = 2
        call command_check(dir, mode, .true., text, err_text, exitcode)
        call command_response(id_str, text, err_text, exitcode, response)
    end subroutine handle_check

    subroutine handle_lint(line, id_str, dir, response)
        character(len=*), intent(in) :: line, id_str, dir
        character(len=:), allocatable, intent(out) :: response

        character(len=:), allocatable :: text, deep_report, tail, err_text
        integer :: exitcode

        call command_lint(dir, [character(len=1) ::], bool_option(line, 'deep'), &
            bool_option(line, 'fix'), .true., text, deep_report, tail, err_text, exitcode)
        call command_response(id_str, text, err_text, exitcode, response)
    end subroutine handle_lint

    subroutine handle_graph(line, id_str, dir, response)
        character(len=*), intent(in) :: line, id_str, dir
        character(len=:), allocatable, intent(out) :: response

        character(len=:), allocatable :: text, err_text
        integer :: exitcode

        call command_graph(dir, bool_option(line, 'dot'), text, err_text, exitcode)
        call command_response(id_str, text, err_text, exitcode, response)
    end subroutine handle_graph

    subroutine handle_clean(line, id_str, dir, response)
        character(len=*), intent(in) :: line, id_str, dir
        character(len=:), allocatable, intent(out) :: response

        character(len=:), allocatable :: text
        integer :: exitcode

        call command_clean(dir, bool_option(line, 'cache'), text, exitcode)
        call command_response(id_str, text, '', exitcode, response)
    end subroutine handle_clean

    subroutine handle_install(line, id_str, dir, response)
        character(len=*), intent(in) :: line, id_str, dir
        character(len=:), allocatable, intent(out) :: response

        character(len=:), allocatable :: text, err_text, raw_value, prefix
        type(json_event_t) :: event
        integer :: exitcode, prefix_count, prefix_error

        prefix = ''
        call json_object_member(line, 'prefix', raw_value, prefix_count, prefix_error, &
            event)
        if (prefix_error == 0 .and. prefix_count == 1) prefix = trim(event%string_val)
        call command_install(dir, prefix, text, err_text, exitcode)
        call command_response(id_str, text, err_text, exitcode, response)
    end subroutine handle_install

    subroutine handle_backend_build(id_str, dir, tmpfile, response)
        use fo_build_backend, only: backend_build
        character(len=*), intent(in) :: id_str, dir, tmpfile
        character(len=:), allocatable, intent(out) :: response

        type(backend_t) :: b
        character(len=:), allocatable :: output_text
        integer :: exitcode, ierr

        b = detect_backend(trim(dir))
        if (b%kind == BACKEND_NONE) then
            output_text = 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
        else
            call backend_build(b, exitcode, log_file=tmpfile)
            call read_text_file_alloc(tmpfile, output_text, ierr)
            if (ierr /= 0) output_text = 'fo: could not read build log'
        end if
        call delete_tmpfile(tmpfile)
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_backend_build

    subroutine handle_backend_test_named(line, id_str, dir, tmpfile, response)
        use fo_build_backend, only: backend_test, backend_test_names
        use fo_project_model, only: model_request_t, model_selection_t, &
            project_model_t, SELECT_EMPTY
        use fo_project_load, only: project_has_model, project_select, &
            project_run_cases
        use fx_string, only: string_t
        use fo_test_results, only: test_result_entry_t, parse_test_results, &
            format_test_results_json
        use fo_progress, only: progress_suppress
        character(len=*), intent(in) :: line, id_str, dir, tmpfile
        character(len=:), allocatable, intent(out) :: response

        logical :: full_json
        type(backend_t) :: b
        character(len=:), allocatable :: output_text
        character(len=16) :: json_mode
        integer :: exitcode
        character(len=:), allocatable :: test_names(:)
        integer :: n_names
        type(test_result_entry_t), allocatable :: entries(:)
        integer :: n_entries, ierr, i, mode_count, mode_error
        type(model_request_t) :: request
        type(model_selection_t) :: selection
        type(project_model_t) :: model
        character(len=:), allocatable :: message
        type(string_t), allocatable :: widen_reasons(:)
        logical :: only_changed
        integer :: changed_count, changed_error

        b = detect_backend(trim(dir))
        if (b%kind == BACKEND_NONE) then
            output_text = 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            call make_tool_text_response(id_str, output_text, exitcode, response)
            return
        end if

        n_names = 0
        call extract_test_names_from_params(line, test_names, n_names, ierr)
        if (ierr /= 0) then
            call delete_tmpfile(tmpfile)
            call rpc_error(id_str, -32602, &
                'test args must be an array of nonempty names', response)
            return
        end if

        call extract_json_string_member(line, 'json', json_mode, &
            mode_count, mode_error)
        if (mode_count > 1 .or. (mode_count == 1 .and. &
            (mode_error /= 0 .or. &
            (json_mode /= 'compact' .and. json_mode /= 'full')))) then
            call delete_tmpfile(tmpfile)
            call rpc_error(id_str, -32602, 'invalid test json mode', response)
            return
        end if

        call extract_json_bool_member(line, 'only_changed', only_changed, &
            changed_count, changed_error)
        if (changed_count > 1 .or. changed_error /= 0) then
            call delete_tmpfile(tmpfile)
            call rpc_error(id_str, -32602, 'only_changed must be a boolean', response)
            return
        end if
        if (only_changed .and. .not. project_has_model(b)) then
            output_text = 'fo: only_changed needs the project model '// &
                '(unsupported with FO_CMAKE_NATIVE)'
            call delete_tmpfile(tmpfile)
            call make_tool_text_response(id_str, output_text, 1, response)
            return
        end if

        if (.not. project_has_model(b)) then
            ! Frozen FO_CMAKE_NATIVE route: the CTest registry is
            ! authoritative.
            call progress_suppress(.true.)
            if (n_names > 0) then
                call backend_test_names(b, test_names(1:n_names), n_names, &
                    exitcode, .true., tmpfile)
            else
                call backend_test(b, exitcode, log_file=tmpfile)
            end if
            call progress_suppress(.false.)
        else
            ! Same selection as `fo test`: the project model decides.
            allocate (request%names(n_names), request%include_labels(0), &
                request%exclude_labels(0), request%layers(0))
            do i = 1, n_names
                request%names(i)%s = trim(test_names(i))
            end do
            call project_select(b, '', request, only_changed, model, selection, &
                widen_reasons, ierr, message)
            if (ierr == 0 .and. selection%status == SELECT_EMPTY) then
                if (only_changed) then
                    output_text = 'no affected tests'
                else if (model%n_cases == 0 .and. n_names == 0) then
                    output_text = 'fo: project registers no tests'
                else
                    ierr = 1
                    message = selection%message
                end if
                if (ierr == 0) then
                    call delete_tmpfile(tmpfile)
                    call make_tool_text_response(id_str, output_text, 0, response)
                    return
                end if
            end if
            if (ierr /= 0) then
                output_text = 'fo: '//message
                call delete_tmpfile(tmpfile)
                call make_tool_text_response(id_str, output_text, 1, response)
                return
            end if
            call progress_suppress(.true.)
            call project_run_cases(b, model, selection%cases, '', .false., &
                tmpfile, exitcode, selection)
            call progress_suppress(.false.)
        end if

        call parse_test_results(tmpfile, entries, n_entries, ierr)
        full_json = mode_error == 0 .and. mode_count == 1 .and. &
            json_mode == 'full'
        if (ierr /= 0) then
            output_text = 'fo: could not parse test results'
            exitcode = 1
        else if (full_json) then
            call format_test_results_json(entries, n_entries, exitcode, &
                output_text, tmpfile)
        else if (n_entries > 0 .or. (mode_error == 0 .and. mode_count == 1 &
                .and. json_mode == 'compact')) then
            ! The default is the compact report: an agent gets the verdict,
            ! the failures and a handle, not every passing case.
            call format_test_results_json(entries, n_entries, exitcode, &
                output_text, tmpfile, compact=.true.)
        else
            call read_text_file_alloc(tmpfile, output_text, ierr)
            if (ierr /= 0) then
                output_text = 'fo: could not read test diagnostics'
                exitcode = 1
            end if
        end if
        call delete_tmpfile(tmpfile)
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_backend_test_named

    subroutine extract_test_names_from_params(line, names, n_names, ierr)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: names(:)
        integer, intent(out) :: n_names, ierr

        character(len=:), allocatable :: args_json
        type(json_parser_t) :: parser
        type(json_event_t) :: event, array_event
        integer :: args_count, max_name_length, name_index

        n_names = 0
        call json_object_member(line, 'args', args_json, args_count, ierr, array_event)
        if (ierr /= 0) return
        if (args_count == 0) return
        if (args_count /= 1 .or. array_event%event_type /= EV_ARRAY_START) then
            ierr = 1
            return
        end if
        max_name_length = 0
        call json_parser_init_strict(parser, args_json)
        call json_parser_next(parser, event)
        do
            call json_parser_next(parser, event)
            if (event%event_type == EV_ARRAY_END) exit
            if (event%event_type /= JSON_STRING .or. &
                .not. allocated(event%string_val)) then
                ierr = 1
                return
            end if
            if (len_trim(event%string_val) == 0) then
                ierr = 1
                return
            end if
            n_names = n_names + 1
            max_name_length = max(max_name_length, len(event%string_val))
        end do
        if (n_names == 0) return
        allocate (character(len=max(1, max_name_length)) :: names(n_names))

        call json_parser_init_strict(parser, args_json)
        call json_parser_next(parser, event)
        name_index = 0
        do
            call json_parser_next(parser, event)
            if (event%event_type == EV_ARRAY_END .or. &
                event%event_type == JSON_ERROR .or. &
                event%event_type == JSON_END_OF_INPUT) exit
            if (event%event_type /= JSON_STRING .or. &
                .not. allocated(event%string_val)) exit
            name_index = name_index + 1
            if (name_index > n_names) exit
            names(name_index) = event%string_val
        end do
    end subroutine extract_test_names_from_params

    subroutine handle_resources_read(params_json, id_str, response, async_state)
        character(len=*), intent(in) :: params_json, id_str
        character(len=:), allocatable, intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=256) :: uri
        character(len=:), allocatable :: output_text
        type(json_builder_t) :: jb
        integer :: property_count, parse_status, read_error
        type(check_result_t) :: check_res

        call extract_json_string_member(params_json, 'uri', uri, &
            property_count, parse_status)
        call async_poll(async_state)

        if (parse_status == 0 .and. property_count == 1 .and. &
            trim(uri) == 'fo://diagnostics') then
            if (len_trim(async_state%last_output) > 0) then
                call read_text_file_alloc(async_state%last_output, &
                    output_text, read_error)
                if (read_error /= 0) then
                    call rpc_error(id_str, -32603, &
                        'could not read completed check diagnostics', response)
                    return
                end if
            else
                call fo_check_run('.', check_res)
                output_text = check_result_compact_json(check_res)
            end if

            jb = json_new()
            call json_object_start(jb)
            call json_key(jb, 'contents')
            call json_array_start(jb)
            call json_object_start(jb)
            call json_key_string(jb, 'uri', 'fo://diagnostics')
            call json_key_string(jb, 'mimeType', 'text/plain')
            call json_key_string(jb, 'text', output_text)
            call json_object_end(jb)
            call json_array_end(jb)
            call json_object_end(jb)
            call rpc_result(id_str, json_to_string(jb), response)
        else
            call rpc_error(id_str, -32602, 'unknown resource', response)
        end if
    end subroutine handle_resources_read

    subroutine handle_async_start(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=:), allocatable, intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=512) :: root
        character(len=32) :: output_mode
        type(json_builder_t) :: jb
        integer :: ierr, run_id, started_before
        integer :: root_count, root_error, mode_count, mode_error
        logical :: pending

        call extract_json_string_member(line, 'root', root, root_count, root_error)
        if (root_count > 1 .or. (root_count == 1 .and. root_error /= 0)) then
            call rpc_error(id_str, -32602, 'invalid root', response)
            return
        end if
        if (root_count == 0 .or. len_trim(root) == 0) root = '.'
        output_mode = 'compact'
        call extract_json_string_member(line, 'json', output_mode, &
            mode_count, mode_error)
        if (mode_count > 1 .or. (mode_count == 1 .and. &
            (mode_error /= 0 .or. &
            (output_mode /= 'compact' .and. output_mode /= 'full')))) then
            call rpc_error(id_str, -32602, 'invalid json mode', response)
            return
        end if
        if (mode_count == 0) output_mode = 'compact'
        started_before = async_state%queue%started
        call async_state%queue%request(root, output_mode, ierr)
        if (ierr /= 0) then
            call rpc_error(id_str, -32602, 'invalid root', response)
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
                call rpc_error(id_str, -32603, 'could not start check', response)
                return
            end if
            run_id = async_state%active_run_id
        end if

        jb = json_new()
        call json_object_start(jb)
        call json_key_int(jb, 'run_id', run_id)
        call json_key_string(jb, 'state', merge('rerun-pending', 'running      ', &
            pending))
        call json_key_bool(jb, 'pending', pending)
        call json_object_end(jb)
        call rpc_result(id_str, json_to_string(jb), response)
    end subroutine handle_async_start

    subroutine handle_async_status(id_str, response, async_state)
        character(len=*), intent(in) :: id_str
        character(len=:), allocatable, intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        type(json_builder_t) :: jb

        call async_poll(async_state)

        jb = json_new()
        call json_object_start(jb)
        if (async_state%active_pid > 0) then
            call json_key_string(jb, 'state', 'running')
            call json_key_int(jb, 'run_id', async_state%active_run_id)
        else if (async_state%pending_run_id > 0) then
            call json_key_string(jb, 'state', 'rerun-pending')
            call json_key_int(jb, 'run_id', async_state%pending_run_id)
        else if (async_state%last_run_id > 0) then
            call json_key_string(jb, 'state', 'finished')
            call json_key_int(jb, 'run_id', async_state%last_run_id)
            call json_key_int(jb, 'exitcode', async_state%last_exitcode)
        else
            call json_key_string(jb, 'state', 'idle')
        end if
        call json_object_end(jb)
        call make_tool_text_response(id_str, json_to_string(jb), 0, response)
    end subroutine handle_async_status

    subroutine handle_async_diagnostics(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=:), allocatable, intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=:), allocatable :: output_text
        integer :: run_id, ierr

        call async_poll(async_state)
        call requested_run_id(line, async_state, run_id, ierr)
        if (ierr /= 0) then
            call rpc_error(id_str, -32602, 'unknown run_id', response)
            return
        end if

        output_text = ''
        if (run_id > 0 .and. run_id == async_state%last_run_id .and. &
            len_trim(async_state%last_output) > 0) then
            call read_text_file_alloc(async_state%last_output, output_text, ierr)
            if (ierr /= 0) then
                call rpc_error(id_str, -32603, &
                    'could not read completed check diagnostics', response)
                return
            end if
        end if

        if (len_trim(output_text) == 0) then
            output_text = '{"state":"idle","diagnostics":""}'
        end if

        call make_tool_text_response(id_str, output_text, 0, response)
    end subroutine handle_async_diagnostics

    subroutine handle_async_cancel(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=:), allocatable, intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        type(json_builder_t) :: jb
        integer :: run_id, ierr, exitcode

        call requested_run_id(line, async_state, run_id, ierr)
        if (ierr /= 0) then
            call rpc_error(id_str, -32602, 'unknown run_id', response)
            return
        end if
        if (run_id /= async_state%active_run_id .or. async_state%active_pid <= 0) then
            call rpc_error(id_str, -32602, 'run is not active', response)
            return
        end if

        call process_cancel_pid(async_state%active_pid, exitcode)
        if (exitcode /= 0) then
            call rpc_error(id_str, -32603, &
                'failed to cancel active run; ownership remains active', response)
            return
        end if
        async_state%active_pid = 0
        call async_state%queue%finish(130)
        async_state%last_run_id = run_id
        async_state%last_exitcode = 130
        async_state%last_output = async_state%active_output
        async_state%active_output = ''
        async_state%active_run_id = 0
        call async_start_pending_if_ready(async_state)

        jb = json_new()
        call json_object_start(jb)
        call json_key_bool(jb, 'cancelled', .true.)
        call json_key_int(jb, 'run_id', run_id)
        call json_object_end(jb)
        call make_tool_text_response(id_str, json_to_string(jb), 0, response)
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

    subroutine async_cancel_all(async_state, cancel_status)
        type(mcp_async_state_t), intent(inout) :: async_state
        integer, intent(out) :: cancel_status

        integer :: exitcode, attempt

        cancel_status = 0
        if (async_state%active_pid > 0) then
            exitcode = 1
            do attempt = 1, 3
                call process_cancel_pid(async_state%active_pid, exitcode)
                if (exitcode == 0) exit
                if (attempt < 3) call fs_sleep_ms(25*attempt)
            end do
            if (exitcode /= 0) then
                cancel_status = exitcode
                return
            end if
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
        character(len=:), allocatable :: raw_value
        type(json_event_t) :: run_event
        integer :: iostat, run_count

        ierr = 0
        run_id = async_state%last_run_id
        call json_object_member(line, 'run_id', raw_value, run_count, ierr, run_event)
        if (ierr /= 0 .or. run_count > 1) then
            ierr = 1
            return
        end if
        if (run_count == 0) then
            if (async_state%active_run_id > 0) run_id = async_state%active_run_id
            if (async_state%pending_run_id > 0) run_id = async_state%pending_run_id
            return
        end if
        if (run_event%event_type == JSON_STRING .and. &
            allocated(run_event%string_val)) then
            if (len(run_event%string_val) > len(run_text)) then
                ierr = 1
                return
            end if
            run_text = run_event%string_val
            if (trim(run_text) == 'latest') then
                if (async_state%active_run_id > 0) run_id = async_state%active_run_id
                if (async_state%pending_run_id > 0) run_id = async_state%pending_run_id
                return
            end if
        else if (run_event%event_type == JSON_INTEGER .and. &
                allocated(run_event%raw_val)) then
            if (len(run_event%raw_val) > len(run_text)) then
                ierr = 1
                return
            end if
            run_text = run_event%raw_val
        else
            ierr = 1
            return
        end if
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

    subroutine make_initialize_response(id_str, params_json, response)
        character(len=*), intent(in) :: id_str, params_json
        character(len=:), allocatable, intent(out) :: response

        character(len=32) :: proto_ver
        type(json_builder_t) :: jb
        integer :: property_count, parse_status

        call extract_json_string_member(params_json, 'protocolVersion', proto_ver, &
            property_count, parse_status)
        if (parse_status /= 0 .or. property_count /= 1 .or. &
            len_trim(proto_ver) == 0) proto_ver = '2025-03-26'
        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'protocolVersion', trim(proto_ver))
        call json_key(jb, 'capabilities')
        call json_object_start(jb)
        call json_key(jb, 'tools')
        call json_object_start(jb)
        call json_key_bool(jb, 'listChanged', .false.)
        call json_object_end(jb)
        call json_key(jb, 'resources')
        call json_object_start(jb)
        call json_key_bool(jb, 'listChanged', .false.)
        call json_object_end(jb)
        call json_object_end(jb)
        call json_key(jb, 'serverInfo')
        call json_object_start(jb)
        call json_key_string(jb, 'name', 'fo')
        call json_key_string(jb, 'version', FO_VERSION)
        call json_object_end(jb)
        call json_object_end(jb)
        call rpc_result(id_str, json_to_string(jb), response)
    end subroutine make_initialize_response

    subroutine schema_property(jb, name, type_name, description, choices)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: name, type_name
        character(len=*), intent(in), optional :: description, choices

        call json_key(jb, name)
        call json_object_start(jb)
        call json_key_string(jb, 'type', type_name)
        if (present(choices)) call enum_array(jb, choices)
        if (present(description)) call json_key_string(jb, 'description', description)
        call json_object_end(jb)
    end subroutine schema_property

    subroutine enum_array(jb, choices)
        !! "enum": [...] from a comma separated list.
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: choices
        integer :: start, comma

        call json_key(jb, 'enum')
        call json_array_start(jb)
        start = 1
        do
            comma = index(choices(start:), ',')
            if (comma == 0) then
                call json_value_string(jb, trim(choices(start:)))
                exit
            end if
            call json_value_string(jb, choices(start:start + comma - 2))
            start = start + comma
        end do
        call json_array_end(jb)
    end subroutine enum_array

    subroutine make_tools_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=:), allocatable, intent(out) :: response

        type(json_builder_t) :: jb
        integer :: i
        character(len=*), parameter :: actions = 'check,status,diagnostics,cancel,'// &
            'build,test,graph,info,changed,clean,lint,fmt,install,gremlin_start,'// &
            'gremlin_status,gremlin_wait,gremlin_events,gremlin_failures,'// &
            'gremlin_reproduce,gremlin_stop'

        jb = json_new()
        call json_object_start(jb)
        call json_key(jb, 'tools')
        call json_array_start(jb)
        call json_object_start(jb)
        call json_key_string(jb, 'name', 'fo')
        call json_key_string(jb, 'description', 'Fortran build driver')
        call json_key(jb, 'inputSchema')
        call json_object_start(jb)
        call json_key_string(jb, 'type', 'object')
        call json_key(jb, 'properties')
        call json_object_start(jb)
        call schema_property(jb, 'action', 'string', 'Action to run', actions)
        call schema_property(jb, 'dir', 'string', 'Project directory (default: cwd)')
        call schema_property(jb, 'json', 'string', 'check/test: JSON result; test '// &
            'defaults to compact (verdict first, failures with excerpts and a '// &
            'log handle), full lists every case with its whole output', &
            'compact,full')
        call schema_property(jb, 'dot', 'boolean', 'graph: output Graphviz DOT')
        call schema_property(jb, 'cache', 'boolean', &
            'clean: also purge the shared content store')
        call schema_property(jb, 'prefix', 'string', 'install: destination prefix')
        call schema_property(jb, 'fix', 'boolean', &
            'lint: remove unused imports in place')
        call schema_property(jb, 'deep', 'boolean', &
            'lint: add the fluff deep diagnostics')
        do i = 1, N_GREMLIN_OPTIONS
            call gremlin_schema_property(jb, GREMLIN_OPTIONS(i))
        end do
        call json_object_end(jb)
        call json_key(jb, 'required')
        call json_array_start(jb)
        call json_value_string(jb, 'action')
        call json_array_end(jb)
        call json_object_end(jb)
        call json_object_end(jb)
        call json_array_end(jb)
        call json_object_end(jb)
        call rpc_result(id_str, json_to_string(jb), response)
    end subroutine make_tools_list_response

    subroutine gremlin_schema_property(jb, option)
        use fo_gremlin_request, only: gremlin_option_t, GA_UNBOUNDED
        type(json_builder_t), intent(inout) :: jb
        type(gremlin_option_t), intent(in) :: option

        call json_key(jb, trim(option%key))
        call json_object_start(jb)
        select case (trim(option%kind))
        case ('list')
            call json_key_string(jb, 'type', 'array')
            call json_key(jb, 'items')
            call json_object_start(jb)
            call json_key_string(jb, 'type', 'string')
            call json_object_end(jb)
        case ('int')
            call json_key_string(jb, 'type', 'integer')
            if (option%lo == option%hi) then
                call json_key(jb, 'const')
                call json_value_raw(jb, int_text(option%lo))
            else
                if (option%lo /= -int(huge(0), int64) - 1_int64) then
                    call json_key(jb, 'minimum')
                    call json_value_raw(jb, int_text(option%lo))
                end if
                if (option%hi /= GA_UNBOUNDED .and. option%hi /= int(huge(0), int64)) then
                    call json_key(jb, 'maximum')
                    call json_value_raw(jb, int_text(option%hi))
                end if
            end if
        case ('bool')
            call json_key_string(jb, 'type', 'boolean')
        case default
            call json_key_string(jb, 'type', 'string')
            if (len_trim(option%choices) > 0) call enum_array(jb, option%choices)
        end select
        if (len_trim(option%note) > 0) &
            call json_key_string(jb, 'description', trim(option%note))
        call json_object_end(jb)
    end subroutine gremlin_schema_property

    function int_text(value) result(text)
        integer(int64), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=24) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function int_text

    subroutine make_resources_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=:), allocatable, intent(out) :: response

        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call json_key(jb, 'resources')
        call json_array_start(jb)
        call json_object_start(jb)
        call json_key_string(jb, 'uri', 'fo://diagnostics')
        call json_key_string(jb, 'name', 'diagnostics')
        call json_key_string(jb, 'description', 'Current fo check diagnostics')
        call json_key_string(jb, 'mimeType', 'text/plain')
        call json_object_end(jb)
        call json_array_end(jb)
        call json_object_end(jb)
        call rpc_result(id_str, json_to_string(jb), response)
    end subroutine make_resources_list_response

end module fo_mcp
