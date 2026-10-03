module fo_mcp
    use fo_util, only: json_bool, json_int, extract_json_field, make_tmpfile, &
        delete_tmpfile, read_text_file, clean_root_build_artifacts, &
        strip_path_prefix_in_str, jsonrpc_error_fixed => jsonrpc_error, &
        jsonrpc_null_fixed => jsonrpc_null
    use fx_json_build, only: json_escape_string
    use fx_mcp, only: mcp_read_message, mcp_send_response, MCP_FRAME_UNKNOWN
    use fo_check, only: check_result_t, fo_check_run
    use fo_check_output, only: check_result_compact_json, &
        check_result_full_json
    use fo_fmt, only: fo_fmt_run
    use fo_capabilities, only: capabilities_t, detect_capabilities, &
        capabilities_json
    use fo_process, only: process_start_fo_check, process_poll_pid, &
        process_cancel_pid, process_run_argv_logged, argv_push, &
        process_suppress_heartbeats
    use fo_progress, only: progress_suppress
    use fo_version_info, only: FO_VERSION
    use fo_run_queue, only: run_queue_t, RUN_IDLE, RUN_RUNNING, &
        RUN_RERUN_PENDING
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
    use fo_fs, only: fs_sleep_ms
    use fo_gremlin_supervisor, only: gremlin_handle
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
        character(len=MAX_LINE) :: line
        character(len=MAX_LINE) :: params_json, raw_value
        character(len=:), allocatable :: response
        character(len=256) :: method
        character(len=MAX_LINE) :: id_str
        integer :: framing, read_status, parse_status, property_count
        logical :: eof_flag
        type(mcp_async_state_t) :: async_state

        call process_suppress_heartbeats(.true.)
        framing = MCP_FRAME_UNKNOWN
        do
            call mcp_read_message(line, MAX_LINE, framing, eof_flag, read_status)
            if (eof_flag) exit
            if (read_status /= 0) cycle
            if (len_trim(line) == 0) cycle

            call extract_json_string_member(line, 'method', method, &
                property_count, parse_status)
            if (parse_status /= 0 .or. property_count /= 1) then
                call jsonrpc_error('null', -32600, 'invalid JSON-RPC request', response)
                call mcp_send_response(trim(response), framing)
                cycle
            end if
            call extract_json_member(line, 'id', id_str, property_count, parse_status)
            if (parse_status /= 0 .or. property_count > 1) then
                call jsonrpc_error('null', -32600, 'invalid JSON-RPC id', response)
                call mcp_send_response(trim(response), framing)
                cycle
            end if
            if (property_count == 0) id_str = ''
            params_json = '{}'
            call extract_json_member(line, 'params', raw_value, property_count, &
                parse_status)
            if (parse_status == 0 .and. property_count == 1) then
                if (len_trim(raw_value) > 0) then
                    if (raw_value(1:1) == '{') params_json = trim(raw_value)
                end if
            end if
            call async_poll(async_state)

            select case (trim(method))
            case ('initialize')
                call make_initialize_response(id_str, params_json, response)
                call mcp_send_response(trim(response), framing)
            case ('initialized')
                ! notification, no response needed
                cycle
            case ('tools/list')
                call make_tools_list_response(id_str, response)
                call mcp_send_response(trim(response), framing)
            case ('tools/call')
                call handle_tools_call(params_json, id_str, response, async_state)
                call mcp_send_response(trim(response), framing)
            case ('resources/list')
                call make_resources_list_response(id_str, response)
                call mcp_send_response(trim(response), framing)
            case ('resources/read')
                call handle_resources_read(params_json, id_str, response, async_state)
                call mcp_send_response(trim(response), framing)
            case ('shutdown')
                call async_cancel_all(async_state, read_status)
                if (read_status /= 0) then
                    call jsonrpc_error(id_str, -32603, &
                        'failed to cancel active run; shutdown remains active', response)
                    call mcp_send_response(trim(response), framing)
                    cycle
                end if
                call jsonrpc_null(id_str, response)
                call mcp_send_response(trim(response), framing)
                exit
            case default
                if (len_trim(id_str) > 0) then
                    call jsonrpc_error(id_str, -32601, &
                        'method not found', response)
                    call mcp_send_response(trim(response), framing)
                end if
            end select
        end do
        do
            call async_cancel_all(async_state, read_status)
            if (read_status == 0) exit
            call fs_sleep_ms(100)
        end do
        call process_suppress_heartbeats(.false.)
    end subroutine mcp_serve

    subroutine handle_tools_call(params_json, id_str, response, async_state)
        character(len=*), intent(in) :: params_json, id_str
        character(len=:), allocatable, intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=64) :: action, mode
        character(len=MAX_LINE) :: arguments, raw_value
        character(len=MAX_LINE) :: decoded_dir
        character(len=16384) :: output_text
        integer :: exitcode, property_count, parse_status
        logical :: valid_string
        character(len=512) :: tmpfile, dir
        type(check_result_t) :: check_res

        call extract_json_member(params_json, 'arguments', arguments, &
            property_count, parse_status)
        if (parse_status /= 0 .or. property_count /= 1 .or. &
            len_trim(arguments) == 0) then
            call jsonrpc_error(id_str, -32602, 'missing MCP arguments object', response)
            return
        end if
        if (arguments(1:1) /= '{') then
            call jsonrpc_error(id_str, -32602, 'MCP arguments must be an object', response)
            return
        end if
        call extract_json_string_member(arguments, 'action', action, &
            property_count, parse_status)
        if (parse_status /= 0 .or. property_count /= 1) then
            call jsonrpc_error(id_str, -32602, 'arguments need one string action', response)
            return
        end if
        call extract_json_member(arguments, 'dir', raw_value, property_count, &
            parse_status)
        dir = ''
        if (parse_status /= 0 .or. property_count > 1) then
            call jsonrpc_error(id_str, -32602, 'dir must occur at most once', response)
            return
        end if
        if (property_count == 1) then
            call decode_json_string(raw_value, decoded_dir, valid_string)
            if (.not. valid_string .or. len_trim(decoded_dir) > len(dir)) then
                call jsonrpc_error(id_str, -32602, 'dir must be a valid string', response)
                return
            end if
            dir = trim(decoded_dir)
        end if
        if (len_trim(dir) == 0) dir = '.'
        call make_tmpfile('fo_mcp_output', tmpfile)

        select case (trim(action))
        case ('gremlin_start', 'gremlin_status', 'gremlin_wait', &
                'gremlin_events', 'gremlin_failures', 'gremlin_reproduce', &
                'gremlin_stop')
            call handle_gremlin_action(arguments, id_str, response)
            call delete_tmpfile(tmpfile)
            return
        case ('check')
            call extract_json_field(arguments, '"mode"', mode)
            if (trim(mode) == 'start') then
                call handle_async_start(arguments, id_str, response, async_state)
                return
            end if
            call handle_check(arguments, id_str, dir, check_res, output_text, &
                exitcode, response)
        case ('status')
            call handle_async_status(id_str, response, async_state)
            return
        case ('diagnostics')
            call handle_async_diagnostics(arguments, id_str, response, async_state)
            return
        case ('cancel')
            call handle_async_cancel(arguments, id_str, response, async_state)
            return
        case ('lint')
            call handle_lint(arguments, id_str, dir, output_text, exitcode, response)
        case ('fmt')
            call fo_fmt_run(trim(dir), exitcode)
            if (exitcode == 0) then
                output_text = 'formatted'
            else
                output_text = 'fo fmt: formatting failed'
            end if
            call make_tool_text_response(id_str, output_text, exitcode, response)
        case ('build')
            call handle_backend_build(id_str, dir, tmpfile, response)
            return
        case ('test')
            call handle_backend_test_named(arguments, id_str, dir, tmpfile, response)
            return
        case ('graph')
            call handle_graph(arguments, id_str, dir, output_text, exitcode, response)
        case ('info')
            call handle_info(id_str, dir, output_text, exitcode, response)
        case ('changed')
            call handle_changed(id_str, dir, output_text, exitcode, response)
        case ('clean')
            call handle_clean(arguments, id_str, dir, output_text, exitcode, response)
        case ('install')
            call handle_install(arguments, id_str, dir, output_text, exitcode, response)
        case default
            call jsonrpc_error(id_str, -32602, &
                'unknown action: '//trim(action), response)
        end select
        call delete_tmpfile(tmpfile)
    end subroutine handle_tools_call

    subroutine handle_gremlin_action(arguments, id_str, response)
        character(len=*), intent(in) :: arguments, id_str
        character(len=:), allocatable, intent(out) :: response

        character(len=MAX_LINE) :: project_dir
        character(len=64) :: public_action, core_action
        character(len=256) :: message
        character(len=:), allocatable :: request_json, result_json
        integer :: ierr, exitcode

        call normalize_gremlin_arguments(arguments, public_action, core_action, &
            project_dir, request_json, ierr, message)
        if (ierr /= 0) then
            call make_tool_text_response(id_str, '{"error":"'// &
                trim(json_escape_string(trim(message)))//'"}', 2, response)
            return
        end if

        call gremlin_handle(trim(core_action), trim(project_dir), &
            trim(request_json), result_json, exitcode)
        call make_tool_text_response(id_str, result_json, exitcode, response)
    end subroutine handle_gremlin_action

    subroutine extract_json_member(object_json, property, raw_value, count, ierr)
        character(len=*), intent(in) :: object_json, property
        character(len=*), intent(out) :: raw_value
        integer, intent(out) :: count, ierr

        character(len=MAX_LINE) :: key
        integer :: position, key_status, value_end, value_first, key_length
        logical :: closed

        raw_value = ''
        count = 0
        ierr = 1
        position = 1
        call skip_json_space(object_json, position)
        if (position > len_trim(object_json)) return
        if (object_json(position:position) /= '{') return
        position = position + 1
        closed = .false.
        do
            call skip_json_space(object_json, position)
            if (position > len_trim(object_json)) return
            if (object_json(position:position) == '}') then
                position = position + 1
                closed = .true.
                exit
            end if
            call read_json_string(object_json, position, key, key_status, key_length)
            if (key_status /= 0) return
            call skip_json_space(object_json, position)
            if (position > len_trim(object_json)) return
            if (object_json(position:position) /= ':') return
            position = position + 1
            call skip_json_space(object_json, position)
            value_first = position
            call scan_json_value(object_json, position, value_end, key_status)
            if (key_status /= 0) return
            if (json_key_matches(key, key_length, property)) then
                count = count + 1
                if (count == 1) then
                    if (value_end - value_first + 1 > len(raw_value)) return
                    raw_value = object_json(value_first:value_end)
                end if
            end if
            position = value_end + 1
            call skip_json_space(object_json, position)
            if (position > len_trim(object_json)) return
            if (object_json(position:position) == ',') then
                position = position + 1
                call skip_json_space(object_json, position)
                if (position > len_trim(object_json)) return
                if (object_json(position:position) == '}') return
            else if (object_json(position:position) == '}') then
                position = position + 1
                closed = .true.
                exit
            else
                return
            end if
        end do
        if (.not. closed) return
        call skip_json_space(object_json, position)
        if (position <= len_trim(object_json)) return
        ierr = 0
    end subroutine extract_json_member

    subroutine extract_json_string_member(object_json, property, value, count, ierr)
        character(len=*), intent(in) :: object_json, property
        character(len=*), intent(out) :: value
        integer, intent(out) :: count, ierr

        character(len=MAX_LINE) :: raw_value, decoded
        logical :: valid

        value = ''
        call extract_json_member(object_json, property, raw_value, count, ierr)
        if (ierr /= 0 .or. count /= 1) return
        call decode_json_string(raw_value, decoded, valid)
        if (.not. valid .or. len_trim(decoded) > len(value)) then
            ierr = 1
            return
        end if
        value = trim(decoded)
    end subroutine extract_json_string_member

    subroutine normalize_gremlin_arguments(arguments, public_action, core_action, &
            project_dir, request_json, ierr, message)
        character(len=*), intent(in) :: arguments
        character(len=*), intent(out) :: public_action, core_action, project_dir
        character(len=:), allocatable, intent(out) :: request_json
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        integer, parameter :: MAX_PROPERTIES = 128
        character(len=128) :: keys(MAX_PROPERTIES)
        character(len=MAX_LINE) :: decoded
        integer :: key_first(MAX_PROPERTIES), key_last(MAX_PROPERTIES)
        integer :: key_length(MAX_PROPERTIES)
        integer :: value_first(MAX_PROPERTIES), value_last(MAX_PROPERTIES)
        integer :: n_properties, position, parse_status, action_count, dir_count
        integer :: i, value_end, action_length
        logical :: valid_string

        keys = ''
        key_first = 0
        key_last = 0
        key_length = 0
        value_first = 0
        value_last = 0
        n_properties = 0
        action_count = 0
        dir_count = 0
        public_action = ''
        project_dir = '.'
        core_action = ''
        message = ''
        ierr = 1

        position = 2
        call skip_json_space(arguments, position)
        do while (position <= len_trim(arguments))
            if (arguments(position:position) == '}') exit
            if (n_properties >= MAX_PROPERTIES) then
                message = 'too many Gremlin request fields'
                return
            end if
            key_first(n_properties + 1) = position
            call read_json_string(arguments, position, decoded, parse_status, &
                key_length(n_properties + 1))
            if (parse_status /= 0) then
                message = 'malformed Gremlin request key'
                return
            end if
            n_properties = n_properties + 1
            key_last(n_properties) = position - 1
            keys(n_properties) = trim(decoded)
            call skip_json_space(arguments, position)
            if (position > len_trim(arguments)) then
                message = 'missing Gremlin request field value'
                return
            end if
            if (arguments(position:position) /= ':') then
                message = 'malformed Gremlin request field'
                return
            end if
            position = position + 1
            call skip_json_space(arguments, position)
            value_first(n_properties) = position
            call scan_json_value(arguments, position, value_end, parse_status)
            if (parse_status /= 0) then
                message = 'malformed Gremlin request field value'
                return
            end if
            value_last(n_properties) = value_end
            position = value_end + 1
            call skip_json_space(arguments, position)
            if (position > len_trim(arguments)) then
                message = 'unterminated Gremlin arguments object'
                return
            end if
            if (arguments(position:position) == ',') then
                position = position + 1
                call skip_json_space(arguments, position)
            else if (arguments(position:position) == '}') then
                exit
            else
                message = 'malformed Gremlin arguments separator'
                return
            end if
        end do

        do i = 1, n_properties
            if (json_key_matches(keys(i), key_length(i), 'action')) then
                action_count = action_count + 1
                call decode_json_string(arguments(value_first(i):value_last(i)), &
                    decoded, valid_string, action_length)
                if (.not. valid_string) then
                    message = 'Gremlin action must be a string'
                    return
                end if
                if (action_length > len(public_action) .or. &
                    action_length /= len_trim(decoded)) then
                    message = 'Gremlin action must be an exact public name'
                    return
                end if
                public_action = trim(decoded)
            else if (json_key_matches(keys(i), key_length(i), 'dir')) then
                dir_count = dir_count + 1
                call decode_json_string(arguments(value_first(i):value_last(i)), &
                    decoded, valid_string)
                if (.not. valid_string) then
                    message = 'Gremlin dir must be a string'
                    return
                end if
                project_dir = trim(decoded)
            end if
        end do
        if (action_count /= 1 .or. dir_count > 1) then
            message = 'Gremlin arguments need one action and at most one dir'
            return
        end if
        call map_gremlin_action(trim(public_action), core_action, valid_string)
        if (.not. valid_string) then
            message = 'unknown Gremlin MCP action: '//trim(public_action)
            return
        end if

        request_json = '{'
        action_count = 0
        do i = 1, n_properties
            if (json_key_matches(keys(i), key_length(i), 'action')) cycle
            if (json_key_matches(keys(i), key_length(i), 'dir')) cycle
            if (action_count > 0) request_json = request_json//','
            ! Keep the parsed JSON token: re-emitting the decoded key would
            ! change escaping or trim significant trailing spaces.
            request_json = request_json// &
                arguments(key_first(i):key_last(i))//':'// &
                trim(arguments(value_first(i):value_last(i)))
            action_count = action_count + 1
        end do
        request_json = request_json//'}'
        ierr = 0
    end subroutine normalize_gremlin_arguments

    subroutine map_gremlin_action(public_action, core_action, valid)
        character(len=*), intent(in) :: public_action
        character(len=*), intent(out) :: core_action
        logical, intent(out) :: valid

        valid = .true.
        select case (trim(public_action))
        case ('gremlin_start')
            core_action = 'start'
        case ('gremlin_status')
            core_action = 'status'
        case ('gremlin_wait')
            core_action = 'wait'
        case ('gremlin_events')
            core_action = 'events'
        case ('gremlin_failures')
            core_action = 'failures'
        case ('gremlin_reproduce')
            core_action = 'reproduce'
        case ('gremlin_stop')
            core_action = 'stop'
        case default
            core_action = ''
            valid = .false.
        end select
    end subroutine map_gremlin_action

    subroutine skip_json_space(text, position)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position

        do while (position <= len_trim(text))
            select case (text(position:position))
            case (' ', achar(9), achar(10), achar(13))
                position = position + 1
            case default
                return
            end select
        end do
    end subroutine skip_json_space

    subroutine read_json_string(text, position, value, ierr, value_length)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        character(len=*), intent(out) :: value
        integer, intent(out) :: ierr
        integer, intent(out), optional :: value_length

        integer :: i, n, code, digit, j
        character(len=1) :: ch

        value = ''
        ierr = 1
        if (present(value_length)) value_length = 0
        if (position > len_trim(text)) return
        if (text(position:position) /= '"') return
        n = 0
        i = position + 1
        do while (i <= len_trim(text))
            ch = text(i:i)
            if (ch == '"') then
                position = i + 1
                ierr = 0
                if (present(value_length)) value_length = n
                return
            end if
            if (ch == achar(92)) then
                i = i + 1
                if (i > len_trim(text)) return
                ch = text(i:i)
                select case (ch)
                case ('"', achar(92), '/')
                    call append_json_char(value, n, ch)
                case ('b')
                    call append_json_char(value, n, achar(8))
                case ('f')
                    call append_json_char(value, n, achar(12))
                case ('n')
                    call append_json_char(value, n, achar(10))
                case ('r')
                    call append_json_char(value, n, achar(13))
                case ('t')
                    call append_json_char(value, n, achar(9))
                case ('u')
                    if (i + 4 > len_trim(text)) return
                    code = 0
                    do j = i + 1, i + 4
                        digit = json_hex_digit(text(j:j))
                        if (digit < 0) return
                        code = code*16 + digit
                    end do
                    call append_utf8(value, n, code)
                    i = i + 4
                case default
                    return
                end select
            else
                call append_json_char(value, n, ch)
            end if
            i = i + 1
        end do
    end subroutine read_json_string

    logical function json_key_matches(key, key_length, expected)
        character(len=*), intent(in) :: key, expected
        integer, intent(in) :: key_length

        json_key_matches = .false.
        if (key_length /= len(expected)) return
        json_key_matches = key(1:len(expected)) == expected
    end function json_key_matches

    subroutine scan_json_value(text, position, last, ierr)
        character(len=*), intent(in) :: text
        integer, intent(in) :: position
        integer, intent(out) :: last
        integer, intent(out) :: ierr

        integer :: first, i, object_depth, array_depth
        logical :: in_string, escaped, container
        character(len=1) :: ch

        last = 0
        ierr = 1
        first = position
        if (first > len_trim(text)) return
        object_depth = 0
        array_depth = 0
        in_string = .false.
        escaped = .false.
        container = text(first:first) == '{' .or. text(first:first) == '['
        last = 0
        do i = first, len_trim(text)
            ch = text(i:i)
            if (in_string) then
                if (escaped) then
                    escaped = .false.
                else if (ch == achar(92)) then
                    escaped = .true.
                else if (ch == '"') then
                    in_string = .false.
                end if
            else if (ch == '"') then
                in_string = .true.
            else if (ch == '{') then
                object_depth = object_depth + 1
            else if (ch == '[') then
                array_depth = array_depth + 1
            else if (ch == '}') then
                if (object_depth > 0) then
                    object_depth = object_depth - 1
                    if (container .and. object_depth == 0 .and. array_depth == 0) then
                        last = i
                        exit
                    end if
                else if (array_depth == 0) then
                    last = i - 1
                    exit
                end if
            else if (ch == ']') then
                if (array_depth > 0) then
                    array_depth = array_depth - 1
                    if (container .and. object_depth == 0 .and. array_depth == 0) then
                        last = i
                        exit
                    end if
                else
                    return
                end if
            else if (ch == ',' .and. object_depth == 0 .and. array_depth == 0) then
                last = i - 1
                exit
            end if
        end do
        if (last == 0) then
            if (object_depth /= 0 .or. array_depth /= 0 .or. in_string) return
            last = len_trim(text)
        end if
        if (last < first) return
        ierr = 0
    end subroutine scan_json_value

    subroutine decode_json_string(raw, value, valid, value_length)
        character(len=*), intent(in) :: raw
        character(len=*), intent(out) :: value
        logical, intent(out) :: valid
        integer, intent(out), optional :: value_length

        integer :: position, ierr, decoded_length

        position = 1
        call read_json_string(raw, position, value, ierr, decoded_length)
        if (present(value_length)) value_length = decoded_length
        valid = ierr == 0
        if (.not. valid) return
        call skip_json_space(raw, position)
        if (position <= len_trim(raw)) valid = .false.
    end subroutine decode_json_string

    subroutine append_json_char(value, n, ch)
        character(len=*), intent(inout) :: value
        integer, intent(inout) :: n
        character(len=1), intent(in) :: ch

        if (n >= len(value)) return
        n = n + 1
        value(n:n) = ch
    end subroutine append_json_char

    subroutine append_utf8(value, n, code)
        character(len=*), intent(inout) :: value
        integer, intent(inout) :: n
        integer, intent(in) :: code

        if (code < 128) then
            call append_json_char(value, n, achar(code))
        else if (code < 2048) then
            call append_json_char(value, n, achar(192 + code/64))
            call append_json_char(value, n, achar(128 + mod(code, 64)))
        else if (code < 55296 .or. code > 57343) then
            call append_json_char(value, n, achar(224 + code/4096))
            call append_json_char(value, n, achar(128 + mod(code/64, 64)))
            call append_json_char(value, n, achar(128 + mod(code, 64)))
        else
            call append_json_char(value, n, '?')
        end if
    end subroutine append_utf8

    integer function json_hex_digit(ch)
        character(len=1), intent(in) :: ch

        select case (ch)
        case ('0':'9')
            json_hex_digit = iachar(ch) - iachar('0')
        case ('a':'f')
            json_hex_digit = iachar(ch) - iachar('a') + 10
        case ('A':'F')
            json_hex_digit = iachar(ch) - iachar('A') + 10
        case default
            json_hex_digit = -1
        end select
    end function json_hex_digit

    subroutine handle_check(line, id_str, dir, check_res, output_text, &
            exitcode, response)
        character(len=*), intent(in) :: line, id_str, dir
        type(check_result_t), intent(out) :: check_res
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=:), allocatable, intent(out) :: response

        logical :: want_full
        type(capabilities_t) :: cap
        character(len=2048) :: cap_json
        character(len=514) :: dir_prefix
        want_full = (index(line, '"full"') > 0 .or. &
            index(line, '"json":"full"') > 0)
        cap_json = ''
        if (want_full) then
            call detect_capabilities(cap)
            call capabilities_json(cap, cap_json)
        end if
        call progress_suppress(.true.)
        call fo_check_run(trim(dir), check_res)
        call progress_suppress(.false.)
        if (want_full) then
            output_text = check_result_full_json(check_res, cap_json)
        else
            output_text = check_result_compact_json(check_res)
        end if
        dir_prefix = trim(dir)//'/'
        call strip_path_prefix_in_str(output_text, trim(dir_prefix))
        exitcode = 0
        if (.not. (check_res%build_ok .and. check_res%tests_ok)) exitcode = 1
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_check

    subroutine handle_lint(line, id_str, dir, output_text, exitcode, response)
        use fo_lint, only: lint_finding_t, lint_warning_t, lint_dir, &
            lint_compiler, lint_dedup_warnings, lint_all_json, &
            lint_fix_dir, MAX_FINDINGS, MAX_WARNINGS
        character(len=*), intent(in) :: line, id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=:), allocatable, intent(out) :: response

        type(lint_finding_t) :: findings(MAX_FINDINGS)
        type(lint_warning_t), allocatable :: warnings(:)
        integer :: n_findings, n_warnings, n_removed, n_remaining
        character(len=16384) :: lint_output
        character(len=514) :: dir_prefix
        character(len=16) :: fix_flag

        call extract_json_field(line, '"fix"', fix_flag)
        if (trim(fix_flag) == 'true') then
            call lint_fix_dir(trim(dir), n_removed, n_remaining)
            write (lint_output, '(a,i0,a,i0,a)') &
                '{"removed":', n_removed, ',"remaining":', n_remaining, '}'
            output_text = trim(lint_output)
            exitcode = 0
            if (n_remaining > 0) exitcode = 1
            call make_tool_text_response(id_str, lint_output, exitcode, response)
            return
        end if

        allocate (warnings(MAX_WARNINGS))
        call lint_dir(trim(dir), findings, n_findings)
        call lint_compiler(trim(dir), warnings, n_warnings)
        call lint_dedup_warnings(warnings, n_warnings)
        lint_output = lint_all_json(findings, n_findings, warnings, n_warnings)
        dir_prefix = trim(dir)//'/'
        call strip_path_prefix_in_str(lint_output, trim(dir_prefix))
        output_text = trim(lint_output)
        exitcode = 0
        if (n_findings > 0 .or. n_warnings > 0) exitcode = 1
        call make_tool_text_response(id_str, lint_output, exitcode, response)
    end subroutine handle_lint

    subroutine handle_backend_build(id_str, dir, tmpfile, response)
        use fo_build_backend, only: backend_t, detect_backend, backend_build, &
            BACKEND_NONE
        character(len=*), intent(in) :: id_str, dir, tmpfile
        character(len=:), allocatable, intent(out) :: response

        type(backend_t) :: b
        character(len=8192) :: output_text
        integer :: exitcode

        b = detect_backend(trim(dir))
        if (b%kind == BACKEND_NONE) then
            output_text = 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
        else
            call backend_build(b, exitcode, log_file=tmpfile)
            call read_text_file(tmpfile, output_text)
        end if
        call delete_tmpfile(tmpfile)
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_backend_build

    subroutine handle_backend_test_named(line, id_str, dir, tmpfile, response)
        use fo_build_backend, only: backend_t, detect_backend, backend_test, &
            backend_test_names, BACKEND_NONE
        use fx_dag, only: MAX_NODES
        use fo_test_results, only: test_result_entry_t, parse_test_results, &
            format_test_results_human, format_test_results_json
        character(len=*), intent(in) :: line, id_str, dir, tmpfile
        character(len=:), allocatable, intent(out) :: response

        type(backend_t) :: b
        character(len=:), allocatable :: output_text
        character(len=16384) :: human_output
        character(len=16) :: json_mode
        integer :: exitcode
        character(len=128) :: test_names(MAX_NODES)
        integer :: n_names
        type(test_result_entry_t), allocatable :: entries(:)
        integer :: n_entries, ierr

        b = detect_backend(trim(dir))
        if (b%kind == BACKEND_NONE) then
            output_text = 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            call make_tool_text_response(id_str, output_text, exitcode, response)
            return
        end if

        n_names = 0
        call extract_test_names_from_params(line, test_names, n_names)

        if (n_names > 0) then
            call backend_test_names(b, test_names, n_names, exitcode, &
                log_file=tmpfile)
        else
            call backend_test(b, exitcode, log_file=tmpfile)
        end if

        call parse_test_results(tmpfile, entries, n_entries, ierr)
        call extract_json_field(line, '"json"', json_mode)
        if (ierr == 0 .and. &
            (json_mode == 'compact' .or. json_mode == 'full')) then
            call format_test_results_json(entries, n_entries, exitcode, output_text)
        else if (ierr == 0 .and. n_entries > 0) then
            call format_test_results_human(entries, n_entries, tmpfile, &
                n_names == 0, human_output)
            output_text = trim(human_output)
        else
            call read_text_file(tmpfile, human_output)
            output_text = trim(human_output)
        end if
        call delete_tmpfile(tmpfile)
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_backend_test_named

    subroutine extract_test_names_from_params(line, names, n_names)
        character(len=*), intent(in) :: line
        character(len=128), intent(out) :: names(:)
        integer, intent(out) :: n_names

        integer :: p, q, len_line

        n_names = 0
        len_line = len(line)
        p = index(line, '"args"')
        if (p == 0) return
        q = index(line(p:len_line), '[')
        if (q == 0) return
        p = p + q - 1

        do while (p <= len_line)
            q = index(line(p:len_line), '"')
            if (q == 0) exit
            p = p + q
            q = index(line(p:len_line), '"')
            if (q == 0) exit
            if (n_names < size(names)) then
                n_names = n_names + 1
                names(n_names) = line(p:p + q - 2)
            end if
            p = p + q
        end do
    end subroutine extract_test_names_from_params

    subroutine handle_graph(line, id_str, dir, output_text, exitcode, response)
        use fo_scan, only: scan_unit_t, scan_dir
        use fx_dag, only: dag_t, dag_to_dot
        use fo_dag_bridge, only: build_dag_from_units
        use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
        character(len=*), intent(in) :: line, id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=:), allocatable, intent(out) :: response

        type(backend_t) :: b
        type(scan_unit_t), allocatable :: units(:)
        type(dag_t) :: dag
        character(len=512) :: scan_root
        integer :: n_units, ierr, i, j
        character(len=:), allocatable :: dot_out

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        call scan_dir(trim(scan_root), units, n_units, ierr)
        if (ierr /= 0) then
            output_text = 'fo: scan failed'
            exitcode = 1
        else
            call build_dag_from_units(units, n_units, dag)
            exitcode = 0
            if (index(line, '"dot"') > 0) then
                call dag_to_dot(dag, dot_out)
                output_text = trim(dot_out)
            else
                output_text = ''
                do i = 1, dag%n_nodes
                    if (dag%nodes(i)%n_edges == 0) then
                        output_text = trim(output_text)// &
                            trim(dag%nodes(i)%label)//achar(10)
                    else
                        do j = 1, dag%nodes(i)%n_edges
                            output_text = trim(output_text)// &
                                trim(dag%nodes(i)%label)//' -> '// &
                                trim(dag%nodes(dag%nodes(i)%edges(j))%label)//achar(10)
                        end do
                    end if
                end do
            end if
        end if
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_graph

    subroutine handle_info(id_str, dir, output_text, exitcode, response)
        use fo_scan, only: scan_unit_t, scan_dir
        use fx_dag, only: dag_t
        use fo_dag_bridge, only: build_dag_from_units
        use fo_build_backend, only: backend_t, detect_backend, &
            BACKEND_NONE, BACKEND_NATIVE, BACKEND_CMAKE
        use fo_cache, only: cache_schema, cache_store_root
        character(len=*), intent(in) :: id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=:), allocatable, intent(out) :: response

        type(backend_t) :: b
        type(scan_unit_t), allocatable :: units(:)
        type(dag_t) :: dag
        character(len=512) :: scan_root, cache_text
        integer :: n_units, ierr

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        select case (b%kind)
        case (BACKEND_NATIVE)
            output_text = 'backend: native'//achar(10)
        case (BACKEND_CMAKE)
            output_text = 'backend: cmake'//achar(10)
        case default
            output_text = 'backend: none'//achar(10)
        end select
        call cache_schema(cache_text)
        output_text = trim(output_text)//'cache-schema: '// &
            trim(cache_text)//achar(10)
        call cache_store_root(cache_text)
        output_text = trim(output_text)//'cache-root: '// &
            trim(cache_text)//achar(10)//'cache-shards: 256'//achar(10)
        call scan_dir(trim(scan_root), units, n_units, ierr)
        if (ierr == 0) then
            call build_dag_from_units(units, n_units, dag)
            write (output_text(len_trim(output_text) + 1:), '(a,i0)') &
                'files: ', n_units
            output_text = trim(output_text)//achar(10)
            write (output_text(len_trim(output_text) + 1:), '(a,i0)') &
                'modules: ', dag%n_nodes
        end if
        exitcode = 0
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_info

    subroutine handle_changed(id_str, dir, output_text, exitcode, response)
        use fo_check, only: fo_changed_modules
        use fx_dag, only: dag_t, MAX_NODES
        use fo_scan, only: MAX_PATH
        character(len=*), intent(in) :: id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=:), allocatable, intent(out) :: response

        type(dag_t) :: dag
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, i, n_tests
        character(len=MAX_PATH) :: filenames(MAX_NODES)
        logical :: is_test_arr(MAX_NODES)
        call fo_changed_modules(trim(dir), dag, changed_ids, n_changed, &
            affected_ids, n_affected, n_cached, ierr, &
            filenames=filenames, is_test_arr=is_test_arr)
        if (ierr /= 0) then
            output_text = 'fo: scan or dag failed'
            exitcode = 1
            call make_tool_text_response(id_str, output_text, exitcode, response)
            return
        end if
        exitcode = 0
        if (n_changed == 0) then
            write (output_text, '(a,i0,a)') 'all ', n_cached, ' modules cached'
            call make_tool_text_response(id_str, output_text, exitcode, response)
            return
        end if
        write (output_text, '(a,i0,a)') 'changed (', n_changed, '):'
        do i = 1, n_changed
            output_text = trim(output_text)//achar(10)//'  '// &
                trim(dag%nodes(changed_ids(i))%label)//'  '// &
                trim(filenames(changed_ids(i)))
        end do
        output_text = trim(output_text)//achar(10)
        write (output_text(len_trim(output_text) + 1:), '(a,i0,a)') &
            'affected (', n_affected, '):'
        do i = 1, n_affected
            output_text = trim(output_text)//achar(10)//'  '// &
                trim(dag%nodes(affected_ids(i))%label)//'  '// &
                trim(filenames(affected_ids(i)))
        end do
        n_tests = 0
        do i = 1, n_affected
            if (is_test_arr(affected_ids(i))) n_tests = n_tests + 1
        end do
        if (n_tests > 0) then
            output_text = trim(output_text)//achar(10)
            write (output_text(len_trim(output_text) + 1:), '(a,i0,a)') &
                'affected tests (', n_tests, '):'
            do i = 1, n_affected
                if (is_test_arr(affected_ids(i))) then
                    output_text = trim(output_text)//achar(10)//'  '// &
                        trim(dag%nodes(affected_ids(i))%label)//'  '// &
                        trim(filenames(affected_ids(i)))
                end if
            end do
        end if
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_changed

    subroutine handle_clean(line, id_str, dir, output_text, exitcode, response)
        use fo_cache, only: cache_store_root
        use fo_build_backend, only: backend_clean
        character(len=*), intent(in) :: line, id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=:), allocatable, intent(out) :: response

        character(len=512) :: store_root
        type(backend_t) :: b
        logical :: purge_store, build_removed, store_removed

        purge_store = index(line, '"cache":true') > 0
        call cache_store_root(store_root)
        b = detect_backend(trim(dir))
        call backend_clean(trim(b%project_dir), purge_store, build_removed, &
            store_removed)
        output_text = ''
        if (build_removed) output_text = 'build tree cleared: '// &
            trim(b%project_dir)//'/build'
        if (store_removed) then
            if (len_trim(output_text) > 0) output_text = trim(output_text)//achar(10)
            output_text = trim(output_text)//'cache cleared: '//trim(store_root)
        else
            if (len_trim(output_text) > 0) output_text = trim(output_text)//achar(10)
            output_text = trim(output_text)//'cache kept (shared store)'
        end if
        exitcode = 0
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_clean

    subroutine handle_install(line, id_str, dir, output_text, exitcode, response)
        use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
        character(len=*), intent(in) :: line, id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=:), allocatable, intent(out) :: response

        type(backend_t) :: b
        character(len=256) :: prefix, requested_prefix
        character(len=512) :: home, install_log
        character(len=:), allocatable :: packed
        integer :: status, n_args

        call get_environment_variable('HOME', home, status=status)
        if (status /= 0 .or. len_trim(home) == 0) home = '/usr/local'
        prefix = trim(home)//'/.local'
        call extract_json_field(line, '"prefix"', requested_prefix)
        if (len_trim(requested_prefix) > 0) prefix = trim(requested_prefix)

        b = detect_backend(trim(dir))
        if (b%kind == BACKEND_NONE) then
            output_text = 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            call make_tool_text_response(id_str, output_text, exitcode, response)
            return
        end if

        call make_tmpfile('fo-install', install_log)
        n_args = 0
        call argv_push(packed, n_args, 'fpm')
        call argv_push(packed, n_args, 'install')
        call argv_push(packed, n_args, '--profile')
        call argv_push(packed, n_args, 'release')
        call argv_push(packed, n_args, '--prefix')
        call argv_push(packed, n_args, trim(prefix))
        call process_run_argv_logged('.', packed, n_args, install_log, &
            .false., 0, exitcode)
        if (exitcode /= 0) then
            call read_text_file(install_log, output_text)
        else
            output_text = 'installed: '//trim(prefix)//'/bin/'
        end if
        call delete_tmpfile(install_log)
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_install

    subroutine handle_resources_read(params_json, id_str, response, async_state)
        character(len=*), intent(in) :: params_json, id_str
        character(len=:), allocatable, intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=256) :: uri
        character(len=8192) :: output_text
        integer :: exitcode, property_count, parse_status
        type(check_result_t) :: check_res

        call extract_json_string_member(params_json, 'uri', uri, &
            property_count, parse_status)
        call async_poll(async_state)

        if (parse_status == 0 .and. property_count == 1 .and. &
            trim(uri) == 'fo://diagnostics') then
            if (len_trim(async_state%last_output) > 0) then
                call read_text_file(async_state%last_output, output_text)
                exitcode = async_state%last_exitcode
            else
                call fo_check_run('.', check_res)
                output_text = check_result_compact_json(check_res)
                exitcode = 0
                if (.not. (check_res%build_ok .and. check_res%tests_ok)) exitcode = 1
            end if

            output_text = json_escape_string(output_text)
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
        character(len=:), allocatable, intent(out) :: response
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
        character(len=:), allocatable, intent(out) :: response
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
        character(len=:), allocatable, intent(out) :: response
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
        character(len=:), allocatable, intent(out) :: response
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
        if (exitcode /= 0) then
            call jsonrpc_error(id_str, -32603, &
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

    subroutine make_initialize_response(id_str, params_json, response)
        character(len=*), intent(in) :: id_str, params_json
        character(len=:), allocatable, intent(out) :: response

        character(len=32) :: proto_ver
        integer :: property_count, parse_status

        call extract_json_string_member(params_json, 'protocolVersion', proto_ver, &
            property_count, parse_status)
        if (parse_status /= 0 .or. property_count /= 1 .or. &
            len_trim(proto_ver) == 0) proto_ver = '2025-03-26'
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
            '"result":{"protocolVersion":"'//trim(proto_ver)//'",'// &
            '"capabilities":{"tools":{"listChanged":false},'// &
            '"resources":{"listChanged":false}},'// &
            '"serverInfo":{"name":"fo","version":"'//FO_VERSION//'"}}}'
    end subroutine make_initialize_response

    subroutine make_tools_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=:), allocatable, intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
            '"result":{"tools":[{"name":"fo",'// &
            '"description":"Fortran build driver",'// &
            '"inputSchema":{"type":"object","properties":{'// &
            '"action":{"type":"string",'// &
            '"enum":["check","status","diagnostics","cancel",'// &
            '"build","test","graph","info","changed","clean",'// &
            '"lint","fmt","install","gremlin_start",'// &
            '"gremlin_status","gremlin_wait","gremlin_events",'// &
            '"gremlin_failures","gremlin_reproduce","gremlin_stop"],'// &
            '"description":"Action to run"},'// &
            '"dir":{"type":"string",'// &
            '"description":"Project directory (default: cwd)"},'// &
            '"json":{"type":"string","enum":["compact","full"],'// &
            '"description":"check/test: structured JSON result"},'// &
            '"cache":{"type":"boolean",'// &
            '"description":"clean: also purge the shared content store"},'// &
            '"prefix":{"type":"string",'// &
            '"description":"install: destination prefix"},'// &
            '"fix":{"type":"boolean",'// &
            '"description":"lint: remove unused imports in place"},'// &
            '"lane_id":{"type":"string"},'// &
            '"session_id":{"type":"string"},'// &
            '"targets":{"type":"array","items":{"type":"string"}},'// &
            '"only_changed":{"type":"boolean"},'// &
            '"shuffle":{"type":"boolean"},'// &
            '"random_count":{"type":"integer","minimum":1,"maximum":32},'// &
            '"seed":{"type":"integer"},'// &
            '"campaign_seconds":{"type":"integer","minimum":1,"maximum":60},'// &
            '"timeout_seconds":{"type":"integer","minimum":1,"maximum":5},'// &
            '"jobs":{"type":"integer","const":1},'// &
            '"cursor":{"type":"integer","minimum":0},'// &
            '"max_records":{"type":"integer","minimum":1,"maximum":128},'// &
            '"max_bytes":{"type":"integer","minimum":1,"maximum":262144},'// &
            '"wait_ms":{"type":"integer","minimum":0,"maximum":30000},'// &
            '"fail_on_failure":{"type":"boolean"},'// &
            '"case_id":{"type":"string"},'// &
            '"generation_id":{"type":"string"}},'// &
            '"required":["action"]}}]}}'
    end subroutine make_tools_list_response

    subroutine make_resources_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=:), allocatable, intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
            '"result":{"resources":[{"uri":"fo://diagnostics",'// &
            '"name":"diagnostics",'// &
            '"description":"Current fo check diagnostics",'// &
            '"mimeType":"text/plain"}]}}'
    end subroutine make_resources_list_response

    subroutine make_run_start_response(id_str, run_id, pending, response)
        character(len=*), intent(in) :: id_str
        integer, intent(in) :: run_id
        logical, intent(in) :: pending
        character(len=:), allocatable, intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
            '"result":{"run_id":'//trim(json_int(run_id))// &
            ',"state":"'
        if (pending) then
            response = trim(response)//'rerun-pending"'
        else
            response = trim(response)//'running"'
        end if
        response = trim(response)//',"pending":'//trim(json_bool(pending))//'}}'
    end subroutine make_run_start_response

    subroutine jsonrpc_error(id_str, code, message, response)
        character(len=*), intent(in) :: id_str, message
        integer, intent(in) :: code
        character(len=:), allocatable, intent(out) :: response
        character(len=len_trim(id_str) + len_trim(message) + 128) :: buffer

        call jsonrpc_error_fixed(id_str, code, message, buffer)
        response = trim(buffer)
    end subroutine jsonrpc_error

    subroutine jsonrpc_null(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=:), allocatable, intent(out) :: response
        character(len=len_trim(id_str) + 64) :: buffer

        call jsonrpc_null_fixed(id_str, buffer)
        response = trim(buffer)
    end subroutine jsonrpc_null

    subroutine make_tool_text_response(id_str, output_text, exitcode, response)
        character(len=*), intent(in) :: id_str, output_text
        integer, intent(in) :: exitcode
        character(len=:), allocatable, intent(out) :: response

        character(len=:), allocatable :: escaped

        escaped = json_escape_string(output_text)
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
            '"result":{"content":[{"type":"text",'// &
            '"text":"'//trim(escaped)//'"}],"isError":'// &
            trim(json_bool(exitcode /= 0))//'}}'
    end subroutine make_tool_text_response

end module fo_mcp
