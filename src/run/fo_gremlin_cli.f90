module fo_gremlin_cli
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_gremlin_supervisor, only: gremlin_handle
    use fo_process, only: process_exit
    use fx_json_build, only: json_escape_string
    implicit none
    private
    public :: gremlin_cli_run

    integer, parameter :: ARG_LEN = 4096, KEY_LEN = 128

contains

    subroutine gremlin_cli_run()
        character(len=ARG_LEN), allocatable :: tokens(:)
        character(len=64) :: action
        character(len=ARG_LEN) :: project_dir
        character(len=:), allocatable :: request_json, response_json
        integer :: n_tokens, i, exitcode
        logical :: show_help

        n_tokens = max(0, command_argument_count() - 1)
        allocate (tokens(max(1, n_tokens)))
        tokens = ''
        do i = 1, n_tokens
            call get_command_argument(i + 1, tokens(i))
        end do

        call parse_cli(tokens, n_tokens, action, project_dir, request_json, &
            show_help)
        if (show_help) then
            call print_gremlin_usage(output_unit)
            return
        end if

        call gremlin_handle(trim(action), trim(project_dir), trim(request_json), &
            response_json, exitcode)
        if (len_trim(response_json) == 0) response_json = '{}'
        write (output_unit, '(a)') response_json
        if (exitcode /= 0) call process_exit(exitcode)
    end subroutine gremlin_cli_run

    subroutine parse_cli(tokens, n_tokens, action, project_dir, request_json, &
            show_help)
        character(len=*), intent(in) :: tokens(:)
        integer, intent(in) :: n_tokens
        character(len=*), intent(out) :: action, project_dir
        character(len=:), allocatable, intent(out) :: request_json
        logical, intent(out) :: show_help

        character(len=KEY_LEN), allocatable :: keys(:)
        character(len=ARG_LEN), allocatable :: values(:), targets(:)
        character(len=ARG_LEN) :: arg, name, value, raw
        character(len=:), allocatable :: escaped
        integer :: first, i, n_fields, n_targets, eq, j
        logical :: recognized, needs_value, has_equal, missing_value
        character(len=8) :: kind

        allocate (keys(max(1, n_tokens)))
        allocate (values(max(1, n_tokens)))
        allocate (targets(max(1, n_tokens)))
        keys = ''
        values = ''
        targets = ''
        n_fields = 0
        n_targets = 0
        action = 'start'
        project_dir = '.'
        first = 1
        show_help = .false.

        if (n_tokens > 0) then
            if (len_trim(tokens(1)) > 0) then
                if (tokens(1)(1:1) /= '-') then
                    action = tokens(1)
                    first = 2
                end if
            end if
        end if
        if (trim(action) == 'help') then
            show_help = .true.
            request_json = '{}'
            return
        end if

        i = first
        do while (i <= n_tokens)
            arg = trim(tokens(i))
            if (arg == '--help' .or. arg == '-h') then
                show_help = .true.
                request_json = '{}'
                return
            end if
            if (arg == '--json') then
                i = i + 1
                cycle
            end if

            if (len_trim(arg) == 0) then
                i = i + 1
                cycle
            end if
            if (arg(1:1) /= '-') then
                if (trim(action) == 'reproduce' .and. &
                    .not. field_present(keys, n_fields, 'case_id')) then
                    call add_string_field('case_id', trim(arg), keys, values, &
                        n_fields)
                else
                    call add_string_field('argument', trim(arg), keys, values, &
                        n_fields)
                end if
                i = i + 1
                cycle
            end if

            eq = index(trim(arg), '=')
            has_equal = eq > 0
            if (has_equal) then
                name = arg(1:eq - 1)
                value = arg(eq + 1:)
            else
                name = arg
                value = ''
            end if
            call option_spec(trim(name), raw, kind, needs_value, &
                recognized)

            if (trim(name) == '--dir') then
                if (has_equal) then
                    project_dir = value
                else if (i < n_tokens) then
                    if (.not. looks_like_option(tokens(i + 1))) then
                        project_dir = trim(tokens(i + 1))
                        i = i + 1
                    else
                        project_dir = ''
                    end if
                else
                    project_dir = ''
                end if
                i = i + 1
                cycle
            end if

            if (recognized .and. trim(raw) == 'targets') then
                if (.not. has_equal .and. i < n_tokens) then
                    if (.not. looks_like_option(tokens(i + 1))) then
                        value = trim(tokens(i + 1))
                        i = i + 1
                    else
                        value = ''
                    end if
                else
                    value = ''
                end if
                n_targets = n_targets + 1
                targets(n_targets) = value
                i = i + 1
                cycle
            end if

            if (.not. recognized) then
                call unknown_option_name(trim(name), raw)
                if (has_equal) then
                    value = json_string(trim(value))
                    call append_field(trim(raw), value, keys, values, n_fields)
                else if (i < n_tokens) then
                    if (.not. looks_like_option(tokens(i + 1))) then
                        value = json_string(trim(tokens(i + 1)))
                        call append_field(trim(raw), value, keys, values, n_fields)
                        i = i + 1
                    else
                        call append_field(trim(raw), 'true', keys, values, n_fields)
                    end if
                else
                    call append_field(trim(raw), 'true', keys, values, n_fields)
                end if
                i = i + 1
                cycle
            end if

            missing_value = .false.
            if (needs_value) then
                if (.not. has_equal) then
                    if (i < n_tokens) then
                        if (.not. looks_like_option(tokens(i + 1)) .or. &
                            is_integer_literal(tokens(i + 1))) then
                            value = trim(tokens(i + 1))
                            i = i + 1
                        else
                            missing_value = .true.
                        end if
                    else
                        missing_value = .true.
                    end if
                end if
            else if (.not. has_equal) then
                value = 'true'
            end if

            if (trim(raw) == 'lane_id' .or. trim(raw) == 'session_id' .or. &
                trim(raw) == 'case_id' .or. trim(raw) == 'generation_id' .or. &
                trim(raw) == 'wait_until') then
                if (missing_value) value = ''
                call add_string_field(trim(raw), trim(value), keys, values, n_fields)
            else if (trim(kind) == 'bool') then
                if (missing_value) then
                    call append_field(trim(raw), '""', keys, values, n_fields)
                else
                    call append_field(trim(raw), boolean_or_string(trim(value)), &
                        keys, values, n_fields)
                end if
            else
                if (missing_value) value = ''
                if (trim(kind) == 'int' .and. is_integer_literal(trim(value))) then
                    call append_field(trim(raw), trim(value), keys, values, n_fields)
                else
                    escaped = json_string(trim(value))
                    call append_field(trim(raw), escaped, keys, values, n_fields)
                end if
            end if
            i = i + 1
        end do

        if (n_targets > 0) then
            raw = '['
            do j = 1, n_targets
                if (j > 1) raw = trim(raw)//','
                escaped = json_string(trim(targets(j)))
                raw = trim(raw)//trim(escaped)
            end do
            raw = trim(raw)//']'
            call append_field('targets', trim(raw), keys, values, n_fields)
        end if

        if ((trim(action) == 'status' .or. trim(action) == 'wait') .and. &
            .not. field_present(keys, n_fields, 'detail')) then
            call add_string_field('detail', 'summary', keys, values, n_fields)
        end if

        request_json = '{'
        do j = 1, n_fields
            if (j > 1) request_json = request_json//','
            request_json = request_json//'"'//trim(keys(j))//'":'//trim(values(j))
        end do
        request_json = request_json//'}'
    end subroutine parse_cli

    subroutine option_spec(option, key, kind, needs_value, recognized)
        character(len=*), intent(in) :: option
        character(len=*), intent(out) :: key, kind
        logical, intent(out) :: needs_value, recognized

        key = ''
        kind = 'int'
        needs_value = .false.
        recognized = .true.
        select case (trim(option))
        case ('--lane', '--lane-id')
            key = 'lane_id'
            kind = 'string'
            needs_value = .true.
        case ('--session', '--session-id')
            key = 'session_id'
            kind = 'string'
            needs_value = .true.
        case ('--target')
            key = 'targets'
            kind = 'string'
            needs_value = .true.
        case ('--only-changed')
            key = 'only_changed'
            kind = 'bool'
        case ('--shuffle')
            key = 'shuffle'
            kind = 'bool'
        case ('--random', '--random-count')
            key = 'random_count'
            needs_value = .true.
        case ('--seed')
            key = 'seed'
            needs_value = .true.
        case ('--campaign-seconds')
            key = 'campaign_seconds'
            needs_value = .true.
        case ('--timeout-seconds')
            key = 'timeout_seconds'
            needs_value = .true.
        case ('--jobs')
            key = 'jobs'
            needs_value = .true.
        case ('--cursor')
            key = 'cursor'
            needs_value = .true.
        case ('--lifecycle-cursor')
            key = 'lifecycle_cursor'
            needs_value = .true.
        case ('--max-records')
            key = 'max_records'
            needs_value = .true.
        case ('--max-bytes')
            key = 'max_bytes'
            needs_value = .true.
        case ('--wait-ms', '--timeout-ms')
            key = 'wait_ms'
            needs_value = .true.
        case ('--detail')
            key = 'detail'
            kind = 'string'
            needs_value = .true.
        case ('--failure', '--fail-on-failure')
            key = 'fail_on_failure'
            kind = 'bool'
        case ('--until')
            key = 'wait_until'
            kind = 'string'
            needs_value = .true.
        case ('--case', '--case-id')
            key = 'case_id'
            kind = 'string'
            needs_value = .true.
        case ('--generation', '--generation-id')
            key = 'generation_id'
            kind = 'string'
            needs_value = .true.
        case default
            recognized = .false.
            call unknown_option_name(trim(option), key)
            kind = 'unknown'
        end select
    end subroutine option_spec

    subroutine unknown_option_name(option, key)
        character(len=*), intent(in) :: option
        character(len=*), intent(out) :: key
        integer :: i, start, target

        key = ''
        start = 1
        do while (start <= len_trim(option))
            if (option(start:start) /= '-') exit
            start = start + 1
        end do
        if (start > len_trim(option)) then
            key = 'argument'
            return
        end if
        do i = start, len_trim(option)
            target = i - start + 1
            if (target > len(key)) exit
            if (option(i:i) == '-') then
                key(target:target) = '_'
            else if (option(i:i) == '=') then
                exit
            else
                key(target:target) = option(i:i)
            end if
        end do
    end subroutine unknown_option_name

    subroutine add_string_field(key, value, keys, values, n_fields)
        character(len=*), intent(in) :: key, value
        character(len=*), intent(inout) :: keys(:), values(:)
        integer, intent(inout) :: n_fields
        character(len=:), allocatable :: escaped

        escaped = json_string(value)
        call append_field(key, escaped, keys, values, n_fields)
    end subroutine add_string_field

    subroutine append_field(key, value, keys, values, n_fields)
        character(len=*), intent(in) :: key, value
        character(len=*), intent(inout) :: keys(:), values(:)
        integer, intent(inout) :: n_fields

        if (n_fields >= size(keys)) return
        n_fields = n_fields + 1
        keys(n_fields) = key
        values(n_fields) = value
    end subroutine append_field

    function json_string(value) result(encoded)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: encoded

        encoded = '"'//trim(json_escape_string(value))//'"'
    end function json_string

    function boolean_or_string(value) result(encoded)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: encoded

        if (trim(value) == 'true' .or. trim(value) == 'false') then
            encoded = trim(value)
        else
            encoded = json_string(value)
        end if
    end function boolean_or_string

    logical function looks_like_option(value)
        character(len=*), intent(in) :: value

        looks_like_option = .false.
        if (len_trim(value) == 0) return
        if (value(1:1) == '-') then
            looks_like_option = .not. is_integer_literal(trim(value))
        end if
    end function looks_like_option

    logical function is_integer_literal(value)
        character(len=*), intent(in) :: value
        integer :: i, first

        is_integer_literal = .false.
        if (len_trim(value) == 0) return
        first = 1
        if (value(1:1) == '+') return
        if (value(1:1) == '-') first = 2
        if (first > len_trim(value)) return
        do i = first, len_trim(value)
            if (value(i:i) < '0' .or. value(i:i) > '9') return
        end do
        is_integer_literal = .true.
    end function is_integer_literal

    logical function field_present(keys, n_fields, field)
        character(len=*), intent(in) :: keys(:), field
        integer, intent(in) :: n_fields
        integer :: i

        field_present = .false.
        do i = 1, n_fields
            if (trim(keys(i)) == trim(field)) then
                field_present = .true.
                return
            end if
        end do
    end function field_present

    subroutine print_gremlin_usage(unit)
        integer, intent(in) :: unit

        write (unit, '(a)') 'usage: fo gremlin [start|status|wait|events|failures|reproduce|stop] [options]'
        write (unit, '(a)') '  start: --dir DIR --lane ID --target NAME --random N --seed N'
        write (unit, '(a)') '         --random 0 quiesces after required targets pass'
        write (unit, '(a)') '         --only-changed --shuffle --campaign-seconds N --timeout-seconds N'
        write (unit, '(a)') '  reads: --session ID --cursor N --lifecycle-cursor N'
        write (unit, '(a)') '         --max-records N --max-bytes N'
        write (unit, '(a)') '  reads: --detail summary|full (status/wait default: summary)'
        write (unit, '(a)') '  wait: --wait-ms N --failure or --until CONDITION'
        write (unit, '(a)') '  reproduce: ID or --case ID; optional --generation ID'
        write (unit, '(a)') '  output is the shared Gremlin JSON response; --json is accepted'
    end subroutine print_gremlin_usage

end module fo_gremlin_cli
