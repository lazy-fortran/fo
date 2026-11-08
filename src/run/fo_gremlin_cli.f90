module fo_gremlin_cli
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_gremlin_request, only: GREMLIN_OPTIONS, N_GREMLIN_OPTIONS, &
        gremlin_option_index, gremlin_apply_adapter_defaults, gremlin_option_t, &
        gremlin_request_t, gremlin_action_bit, GA_OTHER, GA_WAIT, GA_UNBOUNDED
    use fo_fs, only: fs_canonical_path
    use fo_gremlin_supervisor, only: gremlin_handle
    use fo_json_object, only: json_value_raw
    use fo_process, only: process_exit
    use fo_util, only: read_text_file_alloc
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_array_start, json_array_end, json_key, &
        json_value_string, json_value_bool, json_to_string
    use fx_string, only: string_t
    implicit none
    private
    public :: gremlin_cli_run, print_gremlin_usage

    integer, parameter :: ARG_LEN = 4096

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

        if (n_tokens == 7) then
            ! Internal adapter form of an asynchronous MCP request:
            ! ACTION --request-file REQUEST --response-file RESPONSE --dir DIR.
            if (tokens(2) == '--request-file' .and. tokens(4) == '--response-file' &
                .and. tokens(6) == '--dir') then
                call run_request_file(tokens(1), tokens(3), tokens(5), tokens(7))
                return
            end if
        end if
        call parse_cli(tokens, n_tokens, action, project_dir, request_json, &
            show_help)
        if (show_help) then
            call print_gremlin_usage(output_unit, trim(action))
            return
        end if

        call gremlin_handle(trim(action), fs_canonical_path(trim(project_dir)), &
            trim(request_json), response_json, exitcode)
        if (len_trim(response_json) == 0) response_json = '{}'
        write (output_unit, '(a)') response_json
        if (exitcode /= 0) call process_exit(exitcode)
    end subroutine gremlin_cli_run

    subroutine run_request_file(action, request_file, response_file, project_dir)
        !! Handle one normalized request JSON and write its response JSON.
        character(len=*), intent(in) :: action, request_file, response_file, project_dir
        character(len=:), allocatable :: request_json, response_json
        integer :: exitcode, ierr, unit

        call read_text_file_alloc(trim(request_file), request_json, ierr)
        if (ierr /= 0) call process_exit(2)
        call gremlin_handle(trim(action), trim(project_dir), request_json, &
            response_json, exitcode)
        open (newunit=unit, file=trim(response_file), access='stream', &
            form='unformatted', status='replace', action='write', iostat=ierr)
        if (ierr == 0) write (unit, iostat=ierr) response_json
        if (ierr == 0) close (unit, iostat=ierr)
        if (ierr /= 0) call process_exit(2)
        if (exitcode /= 0) call process_exit(exitcode)
    end subroutine run_request_file

    subroutine parse_cli(tokens, n_tokens, action, project_dir, request_json, &
            show_help)
        !! Flags come from the shared option table; this routine only splits
        !! `--flag value` / `--flag=value` and renders the request object.
        character(len=*), intent(in) :: tokens(:)
        integer, intent(in) :: n_tokens
        character(len=*), intent(out) :: action, project_dir
        character(len=:), allocatable, intent(out) :: request_json
        logical, intent(out) :: show_help

        type(json_builder_t) :: jb
        type(string_t) :: lists(N_GREMLIN_OPTIONS)
        logical :: seen(N_GREMLIN_OPTIONS)
        character(len=ARG_LEN) :: arg, name, value
        integer :: first, i, eq, row, j
        logical :: has_equal, missing_value
        character(len=6) :: kind

        seen = .false.
        action = 'start'
        project_dir = '.'
        first = 1
        show_help = .false.
        request_json = '{}'

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
            return
        end if

        jb = json_new()
        call json_object_start(jb)
        i = first
        do while (i <= n_tokens)
            arg = trim(tokens(i))
            if (arg == '--help' .or. arg == '-h') then
                show_help = .true.
                request_json = '{}'
                if (first == 1) action = 'help'
                return
            end if
            if (len_trim(arg) == 0) then
                i = i + 1
                cycle
            end if
            if (arg(1:1) /= '-') then
                row = 0
                if (trim(action) == 'reproduce') row = gremlin_option_index('case_id')
                if (row > 0) then
                    if (seen(row)) row = 0
                end if
                if (row > 0) then
                    seen(row) = .true.
                    call json_key(jb, 'case_id')
                else
                    call json_key(jb, 'argument')
                end if
                call json_value_string(jb, trim(arg))
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

            if (trim(name) == '--dir') then
                if (has_equal) then
                    project_dir = value
                else if (i < n_tokens .and. .not. looks_like_option(tokens(min(i + 1, n_tokens)))) then
                    project_dir = trim(tokens(i + 1))
                    i = i + 1
                else
                    project_dir = ''
                end if
                i = i + 1
                cycle
            end if

            row = gremlin_option_index(trim(name), flag=.true.)
            if (row == 0) then
                write (error_unit, '(a)') 'fo gremlin: unknown option '//trim(name)
                call process_exit(2)
            end if
            kind = GREMLIN_OPTIONS(row)%kind

            missing_value = .false.
            if (trim(kind) == 'list') then
                if (.not. has_equal .and. i < n_tokens) then
                    if (.not. looks_like_option(tokens(i + 1))) then
                        value = trim(tokens(i + 1))
                        i = i + 1
                    end if
                end if
                if (.not. allocated(lists(row)%s)) then
                    lists(row)%s = ''
                else
                    lists(row)%s = lists(row)%s//char(0)
                end if
                lists(row)%s = lists(row)%s//trim(value)
                i = i + 1
                cycle
            end if
            if (trim(kind) /= 'bool' .and. .not. has_equal) then
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
            else if (trim(kind) == 'bool' .and. .not. has_equal) then
                value = 'true'
            end if

            seen(row) = .true.
            call json_key(jb, trim(GREMLIN_OPTIONS(row)%key))
            if (missing_value) value = ''
            if (trim(kind) == 'bool') then
                if (missing_value) then
                    call json_value_string(jb, '')
                else if (trim(value) == 'true' .or. trim(value) == 'false') then
                    call json_value_bool(jb, trim(value) == 'true')
                else
                    call json_value_string(jb, trim(value))
                end if
            else if (trim(kind) == 'int' .and. is_integer_literal(trim(value))) then
                call json_value_raw(jb, trim(value))
            else
                call json_value_string(jb, trim(value))
            end if
            i = i + 1
        end do

        do row = 1, N_GREMLIN_OPTIONS
            if (.not. allocated(lists(row)%s)) cycle
            call json_key(jb, trim(GREMLIN_OPTIONS(row)%key))
            call json_array_start(jb)
            j = 1
            do
                eq = index(lists(row)%s(j:), char(0))
                if (eq == 0) then
                    call json_value_string(jb, lists(row)%s(j:))
                    exit
                end if
                call json_value_string(jb, lists(row)%s(j:j + eq - 2))
                j = j + eq
            end do
            call json_array_end(jb)
        end do
        call json_object_end(jb)
        request_json = json_to_string(jb)
        call gremlin_apply_adapter_defaults(action, request_json)
    end subroutine parse_cli

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

    subroutine print_gremlin_usage(unit, action)
        !! Per-action help rendered from the shared option table, so every
        !! accepted flag is listed with its value kind, choices and default.
        integer, intent(in) :: unit
        character(len=*), intent(in) :: action
        character(len=10), parameter :: actions(7) = [character(len=10) :: &
            'start', 'status', 'wait', 'events', 'failures', 'reproduce', 'stop']
        integer :: i, bit

        bit = gremlin_action_bit(action)
        if (bit == GA_OTHER) then
            write (unit, '(a)') 'usage: fo gremlin [ACTION] [--dir DIR] [options]'
            write (unit, '(a)') '  actions: start (default), status, wait, events, failures,'
            write (unit, '(a)') '           reproduce, stop'
            write (unit, '(a)') '  fo gremlin ACTION --help lists the options of one action'
            write (unit, '(a)') '  output is the shared Gremlin JSON response'
            do i = 1, size(actions)
                write (unit, '(a)') ''
                call print_action_options(unit, trim(actions(i)))
            end do
            return
        end if
        call print_action_options(unit, action)
    end subroutine print_gremlin_usage

    subroutine print_action_options(unit, action)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: action
        type(gremlin_option_t) :: o
        character(len=:), allocatable :: head, detail
        integer :: i, bit

        bit = gremlin_action_bit(action)
        if (action == 'reproduce') then
            write (unit, '(a)') 'usage: fo gremlin reproduce [ID] [--dir DIR] [options]'
        else
            write (unit, '(a)') 'usage: fo gremlin '//action//' [--dir DIR] [options]'
        end if
        write (unit, '(a)') '  --dir DIR  project directory (default: .)'
        do i = 1, N_GREMLIN_OPTIONS
            o = GREMLIN_OPTIONS(i)
            if (iand(o%actions, bit) == 0) cycle
            head = '  '//trim(o%flag)
            if (len_trim(o%alias) > 0) head = head//'|'//trim(o%alias)
            select case (trim(o%kind))
            case ('int')
                head = head//' N'
            case ('string', 'list')
                head = head//' '//value_name(o)
            end select
            detail = ''
            if (len_trim(o%choices) > 0) then
                detail = 'one of '//trim(o%choices)
            else if (trim(o%kind) == 'int') then
                detail = range_text(o)
            end if
            if (trim(o%kind) == 'list') detail = append(detail, 'repeatable')
            detail = append(detail, option_default(o%key, bit))
            if (len_trim(o%note) > 0) detail = append(detail, trim(o%note))
            write (unit, '(a)') head
            if (len(detail) > 0) write (unit, '(a)') '      '//detail
        end do
    end subroutine print_action_options

    function value_name(o) result(text)
        type(gremlin_option_t), intent(in) :: o
        character(len=:), allocatable :: text
        integer :: i

        text = trim(o%key)
        if (len_trim(o%choices) > 0) text = 'CHOICE'
        if (trim(o%kind) == 'list' .and. text(len(text):) == 's') &
            text = text(:len(text) - 1)
        do i = 1, len(text)
            if (text(i:i) == '_') then
                text(i:i) = '-'
            else if (text(i:i) >= 'a' .and. text(i:i) <= 'z') then
                text(i:i) = achar(iachar(text(i:i)) - 32)
            end if
        end do
    end function value_name

    function range_text(o) result(text)
        type(gremlin_option_t), intent(in) :: o
        character(len=:), allocatable :: text
        character(len=24) :: lo, hi

        write (lo, '(i0)') o%lo
        write (hi, '(i0)') o%hi
        if (o%hi == GA_UNBOUNDED) then
            text = 'at least '//trim(lo)
        else
            text = 'range '//trim(lo)//'..'//trim(hi)
        end if
    end function range_text

    function append(text, part) result(joined)
        character(len=*), intent(in) :: text, part
        character(len=:), allocatable :: joined

        joined = text
        if (len(part) == 0) return
        if (len(joined) > 0) joined = joined//'; '
        joined = joined//part
    end function append

    function option_default(key, bit) result(text)
        !! The default taken from a default-initialised request, so help cannot
        !! drift from the parser.
        character(len=*), intent(in) :: key
        integer, intent(in) :: bit
        character(len=:), allocatable :: text
        type(gremlin_request_t) :: d
        character(len=24) :: number

        number = ''
        text = ''
        select case (trim(key))
        case ('lane_id')
            text = trim(d%lane_id)
        case ('random_count')
            write (number, '(i0)') d%random_count
        case ('seed')
            text = '0 (seed from the clock)'
        case ('campaign_seconds')
            write (number, '(i0)') d%campaign_seconds
        case ('timeout_seconds')
            write (number, '(i0)') d%timeout_seconds
        case ('cursor', 'lifecycle_cursor')
            if (iand(bit, GA_WAIT) /= 0) then
                text = 'end of the journal for an untyped wait'
            else
                text = '0'
            end if
        case ('max_records')
            write (number, '(i0)') d%max_records
        case ('max_bytes')
            write (number, '(i0)') d%max_bytes
        case ('wait_ms')
            write (number, '(i0)') d%wait_ms
        case ('lifetime')
            text = trim(d%lifetime)
        case ('detail')
            text = 'summary'
        case ('only_changed', 'shuffle')
            text = 'false'
        case ('wait_until')
            text = 'the next event'
        end select
        if (len_trim(number) > 0 .and. len(text) == 0) text = trim(number)
        if (len(text) > 0) text = 'default: '//text
    end function option_default


end module fo_gremlin_cli
