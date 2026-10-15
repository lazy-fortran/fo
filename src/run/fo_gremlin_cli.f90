module fo_gremlin_cli
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_gremlin_request, only: GREMLIN_OPTIONS, N_GREMLIN_OPTIONS, &
        gremlin_option_index, gremlin_apply_adapter_defaults
    use fo_gremlin_supervisor, only: gremlin_handle
    use fo_json_object, only: json_value_raw
    use fo_process, only: process_exit
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_array_start, json_array_end, json_key, &
        json_value_string, json_value_bool, json_to_string
    use fx_string, only: string_t
    implicit none
    private
    public :: gremlin_cli_run

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

    subroutine print_gremlin_usage(unit)
        integer, intent(in) :: unit

        write (unit, '(a)') 'usage: fo gremlin [start|status|wait|events|failures|reproduce|stop] [options]'
        write (unit, '(a)') '  start: --dir DIR --lane ID --target NAME --random N --seed N'
        write (unit, '(a)') '         --random 0 quiesces after required targets pass'
        write (unit, '(a)') '         --only-changed --shuffle --campaign-seconds N --timeout-seconds N'
        write (unit, '(a)') '         -L|--label RE -LE|--exclude-label RE --layer NAME'
        write (unit, '(a)') '  reads: --session ID --cursor N --lifecycle-cursor N'
        write (unit, '(a)') '         --max-records N --max-bytes N'
        write (unit, '(a)') '  reads: --detail summary|full (status/wait default: summary)'
        write (unit, '(a)') '  wait: --wait-ms N --until CONDITION'
        write (unit, '(a)') '  reproduce: ID or --case ID; optional --generation ID'
        write (unit, '(a)') '  output is the shared Gremlin JSON response'
    end subroutine print_gremlin_usage

end module fo_gremlin_cli
