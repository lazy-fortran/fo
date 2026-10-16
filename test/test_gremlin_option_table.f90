program test_gremlin_option_table
    !! One option table drives Gremlin CLI parsing, request validation and the
    !! MCP tool schema, so the three surfaces agree on every option. Also covers
    !! the shared strict JSON member lookup.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_string, assert_equal_integer
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_number_value
    use fo_test_mcp, only: mcp_request, mcp_call, mcp_quote, mcp_exchange
    use fo_gremlin_request, only: GREMLIN_OPTIONS, N_GREMLIN_OPTIONS, gremlin_option_t, &
        GA_START, GA_STATUS, GA_WAIT, GA_REPRODUCE, gremlin_json_field, &
        gremlin_option_index
    use fo_json_object, only: json_object_member, json_text_has_control
    implicit none

    integer, parameter :: max_probes = 3*N_GREMLIN_OPTIONS
    character(len=1), parameter :: nl = achar(10)
    character(:), allocatable :: driver, scratch, project, state, cache
    type(json_value_t) :: properties
    character(len=32) :: flags(max_probes), keys(max_probes)
    character(len=32) :: actions(max_probes), values(max_probes)
    logical :: valid(max_probes)
    integer :: n

    call resolve_driver(driver)
    call make_scratch('fo-gremlin-options', scratch)
    project = join_path(scratch, 'project')
    state = join_path(scratch, 'state')
    cache = join_path(scratch, 'cache')
    call write_text(join_path(project, 'fpm.toml'), 'name = "options"'//nl)

    call read_schema()
    call test_schema()
    call test_surfaces()
    call test_table_lookup()
    call test_member_lookup()
    call test_field_status()

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine read_schema()
        type(json_value_t), allocatable :: replies(:)
        type(process_result_t) :: result
        character(:), allocatable :: input

        input = mcp_request(1, 'tools/list', '{}')//nl
        call mcp_exchange(driver, project, cache, input, .false., replies, result)
        properties = json_member(json_member(json_element(json_member(json_member( &
            replies(1), 'result'), 'tools'), 1), 'inputSchema'), 'properties')
    end subroutine read_schema

    subroutine test_schema()
        type(json_value_t) :: property
        integer :: i, j, choices
        character(:), allocatable :: label

        do i = 1, N_GREMLIN_OPTIONS
            associate (option => GREMLIN_OPTIONS(i))
                label = 'schema '//trim(option%key)
                property = json_member(properties, trim(option%key))
                call assert_equal_string(json_string_value(json_member(property, 'type')), &
                    schema_type(trim(option%kind)), label//' type')
                if (trim(option%kind) == 'int') then
                    if (option%lo == option%hi) then
                        call assert_equal_integer(int(json_number_value( &
                            json_member(property, 'const'))), int(option%lo), &
                            label//' const')
                    else
                        if (option%lo >= 0) call assert_equal_integer(int( &
                            json_number_value(json_member(property, 'minimum'))), &
                            int(option%lo), label//' minimum')
                        if (option%hi < int(huge(0), int64)) call assert_equal_integer( &
                            int(json_number_value(json_member(property, 'maximum'))), &
                            int(option%hi), label//' maximum')
                    end if
                end if
                if (len_trim(option%choices) > 0) then
                    choices = count([(option%choices(j:j) == ',', j=1, &
                        len_trim(option%choices))]) + 1
                    call assert_equal_integer(json_size(json_member(property, 'enum')), &
                        choices, label//' choices')
                end if
            end associate
        end do
    end subroutine test_schema

    function schema_type(kind) result(name)
        character(len=*), intent(in) :: kind
        character(:), allocatable :: name

        select case (kind)
        case ('int')
            name = 'integer'
        case ('bool')
            name = 'boolean'
        case ('list')
            name = 'array'
        case default
            name = 'string'
        end select
    end function schema_type

    function action_name(actions) result(name)
        integer, intent(in) :: actions
        character(:), allocatable :: name

        if (iand(actions, GA_START) /= 0) then
            name = 'start'
        else if (iand(actions, GA_STATUS) /= 0) then
            name = 'status'
        else if (iand(actions, GA_WAIT) /= 0) then
            name = 'wait'
        else
            name = 'reproduce'
        end if
    end function action_name

    function int_digits(value) result(text)
        integer(int64), intent(in) :: value
        character(:), allocatable :: text
        character(len=24) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function int_digits

    subroutine test_surfaces()
        !! Out-of-range values are refused, and in-range ones are not, with the
        !! same message on the CLI and over MCP.
        integer :: i
        type(json_value_t), allocatable :: replies(:)
        type(process_result_t) :: result, cli_result
        type(string_list_t) :: arguments
        type(json_value_t) :: document
        character(:), allocatable :: input, cli_error, mcp_error, parse_message
        logical :: ok

        n = 0
        do i = 1, N_GREMLIN_OPTIONS
            associate (option => GREMLIN_OPTIONS(i))
                select case (trim(option%kind))
                case ('int')
                    if (option%lo >= 0) call add_probe(option, option%lo - 1, .false.)
                    if (option%hi < int(huge(0), int64)) &
                        call add_probe(option, option%hi + 1, .false.)
                    call add_probe(option, max(option%lo, 0_int64), .true.)
                case ('string')
                    if (len_trim(option%choices) > 0) then
                        call add_text_probe(option, 'bogus-value', .false.)
                        call add_text_probe(option, first_choice(option%choices), .true.)
                    end if
                    if (trim(option%key) == 'filter') then
                        call add_text_probe(option, 'name(', .false.)
                        call add_text_probe(option, 'affected()', .false.)
                    end if
                end select
            end associate
        end do

        call assert_true(n > 20, 'the table yields option probes')
        input = mcp_request(1, 'initialize', '{"protocolVersion":"2025-11-25"}')//nl
        do i = 1, n
            input = input//mcp_call(i + 1, '{"action":"gremlin_'//trim(actions(i))// &
                '","dir":'//mcp_quote(project)//',"'//trim(keys(i))//'":'// &
                json_value(trim(keys(i)), trim(values(i)))//'}')//nl
        end do
        call mcp_exchange(driver, project, cache, input, .false., replies, result)
        call assert_equal_integer(size(replies), n + 1, 'every option probe is answered')
        if (size(replies) /= n + 1) return
        do i = 1, n
            arguments = string_list_t()
            call list_add(arguments, 'gremlin')
            call list_add(arguments, trim(actions(i)))
            call list_add(arguments, '--dir')
            call list_add(arguments, project)
            call list_add(arguments, trim(flags(i)))
            call list_add(arguments, trim(values(i)))
            call run_gremlin(arguments, cli_result)
            call json_parse(cli_result%stdout, document, ok, parse_message)
            cli_error = json_string_value(json_member(document, 'error'))
            call json_parse(json_string_value(json_member(json_element(json_member( &
                json_member(replies(i + 1), 'result'), 'content'), 1), 'text')), &
                document, ok, parse_message)
            mcp_error = json_string_value(json_member(document, 'error'))
            call assert_equal_string(mcp_error, cli_error, &
                trim(keys(i))//' '//trim(values(i))//': MCP and CLI report the same error')
            if (valid(i)) then
                call assert_true(index(mcp_error, trim(keys(i))//' must') /= 1 .and. &
                    index(mcp_error, 'integer request field') /= 1, &
                    trim(keys(i))//' '//trim(values(i))//' is accepted')
            else
                call assert_true(index(mcp_error, trim(keys(i))) == 1, &
                    trim(keys(i))//' '//trim(values(i))//' is refused')
            end if
        end do
    end subroutine test_surfaces

    subroutine add_probe(option, value, accepted)
        type(gremlin_option_t), intent(in) :: option
        integer(int64), intent(in) :: value
        logical, intent(in) :: accepted

        call add_text_probe(option, int_digits(value), accepted)
    end subroutine add_probe

    subroutine add_text_probe(option, value, accepted)
        type(gremlin_option_t), intent(in) :: option
        character(len=*), intent(in) :: value
        logical, intent(in) :: accepted

        ! Accepting a start option would launch a campaign.
        if (accepted .and. action_name(option%actions) == 'start') return
        n = n + 1
        flags(n) = option%flag
        keys(n) = option%key
        values(n) = value
        actions(n) = action_name(option%actions)
        valid(n) = accepted
    end subroutine add_text_probe

    function first_choice(choices) result(choice)
        character(len=*), intent(in) :: choices
        character(:), allocatable :: choice
        integer :: comma

        comma = index(choices, ',')
        if (comma == 0) then
            choice = trim(choices)
        else
            choice = choices(:comma - 1)
        end if
    end function first_choice

    function json_value(key, value) result(text)
        character(len=*), intent(in) :: key, value
        character(:), allocatable :: text
        integer :: row

        row = gremlin_option_index(key)
        if (GREMLIN_OPTIONS(row)%kind == 'int') then
            text = value
        else
            text = mcp_quote(value)
        end if
    end function json_value

    subroutine run_gremlin(arguments, result)
        type(string_list_t), intent(in) :: arguments
        type(process_result_t), intent(out) :: result
        type(string_list_t) :: environment

        call list_add(environment, 'FO_GREMLIN_STATE_DIR='//state)
        call run_fo(driver, arguments, project, cache, result, environment)
    end subroutine run_gremlin

    subroutine test_table_lookup()
        call assert_equal_integer(gremlin_option_index('--lane', flag=.true.), &
            gremlin_option_index('lane_id'), 'a flag finds its option')
        call assert_equal_integer(gremlin_option_index('--label', flag=.true.), &
            gremlin_option_index('labels'), 'an alias finds its option')
        call assert_equal_integer(gremlin_option_index('-L', flag=.true.), &
            gremlin_option_index('labels'), 'the short flag finds its option')
        call assert_equal_integer(gremlin_option_index('cursor', 'start'), 0, &
            'start does not accept a read cursor')
        call assert_true(gremlin_option_index('cursor', 'gremlin_status') > 0, &
            'status accepts a read cursor by its public action name')
        call assert_equal_integer(gremlin_option_index('session_id', 'start'), 0, &
            'start does not accept a session')
    end subroutine test_table_lookup

    subroutine test_member_lookup()
        character(:), allocatable :: raw
        integer :: count, ierr

        call json_object_member('{"a":1,"a":2}', 'a', raw, count, ierr)
        call assert_true(ierr == 0 .and. count == 2, 'a duplicate key is counted')
        call json_object_member('{"a":1,"a":2}', 'a', raw, count, ierr, last=.true.)
        call assert_equal_string(raw, '2', 'the last duplicate can be selected')
        call json_object_member('{"a":{"a":3}}', 'a', raw, count, ierr)
        call assert_true(ierr == 0 .and. count == 1, 'nested keys are not top-level')
        call assert_equal_string(raw, '{"a":3}', 'an object value is returned whole')
        call json_object_member('{"a":1} x', 'a', raw, count, ierr)
        call assert_true(ierr /= 0, 'trailing text is malformed')
        call json_object_member('[1]', 'a', raw, count, ierr)
        call assert_true(ierr /= 0, 'a non-object is malformed')
        call assert_true(json_text_has_control('a'//achar(9)), 'a tab is a control character')
        call assert_true(.not. json_text_has_control('a b'), 'text without controls passes')
    end subroutine test_member_lookup

    subroutine test_field_status()
        character(len=8) :: value
        integer :: status

        call gremlin_json_field('{"a":"x"}', 'a', value, status=status)
        call assert_true(status == 0 .and. value == 'x', 'a present field is read')
        call gremlin_json_field('{"a":"x","a":"y"}', 'a', value, status=status)
        call assert_true(status == 2 .and. value == '', 'a duplicate field is malformed')
        call gremlin_json_field('{"a":"x","a":"y"}', 'a', value, last=.true., status=status)
        call assert_true(status == 0 .and. value == 'y', 'the last duplicate is selectable')
        call gremlin_json_field('{"a":99999999999999999999999}', 'a', value, status=status)
        call assert_true(status == 2 .and. value == '', 'an oversized value is malformed')
        call gremlin_json_field('{"a":', 'a', value, status=status)
        call assert_equal_integer(status, 2, 'truncated JSON is malformed')
        call gremlin_json_field('{"b":1}', 'a', value, status=status)
        call assert_equal_integer(status, 1, 'an absent field is reported absent')
        call gremlin_json_field('{"a":{"b":1}}', 'a', value, status=status)
        call assert_equal_integer(status, 1, 'an object value is not a scalar')
    end subroutine test_field_status

end program test_gremlin_option_table
