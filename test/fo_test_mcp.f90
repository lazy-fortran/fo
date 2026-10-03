module fo_test_mcp
    use fo_test_harness, only: string_list_t, process_result_t, list_add, list_with_first
    use fo_test_harness, only: run_process
    use fo_test_json, only: json_value_t, json_parse
    use fo_test_harness, only: assert_true, assert_equal_integer
    implicit none
    private

    public :: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange

contains

    function mcp_quote(value) result(encoded)
        character(len=*), intent(in) :: value
        character(:), allocatable :: encoded
        integer :: i, code

        encoded = '"'
        do i = 1, len(value)
            code = iachar(value(i:i))
            select case (value(i:i))
            case ('"')
                encoded = encoded // '\"'
            case ('\')
                encoded = encoded // '\\'
            case default
                if (code < 32) then
                    encoded = encoded // '\u00' // hex_digit(code / 16) // hex_digit(mod(code, 16))
                else
                    encoded = encoded // value(i:i)
                end if
            end select
        end do
        encoded = encoded // '"'
    end function mcp_quote

    function hex_digit(value) result(digit)
        integer, intent(in) :: value
        character(len=1) :: digit
        character(len=*), parameter :: digits = '0123456789abcdef'

        digit = digits(value + 1:value + 1)
    end function hex_digit

    function mcp_encode(message, framed) result(encoded)
        character(len=*), intent(in) :: message
        logical, intent(in) :: framed
        character(:), allocatable :: encoded
        character(len=32) :: length_text

        if (framed) then
            write(length_text, '(i0)') len(message)
            encoded = 'Content-Length: ' // trim(length_text) // achar(13) // achar(10) // &
                achar(13) // achar(10) // message
        else
            encoded = message // achar(10)
        end if
    end function mcp_encode

    function mcp_request(id, method, params) result(message)
        integer, intent(in) :: id
        character(len=*), intent(in) :: method
        character(len=*), intent(in), optional :: params
        character(:), allocatable :: message
        character(len=32) :: id_text

        write(id_text, '(i0)') id
        message = '{"jsonrpc":"2.0","id":' // trim(id_text) // ',"method":' // &
            mcp_quote(method)
        if (present(params)) message = message // ',"params":' // trim(params)
        message = message // '}'
    end function mcp_request

    function mcp_call(id, arguments) result(message)
        integer, intent(in) :: id
        character(len=*), intent(in) :: arguments
        character(:), allocatable :: message

        message = mcp_request(id, 'tools/call', &
            '{"name":"fo","arguments":' // trim(arguments) // '}')
    end function mcp_call

    subroutine mcp_exchange(driver, cwd, cache, input, framed, responses, result)
        character(len=*), intent(in) :: driver, cwd, cache, input
        logical, intent(in) :: framed
        type(json_value_t), allocatable, intent(out) :: responses(:)
        type(process_result_t), intent(out) :: result
        type(string_list_t) :: arguments, command, environment
        character(:), allocatable :: output, raw, error_message
        integer :: position, header_end, body_start, body_length, line_end
        integer :: count, end_position
        logical :: valid
        type(json_value_t), allocatable :: parsed(:)
        type(json_value_t) :: value

        call list_add(arguments, 'mcp-server')
        call list_with_first(driver, arguments, command)
        call list_add(environment, 'FO_CACHE_DIR=' // trim(cache))
        call list_add(environment, 'FO_DISABLE_SELF_REFRESH=1')
        call list_add(environment, 'FO_SELF_REFRESH=0')
        call list_add(environment, 'TMPDIR=/var/tmp')
        call run_process(command, cwd, result, environment, input=input, timeout_ms=300000)
        call assert_true(.not. result%runner_failed, 'MCP process runner completed')
        call assert_true(.not. result%timed_out, 'MCP server responds before timeout')
        output = result%stdout
        position = 1
        count = 0
        do while (position <= len(output))
            if (framed) then
                header_end = index(output(position:), achar(13) // achar(10) // &
                    achar(13) // achar(10))
                if (header_end == 0) exit
                header_end = position + header_end - 1
                body_start = header_end + 4
                body_length = content_length(output(position:header_end - 1))
                if (body_length < 0) exit
                end_position = body_start + body_length - 1
                if (end_position > len(output)) exit
            else
                line_end = index(output(position:), achar(10))
                if (line_end == 0) exit
                end_position = position + line_end - 2
                body_start = position
            end if
            raw = output(body_start:end_position)
            call json_parse(raw, value, valid, error_message)
            call assert_true(valid, 'MCP response parses with independent test JSON parser')
            if (valid) then
                call append_response(parsed, count, value)
            end if
            if (framed) then
                position = end_position + 1
                if (position <= len(output)) then
                    if (output(position:position + 1) == achar(13) // achar(10)) &
                        position = position + 2
                end if
            else
                position = end_position + 2
            end if
        end do
        allocate(responses(count))
        if (count > 0) responses = parsed
        call assert_equal_integer(count, expected_response_count(input), &
            'response count, child status ' // integer_text(result%exit_code) // &
            ', stderr: ' // result%stderr(1:min(len(result%stderr), 240)))
    end subroutine mcp_exchange

    integer function content_length(header) result(length)
        character(len=*), intent(in) :: header
        integer :: marker, first, last, status

        length = -1
        marker = index(header, ':')
        if (marker <= 0) return
        first = marker + 1
        do while (first <= len(header))
            if (header(first:first) /= ' ') exit
            first = first + 1
        end do
        last = index(header(first:), achar(13) // achar(10))
        if (last <= 0) last = len(header) - first + 2
        last = first + last - 2
        read(header(first:last), *, iostat=status) length
        if (status /= 0) length = -1
    end function content_length

    subroutine append_response(items, count, value)
        type(json_value_t), allocatable, intent(inout) :: items(:)
        integer, intent(inout) :: count
        type(json_value_t), intent(in) :: value
        type(json_value_t), allocatable :: grown(:)

        allocate(grown(count + 1))
        if (count > 0) grown(1:count) = items
        grown(count + 1) = value
        call move_alloc(grown, items)
        count = count + 1
    end subroutine append_response

    integer function expected_response_count(input) result(count)
        character(len=*), intent(in) :: input
        integer :: i, marker

        count = 0
        i = 1
        do while (i <= len(input))
            marker = index(input(i:), '"id":')
            if (marker <= 0) exit
            i = i + marker + 4
            count = count + 1
        end do
    end function expected_response_count

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

end module fo_test_mcp
