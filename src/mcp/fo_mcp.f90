module fo_mcp
    use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
    implicit none
    private
    public :: mcp_serve

    integer, parameter :: MAX_LINE = 8192

contains

    subroutine mcp_serve()
        character(len=MAX_LINE) :: line, response
        character(len=256) :: method, id_str
        integer :: iostat

        do
            read(input_unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle

            call extract_field(line, '"method"', method)
            call extract_field(line, '"id"', id_str)

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
                call handle_tools_call(line, id_str, response)
                call send_response(response)
            case ('resources/list')
                call make_resources_list_response(id_str, response)
                call send_response(response)
            case ('resources/read')
                call handle_resources_read(line, id_str, response)
                call send_response(response)
            case ('shutdown')
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
    end subroutine mcp_serve

    subroutine send_response(response)
        character(len=*), intent(in) :: response

        character(len=16) :: len_str
        integer :: n

        n = len_trim(response)
        write(len_str, '(i0)') n
        write(output_unit, '(a,a,a,a,a)', advance='no') &
            'Content-Length: ', trim(len_str), char(13)//char(10), &
            char(13)//char(10), trim(response)
        flush(output_unit)
    end subroutine send_response

    subroutine make_initialize_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',' // &
            '"result":{"protocolVersion":"2024-11-05",' // &
            '"capabilities":{"tools":{},"resources":{}},' // &
            '"serverInfo":{"name":"fo","version":"0.1.0"}}}'
    end subroutine make_initialize_response

    subroutine make_tools_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',' // &
            '"result":{"tools":[{"name":"fo",' // &
            '"description":"Fortran build driver",' // &
            '"inputSchema":{"type":"object","properties":{' // &
            '"action":{"type":"string",' // &
            '"enum":["check","build","test","graph","info","changed","clean"],' // &
            '"description":"Action to run"}},' // &
            '"required":["action"]}}]}}'
    end subroutine make_tools_list_response

    subroutine make_resources_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',' // &
            '"result":{"resources":[{"uri":"fo://diagnostics",' // &
            '"name":"diagnostics",' // &
            '"description":"Current fo check diagnostics",' // &
            '"mimeType":"text/plain"}]}}'
    end subroutine make_resources_list_response

    subroutine handle_tools_call(line, id_str, response)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response

        character(len=64) :: action
        character(len=4096) :: output_text
        integer :: exitcode, cmdstat
        character(len=512) :: tmpfile, cmd, buf
        integer :: u, iostat, n

        call extract_action(line, action)
        call make_tmpfile('fo_mcp_output', tmpfile)

        select case (trim(action))
        case ('check', 'build', 'test', 'graph', 'info', 'changed', 'clean')
            cmd = 'fo '//trim(action)//' > '//trim(tmpfile)//' 2>&1'
            call execute_command_line(cmd, exitstat=exitcode, &
                cmdstat=cmdstat, wait=.true.)
            if (cmdstat /= 0) exitcode = 1

            output_text = ''
            n = 0
            open(newunit=u, file=tmpfile, status='old', iostat=iostat)
            if (iostat == 0) then
                do
                    read(u, '(a)', iostat=iostat) buf
                    if (iostat /= 0) exit
                    if (n + len_trim(buf) + 1 > len(output_text)) exit
                    output_text(n+1:n+len_trim(buf)) = trim(buf)
                    n = n + len_trim(buf)
                    n = n + 1
                    output_text(n:n) = char(10)
                end do
                close(u)
            end if
            call execute_command_line('rm -f '//trim(tmpfile), wait=.true.)

            call escape_json(output_text)
            response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',' // &
                '"result":{"content":[{"type":"text",' // &
                '"text":"'//trim(output_text)//'"}],' // &
                '"isError":'
            if (exitcode /= 0) then
                response = trim(response)//'true}}'
            else
                response = trim(response)//'false}}'
            end if
        case default
            call make_error_response(id_str, -32602, &
                'unknown action: '//trim(action), response)
        end select
    end subroutine handle_tools_call

    subroutine handle_resources_read(line, id_str, response)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response

        character(len=256) :: uri
        character(len=4096) :: output_text
        character(len=512) :: tmpfile, buf
        integer :: u, iostat, n, exitcode, cmdstat

        call extract_field(line, '"uri"', uri)

        if (trim(uri) == 'fo://diagnostics' .or. &
            index(line, 'fo://diagnostics') > 0) then
            call make_tmpfile('fo_mcp_diag', tmpfile)
            call execute_command_line('fo check > '//trim(tmpfile)//' 2>&1', &
                exitstat=exitcode, cmdstat=cmdstat, wait=.true.)

            output_text = ''
            n = 0
            open(newunit=u, file=tmpfile, status='old', iostat=iostat)
            if (iostat == 0) then
                do
                    read(u, '(a)', iostat=iostat) buf
                    if (iostat /= 0) exit
                    if (n + len_trim(buf) + 1 > len(output_text)) exit
                    output_text(n+1:n+len_trim(buf)) = trim(buf)
                    n = n + len_trim(buf)
                    n = n + 1
                    output_text(n:n) = char(10)
                end do
                close(u)
            end if
            call execute_command_line('rm -f '//trim(tmpfile), wait=.true.)

            call escape_json(output_text)
            response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',' // &
                '"result":{"contents":[{"uri":"fo://diagnostics",' // &
                '"mimeType":"text/plain",' // &
                '"text":"'//trim(output_text)//'"}]}}'
        else
            call make_error_response(id_str, -32602, &
                'unknown resource', response)
        end if
    end subroutine handle_resources_read

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

        write(code_str, '(i0)') code
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',' // &
            '"error":{"code":'//trim(code_str)//',' // &
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
                if (line(fin:fin) == '"' .and. line(fin-1:fin-1) /= '\') exit
                fin = fin + 1
            end do
            val = line(start:fin-1)
        else
            ! number or other value
            start = pos
            fin = pos
            do while (fin <= len_trim(line))
                ch = line(fin:fin)
                if (ch == ',' .or. ch == '}' .or. ch == ' ') exit
                fin = fin + 1
            end do
            val = line(start:fin-1)
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
