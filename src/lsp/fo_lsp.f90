module fo_lsp
    use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
    implicit none
    private
    public :: lsp_serve

    integer, parameter :: MAX_LINE = 16384

contains

    subroutine lsp_serve()
        character(len=MAX_LINE) :: body, response
        character(len=256) :: method, id_str
        integer :: content_length, iostat
        character(len=512) :: header_line

        do
            ! read headers until blank line
            content_length = 0
            do
                read(input_unit, '(a)', iostat=iostat) header_line
                if (iostat /= 0) return
                if (len_trim(header_line) == 0 .or. &
                    trim(header_line) == char(13)) exit
                call parse_content_length(header_line, content_length)
            end do

            if (content_length <= 0 .or. content_length > MAX_LINE) cycle

            ! read body
            read(input_unit, '(a)', iostat=iostat) body
            if (iostat /= 0) return

            call extract_lsp_field(body, '"method"', method)
            call extract_lsp_field(body, '"id"', id_str)

            select case (trim(method))
            case ('initialize')
                call make_lsp_init_response(id_str, response)
                call send_lsp(response)
            case ('initialized')
                cycle
            case ('textDocument/didSave')
                call handle_did_save(body)
            case ('textDocument/didOpen')
                cycle
            case ('textDocument/didClose')
                cycle
            case ('shutdown')
                call make_lsp_null_result(id_str, response)
                call send_lsp(response)
            case ('exit')
                return
            case default
                if (len_trim(id_str) > 0) then
                    call make_lsp_error(id_str, -32601, &
                        'method not found', response)
                    call send_lsp(response)
                end if
            end select
        end do
    end subroutine lsp_serve

    subroutine parse_content_length(header, length)
        character(len=*), intent(in) :: header
        integer, intent(inout) :: length

        integer :: pos, iostat
        character(len=256) :: lower_hdr

        lower_hdr = header
        call to_lower_lsp(lower_hdr)

        pos = index(lower_hdr, 'content-length:')
        if (pos == 0) return

        read(header(pos+15:), *, iostat=iostat) length
    end subroutine parse_content_length

    subroutine send_lsp(response)
        character(len=*), intent(in) :: response

        character(len=16) :: len_str
        integer :: n

        n = len_trim(response)
        write(len_str, '(i0)') n
        write(output_unit, '(a,a,a,a,a)', advance='no') &
            'Content-Length: ', trim(len_str), char(13)//char(10), &
            char(13)//char(10), trim(response)
        flush(output_unit)
    end subroutine send_lsp

    subroutine make_lsp_init_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',' // &
            '"result":{"capabilities":{' // &
            '"textDocumentSync":{"openClose":true,"save":true}' // &
            '},"serverInfo":{"name":"fo","version":"0.1.0"}}}'
    end subroutine make_lsp_init_response

    subroutine handle_did_save(body)
        character(len=*), intent(in) :: body

        character(len=512) :: uri, tmpfile, buf
        character(len=4096) :: diag_text
        character(len=MAX_LINE) :: notification
        integer :: exitcode, cmdstat, u, iostat, n

        ! extract the saved file URI
        call extract_lsp_field(body, '"uri"', uri)

        ! run fo check and capture output
        tmpfile = '/tmp/fo_lsp_check.tmp'
        call execute_command_line('fo check > '//trim(tmpfile)//' 2>&1', &
            exitstat=exitcode, cmdstat=cmdstat, wait=.true.)
        if (cmdstat /= 0) exitcode = 1

        diag_text = ''
        n = 0
        open(newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read(u, '(a)', iostat=iostat) buf
                if (iostat /= 0) exit
                if (n + len_trim(buf) + 1 > len(diag_text)) exit
                diag_text(n+1:n+len_trim(buf)) = trim(buf)
                n = n + len_trim(buf)
                n = n + 1
                diag_text(n:n) = char(10)
            end do
            close(u)
        end if
        call execute_command_line('rm -f '//trim(tmpfile), wait=.true.)

        ! publish diagnostics notification
        call escape_lsp_json(diag_text)
        if (exitcode /= 0) then
            notification = '{"jsonrpc":"2.0",' // &
                '"method":"textDocument/publishDiagnostics",' // &
                '"params":{"uri":"'//trim(uri)//'",' // &
                '"diagnostics":[{"range":{' // &
                '"start":{"line":0,"character":0},' // &
                '"end":{"line":0,"character":0}},' // &
                '"severity":1,' // &
                '"source":"fo",' // &
                '"message":"'//trim(diag_text)//'"}]}}'
        else
            ! clear diagnostics on success
            notification = '{"jsonrpc":"2.0",' // &
                '"method":"textDocument/publishDiagnostics",' // &
                '"params":{"uri":"'//trim(uri)//'",' // &
                '"diagnostics":[]}}'
        end if

        call send_lsp(notification)
    end subroutine handle_did_save

    subroutine make_lsp_null_result(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',"result":null}'
    end subroutine make_lsp_null_result

    subroutine make_lsp_error(id_str, code, msg, response)
        character(len=*), intent(in) :: id_str, msg
        integer, intent(in) :: code
        character(len=*), intent(out) :: response

        character(len=16) :: code_str

        write(code_str, '(i0)') code
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',' // &
            '"error":{"code":'//trim(code_str)//',' // &
            '"message":"'//trim(msg)//'"}}'
    end subroutine make_lsp_error

    subroutine extract_lsp_field(line, key, val)
        character(len=*), intent(in) :: line, key
        character(len=*), intent(out) :: val

        integer :: pos, start, fin
        character(len=1) :: ch

        val = ''
        pos = index(line, trim(key))
        if (pos == 0) return

        pos = pos + len_trim(key)
        do while (pos <= len_trim(line))
            if (line(pos:pos) == ':') exit
            pos = pos + 1
        end do
        pos = pos + 1

        do while (pos <= len_trim(line) .and. line(pos:pos) == ' ')
            pos = pos + 1
        end do

        if (pos > len_trim(line)) return

        ch = line(pos:pos)
        if (ch == '"') then
            start = pos + 1
            fin = start
            do while (fin <= len_trim(line))
                if (line(fin:fin) == '"' .and. &
                    (fin == start .or. line(fin-1:fin-1) /= '\')) exit
                fin = fin + 1
            end do
            val = line(start:fin-1)
        else
            start = pos
            fin = pos
            do while (fin <= len_trim(line))
                ch = line(fin:fin)
                if (ch == ',' .or. ch == '}' .or. ch == ' ') exit
                fin = fin + 1
            end do
            val = line(start:fin-1)
        end if
    end subroutine extract_lsp_field

    subroutine escape_lsp_json(str)
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
            case default
                if (j + 1 > len(buf)) exit
                j = j + 1; buf(j:j) = str(i:i)
            end select
        end do

        str = buf(1:j)
    end subroutine escape_lsp_json

    subroutine to_lower_lsp(str)
        character(len=*), intent(inout) :: str
        integer :: i, ic

        do i = 1, len_trim(str)
            ic = iachar(str(i:i))
            if (ic >= iachar('A') .and. ic <= iachar('Z')) then
                str(i:i) = achar(ic + 32)
            end if
        end do
    end subroutine to_lower_lsp

end module fo_lsp
