module fo_lsp
    use, intrinsic :: iso_fortran_env, only: input_unit
    use fo_json, only: json_escape, extract_json_field, make_tmpfile, &
                       read_text_file, delete_tmpfile, send_jsonrpc, &
                       jsonrpc_error, jsonrpc_null
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
                read (input_unit, '(a)', iostat=iostat) header_line
                if (iostat /= 0) return
                if (len_trim(header_line) == 0 .or. &
                    trim(header_line) == char(13)) exit
                call parse_content_length(header_line, content_length)
            end do

            if (content_length <= 0 .or. content_length > MAX_LINE) cycle

            ! read body
            read (input_unit, '(a)', iostat=iostat) body
            if (iostat /= 0) return

            call extract_json_field(body, '"method"', method)
            call extract_json_field(body, '"id"', id_str)

            select case (trim(method))
            case ('initialize')
                call make_lsp_init_response(id_str, response)
                call send_jsonrpc(response)
            case ('initialized')
                cycle
            case ('textDocument/didSave')
                call handle_did_save(body)
            case ('textDocument/didOpen')
                cycle
            case ('textDocument/didClose')
                cycle
            case ('shutdown')
                call jsonrpc_null(id_str, response)
                call send_jsonrpc(response)
            case ('exit')
                return
            case default
                if (len_trim(id_str) > 0) then
                    call jsonrpc_error(id_str, -32601, &
                                       'method not found', response)
                    call send_jsonrpc(response)
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

        read (header(pos + 15:), *, iostat=iostat) length
    end subroutine parse_content_length

    subroutine make_lsp_init_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"capabilities":{'// &
                   '"textDocumentSync":{"openClose":true,"save":true}'// &
                   '},"serverInfo":{"name":"fo","version":"0.1.0"}}}'
    end subroutine make_lsp_init_response

    subroutine handle_did_save(body)
        character(len=*), intent(in) :: body

        character(len=512) :: uri, tmpfile
        character(len=4096) :: diag_text
        character(len=MAX_LINE) :: notification
        integer :: exitcode, cmdstat

        call extract_json_field(body, '"uri"', uri)

        call make_tmpfile('fo_lsp_check', tmpfile)
        call execute_command_line('fo check > '//trim(tmpfile)//' 2>&1', &
                                  exitstat=exitcode, cmdstat=cmdstat, wait=.true.)
        if (cmdstat /= 0) exitcode = 1

        call read_text_file(tmpfile, diag_text)
        call delete_tmpfile(tmpfile)

        ! publish diagnostics notification
        call json_escape(diag_text)
        if (exitcode /= 0) then
            notification = '{"jsonrpc":"2.0",'// &
                           '"method":"textDocument/publishDiagnostics",'// &
                           '"params":{"uri":"'//trim(uri)//'",'// &
                           '"diagnostics":[{"range":{'// &
                           '"start":{"line":0,"character":0},'// &
                           '"end":{"line":0,"character":0}},'// &
                           '"severity":1,'// &
                           '"source":"fo",'// &
                           '"message":"'//trim(diag_text)//'"}]}}'
        else
            ! clear diagnostics on success
            notification = '{"jsonrpc":"2.0",'// &
                           '"method":"textDocument/publishDiagnostics",'// &
                           '"params":{"uri":"'//trim(uri)//'",'// &
                           '"diagnostics":[]}}'
        end if

        call send_jsonrpc(notification)
    end subroutine handle_did_save

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
