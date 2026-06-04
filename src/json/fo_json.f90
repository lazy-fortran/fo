module fo_json
    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    private
    public :: json_bool, json_int
    public :: json_escape, json_escape_str, json_append
    public :: extract_json_field
    public :: make_tmpfile, delete_tmpfile, read_text_file
    public :: send_jsonrpc, jsonrpc_error, jsonrpc_null

contains

    function json_bool(value) result(text)
        logical, intent(in) :: value
        character(len=5) :: text

        if (value) then
            text = 'true'
        else
            text = 'false'
        end if
    end function json_bool

    function json_int(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        write (text, '(i0)') value
    end function json_int

    subroutine json_escape(str)
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
    end subroutine json_escape

    function json_escape_str(input) result(output)
        character(len=*), intent(in) :: input
        character(len=1024) :: output

        integer :: i, n
        character(len=1) :: ch

        output = ''
        n = 0
        do i = 1, len_trim(input)
            ch = input(i:i)
            select case (ch)
            case ('"')
                call json_append(output, n, achar(92)//achar(34))
            case (achar(92))
                call json_append(output, n, achar(92)//achar(92))
            case (char(10))
                call json_append(output, n, achar(92)//'n')
            case (char(13))
                call json_append(output, n, achar(92)//'r')
            case (char(9))
                call json_append(output, n, achar(92)//'t')
            case default
                if (iachar(ch) >= 32) call json_append(output, n, ch)
            end select
        end do
    end function json_escape_str

    subroutine json_append(output, n, text)
        character(len=*), intent(inout) :: output
        integer, intent(inout) :: n
        character(len=*), intent(in) :: text

        integer :: m

        m = len(text)
        if (m <= 0) return
        if (n + m > len(output)) return
        output(n + 1:n + m) = text(1:m)
        n = n + m
    end subroutine json_append

    subroutine extract_json_field(line, key, val)
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
                    (fin == start .or. line(fin - 1:fin - 1) /= '\')) exit
                fin = fin + 1
            end do
            val = line(start:fin - 1)
        else
            start = pos
            fin = pos
            do while (fin <= len_trim(line))
                ch = line(fin:fin)
                if (ch == ',' .or. ch == '}' .or. ch == ' ') exit
                fin = fin + 1
            end do
            val = line(start:fin - 1)
        end if
    end subroutine extract_json_field

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

    subroutine delete_tmpfile(path)
        character(len=*), intent(in) :: path

        call execute_command_line('rm -f '//trim(path), wait=.true.)
    end subroutine delete_tmpfile

    subroutine read_text_file(path, text)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: text

        character(len=512) :: buf
        integer :: u, iostat, n

        text = ''
        n = 0
        open (newunit=u, file=trim(path), status='old', iostat=iostat)
        if (iostat /= 0) return
        do
            read (u, '(a)', iostat=iostat) buf
            if (iostat /= 0) exit
            if (n + len_trim(buf) + 1 > len(text)) exit
            text(n + 1:n + len_trim(buf)) = trim(buf)
            n = n + len_trim(buf)
            n = n + 1
            text(n:n) = char(10)
        end do
        close (u)
    end subroutine read_text_file

    subroutine send_jsonrpc(response)
        use fo_process, only: process_get_mcp_framing
        character(len=*), intent(in) :: response

        character(len=16) :: len_str
        integer :: n, framing

        n = len_trim(response)
        framing = process_get_mcp_framing()
        if (framing == 0) then
            write (output_unit, '(a)') trim(response)
        else
            write (len_str, '(i0)') n
            write (output_unit, '(a,a,a,a,a)', advance='no') &
                'Content-Length: ', trim(len_str), char(13)//char(10), &
                char(13)//char(10), trim(response)
        end if
        flush (output_unit)
    end subroutine send_jsonrpc

    subroutine jsonrpc_error(id_str, code, msg, response)
        character(len=*), intent(in) :: id_str, msg
        integer, intent(in) :: code
        character(len=*), intent(out) :: response

        character(len=16) :: code_str

        write (code_str, '(i0)') code
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"error":{"code":'//trim(code_str)//','// &
                   '"message":"'//trim(msg)//'"}}'
    end subroutine jsonrpc_error

    subroutine jsonrpc_null(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',"result":null}'
    end subroutine jsonrpc_null

end module fo_json
