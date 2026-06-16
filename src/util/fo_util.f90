module fo_util
    use, intrinsic :: iso_c_binding, only: c_int
    use fo_fs, only: fs_collect_files, fs_remove_file
    use fx_mcp, only: mcp_send_response, MCP_FRAME_UNKNOWN
    implicit none
    private
    public :: make_tmpfile, delete_tmpfile, read_text_file
    public :: clean_root_build_artifacts
    public :: strip_path_prefix_in_str
    public :: send_jsonrpc, jsonrpc_error, jsonrpc_null
    public :: json_bool, json_int
    public :: extract_json_field

    interface
        subroutine fo_c_getpid(pid_out) bind(C, name='fo_c_getpid')
            import :: c_int
            integer(c_int), intent(out) :: pid_out
        end subroutine fo_c_getpid
    end interface

contains

    subroutine make_tmpfile(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path

        integer :: count
        integer(c_int) :: pid
        integer, save :: serial = 0
        integer :: serial_local

        !$omp critical (fo_tmpfile_serial)
        serial = serial + 1
        serial_local = serial
        !$omp end critical (fo_tmpfile_serial)
        call fo_c_getpid(pid)
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a,i0,a)') '/tmp/', trim(prefix), '-', &
            int(pid), '-', count, '-', serial_local, '.tmp'
    end subroutine make_tmpfile

    subroutine delete_tmpfile(path)
        character(len=*), intent(in) :: path

        integer :: u, ios

        ! Delete via Fortran close(status='delete'), never execute_command_line:
        ! cache_lookup deletes its scratch record from inside an OpenMP parallel
        ! region, and forking (fork+exec for rm) from a multithreaded region
        ! corrupts the libgomp runtime, surfacing as nondeterministic crashes
        ! and spurious cache misses.
        open (newunit=u, file=trim(path), status='old', iostat=ios)
        if (ios == 0) close (u, status='delete')
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

    subroutine clean_root_build_artifacts(dir, n_removed)
        character(len=*), intent(in) :: dir
        integer, intent(out) :: n_removed

        character(len=512), allocatable :: hits(:)
        integer :: n, k, s
        character(len=8) :: suffixes(3)

        n_removed = 0
        suffixes(1) = '.mod'
        suffixes(2) = '.smod'
        suffixes(3) = '.o'
        allocate (hits(1024))
        do s = 1, 3
            ! Top-level build artifacts only (find -maxdepth 1), removed one by
            ! one. Editors and stray builds drop these in the project root where
            ! they shadow build/fo/mod.
            call fs_collect_files(trim(dir), '', trim(suffixes(s)), '', hits, n, &
                                  recursive=.false.)
            do k = 1, n
                if (len_trim(hits(k)) == 0) cycle
                call fs_remove_file(trim(hits(k)))
                n_removed = n_removed + 1
            end do
        end do
        deallocate (hits)
    end subroutine clean_root_build_artifacts

    subroutine strip_path_prefix_in_str(text, prefix)
        character(len=*), intent(inout) :: text
        character(len=*), intent(in) :: prefix

        character(len=len(text)) :: buf
        integer :: pos, plen, tlen, n

        plen = len_trim(prefix)
        if (plen == 0) return

        buf = ''
        n = 0
        pos = 1
        tlen = len_trim(text)

        do while (pos <= tlen)
            if (pos + plen - 1 <= tlen .and. &
                text(pos:pos + plen - 1) == prefix(1:plen)) then
            pos = pos + plen
        else
            if (n + 1 <= len(buf)) then
                n = n + 1
                buf(n:n) = text(pos:pos)
            end if
            pos = pos + 1
        end if
    end do

    text = buf(1:n)
end subroutine strip_path_prefix_in_str

subroutine send_jsonrpc(response)
    character(len=*), intent(in) :: response

    call mcp_send_response(trim(response), MCP_FRAME_UNKNOWN)
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

pure function sq(s) result(r)
    character(len=*), intent(in) :: s
    character(len=len_trim(s) + 2) :: r
    r = "'"//trim(s)//"'"
end function sq

subroutine jsonrpc_null(id_str, response)
    character(len=*), intent(in) :: id_str
    character(len=*), intent(out) :: response

    response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',"result":null}'
end subroutine jsonrpc_null

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

! Extract a value from flat JSON by key. key may include surrounding
! double-quotes (e.g. '"method"') or omit them ('method').
! Handles both quoted string values and bare values (numbers, booleans).
subroutine extract_json_field(line, key, val)
    character(len=*), intent(in) :: line, key
    character(len=*), intent(out) :: val

    integer :: pos, start, fin, k1, k2
    character(len=len_trim(key)) :: clean_key
    character(len=1) :: ch

    val = ''

    ! Strip surrounding quotes from key
    k1 = 1
    k2 = len_trim(key)
    if (k2 >= k1 .and. key(k1:k1) == '"') k1 = k1 + 1
    if (k2 >= k1 .and. key(k2:k2) == '"') k2 = k2 - 1
    if (k2 < k1) return
    clean_key = key(k1:k2)

    pos = index(line, '"'//trim(clean_key)//'"')
    if (pos == 0) return

    pos = pos + len_trim(clean_key) + 2
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

end module fo_util
