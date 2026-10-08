module fo_util
    use, intrinsic :: iso_fortran_env, only: int64, real64, iostat_eor
    use, intrinsic :: iso_c_binding, only: c_int
    use fo_fs, only: fs_collect_files, fs_remove_file
    use fx_mcp, only: mcp_send_response, MCP_FRAME_UNKNOWN
    implicit none
    private
    public :: make_tmpfile, make_sibling_tmpfile, delete_tmpfile, read_text_file
    public :: clean_root_build_artifacts, read_text_file_alloc
    public :: strip_path_prefix_in_str
    public :: send_jsonrpc, jsonrpc_error, jsonrpc_null
    public :: json_bool, json_int
    public :: wall_time_seconds

    interface
        subroutine fo_c_getpid(pid_out) bind(C, name='fo_c_getpid')
            import :: c_int
            integer(c_int), intent(out) :: pid_out
        end subroutine fo_c_getpid
    end interface

contains

    function wall_time_seconds() result(seconds)
        real(real64) :: seconds
        integer(int64) :: count, rate

        call system_clock(count=count, count_rate=rate)
        if (rate > 0_int64) then
            seconds = real(count, real64)/real(rate, real64)
        else
            seconds = 0.0_real64
        end if
    end function wall_time_seconds

    subroutine make_tmpfile(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path

        integer :: count
        integer(c_int) :: pid
        integer, save :: serial = 0
        integer :: serial_local, tmpdir_len, tmpdir_status
        character(len=512) :: tmpdir

        !$omp critical (fo_tmpfile_serial)
        serial = serial + 1
        serial_local = serial
        !$omp end critical (fo_tmpfile_serial)
        call fo_c_getpid(pid)
        call system_clock(count)
        tmpdir = ''
        call get_environment_variable('TMPDIR', tmpdir, length=tmpdir_len, &
            status=tmpdir_status)
        if (tmpdir_status /= 0 .or. tmpdir_len <= 0 .or. tmpdir_len > len(tmpdir)) then
            tmpdir = '/var/tmp'
        else
            tmpdir = trim(tmpdir)
            if (len_trim(tmpdir) == 0) tmpdir = '/var/tmp'
        end if
        if (tmpdir(len_trim(tmpdir):len_trim(tmpdir)) == '/') then
            write (path, '(a,a,a,i0,a,i0,a,i0,a)') trim(tmpdir), trim(prefix), '-', &
                int(pid), '-', count, '-', serial_local, '.tmp'
        else
            write (path, '(a,a,a,a,i0,a,i0,a,i0,a)') trim(tmpdir), '/', &
                trim(prefix), '-', int(pid), '-', count, '-', serial_local, '.tmp'
        end if
    end subroutine make_tmpfile

    subroutine make_sibling_tmpfile(target, path)
        character(len=*), intent(in) :: target
        character(len=*), intent(out) :: path

        integer :: count
        integer(c_int) :: pid
        integer, save :: serial = 0
        integer :: serial_local

        !$omp critical (fo_sibling_tmpfile_serial)
        serial = serial + 1
        serial_local = serial
        !$omp end critical (fo_sibling_tmpfile_serial)
        call fo_c_getpid(pid)
        call system_clock(count)
        write (path, '(a,a,i0,a,i0,a,i0)') trim(target), '.tmp-', int(pid), '-', &
            count, '-', serial_local
    end subroutine make_sibling_tmpfile

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
        integer :: u, iostat, n, got, i, out_pos
        integer(int64) :: record_len, trimmed_len, available
        logical :: record_complete

        text = ''
        n = 0
        open (newunit=u, file=trim(path), status='old', iostat=iostat)
        if (iostat /= 0) return
        do
            record_len = 0_int64
            trimmed_len = 0_int64
            available = int(len(text) - n - 1, int64)
            do
                got = 0
                read (u, '(a)', advance='no', size=got, iostat=iostat) buf
                do i = 1, got
                    record_len = record_len + 1_int64
                    if (buf(i:i) /= ' ') trimmed_len = record_len
                    if (record_len <= available) then
                        out_pos = n + int(record_len)
                        text(out_pos:out_pos) = buf(i:i)
                    end if
                end do
                if (iostat == 0) cycle
                record_complete = iostat == iostat_eor
                exit
            end do
            if (.not. record_complete) then
                if (n < len(text)) text(n + 1:) = ''
                exit
            end if
            if (trimmed_len + 1_int64 > int(len(text) - n, int64)) then
                if (n < len(text)) text(n + 1:) = ''
                exit
            end if
            n = n + int(trimmed_len)
            n = n + 1
            text(n:n) = char(10)
        end do
        close (u)
    end subroutine read_text_file

    subroutine read_text_file_alloc(path, text, ierr)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: ierr

        integer :: unit, file_size

        text = ''
        open (newunit=unit, file=trim(path), status='old', &
            access='stream', form='unformatted', iostat=ierr)
        if (ierr /= 0) return
        inquire (unit=unit, size=file_size, iostat=ierr)
        if (ierr == 0) then
            if (file_size < 0) then
                ierr = 1
            else
                deallocate (text)
                allocate (character(len=file_size) :: text)
                read (unit, iostat=ierr) text
                if (ierr /= 0) text = ''
            end if
        end if
        close (unit)
    end subroutine read_text_file_alloc

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
        character(len=:), allocatable, intent(out) :: response

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
        character(len=:), allocatable, intent(out) :: response

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

end module fo_util
