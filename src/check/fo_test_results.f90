module fo_test_results
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_util, only: json_int
    use fx_json_build, only: json_escape_string
    implicit none
    private
    public :: test_result_entry_t, MAX_TEST_RESULTS_ENTRIES
    public :: parse_test_results, format_test_results_human, format_test_results_json

    integer, parameter :: MAX_TEST_RESULTS_ENTRIES = 256

    type :: test_result_entry_t
        character(len=128) :: name
        character(len=10)   :: status
        real                :: seconds
        integer             :: exit_code
    end type test_result_entry_t

contains

    subroutine parse_test_results(log_file, entries, n_entries, ierr)
        character(len=*), intent(in) :: log_file
        type(test_result_entry_t), intent(inout) :: entries(:)
        integer, intent(out) :: n_entries
        integer, intent(out) :: ierr

        character(len=1024) :: line
        character(len=128) :: name
        character(len=10) :: status
        character(len=10) :: exit_str
        real :: secs
        integer :: u, ios, iostat

        n_entries = 0
        ierr = 0
        open (newunit=u, file=trim(log_file), status='old', iostat=ios)
        if (ios /= 0) then
            ierr = 1
            return
        end if
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'TEST_RESULT ') == 1) then
                if (n_entries >= size(entries)) then
                    write (error_unit, '(a,i0)') 'fo: test output truncated at ', &
                        n_entries, ' entries (limit reached)'
                    exit
                end if
                n_entries = n_entries + 1
                call parse_test_result_line(line, name, status, exit_str, secs, iostat)
                if (iostat == 0) then
                    entries(n_entries)%name = name
                    entries(n_entries)%status = status
                    entries(n_entries)%seconds = secs
                    entries(n_entries)%exit_code = 0
                    if (trim(exit_str) /= '-') then
                        read (exit_str, *, iostat=iostat) entries(n_entries)%exit_code
                        if (iostat /= 0) entries(n_entries)%exit_code = 1
                    end if
                end if
            else
                call parse_ctest_result_line(line, name, status, secs, iostat)
                if (iostat == 0) then
                    if (n_entries >= size(entries)) then
                        write (error_unit, '(a,i0)') &
                            'fo: test output truncated at ', n_entries, &
                            ' entries (limit reached)'
                        exit
                    end if
                    n_entries = n_entries + 1
                    entries(n_entries)%name = name
                    entries(n_entries)%status = status
                    entries(n_entries)%seconds = secs
                    entries(n_entries)%exit_code = 0
                    if (trim(status) == 'FAIL' .or. &
                        trim(status) == 'TIMEOUT') then
                        entries(n_entries)%exit_code = 1
                    end if
                end if
            end if
        end do
        close (u)
    end subroutine parse_test_results

    subroutine parse_test_result_line(line, name, status, exit_str, secs, iostat)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name, status, exit_str
        real, intent(out) :: secs
        integer, intent(out) :: iostat

        character(len=128) :: name_local, status_local, exit_local
        character(len=10) :: secs_str

        name = ''
        status = ''
        exit_str = ''
        secs = 0.0
        iostat = 0

        call parse_test_result_fields(line, name_local, status_local, &
            exit_local, secs_str, iostat)
        if (iostat /= 0) return
        name = name_local
        status = status_local
        exit_str = exit_local
        read (secs_str, *, iostat=iostat) secs
        if (iostat /= 0) secs = 0.0
    end subroutine parse_test_result_line

    subroutine parse_ctest_result_line(line, name, status, secs, iostat)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name, status
        real, intent(out) :: secs
        integer, intent(out) :: iostat

        character(len=1024) :: tail, timing
        integer :: test_pos, hash_offset, hash_pos
        integer :: colon_offset, colon_pos, status_pos, status_width
        integer :: time_iostat

        name = ''
        status = ''
        secs = 0.0
        iostat = 1
        test_pos = index(line, ' Test')
        if (test_pos == 0) return
        ! CTest right-aligns the numeric test id to the largest id in the run:
        ! "Test  #1" and "Test #100" can therefore occur in the same log.
        hash_offset = index(line(test_pos:), '#')
        if (hash_offset == 0) return
        hash_pos = test_pos + hash_offset - 1
        colon_offset = index(line(hash_pos:), ':')
        if (colon_offset == 0) return
        colon_pos = hash_pos + colon_offset - 1
        tail = line(colon_pos + 1:)
        call find_ctest_status(tail, status, status_pos, status_width)
        if (status_pos == 0) return
        call extract_ctest_name(tail(:status_pos - 1), name)
        if (len_trim(name) == 0) return
        timing = adjustl(tail(status_pos + status_width:))
        if (timing(1:1) == ':') timing = adjustl(timing(2:))
        read (timing, *, iostat=time_iostat) secs
        if (time_iostat /= 0) secs = 0.0
        iostat = 0
    end subroutine parse_ctest_result_line

    subroutine find_ctest_status(text, status, pos, width)
        character(len=*), intent(in) :: text
        character(len=*), intent(out) :: status
        integer, intent(out) :: pos, width

        status = ''
        pos = index(text, '***Failed')
        width = len('***Failed')
        if (pos > 0) then
            status = 'FAIL'
            return
        end if
        pos = index(text, '***Exception')
        width = len('***Exception')
        if (pos > 0) then
            status = 'FAIL'
            return
        end if
        pos = index(text, '***Timeout')
        width = len('***Timeout')
        if (pos > 0) then
            status = 'TIMEOUT'
            return
        end if
        pos = index(text, 'Passed')
        width = len('Passed')
        if (pos > 0) then
            status = 'PASS'
            return
        end if
        pos = index(text, 'Not Run')
        width = len('Not Run')
        if (pos > 0) then
            status = 'SKIP'
            return
        end if
        pos = index(text, 'Skipped')
        width = len('Skipped')
        if (pos > 0) status = 'SKIP'
    end subroutine find_ctest_status

    subroutine extract_ctest_name(text, name)
        character(len=*), intent(in) :: text
        character(len=*), intent(out) :: name

        character(len=1024) :: local
        integer :: padding

        local = adjustl(text)
        padding = index(local, '...')
        if (padding > 0) local = local(:padding - 1)
        name = trim(local)
    end subroutine extract_ctest_name

    subroutine parse_test_result_fields(line, name, status, exit_str, &
            secs_str, iostat)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name, status, exit_str, secs_str
        integer, intent(out) :: iostat

        character(len=128) :: name_local, status_local, exit_local
        character(len=10) :: secs_local

        name = ''
        status = ''
        exit_str = ''
        secs_str = ''
        iostat = 0

        if (len_trim(line) < 15) then
            iostat = 1
            return
        end if

        call extract_word(line, 2, name_local)
        call extract_word(line, 3, status_local)
        call extract_word(line, 4, exit_local)
        call extract_word(line, 5, secs_local)

        if (len_trim(name_local) == 0) then
            iostat = 1
            return
        end if

        name = name_local
        status = status_local
        exit_str = exit_local
        secs_str = secs_local
    end subroutine parse_test_result_fields

    subroutine extract_word(line, word_num, word)
        character(len=*), intent(in) :: line
        integer, intent(in) :: word_num
        character(len=*), intent(out) :: word

        integer :: pos, start_pos, word_count

        word = ''
        pos = 1
        word_count = 0

        do while (pos <= len_trim(line))
            do while (pos <= len_trim(line))
                if (line(pos:pos) == ' ') then
                    pos = pos + 1
                else
                    exit
                end if
            end do
            if (pos > len_trim(line)) exit
            start_pos = pos
            do while (pos <= len_trim(line))
                if (line(pos:pos) == ' ') exit
                pos = pos + 1
            end do
            word_count = word_count + 1
            if (word_count == word_num) then
                word = line(start_pos:pos - 1)
                return
            end if
        end do
    end subroutine extract_word

    subroutine format_test_results_human(entries, n_entries, log_file, &
            summary_mode, output)
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries
        character(len=*), intent(in) :: log_file
        logical, intent(in) :: summary_mode
        character(len=*), intent(out) :: output

        integer :: i, n_pass, n_fail, n_skip, n_shown
        real :: total_secs
        character(len=512) :: line
        character(len=16384) :: captured

        n_pass = 0
        n_fail = 0
        n_skip = 0
        n_shown = 0
        total_secs = 0.0

        do i = 1, n_entries
            total_secs = total_secs + entries(i)%seconds
            if (trim(entries(i)%status) == 'PASS' .or. &
                trim(entries(i)%status) == 'FLAKY') then
                n_pass = n_pass + 1
            else if (trim(entries(i)%status) == 'FAIL' .or. &
                    trim(entries(i)%status) == 'TIMEOUT') then
                n_fail = n_fail + 1
            else
                n_skip = n_skip + 1
            end if
        end do

        output = ''
        do i = 1, n_entries
            if (summary_mode .and. trim(entries(i)%status) == 'PASS') cycle
            call format_single_test_entry(entries(i), line)
            output = trim(output)//trim(line)//achar(10)
            n_shown = n_shown + 1
            if (trim(entries(i)%status) == 'FAIL') then
                call extract_captured_stdout(log_file, entries(i)%name, captured)
                if (len_trim(captured) > 0) then
                    output = trim(output)//'  --- captured stdout ---'//achar(10)
                    output = trim(output)//trim(captured)//achar(10)
                    output = trim(output)//'  --- end captured stdout ---'//achar(10)
                end if
            end if
        end do

        if (n_entries == n_shown) then
            call format_summary_line(output, n_pass, n_fail, n_skip, total_secs)
        else if (n_fail > 0) then
            call format_summary_line(output, n_pass, n_fail, n_skip, total_secs)
        else if (n_shown == 0) then
            output = ''
            write (line, '(a,i0,a,f6.1,a)') 'Tests: ', n_pass, ' passed (', &
                total_secs, 's)'
            output = trim(line)
        end if
    end subroutine format_test_results_human

    subroutine format_single_test_entry(entry, line)
        type(test_result_entry_t), intent(in) :: entry
        character(len=*), intent(out) :: line

        character(len=16) :: secs_str

        write (secs_str, '(f6.2)') entry%seconds
        line = format_name_field(entry%name)//' '// &
            format_status_field(entry%status)//' '// &
            trim(secs_str)//'s'
    end subroutine format_single_test_entry

    function format_name_field(name) result(field)
        character(len=*), intent(in) :: name
        character(len=42) :: field
        integer :: nlen

        nlen = min(len_trim(name), 42)
        field = trim(name(1:nlen))//repeat(' ', 42 - nlen)
    end function format_name_field

    function format_status_field(status) result(field)
        character(len=*), intent(in) :: status
        character(len=8) :: field
        integer :: nlen

        nlen = min(len_trim(status), 8)
        field = trim(status(1:nlen))//repeat(' ', 8 - nlen)
    end function format_status_field

    subroutine format_summary_line(output, n_pass, n_fail, n_skip, total_secs)
        character(len=*), intent(inout) :: output
        integer, intent(in) :: n_pass, n_fail, n_skip
        real, intent(in) :: total_secs

        character(len=512) :: line

        write (line, '(a,i0,a,i0,a,i0,a,f6.2,a)') &
            'Summary: ', n_pass, ' passed, ', n_fail, ' failed, ', &
            n_skip, ' skipped (', total_secs, 's)'
        output = trim(output)//trim(line)
    end subroutine format_summary_line

    subroutine extract_captured_stdout(log_file, test_name, captured)
        !! Return the stdout the build side wrote for test_name, wrapped in
        !! name-keyed delimiters. The block may appear anywhere in the master
        !! log (TEST_RESULT summary lines are emitted separately, at the end),
        !! so scan the whole file rather than reading forward from a marker.
        character(len=*), intent(in) :: log_file, test_name
        character(len=*), intent(out) :: captured

        character(len=1024) :: line
        character(len=160) :: start_marker, end_marker
        integer :: u, ios
        logical :: in_block

        captured = ''
        start_marker = '--- stdout '//trim(test_name)//' ---'
        end_marker = '--- end stdout '//trim(test_name)//' ---'
        in_block = .false.

        open (newunit=u, file=trim(log_file), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (.not. in_block) then
                if (trim(line) == trim(start_marker)) in_block = .true.
            else if (trim(line) == trim(end_marker)) then
                exit
            else if (len_trim(captured) > 0) then
                captured = trim(captured)//achar(10)//'  '//trim(line)
            else
                captured = '  '//trim(line)
            end if
        end do
        close (u)
    end subroutine extract_captured_stdout

    subroutine format_test_results_json(entries, n_entries, exit_code, output)
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries
        integer, intent(in) :: exit_code
        character(len=*), intent(out) :: output

        integer :: i, n_pass, n_fail, n_skip
        real :: total_secs
        character(len=16) :: secs_str, total_str

        n_pass = 0
        n_fail = 0
        n_skip = 0
        total_secs = 0.0

        do i = 1, n_entries
            total_secs = total_secs + entries(i)%seconds
            if (trim(entries(i)%status) == 'PASS' .or. &
                trim(entries(i)%status) == 'FLAKY') then
                n_pass = n_pass + 1
            else if (trim(entries(i)%status) == 'FAIL' .or. &
                    trim(entries(i)%status) == 'TIMEOUT') then
                n_fail = n_fail + 1
            else
                n_skip = n_skip + 1
            end if
        end do

        output = '{"tests":['
        do i = 1, n_entries
            if (i > 1) output = trim(output)//','
            output = trim(output)//'{"name":"'// &
                trim(json_escape_string(entries(i)%name))//'"'
            output = trim(output)//',"status":"'//lower_status(entries(i)%status)//'"'
            output = trim(output)//',"seconds":'
            write (secs_str, '(f8.2)') entries(i)%seconds
            output = trim(output)//trim(adjustl(secs_str))
            output = trim(output)//'}'
        end do
        output = trim(output)//'],'
        output = trim(output)//'"summary":{'
        output = trim(output)//'"passed":'//trim(json_int(n_pass))
        output = trim(output)//',"failed":'//trim(json_int(n_fail))
        output = trim(output)//',"skipped":'//trim(json_int(n_skip))
        write (total_str, '(f8.2)') total_secs
        output = trim(output)//',"total_seconds":'//trim(adjustl(total_str))
        output = trim(output)//'}'
        output = trim(output)//',"exit_code":'//trim(json_int(exit_code))//'}'
    end subroutine format_test_results_json

    pure function lower_status(status) result(out)
        character(len=*), intent(in) :: status
        character(len=len_trim(status)) :: out
        integer :: i, c

        out = trim(status)
        do i = 1, len(out)
            c = iachar(out(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) out(i:i) = achar(c + 32)
        end do
    end function lower_status

end module fo_test_results
