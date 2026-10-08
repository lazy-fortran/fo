module fo_test_results
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fo_util, only: json_int, read_text_file_alloc
    use fx_json_build, only: json_escape_string
    implicit none
    private
    public :: test_result_entry_t, MAX_TEST_RESULTS_ENTRIES
    public :: parse_test_results, format_test_results_json
    public :: format_test_results_text, read_logical_line

    !! Initial capacity only. The entry buffer grows on demand, so a suite with
    !! more tests than this still reports every result: truncating the list used
    !! to hide failures behind a passing-looking count.
    integer, parameter :: MAX_TEST_RESULTS_ENTRIES = 256

    type :: test_result_entry_t
        character(len=:), allocatable :: name, reason
        character(len=10)   :: status
        real                :: seconds
        integer             :: exit_code
    end type test_result_entry_t

contains

    subroutine parse_test_results(log_file, entries, n_entries, ierr)
        character(len=*), intent(in) :: log_file
        type(test_result_entry_t), allocatable, intent(inout) :: entries(:)
        integer, intent(out) :: n_entries
        integer, intent(out) :: ierr

        character(len=:), allocatable :: line, name, pending_reason
        character(len=10) :: status
        character(len=10) :: exit_str
        real :: secs
        integer :: u, ios, iostat

        n_entries = 0
        pending_reason = ''
        ierr = 0
        if (.not. allocated(entries)) allocate (entries(MAX_TEST_RESULTS_ENTRIES))
        open (newunit=u, file=trim(log_file), status='old', iostat=ios)
        if (ios /= 0) then
            ierr = 1
            return
        end if
        do
            call read_logical_line(u, line, ios)
            if (ios == iostat_end) exit
            if (ios /= 0) then
                ierr = 1
                close (u)
                return
            end if
            if (index(line, 'TEST_RESULT ') == 1) then
                call parse_test_result_line(line, name, status, exit_str, secs, iostat)
                if (iostat /= 0) then
                    ierr = 1
                    close (u)
                    return
                end if
                if (n_entries >= size(entries)) call grow_entries(entries)
                n_entries = n_entries + 1
                entries(n_entries)%name = name
                entries(n_entries)%reason = ''
                entries(n_entries)%status = status
                entries(n_entries)%seconds = secs
                entries(n_entries)%exit_code = 0
                if (trim(exit_str) /= '-') then
                    read (exit_str, *, iostat=iostat) entries(n_entries)%exit_code
                    if (iostat /= 0) entries(n_entries)%exit_code = 1
                end if
            else
                call parse_ctest_result_line(line, name, status, secs, iostat)
                if (iostat == 0) then
                    if (n_entries >= size(entries)) call grow_entries(entries)
                    n_entries = n_entries + 1
                    entries(n_entries)%name = name
                    entries(n_entries)%reason = ''
                    if (status == 'UNTESTED') then
                        entries(n_entries)%reason = pending_reason
                        if (len(pending_reason) == 0) entries(n_entries)%reason = &
                            'CTest did not run this test'
                    else if (status == 'SKIP' .and. &
                        index(line, 'Not Run (Disabled)', back=.true.) > &
                        index(line, '...', back=.true.)) then
                        entries(n_entries)%reason = 'CTest test is disabled'
                    end if
                    pending_reason = ''
                    entries(n_entries)%status = status
                    entries(n_entries)%seconds = secs
                    entries(n_entries)%exit_code = 0
                    if (trim(status) == 'FAIL' .or. &
                        trim(status) == 'TIMEOUT') then
                        entries(n_entries)%exit_code = 1
                    end if
                else if (index(line, 'Unable to find executable:') > 0 .or. &
                    index(line, 'Could not find executable ') > 0) then
                    pending_reason = line
                else if (looks_like_ctest_result_line(line)) then
                    ierr = 1
                    close (u)
                    return
                end if
            end if
        end do
        close (u)
    end subroutine parse_test_results

    subroutine grow_entries(entries)
        !! Double the entry buffer so no result is ever dropped.
        type(test_result_entry_t), allocatable, intent(inout) :: entries(:)

        type(test_result_entry_t), allocatable :: bigger(:)

        allocate (bigger(max(2*size(entries), MAX_TEST_RESULTS_ENTRIES)))
        bigger(1:size(entries)) = entries
        call move_alloc(bigger, entries)
    end subroutine grow_entries

    subroutine read_logical_line(unit, line, iostat)
        integer, intent(in) :: unit
        character(:), allocatable, intent(out) :: line
        integer, intent(out) :: iostat

        character(len=1024) :: chunk
        integer :: count

        line = ''
        do
            count = 0
            read (unit, '(a)', advance='no', size=count, iostat=iostat) chunk
            if (count > 0) line = line//chunk(:count)
            if (iostat == 0) cycle
            if (iostat == iostat_eor) then
                iostat = 0
            else if (iostat == iostat_end .and. len(line) > 0) then
                iostat = 0
            end if
            return
        end do
    end subroutine read_logical_line

    subroutine parse_test_result_line(line, name, status, exit_str, secs, iostat)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: name
        character(len=*), intent(out) :: status, exit_str
        real, intent(out) :: secs
        integer, intent(out) :: iostat

        character(len=:), allocatable :: name_local
        character(len=:), allocatable :: status_local, exit_local, secs_str
        integer :: exit_value

        name = ''
        status = ''
        exit_str = ''
        secs = 0.0
        iostat = 0

        call parse_test_result_fields(line, name_local, status_local, &
            exit_local, secs_str, iostat)
        if (iostat /= 0) return
        select case (status_local)
        case ('PASS', 'FAIL', 'TIMEOUT', 'SKIP', 'FLAKY')
        case default
            iostat = 1
            return
        end select
        if (exit_local /= '-') then
            read (exit_local, *, iostat=iostat) exit_value
            if (iostat /= 0) return
        end if
        read (secs_str, *, iostat=iostat) secs
        if (iostat /= 0) return
        if (.not. ieee_is_finite(secs) .or. secs < 0.0) then
            iostat = 1
            return
        end if
        name = name_local
        status = status_local
        exit_str = exit_local
    end subroutine parse_test_result_line

    subroutine parse_ctest_result_line(line, name, status, secs, iostat)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: name
        character(len=*), intent(out) :: status
        real, intent(out) :: secs
        integer, intent(out) :: iostat

        character(len=:), allocatable :: tail, timing
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
        if (status_pos + status_width > len(tail)) return
        timing = trim(adjustl(tail(status_pos + status_width:)))
        if (len(timing) == 0) return
        if (timing(1:1) == ':') then
            if (len(timing) == 1) return
            timing = trim(adjustl(timing(2:)))
        end if
        if (len(timing) == 0) return
        if (index(timing, '(Disabled)') == 1) then
            if (len(timing) <= len('(Disabled)')) return
            timing = trim(adjustl(timing(len('(Disabled)') + 1:)))
        end if
        read (timing, *, iostat=time_iostat) secs
        if (time_iostat /= 0) return
        if (.not. ieee_is_finite(secs) .or. secs < 0.0) return
        iostat = 0
    end subroutine parse_ctest_result_line

    logical function looks_like_ctest_result_line(line) result(is_result)
        character(len=*), intent(in) :: line

        character(len=:), allocatable :: prefix
        integer :: test_pos, slash_pos

        is_result = .false.
        test_pos = index(line, ' Test')
        if (test_pos <= 1) return
        prefix = trim(adjustl(line(:test_pos - 1)))
        slash_pos = index(prefix, '/')
        if (slash_pos <= 1 .or. slash_pos >= len(prefix)) return
        if (verify(prefix(:slash_pos - 1), '0123456789') /= 0) return
        if (verify(prefix(slash_pos + 1:), '0123456789') /= 0) return
        is_result = .true.
    end function looks_like_ctest_result_line

    subroutine find_ctest_status(text, status, pos, width)
        character(len=*), intent(in) :: text
        character(len=*), intent(out) :: status
        integer, intent(out) :: pos, width

        character(len=:), allocatable :: status_text
        integer :: padding_pos, local_pos, local_width

        status = ''
        pos = 0
        width = 0
        padding_pos = index(text, '...', back=.true.)
        if (padding_pos == 0) return
        if (padding_pos + 3 > len(text)) return
        status_text = text(padding_pos + 3:)

        local_pos = index(status_text, '***Failed')
        local_width = len('***Failed')
        if (local_pos > 0) then
            status = 'FAIL'
            pos = padding_pos + 2 + local_pos
            width = local_width
            return
        end if
        local_pos = index(status_text, '***Exception')
        local_width = len('***Exception')
        if (local_pos > 0) then
            status = 'FAIL'
            pos = padding_pos + 2 + local_pos
            width = local_width
            return
        end if
        local_pos = index(status_text, '***Timeout')
        local_width = len('***Timeout')
        if (local_pos > 0) then
            status = 'TIMEOUT'
            pos = padding_pos + 2 + local_pos
            width = local_width
            return
        end if
        local_pos = index(status_text, 'Passed')
        local_width = len('Passed')
        if (local_pos > 0) then
            status = 'PASS'
            pos = padding_pos + 2 + local_pos
            width = local_width
            return
        end if
        local_pos = index(status_text, 'Not Run')
        local_width = len('Not Run')
        if (local_pos > 0) then
            status = 'UNTESTED'
            if (index(status_text, '(Disabled)') > 0) status = 'SKIP'
            pos = padding_pos + 2 + local_pos
            width = local_width
            return
        end if
        local_pos = index(status_text, 'Skipped')
        local_width = len('Skipped')
        if (local_pos > 0) then
            status = 'SKIP'
            pos = padding_pos + 2 + local_pos
            width = local_width
        end if
    end subroutine find_ctest_status

    subroutine extract_ctest_name(text, name)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: name

        character(len=:), allocatable :: local
        integer :: padding

        local = adjustl(text)
        padding = index(local, '...')
        if (padding > 0) local = local(:padding - 1)
        name = trim(local)
    end subroutine extract_ctest_name

    subroutine parse_test_result_fields(line, name, status, exit_str, &
            secs_str, iostat)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable, intent(out) :: status, exit_str, secs_str
        integer, intent(out) :: iostat

        character(len=:), allocatable :: name_local, status_local, exit_local
        character(len=:), allocatable :: secs_local, extra

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
        call extract_word(line, 6, extra)

        if (len_trim(name_local) == 0 .or. len_trim(status_local) == 0 .or. &
            len_trim(exit_local) == 0 .or. len_trim(secs_local) == 0 .or. &
            len(extra) /= 0) then
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
        character(len=:), allocatable, intent(out) :: word

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

    subroutine format_test_results_text(entries, n_entries, log_file, &
            summary_mode, output)
        !! Whole human report. A bounded buffer would drop late failures and the
        !! summary once diagnostics pass its length, so the result is allocatable.
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries
        character(len=*), intent(in) :: log_file
        logical, intent(in) :: summary_mode
        character(len=:), allocatable, intent(out) :: output

        integer :: i, n_pass, n_fail, n_skip, n_untested, n_shown
        real(real64) :: total_secs
        character(len=:), allocatable :: line, captured

        n_pass = 0
        n_fail = 0
        n_skip = 0
        n_untested = 0
        n_shown = 0
        total_secs = 0.0

        do i = 1, n_entries
            total_secs = total_secs + real(entries(i)%seconds, real64)
            if (trim(entries(i)%status) == 'PASS' .or. &
                trim(entries(i)%status) == 'FLAKY') then
                n_pass = n_pass + 1
            else if (trim(entries(i)%status) == 'FAIL' .or. &
                    trim(entries(i)%status) == 'TIMEOUT') then
                n_fail = n_fail + 1
            else if (entries(i)%status == 'UNTESTED') then
                n_untested = n_untested + 1
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
            if (allocated(entries(i)%reason)) then
                if (len(entries(i)%reason) > 0) &
                    output = output//'  '//entries(i)%reason//achar(10)
            end if
            if (trim(entries(i)%status) == 'FLAKY') then
                output = trim(output)//'  attempts: first failed, retry passed'//achar(10)
            end if
            ! A timed-out test's output ends with the line naming the limit
            ! that killed it (CPU budget or wall-clock cap).
            if (trim(entries(i)%status) == 'FAIL' .or. &
                trim(entries(i)%status) == 'TIMEOUT' .or. &
                trim(entries(i)%status) == 'FLAKY') then
                call extract_captured_stdout(log_file, entries(i)%name, captured)
                if (len(captured) > 0) then
                    output = trim(output)//'  --- captured stdout ---'//achar(10)
                    output = output//captured//achar(10)
                    output = trim(output)//'  --- end captured stdout ---'//achar(10)
                end if
            end if
        end do

        if (n_entries == n_shown) then
            call format_summary_line(output, n_pass, n_fail, n_skip, &
                n_untested, total_secs)
        else if (n_fail > 0 .or. n_shown > 0) then
            call format_summary_line(output, n_pass, n_fail, n_skip, &
                n_untested, total_secs)
        else if (n_shown == 0) then
            output = 'Tests: '//trim(json_int(n_pass))//' passed ('// &
                format_seconds(total_secs)//'s)'
        end if
    end subroutine format_test_results_text

    subroutine format_single_test_entry(entry, line)
        type(test_result_entry_t), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: line

        line = format_name_field(entry%name)//' '// &
            format_status_field(entry%status)//' '// &
            format_seconds(real(entry%seconds, real64))//'s'
    end subroutine format_single_test_entry

    function format_name_field(name) result(field)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: field
        integer :: nlen

        nlen = len_trim(name)
        field = trim(name)//repeat(' ', max(0, 42 - nlen))
    end function format_name_field

    function format_status_field(status) result(field)
        character(len=*), intent(in) :: status
        character(len=8) :: field
        integer :: nlen

        nlen = min(len_trim(status), 8)
        field = trim(status(1:nlen))//repeat(' ', 8 - nlen)
    end function format_status_field

    subroutine format_summary_line(output, n_pass, n_fail, n_skip, &
            n_untested, total_secs)
        character(len=:), allocatable, intent(inout) :: output
        integer, intent(in) :: n_pass, n_fail, n_skip, n_untested
        real(real64), intent(in) :: total_secs

        character(len=512) :: line

        write (line, '(a,i0,a,i0,a,i0,a)') &
            'Summary: ', n_pass, ' passed, ', n_fail, ' failed, ', n_skip, ' skipped'
        if (n_untested > 0) line = trim(line)//', '// &
            trim(json_int(n_untested))//' untested'
        output = output//trim(line)//' ('//format_seconds(total_secs)//'s)'
    end subroutine format_summary_line

    subroutine extract_captured_stdout(log_file, test_name, captured)
        !! Return the stdout the build side wrote for test_name, wrapped in
        !! name-keyed delimiters. The block may appear anywhere in the master
        !! log (TEST_RESULT summary lines are emitted separately, at the end),
        !! so scan the whole file rather than reading forward from a marker.
        character(len=*), intent(in) :: log_file, test_name
        character(len=:), allocatable, intent(out) :: captured

        character(len=:), allocatable :: line
        character(len=:), allocatable :: start_marker, end_marker
        integer :: u, ios
        logical :: in_block, ctest_block
        character(len=:), allocatable :: result_name, stripped
        character(len=10) :: result_status
        real :: seconds
        integer :: parse_error

        captured = ''
        start_marker = '--- stdout '//trim(test_name)//' ---'
        end_marker = '--- end stdout '//trim(test_name)//' ---'
        in_block = .false.
        ctest_block = .false.

        open (newunit=u, file=trim(log_file), status='old', iostat=ios)
        if (ios /= 0) return
        do
            call read_logical_line(u, line, ios)
            if (ios /= 0) exit
            stripped = trim(adjustl(line))
            if (.not. in_block) then
                if (trim(line) == trim(start_marker)) then
                    in_block = .true.
                else
                    call parse_ctest_result_line(line, result_name, result_status, &
                        seconds, parse_error)
                    if (parse_error == 0) then
                        if (result_name == test_name) then
                            in_block = .true.
                            ctest_block = .true.
                        end if
                    end if
                end if
                cycle
            end if
            if (ctest_block) then
                if (looks_like_ctest_result_line(line)) exit
                if (is_ctest_start_line(line)) exit
                if (is_ctest_summary_line(stripped)) exit
            else
                if (trim(line) == trim(end_marker)) exit
            end if
            if (len(captured) > 0) then
                captured = captured//achar(10)//'  '//line
            else
                captured = '  '//line
            end if
        end do
        close (u)
    end subroutine extract_captured_stdout

    logical function is_ctest_start_line(line) result(is_start)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: stripped, number
        integer :: colon

        is_start = .false.
        if (len(line) < 12) return
        if (line(:4) /= '    ') return
        stripped = trim(adjustl(line))
        if (index(stripped, 'Start ') /= 1) return
        colon = index(stripped, ':')
        if (colon <= 7) return
        number = trim(adjustl(stripped(7:colon - 1)))
        if (len(number) == 0) return
        is_start = verify(number, '0123456789') == 0
    end function is_ctest_start_line

    logical function is_ctest_summary_line(line) result(is_summary)
        character(len=*), intent(in) :: line
        integer :: percent

        is_summary = .false.
        percent = index(line, '% tests passed,')
        if (percent <= 1) return
        is_summary = verify(line(:percent - 1), '0123456789') == 0
    end function is_ctest_summary_line

    subroutine format_test_results_json(entries, n_entries, exit_code, output, log_file)
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries
        integer, intent(in) :: exit_code
        character(len=:), allocatable, intent(out) :: output
        character(len=*), optional, intent(in) :: log_file
        character(len=:), allocatable :: captured

        integer :: i, n_pass, n_fail, n_skip, n_untested, read_error
        real(real64) :: total_secs

        n_pass = 0
        n_fail = 0
        n_skip = 0
        n_untested = 0
        total_secs = 0.0

        do i = 1, n_entries
            total_secs = total_secs + real(entries(i)%seconds, real64)
            if (trim(entries(i)%status) == 'PASS' .or. &
                trim(entries(i)%status) == 'FLAKY') then
                n_pass = n_pass + 1
            else if (trim(entries(i)%status) == 'FAIL' .or. &
                    trim(entries(i)%status) == 'TIMEOUT') then
                n_fail = n_fail + 1
            else if (entries(i)%status == 'UNTESTED') then
                n_untested = n_untested + 1
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
            output = trim(output)//format_seconds(real(entries(i)%seconds, real64))
            if (present(log_file)) then
                call extract_captured_stdout(log_file, entries(i)%name, captured)
                output = output//',"output":"'//json_escape_string(captured)//'"'
            end if
            if (allocated(entries(i)%reason)) then
                if (len(entries(i)%reason) > 0) output = output//',"reason":"'// &
                    json_escape_string(entries(i)%reason)//'"'
            end if
            if (trim(entries(i)%status) == 'FLAKY') then
                output = output//',"attempts":{"first":"fail",'// &
                    '"retry":"pass"}'
            end if
            output = trim(output)//'}'
        end do
        output = trim(output)//'],'
        output = trim(output)//'"summary":{'
        output = trim(output)//'"passed":'//trim(json_int(n_pass))
        output = trim(output)//',"failed":'//trim(json_int(n_fail))
        output = trim(output)//',"skipped":'//trim(json_int(n_skip))
        if (n_untested > 0) output = output//',"untested":'//trim(json_int(n_untested))
        output = trim(output)//',"total_seconds":'//format_seconds(total_secs)
        output = trim(output)//'}'
        output = trim(output)//',"exit_code":'//trim(json_int(exit_code))
        if (n_entries == 0 .and. exit_code /= 0 .and. present(log_file)) then
            call read_text_file_alloc(log_file, captured, read_error)
            if (read_error == 0) then
                output = output//',"output":"'//json_escape_string(captured)//'"'
            else
                output = output//',"error":"could not read test diagnostics"'
            end if
        end if
        output = output//'}'
    end subroutine format_test_results_json

    function format_seconds(seconds) result(text)
        real(real64), intent(in) :: seconds
        character(len=:), allocatable :: text
        character(len=64) :: buffer

        if (seconds == 0.0_real64) then
            text = '0.00'
            return
        end if
        write (buffer, '(f0.2)') seconds
        text = trim(buffer)
        if (text(:1) == '.') text = '0'//text
    end function format_seconds

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
