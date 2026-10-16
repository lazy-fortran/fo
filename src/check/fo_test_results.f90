module fo_test_results
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fo_util, only: json_int, read_text_file_alloc, temporary_root
    use fx_json_build, only: json_escape_string
    implicit none
    private
    public :: test_result_entry_t, MAX_TEST_RESULTS_ENTRIES
    public :: parse_test_results, format_test_results_json
    public :: format_test_results_text, read_logical_line
    public :: extract_captured_stdout

    !! Initial capacity only. The entry buffer grows on demand, so a suite with
    !! more tests than this still reports every result: truncating the list used
    !! to hide failures behind a passing-looking count.
    integer, parameter :: MAX_TEST_RESULTS_ENTRIES = 256

    !! Bounds of a failure excerpt in agent-facing output: the head and tail
    !! of a case's captured output, at most this many lines and bytes.
    integer, parameter :: EXCERPT_LINES = 20, EXCERPT_HEAD = 12
    integer, parameter :: EXCERPT_BYTES = 2048
    !! Failing cases beyond this many list name, status and time only.
    integer, parameter :: MAX_EXCERPT_CASES = 10

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

        character(len=:), allocatable :: line, name, pending_reason, continuation
        character(len=10) :: status
        character(len=10) :: exit_str
        real :: secs
        integer :: u, ios, iostat, n_continuations, n_console
        character(len=:), allocatable :: native_reason

        n_entries = 0
        pending_reason = ''
        native_reason = ''
        n_console = 0
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
            if (index(line, 'TEST_REASON ') == 1) then
                native_reason = line(13:)
            else if (line == 'TEST_JUNIT_IMPORT') then
                ! The verdicts of the console block just read follow exactly.
                n_entries = n_entries - n_console
                n_console = 0
            else if (index(line, 'TEST_RESULT ') == 1) then
                call parse_test_result_line(line, name, status, exit_str, secs, iostat)
                if (iostat /= 0) then
                    ierr = 1
                    close (u)
                    return
                end if
                if (n_entries >= size(entries)) call grow_entries(entries)
                n_entries = n_entries + 1
                entries(n_entries)%name = name
                entries(n_entries)%reason = native_reason
                native_reason = ''
                n_console = 0
                entries(n_entries)%status = status
                entries(n_entries)%seconds = secs
                entries(n_entries)%exit_code = 0
                if (trim(exit_str) /= '-') then
                    read (exit_str, *, iostat=iostat) entries(n_entries)%exit_code
                    if (iostat /= 0) entries(n_entries)%exit_code = 1
                end if
            else
                call parse_ctest_result_line(line, name, status, secs, iostat)
                ! CTest puts the closing regex bracket and duration on a
                ! following line when a required expression was not found.
                n_continuations = 0
                do while (iostat /= 0 .and. len_trim(status) > 0 .and. &
                          index(line, 'Regex=[') > 0)
                    call read_logical_line(u, continuation, ios)
                    if (ios /= 0) exit
                    if (looks_like_ctest_result_line(continuation) .or. &
                        index(continuation, 'TEST_RESULT ') == 1) exit
                    line = line//' '//continuation
                    n_continuations = n_continuations + 1
                    if (n_continuations > 4096) exit
                    call parse_ctest_result_line(line, name, status, secs, iostat)
                end do
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
                    n_console = n_console + 1
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
        case ('PASS', 'FAIL', 'TIMEOUT', 'SKIP', 'FLAKY', 'UNTESTED', 'REUSED')
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
        integer :: time_iostat, seconds_end, seconds_start

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
        ! Reasons such as "SegFault" or regex diagnostics precede the
        ! duration. Parse the final numeric field before CTest's unit.
        seconds_end = index(timing, ' sec', back=.true.) - 1
        if (seconds_end < 1) return
        do while (seconds_end > 0)
            if (timing(seconds_end:seconds_end) /= ' ') exit
            seconds_end = seconds_end - 1
        end do
        seconds_start = seconds_end
        do while (seconds_start > 1)
            if (timing(seconds_start - 1:seconds_start - 1) == ' ') exit
            seconds_start = seconds_start - 1
        end do
        if (seconds_end < seconds_start) return
        read (timing(seconds_start:seconds_end), *, iostat=time_iostat) secs
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
        integer :: last_char

        ! The name is followed by a space and a run of padding dots; a name may
        ! itself contain or end in dots.
        local = trim(adjustl(text))
        last_char = len(local)
        do while (last_char > 0)
            if (local(last_char:last_char) /= '.') exit
            last_char = last_char - 1
        end do
        name = trim(local(:last_char))
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
            summary_mode, output, bounded)
        !! Whole human report, ending in the verdict line. A bounded buffer
        !! would drop late failures and the verdict once diagnostics pass its
        !! length, so the result is allocatable. With bounded, a failing case's
        !! captured output is cut to a head/tail excerpt and the full log is
        !! kept and named.
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries
        character(len=*), intent(in) :: log_file
        logical, intent(in) :: summary_mode
        character(len=:), allocatable, intent(out) :: output
        logical, optional, intent(in) :: bounded

        integer :: i, n_pass, n_fail, n_skip, n_untested, n_flaky
        real(real64) :: total_secs
        character(len=:), allocatable :: line, captured, excerpt, source_log, handle
        logical :: cut, limit_output

        limit_output = .false.
        if (present(bounded)) limit_output = bounded
        call count_statuses(entries, n_entries, n_pass, n_fail, n_skip, &
            n_untested, n_flaky, total_secs)
        source_log = log_file
        handle = ''
        if (limit_output .and. (n_fail > 0 .or. n_flaky > 0)) then
            handle = retain_log(log_file)
            if (len(handle) > 0) source_log = handle
        end if

        output = ''
        do i = 1, n_entries
            if (summary_mode .and. (trim(entries(i)%status) == 'PASS' .or. &
                trim(entries(i)%status) == 'REUSED')) cycle
            call format_single_test_entry(entries(i), line)
            output = output//trim(line)//achar(10)
            if (allocated(entries(i)%reason)) then
                if (len(entries(i)%reason) > 0) &
                    output = output//'  '//entries(i)%reason//achar(10)
            end if
            if (trim(entries(i)%status) == 'FLAKY') then
                output = output//'  attempts: first failed, retry passed'//achar(10)
            end if
            ! A timed-out test's output ends with the line naming the limit
            ! that killed it (CPU budget or wall-clock cap).
            if (trim(entries(i)%status) == 'FAIL' .or. &
                trim(entries(i)%status) == 'TIMEOUT' .or. &
                trim(entries(i)%status) == 'FLAKY') then
                call extract_captured_stdout(source_log, entries(i)%name, captured)
                if (len(captured) > 0) then
                    cut = .false.
                    if (limit_output) then
                        call bounded_excerpt(captured, excerpt, cut)
                        captured = excerpt
                    end if
                    output = output//'  --- captured stdout ---'//achar(10)
                    output = output//captured//achar(10)
                    if (cut) output = output//'  --- excerpt only; full output in '// &
                        'the log below ---'//achar(10)
                    output = output//'  --- end captured stdout ---'//achar(10)
                end if
            end if
        end do
        if (len(handle) > 0) output = output//'log: '//handle//achar(10)
        output = output//verdict_line(n_pass, n_fail, n_skip, n_untested, &
            n_flaky, read_wall_seconds(log_file), total_secs, &
            count_reused(entries, n_entries))
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

    subroutine format_test_results_json(entries, n_entries, exit_code, output, &
            log_file, compact)
        !! Legacy full report by default: every case with its complete output.
        !! With compact, the one report shared by `fo test --json` and the MCP
        !! test tool: verdict first, passing cases counted not listed, bounded
        !! failure excerpts and a handle to the retained full log.
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries
        integer, intent(in) :: exit_code
        character(len=:), allocatable, intent(out) :: output
        character(len=*), optional, intent(in) :: log_file
        logical, optional, intent(in) :: compact
        character(len=:), allocatable :: captured

        integer :: i, n_pass, n_fail, n_skip, n_untested, n_flaky, read_error
        real(real64) :: total_secs

        if (present(compact)) then
            if (compact) then
                call format_compact_json(entries, n_entries, exit_code, output, &
                    log_file)
                return
            end if
        end if
        call count_statuses(entries, n_entries, n_pass, n_fail, n_skip, &
            n_untested, n_flaky, total_secs)
        n_pass = n_pass + n_flaky

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
        if (count_reused(entries, n_entries) > 0) output = output//',"reused":'// &
            trim(json_int(count_reused(entries, n_entries)))
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

    subroutine format_compact_json(entries, n_entries, exit_code, output, log_file)
        !! Keys: v verdict line, ok, pass/fail/skip (untested, flaky when
        !! nonzero), wall_s or sum_s (the run's wall clock, or the sum of the
        !! per-case times when the wall clock is unknown), exit, log (retained
        !! full log, only when something failed), cases (every non-passing case:
        !! n name, st status, s seconds, at file:line, msg first diagnostic line,
        !! out bounded excerpt, cut excerpt was shortened, why reason), and,
        !! when no case ran, at/out/cut for the bounded log excerpt.
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries, exit_code
        character(len=:), allocatable, intent(out) :: output
        character(len=*), optional, intent(in) :: log_file

        integer :: i, n_pass, n_fail, n_skip, n_untested, n_flaky, read_error
        integer :: n_listed
        real(real64) :: total_secs, wall
        character(len=:), allocatable :: handle, captured, excerpt, at, msg, log_text
        character(len=:), allocatable :: source_log
        logical :: cut, failing

        call count_statuses(entries, n_entries, n_pass, n_fail, n_skip, &
            n_untested, n_flaky, total_secs)
        failing = n_fail > 0 .or. n_flaky > 0 .or. exit_code /= 0
        wall = -1.0_real64
        handle = ''
        source_log = ''
        if (present(log_file)) then
            source_log = log_file
            wall = read_wall_seconds(log_file)
            if (failing) handle = retain_log(log_file)
            if (len(handle) > 0) source_log = handle
        end if
        output = '{"v":"'//verdict_line(n_pass, n_fail, n_skip, n_untested, &
            n_flaky, wall, total_secs, count_reused(entries, n_entries))//'"'
        if (exit_code == 0 .and. n_fail == 0) then
            output = output//',"ok":true'
        else
            output = output//',"ok":false'
        end if
        output = output//',"pass":'//trim(json_int(n_pass))
        output = output//',"fail":'//trim(json_int(n_fail))
        output = output//',"skip":'//trim(json_int(n_skip))
        if (n_untested > 0) output = output//',"untested":'//trim(json_int(n_untested))
        if (n_flaky > 0) output = output//',"flaky":'//trim(json_int(n_flaky))
        if (count_reused(entries, n_entries) > 0) output = output//',"reused":'// &
            trim(json_int(count_reused(entries, n_entries)))
        if (wall >= 0.0_real64) then
            output = output//',"wall_s":'//format_seconds(wall)
        else
            output = output//',"sum_s":'//format_seconds(total_secs)
        end if
        output = output//',"exit":'//trim(json_int(exit_code))
        if (len(handle) > 0) output = output//',"log":"'// &
            json_escape_string(handle)//'"'
        if (n_entries > n_pass + count_reused(entries, n_entries)) then
            output = output//',"cases":['
            n_listed = 0
            do i = 1, n_entries
                if (trim(entries(i)%status) == 'PASS' .or. &
                    trim(entries(i)%status) == 'REUSED') cycle
                if (n_listed > 0) output = output//','
                n_listed = n_listed + 1
                output = output//'{"n":"'//json_escape_string(entries(i)%name)// &
                    '","st":"'//lower_status(entries(i)%status)//'","s":'// &
                    format_seconds(real(entries(i)%seconds, real64))
                if (allocated(entries(i)%reason)) then
                    if (len(entries(i)%reason) > 0) output = output//',"why":"'// &
                        json_escape_string(entries(i)%reason)//'"'
                end if
                select case (trim(entries(i)%status))
                case ('FAIL', 'TIMEOUT', 'FLAKY')
                    captured = ''
                    if (len(source_log) > 0 .and. n_listed <= MAX_EXCERPT_CASES) &
                        call extract_captured_stdout(source_log, entries(i)%name, &
                        captured)
                    if (len(captured) > 0) then
                        call find_location(captured, at, msg)
                        if (len(at) > 0) output = output//',"at":"'// &
                            json_escape_string(at)//'"'
                        if (len(msg) > 0) output = output//',"msg":"'// &
                            json_escape_string(msg)//'"'
                        call bounded_excerpt(captured, excerpt, cut)
                        output = output//',"out":"'//json_escape_string(dedent(excerpt))//'"'
                        if (cut) output = output//',"cut":true'
                    end if
                end select
                output = output//'}'
            end do
            output = output//']'
        end if
        if (n_entries == 0 .and. exit_code /= 0 .and. len(source_log) > 0) then
            call read_text_file_alloc(source_log, log_text, read_error)
            if (read_error == 0) then
                call find_location(log_text, at, msg)
                if (len(at) > 0) output = output//',"at":"'//json_escape_string(at)//'"'
                call bounded_excerpt(log_text, excerpt, cut)
                output = output//',"out":"'//json_escape_string(excerpt)//'"'
                if (cut) output = output//',"cut":true'
            else
                output = output//',"error":"could not read test diagnostics"'
            end if
        end if
        output = output//'}'
    end subroutine format_compact_json

    subroutine count_statuses(entries, n_entries, n_pass, n_fail, n_skip, &
            n_untested, n_flaky, total_secs)
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries
        integer, intent(out) :: n_pass, n_fail, n_skip, n_untested, n_flaky
        real(real64), intent(out) :: total_secs

        integer :: i

        n_pass = 0
        n_fail = 0
        n_skip = 0
        n_untested = 0
        n_flaky = 0
        total_secs = 0.0_real64
        do i = 1, n_entries
            total_secs = total_secs + real(entries(i)%seconds, real64)
            select case (trim(entries(i)%status))
            case ('PASS')
                n_pass = n_pass + 1
            case ('REUSED')
                ! Counted by count_reused: neither run nor skipped.
            case ('FLAKY')
                n_flaky = n_flaky + 1
            case ('FAIL', 'TIMEOUT')
                n_fail = n_fail + 1
            case ('UNTESTED')
                n_untested = n_untested + 1
            case default
                n_skip = n_skip + 1
            end select
        end do
    end subroutine count_statuses

    integer function count_reused(entries, n_entries) result(n)
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries
        integer :: i

        n = 0
        do i = 1, n_entries
            if (trim(entries(i)%status) == 'REUSED') n = n + 1
        end do
    end function count_reused

    function verdict_line(n_pass, n_fail, n_skip, n_untested, n_flaky, wall, &
            total_secs, n_reused) result(line)
        !! `12 pass, 1 FAIL, 2 skip (4.10s wall)`. Zero counts other than pass
        !! are omitted. The time is the run's wall clock when known, otherwise
        !! the sum of the per-case times, and says which.
        integer, intent(in) :: n_pass, n_fail, n_skip, n_untested, n_flaky
        real(real64), intent(in) :: wall, total_secs
        !! Cases not run: their PASS at an unchanged input closure was reused.
        integer, intent(in), optional :: n_reused
        character(len=:), allocatable :: line

        line = trim(json_int(n_pass))//' pass'
        if (present(n_reused)) then
            if (n_reused > 0) line = line//', '//trim(json_int(n_reused))//' reused'
        end if
        if (n_fail > 0) line = line//', '//trim(json_int(n_fail))//' FAIL'
        if (n_flaky > 0) line = line//', '//trim(json_int(n_flaky))//' flaky'
        if (n_skip > 0) line = line//', '//trim(json_int(n_skip))//' skip'
        if (n_untested > 0) line = line//', '//trim(json_int(n_untested))// &
            ' untested'
        if (wall >= 0.0_real64) then
            line = line//' ('//format_seconds(wall)//'s wall)'
        else
            line = line//' ('//format_seconds(total_secs)//'s summed)'
        end if
    end function verdict_line

    function read_wall_seconds(log_file) result(wall)
        !! The last `TEST_WALL <seconds>` record of the log, or -1 when the
        !! runner recorded none (the CTest route has no run-level clock).
        character(len=*), intent(in) :: log_file
        real(real64) :: wall

        character(len=:), allocatable :: line
        integer :: u, ios, read_ios
        real :: value

        wall = -1.0_real64
        open (newunit=u, file=trim(log_file), status='old', iostat=ios)
        if (ios /= 0) return
        do
            call read_logical_line(u, line, ios)
            if (ios /= 0) exit
            if (index(line, 'TEST_WALL ') /= 1) cycle
            read (line(11:), *, iostat=read_ios) value
            if (read_ios == 0) then
                if (ieee_is_finite(value) .and. value >= 0.0) &
                    wall = real(value, real64)
            end if
        end do
        close (u)
    end function read_wall_seconds

    function retain_log(log_file) result(handle)
        !! Copy the run's full log to `fo-test-last.log` in the temporary root,
        !! so the report can name a path that outlives the run. One file, the
        !! last failing run wins. Empty when the copy fails.
        character(len=*), intent(in) :: log_file
        character(len=:), allocatable :: handle

        character(len=:), allocatable :: text
        integer :: ierr, u, ios

        handle = ''
        call read_text_file_alloc(log_file, text, ierr)
        if (ierr /= 0) return
        handle = temporary_root()//'/fo-test-last.log'
        open (newunit=u, file=handle, status='replace', action='write', &
            access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            handle = ''
            return
        end if
        write (u, iostat=ios) text
        close (u)
        if (ios /= 0) handle = ''
    end function retain_log

    subroutine find_location(text, at, msg)
        !! The first diagnostic line carrying a source location: a `file.ext:N`
        !! token or gfortran's `At line N of file F`. msg is that line,
        !! trimmed and clamped; with no location it is the first nonblank line.
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: at, msg

        integer :: pos, nl, stop_at
        character(len=:), allocatable :: line

        at = ''
        msg = ''
        pos = 1
        do while (pos <= len(text))
            nl = index(text(pos:), achar(10))
            if (nl == 0) then
                stop_at = len(text)
            else
                stop_at = pos + nl - 2
            end if
            line = trim(adjustl(text(pos:stop_at)))
            if (len(line) > 0) then
                if (len(msg) == 0) msg = clamp_line(line)
                at = line_location(line)
                if (len(at) > 0) then
                    msg = clamp_line(line)
                    return
                end if
            end if
            if (nl == 0) exit
            pos = pos + nl
        end do
    end subroutine find_location

    function line_location(line) result(at)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: at

        integer :: p, q, r, s, of_pos

        at = ''
        ! gfortran runtime: "At line 12 of file src/x.f90 (Unit 6 ...)"
        p = index(line, 'At line ')
        of_pos = index(line, ' of file ')
        if (p > 0 .and. of_pos > p) then
            q = of_pos + len(' of file ')
            r = index(line(q:), ' ')
            if (r == 0) then
                r = len(line)
            else
                r = q + r - 2
            end if
            at = line(q:r)//':'//trim(adjustl(line(p + 8:of_pos - 1)))
            return
        end if
        ! "file.ext:N": a token with a dot, a colon and a digit after it.
        q = 1
        do
            p = index(line(q:), ':')
            if (p == 0) return
            p = q + p - 1
            q = p + 1
            if (p == 1 .or. p == len(line)) cycle
            if (index('0123456789', line(p + 1:p + 1)) == 0) cycle
            r = p + 1
            do while (r < len(line))
                if (index('0123456789', line(r + 1:r + 1)) == 0) exit
                r = r + 1
            end do
            s = p - 1
            do while (s > 1)
                if (index(' "''(,=<>', line(s - 1:s - 1)) > 0) exit
                s = s - 1
            end do
            if (index(line(s:p - 1), '.') == 0) cycle
            if (line(p - 1:p - 1) == '.') cycle
            at = line(s:r)
            return
        end do
    end function line_location

    function dedent(text) result(out)
        !! Drop the two-space prefix extract_captured_stdout puts on each line.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out

        integer :: pos, nl, stop_at

        out = ''
        pos = 1
        do while (pos <= len(text))
            nl = index(text(pos:), achar(10))
            if (nl == 0) then
                stop_at = len(text)
            else
                stop_at = pos + nl - 2
            end if
            if (pos + 1 <= stop_at .and. text(pos:pos + 1) == '  ') then
                out = out//text(pos + 2:stop_at)
            else
                out = out//text(pos:stop_at)
            end if
            if (nl == 0) exit
            out = out//achar(10)
            pos = pos + nl
        end do
    end function dedent

    function clamp_line(line) result(out)
        !! At most 240 bytes, cut on a UTF-8 character boundary.
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: out

        integer, parameter :: limit = 240

        if (len(line) <= limit) then
            out = line
        else
            out = line(:utf8_boundary(line, limit))//' ...'
        end if
    end function clamp_line

    integer function utf8_boundary(text, pos) result(last)
        !! Largest index <= pos that ends a complete UTF-8 character.
        character(len=*), intent(in) :: text
        integer, intent(in) :: pos

        last = min(pos, len(text))
        if (last >= len(text)) return
        do while (last > 0)
            if (iand(iachar(text(last + 1:last + 1)), 192) /= 128) exit
            last = last - 1
        end do
    end function utf8_boundary

    subroutine bounded_excerpt(text, excerpt, cut)
        !! At most EXCERPT_LINES lines and EXCERPT_BYTES bytes: the head and the
        !! tail of the text (a failure usually ends with the cause) with a
        !! marker for what was left out. cut tells the caller to point at the
        !! full log.
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: excerpt
        logical, intent(out) :: cut

        integer :: n_lines, pos, nl, stop_at, k, keep, head
        character(len=:), allocatable :: line

        n_lines = 0
        pos = 1
        do while (pos <= len(text))
            n_lines = n_lines + 1
            nl = index(text(pos:), achar(10))
            if (nl == 0) exit
            pos = pos + nl
        end do
        cut = .false.
        excerpt = ''
        head = n_lines
        if (n_lines > EXCERPT_LINES) head = EXCERPT_HEAD
        pos = 1
        k = 0
        do while (pos <= len(text))
            k = k + 1
            nl = index(text(pos:), achar(10))
            if (nl == 0) then
                stop_at = len(text)
            else
                stop_at = pos + nl - 2
            end if
            if (k <= head .or. k > n_lines - (EXCERPT_LINES - EXCERPT_HEAD)) then
                line = clamp_line(text(pos:stop_at))
                if (len(line) < stop_at - pos + 1) cut = .true.
                if (len(excerpt) > 0) excerpt = excerpt//achar(10)
                excerpt = excerpt//line
            else if (k == head + 1) then
                cut = .true.
                if (len(excerpt) > 0) excerpt = excerpt//achar(10)
                excerpt = excerpt//'  ... '//trim(json_int(n_lines - EXCERPT_LINES))// &
                    ' lines omitted ...'
            end if
            if (nl == 0) exit
            pos = pos + nl
        end do
        if (len(excerpt) > EXCERPT_BYTES) then
            cut = .true.
            keep = utf8_boundary(excerpt, EXCERPT_BYTES - 40)
            excerpt = excerpt(:keep)//achar(10)//'  ... truncated ...'
        end if
    end subroutine bounded_excerpt

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
