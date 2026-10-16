module fo_junit
    !! JUnit XML of a test run for the user's own CI: one testcase per result
    !! of the shared test results (PASS, reused, flaky, FAIL, TIMEOUT, skipped
    !! and untested cases alike). FAIL is a <failure>, TIMEOUT an <error>, SKIP
    !! and UNTESTED are <skipped>; a reused case passes and says so in its
    !! <system-out>. A flaky case passes with a <flakyFailure> (Surefire and
    !! nextest spelling) unless the run is strict (--strict-flaky), where it is a
    !! <failure> like the run's exit status.
    use, intrinsic :: iso_fortran_env, only: real64
    use fo_test_results, only: test_result_entry_t, extract_captured_stdout
    use fo_util, only: make_sibling_tmpfile, delete_tmpfile
    use fo_fs, only: fs_rename
    implicit none
    private

    !! Bound of the captured output kept in one failure element.
    integer, parameter :: MAX_OUTPUT_BYTES = 32768

    public :: junit_write, junit_xml, xml_escape

contains

    subroutine junit_write(path, entries, n_entries, log_file, ierr, strict_flaky)
        !! Write the report of entries(1:n_entries) to path atomically.
        !! log_file holds the captured output of failing cases.
        character(len=*), intent(in) :: path, log_file
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries
        integer, intent(out) :: ierr
        logical, intent(in), optional :: strict_flaky
        character(len=:), allocatable :: text
        character(len=4096) :: temporary
        integer :: unit, ios

        ierr = 1
        call junit_xml(entries, n_entries, log_file, text, strict_flaky)
        call make_sibling_tmpfile(path, temporary)
        open (newunit=unit, file=trim(temporary), status='new', action='write', &
            access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        write (unit, iostat=ios) text
        close (unit)
        if (ios == 0) ierr = fs_rename(trim(temporary), path)
        if (ierr /= 0) call delete_tmpfile(trim(temporary))
    end subroutine junit_write

    subroutine junit_xml(entries, n_entries, log_file, text, strict_flaky)
        type(test_result_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries
        character(len=*), intent(in) :: log_file
        character(len=:), allocatable, intent(out) :: text
        logical, intent(in), optional :: strict_flaky
        character(len=:), allocatable :: body, captured, reason
        character(len=*), parameter :: nl = achar(10)
        integer :: i, n_fail, n_error, n_skip
        real(real64) :: total
        logical :: strict

        strict = .false.
        if (present(strict_flaky)) strict = strict_flaky
        n_fail = 0
        n_error = 0
        n_skip = 0
        total = 0.0_real64
        body = ''
        do i = 1, n_entries
            total = total + real(entries(i)%seconds, real64)
            reason = ''
            if (allocated(entries(i)%reason)) reason = entries(i)%reason
            body = body//'    <testcase classname="fo" name="'// &
                xml_escape(entries(i)%name)//'" time="'// &
                seconds_text(real(entries(i)%seconds, real64))//'"'
            select case (trim(entries(i)%status))
            case ('FAIL', 'TIMEOUT')
                if (trim(entries(i)%status) == 'FAIL') then
                    n_fail = n_fail + 1
                else
                    n_error = n_error + 1
                end if
                if (len(reason) == 0) then
                    reason = trim(entries(i)%status)
                    if (entries(i)%exit_code /= 0) reason = reason//', exit code '// &
                        int_text(entries(i)%exit_code)
                end if
                call extract_captured_stdout(log_file, entries(i)%name, captured)
                if (len(captured) > MAX_OUTPUT_BYTES) captured = &
                    captured(1:MAX_OUTPUT_BYTES)//nl//'[output truncated]'
                body = body//'>'//nl//'      <'//element(entries(i)%status)// &
                    ' message="'//xml_escape(reason)//'" type="'// &
                    trim(entries(i)%status)//'">'//xml_escape(captured)//'</'// &
                    element(entries(i)%status)//'>'//nl//'    </testcase>'//nl
            case ('SKIP', 'UNTESTED')
                n_skip = n_skip + 1
                if (trim(entries(i)%status) == 'UNTESTED' .and. len(reason) == 0) &
                    reason = 'not run'
                body = body//'>'//nl//'      <skipped message="'// &
                    xml_escape(reason)//'"/>'//nl//'    </testcase>'//nl
            case ('REUSED')
                body = body//'>'//nl//'      <system-out>reused: the case passed '// &
                    'at an unchanged input closure; not run</system-out>'//nl// &
                    '    </testcase>'//nl
            case ('FLAKY')
                reason = 'flaky: failed first, passed on retry'
                call extract_captured_stdout(log_file, entries(i)%name, captured)
                if (len(captured) > MAX_OUTPUT_BYTES) captured = &
                    captured(1:MAX_OUTPUT_BYTES)//nl//'[output truncated]'
                if (strict) then
                    n_fail = n_fail + 1
                    body = body//'>'//nl//'      <failure message="'// &
                        xml_escape(reason//' (--strict-flaky)')// &
                        '" type="FLAKY">'//xml_escape(captured)//'</failure>'//nl// &
                        '    </testcase>'//nl
                else
                    body = body//'>'//nl//'      <flakyFailure message="'// &
                        xml_escape(reason)//'" type="FLAKY">'// &
                        xml_escape(captured)//'</flakyFailure>'//nl// &
                        '    </testcase>'//nl
                end if
            case default
                body = body//'/>'//nl
            end select
        end do
        text = '<?xml version="1.0" encoding="UTF-8"?>'//nl// &
            '<testsuites tests="'//int_text(n_entries)//'" failures="'// &
            int_text(n_fail)//'" errors="'//int_text(n_error)//'" skipped="'// &
            int_text(n_skip)//'" time="'//seconds_text(total)//'">'//nl// &
            '  <testsuite name="fo" tests="'//int_text(n_entries)// &
            '" failures="'//int_text(n_fail)//'" errors="'//int_text(n_error)// &
            '" skipped="'//int_text(n_skip)//'" time="'//seconds_text(total)// &
            '">'//nl//body//'  </testsuite>'//nl//'</testsuites>'//nl
    end subroutine junit_xml

    function element(status) result(name)
        character(len=*), intent(in) :: status
        character(len=:), allocatable :: name

        name = 'failure'
        if (trim(status) == 'TIMEOUT') name = 'error'
    end function element

    function xml_escape(text) result(out)
        !! Text for element content or a double-quoted attribute: the five
        !! XML specials escaped, and every character XML 1.0 forbids (control
        !! characters other than tab, line feed and carriage return, invalid
        !! UTF-8, U+FFFE and U+FFFF) replaced by '?'.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i, c, width

        out = ''
        i = 1
        do while (i <= len(text))
            c = iachar(text(i:i))
            if (c >= 128) then
                width = utf8_width(text, i)
                if (width == 0) then
                    out = out//'?'
                    i = i + 1
                else
                    out = out//text(i:i + width - 1)
                    i = i + width
                end if
                cycle
            end if
            select case (c)
            case (38)
                out = out//'&amp;'
            case (60)
                out = out//'&lt;'
            case (62)
                out = out//'&gt;'
            case (34)
                out = out//'&quot;'
            case (39)
                out = out//'&apos;'
            case (9, 10, 13)
                out = out//text(i:i)
            case (0:8, 11, 12, 14:31, 127)
                out = out//'?'
            case default
                out = out//text(i:i)
            end select
            i = i + 1
        end do
    end function xml_escape

    integer function utf8_width(text, i) result(width)
        !! Length of the well-formed UTF-8 sequence at text(i:), 0 when it is
        !! malformed, overlong, a surrogate, beyond U+10FFFF or U+FFFE/FFFF.
        character(len=*), intent(in) :: text
        integer, intent(in) :: i
        integer :: lead, k, code

        width = 0
        lead = iachar(text(i:i))
        if (lead >= 194 .and. lead <= 223) then
            width = 2
            code = iand(lead, 31)
        else if (lead >= 224 .and. lead <= 239) then
            width = 3
            code = iand(lead, 15)
        else if (lead >= 240 .and. lead <= 244) then
            width = 4
            code = iand(lead, 7)
        else
            return
        end if
        if (i + width - 1 > len(text)) then
            width = 0
            return
        end if
        do k = 1, width - 1
            if (iand(iachar(text(i + k:i + k)), 192) /= 128) then
                width = 0
                return
            end if
            code = ior(ishft(code, 6), iand(iachar(text(i + k:i + k)), 63))
        end do
        if ((width == 3 .and. code < 2048) .or. (width == 4 .and. code < 65536) &
            .or. code > 1114111 .or. (code >= 55296 .and. code <= 57343) .or. &
            code == 65534 .or. code == 65535) width = 0
    end function utf8_width

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=16) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function int_text

    function seconds_text(seconds) result(text)
        real(real64), intent(in) :: seconds
        character(len=:), allocatable :: text
        character(len=32) :: buffer

        write (buffer, '(f0.3)') seconds
        text = trim(buffer)
        if (text(1:1) == '.') text = '0'//text
    end function seconds_text

end module fo_junit
