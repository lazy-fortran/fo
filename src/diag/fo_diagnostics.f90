module fo_diagnostics
    implicit none
    private
    public :: diagnostic_t, diagnostic_from_log, is_runner_crash

    type :: diagnostic_t
        character(len=32) :: kind = 'backend'
        character(len=256) :: file = ''
        integer :: line = 0
        integer :: column = 0
        character(len=128) :: target = ''
        character(len=512) :: message = ''
        character(len=256) :: hint = ''
        character(len=256) :: rerun = ''
        character(len=512) :: log_path = ''
    end type diagnostic_t

contains

    subroutine diagnostic_from_log(kind, log_file, rerun, diag)
        character(len=*), intent(in) :: kind, log_file, rerun
        type(diagnostic_t), intent(out) :: diag

        character(len=512) :: line, fallback
        character(len=256) :: current_file, parsed_file
        integer :: current_line, current_column, parsed_line, parsed_column
        integer :: u, iostat, best_priority
        logical :: has_location, selected

        diag%kind = kind
        diag%log_path = log_file
        diag%rerun = rerun
        diag%hint = default_hint(kind)
        fallback = ''
        current_file = ''
        current_line = 0
        current_column = 0
        best_priority = 0

        open (newunit=u, file=log_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (u, '(a)', iostat=iostat) line
                if (iostat /= 0) exit

                call parse_location_line(line, parsed_file, parsed_line, &
                    parsed_column, has_location)
                if (has_location) then
                    current_file = parsed_file
                    current_line = parsed_line
                    current_column = parsed_column
                end if

                call consider_log_line(line, diag%message, fallback, &
                    best_priority, selected)
                if (selected .and. len_trim(current_file) > 0) then
                    diag%file = current_file
                    diag%line = current_line
                    diag%column = current_column
                end if
            end do
            close (u)
        end if

        if (len_trim(diag%message) == 0) diag%message = fallback
        if (len_trim(diag%message) == 0) then
            diag%message = 'backend returned nonzero status'
        end if

        diag%target = infer_target(diag%message)
        if (is_linker_error(diag%message)) then
            diag%hint = 'check LIBRARY_PATH and link = [...] in fpm.toml'
        end if
        if (trim(kind) == 'test') then
            diag%hint = 'make this test faster or mark it slow'
            if (len_trim(diag%target) > 0) then
                diag%rerun = trim(rerun)//' '//trim(diag%target)
            end if
            if (is_timeout_text(diag%message)) then
                diag%hint = 'make this test faster or rename it *_slow'
            else if (index(diag%message, 'crashed:') > 0) then
                ! A 128+signal exit is a crash, not a slow test. Point at the
                ! usual causes and the tools that localize them.
                diag%hint = 'test crashed (memory bug or stack overflow): '// &
                    'rerun it alone, raise the stack (ulimit -s unlimited), '// &
                    'or rebuild with --flag "-fcheck=all -fbacktrace -g"'
            end if
        end if
    end subroutine diagnostic_from_log

    subroutine parse_location_line(line, file, line_no, column, found)
        character(len=*), intent(in) :: line
        character(len=256), intent(out) :: file
        integer, intent(out) :: line_no, column
        logical, intent(out) :: found

        character(len=512) :: clean, number
        integer :: ext, colon1, colon2, iostat, n_file

        file = ''
        line_no = 0
        column = 0
        found = .false.

        clean = adjustl(line)
        ext = index(clean, '.f90:')
        if (ext == 0) ext = index(clean, '.F90:')
        if (ext == 0) return

        colon1 = ext + 4
        colon2 = index(clean(colon1 + 1:), ':')
        if (colon2 == 0) return
        colon2 = colon1 + colon2

        number = clean(colon1 + 1:colon2 - 1)
        read (number, *, iostat=iostat) line_no
        if (iostat /= 0) return

        number = clean(colon2 + 1:)
        if (index(number, ':') > 0) number = number(1:index(number, ':') - 1)
        read (number, *, iostat=iostat) column
        if (iostat /= 0) column = 0

        n_file = min(colon1 - 1, len(file))
        if (n_file <= 0) return
        file = ''
        file(1:n_file) = clean(1:n_file)
        found = .true.
    end subroutine parse_location_line

    subroutine consider_log_line(line, summary, fallback, best_priority, selected)
        character(len=*), intent(in) :: line
        character(len=*), intent(inout) :: summary, fallback
        integer, intent(inout) :: best_priority
        logical, intent(out) :: selected

        character(len=512) :: clean
        integer :: priority

        selected = .false.
        clean = adjustl(line)
        if (len_trim(clean) == 0) return
        if (trim(clean) == 'STOP 1') return
        if (index(clean, 'Backtrace') > 0) return

        fallback = clean
        priority = 0
        if (index(clean, 'fo: test target ') > 0 .and. &
            index(clean, ' returned exit code') > 0) then
        priority = 7
    else if (index(clean, 'ERROR STOP') > 0) then
        priority = 6
    else if (index(clean, 'Fatal Error:') > 0 .or. &
            index(clean, 'Cannot open file') > 0) then
        priority = 5
    else if (index(clean, 'undefined reference') > 0 .or. &
            index(clean, 'ld: cannot find') > 0 .or. &
            index(clean, 'cannot find -l') > 0 .or. &
            index(clean, 'library not found') > 0) then
        priority = 5
    else if (index(clean, 'Error:') > 0 .or. &
            index(clean, 'error:') > 0) then
        priority = 4
    else if (index(clean, 'timeout') > 0 .or. &
            index(clean, 'Timeout') > 0) then
        priority = 4
    else if (index(clean, 'FAIL:') > 0) then
        priority = 3
    else if (index(clean, 'returned exit code') > 0) then
        priority = 2
    else if (index(clean, '<ERROR>') > 0 .or. &
            index(clean, 'FAIL') > 0) then
        priority = 1
    end if

    if (priority > 0 .and. priority >= best_priority) then
        summary = clean
        best_priority = priority
        selected = .true.
    end if
end subroutine consider_log_line

function default_hint(kind) result(hint)
    character(len=*), intent(in) :: kind
    character(len=256) :: hint

    select case (trim(kind))
    case ('build')
        hint = 'fix the first compiler diagnostic, then rerun fo build'
    case ('test')
        hint = 'rerun the failing test, then fix or mark it slow'
    case default
        hint = 'rerun the reported fo command after fixing the input'
    end select
end function default_hint

function infer_target(summary) result(target)
    character(len=*), intent(in) :: summary
    character(len=128) :: target

    integer :: pos, start, finish

    target = ''
    pos = index(summary, 'test_')
    if (pos == 0) return

    start = pos
    finish = start
    do while (finish <= len_trim(summary))
        select case (summary(finish:finish))
        case (' ', ':', ';', ',', ')', '(', '"')
            exit
        case default
            finish = finish + 1
        end select
    end do
    target = summary(start:finish - 1)
end function infer_target

logical function is_timeout_text(text)
    character(len=*), intent(in) :: text

    is_timeout_text = index(text, 'timeout') > 0 .or. &
        index(text, 'Timeout') > 0 .or. &
        index(text, 'timed out') > 0
end function is_timeout_text

logical function is_linker_error(text)
    character(len=*), intent(in) :: text

    is_linker_error = index(text, 'undefined reference') > 0 .or. &
        index(text, 'ld: cannot find') > 0 .or. &
        index(text, 'cannot find -l') > 0 .or. &
        index(text, 'library not found') > 0
end function is_linker_error

logical function is_runner_crash(text)
    character(len=*), intent(in) :: text

    is_runner_crash = index(text, 'malloc') > 0 .or. &
        index(text, 'Assertion') > 0 .or. &
        index(text, 'SIGABRT') > 0 .or. &
        index(text, 'SIGSEGV') > 0 .or. &
        index(text, 'double free') > 0 .or. &
        index(text, 'corrupted') > 0
end function is_runner_crash

end module fo_diagnostics
