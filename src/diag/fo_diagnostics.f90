module fo_diagnostics
    use fortfront_compiler, only: compiler_diagnostic_t, &
        compiler_frontend_options_t, compiler_frontend_result_t, &
        compile_frontend_from_file, get_compiler_diagnostics, &
        DIAGNOSTIC_PHASE_PARSER, DIAGNOSTIC_PHASE_SEMANTIC, &
        DIAGNOSTIC_CODE_PARSER, DIAGNOSTIC_CODE_SEMANTIC, &
        DIAGNOSTIC_ERROR, DIAGNOSTIC_WARNING, DIAGNOSTIC_INFO, &
        INPUT_MODE_STANDARD, OPERATING_MODE_INFER
    implicit none
    private
    public :: diagnostic_t, diagnostic_from_log, is_runner_crash
    public :: array_temporary_warnings_from_log
    public :: map_fortfront_diagnostic, frontend_diagnostics_from_file

    ! Severity values follow FortFront's public diagnostic scale so a mapped
    ! frontend diagnostic keeps its original severity without any prose
    ! parsing. 0 means "not a frontend diagnostic".
    integer, parameter, public :: FO_DIAG_SEVERITY_NONE = 0
    integer, parameter, public :: FO_DIAG_SEVERITY_ERROR = 1
    integer, parameter, public :: FO_DIAG_SEVERITY_WARNING = 2

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
        ! Frontend-only fields: populated by map_fortfront_diagnostic, left
        ! at their defaults by the log parsers.
        integer :: severity = FO_DIAG_SEVERITY_NONE
        integer :: phase = 0
        integer :: code = 0
    end type diagnostic_t

contains

    subroutine array_temporary_warnings_from_log(log_file, warnings, n_warnings)
        character(len=*), intent(in) :: log_file
        type(diagnostic_t), intent(out) :: warnings(:)
        integer, intent(out) :: n_warnings

        character(len=512) :: line
        character(len=256) :: current_file, parsed_file
        integer :: current_line, current_column, parsed_line, parsed_column
        integer :: u, iostat
        logical :: has_location

        n_warnings = 0
        current_file = ''
        current_line = 0
        current_column = 0
        open (newunit=u, file=log_file, status='old', iostat=iostat)
        if (iostat /= 0) return
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
            if (index(line, 'Warning:') == 0) cycle
            if (index(line, 'array temporary') == 0 .and. &
                index(line, 'array temporaries') == 0) cycle
            if (n_warnings >= size(warnings)) exit
            n_warnings = n_warnings + 1
            warnings(n_warnings)%kind = 'array-temporary'
            warnings(n_warnings)%file = current_file
            warnings(n_warnings)%line = current_line
            warnings(n_warnings)%column = current_column
            warnings(n_warnings)%message = trim(adjustl(line))
            warnings(n_warnings)%log_path = log_file
        end do
        close (u)
    end subroutine array_temporary_warnings_from_log

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
        case ('install')
            hint = 'fix the first install diagnostic, then rerun fo install'
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

    subroutine frontend_diagnostics_from_file(filename, diags, n_diags, &
            had_error, with_semantics)
        !! Run FortFront's standard-mode frontend over one source file and map
        !! every structured parser/semantic diagnostic into fo diagnostics.
        !! Errors set had_error so the caller can fail the check; warnings are
        !! surfaced but do not fail. Per-file semantic analysis cannot resolve
        !! cross-file interfaces and declarations, so callers that only want
        !! reliable syntax diagnostics pass with_semantics=.false. (the
        !! default here is .true. so the mapping is fully exercised).
        character(len=*), intent(in) :: filename
        type(diagnostic_t), intent(inout) :: diags(:)
        integer, intent(out) :: n_diags
        logical, intent(out) :: had_error
        logical, intent(in), optional :: with_semantics

        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        type(compiler_diagnostic_t), allocatable :: fds(:)
        integer :: i
        logical :: do_semantics

        do_semantics = .true.
        if (present(with_semantics)) do_semantics = with_semantics

        n_diags = 0
        had_error = .false.
        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%operating_mode = OPERATING_MODE_INFER
        options%run_semantics = do_semantics
        call compile_frontend_from_file(filename, result, options)
        fds = get_compiler_diagnostics(result)
        do i = 1, size(fds)
            if (n_diags >= size(diags)) exit
            n_diags = n_diags + 1
            call map_fortfront_diagnostic(fds(i), filename, diags(n_diags))
            if (fds(i)%severity == DIAGNOSTIC_ERROR) had_error = .true.
        end do
    end subroutine frontend_diagnostics_from_file

    subroutine map_fortfront_diagnostic(fd, source_file, diag)
        !! Map FortFront's structured phase/code/severity/span fields into a
        !! fo diagnostic without parsing diagnostic prose. The source path and
        !! start-of-span location are preserved, severity is carried on the
        !! diagnostic's own field, the stable integer code is kept, and the
        !! formatted message embeds the phase, code, severity and message text.
        type(compiler_diagnostic_t), intent(in) :: fd
        character(len=*), intent(in) :: source_file
        type(diagnostic_t), intent(out) :: diag

        character(len=32) :: phase_str, code_str, severity_str
        character(len=512) :: message_text

        diag = diagnostic_t()
        diag%kind = 'frontend'
        diag%file = source_file
        diag%line = fd%span%start%line
        diag%column = fd%span%start%column
        diag%severity = fd%severity
        diag%phase = fd%phase
        diag%code = fd%code

        select case (fd%phase)
        case (DIAGNOSTIC_PHASE_PARSER)
            phase_str = 'parser'
            if (fd%code == 0) diag%code = DIAGNOSTIC_CODE_PARSER
        case (DIAGNOSTIC_PHASE_SEMANTIC)
            phase_str = 'semantic'
            if (fd%code == 0) diag%code = DIAGNOSTIC_CODE_SEMANTIC
        case default
            phase_str = 'frontend'
        end select

        write (code_str, '(i0)') diag%code

        select case (fd%severity)
        case (DIAGNOSTIC_ERROR)
            severity_str = 'error'
        case (DIAGNOSTIC_WARNING)
            severity_str = 'warning'
        case (DIAGNOSTIC_INFO)
            severity_str = 'info'
        case default
            severity_str = 'note'
        end select

        message_text = ''
        if (allocated(fd%message)) then
            if (len_trim(fd%message) > 0) message_text = trim(fd%message)
        end if
        if (len_trim(message_text) == 0) message_text = 'FortFront reported no message'

        diag%message = '['//trim(phase_str)//' '//trim(code_str)//'] '// &
            trim(severity_str)//': '//trim(message_text)
        diag%hint = 'fix the reported frontend diagnostic in '// &
            trim(source_file)
    end subroutine map_fortfront_diagnostic

end module fo_diagnostics
