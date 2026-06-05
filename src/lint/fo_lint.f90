module fo_lint
    use fo_json, only: json_escape_str, json_int, make_tmpfile, delete_tmpfile
    implicit none
    private
    public :: lint_finding_t, lint_file, lint_dir, lint_findings_json
    public :: lint_warning_t, lint_compiler, lint_warnings_json, lint_all_json
    public :: lint_dedup_warnings
    public :: MAX_FINDINGS, MAX_WARNINGS

    integer, parameter :: MAX_FINDINGS = 512
    integer, parameter :: MAX_WARNINGS = 256
    integer, parameter :: MAX_SYMS = 64
    integer, parameter :: MAX_SYM_LEN = 128

    type :: lint_finding_t
        character(len=256) :: file = ''
        integer :: line = 0
        character(len=128) :: module_name = ''
        character(len=128) :: symbol = ''
    end type lint_finding_t

    type :: lint_warning_t
        character(len=256) :: file = ''
        integer :: line = 0
        integer :: column = 0
        character(len=512) :: message = ''
    end type lint_warning_t

contains

    subroutine lint_file(filename, findings, n_findings)
        character(len=*), intent(in) :: filename
        type(lint_finding_t), intent(inout) :: findings(MAX_FINDINGS)
        integer, intent(inout) :: n_findings

        character(len=1024) :: lines(10000)
        integer :: n_lines, u, iostat, i
        character(len=1024) :: lowered(10000)

        n_lines = 0
        open (newunit=u, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            n_lines = n_lines + 1
            if (n_lines > 10000) then
                n_lines = n_lines - 1
                exit
            end if
            read (u, '(a)', iostat=iostat) lines(n_lines)
            if (iostat /= 0) then
                n_lines = n_lines - 1
                exit
            end if
        end do
        close (u)

        do i = 1, n_lines
            lowered(i) = to_lower(lines(i))
        end do

        do i = 1, n_lines
            call check_use_line(filename, i, lowered, n_lines, &
                                findings, n_findings)
        end do
    end subroutine lint_file

    subroutine check_use_line(filename, line_no, lowered, n_lines, &
                              findings, n_findings)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_no, n_lines
        character(len=1024), intent(in) :: lowered(n_lines)
        type(lint_finding_t), intent(inout) :: findings(MAX_FINDINGS)
        integer, intent(inout) :: n_findings

        character(len=1024) :: line
        character(len=MAX_SYM_LEN) :: mod_name, syms(MAX_SYMS)
        character(len=MAX_SYM_LEN) :: local_names(MAX_SYMS)
        integer :: n_syms, only_pos, i
        logical :: used

        line = lowered(line_no)
        call strip_leading(line)

        if (.not. starts_with(line, 'use ') .and. &
            .not. starts_with(line, 'use,')) return
        only_pos = index(line, 'only:')
        if (only_pos == 0) return

        call parse_use_only(line, mod_name, syms, local_names, n_syms)
        if (n_syms == 0) return

        do i = 1, n_syms
            call is_symbol_used(local_names(i), line_no, lowered, n_lines, used)
            if (.not. used) then
                if (n_findings < MAX_FINDINGS) then
                    n_findings = n_findings + 1
                    findings(n_findings)%file = filename
                    findings(n_findings)%line = line_no
                    findings(n_findings)%module_name = mod_name
                    findings(n_findings)%symbol = syms(i)
                end if
            end if
        end do
    end subroutine check_use_line

    subroutine parse_use_only(line, mod_name, syms, local_names, n_syms)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: mod_name
        character(len=MAX_SYM_LEN), intent(out) :: syms(MAX_SYMS)
        character(len=MAX_SYM_LEN), intent(out) :: local_names(MAX_SYMS)
        integer, intent(out) :: n_syms

        integer :: only_pos, comma_pos, arrow_pos, start, i
        character(len=1024) :: after_only, token
        character(len=1024) :: before_only

        mod_name = ''
        n_syms = 0
        syms = ''
        local_names = ''

        only_pos = index(line, 'only:')
        if (only_pos == 0) return

        before_only = line(1:only_pos - 1)
        call strip_leading(before_only)
        ! skip "use" or "use, intrinsic ::"
        i = index(before_only, '::')
        if (i > 0) then
            mod_name = before_only(i + 2:)
        else
            mod_name = before_only(5:)
        end if
        call strip_leading(mod_name)
        ! remove trailing comma
        i = index(mod_name, ',')
        if (i > 0) mod_name = mod_name(1:i - 1)
        call strip_trailing(mod_name)

        after_only = line(only_pos + 5:)
        ! remove comment
        i = index(after_only, '!')
        if (i > 0) after_only = after_only(1:i - 1)
        ! remove continuation &
        i = index(after_only, '&')
        if (i > 0) after_only = after_only(1:i - 1)
        call strip_leading(after_only)
        call strip_trailing(after_only)
        if (len_trim(after_only) == 0) return

        start = 1
        do
            comma_pos = index(after_only(start:), ',')
            if (comma_pos > 0) then
                token = after_only(start:start + comma_pos - 2)
                start = start + comma_pos
            else
                token = after_only(start:)
                start = len_trim(after_only) + 1
            end if

            call strip_leading(token)
            call strip_trailing(token)
            if (len_trim(token) == 0) then
                if (start > len_trim(after_only)) exit
                cycle
            end if

            n_syms = n_syms + 1
            if (n_syms > MAX_SYMS) then
                n_syms = n_syms - 1
                exit
            end if

            arrow_pos = index(token, '=>')
            if (arrow_pos > 0) then
                local_names(n_syms) = trim(token(1:arrow_pos - 1))
                syms(n_syms) = trim(token)
                call strip_trailing(local_names(n_syms))
            else
                syms(n_syms) = trim(token)
                local_names(n_syms) = trim(token)
            end if

            if (start > len_trim(after_only)) exit
        end do
    end subroutine parse_use_only

    subroutine is_symbol_used(sym, use_line, lowered, n_lines, used)
        character(len=*), intent(in) :: sym
        integer, intent(in) :: use_line, n_lines
        character(len=1024), intent(in) :: lowered(n_lines)
        logical, intent(out) :: used

        integer :: i, pos, sym_len
        character(len=1) :: before_ch, after_ch
        character(len=MAX_SYM_LEN) :: search_sym

        used = .false.
        search_sym = sym
        call strip_leading(search_sym)
        call strip_trailing(search_sym)
        sym_len = len_trim(search_sym)
        if (sym_len == 0) then
            used = .true.
            return
        end if

        do i = 1, n_lines
            if (i == use_line) cycle
            ! skip other use lines (symbol might appear there as import)
            block
                character(len=1024) :: stripped
                stripped = lowered(i)
                call strip_leading(stripped)
                if (starts_with(stripped, 'use ') .or. &
                    starts_with(stripped, 'use,')) cycle
            end block

            pos = 1
            do
                pos = index(lowered(i) (pos:), trim(search_sym))
                if (pos == 0) exit
                pos = pos + (1 - 1)
                ! recompute absolute position
                block
                    integer :: abs_pos
                    abs_pos = index(lowered(i) (1:), trim(search_sym))
                    if (abs_pos == 0) exit
                    ! check word boundary
                    before_ch = ' '
                    if (abs_pos > 1) before_ch = lowered(i) (abs_pos - 1:abs_pos - 1)
                    after_ch = ' '
                    if (abs_pos + sym_len <= len_trim(lowered(i))) &
                        after_ch = lowered(i) (abs_pos + sym_len:abs_pos + sym_len)
                    if (.not. is_ident_char(before_ch) .and. &
                        .not. is_ident_char(after_ch)) then
                        used = .true.
                        return
                    end if
                end block
                exit
            end do
        end do
    end subroutine is_symbol_used

    subroutine lint_dir(dir, findings, n_findings)
        character(len=*), intent(in) :: dir
        type(lint_finding_t), intent(out) :: findings(MAX_FINDINGS)
        integer, intent(out) :: n_findings

        character(len=512) :: tmpfile, cmd, fpath
        integer :: u, iostat

        n_findings = 0
        call make_tmpfile('fo_lint_files', tmpfile)
        cmd = 'find '//trim(dir)// &
              " -path '*/build' -prune -o"// &
              " -path '*/.git' -prune -o"// &
              " \( -name '*.f90' -o -name '*.F90'"// &
              " -o -name '*.f' -o -name '*.F' \) -print 2>/dev/null"// &
              ' | sort > '//trim(tmpfile)
        call execute_command_line(cmd, wait=.true.)

        open (newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) fpath
            if (iostat /= 0) exit
            if (len_trim(fpath) == 0) cycle
            call lint_file(trim(fpath), findings, n_findings)
        end do
        close (u)
        call delete_tmpfile(tmpfile)
    end subroutine lint_dir

    function lint_findings_json(findings, n_findings) result(json)
        type(lint_finding_t), intent(in) :: findings(*)
        integer, intent(in) :: n_findings
        character(len=8192) :: json

        integer :: i

        if (n_findings == 0) then
            json = '{"unused_imports":[],"count":0}'
            return
        end if

        json = '{"unused_imports":['
        do i = 1, n_findings
            if (i > 1) json = trim(json)//','
            json = trim(json)//'{"file":"'// &
                   trim(json_escape_str(findings(i)%file))//'"'// &
                   ',"line":'//trim(json_int(findings(i)%line))// &
                   ',"module":"'// &
                   trim(json_escape_str(findings(i)%module_name))//'"'// &
                   ',"symbol":"'// &
                   trim(json_escape_str(findings(i)%symbol))//'"}'
        end do
        json = trim(json)//'],"count":'//trim(json_int(n_findings))//'}'
    end function lint_findings_json

    subroutine lint_compiler(dir, warnings, n_warnings)
        character(len=*), intent(in) :: dir
        type(lint_warning_t), intent(out) :: warnings(MAX_WARNINGS)
        integer, intent(out) :: n_warnings

        character(len=512) :: tmpfile, fpath
        character(len=4096) :: cmd
        character(len=2048) :: mod_flags
        integer :: u, iostat

        n_warnings = 0
        call find_mod_include_flags(dir, mod_flags)

        call make_tmpfile('fo_lint_warn_files', tmpfile)
        cmd = 'find '//trim(dir)// &
              " -path '*/build' -prune -o"// &
              " -path '*/.git' -prune -o"// &
              " \( -name '*.f90' -o -name '*.F90'"// &
              " -o -name '*.f' -o -name '*.F' \) -print 2>/dev/null"// &
              ' | sort > '//trim(tmpfile)
        call execute_command_line(cmd, wait=.true.)

        open (newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat /= 0) then
            call delete_tmpfile(tmpfile)
            return
        end if

        do
            read (u, '(a)', iostat=iostat) fpath
            if (iostat /= 0) exit
            if (len_trim(fpath) == 0) cycle
            call lint_file_compiler(trim(fpath), mod_flags, &
                                    warnings, n_warnings)
            call lint_file_lengths(trim(fpath), warnings, n_warnings)
        end do
        close (u)
        call delete_tmpfile(tmpfile)
    end subroutine lint_compiler

    subroutine find_mod_include_flags(dir, flags)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: flags

        character(len=512) :: tmpfile, line
        character(len=4096) :: cmd
        integer :: u, iostat

        flags = ''
        call make_tmpfile('fo_lint_moddirs', tmpfile)
        cmd = 'find '//trim(dir)// &
              "/build -name '*.mod' -type f"// &
              ' -exec dirname {} \; 2>/dev/null'// &
              ' | sort -u > '//trim(tmpfile)
        call execute_command_line(cmd, wait=.true.)

        open (newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (u, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                if (len_trim(line) == 0) cycle
                if (len_trim(flags) + len_trim(line) + 4 > len(flags)) exit
                flags = trim(flags)//' -I'//trim(line)
            end do
            close (u)
        end if
        call delete_tmpfile(tmpfile)
    end subroutine find_mod_include_flags

    subroutine lint_file_lengths(filepath, warnings, n_warnings)
        character(len=*), intent(in) :: filepath
        type(lint_warning_t), intent(inout) :: warnings(MAX_WARNINGS)
        integer, intent(inout) :: n_warnings

        character(len=2048) :: line
        integer :: u, iostat, lineno, linelen

        open (newunit=u, file=trim(filepath), status='old', iostat=iostat)
        if (iostat /= 0) return

        lineno = 0
        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            lineno = lineno + 1
            linelen = len_trim(line)
            if (linelen > 132) then
                block
                    character(len=len(line)) :: stripped
                    stripped = adjustl(line)
                    if (stripped(1:1) == '#') cycle
                end block
                if (n_warnings < MAX_WARNINGS) then
                    n_warnings = n_warnings + 1
                    warnings(n_warnings)%file = trim(filepath)
                    warnings(n_warnings)%line = lineno
                    warnings(n_warnings)%column = 133
                    write (warnings(n_warnings)%message, '(a,i0,a)') &
                        'line exceeds 132 characters (', linelen, ' chars)'
                end if
            end if
        end do
        close (u)
    end subroutine lint_file_lengths

    subroutine lint_file_compiler(filepath, mod_flags, warnings, n_warnings)
        character(len=*), intent(in) :: filepath, mod_flags
        type(lint_warning_t), intent(inout) :: warnings(MAX_WARNINGS)
        integer, intent(inout) :: n_warnings

        character(len=512) :: errfile
        character(len=4096) :: cmd
        character(len=1024) :: line
        character(len=256) :: cur_file
        integer :: cur_line, cur_col
        integer :: u, iostat

        call make_tmpfile('fo_lint_gfortran', errfile)
        cmd = 'gfortran -fsyntax-only -Wall -Wextra'// &
              ' -Wimplicit-interface -Wimplicit-procedure'// &
              trim(mod_flags)//' '//trim(filepath)// &
              ' 2>'//trim(errfile)
        call execute_command_line(cmd, wait=.true.)

        cur_file = ''
        cur_line = 0
        cur_col = 0

        open (newunit=u, file=errfile, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (u, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                call parse_gfortran_warning(line, cur_file, cur_line, &
                                            cur_col, warnings, n_warnings)
            end do
            close (u)
        end if
        call delete_tmpfile(errfile)
    end subroutine lint_file_compiler

    subroutine parse_gfortran_warning(line, cur_file, cur_line, cur_col, &
                                      warnings, n_warnings)
        character(len=*), intent(in) :: line
        character(len=256), intent(inout) :: cur_file
        integer, intent(inout) :: cur_line, cur_col
        type(lint_warning_t), intent(inout) :: warnings(MAX_WARNINGS)
        integer, intent(inout) :: n_warnings

        character(len=1024) :: clean
        integer :: ext_pos, c1, c2, c3, iostat
        character(len=32) :: num_str

        clean = adjustl(line)
        if (len_trim(clean) == 0) return

        ext_pos = index(clean, '.f90:')
        if (ext_pos == 0) ext_pos = index(clean, '.F90:')
        if (ext_pos > 0) then
            c1 = ext_pos + 4
            c2 = index(clean(c1 + 1:), ':')
            if (c2 > 0) then
                c2 = c1 + c2
                num_str = clean(c1 + 1:c2 - 1)
                read (num_str, *, iostat=iostat) cur_line
                if (iostat == 0) then
                    cur_file = clean(1:c1 - 1)
                    cur_col = 0
                    c3 = index(clean(c2 + 1:), ':')
                    if (c3 > 0) then
                        c3 = c2 + c3
                        num_str = clean(c2 + 1:c3 - 1)
                        read (num_str, *, iostat=iostat) cur_col
                        if (iostat /= 0) cur_col = 0
                    end if
                end if
            end if
            return
        end if

        if (index(clean, 'Warning:') /= 1) return
        if (index(clean, "Can't open module file") > 0) return
        if (index(clean, 'Fatal Error') > 0) return
        if (index(clean, 'stack-var-size') > 0) return
        if (len_trim(cur_file) == 0) return
        if (n_warnings >= MAX_WARNINGS) return

        n_warnings = n_warnings + 1
        warnings(n_warnings)%file = trim(cur_file)
        warnings(n_warnings)%line = cur_line
        warnings(n_warnings)%column = cur_col
        warnings(n_warnings)%message = trim(clean)
    end subroutine parse_gfortran_warning

    function lint_warnings_json(warnings, n_warnings) result(json)
        type(lint_warning_t), intent(in) :: warnings(*)
        integer, intent(in) :: n_warnings
        character(len=8192) :: json

        integer :: i

        if (n_warnings == 0) then
            json = '{"warnings":[],"count":0}'
            return
        end if

        json = '{"warnings":['
        do i = 1, n_warnings
            if (i > 1) json = trim(json)//','
            json = trim(json)//'{"file":"'// &
                   trim(json_escape_str(warnings(i)%file))//'"'// &
                   ',"line":'//trim(json_int(warnings(i)%line))// &
                   ',"column":'//trim(json_int(warnings(i)%column))// &
                   ',"message":"'// &
                   trim(json_escape_str(warnings(i)%message))//'"}'
        end do
        json = trim(json)//'],"count":'//trim(json_int(n_warnings))//'}'
    end function lint_warnings_json

    function lint_all_json(findings, n_findings, warnings, n_warnings) &
        result(json)
        type(lint_finding_t), intent(in) :: findings(*)
        integer, intent(in) :: n_findings
        type(lint_warning_t), intent(in) :: warnings(*)
        integer, intent(in) :: n_warnings
        character(len=16384) :: json

        integer, parameter :: LIMIT = 15000
        integer :: i, total, n_emitted

        total = n_findings + n_warnings
        json = '{"unused_imports":['

        do i = 1, n_findings
            if (len_trim(json) > LIMIT) exit
            if (i > 1) json = trim(json)//','
            json = trim(json)//'{"file":"'// &
                   trim(json_escape_str(findings(i)%file))//'"'// &
                   ',"line":'//trim(json_int(findings(i)%line))// &
                   ',"module":"'// &
                   trim(json_escape_str(findings(i)%module_name))//'"'// &
                   ',"symbol":"'// &
                   trim(json_escape_str(findings(i)%symbol))//'"}'
        end do

        json = trim(json)//'],"warnings":['

        n_emitted = 0
        do i = 1, n_warnings
            if (len_trim(json) > LIMIT) exit
            if (n_emitted > 0) json = trim(json)//','
            n_emitted = n_emitted + 1
            json = trim(json)//'{"file":"'// &
                   trim(json_escape_str(warnings(i)%file))//'"'// &
                   ',"line":'//trim(json_int(warnings(i)%line))// &
                   ',"column":'//trim(json_int(warnings(i)%column))// &
                   ',"message":"'// &
                   trim(json_escape_str(warnings(i)%message))//'"}'
        end do

        json = trim(json)//'],"count":'//trim(json_int(total))//'}'
    end function lint_all_json

    pure function to_lower(str) result(low)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: low
        integer :: i, ic

        low = str
        do i = 1, len_trim(low)
            ic = iachar(low(i:i))
            if (ic >= iachar('A') .and. ic <= iachar('Z')) &
                low(i:i) = achar(ic + 32)
        end do
    end function to_lower

    pure subroutine strip_leading(str)
        character(len=*), intent(inout) :: str
        integer :: i

        do i = 1, len_trim(str)
            if (str(i:i) /= ' ' .and. str(i:i) /= char(9)) then
                str = str(i:)
                return
            end if
        end do
        str = ''
    end subroutine strip_leading

    pure subroutine strip_trailing(str)
        character(len=*), intent(inout) :: str
        integer :: i

        do i = len_trim(str), 1, -1
            if (str(i:i) /= ' ' .and. str(i:i) /= char(9)) then
                str(i + 1:) = ''
                return
            end if
        end do
        str = ''
    end subroutine strip_trailing

    pure logical function starts_with(str, prefix)
        character(len=*), intent(in) :: str, prefix

        starts_with = (len_trim(str) >= len_trim(prefix) .and. &
                       str(1:len_trim(prefix)) == prefix)
    end function starts_with

    pure logical function is_ident_char(c)
        character(len=1), intent(in) :: c
        integer :: ic

        ic = iachar(c)
        is_ident_char = (ic >= iachar('a') .and. ic <= iachar('z')) .or. &
                        (ic >= iachar('A') .and. ic <= iachar('Z')) .or. &
                        (ic >= iachar('0') .and. ic <= iachar('9')) .or. &
                        c == '_'
    end function is_ident_char

    subroutine lint_dedup_warnings(warnings, n_warnings)
        type(lint_warning_t), intent(inout) :: warnings(MAX_WARNINGS)
        integer, intent(inout) :: n_warnings

        integer :: i, j, out
        logical :: dup

        out = 0
        do i = 1, n_warnings
            dup = .false.
            do j = 1, out
                if (trim(warnings(j)%file) == trim(warnings(i)%file) .and. &
                    warnings(j)%line == warnings(i)%line .and. &
                    trim(warnings(j)%message) == trim(warnings(i)%message)) then
                    dup = .true.
                    exit
                end if
            end do
            if (.not. dup) then
                out = out + 1
                if (out /= i) warnings(out) = warnings(i)
            end if
        end do
        n_warnings = out
    end subroutine lint_dedup_warnings

end module fo_lint
