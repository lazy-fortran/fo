module fo_lint
    use fo_util, only: json_int, make_tmpfile, delete_tmpfile, &
        clean_root_build_artifacts
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_collect_files, &
        fs_collect_mod_dirs
    use fo_process, only: process_run_argv_logged, argv_push, argv_push_split
    use fo_lint_shortcircuit, only: shortcircuit_scan_file
    use fx_json_build, only: json_escape_string
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

        character(len=1024), allocatable :: lines(:), lowered(:)
        integer :: n_lines, u, iostat, i

        allocate (lines(10000), lowered(10000))

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

        integer :: i, pos, rel_pos, abs_pos, sym_len
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
            do while (pos <= len_trim(lowered(i)))
                rel_pos = index(lowered(i) (pos:), trim(search_sym))
                if (rel_pos == 0) exit
                abs_pos = pos + rel_pos - 1
                before_ch = ' '
                if (abs_pos > 1) before_ch = lowered(i) (abs_pos - 1:abs_pos - 1)
                after_ch = ' '
                if (abs_pos + sym_len <= len_trim(lowered(i))) &
                    after_ch = lowered(i) (abs_pos + sym_len:abs_pos + sym_len)
                if (is_symbol_boundary_before(lowered(i), abs_pos) .and. &
                    .not. is_ident_char(after_ch)) then
                used = .true.
                return
            end if
            pos = abs_pos + 1
        end do
    end do
end subroutine is_symbol_used

subroutine collect_fortran_sources(dir, files, n_files)
    !! Collect *.f90/*.F90/*.f/*.F under dir, sorted, excluding any path
    !! component */build or */.git. Replaces the find | sort pipeline.
    character(len=*), intent(in) :: dir
    character(len=512), allocatable, intent(out) :: files(:)
    integer, intent(out) :: n_files

    character(len=512), allocatable :: hits(:)
    integer :: n_hits, s, i
    character(len=4), parameter :: suffixes(4) = &
        ['.f90', '.F90', '.f  ', '.F  ']

    allocate (files(20000))
    allocate (hits(20000))
    n_files = 0
    do s = 1, size(suffixes)
        call fs_collect_files(dir, '', trim(suffixes(s)), '', hits, n_hits)
        do i = 1, n_hits
            if (index(hits(i), '/build/') > 0) cycle
            if (index(hits(i), '/.git/') > 0) cycle
            if (n_files >= size(files)) exit
            n_files = n_files + 1
            files(n_files) = hits(i)
        end do
    end do
    ! merge-sort not needed for correctness, but keep deterministic order:
    ! each suffix block is already sorted; sort the union by simple insertion.
    do i = 2, n_files
        block
            character(len=512) :: key
            integer :: j
            key = files(i)
            j = i - 1
            do while (j >= 1)
                if (llt(files(j), key) .or. files(j) == key) exit
                files(j + 1) = files(j)
                j = j - 1
            end do
            files(j + 1) = key
        end block
    end do
    deallocate (hits)
end subroutine collect_fortran_sources

subroutine lint_dir(dir, findings, n_findings)
    character(len=*), intent(in) :: dir
    type(lint_finding_t), intent(out) :: findings(MAX_FINDINGS)
    integer, intent(out) :: n_findings

    character(len=512), allocatable :: files(:)
    integer :: n_files, i

    n_findings = 0
    call collect_fortran_sources(dir, files, n_files)
    do i = 1, n_files
        if (len_trim(files(i)) == 0) cycle
        call lint_file(trim(files(i)), findings, n_findings)
    end do
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
            trim(json_escape_string(findings(i)%file))//'"'// &
            ',"line":'//trim(json_int(findings(i)%line))// &
            ',"module":"'// &
            trim(json_escape_string(findings(i)%module_name))//'"'// &
            ',"symbol":"'// &
            trim(json_escape_string(findings(i)%symbol))//'"}'
    end do
    json = trim(json)//'],"count":'//trim(json_int(n_findings))//'}'
end function lint_findings_json

subroutine lint_compiler(dir, warnings, n_warnings)
    character(len=*), intent(in) :: dir
    type(lint_warning_t), intent(out) :: warnings(MAX_WARNINGS)
    integer, intent(out) :: n_warnings

    character(len=512) :: moddir
    character(len=512), allocatable :: files(:)
    character(len=2048) :: mod_flags
    integer :: n_files, i, n_removed

    n_warnings = 0
    call find_mod_include_flags(dir, mod_flags)

    ! Defence in depth: clear any stray root .mod/.smod/.o before linting, the
    ! same sweep the build does, so lint leaves the project root clean.
    call clean_root_build_artifacts(dir, n_removed)

    ! Direct gfortran's module output to a temp dir. Without -J, even
    ! -fsyntax-only writes .mod into the cwd (the project root), where they
    ! shadow build/fo/mod and break later builds with stale module interfaces.
    call make_tmpfile('fo_lint_mod', moddir)
    call fs_make_dir(trim(moddir))
    if (len_trim(mod_flags) + len_trim(moddir) + 4 <= len(mod_flags)) &
        mod_flags = trim(mod_flags)//' -J'//trim(moddir)

    call collect_fortran_sources(dir, files, n_files)
    do i = 1, n_files
        if (len_trim(files(i)) == 0) cycle
        call lint_file_compiler(trim(files(i)), mod_flags, &
            warnings, n_warnings)
        call lint_file_lengths(trim(files(i)), warnings, n_warnings)
        call lint_file_shortcircuit(trim(files(i)), warnings, n_warnings)
    end do
    call fs_remove_tree(trim(moddir))
end subroutine lint_compiler

subroutine find_mod_include_flags(dir, flags)
    character(len=*), intent(in) :: dir
    character(len=*), intent(out) :: flags

    character(len=512), allocatable :: moddirs(:)
    integer :: n_moddirs, i

    flags = ''
    allocate (moddirs(4096))
    call fs_collect_mod_dirs(trim(dir)//'/build', moddirs, n_moddirs)
    do i = 1, n_moddirs
        if (len_trim(moddirs(i)) == 0) cycle
        if (len_trim(flags) + len_trim(moddirs(i)) + 4 > len(flags)) exit
        flags = trim(flags)//' -I'//trim(moddirs(i))
    end do
    deallocate (moddirs)
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
    character(len=1024) :: line
    character(len=256) :: cur_file
    integer :: cur_line, cur_col
    integer :: u, iostat
    character(len=:), allocatable :: packed
    integer :: n_args, exitcode

    call make_tmpfile('fo_lint_gfortran', errfile)
    n_args = 0
    call argv_push(packed, n_args, 'gfortran')
    call argv_push(packed, n_args, '-fsyntax-only')
    call argv_push(packed, n_args, '-Wall')
    call argv_push(packed, n_args, '-Wextra')
    call argv_push(packed, n_args, '-Wimplicit-interface')
    call argv_push(packed, n_args, '-Wimplicit-procedure')
    call argv_push_split(packed, n_args, trim(mod_flags))
    call argv_push(packed, n_args, trim(filepath))
    call process_run_argv_logged('', packed, n_args, trim(errfile), &
        .false., 120, exitcode)

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

subroutine lint_file_shortcircuit(filepath, warnings, n_warnings)
    !! Append short-circuit-evaluation hazards found by the textual detector.
    character(len=*), intent(in) :: filepath
    type(lint_warning_t), intent(inout) :: warnings(MAX_WARNINGS)
    integer, intent(inout) :: n_warnings

    integer :: hit_line(MAX_WARNINGS), n_hits, k
    character(len=512) :: hit_msg(MAX_WARNINGS)

    n_hits = 0
    call shortcircuit_scan_file(filepath, hit_line, hit_msg, n_hits, MAX_WARNINGS)
    do k = 1, n_hits
        if (n_warnings >= MAX_WARNINGS) exit
        n_warnings = n_warnings + 1
        warnings(n_warnings)%file = trim(filepath)
        warnings(n_warnings)%line = hit_line(k)
        warnings(n_warnings)%column = 0
        warnings(n_warnings)%message = trim(hit_msg(k))
    end do
end subroutine lint_file_shortcircuit

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
            trim(json_escape_string(warnings(i)%file))//'"'// &
            ',"line":'//trim(json_int(warnings(i)%line))// &
            ',"column":'//trim(json_int(warnings(i)%column))// &
            ',"message":"'// &
            trim(json_escape_string(warnings(i)%message))//'"}'
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
            trim(json_escape_string(findings(i)%file))//'"'// &
            ',"line":'//trim(json_int(findings(i)%line))// &
            ',"module":"'// &
            trim(json_escape_string(findings(i)%module_name))//'"'// &
            ',"symbol":"'// &
            trim(json_escape_string(findings(i)%symbol))//'"}'
    end do

    json = trim(json)//'],"warnings":['

    n_emitted = 0
    do i = 1, n_warnings
        if (len_trim(json) > LIMIT) exit
        if (n_emitted > 0) json = trim(json)//','
        n_emitted = n_emitted + 1
        json = trim(json)//'{"file":"'// &
            trim(json_escape_string(warnings(i)%file))//'"'// &
            ',"line":'//trim(json_int(warnings(i)%line))// &
            ',"column":'//trim(json_int(warnings(i)%column))// &
            ',"message":"'// &
            trim(json_escape_string(warnings(i)%message))//'"}'
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

    starts_with = (len_trim(str) >= len(prefix) .and. &
        str(1:len(prefix)) == prefix)
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

pure logical function is_symbol_boundary_before(line, pos)
    character(len=*), intent(in) :: line
    integer, intent(in) :: pos

    character(len=1) :: before_ch, kind_prefix

    is_symbol_boundary_before = .true.
    if (pos <= 1) return

    before_ch = line(pos - 1:pos - 1)
    if (.not. is_ident_char(before_ch)) return

    is_symbol_boundary_before = .false.
    if (before_ch /= '_' .or. pos <= 2) return

    kind_prefix = line(pos - 2:pos - 2)
    is_symbol_boundary_before = (kind_prefix >= '0' .and. kind_prefix <= '9') .or. &
        kind_prefix == '"' .or. kind_prefix == "'"
end function is_symbol_boundary_before

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
