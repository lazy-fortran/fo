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
    public :: lint_fix_dir
    public :: MAX_FINDINGS, MAX_WARNINGS

    integer, parameter :: MAX_FINDINGS = 512
    ! Global aggregate cap across all files. Large so big projects are not
    ! silently truncated (which also made the parallel-collected count
    ! nondeterministic). Per-file passes use the smaller MAX_FILE_WARN buffer.
    integer, parameter :: MAX_WARNINGS = 16384
    integer, parameter :: MAX_FILE_WARN = 512
    integer, parameter :: MAX_SYMS = 64
    integer, parameter :: MAX_SYM_LEN = 128
    ! Usage-scan buffer. Sized for a module plus all of its submodule bodies and
    ! their includes appended together (see lint_file_ex).
    integer, parameter :: MAX_LINT_LINES = 60000

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

        character(len=512) :: no_extra(1)

        no_extra(1) = ''
        call lint_file_ex(filename, no_extra, 0, findings, n_findings)
    end subroutine lint_file

    subroutine lint_file_ex(filename, extra_files, n_extra, findings, n_findings)
        !! Report unused imports in filename. extra_files are sibling sources
        !! (the module's submodules, or vice versa) whose bodies are appended to
        !! the usage scan only: a submodule inherits its parent module's
        !! imports, so an import used solely in a submodule is not unused.
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: extra_files(:)
        integer, intent(in) :: n_extra
        type(lint_finding_t), intent(inout) :: findings(MAX_FINDINGS)
        integer, intent(inout) :: n_findings

        character(len=1024), allocatable :: lines(:), lowered(:)
        character(len=256), allocatable :: seen(:)
        integer :: n_lines, n_main, u, iostat, i, n_seen

        allocate (lines(MAX_LINT_LINES), lowered(MAX_LINT_LINES), seen(2048))
        n_seen = 0

        n_lines = 0
        open (newunit=u, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) then
            deallocate (lines, lowered, seen)
            return
        end if

        do
            n_lines = n_lines + 1
            if (n_lines > MAX_LINT_LINES) then
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

        ! Append the bodies of any `include 'file'` so a symbol used only inside
        ! an included fragment is not mis-reported as an unused import. Usage
        ! scanning sees the extra lines; use-line detection still fires only on
        ! the main file's own lines (1..n_main). The seen set is shared across
        ! all expansions so each fragment is appended at most once.
        n_main = n_lines
        call append_include_bodies(filename, lines, n_lines, seen, n_seen)

        ! Append sibling module/submodule bodies (and their includes) so imports
        ! consumed only through submodule scope inheritance count as used.
        do i = 1, n_extra
            call append_file_body(trim(extra_files(i)), lines, n_lines)
            call append_include_bodies(trim(extra_files(i)), lines, n_lines, &
                seen, n_seen)
        end do

        do i = 1, n_lines
            lowered(i) = to_lower(lines(i))
        end do

        ! A module with no bare `private` statement is default-public: its
        ! USE-associated names are re-exported to anything that uses it, so an
        ! import that looks unused here may still be a deliberate re-export.
        ! Skip such modules. Programs and submodules never re-export, so they
        ! are always linted.
        if (module_reexports_imports(lowered, n_main)) then
            deallocate (lines, lowered, seen)
            return
        end if

        do i = 1, n_main
            call check_use_line(filename, i, lowered, n_lines, &
                findings, n_findings)
        end do
        deallocate (lines, lowered)
    end subroutine lint_file_ex

    subroutine append_file_body(filename, lines, n_lines)
        !! Append every line of filename to the usage-scan buffer.
        character(len=*), intent(in) :: filename
        character(len=1024), intent(inout) :: lines(:)
        integer, intent(inout) :: n_lines

        character(len=1024) :: line
        integer :: u, iostat

        open (newunit=u, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) return
        do
            if (n_lines >= size(lines)) exit
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            n_lines = n_lines + 1
            lines(n_lines) = line
        end do
        close (u)
    end subroutine append_file_body

    subroutine append_include_bodies(filename, lines, n_lines, seen, n_seen)
        !! Append the body of every `include 'frag'` (resolved relative to the
        !! source's directory) so symbol-usage scanning covers included code.
        !! Expansion is transitive: a fragment that itself includes another is
        !! followed too. seen carries across calls so each fragment is appended
        !! at most once (no duplication when many siblings share includes), which
        !! also breaks circular includes.
        character(len=*), intent(in) :: filename
        character(len=1024), intent(inout) :: lines(:)
        integer, intent(inout) :: n_lines
        character(len=256), intent(inout) :: seen(:)
        integer, intent(inout) :: n_seen

        character(len=1024) :: line, incpath, dir, search_dir
        character(len=:), allocatable :: incname
        integer :: i, slash, parent_slash, u, iostat, k
        logical :: dup

        slash = index(filename, '/', back=.true.)
        dir = ''
        if (slash > 0) dir = filename(1:slash)

        i = 1
        ! Walk every line, including ones appended below, so nested includes are
        ! expanded as they come into view.
        do while (i <= n_lines)
            line = adjustl(lines(i))
            call include_target(line, incname)
            if (.not. allocated(incname)) then
                i = i + 1
                cycle
            end if

            dup = .false.
            do k = 1, n_seen
                if (trim(seen(k)) == trim(incname)) then
                    dup = .true.
                    exit
                end if
            end do
            if (dup) then
                i = i + 1
                cycle
            end if
            if (n_seen < size(seen)) then
                n_seen = n_seen + 1
                seen(n_seen) = incname
            end if

            search_dir = dir
            iostat = 1
            do while (len_trim(search_dir) > 0)
                incpath = trim(search_dir)//trim(incname)
                open (newunit=u, file=trim(incpath), status='old', iostat=iostat)
                if (iostat == 0) exit
                parent_slash = 0
                if (len_trim(search_dir) > 1) then
                    parent_slash = index( &
                        search_dir(1:len_trim(search_dir) - 1), '/', back=.true.)
                end if
                if (parent_slash == 0) exit
                search_dir = search_dir(1:parent_slash)
            end do
            if (iostat /= 0) then
                open (newunit=u, file=trim(incname), status='old', iostat=iostat)
                if (iostat /= 0) then
                    i = i + 1
                    cycle
                end if
            end if
            do
                if (n_lines >= size(lines)) exit
                read (u, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                n_lines = n_lines + 1
                lines(n_lines) = line
            end do
            close (u)
            i = i + 1
        end do
    end subroutine append_include_bodies

    logical function module_reexports_imports(lowered, n_main) result(reexports)
        !! True when the file's first program unit is a module (not a submodule
        !! or program) that has no bare `private` statement. Such a module is
        !! default-public, so its USE-associated names are re-exported and must
        !! not be reported as unused.
        character(len=1024), intent(in) :: lowered(:)
        integer, intent(in) :: n_main

        character(len=1024) :: line
        integer :: i, bang
        logical :: is_module, header_seen, has_bare_private

        reexports = .false.
        is_module = .false.
        header_seen = .false.
        has_bare_private = .false.

        do i = 1, n_main
            line = lowered(i)
            call strip_leading(line)
            bang = index(line, '!')
            if (bang > 0) line = line(1:bang - 1)
            call strip_trailing(line)

            if (.not. header_seen) then
                if (starts_with(line, 'submodule')) then
                    return
                else if (starts_with(line, 'module ') .and. &
                        .not. starts_with(line, 'module procedure')) then
                    is_module = .true.
                    header_seen = .true.
                end if
            end if

            if (trim(line) == 'private') has_bare_private = .true.
        end do

        reexports = is_module .and. .not. has_bare_private
    end function module_reexports_imports

    subroutine include_target(line, incname)
        !! Extract frag from Fortran or C-preprocessor include; else unset.
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: incname

        character(len=1) :: q
        integer :: lo, hi

        if (allocated(incname)) deallocate (incname)
        if (.not. starts_with(to_lower(line), 'include ') .and. &
            .not. starts_with(to_lower(line), '#include ')) return
        lo = index(line, "'")
        if (lo == 0) lo = index(line, '"')
        if (lo == 0) return
        q = line(lo:lo)
        hi = index(line(lo + 1:), q)
        if (hi == 0) return
        incname = line(lo + 1:lo + hi - 1)
    end subroutine include_target

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
        if (sym_len > 10) then
            if (search_sym(1:9) == 'operator(' .and. &
                search_sym(sym_len:sym_len) == ')') then
                search_sym = search_sym(10:sym_len - 1)
                sym_len = len_trim(search_sym)
            end if
        end if
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
        !! Collect *.f90/*.F90/*.f/*.F under dir, sorted, excluding generated
        !! and dependency trees. Replaces the find | sort pipeline.
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
                if (skip_generated_source(dir, hits(i))) cycle
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

    logical function skip_generated_source(root, path)
        character(len=*), intent(in) :: root, path

        character(len=512) :: rel, first, padded
        integer :: root_len, slash

        rel = trim(path)
        root_len = len_trim(root)
        if (root_len > 0 .and. len_trim(path) > root_len) then
            if (path(1:root_len) == root(1:root_len)) then
                rel = path(root_len + 1:)
                if (rel(1:1) == '/') rel = rel(2:)
            end if
        end if

        first = rel
        slash = index(first, '/')
        if (slash > 0) first = first(1:slash - 1)

        skip_generated_source = .true.
        if (index(trim(first), 'build') == 1) return
        if (trim(first) == 'SRC') return
        if (len_trim(first) > 0) then
            if (first(1:1) == '.') return
        end if

        padded = '/'//trim(rel)//'/'
        if (index(padded, '/build/') > 0) return
        if (index(padded, '/CMakeFiles/') > 0) return
        if (index(padded, '/_deps/') > 0) return
        if (index(padded, '/dependencies/') > 0) return
        if (index(padded, '/deps-src/') > 0) return
        if (index(padded, '/.git/') > 0) return
        if (index(padded, '/.venv/') > 0) return
        if (index(padded, '/venv/') > 0) return
        if (index(padded, '/site-packages/') > 0) return
        if (index(padded, '/node_modules/') > 0) return

        skip_generated_source = .false.
    end function skip_generated_source

    subroutine lint_dir(dir, findings, n_findings)
        character(len=*), intent(in) :: dir
        type(lint_finding_t), intent(out) :: findings(MAX_FINDINGS)
        integer, intent(out) :: n_findings

        character(len=512), allocatable :: files(:), extra(:)
        character(len=128), allocatable :: roots(:)
        integer :: n_files, i, j, n_extra

        n_findings = 0
        call collect_fortran_sources(dir, files, n_files)

        ! Root module identity per file: the module it defines, or for a
        ! submodule its root ancestor module. Files sharing a root form one
        ! scope for unused-import purposes (a submodule sees its module's
        ! imports), so each is linted with the others appended to its usage scan.
        allocate (roots(max(1, n_files)), extra(max(1, n_files)))
        do i = 1, n_files
            call file_root_module(trim(files(i)), roots(i))
        end do

        do i = 1, n_files
            if (len_trim(files(i)) == 0) cycle
            n_extra = 0
            if (len_trim(roots(i)) > 0) then
                do j = 1, n_files
                    if (j == i) cycle
                    if (len_trim(roots(j)) == 0) cycle
                    if (roots(j) == roots(i)) then
                        n_extra = n_extra + 1
                        extra(n_extra) = files(j)
                    end if
                end do
            end if
            call lint_file_ex(trim(files(i)), extra, n_extra, findings, n_findings)
        end do
        deallocate (roots, extra)
    end subroutine lint_dir

    subroutine file_root_module(filename, root)
        !! Root module identity (lowercased): the name of the module the file
        !! defines, or for a `submodule(parent...) name` the root parent module
        !! (the part before any ':'). Empty when the file defines neither.
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: root

        character(len=1024) :: line, low
        integer :: u, iostat, lo, hi, colon, scanned

        root = ''
        open (newunit=u, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) return
        scanned = 0
        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            scanned = scanned + 1
            if (scanned > 500) exit
            low = to_lower(line)
            call strip_leading(low)
            if (starts_with(low, 'submodule')) then
                lo = index(low, '(')
                hi = index(low, ')')
                if (lo > 0 .and. hi > lo) then
                    root = adjustl(low(lo + 1:hi - 1))
                    colon = index(root, ':')
                    if (colon > 0) root = root(1:colon - 1)
                    call strip_trailing(root)
                    call strip_leading(root)
                end if
                exit
            else if (starts_with(low, 'module ')) then
                if (.not. starts_with(low, 'module procedure')) then
                    root = adjustl(low(8:))
                    ! keep only the first token (the module name)
                    hi = index(trim(root), ' ')
                    if (hi > 0) root = root(1:hi - 1)
                    call strip_trailing(root)
                end if
                exit
            end if
        end do
        close (u)
    end subroutine file_root_module

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
        integer :: n_files, i, n_removed, k, local_n
        type(lint_warning_t), allocatable :: local_w(:)

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

        ! Each file's -fsyntax-only pass is independent (dependency interfaces come
        ! from the build's -I dirs, not the throwaway -J output), so compile them in
        ! parallel: a per-thread buffer collects findings, merged under a critical
        ! section. process_run and make_tmpfile are async-signal-safe and OpenMP-safe,
        ! the same primitives the build's parallel compile uses.
        !$omp parallel default(shared) private(i, k, local_w, local_n)
        allocate (local_w(MAX_FILE_WARN))
        !$omp do schedule(dynamic)
        do i = 1, n_files
            if (len_trim(files(i)) /= 0) then
                local_n = 0
                call lint_file_compiler(trim(files(i)), mod_flags, local_w, local_n)
                call lint_file_lengths(trim(files(i)), local_w, local_n)
                call lint_file_shortcircuit(trim(files(i)), local_w, local_n)
                !$omp critical (lint_merge)
                do k = 1, local_n
                    if (n_warnings < MAX_WARNINGS) then
                        n_warnings = n_warnings + 1
                        warnings(n_warnings) = local_w(k)
                    end if
                end do
                !$omp end critical (lint_merge)
            end if
        end do
        !$omp end do
        deallocate (local_w)
        !$omp end parallel

        call sort_warnings(warnings, n_warnings)
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
        type(lint_warning_t), intent(inout) :: warnings(:)
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
                if (n_warnings < size(warnings)) then
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
        type(lint_warning_t), intent(inout) :: warnings(:)
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
        ! Preprocess like the real build: source files carry #ifdef guards
        ! (optional features), which a non-preprocessed pass misreads as illegal
        ! directives.
        call argv_push(packed, n_args, '-cpp')
        call argv_push(packed, n_args, '-Wall')
        call argv_push(packed, n_args, '-Wextra')
        call argv_push(packed, n_args, '-Wno-unused')
        ! -Wcompare-reals (from -Wextra) fires on deliberate exact float idioms
        ! that have no safe rewrite: NaN tests (x /= x), exact-zero guards
        ! (x == 0.0), endpoint/step matches (t == tout, t + h /= t), and
        ! serial-vs-parallel determinism checks. It carries no signal here.
        call argv_push(packed, n_args, '-Wno-compare-reals')
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

    subroutine sort_warnings(warnings, n_warnings)
        !! Stable order by (file, line, column) so parallel collection still yields
        !! deterministic output. Insertion sort: n_warnings is small (<= MAX).
        type(lint_warning_t), intent(inout) :: warnings(:)
        integer, intent(in) :: n_warnings

        type(lint_warning_t) :: key
        integer :: i, j

        do i = 2, n_warnings
            key = warnings(i)
            j = i - 1
            do while (j >= 1)
                if (.not. warning_after(warnings(j), key)) exit
                warnings(j + 1) = warnings(j)
                j = j - 1
            end do
            warnings(j + 1) = key
        end do
    end subroutine sort_warnings

    logical function warning_after(a, b)
        !! True if a sorts after b by file, then line, then column.
        type(lint_warning_t), intent(in) :: a, b

        warning_after = .false.
        if (trim(a%file) /= trim(b%file)) then
            warning_after = trim(a%file) > trim(b%file)
        else if (a%line /= b%line) then
            warning_after = a%line > b%line
        else
            warning_after = a%column > b%column
        end if
    end function warning_after

    subroutine lint_file_shortcircuit(filepath, warnings, n_warnings)
        !! Append short-circuit-evaluation hazards found by the textual detector.
        character(len=*), intent(in) :: filepath
        type(lint_warning_t), intent(inout) :: warnings(:)
        integer, intent(inout) :: n_warnings

        integer :: hit_line(MAX_FILE_WARN), n_hits, k
        character(len=512) :: hit_msg(MAX_FILE_WARN)

        n_hits = 0
        call shortcircuit_scan_file(filepath, hit_line, hit_msg, n_hits, MAX_FILE_WARN)
        do k = 1, n_hits
            if (n_warnings >= size(warnings)) exit
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
        type(lint_warning_t), intent(inout) :: warnings(:)
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
        if (n_warnings >= size(warnings)) return

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

        starts_with = .false.
        if (len_trim(str) >= len(prefix)) starts_with = (str(1:len(prefix)) == prefix)
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
        type(lint_warning_t), intent(inout) :: warnings(:)
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

    subroutine lint_fix_dir(dir, n_removed, n_remaining)
        !! Remove unused imports in place across all sources under dir.
        !! Each pass removes up to MAX_FINDINGS imports, so several passes clear
        !! projects that exceed the per-pass cap. n_remaining is the count still
        !! flagged after fixing (symbols the rewriter could not place, e.g. on a
        !! continuation line rather than the use line itself).
        character(len=*), intent(in) :: dir
        integer, intent(out) :: n_removed, n_remaining

        type(lint_finding_t), allocatable :: findings(:)
        integer :: n_findings, pass, removed_this_pass

        n_removed = 0
        n_remaining = 0
        allocate (findings(MAX_FINDINGS))
        do pass = 1, 200
            call lint_dir(dir, findings, n_findings)
            if (n_findings == 0) exit
            call apply_findings(findings, n_findings, removed_this_pass)
            n_removed = n_removed + removed_this_pass
            if (removed_this_pass == 0) exit ! no progress; stop looping
        end do
        call lint_dir(dir, findings, n_findings)
        n_remaining = n_findings
        deallocate (findings)
    end subroutine lint_fix_dir

    subroutine apply_findings(findings, n_findings, n_removed)
        !! Apply findings file by file. lint_dir emits findings grouped by file
        !! (sources are scanned in sorted order), so equal-file findings are
        !! contiguous.
        type(lint_finding_t), intent(in) :: findings(:)
        integer, intent(in) :: n_findings
        integer, intent(out) :: n_removed

        integer :: i, j, removed_file

        n_removed = 0
        i = 1
        do while (i <= n_findings)
            j = i
            do while (j < n_findings)
                if (trim(findings(j + 1)%file) /= trim(findings(i)%file)) exit
                j = j + 1
            end do
            call fix_one_file(trim(findings(i)%file), findings(i:j), j - i + 1, &
                removed_file)
            n_removed = n_removed + removed_file
            i = j + 1
        end do
    end subroutine apply_findings

    subroutine fix_one_file(filename, ff, nf, n_removed)
        character(len=*), intent(in) :: filename
        type(lint_finding_t), intent(in) :: ff(nf)
        integer, intent(in) :: nf
        integer, intent(out) :: n_removed

        character(len=1024), allocatable :: lines(:)
        logical, allocatable :: drop(:), handled(:)
        character(len=MAX_SYM_LEN) :: flagged(MAX_SYMS)
        integer :: n_lines, u, iostat, i, k, nflag, removed_here, target_line
        character(len=:), allocatable :: newline
        logical :: delete_line, changed

        n_removed = 0
        allocate (lines(20000), drop(20000), handled(20000))
        drop = .false.
        handled = .false.
        n_lines = 0
        open (newunit=u, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) then
            deallocate (lines, drop, handled)
            return
        end if
        do
            if (n_lines >= size(lines)) exit
            read (u, '(a)', iostat=iostat) lines(n_lines + 1)
            if (iostat /= 0) exit
            n_lines = n_lines + 1
        end do
        close (u)

        changed = .false.
        do i = 1, nf
            target_line = ff(i)%line
            if (target_line < 1 .or. target_line > n_lines) cycle
            if (handled(target_line)) cycle
            handled(target_line) = .true.

            ! A use line may carry several unused symbols; gather them all and
            ! rewrite the line once.
            nflag = 0
            do k = 1, nf
                if (ff(k)%line /= target_line) cycle
                if (nflag >= MAX_SYMS) exit
                nflag = nflag + 1
                flagged(nflag) = normalize_sym(ff(k)%symbol)
            end do

            call rewrite_use_line(lines(target_line), flagged, nflag, newline, &
                delete_line, removed_here)
            if (removed_here == 0) cycle
            n_removed = n_removed + removed_here
            changed = .true.
            if (delete_line) then
                drop(target_line) = .true.
            else
                lines(target_line) = newline
            end if
        end do

        if (changed) then
            open (newunit=u, file=filename, status='replace', iostat=iostat)
            if (iostat == 0) then
                do i = 1, n_lines
                    if (drop(i)) cycle
                    write (u, '(a)') trim(lines(i))
                end do
                close (u)
            end if
        end if
        deallocate (lines, drop, handled)
    end subroutine fix_one_file

    subroutine rewrite_use_line(orig, flagged, nflag, newline, delete_line, &
            n_removed)
        !! Drop the flagged symbols from a `use ..., only:` line, preserving the
        !! module prefix, indentation, a trailing continuation '&', and any
        !! trailing comment. delete_line is set when the only-list becomes empty
        !! and the statement does not continue onto the next line.
        character(len=*), intent(in) :: orig
        character(len=MAX_SYM_LEN), intent(in) :: flagged(:)
        integer, intent(in) :: nflag
        character(len=:), allocatable, intent(out) :: newline
        logical, intent(out) :: delete_line
        integer, intent(out) :: n_removed

        character(len=1024) :: lowered, rest, comment, token
        character(len=:), allocatable :: prefix, kept
        integer :: op, cpos, start, comma_pos, i
        logical :: drop_tok, has_amp

        newline = ''
        delete_line = .false.
        n_removed = 0

        lowered = to_lower(orig)
        op = index(lowered, 'only:')
        if (op == 0) return
        prefix = orig(1:op + 4) ! through the ':' of 'only:'
        rest = orig(op + 5:)

        comment = ''
        cpos = index(rest, '!')
        if (cpos > 0) then
            comment = rest(cpos:)
            rest = rest(1:cpos - 1)
        end if

        has_amp = .false.
        call strip_trailing(rest)
        if (len_trim(rest) > 0) then
            if (rest(len_trim(rest):len_trim(rest)) == '&') then
                has_amp = .true.
                rest(len_trim(rest):len_trim(rest)) = ' '
            end if
        end if

        kept = ''
        start = 1
        do
            if (start > len_trim(rest)) exit
            comma_pos = index(rest(start:), ',')
            if (comma_pos > 0) then
                token = rest(start:start + comma_pos - 2)
                start = start + comma_pos
            else
                token = rest(start:)
                start = len_trim(rest) + 1
            end if
            call strip_leading(token)
            call strip_trailing(token)
            if (len_trim(token) == 0) cycle
            drop_tok = .false.
            do i = 1, nflag
                if (normalize_sym(token) == flagged(i)) then
                    drop_tok = .true.
                    exit
                end if
            end do
            if (drop_tok) then
                n_removed = n_removed + 1
            else if (len_trim(kept) == 0) then
                kept = trim(token)
            else
                kept = kept//', '//trim(token)
            end if
        end do

        if (n_removed == 0) return

        if (len_trim(kept) == 0 .and. .not. has_amp) then
            delete_line = .true.
            return
        end if

        if (len_trim(kept) == 0) then
            ! Whole first-line list removed but the statement continues; the
            ! continuation supplies the remaining symbols.
            newline = trim(prefix)//' &'
        else if (has_amp) then
            ! Kept symbols precede a continuation, so the list needs its
            ! trailing comma before '&'.
            newline = trim(prefix)//' '//trim(kept)//', &'
        else
            newline = trim(prefix)//' '//trim(kept)
        end if
        if (len_trim(comment) > 0) newline = newline//' '//trim(comment)
    end subroutine rewrite_use_line

    pure function normalize_sym(s) result(norm)
        !! Lowercase and strip all blanks so renames compare regardless of the
        !! spacing around '=>'.
        character(len=*), intent(in) :: s
        character(len=MAX_SYM_LEN) :: norm

        character(len=len(s)) :: low
        integer :: i, k

        low = to_lower(s)
        norm = ''
        k = 0
        do i = 1, len_trim(low)
            if (low(i:i) == ' ' .or. low(i:i) == char(9)) cycle
            k = k + 1
            if (k > MAX_SYM_LEN) exit
            norm(k:k) = low(i:i)
        end do
    end function normalize_sym

end module fo_lint
