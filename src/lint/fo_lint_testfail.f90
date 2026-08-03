module fo_lint_testfail
    !! Detect a test program that cannot fail the build. A test that tallies its
    !! own results, prints "some tests failed", and then runs off the end of the
    !! program exits 0: the suite stays green no matter what the assertions
    !! found, so every correctness claim resting on it is unverified. The rule
    !! fires when a program unit contains no statement that can terminate with a
    !! nonzero status.
    !!
    !! What counts as a failure path, matched on masked code (comments and
    !! string literals removed, see fo_lint_lex):
    !!   - `error stop`, with or without a code (always nonzero)
    !!   - `stop` with a nonzero integer, a variable, or an expression
    !!   - `call exit(...)` with a nonzero or non-constant argument
    !!   - `call abort()`
    !! A bare `stop`, `stop 0`, and a character stop code (`stop 'failed'`,
    !! which gfortran and ifort exit 0 on) are deliberately NOT failure paths:
    !! they are exactly the vacuous endings this rule exists to catch.
    !!
    !! The search covers the file's own code, including its contained
    !! procedures, the bodies of any files it `include`s (transitively, to a
    !! small depth), and calls into the project's own module procedures: a test
    !! whose only exit is `call test_suite_exit(suite)` passes when that helper
    !! holds the `stop 1`. Which helpers those are is decided by
    !! fo_lint_failpath, which scans src/, app/ and test/ once per project. A
    !! call to a helper that cannot itself fail is still no failure path, so
    !! the rule is not silenced by a test that merely calls something.
    use fo_lint_lex, only: lex_read_logical_line, is_ident_char, lower_ch, &
        next_token, skip_blanks
    use fo_lint_failpath, only: failpath_line_exits_nonzero, &
        failpath_line_calls_helper, failpath_load_helpers
    implicit none
    private
    public :: testfail_scan_file

    integer, parameter :: MAXLEN = 4096
    integer, parameter :: MAX_INCLUDE_DEPTH = 4
    integer, parameter :: MAX_ROOT_DEPTH = 24
    integer, parameter :: MAX_SHADOWED = 128

contains

    subroutine testfail_scan_file(filepath, lines_no, msgs, n, cap)
        !! Scan one Fortran source; append one (line, message) if it holds a
        !! program unit with no way to exit nonzero. Callers restrict this to
        !! test sources: a program under src/ or app/ is not a test and may
        !! legitimately always succeed.
        character(len=*), intent(in) :: filepath
        integer, intent(inout) :: lines_no(:)
        character(len=*), intent(inout) :: msgs(:)
        integer, intent(inout) :: n
        integer, intent(in) :: cap

        character(len=128) :: prog_name
        character(len=1024) :: root
        character(len=64) :: shadowed_names(MAX_SHADOWED)
        integer :: n_shadowed
        integer :: prog_line

        call find_program_unit(filepath, prog_name, prog_line)
        if (prog_line == 0) return
        ! bench_* programs are fo's benchmark targets, not tests: they measure
        ! timing and have nothing to assert.
        if (is_bench_program(prog_name)) return
        ! Load the project's failing helpers before any line is judged: the
        ! set has to be complete before file_has_failure_path consults it, and
        ! loading is what makes that safe under the parallel lint pass.
        call find_project_root(filepath, root)
        call failpath_load_helpers(trim(root))
        call collect_shadowed_names(filepath, shadowed_names, n_shadowed)
        if (file_has_failure_path(filepath, 0, shadowed_names, n_shadowed)) return
        if (n >= cap) return
        n = n + 1
        lines_no(n) = prog_line
        msgs(n) = "test program '"//trim(prog_name)//"' cannot fail the "// &
            "build: no error stop, no nonzero stop, no nonzero exit "// &
            "(a test that only prints its failures always exits 0)"
    end subroutine testfail_scan_file

    subroutine find_program_unit(filepath, name, line_no)
        !! Locate the first `program NAME` statement. line_no is 0 when the file
        !! holds no program unit (a module, a submodule, an include fragment).
        character(len=*), intent(in) :: filepath
        character(len=*), intent(out) :: name
        integer, intent(out) :: line_no

        character(len=MAXLEN) :: code
        character(len=128) :: tok
        integer :: u, iostat, phys_no, start_line, after, name_end

        name = ''
        line_no = 0
        open (newunit=u, file=filepath, status='old', iostat=iostat)
        if (iostat /= 0) return

        phys_no = 0
        do
            call lex_read_logical_line(u, code, start_line, phys_no, iostat)
            if (iostat /= 0) exit
            call next_token(code, 1, tok, after)
            if (trim(tok) /= 'program') cycle
            call next_token(code, after, tok, name_end)
            if (len_trim(tok) == 0) cycle
            name = tok
            line_no = start_line
            exit
        end do
        close (u)
    end subroutine find_program_unit

    logical function is_bench_program(name)
        character(len=*), intent(in) :: name

        is_bench_program = .false.
        if (len_trim(name) >= 6) is_bench_program = name(1:6) == 'bench_'
    end function is_bench_program

    recursive function file_has_failure_path(filepath, depth, shadowed_names, &
            n_shadowed) result(found)
        !! True if this file, or anything it includes, can terminate nonzero.
        character(len=*), intent(in) :: filepath
        integer, intent(in) :: depth
        character(len=*), intent(in) :: shadowed_names(:)
        integer, intent(in) :: n_shadowed
        logical :: found

        character(len=MAXLEN) :: code
        character(len=1024) :: incpath
        integer :: u, iostat, phys_no, start_line
        logical :: resolved

        found = .false.
        open (newunit=u, file=filepath, status='old', iostat=iostat)
        if (iostat /= 0) return

        phys_no = 0
        do
            call lex_read_logical_line(u, code, start_line, phys_no, iostat)
            if (iostat /= 0) exit
            if (line_is_failure_path(code(1:max(len_trim(code), 1)), &
                    shadowed_names, n_shadowed)) then
                found = .true.
                exit
            end if
        end do
        close (u)
        if (found) return
        if (depth >= MAX_INCLUDE_DEPTH) return

        ! Include directives are read from the raw text: the fragment name is a
        ! string literal, which masking blanks out.
        open (newunit=u, file=filepath, status='old', iostat=iostat)
        if (iostat /= 0) return
        do
            read (u, '(a)', iostat=iostat) code
            if (iostat /= 0) exit
            call resolve_include(filepath, code, incpath, resolved)
            if (.not. resolved) cycle
            if (file_has_failure_path(trim(incpath), depth + 1, shadowed_names, &
                    n_shadowed)) then
                found = .true.
                exit
            end if
        end do
        close (u)
    end function file_has_failure_path

    logical function line_is_failure_path(code, shadowed_names, n_shadowed)
        !! A statement that exits nonzero here, or a call to a project helper
        !! that exits nonzero there.
        character(len=*), intent(in) :: code
        character(len=*), intent(in) :: shadowed_names(:)
        integer, intent(in) :: n_shadowed

        line_is_failure_path = failpath_line_exits_nonzero(code)
        if (line_is_failure_path) return
        line_is_failure_path = failpath_line_calls_helper(code, shadowed_names, &
            n_shadowed)
    end function line_is_failure_path

    subroutine collect_shadowed_names(filepath, names, n_names)
        !! Collect data objects declared by a test source. A declaration is
        !! enough to distinguish an array or substring reference from a
        !! function call when both share a helper's name; the rule remains
        !! conservative for names it cannot resolve.
        character(len=*), intent(in) :: filepath
        character(len=*), intent(out) :: names(:)
        integer, intent(out) :: n_names

        character(len=MAXLEN) :: code
        integer :: u, iostat, phys_no, start_line

        names = ''
        n_names = 0
        open (newunit=u, file=filepath, status='old', iostat=iostat)
        if (iostat /= 0) return
        phys_no = 0
        do
            call lex_read_logical_line(u, code, start_line, phys_no, iostat)
            if (iostat /= 0) exit
            call collect_declaration_names(code, names, n_names)
        end do
        close (u)
    end subroutine collect_shadowed_names

    subroutine collect_declaration_names(code, names, n_names)
        character(len=*), intent(in) :: code
        character(len=*), intent(inout) :: names(:)
        integer, intent(inout) :: n_names

        character(len=64) :: first, name
        integer :: p, after, colon, depth, L

        call next_token(code, 1, first, after)
        if (.not. is_data_declaration(first)) return
        colon = index(code, '::')
        if (colon == 0) return
        L = len_trim(code)
        p = colon + 2
        do
            p = skip_blanks(code, p)
            if (p > L) return
            call next_token(code, p, name, after)
            if (len_trim(name) == 0) return
            call add_shadowed_name(name, names, n_names)
            p = after
            depth = 0
            do while (p <= L)
                if (code(p:p) == '(') then
                    depth = depth + 1
                else if (code(p:p) == ')') then
                    depth = max(depth - 1, 0)
                else if (code(p:p) == ',' .and. depth == 0) then
                    exit
                end if
                p = p + 1
            end do
            if (p > L) return
            p = p + 1
        end do
    end subroutine collect_declaration_names

    logical function is_data_declaration(first)
        character(len=*), intent(in) :: first

        is_data_declaration = .false.
        select case (trim(first))
        case ('integer', 'real', 'complex', 'logical', 'character', 'type', &
                'class', 'double')
            is_data_declaration = .true.
        case default
            return
        end select
    end function is_data_declaration

    subroutine add_shadowed_name(name, names, n_names)
        character(len=*), intent(in) :: name
        character(len=*), intent(inout) :: names(:)
        integer, intent(inout) :: n_names

        integer :: i

        do i = 1, n_names
            if (trim(names(i)) == trim(name)) return
        end do
        if (n_names >= size(names)) return
        n_names = n_names + 1
        names(n_names) = name
    end subroutine add_shadowed_name

    subroutine find_project_root(filepath, root)
        !! Nearest enclosing directory holding an fpm.toml, walking up from the
        !! scanned file. Empty when there is none: a source outside a project
        !! (a fixture in a temp directory) has no helper set to consult, and
        !! then the rule sees only what the file itself contains.
        character(len=*), intent(in) :: filepath
        character(len=*), intent(out) :: root

        character(len=1024) :: dir
        integer :: slash, level
        logical :: exists

        root = ''
        dir = filepath
        do level = 1, MAX_ROOT_DEPTH
            slash = index(trim(dir), '/', back=.true.)
            if (slash > 1) then
                dir = dir(1:slash - 1)
            else if (slash == 1) then
                dir = '/'
            else
                dir = '.'
            end if
            inquire (file=trim(dir)//'/fpm.toml', exist=exists)
            if (exists) then
                root = dir
                return
            end if
            if (trim(dir) == '/' .or. trim(dir) == '.') return
        end do
    end subroutine find_project_root

    subroutine resolve_include(base_file, line, incpath, resolved)
        !! If line is an `include 'frag'` (or `#include "frag"`) directive,
        !! return the fragment path, resolved next to base_file first and then
        !! as given.
        character(len=*), intent(in) :: base_file, line
        character(len=*), intent(out) :: incpath
        logical, intent(out) :: resolved

        character(len=1024) :: head, name
        integer :: slash, u, iostat

        incpath = ''
        resolved = .false.
        call include_name(line, name)
        if (len_trim(name) == 0) return

        slash = index(base_file, '/', back=.true.)
        head = ''
        if (slash > 0) head = base_file(1:slash)
        incpath = trim(head)//trim(name)
        open (newunit=u, file=trim(incpath), status='old', iostat=iostat)
        if (iostat == 0) then
            close (u)
            resolved = .true.
            return
        end if
        incpath = trim(name)
        open (newunit=u, file=trim(incpath), status='old', iostat=iostat)
        if (iostat /= 0) return
        close (u)
        resolved = .true.
    end subroutine resolve_include

    subroutine include_name(line, name)
        !! Extract frag from a line that starts with `include` or `#include`.
        !! The directive must open the statement, so a quoted include inside a
        !! fixture-writing statement is not mistaken for one.
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name

        character(len=1024) :: text
        character(len=1) :: q
        integer :: p, lo, hi

        name = ''
        text = adjustl(line)
        p = 1
        if (text(1:1) == '#') p = 2
        if (len_trim(text) < p + 7) return
        if (lower_str(text(p:p + 6)) /= 'include') return
        if (is_ident_char(text(p + 7:p + 7))) return

        lo = index(text, "'")
        if (lo == 0) lo = index(text, '"')
        if (lo == 0) return
        q = text(lo:lo)
        hi = index(text(lo + 1:), q)
        if (hi == 0) return
        name = text(lo + 1:lo + hi - 1)
    end subroutine include_name

    function lower_str(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i

        do i = 1, len(s)
            out(i:i) = lower_ch(s(i:i))
        end do
    end function lower_str

end module fo_lint_testfail
