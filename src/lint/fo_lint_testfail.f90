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
    !! procedures, and the bodies of any files it `include`s (transitively, to a
    !! small depth). It does NOT follow calls into modules: a test whose only
    !! exit lives in a shared helper module is reported even though it can fail.
    !! Such a helper is one `include` or one inline `error stop` away from
    !! satisfying the rule.
    use fo_lint_lex, only: lex_read_logical_line, is_ident_char, lower_ch
    implicit none
    private
    public :: testfail_scan_file

    integer, parameter :: MAXLEN = 4096
    integer, parameter :: MAX_INCLUDE_DEPTH = 4

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
        integer :: prog_line

        call find_program_unit(filepath, prog_name, prog_line)
        if (prog_line == 0) return
        ! bench_* programs are fo's benchmark targets, not tests: they measure
        ! timing and have nothing to assert.
        if (is_bench_program(prog_name)) return
        if (file_has_failure_path(filepath, 0)) return
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

    recursive function file_has_failure_path(filepath, depth) result(found)
        !! True if this file, or anything it includes, can terminate nonzero.
        character(len=*), intent(in) :: filepath
        integer, intent(in) :: depth
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
            if (line_exits_nonzero(code)) then
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
            if (file_has_failure_path(trim(incpath), depth + 1)) then
                found = .true.
                exit
            end if
        end do
        close (u)
    end function file_has_failure_path

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

    logical function line_exits_nonzero(code)
        !! True if the masked logical line holds a statement that can terminate
        !! the program with a nonzero status.
        character(len=*), intent(in) :: code

        line_exits_nonzero = has_error_stop(code)
        if (.not. line_exits_nonzero) line_exits_nonzero = has_nonzero_stop(code)
        if (.not. line_exits_nonzero) line_exits_nonzero = has_abort_call(code)
        if (.not. line_exits_nonzero) line_exits_nonzero = has_nonzero_exit(code)
    end function line_exits_nonzero

    logical function has_error_stop(code)
        !! `error stop` terminates with a nonzero status whether or not a stop
        !! code is given, so the code itself does not matter here.
        character(len=*), intent(in) :: code

        character(len=128) :: tok
        integer :: p, hit, after

        has_error_stop = .false.
        p = 1
        do
            call word_pos(code, 'error', p, hit)
            if (hit == 0) return
            call next_token(code, hit + 5, tok, after)
            if (trim(tok) == 'stop') then
                has_error_stop = .true.
                return
            end if
            p = hit + 5
        end do
    end function has_error_stop

    logical function has_nonzero_stop(code)
        character(len=*), intent(in) :: code

        integer :: p, hit

        has_nonzero_stop = .false.
        p = 1
        do
            call word_pos(code, 'stop', p, hit)
            if (hit == 0) return
            if (code_arg_nonzero(code, hit + 4)) then
                has_nonzero_stop = .true.
                return
            end if
            p = hit + 4
        end do
    end function has_nonzero_stop

    logical function has_abort_call(code)
        !! `call abort()` raises SIGABRT: the process dies with a nonzero status.
        character(len=*), intent(in) :: code

        has_abort_call = called_word(code, 'abort', 0) > 0
    end function has_abort_call

    logical function has_nonzero_exit(code)
        !! `call exit(status)` from the GNU extension family.
        character(len=*), intent(in) :: code

        integer :: p, open_paren

        has_nonzero_exit = .false.
        p = 1
        do
            open_paren = called_word(code, 'exit', p)
            if (open_paren == 0) return
            if (code_arg_nonzero(code, open_paren + 1)) then
                has_nonzero_exit = .true.
                return
            end if
            p = open_paren
        end do
    end function has_nonzero_exit

    integer function called_word(code, word, from)
        !! Position of the '(' that follows `call WORD` at or after from, or the
        !! position just past WORD when it takes no argument list. 0 if `call
        !! WORD` does not occur. Requiring the `call` keyword keeps the loop
        !! statement `exit` and any variable named exit out of the match.
        character(len=*), intent(in) :: code
        character(len=*), intent(in) :: word
        integer, intent(in) :: from

        character(len=128) :: prev
        integer :: p, q, hit

        called_word = 0
        p = max(from, 1)
        do
            call word_pos(code, word, p, hit)
            if (hit == 0) return
            call prev_token(code, hit - 1, prev)
            if (trim(prev) == 'call') then
                q = skip_blanks(code, hit + len(word))
                called_word = q
                if (q <= len_trim(code)) then
                    if (code(q:q) /= '(') called_word = hit + len(word)
                end if
                return
            end if
            p = hit + len(word)
        end do
    end function called_word

    logical function code_arg_nonzero(code, from)
        !! Read the stop code or exit status starting at from and decide whether
        !! it can be nonzero. Nothing there (a bare `stop`, or a character stop
        !! code, which masking blanked) means the program exits 0. A literal 0
        !! means the same. Anything else - a nonzero literal, a variable, an
        !! expression - is assumed to be able to fail.
        character(len=*), intent(in) :: code
        integer, intent(in) :: from

        character(len=128) :: tok
        integer :: p, after, value, iostat

        code_arg_nonzero = .false.
        p = skip_blanks(code, from)
        if (p > len_trim(code)) return
        call next_token(code, p, tok, after)
        if (len_trim(tok) == 0) then
            code_arg_nonzero = .true.
            return
        end if
        if (.not. all_digits(tok)) then
            code_arg_nonzero = .true.
            return
        end if
        read (tok, *, iostat=iostat) value
        if (iostat /= 0) then
            code_arg_nonzero = .true.
            return
        end if
        code_arg_nonzero = value /= 0
    end function code_arg_nonzero

    subroutine word_pos(code, word, from, pos)
        !! Position of word as a whole word at or after from; 0 if absent.
        character(len=*), intent(in) :: code, word
        integer, intent(in) :: from
        integer, intent(out) :: pos

        integer :: base, p, after, L

        pos = 0
        L = len_trim(code)
        base = max(from, 1) - 1
        do
            if (base >= L) return
            p = index(code(base + 1:L), word)
            if (p == 0) return
            p = base + p
            after = p + len(word)
            if (boundary_before(code, p)) then
                if (after > L) then
                    pos = p
                    return
                else if (.not. is_ident_char(code(after:after))) then
                    pos = p
                    return
                end if
            end if
            base = p
        end do
    end subroutine word_pos

    subroutine next_token(code, from, tok, after)
        !! First identifier-like token at or after from. tok is empty when the
        !! next non-blank character cannot start one (or input ends); after is
        !! the position just past the token.
        character(len=*), intent(in) :: code
        integer, intent(in) :: from
        character(len=*), intent(out) :: tok
        integer, intent(out) :: after

        integer :: p, s, L

        tok = ''
        L = len_trim(code)
        p = skip_blanks(code, from)
        after = p
        if (p > L) return
        if (.not. is_ident_char(code(p:p))) return
        s = p
        do
            if (p > L) exit
            if (.not. is_ident_char(code(p:p))) exit
            p = p + 1
        end do
        tok = code(s:p - 1)
        after = p
    end subroutine next_token

    subroutine prev_token(code, before, tok)
        !! Identifier-like token ending at or before position before; empty when
        !! the preceding non-blank character cannot end one.
        character(len=*), intent(in) :: code
        integer, intent(in) :: before
        character(len=*), intent(out) :: tok

        integer :: p, e

        tok = ''
        p = before
        do
            if (p < 1) return
            if (code(p:p) /= ' ') exit
            p = p - 1
        end do
        if (.not. is_ident_char(code(p:p))) return
        e = p
        do
            if (p < 1) exit
            if (.not. is_ident_char(code(p:p))) exit
            p = p - 1
        end do
        tok = code(p + 1:e)
    end subroutine prev_token

    integer function skip_blanks(code, from)
        character(len=*), intent(in) :: code
        integer, intent(in) :: from

        integer :: L

        L = len_trim(code)
        skip_blanks = max(from, 1)
        do
            if (skip_blanks > L) return
            if (code(skip_blanks:skip_blanks) /= ' ') return
            skip_blanks = skip_blanks + 1
        end do
    end function skip_blanks

    logical function boundary_before(code, pos)
        character(len=*), intent(in) :: code
        integer, intent(in) :: pos

        boundary_before = .true.
        if (pos <= 1) return
        boundary_before = .not. is_ident_char(code(pos - 1:pos - 1))
    end function boundary_before

    logical function all_digits(tok)
        character(len=*), intent(in) :: tok
        integer :: i, ic

        all_digits = len_trim(tok) > 0
        do i = 1, len_trim(tok)
            ic = iachar(tok(i:i))
            if (ic < iachar('0') .or. ic > iachar('9')) then
                all_digits = .false.
                return
            end if
        end do
    end function all_digits

    function lower_str(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i

        do i = 1, len(s)
            out(i:i) = lower_ch(s(i:i))
        end do
    end function lower_str

end module fo_lint_testfail
