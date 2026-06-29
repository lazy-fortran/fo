module fo_lint_shortcircuit
    !! Detect reliance on short-circuit evaluation of .and./.or.. Fortran does
    !! not guarantee operand evaluation order, so a guard operand (len/size,
    !! allocated, associated, present, or an index bound) does not protect a
    !! later operand that subscripts or dereferences the same name. Such code
    !! reads out of bounds, derefs null, or touches unallocated storage
    !! depending on the compiler. The detector is deliberately scoped: it fires
    !! only when the guard and the protected access name the SAME variable, so
    !! `len(a) > 0 .and. n < size(b)` (independent operands) is left alone.
    implicit none
    private
    public :: shortcircuit_scan_file

    integer, parameter :: MAXLEN = 4096

contains

    subroutine shortcircuit_scan_file(filepath, lines_no, msgs, n, cap)
        !! Scan one Fortran source, appending one (line, message) per hazard.
        character(len=*), intent(in) :: filepath
        integer, intent(inout) :: lines_no(:)
        character(len=*), intent(inout) :: msgs(:)
        integer, intent(inout) :: n
        integer, intent(in) :: cap

        character(len=MAXLEN) :: phys, code, logical_buf
        integer :: u, iostat, phys_no, start_line
        logical :: cont, continuing

        open (newunit=u, file=filepath, status='old', iostat=iostat)
        if (iostat /= 0) return

        logical_buf = ''
        continuing = .false.
        start_line = 0
        phys_no = 0
        do
            read (u, '(a)', iostat=iostat) phys
            if (iostat /= 0) exit
            phys_no = phys_no + 1
            call mask_code(phys, code)
            call rstrip(code)
            cont = ends_with_amp(code)
            if (cont) call drop_trailing_amp(code)
            if (continuing) then
                call drop_leading_amp(code)
            else
                start_line = phys_no
            end if
            call append_joined(logical_buf, code)
            if (cont) then
                continuing = .true.
            else
                call scan_logical(logical_buf, start_line, lines_no, msgs, n, cap)
                logical_buf = ''
                continuing = .false.
            end if
        end do
        close (u)
    end subroutine shortcircuit_scan_file

    subroutine append_joined(buf, code)
        character(len=*), intent(inout) :: buf
        character(len=*), intent(in) :: code
        if (len_trim(buf) == 0) then
            buf = adjustl(code)
        else if (len_trim(buf) + len_trim(code) + 1 <= len(buf)) then
            buf = trim(buf)//' '//trim(adjustl(code))
        end if
    end subroutine append_joined

    subroutine mask_code(line, code)
        !! Copy line into code with comments and string literals blanked and the
        !! rest lowercased, so tokens inside strings never match. Doubled quotes
        !! inside a literal are the Fortran escape and stay inside the string.
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: code
        integer :: i, L
        character(len=1) :: c, q
        logical :: in_str

        code = ''
        L = len(line)
        in_str = .false.
        q = ' '
        i = 1
        do
            if (i > L) exit
            c = line(i:i)
            if (.not. in_str) then
                if (c == '!') exit
                if (c == '''' .or. c == '"') then
                    in_str = .true.
                    q = c
                    code(i:i) = ' '
                else
                    code(i:i) = lower_ch(c)
                end if
            else
                if (c == q) then
                    if (i < L) then
                        if (line(i + 1:i + 1) == q) then
                            code(i + 1:i + 1) = ' '
                            i = i + 1
                        else
                            in_str = .false.
                        end if
                    else
                        in_str = .false.
                    end if
                end if
                code(i:i) = ' '
            end if
            i = i + 1
        end do
    end subroutine mask_code

    subroutine scan_logical(code, start_line, lines_no, msgs, n, cap)
        !! Walk a joined logical line; for every .and./.or. pair the operand to
        !! its left is the guard candidate and the operand to its right the
        !! protected access.
        character(len=*), intent(in) :: code
        integer, intent(in) :: start_line
        integer, intent(inout) :: lines_no(:)
        character(len=*), intent(inout) :: msgs(:)
        integer, intent(inout) :: n
        integer, intent(in) :: cap

        character(len=MAXLEN) :: left, right
        character(len=128) :: var, kind
        integer :: i, ol, L
        logical :: hazard

        L = len_trim(code)
        i = 1
        do
            if (i > L) exit
            ol = op_at(code, i, L)
            if (ol > 0) then
                call extract_left(code, i - 1, left)
                call extract_right(code, i + ol, L, right)
                call analyze_pair(left, right, hazard, var, kind)
                if (hazard) call record(start_line, var, kind, lines_no, msgs, n, cap)
                i = i + ol
            else
                i = i + 1
            end if
        end do
    end subroutine scan_logical

    subroutine record(line_no, var, kind, lines_no, msgs, n, cap)
        integer, intent(in) :: line_no
        character(len=*), intent(in) :: var, kind
        integer, intent(inout) :: lines_no(:)
        character(len=*), intent(inout) :: msgs(:)
        integer, intent(inout) :: n
        integer, intent(in) :: cap

        if (n >= cap) return
        n = n + 1
        lines_no(n) = line_no
        msgs(n) = "short-circuit reliance: "//trim(kind)//" guard on '"// &
            trim(var)//"' may not protect its use ("// &
            ".and./.or. operand order is not guaranteed; split the test)"
    end subroutine record

    subroutine analyze_pair(left, right, hazard, var, kind)
        character(len=*), intent(in) :: left, right
        logical, intent(out) :: hazard
        character(len=*), intent(out) :: var, kind

        character(len=128) :: idx1, idx2
        logical :: bound

        hazard = .false.
        var = ''
        kind = ''
        ! Evaluate the length-family guards one at a time: each writes var, so a
        ! combined .or. would leave var set by whichever ran last, not the one
        ! that matched (the very hazard this module reports).
        bound = guard_var(left, 'len_trim(', var)
        if (.not. bound) bound = guard_var(left, 'len(', var)
        if (.not. bound) bound = guard_var(left, 'size(', var)
        if (bound) then
            if (has_subscript(right, var)) then
                hazard = .true.
                kind = 'length'
                return
            end if
        end if
        if (guard_var(left, 'allocated(', var)) then
            if (has_subscript(right, var) .or. has_member(right, var)) then
                hazard = .true.
                kind = 'allocated'
                return
            end if
        end if
        if (guard_var(left, 'associated(', var)) then
            if (has_member(right, var) .or. has_subscript(right, var)) then
                hazard = .true.
                kind = 'associated'
                return
            end if
        end if
        if (guard_var(left, 'present(', var)) then
            if (has_word(right, var)) then
                hazard = .true.
                kind = 'present'
                return
            end if
        end if
        call index_guard_vars(left, idx1, idx2)
        if (len_trim(idx1) > 0) then
            if (has_index_use(right, idx1)) then
                hazard = .true.
                kind = 'index'
                var = idx1
                return
            end if
        end if
        if (len_trim(idx2) > 0) then
            if (has_index_use(right, idx2)) then
                hazard = .true.
                kind = 'index'
                var = idx2
            end if
        end if
    end subroutine analyze_pair

    logical function guard_var(expr, fname, var)
        !! True if expr calls fname(VAR ...) as a whole word; returns VAR.
        character(len=*), intent(in) :: expr, fname
        character(len=*), intent(out) :: var
        integer :: p, base

        guard_var = .false.
        var = ''
        base = 0
        do
            p = index(expr(base + 1:), fname)
            if (p == 0) exit
            p = base + p
            if (boundary_before(expr, p)) then
                call read_ident(expr, p + len(fname), var)
                if (len_trim(var) > 0) then
                    guard_var = .true.
                    return
                end if
            end if
            base = p
        end do
    end function guard_var

    subroutine index_guard_vars(left, idx1, idx2)
        !! Pull the bare identifiers compared by a <, <=, >, >= bound in left.
        !! A function result (token followed by '(') is not a bare index.
        character(len=*), intent(in) :: left
        character(len=*), intent(out) :: idx1, idx2
        integer :: i, L

        idx1 = ''
        idx2 = ''
        L = len_trim(left)
        i = 1
        do
            if (i >= L) exit
            if (left(i:i) == '<' .or. left(i:i) == '>') then
                call bare_ident_left(left, i - 1, idx1)
                if (i + 1 <= L) then
                    if (left(i + 1:i + 1) == '=') then
                        call bare_ident_right(left, i + 2, idx2)
                    else
                        call bare_ident_right(left, i + 1, idx2)
                    end if
                end if
                return
            end if
            i = i + 1
        end do
    end subroutine index_guard_vars

    subroutine bare_ident_left(expr, before, var)
        character(len=*), intent(in) :: expr
        integer, intent(in) :: before
        character(len=*), intent(out) :: var
        integer :: j, e

        var = ''
        j = before
        do
            if (j < 1) return
            if (expr(j:j) /= ' ') exit
            j = j - 1
        end do
        e = j
        do
            if (j < 1) exit
            if (.not. is_expr_char(expr(j:j))) exit
            j = j - 1
        end do
        if (e > j) var = expr(j + 1:e)
        call reject_numeric(var)
    end subroutine bare_ident_left

    subroutine bare_ident_right(expr, after, var)
        character(len=*), intent(in) :: expr
        integer, intent(in) :: after
        character(len=*), intent(out) :: var
        integer :: i, s, L

        var = ''
        L = len_trim(expr)
        i = after
        do
            if (i > L) return
            if (expr(i:i) /= ' ') exit
            i = i + 1
        end do
        s = i
        do
            if (i > L) exit
            if (.not. is_expr_char(expr(i:i))) exit
            i = i + 1
        end do
        if (i <= s) return
        if (i <= L) then
            if (expr(i:i) == '(') return
        end if
        var = expr(s:i - 1)
        call reject_numeric(var)
    end subroutine bare_ident_right

    subroutine reject_numeric(var)
        !! Drop tokens that begin with a digit; they are literals, not indices.
        character(len=*), intent(inout) :: var
        integer :: ic

        if (len_trim(var) == 0) return
        ic = iachar(var(1:1))
        if (ic >= iachar('0') .and. ic <= iachar('9')) var = ''
    end subroutine reject_numeric

    subroutine read_ident(expr, start, var)
        character(len=*), intent(in) :: expr
        integer, intent(in) :: start
        character(len=*), intent(out) :: var
        integer :: i, s, L

        var = ''
        L = len_trim(expr)
        i = start
        do
            if (i > L) return
            if (expr(i:i) /= ' ') exit
            i = i + 1
        end do
        s = i
        do
            if (i > L) exit
            if (.not. is_expr_char(expr(i:i))) exit
            i = i + 1
        end do
        if (i > s) var = expr(s:i - 1)
    end subroutine read_ident

    logical function has_subscript(expr, var)
        character(len=*), intent(in) :: expr, var
        has_subscript = has_followed_by(expr, var, '(')
    end function has_subscript

    logical function has_member(expr, var)
        character(len=*), intent(in) :: expr, var
        has_member = has_followed_by(expr, var, '%')
    end function has_member

    logical function has_followed_by(expr, var, suffix)
        !! True if var occurs as a whole word immediately followed by suffix.
        character(len=*), intent(in) :: expr, var
        character(len=1), intent(in) :: suffix
        integer :: p, base

        has_followed_by = .false.
        if (len_trim(var) == 0) return
        base = 0
        do
            p = index(expr(base + 1:), trim(var)//suffix)
            if (p == 0) exit
            p = base + p
            if (boundary_before(expr, p)) then
                has_followed_by = .true.
                return
            end if
            base = p
        end do
    end function has_followed_by

    logical function has_word(expr, var)
        character(len=*), intent(in) :: expr, var
        integer :: p, base, vlen, after

        has_word = .false.
        if (len_trim(var) == 0) return
        vlen = len_trim(var)
        base = 0
        do
            p = index(expr(base + 1:), trim(var))
            if (p == 0) exit
            p = base + p
            after = p + vlen
            if (boundary_before(expr, p)) then
                if (after > len_trim(expr)) then
                    has_word = .true.
                    return
                else if (.not. is_ident_char(expr(after:after))) then
                    has_word = .true.
                    return
                end if
            end if
            base = p
        end do
    end function has_word

    logical function has_index_use(expr, idx)
        !! True if idx appears as an array/string subscript. A subscript opens
        !! with 'name(idx' (the '(' follows an identifier); a bare '(idx' is a
        !! grouping paren, e.g. (ic >= 0), and is not a subscript. ',idx' inside
        !! an existing argument list also counts.
        character(len=*), intent(in) :: expr, idx

        has_index_use = subscript_open(expr, idx) .or. &
            bounded_after(expr, ','//trim(idx))
    end function has_index_use

    logical function subscript_open(expr, idx)
        !! A subscript opens with 'array(idx'. The identifier before the '(' must
        !! be an array name, not a value/array intrinsic: real(n,dp), all(a==0),
        !! any(a<0) pass the value through, they do not index by it, so the guard
        !! `n < 0` / `a < 0` next to them is not a real index hazard.
        character(len=*), intent(in) :: expr, idx
        integer :: p, base, after
        logical :: ok_after, ok_before
        character(len=128) :: head

        subscript_open = .false.
        base = 0
        do
            p = index(expr(base + 1:), '('//trim(idx))
            if (p == 0) exit
            p = base + p
            after = p + 1 + len_trim(idx)
            ok_after = after > len_trim(expr)
            if (.not. ok_after) ok_after = .not. is_ident_char(expr(after:after))
            ok_before = .false.
            if (p > 1) ok_before = is_ident_char(expr(p - 1:p - 1))
            if (ok_before) then
                call head_ident(expr, p - 1, head)
                if (is_value_intrinsic(head)) ok_before = .false.
            end if
            if (ok_after .and. ok_before) then
                subscript_open = .true.
                return
            end if
            base = p
        end do
    end function subscript_open

    subroutine head_ident(expr, endp, head)
        !! Read the identifier ending at endp (the char before a '(').
        character(len=*), intent(in) :: expr
        integer, intent(in) :: endp
        character(len=*), intent(out) :: head
        integer :: j

        head = ''
        j = endp
        do
            if (j < 1) exit
            if (.not. is_ident_char(expr(j:j))) exit
            j = j - 1
        end do
        if (endp > j) head = expr(j + 1:endp)
    end subroutine head_ident

    logical function is_value_intrinsic(name)
        !! Common intrinsics that take a value or array argument rather than
        !! indexing it; an array shadowing one of these names is itself flagged
        !! by -Wintrinsic-shadow, so excluding them here is safe.
        character(len=*), intent(in) :: name
        character(len=*), parameter :: list = &
            ' real dble dfloat int nint cmplx aimag conjg abs sign mod modulo '// &
            'sqrt exp log log10 sin cos tan all any sum product count merge '// &
            'maxval minval dot_product max min huge tiny epsilon '
        is_value_intrinsic = len_trim(name) > 0 .and. &
            index(list, ' '//trim(name)//' ') > 0
    end function is_value_intrinsic

    logical function bounded_after(expr, pat)
        character(len=*), intent(in) :: expr, pat
        integer :: p, base, after

        bounded_after = .false.
        base = 0
        do
            p = index(expr(base + 1:), pat)
            if (p == 0) exit
            p = base + p
            after = p + len(pat)
            if (after > len_trim(expr)) then
                bounded_after = .true.
                return
            else if (.not. is_ident_char(expr(after:after))) then
                bounded_after = .true.
                return
            end if
            base = p
        end do
    end function bounded_after

    subroutine extract_left(code, upto, left)
        character(len=*), intent(in) :: code
        integer, intent(in) :: upto
        character(len=*), intent(out) :: left
        integer :: depth, j

        left = ''
        if (upto < 1) return
        depth = 0
        j = upto
        do
            if (j < 1) exit
            if (code(j:j) == ')') then
                depth = depth + 1
            else if (code(j:j) == '(') then
                if (depth == 0) exit
                depth = depth - 1
            else if (depth == 0) then
                if (op_ends_at(code, j) > 0) exit
            end if
            j = j - 1
        end do
        left = adjustl(code(j + 1:upto))
    end subroutine extract_left

    subroutine extract_right(code, from, L, right)
        character(len=*), intent(in) :: code
        integer, intent(in) :: from, L
        character(len=*), intent(out) :: right
        integer :: depth, j

        right = ''
        if (from > L) return
        depth = 0
        j = from
        do
            if (j > L) exit
            if (code(j:j) == '(') then
                depth = depth + 1
            else if (code(j:j) == ')') then
                if (depth == 0) exit
                depth = depth - 1
            else if (depth == 0) then
                if (op_at(code, j, L) > 0) exit
            end if
            j = j + 1
        end do
        right = adjustl(code(from:j - 1))
    end subroutine extract_right

    integer function op_at(code, i, L)
        !! 5 if '.and.' starts at i, 4 if '.or.' starts at i, else 0.
        character(len=*), intent(in) :: code
        integer, intent(in) :: i, L

        op_at = 0
        if (i + 4 <= L) then
            if (code(i:i + 4) == '.and.') then
                op_at = 5
                return
            end if
        end if
        if (i + 3 <= L) then
            if (code(i:i + 3) == '.or.') op_at = 4
        end if
    end function op_at

    integer function op_ends_at(code, j)
        !! 5 if '.and.' ends at j, 4 if '.or.' ends at j, else 0.
        character(len=*), intent(in) :: code
        integer, intent(in) :: j

        op_ends_at = 0
        if (j >= 5) then
            if (code(j - 4:j) == '.and.') then
                op_ends_at = 5
                return
            end if
        end if
        if (j >= 4) then
            if (code(j - 3:j) == '.or.') op_ends_at = 4
        end if
    end function op_ends_at

    logical function boundary_before(expr, pos)
        character(len=*), intent(in) :: expr
        integer, intent(in) :: pos

        boundary_before = .true.
        if (pos <= 1) return
        boundary_before = .not. is_ident_char(expr(pos - 1:pos - 1))
    end function boundary_before

    logical function ends_with_amp(code)
        character(len=*), intent(in) :: code
        integer :: e

        ends_with_amp = .false.
        e = len_trim(code)
        if (e >= 1) ends_with_amp = code(e:e) == '&'
    end function ends_with_amp

    subroutine drop_trailing_amp(code)
        character(len=*), intent(inout) :: code
        integer :: e

        e = len_trim(code)
        if (e >= 1) code(e:e) = ' '
    end subroutine drop_trailing_amp

    subroutine drop_leading_amp(code)
        character(len=*), intent(inout) :: code
        integer :: i

        i = 1
        do
            if (i > len_trim(code)) return
            if (code(i:i) /= ' ') exit
            i = i + 1
        end do
        if (code(i:i) == '&') code(i:i) = ' '
    end subroutine drop_leading_amp

    subroutine rstrip(s)
        character(len=*), intent(inout) :: s
        integer :: e

        e = len_trim(s)
        if (e < len(s)) s(e + 1:) = ''
    end subroutine rstrip

    pure logical function is_ident_char(c)
        character(len=1), intent(in) :: c
        integer :: ic

        ic = iachar(c)
        is_ident_char = (ic >= iachar('a') .and. ic <= iachar('z')) .or. &
            (ic >= iachar('A') .and. ic <= iachar('Z')) .or. &
            (ic >= iachar('0') .and. ic <= iachar('9')) .or. c == '_'
    end function is_ident_char

    pure logical function is_expr_char(c)
        !! Identifier char or '%', so a derived-type index like p%pos is read as
        !! one subscript expression rather than the bare component name.
        character(len=1), intent(in) :: c

        is_expr_char = is_ident_char(c) .or. c == '%'
    end function is_expr_char

    pure character(len=1) function lower_ch(c)
        character(len=1), intent(in) :: c
        integer :: ic

        ic = iachar(c)
        if (ic >= iachar('A') .and. ic <= iachar('Z')) then
            lower_ch = achar(ic + 32)
        else
            lower_ch = c
        end if
    end function lower_ch

end module fo_lint_shortcircuit
