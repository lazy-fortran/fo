program test_lint_shortcircuit
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_lint_shortcircuit, only: shortcircuit_scan_file
    use fo_util, only: make_tmpfile, delete_tmpfile
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_flags_length_guard()
    call test_flags_index_guard()
    call test_flags_allocated_guard()
    call test_flags_associated_guard()
    call test_flags_present_guard()
    call test_flags_or_negated_guard()
    call test_flags_continuation()
    call test_ignores_independent_operands()
    call test_ignores_size_on_access_side()
    call test_ignores_different_variable()
    call test_ignores_string_literal()
    call test_ignores_plain_logical_and()
    call test_ignores_grouping_paren_after_or()
    call test_flags_or_range_guard()
    call test_flags_derived_type_cursor()
    call test_ignores_function_call_of_component()
    call test_ignores_alloc_guard_other_component()
    call test_flags_alloc_guard_same_component()

    write (output_unit, '(a,i0,a,i0,a)') 'lint_shortcircuit: ', n_pass, &
        ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_flags_length_guard()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = "        if (len(v) >= 7 .and. v(1:7) == 'lstset:') then"
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 1, 'length guard + same-var substring flagged')
    end subroutine test_flags_length_guard

    subroutine test_flags_index_guard()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = "        do while (i <= np .and. text(i:i) /= ' ')"
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 1, 'index bound + same-index subscript flagged')
    end subroutine test_flags_index_guard

    subroutine test_flags_allocated_guard()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = '        if (allocated(arr) .and. arr(1) > 0) then'
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 1, 'allocated guard + same-var subscript flagged')
    end subroutine test_flags_allocated_guard

    subroutine test_flags_associated_guard()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = '        if (associated(node) .and. node%kind == 3) then'
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 1, 'associated guard + same-var member flagged')
    end subroutine test_flags_associated_guard

    subroutine test_flags_present_guard()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = '        if (present(opt) .and. opt > 0) then'
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 1, 'present guard + same-var use flagged')
    end subroutine test_flags_present_guard

    subroutine test_flags_or_negated_guard()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = "        if (len(s) == 0 .or. s(1:1) == ' ') then"
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 1, 'or-form negated length guard flagged')
    end subroutine test_flags_or_negated_guard

    subroutine test_flags_continuation()
        integer :: flagged(64), n
        character(len=200) :: lines(2)

        lines(1) = '        if (len_trim(name) > 0 .and. &'
        lines(2) = "            name(1:1) == '#') then"
        call scan_lines(lines, 2, flagged, n)
        call assert(n == 1, 'hazard across a line continuation flagged')
        if (n == 1) call assert(flagged(1) == 1, 'continuation reports start line')
    end subroutine test_flags_continuation

    subroutine test_ignores_independent_operands()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = '        if (len_trim(work) > 0 .and. np < size(last)) then'
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 0, 'guard on one var, bound on another not flagged')
    end subroutine test_ignores_independent_operands

    subroutine test_ignores_size_on_access_side()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = '        if (len(v) >= 1 .and. size(v) > 0) then'
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 0, 'size(v) on access side is not a subscript')
    end subroutine test_ignores_size_on_access_side

    subroutine test_ignores_different_variable()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = "        if (len(v) >= 2 .and. other(1:2) == 'ab') then"
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 0, 'access of a different variable not flagged')
    end subroutine test_ignores_different_variable

    subroutine test_ignores_string_literal()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = "        label = 'trap len(v) .and. v(1:1) inside'"
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 0, 'operator inside a string literal not flagged')
    end subroutine test_ignores_string_literal

    subroutine test_ignores_plain_logical_and()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = '        ready = started .and. connected'
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 0, 'plain logical .and. of scalars not flagged')
    end subroutine test_ignores_plain_logical_and

    subroutine test_ignores_grouping_paren_after_or()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = "        ok = (ic >= 65 .and. ic <= 90) .or. (ic >= 97 .and. ic <= 122)"
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 0, 'grouping paren (ic >= ...) is not a subscript')
    end subroutine test_ignores_grouping_paren_after_or

    subroutine test_flags_or_range_guard()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = "        if (i > env_len .or. env(i:i) == ':') then"
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 1, 'or-form index bound + real subscript flagged')
    end subroutine test_flags_or_range_guard

    subroutine test_ignores_alloc_guard_other_component()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = '        if (.not. allocated(self%buf) .or. self%n <= 0) return'
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 0, 'alloc guard on buf, scalar read of another component, not flagged')
    end subroutine test_ignores_alloc_guard_other_component

    subroutine test_flags_alloc_guard_same_component()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = '        if (allocated(self%buf) .and. self%buf(1) > 0) then'
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 1, 'alloc guard + subscript of the same component flagged')
    end subroutine test_flags_alloc_guard_same_component

    subroutine test_flags_derived_type_cursor()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = '        if (p%pos <= p%n .and. kind(p%tokens(p%pos)) == 3) then'
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 1, 'derived-type cursor p%pos guard + subscript flagged')
    end subroutine test_flags_derived_type_cursor

    subroutine test_ignores_function_call_of_component()
        integer :: flagged(64), n
        character(len=200) :: lines(1)

        lines(1) = '        if (p%pos <= p%n .and. ready(p%flag)) then'
        call scan_lines(lines, 1, flagged, n)
        call assert(n == 0, 'function call of an unrelated component not flagged')
    end subroutine test_ignores_function_call_of_component

    subroutine scan_lines(lines, n_lines, flagged, n_flag)
        character(len=200), intent(in) :: lines(:)
        integer, intent(in) :: n_lines
        integer, intent(out) :: flagged(:)
        integer, intent(out) :: n_flag

        character(len=512) :: path, msgs(64)
        integer :: u, i

        call make_tmpfile('fo_sc', path)
        open (newunit=u, file=trim(path), status='replace')
        do i = 1, n_lines
            write (u, '(a)') trim(lines(i))
        end do
        close (u)

        n_flag = 0
        call shortcircuit_scan_file(trim(path), flagged, msgs, n_flag, 64)
        call delete_tmpfile(path)
    end subroutine scan_lines

end program test_lint_shortcircuit
