program test_fmt
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_format, only: format_lines, MAX_LINE_LEN
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_module_indent()
    call test_subroutine_indent()
    call test_if_then_else_indent()
    call test_do_loop_indent()
    call test_select_case_indent()
    call test_contains_dual()
    call test_one_line_if()
    call test_continuation_line()
    call test_comment_only_line()
    call test_preprocessor_passthrough()
    call test_empty_line_preserved()
    call test_nested_blocks()
    call test_where_block()

    write (output_unit, '(a,i0,a,i0,a)') 'fmt: ', n_pass, ' pass, ', n_fail, ' fail'
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

    subroutine fmt(lines_in, n, lines_out, n_out)
        integer, intent(in) :: n
        character(len=*), intent(in) :: lines_in(n)
        character(len=MAX_LINE_LEN), intent(out) :: lines_out(n)
        integer, intent(out) :: n_out

        character(len=MAX_LINE_LEN) :: buf(n)
        integer :: i

        do i = 1, n
            buf(i) = lines_in(i)
        end do
        call format_lines(buf, n, lines_out, n_out)
    end subroutine fmt

    subroutine test_module_indent()
        character(len=MAX_LINE_LEN) :: out(10)
        integer :: n_out
        character(len=64) :: inp(4)
        inp(1) = 'module foo'
        inp(2) = 'implicit none'
        inp(3) = 'contains'
        inp(4) = 'end module foo'
        call fmt(inp, 4, out, n_out)
        call assert(trim(out(1)) == 'module foo', 'module: header not indented')
        call assert(trim(out(2)) == '    implicit none', 'module: body indented')
        call assert(trim(out(3)) == 'contains', 'module: contains at level 0')
        call assert(trim(out(4)) == 'end module foo', 'module: end not indented')
    end subroutine test_module_indent

    subroutine test_subroutine_indent()
        character(len=MAX_LINE_LEN) :: out(6)
        integer :: n_out
        character(len=64) :: inp(6)
        inp(1) = 'subroutine foo(x)'
        inp(2) = 'integer, intent(in) :: x'
        inp(3) = 'implicit none'
        inp(4) = 'x = 1'
        inp(5) = 'end subroutine foo'
        inp(6) = ''
        call fmt(inp, 6, out, n_out)
        call assert(trim(out(1)) == 'subroutine foo(x)', 'sub: header at 0')
        call assert(trim(out(2)) == '    integer, intent(in) :: x', 'sub: decl indented')
        call assert(trim(out(5)) == 'end subroutine foo', 'sub: end at 0')
    end subroutine test_subroutine_indent

    subroutine test_if_then_else_indent()
        character(len=MAX_LINE_LEN) :: out(8)
        integer :: n_out
        character(len=64) :: inp(8)
        inp(1) = 'if (x > 0) then'
        inp(2) = 'y = 1'
        inp(3) = 'else if (x < 0) then'
        inp(4) = 'y = -1'
        inp(5) = 'else'
        inp(6) = 'y = 0'
        inp(7) = 'end if'
        inp(8) = ''
        call fmt(inp, 8, out, n_out)
        call assert(trim(out(1)) == 'if (x > 0) then', 'if: header at 0')
        call assert(trim(out(2)) == '    y = 1', 'if: body indented')
        call assert(trim(out(3)) == 'else if (x < 0) then', 'else if: at 0')
        call assert(trim(out(4)) == '    y = -1', 'else if body indented')
        call assert(trim(out(5)) == 'else', 'else: at 0')
        call assert(trim(out(6)) == '    y = 0', 'else body indented')
        call assert(trim(out(7)) == 'end if', 'end if: at 0')
    end subroutine test_if_then_else_indent

    subroutine test_do_loop_indent()
        character(len=MAX_LINE_LEN) :: out(6)
        integer :: n_out
        character(len=64) :: inp(4)
        inp(1) = 'do i = 1, n'
        inp(2) = 'x(i) = 0'
        inp(3) = 'end do'
        inp(4) = ''
        call fmt(inp, 4, out, n_out)
        call assert(trim(out(1)) == 'do i = 1, n', 'do: header at 0')
        call assert(trim(out(2)) == '    x(i) = 0', 'do: body indented')
        call assert(trim(out(3)) == 'end do', 'do: end at 0')
    end subroutine test_do_loop_indent

    subroutine test_select_case_indent()
        character(len=MAX_LINE_LEN) :: out(10)
        integer :: n_out
        character(len=64) :: inp(8)
        inp(1) = 'select case (flag)'
        inp(2) = 'case (1)'
        inp(3) = 'x = 1'
        inp(4) = 'case default'
        inp(5) = 'x = 0'
        inp(6) = 'end select'
        inp(7) = ''
        inp(8) = ''
        call fmt(inp, 8, out, n_out)
        call assert(trim(out(1)) == 'select case (flag)', 'select: at 0')
        call assert(trim(out(2)) == 'case (1)', 'case: at 0')
        call assert(trim(out(3)) == '    x = 1', 'case body indented')
        call assert(trim(out(4)) == 'case default', 'case default: at 0')
        call assert(trim(out(5)) == '    x = 0', 'case default body indented')
        call assert(trim(out(6)) == 'end select', 'end select: at 0')
    end subroutine test_select_case_indent

    subroutine test_contains_dual()
        character(len=MAX_LINE_LEN) :: out(8)
        integer :: n_out
        character(len=64) :: inp(6)
        inp(1) = 'module m'
        inp(2) = 'implicit none'
        inp(3) = 'contains'
        inp(4) = 'subroutine foo()'
        inp(5) = 'end subroutine foo'
        inp(6) = 'end module m'
        call fmt(inp, 6, out, n_out)
        call assert(trim(out(1)) == 'module m', 'contains: module at 0')
        call assert(trim(out(2)) == '    implicit none', 'contains: body indented')
        call assert(trim(out(3)) == 'contains', 'contains: at 0')
        call assert(trim(out(4)) == '    subroutine foo()', 'contains: sub indented')
        call assert(trim(out(5)) == '    end subroutine foo', 'contains: end sub indented')
        call assert(trim(out(6)) == 'end module m', 'contains: end module at 0')
    end subroutine test_contains_dual

    subroutine test_one_line_if()
        character(len=MAX_LINE_LEN) :: out(4)
        integer :: n_out
        character(len=64) :: inp(3)
        inp(1) = 'do i = 1, n'
        inp(2) = 'if (x > 0) y = 1'
        inp(3) = 'end do'
        call fmt(inp, 3, out, n_out)
        call assert(trim(out(2)) == '    if (x > 0) y = 1', 'one-line if: indented, no extra indent')
        call assert(trim(out(3)) == 'end do', 'one-line if: end do at 0')
    end subroutine test_one_line_if

    subroutine test_continuation_line()
        character(len=MAX_LINE_LEN) :: out(6)
        integer :: n_out
        character(len=64) :: inp(4)
        inp(1) = 'subroutine foo()'
        inp(2) = 'x = a + &'
        inp(3) = 'b'
        inp(4) = 'end subroutine foo'
        call fmt(inp, 4, out, n_out)
        call assert(trim(out(2)) == '    x = a + &', 'continuation: first line indented')
        call assert(trim(out(3)) == '        b', 'continuation: second line double-indented')
        call assert(trim(out(4)) == 'end subroutine foo', 'continuation: end at 0')
    end subroutine test_continuation_line

    subroutine test_comment_only_line()
        character(len=MAX_LINE_LEN) :: out(6)
        integer :: n_out
        character(len=64) :: inp(4)
        inp(1) = 'subroutine foo()'
        inp(2) = '! a comment'
        inp(3) = 'x = 1'
        inp(4) = 'end subroutine foo'
        call fmt(inp, 4, out, n_out)
        call assert(trim(out(2)) == '    ! a comment', 'comment: indented with block')
    end subroutine test_comment_only_line

    subroutine test_preprocessor_passthrough()
        character(len=MAX_LINE_LEN) :: out(6)
        integer :: n_out
        character(len=64) :: inp(4)
        inp(1) = 'subroutine foo()'
        inp(2) = '#ifdef DEBUG'
        inp(3) = 'x = 1'
        inp(4) = 'end subroutine foo'
        call fmt(inp, 4, out, n_out)
        call assert(trim(out(2)) == '#ifdef DEBUG', 'preprocessor: passed through unchanged')
        call assert(trim(out(3)) == '    x = 1', 'preprocessor: normal line still indented')
    end subroutine test_preprocessor_passthrough

    subroutine test_empty_line_preserved()
        character(len=MAX_LINE_LEN) :: out(6)
        integer :: n_out
        character(len=64) :: inp(5)
        inp(1) = 'subroutine foo()'
        inp(2) = ''
        inp(3) = 'x = 1'
        inp(4) = ''
        inp(5) = 'end subroutine foo'
        call fmt(inp, 5, out, n_out)
        call assert(trim(out(2)) == '', 'empty: preserved as empty')
        call assert(trim(out(3)) == '    x = 1', 'after empty: still indented')
        call assert(trim(out(4)) == '', 'empty: second blank preserved')
    end subroutine test_empty_line_preserved

    subroutine test_nested_blocks()
        character(len=MAX_LINE_LEN) :: out(12)
        integer :: n_out
        character(len=64) :: inp(9)
        inp(1) = 'subroutine foo()'
        inp(2) = 'do i = 1, n'
        inp(3) = 'if (x > 0) then'
        inp(4) = 'y = 1'
        inp(5) = 'end if'
        inp(6) = 'end do'
        inp(7) = 'end subroutine foo'
        inp(8) = ''
        inp(9) = ''
        call fmt(inp, 9, out, n_out)
        call assert(trim(out(1)) == 'subroutine foo()', 'nested: sub at 0')
        call assert(trim(out(2)) == '    do i = 1, n', 'nested: do at 1')
        call assert(trim(out(3)) == '        if (x > 0) then', 'nested: if at 2')
        call assert(trim(out(4)) == '            y = 1', 'nested: body at 3')
        call assert(trim(out(5)) == '        end if', 'nested: end if at 2')
        call assert(trim(out(6)) == '    end do', 'nested: end do at 1')
        call assert(trim(out(7)) == 'end subroutine foo', 'nested: end sub at 0')
    end subroutine test_nested_blocks

    subroutine test_where_block()
        character(len=MAX_LINE_LEN) :: out(6)
        integer :: n_out
        character(len=64) :: inp(4)
        ! block where
        inp(1) = 'where (a > 0)'
        inp(2) = 'b = 1'
        inp(3) = 'end where'
        inp(4) = ''
        call fmt(inp, 4, out, n_out)
        call assert(trim(out(1)) == 'where (a > 0)', 'where: header at 0')
        call assert(trim(out(2)) == '    b = 1', 'where: body indented')
        call assert(trim(out(3)) == 'end where', 'where: end at 0')
    end subroutine test_where_block

end program test_fmt
