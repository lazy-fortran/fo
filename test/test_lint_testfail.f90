program test_lint_testfail
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_lint_testfail, only: testfail_scan_file
    use fo_lint, only: lint_warning_t, lint_compiler, MAX_WARNINGS
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_fs, only: fs_make_dir, fs_remove_tree
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_flags_tallying_program_that_exits_zero()
    call test_flags_bare_stop()
    call test_flags_zero_stop()
    call test_flags_character_stop_code()
    call test_flags_failure_path_only_in_string_literal()
    call test_flags_failure_path_only_in_comment()
    call test_flags_zero_exit_call()
    call test_accepts_error_stop()
    call test_accepts_error_stop_without_code()
    call test_accepts_stop_one()
    call test_accepts_stop_expression()
    call test_accepts_nonzero_exit_call()
    call test_accepts_abort_call()
    call test_accepts_continuation_error_stop()
    call test_accepts_failure_path_in_include()
    call test_ignores_module_without_program()
    call test_ignores_bench_program()
    call test_reports_program_line()
    call test_lint_reports_only_test_directory_programs()

    write (output_unit, '(a,i0,a,i0,a)') 'lint_testfail: ', n_pass, &
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

    subroutine test_flags_tallying_program_that_exits_zero()
        integer :: flagged(8), n
        character(len=200) :: lines(9)

        lines(1) = 'program test_vacuous'
        lines(2) = '    implicit none'
        lines(3) = '    integer :: passed, total'
        lines(4) = '    passed = 1'
        lines(5) = '    total = 2'
        lines(6) = '    if (passed == total) then'
        lines(7) = '        print *, "all tests passed"'
        lines(8) = '    end if'
        lines(9) = 'end program test_vacuous'
        call scan_lines(lines, 9, flagged, n)
        call assert(n == 1, 'tallying program that always exits 0 is flagged')
    end subroutine test_flags_tallying_program_that_exits_zero

    subroutine test_flags_bare_stop()
        integer :: flagged(8), n
        character(len=200) :: lines(4)

        lines(1) = 'program test_bare_stop'
        lines(2) = '    implicit none'
        lines(3) = '    stop'
        lines(4) = 'end program test_bare_stop'
        call scan_lines(lines, 4, flagged, n)
        call assert(n == 1, 'bare stop exits 0 and is flagged')
    end subroutine test_flags_bare_stop

    subroutine test_flags_zero_stop()
        integer :: flagged(8), n
        character(len=200) :: lines(4)

        lines(1) = 'program test_zero_stop'
        lines(2) = '    implicit none'
        lines(3) = '    stop 0'
        lines(4) = 'end program test_zero_stop'
        call scan_lines(lines, 4, flagged, n)
        call assert(n == 1, 'stop 0 exits 0 and is flagged')
    end subroutine test_flags_zero_stop

    subroutine test_flags_character_stop_code()
        integer :: flagged(8), n
        character(len=200) :: lines(4)

        lines(1) = 'program test_char_stop'
        lines(2) = '    implicit none'
        lines(3) = "    stop 'some tests failed'"
        lines(4) = 'end program test_char_stop'
        call scan_lines(lines, 4, flagged, n)
        call assert(n == 1, 'character stop code exits 0 and is flagged')
    end subroutine test_flags_character_stop_code

    subroutine test_flags_failure_path_only_in_string_literal()
        integer :: flagged(8), n
        character(len=200) :: lines(6)

        ! The fixture text this program writes out contains the very tokens the
        ! rule looks for. Matching raw file text credits the program with a
        ! failure path it does not have.
        lines(1) = 'program test_fixture_writer'
        lines(2) = '    implicit none'
        lines(3) = '    integer :: u'
        lines(4) = "    open (newunit=u, file='frag.f90', status='replace')"
        lines(5) = "    write (u, '(a)') '        stop 1' // new_line('a')"
        lines(6) = 'end program test_fixture_writer'
        call scan_lines(lines, 6, flagged, n)
        call assert(n == 1, 'stop 1 only inside a string literal is flagged')
    end subroutine test_flags_failure_path_only_in_string_literal

    subroutine test_flags_failure_path_only_in_comment()
        integer :: flagged(8), n
        character(len=200) :: lines(4)

        lines(1) = 'program test_commented_out'
        lines(2) = '    implicit none'
        lines(3) = '    ! if (n_fail > 0) error stop 1'
        lines(4) = 'end program test_commented_out'
        call scan_lines(lines, 4, flagged, n)
        call assert(n == 1, 'error stop only inside a comment is flagged')
    end subroutine test_flags_failure_path_only_in_comment

    subroutine test_flags_zero_exit_call()
        integer :: flagged(8), n
        character(len=200) :: lines(4)

        lines(1) = 'program test_exit_zero'
        lines(2) = '    implicit none'
        lines(3) = '    call exit(0)'
        lines(4) = 'end program test_exit_zero'
        call scan_lines(lines, 4, flagged, n)
        call assert(n == 1, 'call exit(0) exits 0 and is flagged')
    end subroutine test_flags_zero_exit_call

    subroutine test_accepts_error_stop()
        integer :: flagged(8), n
        character(len=200) :: lines(5)

        lines(1) = 'program test_error_stop'
        lines(2) = '    implicit none'
        lines(3) = '    integer :: n_fail'
        lines(4) = '    n_fail = 0'
        lines(5) = '    if (n_fail > 0) error stop 1'
        call scan_lines(lines, 5, flagged, n)
        call assert(n == 0, 'error stop 1 is a failure path')
    end subroutine test_accepts_error_stop

    subroutine test_accepts_error_stop_without_code()
        integer :: flagged(8), n
        character(len=200) :: lines(3)

        lines(1) = 'program test_error_stop_plain'
        lines(2) = '    implicit none'
        lines(3) = '    error stop'
        call scan_lines(lines, 3, flagged, n)
        call assert(n == 0, 'error stop without a code is a failure path')
    end subroutine test_accepts_error_stop_without_code

    subroutine test_accepts_stop_one()
        integer :: flagged(8), n
        character(len=200) :: lines(5)

        lines(1) = 'program test_stop_one'
        lines(2) = '    implicit none'
        lines(3) = '    integer :: n_fail'
        lines(4) = '    n_fail = 0'
        lines(5) = '    if (n_fail > 0) stop 1'
        call scan_lines(lines, 5, flagged, n)
        call assert(n == 0, 'stop 1 is a failure path')
    end subroutine test_accepts_stop_one

    subroutine test_accepts_stop_expression()
        integer :: flagged(8), n
        character(len=200) :: lines(4)

        lines(1) = 'program test_stop_var'
        lines(2) = '    implicit none'
        lines(3) = '    integer :: status'
        lines(4) = '    stop status'
        call scan_lines(lines, 4, flagged, n)
        call assert(n == 0, 'stop with a variable code is a failure path')
    end subroutine test_accepts_stop_expression

    subroutine test_accepts_nonzero_exit_call()
        integer :: flagged(8), n
        character(len=200) :: lines(3)

        lines(1) = 'program test_exit_one'
        lines(2) = '    implicit none'
        lines(3) = '    call exit(1)'
        call scan_lines(lines, 3, flagged, n)
        call assert(n == 0, 'call exit(1) is a failure path')
    end subroutine test_accepts_nonzero_exit_call

    subroutine test_accepts_abort_call()
        integer :: flagged(8), n
        character(len=200) :: lines(3)

        lines(1) = 'program test_abort'
        lines(2) = '    implicit none'
        lines(3) = '    call abort()'
        call scan_lines(lines, 3, flagged, n)
        call assert(n == 0, 'call abort() is a failure path')
    end subroutine test_accepts_abort_call

    subroutine test_accepts_continuation_error_stop()
        integer :: flagged(8), n
        character(len=200) :: lines(5)

        lines(1) = 'program test_continued'
        lines(2) = '    implicit none'
        lines(3) = '    integer :: n_fail'
        lines(4) = '    n_fail = 0'
        lines(5) = '    if (n_fail > 0) error &'
        call scan_lines_plus(lines, 5, '        stop 1', flagged, n)
        call assert(n == 0, 'error stop split across a continuation is a failure path')
    end subroutine test_accepts_continuation_error_stop

    subroutine test_accepts_failure_path_in_include()
        integer :: flagged(8), n
        character(len=512) :: root, main_path, frag_path
        character(len=512) :: msgs(8)
        integer :: u

        call make_tmpfile('fo_testfail_inc', root)
        call fs_make_dir(trim(root))
        main_path = trim(root)//'/test_with_include.f90'
        frag_path = trim(root)//'/helpers.inc'

        open (newunit=u, file=trim(main_path), status='replace')
        write (u, '(a)') 'program test_with_include'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') '    integer :: n_fail'
        write (u, '(a)') '    n_fail = 0'
        write (u, '(a)') '    call report()'
        write (u, '(a)') 'contains'
        write (u, '(a)') "    include 'helpers.inc'"
        write (u, '(a)') 'end program test_with_include'
        close (u)

        open (newunit=u, file=trim(frag_path), status='replace')
        write (u, '(a)') '    subroutine report()'
        write (u, '(a)') '        if (n_fail > 0) stop 1'
        write (u, '(a)') '    end subroutine report'
        close (u)

        n = 0
        call testfail_scan_file(trim(main_path), flagged, msgs, n, 8)
        call fs_remove_tree(trim(root))
        call assert(n == 0, 'failure path inside an included fragment counts')
    end subroutine test_accepts_failure_path_in_include

    subroutine test_ignores_module_without_program()
        integer :: flagged(8), n
        character(len=200) :: lines(4)

        lines(1) = 'module helper_mod'
        lines(2) = '    implicit none'
        lines(3) = '    integer :: counter'
        lines(4) = 'end module helper_mod'
        call scan_lines(lines, 4, flagged, n)
        call assert(n == 0, 'a module is not a test program')
    end subroutine test_ignores_module_without_program

    subroutine test_ignores_bench_program()
        integer :: flagged(8), n
        character(len=200) :: lines(3)

        lines(1) = 'program bench_noop'
        lines(2) = '    implicit none'
        lines(3) = 'end program bench_noop'
        call scan_lines(lines, 3, flagged, n)
        call assert(n == 0, 'a bench_ target is not held to the rule')
    end subroutine test_ignores_bench_program

    subroutine test_reports_program_line()
        integer :: flagged(8), n
        character(len=200) :: lines(5)

        lines(1) = '! leading comment'
        lines(2) = ''
        lines(3) = 'program test_offset'
        lines(4) = '    implicit none'
        lines(5) = 'end program test_offset'
        call scan_lines(lines, 5, flagged, n)
        call assert(n == 1, 'program preceded by comments is flagged')
        if (n == 1) call assert(flagged(1) == 3, &
            'finding is reported on the program statement line')
    end subroutine test_reports_program_line

    subroutine test_lint_reports_only_test_directory_programs()
        !! End-to-end through lint_compiler: the rule must reach fo's warning
        !! stream (and so `fo lint`, `--json`, and MCP) for test sources only.
        type(lint_warning_t), allocatable :: warnings(:)
        character(len=512) :: root
        integer :: n_warnings, u, n_hits_test, n_hits_app

        allocate (warnings(MAX_WARNINGS))
        call make_tmpfile('fo_testfail_proj', root)
        call fs_make_dir(trim(root))
        call fs_make_dir(trim(root)//'/test')
        call fs_make_dir(trim(root)//'/app')

        open (newunit=u, file=trim(root)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "demo"'
        close (u)

        open (newunit=u, file=trim(root)//'/test/test_demo.f90', status='replace')
        write (u, '(a)') 'program test_demo'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') '    print *, "some tests failed"'
        write (u, '(a)') 'end program test_demo'
        close (u)

        open (newunit=u, file=trim(root)//'/app/main.f90', status='replace')
        write (u, '(a)') 'program main'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') '    print *, "hello"'
        write (u, '(a)') 'end program main'
        close (u)

        n_warnings = 0
        call lint_compiler(trim(root), warnings, n_warnings)
        n_hits_test = count_testfail_hits(warnings, n_warnings, 'test_demo.f90')
        n_hits_app = count_testfail_hits(warnings, n_warnings, 'app/main.f90')
        call fs_remove_tree(trim(root))
        deallocate (warnings)

        call assert(n_hits_test == 1, 'lint reports a vacuous test program')
        call assert(n_hits_app == 0, 'lint leaves programs outside test/ alone')
    end subroutine test_lint_reports_only_test_directory_programs

    integer function count_testfail_hits(warnings, n_warnings, suffix)
        type(lint_warning_t), intent(in) :: warnings(:)
        integer, intent(in) :: n_warnings
        character(len=*), intent(in) :: suffix
        integer :: i

        count_testfail_hits = 0
        do i = 1, n_warnings
            if (index(warnings(i)%message, 'cannot fail the build') == 0) cycle
            if (index(trim(warnings(i)%file), suffix) == 0) cycle
            count_testfail_hits = count_testfail_hits + 1
        end do
    end function count_testfail_hits

    subroutine scan_lines(lines, n_lines, flagged, n_flag)
        character(len=200), intent(in) :: lines(:)
        integer, intent(in) :: n_lines
        integer, intent(out) :: flagged(:)
        integer, intent(out) :: n_flag

        call scan_lines_plus(lines, n_lines, '', flagged, n_flag)
    end subroutine scan_lines

    subroutine scan_lines_plus(lines, n_lines, extra, flagged, n_flag)
        !! Write the fixture to a temp source and scan it. extra is appended as
        !! a final line when non-empty (fixtures that need a continuation line
        !! kept verbatim, including its leading blanks).
        character(len=200), intent(in) :: lines(:)
        integer, intent(in) :: n_lines
        character(len=*), intent(in) :: extra
        integer, intent(out) :: flagged(:)
        integer, intent(out) :: n_flag

        character(len=512) :: path, msgs(8)
        integer :: u, i

        call make_tmpfile('fo_testfail', path)
        open (newunit=u, file=trim(path), status='replace')
        do i = 1, n_lines
            write (u, '(a)') trim(lines(i))
        end do
        if (len(extra) > 0) write (u, '(a)') extra
        close (u)

        n_flag = 0
        call testfail_scan_file(trim(path), flagged, msgs, n_flag, 8)
        call delete_tmpfile(path)
    end subroutine scan_lines_plus

end program test_lint_testfail
