program test_lint
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_lint, only: lint_finding_t, lint_file, MAX_FINDINGS
    use fo_util, only: make_tmpfile, delete_tmpfile
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_symbol_used_after_prefixed_occurrence()
    call test_kind_suffix_counts_as_use()
    call test_use_prefixed_symbol_assignment_counts_as_use()
    call test_unused_import_still_reported()
    call test_symbol_used_only_in_include()

    write (output_unit, '(a,i0,a,i0,a)') 'lint: ', n_pass, ' pass, ', n_fail, ' fail'
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

    subroutine test_symbol_used_after_prefixed_occurrence()
        type(lint_finding_t) :: findings(MAX_FINDINGS)
        integer :: n_findings
        character(len=1024) :: lines(7)

        lines(1) = 'module m'
        lines(2) = 'contains'
        lines(3) = 'function wrapper_lambda_over_b_squared(phi)'
        lines(4) = 'use fieldline_integrands, only: lambda_over_b_squared'
        lines(5) = 'wrapper_lambda_over_b_squared = lambda_over_b_squared(phi)'
        lines(6) = 'end function wrapper_lambda_over_b_squared'
        lines(7) = 'end module m'

        call lint_lines('fo_lint_prefixed_use', lines, 7, findings, n_findings)

        call assert(n_findings == 0, &
            'lint accepts symbol used after prefixed occurrence on same line')
    end subroutine test_symbol_used_after_prefixed_occurrence

    subroutine test_kind_suffix_counts_as_use()
        type(lint_finding_t) :: findings(MAX_FINDINGS)
        integer :: n_findings
        character(len=1024) :: lines(5)

        lines(1) = 'module m'
        lines(2) = 'use constants, only: dp'
        lines(3) = 'implicit none'
        lines(4) = 'real(dp), parameter :: x = 1.0_dp'
        lines(5) = 'end module m'

        call lint_lines('fo_lint_kind_suffix', lines, 5, findings, n_findings)

        call assert(n_findings == 0, 'lint counts kind suffix use as symbol use')
    end subroutine test_kind_suffix_counts_as_use

    subroutine test_use_prefixed_symbol_assignment_counts_as_use()
        type(lint_finding_t) :: findings(MAX_FINDINGS)
        integer :: n_findings
        character(len=1024) :: lines(7)

        lines(1) = 'module m'
        lines(2) = 'contains'
        lines(3) = 'subroutine s()'
        lines(4) = 'use boozer_coordinates_mod, only: use_b_r'
        lines(5) = 'use_b_r = .true.'
        lines(6) = 'end subroutine s'
        lines(7) = 'end module m'

        call lint_lines('fo_lint_use_prefix_assignment', lines, 7, findings, n_findings)

        call assert(n_findings == 0, &
            'lint accepts imported names that start with use')
    end subroutine test_use_prefixed_symbol_assignment_counts_as_use

    subroutine test_unused_import_still_reported()
        type(lint_finding_t) :: findings(MAX_FINDINGS)
        integer :: n_findings
        character(len=1024) :: lines(5)

        lines(1) = 'module m'
        lines(2) = 'use constants, only: dp'
        lines(3) = 'implicit none'
        lines(4) = 'integer :: i'
        lines(5) = 'end module m'

        call lint_lines('fo_lint_unused_import', lines, 5, findings, n_findings)

        call assert(n_findings == 1, 'lint still reports a genuinely unused import')
        if (n_findings == 1) then
            call assert(trim(findings(1)%symbol) == 'dp', &
                'unused import finding names the imported symbol')
        end if
    end subroutine test_unused_import_still_reported

    subroutine test_symbol_used_only_in_include()
        type(lint_finding_t) :: findings(MAX_FINDINGS)
        integer :: n_findings, u, slash
        character(len=512) :: main_path, inc_path, inc_base

        call make_tmpfile('fo_lint_incmain', main_path)
        call make_tmpfile('fo_lint_incfrag', inc_path)
        slash = index(trim(inc_path), '/', back=.true.)
        inc_base = inc_path(slash + 1:)

        open (newunit=u, file=trim(inc_path), status='replace')
        write (u, '(a)') '    b = detect_backend(project_dir)'
        close (u)

        open (newunit=u, file=trim(main_path), status='replace')
        write (u, '(a)') 'module m'
        write (u, '(a)') '    use fo_build_backend, only: backend_t, detect_backend'
        write (u, '(a)') 'contains'
        write (u, '(a)') '    subroutine s(project_dir)'
        write (u, '(a)') '        character(len=*), intent(in) :: project_dir'
        write (u, '(a)') '        type(backend_t) :: b'
        write (u, '(a)') "        include '"//trim(inc_base)//"'"
        write (u, '(a)') '    end subroutine s'
        write (u, '(a)') 'end module m'
        close (u)

        n_findings = 0
        call lint_file(trim(main_path), findings, n_findings)
        call delete_tmpfile(main_path)
        call delete_tmpfile(inc_path)

        call assert(n_findings == 0, &
            'import used only inside an included file is not flagged unused')
    end subroutine test_symbol_used_only_in_include

    subroutine lint_lines(prefix, lines, n_lines, findings, n_findings)
        character(len=*), intent(in) :: prefix
        character(len=1024), intent(in) :: lines(:)
        integer, intent(in) :: n_lines
        type(lint_finding_t), intent(out) :: findings(MAX_FINDINGS)
        integer, intent(out) :: n_findings

        character(len=512) :: path
        integer :: u, i

        call make_tmpfile(prefix, path)
        open (newunit=u, file=trim(path), status='replace')
        do i = 1, n_lines
            write (u, '(a)') trim(lines(i))
        end do
        close (u)

        n_findings = 0
        call lint_file(trim(path), findings, n_findings)
        call delete_tmpfile(path)
    end subroutine lint_lines
end program test_lint
