program test_cover
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_cover, only: coverage_total_percent
    use fo_util, only: make_tmpfile, delete_tmpfile
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_total_percent_from_markdown()
    call report()

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//msg
        end if
    end subroutine assert

    subroutine test_total_percent_from_markdown()
        character(len=512) :: path
        character(len=32) :: total
        integer :: u

        call make_tmpfile('fo_cover_report', path)
        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') '| File | Lines | Covered | Coverage |'
        write (u, '(a)') '| TOTAL | 10 | 7 | 70.0% |'
        close (u)

        call coverage_total_percent(path, total)
        call assert(trim(total) == '70.0%', 'extracts TOTAL coverage percent')
        call delete_tmpfile(path)
    end subroutine test_total_percent_from_markdown

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'cover: pass=', n_pass, &
            ' fail=', n_fail
        if (n_fail > 0) stop 1
    end subroutine report

end program test_cover
