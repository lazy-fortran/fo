program test_scan
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t, scan_file
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_scan_use_statements()
    call test_scan_module_def()
    call test_scan_program_def()
    call test_scan_intrinsic_skip()

    write(output_unit, '(a,i0,a,i0,a)') 'scan: ', n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write(error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine write_file(filename, lines, n_lines)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: n_lines
        integer :: u, i

        open(newunit=u, file=filename, status='replace')
        do i = 1, n_lines
            write(u, '(a)') trim(lines(i))
        end do
        close(u)
    end subroutine write_file

    subroutine test_scan_use_statements()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=80) :: lines(5)

        lines(1) = 'module foo'
        lines(2) = '    use bar, only: x'
        lines(3) = '    use baz'
        lines(4) = '    implicit none'
        lines(5) = 'end module foo'
        call write_file('/tmp/fo_test_use.f90', lines, 5)

        call scan_file('/tmp/fo_test_use.f90', info, ierr)
        call assert(ierr == 0, 'scan_use: no error')
        call assert(trim(info%module_name) == 'foo', 'scan_use: module name')
        call assert(info%n_deps == 2, 'scan_use: 2 deps')
        call assert(trim(info%deps(1)) == 'bar', 'scan_use: dep 1 is bar')
        call assert(trim(info%deps(2)) == 'baz', 'scan_use: dep 2 is baz')
    end subroutine test_scan_use_statements

    subroutine test_scan_module_def()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=80) :: lines(3)

        lines(1) = 'module my_module'
        lines(2) = '    implicit none'
        lines(3) = 'end module my_module'
        call write_file('/tmp/fo_test_mod.f90', lines, 3)

        call scan_file('/tmp/fo_test_mod.f90', info, ierr)
        call assert(ierr == 0, 'scan_mod: no error')
        call assert(trim(info%module_name) == 'my_module', 'scan_mod: name')
        call assert(info%n_deps == 0, 'scan_mod: no deps')
        call assert(.not. info%is_program, 'scan_mod: not a program')
    end subroutine test_scan_module_def

    subroutine test_scan_program_def()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=80) :: lines(4)

        lines(1) = 'program main'
        lines(2) = '    use utils, only: helper'
        lines(3) = '    implicit none'
        lines(4) = 'end program main'
        call write_file('/tmp/fo_test_prog.f90', lines, 4)

        call scan_file('/tmp/fo_test_prog.f90', info, ierr)
        call assert(ierr == 0, 'scan_prog: no error')
        call assert(trim(info%program_name) == 'main', 'scan_prog: name')
        call assert(info%is_program, 'scan_prog: is program')
        call assert(info%n_deps == 1, 'scan_prog: 1 dep')
        call assert(trim(info%deps(1)) == 'utils', 'scan_prog: dep is utils')
    end subroutine test_scan_program_def

    subroutine test_scan_intrinsic_skip()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=80) :: lines(5)

        lines(1) = 'module calc'
        lines(2) = '    use, intrinsic :: iso_fortran_env, only: dp => real64'
        lines(3) = '    use my_lib'
        lines(4) = '    implicit none'
        lines(5) = 'end module calc'
        call write_file('/tmp/fo_test_intrinsic.f90', lines, 5)

        call scan_file('/tmp/fo_test_intrinsic.f90', info, ierr)
        call assert(ierr == 0, 'scan_intrinsic: no error')
        call assert(info%n_deps == 1, 'scan_intrinsic: 1 dep (intrinsic skipped)')
        call assert(trim(info%deps(1)) == 'my_lib', 'scan_intrinsic: dep is my_lib')
    end subroutine test_scan_intrinsic_skip

end program test_scan
