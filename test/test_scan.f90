program test_scan
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t, scan_file, scan_dir, MAX_UNITS, is_slow_test
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_scan_use_statements()
    call test_scan_module_def()
    call test_scan_program_def()
    call test_scan_intrinsic_skip()
    call test_slow_test_detection()
    call test_scan_dir_empty()
    call test_scan_dir_path_with_spaces()
    call test_scan_dir_skips_nested_projects()

    write (output_unit, '(a,i0,a,i0,a)') 'scan: ', n_pass, ' pass, ', n_fail, ' fail'
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

    subroutine write_file(filename, lines, n_lines)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: n_lines
        integer :: u, i

        open (newunit=u, file=filename, status='replace')
        do i = 1, n_lines
            write (u, '(a)') trim(lines(i))
        end do
        close (u)
    end subroutine write_file

    subroutine test_scan_use_statements()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(5)

        lines(1) = 'module foo'
        lines(2) = '    use bar, only: x'
        lines(3) = '    use baz'
        lines(4) = '    implicit none'
        lines(5) = 'end module foo'
        call make_tmp_path('fo_test_use', path, '.f90')
        call write_file(path, lines, 5)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_use: no error')
        call assert(trim(info%module_name) == 'foo', 'scan_use: module name')
        call assert(info%n_deps == 2, 'scan_use: 2 deps')
        call assert(trim(info%deps(1)) == 'bar', 'scan_use: dep 1 is bar')
        call assert(trim(info%deps(2)) == 'baz', 'scan_use: dep 2 is baz')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_use_statements

    subroutine test_scan_module_def()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(3)

        lines(1) = 'module my_module'
        lines(2) = '    implicit none'
        lines(3) = 'end module my_module'
        call make_tmp_path('fo_test_mod', path, '.f90')
        call write_file(path, lines, 3)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_mod: no error')
        call assert(trim(info%module_name) == 'my_module', 'scan_mod: name')
        call assert(info%n_deps == 0, 'scan_mod: no deps')
        call assert(.not. info%is_program, 'scan_mod: not a program')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_module_def

    subroutine test_scan_program_def()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(4)

        lines(1) = 'program main'
        lines(2) = '    use utils, only: helper'
        lines(3) = '    implicit none'
        lines(4) = 'end program main'
        call make_tmp_path('fo_test_prog', path, '.f90')
        call write_file(path, lines, 4)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_prog: no error')
        call assert(trim(info%program_name) == 'main', 'scan_prog: name')
        call assert(info%is_program, 'scan_prog: is program')
        call assert(info%n_deps == 1, 'scan_prog: 1 dep')
        call assert(trim(info%deps(1)) == 'utils', 'scan_prog: dep is utils')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_program_def

    subroutine test_scan_intrinsic_skip()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(5)

        lines(1) = 'module calc'
        lines(2) = '    use, intrinsic :: iso_fortran_env, only: dp => real64'
        lines(3) = '    use my_lib'
        lines(4) = '    implicit none'
        lines(5) = 'end module calc'
        call make_tmp_path('fo_test_intrinsic', path, '.f90')
        call write_file(path, lines, 5)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_intrinsic: no error')
        call assert(info%n_deps == 1, 'scan_intrinsic: 1 dep (intrinsic skipped)')
        call assert(trim(info%deps(1)) == 'my_lib', 'scan_intrinsic: dep is my_lib')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_intrinsic_skip

    subroutine test_slow_test_detection()
        ! *_slow suffix
        call assert(is_slow_test('test_integration_slow'), &
                    'slow: test_integration_slow is slow')
        call assert(is_slow_test('perf_slow'), &
                    'slow: perf_slow is slow')

        ! *_slow_* infix
        call assert(is_slow_test('test_slow_network'), &
                    'slow: test_slow_network is slow')
        call assert(is_slow_test('my_slow_test'), &
                    'slow: my_slow_test is slow')

        ! not slow
        call assert(.not. is_slow_test('test_fast'), &
                    'slow: test_fast is not slow')
        call assert(.not. is_slow_test('test_slowly'), &
                    'slow: test_slowly is not slow')
        call assert(.not. is_slow_test('slowtest'), &
                    'slow: slowtest is not slow')
        call assert(.not. is_slow_test('test_cache'), &
                    'slow: test_cache is not slow')
    end subroutine test_slow_test_detection

    subroutine test_scan_dir_empty()
        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr
        character(len=512) :: dir

        allocate (units(MAX_UNITS))
        call make_tmp_path('fo_test_empty_dir', dir, '')
        call execute_command_line('rm -rf '//trim(dir))
        call execute_command_line('mkdir -p '//trim(dir))

        call scan_dir(dir, units, n_units, ierr)
        call assert(ierr == 0, 'empty_dir: no error')
        call assert(n_units == 0, 'empty_dir: 0 files')

        call execute_command_line('rm -rf '//trim(dir))
    end subroutine test_scan_dir_empty

    subroutine test_scan_dir_path_with_spaces()
        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr, n_tests
        character(len=512) :: base_dir, dir
        character(len=80) :: lib_lines(3), test_lines(3)

        allocate (units(MAX_UNITS))
        call make_tmp_path('fo_test_scan_spaces', base_dir, '')
        dir = trim(base_dir)//' path with spaces'
        call remove_tree(dir)
        call make_dir(trim(dir)//'/src')
        call make_dir(trim(dir)//'/test')

        lib_lines(1) = 'module lib'
        lib_lines(2) = 'implicit none'
        lib_lines(3) = 'end module lib'
        call write_file(trim(dir)//'/src/lib.f90', lib_lines, 3)

        test_lines(1) = 'program test_fast'
        test_lines(2) = 'use lib'
        test_lines(3) = 'end program test_fast'
        call write_file(trim(dir)//'/test/test_fast.f90', test_lines, 3)

        call scan_dir(dir, units, n_units, ierr)
        n_tests = count(units(1:n_units)%is_test)
        call assert(ierr == 0, 'scan spaces: no error')
        call assert(n_units == 2, 'scan spaces: finds source files')
        call assert(n_tests == 1, 'scan spaces: marks test file')

        call remove_tree(dir)
    end subroutine test_scan_dir_path_with_spaces

    subroutine test_scan_dir_skips_nested_projects()
        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr, u
        character(len=512) :: dir
        character(len=80) :: root_lines(3), nested_lines(3)

        allocate (units(MAX_UNITS))
        call make_tmp_path('fo_test_nested_project', dir, '')
        call remove_tree(dir)
        call make_dir(trim(dir)//'/src')
        call make_dir(trim(dir)//'/bench/nested/src')

        open (newunit=u, file=trim(dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "root"'
        close (u)
        open (newunit=u, file=trim(dir)//'/bench/nested/fpm.toml', &
              status='replace')
        write (u, '(a)') 'name = "nested"'
        close (u)

        root_lines(1) = 'module root_mod'
        root_lines(2) = 'implicit none'
        root_lines(3) = 'end module root_mod'
        call write_file(trim(dir)//'/src/root_mod.f90', root_lines, 3)

        nested_lines(1) = 'module nested_mod'
        nested_lines(2) = 'implicit none'
        nested_lines(3) = 'end module nested_mod'
        call write_file(trim(dir)//'/bench/nested/src/nested_mod.f90', &
                        nested_lines, 3)

        call scan_dir(dir, units, n_units, ierr)
        call assert(ierr == 0, 'nested project: no scan error')
        call assert(n_units == 1, 'nested project: nested fpm tree skipped')
        call assert(trim(units(1)%module_name) == 'root_mod', &
                    'nested project: root module remains')

        call remove_tree(dir)
    end subroutine test_scan_dir_skips_nested_projects

    subroutine make_tmp_path(prefix, path, suffix)
        character(len=*), intent(in) :: prefix, suffix
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a)') '/tmp/', trim(prefix), '-', &
            count, '-', serial, trim(suffix)
    end subroutine make_tmp_path

    subroutine make_dir(path)
        character(len=*), intent(in) :: path

        call execute_command_line('mkdir -p "'//trim(path)//'"')
    end subroutine make_dir

    subroutine remove_tree(path)
        character(len=*), intent(in) :: path

        call execute_command_line('rm -rf "'//trim(path)//'"')
    end subroutine remove_tree

end program test_scan
