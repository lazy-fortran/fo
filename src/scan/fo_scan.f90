module fo_scan
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_process, only: process_scan_sources
    implicit none
    private
    integer, parameter, public :: MAX_NAME = 128
    integer, parameter, public :: MAX_UNITS = 2048
    integer, parameter :: MAX_DEPS = 64

    public :: scan_unit_t, scan_file, scan_dir, is_slow_test

    type :: scan_unit_t
        character(len=MAX_NAME) :: filename = ''
        character(len=MAX_NAME) :: module_name = ''
        character(len=MAX_NAME) :: program_name = ''
        logical :: is_program = .false.
        logical :: is_test = .false.
        integer :: n_deps = 0
        character(len=MAX_NAME) :: deps(MAX_DEPS)
    end type scan_unit_t

    character(len=32), dimension(10), parameter :: INTRINSIC_MODULES = [ &
                                                   'iso_fortran_env  ', &
                                                   'iso_c_binding    ', &
                                                   'ieee_arithmetic  ', &
                                                   'ieee_exceptions  ', &
                                                   'ieee_features    ', &
                                                   'omp_lib          ', &
                                                   'openacc          ', &
                                                   'mpi              ', &
                                                   'mpi_f08          ', &
                                                   'coarray_intrinsic' &
                                                   ]

contains

    subroutine scan_file(filename, unit_info, ierr)
        character(len=*), intent(in) :: filename
        type(scan_unit_t), intent(out) :: unit_info
        integer, intent(out) :: ierr

        integer :: funit, iostat
        character(len=512) :: line

        ierr = 0
        unit_info%filename = ''
        unit_info%module_name = ''
        unit_info%program_name = ''
        unit_info%is_program = .false.
        unit_info%is_test = .false.
        unit_info%n_deps = 0
        unit_info%deps = ''
        unit_info%filename = filename
        unit_info%is_test = is_test_path(filename)

        open (newunit=funit, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, '(a)') 'fo: cannot open '//trim(filename)
            ierr = 1
            return
        end if

        do
            read (funit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            call parse_line(line, unit_info)
        end do

        close (funit)
    end subroutine scan_file

    subroutine scan_dir(dirname, units, n_units, ierr)
        character(len=*), intent(in) :: dirname
        type(scan_unit_t), intent(out) :: units(MAX_UNITS)
        integer, intent(out) :: n_units, ierr

        character(len=512) :: tmpfile, line
        integer :: funit, iostat, sub_ierr

        ierr = 0
        n_units = 0
        call make_tmpfile('fo_scan_files', tmpfile)

        call process_scan_sources(dirname, tmpfile, sub_ierr)
        if (sub_ierr /= 0) then
            ierr = 1
            return
        end if

        open (newunit=funit, file=tmpfile, status='old', iostat=iostat)
        if (iostat /= 0) then
            ierr = 1
            return
        end if

        do
            read (funit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle
            n_units = n_units + 1
            if (n_units > MAX_UNITS) then
                write (error_unit, '(a,i0)') &
                    'fo: too many source files, max ', MAX_UNITS
                n_units = MAX_UNITS
                exit
            end if
            call scan_file(trim(line), units(n_units), sub_ierr)
            if (sub_ierr /= 0) then
                n_units = n_units - 1
            end if
        end do

        close (funit)
        call delete_tmpfile(tmpfile)
    end subroutine scan_dir

    logical function is_test_path(path)
        character(len=*), intent(in) :: path

        character(len=512) :: clean, base
        integer :: slash

        clean = trim(path)
        slash = index(clean, '/', back=.true.)
        if (slash > 0) then
            base = clean(slash + 1:)
        else
            base = clean
        end if

        is_test_path = index(clean, '/test/') > 0 .or. &
                       index(clean, '/tests/') > 0 .or. &
                       index(base, 'test_') == 1
    end function is_test_path

    subroutine parse_line(line, unit_info)
        character(len=*), intent(in) :: line
        type(scan_unit_t), intent(inout) :: unit_info

        character(len=512) :: trimmed
        character(len=MAX_NAME) :: name

        trimmed = adjustl(line)
        if (len_trim(trimmed) == 0) return
        if (trimmed(1:1) == '!') return

        call extract_use(trimmed, name)
        if (len_trim(name) > 0) then
            if (.not. is_intrinsic(name)) then
                call add_dep(unit_info, name)
            end if
            return
        end if

        call extract_module_def(trimmed, name)
        if (len_trim(name) > 0) then
            unit_info%module_name = name
            return
        end if

        call extract_program_def(trimmed, name)
        if (len_trim(name) > 0) then
            unit_info%program_name = name
            unit_info%is_program = .true.
        end if
    end subroutine parse_line

    subroutine extract_use(line, name)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name

        integer :: start, fin, comma_pos, only_pos

        name = ''
        if (len_trim(line) < 5) return

        if (line(1:4) /= 'use ' .and. line(1:4) /= 'USE ') return

        start = 5

        ! skip 'intrinsic ::' or ', intrinsic ::'
        if (index(line, '::') > 0) then
            start = index(line, '::') + 2
        end if

        ! skip whitespace
        do while (start <= len_trim(line) .and. line(start:start) == ' ')
            start = start + 1
        end do

        comma_pos = index(line(start:), ',')
        only_pos = index(line(start:), ' ')

        if (comma_pos > 0 .and. (only_pos == 0 .or. comma_pos < only_pos)) then
            fin = start + comma_pos - 2
        else if (only_pos > 0) then
            fin = start + only_pos - 2
        else
            fin = len_trim(line)
        end if

        do while (fin >= start .and. line(fin:fin) == ' ')
            fin = fin - 1
        end do

        if (fin >= start) then
            name = line(start:fin)
            call to_lower(name)
        end if
    end subroutine extract_use

    subroutine extract_module_def(line, name)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name

        character(len=512) :: lower_line
        integer :: start

        name = ''
        lower_line = line
        call to_lower(lower_line)

        if (len_trim(lower_line) < 8) return
        if (lower_line(1:7) /= 'module ') return
        ! Exclude subprogram statements, but allow names like ast_nodes_procedure.
        if (index(lower_line, 'module procedure') == 1) return
        if (index(lower_line, 'module subroutine') == 1) return
        if (index(lower_line, 'module function') == 1) return

        start = 8
        do while (start <= len_trim(line) .and. line(start:start) == ' ')
            start = start + 1
        end do

        name = adjustl(line(start:))
        ! trim at first space or comment
        if (index(name, ' ') > 0) name = name(1:index(name, ' ') - 1)
        if (index(name, '!') > 0) name = name(1:index(name, '!') - 1)
        call to_lower(name)
    end subroutine extract_module_def

    subroutine extract_program_def(line, name)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name

        character(len=512) :: lower_line
        integer :: start

        name = ''
        lower_line = line
        call to_lower(lower_line)

        if (len_trim(lower_line) < 9) return
        if (lower_line(1:8) /= 'program ') return

        start = 9
        do while (start <= len_trim(line) .and. line(start:start) == ' ')
            start = start + 1
        end do

        name = adjustl(line(start:))
        if (index(name, ' ') > 0) name = name(1:index(name, ' ') - 1)
        if (index(name, '!') > 0) name = name(1:index(name, '!') - 1)
        call to_lower(name)
    end subroutine extract_program_def

    logical function is_intrinsic(name)
        character(len=*), intent(in) :: name
        integer :: i

        is_intrinsic = .false.
        do i = 1, size(INTRINSIC_MODULES)
            if (trim(name) == trim(INTRINSIC_MODULES(i))) then
                is_intrinsic = .true.
                return
            end if
        end do
    end function is_intrinsic

    subroutine add_dep(unit_info, name)
        type(scan_unit_t), intent(inout) :: unit_info
        character(len=*), intent(in) :: name

        integer :: i

        ! skip duplicates
        do i = 1, unit_info%n_deps
            if (trim(unit_info%deps(i)) == trim(name)) return
        end do

        if (unit_info%n_deps < MAX_DEPS) then
            unit_info%n_deps = unit_info%n_deps + 1
            unit_info%deps(unit_info%n_deps) = name
        end if
    end subroutine add_dep

    subroutine to_lower(str)
        character(len=*), intent(inout) :: str
        integer :: i, ic

        do i = 1, len_trim(str)
            ic = iachar(str(i:i))
            if (ic >= iachar('A') .and. ic <= iachar('Z')) then
                str(i:i) = achar(ic + 32)
            end if
        end do
    end subroutine to_lower

    logical function is_slow_test(name)
        character(len=*), intent(in) :: name

        character(len=MAX_NAME) :: lower_name
        integer :: n

        is_slow_test = .false.
        lower_name = name
        call to_lower(lower_name)
        n = len_trim(lower_name)
        if (n == 0) return

        ! matches *_slow or *_slow_*
        if (n >= 5) then
            if (lower_name(n - 4:n) == '_slow') then
                is_slow_test = .true.
                return
            end if
        end if
        if (index(trim(lower_name), '_slow_') > 0) then
            is_slow_test = .true.
        end if
    end function is_slow_test

end module fo_scan
