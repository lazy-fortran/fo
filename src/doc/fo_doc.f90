module fo_doc
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS, MAX_NAME
    implicit none
    private

    public :: fo_doc_run, collect_public_symbols

    integer, parameter :: MAX_SYMS = 256

contains

    subroutine fo_doc_run(project_dir, use_json, exitcode)
        character(len=*), intent(in) :: project_dir
        logical, intent(in) :: use_json
        integer, intent(out) :: exitcode

        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr, i
        logical :: first

        exitcode = 0
        allocate (units(MAX_UNITS))
        call scan_dir(project_dir, units, n_units, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo doc: scan failed'
            exitcode = 1
            return
        end if

        if (.not. use_json) call prepare_doc_dir(project_dir, ierr)
        if (ierr /= 0) then
            exitcode = 1
            return
        end if

        if (use_json) write (output_unit, '(a)') '['
        first = .true.
        do i = 1, n_units
            if (is_documentable(units(i))) then
                if (use_json) then
                    call emit_json_module(units(i), first)
                else
                    call emit_md_module(project_dir, units(i))
                end if
            end if
        end do
        if (use_json) then
            if (.not. first) write (output_unit, '(a)') ''
            write (output_unit, '(a)') ']'
        else
            call emit_index(project_dir, units, n_units)
        end if
    end subroutine fo_doc_run

    logical function is_documentable(u)
        type(scan_unit_t), intent(in) :: u

        is_documentable = .false.
        if (len_trim(u%module_name) == 0) return
        if (u%is_program) return
        if (u%is_test) return
        is_documentable = .true.
    end function is_documentable

    subroutine prepare_doc_dir(project_dir, ierr)
        character(len=*), intent(in) :: project_dir
        integer, intent(out) :: ierr

        call execute_command_line('mkdir -p '//trim(project_dir)// &
            '/build/fo/doc', wait=.true., exitstat=ierr)
    end subroutine prepare_doc_dir

    subroutine emit_md_module(project_dir, u)
        character(len=*), intent(in) :: project_dir
        type(scan_unit_t), intent(in) :: u

        character(len=MAX_NAME) :: syms(MAX_SYMS)
        integer :: n_syms, unit, ios, i

        call collect_public_symbols(trim(u%filename), syms, n_syms)

        open (newunit=unit, file=trim(project_dir)//'/build/fo/doc/'// &
            trim(u%module_name)//'.md', status='replace', action='write', &
            iostat=ios)
        if (ios /= 0) return

        write (unit, '(a)') '# '//trim(u%module_name)
        write (unit, '(a)') ''
        write (unit, '(a)') 'Source: `'//trim(u%filename)//'`'
        write (unit, '(a)') ''
        write (unit, '(a)') '## Public API'
        write (unit, '(a)') ''
        if (n_syms == 0) then
            write (unit, '(a)') '(none)'
        else
            do i = 1, n_syms
                write (unit, '(a)') '- `'//trim(syms(i))//'`'
            end do
        end if
        close (unit)
    end subroutine emit_md_module

    subroutine emit_index(project_dir, units, n_units)
        character(len=*), intent(in) :: project_dir
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units

        integer :: unit, ios, i

        open (newunit=unit, file=trim(project_dir)//'/build/fo/doc/index.md', &
            status='replace', action='write', iostat=ios)
        if (ios /= 0) return

        write (unit, '(a)') '# API Documentation'
        write (unit, '(a)') ''
        do i = 1, n_units
            if (is_documentable(units(i))) then
                write (unit, '(a)') '- ['//trim(units(i)%module_name)//']('// &
                    trim(units(i)%module_name)//'.md)'
            end if
        end do
        close (unit)
    end subroutine emit_index

    subroutine emit_json_module(u, first)
        type(scan_unit_t), intent(in) :: u
        logical, intent(inout) :: first

        character(len=MAX_NAME) :: syms(MAX_SYMS)
        integer :: n_syms, i

        call collect_public_symbols(trim(u%filename), syms, n_syms)

        if (.not. first) write (output_unit, '(a)') ','
        first = .false.
        write (output_unit, '(a)', advance='no') '  {"module":"'// &
            trim(u%module_name)//'","file":"'//trim(u%filename)//'","symbols":['
        do i = 1, n_syms
            write (output_unit, '(a)', advance='no') '"'//trim(syms(i))//'"'
            if (i < n_syms) write (output_unit, '(a)', advance='no') ','
        end do
        write (output_unit, '(a)', advance='no') ']}'
    end subroutine emit_json_module

    subroutine collect_public_symbols(filename, syms, n_syms)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: syms(:)
        integer, intent(out) :: n_syms

        character(len=2048) :: logical_line, line
        integer :: funit, ios
        logical :: continuing

        n_syms = 0
        open (newunit=funit, file=filename, status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return

        logical_line = ''
        continuing = .false.
        do
            read (funit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            call append_logical_line(line, logical_line, continuing)
            if (.not. continuing) then
                call extract_public(logical_line, syms, n_syms)
                logical_line = ''
            end if
        end do
        close (funit)
    end subroutine collect_public_symbols

    subroutine append_logical_line(line, logical_line, continuing)
        character(len=*), intent(in) :: line
        character(len=*), intent(inout) :: logical_line
        logical, intent(out) :: continuing

        character(len=len(line)) :: trimmed
        integer :: amp

        trimmed = adjustl(line)
        amp = index(trim(trimmed), '&', back=.true.)
        if (amp == len_trim(trimmed) .and. amp > 0) then
            logical_line = trim(logical_line)//' '//trimmed(:amp - 1)
            continuing = .true.
        else
            logical_line = trim(logical_line)//' '//trim(trimmed)
            continuing = .false.
        end if
    end subroutine append_logical_line

    subroutine extract_public(logical_line, syms, n_syms)
        character(len=*), intent(in) :: logical_line
        character(len=*), intent(inout) :: syms(:)
        integer, intent(inout) :: n_syms

        integer :: pcol, dcol, start, comma
        character(len=len(logical_line)) :: rest, item

        pcol = index(logical_line, 'public ::')
        if (pcol == 0) return

        dcol = index(logical_line(pcol:), '::')
        rest = adjustl(logical_line(pcol + dcol + 1:))

        start = 1
        do
            comma = index(rest(start:), ',')
            if (comma == 0) then
                item = adjustl(rest(start:))
                call add_symbol(item, syms, n_syms)
                exit
            end if
            item = adjustl(rest(start:start + comma - 2))
            call add_symbol(item, syms, n_syms)
            start = start + comma
        end do
    end subroutine extract_public

    subroutine add_symbol(item, syms, n_syms)
        character(len=*), intent(in) :: item
        character(len=*), intent(inout) :: syms(:)
        integer, intent(inout) :: n_syms

        character(len=len(item)) :: name
        integer :: paren, eq

        name = item
        paren = index(name, '(')
        if (paren > 0) name = name(:paren - 1)
        eq = index(name, '=')
        if (eq > 0) name = name(:eq - 1)
        name = adjustl(name)
        if (len_trim(name) == 0) return
        if (n_syms >= size(syms)) return
        n_syms = n_syms + 1
        syms(n_syms) = trim(name)
    end subroutine add_symbol

end module fo_doc
