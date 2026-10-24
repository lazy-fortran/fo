program test_external_input_inventory
    !! A project using the OpenMP metapackage, a BLAS link library and an
    !! absolute include directory, as pkg-config emits for BLAS providers, has
    !! a complete input inventory, and a changed library changes it.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_input_inventory, only: input_declaration_t, input_inventory_t, &
        input_inventory_discover, input_inventory_revalidate
    use fo_util, only: make_tmpfile, read_text_file_alloc
    implicit none

    interface
        integer(c_int) function c_setenv(name, value, overwrite) &
                bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
    end interface

    character(len=4096) :: fixture, project, library
    character(len=1024) :: message
    character(len=:), allocatable :: record
    type(input_declaration_t) :: none(0)
    type(input_inventory_t) :: initial, changed, missing
    integer :: ierr

    call make_tmpfile('fo-external-input-fixture', fixture)
    call fs_make_dir(trim(fixture)//'/lib')
    project = trim(fixture)//'/project'
    library = trim(fixture)//'/lib/libblas.a'
    call fs_make_dir(trim(project)//'/src')
    call fs_make_dir(trim(fixture)//'/include')
    call write(trim(fixture)//'/include/cblas.h', '/* provider header */')
    call write(trim(library), '!<arch>'//new_line('a')//'first')
    call require(c_setenv('LIBRARY_PATH'//c_null_char, &
        trim(fixture)//'/lib'//c_null_char, 1_c_int) == 0, 'set LIBRARY_PATH')
    call write(trim(project)//'/fpm.toml', &
        'name = "external-inputs"'//new_line('a')// &
        '[build]'//new_line('a')// &
        'link = ["blas"]'//new_line('a')// &
        'flags = "-I'//trim(fixture)//'/include/"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'openmp = "*"')
    call write(trim(project)//'/src/kernel.f90', 'module kernel'//new_line('a')// &
        'end module kernel')

    call input_inventory_discover(trim(project), none, initial, ierr, message)
    call require(ierr == 0, 'discover: '//trim(message))
    call require(initial%complete, 'OpenMP and BLAS inputs are complete: '// &
        trim(initial%diagnostic))
    call require(has_entry(initial, 'build/.fo-external-inputs.txt'), &
        'external input record is an inventory entry')
    call read_text_file_alloc(trim(project)//'/build/.fo-external-inputs.txt', &
        record, ierr)
    call require(ierr == 0, 'read external input record')
    call require(index(record, 'link-fo blas ') > 0 .and. &
        index(record, '/lib/libblas.a') > 0, 'record names the linked BLAS file')
    call require(index(record, 'openmp libgomp.spec ') > 0 .and. &
        index(record, 'openmp finclude/omp_lib.mod ') > 0, &
        'record names the compiler OpenMP inputs')
    call require(index(record, 'include cblas.h ') > 0, &
        'record names the provider include files')

    call input_inventory_revalidate(trim(project), none, initial, ierr, message)
    call require(ierr == 0, 'unchanged externals revalidate: '//trim(message))

    call write(trim(library), '!<arch>'//new_line('a')//'second')
    call input_inventory_revalidate(trim(project), none, initial, ierr, message)
    call require(ierr /= 0 .and. &
        index(message, 'build/.fo-external-inputs.txt') > 0, &
        'changed BLAS library is detected: '//trim(message))
    call input_inventory_discover(trim(project), none, changed, ierr, message)
    call require(ierr == 0 .and. changed%complete .and. &
        changed%digest /= initial%digest, 'changed library changes the inventory')

    call write(trim(project)//'/fpm.toml', &
        'name = "external-inputs"'//new_line('a')// &
        '[build]'//new_line('a')// &
        'link = ["fo_absent_external_library"]')
    call input_inventory_discover(trim(project), none, missing, ierr, message)
    call require(ierr == 0 .and. .not. missing%complete .and. &
        index(missing%diagnostic, 'fo_absent_external_library') > 0, &
        'unresolvable link library keeps the inventory incomplete')

    call fs_remove_tree(trim(fixture))
    write (*, '(a)') 'PASS: external input inventory'

contains

    logical function has_entry(inventory, path)
        type(input_inventory_t), intent(in) :: inventory
        character(len=*), intent(in) :: path
        integer :: i

        has_entry = .false.
        do i = 1, inventory%entry_count
            if (trim(inventory%entries(i)%root_alias) /= 'project') cycle
            if (trim(inventory%entries(i)%relative_path) /= path) cycle
            has_entry = trim(inventory%entries(i)%role) == 'toolchain-external-inputs'
            return
        end do
    end function has_entry

    subroutine write(path, contents)
        character(len=*), intent(in) :: path, contents
        integer :: unit, status

        open (newunit=unit, file=path, status='replace', action='write', &
            iostat=status)
        call require(status == 0, 'create fixture file: '//path)
        write (unit, '(a)', iostat=status) contents
        call require(status == 0, 'write fixture file: '//path)
        close (unit, iostat=status)
        call require(status == 0, 'close fixture file: '//path)
    end subroutine write

    subroutine require(condition, reason)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: reason

        if (condition) return
        write (*, '(a)') 'FAIL: '//trim(reason)
        error stop 1
    end subroutine require

end program test_external_input_inventory
