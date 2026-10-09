program test_util
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_fs, only: fs_collect_files, fs_make_dir, fs_remove_tree
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file
    implicit none

    interface
        function setenv(name, value, overwrite) bind(C, name='setenv') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: ierr
        end function setenv

        function unsetenv(name) bind(C, name='unsetenv') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: ierr
        end function unsetenv

        function chmod(path, mode) bind(C, name='chmod') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
            integer(c_int) :: ierr
        end function chmod
    end interface

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0
    call test_tmpdir_is_honoured()
    call test_readonly_owned_scratch_cleanup()
    call test_read_text_file_preserves_long_records()
    call test_profile_filter_ignores_compiler_name_in_basename()
    call test_large_file_collection_preserves_every_match()
    write (output_unit, '(a,i0,a,i0,a)') 'util: ', n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine test_readonly_owned_scratch_cleanup()
        character(len=512) :: root, nested, file
        integer(c_int) :: chmod_status
        integer :: cleanup_status, unit
        logical :: exists

        call make_tmpfile('fo-readonly-scratch', root)
        nested = trim(root)//'/nested'
        file = trim(nested)//'/owned.txt'
        call fs_make_dir(trim(nested))
        open (newunit=unit, file=trim(file), status='replace')
        write (unit, '(a)') 'owned scratch'
        close (unit)
        chmod_status = chmod(trim(nested)//c_null_char, 320_c_int)
        call assert(chmod_status == 0, 'make owned scratch read only')
        call fs_remove_tree(trim(root), cleanup_status)
        inquire (file=trim(root), exist=exists)
        call assert(cleanup_status == 0 .and. .not. exists, &
            'remove an owned scratch tree with a read only child directory')
    end subroutine test_readonly_owned_scratch_cleanup

    subroutine test_tmpdir_is_honoured()
        character(len=512) :: path
        integer(c_int) :: ierr

        ierr = setenv('TMPDIR'//c_null_char, '/var/tmp/fo-util-test'//c_null_char, 1_c_int)
        call assert(ierr == 0, 'set TMPDIR succeeds')
        call make_tmpfile('fo-util', path)
        call assert(index(trim(path), '/var/tmp/fo-util-test/fo-util-') == 1, &
            'make_tmpfile uses TMPDIR')
        ierr = setenv('TMPDIR'//c_null_char, c_null_char, 1_c_int)
        call assert(ierr == 0, 'set empty TMPDIR succeeds')
        call make_tmpfile('fo-util', path)
        call assert(index(trim(path), '/var/tmp/fo-util-') == 1, &
            'make_tmpfile falls back to durable scratch for empty TMPDIR')
        ierr = unsetenv('TMPDIR'//c_null_char)
        call assert(ierr == 0, 'unset TMPDIR succeeds')
    end subroutine test_tmpdir_is_honoured

    subroutine test_read_text_file_preserves_long_records()
        character(len=512) :: path
        character(len=2048) :: text
        character(len=9) :: small_text
        character(len=512) :: exact_line
        character(len=:), allocatable :: long_line, expected, trailing_line
        integer :: u

        call make_tmpfile('fo-util-long-line', path)
        long_line = repeat('x', 900)//'unique-tail-fo-util-41c7'
        trailing_line = 'trim me'//'   '
        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') long_line
        write (u, '(a)') 'following-short-record'
        write (u, '(a)') trailing_line
        write (u, '(a)') ''
        close (u)

        call read_text_file(trim(path), text)
        expected = long_line//char(10)//'following-short-record'//char(10)// &
            'trim me'//char(10)//char(10)
        call assert(text(1:len(expected)) == expected, &
            'reader preserves long tail, next record, trim, and blank line')

        exact_line = repeat('y', len(exact_line))
        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') exact_line
        close (u)
        call read_text_file(trim(path), text)
        expected = exact_line//char(10)
        call assert(text(1:len(expected)) == expected, &
            'reader preserves a record exactly 512 characters long')

        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') 'complete'
        write (u, '(a)') 'does-not-fit'
        close (u)
        call read_text_file(trim(path), small_text)
        call assert(small_text == 'complete'//char(10), &
            'bounded output retains only complete prior records')

        call delete_tmpfile(path)
        call read_text_file(trim(path), text)
        call assert(len_trim(text) == 0, 'missing text file still returns empty text')
    end subroutine test_read_text_file_preserves_long_records

    subroutine test_profile_filter_ignores_compiler_name_in_basename()
        character(len=512) :: root, gnu_dir, nvhpc_dir, path
        character(len=512) :: items(8)
        integer :: n_items, u
        logical :: gnu_found, nvhpc_found

        call make_tmpfile('fo-profile-filter', root)
        call fs_make_dir(root)
        gnu_dir = trim(root)//'/gfortran_ABC'
        nvhpc_dir = trim(root)//'/x86_64-linux-gnu-nvfortran-26.5_DEF'
        call fs_make_dir(gnu_dir)
        call fs_make_dir(nvhpc_dir)
        path = trim(gnu_dir)//'/semantic_analyzer_nvfortran_wrappers.f90.o'
        open (newunit=u, file=trim(path), status='replace')
        close (u)
        path = trim(nvhpc_dir)//'/semantic_analyzer_nvfortran_wrappers.f90.o'
        open (newunit=u, file=trim(path), status='replace')
        close (u)

        call fs_collect_files(root, 'semantic_analyzer_nvfortran_wrappers', &
            '.f90.o', 'nvfortran', items, n_items)
        gnu_found = .false.
        nvhpc_found = .false.
        if (n_items > 0) then
            do u = 1, n_items
                if (index(trim(items(u)), '/gfortran_') > 0) gnu_found = .true.
                if (index(trim(items(u)), '/x86_64-linux-gnu-nvfortran-') > 0) &
                    nvhpc_found = .true.
            end do
        end if
        call assert(n_items == 1, &
            'profile filter matches one directory, not a compiler-named basename')
        call assert(.not. gnu_found, &
            'profile filter excludes GNU profile with nvfortran in source name')
        call assert(nvhpc_found, &
            'profile filter accepts versioned NVHPC profile directory')
        call fs_remove_tree(root)
    end subroutine test_profile_filter_ignores_compiler_name_in_basename

    subroutine test_large_file_collection_preserves_every_match()
        character(len=512) :: root, nested, path
        character(len=512) :: items(800), expected(800), small_items(8)
        character(len=4) :: number
        integer :: i, unit, n_items, status, path_bytes
        logical :: same

        call make_tmpfile('fo-large-collection', root)
        nested = trim(root)//'/'//repeat('d', 190)
        call fs_make_dir(trim(nested))
        path_bytes = 0
        do i = 1, size(expected)
            write (number, '(i4.4)') i
            path = trim(nested)//'/'//repeat('x', 180)//number//'.f90'
            expected(i) = path
            path_bytes = path_bytes + len_trim(path) + 1
            open (newunit=unit, file=trim(path), status='replace')
            close (unit)
        end do
        call assert(path_bytes > 262144, 'large inventory exceeds old byte limit')
        call fs_collect_files(root, '', '.f90', '', items, n_items, ierr=status)
        same = n_items == size(expected)
        if (same) same = all(items == expected)
        call assert(status == 0 .and. same, &
            'large inventory returns every full path in sorted order')
        call fs_collect_files(root, '', '.f90', '', small_items, n_items, ierr=status)
        call assert(status == 1 .and. n_items == size(small_items), &
            'caller item limit remains a reported partial inventory')
        same = .true.
        do i = 1, n_items
            if (.not. any(small_items(i) == expected)) same = .false.
        end do
        do i = 2, n_items
            if (llt(small_items(i), small_items(i - 1))) same = .false.
        end do
        call assert(same, 'bounded caller retains sorted complete paths')
        call fs_collect_files(trim(expected(1)), '', '.f90', '', items, n_items, &
            ierr=status)
        call assert(status == 1 .and. n_items == 0, &
            'non-directory input remains an error, not a buffer retry')
        call fs_remove_tree(root)
    end subroutine test_large_file_collection_preserves_every_match

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

end program test_util
