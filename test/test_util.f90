program test_util
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_fs, only: fs_collect_files, fs_make_dir, fs_remove_tree
    use fo_util, only: make_tmpfile
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
    end interface

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0
    call test_tmpdir_is_honoured()
    call test_profile_filter_ignores_compiler_name_in_basename()
    write (output_unit, '(a,i0,a,i0,a)') 'util: ', n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

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
        call assert(index(trim(path), '/tmp/fo-util-') == 1, &
            'make_tmpfile falls back for empty TMPDIR')
        ierr = unsetenv('TMPDIR'//c_null_char)
        call assert(ierr == 0, 'unset TMPDIR succeeds')
    end subroutine test_tmpdir_is_honoured

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
