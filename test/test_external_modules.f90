program test_external_modules
    !! Oracle for resolving `[build] external-modules` to include directories.
    !!
    !! The behaviour under test is name-to-directory resolution, so each case
    !! plants a module file in a temporary directory of its own and checks
    !! which directory comes back.  Nothing here depends on hdf5 or netcdf
    !! actually being installed, so the result is the same on any machine.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_external_modules, only: collect_external_module_dirs
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_declared_module_is_located()
    call test_undeclared_module_is_not_located()
    call test_case_insensitive_module_name()
    call test_duplicate_directories_collapse()

    write (output_unit, '(a,i0,a,i0,a)') 'external_modules: ', n_pass, &
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
            write (output_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine plant_module(dir, name)
        character(len=*), intent(in) :: dir, name
        integer :: u

        call execute_command_line('mkdir -p '//dir, wait=.true.)
        open (newunit=u, file=dir//'/'//name//'.mod', status='replace')
        write (u, '(a)') 'GFORTRAN module placeholder'
        close (u)
    end subroutine plant_module

    subroutine test_declared_module_is_located()
        character(len=*), parameter :: dir = '/tmp/fo_extmod_located'
        character(len=128) :: names(1)
        character(len=512) :: dirs(4)
        integer :: n_dirs

        call plant_module(dir, 'pretend_hdf5')
        call execute_command_line('true', wait=.true.)
        call set_module_path(dir)
        names(1) = 'pretend_hdf5'
        n_dirs = 0
        call collect_external_module_dirs(names, 1, dirs, n_dirs, 4)
        call assert(n_dirs == 1, 'declared module yields one directory')
        if (n_dirs == 1) then
            call assert(trim(dirs(1)) == dir, &
                'declared module yields the planting directory')
        end if
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_declared_module_is_located

    subroutine test_undeclared_module_is_not_located()
        character(len=*), parameter :: dir = '/tmp/fo_extmod_absent'
        character(len=128) :: names(1)
        character(len=512) :: dirs(4)
        integer :: n_dirs

        call execute_command_line('mkdir -p '//dir, wait=.true.)
        call set_module_path(dir)
        names(1) = 'module_that_is_not_installed'
        n_dirs = 0
        call collect_external_module_dirs(names, 1, dirs, n_dirs, 4)
        call assert(n_dirs == 0, 'module with no .mod anywhere yields nothing')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_undeclared_module_is_not_located

    subroutine test_case_insensitive_module_name()
        !! gfortran writes module files in lower case whatever case the source
        !! used, so a manifest naming "HDF5" must still resolve.
        character(len=*), parameter :: dir = '/tmp/fo_extmod_case'
        character(len=128) :: names(1)
        character(len=512) :: dirs(4)
        integer :: n_dirs

        call plant_module(dir, 'pretend_mixed')
        call set_module_path(dir)
        names(1) = 'Pretend_Mixed'
        n_dirs = 0
        call collect_external_module_dirs(names, 1, dirs, n_dirs, 4)
        call assert(n_dirs == 1, 'mixed-case manifest name resolves')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_case_insensitive_module_name

    subroutine test_duplicate_directories_collapse()
        !! Two modules from the same package must not put the same -I on the
        !! command line twice.
        character(len=*), parameter :: dir = '/tmp/fo_extmod_dup'
        character(len=128) :: names(2)
        character(len=512) :: dirs(4)
        integer :: n_dirs

        call plant_module(dir, 'pretend_one')
        call plant_module(dir, 'pretend_two')
        call set_module_path(dir)
        names(1) = 'pretend_one'
        names(2) = 'pretend_two'
        n_dirs = 0
        call collect_external_module_dirs(names, 2, dirs, n_dirs, 4)
        call assert(n_dirs == 1, 'two modules in one directory yield one -I')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_duplicate_directories_collapse

    subroutine set_module_path(dir)
        character(len=*), intent(in) :: dir
        integer :: status

        call execute_command_line('true', wait=.true., exitstat=status)
        call putenv_module_path(dir)
    end subroutine set_module_path

    subroutine putenv_module_path(dir)
        use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
        character(len=*), intent(in) :: dir
        interface
            function c_setenv(name, value, overwrite) bind(c, name='setenv') &
                result(status)
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: name(*), value(*)
                integer(c_int), value :: overwrite
                integer(c_int) :: status
            end function c_setenv
        end interface
        integer :: status

        status = int(c_setenv('FO_MODULE_PATH'//c_null_char, &
                              dir//c_null_char, 1_c_int))
    end subroutine putenv_module_path

end program test_external_modules
