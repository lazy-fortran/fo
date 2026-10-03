program test_dependency_freshness
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_fs, only: fs_collect_files, fs_make_dir, fs_remove_file, fs_remove_tree, &
        fs_stat
    use fo_gfortran_build, only: gfortran_build, gfortran_test
    use fo_process, only: process_getpid, process_run_logged
    implicit none

    interface
        function setenv(name, value, overwrite) bind(C, name='setenv') result(code)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: code
        end function setenv
        function unsetenv(name) bind(C, name='unsetenv') result(code)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: code
        end function unsetenv
    end interface

    character(len=512) :: root, app, lib, source, reference, log_file, run_log
    character(len=512) :: binary, object, objects(8)
    integer :: exitcode, n_compiled, n_objects, command_status
    integer(c_int) :: env_status
    integer(c_long_long) :: source_mtime, source_size, reference_mtime, reference_size
    integer(c_long_long) :: object_mtime, object_size, binary_mtime, binary_size
    integer(c_long_long) :: changed_object_mtime, changed_object_size
    integer(c_long_long) :: changed_binary_mtime, changed_binary_size
    logical :: current, stat_ok

    call make_paths()
    call fs_remove_tree(root)
    call fs_make_dir(trim(lib)//'/src')
    call fs_make_dir(trim(app)//'/app')
    call fs_make_dir(trim(app)//'/test')
    env_status = setenv('FO_CACHE_DIR'//c_null_char, &
        trim(root)//'/cache'//c_null_char, 1_c_int)
    call require(env_status == 0, 'set isolated fixture cache')

    call write_manifests()
    call write_dependency(11)
    call write_application()
    call write_fixed_v1_test()

    call gfortran_build(app, log_file, exitcode, n_compiled=n_compiled, &
        up_to_date=current)
    call require(exitcode == 0, 'initial path-dependency build succeeds')
    call require(.not. current .and. n_compiled > 0, &
        'initial path-dependency build compiles sources')
    call collect_dependency_object()
    binary = trim(app)//'/build/fo/bin/depapp'
    call run_app('11', 'initial executable observes dependency value 11')

    call gfortran_test(app, log_file, exitcode)
    call require(exitcode == 0, 'fixed-v1 dependency test passes initially')

    call copy_reference()
    call fs_stat(source, source_mtime, source_size, stat_ok)
    call require(stat_ok, 'initial dependency source can be statted')
    call fs_stat(object, object_mtime, object_size, stat_ok)
    call require(stat_ok, 'initial dependency object can be statted')
    call fs_stat(binary, binary_mtime, binary_size, stat_ok)
    call require(stat_ok, 'initial app executable can be statted')

    call write_dependency(22)
    call restore_source_mtime()
    call fs_stat(source, reference_mtime, reference_size, stat_ok)
    call require(stat_ok, 'edited dependency source can be statted')
    call require(reference_mtime == source_mtime .and. &
        reference_size == source_size, &
        'same-size edit restores the original source mtime')

    call gfortran_build(app, log_file, exitcode, n_compiled=n_compiled, &
        up_to_date=current)
    call require(exitcode == 0, 'restored-mtime dependency edit rebuilds')
    call require(.not. current .and. n_compiled > 0, &
        'restored-mtime edit is not treated as an up-to-date build')
    call collect_dependency_object()
    call fs_stat(object, changed_object_mtime, changed_object_size, stat_ok)
    call require(stat_ok .and. changed_object_mtime /= object_mtime, &
        'dependency object is rebuilt after the edit')
    call fs_stat(binary, changed_binary_mtime, changed_binary_size, stat_ok)
    call require(stat_ok .and. changed_binary_mtime /= binary_mtime, &
        'app executable is relinked after the edit')
    call run_app('22', 'rebuilt executable observes dependency value 22')

    call gfortran_build(app, log_file, exitcode, n_compiled=n_compiled, &
        up_to_date=current)
    call require(exitcode == 0 .and. current .and. n_compiled == 0, &
        'unchanged edited input is reused as a warm build')
    call fs_stat(object, object_mtime, object_size, stat_ok)
    call require(stat_ok .and. object_mtime == changed_object_mtime, &
        'warm build preserves dependency object')
    call fs_stat(binary, binary_mtime, binary_size, stat_ok)
    call require(stat_ok .and. binary_mtime == changed_binary_mtime, &
        'warm build preserves app executable')

    call gfortran_test(app, log_file, exitcode)
    call require(exitcode /= 0, 'fixed-v1 dependency test fails after the edit')
    call require(file_contains(log_file, 'expected dependency v1 value 11'), &
        'post-edit test failure reports the independent v1 assertion')

    env_status = unsetenv('FO_CACHE_DIR'//c_null_char)
    call require(env_status == 0, 'restore cache environment')
    call fs_remove_tree(root)
    call fs_remove_file(log_file)
    call fs_remove_file(run_log)
    write (output_unit, '(a)') &
        'PASS: same-size restored-mtime dependency edits rebuild and warm reuse holds'

contains

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        write (error_unit, '(a,a)') 'FAIL: ', message
        error stop 1
    end subroutine require

    subroutine make_paths()
        integer :: count

        call system_clock(count)
        write (root, '(a,i0,a,i0)') '/tmp/fo_dependency_freshness_', &
            process_getpid(), '_', count
        app = trim(root)//'/app'
        lib = trim(root)//'/lib'
        source = trim(lib)//'/src/depmod.f90'
        reference = trim(root)//'/depmod.v1.reference'
        log_file = trim(root)//'/build.log'
        run_log = trim(root)//'/run.log'
    end subroutine make_paths

    subroutine write_manifests()
        integer :: unit

        open (newunit=unit, file=trim(lib)//'/fpm.toml', status='replace')
        write (unit, '(a)') 'name = "depmodlib"'
        close (unit)
        open (newunit=unit, file=trim(app)//'/fpm.toml', status='replace')
        write (unit, '(a)') 'name = "depapp"'
        write (unit, '(a)') '[dependencies]'
        write (unit, '(a)') 'depmodlib = { path = "../lib" }'
        close (unit)
    end subroutine write_manifests

    subroutine write_dependency(value)
        integer, intent(in) :: value
        integer :: unit

        open (newunit=unit, file=trim(source), status='replace')
        write (unit, '(a)') 'module depmod'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') '  integer function dep_value()'
        write (unit, '(a,i0)') '    dep_value = ', value
        write (unit, '(a)') '  end function dep_value'
        write (unit, '(a)') 'end module depmod'
        close (unit)
    end subroutine write_dependency

    subroutine write_application()
        integer :: unit

        open (newunit=unit, file=trim(app)//'/app/main.f90', status='replace')
        write (unit, '(a)') 'program main'
        write (unit, '(a)') '  use depmod, only: dep_value'
        write (unit, '(a)') "  print '(i0)', dep_value()"
        write (unit, '(a)') 'end program main'
        close (unit)
    end subroutine write_application

    subroutine write_fixed_v1_test()
        integer :: unit

        open (newunit=unit, file=trim(app)//'/test/check_dependency.f90', &
            status='replace')
        write (unit, '(a)') 'program check_dependency'
        write (unit, '(a)') '  use depmod, only: dep_value'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') &
            "  if (dep_value() /= 11) error stop 'expected dependency v1 value 11'"
        write (unit, '(a)') 'end program check_dependency'
        close (unit)
    end subroutine write_fixed_v1_test

    subroutine copy_reference()
        integer :: status
        character(len=1200) :: command

        command = 'cp -p "'//trim(source)//'" "'//trim(reference)//'"'
        call execute_command_line(trim(command), exitstat=status)
        call require(status == 0, 'copy source metadata for mtime restoration')
    end subroutine copy_reference

    subroutine restore_source_mtime()
        integer :: status
        character(len=1200) :: command

        command = 'touch -r "'//trim(reference)//'" "'//trim(source)//'"'
        call execute_command_line(trim(command), exitstat=status)
        call require(status == 0, 'restore dependency source mtime')
    end subroutine restore_source_mtime

    subroutine collect_dependency_object()
        call fs_collect_files(trim(app)//'/build/fo/obj', 'depmod', '.o', '', &
            objects, n_objects)
        call require(n_objects == 1, 'find exactly one dependency object')
        object = objects(1)
    end subroutine collect_dependency_object

    subroutine run_app(expected, message)
        character(len=*), intent(in) :: expected, message
        integer :: run_exit, unit, ios
        character(len=128) :: line

        call process_run_logged(trim(app), trim(binary), trim(run_log), &
            .false., 10, run_exit)
        call require(run_exit == 0, 'fixture app exits successfully')
        open (newunit=unit, file=trim(run_log), status='old', action='read', &
            iostat=ios)
        call require(ios == 0, 'fixture app writes an output log')
        read (unit, '(a)', iostat=ios) line
        close (unit)
        call require(ios == 0 .and. trim(line) == expected, message)
    end subroutine run_app

    logical function file_contains(path, needle)
        character(len=*), intent(in) :: path, needle
        integer :: unit, ios
        character(len=2048) :: line

        file_contains = .false.
        open (newunit=unit, file=trim(path), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, needle) > 0) then
                file_contains = .true.
                exit
            end if
        end do
        close (unit)
    end function file_contains

end program test_dependency_freshness
