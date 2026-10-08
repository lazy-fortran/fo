program test_submodule_build_order
    use fo_util, only: temporary_root
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree
    use fo_gfortran_build, only: gfortran_test
    use fo_process, only: process_getpid
    implicit none

    integer :: exitcode, n_fail, n_pass, cold_count, warm_count, changed_count
    integer :: multi_cold_count, multi_warm_count
    integer(c_int) :: rc
    character(len=512) :: log_file, project_dir, compiler_script, count_file
    logical :: smod_exists

    interface
        integer(c_int) function c_setenv(name, value, overwrite) &
                bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv

        integer(c_int) function c_chmod(path, mode) bind(C, name='chmod')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_chmod
    end interface

    n_fail = 0
    n_pass = 0
    call make_scratch_paths(project_dir, log_file)
    call write_fixture(project_dir)
    compiler_script = trim(project_dir)//'/gfortran-count'
    count_file = trim(project_dir)//'/compile-count.log'
    call write_compiler_script(compiler_script, count_file)
    rc = c_setenv('FO_FC'//c_null_char, trim(compiler_script)//c_null_char, &
        1_c_int)
    if (rc /= 0) error stop 'cannot select counting compiler'
    rc = c_setenv('FO_CACHE_DIR'//c_null_char, &
        trim(project_dir)//'/cache'//c_null_char, 1_c_int)
    if (rc /= 0) error stop 'cannot select isolated cache'
    rc = c_setenv('FO_JOBS'//c_null_char, '1'//c_null_char, 1_c_int)
    if (rc /= 0) error stop 'cannot select one build job'

    call gfortran_test(project_dir, log_file, exitcode, &
        include_slow=.true., use_cache=.true.)
    call assert(exitcode == 0, &
        'nested submodules compile and execute in ancestry order')
    call assert(file_contains(log_file, 'TEST_RESULT test_lineage PASS'), &
        'nested submodule implementation produces the expected value')
    cold_count = line_count(count_file)
    call assert(cold_count == 3, 'cold view compiles all three sources')

    call fs_remove_tree(trim(project_dir)//'/build')
    call fs_remove_file(log_file)
    call gfortran_test(project_dir, log_file, exitcode, &
        include_slow=.true., use_cache=.true.)
    warm_count = line_count(count_file)
    call assert(exitcode == 0 .and. warm_count == cold_count, &
        'fresh view restores every source without compiler invocation')
    call assert(file_contains(log_file, 'TEST_RESULT test_lineage PASS'), &
        'restored objects and interfaces execute correctly')
    inquire (file=trim(project_dir)//'/build/fo/mod/lineage_m.smod', &
        exist=smod_exists)
    call assert(smod_exists, 'restored ancestor interface is present')
    inquire (file=trim(project_dir)//'/build/fo/mod/lineage_m@parent_sm.smod', &
        exist=smod_exists)
    call assert(smod_exists, 'restored parent interface is present')
    inquire (file=trim(project_dir)//'/build/fo/mod/lineage_m@child_sm.smod', &
        exist=smod_exists)
    call assert(smod_exists, 'restored child interface is present')

    call write_parent_value(project_dir, 41)
    call write_test_expectation(project_dir, 43)
    call fs_remove_tree(trim(project_dir)//'/build')
    call fs_remove_file(log_file)
    call gfortran_test(project_dir, log_file, exitcode, &
        include_slow=.true., use_cache=.true.)
    changed_count = line_count(count_file)
    call assert(exitcode == 0 .and. changed_count > warm_count, &
        'changed include recompiles its source')
    call assert(file_contains(log_file, 'TEST_RESULT test_lineage PASS'), &
        'include change reaches executable behavior')

    call append_extra_submodule(project_dir)
    call fs_remove_tree(trim(project_dir)//'/build')
    call fs_remove_file(log_file)
    call gfortran_test(project_dir, log_file, exitcode, &
        include_slow=.true., use_cache=.true.)
    multi_cold_count = line_count(count_file)
    call assert(exitcode == 0 .and. multi_cold_count > changed_count, &
        'source with two submodule outputs compiles successfully')
    inquire (file=trim(project_dir)//'/build/fo/mod/lineage_m@extra_sm.smod', &
        exist=smod_exists)
    call assert(smod_exists, 'second submodule interface is present')
    call fs_remove_tree(trim(project_dir)//'/build')
    call fs_remove_file(log_file)
    call gfortran_test(project_dir, log_file, exitcode, &
        include_slow=.true., use_cache=.true.)
    multi_warm_count = line_count(count_file)
    call assert(exitcode == 0 .and. multi_warm_count == multi_cold_count + 1, &
        'multi-output source bypasses incomplete action cache')
    call assert(file_contains(log_file, 'TEST_RESULT test_lineage PASS'), &
        'multi-output source still executes correctly')

    call fs_remove_tree(project_dir)
    call fs_remove_file(log_file)
    write (output_unit, '(a,i0,a,i0,a)') 'submodule_build_order: ', n_pass, &
        ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', message
        end if
    end subroutine assert

    subroutine make_scratch_paths(project, log)
        character(len=*), intent(out) :: project, log

        integer :: count

        call system_clock(count)
        write (project, '(a,i0,a,i0)') temporary_root()//'/fo_submodule_order-', &
            process_getpid(), '-', count
        log = trim(project)//'.log'
        call fs_remove_tree(project)
        call fs_remove_file(log)
    end subroutine make_scratch_paths

    subroutine write_fixture(project)
        character(len=*), intent(in) :: project

        integer :: unit

        call fs_make_dir(trim(project)//'/src')
        call fs_make_dir(trim(project)//'/test')

        open (newunit=unit, file=trim(project)//'/fpm.toml', status='replace')
        write (unit, '(a)') 'name = "submodule-order-oracle"'
        close (unit)

        ! Names deliberately sort child before parent before ancestor. A graph
        ! containing only child -> ancestor compiles 00_child before 10_parent.
        open (newunit=unit, file=trim(project)//'/src/00_child.f90', &
            status='replace')
        write (unit, '(a)') 'submodule (lineage_m:parent_sm) child_sm'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') 'module procedure child_value'
        write (unit, '(a)') 'child_value = 2'
        write (unit, '(a)') 'end procedure child_value'
        write (unit, '(a)') 'end submodule child_sm'
        close (unit)

        open (newunit=unit, file=trim(project)//'/src/10_parent.f90', &
            status='replace')
        write (unit, '(a)') 'submodule (lineage_m) parent_sm'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') 'module procedure parent_value'
        write (unit, '(a)') 'include "parent_value.inc"'
        write (unit, '(a)') 'end procedure parent_value'
        write (unit, '(a)') 'end submodule parent_sm'
        close (unit)
        call write_parent_value(project, 40)

        open (newunit=unit, file=trim(project)//'/src/20_ancestor.f90', &
            status='replace')
        write (unit, '(a)') 'module lineage_m'
        write (unit, '(a)') 'implicit none'
        write (unit, '(a)') 'interface'
        write (unit, '(a)') 'module integer function parent_value()'
        write (unit, '(a)') 'end function parent_value'
        write (unit, '(a)') 'module integer function child_value()'
        write (unit, '(a)') 'end function child_value'
        write (unit, '(a)') 'end interface'
        write (unit, '(a)') 'end module lineage_m'
        close (unit)

        call write_test_expectation(project, 42)
    end subroutine write_fixture

    subroutine append_extra_submodule(project)
        character(len=*), intent(in) :: project
        integer :: unit

        open (newunit=unit, file=trim(project)//'/src/00_child.f90', &
            status='old', position='append')
        write (unit, '(a)') 'submodule (lineage_m) extra_sm'
        write (unit, '(a)') 'end submodule extra_sm'
        close (unit)
    end subroutine append_extra_submodule

    subroutine write_parent_value(project, value)
        character(len=*), intent(in) :: project
        integer, intent(in) :: value
        integer :: unit

        open (newunit=unit, file=trim(project)//'/src/parent_value.inc', &
            status='replace')
        write (unit, '(a,i0)') 'parent_value = ', value
        close (unit)
    end subroutine write_parent_value

    subroutine write_test_expectation(project, value)
        character(len=*), intent(in) :: project
        integer, intent(in) :: value
        integer :: unit

        open (newunit=unit, file=trim(project)//'/test/test_lineage.f90', &
            status='replace')
        write (unit, '(a)') 'program test_lineage'
        write (unit, '(a)') 'use lineage_m, only: child_value, parent_value'
        write (unit, '(a)') 'implicit none'
        write (unit, '(a,i0,a)') &
            'if (parent_value() + child_value() /= ', value, ') error stop 1'
        write (unit, '(a)') 'end program test_lineage'
        close (unit)
    end subroutine write_test_expectation

    subroutine write_compiler_script(path, count_path)
        character(len=*), intent(in) :: path, count_path
        integer :: unit
        integer(c_int) :: status

        open (newunit=unit, file=trim(path), status='replace')
        write (unit, '(a)') '#!/bin/sh'
        write (unit, '(a)') 'for arg in "$@"; do'
        write (unit, '(a)') '  case "$arg" in'
        write (unit, '(a)') '    */src/*.f90) printf "%s\n" "$arg" >> "'// &
            trim(count_path)//'" ;;'
        write (unit, '(a)') '  esac'
        write (unit, '(a)') 'done'
        write (unit, '(a)') 'exec gfortran "$@"'
        close (unit)
        status = c_chmod(trim(path)//c_null_char, 493_c_int)
        if (status /= 0) error stop 'cannot make compiler wrapper executable'
    end subroutine write_compiler_script

    integer function line_count(path) result(n)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        character(len=512) :: line

        n = 0
        open (newunit=unit, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            n = n + 1
        end do
        close (unit)
    end function line_count

    logical function file_contains(path, needle)
        character(len=*), intent(in) :: path, needle

        character(len=512) :: line
        integer :: iostat, unit

        file_contains = .false.
        open (newunit=unit, file=trim(path), status='old', action='read', &
            iostat=iostat)
        if (iostat /= 0) return
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, trim(needle)) > 0) then
                file_contains = .true.
                exit
            end if
        end do
        close (unit)
    end function file_contains

end program test_submodule_build_order
