program test_submodule_build_order
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree
    use fo_gfortran_build, only: gfortran_test
    use fo_process, only: process_getpid
    implicit none

    integer :: exitcode, n_fail, n_pass
    character(len=512) :: log_file, project_dir

    n_fail = 0
    n_pass = 0
    call make_scratch_paths(project_dir, log_file)
    call write_fixture(project_dir)

    ! Disable the action cache so the compiler, rather than restored artifacts,
    ! is the independent oracle for the graph's source order.
    call gfortran_test(project_dir, log_file, exitcode, &
        include_slow=.true., use_cache=.false.)
    call assert(exitcode == 0, &
        'nested submodules compile and execute in ancestry order')
    call assert(file_contains(log_file, 'TEST_RESULT test_lineage PASS'), &
        'nested submodule implementation produces the expected value')

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
        write (project, '(a,i0,a,i0)') '/tmp/fo_submodule_order-', &
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
        write (unit, '(a)') 'parent_value = 40'
        write (unit, '(a)') 'end procedure parent_value'
        write (unit, '(a)') 'end submodule parent_sm'
        close (unit)

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

        open (newunit=unit, file=trim(project)//'/test/test_lineage.f90', &
            status='replace')
        write (unit, '(a)') 'program test_lineage'
        write (unit, '(a)') 'use lineage_m, only: child_value, parent_value'
        write (unit, '(a)') 'implicit none'
        write (unit, '(a)') 'if (parent_value() + child_value() /= 42) error stop 1'
        write (unit, '(a)') 'end program test_lineage'
        close (unit)
    end subroutine write_fixture

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
