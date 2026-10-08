program test_clean_stale
    !! `fo clean --stale` deletes what nothing references and keeps what
    !! something does. Runs over a fake build tree in /var/tmp, so the
    !! selection logic is the thing under test and not the compiler.
    use fo_util, only: temporary_root
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_clean_stale, only: clean_stale_select, clean_stale_plan, clean_stale_run
    use fo_fs, only: fs_make_dir, fs_write_text, fs_remove_tree
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_select_keeps_referenced_and_newest()
    call test_plan_and_run_over_a_fake_tree()
    call report()

contains

    subroutine check(cond, name)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//name
        end if
    end subroutine check

    subroutine test_select_keeps_referenced_and_newest()
        character(len=512) :: paths(4), doomed(4)
        integer(8) :: mtimes(4), sizes(4), freed
        logical :: referenced(4)
        integer :: n

        paths(1) = '/b/objects_old.a'
        paths(2) = '/b/objects_mid.a'
        paths(3) = '/b/objects_new.a'
        paths(4) = '/b/objects_live.a'
        mtimes(1) = 100_8
        mtimes(2) = 200_8
        mtimes(3) = 300_8
        mtimes(4) = 50_8
        sizes = 1000_8
        referenced = [.false., .false., .false., .true.]

        call clean_stale_select(paths, mtimes, sizes, referenced, 1, doomed, n, &
            freed)
        call check(n == 2, 'two of four unreferenced archives die')
        call check(freed == 2000_8, 'freed bytes are counted')
        call check(sees(doomed, n, '/b/objects_old.a'), 'oldest archive dies')
        call check(sees(doomed, n, '/b/objects_mid.a'), 'middle archive dies')
        call check(.not. sees(doomed, n, '/b/objects_new.a'), &
            'newest unreferenced archive survives the keep window')
        call check(.not. sees(doomed, n, '/b/objects_live.a'), &
            'referenced archive never dies, even when oldest')

        call clean_stale_select(paths, mtimes, sizes, referenced, 0, doomed, n, &
            freed)
        call check(n == 3, 'keep=0 deletes every unreferenced archive')
    end subroutine test_select_keeps_referenced_and_newest

    subroutine test_plan_and_run_over_a_fake_tree()
        character(len=512) :: paths(64), proj
        integer(8) :: sizes(64), freed
        integer :: n, removed

        proj = temporary_root()//'/ffc-goal/clean-stale-tree'
        call make_tree(proj)

        call clean_stale_plan(proj, 0, paths, sizes, n, freed)
        call check(sees(paths, n, trim(proj)//'/build/gfortran_STALE'), &
            'stale profile dir is planned')
        call check(sees(paths, n, trim(proj)//'/build/fo/lib/objects_aaa.a'), &
            'oldest unreferenced archive is planned')
        call check(.not. sees(paths, n, trim(proj)//'/build/gfortran_LIVE'), &
            'profile dir named by compile_commands survives')
        call check(.not. sees(paths, n, trim(proj)//'/build/fo/lib/objects_bbb.a'), &
            'archive named by compile_commands survives')
        call check(.not. sees(paths, n, trim(proj)//'/src/m.f90'), &
            'sources are never candidates')

        call clean_stale_run(proj, 0, removed, freed)
        call check(removed == n, 'run removes exactly what the plan listed')
        call check(.not. exists(trim(proj)//'/build/gfortran_STALE'), &
            'stale profile dir is gone')
        call check(.not. exists(trim(proj)//'/build/fo/lib/objects_aaa.a'), &
            'stale archive is gone')
        call check(exists(trim(proj)//'/build/gfortran_LIVE'), &
            'referenced profile dir still exists')
        call check(exists(trim(proj)//'/build/fo/lib/objects_bbb.a'), &
            'referenced archive still exists')
        call check(exists(trim(proj)//'/src/m.f90'), 'sources are never touched')
    end subroutine test_plan_and_run_over_a_fake_tree

    subroutine make_tree(proj)
        character(len=*), intent(in) :: proj

        call fs_remove_tree(proj)
        call fs_make_dir(trim(proj)//'/build/fo/lib')
        call fs_make_dir(trim(proj)//'/build/gfortran_STALE/ffc')
        call fs_make_dir(trim(proj)//'/build/gfortran_LIVE/ffc')
        call fs_make_dir(trim(proj)//'/src')
        call fs_write_text(trim(proj)//'/src/m.f90', 'module m'//new_line('a')// &
            'end module'//new_line('a'))
        call fs_write_text(trim(proj)//'/build/fo/lib/objects_aaa.a', 'archive-a')
        call fs_write_text(trim(proj)//'/build/fo/lib/objects_bbb.a', 'archive-b')
        call fs_write_text(trim(proj)//'/build/gfortran_STALE/ffc/m.mod', 'mod-stale')
        call fs_write_text(trim(proj)//'/build/gfortran_LIVE/ffc/m.mod', 'mod-live')
        ! the reference evidence a real build leaves behind
        call fs_write_text(trim(proj)//'/build/compile_commands.json', &
            '{"arguments": ["'//trim(proj)//'/build/gfortran_LIVE", "'// &
            trim(proj)//'/build/fo/lib/objects_bbb.a"]}'//new_line('a'))
        ! backdate the stale artifacts so "oldest first" is deterministic
        call backdate(trim(proj)//'/build/fo/lib/objects_aaa.a')
        call backdate(trim(proj)//'/build/gfortran_STALE/ffc/m.mod')
        call backdate(trim(proj)//'/build/gfortran_STALE')
    end subroutine make_tree

    subroutine backdate(path)
        character(len=*), intent(in) :: path

        integer :: rc
        character(len=700) :: cmd

        cmd = 'touch -d 2020-01-01T00:00:00 '//trim(path)
        call execute_command_line(cmd, exitstat=rc)
    end subroutine backdate

    logical function exists(path) result(yes)
        character(len=*), intent(in) :: path

        inquire (file=trim(path), exist=yes)
    end function exists

    pure logical function sees(paths, n, value) result(yes)
        character(len=512), intent(in) :: paths(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: value

        integer :: i

        yes = .false.
        do i = 1, n
            if (trim(paths(i)) == trim(value)) then
                yes = .true.
                return
            end if
        end do
    end function sees

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'clean --stale: pass=', n_pass, &
            ' fail=', n_fail
        if (n_fail > 0) stop 1
    end subroutine report

end program test_clean_stale
