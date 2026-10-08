program test_clean_help
    !! `fo clean --help` must print usage and delete nothing (#136).
    !!
    !! The bug lived in the flag loop of `cmd_clean` in app/main.f90: it
    !! recognised --cache/--all/--stale/--keep and silently ignored every other
    !! flag, so `fo clean --help` fell straight through to the destructive full
    !! clean and reported "build tree cleared". The flag a user types to ask what
    !! a command does was the flag that destroyed the build.
    !!
    !! So the thing under test is the **binary's argument handling**, not a
    !! library entry point - calling clean_stale_run() directly could never have
    !! caught this. The test runs over a fake project in /var/tmp and checks both
    !! halves of the contract: help is inert, and the flags that are supposed to
    !! delete still delete, because an over-correction that refuses to clean
    !! anything is not a fix either.
    use fo_util, only: temporary_root
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use, intrinsic :: iso_c_binding, only: c_long_long
    use fo_fs, only: fs_make_dir, fs_write_text, fs_remove_tree
    implicit none
    integer :: n_pass, n_fail
    character(len=512) :: fo_bin
    character(len=:), allocatable :: root
    character(len=:), allocatable :: out

    n_pass = 0
    n_fail = 0

    fo_bin = locate_fo_binary()
    if (len_trim(fo_bin) == 0) then
        write (output_unit, '(a)') 'SKIP: fo binary not found for clean --help test'
        call report()
        return
    end if

    root = temporary_root()//'/fo_clean_help_test'
    call fs_remove_tree(root)
    ! fs_make_dir is a plain mkdir, not mkdir -p, so every level is created
    ! explicitly - otherwise the fake project root never exists and every later
    ! `cd <root>` fails, which is exactly what happened here.
    call fs_make_dir(root)
    call fs_make_dir(root//'/build')
    call fs_make_dir(root//'/build/fo')
    call fs_make_dir(root//'/build/fo/obj')
    call fs_write_text(root//'/fpm.toml', 'name = "cleanhelp"'//new_line('a'))
    call fs_write_text(root//'/build/fo/obj/keep.o', 'artifact')

    ! --- help must be inert -------------------------------------------------
    call run_fo('--help', .true., out)
    call check(index(out, 'usage: fo clean') > 0, 'help prints a usage line')
    call check(index(out, '--stale') > 0, 'help documents --stale')
    call check(index(out, '--cache') > 0, 'help documents --cache')
    call check(index(out, 'without deleting') > 0, &
              'help states it deletes nothing')
    call check(index(out, 'build tree cleared') == 0, &
              'help does not report clearing the tree')
    call check(path_exists(root//'/build/fo/obj/keep.o'), &
              'help leaves the build tree intact')

    call run_fo('-h', .true., out)
    call check(index(out, 'usage: fo clean') > 0, 'short -h prints usage')
    call check(index(out, 'build tree cleared') == 0, '-h reports no destruction')
    call check(path_exists(root//'/build/fo/obj/keep.o'), '-h leaves tree intact')

    ! --- the destructive flags must still destroy -------------------------
    ! Guard against an over-correction that makes `clean` inert.
    call run_fo('', .true., out)
    call check(index(out, 'build tree cleared') > 0, 'plain clean still cleans')
    call check(.not. path_exists(root//'/build/fo/obj/keep.o'), &
              'plain clean really removed the artifact')

    ! Recreate and check --stale is untouched by the help change.
    call fs_make_dir(root//'/build')
    call fs_make_dir(root//'/build/fo')
    call fs_make_dir(root//'/build/fo/obj')
    call fs_write_text(root//'/build/fo/obj/a.o', 'a')
    call run_fo('--stale', .true., out)
    call check(index(out, 'stale artifacts removed') > 0, '--stale still runs')
    call check(index(out, 'usage: fo clean') == 0, '--stale does not print usage')

    call fs_remove_tree(root)
    call report()

contains

    logical function path_exists(target)
        !! fo_fs exposes no `fs_exists` predicate (its surface is fs_make_dir,
        !! fs_write_text, fs_remove_tree, fs_stat, ...), so existence is asked of
        !! the shell. `execute_command_line` is an intrinsic **subroutine**, not a
        !! function: the status arrives through exitstat=, and using it in an
        !! expression is a compile error, not a missing USE.
        character(len=*), intent(in) :: target
        integer :: rc

        call execute_command_line('test -e "'//trim(target)//'"', exitstat=rc)
        path_exists = (rc == 0)
    end function path_exists

    function locate_fo_binary() result(path)
        !! Absolute path only. The first version returned the relative
        !! `build/fo/app/fo`, which then broke at runtime: run_fo shells out as
        !! `cd <root> && <binary> clean ...`, so by the time the shell ran the
        !! binary the cwd had changed and a relative path could no longer resolve
        !! (gfortran reported EXECUTE_COMMAND_LINE: Invalid command line). The
        !! binary lives in the project the test runs from, so pwd at startup is
        !! the prefix that makes it absolute.
        character(len=512) :: path
        character(len=4096) :: env, pwd
        character(len=64) :: cands(2)
        integer :: stat, u, io, i
        character(len=512) :: line

        cands = [character(len=64) :: '/build/fo/app/fo', &
                 '/../fo/build/fo/app/fo']
        path = ''
        call get_environment_variable('FO_BIN', env, status=stat)
        if (stat == 0 .and. len_trim(env) > 0) then
            if (path_exists(trim(env))) then
                path = trim(env)
                return
            end if
        end if
        pwd = ''
        call execute_command_line('pwd >'//temporary_root()//'/fo_clean_help_pwd.txt', &
            exitstat=stat)
        open (newunit=u, file=temporary_root()//'/fo_clean_help_pwd.txt', &
             status='old', &
             action='read', iostat=io)
        if (io == 0) then
            read (u, '(a)', iostat=io) line
            if (io == 0) pwd = trim(line)
            close (u)
        end if
        if (len_trim(pwd) == 0) return
        do i = 1, size(cands)
            if (path_exists(trim(pwd)//'/'//cands(i))) then
                path = trim(pwd)//'/'//cands(i)
                return
            end if
        end do
    end function locate_fo_binary

    subroutine run_fo(flags, in_root, out)
        character(len=*), intent(in) :: flags
        logical, intent(in) :: in_root
        character(len=:), allocatable, intent(out) :: out
        character(len=4096) :: cmd, line
        character(len=32768) :: acc
        integer :: unit, io, len

        acc = ''
        cmd = trim(fo_bin)//' clean'
        if (len_trim(flags) > 0) cmd = trim(cmd)//' '//trim(flags)
        if (in_root) cmd = 'cd '//trim(root)//' && '//trim(cmd)
        cmd = trim(cmd)//' >'//temporary_root()//'/fo_clean_help_out.txt 2>&1'
        call execute_command_line(cmd, exitstat=io)
        ! Leave `out` unallocated and let assignment set the length.
        ! `allocate(character(len=0) :: out)` pins the length at zero, so the
        ! later `out = trim(acc)` is an illegal reallocation of a fixed-length
        ! allocatable scalar and aborts at runtime - which is what happened when
        ! the test ran without FO_BIN in the environment, where this path was
        ! first reached after the file read.
        open (newunit=unit, file=temporary_root()//'/fo_clean_help_out.txt', &
             status='old', &
             action='read', iostat=io)
        if (io /= 0) return
        do
            read (unit, '(a)', iostat=io) line
            if (io /= 0) exit
            len = len_trim(line)
            if (len == 0) cycle
            acc = trim(acc)//trim(line)//new_line('a')
        end do
        close (unit)
        out = trim(acc)
    end subroutine run_fo

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

    subroutine report()
        write (output_unit, '(a,i0,a,i0,a)') 'clean_help: pass=', n_pass, &
            ' fail=', n_fail
        if (n_fail > 0) error stop 1
    end subroutine report
end program test_clean_help
