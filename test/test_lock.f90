program test_lock
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_lock, only: lock_write, lock_check
    use fo_process, only: process_getpid
    use fo_util, only: read_text_file
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_remove_file, fs_stat, fs_sleep_ms
    use fo_test_harness, only: run_git
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_lock_write_stable()
    call test_lock_detects_git_drift()
    call test_lock_honors_explicit_rev()

    write (output_unit, '(a,i0,a,i0,a)') 'lock: ', n_pass, ' pass, ', n_fail, &
        ' fail'
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

    subroutine test_lock_write_stable()
        character(len=512) :: base, libdir, appdir, lock_path
        character(len=32768) :: text
        integer :: ierr
        integer(8) :: t1, t2

        call make_git_project(base, libdir, appdir)
        call lock_write(appdir, '', ierr)
        lock_path = trim(appdir)//'/fo.lock'
        call read_text_file(lock_path, text)
        call assert(ierr == 0, 'lock write succeeds')
        call assert(index(text, 'version = 1') > 0, 'lock records format version')
        call assert(index(text, 'name = "depapp"') > 0, 'lock records project name')
        call assert(index(text, '[[dependencies.git]]') > 0, &
            'lock records git dependency')
        call assert(index(text, 'rev = "') > 0, 'lock records git revision')

        t1 = file_mtime(lock_path)
        call fs_sleep_ms(1100)
        call lock_write(appdir, '', ierr)
        t2 = file_mtime(lock_path)
        call assert(ierr == 0, 'second lock write succeeds')
        call assert(t1 > 0 .and. t1 == t2, 'unchanged lock is not rewritten')

        call fs_remove_tree(trim(base))
    end subroutine test_lock_write_stable

    subroutine test_lock_detects_git_drift()
        character(len=512) :: base, libdir, appdir
        character(len=256) :: message
        integer :: ierr, u
        logical :: ok

        call make_git_project(base, libdir, appdir)
        call lock_write(appdir, '', ierr)
        call assert(ierr == 0, 'initial lock for drift test succeeds')

        open (newunit=u, file=trim(libdir)//'/src/depmod.f90', position='append')
        write (u, '(a)') '! drift'
        close (u)
        call git_in(libdir, [character(len=64) :: 'add', 'src/depmod.f90'])
        call git_in(libdir, [character(len=64) :: 'commit', '-q', '-m', 'drift'])

        call lock_check(appdir, '', ok, message)
        call assert(.not. ok, 'lock check fails after git dependency moves')
        call assert(index(message, 'fo.lock is out of date') > 0, &
            'lock check reports stale lock')

        call fs_remove_tree(trim(base))
    end subroutine test_lock_detects_git_drift

    subroutine test_lock_honors_explicit_rev()
        character(len=512) :: base, libdir, appdir
        character(len=32768) :: rev, text
        character(len=256) :: message
        integer :: ierr, u, nl
        character(:), allocatable :: git_output
        logical :: ok

        call make_git_project(base, libdir, appdir)
        call run_git(libdir, [character(len=16) :: 'rev-parse', 'HEAD'], ierr, git_output)
        rev = git_output
        nl = index(rev, new_line('a'))
        if (nl > 0) rev = rev(:nl - 1)
        open (newunit=u, file=trim(appdir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "depapp"'
        write (u, '(a)') 'version = "0.1.0"'
        write (u, '(a)') '[dependencies]'
        write (u, '(a)') 'deplib.git = "file://'//trim(libdir)//'"'
        write (u, '(a)') 'deplib.rev = "'//trim(rev)//'"'
        close (u)

        call lock_write(appdir, '', ierr)
        call read_text_file(trim(appdir)//'/fo.lock', text)
        call assert(ierr == 0, 'lock with explicit rev succeeds')
        call assert(index(text, 'rev = "'//trim(rev)//'"') > 0, &
            'lock preserves explicit revision')

        open (newunit=u, file=trim(libdir)//'/src/depmod.f90', position='append')
        write (u, '(a)') '! drift after pin'
        close (u)
        call git_in(libdir, [character(len=64) :: 'add', 'src/depmod.f90'])
        call git_in(libdir, [character(len=64) :: 'commit', '-q', '-m', 'drift'])
        call lock_check(appdir, '', ok, message)
        call assert(ok, 'explicit revision is stable when remote HEAD moves')

        call fs_remove_tree(trim(base))
    end subroutine test_lock_honors_explicit_rev

    subroutine make_git_project(base, libdir, appdir)
        character(len=*), intent(out) :: base, libdir, appdir
        integer :: u

        call make_tmp_path('fo_lock_project', base)
        libdir = trim(base)//'/lib'
        appdir = trim(base)//'/app'
        call fs_make_dir(trim(libdir)//'/src')
        call fs_make_dir(trim(appdir)//'/app')
        open (newunit=u, file=trim(libdir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "deplib"'
        close (u)
        open (newunit=u, file=trim(libdir)//'/src/depmod.f90', status='replace')
        write (u, '(a)') 'module depmod'
        write (u, '(a)') 'end module depmod'
        close (u)
        call git_in(libdir, [character(len=64) :: 'init', '-q'])
        call git_in(libdir, [character(len=64) :: 'add', 'fpm.toml', 'src/depmod.f90'])
        call git_in(libdir, [character(len=64) :: 'commit', '-q', '-m', 'init'])
        open (newunit=u, file=trim(appdir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "depapp"'
        write (u, '(a)') 'version = "0.1.0"'
        write (u, '(a)') '[dependencies]'
        write (u, '(a,a,a)') 'deplib = { git = "file://', trim(libdir), '" }'
        close (u)
    end subroutine make_git_project

    integer(8) function file_mtime(path) result(t)
        character(len=*), intent(in) :: path
        integer(8) :: bytes
        logical :: ok

        call fs_stat(trim(path), t, bytes, ok)
        if (.not. ok) t = 0
    end function file_mtime

    subroutine git_in(dir, arguments)
        !! Run git in dir with fixed committer identity; fixtures must succeed.
        character(len=*), intent(in) :: dir
        character(len=*), intent(in) :: arguments(:)
        integer :: status

        call run_git(dir, arguments, status)
        call assert(status == 0, 'git '//trim(arguments(1))//' in lock fixture')
    end subroutine git_in

    subroutine make_tmp_path(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path
        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a,i0)') '/tmp/', trim(prefix), '-', &
            process_getpid(), '-', count, '-', serial
    end subroutine make_tmp_path

end program test_lock
