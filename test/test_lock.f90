program test_lock
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_lock, only: lock_write, lock_check
    use fo_process, only: process_getpid
    use fo_util, only: read_text_file
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
        integer :: ierr, t1, t2

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
        call execute_command_line('sleep 1')
        call lock_write(appdir, '', ierr)
        t2 = file_mtime(lock_path)
        call assert(ierr == 0, 'second lock write succeeds')
        call assert(t1 > 0 .and. t1 == t2, 'unchanged lock is not rewritten')

        call execute_command_line('rm -rf '//trim(base))
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
        call execute_command_line('git -C '//trim(libdir)//' add src/depmod.f90')
        call execute_command_line('git -C '//trim(libdir)// &
            ' -c user.email=fo@example.invalid -c user.name=fo commit -q -m drift')

        call lock_check(appdir, '', ok, message)
        call assert(.not. ok, 'lock check fails after git dependency moves')
        call assert(index(message, 'fo.lock is out of date') > 0, &
            'lock check reports stale lock')

        call execute_command_line('rm -rf '//trim(base))
    end subroutine test_lock_detects_git_drift

    subroutine test_lock_honors_explicit_rev()
        character(len=512) :: base, libdir, appdir, rev_path
        character(len=32768) :: rev, text
        character(len=256) :: message
        integer :: ierr, u, nl
        logical :: ok

        call make_git_project(base, libdir, appdir)
        call make_tmp_path('fo_lock_rev', rev_path)
        call execute_command_line('git -C '//trim(libdir)//' rev-parse HEAD > '// &
            trim(rev_path))
        call read_text_file(rev_path, rev)
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
        call execute_command_line('git -C '//trim(libdir)//' add src/depmod.f90')
        call execute_command_line('git -C '//trim(libdir)// &
            ' -c user.email=fo@example.invalid -c user.name=fo commit -q -m drift')
        call lock_check(appdir, '', ok, message)
        call assert(ok, 'explicit revision is stable when remote HEAD moves')

        call execute_command_line('rm -rf '//trim(base)//' '//trim(rev_path))
    end subroutine test_lock_honors_explicit_rev

    subroutine make_git_project(base, libdir, appdir)
        character(len=*), intent(out) :: base, libdir, appdir
        integer :: u

        call make_tmp_path('fo_lock_project', base)
        libdir = trim(base)//'/lib'
        appdir = trim(base)//'/app'
        call execute_command_line('mkdir -p '//trim(libdir)//'/src '// &
            trim(appdir)//'/app')
        open (newunit=u, file=trim(libdir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "deplib"'
        close (u)
        open (newunit=u, file=trim(libdir)//'/src/depmod.f90', status='replace')
        write (u, '(a)') 'module depmod'
        write (u, '(a)') 'end module depmod'
        close (u)
        call execute_command_line('git -C '//trim(libdir)//' init -q')
        call execute_command_line('git -C '//trim(libdir)// &
            ' -c user.email=fo@example.invalid -c user.name=fo add fpm.toml src/depmod.f90')
        call execute_command_line('git -C '//trim(libdir)// &
            ' -c user.email=fo@example.invalid -c user.name=fo commit -q -m init')
        open (newunit=u, file=trim(appdir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "depapp"'
        write (u, '(a)') 'version = "0.1.0"'
        write (u, '(a)') '[dependencies]'
        write (u, '(a,a,a)') 'deplib = { git = "file://', trim(libdir), '" }'
        close (u)
    end subroutine make_git_project

    integer function file_mtime(path) result(t)
        character(len=*), intent(in) :: path
        character(len=512) :: tmp
        integer :: u, ios

        t = 0
        call make_tmp_path('fo_lock_mtime', tmp)
        call execute_command_line('(stat -c %Y "'//trim(path)//'" 2>/dev/null || '// &
            'stat -f %m "'//trim(path)//'" 2>/dev/null) > '//trim(tmp))
        open (newunit=u, file=trim(tmp), status='old', iostat=ios)
        if (ios == 0) then
            read (u, *, iostat=ios) t
            close (u)
        end if
        call execute_command_line('rm -f '//trim(tmp))
    end function file_mtime

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
