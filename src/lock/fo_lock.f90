module fo_lock
    use fo_fpm_config, only: fpm_config_t, fpm_dep_t, fpm_config_parse, dep_kind, &
        DEP_PATH, DEP_GIT, DEP_REGISTRY
    use fo_fs, only: fs_write_text
    use fo_process, only: process_run_argv_logged, argv_push, argv_push_split
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file
    implicit none
    private
    public :: lock_write, lock_check

contains

    subroutine lock_write(project_dir, flags, ierr)
        character(len=*), intent(in) :: project_dir, flags
        integer, intent(out) :: ierr

        character(len=32768) :: want, have

        call lock_content(project_dir, flags, want, ierr)
        if (ierr /= 0) return
        call read_text_file(trim(project_dir)//'/fo.lock', have)
        if (trim(canonical_text(have)) == trim(canonical_text(want))) return
        call fs_write_text(trim(project_dir)//'/fo.lock', trim(canonical_text(want)))
    end subroutine lock_write

    subroutine lock_check(project_dir, flags, ok, message)
        character(len=*), intent(in) :: project_dir, flags
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=32768) :: want, have
        integer :: ierr
        logical :: exists

        ok = .true.
        message = ''
        inquire (file=trim(project_dir)//'/fo.lock', exist=exists)
        if (.not. exists) return

        call lock_content(project_dir, flags, want, ierr)
        if (ierr /= 0) then
            ok = .false.
            message = 'cannot resolve current lock inputs'
            return
        end if
        call read_text_file(trim(project_dir)//'/fo.lock', have)
        if (trim(canonical_text(have)) == trim(canonical_text(want))) return

        ok = .false.
        message = 'fo.lock is out of date; run fo lock'
    end subroutine lock_check

    subroutine lock_content(project_dir, flags, text, ierr)
        character(len=*), intent(in) :: project_dir, flags
        character(len=*), intent(out) :: text
        integer, intent(out) :: ierr

        type(fpm_config_t), allocatable :: cfg
        character(len=512) :: compiler
        integer :: i

        text = ''
        allocate (cfg)
        call fpm_config_parse(project_dir, cfg, ierr)
        if (ierr /= 0) return

        call compiler_identity(compiler)
        call append_line(text, 'version = 1')
        call append_line(text, '')
        call append_line(text, '[project]')
        call append_kv(text, 'name', cfg%name)
        call append_kv(text, 'version', cfg%version)
        call append_line(text, '')
        call append_line(text, '[compiler]')
        call append_kv(text, 'identity', compiler)
        call append_kv(text, 'flags', flags)
        call append_line(text, '')
        call append_line(text, '[dependencies]')
        do i = 1, cfg%n_deps
            call append_dep(text, cfg%deps(i), ierr)
            if (ierr /= 0) return
        end do
        ierr = 0
    end subroutine lock_content

    subroutine append_dep(text, dep, ierr)
        character(len=*), intent(inout) :: text
        type(fpm_dep_t), intent(in) :: dep
        integer, intent(out) :: ierr

        character(len=512) :: rev

        ierr = 0

        select case (dep_kind(dep))
        case (DEP_PATH)
            call append_line(text, '[[dependencies.path]]')
            call append_kv(text, 'name', dep%name)
            call append_kv(text, 'path', dep%path)
        case (DEP_GIT)
            call resolve_git_ref(dep, rev, ierr)
            if (ierr /= 0) return
            call append_line(text, '[[dependencies.git]]')
            call append_kv(text, 'name', dep%name)
            call append_kv(text, 'git', dep%git)
            call append_kv(text, 'branch', dep%branch)
            call append_kv(text, 'tag', dep%tag)
            call append_kv(text, 'rev', rev)
        case (DEP_REGISTRY)
            call append_line(text, '[[dependencies.registry]]')
            call append_kv(text, 'name', dep%name)
            call append_kv(text, 'version', dep%version)
        end select
    end subroutine append_dep

    subroutine resolve_git_ref(dep, rev, ierr)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(out) :: rev
        integer, intent(out) :: ierr

        character(len=:), allocatable :: packed
        character(len=512) :: tmpfile, ref
        character(len=4096) :: out
        integer :: n_args, exitcode, tab

        rev = ''
        ierr = 0
        if (allocated(dep%rev)) then
            if (len_trim(dep%rev) > 0) then
                rev = trim(dep%rev)
                return
            end if
        end if
        ref = 'HEAD'
        if (len_trim(dep%branch) > 0) ref = trim(dep%branch)
        if (len_trim(dep%tag) > 0) ref = trim(dep%tag)

        call make_tmpfile('fo-lock-git', tmpfile)
        n_args = 0
        call argv_push(packed, n_args, 'git')
        call argv_push(packed, n_args, 'ls-remote')
        call argv_push(packed, n_args, trim(dep%git))
        call argv_push(packed, n_args, trim(ref))
        call process_run_argv_logged('', packed, n_args, tmpfile, .false., 60, exitcode)
        if (exitcode /= 0) then
            ierr = exitcode
            call delete_tmpfile(tmpfile)
            return
        end if

        call read_text_file(tmpfile, out)
        call delete_tmpfile(tmpfile)
        tab = index(out, char(9))
        if (tab <= 1) then
            ierr = 1
            return
        end if
        rev = out(1:tab - 1)
    end subroutine resolve_git_ref

    subroutine compiler_identity(compiler)
        character(len=*), intent(out) :: compiler

        character(len=:), allocatable :: packed
        character(len=512) :: tmpfile
        character(len=4096) :: out
        integer :: n_args, exitcode, nl

        compiler = 'gfortran'
        call make_tmpfile('fo-lock-compiler', tmpfile)
        n_args = 0
        call argv_push_split(packed, n_args, 'gfortran')
        call argv_push(packed, n_args, '--version')
        call process_run_argv_logged('', packed, n_args, tmpfile, .false., 30, exitcode)
        if (exitcode == 0) then
            call read_text_file(tmpfile, out)
            nl = index(out, new_line('a'))
            if (nl > 1) compiler = out(1:nl - 1)
        end if
        call delete_tmpfile(tmpfile)
    end subroutine compiler_identity

    subroutine append_kv(text, key, value)
        character(len=*), intent(inout) :: text
        character(len=*), intent(in) :: key, value

        if (len_trim(value) == 0) then
            call append_line(text, trim(key)//' = ""')
        else
            call append_line(text, trim(key)//' = "'//trim(escape(trim(value)))//'"')
        end if
    end subroutine append_kv

    subroutine append_line(text, line)
        character(len=*), intent(inout) :: text
        character(len=*), intent(in) :: line

        integer :: n, m

        n = len_trim(text)
        m = len_trim(line)
        if (n + m + 1 > len(text)) return
        text(n + 1:n + m) = line(1:m)
        text(n + m + 1:n + m + 1) = new_line('a')
    end subroutine append_line

    function escape(value) result(out)
        character(len=*), intent(in) :: value
        character(len=len_trim(value) * 2 + 1) :: out
        integer :: i, n

        out = ''
        n = 0
        do i = 1, len_trim(value)
            if (value(i:i) == '"' .or. value(i:i) == '\') then
                n = n + 1
                out(n:n) = '\'
            end if
            n = n + 1
            out(n:n) = value(i:i)
        end do
    end function escape

    function canonical_text(text) result(out)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: out
        integer :: n

        out = ''
        n = len(text)
        do while (n > 0)
            if (text(n:n) /= ' ' .and. text(n:n) /= new_line('a')) exit
            n = n - 1
        end do
        if (n > 0) out(1:n) = text(1:n)
    end function canonical_text

end module fo_lock
