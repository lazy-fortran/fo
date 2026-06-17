module fo_fs
    !! Shell-free filesystem operations. Every routine here calls a libc
    !! primitive (via fo_fs.c) or pure Fortran I/O, never execute_command_line,
    !! so nothing forks /bin/sh and nothing is corrupted from an OpenMP region.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    implicit none
    private

    public :: fs_remove_tree, fs_remove_file, fs_make_dir
    public :: fs_delete_suffix, fs_append_file, fs_write_text
    public :: fs_collect_files, fs_collect_mod_dirs
    public :: fs_mkdir_excl, fs_sleep_ms, fs_pid_alive
    public :: fs_copy_exec, fs_rename

    interface
        integer(c_int) function fo_c_rm_rf(path) bind(C, name='fo_c_rm_rf')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_rm_rf

        integer(c_int) function fo_c_rm_file(path) bind(C, name='fo_c_rm_file')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_rm_file

        integer(c_int) function fo_c_mkdir_p(path) bind(C, name='fo_c_mkdir_p')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_mkdir_p

        integer(c_int) function fo_c_delete_suffix(root, suffix, recursive) &
                bind(C, name='fo_c_delete_suffix')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), suffix(*)
            integer(c_int), value :: recursive
        end function fo_c_delete_suffix

        integer(c_int) function fo_c_append_file(src, dst) &
                bind(C, name='fo_c_append_file')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: src(*), dst(*)
        end function fo_c_append_file

        integer(c_int) function fo_c_copy_exec(src, dst) &
                bind(C, name='fo_c_copy_exec')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: src(*), dst(*)
        end function fo_c_copy_exec

        integer(c_int) function fo_c_rename_path(src, dst) &
                bind(C, name='fo_c_rename_path')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: src(*), dst(*)
        end function fo_c_rename_path

        integer(c_int) function fo_c_collect_files(root, infix, suffix, &
                path_needle, recursive, out, cap) &
                bind(C, name='fo_c_collect_files')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), infix(*), suffix(*)
            character(kind=c_char), intent(in) :: path_needle(*)
            integer(c_int), value :: recursive
            character(kind=c_char), intent(out) :: out(*)
            integer(c_int), value :: cap
        end function fo_c_collect_files

        integer(c_int) function fo_c_mkdir_excl(path) &
                bind(C, name='fo_c_mkdir_excl')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_mkdir_excl

        subroutine fo_c_sleep_ms(ms) bind(C, name='fo_c_sleep_ms')
            import :: c_int
            integer(c_int), value :: ms
        end subroutine fo_c_sleep_ms

        integer(c_int) function fo_c_pid_alive(pid) &
                bind(C, name='fo_c_pid_alive')
            import :: c_int
            integer(c_int), value :: pid
        end function fo_c_pid_alive

        integer(c_int) function fo_c_collect_mod_dirs(root, out, cap) &
                bind(C, name='fo_c_collect_mod_dirs')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            character(kind=c_char), intent(out) :: out(*)
            integer(c_int), value :: cap
        end function fo_c_collect_mod_dirs
    end interface

    integer, parameter :: FS_COLLECT_CAP = 262144

contains

    subroutine fs_remove_tree(path)
        !! Recursive delete (rm -rf). Missing path is success.
        character(len=*), intent(in) :: path
        integer(c_int) :: rc
        if (len_trim(path) == 0) return
        rc = fo_c_rm_rf(trim(path)//c_null_char)
    end subroutine fs_remove_tree

    subroutine fs_remove_file(path)
        !! Delete one file (rm -f). Missing file is success.
        character(len=*), intent(in) :: path
        integer(c_int) :: rc
        if (len_trim(path) == 0) return
        rc = fo_c_rm_file(trim(path)//c_null_char)
    end subroutine fs_remove_file

    subroutine fs_make_dir(path)
        !! Create path and missing parents (mkdir -p).
        character(len=*), intent(in) :: path
        integer(c_int) :: rc
        if (len_trim(path) == 0) return
        rc = fo_c_mkdir_p(trim(path)//c_null_char)
    end subroutine fs_make_dir

    subroutine fs_delete_suffix(root, suffix, recursive)
        !! Delete files under root whose name ends with suffix (find -delete).
        character(len=*), intent(in) :: root, suffix
        logical, intent(in) :: recursive
        integer(c_int) :: rec, rc
        rec = 0
        if (recursive) rec = 1
        rc = fo_c_delete_suffix(trim(root)//c_null_char, &
            trim(suffix)//c_null_char, rec)
    end subroutine fs_delete_suffix

    subroutine fs_append_file(src, dst)
        !! Append src onto dst (cat src >> dst).
        character(len=*), intent(in) :: src, dst
        integer(c_int) :: rc
        rc = fo_c_append_file(trim(src)//c_null_char, trim(dst)//c_null_char)
    end subroutine fs_append_file

    subroutine fs_write_text(path, text)
        !! Write a single line of text to path, replacing it (printf text > path).
        character(len=*), intent(in) :: path, text
        integer :: u, ios
        open (newunit=u, file=trim(path), status='replace', action='write', &
            iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') text
        close (u)
    end subroutine fs_write_text

    subroutine fs_collect_files(root, infix, suffix, path_needle, items, &
            n_items, recursive)
        !! Collect regular files under root whose basename contains infix and
        !! ends with suffix and whose path contains path_needle, into items
        !! (each a full path). Recurses unless recursive is .false. Replaces a
        !! find pipeline.
        character(len=*), intent(in) :: root, infix, suffix, path_needle
        character(len=*), intent(out) :: items(:)
        integer, intent(out) :: n_items
        logical, intent(in), optional :: recursive
        character(kind=c_char), allocatable :: buf(:)
        integer(c_int) :: rc, rec

        rec = 1
        if (present(recursive)) then
            if (.not. recursive) rec = 0
        end if
        allocate (buf(FS_COLLECT_CAP))
        rc = fo_c_collect_files(trim(root)//c_null_char, trim(infix)//c_null_char, &
            trim(suffix)//c_null_char, &
            trim(path_needle)//c_null_char, rec, buf, &
            int(FS_COLLECT_CAP, c_int))
        call unpack_buffer(buf, int(rc), items, n_items)
        call sort_items(items, n_items)
        deallocate (buf)
    end subroutine fs_collect_files

    function fs_mkdir_excl(path) result(state)
        !! Atomic exclusive mkdir used as a lock: 0 created, 1 already existed,
        !! -1 error.
        character(len=*), intent(in) :: path
        integer :: state
        state = int(fo_c_mkdir_excl(trim(path)//c_null_char))
    end function fs_mkdir_excl

    subroutine fs_sleep_ms(ms)
        !! Sleep for ms milliseconds without a shell.
        integer, intent(in) :: ms
        call fo_c_sleep_ms(int(ms, c_int))
    end subroutine fs_sleep_ms

    logical function fs_pid_alive(pid)
        !! True if a process with this pid currently exists (kill -0).
        integer, intent(in) :: pid
        fs_pid_alive = fo_c_pid_alive(int(pid, c_int)) /= 0
    end function fs_pid_alive

    integer function fs_copy_exec(src, dst) result(rc)
        !! Copy src over dst and make dst executable (0755). 0 on success.
        character(len=*), intent(in) :: src, dst
        rc = int(fo_c_copy_exec(trim(src)//c_null_char, trim(dst)//c_null_char))
    end function fs_copy_exec

    integer function fs_rename(src, dst) result(rc)
        !! Atomic rename src to dst. 0 on success.
        character(len=*), intent(in) :: src, dst
        rc = int(fo_c_rename_path(trim(src)//c_null_char, trim(dst)//c_null_char))
    end function fs_rename

    subroutine fs_collect_mod_dirs(root, items, n_items)
        !! Recursively collect unique parent directories of *.mod files under
        !! root. Replaces `find -name '*.mod' -printf '%h\n' | sort -u`.
        character(len=*), intent(in) :: root
        character(len=*), intent(out) :: items(:)
        integer, intent(out) :: n_items
        character(kind=c_char), allocatable :: buf(:)
        integer(c_int) :: rc

        allocate (buf(FS_COLLECT_CAP))
        rc = fo_c_collect_mod_dirs(trim(root)//c_null_char, buf, &
            int(FS_COLLECT_CAP, c_int))
        call unpack_buffer(buf, int(rc), items, n_items)
        call sort_items(items, n_items)
        deallocate (buf)
    end subroutine fs_collect_mod_dirs

    subroutine sort_items(items, n)
        !! Stable lexical sort so collected file lists are deterministic, the
        !! way `find | sort` was, keeping cache keys and link order stable.
        character(len=*), intent(inout) :: items(:)
        integer, intent(in) :: n
        integer :: i, j
        character(len=len(items)) :: tmp

        do i = 2, n
            tmp = items(i)
            j = i - 1
            do while (j >= 1)
                if (llt(trim(tmp), trim(items(j)))) then
                    items(j + 1) = items(j)
                    j = j - 1
                else
                    exit
                end if
            end do
            items(j + 1) = tmp
        end do
    end subroutine sort_items

    subroutine unpack_buffer(buf, rc, items, n_items)
        !! Split a NUL-separated C buffer of rc entries into items(:).
        character(kind=c_char), intent(in) :: buf(:)
        integer, intent(in) :: rc
        character(len=*), intent(out) :: items(:)
        integer, intent(out) :: n_items
        integer :: i, start, k

        n_items = 0
        if (rc <= 0) return
        start = 1
        k = 0
        do i = 1, size(buf)
            if (k >= rc .or. n_items >= size(items)) exit
            if (buf(i) == c_null_char) then
                if (i > start) then
                    n_items = n_items + 1
                    call chars_to_str(buf, start, i - 1, items(n_items))
                else
                    n_items = n_items + 1
                    items(n_items) = ''
                end if
                k = k + 1
                start = i + 1
            end if
        end do
    end subroutine unpack_buffer

    subroutine chars_to_str(buf, lo, hi, str)
        character(kind=c_char), intent(in) :: buf(:)
        integer, intent(in) :: lo, hi
        character(len=*), intent(out) :: str
        integer :: i, n

        str = ''
        n = min(hi - lo + 1, len(str))
        do i = 1, n
            str(i:i) = char(ichar(buf(lo + i - 1)))
        end do
    end subroutine chars_to_str

end module fo_fs
