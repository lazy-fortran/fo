module fo_fs
    !! Shell-free filesystem operations. Every routine here calls a libc
    !! primitive (via fo_fs.c) or pure Fortran I/O, never execute_command_line,
    !! so nothing forks /bin/sh and nothing is corrupted from an OpenMP region.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, &
        c_long_long
    implicit none
    private

    public :: fs_remove_tree, fs_remove_file, fs_make_dir
    public :: fs_delete_suffix, fs_append_file, fs_write_text
    public :: fs_collect_files, fs_collect_mod_dirs
    public :: fs_mkdir_excl, fs_sleep_ms, fs_pid_alive
    public :: fs_copy_exec, fs_rename, fs_stat, fs_identity
    public :: fs_tree_fingerprint
    public :: fs_realpath, fs_find_executable
    public :: fs_collect_git_checkouts
    public :: fs_is_windows, fs_native_path, fs_path_root_len
    public :: fs_path_is_absolute, fs_path_has_drive, fs_parent_path

    interface
        integer(c_int) function fo_c_is_windows() bind(C, name='fo_c_is_windows')
            import :: c_int
        end function fo_c_is_windows

        integer(c_int) function fo_c_realpath(path, resolved, capacity) &
                bind(C, name='fo_c_realpath')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: resolved(*)
            integer(c_int), value :: capacity
        end function fo_c_realpath

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
                path_needle, recursive, out, cap, reject_aliases) &
                bind(C, name='fo_c_collect_files')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), infix(*), suffix(*)
            character(kind=c_char), intent(in) :: path_needle(*)
            integer(c_int), value :: recursive
            character(kind=c_char), intent(out) :: out(*)
            integer(c_int), value :: cap, reject_aliases
        end function fo_c_collect_files

        integer(c_int) function fo_c_collect_git_checkouts(root, out, cap, &
                item_cap) bind(C, name='fo_c_collect_git_checkouts')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            character(kind=c_char), intent(out) :: out(*)
            integer(c_int), value :: cap, item_cap
        end function fo_c_collect_git_checkouts

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

        integer(c_int) function fo_c_stat_fingerprint(path, mtime_ns, size) &
                bind(C, name='fo_c_stat_fingerprint')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), intent(out) :: mtime_ns, size
        end function fo_c_stat_fingerprint

        integer(c_int) function fo_c_stat_identity(path, device, inode) &
                bind(C, name='fo_c_stat_identity')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), intent(out) :: device, inode
        end function fo_c_stat_identity

        integer(c_int) function fo_c_tree_fingerprint(path, input_mode, sum, &
                mixed, count) bind(C, name='fo_c_tree_fingerprint')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: input_mode
            integer(c_long_long), intent(out) :: sum, mixed, count
        end function fo_c_tree_fingerprint

        integer(c_int) function fo_c_find_executable(command, path, cap) &
                bind(C, name='fo_c_find_executable')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: command(*)
            character(kind=c_char), intent(out) :: path(*)
            integer(c_int), value :: cap
        end function fo_c_find_executable

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

    logical function fs_is_windows() result(windows)
        windows = fo_c_is_windows() /= 0
    end function fs_is_windows

    function fs_native_path(path) result(native)
        !! Keep native drive/UNC paths; POSIX backslashes remain filename bytes.
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: native
        integer :: i

        native = trim(path)
        if (.not. fs_is_windows()) return
        do i = 1, len(native)
            if (native(i:i) == achar(92)) native(i:i) = '/'
        end do
    end function fs_native_path

    logical function fs_path_has_drive(path) result(drive)
        character(len=*), intent(in) :: path
        integer :: code

        drive = .false.
        if (.not. fs_is_windows()) return
        if (len_trim(path) < 2) return
        if (path(2:2) /= ':') return
        code = iachar(path(1:1))
        drive = (code >= iachar('A') .and. code <= iachar('Z')) .or. &
            (code >= iachar('a') .and. code <= iachar('z'))
    end function fs_path_has_drive

    integer function fs_path_root_len(path) result(root)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: native
        integer :: server, share

        native = fs_native_path(path)
        root = 0
        if (len(native) == 0) return
        if (native(1:1) == '/') root = 1
        if (.not. fs_is_windows()) return
        if (fs_path_has_drive(native)) then
            root = 2
            if (len(native) >= 3) then
                if (native(3:3) == '/') root = 3
            end if
            return
        end if
        if (len(native) < 2) return
        if (native(:2) /= '//') return
        root = 2
        server = index(native(3:), '/')
        if (server <= 1) return
        server = server + 2
        if (server >= len(native)) return
        share = index(native(server + 1:), '/')
        if (share == 1) return
        root = len(native)
        if (share > 0) root = server + share
    end function fs_path_root_len

    logical function fs_path_is_absolute(path) result(absolute)
        character(len=*), intent(in) :: path
        integer :: root

        root = fs_path_root_len(path)
        absolute = root > 0
        if (fs_is_windows()) absolute = root >= 3
    end function fs_path_is_absolute

    subroutine fs_parent_path(path, parent)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: parent
        character(len=:), allocatable :: native
        integer :: n, root, slash

        native = fs_native_path(path)
        root = fs_path_root_len(native)
        n = len(native)
        do while (n > max(1, root))
            if (native(n:n) /= '/') exit
            n = n - 1
        end do
        native = native(:n)
        if (n <= root) then
            parent = native
            return
        end if
        slash = index(native, '/', back=.true.)
        parent = '.'
        if (root > 0 .or. slash > 1) parent = native(:max(root, slash - 1))
    end subroutine fs_parent_path

    subroutine fs_remove_tree(path, ierr)
        !! Recursive delete (rm -rf). Missing path is success.
        character(len=*), intent(in) :: path
        integer, intent(out), optional :: ierr
        integer(c_int) :: rc
        if (present(ierr)) ierr = 0
        if (len_trim(path) == 0) return
        rc = fo_c_rm_rf(trim(path)//c_null_char)
        if (present(ierr)) ierr = int(rc)
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
            n_items, recursive, ierr, reject_aliases)
        !! Collect regular files under root whose basename contains infix and
        !! ends with suffix and whose path contains path_needle, into items
        !! (each a full path). Recurses unless recursive is .false. Replaces a
        !! find pipeline.
        character(len=*), intent(in) :: root, infix, suffix, path_needle
        character(len=*), intent(out) :: items(:)
        integer, intent(out) :: n_items
        logical, intent(in), optional :: recursive, reject_aliases
        integer, intent(out), optional :: ierr
        character(kind=c_char), allocatable :: buf(:)
        integer(c_int) :: rc, rec, strict_aliases, capacity

        rec = 1
        if (present(recursive)) then
            if (.not. recursive) rec = 0
        end if
        strict_aliases = 0
        if (present(reject_aliases)) then
            if (reject_aliases) strict_aliases = 1
        end if
        capacity = FS_COLLECT_CAP
        do
            allocate (buf(capacity))
            rc = fo_c_collect_files(trim(root)//c_null_char, &
                trim(infix)//c_null_char, trim(suffix)//c_null_char, &
                trim(path_needle)//c_null_char, rec, buf, capacity, strict_aliases)
            if (rc /= -3) exit
            ! Retry only buffer exhaustion; preserve I/O and alias errors.
            if (capacity > huge(capacity) - capacity) exit
            deallocate (buf)
            capacity = 2*capacity
        end do
        if (present(ierr)) then
            ierr = 0
            if (rc < 0 .or. rc > size(items)) ierr = 1
            if (rc == -2) ierr = 2
        end if
        call unpack_buffer(buf, int(rc), items, n_items)
        call sort_items(items, n_items)
        deallocate (buf)
    end subroutine fs_collect_files

    subroutine fs_collect_git_checkouts(root, items, n_items, ok)
        !! Immediate children of root that have a .git directory or file.
        character(len=*), intent(in) :: root
        character(len=*), intent(out) :: items(:)
        integer, intent(out) :: n_items
        logical, intent(out) :: ok
        character(kind=c_char), allocatable :: buf(:)
        integer(c_int) :: rc

        allocate (buf(FS_COLLECT_CAP))
        rc = fo_c_collect_git_checkouts(trim(root)//c_null_char, buf, &
            int(FS_COLLECT_CAP, c_int), int(len(items), c_int))
        ok = rc >= 0 .and. rc <= size(items)
        n_items = 0
        if (ok) then
            call unpack_buffer(buf, int(rc), items, n_items)
            call sort_items(items, n_items)
        end if
        deallocate (buf)
    end subroutine fs_collect_git_checkouts

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

    subroutine fs_stat(path, mtime_ns, size, ok)
        !! Filesystem fingerprint of path: nanosecond mtime and byte size.
        !! ok is .false. if the path does not exist or cannot be stat'd.
        character(len=*), intent(in) :: path
        integer(c_long_long), intent(out) :: mtime_ns, size
        logical, intent(out) :: ok

        integer(c_int) :: rc

        mtime_ns = 0_c_long_long
        size = 0_c_long_long
        rc = fo_c_stat_fingerprint(trim(path)//c_null_char, mtime_ns, size)
        ok = (rc == 0)
    end subroutine fs_stat

    subroutine fs_identity(path, device, inode, ok)
        !! Return the filesystem device and inode for a path.
        character(len=*), intent(in) :: path
        integer(c_long_long), intent(out) :: device, inode
        logical, intent(out) :: ok

        integer(c_int) :: rc

        device = 0_c_long_long
        inode = 0_c_long_long
        rc = fo_c_stat_identity(trim(path)//c_null_char, device, inode)
        ok = (rc == 0)
    end subroutine fs_identity

    subroutine fs_tree_fingerprint(path, input_mode, sum, mixed, count, ok)
        character(len=*), intent(in) :: path
        logical, intent(in) :: input_mode
        integer(c_long_long), intent(out) :: sum, mixed, count
        logical, intent(out) :: ok

        integer(c_int) :: mode, rc

        mode = 0
        if (input_mode) mode = 1
        sum = 0_c_long_long
        mixed = 0_c_long_long
        count = 0_c_long_long
        rc = fo_c_tree_fingerprint(trim(path)//c_null_char, mode, sum, mixed, count)
        ok = (rc == 0)
    end subroutine fs_tree_fingerprint

    subroutine fs_realpath(path, resolved, ok)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: resolved
        logical, intent(out) :: ok
        integer(c_int) :: rc
        integer :: nul

        resolved = ''
        rc = fo_c_realpath(trim(path)//c_null_char, resolved, int(len(resolved), c_int))
        ok = rc == 0
        if (.not. ok) then
            resolved = ''
            return
        end if
        nul = index(resolved, achar(0))
        if (nul > 0) resolved(nul:) = ' '
    end subroutine fs_realpath

    subroutine fs_find_executable(command, path, ok)
        character(len=*), intent(in) :: command
        character(len=*), intent(out) :: path
        logical, intent(out) :: ok

        character(kind=c_char), allocatable :: buf(:)
        integer(c_int) :: rc
        integer :: i

        path = ''
        allocate (buf(len(path) + 1))
        rc = fo_c_find_executable(trim(command)//c_null_char, buf, &
            int(size(buf), c_int))
        ok = rc == 0
        if (.not. ok) return
        do i = 1, size(buf)
            if (buf(i) == c_null_char) then
                call chars_to_str(buf, 1, i - 1, path)
                return
            end if
        end do
        ok = .false.
    end subroutine fs_find_executable

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
