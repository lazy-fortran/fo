module fo_stat_memo
    !! Persistent file-hash memo: maps (path, mtime, size) -> sha256 so a warm
    !! build never re-reads an unchanged file to hash it. This is the go/Bazel
    !! "stat first, hash only on change" trick. Without it, every build re-hashes
    !! all sources (for compile keys) and all objects/archives (for link keys);
    !! with it, a warm build is stat-bound, not read-bound.
    !!
    !! The memo lives under the shared cache root so it survives a project-scoped
    !! `fo clean` and is reused across builds. Access is guarded by a named
    !! critical so the parallel link loop can hash program objects safely.
    use fx_hash, only: sha256_file, fnv1a_string
    use fo_fs, only: fs_stat
    use, intrinsic :: iso_c_binding, only: c_long_long
    implicit none
    private
    public :: memo_hash_file, memo_save, memo_reset

    integer, parameter :: CAP = 16384          ! power of two, >> any real build
    integer, parameter :: PATH_LEN = 512
    integer, parameter :: HASH_LEN = 64

    character(len=PATH_LEN), save :: t_path(CAP)
    integer(c_long_long), save :: t_mtime(CAP) = 0_c_long_long
    integer(c_long_long), save :: t_size(CAP) = 0_c_long_long
    character(len=HASH_LEN), save :: t_hash(CAP)
    logical, save :: t_used(CAP) = .false.
    logical, save :: loaded = .false.
    logical, save :: dirty = .false.

contains

    subroutine memo_hash_file(path, hash, ierr)
        !! sha256 of path, served from the memo when (mtime, size) are unchanged.
        !! ierr is nonzero only when the file cannot be hashed at all.
        character(len=*), intent(in) :: path
        character(len=HASH_LEN), intent(out) :: hash
        integer, intent(out) :: ierr

        integer(c_long_long) :: mt, sz
        integer :: slot
        logical :: ok

        hash = ''
        ierr = 0

        !$omp critical (fo_stat_memo)
        if (.not. loaded) call load_impl()
        call fs_stat(path, mt, sz, ok)
        if (.not. ok) then
            call sha256_file(path, hash, ierr)
        else
            slot = find_slot(path)
            if (slot > 0) then
                if (t_used(slot) .and. trim(t_path(slot)) == trim(path) .and. &
                    t_mtime(slot) == mt .and. t_size(slot) == sz) then
                    hash = t_hash(slot)
                else
                    call sha256_file(path, hash, ierr)
                    if (ierr == 0) then
                        t_used(slot) = .true.
                        t_path(slot) = trim(path)
                        t_mtime(slot) = mt
                        t_size(slot) = sz
                        t_hash(slot) = hash
                        dirty = .true.
                    end if
                end if
            else
                call sha256_file(path, hash, ierr)
            end if
        end if
        !$omp end critical (fo_stat_memo)
    end subroutine memo_hash_file

    integer function find_slot(path) result(slot)
        !! Open-addressed slot for path: the entry matching path, or the first
        !! empty slot to claim. 0 if the table is full (caller falls back to a
        !! direct hash without caching).
        character(len=*), intent(in) :: path
        integer(8) :: h
        integer :: start, i, idx

        h = fnv1a_string(trim(path))
        start = int(modulo(h, int(CAP, 8))) + 1
        do i = 0, CAP - 1
            idx = start + i
            if (idx > CAP) idx = idx - CAP
            if (.not. t_used(idx)) then
                slot = idx
                return
            end if
            if (trim(t_path(idx)) == trim(path)) then
                slot = idx
                return
            end if
        end do
        slot = 0
    end function find_slot

    subroutine memo_save()
        !! Persist the memo atomically (temp + rename) if anything changed.
        character(len=PATH_LEN) :: file, tmp
        integer :: u, ios, i

        !$omp critical (fo_stat_memo)
        if (dirty) then
            call memo_file(file)
            if (len_trim(file) > 0) then
                tmp = trim(file)//'.tmp'
                call ensure_parent(file)
                open (newunit=u, file=trim(tmp), status='replace', iostat=ios)
                if (ios == 0) then
                    do i = 1, CAP
                        if (.not. t_used(i)) cycle
                        write (u, '(i0,1x,i0,1x,a,1x,a)') t_mtime(i), &
                            t_size(i), trim(t_hash(i)), trim(t_path(i))
                    end do
                    close (u)
                    call rename_file(tmp, file)
                    dirty = .false.
                end if
            end if
        end if
        !$omp end critical (fo_stat_memo)
    end subroutine memo_save

    subroutine memo_reset()
        !! Drop in-memory state (tests use this to force a reload).
        !$omp critical (fo_stat_memo)
        t_used = .false.
        loaded = .false.
        dirty = .false.
        !$omp end critical (fo_stat_memo)
    end subroutine memo_reset

    subroutine load_impl()
        !! Load persisted entries into the table. Caller holds the critical.
        character(len=PATH_LEN) :: file, path
        character(len=HASH_LEN) :: h
        integer(c_long_long) :: mt, sz
        integer :: u, ios, slot

        loaded = .true.
        call memo_file(file)
        if (len_trim(file) == 0) return
        open (newunit=u, file=trim(file), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, *, iostat=ios) mt, sz, h, path
            if (ios /= 0) exit
            slot = find_slot(path)
            if (slot > 0) then
                t_used(slot) = .true.
                t_path(slot) = trim(path)
                t_mtime(slot) = mt
                t_size(slot) = sz
                t_hash(slot) = h
            end if
        end do
        close (u)
    end subroutine load_impl

    subroutine memo_file(path)
        !! <cache_root>/stat/v1/memo, honoring FO_CACHE_DIR like fo_cache does.
        !! Resolved here (not via fo_cache) to keep this module dependency-free.
        character(len=*), intent(out) :: path
        character(len=PATH_LEN) :: root

        call get_environment_variable('FO_CACHE_DIR', root)
        if (len_trim(root) == 0) then
            call get_environment_variable('HOME', root)
            if (len_trim(root) == 0) then
                path = ''
                return
            end if
            root = trim(root)//'/.cache/fo'
        end if
        path = trim(root)//'/stat/v1/memo'
    end subroutine memo_file

    subroutine ensure_parent(file)
        !! mkdir -p of the memo's parent dir via shell-free C primitive.
        use fo_fs, only: fs_make_dir
        character(len=*), intent(in) :: file
        integer :: s

        s = index(file, '/', back=.true.)
        if (s > 1) call fs_make_dir(file(1:s - 1))
    end subroutine ensure_parent

    subroutine rename_file(src, dst)
        use fo_fs, only: fs_rename
        character(len=*), intent(in) :: src, dst
        integer :: rc

        rc = fs_rename(src, dst)
    end subroutine rename_file

end module fo_stat_memo
