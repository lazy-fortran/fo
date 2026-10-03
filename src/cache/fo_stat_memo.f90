module fo_stat_memo
    !! Persistent file-hash memo: maps (path, mtime, ctime, size) -> sha256 so a
    !! warm build never re-reads an unchanged file to hash it. ctime detects a
    !! same-size rewrite even when the original mtime is restored.
    !!
    !! The memo lives under the shared cache root so it survives a project-scoped
    !! `fo clean` and is reused across builds. Access is guarded by a named
    !! critical so the parallel link loop can hash program objects safely.
    use fx_hash, only: sha256_file, fnv1a_string
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    implicit none
    private
    public :: memo_hash_file, memo_save, memo_reset

    integer, parameter :: CAP = 16384 ! power of two, >> any real build
    integer, parameter :: PATH_LEN = 512
    integer, parameter :: HASH_LEN = 64
    integer, parameter :: HASH_ATTEMPTS = 3

    interface
        integer(c_int) function fo_c_mkstemp(path) bind(C, name='mkstemp')
            import :: c_char, c_int
            character(kind=c_char), intent(inout) :: path(*)
        end function fo_c_mkstemp

        integer(c_int) function fo_c_close(fd) bind(C, name='close')
            import :: c_int
            integer(c_int), value :: fd
        end function fo_c_close
    end interface

    interface
        integer(c_int) function fo_c_stat_change_fingerprint( &
                path, mtime_ns, ctime_ns, size) &
                bind(C, name='fo_c_stat_change_fingerprint')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), intent(out) :: mtime_ns, ctime_ns, size
        end function fo_c_stat_change_fingerprint
    end interface

    character(len=PATH_LEN), save :: t_path(CAP)
    integer(c_long_long), save :: t_mtime(CAP) = 0_c_long_long
    integer(c_long_long), save :: t_ctime(CAP) = 0_c_long_long
    integer(c_long_long), save :: t_size(CAP) = 0_c_long_long
    character(len=HASH_LEN), save :: t_hash(CAP)
    logical, save :: t_used(CAP) = .false.
    logical, save :: loaded = .false.
    logical, save :: dirty = .false.

contains

    subroutine memo_hash_file(path, hash, ierr)
        !! sha256 of path, served from the memo when its complete stat key matches.
        !! ierr is nonzero only when the file cannot be hashed at all.
        character(len=*), intent(in) :: path
        character(len=HASH_LEN), intent(out) :: hash
        integer, intent(out) :: ierr

        integer(c_long_long) :: mt, ct, sz
        integer :: slot
        integer(c_int) :: stat_rc
        logical :: stable, cache_hit

        hash = ''
        ierr = 0

        !$omp critical (fo_stat_memo)
        if (.not. loaded) call load_impl()
        stat_rc = fo_c_stat_change_fingerprint( &
            trim(path)//c_null_char, mt, ct, sz)
        if (stat_rc /= 0) then
            call sha256_file(path, hash, ierr)
        else
            slot = find_slot(path)
            cache_hit = .false.
            if (slot > 0) then
                cache_hit = t_used(slot) .and. &
                    trim(t_path(slot)) == trim(path) .and. &
                    t_mtime(slot) == mt .and. t_ctime(slot) == ct .and. &
                    t_size(slot) == sz
            end if
            if (cache_hit) then
                hash = t_hash(slot)
            else
                call hash_with_stable_stat(path, mt, ct, sz, hash, ierr, stable)
                if (ierr == 0 .and. stable .and. slot > 0) then
                    t_used(slot) = .true.
                    t_path(slot) = trim(path)
                    t_mtime(slot) = mt
                    t_ctime(slot) = ct
                    t_size(slot) = sz
                    t_hash(slot) = hash
                    dirty = .true.
                end if
            end if
        end if
        !$omp end critical (fo_stat_memo)
    end subroutine memo_hash_file

    subroutine hash_with_stable_stat(path, mtime_ns, ctime_ns, size, hash, &
            ierr, stable)
        !! Cache only a digest bracketed by matching pre/post stat snapshots.
        character(len=*), intent(in) :: path
        integer(c_long_long), intent(inout) :: mtime_ns, ctime_ns, size
        character(len=HASH_LEN), intent(out) :: hash
        integer, intent(out) :: ierr
        logical, intent(out) :: stable

        integer(c_long_long) :: after_mtime, after_ctime, after_size
        integer(c_int) :: stat_rc
        integer :: attempt

        stable = .false.
        ierr = 0
        hash = ''
        do attempt = 1, HASH_ATTEMPTS
            call sha256_file(path, hash, ierr)
            if (ierr /= 0) return

            stat_rc = fo_c_stat_change_fingerprint( &
                trim(path)//c_null_char, after_mtime, after_ctime, after_size)
            if (stat_rc /= 0) return
            if (mtime_ns == after_mtime .and. ctime_ns == after_ctime .and. &
                size == after_size) then
                stable = .true.
                return
            end if

            mtime_ns = after_mtime
            ctime_ns = after_ctime
            size = after_size
        end do
    end subroutine hash_with_stable_stat

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
        !! Persist through a unique, exclusively-created sibling and publish it
        !! only after every write and close succeeded. Concurrent processes may
        !! replace one another's snapshots, but readers always see a whole file.
        character(len=PATH_LEN) :: file
        character(len=PATH_LEN + 32) :: tmp
        character(len=PATH_LEN * 2 + 2) :: quoted_path
        character(kind=c_char) :: ctmp(PATH_LEN + 32)
        integer :: u, ios, close_ios, i
        integer(c_int) :: fd, close_rc
        logical :: ok

        !$omp critical (fo_stat_memo)
        if (dirty) then
            call memo_file(file)
            if (len_trim(file) > 0) then
                call ensure_parent(file)
                call make_temp_template(file, tmp, ctmp, ok)
                if (ok) then
                    fd = fo_c_mkstemp(ctmp)
                else
                    fd = -1_c_int
                end if
                if (fd >= 0_c_int) then
                    close_rc = fo_c_close(fd)
                    if (close_rc /= 0_c_int) then
                        call temp_path_string(ctmp, tmp)
                        call remove_temp(tmp)
                    else
                        call temp_path_string(ctmp, tmp)
                        open (newunit=u, file=trim(tmp), status='old', &
                            action='write', access='sequential', &
                            form='formatted', position='rewind', iostat=ios)
                        if (ios == 0) then
                            close_ios = 0
                            do i = 1, CAP
                                if (.not. t_used(i)) cycle
                                quoted_path = quote_path(trim(t_path(i)))
                                write (u, '(i0,1x,i0,1x,i0,1x,a,1x,a)', &
                                    iostat=ios) t_mtime(i), t_ctime(i), &
                                    t_size(i), trim(t_hash(i)), trim(quoted_path)
                                if (ios /= 0) exit
                            end do
                            close (u, iostat=close_ios)
                            if (ios == 0 .and. close_ios == 0) then
                                if (rename_file(tmp, file)) then
                                    dirty = .false.
                                else
                                    call remove_temp(tmp)
                                end if
                            else
                                call remove_temp(tmp)
                            end if
                        else
                            call remove_temp(tmp)
                        end if
                    end if
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
        integer(c_long_long) :: mt, ct, sz
        integer :: u, ios, slot

        loaded = .true.
        call memo_file(file)
        if (len_trim(file) == 0) return
        open (newunit=u, file=trim(file), status='old', iostat=ios)
        if (ios /= 0) return
        do
            h = ''
            path = ''
            read (u, *, iostat=ios) mt, ct, sz, h, path
            if (ios /= 0) exit
            if (len_trim(h) /= HASH_LEN) cycle
            if (verify(trim(h), '0123456789abcdefABCDEF') /= 0) cycle
            if (len_trim(path) == 0) cycle
            slot = find_slot(path)
            if (slot > 0) then
                t_used(slot) = .true.
                t_path(slot) = trim(path)
                t_mtime(slot) = mt
                t_ctime(slot) = ct
                t_size(slot) = sz
                t_hash(slot) = h
            end if
        end do
        close (u)
    end subroutine load_impl

    subroutine memo_file(path)
        !! <cache_root>/stat/v2/memo, honoring FO_CACHE_DIR like fo_cache does.
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
        path = trim(root)//'/stat/v2/memo'
    end subroutine memo_file

    subroutine ensure_parent(file)
        !! mkdir -p of the memo's parent dir via shell-free C primitive.
        use fo_fs, only: fs_make_dir
        character(len=*), intent(in) :: file
        integer :: s

        s = index(file, '/', back=.true.)
        if (s > 1) call fs_make_dir(file(1:s - 1))
    end subroutine ensure_parent

    subroutine make_temp_template(file, tmp, ctmp, ok)
        character(len=*), intent(in) :: file
        character(len=*), intent(out) :: tmp
        character(kind=c_char), intent(out) :: ctmp(:)
        logical, intent(out) :: ok
        integer :: i, n

        tmp = trim(file)//'.tmp.XXXXXX'
        n = len_trim(tmp)
        ok = n < size(ctmp)
        ctmp = c_null_char
        if (.not. ok) return
        do i = 1, n
            ctmp(i) = tmp(i:i)
        end do
    end subroutine make_temp_template

    function quote_path(path) result(quoted)
        character(len=*), intent(in) :: path
        character(len=PATH_LEN * 2 + 2) :: quoted
        character(len=1), parameter :: quote = achar(39)
        integer :: i, j

        quoted = ''
        j = 1
        quoted(j:j) = quote
        j = j + 1
        do i = 1, len_trim(path)
            quoted(j:j) = path(i:i)
            j = j + 1
            if (path(i:i) == quote) then
                quoted(j:j) = quote
                j = j + 1
            end if
        end do
        quoted(j:j) = quote
    end function quote_path

    subroutine temp_path_string(ctmp, tmp)
        character(kind=c_char), intent(in) :: ctmp(:)
        character(len=*), intent(out) :: tmp
        integer :: i

        tmp = ''
        do i = 1, min(size(ctmp), len(tmp))
            if (ctmp(i) == c_null_char) exit
            tmp(i:i) = ctmp(i)
        end do
    end subroutine temp_path_string

    subroutine remove_temp(tmp)
        use fo_fs, only: fs_remove_file
        character(len=*), intent(in) :: tmp
        if (len_trim(tmp) > 0) call fs_remove_file(trim(tmp))
    end subroutine remove_temp

    logical function rename_file(src, dst)
        use fo_fs, only: fs_rename
        character(len=*), intent(in) :: src, dst
        integer :: rc

        rc = fs_rename(src, dst)
        rename_file = rc == 0
    end function rename_file

end module fo_stat_memo
