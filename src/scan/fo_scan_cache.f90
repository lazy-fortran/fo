module fo_scan_cache
    use fo_scan_types, only: scan_unit_t, MAX_PATH, MAX_UNITS
    use fo_fs, only: fs_make_dir, fs_rename, fs_stat
    use fo_util, only: make_sibling_tmpfile, delete_tmpfile
    use fx_hash, only: fnv1a_string
    use, intrinsic :: iso_c_binding, only: c_long_long
    implicit none
    private

    character(len=16), parameter :: CACHE_MAGIC = 'fo-scan-v1'

    public :: scan_cache_load, scan_cache_load_trusted, scan_cache_save

contains

    subroutine scan_cache_load(root, paths, units, hit)
        character(len=*), intent(in) :: root
        character(len=*), intent(in) :: paths(:)
        type(scan_unit_t), intent(out) :: units(:)
        logical, intent(out) :: hit

        character(len=MAX_PATH) :: file, stored_root, stored_path
        character(len=16) :: magic
        integer(c_long_long) :: stored_mtime, stored_size, mtime, bytes
        integer :: u, ios, i, n_stored
        logical :: ok

        hit = .false.
        call cache_file(root, file)
        if (len_trim(file) == 0) return
        open (newunit=u, file=trim(file), access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (u, iostat=ios) magic, stored_root, n_stored
        if (ios /= 0 .or. magic /= CACHE_MAGIC .or. &
            trim(stored_root) /= trim(root) .or. n_stored /= size(paths)) then
            close (u)
            return
        end if
        do i = 1, size(paths)
            read (u, iostat=ios) stored_path, stored_mtime, stored_size, units(i)
            if (ios /= 0 .or. trim(stored_path) /= trim(paths(i))) then
                close (u)
                return
            end if
            call fs_stat(paths(i), mtime, bytes, ok)
            if (.not. ok) then
                close (u)
                return
            end if
            if (mtime /= stored_mtime .or. bytes /= stored_size) then
                close (u)
                return
            end if
        end do
        close (u)
        hit = .true.
    end subroutine scan_cache_load

    subroutine scan_cache_load_trusted(root, units, hit)
        !! Load a scan after a caller has independently validated the complete
        !! source tree (the warm build stamp does this). This avoids enumerating
        !! the directory a second time on the warm test path.
        character(len=*), intent(in) :: root
        type(scan_unit_t), allocatable, intent(out) :: units(:)
        logical, intent(out) :: hit

        character(len=MAX_PATH) :: file, stored_root, stored_path
        character(len=16) :: magic
        integer(c_long_long) :: stored_mtime, stored_size
        integer :: u, ios, i, n_stored

        hit = .false.
        allocate (units(0))
        call cache_file(root, file)
        if (len_trim(file) == 0) return
        open (newunit=u, file=trim(file), access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (u, iostat=ios) magic, stored_root, n_stored
        if (ios /= 0 .or. magic /= CACHE_MAGIC .or. &
            trim(stored_root) /= trim(root) .or. n_stored < 0 .or. &
            n_stored > MAX_UNITS) then
            close (u)
            return
        end if
        deallocate (units)
        allocate (units(n_stored))
        do i = 1, n_stored
            read (u, iostat=ios) stored_path, stored_mtime, stored_size, units(i)
            if (ios /= 0) then
                close (u)
                deallocate (units)
                allocate (units(0))
                return
            end if
        end do
        close (u)
        hit = .true.
    end subroutine scan_cache_load_trusted

    subroutine scan_cache_save(root, paths, units)
        character(len=*), intent(in) :: root
        character(len=*), intent(in) :: paths(:)
        type(scan_unit_t), intent(in) :: units(:)

        character(len=MAX_PATH) :: file, tmpfile, stored_root, stored_path
        integer(c_long_long) :: mtime, bytes
        integer :: u, ios, i, rc
        logical :: ok

        if (size(paths) /= size(units)) return
        call cache_file(root, file)
        if (len_trim(file) == 0) return
        call make_sibling_tmpfile(file, tmpfile)
        open (newunit=u, file=trim(tmpfile), access='stream', form='unformatted', &
            status='replace', action='write', iostat=ios)
        if (ios /= 0) return
        stored_root = root
        write (u, iostat=ios) CACHE_MAGIC, stored_root, size(paths)
        if (ios /= 0) then
            close (u)
            call delete_tmpfile(tmpfile)
            return
        end if
        do i = 1, size(paths)
            call fs_stat(paths(i), mtime, bytes, ok)
            if (.not. ok) then
                close (u)
                call delete_tmpfile(tmpfile)
                return
            end if
            stored_path = paths(i)
            write (u, iostat=ios) stored_path, mtime, bytes, units(i)
            if (ios /= 0) then
                close (u)
                call delete_tmpfile(tmpfile)
                return
            end if
        end do
        close (u)
        rc = fs_rename(tmpfile, file)
        if (rc /= 0) call delete_tmpfile(tmpfile)
    end subroutine scan_cache_save

    subroutine cache_file(root, path)
        character(len=*), intent(in) :: root
        character(len=*), intent(out) :: path

        character(len=MAX_PATH) :: cache_root, directory
        character(len=16) :: key
        integer(8) :: hash

        path = ''
        if (len_trim(root) == 0) return
        if (root(1:1) /= '/') return
        call get_environment_variable('FO_CACHE_DIR', cache_root)
        if (len_trim(cache_root) == 0) then
            call get_environment_variable('HOME', cache_root)
            if (len_trim(cache_root) == 0) return
            cache_root = trim(cache_root)//'/.cache/fo'
        end if
        directory = trim(cache_root)//'/scan/v1'
        call fs_make_dir(directory)
        hash = fnv1a_string(trim(root))
        write (key, '(z16.16)') hash
        path = trim(directory)//'/'//key
    end subroutine cache_file

end module fo_scan_cache
