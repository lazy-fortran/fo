module fo_compiler_memo
    use fo_fs, only: fs_find_executable, fs_make_dir, fs_rename, fs_stat
    use fo_util, only: make_sibling_tmpfile, delete_tmpfile
    use fx_hash, only: fnv1a_string
    use, intrinsic :: iso_c_binding, only: c_long_long
    implicit none
    private

    character(len=16), parameter :: MEMO_MAGIC = 'fo-compiler-v1'

    public :: compiler_memo_load, compiler_memo_save

contains

    subroutine compiler_memo_load(command, identity, hit)
        character(len=*), intent(in) :: command
        character(len=*), intent(out) :: identity
        logical, intent(out) :: hit

        character(len=512) :: file, executable, stored_command, stored_executable
        character(len=512) :: stored_identity
        character(len=16) :: magic
        integer(c_long_long) :: mtime, bytes, stored_mtime, stored_bytes
        integer :: u, ios
        logical :: ok

        identity = ''
        hit = .false.
        call fs_find_executable(command, executable, ok)
        if (.not. ok) return
        call memo_file(command, file)
        if (len_trim(file) == 0) return
        open (newunit=u, file=trim(file), access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (u, iostat=ios) magic, stored_command, stored_executable, &
            stored_mtime, stored_bytes, stored_identity
        close (u)
        if (ios /= 0) return
        if (magic /= MEMO_MAGIC) return
        if (trim(stored_command) /= trim(command)) return
        if (trim(stored_executable) /= trim(executable)) return
        call fs_stat(executable, mtime, bytes, ok)
        if (.not. ok) return
        if (mtime /= stored_mtime .or. bytes /= stored_bytes) return
        identity = stored_identity
        hit = len_trim(identity) > 0
    end subroutine compiler_memo_load

    subroutine compiler_memo_save(command, identity)
        character(len=*), intent(in) :: command, identity

        character(len=512) :: file, tmpfile, executable
        character(len=512) :: stored_command, stored_executable, stored_identity
        integer(c_long_long) :: mtime, bytes
        integer :: u, ios, rc
        logical :: ok

        call fs_find_executable(command, executable, ok)
        if (.not. ok) return
        call fs_stat(executable, mtime, bytes, ok)
        if (.not. ok) return
        call memo_file(command, file)
        if (len_trim(file) == 0) return
        call make_sibling_tmpfile(file, tmpfile)
        open (newunit=u, file=trim(tmpfile), access='stream', form='unformatted', &
            status='replace', action='write', iostat=ios)
        if (ios /= 0) return
        stored_command = command
        stored_executable = executable
        stored_identity = identity
        write (u, iostat=ios) MEMO_MAGIC, stored_command, stored_executable, &
            mtime, bytes, stored_identity
        close (u)
        if (ios /= 0) then
            call delete_tmpfile(tmpfile)
            return
        end if
        rc = fs_rename(tmpfile, file)
        if (rc /= 0) call delete_tmpfile(tmpfile)
    end subroutine compiler_memo_save

    subroutine memo_file(command, path)
        character(len=*), intent(in) :: command
        character(len=*), intent(out) :: path

        character(len=512) :: cache_root, directory
        character(len=16) :: key
        integer(8) :: hash

        path = ''
        call get_environment_variable('FO_CACHE_DIR', cache_root)
        if (len_trim(cache_root) == 0) then
            call get_environment_variable('HOME', cache_root)
            if (len_trim(cache_root) == 0) return
            cache_root = trim(cache_root)//'/.cache/fo'
        end if
        directory = trim(cache_root)//'/compiler/v1'
        call fs_make_dir(directory)
        hash = fnv1a_string(trim(command))
        write (key, '(z16.16)') hash
        path = trim(directory)//'/'//key
    end subroutine memo_file

end module fo_compiler_memo
