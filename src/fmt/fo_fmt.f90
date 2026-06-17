module fo_fmt
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_fs, only: fs_make_dir, fs_write_text, fs_append_file, fs_collect_files
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
    use fo_cache, only: cache_store_root, HASH_LEN
    use fx_hash, only: sha256_file
    use fo_format, only: format_file
    implicit none
    private
    public :: fo_fmt_run, fo_fmt_check_run

contains

    subroutine write_source_list(scan_root, list_file)
        !! Write the sorted list of *.f90/*.F90 files under scan_root, excluding
        !! any */build or */.git path component, into list_file (one per line).
        !! Replaces the `find ... | sort > list_file` pipeline.
        character(len=*), intent(in) :: scan_root, list_file

        character(len=512), allocatable :: hits(:)
        character(len=512) :: key
        integer :: n_hits, n_files, s, i, j, u, ios
        character(len=4), parameter :: suffixes(2) = ['.f90', '.F90']
        character(len=512), allocatable :: files(:)

        allocate (files(20000))
        allocate (hits(20000))
        n_files = 0
        do s = 1, size(suffixes)
            call fs_collect_files(trim(scan_root), '', trim(suffixes(s)), '', &
                hits, n_hits)
            do i = 1, n_hits
                if (index(hits(i), '/build/') > 0) cycle
                if (index(hits(i), '/.git/') > 0) cycle
                ! Vendored/virtualenv trees (numpy ships .f90 under site-packages)
                ! are not project sources; the build scanner skips them too, so
                ! the format check must not flag them. Keep the two scanners in sync.
                if (index(hits(i), '/.venv/') > 0) cycle
                if (index(hits(i), '/venv/') > 0) cycle
                if (index(hits(i), '/site-packages/') > 0) cycle
                if (index(hits(i), '/node_modules/') > 0) cycle
                if (n_files >= size(files)) exit
                n_files = n_files + 1
                files(n_files) = hits(i)
            end do
        end do
        ! Each suffix block is sorted; merge the union into a sorted order.
        do i = 2, n_files
            key = files(i)
            j = i - 1
            do while (j >= 1)
                if (llt(files(j), key) .or. files(j) == key) exit
                files(j + 1) = files(j)
                j = j - 1
            end do
            files(j + 1) = key
        end do

        open (newunit=u, file=trim(list_file), status='replace', iostat=ios)
        if (ios == 0) then
            do i = 1, n_files
                write (u, '(a)') trim(files(i))
            end do
            close (u)
        end if
        deallocate (hits)
        deallocate (files)
    end subroutine write_source_list

    subroutine fo_fmt_run(dir, exitcode)
        character(len=*), intent(in) :: dir
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=512) :: scan_root, list_file, fpath
        integer :: u, ios, fmt_exit

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        exitcode = 0
        call make_tmpfile('fo_fmt_files', list_file)
        call write_source_list(scan_root, list_file)

        open (newunit=u, file=trim(list_file), status='old', iostat=ios)
        if (ios /= 0) then
            call delete_tmpfile(list_file)
            return
        end if
        do
            read (u, '(a)', iostat=ios) fpath
            if (ios /= 0) exit
            if (len_trim(fpath) == 0) cycle
            call format_file(trim(fpath), fmt_exit)
            if (fmt_exit /= 0) exitcode = 1
        end do
        close (u)
        call delete_tmpfile(list_file)
    end subroutine fo_fmt_run

    subroutine fo_fmt_check_run(dir, output, exitcode)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: output
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=512) :: scan_root, list_file, fpath, tmpf
        character(len=HASH_LEN) :: action_id, orig_hash, fmt_hash
        integer :: u, ios, n_bad, fmt_exit, ho, hf

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        exitcode = 0
        output = ''

        n_bad = 0
        call make_tmpfile('fo_fmt_files', list_file)
        call write_source_list(scan_root, list_file)

        call fmt_action_id(list_file, action_id)
        if (len_trim(action_id) == HASH_LEN .and. fmt_marker_exists(action_id)) then
            call delete_tmpfile(list_file)
            return
        end if

        open (newunit=u, file=trim(list_file), status='old', iostat=ios)
        if (ios /= 0) then
            call delete_tmpfile(list_file)
            return
        end if

        do
            read (u, '(a)', iostat=ios) fpath
            if (ios /= 0) exit
            if (len_trim(fpath) == 0) cycle

            call make_tmpfile('fo_fmt_check', tmpf)
            call fs_append_file(trim(fpath), trim(tmpf))
            call format_file(trim(tmpf), fmt_exit)
            if (fmt_exit /= 0) then
                call delete_tmpfile(tmpf)
                cycle
            end if
            ! diff -q via byte-exact content hash of original vs formatted copy.
            call sha256_file(trim(fpath), orig_hash, ho)
            call sha256_file(trim(tmpf), fmt_hash, hf)
            call delete_tmpfile(tmpf)

            if (ho /= 0 .or. hf /= 0 .or. orig_hash /= fmt_hash) then
                n_bad = n_bad + 1
                output = trim(output)//trim(fpath)//': needs formatting'//achar(10)
            end if
        end do
        close (u)
        call delete_tmpfile(list_file)

        if (n_bad > 0) then
            exitcode = 1
        else if (len_trim(action_id) == HASH_LEN) then
            call store_fmt_marker(action_id)
        end if
    end subroutine fo_fmt_check_run

    subroutine fmt_action_id(list_file, action_id)
        character(len=*), intent(in) :: list_file
        character(len=*), intent(out) :: action_id

        character(len=512) :: manifest, fpath
        character(len=HASH_LEN) :: file_hash
        integer :: in_u, out_u, ios, ierr

        action_id = ''
        call make_tmpfile('fo_fmt_manifest', manifest)

        open (newunit=out_u, file=trim(manifest), status='replace', iostat=ios)
        if (ios /= 0) then
            call delete_tmpfile(manifest)
            return
        end if
        write (out_u, '(a)') 'fo-fmt-native-v1'

        open (newunit=in_u, file=trim(list_file), status='old', iostat=ios)
        if (ios == 0) then
            do
                read (in_u, '(a)', iostat=ios) fpath
                if (ios /= 0) exit
                if (len_trim(fpath) == 0) cycle
                call sha256_file(trim(fpath), file_hash, ierr)
                if (ierr == 0) write (out_u, '(a,1x,a)') trim(file_hash), trim(fpath)
            end do
            close (in_u)
        end if
        close (out_u)

        call sha256_file(trim(manifest), action_id, ierr)
        if (ierr /= 0) action_id = ''
        call delete_tmpfile(manifest)
    end subroutine fmt_action_id

    logical function fmt_marker_exists(action_id) result(found)
        character(len=*), intent(in) :: action_id

        character(len=512) :: path

        call fmt_marker_path(action_id, path)
        inquire (file=trim(path), exist=found)
    end function fmt_marker_exists

    subroutine store_fmt_marker(action_id)
        character(len=*), intent(in) :: action_id

        character(len=512) :: path

        call fmt_marker_path(action_id, path)
        call fs_make_dir(dirname(path))
        call fs_write_text(trim(path), '1')
    end subroutine store_fmt_marker

    subroutine fmt_marker_path(action_id, path)
        character(len=*), intent(in) :: action_id
        character(len=*), intent(out) :: path

        character(len=512) :: root

        call cache_store_root(root)
        path = trim(root)//'/'//action_id(1:2)//'/'//trim(action_id)//'-fmt'
    end subroutine fmt_marker_path

    pure function dirname(path) result(dir)
        character(len=*), intent(in) :: path
        character(len=len_trim(path)) :: dir

        integer :: slash

        slash = index(trim(path), '/', back=.true.)
        if (slash > 1) then
            dir = path(1:slash - 1)
        else
            dir = '.'
        end if
    end function dirname

end module fo_fmt
