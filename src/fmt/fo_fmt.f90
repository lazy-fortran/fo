module fo_fmt
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_fs, only: fs_make_dir, fs_write_text, fs_append_file, fs_collect_files
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
    use fo_cache, only: cache_store_root, HASH_LEN
    use fx_hash, only: sha256_file
    use fo_format, only: format_file
    use fo_process, only: process_run_argv_logged, argv_push
    implicit none
    private
    public :: fo_fmt_run, fo_fmt_files, fo_fmt_changed_run
    public :: fo_fmt_check_run, fo_fmt_check_files
    public :: fo_fmt_check_changed_run

contains

    subroutine write_source_list(scan_root, list_file)
        !! Write the sorted list of *.f90/*.F90 project sources under scan_root.
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
                if (skip_generated_source(scan_root, hits(i))) cycle
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

    logical function skip_generated_source(root, path)
        character(len=*), intent(in) :: root, path

        character(len=512) :: rel, first, padded
        integer :: root_len, slash

        rel = trim(path)
        root_len = len_trim(root)
        if (root_len > 0 .and. len_trim(path) > root_len) then
            if (path(1:root_len) == root(1:root_len)) then
                rel = path(root_len + 1:)
                if (rel(1:1) == '/') rel = rel(2:)
            end if
        end if

        first = rel
        slash = index(first, '/')
        if (slash > 0) first = first(1:slash - 1)

        skip_generated_source = .true.
        if (index(trim(first), 'build') == 1) return
        if (trim(first) == 'SRC') return
        if (len_trim(first) > 0 .and. first(1:1) == '.') return

        padded = '/'//trim(rel)//'/'
        if (index(padded, '/_deps/') > 0) return
        if (index(padded, '/dependencies/') > 0) return
        if (index(padded, '/deps-src/') > 0) return
        if (index(padded, '/.git/') > 0) return
        if (index(padded, '/.venv/') > 0) return
        if (index(padded, '/venv/') > 0) return
        if (index(padded, '/site-packages/') > 0) return
        if (index(padded, '/node_modules/') > 0) return

        skip_generated_source = .false.
    end function skip_generated_source

    subroutine fo_fmt_run(dir, exitcode)
        character(len=*), intent(in) :: dir
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=512) :: scan_root, list_file, config_file

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        call find_fprettify_config(scan_root, config_file)

        exitcode = 0
        call make_tmpfile('fo_fmt_files', list_file)
        call write_source_list(scan_root, list_file)

        call fo_fmt_list(config_file, list_file, exitcode)
        call delete_tmpfile(list_file)
    end subroutine fo_fmt_run

    subroutine fo_fmt_files(dir, files, n_files, exitcode)
        character(len=*), intent(in) :: dir
        character(len=*), intent(in) :: files(:)
        integer, intent(in) :: n_files
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=512) :: scan_root, list_file, config_file

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        call find_fprettify_config(scan_root, config_file)

        call make_tmpfile('fo_fmt_files', list_file)
        call write_explicit_source_list(scan_root, files, n_files, list_file)
        call fo_fmt_list(config_file, list_file, exitcode)
        call delete_tmpfile(list_file)
    end subroutine fo_fmt_files

    subroutine fo_fmt_changed_run(dir, exitcode)
        character(len=*), intent(in) :: dir
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=512) :: scan_root, list_file, config_file
        integer :: n_git
        logical :: git_ok

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        call find_fprettify_config(scan_root, config_file)

        call make_tmpfile('fo_fmt_files', list_file)
        call write_git_changed_source_list(scan_root, list_file, git_ok, n_git)
        if (.not. git_ok) then
            exitcode = 1
            call delete_tmpfile(list_file)
            return
        end if

        call fo_fmt_list(config_file, list_file, exitcode)
        call delete_tmpfile(list_file)
    end subroutine fo_fmt_changed_run

    subroutine fo_fmt_list(config_file, list_file, exitcode)
        character(len=*), intent(in) :: config_file, list_file
        integer, intent(out) :: exitcode

        character(len=512) :: fpath
        integer :: u, ios, fmt_exit

        exitcode = 0
        open (newunit=u, file=trim(list_file), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) fpath
            if (ios /= 0) exit
            if (len_trim(fpath) == 0) cycle
            call format_project_file(config_file, trim(fpath), fmt_exit)
            if (fmt_exit /= 0) exitcode = 1
        end do
        close (u)
    end subroutine fo_fmt_list

    subroutine fo_fmt_check_run(dir, output, exitcode)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: output
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=512) :: scan_root, list_file, config_file

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        call find_fprettify_config(scan_root, config_file)

        call make_tmpfile('fo_fmt_files', list_file)
        call write_source_list(scan_root, list_file)
        call fo_fmt_check_list(config_file, list_file, output, exitcode)
        call delete_tmpfile(list_file)
    end subroutine fo_fmt_check_run

    subroutine fo_fmt_check_files(project_dir, files, n_files, output, exitcode)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: files(:)
        integer, intent(in) :: n_files
        character(len=*), intent(out) :: output
        integer, intent(out) :: exitcode

        character(len=512) :: list_file, config_file

        call find_fprettify_config(project_dir, config_file)
        call make_tmpfile('fo_fmt_files', list_file)
        call write_explicit_source_list(project_dir, files, n_files, list_file)
        call fo_fmt_check_list(config_file, list_file, output, exitcode)
        call delete_tmpfile(list_file)
    end subroutine fo_fmt_check_files

    subroutine fo_fmt_check_changed_run(project_dir, fallback_files, n_files, &
            output, exitcode)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: fallback_files(:)
        integer, intent(in) :: n_files
        character(len=*), intent(out) :: output
        integer, intent(out) :: exitcode

        character(len=512) :: list_file, config_file
        integer :: n_git
        logical :: git_ok

        call find_fprettify_config(project_dir, config_file)
        call make_tmpfile('fo_fmt_files', list_file)
        call write_git_changed_source_list(project_dir, list_file, git_ok, n_git)
        if (.not. git_ok) then
            call write_explicit_source_list(project_dir, fallback_files, n_files, &
                list_file)
        end if

        call fo_fmt_check_list(config_file, list_file, output, exitcode)
        call delete_tmpfile(list_file)
    end subroutine fo_fmt_check_changed_run

    subroutine write_explicit_source_list(project_dir, files, n_files, list_file)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: files(:)
        integer, intent(in) :: n_files
        character(len=*), intent(in) :: list_file

        character(len=512) :: fpath
        integer :: u, ios, i, n_selected

        open (newunit=u, file=trim(list_file), status='replace', iostat=ios)
        if (ios /= 0) return

        n_selected = min(n_files, size(files))
        do i = 1, n_selected
            fpath = trim(files(i))
            call write_source_path(project_dir, fpath, u)
        end do
        close (u)
    end subroutine write_explicit_source_list

    subroutine write_git_changed_source_list(project_dir, list_file, git_ok, &
            n_written)
        character(len=*), intent(in) :: project_dir, list_file
        logical, intent(out) :: git_ok
        integer, intent(out) :: n_written

        character(len=:), allocatable :: packed
        character(len=512) :: raw_file, line
        integer :: n_args, exitcode, raw_u, out_u, ios

        git_ok = .false.
        n_written = 0
        call make_tmpfile('fo_git_changed', raw_file)

        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'git')
        call argv_push(packed, n_args, '-C')
        call argv_push(packed, n_args, trim(project_dir))
        call argv_push(packed, n_args, 'diff')
        call argv_push(packed, n_args, '--name-only')
        call argv_push(packed, n_args, '--')
        call argv_push(packed, n_args, '*.f90')
        call argv_push(packed, n_args, '*.F90')
        call process_run_argv_logged('', packed, n_args, trim(raw_file), &
            .false., 0, exitcode)
        if (exitcode /= 0) then
            call delete_tmpfile(raw_file)
            return
        end if

        call append_git_changed(project_dir, raw_file, '--cached')
        call append_git_others(project_dir, raw_file)

        open (newunit=out_u, file=trim(list_file), status='replace', iostat=ios)
        if (ios /= 0) then
            call delete_tmpfile(raw_file)
            return
        end if

        open (newunit=raw_u, file=trim(raw_file), status='old', iostat=ios)
        if (ios == 0) then
            do
                read (raw_u, '(a)', iostat=ios) line
                if (ios /= 0) exit
                call write_source_path(project_dir, line, out_u, n_written)
            end do
            close (raw_u)
        end if
        close (out_u)

        git_ok = .true.
        call delete_tmpfile(raw_file)
    end subroutine write_git_changed_source_list

    subroutine append_git_changed(project_dir, raw_file, diff_arg)
        character(len=*), intent(in) :: project_dir, raw_file, diff_arg

        character(len=:), allocatable :: packed
        integer :: n_args, exitcode

        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'git')
        call argv_push(packed, n_args, '-C')
        call argv_push(packed, n_args, trim(project_dir))
        call argv_push(packed, n_args, 'diff')
        call argv_push(packed, n_args, '--name-only')
        call argv_push(packed, n_args, trim(diff_arg))
        call argv_push(packed, n_args, '--')
        call argv_push(packed, n_args, '*.f90')
        call argv_push(packed, n_args, '*.F90')
        call process_run_argv_logged('', packed, n_args, trim(raw_file), &
            .true., 0, exitcode)
    end subroutine append_git_changed

    subroutine append_git_others(project_dir, raw_file)
        character(len=*), intent(in) :: project_dir, raw_file

        character(len=:), allocatable :: packed
        integer :: n_args, exitcode

        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'git')
        call argv_push(packed, n_args, '-C')
        call argv_push(packed, n_args, trim(project_dir))
        call argv_push(packed, n_args, 'ls-files')
        call argv_push(packed, n_args, '--others')
        call argv_push(packed, n_args, '--exclude-standard')
        call argv_push(packed, n_args, '--')
        call argv_push(packed, n_args, '*.f90')
        call argv_push(packed, n_args, '*.F90')
        call process_run_argv_logged('', packed, n_args, trim(raw_file), &
            .true., 0, exitcode)
    end subroutine append_git_others

    subroutine write_source_path(project_dir, path, u, n_written)
        character(len=*), intent(in) :: project_dir, path
        integer, intent(in) :: u
        integer, optional, intent(inout) :: n_written

        character(len=512) :: fpath
        logical :: exists

        fpath = trim(path)
        if (len_trim(fpath) == 0) return
        if (.not. is_fortran_source(fpath)) return
        if (fpath(1:1) /= '/') fpath = trim(project_dir)//'/'//trim(fpath)
        if (skip_generated_source(project_dir, fpath)) return
        inquire (file=trim(fpath), exist=exists)
        if (.not. exists) return

        write (u, '(a)') trim(fpath)
        if (present(n_written)) n_written = n_written + 1
    end subroutine write_source_path

    subroutine fo_fmt_check_list(config_file, list_file, output, exitcode)
        character(len=*), intent(in) :: config_file, list_file
        character(len=*), intent(out) :: output
        integer, intent(out) :: exitcode

        character(len=512) :: fpath, tmpf
        character(len=HASH_LEN) :: action_id, orig_hash, fmt_hash
        integer :: u, ios, n_bad, fmt_exit, ho, hf

        exitcode = 0
        output = ''

        n_bad = 0

        call fmt_action_id(list_file, config_file, action_id)
        if (len_trim(action_id) == HASH_LEN) then
            if (fmt_marker_exists(action_id)) return
        end if

        open (newunit=u, file=trim(list_file), status='old', iostat=ios)
        if (ios /= 0) return

        do
            read (u, '(a)', iostat=ios) fpath
            if (ios /= 0) exit
            if (len_trim(fpath) == 0) cycle

            call make_tmpfile('fo_fmt_check', tmpf)
            call fs_append_file(trim(fpath), trim(tmpf))
            call format_project_file(config_file, trim(tmpf), fmt_exit)
            if (fmt_exit /= 0) then
                call delete_tmpfile(tmpf)
                n_bad = n_bad + 1
                output = trim(output)//trim(fpath)//': formatting failed'//achar(10)
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

        if (n_bad > 0) then
            exitcode = 1
        else if (len_trim(action_id) == HASH_LEN) then
            call store_fmt_marker(action_id)
        end if
    end subroutine fo_fmt_check_list

    logical function is_fortran_source(path)
        character(len=*), intent(in) :: path

        integer :: n

        is_fortran_source = .false.
        n = len_trim(path)
        if (n < 4) return
        if (path(n - 3:n) == '.f90') is_fortran_source = .true.
        if (path(n - 3:n) == '.F90') is_fortran_source = .true.
    end function is_fortran_source

    subroutine find_fprettify_config(project_dir, config_file)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(out) :: config_file

        logical :: exists

        config_file = ''
        inquire (file=trim(project_dir)//'/.fprettify', exist=exists)
        if (exists) then
            config_file = trim(project_dir)//'/.fprettify'
            return
        end if

        inquire (file=trim(project_dir)//'/.fprettify.rc', exist=exists)
        if (exists) config_file = trim(project_dir)//'/.fprettify.rc'
    end subroutine find_fprettify_config

    subroutine format_project_file(config_file, filepath, exitcode)
        character(len=*), intent(in) :: config_file, filepath
        integer, intent(out) :: exitcode

        if (len_trim(config_file) == 0) then
            call format_file(filepath, exitcode)
        else
            call fprettify_file(config_file, filepath, exitcode)
        end if
    end subroutine format_project_file

    subroutine fprettify_file(config_file, filepath, exitcode)
        character(len=*), intent(in) :: config_file, filepath
        integer, intent(out) :: exitcode

        character(len=:), allocatable :: packed
        character(len=512) :: log_file
        integer :: n_args

        packed = ''
        n_args = 0
        call argv_push(packed, n_args, 'fprettify')
        call argv_push(packed, n_args, '-c')
        call argv_push(packed, n_args, trim(config_file))
        call argv_push(packed, n_args, trim(filepath))

        call make_tmpfile('fo_fprettify', log_file)
        call process_run_argv_logged('', packed, n_args, trim(log_file), &
            .false., 0, exitcode)
        call delete_tmpfile(log_file)
    end subroutine fprettify_file

    subroutine fmt_action_id(list_file, config_file, action_id)
        character(len=*), intent(in) :: list_file
        character(len=*), intent(in) :: config_file
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
        if (len_trim(config_file) == 0) then
            write (out_u, '(a)') 'fo-fmt-native-v1'
        else
            write (out_u, '(a)') 'fo-fmt-fprettify-v1'
            call sha256_file(trim(config_file), file_hash, ierr)
            if (ierr == 0) write (out_u, '(a,1x,a)') &
                trim(file_hash), trim(config_file)
        end if

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
