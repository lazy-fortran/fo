module fo_fmt
    use fo_util, only: make_tmpfile, delete_tmpfile, strip_toml_comment, &
        read_text_file_alloc
    use fo_fs, only: fs_make_dir, fs_write_text, fs_append_file, fs_collect_files
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
    use fo_cache, only: cache_store_root, HASH_LEN
    use fx_hash, only: sha256_file
    use fo_format, only: format_file_with_indent
    use fo_lint, only: skip_generated_source
    use fo_process, only: process_run_argv_logged, argv_push
    use fx_string, only: to_lower
    use fx_path, only: path_dirname
    implicit none
    private
    public :: fmt_sources, FMT_SELECT_ALL, FMT_SELECT_FILES, FMT_SELECT_CHANGED
    public :: write_git_changed_source_list, fmt_check_lines, fmt_diff_lines

    integer, parameter :: FMT_SELECT_ALL = 1, FMT_SELECT_FILES = 2, FMT_SELECT_CHANGED = 3

contains

    subroutine write_source_list(scan_root, list_file)
        !! Write the sorted list of lowercase and uppercase F90 sources under scan_root.
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
                if (skip_generated_source(scan_root, hits(i), .false.)) cycle
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

    subroutine fmt_sources(dir, selection, check, deep, files, n_files, output, exitcode)
        !! Format or check Fortran sources. `selection` picks all project sources,
        !! the listed `files`, or the dirty changed sources. With `check`, nothing
        !! is modified and `output` reports differences; for FMT_SELECT_CHANGED the listed
        !! `files` are the fallback when there is no worktree. `deep` uses fluff.
        character(len=*), intent(in) :: dir
        integer, intent(in) :: selection
        logical, intent(in) :: check, deep
        character(len=*), intent(in) :: files(:)
        integer, intent(in) :: n_files
        character(len=*), intent(out) :: output
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=512) :: scan_root, list_file, config_file
        integer :: n_changed
        logical :: changed_ok

        output = ''
        exitcode = 0
        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        config_file = ''
        if (.not. deep) call find_fprettify_config(scan_root, config_file)

        call make_tmpfile('fo_fmt_files', list_file)
        select case (selection)
        case (FMT_SELECT_FILES)
            call write_explicit_source_list(scan_root, files, n_files, list_file)
        case (FMT_SELECT_CHANGED)
            call write_git_changed_source_list(scan_root, list_file, changed_ok, &
                n_changed)
            if (.not. changed_ok) then
                if (.not. check) then
                    exitcode = 1
                    call delete_tmpfile(list_file)
                    return
                end if
                call write_explicit_source_list(scan_root, files, n_files, list_file)
            end if
        case default
            call write_source_list(scan_root, list_file)
        end select

        if (check .and. deep) then
            call fo_fmt_deep_check_list(list_file, output, exitcode)
        else if (check) then
            call fo_fmt_check_list(config_file, list_file, output, exitcode)
        else if (deep) then
            call fo_fmt_deep_list(list_file, exitcode)
        else
            call fo_fmt_list(config_file, list_file, exitcode)
        end if
        call delete_tmpfile(list_file)
    end subroutine fmt_sources

    subroutine fmt_check_lines(dir, path, changed, output, exitcode)
        !! Check only the lines of dir/path flagged in `changed`, so formatting
        !! the formatter would apply elsewhere in a legacy file never blocks.
        !! Appends a report to `output` and sets `exitcode` to 1 on a violation.
        character(len=*), intent(in) :: dir, path
        logical, intent(in) :: changed(:)
        character(len=*), intent(inout) :: output
        integer, intent(inout) :: exitcode

        type(backend_t) :: b
        character(len=512) :: scan_root, config_file, fpath, tmpf, difflog
        character(len=:), allocatable :: packed, text, line
        character(len=16) :: line_text
        integer :: n_args, status, ierr, first, last, k, pos, eol
        logical :: reporting

        fpath = trim(path)
        if (.not. is_fortran_source(fpath)) return
        if (fpath(1:1) /= '/') fpath = trim(dir)//'/'//trim(fpath)
        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        if (skip_generated_source(scan_root, fpath, .false.)) return
        call find_fprettify_config(scan_root, config_file)

        call make_tmpfile('fo_fmt_check', tmpf)
        call fs_append_file(trim(fpath), trim(tmpf))
        call format_project_file(config_file, trim(tmpf), status)
        if (status /= 0) then
            call delete_tmpfile(tmpf)
            output = trim(output)//trim(path)//': formatting failed'//achar(10)
            exitcode = 1
            return
        end if
        call make_tmpfile('fo_fmt_diff', difflog)
        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'git')
        call argv_push(packed, n_args, 'diff')
        call argv_push(packed, n_args, '--no-index')
        call argv_push(packed, n_args, '--no-color')
        call argv_push(packed, n_args, '--no-ext-diff')
        call argv_push(packed, n_args, '--no-textconv')
        call argv_push(packed, n_args, '-U0')
        call argv_push(packed, n_args, '--')
        call argv_push(packed, n_args, trim(fpath))
        call argv_push(packed, n_args, trim(tmpf))
        call process_run_argv_logged('', packed, n_args, trim(difflog), &
            .false., 0, status)
        call delete_tmpfile(tmpf)
        call read_text_file_alloc(trim(difflog), text, ierr)
        call delete_tmpfile(difflog)
        if (status == 0) return
        if (status /= 1 .or. ierr /= 0) then
            output = trim(output)//trim(path)//': formatting comparison failed'//achar(10)
            exitcode = 1
            return
        end if

        ! Report the first formatter hunk that rewrites a changed line.
        reporting = .false.
        pos = 1
        do while (pos <= len(text))
            eol = index(text(pos:), achar(10))
            if (eol == 0) then
                line = text(pos:)
                pos = len(text) + 1
            else
                line = text(pos:pos + eol - 2)
                pos = pos + eol
            end if
            if (index(line, '@@ ') == 1) then
                if (reporting) exit
                call fmt_hunk_range(line, .true., first, last)
                do k = max(first, 1), min(last, size(changed))
                    if (.not. changed(k)) cycle
                    write (line_text, '(i0)') k
                    output = trim(output)//trim(path)//':'//trim(line_text)// &
                        ': needs formatting'//achar(10)
                    exitcode = 1
                    reporting = .true.
                    exit
                end do
            else if (reporting .and. len(line) > 0) then
                if (line(1:1) == '-' .or. line(1:1) == '+') &
                    output = trim(output)//'  '//line//achar(10)
            end if
        end do
    end subroutine fmt_check_lines

    subroutine fmt_diff_lines(text, old_side, lines)
        !! Lines of one side of a zero-context unified diff that the diff touches.
        character(len=*), intent(in) :: text
        logical, intent(in) :: old_side
        logical, allocatable, intent(out) :: lines(:)

        integer :: pass, pos, eol, first, last, top

        top = 0
        do pass = 1, 2
            if (pass == 2) then
                allocate (lines(top))
                lines = .false.
            end if
            pos = 1
            do while (pos <= len(text))
                eol = index(text(pos:), achar(10))
                if (eol == 0) eol = len(text) - pos + 2
                if (index(text(pos:pos + eol - 2), '@@ ') == 1) then
                    call fmt_hunk_range(text(pos:pos + eol - 2), old_side, first, last)
                    if (pass == 1) top = max(top, last)
                    if (pass == 2 .and. last >= first) lines(max(first, 1):last) = .true.
                end if
                pos = pos + eol
            end do
        end do
    end subroutine fmt_diff_lines

    subroutine fmt_hunk_range(header, old_side, first, last)
        !! Line range of one side of a "@@ -a,b +c,d @@" header. A pure
        !! insertion on the old side touches the lines around its position.
        character(len=*), intent(in) :: header
        logical, intent(in) :: old_side
        integer, intent(out) :: first, last

        integer :: start, stop_at, comma, count, ios
        character(len=1) :: mark

        first = 1
        last = 0
        mark = '+'
        if (old_side) mark = '-'
        start = index(header, ' '//mark)
        if (start == 0) return
        start = start + 2
        stop_at = index(header(start:), ' ')
        if (stop_at == 0) return
        stop_at = start + stop_at - 2
        comma = index(header(start:stop_at), ',')
        count = 1
        if (comma > 0) then
            read (header(start:start + comma - 2), *, iostat=ios) first
            if (ios /= 0) return
            read (header(start + comma:stop_at), *, iostat=ios) count
            if (ios /= 0) return
        else
            read (header(start:stop_at), *, iostat=ios) first
            if (ios /= 0) return
        end if
        if (count > 0) then
            last = first + count - 1
        else if (old_side) then
            last = first + 1
        else
            last = first - 1
        end if
    end subroutine fmt_hunk_range

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
        if (skip_generated_source(project_dir, fpath, .false.)) return
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

            if (ho /= 0 .or. hf /= 0 .or. orig_hash /= fmt_hash) then
                n_bad = n_bad + 1
                output = trim(output)//first_difference(trim(fpath), trim(tmpf))
            end if
            call delete_tmpfile(tmpf)
        end do
        close (u)

        if (n_bad > 0) then
            exitcode = 1
        else if (len_trim(action_id) == HASH_LEN) then
            call store_fmt_marker(action_id)
        end if
    end subroutine fo_fmt_check_list

    function first_difference(path, formatted) result(report)
        !! "path:LINE: needs formatting" and the first differing line before
        !! (-) and after (+) formatting, one entry per line of the report.
        character(len=*), intent(in) :: path, formatted
        character(len=:), allocatable :: report
        character(len=4096) :: old_line, new_line
        character(len=16) :: line_text
        integer :: uo, uf, ios_o, ios_f, line_no

        report = path//': needs formatting'//achar(10)
        open (newunit=uo, file=path, status='old', action='read', iostat=ios_o)
        if (ios_o /= 0) return
        open (newunit=uf, file=formatted, status='old', action='read', iostat=ios_f)
        if (ios_f /= 0) then
            close (uo)
            return
        end if
        line_no = 0
        do
            line_no = line_no + 1
            old_line = ''
            new_line = ''
            read (uo, '(a)', iostat=ios_o) old_line
            read (uf, '(a)', iostat=ios_f) new_line
            if (ios_o /= 0 .and. ios_f /= 0) exit
            if (ios_o /= ios_f .or. old_line /= new_line) then
                write (line_text, '(i0)') line_no
                report = path//':'//trim(line_text)//': needs formatting'// &
                    achar(10)//'  -'//trim(old_line)//achar(10)//'  +'// &
                    trim(new_line)//achar(10)
                exit
            end if
        end do
        close (uo)
        close (uf)
    end function first_difference

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
        !! A project that declares an fprettify configuration is formatted by
        !! fprettify itself: the native formatter differs from it (select-type
        !! guards, continuation alignment, OpenMP sentinels), and such projects
        !! check their sources with fprettify.
        character(len=*), intent(in) :: config_file, filepath
        integer, intent(out) :: exitcode

        integer :: indent_width

        if (use_fprettify(config_file)) then
            call run_fprettify(config_file, filepath, exitcode)
            ! fprettify rejects options of newer releases without touching the
            ! file; such configurations keep the native formatter.
            if (exitcode == 0) return
        end if
        call fprettify_indent_width(config_file, indent_width)
        call format_file_with_indent(filepath, exitcode, indent_width)
    end subroutine format_project_file

    logical function use_fprettify(config_file)
        character(len=*), intent(in) :: config_file

        logical, save :: probed = .false., available = .false.
        character(len=:), allocatable :: packed
        integer :: n_args, probe_exit

        use_fprettify = .false.
        if (len_trim(config_file) == 0) return
        if (.not. probed) then
            n_args = 0
            packed = ''
            call argv_push(packed, n_args, 'fprettify')
            call argv_push(packed, n_args, '--version')
            call process_run_argv_logged('', packed, n_args, '/dev/null', &
                .false., 60, probe_exit)
            available = probe_exit == 0
            probed = .true.
        end if
        use_fprettify = available
    end function use_fprettify

    subroutine run_fprettify(config_file, filepath, exitcode)
        character(len=*), intent(in) :: config_file, filepath
        integer, intent(out) :: exitcode

        character(len=:), allocatable :: packed
        integer :: n_args

        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'fprettify')
        call argv_push(packed, n_args, '-c')
        call argv_push(packed, n_args, trim(config_file))
        call argv_push(packed, n_args, trim(filepath))
        call process_run_argv_logged('', packed, n_args, '/dev/null', &
            .false., 120, exitcode)
    end subroutine run_fprettify

    subroutine fprettify_indent_width(config_file, indent_width)
        character(len=*), intent(in) :: config_file
        integer, intent(out) :: indent_width

        character(len=512) :: line, key, value
        integer :: u, ios, eq, parsed

        indent_width = 4
        if (len_trim(config_file) == 0) return

        open (newunit=u, file=trim(config_file), status='old', iostat=ios)
        if (ios /= 0) return

        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            call strip_toml_comment(line)
            eq = index(line, '=')
            if (eq <= 0) cycle
            key = adjustl(line(1:eq - 1))
            value = adjustl(line(eq + 1:))
            key = to_lower(key)
            if (trim(key) == 'indent') then
                read (value, *, iostat=ios) parsed
                if (ios == 0) then
                    if (parsed > 0) indent_width = parsed
                end if
            end if
        end do
        close (u)
    end subroutine fprettify_indent_width

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
        if (use_fprettify(config_file)) then
            write (out_u, '(a)') 'fo-fmt-fprettify-v1'
        else
            write (out_u, '(a)') 'fo-fmt-native-v2'
        end if
        if (len_trim(config_file) > 0) then
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
        call fs_make_dir(path_dirname(path))
        call fs_write_text(trim(path), '1')
    end subroutine store_fmt_marker

    subroutine fmt_marker_path(action_id, path)
        character(len=*), intent(in) :: action_id
        character(len=*), intent(out) :: path

        character(len=512) :: root

        call cache_store_root(root)
        path = trim(root)//'/'//action_id(1:2)//'/'//trim(action_id)//'-fmt'
    end subroutine fmt_marker_path

    subroutine fo_fmt_deep_list(list_file, exitcode)
        character(len=*), intent(in) :: list_file
        integer, intent(out) :: exitcode

        character(len=512) :: fpath
        character(len=:), allocatable :: packed
        integer :: u, ios, fmt_exit, n_args

        exitcode = 0

        open (newunit=u, file=trim(list_file), status='old', iostat=ios)
        if (ios /= 0) return

        do
            read (u, '(a)', iostat=ios) fpath
            if (ios /= 0) exit
            if (len_trim(fpath) == 0) cycle

            n_args = 0
            packed = ''
            call argv_push(packed, n_args, 'fluff')
            call argv_push(packed, n_args, 'format')
            call argv_push(packed, n_args, trim(fpath))
            call process_run_argv_logged('', packed, n_args, '/dev/null', &
                .false., 120, fmt_exit)

            if (fmt_exit /= 0) exitcode = 1
        end do
        close (u)
    end subroutine fo_fmt_deep_list

    subroutine fo_fmt_deep_check_list(list_file, output, exitcode)
        character(len=*), intent(in) :: list_file
        character(len=*), intent(out) :: output
        integer, intent(out) :: exitcode

        character(len=512) :: fpath, errfile
        character(len=:), allocatable :: packed
        integer :: u, ios, fmt_exit, n_args, n_bad

        exitcode = 0
        output = ''
        n_bad = 0

        open (newunit=u, file=trim(list_file), status='old', iostat=ios)
        if (ios /= 0) return

        do
            read (u, '(a)', iostat=ios) fpath
            if (ios /= 0) exit
            if (len_trim(fpath) == 0) cycle

            call make_tmpfile('fo_fmt_deep_check', errfile)
            n_args = 0
            packed = ''
            call argv_push(packed, n_args, 'fluff')
            call argv_push(packed, n_args, 'check')
            call argv_push(packed, n_args, trim(fpath))
            call process_run_argv_logged('', packed, n_args, trim(errfile), &
                .false., 120, fmt_exit)

            if (fmt_exit /= 0) then
                n_bad = n_bad + 1
                output = trim(output)//trim(fpath)//': needs formatting'//achar(10)
            end if
            call delete_tmpfile(errfile)
        end do
        close (u)

        if (n_bad > 0) exitcode = 1
    end subroutine fo_fmt_deep_check_list

end module fo_fmt
