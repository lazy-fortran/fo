module fo_fmt
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
    use fo_cache, only: cache_store_root, HASH_LEN
    use fx_hash, only: sha256_file
    implicit none
    private
    public :: fo_fmt_run, fo_fmt_check_run

    character(len=*), parameter :: FPRETTIFY_FLAGS = &
                                   ' -i 4 -l 88 --strict-indent'

contains

    subroutine fo_fmt_run(dir, exitcode)
        character(len=*), intent(in) :: dir
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=512) :: scan_root
        character(len=4096) :: cmd

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        cmd = 'find '//trim(scan_root)// &
              " -path '*/build' -prune -o"// &
              " -path '*/.git' -prune -o"// &
              " \( -name '*.f90' -o -name '*.F90' \) -print"// &
              ' | xargs -r fprettify'//FPRETTIFY_FLAGS
        call execute_command_line(trim(cmd), exitstat=exitcode, wait=.true.)
    end subroutine fo_fmt_run

    subroutine fo_fmt_check_run(dir, output, exitcode)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: output
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=512) :: scan_root, list_file, fpath, tmpf
        character(len=HASH_LEN) :: action_id
        character(len=4096) :: cmd
        integer :: u, iostat, diff_exit, n_bad

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        exitcode = 0
        output = ''

        n_bad = 0
        call make_tmpfile('fo_fmt_files', list_file)
        cmd = 'find '//trim(scan_root)// &
              " -path '*/build' -prune -o"// &
              " -path '*/.git' -prune -o"// &
              " \( -name '*.f90' -o -name '*.F90' \) -print 2>/dev/null"// &
              ' | sort > '//trim(list_file)
        call execute_command_line(trim(cmd), wait=.true.)

        call fmt_action_id(list_file, action_id)
        if (len_trim(action_id) == HASH_LEN .and. fmt_marker_exists(action_id)) then
            call delete_tmpfile(list_file)
            return
        end if

        open (newunit=u, file=list_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            call delete_tmpfile(list_file)
            return
        end if

        do
            read (u, '(a)', iostat=iostat) fpath
            if (iostat /= 0) exit
            if (len_trim(fpath) == 0) cycle

            call make_tmpfile('fo_fmt_check', tmpf)
            cmd = 'cp '//trim(fpath)//' '//trim(tmpf)
            call execute_command_line(trim(cmd), wait=.true.)
            cmd = 'fprettify'//FPRETTIFY_FLAGS//' '//trim(tmpf)//' >/dev/null 2>&1'
            call execute_command_line(trim(cmd), wait=.true.)
            cmd = 'diff -q '//trim(fpath)//' '//trim(tmpf)//' >/dev/null 2>&1'
            call execute_command_line(trim(cmd), exitstat=diff_exit, wait=.true.)
            call delete_tmpfile(tmpf)

            if (diff_exit /= 0) then
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

        character(len=512) :: manifest, fpath, version_file
        character(len=HASH_LEN) :: file_hash
        character(len=512) :: version
        integer :: in_u, out_u, ios, ierr

        action_id = ''
        call make_tmpfile('fo_fmt_manifest', manifest)
        call make_tmpfile('fo_fmt_version', version_file)
        call execute_command_line('fprettify --version > '//sq(trim(version_file))// &
                                  ' 2>/dev/null', wait=.true.)
        call read_first_line(version_file, version)
        call delete_tmpfile(version_file)

        open (newunit=out_u, file=trim(manifest), status='replace', iostat=ios)
        if (ios /= 0) then
            call delete_tmpfile(manifest)
            return
        end if
        write (out_u, '(a)') 'fo-fmt-check-v1'
        write (out_u, '(a)') trim(FPRETTIFY_FLAGS)
        write (out_u, '(a)') trim(version)

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

        character(len=512) :: path, tmp
        integer :: exitcode

        call fmt_marker_path(action_id, path)
        call make_tmpfile('fo_fmt_marker', tmp)
        call execute_command_line('mkdir -p '//sq(dirname(path))//' && printf 1 > '// &
                                  sq(trim(tmp))//' && mv -f '//sq(trim(tmp))//' '// &
                                  sq(trim(path)), wait=.true., exitstat=exitcode)
        if (exitcode /= 0) call delete_tmpfile(tmp)
    end subroutine store_fmt_marker

    subroutine fmt_marker_path(action_id, path)
        character(len=*), intent(in) :: action_id
        character(len=*), intent(out) :: path

        character(len=512) :: root

        call cache_store_root(root)
        path = trim(root)//'/'//action_id(1:2)//'/'//trim(action_id)//'-fmt'
    end subroutine fmt_marker_path

    subroutine read_first_line(path, line)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: line

        integer :: u, ios

        line = ''
        open (newunit=u, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        close (u)
    end subroutine read_first_line

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

    pure function sq(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len_trim(s) + 2) :: r
        r = "'"//trim(s)//"'"
    end function sq

end module fo_fmt
