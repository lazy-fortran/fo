module fo_fmt
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
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
        character(len=4096) :: cmd
        integer :: u, iostat, diff_exit, n_bad

        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir

        exitcode = 0
        n_bad = 0
        output = ''

        call make_tmpfile('fo_fmt_files', list_file)
        cmd = 'find '//trim(scan_root)// &
              " -path '*/build' -prune -o"// &
              " -path '*/.git' -prune -o"// &
              " \( -name '*.f90' -o -name '*.F90' \) -print 2>/dev/null"// &
              ' | sort > '//trim(list_file)
        call execute_command_line(trim(cmd), wait=.true.)

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

        if (n_bad > 0) exitcode = 1
    end subroutine fo_fmt_check_run

end module fo_fmt
