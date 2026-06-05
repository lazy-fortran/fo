module fo_watch
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    implicit none
    private
    public :: watch_loop

contains

    subroutine watch_loop(dir, fmt_mode)
        character(len=*), intent(in) :: dir
        logical, intent(in), optional :: fmt_mode

        character(len=2048) :: cmd
        character(len=512) :: line, event_file, skip_path
        integer :: u, iostat, exitcode, cmdstat
        character(len=128) :: pipe_path
        logical :: do_fmt

        do_fmt = .false.
        if (present(fmt_mode)) do_fmt = fmt_mode
        skip_path = ''

        call make_pipe_path(pipe_path)

        ! clean up any stale fifo
        call execute_command_line('rm -f '//trim(pipe_path), wait=.true.)
        call execute_command_line('mkfifo '//trim(pipe_path), &
                                  exitstat=exitcode, wait=.true.)
        if (exitcode /= 0) then
            write (error_unit, '(a)') 'fo: cannot create fifo for watch'
            return
        end if

        ! launch inotifywait in background, writing events to fifo
        cmd = 'inotifywait -m -r -e modify,create,delete'// &
              " --include '.*\.(f90|F90|f|F)$' "// &
              trim(dir)//' > '//trim(pipe_path)//' 2>/dev/null &'
        call execute_command_line(cmd, wait=.false.)

        if (do_fmt) then
            write (output_unit, '(a)') &
                'fo: watching for Fortran file changes (--fmt enabled)...'
        else
            write (output_unit, '(a)') 'fo: watching for Fortran file changes...'
        end if

        open (newunit=u, file=pipe_path, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, '(a)') 'fo: cannot open watch fifo'
            call execute_command_line('rm -f '//trim(pipe_path), wait=.true.)
            return
        end if

        ! event loop: each line from inotifywait triggers a rebuild
        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle

            call parse_inotify_path(line, event_file)

            ! skip events caused by our own fmt write
            if (len_trim(skip_path) > 0 .and. &
                trim(event_file) == trim(skip_path)) then
                skip_path = ''
                cycle
            end if
            skip_path = ''

            write (output_unit, '(a,a)') 'change: ', trim(line)

            ! format before build if requested and file path is known
            if (do_fmt .and. len_trim(event_file) > 0) then
                cmd = 'fprettify -i 4 -l 88 --strict-indent '// &
                      trim(event_file)//' 2>/dev/null'
                call execute_command_line(cmd, exitstat=exitcode, &
                                          cmdstat=cmdstat, wait=.true.)
                if (exitcode == 0) then
                    skip_path = trim(event_file)
                end if
            end if

            ! run fo check as a subprocess
            cmd = 'fo check 2>&1'
            call execute_command_line(cmd, exitstat=exitcode, &
                                      cmdstat=cmdstat, wait=.true.)
            if (exitcode == 0) then
                write (output_unit, '(a)') 'watch: OK'
            else
                write (output_unit, '(a)') 'watch: FAIL'
            end if
        end do

        close (u)
        ! clean up
        call execute_command_line('rm -f '//trim(pipe_path), wait=.true.)
        call execute_command_line("pkill -f 'inotifywait.*fo_watch'", &
                                  wait=.true.)
    end subroutine watch_loop

    subroutine parse_inotify_path(line, filepath)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: filepath

        ! inotifywait format: "/path/to/dir/ EVENT filename.f90"
        integer :: last_space, n

        filepath = ''
        n = len_trim(line)
        if (n == 0) return

        ! find last space to separate dir+event from filename
        last_space = 0
        block
            integer :: i
            do i = n, 1, -1
                if (line(i:i) == ' ') then
                    last_space = i
                    exit
                end if
            end do
        end block
        if (last_space == 0) return

        ! find first space to get the directory part
        block
            integer :: i, first_space, second_space
            first_space = 0
            second_space = 0
            do i = 1, n
                if (line(i:i) == ' ') then
                    if (first_space == 0) then
                        first_space = i
                    else
                        second_space = i
                        exit
                    end if
                end if
            end do
            if (first_space == 0) return
            ! dir is from 1 to first_space-1, filename is after last_space
            filepath = line(1:first_space - 1)//line(last_space + 1:n)
        end block
    end subroutine parse_inotify_path

    subroutine make_pipe_path(path)
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,i0,a,i0)') '/tmp/fo_watch_fifo-', count, '-', serial
    end subroutine make_pipe_path

end module fo_watch
