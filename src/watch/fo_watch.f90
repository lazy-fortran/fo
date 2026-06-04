module fo_watch
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    implicit none
    private
    public :: watch_loop

contains

    subroutine watch_loop(dir)
        character(len=*), intent(in) :: dir

        character(len=2048) :: cmd
        character(len=512) :: line
        integer :: u, iostat, exitcode, cmdstat
        character(len=128) :: pipe_path

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

        write (output_unit, '(a)') 'fo: watching for Fortran file changes...'

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

            write (output_unit, '(a,a)') 'change: ', trim(line)

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

    subroutine make_pipe_path(path)
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,i0,a,i0)') '/tmp/fo_watch_fifo-', count, '-', serial
    end subroutine make_pipe_path

end module fo_watch
