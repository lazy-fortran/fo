program test_process_setenv_default
    !! The team's concurrency hint must be a default and never an override: a
    !! width the user exported is a request, and a driver that overwrites it
    !! silently changes the run the user asked for. The child-side walker reads
    !! the same variable, so the test observes the value through a real child
    !! process rather than through the driver's own memory.
    use fo_util, only: temporary_root
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_process, only: process_setenv_default
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call check_unset_gets_default(n_pass, n_fail)
    call check_explicit_value_survives(n_pass, n_fail)
    call check_hint_reaches_a_child(n_pass, n_fail)

    if (n_fail == 0) then
        write (output_unit, '(A,I0,A)') 'PASS: ', n_pass, &
            ' setenv-default checks (default fills, request wins, child sees it)'
        stop
    end if
    write (error_unit, '(A,I0,A,I0,A)') 'FAIL: ', n_pass, ' passed, ', n_fail, &
        ' failed'
    error stop 1
contains

    subroutine child_reads(name, out, rc)
        !! Read the variable the way a shard child does: through a new process,
        !! so the assertion is about the environment block, not this image.
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: out
        integer, intent(out) :: rc

        character(len=512) :: line
        character(len=64) :: tmp
        integer :: unit, ios

        call temp_name('fo_setenv_probe', tmp)
        call execute_command_line('printf %s "$'//trim(name)//'" > '//trim(tmp)// &
            ' 2>/dev/null', exitstat=rc)
        if (rc /= 0) then
            out = ''
            return
        end if
        open (newunit=unit, file=trim(tmp), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            out = ''
            return
        end if
        read (unit, '(A)', iostat=ios) line
        close (unit)
        if (ios /= 0) line = ''
        out = trim(line)
        call execute_command_line('rm -f '//trim(tmp))
    end subroutine child_reads

    subroutine temp_name(prefix, out)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: out

        character(len=64) :: pid
        integer :: status, unit

        ! The harness exports no PID, so ask a child for one: the probe file
        ! must not collide with another concurrent run's probe file.
        call execute_command_line('echo $$ > '//temporary_root()//'/'// &
            trim(prefix)//'.pid 2>/dev/null')
        open (newunit=unit, file=temporary_root()//'/'//trim(prefix)//'.pid', &
            status='old', &
            action='read', iostat=status)
        if (status == 0) then
            read (unit, '(A)', iostat=status) pid
            close (unit)
        end if
        if (status /= 0) pid = '0'
        out = temporary_root()//'/'//trim(prefix)//'_'//trim(adjustl(pid))//'.txt'
    end subroutine temp_name

    subroutine check_unset_gets_default(pass, fail)
        integer, intent(inout) :: pass, fail

        character(len=:), allocatable :: seen
        integer :: rc

        call execute_command_line('unset FOCJ_DEFAULT_PROBE 2>/dev/null; true')
        call process_setenv_default('FOCJ_DEFAULT_PROBE', '1')
        call child_reads('FOCJ_DEFAULT_PROBE', seen, rc)
        ! `unset` in a child cannot change this process's environment, so clear
        ! the hook's own memory by re-pointing the name at a fresh one: the
        ! assertion that matters is that a name nobody set becomes "1".
        if (seen == '1') then
            pass = pass + 1
        else
            fail = fail + 1
            write (error_unit, '(A,A,A)') 'FAIL: unset name did not default to 1, got: ', &
                seen, ''
        end if
    end subroutine check_unset_gets_default

    subroutine check_explicit_value_survives(pass, fail)
        integer, intent(inout) :: pass, fail

        character(len=:), allocatable :: seen
        integer :: rc

        ! Export through a child shell cannot reach this process either, so the
        ! driver's own first write establishes the "explicit" value; the second
        ! call is the one that must not overwrite it.
        call process_setenv_default('FOCJ_REQUEST_PROBE', '8')
        call process_setenv_default('FOCJ_REQUEST_PROBE', '1')
        call child_reads('FOCJ_REQUEST_PROBE', seen, rc)
        if (seen == '8') then
            pass = pass + 1
        else
            fail = fail + 1
            write (error_unit, '(A,A,A)') 'FAIL: default overwrote an explicit value, got: ', &
                seen, ''
        end if
    end subroutine check_explicit_value_survives

    subroutine check_hint_reaches_a_child(pass, fail)
        integer, intent(inout) :: pass, fail

        character(len=:), allocatable :: seen
        integer :: rc

        ! A private name: fo test itself sets FFC_CONFORMANCE_JOBS for its
        ! sharded children, so the real hint is already present here.
        call process_setenv_default('FO_SHARD_HINT_PROBE', '1')
        call child_reads('FO_SHARD_HINT_PROBE', seen, rc)
        if (seen == '1') then
            pass = pass + 1
        else
            fail = fail + 1
            write (error_unit, '(A,A,A)') 'FAIL: shard hint did not reach a child, got: ', &
                seen, ''
        end if
    end subroutine check_hint_reaches_a_child
end program test_process_setenv_default
