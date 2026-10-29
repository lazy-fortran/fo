program test_nproc_limits
    !! The detected CPU count follows the CPUs a process may use: its affinity
    !! mask and, in containers, its cgroup CPU quota.
    use fo_process, only: process_detect_nproc
    use fo_util, only: make_tmpfile
    implicit none

    character(len=4096) :: self, line, out
    character(len=16) :: mode
    integer :: status, unit, ios, n

    call get_environment_variable('FO_NPROC_CHILD', mode, status=status)
    if (status == 0) then
        print '(i0)', process_detect_nproc()
        stop
    end if
    call get_command_argument(0, self)
    call execute_command_line('command -v taskset >/dev/null', exitstat=status)
    if (status /= 0) then
        print *, 'SKIP: taskset not available'
        stop
    end if
    call make_tmpfile('nproc_limits', out)
    ! Pin to the first CPU this process may use: CPU 0 can lie outside the
    ! cpuset of a batch job, and taskset rejects it there.
    call execute_command_line('cpu=$(taskset -pc $$ | sed ''s/.*: *//; '// &
        's/[-,].*//'') && FO_NPROC_CHILD=1 taskset -c "$cpu" '//trim(self)// &
        ' > '//trim(out), exitstat=status)
    if (status /= 0) error stop 'child failed'
    open (newunit=unit, file=trim(out), status='old', action='read')
    read (unit, '(a)', iostat=ios) line
    close (unit, status='delete')
    read (line, *, iostat=ios) n
    if (ios /= 0) error stop 'unreadable child output'
    if (n /= 1) then
        print *, 'FAIL: detected', n, 'CPUs with an affinity mask of one CPU'
        error stop 1
    end if
    print *, 'PASS: affinity mask limits the detected CPUs'
end program test_nproc_limits
