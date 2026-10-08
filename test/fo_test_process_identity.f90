module fo_test_process_identity
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t
    use fo_test_harness, only: assert_true
    implicit none
    private

    public :: mcp_process_start_time, mcp_process_identity_running, mcp_kill_owned_tree

    interface
        integer(c_int64_t) function c_process_start_time(pid) &
                bind(C, name='fo_test_process_start_time')
            import :: c_int, c_int64_t
            integer(c_int), value :: pid
        end function c_process_start_time
        integer(c_int) function c_signal_identity(pid, start_time, signal_number) &
                bind(C, name='fo_test_signal_identity')
            import :: c_int, c_int64_t
            integer(c_int), value :: pid, signal_number
            integer(c_int64_t), value :: start_time
        end function c_signal_identity
        integer(c_int) function c_collect_descendants(root_pid, root_start_time, &
                processes, start_times, capacity) bind(C, name='fo_test_collect_descendants')
            import :: c_int, c_int64_t
            integer(c_int), value :: root_pid, capacity
            integer(c_int64_t), value :: root_start_time
            integer(c_int), intent(out) :: processes(*)
            integer(c_int64_t), intent(out) :: start_times(*)
        end function c_collect_descendants
        integer(c_int) function c_process_running(pid) bind(C, name='fo_test_process_running')
            import :: c_int
            integer(c_int), value :: pid
        end function c_process_running
        integer(c_int) function c_usleep(microseconds) bind(C, name='fo_test_sleep_ms')
            import :: c_int
            integer(c_int), value :: microseconds
        end function c_usleep
    end interface

contains

    integer(c_int64_t) function mcp_process_start_time(pid)
        integer, intent(in) :: pid

        mcp_process_start_time = c_process_start_time(int(pid, c_int))
    end function mcp_process_start_time

    logical function mcp_process_identity_running(pid, start_time)
        integer, intent(in) :: pid
        integer(c_int64_t), intent(in) :: start_time

        mcp_process_identity_running = c_process_running(int(pid, c_int)) == 1 .and. &
            c_process_start_time(int(pid, c_int)) == start_time
    end function mcp_process_identity_running

    subroutine mcp_kill_owned_tree(pid, start_time, status)
        integer, intent(in) :: pid
        integer(c_int64_t), intent(in) :: start_time
        integer, intent(out) :: status
        integer(c_int), allocatable :: processes(:)
        integer(c_int64_t), allocatable :: starts(:)
        integer(c_int) :: count, signal_status
        integer :: i, attempt

        allocate(processes(1024), starts(1024))
        count = c_collect_descendants(int(pid, c_int), start_time, processes, starts, &
            int(size(processes), c_int))
        call assert_true(count >= 0, 'captures descendants only under exact owner identity')
        status = -1
        if (count < 0) return
        do i = int(count) + 1, 2, -1
            signal_status = c_signal_identity(processes(i), starts(i), 9_c_int)
            if (signal_status /= 0 .and. c_process_running(processes(i)) == 1 .and. &
                    c_process_start_time(processes(i)) == starts(i)) then
                call assert_true(.false., 'kills a descendant with its recorded identity')
            end if
        end do
        status = int(c_signal_identity(int(pid, c_int), start_time, 9_c_int))
        call assert_true(status == 0, 'kills the exact owner identity after descendants')
        do attempt = 1, 250
            if (c_process_running(int(pid, c_int)) == 0 .or. &
                    c_process_start_time(int(pid, c_int)) /= start_time) exit
            signal_status = c_usleep(20_c_int)
        end do
        call assert_true(c_process_running(int(pid, c_int)) == 0 .or. &
            c_process_start_time(int(pid, c_int)) /= start_time, &
            'owned supervisor exits after bounded identity-safe termination')
    end subroutine mcp_kill_owned_tree

end module fo_test_process_identity
