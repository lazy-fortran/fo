program test_gremlin_state
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_gremlin_state
    use fo_process, only: process_getpid
    implicit none

    integer :: n_pass = 0, n_fail = 0
    character(len=4096) :: arg, executable
    character(len=1024) :: lane, ready, gate, result, done, seen
    integer :: cmdstat
    character(len=128) :: root

    call get_command_argument(1, arg)
    select case (trim(arg))
    case ('--session-child')
        call get_command_argument(2, lane)
        call get_command_argument(3, ready)
        call get_command_argument(4, gate)
        call get_command_argument(5, result)
        call get_command_argument(6, done)
        call get_command_argument(7, seen)
        call session_child(trim(lane), trim(ready), trim(gate), trim(result), &
            trim(done), trim(seen))
        stop
    case ('--crash-session')
        call get_command_argument(2, lane)
        call get_command_argument(3, result)
        call crash_session_child(trim(lane), trim(result))
        stop
    case ('--acquire-once')
        call get_command_argument(2, lane)
        call get_command_argument(3, result)
        call acquire_once_child(trim(lane), trim(result))
        stop
    case ('--lease-child')
        call get_command_argument(2, lane)
        call get_command_argument(3, ready)
        call get_command_argument(4, gate)
        call get_command_argument(5, result)
        call get_command_argument(6, done)
        call lease_child(trim(lane), trim(ready), trim(gate), trim(result), &
            trim(done))
        stop
    end select

    call get_command_argument(0, executable)
    call test_concurrent_sessions(trim(executable))
    call test_stale_owner_recovery(trim(executable))
    call test_capacity_leases(trim(executable))
    call test_generation_pins()
    call test_root(root)
    call execute_command_line('rm -rf '//trim(root))

    write (output_unit, '(a,i0,a,i0,a)') 'gremlin state: ', n_pass, ' pass, ', &
        n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (output_unit, '(a,a)') 'FAIL: ', message
        end if
    end subroutine assert

    subroutine test_concurrent_sessions(exe)
        character(len=*), intent(in) :: exe
        character(len=128) :: root, lane_id, sid(2)
        character(len=256) :: message, status_text
        character(len=128) :: owner_start
        character(len=512) :: path, wrong_id, command
        integer :: pid, ierr, owners(2), owner_pid(2), contender_error
        integer :: contender_owner, u, cmd_status
        logical :: stopped
        logical :: matches
        type(gremlin_session_t) :: other, attached

        call test_root(root)
        lane_id = 'same-'//trim(root(18:))
        path = trim(root)//'/a.ready'
        call start_session_child(exe, lane_id, trim(path), trim(root)//'/go', &
            trim(root)//'/a.result', trim(root)//'/done', trim(root)//'/seen')
        path = trim(root)//'/b.ready'
        call start_session_child(exe, lane_id, trim(path), trim(root)//'/go', &
            trim(root)//'/b.result', trim(root)//'/done', trim(root)//'/seen')
        call wait_file(trim(root)//'/a.ready', 500)
        call wait_file(trim(root)//'/b.ready', 500)
        call touch(trim(root)//'/go')
        call wait_file(trim(root)//'/a.result', 500)
        call wait_file(trim(root)//'/b.result', 500)
        call read_session_result(trim(root)//'/a.result', sid(1), owners(1), &
            owner_pid(1))
        call read_session_result(trim(root)//'/b.result', sid(2), owners(2), &
            owner_pid(2))
        call assert(trim(sid(1)) == trim(sid(2)), &
            'simultaneous starts attach to the same session ID')
        call assert(sum(owners) == 1, 'exactly one concurrent start owns the session')

        call gremlin_session_read('.', trim(lane_id), path, pid, owner_start, &
            status_text, ierr, message)
        call assert(ierr == 0, 'live session record is readable')
        call assert(index(status_text, 'live') > 0, 'owner status is published')
        call gremlin_session_acquire('.', trim(lane_id), attached, ierr, message)
        call assert(ierr == 0 .and. attached%attached, &
            'a third process attaches to the live session')
        path = trim(attached%state_dir)//'/owner'
        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') trim(sid(1))
        write (u, '(a)') '-1'
        write (u, '(a)') 'stale-start'
        close (u)
        command = '"'//trim(exe)//'" --acquire-once '//trim(lane_id)//' '// &
            trim(root)//'/stale-live.result'
        call execute_command_line(trim(command), exitstat=cmd_status)
        call assert(cmd_status == 0, 'stale-record contender exits')
        call read_acquire_result(trim(root)//'/stale-live.result', contender_error, &
            contender_owner)
        call assert(contender_error /= 0 .and. contender_owner == 0, &
            'stale PID/start metadata cannot steal the held live lock')
        matches = gremlin_session_process_matches(attached)
        call assert(matches, 'original process remains alive while lock is held')
        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') trim(sid(1))
        write (u, '(i0)') pid
        write (u, '(a)') trim(owner_start)
        close (u)
        wrong_id = trim(sid(1))//'-wrong'
        call gremlin_session_request_stop('.', trim(lane_id), trim(wrong_id), &
            ierr, message)
        call assert(ierr /= 0, 'stop rejects a different session ID')
        call gremlin_session_request_stop('.', trim(lane_id), trim(sid(1)), &
            ierr, message)
        call assert(ierr == 0, 'stop request reaches the requested live session')

        call gremlin_session_acquire('.', 'other-'//trim(root(18:)), other, &
            ierr, message)
        call assert(ierr == 0 .and. other%owner, &
            'a distinct lane obtains an independent owner')
        call gremlin_session_stop_requested(other, stopped, ierr, message)
        call assert(ierr == 0 .and. .not. stopped, &
            'stop request does not target another lane session')
        call gremlin_session_release(other, ierr, message)
        call assert(ierr == 0, 'independent lane releases its owner lock')

        call wait_file(trim(root)//'/seen', 500)
        call wait_file(trim(root)//'/done', 500)
    end subroutine test_concurrent_sessions

    subroutine test_stale_owner_recovery(exe)
        character(len=*), intent(in) :: exe
        character(len=128) :: root, lane_id, stale_id, new_id
        character(len=256) :: message
        character(len=512) :: cmd
        integer :: ierr, u
        type(gremlin_session_t) :: session

        call test_root(root)
        lane_id = 'stale-'//trim(root(18:))
        cmd = '"'//trim(exe)//'" --crash-session '//trim(lane_id)//' '// &
            trim(root)//'/stale.result'
        call execute_command_line(trim(cmd), exitstat=ierr, cmdstat=u)
        call assert(u == 0 .and. ierr == 0, 'crashed-owner fixture exits cleanly')
        call read_single_line(trim(root)//'/stale.result', stale_id)
        call gremlin_session_acquire('.', trim(lane_id), session, ierr, message)
        new_id = session%session_id
        call assert(ierr == 0 .and. session%owner, &
            'dead owner record is recovered after its process exits')
        call assert(trim(new_id) /= trim(stale_id), &
            'recovery creates a new session instead of attaching stale identity')
        call gremlin_session_release(session, ierr, message)
        call assert(ierr == 0, 'recovered owner releases cleanly')
    end subroutine test_stale_owner_recovery

    subroutine test_capacity_leases(exe)
        character(len=*), intent(in) :: exe
        character(len=128) :: root, resource, slot_text
        character(len=512) :: cmd, ready_path, result_path
        integer :: i, successes, slots(4), ierr, cmdstat

        call test_root(root)
        resource = 'capacity-'//trim(root(18:))
        do i = 1, 4
            write (ready_path, '(a,a,i0)') trim(root), '/lease-', i
            cmd = '"'//trim(exe)//'" --lease-child '//trim(resource)//' '// &
                trim(ready_path)//'.ready '//trim(root)//'/lease.go '// &
                trim(ready_path)//'.result '//trim(root)//'/lease.done '// &
                '>/dev/null 2>&1 &'
            call execute_command_line(trim(cmd), cmdstat=cmdstat)
            call assert(cmdstat == 0, 'capacity worker starts')
        end do
        do i = 1, 4
            write (ready_path, '(a,a,i0)') trim(root), '/lease-', i
            call wait_file(trim(ready_path)//'.ready', 500)
        end do
        call touch(trim(root)//'/lease.go')
        successes = 0
        do i = 1, 4
            write (result_path, '(a,a,i0)') trim(root), '/lease-', i
            call wait_file(trim(result_path)//'.result', 500)
            call read_lease_result(trim(result_path)//'.result', ierr, slot_text)
            if (ierr == 0) then
                successes = successes + 1
                read (slot_text, *) slots(successes)
            end if
        end do
        call assert(successes == 2, 'four contenders receive only two capacity leases')
        if (successes == 2) call assert(slots(1) /= slots(2), &
            'concurrent capacity leases occupy distinct slots')
        call touch(trim(root)//'/lease.done')
        call wait_file(trim(root)//'/lease-1.result.released', 500)
        call wait_file(trim(root)//'/lease-2.result.released', 500)
        call wait_file(trim(root)//'/lease-3.result.released', 500)
        call wait_file(trim(root)//'/lease-4.result.released', 500)
    end subroutine test_capacity_leases

    subroutine test_generation_pins()
        character(len=128) :: root, generation_id
        character(len=256) :: message
        integer :: ierr
        type(gremlin_lease_t) :: lease

        call test_root(root)
        block
            integer :: stamp
            call system_clock(count=stamp)
            write (generation_id, '(a,i0,a,i0)') 'pinned-', process_getpid(), '-', stamp
        end block
        call gremlin_generation_register(trim(generation_id), ierr, message)
        call assert(ierr == 0, 'generated snapshot registers')
        call gremlin_generation_pin(trim(generation_id), .true., ierr, message)
        call assert(ierr == 0, 'generated snapshot pins')
        call gremlin_generation_prune(trim(generation_id), ierr, message)
        call assert(ierr /= 0, 'pinned generation cannot be pruned')
        call gremlin_generation_pin(trim(generation_id), .false., ierr, message)
        call assert(ierr == 0, 'generated snapshot unpins')
        call gremlin_generation_lease_acquire(trim(generation_id), lease, ierr, message)
        call assert(ierr == 0, 'active generation lease is acquired')
        call gremlin_generation_prune(trim(generation_id), ierr, message)
        call assert(ierr /= 0, 'leased generation cannot be pruned')
        call gremlin_lease_release(lease, ierr, message)
        call assert(ierr == 0, 'generation lease releases')
        call gremlin_generation_prune(trim(generation_id), ierr, message)
        call assert(ierr == 0, 'unleased, unpinned generation prunes')
        call gremlin_generation_lease_acquire(trim(generation_id), lease, ierr, message)
        call assert(ierr /= 0, 'pruned generation is no longer leasable')
    end subroutine test_generation_pins

    subroutine session_child(lane_id, ready_file, gate_file, result_file, done_file, seen_file)
        character(len=*), intent(in) :: lane_id, ready_file, gate_file, result_file
        character(len=*), intent(in) :: done_file, seen_file
        type(gremlin_session_t) :: session
        character(len=256) :: message
        integer :: ierr, i, u
        logical :: requested

        call touch(ready_file)
        call wait_file(gate_file, 1000)
        call gremlin_session_acquire('.', lane_id, session, ierr, message)
        if (ierr /= 0) stop 2
        if (session%owner) then
            call gremlin_session_publish(session, 'live', ierr, message)
            if (ierr /= 0) stop 3
        end if
        open (newunit=u, file=result_file, status='replace')
        write (u, '(a)') trim(session%session_id)
        write (u, '(i0)') merge(1, 0, session%owner)
        write (u, '(i0)') session%owner_pid
        close (u)
        if (.not. session%owner) return
        do i = 1, 1000
            call gremlin_session_stop_requested(session, requested, ierr, message)
            if (ierr == 0 .and. requested) exit
            call execute_command_line('sleep 0.01')
        end do
        if (requested) call touch(seen_file)
        call gremlin_session_release(session, ierr, message)
        call touch(done_file)
    end subroutine session_child

    subroutine crash_session_child(lane_id, result_file)
        character(len=*), intent(in) :: lane_id, result_file
        type(gremlin_session_t) :: session
        character(len=256) :: message
        integer :: ierr, u

        call gremlin_session_acquire('.', lane_id, session, ierr, message)
        if (ierr /= 0) stop 2
        open (newunit=u, file=result_file, status='replace')
        write (u, '(a)') trim(session%session_id)
        close (u)
    end subroutine crash_session_child

    subroutine acquire_once_child(lane_id, result_file)
        character(len=*), intent(in) :: lane_id, result_file
        type(gremlin_session_t) :: session
        character(len=256) :: message
        integer :: ierr, u

        call gremlin_session_acquire('.', lane_id, session, ierr, message)
        open (newunit=u, file=result_file, status='replace')
        write (u, '(i0)') ierr
        write (u, '(i0)') merge(1, 0, session%owner)
        close (u)
        if (session%owner) then
            call gremlin_session_release(session, ierr, message)
        end if
    end subroutine acquire_once_child

    subroutine lease_child(resource, ready_file, gate_file, result_file, done_file)
        character(len=*), intent(in) :: resource, ready_file, gate_file, result_file
        character(len=*), intent(in) :: done_file
        type(gremlin_lease_t) :: lease
        character(len=256) :: message
        integer :: ierr, u

        call touch(ready_file)
        call wait_file(gate_file, 1000)
        call gremlin_lease_acquire(resource, 2, lease, ierr, message)
        open (newunit=u, file=result_file, status='replace')
        write (u, '(i0)') ierr
        write (u, '(i0)') lease%slot
        close (u)
        if (ierr /= 0) then
            call touch(trim(result_file)//'.released')
            return
        end if
        call wait_file(done_file, 1000)
        call gremlin_lease_release(lease, ierr, message)
        call touch(trim(result_file)//'.released')
    end subroutine lease_child

    subroutine start_session_child(exe, lane_id, ready_file, gate_file, result_file, &
            done_file, seen_file)
        character(len=*), intent(in) :: exe, lane_id, ready_file, gate_file
        character(len=*), intent(in) :: result_file, done_file, seen_file
        character(len=2048) :: command
        integer :: cmd_status

        command = '"'//trim(exe)//'" --session-child '//trim(lane_id)//' '// &
            trim(ready_file)//' '//trim(gate_file)//' '//trim(result_file)//' '// &
            trim(done_file)//' '//trim(seen_file)//' >/dev/null 2>&1 &'
        call execute_command_line(trim(command), cmdstat=cmd_status)
        call assert(cmd_status == 0, 'session worker starts')
    end subroutine start_session_child

    subroutine read_session_result(file, sid, owned, pid)
        character(len=*), intent(in) :: file
        character(len=*), intent(out) :: sid
        integer, intent(out) :: owned, pid
        integer :: u, ios

        open (newunit=u, file=file, status='old', iostat=ios)
        if (ios /= 0) then
            call assert(.false., 'session child result opens')
            sid = ''
            owned = 0
            pid = 0
            return
        end if
        read (u, '(a)', iostat=ios) sid
        if (ios == 0) read (u, *, iostat=ios) owned
        if (ios == 0) read (u, *, iostat=ios) pid
        close (u)
        call assert(ios == 0, 'session child result is complete')
    end subroutine read_session_result

    subroutine read_lease_result(file, ierr, slot_text)
        character(len=*), intent(in) :: file
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: slot_text
        integer :: u, ios, slot

        open (newunit=u, file=file, status='old', iostat=ios)
        if (ios /= 0) then
            ierr = ios
            slot_text = '-1'
            call assert(.false., 'capacity worker result opens')
            return
        end if
        read (u, *, iostat=ios) ierr
        if (ios == 0) read (u, *, iostat=ios) slot
        close (u)
        if (ios == 0) then
            write (slot_text, '(i0)') slot
        else
            ierr = ios
            slot_text = '-1'
        end if
    end subroutine read_lease_result

    subroutine read_acquire_result(file, ierr, owner)
        character(len=*), intent(in) :: file
        integer, intent(out) :: ierr, owner
        integer :: u, ios

        open (newunit=u, file=file, status='old', iostat=ios)
        if (ios == 0) read (u, *, iostat=ios) ierr
        if (ios == 0) read (u, *, iostat=ios) owner
        if (ios == 0) close (u)
        call assert(ios == 0, 'contender result is readable')
        if (ios /= 0) then
            ierr = ios
            owner = 0
        end if
    end subroutine read_acquire_result

    subroutine read_single_line(file, value)
        character(len=*), intent(in) :: file
        character(len=*), intent(out) :: value
        integer :: u, ios

        open (newunit=u, file=file, status='old', iostat=ios)
        if (ios == 0) read (u, '(a)', iostat=ios) value
        if (ios == 0) close (u)
        call assert(ios == 0, 'fixture result is readable')
    end subroutine read_single_line

    subroutine test_root(root)
        character(len=*), intent(out) :: root
        character(len=64) :: suffix

        write (suffix, '(i0)') process_getpid()
        root = '/var/tmp/fo_gremlin_state_test_'//trim(suffix)
        call execute_command_line('mkdir -p '//trim(root))
    end subroutine test_root

    subroutine touch(file)
        character(len=*), intent(in) :: file
        integer :: u

        open (newunit=u, file=file, status='replace')
        write (u, '(a)') 'ready'
        close (u)
    end subroutine touch

    subroutine wait_file(file, attempts)
        character(len=*), intent(in) :: file
        integer, intent(in) :: attempts
        logical :: exists
        integer :: i

        do i = 1, attempts
            inquire (file=file, exist=exists)
            if (exists) return
            call execute_command_line('sleep 0.01')
        end do
        inquire (file=file, exist=exists)
        call assert(exists, 'waited for file '//trim(file))
    end subroutine wait_file

end program test_gremlin_state
