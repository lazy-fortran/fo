program test_gremlin_state
    use, intrinsic :: iso_c_binding, only: c_associated, c_char, c_int, &
        c_null_char, c_ptr
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_gremlin_state
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_process, only: process_getpid
    implicit none

    interface
        function c_realpath(path, resolved) bind(C, name='realpath') result(pointer)
            import :: c_char, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: resolved(*)
            type(c_ptr) :: pointer
        end function c_realpath

        integer(c_int) function fo_c_generation_list_tree(root, manifest) &
                bind(C, name='fo_c_generation_list_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), manifest(*)
        end function fo_c_generation_list_tree
    end interface

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
    case ('--generation-lease-child')
        call get_command_argument(2, lane)
        call get_command_argument(3, ready)
        call get_command_argument(4, gate)
        call get_command_argument(5, result)
        call generation_lease_child(trim(lane), trim(ready), trim(gate), &
            trim(result))
        stop
    end select

    call get_command_argument(0, executable)
    call test_concurrent_sessions(trim(executable))
    call test_stale_owner_recovery(trim(executable))
    call test_capacity_leases(trim(executable))
    call test_generation_pins(trim(executable))
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

    subroutine test_generation_pins(exe)
        character(len=*), intent(in) :: exe
        character(len=256) :: root, project, cache, source, manifest_before
        character(len=256) :: manifest_after, actual_root, lease_ready
        character(len=256) :: lease_gate, lease_result
        character(len=256) :: message
        integer :: ierr, u, child_error, ios
        integer(c_int) :: list_rc
        character(kind=c_char) :: canonical_buffer(4096)
        character(len=:), allocatable :: canonical_root
        type(c_ptr) :: canonical_pointer
        type(generation_context_t) :: context
        type(generation_t) :: generation
        logical :: exists

        call test_root(root)
        root = trim(root)//'/generation_fixture'
        project = trim(root)//'/project'
        cache = trim(root)//'/cache'
        source = trim(project)//'/src/main.f90'
        call execute_command_line('mkdir -p '//trim(project)//'/src')
        open (newunit=u, file=trim(source), status='replace')
        write (u, '(a)') 'program immutable_generation_fixture'
        write (u, '(a)') 'end program immutable_generation_fixture'
        close (u)
        context%toolchain = 'state integration fixture'
        context%flags = '-O0'
        context%environment = 'test'
        context%base_commit = 'state-fixture'
        context%patch_digest = trim(root)
        call generation_capture(trim(project), trim(cache), context, generation, &
            ierr, message)
        call assert(ierr == 0, 'actual generation provider publishes immutable snapshot')
        if (ierr /= 0) then
            write (output_unit, '(a)') trim(message)
            return
        end if

        manifest_before = trim(root)//'/before.list'
        manifest_after = trim(root)//'/after.list'
        list_rc = fo_c_generation_list_tree(trim(generation%root)//c_null_char, &
            trim(manifest_before)//c_null_char)
        call assert(list_rc == 0, 'immutable snapshot manifest is captured')

        call gremlin_generation_register_at(trim(generation%root), ierr, message)
        call assert(ierr == 0, 'actual immutable snapshot registers through sidecar')
        canonical_buffer = c_null_char
        canonical_pointer = c_realpath(trim(generation%root)//c_null_char, &
            canonical_buffer)
        call assert(c_associated(canonical_pointer), &
            'snapshot root resolves to its canonical path')
        canonical_root = c_buffer_string(canonical_buffer)
        call gremlin_generation_root(trim(generation%identity), actual_root, ierr, message)
        call assert(ierr == 0 .and. trim(actual_root) == trim(canonical_root), &
            'generation identity resolves to its canonical snapshot root')
        call gremlin_generation_pin_at(trim(generation%root), .true., ierr, message)
        call assert(ierr == 0, 'immutable generation pins outside the bundle')
        call gremlin_generation_pin(trim(generation%identity), .true., ierr, message)
        call assert(ierr == 0, 'generation ID pin resolves to its sidecar')
        call gremlin_generation_prune_at(trim(generation%root), ierr, message)
        call assert(ierr /= 0, 'pinned immutable generation cannot be pruned')
        inquire (file=trim(generation%root), exist=exists)
        call assert(exists, 'pinned immutable generation remains on disk')
        call gremlin_generation_pin(trim(generation%identity), .false., ierr, message)
        call assert(ierr == 0, 'immutable generation unpins outside the bundle')
        lease_ready = trim(root)//'/lease.ready'
        lease_gate = trim(root)//'/lease.go'
        lease_result = trim(root)//'/lease.result'
        call start_generation_lease_child(exe, trim(generation%root), &
            trim(lease_ready), trim(lease_gate), trim(lease_result))
        call wait_file(trim(lease_result), 500)
        call wait_file(trim(lease_ready), 500)
        open (newunit=u, file=trim(lease_result), status='old', iostat=ios)
        if (ios == 0) read (u, *, iostat=ios) child_error
        if (ios == 0) close (u)
        if (ios /= 0) child_error = ios
        call assert(child_error == 0, 'separate process acquires immutable generation lease')
        if (child_error /= 0) return
        call gremlin_generation_prune(trim(generation%identity), ierr, message)
        call assert(ierr /= 0, 'active lease prevents immutable generation pruning')
        inquire (file=trim(generation%root), exist=exists)
        call assert(exists, 'leased immutable generation remains on disk')
        call touch(trim(lease_gate))
        call wait_file(trim(lease_result)//'.released', 500)

        list_rc = fo_c_generation_list_tree(trim(generation%root)//c_null_char, &
            trim(manifest_after)//c_null_char)
        call assert(list_rc == 0, 'snapshot remains readable after state operations')
        call assert(manifests_equal(trim(manifest_before), trim(manifest_after)), &
            'registration, pinning, leasing, and unpinning leave bundle unchanged')

        call gremlin_generation_prune_at(trim(generation%root), ierr, message)
        call assert(ierr == 0, 'released immutable generation prunes successfully')
        inquire (file=trim(generation%root), exist=exists)
        call assert(.not. exists, 'successful prune removes sealed snapshot tree')
        call gremlin_generation_root(trim(generation%identity), actual_root, ierr, message)
        call assert(ierr /= 0, 'pruned generation no longer resolves from sidecar')
        call execute_command_line('rm -rf '//trim(root))
    end subroutine test_generation_pins

    subroutine generation_lease_child(snapshot_root, ready_file, gate_file, result_file)
        character(len=*), intent(in) :: snapshot_root, ready_file, gate_file
        character(len=*), intent(in) :: result_file
        type(gremlin_lease_t) :: lease
        character(len=256) :: message
        integer :: ierr, u

        call gremlin_generation_lease_acquire_at(snapshot_root, lease, ierr, message)
        open (newunit=u, file=result_file, status='replace')
        write (u, '(i0)') ierr
        close (u)
        call touch(ready_file)
        if (ierr /= 0) return
        call wait_file(gate_file, 1000)
        call gremlin_lease_release(lease, ierr, message)
        call touch(trim(result_file)//'.released')
    end subroutine generation_lease_child

    subroutine start_generation_lease_child(exe, snapshot_root, ready_file, &
            gate_file, result_file)
        character(len=*), intent(in) :: exe, snapshot_root, ready_file, gate_file
        character(len=*), intent(in) :: result_file
        character(len=2048) :: command
        integer :: cmd_status

        command = '"'//trim(exe)//'" --generation-lease-child "'// &
            trim(snapshot_root)//'" "'//trim(ready_file)//'" "'// &
            trim(gate_file)//'" "'//trim(result_file)// &
            '" >/dev/null 2>&1 &'
        call execute_command_line(trim(command), cmdstat=cmd_status)
        call assert(cmd_status == 0, 'generation lease worker starts')
    end subroutine start_generation_lease_child

    logical function manifests_equal(first, second)
        character(len=*), intent(in) :: first, second
        character(len=4096) :: line_first, line_second
        integer :: unit_first, unit_second, ios_first, ios_second

        manifests_equal = .false.
        open (newunit=unit_first, file=first, status='old', iostat=ios_first)
        if (ios_first /= 0) return
        open (newunit=unit_second, file=second, status='old', iostat=ios_second)
        if (ios_second /= 0) then
            close (unit_first)
            return
        end if
        do
            read (unit_first, '(a)', iostat=ios_first) line_first
            read (unit_second, '(a)', iostat=ios_second) line_second
            if (ios_first /= ios_second) exit
            if (ios_first /= 0) then
                manifests_equal = ios_first < 0
                exit
            end if
            if (line_first /= line_second) exit
        end do
        close (unit_first)
        close (unit_second)
    end function manifests_equal

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

    function c_buffer_string(buffer) result(value)
        character(kind=c_char), intent(in) :: buffer(:)
        character(len=:), allocatable :: value

        integer :: i, n

        n = 0
        do i = 1, size(buffer)
            if (buffer(i) == c_null_char) exit
            n = n + 1
        end do
        allocate (character(len=n) :: value)
        do i = 1, n
            value(i:i) = buffer(i)
        end do
    end function c_buffer_string

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
