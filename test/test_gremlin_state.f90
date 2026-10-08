program test_gremlin_state
    use fo_util, only: temporary_root
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_null_char
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_cache, only: HASH_LEN
    use fo_gremlin_state
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_process, only: process_getpid, process_set_async_scope, argv_push, &
        process_start_argv_logged, process_poll_pid
    use fo_fs, only: fs_sleep_ms, fs_remove_tree
    implicit none

    interface
        integer(c_int) function fo_c_generation_list_tree(root, manifest) &
                bind(C, name='fo_c_generation_list_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), manifest(*)
        end function fo_c_generation_list_tree
        integer(c_int) function c_setenv(name, value, overwrite) &
                bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
        integer(c_int) function c_unsetenv(name) bind(C, name='unsetenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
        end function c_unsetenv
        integer(c_int) function c_kill(pid, signal_number) bind(C, name='kill')
            import :: c_int
            integer(c_int), value :: pid, signal_number
        end function c_kill
        integer(c_int) function c_open(path, flags) bind(C, name='open')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: flags
        end function c_open
        integer(c_int) function c_flock(fd, operation) bind(C, name='flock')
            import :: c_int
            integer(c_int), value :: fd, operation
        end function c_flock
        integer(c_int) function c_close(fd) bind(C, name='close')
            import :: c_int
            integer(c_int), value :: fd
        end function c_close
        subroutine c_exit(code) bind(C, name="_exit")
            import :: c_int
            integer(c_int), value :: code
        end subroutine c_exit
    end interface

    integer :: n_pass = 0, n_fail = 0
    character(len=4096) :: arg, executable
    character(len=1024) :: lane, ready, gate, result, done, seen
    integer :: cmdstat
    character(len=1024) :: root

    call get_command_argument(1, arg)
    select case (trim(arg))
    case ('--admission-child')
        call get_command_argument(2, lane)
        call get_command_argument(3, arg)
        call get_command_argument(4, ready)
        call get_command_argument(5, gate)
        call get_command_argument(6, result)
        call admission_child(trim(lane), trim(arg), trim(ready), trim(gate), &
            trim(result))
        stop
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
    case ('--host-lease-child')
        call get_command_argument(2, lane)
        call get_command_argument(3, arg)
        call get_command_argument(4, ready)
        call get_command_argument(5, result)
        call get_command_argument(6, done)
        call get_command_argument(7, seen)
        call host_lease_child(trim(lane), trim(arg), trim(ready), trim(result), &
            trim(done), trim(seen))
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
    call test_host_weighted_leases(trim(executable))
    call test_nested_host_admission(trim(executable))
    call test_generation_pins(trim(executable))
    call test_bounded_generation_prune()
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
        character(len=1024) :: root
        character(len=128) :: lane_id, sid(2)
        character(len=256) :: message, status_text
        character(len=128) :: owner_start
        character(len=4096) :: path, wrong_id, command
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
        character(len=1024) :: root
        character(len=128) :: lane_id, stale_id, new_id
        character(len=256) :: message
        character(len=4096) :: cmd
        character(len=1024) :: state_dir, scratch_file, evidence_file
        integer :: ierr, u
        logical :: exists
        type(gremlin_session_t) :: session

        call test_root(root)
        lane_id = 'stale-'//trim(root(18:))
        cmd = '"'//trim(exe)//'" --crash-session '//trim(lane_id)//' '// &
            trim(root)//'/stale.result'
        call execute_command_line(trim(cmd), exitstat=ierr, cmdstat=u)
        call assert(u == 0 .and. ierr == 0, 'crashed-owner fixture exits cleanly')
        call read_single_line(trim(root)//'/stale.result', stale_id)
        call gremlin_session_state_dir('.', trim(lane_id), state_dir, ierr, message)
        call assert(ierr == 0, 'stale lane state is addressable')
        scratch_file = trim(state_dir)//'/views/execution-orphan/.fo-tmp/owned.tmp'
        evidence_file = trim(state_dir)//'/views/execution-orphan/fixture.txt'
        call execute_command_line('mkdir -p '// &
            trim(state_dir)//'/views/execution-orphan/.fo-tmp', exitstat=ierr)
        call assert(ierr == 0, 'create an abandoned private execution view')
        call touch(trim(scratch_file))
        call touch(trim(evidence_file))
        call gremlin_session_acquire('.', trim(lane_id), session, ierr, message)
        new_id = session%session_id
        call assert(ierr == 0 .and. session%owner, &
            'dead owner record is recovered after its process exits')
        inquire (file=trim(scratch_file), exist=exists)
        call assert(.not. exists, 'recovered owner removes abandoned private scratch')
        inquire (file=trim(evidence_file), exist=exists)
        call assert(exists, 'recovered owner preserves retained fixture evidence')
        call assert(trim(new_id) /= trim(stale_id), &
            'recovery creates a new session instead of attaching stale identity')
        call gremlin_session_release(session, ierr, message)
        call assert(ierr == 0, 'recovered owner releases cleanly')
    end subroutine test_stale_owner_recovery

    subroutine test_capacity_leases(exe)
        character(len=*), intent(in) :: exe
        character(len=1024) :: root
        character(len=128) :: resource, slot_text
        character(len=4096) :: cmd, ready_path, result_path
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

    subroutine test_host_weighted_leases(exe)
        character(len=*), intent(in) :: exe
        character(len=1024) :: root
        character(len=128) :: resource
        character(len=4096) :: command, base, ready, result
        character(len=4096) :: previous_state, isolated_state
        character(len=12) :: number
        integer :: i, cmd_status, ierr, count, slot1, slot2, busy
        integer :: env_status, env_rc
        integer :: successes, busy_count, slots(2)
        type(gremlin_lease_t) :: legacy_lease
        logical :: exists

        call test_root(root)
        write (resource, '(a,i0)') 'host-weighted-', process_getpid()
        do i = 1, 3
            write (base, '(a,"/host-lease-",i0)') trim(root), i
            write (number, '(i0)') i
            command = '"'//trim(exe)//'" --host-lease-child '// &
                trim(resource)//' 1 '//trim(base)//'.ready '// &
                trim(base)//'.result '//trim(root)//'/state-'//trim(number)//' '// &
                trim(root)//'/xdg-'//trim(number)// &
                ' >/dev/null 2>&1 &'
            call execute_command_line(trim(command), cmdstat=cmd_status)
            call assert(cmd_status == 0, 'host weighted contender starts')
        end do
        successes = 0
        busy_count = 0
        do i = 1, 3
            write (base, '(a,"/host-lease-",i0)') trim(root), i
            call wait_file(trim(base)//'.ready', 500)
            call wait_file(trim(base)//'.result', 500)
            open (newunit=cmd_status, file=trim(base)//'.result', status='old')
            read (cmd_status, *) ierr, count, slot1, slot2, busy
            close (cmd_status)
            if (ierr == 0) then
                successes = successes + 1
                if (count == 1 .and. successes <= size(slots)) &
                    slots(successes) = slot1
            else if (busy == 1) then
                busy_count = busy_count + 1
            end if
        end do
        call assert(successes == 2 .and. busy_count == 1, &
            'three host lease contenders with distinct state/XDG roots admit only two')
        if (successes == 2) call assert(slots(1) /= slots(2), &
            'host lease contenders occupy distinct capacity slots')
        do i = 1, 3
            write (base, '(a,"/host-lease-",i0)') trim(root), i
            call wait_file(trim(base)//'.result.released', 1500)
        end do

        ! Weight two must be acquired atomically and exclude a weight-one owner.
        base = trim(root)//'/host-heavy'
        command = '"'//trim(exe)//'" --host-lease-child '//trim(resource)// &
            ' 2 '//trim(base)//'.ready '//trim(base)//'.result '// &
            trim(root)//'/heavy-state '//trim(root)//'/heavy-xdg >/dev/null 2>&1 &'
        call execute_command_line(trim(command), cmdstat=cmd_status)
        call wait_file(trim(base)//'.ready', 500)
        call wait_file(trim(base)//'.result', 500)
        open (newunit=cmd_status, file=trim(base)//'.result', status='old')
        read (cmd_status, *) ierr, count, slot1, slot2, busy
        close (cmd_status)
        call assert(ierr == 0 .and. count == 2 .and. busy == 0 .and. &
            slot1 /= slot2, 'weight-two admission atomically owns both slots')
        base = trim(root)//'/host-light'
        command = '"'//trim(exe)//'" --host-lease-child '//trim(resource)// &
            ' 1 '//trim(base)//'.ready '//trim(base)//'.result '// &
            trim(root)//'/light-state '//trim(root)//'/light-xdg >/dev/null 2>&1 &'
        call execute_command_line(trim(command), cmdstat=cmd_status)
        call wait_file(trim(base)//'.ready', 500)
        call wait_file(trim(base)//'.result', 500)
        open (newunit=cmd_status, file=trim(base)//'.result', status='old')
        read (cmd_status, *) ierr, count, slot1, slot2, busy
        close (cmd_status)
        call assert(ierr /= 0 .and. busy == 1, &
            'weight-two host lease excludes another contender without partial admission')
        call wait_file(trim(root)//'/host-heavy.result.released', 1500)
        call execute_command_line('rm -rf '//trim(root)//'/host-lease-* '// &
            trim(root)//'/host-heavy* '//trim(root)//'/host-light*')

        call get_environment_variable('FO_GREMLIN_STATE_DIR', previous_state, &
            status=env_status)
        isolated_state = trim(root)//'/invalid-capacity-state'
        env_rc = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(isolated_state)//c_null_char, 1_c_int)
        call assert(env_rc == 0, 'isolated legacy lease state root is selected')
        call gremlin_lease_acquire('legacy-invalid-capacity', 0, legacy_lease, &
            ierr, command)
        inquire (file=trim(isolated_state), exist=exists)
        call assert(ierr /= 0 .and. .not. exists, &
            'legacy invalid capacity is rejected before filesystem side effects')
        if (env_status == 0) then
            env_rc = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
                trim(previous_state)//c_null_char, 1_c_int)
        else
            env_rc = c_unsetenv('FO_GREMLIN_STATE_DIR'//c_null_char)
        end if
        call assert(env_rc == 0, 'legacy lease state root is restored')
    end subroutine test_host_weighted_leases

    subroutine test_nested_host_admission(exe)
        character(len=*), intent(in) :: exe
        type(gremlin_lease_group_t) :: group
        type(gremlin_session_t) :: owner
        character(len=1024) :: root, resource, base, old_scope, dead_scope
        character(len=4096) :: message, prior_dir, prior_pid, prior_start
        integer :: ierr, i, successes, blocked, count, busy_number, pid
        integer :: children(4), child, status_dir, status_pid, status_start
        integer :: payload_pid, unit, rc, attempt, gate_fd
        logical :: busy

        call test_root(root)
        write (resource, '(a,i0)') 'nested-host-', process_getpid()
        call get_environment_variable('FO_GREMLIN_PROCESS_SCOPE_DIR', prior_dir, &
            status=status_dir)
        call get_environment_variable('FO_GREMLIN_PROCESS_SCOPE_PID', prior_pid, &
            status=status_pid)
        call get_environment_variable('FO_GREMLIN_PROCESS_SCOPE_START', prior_start, &
            status=status_start)
        call gremlin_session_acquire('.', trim(resource), owner, ierr, message)
        call assert(ierr == 0 .and. owner%owner, 'admission test owns an async scope')
        if (ierr /= 0) return
        call process_set_async_scope(owner%state_dir, owner%owner_pid, &
            owner%owner_start, ierr)
        call assert(ierr == 0, 'admission test binds its owned descendant scope')
        call gremlin_host_lease_acquire_weighted(trim(resource), 2, 2, group, &
            busy, ierr, message)
        call assert(ierr == 0, 'outer admission owns both global slots')
        if (ierr /= 0) return
        successes = 0
        blocked = 0
        do i = 1, 4
            write (base, '(a,"/nested-",i0)') trim(root), i
            call start_admission_child(exe, resource, '1', base, 'plain', '', &
                children(i))
        end do
        do i = 1, 4
            write (base, '(a,"/nested-",i0)') trim(root), i
            call wait_file(trim(base)//'.ready', 500)
            call read_admission_result(base, ierr, count, busy_number, pid, old_scope)
            if (ierr == 0) successes = successes + 1
            if (busy_number == 1) blocked = blocked + 1
        end do
        call assert(successes == 2 .and. blocked == 2, &
            'four nested siblings consume exactly two inherited budget slots')
        do i = 1, 2
            write (base, '(a,"/unrelated-",i0)') trim(root), i
            call start_admission_child(exe, resource, '1', base, 'global', '', child)
            call wait_file(trim(base)//'.ready', 500)
            call read_admission_result(base, ierr, count, busy_number, pid, old_scope)
            call assert(ierr /= 0 .and. busy_number == 1, &
                'unrelated contender remains excluded by the real outer global lease')
            call finish_admission_child(base, child)
        end do
        call gremlin_host_lease_release(group, ierr, message)
        call assert(ierr == 0 .and. group%count == 2 .and. group%closing, &
            'outer release revokes fresh delegation but retains active child capacity')
        base = trim(root)//'/closing-contender'
        call start_admission_child(exe, resource, '1', base, 'global', '', child)
        call wait_file(trim(base)//'.ready', 500)
        call read_admission_result(base, ierr, count, busy_number, pid, old_scope)
        call assert(ierr /= 0 .and. busy_number == 1, &
            'closing reservation still excludes actual global work until family drains')
        call finish_admission_child(base, child)
        do i = 1, 4
            write (base, '(a,"/nested-",i0)') trim(root), i
            call finish_admission_child(base, children(i))
        end do
        call gremlin_host_lease_release(group, ierr, message)
        call assert(ierr == 0 .and. group%count == 0, &
            'outer capacity retires after its admitted children finish')

        call gremlin_host_lease_acquire_weighted(trim(resource), 2, 2, group, &
            busy, ierr, message)
        base = trim(root)//'/grandparent'
        call start_admission_child(exe, resource, '2', base, 'grandchild', '', child)
        call wait_file(trim(base)//'.grand.ready', 500)
        call read_admission_result(trim(base)//'.grand', ierr, count, busy_number, &
            pid, old_scope)
        call assert(ierr == 0 .and. count == 2, &
            'grandchild runs under its parent budget without ancestor self-deadlock')
        call finish_admission_child(base, child)
        old_scope = group%scope
        call gremlin_host_lease_release(group, ierr, message)
        call gremlin_host_lease_acquire_weighted(trim(resource), 2, 2, group, &
            busy, ierr, message)
        base = trim(root)//'/stale-live'
        call start_admission_child(exe, resource, '1', base, 'hint', old_scope, child)
        call wait_file(trim(base)//'.ready', 500)
        call read_admission_result(base, ierr, count, busy_number, pid, old_scope)
        call assert(ierr /= 0 .and. busy_number == 1, &
            'same live owner reacquisition rejects a retired admission incarnation')
        call finish_admission_child(base, child)
        call gremlin_host_lease_release(group, ierr, message)

        base = trim(root)//'/dead-owner'
        call start_admission_child(exe, resource, '2', base, 'crash', '', child)
        call wait_file(trim(base)//'.ready', 500)
        call read_admission_result(base, ierr, count, busy_number, pid, dead_scope)
        call assert(ierr == 0, 'crash fixture acquires real global capacity')
        call finish_admission_child(base, child)
        call gremlin_host_lease_acquire_weighted(trim(resource), 2, 2, group, &
            busy, ierr, message)
        do attempt = 1, 500
            if (ierr == 0) exit
            call fs_sleep_ms(20)
            call gremlin_host_lease_acquire_weighted(trim(resource), 2, 2, group, &
                busy, ierr, message)
        end do
        call assert(ierr == 0, &
            'owner death releases global capacity after exact owned work drains')
        base = trim(root)//'/stale-dead'
        call start_admission_child(exe, resource, '1', base, 'hint', dead_scope, child)
        call wait_file(trim(base)//'.ready', 500)
        call read_admission_result(base, ierr, count, busy_number, pid, old_scope)
        call assert(ierr /= 0 .and. busy_number == 1, &
            'dead owner hint cannot spend a replacement owner budget')
        call finish_admission_child(base, child)
        call gremlin_host_lease_release(group, ierr, message)
        call fs_remove_tree(trim(dead_scope))

        ! Kill an already admitted original owner while its borrower has actual
        ! registered payload work. Capacity is probed independently of scopes.
        resource = 'death-host-'//trim(resource)
        base = trim(root)//'/owner-death-live-payload'
        call start_admission_child(exe, resource, '2', base, 'death-root', '', child)
        call wait_file(trim(base)//'.child.payload.pid', 1000)
        open (newunit=unit, file=trim(base)//'.child.payload.pid', status='old')
        read (unit, *) payload_pid
        close (unit)
        call read_admission_result(base, ierr, count, busy_number, pid, old_scope)
        rc = c_kill(int(pid, c_int), 9_c_int)
        call assert(rc == 0, 'SIGKILL removes the actual original admission owner')
        rc = c_kill(int(payload_pid, c_int), 0_c_int)
        call assert(rc == 0, 'borrowed registered payload is live at capacity probe')
        call gremlin_host_lease_acquire_weighted(trim(resource), 2, 2, group, &
            busy, ierr, message)
        call assert(ierr /= 0 .and. busy, &
            'owner death retains global capacity until live borrowed payload drains')
        if (ierr == 0) call gremlin_host_lease_release(group, ierr, message)
        do attempt = 1, 500
            call gremlin_host_lease_acquire_weighted(trim(resource), 2, 2, group, &
                busy, ierr, message)
            if (ierr == 0) exit
            call fs_sleep_ms(20)
        end do
        call assert(ierr == 0, 'death recovery drains payload and releases capacity')
        if (ierr == 0) call gremlin_host_lease_release(group, ierr, message)
        call process_poll_pid(child, busy, rc)

        ! A pending admission can hold the gate before it acquires child slots.
        ! Owner death must serialize revocation with that critical section.
        resource = 'gate-host-'//trim(resource)
        base = trim(root)//'/owner-death-held-gate'
        call start_admission_child(exe, resource, '2', base, 'global', '', child)
        call wait_file(trim(base)//'.ready', 1000)
        call read_admission_result(base, ierr, count, busy_number, pid, old_scope)
        gate_fd = int(c_open(trim(old_scope)//'/gate'//c_null_char, 2_c_int))
        call assert(gate_fd >= 0, 'independent admission gate handle opens')
        if (gate_fd >= 0) then
            rc = int(c_flock(int(gate_fd, c_int), 2_c_int))
            call assert(rc == 0, 'independent admission critical section is held')
            rc = int(c_kill(int(pid, c_int), 9_c_int))
            call assert(rc == 0, 'actual owner dies while admission gate is held')
            call fs_sleep_ms(100)
            call gremlin_host_lease_acquire_weighted(trim(resource), 2, 2, group, &
                busy, ierr, message)
            call assert(ierr /= 0 .and. busy, &
                'owner-death revocation waits for an in-flight admission gate')
            if (ierr == 0) call gremlin_host_lease_release(group, ierr, message)
            rc = int(c_flock(int(gate_fd, c_int), 8_c_int))
            rc = int(c_close(int(gate_fd, c_int)))
            do attempt = 1, 500
                call gremlin_host_lease_acquire_weighted(trim(resource), 2, 2, group, &
                    busy, ierr, message)
                if (ierr == 0) exit
                call fs_sleep_ms(20)
            end do
            call assert(ierr == 0, 'revoked owner releases capacity after gate drainage')
            if (ierr == 0) call gremlin_host_lease_release(group, ierr, message)
        end if
        call process_poll_pid(child, busy, rc)
        call gremlin_session_release(owner, ierr, message)
        call restore_scope_env('FO_GREMLIN_PROCESS_SCOPE_DIR', prior_dir, status_dir)
        call restore_scope_env('FO_GREMLIN_PROCESS_SCOPE_PID', prior_pid, status_pid)
        call restore_scope_env('FO_GREMLIN_PROCESS_SCOPE_START', prior_start, &
            status_start)
    end subroutine test_nested_host_admission

    subroutine restore_scope_env(name, value, status)
        character(len=*), intent(in) :: name, value
        integer, intent(in) :: status
        integer :: rc
        if (status == 0) then
            rc = c_setenv(trim(name)//c_null_char, trim(value)//c_null_char, 1_c_int)
        else
            rc = c_unsetenv(trim(name)//c_null_char)
        end if
        call assert(rc == 0, 'restores inherited process scope')
    end subroutine restore_scope_env

    subroutine test_generation_pins(exe)
        character(len=*), intent(in) :: exe
        character(len=1024) :: root, project, cache, source, manifest_before
        character(len=1024) :: manifest_after, lease_ready
        character(len=1024) :: lease_gate, lease_result
        character(len=256) :: message
        integer :: ierr, u, child_error, ios
        integer(c_int) :: list_rc
        type(generation_context_t) :: context
        type(generation_t) :: generation
        type(gremlin_lease_t) :: lease
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
        context%driver_digest = repeat('a', HASH_LEN)
        context%driver_size = 1234_c_int64_t
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

        call gremlin_generation_register_lease_at(trim(generation%root), &
            lease, ierr, message)
        if (ierr == 0) call gremlin_lease_release(lease, ierr, message)
        call assert(ierr == 0, 'actual immutable snapshot registers through sidecar')
        call gremlin_generation_pin_at(trim(generation%root), .true., ierr, message)
        call assert(ierr == 0, 'immutable generation pins outside the bundle')
        call gremlin_generation_prune_at(trim(generation%root), ierr, message)
        call assert(ierr /= 0, 'pinned immutable generation cannot be pruned')
        inquire (file=trim(generation%root), exist=exists)
        call assert(exists, 'pinned immutable generation remains on disk')
        call gremlin_generation_pin_at(trim(generation%root), .false., ierr, message)
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
        call gremlin_generation_prune_at(trim(generation%root), ierr, message)
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
        call gremlin_generation_lease_acquire_at(trim(generation%root), &
            lease, ierr, message)
        call assert(ierr /= 0, 'pruned generation no longer resolves from sidecar')
        call execute_command_line('rm -rf '//trim(root))
    end subroutine test_generation_pins

    subroutine test_bounded_generation_prune()
        character(len=1024) :: root, base, snapshot
        character(len=256) :: message
        character(len=12) :: number
        type(gremlin_lease_t) :: lease
        integer :: i, ierr, remaining
        logical :: exists

        call test_root(root)
        base = trim(root)//'/bounded-generations'
        call execute_command_line('mkdir -p '//trim(base)//'/.locks')
        do i = 1, 9
            write (number, '(i0)') i
            snapshot = trim(base)//'/snapshot-'//trim(number)
            call execute_command_line('mkdir -p '//trim(snapshot))
            call execute_command_line('chmod 555 '//trim(snapshot))
            call gremlin_generation_register_lease_at(trim(snapshot), lease, &
                ierr, message)
            call assert(ierr == 0, 'bounded fixture registers frozen generation')
            if (ierr /= 0) return
            call gremlin_lease_release(lease, ierr, message)
            call assert(ierr == 0, 'bounded fixture releases registration lease')
            if (ierr /= 0) return
        end do
        call gremlin_generation_prune_inactive(trim(base)//'/snapshot-1', &
            ierr, message, min_age=0)
        if (ierr /= 0) write (output_unit, '(a,i0,2a)') &
            '  bounded prune error ', ierr, ': ', trim(message)
        call assert(ierr == 0, 'bounded inactive scan completes')
        remaining = 0
        do i = 1, 9
            write (number, '(i0)') i
            inquire (file=trim(base)//'/snapshot-'//trim(number), exist=exists)
            if (exists) remaining = remaining + 1
        end do
        call assert(remaining == 1, 'one scan prunes at most eight generations')
        call gremlin_generation_prune_inactive(trim(base)//'/snapshot-1', &
            ierr, message, min_age=0)
        if (ierr /= 0) write (output_unit, '(a,i0,2a)') &
            '  bounded retry error ', ierr, ': ', trim(message)
        call assert(ierr == 0, 'second bounded scan completes')
        remaining = 0
        do i = 1, 9
            write (number, '(i0)') i
            inquire (file=trim(base)//'/snapshot-'//trim(number), exist=exists)
            if (exists) remaining = remaining + 1
        end do
        call assert(remaining == 0, 'later scan retires remaining generation')
    end subroutine test_bounded_generation_prune

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

    subroutine host_lease_child(resource, weight_text, ready_file, result_file, &
            state_dir, xdg_dir)
        character(len=*), intent(in) :: resource, weight_text, ready_file, result_file
        character(len=*), intent(in) :: state_dir, xdg_dir
        type(gremlin_lease_group_t) :: group
        character(len=256) :: message
        integer :: ierr, unit, weight
        logical :: busy

        read (weight_text, *) weight
        ierr = int(c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(state_dir)//c_null_char, 1_c_int))
        ierr = int(c_setenv('XDG_CACHE_HOME'//c_null_char, &
            trim(xdg_dir)//c_null_char, 1_c_int))
        call gremlin_host_lease_acquire_weighted(trim(resource), 2, &
            weight, group, busy, ierr, message)
        open (newunit=unit, file=result_file, status='replace')
        write (unit, *) ierr, group%count, group%members(1)%slot, &
            group%members(2)%slot, merge(1, 0, busy)
        close (unit)
        call touch(ready_file)
        if (ierr == 0) then
            call execute_command_line('sleep 8')
            call gremlin_host_lease_release(group, ierr, message)
        end if
        call touch(trim(result_file)//'.released')
    end subroutine host_lease_child

    subroutine start_admission_child(exe, resource, weight, base, mode, hint, pid)
        character(len=*), intent(in) :: exe, resource, weight, base, mode, hint
        integer, intent(out) :: pid
        character(:), allocatable :: packed
        integer :: n_args, ierr
        packed = ''
        n_args = 0
        call argv_push(packed, n_args, trim(exe))
        call argv_push(packed, n_args, '--admission-child')
        call argv_push(packed, n_args, trim(resource))
        call argv_push(packed, n_args, trim(weight))
        call argv_push(packed, n_args, trim(base))
        call argv_push(packed, n_args, trim(mode))
        call argv_push(packed, n_args, trim(hint))
        call process_start_argv_logged('.', packed, n_args, trim(base)//'.log', &
            pid, ierr)
        call assert(ierr == 0, 'native owned admission child starts')
    end subroutine start_admission_child

    subroutine read_admission_result(base, ierr, count, busy, pid, scope)
        character(len=*), intent(in) :: base
        integer, intent(out) :: ierr, count, busy, pid
        character(len=*), intent(out) :: scope
        integer :: unit, ios, slot1, slot2
        ierr = 1
        count = 0
        busy = 0
        pid = 0
        scope = ''
        open (newunit=unit, file=trim(base)//'.result', status='old', iostat=ios)
        if (ios /= 0) return
        read (unit, *, iostat=ios) ierr, count, slot1, slot2, busy, pid
        if (ios == 0) read (unit, '(a)', iostat=ios) scope
        close (unit)
        call assert(ios == 0, 'admission child emits a complete behavioral result')
    end subroutine read_admission_result

    subroutine finish_admission_child(base, pid)
        character(len=*), intent(in) :: base
        integer, intent(in) :: pid
        integer :: attempt, exitcode
        logical :: finished
        call touch(trim(base)//'.go')
        call wait_file(trim(base)//'.done', 1000)
        finished = .false.
        do attempt = 1, 1000
            call process_poll_pid(pid, finished, exitcode)
            if (finished) exit
            call fs_sleep_ms(10)
        end do
        call assert(finished, 'owned admission child is drained before cleanup')
    end subroutine finish_admission_child

    subroutine admission_child(resource, weight_text, base, mode, hint)
        character(len=*), intent(in) :: resource, weight_text, base, mode, hint
        type(gremlin_lease_group_t) :: group
        type(gremlin_session_t) :: owner
        character(len=4096) :: exe, message, lane, command
        character(:), allocatable :: packed
        integer :: ierr, unit, weight, child, n_args
        logical :: busy
        call get_command_argument(0, exe)
        write (lane, '(a,i0)') 'admission-child-', process_getpid()
        call gremlin_session_acquire('.', trim(lane), owner, ierr, message)
        if (ierr /= 0) stop 1
        call process_set_async_scope(owner%state_dir, owner%owner_pid, &
            owner%owner_start, ierr)
        if (ierr /= 0) stop 1
        if (mode == 'global') then
            ierr = c_unsetenv('FO_GREMLIN_ADMISSION_SCOPE'//c_null_char)
        else if (mode == 'hint') then
            ierr = c_setenv('FO_GREMLIN_ADMISSION_SCOPE'//c_null_char, &
                trim(hint)//c_null_char, 1_c_int)
        end if
        read (weight_text, *) weight
        call gremlin_host_lease_acquire_weighted(resource, 2, weight, group, &
            busy, ierr, message)
        open (newunit=unit, file=trim(base)//'.result', status='replace')
        write (unit, *) ierr, group%count, group%members(1)%slot, &
            group%members(2)%slot, merge(1, 0, busy), process_getpid()
        write (unit, '(a)') trim(group%scope)
        close (unit)
        call touch(trim(base)//'.ready')
        if (ierr == 0) then
            if (mode == 'death-root') then
                call start_admission_child(trim(exe), resource, weight_text, &
                    trim(base)//'.child', 'payload', '', child)
            end if
            if (mode == 'payload') then
                packed = ''
                n_args = 0
                command = 'trap "" TERM; echo $$ > "'//trim(base)// &
                    '.payload.pid"; while :; do sleep 0.1; done'
                call argv_push(packed, n_args, '/bin/sh')
                call argv_push(packed, n_args, '-c')
                call argv_push(packed, n_args, trim(command))
                call process_start_argv_logged('.', packed, n_args, &
                    trim(base)//'.payload.log', child, ierr)
                if (ierr /= 0) stop 1
            end if
            if (mode == 'grandchild') then
                call start_admission_child(trim(exe), resource, weight_text, &
                    trim(base)//'.grand', 'plain', '', child)
            end if
            call wait_file(trim(base)//'.go', 1000)
            if (mode == 'grandchild') then
                call finish_admission_child(trim(base)//'.grand', child)
            end if
            if (mode == 'crash') then
                call touch(trim(base)//'.done')
                call c_exit(99_c_int)
            end if
            call gremlin_host_lease_release(group, ierr, message)
            call assert(ierr == 0 .and. group%count == 0, &
                'child relinquishes its own budget after descendants finish')
        end if
        call touch(trim(base)//'.done')
        call gremlin_session_release(owner, ierr, message)
    end subroutine admission_child

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
        character(len=4096) :: temp_root
        integer :: env_status

        write (suffix, '(i0)') process_getpid()
        call get_environment_variable('TMPDIR', temp_root, status=env_status)
        if (env_status == 0 .and. len_trim(temp_root) > 0) then
            root = trim(temp_root)//'/fo_gremlin_state_test_'//trim(suffix)
        else
            root = temporary_root()//'/fo_gremlin_state_test_'//trim(suffix)
        end if
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
