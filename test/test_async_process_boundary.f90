program test_async_process_boundary
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char, &
        c_funptr, c_funloc, c_associated
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, int64, real64
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_sleep_ms
    use fo_process, only: argv_push, process_cancel_pid, process_getcwd, &
        process_getpid, process_poll_pid, process_start_argv_logged, process_exit, &
        process_set_async_scope
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_read, gremlin_session_release
    use fo_test_harness, only: string_list_t, process_result_t, list_add, run_process
    implicit none

    interface
        function c_signal(number, handler) bind(C, name='signal') result(old_handler)
            import :: c_int, c_funptr
            integer(c_int), value :: number
            type(c_funptr), value :: handler
            type(c_funptr) :: old_handler
        end function c_signal

        integer(c_int) function c_fork() bind(C, name='fork')
            import :: c_int
        end function c_fork

        integer(c_int) function c_setpgid(pid, group) bind(C, name='setpgid')
            import :: c_int
            integer(c_int), value :: pid, group
        end function c_setpgid

        integer(c_int) function c_setsid() bind(C, name='setsid')
            import :: c_int
        end function c_setsid

        integer(c_int) function c_getsid(pid) bind(C, name='getsid')
            import :: c_int
            integer(c_int), value :: pid
        end function c_getsid

        integer(c_int) function c_kill(pid, signal_number) bind(C, name='kill')
            import :: c_int
            integer(c_int), value :: pid, signal_number
        end function c_kill

        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv

        integer(c_int) function c_process_matches(pid, start) &
                bind(C, name='fo_gremlin_process_matches')
            import :: c_char, c_int
            integer(c_int), value :: pid
            character(kind=c_char), intent(in) :: start(*)
        end function c_process_matches

        integer(c_int) function c_recover_scope(state_dir, owner_pid, owner_start) &
                bind(C, name='fo_c_recover_async_scope')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: state_dir(*), owner_start(*)
            integer(c_int), value :: owner_pid
        end function c_recover_scope

        subroutine ignore_term(signal_number) bind(C)
            import :: c_int
            integer(c_int), value :: signal_number
        end subroutine ignore_term
    end interface

    integer :: passed, failed, pid, exitcode
    integer :: owner_pid, sentinel_pid, orphan_pid, quick_pid, ignored
    integer :: scope_owner_pid, scope_child_pid, scope_escaped_pid
    integer :: scope_sentinel_pid, scope_sentinel_lines, scope_escaped_lines
    integer :: acquire_error, release_error, match_result, cleanup_error
    integer :: cleanup_attempt
    integer :: setsid_result, io_status
    integer(int64) :: clock_rate
    integer(int64) :: ticks_start, ticks_finish
    real(real64) :: cancel_seconds
    character(len=4096) :: workdir, executable, mode, prefix
    character(len=4096) :: project, lane_id, state_root, state_message
    character(len=4096) :: owner_state_dir
    character(len=128) :: session_id, owner_start
    character(len=32) :: setsid_text
    character(len=65536) :: status_text
    character(len=512) :: scratch
    character(len=:), allocatable :: packed
    integer :: n_args
    type(c_funptr) :: previous_handler
    logical :: done, scope_owner_reaped, scratch_clean
    logical :: scope_child_quiet, scope_escaped_quiet, scope_sentinel_quiet
    logical :: recovered_owner
    logical :: darwin
    type(gremlin_session_t) :: recovered_session
    type(string_list_t) :: platform_command
    type(process_result_t) :: platform_result

    call get_command_argument(0, executable)
    call process_getcwd(workdir, exitcode)
    if (index(trim(executable), '/') == 0) executable = trim(workdir)//'/'//trim(executable)
    call get_command_argument(1, mode)
    if (trim(mode) == '--owner' .or. trim(mode) == '--sentinel' .or. &
        trim(mode) == '--orphan' .or. trim(mode) == '--scope-child' .or. &
        trim(mode) == '--quick-exit') then
        call get_command_argument(2, prefix)
        call run_helper(trim(mode), trim(prefix))
        call process_exit(2)
    end if
    if (trim(mode) == '--scope-owner') then
        call get_command_argument(2, project)
        call get_command_argument(3, lane_id)
        call get_command_argument(4, prefix)
        call run_scope_owner(trim(project), trim(lane_id), trim(prefix))
        call process_exit(2)
    end if

    passed = 0
    failed = 0
    owner_pid = 0
    sentinel_pid = 0
    orphan_pid = 0
    quick_pid = 0
    scope_owner_pid = 0
    scope_child_pid = 0
    scope_escaped_pid = 0
    scope_sentinel_pid = 0
    scope_owner_reaped = .false.
    owner_start = ''
    scratch_clean = .true.
    scope_child_quiet = .false.
    scope_escaped_quiet = .false.
    scope_sentinel_quiet = .false.
    recovered_owner = .false.
    call check(exitcode == 0 .and. len_trim(workdir) > 0, &
        'reads the test working directory')
    pid = process_getpid()
    call system_clock(count=ticks_start)
    write (scratch, '("/var/tmp/fo-async-boundary-",i0,"-",i0)') &
        pid, ticks_start
    call fs_make_dir(trim(scratch))

    call list_add(platform_command, '/usr/bin/uname')
    call list_add(platform_command, '-s')
    call run_process(platform_command, trim(workdir), platform_result)
    call check(platform_result%exit_code == 0, 'identifies the containment platform')
    darwin = index(platform_result%stdout, 'Darwin') == 1
    call check(darwin .or. index(platform_result%stdout, 'Linux') == 1, &
        'runs the Linux or Darwin containment contract')

    call start_helper('--owner', trim(scratch)//'/owned', owner_pid)
    call check(wait_for_pid(trim(scratch)//'/owned.owner.pid', owner_pid), &
        'starts an asynchronously owned process')
    call check(wait_for_pid(trim(scratch)//'/owned.nested.pid', pid), &
        'starts a descendant in a nested process group')
    call check(wait_for_pid(trim(scratch)//'/owned.deep.pid', pid), &
        'starts a descendant in a second nested process group')
    call check(wait_for_data(trim(scratch)//'/owned.owner.heartbeat'), &
        'owned process begins its heartbeat')
    call check(wait_for_data(trim(scratch)//'/owned.deep.heartbeat'), &
        'deep descendant begins its heartbeat')

    call start_helper('--sentinel', trim(scratch)//'/sentinel', sentinel_pid)
    call check(wait_for_pid(trim(scratch)//'/sentinel.sentinel.pid', pid), &
        'starts an unrelated process session')
    call check(wait_for_data(trim(scratch)//'/sentinel.sentinel.heartbeat'), &
        'unrelated sentinel begins its heartbeat')
    ignored = line_count(trim(scratch)//'/sentinel.sentinel.heartbeat')

    call system_clock(count_rate=clock_rate)
    call system_clock(count=ticks_start)
    call process_cancel_pid(owner_pid, exitcode)
    call system_clock(count=ticks_finish)
    if (clock_rate > 0) then
        cancel_seconds = real(ticks_finish - ticks_start, real64) / &
            real(clock_rate, real64)
    else
        cancel_seconds = 0.0_real64
    end if
    write (output_unit, '(a,f0.2,a)') 'Owner cancellation took ', &
        cancel_seconds, ' seconds'
    call check(exitcode == 0, 'cancels the exact owned session')
    call check(cancel_seconds >= 1.5_real64 .and. cancel_seconds < 4.0_real64, &
        'gives TERM-ignoring descendants bounded grace before KILL')
    call check(wait_for_quiet(trim(scratch)//'/owned.owner.heartbeat'), &
        'stops the owned session leader heartbeat')
    call check(wait_for_quiet(trim(scratch)//'/owned.nested.heartbeat'), &
        'stops a nested process-group descendant heartbeat')
    call check(wait_for_quiet(trim(scratch)//'/owned.deep.heartbeat'), &
        'stops a second nested process-group descendant heartbeat')
    call fs_sleep_ms(100)
    call check(line_count(trim(scratch)//'/sentinel.sentinel.heartbeat') > ignored, &
        'unrelated sentinel survives owner cancellation')

    call process_cancel_pid(sentinel_pid, exitcode)
    call check(exitcode == 0, 'cancels the unrelated sentinel by its own owner')
    call check(wait_for_quiet(trim(scratch)//'/sentinel.sentinel.heartbeat'), &
        'stops the sentinel heartbeat after exact cancellation')

    call start_helper('--quick-exit', trim(scratch)//'/quick-exit', quick_pid)
    call fs_sleep_ms(100)
    done = .false.
    exitcode = -1
    do ignored = 1, 100
        call process_poll_pid(quick_pid, done, exitcode)
        if (done) exit
        call fs_sleep_ms(10)
    end do
    call check(done .and. exitcode == 0, &
        'poll reaps the exact child that exited before its first observation')

    call start_helper('--orphan', trim(scratch)//'/orphan', orphan_pid)
    call check(wait_for_pid(trim(scratch)//'/orphan.nested.pid', pid), &
        'starts descendants before an abrupt owner exit')
    call check(wait_for_pid(trim(scratch)//'/orphan.deep.pid', pid), &
        'records nested descendants before an abrupt owner exit')
    call check(wait_for_data(trim(scratch)//'/orphan.nested.heartbeat'), &
        'nested orphan fixture is observable before owner recovery')
    done = .false.
    exitcode = -1
    do ignored = 1, 500
        call process_poll_pid(orphan_pid, done, exitcode)
        if (done) exit
        call fs_sleep_ms(10)
    end do
    if (.not. done) call process_cancel_pid(orphan_pid, exitcode)
    call check(done .and. exitcode == 0, &
        'poll recovers and cleans descendants after the owner exits')
    call check(wait_for_quiet(trim(scratch)//'/orphan.nested.heartbeat'), &
        'stops an orphaned nested process-group descendant heartbeat')
    call check(wait_for_quiet(trim(scratch)//'/orphan.deep.heartbeat'), &
        'stops an orphaned second process-group descendant heartbeat')

    state_root = trim(scratch)//'/state'
    call fs_make_dir(trim(state_root))
    ignored = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
        trim(state_root)//c_null_char, 1_c_int)
    call check(ignored == 0, 'sets an isolated Gremlin state directory')
    write (lane_id, '("boundary-crash-",i0)') process_getpid()
    prefix = trim(scratch)//'/crash'
    call start_helper('--sentinel', trim(scratch)//'/recovery-sentinel', &
        scope_sentinel_pid)
    call check(wait_for_pid(trim(scratch)//'/recovery-sentinel.sentinel.pid', pid), &
        'starts an unrelated sentinel during stale-owner recovery')
    call check(wait_for_data(trim(scratch)//'/recovery-sentinel.sentinel.heartbeat'), &
        'unrelated sentinel runs during stale-owner recovery')
    scope_sentinel_lines = line_count(trim(scratch)//'/recovery-sentinel.sentinel.heartbeat')
    call start_scope_owner(trim(workdir), trim(lane_id), trim(prefix), scope_owner_pid)
    call check(wait_for_pid(trim(prefix)//'.scope-child.pid', scope_child_pid), &
        'records the child session before the crash owner releases it')
    call check(wait_for_data(trim(prefix)//'.scope-child.heartbeat'), &
        'scoped descendant session is active before supervisor crash')
    call check(wait_for_pid(trim(prefix)//'.scope-escaped.pid', scope_escaped_pid), &
        'records the forked descendant before supervisor crash')
    call check(wait_for_data(trim(prefix)//'.scope-escaped.heartbeat'), &
        'forked descendant is active before supervisor crash')
    call check(wait_for_data(trim(prefix)//'.scope-setsid'), &
        'records the descendant setsid result')
    call read_text_file(trim(prefix)//'.scope-setsid', setsid_text)
    setsid_result = 0
    read (setsid_text, *, iostat=io_status) setsid_result
    call check(io_status == 0, 'reads the descendant setsid result')
    if (darwin) then
        call check(setsid_result == scope_escaped_pid .and. setsid_result > 0, &
            'Darwin descendant successfully escapes with setsid')
        call check(c_getsid(int(scope_escaped_pid, c_int)) == &
            int(scope_escaped_pid, c_int), 'Darwin descendant owns its new session')
    else
        call check(setsid_result == -1, 'Linux containment rejects descendant setsid')
        call check(c_getsid(int(scope_escaped_pid, c_int)) > 0 .and. &
            c_getsid(int(scope_escaped_pid, c_int)) == &
            c_getsid(int(scope_child_pid, c_int)), &
            'Linux descendant remains in the owned session')
    end if
    scope_escaped_lines = line_count(trim(prefix)//'.scope-escaped.heartbeat')
    call check(wait_for_data(trim(prefix)//'.scope-state-dir'), &
        'records the exact owner registry location before supervisor crash')
    call check(wait_for_data(trim(prefix)//'.scope-owner-start'), &
        'records the exact owner birth identity before supervisor crash')
    call fs_sleep_ms(150)
    call gremlin_session_read(trim(workdir), trim(lane_id), session_id, &
        owner_pid, owner_start, status_text, acquire_error, state_message)
    if (len_trim(owner_start) == 0) then
        call read_text_file(trim(prefix)//'.scope-owner-start', owner_start)
    end if
    call check(acquire_error == 0 .and. owner_pid == scope_owner_pid, &
        'reads the exact crash-owner identity')
    match_result = 0
    if (len_trim(owner_start) > 0) then
        match_result = c_process_matches(int(scope_owner_pid, c_int), &
            trim(owner_start)//c_null_char)
    end if
    call check(match_result /= 0, 'owner start identity matches before crash signal')
    ignored = -1
    if (match_result /= 0) ignored = c_kill(int(scope_owner_pid, c_int), 9_c_int)
    call check(ignored == 0, 'abruptly kills only the exact scoped supervisor')
    done = .false.
    do ignored = 1, 500
        call process_poll_pid(scope_owner_pid, done, exitcode)
        if (done) exit
        call fs_sleep_ms(10)
    end do
    if (.not. done) then
        call process_cancel_pid(scope_owner_pid, cleanup_error)
        do cleanup_attempt = 1, 500
            call process_poll_pid(scope_owner_pid, done, exitcode)
            if (done) exit
            call fs_sleep_ms(10)
        end do
    end if
    scope_owner_reaped = done
    call check(done, 'reaps the abruptly killed supervisor process')
    call fs_sleep_ms(100)
    call check(line_count(trim(prefix)//'.scope-escaped.heartbeat') > &
        scope_escaped_lines, 'forked descendant outlives abrupt supervisor death')
    match_result = c_process_matches(int(scope_owner_pid, c_int), &
        trim(owner_start)//c_null_char)
    call check(match_result == 0, 'crash owner exact PID/start identity is gone')
    owner_state_dir = ''
    call read_text_file(trim(prefix)//'.scope-state-dir', owner_state_dir)
    acquire_error = 1
    if (scope_owner_reaped .and. len_trim(owner_state_dir) > 0) then
        do cleanup_attempt = 1, 3
            call gremlin_session_acquire(trim(workdir), trim(lane_id), &
                recovered_session, acquire_error, state_message)
            if (acquire_error == 0) then
                recovered_owner = recovered_session%owner
                exit
            end if
            cleanup_error = c_recover_scope(trim(owner_state_dir)//c_null_char, &
                int(scope_owner_pid, c_int), trim(owner_start)//c_null_char)
            if (cleanup_error /= 0) exit
        end do
    end if
    call check(acquire_error == 0 .and. recovered_owner, &
        'stale acquisition completes scoped descendant cleanup')
    scope_child_quiet = wait_for_quiet(trim(prefix)//'.scope-child.heartbeat')
    call check(scope_child_quiet, &
        'stale cleanup stops the exact child session heartbeat')
    scope_escaped_quiet = wait_for_quiet(trim(prefix)//'.scope-escaped.heartbeat')
    call check(scope_escaped_quiet, &
        'stale cleanup stops the exact forked descendant heartbeat')
    call fs_sleep_ms(100)
    call check(line_count(trim(scratch)//'/recovery-sentinel.sentinel.heartbeat') > &
        scope_sentinel_lines, 'unrelated sentinel survives stale-owner recovery')
    if (recovered_owner) then
        call gremlin_session_release(recovered_session, release_error, state_message)
        call check(release_error == 0, 'releases the recovered lane owner')
    end if
    call process_cancel_pid(scope_sentinel_pid, exitcode)
    call check(exitcode == 0, 'stops recovery sentinel by its own exact owner')
    scope_sentinel_quiet = wait_for_quiet( &
        trim(scratch)//'/recovery-sentinel.sentinel.heartbeat')
    call check(scope_sentinel_quiet, 'recovery sentinel stops after exact cancellation')

    call cleanup_crash_owner(scope_owner_pid, owner_start, scope_owner_reaped)
    if (len_trim(owner_start) > 0) then
        call check(c_process_matches(int(scope_owner_pid, c_int), &
            trim(owner_start)//c_null_char) == 0, &
            'confirms exact crash owner identity is absent before scratch cleanup')
    else
        call check(scope_owner_reaped, &
            'confirms exact launched crash owner was reaped before scratch cleanup')
    end if
    scope_escaped_quiet = wait_for_quiet(trim(prefix)//'.scope-escaped.heartbeat')
    call check(scope_escaped_quiet, &
        'confirms descendant heartbeat stays stopped before scratch cleanup')
    scratch_clean = failed == 0 .and. scope_owner_reaped .and. &
        acquire_error == 0 .and. &
        scope_child_quiet .and. scope_escaped_quiet .and. scope_sentinel_quiet
    if (scratch_clean) then
        call fs_remove_tree(trim(scratch))
    else
        write (error_unit, '(a,a)') 'Retaining process-boundary evidence at ', &
            trim(scratch)
    end if
    write (output_unit, '(a,i0,a,i0,a)') 'Summary: ', passed, &
        ' passed, ', failed, ' failed'
    if (failed > 0) stop 1

contains

    subroutine check(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) then
            passed = passed + 1
        else
            failed = failed + 1
            write (error_unit, '(a,a)') 'FAIL: ', description
        end if
    end subroutine check

    subroutine start_helper(helper_mode, target, child_pid)
        character(len=*), intent(in) :: helper_mode, target
        integer, intent(out) :: child_pid

        integer :: count, start_error
        character(len=:), allocatable :: args

        args = ''
        count = 0
        call argv_push(args, count, trim(executable))
        call argv_push(args, count, helper_mode)
        call argv_push(args, count, target)
        call process_start_argv_logged(trim(workdir), args, count, &
            target//'.launch.log', child_pid, start_error)
        call check(start_error == 0 .and. child_pid > 0, &
            'launches helper mode '//helper_mode)
    end subroutine start_helper

    subroutine start_scope_owner(project_dir, owner_lane, target, child_pid)
        character(len=*), intent(in) :: project_dir, owner_lane, target
        integer, intent(out) :: child_pid

        integer :: count, start_error
        character(len=:), allocatable :: args

        args = ''
        count = 0
        call argv_push(args, count, trim(executable))
        call argv_push(args, count, '--scope-owner')
        call argv_push(args, count, trim(project_dir))
        call argv_push(args, count, trim(owner_lane))
        call argv_push(args, count, trim(target))
        call process_start_argv_logged(trim(workdir), args, count, &
            target//'.scope-owner.log', child_pid, start_error)
        call check(start_error == 0 .and. child_pid > 0, &
            'launches crashable Gremlin state owner')
    end subroutine start_scope_owner

    subroutine run_scope_owner(project_dir, owner_lane, target)
        character(len=*), intent(in) :: project_dir, owner_lane, target

        type(gremlin_session_t) :: session
        integer :: ierr, child_pid, child_exit
        logical :: child_done
        character(len=256) :: message

        call gremlin_session_acquire(project_dir, owner_lane, session, ierr, message)
        if (ierr /= 0 .or. .not. session%owner) call process_exit(11)
        call write_text_file(trim(target)//'.scope-state-dir', &
            trim(session%state_dir))
        call write_text_file(trim(target)//'.scope-owner-start', &
            trim(session%owner_start))
        call process_set_async_scope(session%state_dir, session%owner_pid, &
            session%owner_start, ierr)
        if (ierr /= 0) call process_exit(12)
        call start_helper('--scope-child', target, child_pid)
        if (child_pid <= 0) call process_exit(13)
        do
            call process_poll_pid(child_pid, child_done, child_exit)
            if (child_done) call process_exit(14)
            call fs_sleep_ms(50)
        end do
    end subroutine run_scope_owner

    subroutine run_helper(helper_mode, target)
        character(len=*), intent(in) :: helper_mode, target

        integer(c_int) :: child, grandchild, setpgid_error, escaped_sid
        character(len=32) :: result_text

        previous_handler = c_signal(15_c_int, c_funloc(ignore_term))
        if (c_associated(previous_handler)) call process_exit(8)
        if (helper_mode == '--quick-exit') call process_exit(0)
        if (helper_mode == '--sentinel') then
            call write_pid(target, 'sentinel')
            call heartbeat_loop(target, 'sentinel')
        end if
        if (helper_mode == '--scope-child') then
            call write_pid(target, 'scope-child')
            child = c_fork()
            if (child < 0_c_int) call process_exit(15)
            if (child == 0_c_int) then
                escaped_sid = c_setsid()
                ! Linux rejects session escape; Darwin tracks escaped descendants.
                ! Keep the descendant alive so both platforms test stale cleanup.
                write (result_text, '(i0)') escaped_sid
                call write_text_file(target//'.scope-setsid', trim(result_text))
                previous_handler = c_signal(15_c_int, c_funloc(ignore_term))
                if (.not. c_associated(previous_handler)) call process_exit(17)
                call write_pid(target, 'scope-escaped')
                call heartbeat_loop(target, 'scope-escaped')
            end if
            call wait_for_child_file(target, 'scope-escaped')
            call heartbeat_loop(target, 'scope-child')
        end if
        call write_pid(target, 'owner')
        child = c_fork()
        if (child < 0_c_int) call process_exit(3)
        if (child == 0_c_int) then
            setpgid_error = c_setpgid(0_c_int, 0_c_int)
            if (setpgid_error /= 0_c_int) call process_exit(4)
            previous_handler = c_signal(15_c_int, c_funloc(ignore_term))
            if (.not. c_associated(previous_handler)) call process_exit(9)
            call write_pid(target, 'nested')
            grandchild = c_fork()
            if (grandchild < 0_c_int) call process_exit(5)
            if (grandchild == 0_c_int) then
                setpgid_error = c_setpgid(0_c_int, 0_c_int)
                if (setpgid_error /= 0_c_int) call process_exit(6)
                previous_handler = c_signal(15_c_int, c_funloc(ignore_term))
                if (.not. c_associated(previous_handler)) call process_exit(10)
                call write_pid(target, 'deep')
                call heartbeat_loop(target, 'deep')
            end if
            call wait_for_child_file(target, 'deep')
            if (helper_mode == '--orphan') then
                call write_heartbeat_once(target, 'nested')
                call process_exit(0)
            end if
            call heartbeat_loop(target, 'nested')
        end if
        call wait_for_child_file(target, 'nested')
        call wait_for_child_file(target, 'deep')
        if (helper_mode == '--orphan') call process_exit(0)
        call heartbeat_loop(target, 'owner')
    end subroutine run_helper

    subroutine heartbeat_loop(target, role)
        character(len=*), intent(in) :: target, role

        integer :: unit, io_status
        character(len=4096) :: path

        path = trim(target)//'.'//trim(role)//'.heartbeat'
        do
            open(newunit=unit, file=trim(path), status='unknown', &
                position='append', action='write', iostat=io_status)
            if (io_status == 0) then
                write (unit, '(a)', iostat=io_status) 'alive'
                close(unit)
            end if
            call fs_sleep_ms(20)
        end do
    end subroutine heartbeat_loop

    subroutine write_heartbeat_once(target, role)
        character(len=*), intent(in) :: target, role

        integer :: unit
        character(len=4096) :: path

        path = trim(target)//'.'//trim(role)//'.heartbeat'
        open(newunit=unit, file=trim(path), status='unknown', &
            position='append', action='write')
        write (unit, '(a)') 'alive'
        close(unit)
    end subroutine write_heartbeat_once

    subroutine write_pid(target, role)
        character(len=*), intent(in) :: target, role

        integer :: unit, current_pid
        character(len=4096) :: path

        current_pid = process_getpid()
        path = trim(target)//'.'//trim(role)//'.pid'
        open(newunit=unit, file=trim(path), status='replace', action='write')
        write (unit, '(i0)') current_pid
        close(unit)
    end subroutine write_pid

    subroutine write_text_file(path, value)
        character(len=*), intent(in) :: path, value
        integer :: unit, io_status

        open(newunit=unit, file=trim(path), status='replace', &
            action='write', iostat=io_status)
        if (io_status /= 0) call process_exit(18)
        write (unit, '(a)', iostat=io_status) trim(value)
        close(unit)
        if (io_status /= 0) call process_exit(19)
    end subroutine write_text_file

    subroutine read_text_file(path, value)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: value
        integer :: unit, io_status

        value = ''
        open(newunit=unit, file=trim(path), status='old', &
            action='read', iostat=io_status)
        if (io_status /= 0) return
        read (unit, '(a)', iostat=io_status) value
        close(unit)
        if (io_status /= 0) value = ''
    end subroutine read_text_file

    subroutine cleanup_crash_owner(child_pid, start_text, was_reaped)
        integer, intent(in) :: child_pid
        character(len=*), intent(in) :: start_text
        logical, intent(inout) :: was_reaped

        integer :: attempt, cancel_error, poll_exit
        logical :: child_done

        if (child_pid <= 0 .or. was_reaped) return
        call process_cancel_pid(child_pid, cancel_error)
        child_done = .false.
        do attempt = 1, 500
            call process_poll_pid(child_pid, child_done, poll_exit)
            if (child_done) exit
            call fs_sleep_ms(10)
        end do
        was_reaped = child_done
        if (was_reaped .and. len_trim(start_text) > 0) then
            was_reaped = c_process_matches(int(child_pid, c_int), &
                trim(start_text)//c_null_char) == 0
        end if
        if (.not. was_reaped) then
            write (error_unit, '(a,i0,a,i0)') &
                'Could not confirm crash owner cleanup; pid=', child_pid, &
                ' cancel status=', cancel_error
        end if
    end subroutine cleanup_crash_owner

    subroutine wait_for_child_file(target, role)
        character(len=*), intent(in) :: target, role

        integer :: attempt
        logical :: exists
        character(len=4096) :: path

        path = trim(target)//'.'//trim(role)//'.pid'
        do attempt = 1, 500
            inquire(file=trim(path), exist=exists)
            if (exists) return
            call fs_sleep_ms(10)
        end do
        call process_exit(7)
    end subroutine wait_for_child_file

    logical function wait_for_pid(path, found_pid)
        character(len=*), intent(in) :: path
        integer, intent(out) :: found_pid

        integer :: attempt
        found_pid = 0
        do attempt = 1, 500
            found_pid = pid_from_file(path)
            if (found_pid > 0) then
                wait_for_pid = .true.
                return
            end if
            call fs_sleep_ms(10)
        end do
        wait_for_pid = .false.
    end function wait_for_pid

    integer function pid_from_file(path)
        character(len=*), intent(in) :: path

        integer :: unit, io_status
        pid_from_file = 0
        open(newunit=unit, file=trim(path), status='old', action='read', &
            iostat=io_status)
        if (io_status /= 0) return
        read (unit, *, iostat=io_status) pid_from_file
        close(unit)
        if (io_status /= 0) pid_from_file = 0
    end function pid_from_file

    logical function wait_for_data(path)
        character(len=*), intent(in) :: path

        integer :: attempt, bytes
        logical :: exists
        wait_for_data = .false.
        do attempt = 1, 500
            inquire(file=trim(path), exist=exists)
            if (exists) then
                inquire(file=trim(path), size=bytes)
                if (bytes > 0) then
                    wait_for_data = .true.
                    return
                end if
            end if
            call fs_sleep_ms(10)
        end do
    end function wait_for_data

    integer function line_count(path)
        character(len=*), intent(in) :: path

        integer :: unit, io_status
        character(len=16) :: line
        line_count = 0
        open(newunit=unit, file=trim(path), status='old', action='read', &
            iostat=io_status)
        if (io_status /= 0) return
        do
            read (unit, '(a)', iostat=io_status) line
            if (io_status /= 0) exit
            line_count = line_count + 1
        end do
        close(unit)
    end function line_count

    logical function wait_for_quiet(path)
        character(len=*), intent(in) :: path
        integer :: before, after

        before = line_count(path)
        wait_for_quiet = .false.
        if (before <= 0) return
        call fs_sleep_ms(250)
        after = line_count(path)
        wait_for_quiet = after == before
    end function wait_for_quiet

end program test_async_process_boundary

subroutine ignore_term(signal_number) bind(C)
    use, intrinsic :: iso_c_binding, only: c_int
    integer(c_int), value :: signal_number
    if (signal_number == 0_c_int) return
end subroutine ignore_term
