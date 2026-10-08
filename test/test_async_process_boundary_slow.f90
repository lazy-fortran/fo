program test_async_process_boundary_slow
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char, c_int64_t, &
        c_funptr, c_funloc, c_associated, c_null_funptr
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, int64, real64
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_sleep_ms
    use fo_process, only: argv_push, process_cancel_pid, process_getcwd, &
        process_getpid, process_poll_pid, process_start_argv_logged, process_exit, &
        process_set_async_scope
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_read, gremlin_session_release
    use fo_test_harness, only: string_list_t, process_result_t, list_add, run_process, &
        spawn_process, poll_process, make_scratch
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

        integer(c_int64_t) function c_test_start(pid) &
                bind(C, name='fo_test_process_start_time')
            import :: c_int, c_int64_t
            integer(c_int), value :: pid
        end function c_test_start

        integer(c_int) function c_test_signal(pid, start, number) &
                bind(C, name='fo_test_signal_identity')
            import :: c_int, c_int64_t
            integer(c_int), value :: pid, number
            integer(c_int64_t), value :: start
        end function c_test_signal

        integer(c_int) function c_private_mode(path, directory) &
                bind(C, name='fo_test_private_mode')
            import :: c_char, c_int
            character(c_char), intent(in) :: path(*)
            integer(c_int), value :: directory
        end function c_private_mode

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
    character(len=:), allocatable :: scratch
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
        trim(mode) == '--quick-exit' .or. &
        trim(mode) == '--scope-child-unpolled') then
        call get_command_argument(2, prefix)
        call run_helper(trim(mode), trim(prefix))
        call process_exit(2)
    end if
    if (trim(mode) == '--scope-owner' .or. &
        trim(mode) == '--scope-owner-unpolled') then
        call get_command_argument(2, project)
        call get_command_argument(3, lane_id)
        call get_command_argument(4, prefix)
        call run_scope_owner(trim(project), trim(lane_id), trim(prefix), &
            trim(mode) == '--scope-owner-unpolled')
        call process_exit(2)
    end if
    if (trim(mode) == '--reuse-session') then
        call get_command_argument(2, prefix)
        call run_reuse_fixture(trim(prefix))
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
    call make_scratch('fo-async-boundary', scratch)

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
    call check(cancel_seconds >= 1.5_real64 .and. cancel_seconds < 3.5_real64, &
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
    call process_poll_pid(orphan_pid, done, exitcode)
    call check(.not. done .and. exitcode == 0, &
        'captures exact descendant identities while the orphan leader is alive')
    call write_text_file(trim(scratch)//'/orphan.release', 'exit')
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
        call check(setsid_result == scope_escaped_pid .and. setsid_result > 0, &
            'Linux descendant successfully escapes with setsid')
        call check(c_getsid(int(scope_escaped_pid, c_int)) == &
            int(scope_escaped_pid, c_int), 'Linux descendant owns its new session')
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

    call exercise_stale_session(trim(scratch)//'/reuse', darwin)
    call exercise_unpolled_recovery(trim(scratch)//'/unpolled')

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

    subroutine exercise_unpolled_recovery(target)
        character(len=*), intent(in) :: target
        type(string_list_t) :: command
        integer :: helper, wrapper, escaped, sentinel, rc, attempt
        integer :: sentinel_before, escaped_before, escaped_after, wrapper_after
        integer(c_int64_t) :: owner_birth, wrapper_birth, escaped_birth
        character(len=4096) :: state_dir
        character(len=128) :: lane, start_text

        helper = 0
        wrapper = 0
        escaped = 0
        write (lane, '("boundary-unpolled-",i0)') process_getpid()
        call start_helper('--sentinel', target//'-sentinel', sentinel)
        call check(wait_for_data(target//'-sentinel.sentinel.heartbeat'), &
            'unpolled recovery starts an unrelated sentinel')
        call list_add(command, trim(executable))
        call list_add(command, '--scope-owner-unpolled')
        call list_add(command, trim(workdir))
        call list_add(command, trim(lane))
        call list_add(command, target)
        ! The independent harness never invokes Fo's async descendant polling.
        call spawn_process(command, trim(workdir), helper)
        call check(helper > 0, 'launches exact unpolled scope owner')
        owner_birth = c_test_start(int(helper, c_int))
        call check(wait_for_pid(target//'.unpolled-wrapper.pid', wrapper), &
            'registered wrapper exists before its scope owner exits')
        call check(wait_for_pid(target//'.unpolled-escaped.pid', escaped), &
            'raw-forked descendant exists before its scope owner exits')
        call check(wait_for_data(target//'.unpolled-escaped.heartbeat'), &
            'unpolled escaped descendant starts its heartbeat')
        call read_birth_file(target//'.unpolled-wrapper.birth', wrapper_birth)
        call read_birth_file(target//'.unpolled-escaped.birth', escaped_birth)
        call check(wrapper_birth > 0_c_int64_t .and. escaped_birth > 0_c_int64_t, &
            'records independent wrapper and escaped descendant birth identities')
        call check(c_getsid(int(escaped, c_int)) == int(escaped, c_int), &
            'unpolled raw descendant owns a separate session')
        call check(wait_for_data(target//'.scope-state-dir'), &
            'unpolled owner records its exact scope directory')
        call check(wait_for_data(target//'.scope-owner-start'), &
            'unpolled owner records its exact birth identity')
        call read_text_file(target//'.scope-state-dir', state_dir)
        call read_text_file(target//'.scope-owner-start', start_text)
        call write_text_file(target//'.unpolled-owner.release', 'exit')
        rc = 999
        do attempt = 1, 500
            call poll_process(helper, rc)
            if (rc /= 999) exit
            call fs_sleep_ms(10)
        end do
        call check(rc == 0, 'scope owner exits without async descendant polling')
        call check(c_process_matches(int(helper, c_int), &
            trim(start_text)//c_null_char) == 0, &
            'exact unpolled scope owner is absent before recovery')
        escaped_before = line_count(target//'.unpolled-escaped.heartbeat')
        call fs_sleep_ms(120)
        call check(line_count(target//'.unpolled-escaped.heartbeat') > escaped_before, &
            'unrecorded escaped descendant outlives its scope owner')
        sentinel_before = line_count(target//'-sentinel.sentinel.heartbeat')
        rc = c_recover_scope(trim(state_dir)//c_null_char, int(helper, c_int), &
            trim(start_text)//c_null_char)
        call check(rc == 0, 'recovers a scope before normal descendant capture')
        escaped_after = line_count(target//'.unpolled-escaped.heartbeat')
        wrapper_after = line_count(target//'.unpolled-wrapper.heartbeat')
        call fs_sleep_ms(150)
        call check(line_count(target//'.unpolled-escaped.heartbeat') == escaped_after, &
            'recovery returns only after the TERM-ignoring escaped heartbeat stops')
        call check(line_count(target//'.unpolled-wrapper.heartbeat') == wrapper_after, &
            'recovery stops the registered TERM-sensitive wrapper heartbeat')
        call check(line_count(target//'-sentinel.sentinel.heartbeat') > &
            sentinel_before, 'unrelated sentinel survives unpolled scope recovery')

        ! Baseline failure must leave no payload running after its oracle is read.
        call cleanup_exact_fixture(escaped, escaped_birth)
        call cleanup_exact_fixture(wrapper, wrapper_birth)
        call cleanup_exact_fixture(helper, owner_birth)
        call process_cancel_pid(sentinel, rc)
        call check(rc == 0, 'cancels only the unpolled recovery sentinel')
        call check(wait_for_quiet(target//'.unpolled-escaped.heartbeat'), &
            'exact cleanup stops the unpolled fixture descendant')
        call check(wait_for_quiet(target//'-sentinel.sentinel.heartbeat'), &
            'exact cleanup stops the unrelated fixture sentinel')
    end subroutine exercise_unpolled_recovery

    function birth_text(identity, macos) result(text)
        integer(c_int64_t), intent(in) :: identity
        logical, intent(in) :: macos
        character(len=64) :: text

        if (macos) then
            write (text, '(i0,".",i6.6)') identity/1000000_c_int64_t, &
                mod(identity, 1000000_c_int64_t)
        else
            write (text, '(i0)') identity
        end if
    end function birth_text

    subroutine run_reuse_fixture(target)
        character(len=*), intent(in) :: target
        integer(c_int) :: child, descendant, sid
        integer(c_int64_t) :: identity
        character(len=64) :: text

        previous_handler = c_signal(15_c_int, c_funloc(ignore_term))
        child = c_fork()
        if (child < 0_c_int) call process_exit(20)
        if (child /= 0_c_int) then
            call wait_for_child_file(target, 'reuse-owner')
            call process_exit(0)
        end if
        sid = c_setsid()
        if (sid <= 0_c_int) call process_exit(21)
        call write_pid(target, 'reuse-owner')
        identity = c_test_start(int(process_getpid(), c_int))
        write (text, '(i0)') identity
        call write_text_file(target//'.reuse-owner.birth', text)
        descendant = c_fork()
        if (descendant < 0_c_int) call process_exit(22)
        if (descendant == 0_c_int) then
            call write_pid(target, 'reuse-sentinel')
            write (text, '(i0)') c_test_start(int(process_getpid(), c_int))
            call write_text_file(target//'.reuse-sentinel.birth', text)
            call heartbeat_loop(target, 'reuse-sentinel')
        end if
        descendant = c_fork()
        if (descendant < 0_c_int) call process_exit(23)
        if (descendant == 0_c_int) then
            sid = c_setsid()
            if (sid <= 0_c_int) call process_exit(24)
            call write_pid(target, 'reuse-detached')
            write (text, '(i0)') c_test_start(int(process_getpid(), c_int))
            call write_text_file(target//'.reuse-detached.birth', text)
            call heartbeat_loop(target, 'reuse-detached')
        end if
        call wait_for_child_file(target, 'reuse-sentinel')
        call wait_for_child_file(target, 'reuse-detached')
        if (.not. wait_for_data(target//'.reuse-release')) call process_exit(25)
        call process_exit(0)
    end subroutine run_reuse_fixture

    subroutine read_birth_file(path, identity)
        character(len=*), intent(in) :: path
        integer(c_int64_t), intent(out) :: identity
        character(len=64) :: text
        integer :: io_status

        call read_text_file(path, text)
        identity = 0_c_int64_t
        read (text, *, iostat=io_status) identity
        if (io_status /= 0) identity = 0_c_int64_t
    end subroutine read_birth_file

    subroutine write_reuse_records(state_dir, root_pid, fake_start, member_pid, &
            member_start, member_session, parent_start)
        character(len=*), intent(in) :: state_dir, fake_start, member_start
        character(len=*), intent(in) :: parent_start
        integer, intent(in) :: root_pid, member_pid, member_session
        character(len=4096) :: registry, path
        character(len=32) :: root_text, member_text
        integer :: unit, rc

        write (root_text, '(i0)') root_pid
        write (member_text, '(i0)') member_pid
        registry = trim(state_dir)//'/async-processes/'//trim(root_text)// &
            '-'//trim(fake_start)
        call fs_make_dir(trim(registry))
        rc = c_private_mode(trim(state_dir)//c_null_char, 1_c_int)
        call check(rc == 0, 'uses a private stale-record state directory')
        rc = c_private_mode(trim(state_dir)//'/async-processes'//c_null_char, 1_c_int)
        call check(rc == 0, 'uses a private stale-record registry parent')
        rc = c_private_mode(trim(registry)//c_null_char, 1_c_int)
        call check(rc == 0, 'uses a private stale-record owner registry')
        path = trim(registry)//'/'//trim(root_text)//'-'//trim(fake_start)//'.session'
        open(newunit=unit, file=trim(path), status='replace', action='write')
        write (unit, '(i0)') root_pid
        write (unit, '(a)') trim(fake_start)
        write (unit, '(i0)') root_pid
        write (unit, '(a)') '1'
        write (unit, '(a)') trim(fake_start)
        close(unit)
        rc = c_private_mode(trim(path)//c_null_char, 0_c_int)
        call check(rc == 0, 'uses a private stale session record')
        if (member_pid <= 0) return
        path = trim(registry)//'/'//trim(root_text)//'-'//trim(fake_start)// &
            '--member-'//trim(member_text)//'-'//trim(member_start)//'.member'
        open(newunit=unit, file=trim(path), status='replace', action='write')
        write (unit, '(i0)') root_pid
        write (unit, '(a)') trim(fake_start)
        write (unit, '(i0)') member_pid
        write (unit, '(a)') trim(member_start)
        write (unit, '(i0)') member_session
        write (unit, '(i0)') root_pid
        write (unit, '(a)') trim(parent_start)
        write (unit, '(a)') trim(fake_start)
        close(unit)
        rc = c_private_mode(trim(path)//c_null_char, 0_c_int)
        call check(rc == 0, 'uses a private stale member record')
    end subroutine write_reuse_records

    subroutine cleanup_exact_fixture(child_pid, identity)
        integer, intent(in) :: child_pid
        integer(c_int64_t), intent(in) :: identity
        integer :: rc

        if (identity <= 0_c_int64_t) return
        if (c_test_start(int(child_pid, c_int)) /= identity) return
        rc = c_test_signal(int(child_pid, c_int), identity, 15_c_int)
        call fs_sleep_ms(100)
        if (c_test_start(int(child_pid, c_int)) == identity) &
            rc = c_test_signal(int(child_pid, c_int), identity, 9_c_int)
        call poll_process(child_pid, rc)
    end subroutine cleanup_exact_fixture

    subroutine exercise_stale_session(target, macos)
        character(len=*), intent(in) :: target
        logical, intent(in) :: macos
        type(string_list_t) :: command
        integer :: helper_pid, root_pid, sentinel, detached, rc, attempt, before
        integer(c_int64_t) :: root_birth, sentinel_birth, detached_birth
        character(len=4096) :: state_dir
        character(len=64) :: fake_start, real_start, parent_start, wrong_start

        root_pid = 0
        sentinel = 0
        detached = 0
        call list_add(command, trim(executable))
        call list_add(command, '--reuse-session')
        call list_add(command, target)
        call spawn_process(command, trim(workdir), helper_pid)
        call check(helper_pid > 0, 'starts independent unfiltered session fixture')
        call check(wait_for_pid(target//'.reuse-owner.pid', root_pid), &
            'records the unrelated session leader')
        call check(wait_for_pid(target//'.reuse-sentinel.pid', sentinel), &
            'records an unrelated same-session sentinel')
        call check(wait_for_pid(target//'.reuse-detached.pid', detached), &
            'records an independently detached fixture member')
        call check(wait_for_data(target//'.reuse-sentinel.heartbeat'), &
            'unrelated same-session sentinel is active')
        call check(wait_for_data(target//'.reuse-detached.heartbeat'), &
            'independently detached member is active')
        call read_birth_file(target//'.reuse-owner.birth', root_birth)
        call read_birth_file(target//'.reuse-sentinel.birth', sentinel_birth)
        call read_birth_file(target//'.reuse-detached.birth', detached_birth)
        call check(root_birth > 0_c_int64_t .and. sentinel_birth > 0_c_int64_t &
            .and. detached_birth > 0_c_int64_t, 'reads independent OS birth identities')
        call check(c_getsid(int(sentinel, c_int)) == int(root_pid, c_int), &
            'unrelated sentinel belongs to the session represented by the stale SID')
        call check(c_getsid(int(detached, c_int)) == int(detached, c_int), &
            'registered-member fixture really escaped into another session')
        call write_text_file(target//'.reuse-release', 'exit')
        do attempt = 1, 500
            call poll_process(helper_pid, rc)
            if (rc /= 999) exit
            call fs_sleep_ms(10)
        end do
        call check(rc == 0, 'reaps the independent fixture launcher')
        do attempt = 1, 500
            if (c_test_start(int(root_pid, c_int)) == 0_c_int64_t) exit
            if (c_test_start(int(root_pid, c_int)) /= root_birth) exit
            call poll_process(root_pid, rc)
            call fs_sleep_ms(10)
        end do
        call check(c_test_start(int(root_pid, c_int)) == 0_c_int64_t, &
            'stale SID fixture leader is absent before recovery')
        call check(c_getsid(int(sentinel, c_int)) == int(root_pid, c_int), &
            'unrelated session survives after its leader disappears')

        state_dir = target//'.state'
        fake_start = birth_text(1_c_int64_t, macos)
        parent_start = birth_text(root_birth, macos)
        before = line_count(target//'.reuse-sentinel.heartbeat')
        call write_reuse_records(trim(state_dir), root_pid, trim(fake_start), &
            0, '', 0, '')
        rc = c_recover_scope(trim(state_dir)//c_null_char, int(root_pid, c_int), &
            trim(fake_start)//c_null_char)
        call check(rc == 0, 'recovery retires the absent stale leader record')
        call fs_sleep_ms(100)
        call check(line_count(target//'.reuse-sentinel.heartbeat') > before, &
            'numeric stale SID alone never kills an unrelated live session')
        call check(c_test_start(int(sentinel, c_int)) == sentinel_birth, &
            'unrelated sentinel keeps its exact birth after stale SID recovery')

        wrong_start = birth_text(detached_birth + 1_c_int64_t, macos)
        before = line_count(target//'.reuse-detached.heartbeat')
        call write_reuse_records(trim(state_dir), root_pid, trim(fake_start), &
            detached, trim(wrong_start), detached, trim(fake_start))
        rc = c_recover_scope(trim(state_dir)//c_null_char, int(root_pid, c_int), &
            trim(fake_start)//c_null_char)
        call check(rc == 0, 'recovery retires a member with a mismatched birth')
        call fs_sleep_ms(100)
        call check(line_count(target//'.reuse-detached.heartbeat') > before, &
            'a mismatched member birth never establishes ownership')

        real_start = birth_text(detached_birth, macos)
        before = line_count(target//'.reuse-sentinel.heartbeat')
        call write_reuse_records(trim(state_dir), root_pid, trim(parent_start), &
            detached, trim(real_start), detached, trim(parent_start))
        rc = c_recover_scope(trim(state_dir)//c_null_char, int(root_pid, c_int), &
            trim(parent_start)//c_null_char)
        call check(rc == 0, &
            'recovers an exact registered detached member after leader exit')
        call check(wait_for_quiet(target//'.reuse-detached.heartbeat'), &
            'exact registered detached member stops after leader exit')
        call check(line_count(target//'.reuse-sentinel.heartbeat') > before, &
            'exact detached-member cleanup preserves the unrelated same-SID sentinel')

        real_start = birth_text(sentinel_birth, macos)
        call write_reuse_records(trim(state_dir), root_pid, trim(parent_start), &
            sentinel, trim(real_start), root_pid, trim(parent_start))
        rc = c_recover_scope(trim(state_dir)//c_null_char, int(root_pid, c_int), &
            trim(parent_start)//c_null_char)
        call check(rc == 0, &
            'stops the independent sentinel only by its registered birth')
        call check(wait_for_quiet(target//'.reuse-sentinel.heartbeat'), &
            'independent sentinel cleanup is complete')
        call cleanup_exact_fixture(detached, detached_birth)
        call cleanup_exact_fixture(sentinel, sentinel_birth)
        call cleanup_exact_fixture(root_pid, root_birth)
    end subroutine exercise_stale_session

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

    subroutine run_unpolled_wrapper(target)
        character(len=*), intent(in) :: target
        integer(c_int) :: child, sid
        character(len=64) :: text

        previous_handler = c_signal(15_c_int, c_null_funptr)
        call write_pid(target, 'unpolled-wrapper')
        write (text, '(i0)') c_test_start(int(process_getpid(), c_int))
        call write_text_file(target//'.unpolled-wrapper.birth', text)
        if (.not. wait_for_data(target//'.unpolled-fork.release')) &
            call process_exit(26)
        child = c_fork()
        if (child < 0_c_int) call process_exit(27)
        if (child == 0_c_int) then
            sid = c_setsid()
            if (sid <= 0_c_int) call process_exit(28)
            previous_handler = c_signal(15_c_int, c_funloc(ignore_term))
            if (c_associated(previous_handler)) call process_exit(29)
            call write_pid(target, 'unpolled-escaped')
            write (text, '(i0)') c_test_start(int(process_getpid(), c_int))
            call write_text_file(target//'.unpolled-escaped.birth', text)
            call heartbeat_loop(target, 'unpolled-escaped')
        end if
        call heartbeat_loop(target, 'unpolled-wrapper')
    end subroutine run_unpolled_wrapper

    subroutine run_scope_owner(project_dir, owner_lane, target, unpolled)
        character(len=*), intent(in) :: project_dir, owner_lane, target
        logical, intent(in) :: unpolled

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
        if (unpolled) then
            call start_helper('--scope-child-unpolled', target, child_pid)
            if (child_pid <= 0) call process_exit(13)
            ! Release raw fork only after async startup has registered the wrapper.
            call write_text_file(target//'.unpolled-fork.release', 'fork')
            if (.not. wait_for_data(target//'.unpolled-escaped.heartbeat')) &
                call process_exit(15)
            if (.not. wait_for_data(target//'.unpolled-owner.release')) &
                call process_exit(16)
            call process_exit(0)
        end if
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

        if (helper_mode == '--scope-child-unpolled') then
            call run_unpolled_wrapper(target)
            call process_exit(2)
        end if
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
                ! Keep the separate-session descendant alive so both platforms
                ! test stale cleanup through process identity and parent lineage.
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
        if (helper_mode == '--orphan') then
            if (.not. wait_for_data(target//'.release')) call process_exit(26)
            call process_exit(0)
        end if
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

end program test_async_process_boundary_slow

subroutine ignore_term(signal_number) bind(C)
    use, intrinsic :: iso_c_binding, only: c_int
    integer(c_int), value :: signal_number
    if (signal_number == 0_c_int) return
end subroutine ignore_term
