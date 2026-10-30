program test_sync_owner_lifetime
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t
    use fo_test_harness, only: string_list_t, list_add, make_scratch, write_text
    use fo_test_harness, only: read_text, file_exists, sleep_ms, spawn_process
    use fo_test_harness, only: poll_process, signal_process_group, start_sentinel
    use fo_test_harness, only: stop_sentinel, process_alive, assert_true
    use fo_test_harness, only: finish_assertions, remove_tree, terminate_process_group
    use fo_test_cli, only: resolve_driver
    use fo_test_process_identity, only: mcp_process_start_time
    use fo_test_process_identity, only: mcp_process_identity_running
    use fo_test_process_identity, only: mcp_kill_owned_tree
    use fo_fs, only: fs_is_windows
    implicit none

    character(:), allocatable :: driver, scratch, project, marker, text
    character(len=1), parameter :: nl = new_line('a')
    type(string_list_t) :: args, env
    integer :: number, owner, child, monitor, sentinel, status, attempt, ignored
    integer(c_int64_t) :: child_birth, monitor_birth
    logical :: seen, stopped, monitor_stopped, sentinel_alive, reaped

    if (fs_is_windows()) then
        print '(a)', 'SKIP: POSIX owner signals; Windows uses Job Objects'
        stop
    end if
    call resolve_driver(driver)
    call make_scratch('fo-sync-owner', scratch)
    project = scratch//'/project'
    call write_text(project//'/fpm.toml', 'name = "sync_owner"'//nl)
    call write_text(project//'/test/test_sleeper.f90', &
        'program test_sleeper'//nl// &
        'use iso_c_binding, only: c_int, c_funptr, c_funloc'//nl// &
        'implicit none'//nl// &
        'interface'//nl// &
        'integer(c_int) function pid() bind(C,name="getpid")'//nl// &
        'import c_int'//nl//'end function'//nl// &
        'integer(c_int) function parent() bind(C,name="getppid")'//nl// &
        'import c_int'//nl//'end function'//nl// &
        'function handle(n,callback) bind(C,name="signal") result(previous)'//nl// &
        'import c_int,c_funptr'//nl//'integer(c_int),value :: n'//nl// &
        'type(c_funptr),value :: callback'//nl// &
        'type(c_funptr) :: previous'//nl//'end function'//nl// &
        'integer(c_int) function nap(seconds) bind(C,name="sleep")'//nl// &
        'import c_int'//nl//'integer(c_int),value :: seconds'//nl// &
        'end function'//nl//'end interface'//nl// &
        'integer :: unit, result'//nl//'type(c_funptr) :: previous'//nl// &
        'character(4096) :: marker'//nl// &
        'integer(c_int),volatile :: term_seen=0'//nl// &
        'integer :: graceful'//nl// &
        'previous=handle(15_c_int,c_funloc(ignore_term))'//nl// &
        'call get_environment_variable("FO_TEST_SYNC_PID",marker)'//nl// &
        'open(newunit=unit,file=trim(marker),status="replace")'//nl// &
        'write(unit,*) pid(),parent()'//nl//'close(unit)'//nl// &
        'call get_environment_variable("FO_TEST_SYNC_FAST",status=result)'//nl// &
        'if (result /= 0) then'//nl// &
        'call get_environment_variable("FO_TEST_SYNC_GRACEFUL",status=graceful)'//nl// &
        'do'//nl//'result=nap(30_c_int)'//nl// &
        'if (graceful == 0 .and. term_seen /= 0) then'//nl// &
        'open(newunit=unit,file=trim(marker)//".term",status="replace")'//nl// &
        'write(unit,*) "TERM handled"'//nl//'close(unit)'//nl//'exit'//nl// &
        'end if'//nl//'end do'//nl//'end if'//nl// &
        'contains'//nl//'subroutine ignore_term(n) bind(C)'//nl// &
        'integer(c_int),value :: n'//nl//'term_seen=1'//nl// &
        'end subroutine'//nl//'end program'//nl)
    call list_add(args, driver)
    call list_add(args, 'test')
    call list_add(args, '--rerun')
    call list_add(args, '--retries')
    call list_add(args, '0')
    call list_add(args, 'test_sleeper')
    do number = 1, 5
        if (number == 1) then
            marker = scratch//'/term.pid'
        else if (number == 2) then
            marker = scratch//'/kill.pid'
        else if (number == 3) then
            marker = scratch//'/timeout.pid'
            call write_text(project//'/fpm.toml', 'name = "sync_owner"'//nl// &
                '[extra.fo]'//nl//'test-timeout = 1'//nl// &
                'test-wall-timeout = 1'//nl)
        else if (number == 4) then
            marker = scratch//'/normal.pid'
        else
            marker = scratch//'/graceful.pid'
        end if
        env = string_list_t()
        call list_add(env, 'FO_TEST_SYNC_PID='//marker)
        if (number == 4) call list_add(env, 'FO_TEST_SYNC_FAST=1')
        if (number == 5) call list_add(env, 'FO_TEST_SYNC_GRACEFUL=1')
        call list_add(env, 'FO_JOBS=1')
        call list_add(env, 'FO_DISABLE_SELF_REFRESH=1')
        call list_add(env, 'FO_CACHE_DIR='//scratch//'/cache')
        call start_sentinel(sentinel)
        call spawn_process(args, project, owner, env)
        seen = .false.
        do attempt = 1, 1500
            if (file_exists(marker)) then
                text = read_text(marker)
                read (text, *, iostat=status) child, monitor
                if (status == 0) then
                    seen = .true.
                    exit
                end if
            end if
            call sleep_ms(20)
        end do
        if (.not. seen) then
            call terminate_process_group(owner, ignored, reaped)
            call stop_sentinel(sentinel)
            call assert_true(seen, 'native test reaches its PID barrier')
            call finish_assertions(retain_failed_scratch=.true.)
        end if
        child_birth = mcp_process_start_time(child)
        monitor_birth = mcp_process_start_time(monitor)
        if (number == 1) then
            call signal_process_group(owner, 15)
        else if (number == 2) then
            call signal_process_group(owner, 9)
        end if
        do attempt = 1, 300
            call poll_process(owner, status)
            if (status /= 999) exit
            call sleep_ms(20)
        end do
        do attempt = 1, 300
            if (.not. mcp_process_identity_running(child, child_birth) .and. &
                .not. mcp_process_identity_running(monitor, monitor_birth)) exit
            call sleep_ms(20)
        end do
        stopped = .not. mcp_process_identity_running(child, child_birth)
        monitor_stopped = .not. mcp_process_identity_running(monitor, monitor_birth)
        if (.not. stopped) call mcp_kill_owned_tree(child, child_birth, ignored)
        if (.not. monitor_stopped .and. monitor /= owner) &
            call mcp_kill_owned_tree(monitor, monitor_birth, ignored)
        if (status == 999) call terminate_process_group(owner, ignored, reaped)
        sentinel_alive = process_alive(sentinel)
        call stop_sentinel(sentinel)
        call assert_true(status /= 999, 'terminated Fo process is reaped')
        if (number == 3) call assert_true(status /= 0, 'named test hits its wall cap')
        if (number == 4) call assert_true(status == 0, 'ordinary named test passes')
        if (number == 5) call assert_true(file_exists(marker//'.term'), &
            'configured timeout lets the application handle TERM')
        call assert_true(stopped, 'native test exits after its Fo owner dies')
        call assert_true(monitor_stopped, 'command monitor exits with its command')
        call assert_true(sentinel_alive, 'unrelated neighbor survives owner cleanup')
    end do
    call remove_tree(scratch)
    call finish_assertions()
end program test_sync_owner_lifetime
