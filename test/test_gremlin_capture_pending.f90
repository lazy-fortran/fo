program test_gremlin_capture_pending
    use fo_util, only: temporary_root
    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_c_binding, only: c_int
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_remove_file, fs_find_executable, &
        fs_rename, fs_write_text
    use fo_fake_tool, only: fake_tool_dispatch, install_fake_tool
    use fo_test_harness, only: string_list_t, list_add, add_words, run_list, argv, &
        spawn_list, resolve_test_executable
    implicit none

    character(len=4096) :: root, fixture, state_directory
    character(len=4096) :: helper_mode, helper_label, helper_value, helper_fixture
    character(len=4096) :: driver_path, compiler_path, compiler_wrapper
    character(len=4096) :: initial_hold, initial_release, initial_entered
    character(len=65536) :: cli_json
    integer :: failures, status, ios
    logical :: driver_exists, owner_started

    interface
        function pause_microseconds(duration) bind(C, name='usleep') result(rc)
            import :: c_int
            integer(c_int), value :: duration
            integer(c_int) :: rc
        end function pause_microseconds
    end interface

    call fake_tool_dispatch()
    call get_command_argument(1, helper_mode)
    if (trim(helper_mode) == '--source-writer') then
        call get_command_argument(2, helper_label)
        call get_command_argument(3, helper_value)
        call get_command_argument(4, helper_fixture)
        call run_source_writer(trim(helper_fixture), trim(helper_label), trim(helper_value))
        stop
    end if
    failures = 0
    owner_started = .false.
    call get_environment_variable('FO_BIN', driver_path, status=ios)
    driver_exists = ios == 0 .and. len_trim(driver_path) > 0
    if (driver_exists) inquire (file=trim(driver_path), exist=driver_exists)
    call check(driver_exists, 'test runner provides its selected FO_BIN')
    if (.not. driver_exists) stop 1
    call get_environment_variable('PWD', root, status=ios)
    call check(ios == 0, 'project working directory is available')
    if (ios /= 0) stop 1
    call system_clock(count=ios)
    write (fixture, '(a,i0)') temporary_root()//'/fo-capture-pending-', ios
    state_directory = trim(fixture)//'-state'
    initial_hold = trim(fixture)//'-initial-hold'
    initial_release = trim(fixture)//'-initial-release'
    initial_entered = trim(fixture)//'-initial-entered'
    compiler_wrapper = trim(fixture)//'-compiler-wrapper'
    call fs_make_dir(trim(fixture)//'/src', status)
    if (status == 0) call fs_make_dir(trim(fixture)//'/test', status)
    if (status /= 0) stop 1
    call fs_find_executable('gfortran', compiler_path, driver_exists)
    call check(driver_exists, &
        'real gfortran path is available for the initial-build barrier')
    if (.not. driver_exists) stop 1
    call write_compiler_wrapper()
    call touch_marker(initial_hold)
    call write_manifest()
    call write_probe_pair(1)

    call exercise_initial_and_active_capture()
    if (owner_started) call stop_owner()
    if (failures == 0) then
        call fs_remove_tree(trim(fixture))
        call fs_remove_tree(trim(state_directory))
        call remove_control_files()
    else
        write (error_unit, '(a)') 'Preserving failed capture-pending fixture:'
        write (error_unit, '(a)') 'fixture='//trim(fixture)
        write (error_unit, '(a)') 'state='//trim(state_directory)
    end if
    if (failures > 0) stop 1
    print '(a)', 'Public capture-pending readiness oracle PASS'

contains

    subroutine exercise_initial_and_active_capture()
        character(len=64) :: first_candidate, first_active
        integer :: attempt, exitcode
        logical :: entered, building, pending, failure_wait_clear

        call cli('start --target test_case_a --seed 17', cli_json, status)
        call check(status == 0, 'real CLI starts the isolated fixture campaign')
        if (status /= 0) then
            call release_barriers()
            return
        end if
        owner_started = .true.
        entered = wait_for_marker(initial_entered, 10000)
        call check(entered, 'initial compiler call blocks before first activation')
        building = .false.
        do attempt = 1, 300
            call cli('status', cli_json, exitcode)
            if (exitcode == 0 .and. field(cli_json, 'state') == 'building' .and. &
                len_trim(field(cli_json, 'active_generation')) == 0) then
                building = .true.
                exit
            end if
            exitcode = pause_microseconds(10000_c_int)
        end do
        call check(building, 'first candidate is building without an active generation')
        if (.not. building) then
            call release_barriers()
            return
        end if
        first_candidate = field(cli_json, 'candidate_generation')
        call check(len_trim(first_candidate) == 64, 'initial candidate ID is public')

        call start_source_writer('initial', 2)
        pending = wait_for_pending(100)
        if (pending) then
            call check(len_trim(field(cli_json, 'active_generation')) == 0, &
                'initial capture is pending before any generation is active')
            if (.not. (field(cli_json, 'dirty') == 'true' .and. &
                    field(cli_json, 'local_gate_green') == 'false' .and. &
                    field(cli_json, 'gate_token') == '' .and. &
                    field(cli_json, 'phase') == 'starting' .and. &
                    field(cli_json, 'health') == 'degraded')) then
                call diagnose_status('initial capture_pending')
            end if
            call check(field(cli_json, 'dirty') == 'true' .and. &
                field(cli_json, 'local_gate_green') == 'false' .and. &
                field(cli_json, 'gate_token') == '' .and. &
                field(cli_json, 'phase') == 'starting' .and. &
                field(cli_json, 'health') == 'degraded', &
                'initial pending capture is dirty, starting and non-green')
            call cli('wait --until failure --wait-ms 0', cli_json, exitcode)
            failure_wait_clear = exitcode == 0 .and. &
                field(cli_json, 'wait_satisfied') == 'false' .and. &
                field(cli_json, 'wait_timed_out') == 'true'
            if (.not. failure_wait_clear) call diagnose_status('initial failure wait')
            call check(failure_wait_clear, &
                'public failure wait is false while initial capture is pending')
        end if
        call stop_source_writer('initial')
        call check(pending, 'public CLI observes initial capture_pending')
        if (.not. pending) then
            call release_barriers()
            return
        end if
        call touch_marker(initial_release)
        call await_new_green(first_candidate, exitcode)
        call check(exitcode == 0 .and. field(cli_json, 'wait_satisfied') == 'true', &
            'released initial capture builds and verifies the edited source pair')
        first_active = field(cli_json, 'active_generation')
        call check(len_trim(first_active) == 64 .and. first_active /= first_candidate, &
            'edited dependency and expected value activate as a new generation')

        call start_source_writer('active', 100)
        pending = wait_for_pending(100)
        if (pending) then
            if (.not. (field(cli_json, 'active_generation') == first_active .and. &
                    field(cli_json, 'phase') == 'testing' .and. &
                    field(cli_json, 'health') == 'degraded' .and. &
                    field(cli_json, 'dirty') == 'true' .and. &
                    field(cli_json, 'local_gate_green') == 'false' .and. &
                    field(cli_json, 'gate_token') == '')) then
                call diagnose_status('active capture_pending')
            end if
            call check(field(cli_json, 'active_generation') == first_active .and. &
                field(cli_json, 'phase') == 'testing' .and. &
                field(cli_json, 'health') == 'degraded' .and. &
                field(cli_json, 'dirty') == 'true' .and. &
                field(cli_json, 'local_gate_green') == 'false' .and. &
                field(cli_json, 'gate_token') == '', &
                'active capture preserves testing phase and invalidates readiness')
            call cli('wait --until failure --wait-ms 0', cli_json, exitcode)
            failure_wait_clear = exitcode == 0 .and. &
                field(cli_json, 'wait_satisfied') == 'false'
            if (.not. failure_wait_clear) call diagnose_status('active failure wait')
            call check(failure_wait_clear, &
                'public failure wait remains false during active capture pending')
        end if
        call stop_source_writer('active')
        call check(pending, 'public CLI observes active capture_pending')
        if (.not. pending) then
            call release_barriers()
            return
        end if
        call await_new_green(first_active, exitcode)
        call check(exitcode == 0 .and. field(cli_json, 'wait_satisfied') == 'true' .and. &
            field(cli_json, 'active_generation') /= first_active, &
            'active capture progresses to a distinct verified generation')
        call release_barriers()
    end subroutine exercise_initial_and_active_capture

    logical function wait_for_pending(max_attempts) result(found)
        integer, intent(in) :: max_attempts
        integer :: attempt, exitcode, sleep_status
        found = .false.
        do attempt = 1, max_attempts
            call cli('status', cli_json, exitcode)
            if (exitcode == 0 .and. field(cli_json, 'state') == 'capture_pending') then
                found = .true.
                return
            end if
            sleep_status = pause_microseconds(10000_c_int)
        end do
    end function wait_for_pending

    subroutine await_new_green(previous, exitcode)
        character(len=*), intent(in) :: previous
        integer, intent(out) :: exitcode
        integer :: attempt, sleep_status
        do attempt = 1, 300
            call cli('wait --until local-gate-green --wait-ms 1000', cli_json, exitcode)
            if (exitcode /= 0) return
            if (field(cli_json, 'wait_satisfied') == 'true' .and. &
                field(cli_json, 'active_generation') /= previous .and. &
                len_trim(field(cli_json, 'gate_token')) == 64) return
            sleep_status = pause_microseconds(10000_c_int)
        end do
        call diagnose_status('await new green')
        exitcode = 1
    end subroutine await_new_green

    subroutine write_manifest()
        integer :: file_unit
        open (newunit=file_unit, file=trim(fixture)//'/fpm.toml', status='replace')
        write (file_unit, '(a)') 'name = "capture_pending_fixture"'
        write (file_unit, '(a)') '[extra.fo]'
        write (file_unit, '(a)') 'test-timeout = 10'
        close (file_unit)
    end subroutine write_manifest

    subroutine write_probe_pair(value)
        integer, intent(in) :: value
        integer :: command_status

        call fs_write_text(trim(fixture)//'/src/fixture_probe.f90.tmp', probe_source(value))
        call fs_write_text(trim(fixture)//'/test/test_case_a.f90.tmp', case_source(value))
        command_status = fs_rename(trim(fixture)//'/src/fixture_probe.f90.tmp', &
            trim(fixture)//'/src/fixture_probe.f90')
        if (command_status == 0) command_status = fs_rename( &
            trim(fixture)//'/test/test_case_a.f90.tmp', &
            trim(fixture)//'/test/test_case_a.f90')
        call check(command_status == 0, 'publishes matched provider and test sources')
    end subroutine write_probe_pair

    subroutine write_compiler_wrapper()
        character(len=1), parameter :: lf = achar(10)
        character(len=:), allocatable :: barrier

        ! While the initial hold exists and is unreleased, announce entry and wait
        ! (bounded) for the release before running the real compiler.
        barrier = 'ifexists '//trim(initial_hold)//' ifabsent '//trim(initial_release)//' '
        call install_fake_tool(trim(compiler_wrapper), &
            barrier//'touch '//trim(initial_entered)//lf// &
            barrier//'wait-file '//trim(initial_release)//' 30000'//lf// &
            'exec "'//trim(compiler_path)//'" $@')
    end subroutine write_compiler_wrapper

    subroutine start_source_writer(label, first_value)
        character(len=*), intent(in) :: label
        integer, intent(in) :: first_value
        character(len=4096) :: writer
        character(len=32) :: first_text
        character(len=:), allocatable :: self
        logical :: started, resolved

        writer = trim(fixture)//'-writer-'//trim(label)
        write (first_text, '(i0)') first_value
        call resolve_test_executable(self, resolved)
        if (resolved) call spawn_list(argv(self, '--source-writer', trim(label), &
            trim(first_text), trim(fixture)))
        started = wait_for_marker(trim(writer)//'.started', 5000)
        call check(resolved .and. started, &
            'source writer starts for '//trim(label)//' capture')
        if (.not. started) call stop_source_writer(label)
    end subroutine start_source_writer

    subroutine run_source_writer(fixture_path, label, first_text)
        !! Helper process: republish a matched provider and test pair, with a rising
        !! value, until the stop marker appears.
        character(len=*), intent(in) :: fixture_path, label, first_text
        character(len=:), allocatable :: writer
        integer :: value, ios, status

        read (first_text, *, iostat=ios) value
        if (ios /= 0) stop 2
        writer = fixture_path//'-writer-'//label
        call fs_write_text(writer//'.started', '')
        do while (.not. marker_exists(writer//'.stop'))
            call fs_write_text(writer//'.probe.tmp', probe_source(value))
            call fs_write_text(writer//'.case.tmp', case_source(value))
            status = fs_rename(writer//'.probe.tmp', fixture_path//'/src/fixture_probe.f90')
            status = fs_rename(writer//'.case.tmp', fixture_path//'/test/test_case_a.f90')
            value = value + 1
            status = pause_microseconds(20000_c_int)
        end do
        call fs_write_text(writer//'.done', '')
    end subroutine run_source_writer

    logical function marker_exists(path)
        character(len=*), intent(in) :: path

        inquire (file=path, exist=marker_exists)
    end function marker_exists

    function probe_source(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=32) :: digits
        character(len=1), parameter :: lf = achar(10)

        write (digits, '(i0)') value
        text = 'module fixture_probe'//lf//'implicit none'//lf//'contains'//lf// &
            'integer function fixture_value() result(value)'//lf// &
            'value = '//trim(digits)//lf//'end function'//lf// &
            'end module fixture_probe'//lf
    end function probe_source

    function case_source(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=32) :: digits
        character(len=1), parameter :: lf = achar(10)

        write (digits, '(i0)') value
        text = 'program fixture_case_a'//lf// &
            'use fixture_probe, only: fixture_value'//lf//'implicit none'//lf// &
            'if (fixture_value() /= '//trim(digits)//') error stop 9'//lf// &
            'end program fixture_case_a'//lf
    end function case_source

    subroutine stop_source_writer(label)
        character(len=*), intent(in) :: label
        character(len=4096) :: writer
        logical :: started, done
        writer = trim(fixture)//'-writer-'//trim(label)
        call touch_marker(trim(writer)//'.stop')
        inquire (file=trim(writer)//'.started', exist=started)
        if (started) then
            done = wait_for_marker(trim(writer)//'.done', 5000)
            call check(done, 'source writer exits for '//trim(label)//' capture')
        end if
        call fs_remove_file(trim(writer)//'.probe.tmp')
        call fs_remove_file(trim(writer)//'.case.tmp')
    end subroutine stop_source_writer

    subroutine release_barriers()
        call touch_marker(initial_release)
        call stop_source_writer('initial')
        call stop_source_writer('active')
    end subroutine release_barriers

    subroutine stop_owner()
        character(len=64) :: session
        integer :: attempt, exitcode
        session = field(cli_json, 'session_id')
        call cli('stop --session '//trim(session), cli_json, exitcode)
        call check(exitcode == 0, 'stops capture-pending fixture owner through the CLI')
        do attempt = 1, 100
            call cli('status', cli_json, exitcode)
            if (exitcode /= 0) return
            exitcode = pause_microseconds(50000_c_int)
        end do
        call check(.false., 'fixture owner releases after public stop')
    end subroutine stop_owner

    subroutine cli(arguments, json, exitcode)
        character(len=*), intent(in) :: arguments
        character(len=*), intent(out) :: json
        integer, intent(out) :: exitcode
        type(string_list_t) :: invocation, environment
        character(len=:), allocatable :: detail, standard_output, ignored_errors
        integer :: i, used
        ! Readiness fields such as gate_token appear only in full detail.
        detail = ''
        if (index(arguments, 'status') == 1 .or. index(arguments, 'wait') == 1) &
            detail = ' --detail full'
        call list_add(environment, 'FO_JOBS=1')
        call list_add(environment, 'FO_DISABLE_SELF_REFRESH=1')
        call list_add(environment, 'FO_GREMLIN_STATE_DIR='//trim(state_directory))
        call list_add(environment, &
            'FO_GREMLIN_ADMISSION_DIR='//trim(state_directory)//'/admission')
        call list_add(environment, 'FO_FC='//trim(compiler_wrapper))
        call list_add(invocation, trim(driver_path))
        call list_add(invocation, 'gremlin')
        call add_words(invocation, arguments//detail)
        call list_add(invocation, '--dir')
        call list_add(invocation, trim(fixture))
        call list_add(invocation, '--lane')
        call list_add(invocation, 'capture-pending')
        call run_list(invocation, exitcode, standard_output, errors=ignored_errors, &
            cwd=trim(root), environment=environment, timeout_ms=-1)
        if (exitcode < 0) exitcode = 127
        json = ''
        used = 0
        do i = 1, len(standard_output)
            if (standard_output(i:i) == achar(10)) cycle
            if (used >= len(json)) exit
            used = used + 1
            json(used:used) = standard_output(i:i)
        end do
    end subroutine cli

    function field(json, key) result(value)
        character(len=*), intent(in) :: json, key
        character(len=65536) :: value
        integer :: at, cursor, used, n
        logical :: quoted
        value = ''
        at = index(json, '"'//key//'":')
        if (at == 0) return
        cursor = at + len(key) + 3
        n = len_trim(json)
        do while (cursor <= n)
            if (json(cursor:cursor) /= ' ') exit
            cursor = cursor + 1
        end do
        if (cursor > n) return
        quoted = json(cursor:cursor) == '"'
        if (quoted) cursor = cursor + 1
        used = 0
        do while (cursor <= n .and. used < len(value))
            if (quoted) then
                if (json(cursor:cursor) == '"') exit
                if (json(cursor:cursor) == achar(92)) cursor = cursor + 1
            else
                if (index(',}'//achar(10), json(cursor:cursor)) > 0) exit
            end if
            used = used + 1
            value(used:used) = json(cursor:cursor)
            cursor = cursor + 1
        end do
    end function field

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label
        if (condition) return
        failures = failures + 1
        write (error_unit, '(a)') 'FAIL: '//label
    end subroutine check

    subroutine diagnose_status(label)
        character(len=*), intent(in) :: label
        write (error_unit, '(a)') 'Capture-pending status diagnostic: '//trim(label)
        write (error_unit, '(a)') 'raw='//trim(cli_json)
        write (error_unit, '(a)') 'state='//field(cli_json, 'state')// &
            ' phase='//field(cli_json, 'phase')//' health='//field(cli_json, 'health')
        write (error_unit, '(a)') 'active='//field(cli_json, 'active_generation')// &
            ' candidate='//field(cli_json, 'candidate_generation')
        write (error_unit, '(a)') 'dirty='//field(cli_json, 'dirty')// &
            ' green='//field(cli_json, 'local_gate_green')// &
            ' token='//field(cli_json, 'gate_token')// &
            ' wait_satisfied='//field(cli_json, 'wait_satisfied')
    end subroutine diagnose_status

    logical function wait_for_marker(path, timeout_ms) result(found)
        character(len=*), intent(in) :: path
        integer, intent(in) :: timeout_ms
        integer :: attempt, sleep_status
        found = .false.
        do attempt = 1, max(1, timeout_ms / 10)
            inquire (file=trim(path), exist=found)
            if (found) return
            sleep_status = pause_microseconds(10000_c_int)
        end do
    end function wait_for_marker

    subroutine touch_marker(path)
        character(len=*), intent(in) :: path
        integer :: file_unit
        open (newunit=file_unit, file=trim(path), status='replace')
        write (file_unit, '(a)') 'release'
        close (file_unit)
    end subroutine touch_marker

    subroutine remove_control_files()
        character(len=16), parameter :: labels(2) = [character(len=16) :: 'initial', 'active']
        character(len=24), parameter :: suffixes(6) = [character(len=24) :: '', '.started', &
            '.stop', '.done', '.probe.tmp', '.case.tmp']
        integer :: i, j

        call fs_remove_file(trim(compiler_wrapper))
        call fs_remove_file(trim(initial_hold))
        call fs_remove_file(trim(initial_release))
        call fs_remove_file(trim(initial_entered))
        do i = 1, size(labels)
            do j = 1, size(suffixes)
                call fs_remove_file(trim(fixture)//'-writer-'//trim(labels(i))// &
                    trim(suffixes(j)))
            end do
        end do
    end subroutine remove_control_files
end program test_gremlin_capture_pending
