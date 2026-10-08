program test_gremlin_capture_pending
    use fo_util, only: temporary_root
    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none

    character(len=4096) :: root, fixture, state_directory, cli_file
    character(len=4096) :: driver_path, compiler_path, compiler_wrapper
    character(len=4096) :: initial_hold, initial_release, initial_entered
    character(len=65536) :: cli_json, output
    integer :: failures, status, ios
    logical :: driver_exists, owner_started

    interface
        function pause_microseconds(duration) bind(C, name='usleep') result(rc)
            import :: c_int
            integer(c_int), value :: duration
            integer(c_int) :: rc
        end function pause_microseconds
    end interface

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
    cli_file = trim(fixture)//'-cli.json'
    initial_hold = trim(fixture)//'-initial-hold'
    initial_release = trim(fixture)//'-initial-release'
    initial_entered = trim(fixture)//'-initial-entered'
    compiler_wrapper = trim(fixture)//'-compiler-wrapper'
    call command('mkdir -p '//quote(trim(fixture)//'/src')//' '// &
        quote(trim(fixture)//'/test'), status)
    if (status /= 0) stop 1
    call command('command -v gfortran > '//quote(trim(cli_file)), status)
    call read_output(cli_file, output)
    compiler_path = trim(output)
    call check(status == 0 .and. len_trim(compiler_path) > 0, &
        'real gfortran path is available for the initial-build barrier')
    if (status /= 0 .or. len_trim(compiler_path) == 0) stop 1
    call write_compiler_wrapper()
    call command('chmod +x '//quote(trim(compiler_wrapper)), status)
    if (status /= 0) stop 1
    call touch_marker(initial_hold)
    call write_manifest()
    call write_probe_pair(1)

    call exercise_initial_and_active_capture()
    if (owner_started) call stop_owner()
    if (failures == 0) then
        call command('rm -rf '//quote(trim(fixture))//' '// &
            quote(trim(state_directory))//' '//quote(trim(cli_file)), status)
        call remove_control_files()
    else
        write (error_unit, '(a)') 'Preserving failed capture-pending fixture:'
        write (error_unit, '(a)') 'fixture='//trim(fixture)
        write (error_unit, '(a)') 'state='//trim(state_directory)
        write (error_unit, '(a)') 'cli='//trim(cli_file)
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
        integer :: file_unit, command_status
        open (newunit=file_unit, file=trim(fixture)//'/src/fixture_probe.f90.tmp', &
            status='replace')
        write (file_unit, '(a)') 'module fixture_probe'
        write (file_unit, '(a)') 'implicit none'
        write (file_unit, '(a)') 'contains'
        write (file_unit, '(a)') 'integer function fixture_value() result(value)'
        write (file_unit, '(a,i0)') 'value = ', value
        write (file_unit, '(a)') 'end function'
        write (file_unit, '(a)') 'end module fixture_probe'
        close (file_unit)
        open (newunit=file_unit, file=trim(fixture)//'/test/test_case_a.f90.tmp', &
            status='replace')
        write (file_unit, '(a)') 'program fixture_case_a'
        write (file_unit, '(a)') 'use fixture_probe, only: fixture_value'
        write (file_unit, '(a)') 'implicit none'
        write (file_unit, '(a,i0,a)') 'if (fixture_value() /= ', value, ') error stop 9'
        write (file_unit, '(a)') 'end program fixture_case_a'
        close (file_unit)
        call command('mv '//quote(trim(fixture)//'/src/fixture_probe.f90.tmp')//' '// &
            quote(trim(fixture)//'/src/fixture_probe.f90')//' && mv '// &
            quote(trim(fixture)//'/test/test_case_a.f90.tmp')//' '// &
            quote(trim(fixture)//'/test/test_case_a.f90'), command_status)
        call check(command_status == 0, 'publishes matched provider and test sources')
    end subroutine write_probe_pair

    subroutine write_compiler_wrapper()
        integer :: file_unit
        open (newunit=file_unit, file=trim(compiler_wrapper), status='replace')
        write (file_unit, '(a)') '#!/bin/sh'
        write (file_unit, '(a)') 'if [ -e '//trim(initial_hold)//' ] && [ ! -e '// &
            trim(initial_release)//' ]; then'
        write (file_unit, '(a)') '  : > '//trim(initial_entered)
        write (file_unit, '(a)') '  i=0; while [ ! -e '//trim(initial_release)//' ] && [ "$i" -lt 3000 ]; do'
        write (file_unit, '(a)') '    sleep 0.01; i=$((i+1))'
        write (file_unit, '(a)') '  done'
        write (file_unit, '(a)') 'fi'
        write (file_unit, '(a)') 'exec '//trim(compiler_path)//' "$@"'
        close (file_unit)
    end subroutine write_compiler_wrapper

    subroutine start_source_writer(label, first_value)
        character(len=*), intent(in) :: label
        integer, intent(in) :: first_value
        character(len=4096) :: writer
        integer :: file_unit, launch_status
        logical :: started
        writer = trim(fixture)//'-writer-'//trim(label)
        open (newunit=file_unit, file=trim(writer), status='replace')
        write (file_unit, '(a)') '#!/bin/sh'
        write (file_unit, '(a,i0,a)') 'n=', first_value, '; : > '//trim(writer)//'.started'
        write (file_unit, '(a)') 'while [ ! -e '//trim(writer)//'.stop ]; do'
        write (file_unit, '(a)') '  cat > '//trim(writer)//'.probe.tmp <<EOF'
        write (file_unit, '(a)') 'module fixture_probe'
        write (file_unit, '(a)') 'implicit none'
        write (file_unit, '(a)') 'contains'
        write (file_unit, '(a)') 'integer function fixture_value() result(value)'
        write (file_unit, '(a)') 'value = $n'
        write (file_unit, '(a)') 'end function'
        write (file_unit, '(a)') 'end module fixture_probe'
        write (file_unit, '(a)') 'EOF'
        write (file_unit, '(a)') '  cat > '//trim(writer)//'.case.tmp <<EOF'
        write (file_unit, '(a)') 'program fixture_case_a'
        write (file_unit, '(a)') 'use fixture_probe, only: fixture_value'
        write (file_unit, '(a)') 'implicit none'
        write (file_unit, '(a)') 'if (fixture_value() /= $n) error stop 9'
        write (file_unit, '(a)') 'end program fixture_case_a'
        write (file_unit, '(a)') 'EOF'
        write (file_unit, '(a)') '  mv '//trim(writer)//'.probe.tmp '// &
            trim(fixture)//'/src/fixture_probe.f90'
        write (file_unit, '(a)') '  mv '//trim(writer)//'.case.tmp '// &
            trim(fixture)//'/test/test_case_a.f90'
        write (file_unit, '(a)') '  n=$((n+1)); sleep 0.02'
        write (file_unit, '(a)') 'done'
        write (file_unit, '(a)') ': > '//trim(writer)//'.done'
        close (file_unit)
        call command('chmod +x '//quote(trim(writer)), launch_status)
        if (launch_status == 0) call command(quote(trim(writer))//' >/dev/null 2>&1 &', &
            launch_status)
        started = wait_for_marker(trim(writer)//'.started', 5000)
        call check(launch_status == 0 .and. started, &
            'source writer starts for '//trim(label)//' capture')
        if (.not. started) call stop_source_writer(label)
    end subroutine start_source_writer

    subroutine stop_source_writer(label)
        character(len=*), intent(in) :: label
        character(len=4096) :: writer
        integer :: command_status
        logical :: started, done
        writer = trim(fixture)//'-writer-'//trim(label)
        call touch_marker(trim(writer)//'.stop')
        inquire (file=trim(writer)//'.started', exist=started)
        if (started) then
            done = wait_for_marker(trim(writer)//'.done', 5000)
            call check(done, 'source writer exits for '//trim(label)//' capture')
        end if
        call command('rm -f '//quote(trim(writer)//'.probe.tmp')//' '// &
            quote(trim(writer)//'.case.tmp'), command_status)
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
        character(len=:), allocatable :: prefix, detail
        ! Readiness fields such as gate_token appear only in full detail.
        detail = ''
        if (index(arguments, 'status') == 1 .or. index(arguments, 'wait') == 1) &
            detail = ' --detail full'
        prefix = 'cd '//quote(trim(root))//' && FO_JOBS=1 '// &
            'FO_DISABLE_SELF_REFRESH=1 '// &
            'FO_GREMLIN_STATE_DIR='//quote(trim(state_directory))//' '// &
            'FO_FC='//quote(trim(compiler_wrapper))//' '//quote(trim(driver_path))
        call command(prefix//' gremlin '//arguments//detail//' --dir '// &
            quote(trim(fixture))//' --lane capture-pending > '//quote(trim(cli_file)), &
            exitcode)
        call read_output(cli_file, json)
    end subroutine cli

    subroutine command(text, exitcode)
        character(len=*), intent(in) :: text
        integer, intent(out) :: exitcode
        integer :: launch_error
        call execute_command_line(text, exitstat=exitcode, cmdstat=launch_error)
        if (launch_error /= 0) exitcode = launch_error
    end subroutine command

    subroutine read_output(path, text)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: text
        character(len=65536) :: line
        integer :: file_unit, read_status, used
        text = ''
        used = 0
        open (newunit=file_unit, file=trim(path), status='old', iostat=read_status)
        if (read_status /= 0) return
        do
            read (file_unit, '(a)', iostat=read_status) line
            if (read_status /= 0) exit
            if (used + len_trim(line) > len(text)) exit
            text(used + 1:used + len_trim(line)) = trim(line)
            used = used + len_trim(line)
        end do
        close (file_unit)
    end subroutine read_output

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

    function quote(text) result(quoted)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: quoted
        integer :: i
        quoted = "'"
        do i = 1, len(text)
            if (text(i:i) == "'") then
                quoted = quoted//"'"//'"'//"'"//'"'//"'"
            else
                quoted = quoted//text(i:i)
            end if
        end do
        quoted = quoted//"'"
    end function quote

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
        integer :: command_status
        call command('rm -f '//quote(trim(compiler_wrapper))//' '// &
            quote(trim(initial_hold))//' '//quote(trim(initial_release))//' '// &
            quote(trim(initial_entered))//' '// &
            quote(trim(fixture)//'-writer-initial')//' '// &
            quote(trim(fixture)//'-writer-initial.started')//' '// &
            quote(trim(fixture)//'-writer-initial.stop')//' '// &
            quote(trim(fixture)//'-writer-initial.done')//' '// &
            quote(trim(fixture)//'-writer-active')//' '// &
            quote(trim(fixture)//'-writer-active.started')//' '// &
            quote(trim(fixture)//'-writer-active.stop')//' '// &
            quote(trim(fixture)//'-writer-active.done')//' '// &
            quote(trim(fixture)//'-writer-initial.probe.tmp')//' '// &
            quote(trim(fixture)//'-writer-initial.case.tmp')//' '// &
            quote(trim(fixture)//'-writer-active.probe.tmp')//' '// &
            quote(trim(fixture)//'-writer-active.case.tmp'), command_status)
    end subroutine remove_control_files
end program test_gremlin_capture_pending
