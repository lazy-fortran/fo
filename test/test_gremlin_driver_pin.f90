program test_gremlin_driver_pin
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fo_cache, only: HASH_LEN
    use fo_driver, only: driver_pin_t, driver_pin_current
    use fo_fs, only: fs_sleep_ms
    use fx_hash, only: sha256_file
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, write_text, read_text, remove_path
    use fo_test_harness, only: file_exists, move_path, run_process, spawn_process
    use fo_test_harness, only: poll_process, terminate_process_group
    use fo_test_harness, only: assert_true, assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args
    use fo_test_gremlin_oracle, only: gremlin_run, gremlin_json, gremlin_field, &
        gremlin_wait_ms
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element, &
        json_size
    use fo_test_json, only: json_number, json_number_value
    implicit none

    interface
        integer(c_int) function c_chmod(path, mode) bind(C, name='chmod')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_chmod
    end interface

    character(len=4096) :: first_argument
    integer :: argument_status

    first_argument = ''
    if (command_argument_count() > 0) then
        call get_command_argument(1, first_argument, status=argument_status)
        if (argument_status == 0 .and. &
            trim(first_argument) == '--startup-pin-helper') then
            call startup_pin_helper()
            stop
        end if
    end if
    call run_driver_pin_oracles()
    call finish_assertions()

contains

    subroutine run_driver_pin_oracles()
        character(len=4096) :: env_driver
        character(:), allocatable :: driver_a, scratch, project, cache, state
        character(:), allocatable :: b_project, b_binary, b_probe, marker, public_driver
        character(:), allocatable :: helper_stage
        character(:), allocatable :: lane, session, generation, next_generation
        character(:), allocatable :: size_text, receipt_path, identity_text
        type(string_list_t) :: command, args, environment
        type(process_result_t) :: process
        type(json_value_t) :: document, event, events, receipt_driver_size
        integer :: env_status, ierr, child, size_status, j
        integer(int64) :: expected_size, actual_size
        logical :: matched, equal
        character(len=HASH_LEN) :: expected_digest, actual_digest
        character(:), allocatable :: json_message
        logical :: json_valid

        env_driver = ''
        call get_environment_variable('FO_DRIVER_A', env_driver, status=env_status)
        if (env_status /= 0 .or. len_trim(env_driver) == 0 .or. &
            env_driver(1:1) /= '/') then
            call assert_true(.false., &
                'FO_DRIVER_A names the controller-supplied absolute immutable fo image')
            return
        end if
        driver_a = trim(env_driver)
        call sha256_file(driver_a, expected_digest, ierr)
        call assert_true(ierr == 0, &
            'independently hash the supplied immutable fo image')
        if (ierr /= 0) return

        call gremlin_setup(driver_a, scratch, project, cache, state)
        ! gremlin_setup also establishes the isolated cache and state environment.
        driver_a = trim(env_driver)
        b_project = scratch//'/driver-b'
        call make_directory(b_project//'/app')
        call write_text(b_project//'/fpm.toml', &
            'name = "fo-driver-pin-b"'//new_line('a')// &
            'auto-executables = false'//new_line('a')// &
            'auto-tests = false'//new_line('a')// &
            '[[executable]]'//new_line('a')// &
            'name = "fo_driver_b"'//new_line('a')// &
            'source-dir = "app"'//new_line('a')// &
            'main = "main.f90"'//new_line('a'))
        call write_text(b_project//'/app/main.f90', marker_driver_source())
        command = string_list_t()
        call list_add(command, driver_a)
        call list_add(command, 'build')
        call run_process(command, b_project, process)
        call assert_true(process%exit_code == 0 .and. .not. process%runner_failed, &
            'pinned A builds the tiny native marker driver B')
        b_binary = b_project//'/build/fo/bin/fo_driver_b'
        call assert_true(file_exists(b_binary), 'native marker driver B exists')
        if (.not. file_exists(b_binary)) return
        b_probe = scratch//'/driver-b-probe'
        environment = string_list_t()
        call list_add(environment, 'FO_B_MARKER='//b_probe)
        command = string_list_t()
        call list_add(command, b_binary)
        call list_add(command, 'build')
        call run_process(command, b_project, process, environment)
        call assert_true(process%exit_code /= 0 .and. file_exists(b_probe), &
            'B distinctly fails and records a build invocation')
        if (file_exists(b_probe)) then
            call assert_equal_string(read_text(b_probe), 'build'//new_line('a'), &
                'B marker identifies the build command')
            call remove_path(b_probe)
        end if
        command = string_list_t()
        call list_add(command, b_binary)
        call list_add(command, 'test')
        call run_process(command, b_project, process, environment)
        call assert_true(process%exit_code /= 0 .and. file_exists(b_probe), &
            'B distinctly fails and records a test invocation')
        if (file_exists(b_probe)) then
            call assert_equal_string(read_text(b_probe), 'test'//new_line('a'), &
                'B marker identifies the test command')
            call remove_path(b_probe)
        end if

        call startup_fd_oracle(scratch, b_binary)

        public_driver = scratch//'/public/fo'
        call make_directory(scratch//'/public')
        call copy_file(driver_a, public_driver)
        call chmod_executable(public_driver)
        marker = scratch//'/driver-b-invoked'
        call write_text(project//'/fpm.toml', &
            'name = "fo-driver-pin-campaign"'//new_line('a')// &
            'auto-tests = true'//new_line('a'))
        call make_directory(project//'/test')
        call write_campaign_test(project, 1)
        lane = 'driver-pin-ab'
        args = string_list_t()
        call gremlin_start_args(args, project, lane, 'test_driver_pin_case')
        call list_add(args, '--json')
        environment = string_list_t()
        call list_add(environment, 'FO_B_MARKER='//marker)
        call gremlin_run(public_driver, project, cache, state, args, process, &
            environment=environment, timeout=120000)
        call assert_true(process%exit_code == 0 .and. .not. process%runner_failed, &
            'A starts the Gremlin owner')
        call json_parse(process%stdout, document, json_valid, json_message)
        call assert_true(json_valid, &
            'A start response is parseable JSON: '//json_message)
        session = gremlin_field(document, 'session_id')
        call assert_true(len(session) > 0, &
            'A starts the Gremlin owner with its exact image')
        if (len(session) == 0) return
        call wait_for_pass(driver_a, project, cache, state, lane, session, '', &
            generation, document)
        if (len(generation) == 0) then
            call stop_lane(driver_a, project, cache, state, lane, session)
            return
        end if
        call assert_true(gremlin_field(document, 'driver_digest') == expected_digest, &
            'live session status reports the exact running A digest')

        helper_stage = scratch//'/public/b-for-fo'
        call copy_file(b_binary, helper_stage)
        call chmod_executable(helper_stage)
        call move_path(helper_stage, public_driver)
        call write_campaign_test(project, 2)
        call wait_for_pass(driver_a, project, cache, state, lane, session, generation, &
            next_generation, document)
        if (len(next_generation) == 0) then
            call stop_lane(driver_a, project, cache, state, lane, session)
            return
        end if
        call assert_true(next_generation /= generation, &
            'editing the test captures a new generation after public A becomes B')
        call assert_true(gremlin_field(document, 'driver_digest') == expected_digest, &
            'new generation status keeps A pinned after public path replacement')
        receipt_path = gremlin_field(document, 'driver_path')
        call assert_true(len(receipt_path) > 0, &
            'status reports the digest-named A pin path')
        if (len(receipt_path) > 0) then
            actual_digest = ''
            call sha256_file(receipt_path, actual_digest, ierr)
            call assert_true(ierr == 0 .and. actual_digest == expected_digest, &
                'independent hashing confirms the owned pin contains A bytes')
            call files_equal(driver_a, receipt_path, equal)
            call assert_true(equal, &
                'pinned session file equals the supplied A image byte-for-byte')
        end if
        expected_size = 0_int64
        inquire(file=driver_a, size=expected_size, iostat=size_status)
        size_text = int64_text(expected_size)
        call assert_true(size_status == 0 .and. expected_size > 0_int64, &
            'read the supplied A image size independently')
        identity_text = read_text(state//'/fo/gremlin/generations/'// &
            next_generation//'/identity.txt')
        call assert_true(index(identity_text, 'driver_digest='//expected_digest) > 0, &
            'generation identity persists the exact A digest')
        call assert_true(index(identity_text, 'driver_size='//trim(size_text)) > 0, &
            'generation identity persists the exact A size')

        actual_size = 0_int64
        inquire(file=receipt_path, size=actual_size, iostat=ierr)
        call assert_true(size_status == 0 .and. ierr == 0 .and. &
            expected_size > 0_int64 .and. actual_size == expected_size, &
            'owned A pin has the original driver byte size')
        receipt_driver_size = json_member(document, 'driver_size')
        call assert_true(receipt_driver_size%kind == json_number, &
            'session status exposes numeric driver size')
        if (receipt_driver_size%kind == json_number) then
            call assert_true(json_number_value(receipt_driver_size) == &
                real(expected_size, real64), &
                'session status records the original A size')
        end if
        events = json_member(document, 'events')
        matched = .false.
        do j = 1, json_size(events)
            event = json_element(events, j)
            if (gremlin_field(event, 'case_id') /= 'test_driver_pin_case') cycle
            if (gremlin_field(event, 'generation') /= next_generation) cycle
            if (gremlin_field(event, 'status') /= 'PASS') cycle
            call assert_true(gremlin_field(event, 'driver_digest') == expected_digest, &
                'durable PASS receipt records A digest')
            call assert_true(gremlin_field(event, 'driver_path') == receipt_path, &
                'durable PASS receipt names the same owned A pin')
            receipt_driver_size = json_member(event, 'driver_size')
            if (receipt_driver_size%kind == json_number) then
                call assert_true(json_number_value(receipt_driver_size) == &
                    real(expected_size, real64), 'durable PASS receipt records A size')
            else
                call assert_true(.false., 'durable PASS receipt records numeric A size')
            end if
            matched = .true.
        end do
        call assert_true(matched, 'new generation has a durable PASS receipt')
        call assert_true(.not. file_exists(marker), &
            'replaced public B marker was never invoked for build or test')
        call stop_lane(driver_a, project, cache, state, lane, session)
    end subroutine run_driver_pin_oracles

    subroutine startup_fd_oracle(scratch, b_binary)
        character(len=*), intent(in) :: scratch, b_binary
        character(:), allocatable :: test_image, helper_path, reference_path, stage_path
        character(:), allocatable :: helper_state, ready, release, result_file
        character(len=4096) :: pin_path, result_size
        character(len=HASH_LEN) :: digest, result_digest
        type(string_list_t) :: command
        integer :: child, status, unit, ios, hash_status
        integer(int64) :: expected_size
        logical :: equal, found

        call get_command_argument(0, length=status)
        if (status <= 0 .or. status > 4096) then
            call assert_true(.false., 'native test executable path is available')
            return
        end if
        allocate(character(len=status) :: test_image)
        call get_command_argument(0, test_image, status=status)
        if (status /= 0) then
            call assert_true(.false., 'native test executable path is readable')
            return
        end if
        helper_path = scratch//'/startup-image-a'
        reference_path = scratch//'/startup-image-reference'
        stage_path = scratch//'/startup-image-b'
        helper_state = scratch//'/startup-state'
        ready = scratch//'/startup-ready'
        release = scratch//'/startup-release'
        result_file = scratch//'/startup-pin-result'
        call make_directory(helper_state)
        call copy_file(test_image, helper_path)
        call copy_file(test_image, reference_path)
        call chmod_executable(helper_path)
        call sha256_file(reference_path, digest, hash_status)
        call assert_true(hash_status == 0, 'hash the helper A image before launch')
        if (hash_status /= 0) return
        expected_size = 0_int64
        inquire(file=reference_path, size=expected_size, iostat=ios)
        call assert_true(ios == 0 .and. expected_size > 0_int64, &
            'record the helper A image size independently')
        command = string_list_t()
        call list_add(command, helper_path)
        call list_add(command, '--startup-pin-helper')
        call list_add(command, ready)
        call list_add(command, release)
        call list_add(command, helper_state)
        call list_add(command, result_file)
        call spawn_process(command, scratch, child)
        call wait_for_file(ready, 10000, found)
        call assert_true(found, &
            'native helper pauses after its startup image is captured')
        if (.not. found) then
            call terminate_child(child)
            return
        end if
        call copy_file(b_binary, stage_path)
        call chmod_executable(stage_path)
        call move_path(stage_path, helper_path)
        call write_text(release, 'pin now'//new_line('a'))
        call wait_child(child, status)
        call assert_true(status == 0, &
            'startup helper pins its held image after path replacement')
        if (.not. file_exists(result_file)) return
        open(newunit=unit, file=result_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call assert_true(.false., 'startup helper writes pin identity')
            return
        end if
        read(unit, '(a)', iostat=ios) pin_path
        read(unit, '(a)', iostat=ios) result_digest
        read(unit, '(a)', iostat=ios) result_size
        close(unit)
        call assert_true(ios == 0, 'startup helper returns complete pin identity')
        if (ios /= 0) return
        call assert_equal_string(trim(result_digest), trim(digest), &
            'startup-held file descriptor pins A after its public path becomes B')
        call sha256_file(trim(pin_path), digest, hash_status)
        call assert_true(hash_status == 0, 'hash startup pin bytes independently')
        if (hash_status /= 0) return
        call files_equal(reference_path, trim(pin_path), equal)
        call assert_true(equal, 'startup pin bytes equal the pre-replacement A image')
        call assert_equal_string(trim(result_size), trim(int64_text(expected_size)), &
            'startup helper reports A original size')
        ! The helper verifies the provider's constructor-held image handle.
    end subroutine startup_fd_oracle

    subroutine startup_pin_helper()
        character(len=4096) :: ready, release, state_dir, result_file
        type(driver_pin_t) :: pin
        integer :: ierr, attempt, status, unit
        character(len=512) :: message
        logical :: found

        call get_command_argument(2, ready)
        call get_command_argument(3, release)
        call get_command_argument(4, state_dir)
        call get_command_argument(5, result_file)
        call write_text(trim(ready), 'ready'//new_line('a'))
        do attempt = 1, 1500
            inquire(file=trim(release), exist=found)
            if (found) exit
            call fs_sleep_ms(20)
        end do
        if (.not. found) error stop 31
        call driver_pin_current(trim(state_dir), pin, ierr, message)
        if (ierr /= 0) error stop 32
        open(newunit=unit, file=trim(result_file), status='replace', action='write', &
            iostat=status)
        if (status /= 0) error stop 33
        write(unit, '(a)') trim(pin%path)
        write(unit, '(a)') trim(pin%digest)
        write(unit, '(i0)') pin%size
        close(unit)
    end subroutine startup_pin_helper

    subroutine write_campaign_test(project, version)
        character(len=*), intent(in) :: project
        integer, intent(in) :: version
        character(len=1) :: value
        value = achar(48 + version)
        call write_text(project//'/test/test_driver_pin_case.f90', &
            'program test_driver_pin_case'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, parameter :: fixture_version = '//value//new_line('a')// &
            'if (fixture_version < 1) error stop 1'//new_line('a')// &
            'end program test_driver_pin_case'//new_line('a'))
    end subroutine write_campaign_test

    function marker_driver_source() result(source)
        character(:), allocatable :: source
        source = 'program fo_driver_b'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(len=4096) :: marker, command'//new_line('a')// &
            'integer :: unit'//new_line('a')// &
            "marker = ''"//new_line('a')// &
            "call get_environment_variable('FO_B_MARKER', marker)"//new_line('a')// &
            "command = ''"//new_line('a')// &
            'call get_command_argument(1, command)'//new_line('a')// &
            'if (len_trim(marker) > 0) then'//new_line('a')// &
            "open(newunit=unit, file=trim(marker), status='replace')"//new_line('a')// &
            "write(unit, '(a)') trim(command)"//new_line('a')// &
            'close(unit)'//new_line('a')// &
            'end if'//new_line('a')//'error stop 73'//new_line('a')// &
            'end program fo_driver_b'//new_line('a')
    end function marker_driver_source

    subroutine status_now(driver, project, cache, state, lane, session, document)
        character(len=*), intent(in) :: driver, project, cache, state, lane, session
        type(json_value_t), intent(out) :: document
        type(string_list_t) :: args
        type(process_result_t) :: process
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, session)
        call list_add(args, '--cursor')
        call list_add(args, '0')
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, document, process, 30000)
    end subroutine status_now

    subroutine wait_for_pass(driver, project, cache, state, lane, session, previous, &
            generation, document)
        character(len=*), intent(in) :: driver, project, cache, state, lane, session, &
            previous
        character(:), allocatable, intent(out) :: generation
        type(json_value_t), intent(out) :: document
        type(json_value_t) :: events, event
        integer :: attempt, j

        generation = ''
        do attempt = 1, 600
            call status_now(driver, project, cache, state, lane, session, document)
            generation = gremlin_field(document, 'active_generation')
            events = json_member(document, 'events')
            do j = 1, json_size(events)
                event = json_element(events, j)
                if (gremlin_field(event, 'case_id') /= 'test_driver_pin_case') cycle
                if (gremlin_field(event, 'generation') /= generation) cycle
                if (gremlin_field(event, 'status') /= 'PASS') cycle
                if (len(previous) > 0 .and. generation == previous) cycle
                return
            end do
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., &
            'A campaign publishes a PASS receipt for its current generation')
    end subroutine wait_for_pass

    subroutine stop_lane(driver, project, cache, state, lane, session)
        character(len=*), intent(in) :: driver, project, cache, state, lane, session
        type(string_list_t) :: args
        type(process_result_t) :: process
        type(json_value_t) :: document
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'stop')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, session)
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, document, process, 30000)
    end subroutine stop_lane

    subroutine copy_file(source, destination)
        character(len=*), intent(in) :: source, destination
        character(len=65536) :: bytes
        integer(int64) :: total = 0_int64, offset, amount
        integer :: input_unit, output_unit, ios

        open(newunit=input_unit, file=trim(source), status='old', action='read', &
            access='stream', form='unformatted', iostat=ios)
        call assert_true(ios == 0, 'opens binary copy source')
        if (ios /= 0) return
        inquire(unit=input_unit, size=total, iostat=ios)
        call assert_true(ios == 0 .and. total > 0_int64, 'measures binary copy source')
        if (ios /= 0) then
            close(input_unit)
            return
        end if
        open(newunit=output_unit, file=trim(destination), status='replace', &
            action='write', &
            access='stream', form='unformatted', iostat=ios)
        call assert_true(ios == 0, 'creates binary copy destination')
        if (ios /= 0) then
            close(input_unit)
            return
        end if
        offset = 1_int64
        do while (offset <= total)
            amount = min(int(len(bytes), int64), total - offset + 1_int64)
            read(input_unit, pos=offset, iostat=ios) bytes(:int(amount))
            if (ios /= 0) exit
            write(output_unit, iostat=ios) bytes(:int(amount))
            if (ios /= 0) exit
            offset = offset + amount
        end do
        close(input_unit)
        close(output_unit)
        call assert_true(ios == 0 .and. offset > total, 'copies all binary image bytes')
    end subroutine copy_file

    subroutine chmod_executable(path)
        character(len=*), intent(in) :: path
        integer(c_int) :: rc
        rc = c_chmod(trim(path)//c_null_char, 365_c_int)
        call assert_true(rc == 0, 'sets native fixture executable mode')
    end subroutine chmod_executable

    subroutine files_equal(left, right, equal)
        character(len=*), intent(in) :: left, right
        logical, intent(out) :: equal
        character(len=65536) :: a, b
        integer(int64) :: left_size = 0_int64, right_size = 0_int64, offset, amount
        integer :: left_unit, right_unit, ios_left, ios_right

        equal = .false.
        inquire(file=trim(left), size=left_size, iostat=ios_left)
        inquire(file=trim(right), size=right_size, iostat=ios_right)
        if (ios_left /= 0 .or. ios_right /= 0 .or. left_size /= right_size) return
        open(newunit=left_unit, file=trim(left), status='old', action='read', &
            access='stream', form='unformatted', iostat=ios_left)
        open(newunit=right_unit, file=trim(right), status='old', action='read', &
            access='stream', form='unformatted', iostat=ios_right)
        if (ios_left /= 0) then
            if (ios_right == 0) close(right_unit)
            return
        end if
        if (ios_right /= 0) then
            close(left_unit)
            return
        end if
        equal = .true.
        offset = 1_int64
        do while (offset <= left_size)
            amount = min(int(len(a), int64), left_size - offset + 1_int64)
            read(left_unit, pos=offset, iostat=ios_left) a(:int(amount))
            read(right_unit, pos=offset, iostat=ios_right) b(:int(amount))
            if (ios_left /= 0 .or. ios_right /= 0) then
                equal = .false.
                exit
            end if
            if (a(:int(amount)) /= b(:int(amount))) then
                equal = .false.
                exit
            end if
            offset = offset + amount
        end do
        close(left_unit)
        close(right_unit)
    end subroutine files_equal

    subroutine wait_for_file(path, timeout_ms, found)
        character(len=*), intent(in) :: path
        integer, intent(in) :: timeout_ms
        logical, intent(out) :: found
        integer :: elapsed
        found = .false.
        do elapsed = 0, timeout_ms, 20
            inquire(file=trim(path), exist=found)
            if (found) return
            call fs_sleep_ms(20)
        end do
    end subroutine wait_for_file

    subroutine wait_child(child, status)
        integer, intent(inout) :: child
        integer, intent(out) :: status
        integer :: attempt
        status = 999
        do attempt = 1, 1500
            call poll_process(child, status)
            if (status /= 999) return
            call fs_sleep_ms(20)
        end do
        call terminate_child(child)
        status = -1
    end subroutine wait_child

    subroutine terminate_child(child)
        integer, intent(inout) :: child
        integer :: status
        logical :: reaped
        call terminate_process_group(child, status, reaped)
    end subroutine terminate_child

    function int64_text(value) result(text)
        integer(int64), intent(in) :: value
        character(len=32) :: text
        write(text, '(i0)') value
    end function int64_text

end program test_gremlin_driver_pin
