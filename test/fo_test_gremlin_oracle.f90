module fo_test_gremlin_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_null_char, c_null_ptr, c_loc
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory, write_text
    use fo_test_harness, only: read_text, file_exists, run_process, assert_true
    use fo_test_json, only: json_value_t, json_parse, json_member, json_string_value
    implicit none
    private

    public :: gremlin_setup, gremlin_run, gremlin_json, gremlin_write_case
    public :: gremlin_wait_file, gremlin_wait_ms, gremlin_field, gremlin_start_args
    public :: gremlin_spawn, gremlin_wait_child, gremlin_poll_child
    public :: gremlin_fifo, gremlin_release_fifo
    public :: gremlin_stop_child
    public :: gremlin_stop_lane
    public :: gremlin_process_running

    interface
        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
        integer(c_int) function c_usleep(microseconds) bind(C, name='usleep')
            import :: c_int
            integer(c_int), value :: microseconds
        end function c_usleep
        integer(c_int) function c_waitpid(process, status, options) &
                bind(C, name='fo_test_waitpid')
            import :: c_int
            integer(c_int), value :: process, options
            integer(c_int), intent(out) :: status
        end function c_waitpid
        integer(c_int) function c_spawn(arguments, cwd, stdout_path, stderr_path) &
                bind(C, name='fo_test_spawn_capture')
            import :: c_char, c_int, c_ptr
            type(c_ptr), intent(in) :: arguments(*)
            character(kind=c_char), intent(in) :: cwd(*), stdout_path(*), stderr_path(*)
        end function c_spawn
        integer(c_int) function c_mkfifo(path, mode) bind(C, name='mkfifo')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_mkfifo
        integer(c_int) function c_signal_group(process, signal_number) &
                bind(C, name='fo_test_signal_group')
            import :: c_int
            integer(c_int), value :: process, signal_number
        end function c_signal_group
        integer(c_int) function c_release_fifo(path, timeout) &
                bind(C, name='fo_test_release_fifo')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: timeout
        end function c_release_fifo
        integer(c_int) function c_process_running(pid) &
                bind(C, name='fo_test_process_running')
            import :: c_int
            integer(c_int), value :: pid
        end function c_process_running
    end interface

contains

    logical function gremlin_process_running(pid)
        integer, intent(in) :: pid
        gremlin_process_running = c_process_running(int(pid, c_int)) == 1
    end function gremlin_process_running

    subroutine gremlin_setup(driver, scratch, project, cache, state, capture_counter)
        character(:), allocatable, intent(out) :: driver, scratch, project, cache, state
        character(:), allocatable, optional, intent(out) :: capture_counter
        integer :: count, length, status
        character(:), allocatable :: env_fo
        character(len=512) :: value

        count = command_argument_count()
        if (count > 0) then
            call get_command_argument(1, length=length, status=status)
        else
            length = 0
            status = 1
        end if
        if (status == 0 .and. length > 0) then
            allocate(character(len=length) :: driver)
            call get_command_argument(1, driver, status=status)
        else
            call get_environment_variable('FO', length=length, status=status)
            if (status == 0 .and. length > 0) then
                allocate(character(len=length) :: env_fo)
                call get_environment_variable('FO', env_fo, status=status)
                driver = trim(env_fo)
            else
                driver = 'fo'
            end if
        end if
        call make_scratch('fo-gremlin-fortran-oracle', scratch)
        project = join_path(scratch, 'project')
        cache = join_path(scratch, 'cache')
        state = join_path(scratch, 'state')
        call make_directory(project)
        call make_directory(cache)
        call make_directory(state)
        call make_directory(join_path(state, 'tmp'))
        call make_directory(join_path(scratch, 'home'))
        call make_directory(join_path(scratch, 'xdg-cache'))
        call set_env('HOME', join_path(scratch, 'home'))
        call set_env('XDG_CACHE_HOME', join_path(scratch, 'xdg-cache'))
        call set_env('FO_PREFIX', join_path(scratch, 'prefix'))
        call set_env('FO_CACHE_DIR', cache)
        call set_env('FO_GREMLIN_STATE_DIR', state)
        call set_env('FO_JOBS', '1')
        ! Outer fixture budgets must not override the fixture children's own budgets.
        call set_env('FO_TEST_TIMEOUT', '')
        call set_env('FO_TEST_WALL_TIMEOUT', '')
        call set_env('FO_SLOW_TEST_TIMEOUT', '')
        call set_env('FO_SLOW_TEST_WALL_TIMEOUT', '')
        call set_env('FO_SELF_REFRESH', '0')
        call set_env('FO_DISABLE_SELF_REFRESH', '1')
        call set_env('TMPDIR', join_path(state, 'tmp'))
        if (present(capture_counter)) then
            capture_counter = join_path(scratch, 'capture-counter.json')
            call set_env('FO_GREMLIN_TEST_CAPTURE_COUNTER', capture_counter)
        end if
    end subroutine gremlin_setup

    subroutine set_env(name, value)
        character(len=*), intent(in) :: name, value
        integer(c_int) :: result

        result = c_setenv(trim(name)//c_null_char, trim(value)//c_null_char, 1_c_int)
        call assert_true(result == 0, 'sets isolated fixture environment: '//trim(name))
    end subroutine set_env

    subroutine gremlin_run(driver, cwd, cache, state, arguments, result, environment, timeout)
        character(len=*), intent(in) :: driver, cwd, cache, state
        type(string_list_t), intent(in) :: arguments
        type(process_result_t), intent(out) :: result
        type(string_list_t), optional, intent(in) :: environment
        integer, optional, intent(in) :: timeout
        type(string_list_t) :: command, child_environment
        integer :: i

        call list_add(command, trim(driver))
        if (allocated(arguments%items)) then
            do i = 1, size(arguments%items)
                call list_add(command, arguments%items(i)%value)
            end do
        end if
        call list_add(child_environment, 'FO_CACHE_DIR='//trim(cache))
        call list_add(child_environment, 'FO_GREMLIN_STATE_DIR='//trim(state))
        call list_add(child_environment, 'FO_DISABLE_SELF_REFRESH=1')
        call list_add(child_environment, 'FO_SELF_REFRESH=0')
        call list_add(child_environment, 'FO_JOBS=1')
        call list_add(child_environment, 'TMPDIR='//join_path(state, 'tmp'))
        if (present(environment)) then
            if (allocated(environment%items)) then
                do i = 1, size(environment%items)
                    call list_add(child_environment, environment%items(i)%value)
                end do
            end if
        end if
        if (present(timeout)) then
            call run_process(command, cwd, result, child_environment, timeout_ms=timeout)
        else
            call run_process(command, cwd, result, child_environment)
        end if
    end subroutine gremlin_run

    subroutine gremlin_json(driver, cwd, cache, state, arguments, document, result, timeout)
        character(len=*), intent(in) :: driver, cwd, cache, state
        type(string_list_t), intent(in) :: arguments
        type(json_value_t), intent(out) :: document
        type(process_result_t), intent(out) :: result
        integer, optional, intent(in) :: timeout
        character(:), allocatable :: message
        logical :: valid

        call gremlin_run(driver, cwd, cache, state, arguments, result, timeout=timeout)
        call assert_true(.not. result%runner_failed, 'fo process runner succeeds')
        call assert_true(.not. result%timed_out, 'fo command completes within its bound')
        call json_parse(result%stdout, document, valid, message)
        if (.not. valid) call assert_true(.false., 'fo returns parseable JSON: '//message)
        if (valid .and. result%exit_code == 0) &
            call await_admission(driver, cwd, cache, state, arguments)
    end subroutine gremlin_json

    subroutine await_admission(driver, cwd, cache, state, arguments)
        !! A started owner may queue for a host work slot held by other lanes.
        !! Fixture barriers measure the owner's work, so wait out that queue.
        character(len=*), intent(in) :: driver, cwd, cache, state
        type(string_list_t), intent(in) :: arguments
        character(len=*), parameter :: waiting = &
            'waiting for a host Gremlin work slot held by other lanes'
        type(string_list_t) :: status_arguments
        type(process_result_t) :: status_result
        type(json_value_t) :: status
        character(:), allocatable :: message
        integer :: i, attempt
        logical :: valid

        if (.not. allocated(arguments%items)) return
        if (size(arguments%items) < 2) return
        if (arguments%items(1)%value /= 'gremlin' .or. &
            arguments%items(2)%value /= 'start') return
        call list_add(status_arguments, 'gremlin')
        call list_add(status_arguments, 'status')
        do i = 3, size(arguments%items) - 1
            if (arguments%items(i)%value /= '--dir' .and. &
                arguments%items(i)%value /= '--lane') cycle
            call list_add(status_arguments, arguments%items(i)%value)
            call list_add(status_arguments, arguments%items(i + 1)%value)
        end do
        do attempt = 1, 36000
            call gremlin_run(driver, cwd, cache, state, status_arguments, status_result)
            if (status_result%exit_code /= 0) return
            call json_parse(status_result%stdout, status, valid, message)
            if (.not. valid) return
            if (gremlin_field(status, 'diagnostic') /= waiting) return
            call gremlin_wait_ms(100)
        end do
    end subroutine await_admission

    function gremlin_field(document, key) result(value)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: key
        character(:), allocatable :: value
        type(json_value_t) :: item

        item = json_member(document, key)
        value = json_string_value(item)
    end function gremlin_field

    subroutine gremlin_start_args(arguments, project, lane, target)
        type(string_list_t), intent(out) :: arguments
        character(len=*), intent(in) :: project, lane, target

        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'start')
        call list_add(arguments, '--dir')
        call list_add(arguments, trim(project))
        call list_add(arguments, '--lane')
        call list_add(arguments, trim(lane))
        call list_add(arguments, '--target')
        call list_add(arguments, trim(target))
        call list_add(arguments, '--campaign-seconds')
        call list_add(arguments, '60')
        call list_add(arguments, '--timeout-seconds')
        call list_add(arguments, '10')
    end subroutine gremlin_start_args

    subroutine gremlin_write_case(project, name, body)
        character(len=*), intent(in) :: project, name, body
        character(:), allocatable :: source

        call make_directory(join_path(project, 'test'))
        source = 'program '//trim(name)//new_line('a')//'implicit none'//new_line('a')// &
            trim(body)//new_line('a')//'end program '//trim(name)//new_line('a')
        call write_text(join_path(project, 'test/'//trim(name)//'.f90'), source)
    end subroutine gremlin_write_case

    subroutine gremlin_wait_file(path, timeout_ms, found)
        character(len=*), intent(in) :: path
        integer, intent(in) :: timeout_ms
        logical, intent(out) :: found
        integer :: elapsed, result
        character(:), allocatable :: text

        found = .false.
        elapsed = 0
        do while (elapsed < timeout_ms)
            if (file_exists(path)) then
                text = read_text(path)
                if (len(text) > 0) then
                    found = text(len(text):) == new_line('a')
                    if (found) return
                end if
            end if
            result = c_usleep(20000_c_int)
            elapsed = elapsed + 20
        end do
    end subroutine gremlin_wait_file

    subroutine gremlin_wait_ms(duration_ms)
        integer, intent(in) :: duration_ms
        integer :: result, remaining

        remaining = max(0, duration_ms)
        do while (remaining > 0)
            result = c_usleep(int(min(remaining, 200), c_int) * 1000_c_int)
            remaining = remaining - min(remaining, 200)
        end do
    end subroutine gremlin_wait_ms

    subroutine gremlin_spawn(driver, cwd, arguments, stdout_path, stderr_path, process_id)
        character(len=*), intent(in) :: driver, cwd, stdout_path, stderr_path
        type(string_list_t), intent(in) :: arguments
        integer, intent(out) :: process_id
        type(c_ptr), allocatable, target :: pointers(:)
        character(kind=c_char), allocatable, target :: storage(:, :)
        integer :: count, width, i, j

        count = 1
        width = len_trim(driver)
        if (allocated(arguments%items)) then
            count = count + size(arguments%items)
            do i = 1, size(arguments%items)
                width = max(width, len(arguments%items(i)%value))
            end do
        end if
        allocate(storage(width + 1, count), pointers(count + 1))
        storage = c_null_char
        call copy_argument(driver, storage(:, 1))
        pointers(1) = c_loc(storage(1, 1))
        do i = 2, count
            call copy_argument(arguments%items(i - 1)%value, storage(:, i))
            pointers(i) = c_loc(storage(1, i))
        end do
        pointers(count + 1) = c_null_ptr
        process_id = c_spawn(pointers, trim(cwd)//c_null_char, &
            trim(stdout_path)//c_null_char, trim(stderr_path)//c_null_char)
        call assert_true(process_id > 0, 'launches concurrent fo request')
    end subroutine gremlin_spawn

    subroutine copy_argument(value, target)
        character(len=*), intent(in) :: value
        character(kind=c_char), intent(out) :: target(:)
        integer :: i

        target = c_null_char
        do i = 1, len_trim(value)
            target(i) = value(i:i)
        end do
    end subroutine copy_argument

    subroutine gremlin_wait_child(process_id, exit_code)
        integer, intent(in) :: process_id
        integer, intent(out) :: exit_code
        integer(c_int) :: status, waited

        waited = c_waitpid(int(process_id, c_int), status, 0_c_int)
        call assert_true(waited == process_id, 'reaps concurrently launched fo request')
        exit_code = ibits(status, 8, 8)
    end subroutine gremlin_wait_child

    subroutine gremlin_stop_child(process_id, exit_code)
        integer, intent(in) :: process_id
        integer, intent(out) :: exit_code
        integer(c_int) :: result

        result = c_signal_group(int(process_id, c_int), 15_c_int)
        call assert_true(result == 0, 'signals the owned child process group')
        call gremlin_wait_child(process_id, exit_code)
    end subroutine gremlin_stop_child

    subroutine gremlin_poll_child(process_id, done, exit_code)
        integer, intent(in) :: process_id
        logical, intent(out) :: done
        integer, intent(out) :: exit_code
        integer(c_int) :: status, waited

        waited = c_waitpid(int(process_id, c_int), status, 1_c_int)
        done = waited == process_id
        exit_code = -1
        if (done) exit_code = ibits(status, 8, 8)
    end subroutine gremlin_poll_child

    subroutine gremlin_fifo(path)
        character(len=*), intent(in) :: path
        integer(c_int) :: result

        result = c_mkfifo(trim(path)//c_null_char, int(o'600', c_int))
        call assert_true(result == 0, 'creates FIFO process barrier')
    end subroutine gremlin_fifo

    subroutine gremlin_release_fifo(path)
        character(len=*), intent(in) :: path
        integer(c_int) :: status
        status = c_release_fifo(trim(path)//c_null_char, 5000_c_int)
        call assert_true(status == 0, 'releases FIFO with a live reader within its bound')
    end subroutine gremlin_release_fifo

    subroutine gremlin_stop_lane(driver, project, cache, state, lane, owner, &
            allow_terminal_error)
        character(len=*), intent(in) :: driver, project, cache, state, lane, owner
        logical, optional, intent(in) :: allow_terminal_error
        type(string_list_t) :: args
        type(process_result_t) :: process, stop_process
        type(json_value_t) :: reply, stop_reply
        integer :: attempt, owner_pid, ios, dash
        logical :: accept_error

        call list_add(args, 'gremlin')
        call list_add(args, 'stop')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, owner)
        call gremlin_json(driver, project, cache, state, args, stop_reply, &
            stop_process, 10000)
        call assert_true(stop_process%exit_code == 0, &
            'requests shutdown of only the owned lane')
        args%items(2)%value = 'status'
        owner_pid = -1
        accept_error = .false.
        if (present(allow_terminal_error)) accept_error = allow_terminal_error
        ios = 1
        dash = index(owner, '-')
        if (dash > 1) then
            read(owner(:dash - 1), *, iostat=ios) owner_pid
            call assert_true(ios == 0, 'owner ID exposes its process identity')
        end if
        do attempt = 1, 100
            call gremlin_json(driver, project, cache, state, args, reply, process, 10000)
            if (process%exit_code /= 0) exit
            if (gremlin_field(reply, 'state') == 'stopped') then
                if (.not. gremlin_process_running(owner_pid)) return
            else if (accept_error) then
                if (gremlin_field(reply, 'state') == 'error') then
                    if (.not. gremlin_process_running(owner_pid)) then
                        write(error_unit, '(a)') 'intentional failed-input shutdown: '// &
                            'last_outcome='//gremlin_field(reply, 'last_outcome')// &
                            ' last_exitcode='//gremlin_field(reply, 'last_exitcode')// &
                            ' diagnostic='//gremlin_field(reply, 'diagnostic')
                        return
                    end if
                end if
            end if
            call gremlin_wait_ms(50)
        end do
        call gremlin_json(driver, project, cache, state, args, reply, process, 10000)
        write(error_unit, '(a)') 'Gremlin fixture shutdown diagnostic: project='// &
            trim(project)//' owner='//trim(owner)
        write(error_unit, '(a,i0)') 'stop command exit: ', stop_process%exit_code
        write(error_unit, '(a)') 'stop response state: '// &
            gremlin_field(stop_reply, 'state')
        write(error_unit, '(a,i0)') 'fresh status command exit: ', process%exit_code
        write(error_unit, '(a)') 'fresh status state: '//gremlin_field(reply, 'state')
        write(error_unit, '(a)') 'fresh status phase: '//gremlin_field(reply, 'phase')
        write(error_unit, '(a)') 'owner process stat: '//test_process_stat(owner_pid)
        call assert_true(.false., &
            'owned supervisor finishes shutdown before fixture cleanup')
    end subroutine gremlin_stop_lane

    function test_process_stat(pid) result(text)
        integer, intent(in) :: pid
        character(:), allocatable :: text
        character(len=64) :: pid_text
        character(len=2048) :: record
        integer :: unit, ios

        write(pid_text, '(i0)') pid
        open (newunit=unit, file='/proc/'//trim(pid_text)//'/stat', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) then
            text = 'absent'
            return
        end if
        read (unit, '(a)', iostat=ios) record
        close (unit)
        if (ios /= 0) then
            text = 'unreadable'
        else
            text = trim(record)
        end if
    end function test_process_stat

end module fo_test_gremlin_oracle
