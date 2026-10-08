module fo_test_harness
    use fo_test_os_link, only: test_os_link_probe
    use, intrinsic :: iso_c_binding, only: c_char, c_short, c_int, c_int64_t, c_size_t, &
        c_ptr, c_loc, c_null_char, c_null_ptr
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    public :: string_list_t, process_result_t, list_add, list_extend, list_with_first
    public :: make_scratch, join_path
    public :: make_directory, make_symlink, move_path, remove_path, remove_tree
    public :: current_directory, write_text, write_lines, append_text, read_text
    public :: file_exists, environment_value, run_process, assert_true, assert_equal_string
    public :: assert_equal_integer, assert_contains, assert_not_contains
    public :: assert_file_exists, assert_file_absent, assert_file_equals
    public :: assert_process_ok, finish_assertions, register_scratch
    public :: process_alive, start_sentinel, stop_sentinel
    public :: spawn_process, poll_process, signal_process_group
    public :: terminate_process_group, spawn_heartbeat_process
    public :: reset_assertions_for_probe
    public :: exercise_failure_cleanup_probe, open_descriptor_count

    type :: string_t
        character(:), allocatable :: value
    end type string_t

    type :: string_list_t
        type(string_t), allocatable :: items(:)
    end type string_list_t

    type :: process_result_t
        character(:), allocatable :: stdout
        character(:), allocatable :: stderr
        integer :: exit_code = -1
        integer :: term_signal = 0
        logical :: timed_out = .false.
        logical :: runner_failed = .false.
        logical :: reaped = .false.
        integer :: process_id = -1
        integer(c_int64_t) :: elapsed_ms = 0
        character(:), allocatable :: runner_error
    end type process_result_t

    type(string_t), allocatable, save :: failures(:)
    type(string_t), allocatable, save :: scratch_paths(:)

    type, bind(C) :: pollfd_t
        integer(c_int) :: descriptor
        integer(c_short) :: events
        integer(c_short) :: returned_events
    end type pollfd_t

    type :: byte_buffer_t
        character(kind=c_char), allocatable :: bytes(:)
        integer :: length = 0
    end type byte_buffer_t

    interface
        integer(c_int) function open_descriptor_count() bind(C, name='fo_test_open_fds')
            import :: c_int
        end function open_descriptor_count

        integer(c_int) function c_mkdtemp(pattern) bind(C, name='fo_test_mkdtemp')
            import :: c_char, c_int
            character(kind=c_char), intent(inout) :: pattern(*)
        end function c_mkdtemp

        integer(c_int) function c_mkdirs(path) bind(C, name='fo_test_mkdirs')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_mkdirs

        integer(c_int) function c_remove_tree(path) bind(C, name='fo_test_remove_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_remove_tree

        integer(c_int) function c_unlink(path) bind(C, name='fo_test_unlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_unlink

        integer(c_int) function c_rename(source, target) bind(C, name='fo_test_rename')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: source(*), target(*)
        end function c_rename

        integer(c_int) function c_symlink(target, link_path) bind(C, name='fo_test_symlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: target(*), link_path(*)
        end function c_symlink

        integer(c_int) function c_getcwd(buffer, size) bind(C, name='fo_test_getcwd')
            import :: c_char, c_int, c_size_t
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: size
        end function c_getcwd

        integer(c_int) function c_pipe(descriptors) bind(C, name='fo_test_pipe_cloexec')
            import :: c_int
            integer(c_int), intent(out) :: descriptors(2)
        end function c_pipe

        integer(c_int) function c_process_containment_required() &
                bind(C, name='fo_c_process_containment_required')
            import :: c_int
        end function c_process_containment_required

        integer(c_int) function c_start_capture_monitor(monitor, cwd, arguments, &
                environment, input_descriptor, output_descriptor, error_descriptor) &
                bind(C, name='fo_c_start_capture_monitor')
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: monitor(*), cwd(*)
            type(c_ptr), intent(in) :: arguments(*), environment(*)
            integer(c_int), value :: input_descriptor, output_descriptor, error_descriptor
        end function c_start_capture_monitor

        integer(c_int) function c_fork() bind(C, name='fork')
            import :: c_int
        end function c_fork

        integer(c_int) function c_dup2(old_descriptor, new_descriptor) bind(C, name='dup2')
            import :: c_int
            integer(c_int), value :: old_descriptor, new_descriptor
        end function c_dup2

        integer(c_int) function c_close(descriptor) bind(C, name='close')
            import :: c_int
            integer(c_int), value :: descriptor
        end function c_close

        integer(c_int) function c_chdir(path) bind(C, name='chdir')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_chdir

        integer(c_int) function c_setpgid(process, group) bind(C, name='setpgid')
            import :: c_int
            integer(c_int), value :: process, group
        end function c_setpgid

        integer(c_int) function c_execvp(path, arguments) bind(C, name='execvp')
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            type(c_ptr), intent(in) :: arguments(*)
        end function c_execvp

        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv

        subroutine c_exit(status) bind(C, name='_exit')
            import :: c_int
            integer(c_int), value :: status
        end subroutine c_exit

        integer(c_int) function c_kill(process, signal_number) bind(C, name='kill')
            import :: c_int
            integer(c_int), value :: process, signal_number
        end function c_kill

        integer(c_int) function c_waitpid_retry(process, status, options) &
                bind(C, name='fo_test_waitpid')
            import :: c_int
            integer(c_int), value :: process, options
            integer(c_int), intent(out) :: status
        end function c_waitpid_retry

        integer(c_int) function c_set_nonblocking(descriptor) bind(C, name='fo_test_set_nonblocking')
            import :: c_int
            integer(c_int), value :: descriptor
        end function c_set_nonblocking

        integer(c_int) function c_ignore_sigpipe() bind(C, name='fo_test_ignore_sigpipe')
            import :: c_int
        end function c_ignore_sigpipe

        integer(c_int) function c_default_sigpipe() bind(C, name='fo_test_default_sigpipe')
            import :: c_int
        end function c_default_sigpipe

        subroutine c_child_setup_error(fd, operation) bind(C, name='fo_test_child_setup_error')
            import :: c_int
            integer(c_int), value :: fd, operation
        end subroutine c_child_setup_error

        integer(c_int) function c_silence_output() bind(C, name='fo_test_silence_output')
            import :: c_int
        end function c_silence_output

        integer(c_int64_t) function c_read(descriptor, buffer, capacity) bind(C, name='fo_test_read')
            import :: c_char, c_int, c_int64_t, c_size_t
            integer(c_int), value :: descriptor
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: capacity
        end function c_read

        integer(c_int64_t) function c_write(descriptor, buffer, length) bind(C, name='fo_test_write')
            import :: c_char, c_int, c_int64_t, c_size_t
            integer(c_int), value :: descriptor
            character(kind=c_char), intent(in) :: buffer(*)
            integer(c_size_t), value :: length
        end function c_write

        integer(c_int) function c_poll(descriptors, count, timeout_ms) bind(C, name='fo_test_poll')
            import :: c_int, c_size_t, pollfd_t
            type(pollfd_t), intent(inout) :: descriptors(*)
            integer(c_size_t), value :: count
            integer(c_int), value :: timeout_ms
        end function c_poll

        integer(c_int64_t) function c_monotonic_ms() bind(C, name='fo_test_monotonic_ms')
            import :: c_int64_t
        end function c_monotonic_ms

        integer(c_int) function c_spawn_process(arguments, environment, directory) &
                bind(C, name='fo_test_spawn')
            import :: c_char, c_int, c_ptr
            type(c_ptr), intent(in) :: arguments(*), environment(*)
            character(kind=c_char), intent(in) :: directory(*)
        end function c_spawn_process

        integer(c_int) function c_signal_group(process, signal_number) &
                bind(C, name='fo_test_signal_group')
            import :: c_int
            integer(c_int), value :: process, signal_number
        end function c_signal_group

        integer(c_int) function c_wait_nonblocking(process, status) &
                bind(C, name='fo_test_wait_nonblocking')
            import :: c_int
            integer(c_int), value :: process
            integer(c_int), intent(out) :: status
        end function c_wait_nonblocking

        integer(c_int) function c_wait_blocking(process) bind(C, name='fo_test_wait_blocking')
            import :: c_int
            integer(c_int), value :: process
        end function c_wait_blocking

        integer(c_int) function c_spawn_heartbeat(path, directory) &
                bind(C, name='fo_test_spawn_heartbeat')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*), directory(*)
        end function c_spawn_heartbeat
    end interface

contains

    subroutine spawn_process(command, directory, process_id, environment)
        type(string_list_t), intent(in) :: command
        character(len=*), intent(in) :: directory
        integer, intent(out) :: process_id
        type(string_list_t), optional, intent(in) :: environment
        type(c_ptr), allocatable, target :: pointers(:)
        type(c_ptr), allocatable, target :: environment_pointers(:)
        character(kind=c_char), allocatable, target :: storage(:, :), environment_storage(:, :)
        character(kind=c_char), allocatable, target :: directory_bytes(:)
        type(string_list_t) :: empty_environment
        integer(c_int) :: child

        call make_c_vector(command, pointers, storage)
        if (present(environment)) then
            call make_c_vector(environment, environment_pointers, environment_storage)
        else
            call make_c_vector(empty_environment, environment_pointers, environment_storage)
        end if
        call encode_c_string(directory, directory_bytes)
        child = c_spawn_process(pointers, environment_pointers, directory_bytes)
        process_id = int(child)
        call assert_true(process_id > 0, 'start fixture process')
    end subroutine spawn_process

    subroutine poll_process(process_id, status)
        integer, intent(in) :: process_id
        integer, intent(out) :: status
        integer(c_int) :: child_status, waited

        child_status = 0
        waited = c_wait_nonblocking(int(process_id, c_int), child_status)
        status = int(waited)
    end subroutine poll_process

    subroutine signal_process_group(process_id, signal_number)
        integer, intent(in) :: process_id, signal_number
        integer(c_int) :: rc

        rc = c_signal_group(int(process_id, c_int), int(signal_number, c_int))
        call assert_true(rc == 0, 'signal fixture process group')
    end subroutine signal_process_group

    subroutine terminate_process_group(process_id, status, owned_reaped)
        integer, intent(inout) :: process_id
        integer, intent(out) :: status
        logical, intent(out) :: owned_reaped
        integer(c_int) :: rc
        integer :: attempt
        type(pollfd_t) :: no_descriptors(1)

        status = -999
        owned_reaped = process_id <= 0
        if (owned_reaped) then
            process_id = -1
            return
        end if
        rc = c_signal_group(int(process_id, c_int), 15_c_int)
        do attempt = 1, 50
            rc = c_poll(no_descriptors, 0_c_size_t, 20_c_int)
        end do
        rc = c_signal_group(int(process_id, c_int), 9_c_int)
        do attempt = 1, 100
            call poll_process(process_id, status)
            if (status /= 999) exit
            rc = c_poll(no_descriptors, 0_c_size_t, 20_c_int)
        end do
        if (status == 999 .or. status == -999 .or. status == -998) then
            status = int(c_wait_blocking(int(process_id, c_int)))
        end if
        owned_reaped = status /= -999 .and. status /= -998 .and. status /= 999
        if (owned_reaped) process_id = -1
    end subroutine terminate_process_group

    subroutine spawn_heartbeat_process(path, directory, process_id)
        character(len=*), intent(in) :: path, directory
        integer, intent(out) :: process_id
        character(kind=c_char), allocatable, target :: path_bytes(:), directory_bytes(:)
        integer(c_int) :: child

        call encode_c_string(path, path_bytes)
        call encode_c_string(directory, directory_bytes)
        child = c_spawn_heartbeat(path_bytes, directory_bytes)
        process_id = int(child)
        call assert_true(process_id > 0, 'start test heartbeat process')
    end subroutine spawn_heartbeat_process

    subroutine list_add(list, value)
        type(string_list_t), intent(inout) :: list
        character(len=*), intent(in) :: value
        type(string_t), allocatable :: grown(:)
        integer :: count

        count = 0
        if (allocated(list%items)) count = size(list%items)
        allocate(grown(count + 1))
        if (count > 0) grown(1:count) = list%items
        grown(count + 1)%value = value
        call move_alloc(grown, list%items)
    end subroutine list_add

    subroutine list_extend(list, additional)
        type(string_list_t), intent(inout) :: list
        type(string_list_t), intent(in) :: additional
        integer :: i

        if (.not. allocated(additional%items)) return
        do i = 1, size(additional%items)
            call list_add(list, additional%items(i)%value)
        end do
    end subroutine list_extend

    subroutine list_with_first(first, rest, combined)
        character(len=*), intent(in) :: first
        type(string_list_t), intent(in) :: rest
        type(string_list_t), intent(out) :: combined

        call list_add(combined, first)
        call list_extend(combined, rest)
    end subroutine list_with_first

    subroutine encode_c_string(value, bytes)
        character(len=*), intent(in) :: value
        character(kind=c_char), allocatable, target, intent(out) :: bytes(:)
        integer :: i

        allocate(bytes(len(value) + 1))
        do i = 1, len(value)
            bytes(i) = value(i:i)
        end do
        bytes(len(value) + 1) = c_null_char
    end subroutine encode_c_string

    subroutine make_c_vector(list, pointers, storage)
        type(string_list_t), intent(in) :: list
        type(c_ptr), allocatable, target, intent(out) :: pointers(:)
        character(kind=c_char), allocatable, target, intent(out) :: storage(:, :)
        integer :: count, width, i, j

        count = 0
        width = 0
        if (allocated(list%items)) then
            count = size(list%items)
            do i = 1, count
                width = max(width, len(list%items(i)%value))
            end do
        end if
        allocate(storage(width + 1, max(1, count)))
        storage = c_null_char
        allocate(pointers(count + 1))
        do i = 1, count
            do j = 1, len(list%items(i)%value)
                storage(j, i) = list%items(i)%value(j:j)
            end do
            pointers(i) = c_loc(storage(1, i))
        end do
        pointers(count + 1) = c_null_ptr
    end subroutine make_c_vector

    subroutine run_process(arguments, cwd, result, environment, input, timeout_ms, &
            max_output_bytes, child_start_delay_ms)
        type(string_list_t), intent(in) :: arguments
        character(len=*), intent(in) :: cwd
        type(process_result_t), intent(out) :: result
        type(string_list_t), optional, intent(in) :: environment
        character(len=*), optional, intent(in) :: input
        integer, optional, intent(in) :: timeout_ms
        integer(c_int64_t), optional, intent(in) :: max_output_bytes
        integer, optional, intent(in) :: child_start_delay_ms
        type(string_list_t) :: empty_environment
        type(c_ptr), allocatable, target :: argument_pointers(:), env_pointers(:)
        character(kind=c_char), allocatable, target :: argument_storage(:, :)
        character(kind=c_char), allocatable, target :: env_storage(:, :)
        character(kind=c_char), allocatable, target :: cwd_bytes(:), input_storage(:), monitor_bytes(:)
        character(kind=c_char), target :: read_chunk(16384)
        type(pollfd_t) :: poll_descriptors(3)
        integer(c_int) :: input_pipe(2), output_pipe(2), error_pipe(2)
        integer(c_int) :: child, wait_status, waited, rc, containment_status
        integer(c_int) :: input_descriptor, output_descriptor, error_descriptor
        integer(c_int) :: timeout_value, poll_count, poll_slot(3), poll_ready
        integer(c_int) :: child_delay_ms
        integer(c_int64_t) :: input_length, input_offset, amount, now_ms
        integer(c_int64_t) :: deadline_ms, kill_deadline_ms, write_amount
        integer(c_int64_t) :: output_limit, started_ms
        integer :: i, env_count, wait_ms
        logical :: child_done, read_ok
        logical :: monitor_mode
        character(:), allocatable :: monitor_path
        character(:), allocatable :: pipe_error
        type(byte_buffer_t) :: captured_stdout, captured_stderr

        child_delay_ms = 0
        if (present(child_start_delay_ms)) child_delay_ms = max(0, child_start_delay_ms)
        result = process_result_t()
        result%stdout = ''
        result%stderr = ''
        input_pipe = -1
        output_pipe = -1
        error_pipe = -1
        child = -1
        input_descriptor = -1
        output_descriptor = -1
        error_descriptor = -1
        started_ms = -1
        output_limit = 64_c_int64_t * 1024_c_int64_t * 1024_c_int64_t
        if (present(max_output_bytes)) output_limit = max_output_bytes
        if (output_limit < 0) then
            call set_runner_error(result, 'negative process output limit')
            return
        end if
        if (output_limit > int(huge(0), c_int64_t)) then
            call set_runner_error(result, 'process output limit exceeds addressable buffer')
            return
        end if
        if (present(environment)) then
            call make_c_vector(environment, env_pointers, env_storage)
        else
            call make_c_vector(empty_environment, env_pointers, env_storage)
        end if
        env_count = size(env_pointers) - 1
        call make_c_vector(arguments, argument_pointers, argument_storage)
        if (size(argument_pointers) <= 1) then
            call set_runner_error(result, 'empty process argument vector')
            return
        end if
        call encode_c_string(cwd, cwd_bytes)
        input_length = 0
        if (present(input)) input_length = len(input)
        allocate(input_storage(max(1, int(input_length))))
        input_storage = c_null_char
        if (input_length > 0) then
            do i = 1, int(input_length)
                input_storage(i) = input(i:i)
            end do
        end if

        rc = c_pipe(input_pipe)
        if (rc /= 0) then
            call set_runner_error(result, 'create child stdin pipe')
            goto 800
        end if
        rc = c_pipe(output_pipe)
        if (rc /= 0) then
            call set_runner_error(result, 'create child stdout pipe')
            goto 800
        end if
        rc = c_pipe(error_pipe)
        if (rc /= 0) then
            call set_runner_error(result, 'create child stderr pipe')
            goto 800
        end if
        rc = c_ignore_sigpipe()
        if (rc /= 0) then
            call set_runner_error(result, 'ignore parent SIGPIPE while writing stdin')
            goto 800
        end if

        containment_status = c_process_containment_required()
        if (containment_status < 0_c_int) then
            call set_runner_error(result, 'probe child process-group capability')
            goto 800
        end if
        monitor_mode = containment_status > 0_c_int
        if (monitor_mode) then
            monitor_path = environment_value('FO_BIN')
            if (len(monitor_path) == 0) then
                call set_runner_error(result, &
                    'contained process launch requires absolute pinned FO_BIN')
                goto 800
            end if
            if (monitor_path(1:1) /= '/') then
                call set_runner_error(result, &
                    'contained process launch requires absolute pinned FO_BIN')
                goto 800
            end if
            call encode_c_string(monitor_path, monitor_bytes)
            child = c_start_capture_monitor(monitor_bytes, cwd_bytes, argument_pointers, &
                env_pointers, input_pipe(1), output_pipe(2), error_pipe(2))
            if (child <= 0) then
                call set_runner_error(result, &
                    'start contained command monitor from pinned FO_BIN: '//monitor_path)
                goto 800
            end if
        else
            child = c_fork()
            if (child < 0) then
                call set_runner_error(result, 'fork external process')
                goto 800
            end if
            if (child == 0) then
                ! A controlled pre-group delay lets the harness test launch-time cleanup.
                if (child_delay_ms > 0) rc = c_poll(poll_descriptors, 0_c_size_t, child_delay_ms)
                call execute_child(argument_pointers, argument_storage, env_storage, cwd_bytes, &
                    env_count, input_pipe, output_pipe, error_pipe)
                call c_exit(127_c_int)
            end if
        end if

        result%process_id = int(child)
        ! The child establishes its group before exec. Concurrent parent and child
        ! setpgid calls can fail with EPERM on Darwin while the group is being created.
        input_descriptor = input_pipe(2)
        input_pipe(2) = -1
        output_descriptor = output_pipe(1)
        output_pipe(1) = -1
        error_descriptor = error_pipe(1)
        error_pipe(1) = -1
        rc = c_close(input_pipe(1))
        if (rc /= 0) then
            call set_runner_error(result, 'close parent child-stdin pipe end')
            goto 700
        end if
        input_pipe(1) = -1
        rc = c_close(output_pipe(2))
        if (rc /= 0) then
            call set_runner_error(result, 'close parent child-stdout pipe end')
            goto 700
        end if
        output_pipe(2) = -1
        rc = c_close(error_pipe(2))
        if (rc /= 0) then
            call set_runner_error(result, 'close parent child-stderr pipe end')
            goto 700
        end if
        error_pipe(2) = -1
        if (input_length == 0) then
            rc = c_close(input_descriptor)
            if (rc /= 0) then
                call set_runner_error(result, 'close unused child-stdin pipe end')
                goto 700
            end if
            input_descriptor = -1
        end if
        rc = c_set_nonblocking(output_descriptor)
        if (rc /= 0) then
            call set_runner_error(result, 'make stdout pipe nonblocking')
            goto 700
        end if
        rc = c_set_nonblocking(error_descriptor)
        if (rc /= 0) then
            call set_runner_error(result, 'make stderr pipe nonblocking')
            goto 700
        end if
        if (input_descriptor >= 0) then
            rc = c_set_nonblocking(input_descriptor)
            if (rc /= 0) then
                call set_runner_error(result, 'make stdin pipe nonblocking')
                goto 700
            end if
        end if

        timeout_value = 120000_c_int
        if (present(timeout_ms)) timeout_value = int(timeout_ms, c_int)
        now_ms = c_monotonic_ms()
        if (now_ms < 0) then
            call set_runner_error(result, 'read monotonic process clock')
            goto 700
        end if
        started_ms = now_ms
        deadline_ms = huge(deadline_ms)
        if (timeout_value >= 0) deadline_ms = now_ms + timeout_value
        kill_deadline_ms = huge(kill_deadline_ms)
        input_offset = 0
        child_done = .false.
        result%timed_out = .false.

        do
            if (.not. child_done) then
                waited = c_waitpid_retry(child, wait_status, 1_c_int)
                if (waited < 0) then
                    call set_runner_error(result, 'poll child process status')
                    goto 700
                end if
                if (waited == child) then
                    child_done = .true.
                    result%reaped = .true.
                end if
            end if
            now_ms = c_monotonic_ms()
            if (now_ms < 0) then
                call set_runner_error(result, 'read monotonic process clock')
                goto 700
            end if
            if (.not. result%timed_out .and. now_ms >= deadline_ms) then
                result%timed_out = .true.
                if (monitor_mode) then
                    rc = c_kill(child, 15_c_int)
                else
                    if (.not. child_done) rc = c_kill(child, 15_c_int)
                    rc = c_kill(-child, 15_c_int)
                end if
                kill_deadline_ms = now_ms + 1000_c_int64_t
            end if
            if (result%timed_out .and. now_ms >= kill_deadline_ms) then
                if (.not. monitor_mode) then
                    if (.not. child_done) rc = c_kill(child, 9_c_int)
                    rc = c_kill(-child, 9_c_int)
                end if
                kill_deadline_ms = huge(kill_deadline_ms)
            end if
            if (child_done .and. output_descriptor < 0 .and. error_descriptor < 0) exit

            poll_count = 0
            if (output_descriptor >= 0) then
                poll_count = poll_count + 1
                poll_descriptors(poll_count)%descriptor = output_descriptor
                poll_descriptors(poll_count)%events = 17_c_short
                poll_descriptors(poll_count)%returned_events = 0
                poll_slot(poll_count) = 1
            end if
            if (error_descriptor >= 0) then
                poll_count = poll_count + 1
                poll_descriptors(poll_count)%descriptor = error_descriptor
                poll_descriptors(poll_count)%events = 17_c_short
                poll_descriptors(poll_count)%returned_events = 0
                poll_slot(poll_count) = 2
            end if
            if (input_descriptor >= 0) then
                poll_count = poll_count + 1
                poll_descriptors(poll_count)%descriptor = input_descriptor
                poll_descriptors(poll_count)%events = 4_c_short
                poll_descriptors(poll_count)%returned_events = 0
                poll_slot(poll_count) = 3
            end if
            wait_ms = 20
            if (.not. result%timed_out .and. deadline_ms /= huge(deadline_ms)) &
                wait_ms = min(wait_ms, max(0, int(deadline_ms - now_ms)))
            if (result%timed_out .and. kill_deadline_ms /= huge(kill_deadline_ms)) &
                wait_ms = min(wait_ms, max(0, int(kill_deadline_ms - now_ms)))
            poll_ready = c_poll(poll_descriptors, int(poll_count, c_size_t), int(wait_ms, c_int))
            if (poll_ready < 0) then
                call set_runner_error(result, 'poll child process pipes')
                goto 700
            end if
            do i = 1, poll_count
                if (poll_descriptors(i)%returned_events == 0) cycle
                select case (poll_slot(i))
                case (1)
                    call read_pipe(output_descriptor, captured_stdout, read_chunk, &
                        output_limit, read_ok, pipe_error)
                    if (.not. read_ok) then
                        call set_runner_error(result, pipe_error)
                        goto 700
                    end if
                case (2)
                    call read_pipe(error_descriptor, captured_stderr, read_chunk, &
                        output_limit, read_ok, pipe_error)
                    if (.not. read_ok) then
                        call set_runner_error(result, pipe_error)
                        goto 700
                    end if
                case (3)
                    if (child_done) then
                        rc = c_close(input_descriptor)
                        input_descriptor = -1
                        cycle
                    end if
                    write_amount = min(int(size(read_chunk), c_int64_t), input_length - input_offset)
                    if (write_amount <= 0) then
                        rc = c_close(input_descriptor)
                        input_descriptor = -1
                        cycle
                    end if
                    amount = c_write(input_descriptor, input_storage(int(input_offset) + 1), &
                        int(write_amount, c_size_t))
                    if (amount > 0) then
                        input_offset = input_offset + amount
                    else if (amount == -3_c_int64_t) then
                        rc = c_close(input_descriptor)
                        input_descriptor = -1
                    else if (amount == -2_c_int64_t) then
                        cycle
                    else
                        call set_runner_error(result, 'write child stdin')
                        goto 700
                    end if
                    if (input_descriptor >= 0 .and. input_offset >= input_length) then
                        rc = c_close(input_descriptor)
                        input_descriptor = -1
                    end if
                end select
            end do
            if (child_done .and. input_descriptor >= 0) then
                rc = c_close(input_descriptor)
                input_descriptor = -1
            end if
        end do

        call close_open_descriptors(input_pipe, output_pipe, error_pipe, &
            input_descriptor, output_descriptor, error_descriptor, rc)
        if (rc /= 0) then
            call set_runner_error(result, 'close child process pipes')
            goto 700
        end if
        ! A leader can close its pipes before descendants exit. Keep the
        ! process group or contained monitor owned until its descendants finish.
        call stop_owned_process(child, result%reaped, monitor_mode)
        result%exit_code = -1
        result%term_signal = 0
        if (iand(wait_status, 127_c_int) == 0) then
            result%exit_code = iand(ishft(wait_status, -8), 255_c_int)
        else
            result%term_signal = iand(wait_status, 127_c_int)
        end if
        call buffer_to_text(captured_stdout, result%stdout)
        call buffer_to_text(captured_stderr, result%stderr)
        now_ms = c_monotonic_ms()
        if (now_ms >= started_ms .and. started_ms >= 0) result%elapsed_ms = now_ms - started_ms
        return

        700     continue
        result%runner_failed = .true.
        call stop_owned_process(child, result%reaped, monitor_mode)
        call close_open_descriptors(input_pipe, output_pipe, error_pipe, &
            input_descriptor, output_descriptor, error_descriptor, rc)
        call buffer_to_text(captured_stdout, result%stdout)
        call buffer_to_text(captured_stderr, result%stderr)
        now_ms = c_monotonic_ms()
        if (now_ms >= started_ms .and. started_ms >= 0) result%elapsed_ms = now_ms - started_ms
        return

        800     continue
        call close_open_descriptors(input_pipe, output_pipe, error_pipe, &
            input_descriptor, output_descriptor, error_descriptor, rc)
        result%stdout = ''
        result%stderr = ''
    end subroutine run_process

    subroutine set_runner_error(result, message)
        type(process_result_t), intent(inout) :: result
        character(len=*), intent(in) :: message

        result%runner_failed = .true.
        if (.not. allocated(result%runner_error)) result%runner_error = trim(message)
    end subroutine set_runner_error

    subroutine close_open_descriptors(input_pipe, output_pipe, error_pipe, &
            input_descriptor, output_descriptor, error_descriptor, close_status)
        integer(c_int), intent(inout) :: input_pipe(2), output_pipe(2), error_pipe(2)
        integer(c_int), intent(inout) :: input_descriptor, output_descriptor
        integer(c_int), intent(inout) :: error_descriptor
        integer(c_int), intent(out) :: close_status
        integer :: i

        close_status = 0
        do i = 1, 2
            if (input_pipe(i) >= 0) then
                if (c_close(input_pipe(i)) /= 0) close_status = -1
                input_pipe(i) = -1
            end if
            if (output_pipe(i) >= 0) then
                if (c_close(output_pipe(i)) /= 0) close_status = -1
                output_pipe(i) = -1
            end if
            if (error_pipe(i) >= 0) then
                if (c_close(error_pipe(i)) /= 0) close_status = -1
                error_pipe(i) = -1
            end if
        end do
        if (input_descriptor >= 0) then
            if (c_close(input_descriptor) /= 0) close_status = -1
            input_descriptor = -1
        end if
        if (output_descriptor >= 0) then
            if (c_close(output_descriptor) /= 0) close_status = -1
            output_descriptor = -1
        end if
        if (error_descriptor >= 0) then
            if (c_close(error_descriptor) /= 0) close_status = -1
            error_descriptor = -1
        end if
    end subroutine close_open_descriptors

    subroutine stop_owned_process(child, reaped, monitor_mode)
        integer(c_int), intent(in) :: child
        logical, intent(inout) :: reaped
        logical, intent(in) :: monitor_mode
        integer(c_int) :: rc, wait_status, waited
        integer(c_int64_t) :: deadline, now_ms
        integer :: attempts
        type(pollfd_t) :: no_descriptors(1)

        if (child < 0) return
        if (monitor_mode) then
            if (reaped) return
            rc = c_kill(child, 15_c_int)
            do attempts = 1, 200
                waited = c_waitpid_retry(child, wait_status, 1_c_int)
                if (waited == child) then
                    reaped = .true.
                    return
                end if
                if (waited < 0) return
                rc = c_poll(no_descriptors, 0_c_size_t, 20_c_int)
            end do
            ! The fresh monitor has a bounded descendant reap path. Keep
            ! ownership scoped to its PID and let it finish that cleanup.
            rc = c_kill(child, 15_c_int)
            do
                waited = c_waitpid_retry(child, wait_status, 0_c_int)
                if (waited == child) then
                    reaped = .true.
                    exit
                end if
                if (waited < 0) exit
            end do
            return
        end if
        if (.not. reaped) rc = c_kill(child, 15_c_int)
        rc = c_kill(-child, 15_c_int)
        now_ms = c_monotonic_ms()
        deadline = huge(deadline)
        if (now_ms >= 0) deadline = now_ms + 1000_c_int64_t
        attempts = 0
        do while (.not. reaped .and. attempts < 60)
            waited = c_waitpid_retry(child, wait_status, 1_c_int)
            if (waited == child) then
                reaped = .true.
                exit
            end if
            if (waited < 0) exit
            now_ms = c_monotonic_ms()
            if (now_ms >= deadline .or. now_ms < 0) exit
            rc = c_poll(no_descriptors, 0_c_size_t, 20_c_int)
            attempts = attempts + 1
        end do
        if (.not. reaped) rc = c_kill(child, 9_c_int)
        rc = c_kill(-child, 9_c_int)
        if (.not. reaped) then
            do
                waited = c_waitpid_retry(child, wait_status, 0_c_int)
                if (waited == child) then
                    reaped = .true.
                    exit
                end if
                if (waited < 0) exit
            end do
        end if
    end subroutine stop_owned_process

    subroutine execute_child(argument_pointers, argument_storage, env_storage, cwd_bytes, &
            env_count, input_pipe, output_pipe, error_pipe)
        type(c_ptr), contiguous, intent(in) :: argument_pointers(:)
        character(kind=c_char), contiguous, intent(in) :: argument_storage(:, :)
        character(kind=c_char), contiguous, intent(inout) :: env_storage(:, :)
        character(kind=c_char), contiguous, intent(in) :: cwd_bytes(:)
        integer, intent(in) :: env_count
        integer(c_int), intent(in) :: input_pipe(2), output_pipe(2), error_pipe(2)
        integer(c_int) :: rc
        integer :: i, j, equals_at

        rc = c_setpgid(0_c_int, 0_c_int)
        if (rc /= 0) then
            call c_child_setup_error(error_pipe(2), 1_c_int)
            call c_exit(126_c_int)
        end if
        rc = c_default_sigpipe()
        if (rc /= 0) then
            call c_child_setup_error(error_pipe(2), 2_c_int)
            call c_exit(126_c_int)
        end if
        rc = c_dup2(input_pipe(1), 0_c_int)
        if (rc < 0) then
            call c_child_setup_error(error_pipe(2), 3_c_int)
            call c_exit(126_c_int)
        end if
        rc = c_dup2(output_pipe(2), 1_c_int)
        if (rc < 0) then
            call c_child_setup_error(error_pipe(2), 4_c_int)
            call c_exit(126_c_int)
        end if
        rc = c_dup2(error_pipe(2), 2_c_int)
        if (rc < 0) then
            call c_child_setup_error(error_pipe(2), 5_c_int)
            call c_exit(126_c_int)
        end if
        do i = 1, 2
            rc = c_close(input_pipe(i))
            rc = c_close(output_pipe(i))
            rc = c_close(error_pipe(i))
        end do
        rc = c_chdir(cwd_bytes)
        if (rc /= 0) call c_exit(125_c_int)
        if (env_count > 0) then
            do i = 1, env_count
                equals_at = 0
                do j = 1, size(env_storage, 1)
                    if (env_storage(j, i) == c_null_char) exit
                    if (env_storage(j, i) == '=') then
                        equals_at = j
                        exit
                    end if
                end do
                if (equals_at <= 1 .or. equals_at >= size(env_storage, 1)) &
                    call c_exit(125_c_int)
                env_storage(equals_at, i) = c_null_char
                rc = c_setenv(env_storage(1, i), env_storage(equals_at + 1, i), 1_c_int)
                if (rc /= 0) call c_exit(125_c_int)
            end do
        end if
        rc = c_execvp(argument_storage(1, 1), argument_pointers)
        call c_exit(127_c_int)
    end subroutine execute_child

    subroutine read_pipe(descriptor, output, chunk, output_limit, ok, message)
        integer(c_int), intent(inout) :: descriptor
        type(byte_buffer_t), intent(inout) :: output
        character(kind=c_char), contiguous, intent(out) :: chunk(:)
        integer(c_int64_t), intent(in) :: output_limit
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer(c_int64_t) :: count
        integer(c_int) :: rc

        ok = .true.
        message = ''
        do
            count = c_read(descriptor, chunk, int(size(chunk), c_size_t))
            if (count == 0) then
                rc = c_close(descriptor)
                descriptor = -1
                return
            else if (count == -2_c_int64_t) then
                return
            else if (count < 0) then
                ok = .false.
                message = 'read child output pipe'
                return
            else
                call append_output(output, chunk, int(count), output_limit, ok)
                if (.not. ok) then
                    message = 'captured child output exceeds configured limit'
                    return
                end if
            end if
        end do
    end subroutine read_pipe

    subroutine append_output(output, source, count, output_limit, ok)
        type(byte_buffer_t), intent(inout) :: output
        character(kind=c_char), intent(in) :: source(:)
        integer, intent(in) :: count
        integer(c_int64_t), intent(in) :: output_limit
        logical, intent(out) :: ok
        character(kind=c_char), allocatable :: grown(:)
        integer :: required, capacity, bounded_limit

        ok = .false.
        if (count < 0) return
        if (int(count, c_int64_t) > output_limit - int(output%length, c_int64_t)) return
        required = output%length + count
        if (.not. allocated(output%bytes)) allocate(output%bytes(4096))
        if (required > size(output%bytes)) then
            bounded_limit = int(min(output_limit, int(huge(capacity), c_int64_t)))
            capacity = max(required, min(bounded_limit, 2 * size(output%bytes)))
            allocate(grown(capacity))
            if (output%length > 0) grown(1:output%length) = output%bytes(1:output%length)
            call move_alloc(grown, output%bytes)
        end if
        if (count > 0) output%bytes(output%length + 1:required) = source(1:count)
        output%length = required
        ok = .true.
    end subroutine append_output

    subroutine buffer_to_text(buffer, text)
        type(byte_buffer_t), intent(in) :: buffer
        character(:), allocatable, intent(out) :: text
        integer :: i

        allocate(character(len=buffer%length) :: text)
        do i = 1, buffer%length
            text(i:i) = buffer%bytes(i)
        end do
    end subroutine buffer_to_text

    subroutine make_scratch(prefix, path)
        character(len=*), intent(in) :: prefix
        character(:), allocatable, intent(out) :: path
        character(kind=c_char), allocatable, target :: pattern(:)
        character(:), allocatable :: template, scratch_root
        integer(c_int) :: rc
        integer :: i, end_at

        rc = int(test_os_link_probe(), c_int)
        scratch_root = environment_value('TMPDIR')
        if (len_trim(scratch_root) == 0) scratch_root = '/var/tmp'
        scratch_root = trim(scratch_root)
        do while (len(scratch_root) > 1)
            if (scratch_root(len(scratch_root):len(scratch_root)) /= '/') exit
            scratch_root = scratch_root(:len(scratch_root) - 1)
        end do
        if (scratch_root == '/') then
            template = '/' // trim(prefix) // '-XXXXXX'
        else
            template = scratch_root // '/' // trim(prefix) // '-XXXXXX'
        end if
        call encode_c_string(template, pattern)
        rc = c_mkdtemp(pattern)
        if (rc /= 0) then
            call record_failure('create temporary fixture directory')
            path = ''
            return
        end if
        end_at = 0
        do i = 1, size(pattern)
            if (pattern(i) == c_null_char) exit
            end_at = i
        end do
        allocate(character(len=end_at) :: path)
        do i = 1, end_at
            path(i:i) = pattern(i)
        end do
        call register_scratch(path)
    end subroutine make_scratch

    subroutine register_scratch(path)
        character(len=*), intent(in) :: path
        type(string_t), allocatable :: grown(:)
        integer :: count

        if (len_trim(path) == 0) return
        count = 0
        if (allocated(scratch_paths)) count = size(scratch_paths)
        allocate(grown(count + 1))
        if (count > 0) grown(1:count) = scratch_paths
        grown(count + 1)%value = trim(path)
        call move_alloc(grown, scratch_paths)
    end subroutine register_scratch

    subroutine current_directory(path)
        character(:), allocatable, intent(out) :: path
        character(kind=c_char), allocatable, target :: buffer(:)
        integer(c_int) :: rc
        integer :: i, end_at

        allocate(buffer(4096))
        buffer = c_null_char
        rc = c_getcwd(buffer, int(size(buffer), c_size_t))
        if (rc /= 0) then
            call record_failure('get current working directory')
            path = ''
            return
        end if
        end_at = 0
        do i = 1, size(buffer)
            if (buffer(i) == c_null_char) exit
            end_at = i
        end do
        allocate(character(len=end_at) :: path)
        do i = 1, end_at
            path(i:i) = buffer(i)
        end do
    end subroutine current_directory

    function join_path(parent, child) result(path)
        character(len=*), intent(in) :: parent, child
        character(:), allocatable :: path
        integer :: parent_length

        parent_length = len_trim(parent)
        if (parent_length == 0) then
            path = trim(child)
        else if (parent(parent_length:parent_length) == '/') then
            path = parent(:parent_length) // trim(child)
        else
            path = parent(:parent_length) // '/' // trim(child)
        end if
    end function join_path

    subroutine make_directory(path)
        character(len=*), intent(in) :: path
        character(kind=c_char), allocatable, target :: bytes(:)
        integer(c_int) :: rc

        call encode_c_string(path, bytes)
        rc = c_mkdirs(bytes)
        call assert_equal_integer(int(rc), 0, 'create directory: ' // path)
    end subroutine make_directory

    subroutine make_symlink(target, link_path)
        character(len=*), intent(in) :: target, link_path
        character(kind=c_char), allocatable, target :: target_bytes(:), link_bytes(:)
        integer(c_int) :: rc

        call encode_c_string(target, target_bytes)
        call encode_c_string(link_path, link_bytes)
        rc = c_symlink(target_bytes, link_bytes)
        call assert_equal_integer(int(rc), 0, 'create symlink: ' // link_path)
    end subroutine make_symlink

    subroutine move_path(source, target)
        character(len=*), intent(in) :: source, target
        character(kind=c_char), allocatable, target :: source_bytes(:), target_bytes(:)
        integer(c_int) :: rc

        call encode_c_string(source, source_bytes)
        call encode_c_string(target, target_bytes)
        rc = c_rename(source_bytes, target_bytes)
        call assert_equal_integer(int(rc), 0, 'rename path: ' // source)
    end subroutine move_path

    subroutine remove_path(path)
        character(len=*), intent(in) :: path
        character(kind=c_char), allocatable, target :: bytes(:)
        integer(c_int) :: rc

        call encode_c_string(path, bytes)
        rc = c_unlink(bytes)
        call assert_equal_integer(int(rc), 0, 'remove path: ' // path)
    end subroutine remove_path

    subroutine remove_tree(path)
        character(len=*), intent(in) :: path
        character(kind=c_char), allocatable, target :: bytes(:)
        integer(c_int) :: rc

        call encode_c_string(path, bytes)
        rc = c_remove_tree(bytes)
        call assert_equal_integer(int(rc), 0, 'remove fixture tree: ' // path)
    end subroutine remove_tree

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: unit, io_status, slash

        slash = index(path, '/', back=.true.)
        if (slash > 1) call make_directory(path(:slash - 1))
        open(newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='write', iostat=io_status)
        if (io_status /= 0) then
            call record_failure('open output file: ' // path)
            return
        end if
        if (len(text) > 0) write(unit, iostat=io_status) text
        if (len(text) > 0) call assert_equal_integer(io_status, 0, 'write file: ' // path)
        close(unit, iostat=io_status)
        call assert_equal_integer(io_status, 0, 'close output file: ' // path)
    end subroutine write_text

    subroutine write_lines(path, lines)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: lines(:)
        character(:), allocatable :: text
        integer :: i

        text = ''
        do i = 1, size(lines)
            text = text // trim(lines(i)) // new_line('a')
        end do
        call write_text(path, text)
    end subroutine write_lines

    subroutine append_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: unit, io_status

        open(newunit=unit, file=path, access='stream', form='unformatted', &
            status='unknown', position='append', action='write', iostat=io_status)
        if (io_status /= 0) then
            call record_failure('open append file: ' // path)
            return
        end if
        if (len(text) > 0) write(unit, iostat=io_status) text
        if (len(text) > 0) call assert_equal_integer(io_status, 0, 'append file: ' // path)
        close(unit, iostat=io_status)
        call assert_equal_integer(io_status, 0, 'close append file: ' // path)
    end subroutine append_text

    function read_text(path) result(text)
        character(len=*), intent(in) :: path
        character(:), allocatable :: text
        integer :: unit, io_status, byte_count

        byte_count = 0
        inquire(file=path, size=byte_count, iostat=io_status)
        if (io_status /= 0 .or. byte_count < 0) then
            call record_failure('inspect input file: ' // path)
            text = ''
            return
        end if
        allocate(character(len=byte_count) :: text)
        open(newunit=unit, file=path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        if (io_status /= 0) then
            call record_failure('open input file: ' // path)
            text = ''
            return
        end if
        if (byte_count > 0) read(unit, iostat=io_status) text
        if (byte_count > 0) call assert_equal_integer(io_status, 0, 'read file: ' // path)
        close(unit, iostat=io_status)
        call assert_equal_integer(io_status, 0, 'close input file: ' // path)
    end function read_text

    logical function file_exists(path)
        character(len=*), intent(in) :: path
        integer :: io_status

        inquire(file=path, exist=file_exists, iostat=io_status)
        call assert_equal_integer(io_status, 0, 'inspect path: ' // path)
    end function file_exists

    function environment_value(name) result(value)
        character(len=*), intent(in) :: name
        character(:), allocatable :: value
        integer :: value_length, status

        call get_environment_variable(name, length=value_length, status=status)
        if (status /= 0) then
            value = ''
            return
        end if
        if (value_length <= 0) then
            value = ''
            return
        end if
        allocate(character(len=value_length) :: value)
        call get_environment_variable(name, value=value, status=status)
        if (status /= 0) value = ''
    end function environment_value

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        call record_failure(trim(message))
    end subroutine assert_true

    subroutine assert_equal_string(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (actual == expected) return
        call record_failure(trim(message) // ': string lengths ' // integer_text(len(actual)) // &
            ' and ' // integer_text(len(expected)))
    end subroutine assert_equal_string

    subroutine assert_equal_integer(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual == expected) return
        call record_failure(trim(message) // ': got ' // integer_text(actual) // &
            ', expected ' // integer_text(expected))
    end subroutine assert_equal_integer

    subroutine assert_contains(actual, fragment, message)
        character(len=*), intent(in) :: actual, fragment, message

        call assert_true(index(actual, fragment) > 0, message)
    end subroutine assert_contains

    subroutine assert_not_contains(actual, fragment, message)
        character(len=*), intent(in) :: actual, fragment, message

        call assert_true(index(actual, fragment) == 0, message)
    end subroutine assert_not_contains

    subroutine assert_file_exists(path, message)
        character(len=*), intent(in) :: path, message

        call assert_true(file_exists(path), message)
    end subroutine assert_file_exists

    subroutine assert_file_absent(path, message)
        character(len=*), intent(in) :: path, message

        call assert_true(.not. file_exists(path), message)
    end subroutine assert_file_absent

    subroutine assert_file_equals(path, expected, message)
        character(len=*), intent(in) :: path, expected, message

        call assert_equal_string(read_text(path), expected, message)
    end subroutine assert_file_equals

    subroutine assert_process_ok(result, message)
        type(process_result_t), intent(in) :: result
        character(len=*), intent(in) :: message

        if (result%exit_code /= 0 .or. result%term_signal /= 0 .or. &
            result%runner_failed .or. result%timed_out) then
            if (allocated(result%stderr)) then
                if (len(result%stderr) > 0) &
                    write (error_unit, '(a)') message // ': captured stderr: ' // &
                        result%stderr
            end if
        end if
        if (result%runner_failed) then
            if (allocated(result%runner_error)) then
                call assert_true(.false., message // ': process runner failed: ' // &
                    result%runner_error)
            else
                call assert_true(.false., message // ': process runner failed')
            end if
        end if
        call assert_true(.not. result%timed_out, message // ': child timed out')
        call assert_equal_integer(result%term_signal, 0, message // ': child signal')
        call assert_equal_integer(result%exit_code, 0, message // ': child status')
    end subroutine assert_process_ok

    subroutine record_failure(message)
        character(len=*), intent(in) :: message
        type(string_t), allocatable :: grown(:)
        integer :: count

        count = 0
        if (allocated(failures)) count = size(failures)
        allocate(grown(count + 1))
        if (count > 0) grown(1:count) = failures
        grown(count + 1)%value = trim(message)
        call move_alloc(grown, failures)
    end subroutine record_failure

    subroutine finish_assertions(retain_failed_scratch)
        logical, optional, intent(in) :: retain_failed_scratch
        type(string_t), allocatable :: pending_failures(:)
        character(kind=c_char), allocatable, target :: path_bytes(:)
        integer(c_int) :: rc
        integer :: i, failure_count
        logical :: retain

        failure_count = 0
        if (allocated(failures)) then
            failure_count = size(failures)
            pending_failures = failures
            deallocate(failures)
        end if
        retain = .false.
        if (present(retain_failed_scratch)) retain = retain_failed_scratch
        retain = retain .and. failure_count > 0
        if (allocated(scratch_paths)) then
            do i = 1, size(scratch_paths)
                if (.not. allocated(scratch_paths(i)%value)) cycle
                if (retain) then
                    write(error_unit, '(a)') 'Retained failed fixture scratch: '// &
                        scratch_paths(i)%value
                    cycle
                end if
                call encode_c_string(scratch_paths(i)%value, path_bytes)
                rc = c_remove_tree(path_bytes)
                if (rc /= 0) call record_failure(&
                    'remove fixture scratch directory: ' // scratch_paths(i)%value)
            end do
            deallocate(scratch_paths)
        end if
        if (allocated(failures)) failure_count = failure_count + size(failures)
        if (failure_count > 0) then
            if (allocated(pending_failures)) then
                do i = 1, size(pending_failures)
                    write(error_unit, '(a)') 'FAIL: ' // pending_failures(i)%value
                end do
            end if
        end if
        if (allocated(failures)) then
            do i = 1, size(failures)
                write(error_unit, '(a)') 'FAIL: ' // failures(i)%value
            end do
        end if
        if (failure_count > 0) then
            error stop 1
        end if
    end subroutine finish_assertions

    subroutine reset_assertions_for_probe()
        if (allocated(failures)) deallocate(failures)
        if (allocated(scratch_paths)) deallocate(scratch_paths)
    end subroutine reset_assertions_for_probe

    subroutine exercise_failure_cleanup_probe(marker, retain_failed_scratch)
        character(len=*), intent(in) :: marker
        logical, optional, intent(in) :: retain_failed_scratch
        character(:), allocatable :: scratch
        integer(c_int) :: child, waited, wait_status, rc
        integer :: status
        logical :: retain

        retain = .false.
        if (present(retain_failed_scratch)) retain = retain_failed_scratch

        child = c_fork()
        if (child == 0) then
            call reset_assertions_for_probe()
            rc = c_silence_output()
            call make_scratch('fo-cleanup-failure-probe', scratch)
            call write_text(marker, scratch)
            call write_text(join_path(scratch, 'journal.jsonl'), 'unique failure receipt')
            call assert_true(.false., 'intentional scratch cleanup failure probe')
            call assert_file_equals(join_path(scratch, 'missing-receipt'), 'receipt', &
                'missing fixture output records a failure before cleanup')
            call finish_assertions(retain_failed_scratch=retain)
            call c_exit(0_c_int)
        end if
        call assert_true(child > 0, 'fork assertion cleanup probe')
        if (child <= 0) return
        do
            waited = c_waitpid_retry(child, wait_status, 0_c_int)
            if (waited == child) exit
            if (waited < 0) then
                call assert_true(.false., 'wait for assertion cleanup probe')
                return
            end if
        end do
        call assert_equal_integer(iand(wait_status, 127_c_int), 0, &
            'assertion cleanup probe exits normally')
        status = iand(ishft(wait_status, -8), 255_c_int)
        call assert_equal_integer(status, 1, 'assertion failure remains a failing test')
        scratch = read_text(marker)
        call assert_true(len(scratch) > 0, 'cleanup probe reports its scratch path')
        if (retain) then
            call assert_true(file_exists(scratch), &
                'opt-in retains unique failure evidence after probe exits')
            call assert_file_equals(join_path(scratch, 'journal.jsonl'), &
                'unique failure receipt', 'retained journal keeps exact evidence bytes')
            call remove_tree(scratch)
        else
            call assert_true(.not. file_exists(scratch), &
                'assertion failure cleans registered scratch before exiting')
        end if
    end subroutine exercise_failure_cleanup_probe

    subroutine start_sentinel(process_id)
        integer, intent(out) :: process_id
        character(kind=c_char), allocatable, target :: executable(:), duration(:)
        type(c_ptr), allocatable, target :: arguments(:)
        integer(c_int) :: child, rc

        ! Callers stop the sentinel explicitly; its lifetime only has to outlast
        ! a slow fixture on a loaded host, never decide the outcome.
        call encode_c_string('/bin/sleep', executable)
        call encode_c_string('3600', duration)
        allocate(arguments(3))
        arguments(1) = c_loc(executable(1))
        arguments(2) = c_loc(duration(1))
        arguments(3) = c_null_ptr
        child = c_fork()
        if (child == 0) then
            rc = c_execvp(executable, arguments)
            call c_exit(127_c_int)
        end if
        process_id = int(child)
        call assert_true(child > 0, 'start unrelated sentinel child')
    end subroutine start_sentinel

    logical function process_alive(process_id)
        !! A zombie has exited and only awaits its reaper, so it is not alive.
        integer, intent(in) :: process_id
        character(len=64) :: stat_path
        character(len=512) :: stat_line
        integer :: unit, ios, close_paren

        process_alive = .false.
        if (process_id <= 0) return
        process_alive = c_kill(int(process_id, c_int), 0_c_int) == 0
        if (.not. process_alive) return
        write (stat_path, '(a,i0,a)') '/proc/', process_id, '/stat'
        open (newunit=unit, file=trim(stat_path), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) stat_line
        close (unit)
        if (ios /= 0) return
        close_paren = index(stat_line, ')', back=.true.)
        if (close_paren > 0 .and. close_paren + 2 <= len_trim(stat_line)) &
            process_alive = stat_line(close_paren + 2:close_paren + 2) /= 'Z'
    end function process_alive

    subroutine stop_sentinel(process_id)
        integer, intent(in) :: process_id
        integer(c_int) :: rc, waited, wait_status
        integer :: attempts
        type(pollfd_t) :: no_descriptors(1)

        if (process_id <= 0) return
        rc = c_kill(int(process_id, c_int), 15_c_int)
        do attempts = 1, 50
            waited = c_waitpid_retry(int(process_id, c_int), wait_status, 1_c_int)
            if (waited == process_id) return
            if (waited < 0) exit
            rc = c_poll(no_descriptors, 0_c_size_t, 20_c_int)
        end do
        rc = c_kill(int(process_id, c_int), 9_c_int)
        waited = c_waitpid_retry(int(process_id, c_int), wait_status, 0_c_int)
        call assert_equal_integer(int(waited), process_id, 'reap unrelated sentinel child')
    end subroutine stop_sentinel

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

end module fo_test_harness
