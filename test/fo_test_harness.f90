module fo_test_harness
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
    public :: assert_process_ok

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
    end type process_result_t

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

        integer(c_int) function c_pipe(descriptors) bind(C, name='pipe')
            import :: c_int
            integer(c_int), intent(out) :: descriptors(2)
        end function c_pipe

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

        integer(c_int) function c_waitpid(process, status, options) bind(C, name='waitpid')
            import :: c_int
            integer(c_int), value :: process, options
            integer(c_int), intent(out) :: status
        end function c_waitpid

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
    end interface

contains

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

    subroutine run_process(arguments, cwd, result, environment, input, timeout_ms)
        type(string_list_t), intent(in) :: arguments
        character(len=*), intent(in) :: cwd
        type(process_result_t), intent(out) :: result
        type(string_list_t), optional, intent(in) :: environment
        character(len=*), optional, intent(in) :: input
        integer, optional, intent(in) :: timeout_ms
        type(string_list_t) :: empty_environment
        type(c_ptr), allocatable, target :: argument_pointers(:), env_pointers(:)
        character(kind=c_char), allocatable, target :: argument_storage(:, :)
        character(kind=c_char), allocatable, target :: env_storage(:, :)
        character(kind=c_char), allocatable, target :: cwd_bytes(:), input_storage(:)
        character(kind=c_char), target :: read_chunk(16384)
        type(pollfd_t) :: poll_descriptors(3)
        integer(c_int) :: input_pipe(2), output_pipe(2), error_pipe(2)
        integer(c_int) :: child, wait_status, waited, rc
        integer(c_int) :: input_descriptor, output_descriptor, error_descriptor
        integer(c_int) :: timeout_value, poll_count, poll_slot(3), poll_ready
        integer(c_int64_t) :: input_length, input_offset, amount, now_ms
        integer(c_int64_t) :: deadline_ms, kill_deadline_ms, write_amount
        integer :: i, env_count, equals_at, wait_ms
        logical :: child_done
        type(byte_buffer_t) :: captured_stdout, captured_stderr

        if (present(environment)) then
            call make_c_vector(environment, env_pointers, env_storage)
        else
            call make_c_vector(empty_environment, env_pointers, env_storage)
        end if
        env_count = size(env_pointers) - 1
        call make_c_vector(arguments, argument_pointers, argument_storage)
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
        call assert_equal_integer(int(rc), 0, 'create child stdin pipe')
        rc = c_pipe(output_pipe)
        call assert_equal_integer(int(rc), 0, 'create child stdout pipe')
        rc = c_pipe(error_pipe)
        call assert_equal_integer(int(rc), 0, 'create child stderr pipe')
        rc = c_ignore_sigpipe()
        call assert_equal_integer(int(rc), 0, 'ignore parent SIGPIPE while writing stdin')

        child = c_fork()
        call assert_true(child >= 0, 'fork external process')
        if (child == 0) then
            call execute_child(argument_pointers, argument_storage, env_storage, cwd_bytes, &
                env_count, input_pipe, output_pipe, error_pipe)
            call c_exit(127_c_int)
        end if

        rc = c_setpgid(child, child)
        input_descriptor = input_pipe(2)
        output_descriptor = output_pipe(1)
        error_descriptor = error_pipe(1)
        rc = c_close(input_pipe(1))
        rc = c_close(output_pipe(2))
        rc = c_close(error_pipe(2))
        if (input_length == 0) then
            rc = c_close(input_descriptor)
            input_descriptor = -1
        end if
        rc = c_set_nonblocking(output_descriptor)
        call assert_equal_integer(int(rc), 0, 'make stdout pipe nonblocking')
        rc = c_set_nonblocking(error_descriptor)
        call assert_equal_integer(int(rc), 0, 'make stderr pipe nonblocking')
        if (input_descriptor >= 0) then
            rc = c_set_nonblocking(input_descriptor)
            call assert_equal_integer(int(rc), 0, 'make stdin pipe nonblocking')
        end if

        timeout_value = 120000_c_int
        if (present(timeout_ms)) timeout_value = int(timeout_ms, c_int)
        now_ms = c_monotonic_ms()
        call assert_true(now_ms >= 0, 'read monotonic process clock')
        deadline_ms = huge(deadline_ms)
        if (timeout_value >= 0) deadline_ms = now_ms + timeout_value
        kill_deadline_ms = huge(kill_deadline_ms)
        input_offset = 0
        child_done = .false.
        result%timed_out = .false.

        do
            if (.not. child_done) then
                waited = c_waitpid(child, wait_status, 1_c_int)
                if (waited == child) child_done = .true.
            end if
            now_ms = c_monotonic_ms()
            call assert_true(now_ms >= 0, 'read monotonic process clock')
            if (.not. result%timed_out .and. now_ms >= deadline_ms) then
                result%timed_out = .true.
                rc = c_kill(-child, 15_c_int)
                kill_deadline_ms = now_ms + 1000_c_int64_t
            end if
            if (result%timed_out .and. now_ms >= kill_deadline_ms) then
                rc = c_kill(-child, 9_c_int)
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
            call assert_true(poll_ready >= 0, 'poll child process pipes')
            do i = 1, poll_count
                if (poll_descriptors(i)%returned_events == 0) cycle
                select case (poll_slot(i))
                case (1)
                    call read_pipe(output_descriptor, captured_stdout, read_chunk)
                case (2)
                    call read_pipe(error_descriptor, captured_stderr, read_chunk)
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
                    else
                        call assert_true(amount == -2_c_int64_t, 'write child stdin')
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

        result%exit_code = -1
        result%term_signal = 0
        if (iand(wait_status, 127_c_int) == 0) then
            result%exit_code = iand(ishft(wait_status, -8), 255_c_int)
        else
            result%term_signal = iand(wait_status, 127_c_int)
        end if
        call buffer_to_text(captured_stdout, result%stdout)
        call buffer_to_text(captured_stderr, result%stderr)
    end subroutine run_process

    subroutine execute_child(argument_pointers, argument_storage, env_storage, cwd_bytes, &
            env_count, input_pipe, output_pipe, error_pipe)
        type(c_ptr), intent(in) :: argument_pointers(:)
        character(kind=c_char), intent(in) :: argument_storage(:, :)
        character(kind=c_char), intent(inout) :: env_storage(:, :)
        character(kind=c_char), intent(in) :: cwd_bytes(:)
        integer, intent(in) :: env_count
        integer(c_int), intent(in) :: input_pipe(2), output_pipe(2), error_pipe(2)
        integer(c_int) :: rc
        integer :: i, j, equals_at

        rc = c_setpgid(0_c_int, 0_c_int)
        rc = c_default_sigpipe()
        rc = c_dup2(input_pipe(1), 0_c_int)
        if (rc < 0) call c_exit(126_c_int)
        rc = c_dup2(output_pipe(2), 1_c_int)
        if (rc < 0) call c_exit(126_c_int)
        rc = c_dup2(error_pipe(2), 2_c_int)
        if (rc < 0) call c_exit(126_c_int)
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

    subroutine read_pipe(descriptor, output, chunk)
        integer(c_int), intent(inout) :: descriptor
        type(byte_buffer_t), intent(inout) :: output
        character(kind=c_char), intent(out) :: chunk(:)
        integer(c_int64_t) :: count
        integer(c_int) :: rc

        do
            count = c_read(descriptor, chunk, int(size(chunk), c_size_t))
            if (count == 0) then
                rc = c_close(descriptor)
                descriptor = -1
                return
            else if (count == -2_c_int64_t) then
                return
            else if (count < 0) then
                call assert_true(.false., 'read child output pipe')
            else
                call append_output(output, chunk, int(count))
            end if
        end do
    end subroutine read_pipe

    subroutine append_output(output, source, count)
        type(byte_buffer_t), intent(inout) :: output
        character(kind=c_char), intent(in) :: source(:)
        integer, intent(in) :: count
        character(kind=c_char), allocatable :: grown(:)
        integer :: required, capacity

        call assert_true(count >= 0 .and. output%length + count <= 64 * 1024 * 1024, &
            'captured child output stays within 64 MiB')
        required = output%length + count
        if (.not. allocated(output%bytes)) allocate(output%bytes(4096))
        if (required > size(output%bytes)) then
            capacity = max(required, min(64 * 1024 * 1024, 2 * size(output%bytes)))
            allocate(grown(capacity))
            if (output%length > 0) grown(1:output%length) = output%bytes(1:output%length)
            call move_alloc(grown, output%bytes)
        end if
        if (count > 0) output%bytes(output%length + 1:required) = source(1:count)
        output%length = required
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
        character(:), allocatable :: template
        integer(c_int) :: rc
        integer :: i, end_at

        template = '/var/tmp/' // trim(prefix) // '-XXXXXX'
        call encode_c_string(template, pattern)
        rc = c_mkdtemp(pattern)
        call assert_equal_integer(int(rc), 0, 'create temporary fixture directory')
        end_at = 0
        do i = 1, size(pattern)
            if (pattern(i) == c_null_char) exit
            end_at = i
        end do
        allocate(character(len=end_at) :: path)
        do i = 1, end_at
            path(i:i) = pattern(i)
        end do
    end subroutine make_scratch

    subroutine current_directory(path)
        character(:), allocatable, intent(out) :: path
        character(kind=c_char), allocatable, target :: buffer(:)
        integer(c_int) :: rc
        integer :: i, end_at

        allocate(buffer(4096))
        buffer = c_null_char
        rc = c_getcwd(buffer, int(size(buffer), c_size_t))
        call assert_equal_integer(int(rc), 0, 'get current working directory')
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
        call assert_equal_integer(io_status, 0, 'open output file: ' // path)
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
        call assert_equal_integer(io_status, 0, 'open append file: ' // path)
        if (len(text) > 0) write(unit, iostat=io_status) text
        if (len(text) > 0) call assert_equal_integer(io_status, 0, 'append file: ' // path)
        close(unit, iostat=io_status)
        call assert_equal_integer(io_status, 0, 'close append file: ' // path)
    end subroutine append_text

    function read_text(path) result(text)
        character(len=*), intent(in) :: path
        character(:), allocatable :: text
        integer :: unit, io_status, byte_count

        inquire(file=path, size=byte_count, iostat=io_status)
        call assert_equal_integer(io_status, 0, 'inspect input file: ' // path)
        allocate(character(len=byte_count) :: text)
        open(newunit=unit, file=path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_equal_integer(io_status, 0, 'open input file: ' // path)
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
        write(error_unit, '(a)') 'FAIL: ' // trim(message)
        error stop 1
    end subroutine assert_true

    subroutine assert_equal_string(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (actual == expected) return
        write(error_unit, '(a)') 'FAIL: ' // trim(message)
        write(error_unit, '(a,i0,a,i0)') 'actual bytes=', len(actual), &
            ', expected bytes=', len(expected)
        error stop 1
    end subroutine assert_equal_string

    subroutine assert_equal_integer(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual == expected) return
        write(error_unit, '(a)') 'FAIL: ' // trim(message)
        write(error_unit, '(a,i0,a,i0)') 'actual=', actual, ', expected=', expected
        error stop 1
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

        call assert_true(.not. result%timed_out, message // ': child timed out')
        call assert_equal_integer(result%term_signal, 0, message // ': child signal')
        call assert_equal_integer(result%exit_code, 0, message // ': child status')
    end subroutine assert_process_ok

end module fo_test_harness
