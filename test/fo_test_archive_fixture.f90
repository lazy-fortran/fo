! The tools/ar symlink re-enters the test executable in this mode. Fortran
! owns listing mutations; POSIX calls only launch and capture the real archiver.
module fo_test_archive_fixture
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_loc, &
        c_null_char, c_null_ptr
    use, intrinsic :: iso_fortran_env, only: iostat_end, output_unit
    implicit none
    private

    integer, parameter :: MAX_LINE = 1024
    integer, parameter :: MAX_ARGUMENT = 16384
    public :: archive_fixture_main, archive_fixture_exit

    interface
        integer(c_int) function c_fork() bind(C, name='fork')
            import :: c_int
        end function c_fork

        integer(c_int) function c_creat(path, mode) bind(C, name='creat')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_creat

        integer(c_int) function c_dup2(old_descriptor, new_descriptor) &
                bind(C, name='dup2')
            import :: c_int
            integer(c_int), value :: old_descriptor, new_descriptor
        end function c_dup2

        integer(c_int) function c_close(descriptor) bind(C, name='close')
            import :: c_int
            integer(c_int), value :: descriptor
        end function c_close

        integer(c_int) function c_execv(path, arguments) bind(C, name='execv')
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            type(c_ptr), intent(in) :: arguments(*)
        end function c_execv

        integer(c_int) function c_waitpid(pid, status, options) &
                bind(C, name='waitpid')
            import :: c_int
            integer(c_int), value :: pid, options
            integer(c_int), intent(out) :: status
        end function c_waitpid

        integer(c_int) function c_getpid() bind(C, name='getpid')
            import :: c_int
        end function c_getpid

        subroutine c_exit(status) bind(C, name='_exit')
            import :: c_int
            integer(c_int), value :: status
        end subroutine c_exit
    end interface

contains

    integer function archive_fixture_main(real_ar) result(exit_code)
        character(len=*), intent(in) :: real_ar
        character(len=MAX_LINE) :: archive_path, operation, mode, platform
        character(len=32) :: pid_text
        character(:), allocatable :: listing_path
        character(len=MAX_LINE) :: listing(512)
        integer :: argument_count, argument_status, argument_index, line_count
        integer :: io_status

        exit_code = 64
        argument_count = command_argument_count()
        if (argument_count < 1) return
        call get_command_argument(1, operation, status=argument_status)
        if (argument_status /= 0) return
        mode = ''
        platform = ''
        call get_environment_variable('FO_ARCHIVE_MEMBER_MODE', mode)
        call get_environment_variable('FO_ARCHIVE_PLATFORM', platform)

        if (trim(operation) /= 't') then
            exit_code = run_native_ar(real_ar)
            if (exit_code /= 0) return
            ! Cover member-content mismatch and archive-header truncation apart.
            if (trim(operation) == 'rcs' .and. &
                (trim(mode) == 'truncate' .or. trim(mode) == 'corrupt-member')) then
                argument_index = 2
                if (trim(mode) == 'corrupt-member') argument_index = 3
                if (argument_count < argument_index) then
                    exit_code = 64
                    return
                end if
                call get_command_argument(argument_index, archive_path, &
                    status=argument_status)
                if (argument_status /= 0) then
                    exit_code = 64
                    return
                end if
                if (trim(mode) == 'truncate') then
                    call truncate_archive(trim(archive_path), io_status)
                else
                    call corrupt_object(trim(archive_path), io_status)
                end if
                exit_code = io_status
            end if
            return
        end if

        if (argument_count < 2) return
        call get_command_argument(2, archive_path, status=argument_status)
        if (argument_status /= 0) return
        write(pid_text, '(i0)') c_getpid()
        listing_path = trim(archive_path)//'.fo-fixture-listing.'//trim(pid_text)
        exit_code = run_native_ar(real_ar, listing_path)
        if (exit_code /= 0) then
            call delete_file(listing_path)
            return
        end if
        call read_listing(listing_path, listing, line_count, io_status)
        call delete_file(listing_path)
        if (io_status /= 0) then
            exit_code = io_status
            return
        end if
        call emit_listing(listing, line_count, trim(mode), trim(platform), exit_code)
    end function archive_fixture_main

    subroutine archive_fixture_exit(exit_code)
        integer, intent(in) :: exit_code

        flush (output_unit)
        call c_exit(int(exit_code, c_int))
    end subroutine archive_fixture_exit

    integer function run_native_ar(real_ar, output_path) result(exit_code)
        character(len=*), intent(in) :: real_ar
        character(len=*), optional, intent(in) :: output_path
        type(c_ptr), allocatable, target :: arguments(:)
        character(kind=c_char), allocatable, target :: storage(:, :)
        character(len=MAX_ARGUMENT) :: argument
        integer, allocatable :: argument_lengths(:)
        integer(c_int) :: child, output_descriptor, wait_status, waited
        integer :: argument_count, maximum_length, argument_status
        integer :: i, j, k, status

        exit_code = 125
        argument_count = command_argument_count()
        allocate(argument_lengths(0:argument_count))
        argument_lengths(0) = len_trim(real_ar)
        maximum_length = argument_lengths(0)
        do i = 1, argument_count
            call get_command_argument(i, length=argument_lengths(i), &
                status=argument_status)
            if (argument_status /= 0 .or. argument_lengths(i) > MAX_ARGUMENT) return
            maximum_length = max(maximum_length, argument_lengths(i))
        end do
        allocate(storage(maximum_length + 1, argument_count + 1))
        allocate(arguments(argument_count + 2))
        storage = c_null_char
        do i = 0, argument_count
            if (i == 0) then
                argument = real_ar
                j = len_trim(real_ar)
            else
                argument = ''
                call get_command_argument(i, argument, status=argument_status)
                if (argument_status /= 0) return
                j = argument_lengths(i)
            end if
            do k = 1, j
                storage(k, i + 1) = argument(k:k)
            end do
            arguments(i + 1) = c_loc(storage(1, i + 1))
        end do
        arguments(argument_count + 2) = c_null_ptr

        child = c_fork()
        if (child < 0_c_int) return
        if (child == 0_c_int) then
            if (present(output_path)) then
                output_descriptor = c_creat(trim(output_path)//c_null_char, &
                    384_c_int)
                if (output_descriptor < 0_c_int) call c_exit(126_c_int)
                status = c_dup2(output_descriptor, 1_c_int)
                if (status < 0) call c_exit(126_c_int)
                status = c_close(output_descriptor)
                if (status < 0) call c_exit(126_c_int)
            end if
            status = c_execv(trim(real_ar)//c_null_char, arguments)
            call c_exit(127_c_int)
        end if
        waited = c_waitpid(child, wait_status, 0_c_int)
        if (waited /= child) return
        if (iand(wait_status, 127_c_int) == 0_c_int) then
            exit_code = int(ishft(wait_status, -8))
        else
            exit_code = 128 + int(iand(wait_status, 127_c_int))
        end if
    end function run_native_ar

    subroutine read_listing(path, lines, line_count, status)
        character(len=*), intent(in) :: path
        character(len=MAX_LINE), intent(out) :: lines(:)
        integer, intent(out) :: line_count, status
        character(len=MAX_LINE) :: line
        integer :: unit, io_status

        lines = ''
        line_count = 0
        status = 125
        open (newunit=unit, file=trim(path), status='old', action='read', &
            iostat=io_status)
        if (io_status /= 0) return
        do
            read (unit, '(a)', iostat=io_status) line
            if (io_status == iostat_end) exit
            if (io_status /= 0 .or. line_count == size(lines)) then
                close (unit)
                return
            end if
            line_count = line_count + 1
            lines(line_count) = line
        end do
        close (unit, iostat=io_status)
        if (io_status == 0) status = 0
    end subroutine read_listing

    subroutine emit_listing(lines, line_count, mode, platform, exit_code)
        character(len=MAX_LINE), intent(inout) :: lines(:)
        integer, intent(inout) :: line_count
        character(len=*), intent(in) :: mode, platform
        integer, intent(out) :: exit_code
        integer :: i, duplicate_index, io_status
        logical :: has_sorted_index, missing_native_index

        exit_code = 0
        missing_native_index = .false.
        select case (mode)
        case ('missing')
            line_count = 0
        case ('duplicate')
            duplicate_index = 0
            do i = 1, line_count
                if (len_trim(lines(i)) == 0) cycle
                if (index(trim(lines(i)), '__.SYMDEF') == 1) cycle
                duplicate_index = i
                exit
            end do
            if (duplicate_index == 0 .or. line_count == size(lines)) then
                exit_code = 74
                return
            end if
            line_count = line_count + 1
            lines(line_count) = lines(duplicate_index)
        case ('apple-index')
            has_sorted_index = .false.
            do i = 1, line_count
                if (trim(lines(i)) == '__.SYMDEF SORTED') then
                    has_sorted_index = .true.
                    exit
                end if
            end do
            if (trim(platform) == 'Darwin') then
                if (.not. has_sorted_index) then
                    missing_native_index = .true.
                end if
            else if (.not. has_sorted_index) then
                if (line_count == size(lines)) then
                    exit_code = 75
                    return
                end if
                line_count = line_count + 1
                lines(line_count) = '__.SYMDEF SORTED'
            end if
        case ('extra-object')
            if (line_count == size(lines)) then
                exit_code = 75
                return
            end if
            line_count = line_count + 1
            lines(line_count) = 'injected-extra.o'
        case ('extra-payload')
            if (line_count == size(lines)) then
                exit_code = 75
                return
            end if
            line_count = line_count + 1
            lines(line_count) = 'injected-payload.dat'
        end select

        do i = 1, line_count
            write (output_unit, '(a)', iostat=io_status) trim(lines(i))
            if (io_status /= 0) then
                exit_code = 125
                return
            end if
        end do
        if (missing_native_index) exit_code = 73
    end subroutine emit_listing

    subroutine truncate_archive(path, status)
        character(len=*), intent(in) :: path
        integer, intent(out) :: status
        character(len=32) :: prefix
        integer :: unit, io_status

        status = 125
        ! Keep the native magic and only part of the first 60-byte member header.
        open (newunit=unit, file=trim(path), access='stream', &
            form='unformatted', status='old', action='read', iostat=io_status)
        if (io_status /= 0) return
        read (unit, iostat=io_status) prefix
        close (unit)
        if (io_status /= 0) return
        open (newunit=unit, file=trim(path), access='stream', &
            form='unformatted', status='replace', action='write', iostat=io_status)
        if (io_status /= 0) return
        write (unit, iostat=io_status) prefix
        close (unit, iostat=status)
        if (io_status /= 0) status = 125
    end subroutine truncate_archive

    subroutine corrupt_object(path, status)
        character(len=*), intent(in) :: path
        integer, intent(out) :: status
        integer :: unit, io_status

        status = 125
        open (newunit=unit, file=trim(path), access='stream', &
            form='unformatted', status='replace', action='write', iostat=io_status)
        if (io_status /= 0) return
        write (unit, iostat=io_status) '!<arch>'//achar(10)
        close (unit, iostat=status)
        if (io_status /= 0) status = 125
    end subroutine corrupt_object

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, io_status
        logical :: exists

        inquire (file=trim(path), exist=exists)
        if (.not. exists) return
        open (newunit=unit, file=trim(path), status='old', iostat=io_status)
        if (io_status /= 0) return
        close (unit, status='delete', iostat=io_status)
    end subroutine delete_file

end module fo_test_archive_fixture
