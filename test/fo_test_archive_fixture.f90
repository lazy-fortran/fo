! The tools/ar symlink re-enters the test executable in this mode. Fortran
! owns listing mutations; the shared harness captures the real native archiver.
module fo_test_archive_fixture
    use, intrinsic :: iso_fortran_env, only: iostat_end, output_unit, error_unit
    use fo_process, only: process_getpid, process_exit
    use fo_test_harness, only: string_list_t, process_result_t, list_add, run_process, write_text
    implicit none
    private

    integer, parameter :: MAX_LINE = 1024
    integer, parameter :: MAX_ARGUMENT = 16384
    public :: archive_fixture_main, archive_fixture_exit

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
        write(pid_text, '(i0)') process_getpid()
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
        call process_exit(exit_code)
    end subroutine archive_fixture_exit

    integer function run_native_ar(real_ar, output_path) result(exit_code)
        character(len=*), intent(in) :: real_ar
        character(len=*), optional, intent(in) :: output_path
        character(:), allocatable :: argument
        type(string_list_t) :: command
        type(process_result_t) :: result
        integer :: i, length, status

        exit_code = 125
        call list_add(command, real_ar)
        do i = 1, command_argument_count()
            call get_command_argument(i, length=length, status=status)
            if (status /= 0 .or. length > MAX_ARGUMENT) return
            allocate(character(len=length) :: argument)
            call get_command_argument(i, argument, status=status)
            if (status /= 0) return
            call list_add(command, argument)
            deallocate(argument)
        end do
        call run_process(command, '.', result)
        if (result%runner_failed .or. .not. result%reaped) return
        if (present(output_path)) then
            call write_text(output_path, result%stdout)
        else
            write(output_unit, '(a)', advance='no') result%stdout
        end if
        write(error_unit, '(a)', advance='no') result%stderr
        if (result%term_signal > 0) then
            exit_code = 128 + result%term_signal
        else
            exit_code = result%exit_code
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
