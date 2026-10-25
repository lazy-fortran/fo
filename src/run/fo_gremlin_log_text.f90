module fo_gremlin_log_text
    !! A failed candidate build runs `fo build` with a private scratch TMPDIR that
    !! is deleted when the view is released. The child's stderr names its full log
    !! there ("fo: full log: PATH"); keep that content in the retained lane log.
    implicit none
    private

    character(len=*), parameter :: MARKER = 'fo: full log: '
    integer, parameter :: MAX_RETAINED = 262144
    public :: gremlin_retain_build_log, gremlin_log_tail

contains

    subroutine gremlin_retain_build_log(log_file, source_log)
        !! Append the tail of the full log named in `log_file` to `log_file`.
        !! Call before the build's scratch directory is released.
        character(len=*), intent(in) :: log_file
        character(:), allocatable, intent(out), optional :: source_log

        character(:), allocatable :: text, full
        character(len=:), allocatable :: path
        integer :: start, finish, unit, ios, size_bytes

        if (present(source_log)) source_log = ''
        call read_stream(log_file, text, 16384)
        if (.not. allocated(text)) return
        start = index(text, MARKER, back=.true.)
        if (start == 0) return
        start = start + len(MARKER)
        finish = index(text(start:), new_line('a'))
        if (finish == 0) then
            finish = len(text)
        else
            finish = start + finish - 2
        end if
        if (finish < start) return
        path = trim(text(start:finish))
        if (present(source_log)) source_log = path
        call read_stream(path, full, MAX_RETAINED, size_bytes)
        if (.not. allocated(full)) return
        open (newunit=unit, file=log_file, status='old', position='append', &
            action='write', iostat=ios)
        if (ios /= 0) return
        write (unit, '(a)') new_line('a')//'fo gremlin: full build log retained '// &
            'from '//path
        if (size_bytes > MAX_RETAINED) then
            write (unit, '(a)') '[truncated to the last 262144 bytes]'
            write (unit, '(a)') full
        else
            write (unit, '(a)') full
        end if
        close (unit)
    end subroutine gremlin_retain_build_log

    function gremlin_log_tail(path, limit) result(tail)
        !! Last `limit` bytes of a log with line breaks flattened to spaces, for
        !! error messages. Empty when the log is missing or empty.
        character(len=*), intent(in) :: path
        integer, intent(in) :: limit
        character(:), allocatable :: tail
        character(:), allocatable :: text
        integer :: i

        tail = ''
        call read_stream(path, text, limit)
        if (.not. allocated(text)) return
        tail = trim(text(max(1, len(text) - limit + 1):))
        do i = 1, len(tail)
            if (tail(i:i) == new_line('a') .or. tail(i:i) == achar(13)) tail(i:i) = ' '
        end do
        tail = trim(adjustl(tail))
    end function gremlin_log_tail

    subroutine read_stream(path, text, limit, original_size)
        character(len=*), intent(in) :: path
        character(:), allocatable, intent(out) :: text
        integer, intent(in) :: limit
        integer, intent(out), optional :: original_size
        integer :: unit, ios, file_size, kept

        inquire (file=path, size=file_size, iostat=ios)
        if (ios /= 0 .or. file_size < 1) return
        if (present(original_size)) original_size = file_size
        kept = min(file_size, max(0, limit))
        allocate (character(len=kept) :: text)
        open (newunit=unit, file=path, status='old', action='read', &
            access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            deallocate (text)
            return
        end if
        read (unit, pos=file_size - kept + 1, iostat=ios) text
        close (unit)
        if (ios /= 0) deallocate (text)
    end subroutine read_stream

end module fo_gremlin_log_text
