module fo_gremlin_journal
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private
    public :: journal_append, journal_read_page, journal_record_t
    public :: JOURNAL_OK, JOURNAL_INVALID, JOURNAL_IO_ERROR
    public :: JOURNAL_CONFLICT, JOURNAL_TOO_LARGE
    public :: JOURNAL_MAX_RECORD_BYTES

    integer, parameter :: JOURNAL_OK = 0
    integer, parameter :: JOURNAL_INVALID = 1
    integer, parameter :: JOURNAL_IO_ERROR = 2
    integer, parameter :: JOURNAL_CONFLICT = 3
    integer, parameter :: JOURNAL_TOO_LARGE = 4
    integer, parameter :: JOURNAL_MAX_RECORD_BYTES = 256 * 1024

    type :: journal_record_t
        character(len=:), allocatable :: json
    end type journal_record_t

    interface
        function fo_c_gremlin_journal_append(path, completion_id, record) &
                bind(C, name='fo_c_gremlin_journal_append') result(status)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(in) :: completion_id(*)
            character(kind=c_char), intent(in) :: record(*)
            integer(c_int) :: status
        end function fo_c_gremlin_journal_append

        function fo_c_gremlin_journal_validate_record(record) &
                bind(C, name='fo_c_gremlin_journal_validate_record') result(status)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: record(*)
            integer(c_int) :: status
        end function fo_c_gremlin_journal_validate_record
    end interface

contains

    subroutine journal_append(path, completion_id, jsonl_record, status, message)
        character(len=*), intent(in) :: path, completion_id, jsonl_record
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        integer(c_int) :: c_status
        character(len=:), allocatable :: record

        status = JOURNAL_INVALID
        message = 'invalid journal path, completion_id, or receipt'
        if (len_trim(path) == 0 .or. len_trim(completion_id) == 0 .or. &
            len_trim(jsonl_record) == 0) return
        if (index(path, achar(0)) > 0 .or. &
            index(completion_id, achar(0)) > 0 .or. &
            index(jsonl_record, achar(0)) > 0) return
        record = trim(jsonl_record)
        c_status = fo_c_gremlin_journal_append( &
            trim(path)//c_null_char, trim(completion_id)//c_null_char, &
            record//c_null_char)
        status = int(c_status)
        select case (status)
        case (JOURNAL_OK)
            message = ''
        case (JOURNAL_INVALID)
            message = 'receipt must be valid JSON with a matching completion_id and pass/fail outcome'
        case (JOURNAL_CONFLICT)
            message = 'conflicting completion_id already exists in journal'
        case (JOURNAL_TOO_LARGE)
            message = 'receipt exceeds the maximum journal record size'
        case default
            status = JOURNAL_IO_ERROR
            message = 'journal append or durability sync failed'
        end select
    end subroutine journal_append

    subroutine journal_read_page(path, byte_cursor, max_records, max_bytes, &
            records, next_cursor, status, message)
        character(len=*), intent(in) :: path
        integer(int64), intent(in) :: byte_cursor, max_bytes
        integer, intent(in) :: max_records
        type(journal_record_t), allocatable, intent(out) :: records(:)
        integer(int64), intent(out) :: next_cursor
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: buffer(:), exact(:)
        character(len=1) :: byte
        character(len=JOURNAL_MAX_RECORD_BYTES) :: line
        integer(int64) :: current, line_start, page_bytes, file_size
        integer :: unit, ios, count, line_length, i
        integer(c_int) :: validation_status
        logical :: exists, pending_byte

        status = JOURNAL_OK
        message = ''
        next_cursor = byte_cursor
        allocate (records(0))
        if (byte_cursor < 0 .or. max_records < 1 .or. max_bytes < 1 .or. &
            len_trim(path) == 0 .or. index(path, achar(0)) > 0) then
            status = JOURNAL_INVALID
            message = 'invalid journal page request'
            return
        end if
        inquire (file=trim(path), exist=exists, iostat=ios)
        if (ios /= 0) then
            status = JOURNAL_IO_ERROR
            message = 'cannot inspect journal path'
            return
        end if
        if (.not. exists) then
            if (byte_cursor == 0_int64) return
            status = JOURNAL_INVALID
            message = 'journal cursor is beyond the empty journal'
            return
        end if
        inquire (file=trim(path), size=file_size, iostat=ios)
        if (ios /= 0 .or. file_size < 0_int64) then
            status = JOURNAL_IO_ERROR
            message = 'cannot inspect journal size'
            return
        end if
        if (byte_cursor > file_size) then
            status = JOURNAL_INVALID
            message = 'journal cursor is beyond end of file'
            return
        end if
        if (file_size == 0_int64) return
        open (newunit=unit, file=trim(path), access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) then
            status = JOURNAL_IO_ERROR
            message = 'cannot open journal for reading'
            return
        end if
        if (byte_cursor > 0_int64) then
            read (unit, pos=byte_cursor, iostat=ios) byte
            if (ios /= 0) then
                close (unit)
                status = JOURNAL_IO_ERROR
                message = 'cannot validate journal cursor'
                return
            end if
            if (byte /= new_line('a')) then
                close (unit)
                status = JOURNAL_INVALID
                message = 'journal cursor is not at a complete record boundary'
                return
            end if
            if (byte_cursor == file_size) then
                close (unit)
                return
            end if
            read (unit, pos=byte_cursor + 1_int64, iostat=ios) byte
            if (ios /= 0) then
                close (unit)
                status = JOURNAL_IO_ERROR
                message = 'cannot seek journal cursor'
                return
            end if
            pending_byte = .true.
        else
            pending_byte = .false.
        end if

        allocate (buffer(max_records))
        count = 0
        current = byte_cursor
        page_bytes = 0_int64
        do
            line_start = current
            line_length = 0
            ios = 0
            do
                if (pending_byte) then
                    pending_byte = .false.
                else
                    read (unit, iostat=ios) byte
                    if (ios /= 0) exit
                end if
                current = current + 1_int64
                if (byte == new_line('a')) exit
                if (line_length >= JOURNAL_MAX_RECORD_BYTES) then
                    status = JOURNAL_TOO_LARGE
                    message = 'journal record exceeds the maximum record size'
                    current = line_start
                    exit
                end if
                line_length = line_length + 1
                line(line_length:line_length) = byte
            end do
            if (status /= JOURNAL_OK) exit
            if (ios /= 0) then
                current = line_start
                exit
            end if
            if (line_length == 0) then
                status = JOURNAL_INVALID
                message = 'empty journal records are invalid'
                current = line_start
                exit
            end if
            if (count == max_records .or. &
                page_bytes + int(line_length + 1, int64) > max_bytes) then
                current = line_start
                if (count == 0) then
                    status = JOURNAL_TOO_LARGE
                    message = 'journal record exceeds the requested page byte limit'
                end if
                exit
            end if
            if (index(line(1:line_length), achar(0)) > 0) then
                status = JOURNAL_INVALID
            else
                validation_status = fo_c_gremlin_journal_validate_record( &
                    line(1:line_length)//c_null_char)
                if (validation_status /= 0_c_int) status = JOURNAL_INVALID
            end if
            if (status /= JOURNAL_OK) then
                message = 'journal contains an invalid or non-pass/fail receipt'
                current = line_start
                exit
            end if
            count = count + 1
            allocate (character(len=line_length) :: buffer(count)%json)
            buffer(count)%json = line(1:line_length)
            page_bytes = page_bytes + int(line_length + 1, int64)
            next_cursor = current
        end do
        if (status == JOURNAL_OK) next_cursor = current
        close (unit)
        if (count > 0) then
            allocate (exact(count))
            do i = 1, count
                call move_alloc(buffer(i)%json, exact(i)%json)
            end do
            call move_alloc(exact, records)
        end if
    end subroutine journal_read_page

end module fo_gremlin_journal
