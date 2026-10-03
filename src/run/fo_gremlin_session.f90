module fo_gremlin_session
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_gremlin_journal, only: journal_read_page, journal_append, journal_record_t, &
        JOURNAL_OK, JOURNAL_INVALID, JOURNAL_IO_ERROR, JOURNAL_MAX_RECORD_BYTES
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_read, &
        gremlin_session_recovery_complete, gremlin_session_release, &
        GREMLIN_STATE_TEXT_MAX
    use fo_gremlin_request, only: gremlin_request_t
    use fo_util, only: extract_json_field
    use fo_fs, only: fs_remove_file
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096

    public :: gremlin_resolve_read_session, gremlin_load_terminal_session
    public :: gremlin_get_session_journal_path, gremlin_stage_recovery_journal
    public :: gremlin_recover_owner_journal, gremlin_publish_terminal_snapshot
    public :: gremlin_release_stopped_session

    interface
        function c_terminal_journal_path(project, lane, session, path, cap) &
                bind(C, name='fo_gremlin_terminal_journal_path') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project(*), lane(*), session(*)
            character(kind=c_char), intent(out) :: path(*)
            integer(c_int), value :: cap
            integer(c_int) :: ierr
        end function c_terminal_journal_path

        function c_terminal_publish(project, lane, session, status, status_len) &
                bind(C, name='fo_gremlin_terminal_publish') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project(*), lane(*), session(*)
            character(kind=c_char), intent(in) :: status(*)
            integer(c_int), value :: status_len
            integer(c_int) :: ierr
        end function c_terminal_publish

        function c_terminal_read(project, lane, session, status, status_cap, &
                journal, journal_cap) bind(C, name='fo_gremlin_terminal_read') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project(*), lane(*), session(*)
            character(kind=c_char), intent(out) :: status(*), journal(*)
            integer(c_int), value :: status_cap, journal_cap
            integer(c_int) :: ierr
        end function c_terminal_read
    end interface

contains
    subroutine gremlin_resolve_read_session(project_dir, request, session, status_text, &
            owner_pid, is_live, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        type(gremlin_session_t), intent(out) :: session
        character(len=*), intent(out) :: status_text
        integer, intent(out) :: owner_pid, ierr
        logical, intent(out) :: is_live
        character(len=*), intent(out) :: message

        character(len=128) :: live_id, owner_start
        character(len=GREMLIN_STATE_TEXT_MAX) :: live_status
        character(len=PATH_LEN) :: journal_path, live_message

        session = gremlin_session_t()
        status_text = ''
        owner_pid = 0
        is_live = .false.
        call gremlin_session_read(project_dir, trim(request%lane_id), live_id, &
            owner_pid, owner_start, live_status, ierr, live_message)
        if (len_trim(request%session_id) == 0) then
            if (ierr /= 0) then
                message = trim(live_message)
                return
            end if
            call fill_read_session(project_dir, request%lane_id, trim(live_id), &
                session, ierr, message)
            if (ierr /= 0) return
            status_text = live_status
            is_live = .true.
            return
        end if
        if (ierr == 0) then
            if (trim(request%session_id) == trim(live_id)) then
                call fill_read_session(project_dir, request%lane_id, trim(live_id), &
                    session, ierr, message)
                if (ierr /= 0) return
                status_text = live_status
                is_live = .true.
                return
            end if
        end if
        call gremlin_load_terminal_session(project_dir, request%lane_id, &
            trim(request%session_id), session, status_text, ierr, message)
        if (ierr /= 0) then
            if (len_trim(live_id) > 0) then
                message = 'session_id does not match the live owner and has no terminal record'
            else
                message = 'no live or terminal Gremlin session matches session_id'
            end if
        end if
    end subroutine gremlin_resolve_read_session

    subroutine fill_read_session(project_dir, lane_id, session_id, session, ierr, message)
        character(len=*), intent(in) :: project_dir, lane_id, session_id
        type(gremlin_session_t), intent(out) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: journal_path

        call gremlin_get_session_journal_path(project_dir, lane_id, session_id, &
            journal_path, ierr, message)
        if (ierr /= 0) return
        session%session_id = trim(session_id)
        session%lane_id = trim(lane_id)
        session%project_key = trim(project_dir)
        session%state_dir = journal_parent(journal_path)
        session%owner = .false.
        session%attached = .false.
    end subroutine fill_read_session

    subroutine gremlin_load_terminal_session(project_dir, lane_id, session_id, session, &
            status_text, ierr, message)
        character(len=*), intent(in) :: project_dir, lane_id, session_id
        type(gremlin_session_t), intent(out) :: session
        character(len=*), intent(out) :: status_text
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(kind=c_char) :: c_status(GREMLIN_STATE_TEXT_MAX + 1)
        character(kind=c_char) :: c_journal(PATH_LEN + 1)
        integer(c_int) :: c_error
        character(len=:), allocatable :: path

        c_status = c_null_char
        c_journal = c_null_char
        c_error = c_terminal_read(trim(project_dir)//c_null_char, &
            trim(lane_id)//c_null_char, trim(session_id)//c_null_char, &
            c_status, int(size(c_status), c_int), c_journal, &
            int(size(c_journal), c_int))
        ierr = int(c_error)
        if (ierr /= 0) then
            message = 'cannot read exact Gremlin terminal snapshot ('//trim(int_text(ierr))//')'
            return
        end if
        status_text = c_buffer_text(c_status)
        path = c_buffer_text(c_journal)
        if (len(path) <= len('/journal.jsonl')) then
            ierr = 1
            message = 'terminal snapshot has an invalid journal path'
            return
        end if
        if (path(len(path) - len('/journal.jsonl') + 1:) /= '/journal.jsonl') then
            ierr = 1
            message = 'terminal snapshot has an invalid journal path'
            return
        end if
        session%session_id = trim(session_id)
        session%lane_id = trim(lane_id)
        session%project_key = trim(project_dir)
        session%state_dir = path(:len(path) - len('/journal.jsonl'))
        session%owner = .false.
        session%attached = .false.
        message = ''
    end subroutine gremlin_load_terminal_session

    subroutine gremlin_get_session_journal_path(project_dir, lane_id, session_id, path, &
            ierr, message)
        character(len=*), intent(in) :: project_dir, lane_id, session_id
        character(len=*), intent(out) :: path
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(kind=c_char) :: c_path(PATH_LEN + 1)
        integer(c_int) :: c_error

        c_path = c_null_char
        c_error = c_terminal_journal_path(trim(project_dir)//c_null_char, &
            trim(lane_id)//c_null_char, trim(session_id)//c_null_char, c_path, &
            int(size(c_path), c_int))
        ierr = int(c_error)
        path = c_buffer_text(c_path)
        if (ierr == 0) then
            message = ''
        else
            message = 'cannot locate Gremlin session journal ('//trim(int_text(ierr))//')'
        end if
    end subroutine gremlin_get_session_journal_path

    subroutine gremlin_stage_recovery_journal(project_dir, session, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(gremlin_session_t), intent(in) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: source_path, recovery_path

        call gremlin_get_session_journal_path(project_dir, session%lane_id, &
            session%recovered_session_id, source_path, ierr, message)
        if (ierr /= 0) return
        recovery_path = trim(session%state_dir)//'/recovery.jsonl'
        call copy_journal_records(trim(source_path), trim(recovery_path), .false., &
            ierr, message)
    end subroutine gremlin_stage_recovery_journal

    subroutine gremlin_recover_owner_journal(project_dir, session, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(gremlin_session_t), intent(in) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: journal_path, recovery_path

        if (allocated(session%recovered_session_id)) then
            if (len_trim(session%recovered_session_id) > 0) then
                call gremlin_stage_recovery_journal(project_dir, session, ierr, message)
                if (ierr /= 0) return
            end if
        end if
        call gremlin_get_session_journal_path(project_dir, session%lane_id, &
            session%session_id, journal_path, ierr, message)
        if (ierr /= 0) return
        recovery_path = trim(session%state_dir)//'/recovery.jsonl'
        call copy_journal_records(trim(recovery_path), trim(journal_path), .true., &
            ierr, message)
        if (ierr /= 0) return
        call gremlin_session_recovery_complete(session, ierr, message)
    end subroutine gremlin_recover_owner_journal

    subroutine copy_journal_records(source_path, target_path, remove_source, ierr, &
            message)
        character(len=*), intent(in) :: source_path, target_path
        logical, intent(in) :: remove_source
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        character(len=512) :: completion_id
        integer(int64) :: cursor, next_cursor, page_bytes
        integer :: status, i, ios
        logical :: source_exists

        ierr = JOURNAL_OK
        message = ''
        if (trim(source_path) == trim(target_path)) return
        inquire (file=trim(source_path), exist=source_exists, iostat=ios)
        if (ios /= 0) then
            ierr = JOURNAL_IO_ERROR
            message = 'cannot inspect recovery journal'
            return
        end if
        if (.not. source_exists) return

        cursor = 0_int64
        page_bytes = int(JOURNAL_MAX_RECORD_BYTES, int64)*16_int64
        do
            call journal_read_page(trim(source_path), cursor, 16, page_bytes, records, &
                next_cursor, status, message)
            if (status /= JOURNAL_OK) then
                ierr = status
                return
            end if
            if (size(records) == 0) exit
            do i = 1, size(records)
                completion_id = ''
                call extract_json_field(records(i)%json, 'completion_id', completion_id)
                if (len_trim(completion_id) == 0) then
                    ierr = JOURNAL_INVALID
                    message = 'recovery journal record has no completion_id'
                    return
                end if
                call journal_append(trim(target_path), trim(completion_id), &
                    records(i)%json, status, message)
                if (status /= JOURNAL_OK) then
                    ierr = status
                    return
                end if
            end do
            if (next_cursor <= cursor) then
                ierr = JOURNAL_INVALID
                message = 'recovery journal cursor did not advance'
                return
            end if
            cursor = next_cursor
        end do

        if (.not. remove_source) return
        call fs_remove_file(trim(source_path))
        inquire (file=trim(source_path), exist=source_exists, iostat=ios)
        if (ios /= 0) then
            ierr = JOURNAL_IO_ERROR
            message = 'cannot clear imported recovery journal'
        else if (source_exists) then
            ierr = JOURNAL_IO_ERROR
            message = 'cannot clear imported recovery journal'
        end if
    end subroutine copy_journal_records

    subroutine gremlin_publish_terminal_snapshot(project_dir, session, ierr, message, &
            status_override)
        character(len=*), intent(in) :: project_dir
        type(gremlin_session_t), intent(in) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: status_override

        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=PATH_LEN) :: journal_path
        integer(int64) :: file_size
        integer :: ios, n, unit
        logical :: exists
        integer(c_int) :: c_error

        if (present(status_override)) then
            if (len_trim(status_override) < 2 .or. &
                len_trim(status_override) > len(status_text)) then
                ierr = 1
                message = 'constructed Gremlin terminal status is invalid'
                return
            end if
            status_text = status_override
            n = len_trim(status_override)
        else
            file_size = 0_int64
            inquire (file=session%state_dir//'/status', exist=exists, size=file_size, &
                iostat=ios)
            if (ios /= 0 .or. .not. exists .or. file_size < 2_int64 .or. &
                file_size > int(len(status_text), int64)) then
                ierr = 1
                message = 'cannot snapshot Gremlin terminal status before release'
                return
            end if
            open (newunit=unit, file=session%state_dir//'/status', access='stream', &
                form='unformatted', status='old', action='read', iostat=ios)
            if (ios /= 0) then
                ierr = ios
                message = 'cannot open Gremlin terminal status before release'
                return
            end if
            n = int(file_size)
            read (unit, pos=1, iostat=ios) status_text(:n)
            close (unit)
            if (ios /= 0) then
                ierr = ios
                message = 'cannot read full Gremlin terminal status before release'
                return
            end if
        end if
        call gremlin_get_session_journal_path(project_dir, session%lane_id, &
            session%session_id, journal_path, ierr, message)
        if (ierr /= 0) return
        c_error = c_terminal_publish(trim(project_dir)//c_null_char, &
            session%lane_id//c_null_char, session%session_id//c_null_char, &
            status_text, int(n, c_int))
        ierr = int(c_error)
        if (ierr == 0) then
            message = ''
        else
            message = 'cannot publish immutable Gremlin terminal snapshot ('// &
                trim(int_text(ierr))//')'
        end if
    end subroutine gremlin_publish_terminal_snapshot

    subroutine gremlin_release_stopped_session(project_dir, session, status_text, &
            ierr, message)
        character(len=*), intent(in) :: project_dir, status_text
        type(gremlin_session_t), intent(inout) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        if (.not. session%owner) then
            ierr = 1
            message = 'only the Gremlin owner may publish a stopped terminal status'
            return
        end if
        if (.not. allocated(session%session_id) .or. &
            .not. allocated(session%lane_id)) then
            ierr = 1
            message = 'Gremlin owner identity is incomplete'
            return
        end if
        if (index(status_text, '"state":"stopped"') == 0) then
            ierr = 1
            message = 'terminal release requires a constructed stopped status'
            return
        end if
        call gremlin_publish_terminal_snapshot(project_dir, session, ierr, message, &
            status_override=trim(status_text))
        if (ierr /= 0) return
        call gremlin_session_release(session, ierr, message)
        if (ierr /= 0) then
            message = 'cannot release Gremlin owner after durable terminal snapshot: '// &
                trim(message)
        end if
    end subroutine gremlin_release_stopped_session

    function c_buffer_text(buffer) result(text)
        character(kind=c_char), intent(in) :: buffer(:)
        character(len=:), allocatable :: text
        integer :: n, i

        n = 0
        do i = 1, size(buffer)
            if (buffer(i) == c_null_char) exit
            n = n + 1
        end do
        allocate (character(len=n) :: text)
        do i = 1, n
            text(i:i) = buffer(i)
        end do
    end function c_buffer_text

    function journal_parent(path) result(parent)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: parent
        integer :: slash

        slash = index(trim(path), '/', back=.true.)
        if (slash > 0) then
            parent = path(:slash - 1)
        else
            parent = ''
        end if
    end function journal_parent

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
    end function int_text
end module fo_gremlin_session
