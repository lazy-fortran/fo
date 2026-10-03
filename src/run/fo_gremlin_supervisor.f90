module fo_gremlin_supervisor
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_long_long
    use fo_build_backend, only: BACKEND_NATIVE, backend_t, detect_backend
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_check, only: fo_changed_modules
    use fo_fpm_config, only: DEP_PATH, fpm_config_t, fpm_config_parse, dep_kind
    use fo_gremlin_generation, only: generation_context_t, generation_input_t, &
        generation_t, generation_capture
    use fo_gremlin_journal, only: journal_append, journal_read_page, &
        journal_record_t, JOURNAL_OK
    use fo_gremlin_policy, only: select_gremlin_tests, shuffle_gremlin_tests, &
        GREMLIN_POLICY_OK
    use fo_gremlin_state, only: gremlin_session_t, gremlin_lease_t, &
        gremlin_session_acquire, &
        gremlin_session_publish, gremlin_session_read, gremlin_session_release, &
        gremlin_session_request_stop, gremlin_session_stop_requested, &
        gremlin_lease_release, gremlin_generation_register_at, &
        gremlin_generation_lease_acquire_at, gremlin_generation_pin_at, &
        gremlin_generation_root, &
        GREMLIN_STATE_TEXT_MAX
    use fo_gfortran_build, only: gfortran_selected_test_names
    use fo_process, only: argv_push, process_cancel_pid, &
        process_poll_pid, process_start_argv_logged
    use fo_scan_types, only: MAX_PATH
    use fo_util, only: extract_json_field, json_bool, json_int, make_tmpfile, &
        read_text_file
    use fx_dag, only: dag_t, MAX_NODES
    use fx_json_build, only: json_escape_string
    use fo_fs, only: fs_find_executable, fs_make_dir, fs_sleep_ms, &
        fs_tree_fingerprint
    implicit none
    private

    integer, parameter :: TEXT_LEN = 65536, NAME_LEN = 128, PATH_LEN = 4096
    integer, parameter :: DEFAULT_RANDOM = 32, DEFAULT_CAMPAIGN = 60
    integer, parameter :: DEFAULT_CASE_TIMEOUT = 5, MAX_CASE_TIMEOUT = 5
    integer, parameter :: MAX_LANE_LEN = 96

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

    type :: json_reader_t
        character(len=:), allocatable :: text
        integer :: position = 1
    end type json_reader_t

    type :: gremlin_request_t
        character(len=MAX_LANE_LEN) :: lane_id = 'default'
        character(len=128) :: session_id = ''
        character(len=128) :: case_id = ''
        integer :: random_count = DEFAULT_RANDOM
        integer :: seed = 0
        integer :: campaign_seconds = DEFAULT_CAMPAIGN
        integer :: timeout_seconds = DEFAULT_CASE_TIMEOUT
        integer :: jobs = 1
        integer(int64) :: cursor = 0_int64
        integer :: wait_ms = 5000
        integer :: max_records = 32
        integer(int64) :: max_bytes = 262144_int64
        logical :: only_changed = .false.
        logical :: shuffle = .false.
        logical :: fail_on_failure = .false.
        character(len=HASH_LEN) :: generation_id = ''
        character(len=NAME_LEN) :: targets(MAX_NODES) = ''
        integer :: n_targets = 0
    end type gremlin_request_t

    type :: child_t
        integer :: pid = 0
        integer :: campaign = 0
        integer :: case_index = 0
        integer :: seed = 0
        character(len=PATH_LEN) :: log_file = ''
        character(len=NAME_LEN) :: case_name = ''
        real :: started_at = 0.0
    end type child_t

    public :: gremlin_handle

contains

    subroutine gremlin_handle(action, project_dir, request_json, response_json, exitcode)
        character(len=*), intent(in) :: action, project_dir, request_json
        character(len=:), allocatable, intent(out) :: response_json
        integer, intent(out) :: exitcode

        type(gremlin_request_t) :: request
        type(gremlin_session_t) :: session
        character(len=PATH_LEN) :: error_message, owner_start
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=128) :: session_id
        integer :: ierr, owner_pid

        response_json = ''
        exitcode = 0
        call parse_request(action, request_json, request, ierr, error_message)
        if (ierr /= 0) then
            call error_response(action, trim(error_message), response_json)
            exitcode = 2
            return
        end if
        if (len_trim(project_dir) == 0) then
            call error_response(action, 'project directory is required', response_json)
            exitcode = 2
            return
        end if

        select case (trim(action))
        case ('start', 'gremlin_start')
            call handle_start(project_dir, request, response_json, exitcode)
        case ('run')
            call run_owner(project_dir, request, response_json, exitcode)
        case ('status')
            call handle_status(project_dir, request, response_json, exitcode)
        case ('events')
            call handle_events(project_dir, request, response_json, exitcode)
        case ('wait')
            call handle_wait(project_dir, request, response_json, exitcode)
        case ('failures')
            call handle_failures(project_dir, request, response_json, exitcode)
        case ('reproduce')
            call handle_reproduce(project_dir, request, response_json, exitcode)
        case ('stop')
            call gremlin_session_read(project_dir, trim(request%lane_id), &
                session_id, owner_pid, owner_start, status_text, ierr, error_message)
            if (len_trim(request%session_id) == 0 .and. ierr == 0) then
                request%session_id = trim(session_id)
            end if
            if (len_trim(request%session_id) > 0 .and. ierr == 0) then
                if (trim(request%session_id) == trim(session_id)) then
                    call gremlin_session_request_stop(project_dir, &
                        trim(request%lane_id), trim(request%session_id), &
                        ierr, error_message)
                else
                    call load_terminal_session(project_dir, request%lane_id, &
                        trim(request%session_id), session, status_text, ierr, error_message)
                    if (ierr == 0) then
                        call simple_response(action, request%lane_id, &
                            request%session_id, 'stopped', response_json)
                        return
                    end if
                end if
            else if (len_trim(request%session_id) > 0) then
                call load_terminal_session(project_dir, request%lane_id, &
                    trim(request%session_id), session, status_text, ierr, error_message)
                if (ierr == 0) then
                    call simple_response(action, request%lane_id, &
                        request%session_id, 'stopped', response_json)
                    return
                end if
            end if
            if (ierr /= 0) then
                call error_response(action, trim(error_message), response_json)
                exitcode = 2
            else
                call simple_response(action, request%lane_id, request%session_id, &
                    'stopping', response_json)
            end if
        case default
            call error_response(action, 'unknown Gremlin action', response_json)
            exitcode = 2
        end select
    end subroutine gremlin_handle

    subroutine resolve_read_session(project_dir, request, session, status_text, &
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
        call load_terminal_session(project_dir, request%lane_id, &
            trim(request%session_id), session, status_text, ierr, message)
        if (ierr /= 0) then
            if (len_trim(live_id) > 0) then
                message = 'session_id does not match the live owner and has no terminal record'
            else
                message = 'no live or terminal Gremlin session matches session_id'
            end if
        end if
    end subroutine resolve_read_session

    subroutine fill_read_session(project_dir, lane_id, session_id, session, ierr, message)
        character(len=*), intent(in) :: project_dir, lane_id, session_id
        type(gremlin_session_t), intent(out) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: journal_path

        call get_session_journal_path(project_dir, lane_id, session_id, &
            journal_path, ierr, message)
        if (ierr /= 0) return
        session%session_id = trim(session_id)
        session%lane_id = trim(lane_id)
        session%project_key = trim(project_dir)
        session%state_dir = journal_parent(journal_path)
        session%owner = .false.
        session%attached = .false.
    end subroutine fill_read_session

    subroutine load_terminal_session(project_dir, lane_id, session_id, session, &
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
    end subroutine load_terminal_session

    subroutine get_session_journal_path(project_dir, lane_id, session_id, path, &
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
    end subroutine get_session_journal_path

    subroutine publish_terminal_snapshot(project_dir, session, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(gremlin_session_t), intent(in) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=PATH_LEN) :: journal_path
        integer(int64) :: file_size
        integer :: ios, n, unit
        logical :: exists
        integer(c_int) :: c_error

        file_size = 0_int64
        inquire (file=session%state_dir//'/status', exist=exists, size=file_size, iostat=ios)
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
        call get_session_journal_path(project_dir, session%lane_id, &
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
    end subroutine publish_terminal_snapshot

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

    subroutine parse_request(action, json, request, ierr, message)
        character(len=*), intent(in) :: action
        character(len=*), intent(in) :: json
        type(gremlin_request_t), intent(out) :: request
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(json_reader_t) :: reader
        character(len=NAME_LEN) :: key
        character(len=PATH_LEN) :: value
        logical :: is_string, seen(17)
        integer :: field

        ierr = 0
        message = ''
        if (len_trim(json) > 65536) then
            ierr = 1
            message = 'Gremlin request exceeds 65536 characters'
            return
        end if
        seen = .false.
        call reader_init(reader, json)
        call json_expect(reader, '{', ierr, message)
        if (ierr /= 0) return
        call json_skip_space(reader)
        if (reader%position <= len(reader%text)) then
            if (reader%text(reader%position:reader%position) /= '}') then
                do
                    call json_read_string(reader, key, ierr, message)
                    if (ierr /= 0) return
                    call json_expect(reader, ':', ierr, message)
                    if (ierr /= 0) return
                    field = request_field(action, trim(key))
                    if (field == 0) then
                        ierr = 1
                        message = 'unsupported Gremlin request field: '//trim(key)
                        return
                    end if
                    if (seen(field)) then
                        ierr = 1
                        message = 'duplicate Gremlin request field: '//trim(key)
                        return
                    end if
                    seen(field) = .true.
                    if (field == 10) then
                        call parse_targets(reader, request, ierr, message)
                        if (ierr /= 0) return
                    else
                        call json_read_scalar(reader, value, is_string, ierr, message)
                        if (ierr /= 0) return
                        call set_request_field(request, field, value, is_string, &
                            ierr, message)
                        if (ierr /= 0) return
                    end if
                    call json_skip_space(reader)
                    if (reader%position > len(reader%text)) exit
                    if (reader%text(reader%position:reader%position) == '}') exit
                    call json_expect(reader, ',', ierr, message)
                    if (ierr /= 0) return
                end do
            end if
        end if
        call json_expect(reader, '}', ierr, message)
        if (ierr /= 0) return
        call json_skip_space(reader)
        if (reader%position <= len(reader%text)) then
            ierr = 1
            message = 'trailing text after Gremlin request object'
            return
        end if
        if (len_trim(request%lane_id) == 0 .or. len_trim(request%lane_id) > MAX_LANE_LEN .or. &
            index(request%lane_id, '/') > 0 .or. &
            index(request%lane_id, achar(10)) > 0 .or. &
            index(request%lane_id, achar(92)) > 0) then
            ierr = 1
            message = 'lane_id must be a short path-free string'
            return
        end if
        if (len_trim(request%session_id) > 0) then
            if (index(request%session_id, '/') > 0 .or. &
                index(request%session_id, achar(10)) > 0) then
                ierr = 1
                message = 'session_id contains an invalid character'
                return
            end if
        end if
        if (request%n_targets > 0 .and. action /= 'start' .and. action /= 'run' .and. &
            action /= 'gremlin_start') then
            ierr = 1
            message = 'targets are only supported when starting a campaign'
            return
        end if
    end subroutine parse_request

    subroutine reader_init(reader, text)
        type(json_reader_t), intent(out) :: reader
        character(len=*), intent(in) :: text

        reader%text = trim(text)
        reader%position = 1
    end subroutine reader_init

    subroutine json_skip_space(reader)
        type(json_reader_t), intent(inout) :: reader

        do while (reader%position <= len(reader%text))
            select case (reader%text(reader%position:reader%position))
            case (' ', achar(9), achar(10), achar(13))
                reader%position = reader%position + 1
            case default
                exit
            end select
        end do
    end subroutine json_skip_space

    subroutine json_expect(reader, expected, ierr, message)
        type(json_reader_t), intent(inout) :: reader
        character(len=1), intent(in) :: expected
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        call json_skip_space(reader)
        ierr = 1
        message = 'malformed Gremlin request JSON'
        if (reader%position > len(reader%text)) return
        if (reader%text(reader%position:reader%position) /= expected) return
        reader%position = reader%position + 1
        ierr = 0
        message = ''
    end subroutine json_expect

    subroutine json_read_string(reader, value, ierr, message)
        type(json_reader_t), intent(inout) :: reader
        character(len=*), intent(out) :: value
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        integer :: code, digit, i
        character(len=1) :: ch

        value = ''
        call json_expect(reader, '"', ierr, message)
        if (ierr /= 0) return
        ierr = 1
        message = 'unterminated or invalid JSON string'
        do while (reader%position <= len(reader%text))
            ch = reader%text(reader%position:reader%position)
            reader%position = reader%position + 1
            if (ch == '"') then
                ierr = 0
                message = ''
                return
            end if
            if (iachar(ch) < 32) return
            if (ch == achar(92)) then
                if (reader%position > len(reader%text)) return
                ch = reader%text(reader%position:reader%position)
                reader%position = reader%position + 1
                select case (ch)
                case ('"', achar(92), '/')
                case ('b')
                    ch = achar(8)
                case ('f')
                    ch = achar(12)
                case ('n')
                    ch = achar(10)
                case ('r')
                    ch = achar(13)
                case ('t')
                    ch = achar(9)
                case ('u')
                    code = 0
                    do i = 1, 4
                        if (reader%position > len(reader%text)) return
                        digit = hex_digit(reader%text(reader%position:reader%position))
                        if (digit < 0) return
                        code = code*16 + digit
                        reader%position = reader%position + 1
                    end do
                    if (code > 127) then
                        message = 'Gremlin request strings support ASCII values only'
                        return
                    end if
                    ch = achar(code)
                case default
                    return
                end select
            end if
            if (len_trim(value) >= len(value)) then
                message = 'Gremlin request string is too long'
                return
            end if
            value(len_trim(value) + 1:len_trim(value) + 1) = ch
        end do
    end subroutine json_read_string

    subroutine json_read_scalar(reader, value, is_string, ierr, message)
        type(json_reader_t), intent(inout) :: reader
        character(len=*), intent(out) :: value
        logical, intent(out) :: is_string
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        integer :: first, last
        character(len=1) :: ch

        value = ''
        is_string = .false.
        call json_skip_space(reader)
        if (reader%position > len(reader%text)) then
            ierr = 1
            message = 'missing Gremlin request value'
            return
        end if
        if (reader%text(reader%position:reader%position) == '"') then
            is_string = .true.
            call json_read_string(reader, value, ierr, message)
            return
        end if
        first = reader%position
        do while (reader%position <= len(reader%text))
            ch = reader%text(reader%position:reader%position)
            if (ch == ',' .or. ch == '}' .or. ch == ' ' .or. ch == achar(9) .or. &
                ch == achar(10) .or. ch == achar(13)) exit
            reader%position = reader%position + 1
        end do
        last = reader%position - 1
        if (last < first .or. last - first + 1 > len(value)) then
            ierr = 1
            message = 'invalid or oversized Gremlin request value'
            return
        end if
        value = reader%text(first:last)
        ierr = 0
        message = ''
    end subroutine json_read_scalar

    integer function request_field(action, key)
        character(len=*), intent(in) :: action, key

        request_field = 0
        if (key == 'lane_id') then
            request_field = 1
        else if (action == 'start' .or. action == 'run' .or. &
                action == 'gremlin_start') then
            select case (key)
            case ('random_count')
                request_field = 3
            case ('seed')
                request_field = 4
            case ('campaign_seconds')
                request_field = 5
            case ('timeout_seconds')
                request_field = 6
            case ('jobs')
                request_field = 7
            case ('only_changed')
                request_field = 8
            case ('shuffle')
                request_field = 9
            case ('targets')
                request_field = 10
            end select
        else if (key == 'session_id') then
            request_field = 2
        else
            select case (key)
            case ('cursor')
                if (action == 'status' .or. action == 'events' .or. &
                    action == 'wait' .or. action == 'failures') request_field = 11
            case ('max_records')
                if (action == 'status' .or. action == 'events' .or. &
                    action == 'wait' .or. action == 'failures') request_field = 12
            case ('max_bytes')
                if (action == 'status' .or. action == 'events' .or. &
                    action == 'wait' .or. action == 'failures') request_field = 13
            case ('wait_ms')
                if (action == 'wait') request_field = 14
            case ('fail_on_failure')
                if (action == 'wait') request_field = 15
            case ('case_id')
                if (action == 'reproduce') request_field = 16
            case ('generation_id')
                if (action == 'reproduce') request_field = 17
            end select
        end if
    end function request_field

    subroutine set_request_field(request, field, value, is_string, ierr, message)
        type(gremlin_request_t), intent(inout) :: request
        integer, intent(in) :: field
        character(len=*), intent(in) :: value
        logical, intent(in) :: is_string
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        integer :: ios, parsed
        integer(int64) :: wide
        ierr = 0
        message = ''
        select case (field)
        case (1, 2, 16, 17)
            if (.not. is_string) then
                ierr = 1
                message = 'string request field has the wrong JSON type'
                return
            end if
            if (has_control_character(value)) then
                ierr = 1
                message = 'string request field contains a control character'
                return
            end if
            select case (field)
            case (1)
                if (len_trim(value) > MAX_LANE_LEN) then
                    ierr = 1
                    message = 'lane_id is too long'
                    return
                end if
            case (2)
                if (len_trim(value) > len(request%session_id)) then
                    ierr = 1
                    message = 'session_id is too long'
                    return
                end if
            case (16)
                if (len_trim(value) == 0 .or. len_trim(value) > len(request%case_id)) then
                    ierr = 1
                    message = 'case_id must be nonempty and at most 128 characters'
                    return
                end if
            case (17)
                if (len_trim(value) /= HASH_LEN) then
                    ierr = 1
                    message = 'generation_id must contain exactly 64 characters'
                    return
                end if
                if (.not. is_hex_digest(value)) then
                    ierr = 1
                    message = 'generation_id must be a hexadecimal digest'
                    return
                end if
            end select
            select case (field)
            case (1)
                request%lane_id = value
            case (2)
                request%session_id = value
            case (16)
                request%case_id = value
            case (17)
                request%generation_id = value
            end select
        case (8, 9, 15)
            if (is_string) then
                ierr = 1
                message = 'boolean request field has the wrong JSON type'
                return
            end if
            if (trim(value) /= 'true' .and. trim(value) /= 'false') then
                ierr = 1
                message = 'boolean request field must be true or false'
                return
            end if
            select case (field)
            case (8)
                request%only_changed = trim(value) == 'true'
            case (9)
                request%shuffle = trim(value) == 'true'
            case (15)
                request%fail_on_failure = trim(value) == 'true'
            end select
        case default
            if (is_string) then
                ierr = 1
                message = 'integer request field has the wrong JSON type'
                return
            end if
            if (.not. is_json_integer(value)) then
                ierr = 1
                message = 'request field must be a JSON integer'
                return
            end if
            if (field == 11 .or. field == 13) then
                read (value, *, iostat=ios) wide
            else
                read (value, *, iostat=ios) parsed
            end if
            if (ios /= 0) then
                ierr = 1
                message = 'integer request field is outside its supported range'
                return
            end if
            if (field == 11) request%cursor = wide
            if (field == 13) request%max_bytes = wide
            if (field == 3) request%random_count = parsed
            if (field == 4) request%seed = parsed
            if (field == 5) request%campaign_seconds = parsed
            if (field == 6) request%timeout_seconds = parsed
            if (field == 7) request%jobs = parsed
            if (field == 12) request%max_records = parsed
            if (field == 14) request%wait_ms = parsed
        end select
        select case (field)
        case (3)
            if (request%random_count < 1 .or. request%random_count > DEFAULT_RANDOM) then
                ierr = 1
                message = 'random_count must be between 1 and 32'
            end if
        case (5)
            if (request%campaign_seconds < 1 .or. request%campaign_seconds > DEFAULT_CAMPAIGN) then
                ierr = 1
                message = 'campaign_seconds must be between 1 and 60'
            end if
        case (6)
            if (request%timeout_seconds < 1 .or. request%timeout_seconds > MAX_CASE_TIMEOUT) then
                ierr = 1
                message = 'timeout_seconds must be between 1 and 5'
            end if
        case (7)
            if (request%jobs /= 1) then
                ierr = 1
                message = 'this bootstrap Gremlin engine supports jobs=1 only'
            end if
        case (11)
            if (request%cursor < 0_int64) then
                ierr = 1
                message = 'cursor must be nonnegative'
            end if
        case (12)
            if (request%max_records < 1 .or. request%max_records > 128) then
                ierr = 1
                message = 'max_records must be between 1 and 128'
            end if
        case (13)
            if (request%max_bytes < 1_int64 .or. request%max_bytes > 262144_int64) then
                ierr = 1
                message = 'max_bytes must be between 1 and 262144'
            end if
        case (14)
            if (request%wait_ms < 0 .or. request%wait_ms > 30000) then
                ierr = 1
                message = 'wait_ms must be between 0 and 30000'
            end if
        end select
    end subroutine set_request_field

    subroutine parse_targets(reader, request, ierr, message)
        type(json_reader_t), intent(inout) :: reader
        type(gremlin_request_t), intent(inout) :: request
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=NAME_LEN) :: item
        ierr = 0
        message = ''
        call json_expect(reader, '[', ierr, message)
        if (ierr /= 0) return
        call json_skip_space(reader)
        if (reader%position <= len(reader%text)) then
            if (reader%text(reader%position:reader%position) == ']') then
                reader%position = reader%position + 1
                return
            end if
        end if
        do
            if (request%n_targets >= size(request%targets)) then
                ierr = 1
                message = 'too many target names'
                return
            end if
            call json_read_string(reader, item, ierr, message)
            if (ierr /= 0) return
            if (len_trim(item) == 0 .or. len_trim(item) > NAME_LEN) then
                ierr = 1
                message = 'target names must be nonempty and at most 128 characters'
                return
            end if
            if (has_control_character(item)) then
                ierr = 1
                message = 'target names cannot contain control characters'
                return
            end if
            if (any(request%targets(:request%n_targets) == item)) then
                ierr = 1
                message = 'duplicate target name: '//trim(item)
                return
            end if
            request%n_targets = request%n_targets + 1
            request%targets(request%n_targets) = item
            call json_skip_space(reader)
            if (reader%position > len(reader%text)) exit
            if (reader%text(reader%position:reader%position) == ']') then
                reader%position = reader%position + 1
                return
            end if
            call json_expect(reader, ',', ierr, message)
            if (ierr /= 0) return
        end do
        ierr = 1
        message = 'unterminated targets array'
    end subroutine parse_targets

    logical function has_control_character(text)
        character(len=*), intent(in) :: text
        integer :: i

        has_control_character = .false.
        do i = 1, len_trim(text)
            if (iachar(text(i:i)) < 32 .or. iachar(text(i:i)) == 127) then
                has_control_character = .true.
                return
            end if
        end do
    end function has_control_character

    logical function is_hex_digest(text)
        character(len=*), intent(in) :: text
        integer :: i

        is_hex_digest = .false.
        if (len_trim(text) /= HASH_LEN) return
        do i = 1, HASH_LEN
            if (hex_digit(text(i:i)) < 0) return
        end do
        is_hex_digest = .true.
    end function is_hex_digest

    logical function is_json_integer(value)
        character(len=*), intent(in) :: value
        integer :: i, first

        is_json_integer = .false.
        first = 1
        if (len_trim(value) == 0) return
        if (value(1:1) == '-') first = 2
        if (first > len_trim(value)) return
        if (value(first:first) == '0') then
            is_json_integer = first == len_trim(value)
            return
        end if
        if (value(first:first) < '1' .or. value(first:first) > '9') return
        do i = first + 1, len_trim(value)
            if (value(i:i) < '0' .or. value(i:i) > '9') return
        end do
        is_json_integer = .true.
    end function is_json_integer

    integer function hex_digit(character)
        character(len=1), intent(in) :: character
        integer :: code

        code = iachar(character)
        select case (code)
        case (iachar('0'):iachar('9'))
            hex_digit = code - iachar('0')
        case (iachar('a'):iachar('f'))
            hex_digit = code - iachar('a') + 10
        case (iachar('A'):iachar('F'))
            hex_digit = code - iachar('A') + 10
        case default
            hex_digit = -1
        end select
    end function hex_digit

    subroutine handle_start(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        character(len=PATH_LEN) :: message, status_text, owner_start
        character(len=128) :: session_id
        character(len=HASH_LEN) :: stored_policy
        integer :: ierr, owner_pid, pid, spawn_exit, i, n_args
        character(len=:), allocatable :: packed
        character(len=PATH_LEN) :: executable, log_file
        logical :: executable_ok

        exitcode = 0
        call gremlin_session_acquire(project_dir, trim(request%lane_id), &
            session, ierr, message)
        if (ierr /= 0) then
            call error_response('start', trim(message), response)
            exitcode = 2
            return
        end if
        if (session%attached) then
            do i = 1, 60
                call gremlin_session_read(project_dir, trim(request%lane_id), session_id, &
                    owner_pid, owner_start, status_text, ierr, message)
                if (ierr == 0 .and. len_trim(status_text) > 0) exit
                call fs_sleep_ms(50)
            end do
            if (ierr /= 0) then
                call error_response('start', trim(message), response)
                exitcode = 2
                return
            end if
            if (len_trim(status_text) == 0) then
                call error_response('start', 'active Gremlin owner has not published state', &
                    response)
                exitcode = 2
                return
            end if
            call extract_json_field(status_text, 'policy_key', stored_policy)
            if (trim(stored_policy) /= request_policy_key(request)) then
                call error_response('start', &
                    'an active Gremlin session has an incompatible policy', response)
                exitcode = 2
                return
            end if
            call simple_response('start', request%lane_id, trim(session_id), &
                'attached', response)
            return
        end if
        call gremlin_session_release(session, ierr, message)
        if (ierr /= 0) then
            call error_response('start', 'cannot reserve Gremlin owner: '//trim(message), &
                response)
            exitcode = 2
            return
        end if
        call find_self_executable(executable, executable_ok)
        if (.not. executable_ok) then
            call error_response('start', 'cannot locate the current fo executable', response)
            exitcode = 2
            return
        end if
        call make_tmpfile('fo-gremlin-launch', log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'gremlin')
        call argv_push(packed, n_args, 'run')
        call argv_push(packed, n_args, '--dir')
        call argv_push(packed, n_args, trim(project_dir))
        call argv_push(packed, n_args, '--lane-id')
        call argv_push(packed, n_args, trim(request%lane_id))
        call argv_push(packed, n_args, '--random-count')
        call argv_push(packed, n_args, int_text(request%random_count))
        call argv_push(packed, n_args, '--seed')
        call argv_push(packed, n_args, int_text(request%seed))
        call argv_push(packed, n_args, '--timeout-seconds')
        call argv_push(packed, n_args, int_text(request%timeout_seconds))
        call argv_push(packed, n_args, '--campaign-seconds')
        call argv_push(packed, n_args, int_text(request%campaign_seconds))
        call argv_push(packed, n_args, '--jobs')
        call argv_push(packed, n_args, int_text(request%jobs))
        if (request%only_changed) call argv_push(packed, n_args, '--only-changed')
        if (request%shuffle) call argv_push(packed, n_args, '--shuffle')
        do i = 1, request%n_targets
            call argv_push(packed, n_args, '--target')
            call argv_push(packed, n_args, trim(request%targets(i)))
        end do
        call process_start_argv_logged(project_dir, packed, n_args, log_file, &
            pid, spawn_exit)
        if (spawn_exit /= 0) then
            call error_response('start', 'cannot launch the Gremlin owner', response)
            exitcode = 2
            return
        end if
        ! The child obtains the lane lock. Wait only for its ready publication;
        ! campaign progress never depends on this caller staying connected.
        do i = 1, 60
            call fs_sleep_ms(50)
            call gremlin_session_read(project_dir, trim(request%lane_id), session_id, &
                owner_pid, owner_start, status_text, ierr, message)
            if (ierr /= 0 .or. len_trim(session_id) == 0 .or. &
                len_trim(status_text) == 0) cycle
            call extract_json_field(status_text, 'policy_key', stored_policy)
            if (trim(stored_policy) /= request_policy_key(request)) then
                call cancel_launched_owner(pid, spawn_exit)
                if (spawn_exit /= 0) then
                    call error_response('start', 'conflicting owner; failed to release '// &
                        'this launcher process', response)
                    exitcode = 2
                    return
                end if
                call error_response('start', &
                    'a concurrent Gremlin start selected an incompatible policy', response)
                exitcode = 2
                return
            end if
            exit
        end do
        if (i > 60 .or. len_trim(session_id) == 0 .or. len_trim(status_text) == 0) then
            call cancel_launched_owner(pid, spawn_exit)
            if (spawn_exit == 0) then
                call error_response('start', 'Gremlin owner did not publish ready state', &
                    response)
            else
                call error_response('start', 'Gremlin owner did not publish ready state; '// &
                    'launcher cancellation failed', response)
            end if
            exitcode = 2
            return
        end if
        call simple_response('start', request%lane_id, trim(session_id), 'running', response)
    end subroutine handle_start

    subroutine handle_status(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        character(len=65536) :: status_text
        character(len=PATH_LEN) :: message
        type(gremlin_session_t) :: session
        integer :: owner_pid, ierr
        logical :: is_live

        exitcode = 0
        call resolve_read_session(project_dir, request, session, status_text, &
            owner_pid, is_live, ierr, message)
        if (ierr /= 0) then
            call error_response('status', trim(message), response)
            exitcode = 2
            return
        end if
        if (is_live) call poll_launcher(owner_pid)
        call response_with_events('status', status_text, session, request, .false., &
            response, ierr, message)
        if (ierr /= 0) then
            call error_response('status', trim(message), response)
            exitcode = 2
            return
        end if
        exitcode = 0
    end subroutine handle_status

    subroutine handle_events(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        call handle_status(project_dir, request, response, exitcode)
        if (exitcode == 0) call replace_action(response, 'events')
    end subroutine handle_events

    subroutine handle_wait(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        character(len=65536) :: status_text
        character(len=PATH_LEN) :: message
        type(gremlin_request_t) :: poll_request
        type(gremlin_session_t) :: session
        integer :: owner_pid, ierr, i
        integer(int64) :: next_cursor
        logical :: failed, has_events, is_live

        failed = .false.
        poll_request = request
        do i = 1, max(1, (request%wait_ms + 99)/100)
            call resolve_read_session(project_dir, poll_request, session, status_text, &
                owner_pid, is_live, ierr, message)
            if (ierr /= 0) then
                call error_response('wait', trim(message), response)
                exitcode = 2
                return
            end if
            if (is_live) call poll_launcher(owner_pid)
            call event_page_has_failure(session, poll_request, failed, has_events, &
                next_cursor, ierr, message)
            if (ierr /= 0) then
                call error_response('wait', trim(message), response)
                exitcode = 2
                return
            end if
            if ((has_events .and. .not. request%fail_on_failure) .or. &
                (has_events .and. failed) .or. &
                index(status_text, '"state":"stopped"') > 0) exit
            if (.not. is_live) exit
            if (has_events .and. request%fail_on_failure .and. &
                next_cursor > poll_request%cursor) poll_request%cursor = next_cursor
            if (i < max(1, (request%wait_ms + 99)/100)) call fs_sleep_ms(100)
        end do
        call response_with_events('wait', status_text, session, poll_request, .false., &
            response, ierr, message)
        if (ierr /= 0) then
            call error_response('wait', trim(message), response)
            exitcode = 2
            return
        end if
        exitcode = 0
        if (request%fail_on_failure .and. failed) exitcode = 1
    end subroutine handle_wait

    subroutine handle_failures(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        character(len=PATH_LEN) :: message
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        integer :: owner_pid, ierr
        logical :: is_live

        exitcode = 0
        call resolve_read_session(project_dir, request, session, status_text, &
            owner_pid, is_live, ierr, message)
        if (ierr /= 0) then
            call error_response('failures', trim(message), response)
            exitcode = 2
            return
        end if
        if (is_live) call poll_launcher(owner_pid)
        call response_with_events('failures', status_text, session, request, .true., &
            response, ierr, message)
        if (ierr /= 0) then
            call error_response('failures', trim(message), response)
            exitcode = 2
        end if
    end subroutine handle_failures

    subroutine handle_reproduce(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        type(gremlin_lease_t) :: reproduction_lease
        type(generation_t) :: generation
        type(gremlin_request_t) :: selection_request
        character(len=PATH_LEN) :: message, active_project, log_file, executable
        character(len=PATH_LEN) :: cleanup_message
        character(len=PATH_LEN) :: generation_root
        character(len=NAME_LEN) :: selected(MAX_NODES)
        character(len=HASH_LEN) :: active_identity
        character(len=16) :: outcome
        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=128) :: session_id, owner_start
        character(len=:), allocatable :: packed
        integer :: owner_pid, ierr, n_selected, mandatory_count, seed
        integer :: n_args, spawn_exit, test_exit, sequence, release_error
        logical :: executable_ok
        logical :: have_reproduction_lease

        exitcode = 0
        have_reproduction_lease = .false.
        call gremlin_session_read(project_dir, trim(request%lane_id), session_id, &
            owner_pid, owner_start, status_text, ierr, message)
        if (ierr /= 0) then
            call error_response('reproduce', trim(message), response)
            exitcode = 2
            return
        end if
        if (len_trim(request%session_id) > 0 .and. &
            trim(request%session_id) /= trim(session_id)) then
            call error_response('reproduce', 'session_id does not match the lane owner', response)
            exitcode = 2
            return
        end if
        call gremlin_session_acquire(project_dir, trim(request%lane_id), session, &
            ierr, message)
        if (ierr /= 0) then
            call error_response('reproduce', trim(message), response)
            exitcode = 2
            return
        end if
        call extract_json_field(status_text, 'active_generation', active_identity)
        if (len_trim(request%generation_id) > 0) then
            active_identity = request%generation_id
        end if
        if (.not. is_hex_digest(active_identity)) then
            call release_if_owner(session, ierr, message)
            call error_response('reproduce', 'there is no compatible successful generation', &
                response)
            exitcode = 2
            return
        end if
        call gremlin_generation_root(trim(active_identity), generation_root, ierr, message)
        if (ierr /= 0) then
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', 'cannot resolve requested generation: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        active_project = trim(generation_root)//'/bundle/project'
        generation%identity = active_identity
        generation%root = generation_root
        generation%project_root = active_project
        call gremlin_generation_lease_acquire_at(generation%root, reproduction_lease, &
            ierr, message)
        if (ierr /= 0) then
            call error_response('reproduce', 'cannot lease requested generation: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        have_reproduction_lease = .true.
        inquire (file=trim(active_project)//'/fpm.toml', exist=executable_ok)
        if (.not. executable_ok) then
            call release_generation_lease(reproduction_lease, have_reproduction_lease, &
                release_error, cleanup_message)
            call release_if_owner(session, ierr, message)
            call error_response('reproduce', 'requested generation snapshot is unavailable', &
                response)
            exitcode = 2
            return
        end if
        selection_request = request
        selection_request%targets = ''
        selection_request%targets(1) = request%case_id
        selection_request%n_targets = 1
        selection_request%random_count = 0
        call discover_campaign(trim(active_project), selection_request, selected, &
            n_selected, mandatory_count, seed, ierr, message)
        if (ierr /= 0 .or. n_selected == 0) then
            call release_generation_lease(reproduction_lease, have_reproduction_lease, &
                release_error, cleanup_message)
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', 'case_id is not present in that generation', &
                response)
            exitcode = 2
            return
        end if
        if (trim(selected(1)) /= trim(request%case_id)) then
            call release_generation_lease(reproduction_lease, have_reproduction_lease, &
                release_error, cleanup_message)
            call release_if_owner(session, release_error, cleanup_message)
            call error_response('reproduce', 'case_id is not present in that generation', &
                response)
            exitcode = 2
            return
        end if
        call find_self_executable(executable, executable_ok)
        if (.not. executable_ok) then
            call release_generation_lease(reproduction_lease, have_reproduction_lease, &
                release_error, cleanup_message)
            call release_if_owner(session, ierr, message)
            call error_response('reproduce', 'cannot locate the current fo executable', &
                response)
            exitcode = 2
            return
        end if
        call log_path(session, 'reproduce.log', log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'test')
        call argv_push(packed, n_args, trim(request%case_id))
        call process_start_argv_logged(trim(active_project), packed, n_args, &
            trim(log_file), owner_pid, spawn_exit, 'FO_JOBS=1')
        sequence = int(system_clock_count())
        if (spawn_exit == 0) then
            call process_wait_bounded(owner_pid, request%timeout_seconds, test_exit)
            if (test_exit == 124) then
                outcome = 'TIMEOUT'
                test_exit = 124
            else if (test_exit == 125) then
                outcome = 'INFRA_ERROR'
                call gremlin_generation_pin_at(generation%root, .true., &
                    release_error, cleanup_message)
            else if (test_exit == 0) then
                outcome = 'PASS'
            else
                outcome = 'FAIL'
            end if
        else
            test_exit = spawn_exit
            outcome = 'INFRA_ERROR'
        end if
        call record_immediate_case(session, request, generation, request%case_id, &
            test_exit, trim(outcome), sequence, 1, seed, trim(log_file), ierr, message)
        call release_generation_lease(reproduction_lease, have_reproduction_lease, &
            release_error, cleanup_message)
        if (release_error /= 0) then
            call error_response('reproduce', 'cannot release generation lease: '// &
                trim(cleanup_message), response)
            exitcode = 2
            return
        end if
        call release_if_owner(session, release_error, cleanup_message)
        if (release_error /= 0) then
            call error_response('reproduce', 'cannot release Gremlin ownership: '// &
                trim(cleanup_message), response)
            exitcode = 2
            return
        end if
        if (ierr /= JOURNAL_OK) then
            call error_response('reproduce', trim(message), response)
            exitcode = 2
            return
        end if
        if (test_exit /= 0) exitcode = 1
        call simple_response('reproduce', request%lane_id, session_id, trim(outcome), response)
    end subroutine handle_reproduce

    subroutine run_owner(project_dir, request, response, exitcode)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        type(gremlin_lease_t) :: active_lease, candidate_lease
        type(generation_t) :: active_generation, candidate_generation
        type(child_t) :: build_child, test_child
        character(len=PATH_LEN) :: message, state_name, last_failed_identity
        character(len=PATH_LEN) :: fatal_message, state_message
        character(len=PATH_LEN) :: owner_start
        character(len=NAME_LEN) :: selected(MAX_NODES)
        character(len=HASH_LEN) :: stored_policy
        character(len=65536) :: status_text
        character(len=128) :: stored_session_id
        integer :: ierr, build_exit, test_exit, launch_error, completed, selected_count
        integer :: cancel_error, release_error, capture_error, state_error
        integer :: registration_error
        integer :: owner_pid, i
        integer :: sequence, campaign_seed, campaign_number, test_index
        integer(int64) :: next_capture_ms, campaign_started_ms
        logical :: have_active, have_candidate, stop_requested, capture_ok
        logical :: capture_failed
        logical :: have_active_lease, have_candidate_lease
        logical :: active_pinned, candidate_pinned
        logical :: build_done, test_done, fatal_error

        exitcode = 0
        active_generation = generation_t()
        candidate_generation = generation_t()
        build_child = child_t()
        test_child = child_t()
        completed = 0
        selected_count = 0
        sequence = 0
        campaign_number = 0
        test_index = 0
        campaign_started_ms = 0_int64
        have_active = .false.
        have_candidate = .false.
        have_active_lease = .false.
        have_candidate_lease = .false.
        active_pinned = .false.
        candidate_pinned = .false.
        last_failed_identity = ''
        state_name = 'starting'
        campaign_seed = request%seed
        if (campaign_seed == 0) then
            call system_clock(campaign_seed)
        end if
        fatal_error = .false.
        fatal_message = ''
        call gremlin_session_acquire(project_dir, trim(request%lane_id), &
            session, ierr, message)
        if (ierr /= 0) then
            call error_response('run', trim(message), response)
            exitcode = 2
            return
        end if
        if (session%attached) then
            do i = 1, 60
                call gremlin_session_read(project_dir, trim(request%lane_id), &
                    stored_session_id, owner_pid, owner_start, status_text, ierr, message)
                if (ierr == 0 .and. len_trim(status_text) > 0) exit
                call fs_sleep_ms(50)
            end do
            if (ierr /= 0) then
                call error_response('run', trim(message), response)
                exitcode = 2
                return
            end if
            if (len_trim(status_text) == 0) then
                call error_response('run', 'active Gremlin owner has not published state', &
                    response)
                exitcode = 2
                return
            end if
            call extract_json_field(status_text, 'policy_key', stored_policy)
            if (trim(stored_policy) /= request_policy_key(request)) then
                call error_response('run', &
                    'an active Gremlin session has an incompatible policy', response)
                exitcode = 2
                return
            end if
            call simple_response('run', request%lane_id, trim(stored_session_id), &
                'attached', response)
            return
        end if
        call fs_make_dir(session%state_dir//'/logs')
        call publish_state(session, request, 'starting', active_generation, &
            candidate_generation, '', completed, selected_count, campaign_seed, 'NONE', 0, &
            ierr, message)
        if (ierr /= 0) then
            fatal_message = trim(message)
            call gremlin_session_release(session, release_error, state_message)
            if (release_error /= 0) fatal_message = trim(state_message)
            call error_response('run', 'cannot publish initial Gremlin state: '// &
                trim(fatal_message), response)
            exitcode = 2
            return
        end if
        call set_capture_deadline(next_capture_ms)
        call capture_candidate(project_dir, candidate_generation, &
            capture_ok, registration_error, message)
        if (registration_error /= 0) then
            fatal_error = .true.
            fatal_message = 'cannot register captured generation: '//trim(message)
        end if
        if (capture_ok .and. registration_error == 0) then
            have_candidate = .true.
            call gremlin_generation_lease_acquire_at(candidate_generation%root, &
                candidate_lease, ierr, message)
            if (ierr /= 0) then
                fatal_error = .true.
                fatal_message = 'cannot lease captured generation: '//trim(message)
            else
                have_candidate_lease = .true.
            end if
            if (.not. fatal_error) then
                call start_build(session, candidate_generation, build_child, ierr, message)
                if (ierr /= 0) then
                    launch_error = ierr
                    fatal_message = 'cannot start initial candidate build'
                    call publish_state(session, request, 'build_failed', active_generation, &
                        candidate_generation, '', completed, selected_count, campaign_seed, &
                        'INFRA_ERROR', launch_error, state_error, state_message)
                    if (state_error == 0) then
                        sequence = sequence + 1
                        call record_immediate_case(session, request, candidate_generation, &
                            '<build>', launch_error, 'INFRA_ERROR', sequence, 0, &
                            campaign_seed, build_child%log_file, ierr, message)
                    else
                        ierr = state_error
                        message = state_message
                    end if
                    if (ierr /= 0) fatal_message = trim(message)
                    fatal_error = .true.
                    last_failed_identity = candidate_generation%identity
                    have_candidate = .false.
                    if (have_candidate_lease) then
                        call gremlin_lease_release(candidate_lease, release_error, &
                            state_message)
                        if (release_error == 0) have_candidate_lease = .false.
                        if (release_error /= 0) fatal_message = &
                            'cannot release failed candidate lease: '//trim(state_message)
                    end if
                else
                    state_name = 'building'
                end if
            end if
        else if (.not. fatal_error) then
            call publish_state(session, request, 'capture_failed', active_generation, &
                candidate_generation, '', completed, selected_count, 0, 'NONE', 0, &
                ierr, message)
            if (ierr /= 0) fatal_error = .true.
        end if

        do
            if (fatal_error) exit
            call gremlin_session_stop_requested(session, stop_requested, ierr, message)
            if (ierr /= 0) then
                fatal_error = .true.
                exit
            end if
            if (stop_requested) exit

            if (build_child%pid > 0) then
                call process_poll_pid(build_child%pid, build_done, build_exit)
                if (build_done) then
                    call complete_build(session, request, candidate_generation, &
                        build_child, build_exit, active_generation, have_active, &
                        test_child, active_lease, candidate_lease, &
                        have_active_lease, have_candidate_lease, active_pinned, &
                        candidate_pinned, selected, selected_count, campaign_seed, &
                        completed, state_name, last_failed_identity, sequence, ierr, message)
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                    if (build_exit == 0 .and. have_active .and. selected_count > 0) then
                        campaign_number = campaign_number + 1
                        test_index = 1
                        call clock_milliseconds(campaign_started_ms)
                        call launch_selected_case(session, request, active_generation, &
                            selected, selected_count, test_index, campaign_seed, &
                            campaign_number, test_child, ierr, message, completed, sequence)
                        if (ierr /= 0) then
                            fatal_error = .true.
                            exit
                        end if
                    end if
                    have_candidate = .false.
                end if
            end if
            if (fatal_error) exit

            if (test_child%pid > 0) then
                if (elapsed_seconds(test_child%started_at) >= real(request%timeout_seconds)) then
                    call cancel_owned_process(test_child%pid, test_exit)
                    if (test_exit /= 0) then
                        message = 'cannot cancel timed-out Gremlin test process'
                        fatal_error = .true.
                        exit
                    end if
                    call record_case(session, request, active_generation, &
                        test_child, 124, 'TIMEOUT', sequence, campaign_seed, ierr, message)
                    test_child%pid = 0
                    completed = completed + 1
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                    call advance_campaign(session, request, active_generation, selected, &
                        selected_count, test_index, campaign_seed, campaign_number, &
                        campaign_started_ms, test_child, ierr, message, completed, &
                        state_name, sequence)
                    if (ierr /= 0) then
                        fatal_error = .true.
                        exit
                    end if
                else
                    call process_poll_pid(test_child%pid, test_done, test_exit)
                    if (test_done) then
                        if (test_exit == 0) then
                            state_name = 'testing'
                            call record_case(session, request, active_generation, &
                                test_child, test_exit, 'PASS', sequence, campaign_seed, &
                                ierr, message)
                        else
                            call record_case(session, request, active_generation, &
                                test_child, test_exit, 'FAIL', sequence, campaign_seed, &
                                ierr, message)
                        end if
                        test_child%pid = 0
                        completed = completed + 1
                        if (ierr /= 0) then
                            fatal_error = .true.
                            exit
                        end if
                        call advance_campaign(session, request, active_generation, selected, &
                            selected_count, test_index, campaign_seed, campaign_number, &
                            campaign_started_ms, test_child, ierr, message, completed, &
                            state_name, sequence)
                        if (ierr /= 0) then
                            fatal_error = .true.
                            exit
                        end if
                    end if
                end if
            end if
            if (fatal_error) exit

            call maybe_capture_latest(project_dir, session, request, active_generation, &
                candidate_generation, have_active, have_candidate, last_failed_identity, &
                build_child, candidate_lease, have_candidate_lease, next_capture_ms, &
                sequence, capture_failed, ierr, message)
            if (ierr /= 0) then
                fatal_message = trim(message)
                fatal_error = .true.
                exit
            end if
            if (capture_failed) then
                state_name = 'capture_failed'
                capture_error = 1
                call publish_state(session, request, state_name, active_generation, &
                    candidate_generation, current_test_name(selected, selected_count), &
                    completed, selected_count, campaign_seed, 'NONE', capture_error, &
                    state_error, state_message)
                if (state_error /= 0) then
                    fatal_message = 'cannot publish capture failure: '//trim(state_message)
                    fatal_error = .true.
                    exit
                end if
                ierr = 0
            end if
            call fs_sleep_ms(100)
        end do

        if (fatal_error) then
            if (len_trim(fatal_message) == 0) fatal_message = trim(message)
            if (len_trim(fatal_message) == 0) &
                fatal_message = 'Gremlin stopped after an evidence or process error'
            cancel_error = 0
            if (test_child%pid > 0) then
                call cancel_owned_process(test_child%pid, test_exit)
                if (test_exit /= 0) cancel_error = test_exit
                if (test_exit == 0) test_child%pid = 0
            end if
            if (build_child%pid > 0) then
                call cancel_owned_process(build_child%pid, build_exit)
                if (build_exit /= 0 .and. cancel_error == 0) cancel_error = build_exit
                if (build_exit == 0) build_child%pid = 0
            end if
            if (cancel_error == 0) then
                if (candidate_pinned) then
                    call gremlin_generation_pin_at(candidate_generation%root, .false., &
                        state_error, state_message)
                    if (state_error == 0) candidate_pinned = .false.
                    if (state_error /= 0) fatal_message = &
                        'cannot release failed candidate pin: '//trim(state_message)
                end if
                call release_generation_lease(candidate_lease, have_candidate_lease, &
                    state_error, state_message)
                if (state_error /= 0) fatal_message = trim(state_message)
                call release_generation_lease(active_lease, have_active_lease, &
                    state_error, state_message)
                if (state_error /= 0) fatal_message = trim(state_message)
            else if (build_child%pid > 0 .and. len_trim(candidate_generation%root) > 0) then
                call gremlin_generation_pin_at(candidate_generation%root, .true., &
                    state_error, state_message)
            end if
            call publish_state(session, request, 'error', active_generation, &
                candidate_generation, current_test_name(selected, selected_count), &
                completed, selected_count, campaign_seed, 'INFRA_ERROR', 1, &
                state_error, state_message)
            if (state_error /= 0) fatal_message = trim(state_message)
            if (cancel_error /= 0) fatal_message = &
                'process cancellation failed: '//trim(int_text(cancel_error))
            if (cancel_error == 0 .and. state_error == 0) then
                call publish_terminal_snapshot(project_dir, session, release_error, &
                    state_message)
                if (release_error == 0) then
                    call gremlin_session_release(session, release_error, state_message)
                end if
                if (release_error /= 0) fatal_message = &
                    'cannot preserve terminal Gremlin ownership record: '//trim(state_message)
            end if
            call error_response('run', trim(fatal_message), response)
            exitcode = 2
            return
        end if
        stop_requested = test_child%pid > 0 .or. build_child%pid > 0
        cancel_error = 0
        if (test_child%pid > 0) then
            call cancel_owned_process(test_child%pid, test_exit)
            if (test_exit /= 0) cancel_error = test_exit
            if (test_exit == 0) test_child%pid = 0
        end if
        if (build_child%pid > 0) then
            call cancel_owned_process(build_child%pid, build_exit)
            if (build_exit /= 0 .and. cancel_error == 0) cancel_error = build_exit
            if (build_exit == 0) build_child%pid = 0
        end if
        if (cancel_error /= 0) then
            call publish_state(session, request, 'error', active_generation, &
                candidate_generation, current_test_name(selected, selected_count), &
                completed, selected_count, campaign_seed, 'INFRA_ERROR', cancel_error, &
                state_error, state_message)
            if (build_child%pid > 0 .and. len_trim(candidate_generation%root) > 0) then
                call gremlin_generation_pin_at(candidate_generation%root, .true., &
                    release_error, message)
            end if
            call error_response('run', 'cannot cancel owned Gremlin work: '// &
                trim(int_text(cancel_error))//'; '//trim(state_message), response)
            exitcode = 2
            return
        end if
        call release_generation_lease(candidate_lease, have_candidate_lease, &
            state_error, state_message)
        if (state_error == 0) call release_generation_lease(active_lease, &
            have_active_lease, state_error, state_message)
        if (state_error /= 0) then
            call publish_state(session, request, 'error', active_generation, &
                candidate_generation, '', completed, selected_count, campaign_seed, &
                'INFRA_ERROR', state_error, release_error, message)
            if (release_error == 0) then
                call publish_terminal_snapshot(project_dir, session, release_error, message)
                if (release_error == 0) then
                    call gremlin_session_release(session, release_error, message)
                end if
            end if
            call error_response('run', 'cannot release generation lease: '// &
                trim(state_message), response)
            exitcode = 2
            return
        end if
        state_name = 'NONE'
        if (stop_requested) state_name = 'CANCELLED'
        call publish_state(session, request, 'stopped', active_generation, &
            candidate_generation, '', completed, selected_count, campaign_seed, &
            state_name, 0, ierr, message)
        if (ierr /= 0) then
            call publish_terminal_snapshot(project_dir, session, release_error, message)
            if (release_error == 0) then
                call gremlin_session_release(session, release_error, message)
            end if
            call error_response('run', 'cannot publish stopped Gremlin state: '//trim(message), &
                response)
            exitcode = 2
            return
        end if
        call publish_terminal_snapshot(project_dir, session, ierr, message)
        if (ierr /= 0) then
            call error_response('run', 'cannot publish terminal Gremlin snapshot: '// &
                trim(message), response)
            exitcode = 2
            return
        end if
        call gremlin_session_release(session, ierr, message)
        if (ierr /= 0) then
            call error_response('run', 'cannot release Gremlin ownership: '//trim(message), &
                response)
            exitcode = 2
            return
        end if
        call simple_response('run', request%lane_id, session%session_id, 'stopped', response)
    end subroutine run_owner

    subroutine capture_candidate(project_dir, generation, ok, &
            registration_error, message)
        character(len=*), intent(in) :: project_dir
        type(generation_t), intent(out) :: generation
        logical, intent(out) :: ok
        integer, intent(out) :: registration_error
        character(len=*), intent(out) :: message

        type(generation_context_t) :: context
        character(len=PATH_LEN) :: cas_root
        integer :: ierr

        registration_error = 0
        call capture_context(project_dir, context, ierr, message)
        if (ierr /= 0) then
            ok = .false.
            return
        end if
        call common_generation_cas_root(cas_root, ierr, message)
        if (ierr /= 0) then
            ok = .false.
            return
        end if
        call generation_capture(project_dir, trim(cas_root), context, generation, &
            ierr, message)
        ok = ierr == 0
        if (.not. ok) return
        call gremlin_generation_register_at(generation%root, ierr, message)
        ok = ierr == 0
        if (ierr /= 0) registration_error = ierr
    end subroutine capture_candidate

    subroutine common_generation_cas_root(root, ierr, message)
        character(len=*), intent(out) :: root
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: base
        integer :: length, status

        root = ''
        base = ''
        call get_environment_variable('FO_GREMLIN_STATE_DIR', base, length, status)
        if (status == -1 .or. length >= len(base)) then
            ierr = 1
            message = 'FO_GREMLIN_STATE_DIR exceeds the supported path length'
            return
        end if
        if (length <= 0) then
            base = ''
            call get_environment_variable('XDG_CACHE_HOME', base, length, status)
            if (status == -1 .or. length >= len(base)) then
                ierr = 1
                message = 'XDG_CACHE_HOME exceeds the supported path length'
                return
            end if
        end if
        if (length <= 0) then
            base = ''
            call get_environment_variable('HOME', base, length, status)
            if (status /= 0 .or. length <= 0 .or. length >= len(base)) then
                ierr = 1
                message = 'no shared Gremlin state base directory is available'
                return
            end if
            base = trim(base(:length))//'/.cache'
            length = len_trim(base)
        end if
        if (length + len('/fo') > len(root)) then
            ierr = 1
            message = 'shared Gremlin generation CAS path exceeds the supported length'
            return
        end if
        root = trim(base(:length))//'/fo'
        ierr = 0
        message = ''
    end subroutine common_generation_cas_root

    subroutine capture_context(project_dir, context, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(generation_context_t), intent(out) :: context
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(fpm_config_t), allocatable :: config
        type(generation_input_t), allocatable :: inputs(:)
        character(len=PATH_LEN) :: compiler_path, command_path
        character(len=:), allocatable :: packed
        character(len=PATH_LEN) :: log_file, output_line
        character(len=128) :: base_commit
        character(len=131072) :: git_status
        character(len=256) :: fingerprint
        integer(c_long_long) :: tree_sum, tree_mixed, tree_count
        integer :: i, n_inputs, n_args, exitcode, git_exit
        logical :: found, git_found, fingerprint_ok

        ierr = 0
        message = ''
        context%toolchain = ''
        context%flags = ''
        context%environment = ''
        context%base_commit = ''
        context%patch_digest = ''
        allocate (config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            message = 'cannot parse fpm.toml for generation inputs'
            return
        end if
        n_inputs = 0
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) n_inputs = n_inputs + 1
        end do
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) == DEP_PATH) n_inputs = n_inputs + 1
        end do
        allocate (inputs(n_inputs))
        n_inputs = 0
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) /= DEP_PATH) cycle
            call append_path_dependency(project_dir, config%deps(i)%path, &
                config%deps(i)%name, inputs, n_inputs, ierr, message)
            if (ierr /= 0) return
        end do
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) /= DEP_PATH) cycle
            call append_path_dependency(project_dir, config%dev_deps(i)%path, &
                config%dev_deps(i)%name, inputs, n_inputs, ierr, message)
            if (ierr /= 0) return
        end do
        context%inputs = inputs
        do i = 1, config%n_flags
            context%flags = context%flags//' '//trim(config%flags(i))
        end do
        call fs_find_executable('gfortran', compiler_path, found)
        if (.not. found) then
            context%toolchain = 'gfortran-unresolved'
        else
            call make_tmpfile('fo-gremlin-compiler', log_file)
            n_args = 0
            call argv_push(packed, n_args, trim(compiler_path))
            call argv_push(packed, n_args, '--version')
            call process_start_argv_logged(project_dir, packed, n_args, log_file, i, exitcode)
            if (exitcode == 0) then
                call process_wait_bounded(i, 10, exitcode)
                if (exitcode == 0) then
                    call read_first_line(log_file, output_line)
                    context%toolchain = trim(compiler_path)//':'//trim(output_line)
                else
                    context%toolchain = trim(compiler_path)//':version-unavailable'
                end if
            else
                context%toolchain = trim(compiler_path)//':version-unavailable'
            end if
        end if
        call append_environment_value(context%environment, 'FC')
        call append_environment_value(context%environment, 'FFLAGS')
        call append_environment_value(context%environment, 'FPM_FC')
        call append_environment_value(context%environment, 'FPM_FFLAGS')
        call append_environment_value(context%environment, 'OMP_NUM_THREADS')
        call make_tmpfile('fo-gremlin-git-head', log_file)
        call fs_find_executable('git', command_path, found)
        git_found = found
        if (found) then
            n_args = 0
            call argv_push(packed, n_args, trim(command_path))
            call argv_push(packed, n_args, 'rev-parse')
            call argv_push(packed, n_args, 'HEAD')
            call process_start_argv_logged(project_dir, packed, n_args, log_file, &
                i, git_exit)
            if (git_exit == 0) call process_wait_bounded(i, 10, git_exit)
            if (git_exit == 0) then
                call read_first_line(log_file, output_line)
                base_commit = trim(output_line)
            else
                base_commit = 'unavailable'
            end if
        else
            base_commit = 'unavailable'
        end if
        context%base_commit = trim(base_commit)
        call fs_tree_fingerprint(project_dir, .true., tree_sum, tree_mixed, &
            tree_count, fingerprint_ok)
        if (fingerprint_ok) then
            write (fingerprint, '(i0,":",i0,":",i0)') tree_sum, tree_mixed, tree_count
        else
            fingerprint = 'fingerprint-unavailable'
        end if
        call make_tmpfile('fo-gremlin-git-diff', log_file)
        if (git_found) then
            n_args = 0
            call argv_push(packed, n_args, trim(command_path))
            call argv_push(packed, n_args, 'diff')
            call argv_push(packed, n_args, '--binary')
            call argv_push(packed, n_args, 'HEAD')
            call process_start_argv_logged(project_dir, packed, n_args, log_file, &
                i, git_exit)
            if (git_exit == 0) call process_wait_bounded(i, 20, git_exit)
            if (git_exit == 0) then
                call read_text_file(log_file, git_status)
                context%patch_digest = text_digest(trim(git_status)//'|'//trim(fingerprint))
            else
                context%patch_digest = text_digest('git-diff-unavailable|'//trim(fingerprint))
            end if
        else
            context%patch_digest = text_digest('git-unavailable|'//trim(fingerprint))
        end if
    end subroutine capture_context

    subroutine append_environment_value(environment, name)
        character(len=*), intent(inout) :: environment
        character(len=*), intent(in) :: name
        character(len=512) :: value
        integer :: length, status

        value = ''
        call get_environment_variable(name, value, length, status)
        if (status /= 0 .or. length <= 0) return
        if (len_trim(environment) > 0) environment = trim(environment)//';'
        environment = trim(environment)//trim(name)//'='//value(:min(length, len(value)))
    end subroutine append_environment_value

    subroutine append_path_dependency(project_dir, dep_path, dep_name, inputs, &
            n_inputs, ierr, message)
        character(len=*), intent(in) :: project_dir, dep_path, dep_name
        type(generation_input_t), intent(inout) :: inputs(:)
        integer, intent(inout) :: n_inputs
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        integer :: i

        ierr = 0
        message = ''
        if (len_trim(dep_path) == 0) return
        if (dep_path(1:1) == '/') then
            ierr = 1
            message = 'absolute fpm path dependencies cannot be frozen safely'
            return
        end if
        do i = 1, n_inputs
            if (trim(inputs(i)%destination) == trim(dep_path)) return
        end do
        if (n_inputs >= size(inputs)) then
            ierr = 1
            message = 'too many path dependencies to freeze'
            return
        end if
        n_inputs = n_inputs + 1
        inputs(n_inputs)%label = 'dependency:'//trim(dep_name)
        inputs(n_inputs)%source_root = trim(project_dir)//'/'//trim(dep_path)
        inputs(n_inputs)%destination = trim(dep_path)
    end subroutine append_path_dependency

    subroutine maybe_capture_latest(project_dir, session, request, active, candidate, &
            have_active, have_candidate, last_failed, build_child, candidate_lease, &
            have_candidate_lease, next_capture_ms, sequence, capture_failed, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: active
        type(generation_t), intent(inout) :: candidate
        logical, intent(in) :: have_active
        logical, intent(inout) :: have_candidate
        character(len=*), intent(inout) :: last_failed
        type(child_t), intent(inout) :: build_child
        type(gremlin_lease_t), intent(inout) :: candidate_lease
        logical, intent(inout) :: have_candidate_lease
        integer(int64), intent(inout) :: next_capture_ms
        integer, intent(inout) :: sequence
        logical, intent(out) :: capture_failed
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(generation_t) :: current
        logical :: ok
        integer(int64) :: now_ms
        integer :: spawn_error, journal_error, release_error

        ierr = 0
        message = ''
        capture_failed = .false.
        call clock_milliseconds(now_ms)
        if (now_ms < next_capture_ms) return
        next_capture_ms = now_ms + 1000_int64
        call capture_candidate(project_dir, current, ok, &
            release_error, message)
        if (release_error /= 0) then
            ierr = release_error
            message = 'cannot register captured generation: '//trim(message)
            return
        end if
        if (.not. ok) then
            capture_failed = .true.
            return
        end if
        if (have_candidate) then
            if (current%identity == candidate%identity) return
            if (build_child%pid > 0) then
                call cancel_owned_process(build_child%pid, ierr)
                if (ierr /= 0) then
                    message = 'cannot cancel superseded candidate build process'
                    return
                end if
                build_child%pid = 0
            end if
            if (have_candidate_lease) then
                call gremlin_lease_release(candidate_lease, release_error, message)
                if (release_error /= 0) then
                    ierr = release_error
                    message = 'cannot release superseded candidate generation lease'
                    return
                end if
                have_candidate_lease = .false.
            end if
            have_candidate = .false.
        end if
        if (have_active) then
            if (current%identity == active%identity) return
        end if
        if (current%identity == last_failed) return
        candidate = current
        have_candidate = .true.
        call gremlin_generation_lease_acquire_at(candidate%root, candidate_lease, &
            ierr, message)
        if (ierr /= 0) then
            have_candidate = .false.
            message = 'cannot lease candidate generation: '//trim(message)
            return
        end if
        have_candidate_lease = .true.
        call start_build(session, candidate, build_child, spawn_error, message)
        if (spawn_error /= 0) then
            have_candidate = .false.
            last_failed = candidate%identity
            sequence = sequence + 1
            call record_immediate_case(session, request, candidate, '<build>', &
                spawn_error, 'INFRA_ERROR', sequence, 0, request%seed, &
                build_child%log_file, journal_error, message)
            call gremlin_lease_release(candidate_lease, release_error, message)
            if (release_error == 0) have_candidate_lease = .false.
            if (journal_error /= JOURNAL_OK) then
                ierr = journal_error
                return
            end if
            if (release_error /= 0) then
                ierr = release_error
                message = 'cannot release failed candidate generation lease'
                return
            end if
            ierr = spawn_error
            message = 'cannot start candidate build'
        end if
    end subroutine maybe_capture_latest

    subroutine start_build(session, generation, child, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(generation_t), intent(in) :: generation
        type(child_t), intent(out) :: child
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: executable
        character(len=:), allocatable :: packed
        integer :: n_args, spawn_exit
        logical :: executable_ok

        child = child_t()
        call find_self_executable(executable, executable_ok)
        if (.not. executable_ok) then
            ierr = 1
            message = 'cannot locate the current fo executable'
            return
        end if
        call log_path(session, 'build-'//generation%identity(1:16), child%log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'build')
        call process_start_argv_logged(trim(generation%project_root), packed, &
            n_args, trim(child%log_file), child%pid, spawn_exit, &
            'FO_JOBS=1;FO_DISABLE_SELF_REFRESH=1')
        ierr = spawn_exit
        if (ierr /= 0) then
            message = 'cannot start candidate build'
            child%pid = 0
            return
        end if
        call clock_seconds(child%started_at)
        message = ''
    end subroutine start_build

    subroutine complete_build(session, request, candidate, build_child, build_exit, &
            active, have_active, test_child, active_lease, candidate_lease, &
            have_active_lease, have_candidate_lease, active_pinned, candidate_pinned, &
            selected, selected_count, seed, completed, state_name, last_failed, &
            sequence, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(inout) :: candidate, active
        type(child_t), intent(inout) :: build_child, test_child
        type(gremlin_lease_t), intent(inout) :: active_lease, candidate_lease
        integer, intent(in) :: build_exit
        logical, intent(inout) :: have_active
        logical, intent(inout) :: have_active_lease, have_candidate_lease
        logical, intent(inout) :: active_pinned, candidate_pinned
        character(len=*), intent(out) :: selected(:)
        integer, intent(out) :: selected_count, seed
        integer, intent(in) :: completed
        character(len=*), intent(out) :: state_name, last_failed
        integer, intent(inout) :: sequence
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(gremlin_lease_t) :: new_active_lease
        integer :: inventory_status, cancel_exit, mandatory_count, state_status
        integer :: release_status, pin_status
        character(len=PATH_LEN) :: state_message, release_message

        ierr = 0
        message = ''
        call record_build(session, request, candidate, build_exit, build_child%log_file, &
            sequence, ierr, message)
        if (ierr /= 0) return
        build_child%pid = 0
        if (build_exit /= 0) then
            last_failed = candidate%identity
            state_name = 'build_failed'
            call publish_state(session, request, state_name, active, candidate, '', &
                completed, selected_count, seed, 'BUILD_FAIL', build_exit, ierr, message)
            if (ierr /= 0) return
            if (have_candidate_lease) then
                call gremlin_lease_release(candidate_lease, release_status, release_message)
                if (release_status /= 0) then
                    ierr = release_status
                    message = 'cannot release failed candidate generation lease: '// &
                        trim(release_message)
                    return
                end if
                have_candidate_lease = .false.
            end if
            return
        end if
        call gremlin_generation_lease_acquire_at(candidate%root, new_active_lease, &
            ierr, message)
        if (ierr /= 0) then
            message = 'cannot lease successfully built generation: '//trim(message)
            return
        end if
        call gremlin_generation_pin_at(candidate%root, .true., pin_status, message)
        if (pin_status /= 0) then
            ierr = pin_status
            message = 'cannot preserve successfully built generation: '//trim(message)
            call gremlin_lease_release(new_active_lease, release_status, release_message)
            return
        end if
        candidate_pinned = .true.
        if (test_child%pid > 0) then
            call cancel_owned_process(test_child%pid, cancel_exit)
            if (cancel_exit /= 0) then
                ierr = cancel_exit
                message = 'cannot cancel obsolete generation test process'
                call gremlin_generation_pin_at(candidate%root, .false., &
                    pin_status, release_message)
                if (pin_status == 0) candidate_pinned = .false.
                call gremlin_lease_release(new_active_lease, release_status, release_message)
                return
            end if
            test_child%pid = 0
        end if

        if (active_pinned) then
            call gremlin_generation_pin_at(active%root, .false., pin_status, message)
            if (pin_status /= 0) then
                ierr = pin_status
                message = 'cannot release previous generation pin: '//trim(message)
                call gremlin_generation_pin_at(candidate%root, .false., &
                    state_status, release_message)
                if (state_status == 0) candidate_pinned = .false.
                call gremlin_lease_release(new_active_lease, release_status, release_message)
                return
            end if
            active_pinned = .false.
        end if
        if (have_active_lease) then
            call gremlin_lease_release(active_lease, release_status, release_message)
            if (release_status /= 0) then
                call gremlin_generation_pin_at(active%root, .true., state_status, &
                    release_message)
                if (state_status == 0) active_pinned = .true.
                call gremlin_generation_pin_at(candidate%root, .false., &
                    state_status, release_message)
                if (state_status == 0) candidate_pinned = .false.
                call gremlin_lease_release(new_active_lease, state_status, release_message)
                ierr = release_status
                message = 'cannot release previous generation lease: '//trim(release_message)
                return
            end if
            have_active_lease = .false.
        end if

        active_lease%resource = new_active_lease%resource
        active_lease%slot = new_active_lease%slot
        active_lease%lock_fd = new_active_lease%lock_fd
        new_active_lease%lock_fd = -1
        new_active_lease%slot = -1
        have_active_lease = .true.
        active = candidate
        have_active = .true.
        active_pinned = .true.
        candidate_pinned = .false.
        if (have_candidate_lease) then
            call gremlin_lease_release(candidate_lease, release_status, release_message)
            if (release_status /= 0) then
                ierr = release_status
                message = 'cannot release candidate generation lease: '//trim(release_message)
                return
            end if
            have_candidate_lease = .false.
        end if
        last_failed = ''
        call discover_campaign(active%project_root, request, selected, selected_count, &
            mandatory_count, seed, inventory_status, message)
        if (inventory_status /= 0) then
            state_name = 'inventory_failed'
            selected_count = 0
            call publish_state(session, request, state_name, active, candidate, '', &
                completed, selected_count, seed, 'INFRA_ERROR', inventory_status, &
                ierr, message)
            if (ierr == 0) then
                ierr = inventory_status
                message = 'cannot discover the frozen test inventory'
            end if
            return
        end if
        state_name = 'testing'
        if (selected_count == 0) state_name = 'idle'
        call publish_state(session, request, state_name, active, candidate, &
            current_test_name(selected, selected_count), completed, selected_count, &
            seed, 'NONE', 0, ierr, message)
    end subroutine complete_build

    subroutine discover_campaign(project_dir, request, selected, n_selected, &
            n_mandatory_selected, seed, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(gremlin_request_t), intent(in) :: request
        character(len=*), intent(out) :: selected(:)
        integer, intent(out) :: n_selected, n_mandatory_selected, seed, ierr
        character(len=*), intent(out) :: message

        type(backend_t) :: backend
        type(dag_t) :: dag
        integer :: changed_ids(MAX_NODES), affected_ids(MAX_NODES)
        integer :: n_changed, n_affected, n_cached, i, n_all, n_impacted
        integer :: candidate_ids(MAX_NODES), shuffle_status
        character(len=MAX_PATH) :: filenames(MAX_NODES)
        character(len=NAME_LEN) :: all_names(MAX_NODES), impacted(MAX_NODES)
        character(len=NAME_LEN) :: history(0), debt(0)
        logical :: is_test_arr(MAX_NODES)

        selected = ''
        impacted = ''
        n_selected = 0
        n_mandatory_selected = 0
        seed = request%seed
        if (seed == 0) then
            call system_clock(i)
            seed = i
        end if
        backend = detect_backend(project_dir)
        ierr = 0
        message = ''
        if (backend%kind /= BACKEND_NATIVE) then
            ierr = 1
            message = 'Gremlin currently requires the native fpm test backend'
            return
        end if
        call fo_changed_modules(project_dir, dag, changed_ids, n_changed, &
            affected_ids, n_affected, n_cached, ierr, filenames=filenames, &
            is_test_arr=is_test_arr)
        if (ierr /= 0) then
            message = 'cannot scan frozen project test inventory'
            return
        end if
        do i = 1, dag%n_nodes
            candidate_ids(i) = i
        end do
        call gfortran_selected_test_names(project_dir, filenames, candidate_ids, &
            dag%n_nodes, .false., all_names, n_all)
        if (request%only_changed) then
            n_impacted = 0
            call gfortran_selected_test_names(project_dir, filenames, affected_ids, &
                n_affected, .false., impacted, n_impacted)
        else
            n_impacted = 0
        end if
        call select_gremlin_tests(all_names, n_all, request%targets, &
            request%n_targets, impacted, n_impacted, history, 0, debt, 0, &
            request%random_count, seed, selected, n_selected, ierr, &
            n_mandatory_selected=n_mandatory_selected)
        if (ierr /= GREMLIN_POLICY_OK) then
            message = 'Gremlin selection rejected test inventory or requested targets'
            return
        end if
        if (request%shuffle) then
            call shuffle_gremlin_tests(selected, n_mandatory_selected, n_selected, &
                seed, shuffle_status)
            if (shuffle_status /= GREMLIN_POLICY_OK) then
                ierr = shuffle_status
                message = 'Gremlin could not shuffle selected tests'
            end if
        end if
    end subroutine discover_campaign

    subroutine launch_selected_case(session, request, generation, selected, n_selected, &
            index_case, seed, campaign, child, ierr, message, completed, sequence)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: selected(:)
        integer, intent(in) :: n_selected, index_case, seed, campaign, completed
        type(child_t), intent(out) :: child
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer, intent(inout) :: sequence

        character(len=PATH_LEN) :: executable, log_name
        character(len=PATH_LEN) :: journal_message
        character(len=:), allocatable :: packed
        integer :: n_args, spawn_exit, journal_status
        logical :: executable_ok

        child = child_t()
        ierr = 0
        message = ''
        if (index_case < 1 .or. index_case > n_selected) return
        call find_self_executable(executable, executable_ok)
        if (.not. executable_ok) then
            ierr = 1
            message = 'cannot locate the current fo executable'
            return
        end if
        write (log_name, '(a,i0,a,i0,a)') 'case-', campaign, '-', index_case, '.log'
        call log_path(session, trim(log_name), child%log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'test')
        call argv_push(packed, n_args, trim(selected(index_case)))
        call process_start_argv_logged(trim(generation%project_root), packed, &
            n_args, trim(child%log_file), child%pid, spawn_exit, 'FO_JOBS=1')
        ierr = spawn_exit
        if (spawn_exit /= 0) then
            child%pid = 0
            journal_message = 'cannot start selected test '//trim(selected(index_case))
            sequence = sequence + 1
            call record_immediate_case(session, request, generation, &
                selected(index_case), spawn_exit, 'INFRA_ERROR', sequence, &
                index_case, seed, child%log_file, journal_status, message)
            if (journal_status /= JOURNAL_OK) then
                ierr = journal_status
            else
                ierr = spawn_exit
                message = trim(journal_message)
            end if
            return
        end if
        child%case_name = selected(index_case)
        child%campaign = campaign
        child%case_index = index_case
        child%seed = seed
        call clock_seconds(child%started_at)
        call publish_state(session, request, 'testing', generation, generation, &
            child%case_name, completed, n_selected, seed, 'RUNNING', 0, ierr, message)
    end subroutine launch_selected_case

    subroutine advance_campaign(session, request, generation, selected, n_selected, &
            index_case, seed, campaign, campaign_started_ms, child, ierr, message, &
            completed, state_name, sequence)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        character(len=*), intent(inout) :: selected(:)
        integer, intent(inout) :: n_selected, index_case, seed, campaign
        integer(int64), intent(inout) :: campaign_started_ms
        type(child_t), intent(inout) :: child
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message, state_name
        integer, intent(in) :: completed
        integer, intent(inout) :: sequence

        integer(int64) :: now_ms
        integer :: status, clock_count, mandatory_count, launch_error
        character(len=PATH_LEN) :: launch_message, state_message

        ierr = 0
        message = ''
        call clock_milliseconds(now_ms)
        if (index_case < n_selected .and. &
            now_ms - campaign_started_ms < &
            int(request%campaign_seconds, int64)*1000_int64) then
            index_case = index_case + 1
        else
            call discover_campaign(generation%project_root, request, selected, &
                n_selected, mandatory_count, seed, ierr, message)
            if (ierr /= 0) then
                state_name = 'inventory_failed'
                call publish_state(session, request, state_name, generation, generation, '', &
                    completed, n_selected, seed, 'NONE', ierr)
                return
            end if
            campaign = campaign + 1
            index_case = 1
            call system_clock(clock_count)
            seed = ieor(seed, clock_count)
            call clock_milliseconds(campaign_started_ms)
        end if
        if (n_selected == 0) then
            state_name = 'idle'
            call publish_state(session, request, state_name, generation, generation, '', &
                completed, n_selected, seed, 'NONE', 0, ierr, message)
            return
        end if
        state_name = 'testing'
        call launch_selected_case(session, request, generation, selected, n_selected, &
            index_case, seed, campaign, child, ierr, message, completed, sequence)
        if (ierr /= 0) then
            launch_error = ierr
            launch_message = message
            call publish_state(session, request, 'test_launch_failed', generation, &
                generation, current_test_name(selected, n_selected), completed, &
                n_selected, seed, 'INFRA_ERROR', launch_error, status, state_message)
            if (status == 0) then
                ierr = launch_error
                message = trim(launch_message)
            else
                ierr = status
                message = trim(state_message)
            end if
        end if
    end subroutine advance_campaign

    subroutine record_case(session, request, generation, child, exitcode, outcome, &
            sequence, seed, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        type(child_t), intent(in) :: child
        integer, intent(in) :: exitcode, seed
        character(len=*), intent(in) :: outcome
        integer, intent(inout) :: sequence
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        sequence = sequence + 1
        call record_immediate_case(session, request, generation, child%case_name, &
            exitcode, outcome, sequence, child%case_index, seed, child%log_file, ierr, &
            message)
    end subroutine record_case

    subroutine record_immediate_case(session, request, generation, case_name, exitcode, &
            outcome, sequence, case_index, seed, log_file, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: case_name, outcome, log_file
        integer, intent(in) :: exitcode, sequence, case_index, seed
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=256) :: completion_id
        character(len=8192) :: record
        character(len=16) :: journal_outcome
        character(len=PATH_LEN) :: journal_path
        integer :: status

        write (completion_id, '(a,a,a,a,i0,a,i0)') trim(session%session_id), '-', &
            generation%identity(1:12), '-', sequence, '-', case_index
        journal_outcome = 'fail'
        if (outcome == 'PASS' .or. outcome == 'BUILD_PASS') journal_outcome = 'pass'
        write (record, '(a)') '{"completion_id":"'//trim(completion_id)// &
            '","session_id":"'//trim(json_escape_string(session%session_id))// &
            '","lane_id":"'//trim(json_escape_string(request%lane_id))// &
            '","generation":"'//generation%identity// &
            '","case_id":"'//trim(json_escape_string(case_name))// &
            '","outcome":"'//trim(journal_outcome)//'","status":"'// &
            trim(outcome)//'","exitcode":'// &
            trim(json_int(exitcode))//',"seed":'//trim(json_int(seed))// &
            ',"order":'//trim(json_int(case_index))//',"log_path":"'// &
            trim(json_escape_string(log_file))//'"}'
        call get_session_journal_path(session%project_key, request%lane_id, &
            session%session_id, journal_path, ierr, message)
        if (ierr /= 0) return
        call journal_append(trim(journal_path), trim(completion_id), trim(record), &
            status, message)
        ierr = status
    end subroutine record_immediate_case

    subroutine record_build(session, request, generation, exitcode, log_file, sequence, &
            ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        type(generation_t), intent(in) :: generation
        integer, intent(in) :: exitcode
        character(len=*), intent(in) :: log_file
        integer, intent(inout) :: sequence
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=16) :: outcome

        if (exitcode == 0) then
            outcome = 'BUILD_PASS'
        else
            outcome = 'BUILD_FAIL'
        end if
        sequence = sequence + 1
        call record_immediate_case(session, request, generation, '<build>', exitcode, &
            trim(outcome), sequence, 0, request%seed, log_file, ierr, message)
    end subroutine record_build

    subroutine publish_state(session, request, state, active, candidate, current_case, &
            completed, selected_count, seed, last_outcome, last_exitcode, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        character(len=*), intent(in) :: state, current_case, last_outcome
        type(generation_t), intent(in) :: active, candidate
        integer, intent(in) :: completed, selected_count, seed, last_exitcode
        integer, intent(out), optional :: ierr
        character(len=*), intent(out), optional :: message

        character(len=GREMLIN_STATE_TEXT_MAX) :: status_text
        character(len=PATH_LEN) :: local_message
        integer :: status
        character(len=PATH_LEN) :: active_project, candidate_project

        active_project = ''
        candidate_project = ''
        if (len_trim(active%identity) > 0) active_project = trim(active%project_root)
        if (len_trim(candidate%identity) > 0) candidate_project = trim(candidate%project_root)
        status_text = '{"protocol":1,"session_id":"'// &
            trim(json_escape_string(session%session_id))//'","lane_id":"'// &
            trim(json_escape_string(request%lane_id))//'","policy_key":"'// &
            request_policy_key(request)//'","state":"'//trim(state)// &
            '","active_generation":"'//trim(active%identity)// &
            '","candidate_generation":"'//trim(candidate%identity)// &
            '","active_project":"'//trim(json_escape_string(active_project))// &
            '","candidate_project":"'//trim(json_escape_string(candidate_project))// &
            '","current_test":"'//trim(json_escape_string(current_case))// &
            '","completed":'//trim(json_int(completed))// &
            ',"selected":'//trim(json_int(selected_count))// &
            ',"seed":'//trim(json_int(seed))//',"last_outcome":"'// &
            trim(last_outcome)//'","last_exitcode":'// &
            trim(json_int(last_exitcode))//'}'
        call gremlin_session_publish(session, trim(status_text), status, local_message)
        if (present(ierr)) ierr = status
        if (present(message)) message = local_message
    end subroutine publish_state

    subroutine response_with_events(action, status_text, session, request, failures_only, &
            response, ierr, message)
        character(len=*), intent(in) :: action, status_text
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        logical, intent(in) :: failures_only
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        integer(int64) :: next_cursor, file_size
        integer :: journal_status, i, emitted
        logical :: exists, has_more
        character(len=16) :: row_status
        character(len=:), allocatable :: base

        call journal_read_page(session%state_dir//'/journal.jsonl', request%cursor, &
            request%max_records, request%max_bytes, records, next_cursor, &
            journal_status, message)
        ierr = journal_status
        if (ierr /= JOURNAL_OK) return
        file_size = 0_int64
        inquire (file=session%state_dir//'/journal.jsonl', exist=exists, &
            size=file_size, iostat=i)
        if (i /= 0) then
            ierr = 2
            message = 'cannot inspect Gremlin journal after reading its page'
            return
        end if
        has_more = .false.
        if (exists) has_more = next_cursor < file_size
        base = trim(status_text)
        if (len(base) < 2) then
            base = '{"protocol":1,"session_id":"'// &
                trim(json_escape_string(session%session_id))//'","lane_id":"'// &
                trim(json_escape_string(request%lane_id))//'","state":"unknown",'// &
                '"active_generation":"","candidate_generation":"","completed":0,'// &
                '"selected":0,"seed":0,"last_outcome":"NONE","last_exitcode":0}'
        end if
        response = '{"action":"'//trim(action)//'",'//base(2:len(base) - 1)// &
            ',"events":['
        emitted = 0
        do i = 1, size(records)
            if (failures_only) then
                call extract_json_field(records(i)%json, 'status', row_status)
                if (.not. status_is_failure(trim(row_status))) cycle
            end if
            if (emitted > 0) response = response//','
            response = response//records(i)%json
            emitted = emitted + 1
        end do
        response = response//'],"next_cursor":'//trim(int64_text(next_cursor))// &
            ',"has_more":'//trim(json_bool(has_more))//'}'
        ierr = 0
        message = ''
    end subroutine response_with_events

    subroutine event_page_has_failure(session, request, failed, has_events, &
            next_cursor, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(gremlin_request_t), intent(in) :: request
        logical, intent(out) :: failed, has_events
        integer(int64), intent(out) :: next_cursor
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(journal_record_t), allocatable :: records(:)
        integer :: journal_status, i
        character(len=16) :: row_status

        failed = .false.
        has_events = .false.
        next_cursor = request%cursor
        call journal_read_page(session%state_dir//'/journal.jsonl', request%cursor, &
            request%max_records, request%max_bytes, records, next_cursor, &
            journal_status, message)
        ierr = journal_status
        if (ierr == JOURNAL_OK) then
            has_events = size(records) > 0
            do i = 1, size(records)
                call extract_json_field(records(i)%json, 'status', row_status)
                if (status_is_failure(trim(row_status))) failed = .true.
            end do
        end if
    end subroutine event_page_has_failure

    subroutine release_generation_lease(lease, held, ierr, message)
        type(gremlin_lease_t), intent(inout) :: lease
        logical, intent(inout) :: held
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        if (.not. held) then
            ierr = 0
            message = ''
            return
        end if
        call gremlin_lease_release(lease, ierr, message)
        if (ierr == 0) held = .false.
    end subroutine release_generation_lease

    logical function status_is_failure(status)
        character(len=*), intent(in) :: status

        status_is_failure = status == 'FAIL' .or. status == 'BUILD_FAIL' .or. &
            status == 'TIMEOUT' .or. status == 'INFRA_ERROR'
    end function status_is_failure

    function request_policy_key(request) result(key)
        type(gremlin_request_t), intent(in) :: request
        character(len=HASH_LEN) :: key

        character(len=32) :: number
        character(len=:), allocatable :: canonical
        integer :: i

        canonical = 'v1|r='//trim(int_text(request%random_count))// &
            '|s='//trim(int_text(request%seed))//'|c='// &
            trim(int_text(request%campaign_seconds))//'|t='// &
            trim(int_text(request%timeout_seconds))//'|j='// &
            trim(int_text(request%jobs))//'|changed='// &
            trim(json_bool(request%only_changed))//'|shuffle='// &
            trim(json_bool(request%shuffle))
        do i = 1, request%n_targets
            write (number, '(i0)') len_trim(request%targets(i))
            canonical = canonical//'|target:'//trim(number)//':'// &
                trim(request%targets(i))
        end do
        key = text_digest(canonical)
    end function request_policy_key

    function text_digest(text) result(digest)
        character(len=*), intent(in) :: text
        character(len=HASH_LEN) :: digest

        character(len=:), allocatable :: parts(:)

        allocate (character(len=max(1, len(text))) :: parts(1))
        parts(1) = text
        digest = cache_digest(parts, 1)
    end function text_digest

    subroutine simple_response(action, lane_id, session_id, state, response)
        character(len=*), intent(in) :: action, lane_id, session_id, state
        character(len=:), allocatable, intent(out) :: response

        response = '{"action":"'//trim(json_escape_string(action))// &
            '","lane_id":"'//trim(json_escape_string(lane_id))// &
            '","session_id":"'//trim(json_escape_string(session_id))// &
            '","state":"'//trim(json_escape_string(state))//'"}'
    end subroutine simple_response

    subroutine error_response(action, message, response)
        character(len=*), intent(in) :: action, message
        character(len=:), allocatable, intent(out) :: response

        response = '{"action":"'//trim(json_escape_string(action))// &
            '","error":"'//trim(json_escape_string(message))//'"}'
    end subroutine error_response

    subroutine replace_action(response, action)
        character(len=:), allocatable, intent(inout) :: response
        character(len=*), intent(in) :: action

        character(len=:), allocatable :: needle
        integer :: position

        needle = '"action":"status"'
        position = index(response, needle)
        if (position == 0) return
        response = response(:position - 1)//'"action":"'// &
            trim(json_escape_string(action))//'"'//response(position + len(needle):)
    end subroutine replace_action

    subroutine release_if_owner(session, ierr, message)
        type(gremlin_session_t), intent(inout) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        if (session%owner) then
            call gremlin_session_release(session, ierr, message)
        else
            ierr = 0
            message = ''
        end if
    end subroutine release_if_owner

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
    end function int_text

    function int64_text(value) result(text)
        integer(int64), intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
    end function int64_text

    function current_test_name(selected, n_selected) result(name)
        character(len=*), intent(in) :: selected(:)
        integer, intent(in) :: n_selected
        character(len=NAME_LEN) :: name

        name = ''
        if (n_selected <= 0) return
        name = selected(1)
    end function current_test_name

    subroutine find_self_executable(path, ok)
        character(len=*), intent(out) :: path
        logical, intent(out) :: ok

        character(len=PATH_LEN) :: argument
        integer :: status, length

        argument = ''
        call get_command_argument(0, argument, length, status)
        ok = .false.
        if (status == 0 .and. length > 0 .and. length <= len(argument)) then
            call fs_find_executable(trim(argument), path, ok)
            if (ok) return
        end if
        call fs_find_executable('fo', path, ok)
    end subroutine find_self_executable

    subroutine log_path(session, filename, path)
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: path

        path = trim(session%state_dir)//'/logs/'//trim(filename)
    end subroutine log_path

    subroutine poll_launcher(pid)
        integer, intent(in) :: pid

        integer :: ignored_exit
        logical :: done

        if (pid <= 0) return
        call process_poll_pid(pid, done, ignored_exit)
    end subroutine poll_launcher

    subroutine cancel_launched_owner(pid, ierr)
        integer, intent(in) :: pid
        integer, intent(out) :: ierr

        call cancel_owned_process(pid, ierr)
    end subroutine cancel_launched_owner

    subroutine cancel_owned_process(pid, ierr)
        integer, intent(in) :: pid
        integer, intent(out) :: ierr

        integer :: attempt, exitcode
        logical :: done

        ierr = 0
        if (pid <= 0) return
        do attempt = 1, 3
            call process_cancel_pid(pid, ierr)
            if (ierr == 0) return
            call process_poll_pid(pid, done, exitcode)
            if (done) then
                ierr = 0
                return
            end if
            call fs_sleep_ms(50)
        end do
    end subroutine cancel_owned_process

    subroutine read_first_line(path, line)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: line
        integer :: unit, ios

        line = ''
        open (newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) line
        close (unit)
        if (ios /= 0) line = ''
    end subroutine read_first_line

    subroutine set_capture_deadline(deadline)
        integer(int64), intent(out) :: deadline
        integer(int64) :: now

        call clock_milliseconds(now)
        deadline = now + 1000_int64
    end subroutine set_capture_deadline

    subroutine clock_milliseconds(milliseconds)
        integer(int64), intent(out) :: milliseconds
        integer(int64) :: count, rate

        call system_clock(count=count, count_rate=rate)
        milliseconds = 0_int64
        if (rate <= 0_int64) return
        milliseconds = (count/rate)*1000_int64 + &
            modulo(count, rate)*1000_int64/rate
    end subroutine clock_milliseconds

    subroutine clock_seconds(seconds)
        real, intent(out) :: seconds
        integer(int64) :: count, rate

        call system_clock(count=count, count_rate=rate)
        seconds = 0.0
        if (rate > 0_int64) seconds = real(count)/real(rate)
    end subroutine clock_seconds

    real function elapsed_seconds(started)
        real, intent(in) :: started
        real :: now

        call clock_seconds(now)
        elapsed_seconds = max(0.0, now - started)
    end function elapsed_seconds

    integer(int64) function system_clock_count()
        integer(int64) :: count
        call system_clock(count=count)
        system_clock_count = count
    end function system_clock_count

    subroutine process_wait_bounded(pid, timeout_seconds, exitcode)
        integer, intent(in) :: pid, timeout_seconds
        integer, intent(out) :: exitcode

        integer :: cancel_status
        logical :: done
        real :: started

        call clock_seconds(started)
        do
            call process_poll_pid(pid, done, exitcode)
            if (done) return
            if (elapsed_seconds(started) >= real(timeout_seconds)) then
                call cancel_owned_process(pid, cancel_status)
                if (cancel_status == 0) then
                    exitcode = 124
                else
                    exitcode = 125
                end if
                return
            end if
            call fs_sleep_ms(50)
        end do
    end subroutine process_wait_bounded

end module fo_gremlin_supervisor
