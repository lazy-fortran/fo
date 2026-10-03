module fo_gremlin_lifecycle
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_gremlin_request, only: gremlin_json_text_valid
    use fo_util, only: json_bool, json_int
    use fx_json_build, only: json_escape_string
    implicit none
    private

    integer, parameter, public :: LIFECYCLE_OK = 0
    integer, parameter, public :: LIFECYCLE_INVALID = 1
    integer, parameter, public :: LIFECYCLE_IO_ERROR = 2
    integer, parameter, public :: LIFECYCLE_CONFLICT = 3
    integer, parameter, public :: LIFECYCLE_TOO_LARGE = 4
    integer, parameter, public :: LIFECYCLE_MAX_EVENT = 262144

    type, public :: gremlin_lifecycle_event_t
        integer :: version = 1
        character(len=256) :: event_id = ''
        character(len=64) :: event_type = ''
        character(len=HASH_LEN) :: generation = ''
        character(len=HASH_LEN) :: active_generation = ''
        character(len=HASH_LEN) :: candidate_generation = ''
        character(len=HASH_LEN) :: previous_generation = ''
        character(len=16) :: phase = 'starting'
        character(len=16) :: health = 'unknown'
        character(len=16) :: verification_level = 'gate'
        logical :: dirty = .false.
        logical :: local_gate_green = .false.
        logical :: fully_verified = .false.
        integer :: gate_required = 0
        integer :: gate_passed = 0
        integer :: ordinary_required = 0
        integer :: ordinary_passed = 0
        integer :: ordinary_failures = 0
        integer :: full_required = 0
        integer :: full_passed = 0
        integer :: full_failures = 0
        integer :: event_epoch = 0
        character(len=HASH_LEN) :: requirement_digest = ''
        character(len=HASH_LEN) :: gate_token = ''
        integer :: epoch = 0
        integer :: epoch_cursor = 0
        character(len=:), allocatable :: json
    end type gremlin_lifecycle_event_t

    public :: gremlin_lifecycle_encode, gremlin_lifecycle_validate
    public :: gremlin_lifecycle_append, gremlin_lifecycle_read_page
    public :: gremlin_lifecycle_make_id

    interface
        function c_lifecycle_append(path, event_id, record) &
                bind(C, name='fo_c_gremlin_lifecycle_append') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*), event_id(*), record(*)
            integer(c_int) :: ierr
        end function c_lifecycle_append
    end interface

contains

    function gremlin_lifecycle_make_id(session_id, event) result(event_id)
        character(len=*), intent(in) :: session_id
        type(gremlin_lifecycle_event_t), intent(in) :: event
        character(len=256) :: event_id
        character(len=HASH_LEN) :: digest
        character(len=4096) :: parts(1)

        parts(1) = event_facts(event)
        digest = cache_digest(parts, 1)
        event_id = trim(session_id)//':event:'//digest
    end function gremlin_lifecycle_make_id

    subroutine gremlin_lifecycle_encode(event, json, ierr, message)
        type(gremlin_lifecycle_event_t), intent(in) :: event
        character(len=:), allocatable, intent(out) :: json
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        ierr = LIFECYCLE_INVALID
        message = 'invalid typed Gremlin lifecycle event'
        if (event%version /= 1 .or. len_trim(event%event_id) == 0) return
        if (.not. valid_event_type(event%event_type) .or. &
            .not. valid_phase(event%phase) .or. &
            .not. valid_health(event%health) .or. &
            .not. valid_verification(event%verification_level)) return
        if (event%gate_required < 0 .or. event%gate_passed < 0 .or. &
            event%ordinary_required < 0 .or. event%ordinary_passed < 0 .or. &
            event%ordinary_failures < 0 .or. event%full_required < 0 .or. &
            event%full_passed < 0 .or. event%full_failures < 0 .or. &
            event%epoch < 0 .or. event%epoch_cursor < 0) return
        json = '{"event_version":1,"event_id":"'// &
            trim(json_escape_string(event%event_id))//'","event_type":"'// &
            trim(event%event_type)//'","generation":"'//trim(event%generation)// &
            '","active_generation":"'//trim(event%active_generation)// &
            '","candidate_generation":"'//trim(event%candidate_generation)// &
            '","previous_generation":"'//trim(event%previous_generation)// &
            '","phase":"'//trim(event%phase)//'","health":"'// &
            trim(event%health)//'","dirty":'//trim(json_bool(event%dirty))// &
            ',"local_gate_green":'//trim(json_bool(event%local_gate_green))// &
            ',"verification_level":"'//trim(event%verification_level)// &
            '","fully_verified":'//trim(json_bool(event%fully_verified))// &
            ',"gate_required":'//trim(json_int(event%gate_required))// &
            ',"gate_passed":'//trim(json_int(event%gate_passed))// &
            ',"ordinary_required":'//trim(json_int(event%ordinary_required))// &
            ',"ordinary_passed":'//trim(json_int(event%ordinary_passed))// &
            ',"ordinary_failures":'//trim(json_int(event%ordinary_failures))// &
            ',"full_required":'//trim(json_int(event%full_required))// &
            ',"full_passed":'//trim(json_int(event%full_passed))// &
            ',"full_failures":'//trim(json_int(event%full_failures))// &
            ',"event_epoch":'//trim(json_int(event%event_epoch))// &
            ',"requirement_digest":"'//trim(event%requirement_digest)// &
            '","gate_token":"'//trim(event%gate_token)//'"'// &
            ',"epoch":'//trim(json_int(event%epoch))// &
            ',"epoch_cursor":'//trim(json_int(event%epoch_cursor))//'}'
        ierr = gremlin_lifecycle_validate(json)
        if (ierr == LIFECYCLE_OK) then
            message = ''
        else
            message = 'lifecycle event failed schema validation'
        end if
    end subroutine gremlin_lifecycle_encode

    integer function gremlin_lifecycle_validate(json) result(status)
        character(len=*), intent(in) :: json
        type(gremlin_lifecycle_event_t) :: event
        integer :: ios

        status = LIFECYCLE_INVALID
        if (len_trim(json) == 0 .or. len_trim(json) > LIFECYCLE_MAX_EVENT) return
        if (.not. gremlin_json_text_valid(trim(json))) return
        call read_event_fields(trim(json), event, ios)
        if (ios /= 0 .or. event%version /= 1) then
            return
        end if
        if (len_trim(event%event_id) == 0 .or. &
            .not. valid_event_type(event%event_type) .or. &
            .not. valid_phase(event%phase) .or. &
            .not. valid_health(event%health) .or. &
            .not. valid_verification(event%verification_level)) then
            return
        end if
        if (event%gate_required < 0 .or. event%gate_passed < 0 .or. &
            event%ordinary_required < 0 .or. event%ordinary_passed < 0 .or. &
            event%ordinary_failures < 0 .or. event%full_required < 0 .or. &
            event%full_passed < 0 .or. event%full_failures < 0 .or. &
            event%epoch < 0 .or. event%epoch_cursor < 0) then
            return
        end if
        status = LIFECYCLE_OK
    end function gremlin_lifecycle_validate

    subroutine gremlin_lifecycle_append(path, event, ierr, message)
        character(len=*), intent(in) :: path
        type(gremlin_lifecycle_event_t), intent(in) :: event
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=:), allocatable :: json
        integer(c_int) :: c_error

        call gremlin_lifecycle_encode(event, json, ierr, message)
        if (ierr /= LIFECYCLE_OK) return
        c_error = c_lifecycle_append(trim(path)//c_null_char, &
            trim(event%event_id)//c_null_char, trim(json)//c_null_char)
        ierr = int(c_error)
        select case (ierr)
        case (LIFECYCLE_OK)
            message = ''
        case (LIFECYCLE_CONFLICT)
            message = 'conflicting lifecycle event ID already exists'
        case (LIFECYCLE_TOO_LARGE)
            message = 'lifecycle event exceeds the maximum record size'
        case default
            ierr = LIFECYCLE_IO_ERROR
            message = 'cannot durably append lifecycle event'
        end select
    end subroutine gremlin_lifecycle_append

    subroutine gremlin_lifecycle_read_page(path, cursor, max_records, max_bytes, &
            events, n_events, next_cursor, has_more, ierr, message)
        character(len=*), intent(in) :: path
        integer(int64), intent(in) :: cursor, max_bytes
        integer, intent(in) :: max_records
        type(gremlin_lifecycle_event_t), allocatable, intent(out) :: events(:)
        integer, intent(out) :: n_events, ierr
        integer(int64), intent(out) :: next_cursor
        logical, intent(out) :: has_more
        character(len=*), intent(out) :: message
        character(len=LIFECYCLE_MAX_EVENT) :: line
        character(len=1) :: byte
        integer(int64) :: file_size, position, page_bytes
        integer :: unit, ios, line_length, parse_status
        logical :: exists, stopped_for_limit

        allocate(events(max(0, max_records)))
        n_events = 0
        next_cursor = cursor
        has_more = .false.
        ierr = LIFECYCLE_OK
        message = ''
        stopped_for_limit = .false.
        if (cursor < 0_int64 .or. max_records < 1 .or. max_records > 128 .or. &
            max_bytes < 1_int64 .or. max_bytes > int(LIFECYCLE_MAX_EVENT, int64)) then
            ierr = LIFECYCLE_INVALID
            message = 'invalid lifecycle page request'
            return
        end if
        inquire(file=trim(path), exist=exists, iostat=ios)
        if (ios /= 0) then
            ierr = LIFECYCLE_IO_ERROR
            message = 'cannot inspect lifecycle journal'
            return
        end if
        if (.not. exists) then
            if (cursor /= 0_int64) then
                ierr = LIFECYCLE_INVALID
                message = 'lifecycle cursor is beyond the empty journal'
            end if
            return
        end if
        inquire(file=trim(path), size=file_size, iostat=ios)
        if (ios /= 0 .or. file_size < 0_int64 .or. cursor > file_size) then
            ierr = LIFECYCLE_INVALID
            message = 'lifecycle cursor is beyond end of journal'
            return
        end if
        open(newunit=unit, file=trim(path), access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) then
            ierr = LIFECYCLE_IO_ERROR
            message = 'cannot open lifecycle journal'
            return
        end if
        if (cursor > 0_int64) then
            read(unit, pos=cursor, iostat=ios) byte
            if (ios /= 0 .or. byte /= achar(10)) then
                close(unit)
                ierr = LIFECYCLE_INVALID
                message = 'lifecycle cursor is not at a complete event boundary'
                return
            end if
        end if

        line = ''
        line_length = 0
        position = cursor + 1_int64
        page_bytes = 0_int64
        do while (position <= file_size)
            read(unit, pos=position, iostat=ios) byte
            if (ios /= 0) exit
            if (byte == achar(10)) then
                if (line_length == 0) then
                    ierr = LIFECYCLE_INVALID
                    message = 'lifecycle journal contains an empty event'
                    exit
                end if
                parse_status = gremlin_lifecycle_validate(line(:line_length))
                if (parse_status /= LIFECYCLE_OK) then
                    ierr = parse_status
                    message = 'lifecycle journal contains an invalid event envelope'
                    exit
                end if
                if (n_events >= max_records .or. &
                    page_bytes + int(line_length + 1, int64) > max_bytes) then
                    if (n_events == 0 .and. &
                        page_bytes + int(line_length + 1, int64) > max_bytes) then
                        ierr = LIFECYCLE_TOO_LARGE
                        message = 'first lifecycle event exceeds the requested page size'
                        exit
                    end if
                    stopped_for_limit = .true.
                    has_more = .true.
                    exit
                end if
                n_events = n_events + 1
                events(n_events)%json = line(:line_length)
                call read_event_fields(line(:line_length), events(n_events), parse_status)
                if (parse_status /= 0) then
                    ierr = LIFECYCLE_INVALID
                    message = 'lifecycle event fields could not be decoded'
                    exit
                end if
                page_bytes = page_bytes + int(line_length + 1, int64)
                next_cursor = position
                line = ''
                line_length = 0
            else
                if (line_length >= LIFECYCLE_MAX_EVENT) then
                    ierr = LIFECYCLE_TOO_LARGE
                    message = 'lifecycle event exceeds the maximum record size'
                    exit
                end if
                line_length = line_length + 1
                line(line_length:line_length) = byte
            end if
            position = position + 1_int64
        end do
        close(unit)
        if (ierr == LIFECYCLE_OK .and. .not. stopped_for_limit) then
            ! A final unterminated line is an interrupted append, not an event.
            has_more = .false.
        end if
        if (ierr /= LIFECYCLE_OK) n_events = 0
    end subroutine gremlin_lifecycle_read_page

    subroutine extract_event_field(json, name, value)
        character(len=*), intent(in) :: json, name
        character(len=*), intent(out) :: value
        character(len=:), allocatable :: needle
        character(len=1) :: current, escaped
        integer :: search_from, found_at, value_at, cursor, used

        value = ''
        needle = '"'//trim(name)//'"'
        search_from = 1
        found_at = 0
        value_at = 0
        do while (search_from <= len_trim(json))
            found_at = index(json(search_from:), needle)
            if (found_at == 0) return
            found_at = search_from + found_at - 1
            value_at = found_at + len(needle)
            do while (value_at <= len_trim(json))
                if (json(value_at:value_at) /= ' ') exit
                value_at = value_at + 1
            end do
            if (value_at <= len_trim(json)) then
                if (json(value_at:value_at) == ':') exit
            end if
            search_from = found_at + len(needle)
        end do
        if (value_at > len_trim(json)) return
        if (search_from > len_trim(json)) return
        value_at = value_at + 1
        do while (value_at <= len_trim(json))
            if (json(value_at:value_at) /= ' ') exit
            value_at = value_at + 1
        end do
        if (value_at > len_trim(json)) return

        if (json(value_at:value_at) /= '"') then
            cursor = value_at
            do while (cursor <= len_trim(json))
                current = json(cursor:cursor)
                if (current == ',' .or. current == '}' .or. current == ' ') exit
                cursor = cursor + 1
            end do
            used = cursor - value_at
            if (used > 0 .and. used <= len(value)) &
                value(:used) = json(value_at:cursor - 1)
            return
        end if

        cursor = value_at + 1
        used = 0
        do while (cursor <= len_trim(json))
            current = json(cursor:cursor)
            if (current == '"') return
            if (current == achar(92)) then
                cursor = cursor + 1
                if (cursor > len_trim(json)) return
                escaped = json(cursor:cursor)
                select case (escaped)
                case ('"', achar(92), '/')
                    current = escaped
                case ('b')
                    current = achar(8)
                case ('f')
                    current = achar(12)
                case ('n')
                    current = achar(10)
                case ('r')
                    current = achar(13)
                case ('t')
                    current = achar(9)
                case default
                    value = ''
                    return
                end select
            end if
            if (used >= len(value)) then
                value = ''
                return
            end if
            used = used + 1
            value(used:used) = current
            cursor = cursor + 1
        end do
        value = ''
    end subroutine extract_event_field

    subroutine read_event_fields(json, event, ierr)
        character(len=*), intent(in) :: json
        type(gremlin_lifecycle_event_t), intent(inout) :: event
        integer, intent(out) :: ierr
        character(len=4096) :: value
        integer :: ios

        ierr = 1
        call extract_event_field(json, 'event_version', value)
        read(value, *, iostat=ios) event%version
        if (ios /= 0) return
        call extract_event_field(json, 'event_id', event%event_id)
        call extract_event_field(json, 'event_type', event%event_type)
        call extract_event_field(json, 'generation', event%generation)
        call extract_event_field(json, 'active_generation', event%active_generation)
        call extract_event_field(json, 'candidate_generation', event%candidate_generation)
        call extract_event_field(json, 'previous_generation', event%previous_generation)
        call extract_event_field(json, 'phase', event%phase)
        call extract_event_field(json, 'health', event%health)
        call extract_event_field(json, 'verification_level', event%verification_level)
        call extract_event_field(json, 'dirty', value)
        if (trim(value) /= 'true' .and. trim(value) /= 'false') return
        event%dirty = trim(value) == 'true'
        call extract_event_field(json, 'local_gate_green', value)
        if (trim(value) /= 'true' .and. trim(value) /= 'false') return
        event%local_gate_green = trim(value) == 'true'
        call extract_event_field(json, 'fully_verified', value)
        if (trim(value) /= 'true' .and. trim(value) /= 'false') return
        event%fully_verified = trim(value) == 'true'
        call read_integer_field(json, 'gate_required', event%gate_required, ios)
        if (ios /= 0) return
        call read_integer_field(json, 'gate_passed', event%gate_passed, ios)
        if (ios /= 0) return
        call read_integer_field(json, 'ordinary_required', event%ordinary_required, ios)
        if (ios /= 0) return
        call read_integer_field(json, 'ordinary_passed', event%ordinary_passed, ios)
        if (ios /= 0) return
        call read_integer_field(json, 'ordinary_failures', event%ordinary_failures, ios)
        if (ios /= 0) return
        call read_integer_field(json, 'full_required', event%full_required, ios)
        if (ios /= 0) return
        call read_integer_field(json, 'full_passed', event%full_passed, ios)
        if (ios /= 0) return
        call read_integer_field(json, 'full_failures', event%full_failures, ios)
        if (ios /= 0) return
        call read_integer_field(json, 'epoch', event%epoch, ios)
        if (ios /= 0) return
        call read_integer_field(json, 'epoch_cursor', event%epoch_cursor, ios)
        if (ios /= 0) return
        call extract_event_field(json, 'requirement_digest', event%requirement_digest)
        call extract_event_field(json, 'gate_token', event%gate_token)
        call read_integer_field(json, 'event_epoch', event%event_epoch, ios)
        if (ios /= 0) event%event_epoch = 0
        event%json = trim(json)
        ierr = 0
    end subroutine read_event_fields

    subroutine read_integer_field(json, name, value, ierr)
        character(len=*), intent(in) :: json, name
        integer, intent(out) :: value
        integer, intent(out) :: ierr
        character(len=64) :: raw

        call extract_event_field(json, name, raw)
        read(raw, *, iostat=ierr) value
    end subroutine read_integer_field

    function event_facts(event) result(facts)
        type(gremlin_lifecycle_event_t), intent(in) :: event
        character(len=4096) :: facts

        facts = trim(event%event_type)//'|'//trim(event%generation)//'|'// &
            trim(event%active_generation)//'|'//trim(event%candidate_generation)//'|'// &
            trim(event%previous_generation)//'|'//trim(event%phase)//'|'// &
            trim(event%health)//'|'//bool_text(event%dirty)//'|'// &
            bool_text(event%local_gate_green)//'|'//trim(event%verification_level)//'|'// &
            bool_text(event%fully_verified)//'|'//int_text(event%gate_required)//'|'// &
            int_text(event%gate_passed)//'|'//int_text(event%ordinary_required)//'|'// &
            int_text(event%ordinary_passed)//'|'//int_text(event%ordinary_failures)//'|'// &
            int_text(event%full_required)//'|'//int_text(event%full_passed)//'|'// &
            int_text(event%full_failures)//'|'//int_text(event%epoch)//'|'// &
            int_text(event%epoch_cursor)//'|'//int_text(event%event_epoch)//'|'// &
            event%requirement_digest//'|'//event%gate_token
    end function event_facts

    logical function valid_event_type(value)
        character(len=*), intent(in) :: value
        select case (trim(value))
        case ('session_started', 'candidate_build_started', 'candidate_build_succeeded', &
                'candidate_build_failed', 'capture_failed', 'generation_activated', &
                'generation_superseded', 'local_gate_green', 'local_gate_changed', &
                'ordinary_verified', 'fully_verified', 'regression_observed', &
                'coverage_updated', 'session_quiescent', 'session_wake', &
                'session_stopped', 'session_failed', 'state_changed', &
                'generation_started', 'build_passed', 'build_failed', &
                'regression_confirmed', 'ordinary_coverage_complete', &
                'full_verification_complete')
            valid_event_type = .true.
        case default
            valid_event_type = .false.
        end select
    end function valid_event_type

    logical function valid_phase(value)
        character(len=*), intent(in) :: value
        valid_phase = value == 'starting' .or. value == 'building' .or. &
            value == 'testing' .or. value == 'quiescent' .or. &
            value == 'stopped' .or. value == 'failed' .or. &
            value == 'capture_failed' .or. value == 'build_failed' .or. &
            value == 'idle' .or. value == 'unknown'
    end function valid_phase

    logical function valid_health(value)
        character(len=*), intent(in) :: value
        valid_health = value == 'unknown' .or. value == 'healthy' .or. &
            value == 'degraded' .or. value == 'failure'
    end function valid_health

    logical function valid_verification(value)
        character(len=*), intent(in) :: value
        valid_verification = value == 'none' .or. value == 'gate' .or. &
            value == 'ordinary' .or. value == 'full'
    end function valid_verification

    function bool_text(value) result(text)
        logical, intent(in) :: value
        character(len=5) :: text
        text = 'false'
        if (value) text = 'true '
    end function bool_text

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=16) :: text
        write(text, '(i0)') value
    end function int_text

end module fo_gremlin_lifecycle
