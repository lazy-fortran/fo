module fo_gremlin_request
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_cache, only: HASH_LEN
    use fx_dag, only: MAX_NODES
    implicit none
    private

    integer, parameter :: NAME_LEN = 128, PATH_LEN = 4096
    integer, parameter :: DEFAULT_RANDOM = 32, DEFAULT_CAMPAIGN = 60
    integer, parameter :: DEFAULT_CASE_TIMEOUT = 5, MAX_CASE_TIMEOUT = 5
    integer, parameter :: MAX_LANE_LEN = 96

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

    public :: gremlin_request_t, parse_request, is_hex_digest

contains

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

end module fo_gremlin_request
