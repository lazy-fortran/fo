module fo_gremlin_request
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_cache, only: HASH_LEN
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init, &
        json_parser_next, JSON_OBJECT_START, JSON_OBJECT_END, JSON_ARRAY_START, &
        JSON_ARRAY_END, JSON_KEY, JSON_STRING, JSON_INTEGER, JSON_BOOL, &
        JSON_ERROR, JSON_END_OF_INPUT
    use fx_dag, only: MAX_NODES
    implicit none
    private

    integer, parameter :: NAME_LEN = 128, PATH_LEN = 4096
    integer, parameter :: MAX_JSON_DEPTH = 64
    integer, parameter :: DEFAULT_RANDOM = 32, DEFAULT_CAMPAIGN = 60
    integer, parameter :: DEFAULT_CASE_TIMEOUT = 0, MAX_CASE_TIMEOUT = 86400
    integer, parameter :: MAX_LANE_LEN = 96

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
        integer(int64) :: lifecycle_cursor = 0_int64
        integer :: wait_ms = 5000
        integer :: max_records = 32
        integer(int64) :: max_bytes = 262144_int64
        logical :: only_changed = .false.
        logical :: shuffle = .false.
        logical :: fail_on_failure = .false.
        character(len=HASH_LEN) :: generation_id = ''
        character(len=32) :: wait_until = ''
        logical :: input_changed = .false.
        logical :: has_previous_generation = .false.
        character(len=NAME_LEN) :: gate_cases(MAX_NODES) = ''
        integer :: event_epoch = 0
        character(len=HASH_LEN) :: requirement_digest = ''
        integer :: gate_required_count = 0
        character(len=NAME_LEN) :: targets(MAX_NODES) = ''
        integer :: n_targets = 0
    end type gremlin_request_t

    public :: gremlin_request_t, parse_request, is_hex_digest, &
        gremlin_json_text_valid, gremlin_json_parser_next

contains

    subroutine parse_request(action, json, request, ierr, message)
        character(len=*), intent(in) :: action, json
        type(gremlin_request_t), intent(out) :: request
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(json_parser_t) :: parser
        type(json_event_t) :: event, value_event
        character(len=NAME_LEN) :: key
        character(len=:), allocatable :: raw_value
        logical :: seen(19)
        integer :: field, value_start, value_end

        ierr = 1
        message = 'malformed Gremlin request JSON'
        if (len_trim(json) > 65536) then
            message = 'Gremlin request exceeds 65536 characters'
            return
        end if
        if (.not. gremlin_json_text_valid(json)) then
            if (strict_json_has_trailing(text=json)) &
                message = 'trailing text after Gremlin request object'
            return
        end if
        call json_parser_init(parser, json)
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_OBJECT_START) return
        seen = .false.
        do
            ierr = 1
            raw_value = ''
            call gremlin_json_parser_next(parser, event)
            if (event%event_type == JSON_OBJECT_END) exit
            if (event%event_type /= JSON_KEY .or. .not. allocated(event%string_val)) return
            if (len(event%string_val) > len(key)) then
                message = 'Gremlin request field name is too long'
                return
            end if
            if (len_trim(event%string_val) /= len(event%string_val)) then
                message = 'unsupported Gremlin request field: '//event%string_val
                return
            end if
            key = event%string_val
            field = request_field(action, trim(key))
            if (field == 0) then
                message = 'unsupported Gremlin request field: '//trim(key)
                return
            end if
            if (seen(field)) then
                message = 'duplicate Gremlin request field: '//trim(key)
                return
            end if
            seen(field) = .true.
            value_start = json_value_first(parser)
            call gremlin_json_parser_next(parser, value_event)
            value_end = parser%pos - 1
            raw_value = parser%input(value_start:value_end)
            if (field == 11 .or. field == 13 .or. field == 18) then
                if (value_event%event_type == JSON_ERROR .and. &
                    is_json_integer(raw_value)) value_event%event_type = JSON_INTEGER
            end if
            if (value_event%event_type == JSON_ERROR) then
                message = 'malformed Gremlin request JSON'
                return
            end if
            if (field == 10) then
                call parse_target_events(parser, value_event, request, ierr, message)
            else
                call set_event_field(request, field, value_event, &
                    raw_value, ierr, message)
            end if
            if (ierr /= 0) return
        end do
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_END_OF_INPUT) then
            message = 'trailing text after Gremlin request object'
            return
        end if
        if (len_trim(request%lane_id) == 0 .or. &
            len_trim(request%lane_id) > MAX_LANE_LEN .or. &
            index(request%lane_id, '/') > 0 .or. &
            index(request%lane_id, achar(10)) > 0 .or. &
            index(request%lane_id, achar(92)) > 0) then
            message = 'lane_id must be a short path-free string'
            return
        end if
        if (len_trim(request%session_id) > 0) then
            if (index(request%session_id, '/') > 0 .or. &
                index(request%session_id, achar(10)) > 0) then
                message = 'session_id contains an invalid character'
                return
            end if
        end if
        if (request%n_targets > 0 .and. action /= 'start' .and. &
            action /= 'run' .and. action /= 'gremlin_start') then
            message = 'targets are only supported when starting a campaign'
            return
        end if
        ierr = 0
        message = ''
    end subroutine parse_request

    logical function gremlin_json_text_valid(text)
        character(len=*), intent(in) :: text
        integer :: position

        gremlin_json_text_valid = .false.
        position = 1
        call strict_json_space(text, position)
        call strict_json_value(text, position, 0, gremlin_json_text_valid)
        if (.not. gremlin_json_text_valid) return
        call strict_json_space(text, position)
        gremlin_json_text_valid = position > len_trim(text)
    end function gremlin_json_text_valid

    logical function strict_json_has_trailing(text)
        character(len=*), intent(in) :: text
        integer :: position
        logical :: valid

        position = 1
        call strict_json_space(text, position)
        call strict_json_value(text, position, 0, valid)
        if (.not. valid) then
            strict_json_has_trailing = .false.
            return
        end if
        call strict_json_space(text, position)
        strict_json_has_trailing = position <= len_trim(text)
    end function strict_json_has_trailing

    recursive subroutine strict_json_value(text, position, depth, valid)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        integer, intent(in) :: depth
        logical, intent(out) :: valid
        character(len=1) :: ch

        valid = .false.
        if (position > len_trim(text)) return
        ch = text(position:position)
        select case (ch)
        case ('{')
            call strict_json_object(text, position, depth + 1, valid)
        case ('[')
            call strict_json_array(text, position, depth + 1, valid)
        case ('"')
            call strict_json_string(text, position, valid)
        case ('t')
            call strict_json_literal(text, position, 'true', valid)
        case ('f')
            call strict_json_literal(text, position, 'false', valid)
        case ('n')
            call strict_json_literal(text, position, 'null', valid)
        case ('-', '0':'9')
            call strict_json_number(text, position, valid)
        case default
            return
        end select
    end subroutine strict_json_value

    recursive subroutine strict_json_object(text, position, depth, valid)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        integer, intent(in) :: depth
        logical, intent(out) :: valid
        logical :: member_valid

        valid = .false.
        if (depth > MAX_JSON_DEPTH) return
        position = position + 1
        call strict_json_space(text, position)
        if (position <= len_trim(text)) then
            if (text(position:position) == '}') then
                position = position + 1
                valid = .true.
                return
            end if
        end if
        do
            if (position > len_trim(text)) return
            if (text(position:position) /= '"') return
            call strict_json_string(text, position, member_valid)
            if (.not. member_valid) return
            call strict_json_space(text, position)
            if (position > len_trim(text)) return
            if (text(position:position) /= ':') return
            position = position + 1
            call strict_json_space(text, position)
            call strict_json_value(text, position, depth, member_valid)
            if (.not. member_valid) return
            call strict_json_space(text, position)
            if (position > len_trim(text)) return
            if (text(position:position) == '}') then
                position = position + 1
                valid = .true.
                return
            end if
            if (text(position:position) /= ',') return
            position = position + 1
            call strict_json_space(text, position)
        end do
    end subroutine strict_json_object

    recursive subroutine strict_json_array(text, position, depth, valid)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        integer, intent(in) :: depth
        logical, intent(out) :: valid
        logical :: item_valid

        valid = .false.
        if (depth > MAX_JSON_DEPTH) return
        position = position + 1
        call strict_json_space(text, position)
        if (position <= len_trim(text)) then
            if (text(position:position) == ']') then
                position = position + 1
                valid = .true.
                return
            end if
        end if
        do
            call strict_json_value(text, position, depth, item_valid)
            if (.not. item_valid) return
            call strict_json_space(text, position)
            if (position > len_trim(text)) return
            if (text(position:position) == ']') then
                position = position + 1
                valid = .true.
                return
            end if
            if (text(position:position) /= ',') return
            position = position + 1
            call strict_json_space(text, position)
        end do
    end subroutine strict_json_array

    subroutine strict_json_string(text, position, valid, decoded)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        logical, intent(out) :: valid
        character(len=:), allocatable, intent(out), optional :: decoded
        character(len=:), allocatable :: buffer
        character(len=4) :: bytes
        integer :: code, n, byte_count
        character(len=1) :: ch
        logical :: valid_code

        valid = .false.
        n = 0
        if (present(decoded)) allocate (character(len=len(text)) :: buffer)
        if (position > len_trim(text)) return
        if (text(position:position) /= '"') return
        position = position + 1
        do while (position <= len_trim(text))
            ch = text(position:position)
            if (ch == '"') then
                position = position + 1
                valid = .true.
                if (present(decoded)) decoded = buffer(:n)
                return
            end if
            if (iachar(ch) < 32) return
            if (ch == achar(92)) then
                position = position + 1
                if (position > len_trim(text)) return
                ch = text(position:position)
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
                    call json_unicode_code(text, position, code, valid_code)
                    if (.not. valid_code) return
                    if (present(decoded)) then
                        call json_utf8_bytes(code, bytes, byte_count)
                        buffer(n + 1:n + byte_count) = bytes(:byte_count)
                        n = n + byte_count
                    end if
                    position = position + 1
                    cycle
                case default
                    return
                end select
            end if
            if (present(decoded)) then
                n = n + 1
                buffer(n:n) = ch
            end if
            position = position + 1
        end do
    end subroutine strict_json_string

    subroutine json_unicode_code(text, position, code, valid)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        integer, intent(out) :: code
        logical, intent(out) :: valid
        integer :: digit, i, low

        valid = .false.
        code = 0
        if (position + 4 > len(text)) return
        do i = 1, 4
            position = position + 1
            digit = json_hex_digit(text(position:position))
            if (digit < 0) return
            code = code*16 + digit
        end do
        if (code >= 55296 .and. code <= 56319) then
            if (position + 6 > len(text)) return
            if (text(position + 1:position + 2) /= achar(92)//'u') return
            position = position + 2
            low = 0
            do i = 1, 4
                position = position + 1
                digit = json_hex_digit(text(position:position))
                if (digit < 0) return
                low = low*16 + digit
            end do
            if (low < 56320 .or. low > 57343) return
            code = 65536 + (code - 55296)*1024 + low - 56320
        else if (code >= 56320 .and. code <= 57343) then
            return
        end if
        valid = .true.
    end subroutine json_unicode_code

    subroutine json_utf8_bytes(code, bytes, n)
        integer, intent(in) :: code
        character(len=4), intent(out) :: bytes
        integer, intent(out) :: n

        bytes = ''
        if (code < 128) then
            n = 1
            bytes(1:1) = achar(code)
        else if (code < 2048) then
            n = 2
            bytes(1:1) = achar(192 + code/64)
        else if (code < 65536) then
            n = 3
            bytes(1:1) = achar(224 + code/4096)
        else
            n = 4
            bytes(1:1) = achar(240 + code/262144)
            bytes(2:2) = achar(128 + mod(code/4096, 64))
        end if
        if (n >= 3) bytes(n - 1:n - 1) = achar(128 + mod(code/64, 64))
        if (n >= 2) bytes(n:n) = achar(128 + mod(code, 64))
    end subroutine json_utf8_bytes

    subroutine gremlin_json_parser_next(parser, event)
        type(json_parser_t), intent(inout) :: parser
        type(json_event_t), intent(out) :: event
        integer :: position
        logical :: valid

        position = json_value_first(parser)
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_KEY .and. event%event_type /= JSON_STRING) return
        ! fx drops non-ASCII Unicode escapes. Decode from the unchanged raw input
        ! so keys, values and MCP forwarding retain their exact JSON meaning.
        call strict_json_string(parser%input, position, valid, event%string_val)
        if (.not. valid) event%event_type = JSON_ERROR
    end subroutine gremlin_json_parser_next

    subroutine strict_json_number(text, position, valid)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        logical, intent(out) :: valid
        integer :: first_digit

        valid = .false.
        if (text(position:position) == '-') position = position + 1
        if (position > len_trim(text)) return
        if (text(position:position) == '0') then
            position = position + 1
            if (position <= len_trim(text)) then
                if (text(position:position) >= '0' .and. text(position:position) <= '9') return
            end if
        else if (text(position:position) >= '1' .and. text(position:position) <= '9') then
            do while (position <= len_trim(text))
                if (text(position:position) < '0' .or. text(position:position) > '9') exit
                position = position + 1
            end do
        else
            return
        end if
        if (position <= len_trim(text)) then
            if (text(position:position) == '.') then
                position = position + 1
                first_digit = position
                do while (position <= len_trim(text))
                    if (text(position:position) < '0' .or. text(position:position) > '9') exit
                    position = position + 1
                end do
                if (position == first_digit) return
            end if
        end if
        if (position <= len_trim(text)) then
            if (text(position:position) == 'e' .or. text(position:position) == 'E') then
                position = position + 1
                if (position <= len_trim(text)) then
                    if (text(position:position) == '+' .or. text(position:position) == '-') &
                        position = position + 1
                end if
                first_digit = position
                do while (position <= len_trim(text))
                    if (text(position:position) < '0' .or. text(position:position) > '9') exit
                    position = position + 1
                end do
                if (position == first_digit) return
            end if
        end if
        valid = .true.
    end subroutine strict_json_number

    subroutine strict_json_literal(text, position, literal, valid)
        character(len=*), intent(in) :: text, literal
        integer, intent(inout) :: position
        logical, intent(out) :: valid

        valid = .false.
        if (position + len(literal) - 1 > len_trim(text)) return
        if (text(position:position + len(literal) - 1) /= literal) return
        position = position + len(literal)
        valid = .true.
    end subroutine strict_json_literal

    subroutine strict_json_space(text, position)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position

        do while (position <= len_trim(text))
            select case (text(position:position))
            case (' ', achar(9), achar(10), achar(13))
                position = position + 1
            case default
                return
            end select
        end do
    end subroutine strict_json_space

    integer function json_hex_digit(character)
        character(len=1), intent(in) :: character
        integer :: code

        code = iachar(character)
        select case (code)
        case (iachar('0'):iachar('9'))
            json_hex_digit = code - iachar('0')
        case (iachar('a'):iachar('f'))
            json_hex_digit = code - iachar('a') + 10
        case (iachar('A'):iachar('F'))
            json_hex_digit = code - iachar('A') + 10
        case default
            json_hex_digit = -1
        end select
    end function json_hex_digit

    subroutine set_event_field(request, field, event, raw_value, ierr, message)
        type(gremlin_request_t), intent(inout) :: request
        integer, intent(in) :: field
        type(json_event_t), intent(in) :: event
        character(len=*), intent(in) :: raw_value
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: value
        logical :: is_string

        value = ''
        is_string = event%event_type == JSON_STRING
        if (is_string) then
            if (.not. allocated(event%string_val)) then
                ierr = 1
                message = 'invalid Gremlin request string'
                return
            end if
            if (len(event%string_val) > len(value)) then
                ierr = 1
                message = 'Gremlin request string is too long'
                return
            end if
            value = event%string_val
        else
            select case (event%event_type)
            case (JSON_INTEGER)
                if (field == 11 .or. field == 13) then
                    value = raw_value
                else
                    write (value, '(i0)') event%int_val
                end if
            case (JSON_BOOL)
                if (event%bool_val) then
                    value = 'true'
                else
                    value = 'false'
                end if
            case default
                ierr = 1
                message = 'Gremlin request field has the wrong JSON type'
                return
            end select
        end if
        call set_request_field(request, field, trim(value), is_string, ierr, message)
    end subroutine set_event_field

    integer function json_value_first(parser)
        type(json_parser_t), intent(in) :: parser
        integer :: position

        position = parser%pos
        do while (position <= len(parser%input))
            select case (parser%input(position:position))
            case (' ', achar(9), achar(10), achar(13), ':', ',')
                position = position + 1
            case default
                exit
            end select
        end do
        json_value_first = position
    end function json_value_first

    subroutine parse_target_events(parser, first, request, ierr, message)
        type(json_parser_t), intent(inout) :: parser
        type(json_event_t), intent(in) :: first
        type(gremlin_request_t), intent(inout) :: request
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(json_event_t) :: event
        character(len=NAME_LEN) :: item

        ierr = 1
        message = 'targets must be an array of strings'
        if (first%event_type /= JSON_ARRAY_START) return
        do
            call gremlin_json_parser_next(parser, event)
            if (event%event_type == JSON_ARRAY_END) exit
            if (event%event_type /= JSON_STRING .or. &
                .not. allocated(event%string_val)) return
            if (request%n_targets >= size(request%targets)) then
                message = 'too many target names'
                return
            end if
            item = ''
            if (len_trim(event%string_val) == 0 .or. &
                len(event%string_val) > len(item)) then
                message = 'target names must be nonempty and at most 128 characters'
                return
            end if
            item = event%string_val
            if (has_control_character(item)) then
                message = 'target names cannot contain control characters'
                return
            end if
            if (any(request%targets(:request%n_targets) == item)) then
                message = 'duplicate target name: '//trim(item)
                return
            end if
            request%n_targets = request%n_targets + 1
            request%targets(request%n_targets) = item
        end do
        ierr = 0
        message = ''
    end subroutine parse_target_events

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
            case ('lifecycle_cursor')
                if (action == 'status' .or. action == 'events' .or. &
                    action == 'wait' .or. action == 'failures') request_field = 18
            case ('wait_ms')
                if (action == 'wait') request_field = 14
            case ('wait_until')
                if (action == 'wait') request_field = 19
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
        case (1, 2, 16, 17, 19)
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
            case (19)
                if (.not. valid_wait_until(value)) then
                    ierr = 1
                    message = 'wait_until must be local-gate-green, ordinary-verified, '// &
                        'fully-verified, quiescent, or failure'
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
            case (19)
                request%wait_until = value
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
            if (field == 11 .or. field == 13 .or. field == 18) then
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
            if (field == 18) request%lifecycle_cursor = wide
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
            if (request%timeout_seconds < 0 .or. request%timeout_seconds > MAX_CASE_TIMEOUT) then
                ierr = 1
                message = 'timeout_seconds must be between 0 and 86400'
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
        case (18)
            if (request%lifecycle_cursor < 0_int64) then
                ierr = 1
                message = 'lifecycle_cursor must be nonnegative'
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

    logical function valid_wait_until(value)
        character(len=*), intent(in) :: value

        select case (trim(value))
        case ('local-gate-green', 'ordinary-verified', 'fully-verified', &
                'quiescent', 'failure')
            valid_wait_until = .true.
        case default
            valid_wait_until = .false.
        end select
    end function valid_wait_until

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
