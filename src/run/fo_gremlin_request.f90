module fo_gremlin_request
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_cache, only: HASH_LEN
    use fo_json_object, only: json_object_member, json_text_has_control
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
        json_parser_next, JSON_OBJECT_START, JSON_OBJECT_END, JSON_ARRAY_START, &
        JSON_ARRAY_END, JSON_KEY, JSON_STRING, JSON_INTEGER, JSON_BOOL, &
        JSON_NULL_VAL, JSON_REAL, &
        JSON_ERROR, JSON_END_OF_INPUT, JSON_ERR_TRAILING
    use fx_dag, only: MAX_NODES
    use fo_case_filter, only: case_filter_t, filter_parse
    implicit none
    private

    integer, parameter :: NAME_LEN = 1024, PATH_LEN = 4096
    integer, parameter :: DEFAULT_RANDOM = 32, DEFAULT_CAMPAIGN = 60
    integer, parameter :: DEFAULT_CASE_TIMEOUT = 0, MAX_CASE_TIMEOUT = 86400
    integer, parameter :: MAX_LANE_LEN = 96

    type :: gremlin_request_t
        character(len=MAX_LANE_LEN) :: lane_id = 'default'
        character(len=128) :: session_id = ''
        character(len=NAME_LEN) :: case_id = ''
        integer :: random_count = DEFAULT_RANDOM
        integer :: seed = 0
        integer :: campaign_seconds = DEFAULT_CAMPAIGN
        integer :: timeout_seconds = DEFAULT_CASE_TIMEOUT
        integer :: jobs = 0  !! 0: derive from the host job budget
        integer(int64) :: cursor = 0_int64
        integer(int64) :: lifecycle_cursor = 0_int64
        integer :: wait_ms = 5000
        integer :: max_records = 32
        integer(int64) :: max_bytes = 262144_int64
        logical :: only_changed = .false.
        character(len=NAME_LEN) :: filter = ''
        logical :: shuffle = .false.
        character(len=HASH_LEN) :: generation_id = ''
        character(len=32) :: wait_until = ''
        character(len=8) :: detail = 'full'
        !! 'persistent' lanes outlive their client; 'task' lanes stop with it.
        character(len=10) :: lifetime = 'persistent'
        logical :: input_changed = .false.
        logical :: has_previous_generation = .false.
        character(len=NAME_LEN), allocatable :: impact_cases(:)
        integer :: n_impact_cases = 0
        logical :: impact_all = .false.
        ! Keep name inventories off the stack when requests are copied for polling.
        character(len=NAME_LEN), allocatable :: gate_cases(:)
        integer :: event_epoch = 0
        character(len=HASH_LEN) :: requirement_digest = ''
        integer :: gate_required_count = 0
        character(len=NAME_LEN), allocatable :: targets(:)
        integer :: n_targets = 0
        ! Lane scope (`fo test` -L, -LE, --layer semantics); empty = no scope.
        character(len=NAME_LEN), allocatable :: labels(:), exclude_labels(:), layers(:)
        integer :: n_labels = 0, n_exclude_labels = 0, n_layers = 0
    end type gremlin_request_t

    ! Action bits for the option table; GA_OTHER covers unknown action names.
    integer, parameter, public :: GA_START = 1, GA_STATUS = 2, GA_WAIT = 4, &
        GA_EVENTS = 8, GA_FAILURES = 16, GA_REPRODUCE = 32, GA_STOP = 64, &
        GA_OTHER = 128
    integer, parameter :: GA_ANY = 255, GA_READS = GA_STATUS + GA_WAIT + &
        GA_EVENTS + GA_FAILURES
    integer(int64), parameter, public :: GA_UNBOUNDED = huge(0_int64)

    type, public :: gremlin_option_t
        !! One Gremlin request option: its JSON key, CLI flags, value kind
        !! ('int', 'string', 'bool' or 'list'), the actions that accept it, its
        !! integer range and string choices. The single source for CLI parsing,
        !! request validation and the MCP tool schema.
        character(len=16) :: key = ''
        character(len=18) :: flag = ''
        character(len=18) :: alias = ''
        character(len=6) :: kind = ''
        integer :: actions = 0
        integer(int64) :: lo = -GA_UNBOUNDED, hi = GA_UNBOUNDED
        character(len=72) :: choices = ''
        character(len=72) :: note = ''
    end type gremlin_option_t

    integer, parameter, public :: N_GREMLIN_OPTIONS = 24
    type(gremlin_option_t), parameter, public :: GREMLIN_OPTIONS(N_GREMLIN_OPTIONS) = [ &
        gremlin_option_t('lane_id', '--lane', '', 'string', GA_ANY), &
        gremlin_option_t('session_id', '--session', '', 'string', GA_ANY - GA_START), &
        gremlin_option_t('case_id', '--case', '', 'string', GA_REPRODUCE), &
        gremlin_option_t('generation_id', '--generation', '', 'string', GA_REPRODUCE), &
        gremlin_option_t('targets', '--target', '', 'list', GA_START), &
        gremlin_option_t('labels', '-L', '--label', 'list', GA_START, &
            note='gremlin lane scope: fo test -L regexes (AND)'), &
        gremlin_option_t('exclude_labels', '-LE', '--exclude-label', 'list', GA_START, &
            note='gremlin lane scope: fo test -LE regexes'), &
        gremlin_option_t('layers', '--layer', '', 'list', GA_START, &
            note='gremlin lane scope: layer names (union)'), &
        gremlin_option_t('filter', '-E', '--filter', 'string', GA_START, &
            note='case filter expression of fo test -E (no affected()/failed())'), &
        gremlin_option_t('only_changed', '--only-changed', '', 'bool', GA_START), &
        gremlin_option_t('shuffle', '--shuffle', '', 'bool', GA_START), &
        gremlin_option_t('random_count', '--random', '', 'int', GA_START, 0, 32, &
            note='additional random cases; 0 quiesces after gate targets pass'), &
        gremlin_option_t('seed', '--seed', '', 'int', GA_START, &
            -int(huge(0), int64) - 1_int64, int(huge(0), int64)), &
        gremlin_option_t('campaign_seconds', '--campaign-seconds', '', 'int', GA_START, 1, 60), &
        gremlin_option_t('timeout_seconds', '--timeout-seconds', '', 'int', &
            GA_START + GA_REPRODUCE, 0, 86400), &
        gremlin_option_t('jobs', '--jobs', '', 'int', GA_START, 1, 16, &
            note='case children run in parallel (default: host weight)'), &
        gremlin_option_t('cursor', '--cursor', '', 'int', GA_READS, 0, GA_UNBOUNDED), &
        gremlin_option_t('lifecycle_cursor', '--lifecycle-cursor', '', 'int', GA_READS, &
            0, GA_UNBOUNDED), &
        gremlin_option_t('max_records', '--max-records', '', 'int', GA_READS, 1, 128), &
        gremlin_option_t('max_bytes', '--max-bytes', '', 'int', GA_READS, 1, 262144), &
        gremlin_option_t('lifetime', '--owner', '--lifetime', 'string', GA_START, &
            choices='persistent,task', &
            note='task: the lane stops when the client that started it ends'), &
        gremlin_option_t('wait_ms', '--wait-ms', '', 'int', GA_WAIT, 0, 30000), &
        gremlin_option_t('wait_until', '--until', '', 'string', GA_WAIT, &
            choices='local-gate-green,ordinary-verified,fully-verified,quiescent,failure'), &
        gremlin_option_t('detail', '--detail', '', 'string', GA_STATUS + GA_WAIT, &
            choices='summary,full', &
            note='gremlin_status/gremlin_wait response detail')]

    public :: gremlin_request_t, parse_request, is_hex_digest, gremlin_json_text_valid
    public :: gremlin_json_field, gremlin_option_index, gremlin_action_bit
    public :: gremlin_apply_adapter_defaults

contains

    integer function gremlin_action_bit(action) result(bit)
        !! Bit of a core or public (gremlin_*) action name; GA_OTHER if unknown.
        character(len=*), intent(in) :: action
        character(len=len(action)) :: name

        name = action
        if (index(name, 'gremlin_') == 1) name = name(9:)
        select case (trim(name))
        case ('start', 'run')
            bit = GA_START
        case ('status')
            bit = GA_STATUS
        case ('wait')
            bit = GA_WAIT
        case ('events')
            bit = GA_EVENTS
        case ('failures')
            bit = GA_FAILURES
        case ('reproduce')
            bit = GA_REPRODUCE
        case ('stop')
            bit = GA_STOP
        case default
            bit = GA_OTHER
        end select
    end function gremlin_action_bit

    integer function gremlin_option_index(key, action, flag) result(row)
        !! Row of an option by JSON key (or by CLI flag when `flag` is set),
        !! restricted to the options `action` accepts when it is present.
        character(len=*), intent(in) :: key
        character(len=*), intent(in), optional :: action
        logical, intent(in), optional :: flag
        integer :: i
        logical :: by_flag

        by_flag = .false.
        if (present(flag)) by_flag = flag
        row = 0
        do i = 1, N_GREMLIN_OPTIONS
            if (by_flag) then
                if (trim(key) /= trim(GREMLIN_OPTIONS(i)%flag) .and. &
                    (len_trim(GREMLIN_OPTIONS(i)%alias) == 0 .or. &
                    trim(key) /= trim(GREMLIN_OPTIONS(i)%alias))) cycle
            else
                if (trim(key) /= trim(GREMLIN_OPTIONS(i)%key)) cycle
            end if
            if (present(action)) then
                if (iand(GREMLIN_OPTIONS(i)%actions, gremlin_action_bit(action)) == 0) cycle
            end if
            row = i
            return
        end do
    end function gremlin_option_index

    subroutine gremlin_apply_adapter_defaults(action, request_json)
        !! The one adapter default shared by CLI and MCP: status and wait
        !! answer in summary detail unless the caller chose a detail level.
        character(len=*), intent(in) :: action
        character(len=:), allocatable, intent(inout) :: request_json
        character(len=:), allocatable :: raw
        integer :: count, ierr, last

        if (iand(gremlin_action_bit(action), GA_STATUS + GA_WAIT) == 0) return
        call json_object_member(request_json, 'detail', raw, count, ierr)
        if (ierr /= 0 .or. count /= 0) return
        last = len_trim(request_json)
        if (last < 2) return
        if (last > 2) then
            request_json = request_json(:last - 1)//',"detail":"summary"}'
        else
            request_json = '{"detail":"summary"}'
        end if
    end subroutine gremlin_apply_adapter_defaults

    subroutine parse_request(action, json, request, ierr, message)
        character(len=*), intent(in) :: action, json
        type(gremlin_request_t), intent(out) :: request
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(json_parser_t) :: parser
        type(json_event_t) :: event, value_event
        character(len=NAME_LEN) :: key
        logical :: seen(N_GREMLIN_OPTIONS)
        integer :: field
        logical :: trailing

        ierr = 1
        message = 'malformed Gremlin request JSON'
        if (len_trim(json) > 65536) then
            message = 'Gremlin request exceeds 65536 characters'
            return
        end if
        if (.not. gremlin_json_text_valid(json, trailing)) then
            if (trailing) message = 'trailing text after Gremlin request object'
            return
        end if
        call json_parser_init_strict(parser, json)
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_OBJECT_START) return
        allocate (request%gate_cases(MAX_NODES), request%targets(MAX_NODES))
        request%gate_cases = ''
        request%targets = ''
        allocate (request%labels(0), request%exclude_labels(0), request%layers(0))
        seen = .false.
        do
            ierr = 1
            call json_parser_next(parser, event)
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
            field = gremlin_option_index(trim(key), action)
            if (field == 0) then
                message = 'unsupported Gremlin request field: '//trim(key)
                return
            end if
            if (seen(field)) then
                message = 'duplicate Gremlin request field: '//trim(key)
                return
            end if
            seen(field) = .true.
            call json_parser_next(parser, value_event)
            if (value_event%event_type == JSON_ERROR) then
                message = 'malformed Gremlin request JSON'
                return
            end if
            if (GREMLIN_OPTIONS(field)%kind == 'list') then
                call parse_list_events(parser, value_event, request, field, ierr, message)
            else
                call set_event_field(request, field, value_event, ierr, message)
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
        ierr = 0
        message = ''
    end subroutine parse_request

    logical function gremlin_json_text_valid(text, trailing)
        character(len=*), intent(in) :: text
        logical, intent(out), optional :: trailing
        type(json_parser_t) :: parser
        type(json_event_t) :: event

        gremlin_json_text_valid = .false.
        if (present(trailing)) trailing = .false.
        call json_parser_init_strict(parser, text)
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ERROR) then
                if (present(trailing)) trailing = event%error_code == JSON_ERR_TRAILING
                return
            end if
            if (event%event_type == JSON_END_OF_INPUT) then
                gremlin_json_text_valid = .true.
                return
            end if
        end do
    end function gremlin_json_text_valid

    subroutine gremlin_json_field(text, name, value, last, status)
        !! Scalar top-level member of a JSON object as text. status: 0 present,
        !! 1 absent or not a scalar, 2 malformed (invalid JSON, a duplicate key
        !! unless `last` selects the final one, or a value longer than `value`).
        !! value is empty unless status is 0.
        character(len=*), intent(in) :: text, name
        character(len=*), intent(out) :: value
        logical, optional, intent(in) :: last
        integer, optional, intent(out) :: status
        type(json_event_t) :: event
        character(len=:), allocatable :: raw, candidate
        integer :: count, ierr, state

        value = ''
        state = 1
        call json_object_member(text, name, raw, count, ierr, event, last)
        if (ierr /= 0) then
            state = 2
        else if (count > 1 .and. .not. present(last)) then
            state = 2
        else if (count > 0) then
            select case (event%event_type)
            case (JSON_STRING)
                if (allocated(event%string_val)) then
                    candidate = event%string_val
                    state = 0
                end if
            case (JSON_INTEGER, JSON_REAL, JSON_BOOL, JSON_NULL_VAL)
                candidate = raw
                state = 0
            end select
            if (state == 0) then
                if (len(candidate) > len(value)) then
                    state = 2
                else
                    value = candidate
                end if
            end if
        end if
        if (present(status)) status = state
    end subroutine gremlin_json_field

    subroutine set_event_field(request, field, event, ierr, message)
        type(gremlin_request_t), intent(inout) :: request
        integer, intent(in) :: field
        type(json_event_t), intent(in) :: event
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: value
        logical :: is_string

        ierr = 1
        if (GREMLIN_OPTIONS(field)%kind == 'int') then
            if (event%event_type == JSON_BOOL) then
                message = 'request field must be a JSON integer'
                return
            else if (event%event_type /= JSON_INTEGER .and. &
                event%event_type /= JSON_STRING) then
                message = 'Gremlin request field has the wrong JSON type'
                return
            end if
        end if
        value = ''
        is_string = event%event_type == JSON_STRING
        if (is_string) then
            if (.not. allocated(event%string_val)) then
                message = 'invalid Gremlin request string'
                return
            end if
            if (len(event%string_val) > len(value)) then
                message = 'Gremlin request string is too long'
                return
            end if
            value = event%string_val
        else
            select case (event%event_type)
            case (JSON_INTEGER)
                value = event%raw_val
            case (JSON_BOOL)
                value = merge('true ', 'false', event%bool_val)
            case default
                message = 'Gremlin request field has the wrong JSON type'
                return
            end select
        end if
        call set_request_field(request, field, trim(value), is_string, ierr, message)
    end subroutine set_event_field

    subroutine parse_list_events(parser, first, request, field, ierr, message)
        !! targets, labels, exclude_labels and layers: arrays of strings.
        type(json_parser_t), intent(inout) :: parser
        type(json_event_t), intent(in) :: first
        type(gremlin_request_t), intent(inout) :: request
        integer, intent(in) :: field
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(json_event_t) :: event
        character(len=NAME_LEN) :: item
        character(len=16) :: key
        logical :: is_target

        key = GREMLIN_OPTIONS(field)%key
        is_target = trim(key) == 'targets'
        ierr = 1
        message = trim(key)//' must be an array of strings'
        if (first%event_type /= JSON_ARRAY_START) return
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ARRAY_END) exit
            if (event%event_type /= JSON_STRING .or. &
                .not. allocated(event%string_val)) return
            item = ''
            if (is_target) then
                if (request%n_targets >= size(request%targets)) then
                    message = 'too many target names'
                    return
                end if
            end if
            if (len_trim(event%string_val) == 0 .or. &
                len(event%string_val) > len(item)) then
                if (is_target) then
                    message = 'target names must be nonempty and at most 1024 characters'
                else
                    message = trim(key)//' entries must be nonempty and at most 1024 '// &
                        'characters'
                end if
                return
            end if
            item = event%string_val
            if (json_text_has_control(trim(item))) then
                if (is_target) then
                    message = 'target names cannot contain control characters'
                else
                    message = trim(key)//' entries cannot contain control characters'
                end if
                return
            end if
            select case (trim(key))
            case ('targets')
                if (any(request%targets(:request%n_targets) == item)) then
                    message = 'duplicate target name: '//trim(item)
                    return
                end if
                request%n_targets = request%n_targets + 1
                request%targets(request%n_targets) = item
            case ('labels')
                call append_item(request%labels, request%n_labels, item)
            case ('exclude_labels')
                call append_item(request%exclude_labels, request%n_exclude_labels, item)
            case default
                call append_item(request%layers, request%n_layers, item)
            end select
        end do
        ierr = 0
        message = ''
    end subroutine parse_list_events

    subroutine append_item(list, n, item)
        character(len=NAME_LEN), allocatable, intent(inout) :: list(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: item
        character(len=NAME_LEN), allocatable :: grown(:)

        allocate (grown(n + 1))
        grown(:n) = list
        grown(n + 1) = item
        call move_alloc(grown, list)
        n = n + 1
    end subroutine append_item

    subroutine set_request_field(request, field, value, is_string, ierr, message)
        type(gremlin_request_t), intent(inout) :: request
        integer, intent(in) :: field
        character(len=*), intent(in) :: value
        logical, intent(in) :: is_string
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(gremlin_option_t) :: option
        character(len=16) :: key
        type(case_filter_t) :: expression
        character(len=:), allocatable :: filter_message
        integer :: ios
        integer(int64) :: wide

        option = GREMLIN_OPTIONS(field)
        key = option%key
        ierr = 1
        message = ''
        select case (trim(option%kind))
        case ('string')
            if (.not. is_string) then
                message = 'string request field has the wrong JSON type'
                return
            end if
            if (json_text_has_control(trim(value))) then
                message = 'string request field contains a control character'
                return
            end if
            if (len_trim(option%choices) > 0) then
                if (.not. in_choices(value, option%choices)) then
                    message = trim(key)//' must be '//choice_phrase(option%choices)
                    return
                end if
            end if
            select case (trim(key))
            case ('lane_id')
                if (len_trim(value) > MAX_LANE_LEN) then
                    message = 'lane_id is too long'
                    return
                end if
                request%lane_id = value
            case ('session_id')
                if (len_trim(value) > len(request%session_id)) then
                    message = 'session_id is too long'
                    return
                end if
                request%session_id = value
            case ('case_id')
                if (len_trim(value) == 0 .or. len_trim(value) > len(request%case_id)) then
                    message = 'case_id must be nonempty and at most 1024 characters'
                    return
                end if
                request%case_id = value
            case ('generation_id')
                if (len_trim(value) /= HASH_LEN) then
                    message = 'generation_id must contain exactly 64 characters'
                    return
                end if
                if (.not. is_hex_digest(value)) then
                    message = 'generation_id must be a hexadecimal digest'
                    return
                end if
                request%generation_id = value
            case ('filter')
                if (len_trim(value) > len(request%filter)) then
                    message = 'filter is too long'
                    return
                end if
                call filter_parse(trim(value), expression, ios, filter_message)
                if (ios /= 0) then
                    message = 'filter is invalid: '//filter_message
                    return
                end if
                if (expression%uses_affected .or. expression%uses_failed) then
                    message = 'filter cannot use affected() or failed() at lane start'
                    return
                end if
                request%filter = value
            case ('wait_until')
                request%wait_until = value
            case ('lifetime')
                request%lifetime = value
            case ('detail')
                request%detail = value
            end select
        case ('bool')
            if (is_string) then
                message = 'boolean request field has the wrong JSON type'
                return
            end if
            if (trim(value) /= 'true' .and. trim(value) /= 'false') then
                message = 'boolean request field must be true or false'
                return
            end if
            if (trim(key) == 'only_changed') then
                request%only_changed = trim(value) == 'true'
            else
                request%shuffle = trim(value) == 'true'
            end if
        case default
            if (is_string) then
                message = 'integer request field has the wrong JSON type'
                return
            end if
            read (value, *, iostat=ios) wide
            if (ios /= 0) then
                message = 'integer request field is outside its supported range'
                return
            end if
            if (wide < option%lo .or. wide > option%hi) then
                if (option%lo == option%hi) then
                    message = trim(key)//' must be '//int_text(option%lo)
                else if (option%hi == GA_UNBOUNDED .and. option%lo == 0) then
                    message = trim(key)//' must be nonnegative'
                else
                    message = trim(key)//' must be between '//int_text(option%lo)// &
                        ' and '//int_text(option%hi)
                end if
                return
            end if
            select case (trim(key))
            case ('random_count')
                request%random_count = int(wide)
            case ('seed')
                request%seed = int(wide)
            case ('campaign_seconds')
                request%campaign_seconds = int(wide)
            case ('timeout_seconds')
                request%timeout_seconds = int(wide)
            case ('jobs')
                request%jobs = int(wide)
            case ('cursor')
                request%cursor = wide
            case ('lifecycle_cursor')
                request%lifecycle_cursor = wide
            case ('max_records')
                request%max_records = int(wide)
            case ('max_bytes')
                request%max_bytes = wide
            case ('wait_ms')
                request%wait_ms = int(wide)
            end select
        end select
        ierr = 0
    end subroutine set_request_field

    function int_text(value) result(text)
        integer(int64), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=20) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function int_text

    logical function in_choices(value, choices)
        character(len=*), intent(in) :: value, choices
        integer :: start, comma

        in_choices = .false.
        start = 1
        do
            comma = index(choices(start:), ',')
            if (comma == 0) then
                in_choices = trim(value) == trim(choices(start:))
                return
            end if
            if (trim(value) == choices(start:start + comma - 2)) then
                in_choices = .true.
                return
            end if
            start = start + comma
        end do
    end function in_choices

    function choice_phrase(choices) result(text)
        !! "a or b" for two choices, "a, b, or c" for more.
        character(len=*), intent(in) :: choices
        character(len=:), allocatable :: text
        integer :: i, last

        text = ''
        do i = 1, len_trim(choices)
            if (choices(i:i) == ',') then
                text = text//', '
            else
                text = text//choices(i:i)
            end if
        end do
        last = index(text, ', ', back=.true.)
        if (count([(choices(i:i) == ',', i=1, len_trim(choices))]) == 1) then
            text = text(:last - 1)//' or '//text(last + 2:)
        else
            text = text(:last + 1)//'or '//text(last + 2:)
        end if
    end function choice_phrase

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
