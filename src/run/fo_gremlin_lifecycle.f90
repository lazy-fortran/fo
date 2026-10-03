module fo_gremlin_lifecycle
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_json_build, only: json_escape_string
    implicit none
    private

    integer, parameter, public :: GREMLIN_LIFECYCLE_VERSION = 1
    integer, parameter, public :: GREMLIN_LIFECYCLE_MAX_EVENT = 262144

    type, public :: gremlin_lifecycle_event_t
        integer :: version = GREMLIN_LIFECYCLE_VERSION
        character(len=4096) :: event_id = ''
        character(len=64) :: event_type = ''
        character(len=64) :: generation = ''
        character(len=16) :: phase = 'starting'
        character(len=16) :: health = 'unknown'
        character(len=16) :: verification_level = 'gate'
        logical :: promotion_ready = .false.
        character(len=:), allocatable :: json
    end type gremlin_lifecycle_event_t

    public :: gremlin_lifecycle_encode, gremlin_lifecycle_validate
    public :: gremlin_lifecycle_append, gremlin_lifecycle_read_page
    public :: gremlin_lifecycle_make_id

    integer(int64), save :: lifecycle_serial = 0_int64

    interface
        function c_lifecycle_validate(record) &
                bind(C, name='fo_c_gremlin_lifecycle_validate') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: record(*)
            integer(c_int) :: ierr
        end function c_lifecycle_validate

        function c_lifecycle_append(path, event_id, record) &
                bind(C, name='fo_c_gremlin_lifecycle_append') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*), event_id(*), record(*)
            integer(c_int) :: ierr
        end function c_lifecycle_append
    end interface

contains

    subroutine gremlin_lifecycle_encode(event, json, ierr, message)
        type(gremlin_lifecycle_event_t), intent(in) :: event
        character(len=:), allocatable, intent(out) :: json
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        ierr = 1
        message = 'invalid typed Gremlin lifecycle event'
        if (event%version /= GREMLIN_LIFECYCLE_VERSION) return
        if (len_trim(event%event_id) == 0 .or. &
            .not. valid_event_type(event%event_type)) return
        if (.not. valid_phase(event%phase) .or. .not. valid_health(event%health) .or. &
            .not. valid_verification(event%verification_level)) return
        json = '{"event_version":1,"event_id":"'// &
            trim(json_escape_string(event%event_id))//'","event_type":"'// &
            trim(json_escape_string(event%event_type))//'","generation":"'// &
            trim(json_escape_string(event%generation))//'","phase":"'// &
            trim(event%phase)//'","health":"'//trim(event%health)// &
            '","verification_level":"'//trim(event%verification_level)// &
            '","promotion_ready":'//trim(bool_text(event%promotion_ready))//'}'
        ierr = gremlin_lifecycle_validate(json)
        if (ierr == 0) then
            message = ''
        else
            message = 'lifecycle event failed schema validation'
        end if
    end subroutine gremlin_lifecycle_encode

    integer function gremlin_lifecycle_validate(json)
        character(len=*), intent(in) :: json
        type(gremlin_lifecycle_event_t) :: event

        if (len_trim(json) == 0 .or. len_trim(json) > GREMLIN_LIFECYCLE_MAX_EVENT) then
            gremlin_lifecycle_validate = 1
            return
        end if
        gremlin_lifecycle_validate = int(c_lifecycle_validate(trim(json)//c_null_char))
        if (gremlin_lifecycle_validate /= 0) return
        call read_event_fields(json, event)
        if (.not. valid_event_type(event%event_type) .or. &
            .not. valid_phase(event%phase) .or. .not. valid_health(event%health) .or. &
            .not. valid_verification(event%verification_level)) then
            gremlin_lifecycle_validate = 1
        end if
    end function gremlin_lifecycle_validate

    subroutine gremlin_lifecycle_append(path, event, ierr, message)
        character(len=*), intent(in) :: path
        type(gremlin_lifecycle_event_t), intent(in) :: event
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=:), allocatable :: json
        integer(c_int) :: c_error

        call gremlin_lifecycle_encode(event, json, ierr, message)
        if (ierr /= 0) return
        c_error = c_lifecycle_append(trim(path)//c_null_char, &
            trim(event%event_id)//c_null_char, trim(json)//c_null_char)
        ierr = int(c_error)
        if (ierr == 0) then
            message = ''
        else if (ierr == 3) then
            message = 'conflicting lifecycle event ID already exists'
        else
            message = 'cannot durably append typed lifecycle event'
        end if
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

        character(len=GREMLIN_LIFECYCLE_MAX_EVENT) :: line
        integer(int64) :: position, line_bytes, file_bytes
        integer :: unit, ios, capacity
        logical :: exists

        ierr = 0
        message = ''
        n_events = 0
        next_cursor = cursor
        has_more = .false.
        file_bytes = 0_int64
        capacity = max(1, max_records)
        allocate(events(capacity))
        inquire(file=trim(path), exist=exists, size=file_bytes, iostat=ios)
        if (ios /= 0) then
            ierr = 2
            message = 'cannot inspect lifecycle journal'
            return
        end if
        if (.not. exists) return
        open(newunit=unit, file=trim(path), status='old', action='read', &
            form='formatted', access='sequential', iostat=ios)
        if (ios /= 0) then
            ierr = 2
            message = 'cannot open lifecycle journal'
            return
        end if
        position = 0_int64
        do
            line = ''
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            line_bytes = int(len_trim(line) + 1, int64)
            if (position + line_bytes > file_bytes) exit
            if (gremlin_lifecycle_validate(trim(line)) /= 0) then
                ierr = 1
                message = 'lifecycle journal contains an invalid event envelope'
                exit
            end if
            position = position + line_bytes
            if (position <= cursor) cycle
            if (position - line_bytes < cursor) then
                ierr = 1
                message = 'lifecycle event cursor falls inside an event'
                exit
            end if
            if (n_events >= max_records .or. &
                (n_events > 0 .and. position - cursor > max_bytes)) then
                has_more = .true.
                position = position - line_bytes
                exit
            end if
            n_events = n_events + 1
            events(n_events)%json = trim(line)
            events(n_events)%version = GREMLIN_LIFECYCLE_VERSION
            call read_event_fields(trim(line), events(n_events))
            next_cursor = position
        end do
        if (ios < 0 .and. ios /= -1) then
            ierr = 2
            message = 'cannot read lifecycle journal'
        end if
        close(unit)
        if (ierr == 0 .and. position < file_bytes) has_more = .true.
        if (ierr /= 0) n_events = 0
    end subroutine gremlin_lifecycle_read_page

    subroutine read_event_fields(json, event)
        character(len=*), intent(in) :: json
        type(gremlin_lifecycle_event_t), intent(inout) :: event
        integer :: id_start, type_start, phase_start, health_start, level_start, gen_start

        id_start = index(json, '"event_id":"') + 12
        type_start = index(json, '"event_type":"') + 14
        phase_start = index(json, '"phase":"') + 9
        health_start = index(json, '"health":"') + 10
        level_start = index(json, '"verification_level":"') + 22
        gen_start = index(json, '"generation":"') + 14
        if (id_start > 12) event%event_id = value_after(json, id_start)
        if (type_start > 14) event%event_type = value_after(json, type_start)
        if (phase_start > 9) event%phase = value_after(json, phase_start)
        if (health_start > 10) event%health = value_after(json, health_start)
        if (level_start > 22) event%verification_level = value_after(json, level_start)
        if (gen_start > 14) event%generation = value_after(json, gen_start)
        event%promotion_ready = index(json, '"promotion_ready":true') > 0
    end subroutine read_event_fields

    function value_after(json, first) result(value)
        character(len=*), intent(in) :: json
        integer, intent(in) :: first
        character(len=4096) :: value
        integer :: last

        value = ''
        last = index(json(first:), '"')
        if (last > 0) value = json(first:first + last - 2)
    end function value_after

    function gremlin_lifecycle_make_id(session_id, sequence) result(event_id)
        character(len=*), intent(in) :: session_id
        integer(int64), intent(in) :: sequence
        character(len=4096) :: event_id
        character(len=32) :: number, serial

        write(number, '(i0)') sequence
        lifecycle_serial = lifecycle_serial + 1_int64
        write(serial, '(i0)') lifecycle_serial
        event_id = trim(session_id)//':event:'//trim(number)//':'//trim(serial)
    end function gremlin_lifecycle_make_id

    logical function valid_phase(value)
        character(len=*), intent(in) :: value
        valid_phase = value == 'starting' .or. value == 'building' .or. &
            value == 'testing' .or. value == 'quiescent' .or. &
            value == 'stopped' .or. value == 'failed'
    end function valid_phase

    logical function valid_event_type(value)
        character(len=*), intent(in) :: value
        select case (trim(value))
        case ('session_started', 'candidate_build_started', 'candidate_build_failed', &
            'generation_captured', 'generation_activated', 'case_started', &
            'verification_started', 'ordinary_verification_complete', &
            'full_verification_complete', 'verification_gate_failed', &
            'capture_failed', 'coverage_updated', 'dirty_input_detected', &
            'session_quiescent', 'session_stopped', 'session_failed', 'state_changed')
            valid_event_type = .true.
        case default
            valid_event_type = .false.
        end select
    end function valid_event_type

    logical function valid_health(value)
        character(len=*), intent(in) :: value
        valid_health = value == 'unknown' .or. value == 'healthy' .or. &
            value == 'degraded' .or. value == 'failure'
    end function valid_health

    logical function valid_verification(value)
        character(len=*), intent(in) :: value
        valid_verification = value == 'gate' .or. value == 'ordinary' .or. &
            value == 'full'
    end function valid_verification

    function bool_text(value) result(text)
        logical, intent(in) :: value
        character(len=5) :: text

        text = 'false'
        if (value) text = 'true '
    end function bool_text

end module fo_gremlin_lifecycle
