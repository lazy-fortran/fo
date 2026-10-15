module fo_json_object
    !! One strict top-level JSON member lookup shared by the MCP server and
    !! the Gremlin request/status readers, plus the control-character test.
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
        json_parser_next, JSON_OBJECT_START, JSON_OBJECT_END, JSON_ARRAY_START, &
        JSON_ARRAY_END, JSON_KEY, JSON_ERROR, JSON_END_OF_INPUT
    use fx_json_build, only: json_builder_t
    use fx_string, only: builder_append
    implicit none
    private
    public :: json_object_member, json_text_has_control, json_skip_value
    public :: json_value_raw

contains

    subroutine json_object_member(object_json, property, raw_value, count, ierr, &
            selected_event, last)
        !! Look up a top-level member of a complete JSON object. `count` is the
        !! number of top-level occurrences, so a caller rejects count > 1
        !! (duplicate key) unless it asks for the last occurrence with `last`.
        !! ierr /= 0 for anything that is not exactly one well-formed object.
        character(len=*), intent(in) :: object_json, property
        character(len=:), allocatable, intent(out) :: raw_value
        integer, intent(out) :: count, ierr
        type(json_event_t), intent(out), optional :: selected_event
        logical, intent(in), optional :: last

        type(json_parser_t) :: parser
        type(json_event_t) :: event, value_event
        integer :: value_start, value_end, value_error
        logical :: matches, take_last

        take_last = .false.
        if (present(last)) take_last = last
        raw_value = ''
        count = 0
        ierr = 1
        call json_parser_init_strict(parser, object_json)
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_OBJECT_START) return
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_OBJECT_END) exit
            if (event%event_type /= JSON_KEY .or. .not. allocated(event%string_val)) return
            matches = len(event%string_val) == len(property)
            if (matches) matches = event%string_val == property
            call json_parser_next(parser, value_event)
            if (value_event%event_type == JSON_ERROR .or. &
                value_event%event_type == JSON_END_OF_INPUT) return
            value_start = value_event%raw_start
            value_end = value_event%raw_end
            if (value_event%event_type == JSON_ARRAY_START .or. &
                value_event%event_type == JSON_OBJECT_START) then
                call json_skip_value(parser, value_event, value_error)
                if (value_error /= 0) return
                value_end = parser%pos - 1
            end if
            if (matches) then
                count = count + 1
                if (count == 1 .or. take_last) then
                    if (value_start < 1 .or. value_end < value_start) return
                    raw_value = object_json(value_start:value_end)
                    if (present(selected_event)) selected_event = value_event
                end if
            end if
        end do
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_END_OF_INPUT) return
        ierr = 0
    end subroutine json_object_member

    subroutine json_skip_value(parser, first, ierr)
        !! Consume the remaining events of an array or object started by `first`.
        type(json_parser_t), intent(inout) :: parser
        type(json_event_t), intent(in) :: first
        integer, intent(out) :: ierr

        type(json_event_t) :: event
        integer :: depth

        ierr = 0
        if (first%event_type /= JSON_ARRAY_START .and. &
            first%event_type /= JSON_OBJECT_START) return
        depth = 1
        do while (depth > 0)
            call json_parser_next(parser, event)
            select case (event%event_type)
            case (JSON_ARRAY_START, JSON_OBJECT_START)
                depth = depth + 1
            case (JSON_ARRAY_END, JSON_OBJECT_END)
                depth = depth - 1
            case (JSON_END_OF_INPUT, JSON_ERROR)
                ierr = 1
                return
            end select
        end do
    end subroutine json_skip_value

    subroutine json_value_raw(jb, raw)
        !! Append already valid JSON text (an integer literal, a nested
        !! document) as the next value of the builder.
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: raw

        if (jb%depth > 0) then
            if (jb%needs_comma_stack(jb%depth) .and. &
                .not. jb%in_object_stack(jb%depth)) call builder_append(jb%buf, ',')
            jb%needs_comma_stack(jb%depth) = .true.
        end if
        call builder_append(jb%buf, raw)
    end subroutine json_value_raw

    logical function json_text_has_control(text)
        character(len=*), intent(in) :: text
        integer :: i

        json_text_has_control = .false.
        do i = 1, len(text)
            if (iachar(text(i:i)) < 32 .or. iachar(text(i:i)) == 127) then
                json_text_has_control = .true.
                return
            end if
        end do
    end function json_text_has_control

end module fo_json_object
