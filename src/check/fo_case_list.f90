module fo_case_list
    !! The case list `fo test --cases` runs (#363): the object printed by
    !! `fo test --list --json`, of which only `cases[].name` and the optional
    !! `cases[].closure_digest` are read. A digest given in the list is the
    !! one the caller planned with; a case whose digest differs at run time
    !! is not run.
    use fx_string, only: string_t
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
        json_parser_next, JSON_ARRAY_START, JSON_ARRAY_END, JSON_OBJECT_START
    use fo_json_object, only: json_object_member, json_skip_value
    implicit none
    private

    public :: case_list_parse, case_list_read

contains

    subroutine case_list_read(path, text, ierr, message)
        !! The list text of a file, or of standard input for '-'.
        use iso_fortran_env, only: input_unit
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text, message
        integer, intent(out) :: ierr
        character(len=4096) :: line
        integer :: unit, ios, size_read

        text = ''
        message = ''
        if (path == '-') then
            unit = input_unit
        else
            open (newunit=unit, file=path, status='old', action='read', iostat=ierr)
            if (ierr /= 0) then
                message = 'cannot read case list '//path
                return
            end if
        end if
        do
            read (unit, '(a)', advance='no', iostat=ios, size=size_read) line
            if (is_iostat_end(ios)) exit
            text = text//line(1:size_read)
            if (is_iostat_eor(ios)) then
                text = text//new_line('a')
            else if (ios /= 0) then
                exit
            end if
        end do
        if (path /= '-') close (unit)
        ierr = 0
    end subroutine case_list_read

    subroutine case_list_parse(text, names, digests, ierr, message)
        !! names(k) and digests(k) ('' when absent) of every listed case.
        character(len=*), intent(in) :: text
        type(string_t), allocatable, intent(out) :: names(:), digests(:)
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: cases, element, value
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        integer :: count, start, skip_error

        allocate (names(0), digests(0))
        message = 'case list is not an object with a "cases" array'
        call json_object_member(text, 'cases', cases, count, ierr)
        if (ierr /= 0 .or. count /= 1) then
            ierr = 1
            return
        end if
        ierr = 1
        call json_parser_init_strict(parser, cases)
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_ARRAY_START) return
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ARRAY_END) exit
            if (event%event_type /= JSON_OBJECT_START) then
                message = 'every entry of "cases" must be an object'
                return
            end if
            start = event%raw_start
            call json_skip_value(parser, event, skip_error)
            if (skip_error /= 0) return
            element = cases(start:parser%pos - 1)
            call json_object_member(element, 'name', value, count, ierr)
            if (ierr /= 0 .or. count /= 1 .or. len(value) < 3) then
                ierr = 1
                message = 'every case needs one "name" string'
                return
            end if
            call append(names, unquote(value))
            call json_object_member(element, 'closure_digest', value, count, ierr)
            if (ierr /= 0 .or. count > 1) then
                ierr = 1
                return
            end if
            if (count == 1) then
                call append(digests, unquote(value))
            else
                call append(digests, '')
            end if
        end do
        ierr = 0
        message = ''
    end subroutine case_list_parse

    subroutine append(list, text)
        type(string_t), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: text
        type(string_t), allocatable :: grown(:)
        integer :: k

        allocate (grown(size(list) + 1))
        do k = 1, size(list)
            call move_alloc(list(k)%s, grown(k)%s)
        end do
        grown(size(grown))%s = text
        call move_alloc(grown, list)
    end subroutine append

    function unquote(raw) result(text)
        !! A raw JSON string without its quotes; case names and digests
        !! carry no escapes.
        character(len=*), intent(in) :: raw
        character(len=:), allocatable :: text

        text = raw
        if (len(raw) >= 2) then
            if (raw(1:1) == '"' .and. raw(len(raw):len(raw)) == '"') text = raw(2:len(raw) - 1)
        end if
    end function unquote

end module fo_case_list
