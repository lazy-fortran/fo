module fo_hook_presets
    use fo_fs, only: fs_canonical_path
    use fo_util, only: read_text_file_alloc
    use fx_path, only: path_dirname
    use fx_string, only: string_t, replace_str
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
        json_parser_next, JSON_OBJECT_START, JSON_OBJECT_END, JSON_ARRAY_START, &
        JSON_ARRAY_END, JSON_KEY, JSON_STRING, JSON_ERROR, JSON_END_OF_INPUT
    implicit none
    private
    public :: hook_stage_presets
contains
    subroutine hook_stage_presets(snapshot, status, message)
        character(len=*), intent(in) :: snapshot
        integer, intent(out) :: status
        character(:), allocatable, intent(out) :: message
        type(string_t), allocatable :: seen(:)

        allocate(seen(0))
        status = 0
        message = ''
        call adapt(snapshot//'/CMakePresets.json', 0)
        if (status == 0) call adapt(snapshot//'/CMakeUserPresets.json', 0)
    contains
        recursive subroutine adapt(filename, level)
            character(len=*), intent(in) :: filename
            integer, intent(in) :: level
            type(json_parser_t) :: parser
            type(json_event_t) :: event
            type(string_t), allocatable :: includes(:)
            character(:), allocatable :: path, text, output, key, section, child
            integer :: depth, cursor, i, unit, ios
            logical :: exists, binary_dir, members

            if (status /= 0) return
            inquire(file=filename, exist=exists)
            if (.not. exists .and. level == 0) return
            path = fs_canonical_path(filename)
            ! Preset includes must never turn a private edit into a checkout edit.
            if (index(path, snapshot//'/') /= 1 .or. level > 32) then
                status = 1
                message = 'staged CMake preset include is outside the private view: '//filename
                return
            end if
            do i = 1, size(seen)
                if (seen(i)%s == path) return
            end do
            call append_path(seen, path)
            call read_text_file_alloc(path, text, status)
            if (status /= 0) then
                message = 'cannot read staged CMake preset: '//path
                return
            end if
            allocate(includes(0))
            call json_parser_init_strict(parser, text)
            output = ''
            key = ''
            section = ''
            depth = 0
            cursor = 1
            binary_dir = .false.
            members = .false.
            do
                call json_parser_next(parser, event)
                select case (event%event_type)
                case (JSON_ERROR)
                    status = 1
                    message = 'invalid staged CMake preset JSON: '//path
                    return
                case (JSON_END_OF_INPUT)
                    exit
                case (JSON_KEY)
                    key = event%string_val
                    if (depth == 1) section = key
                    if (depth == 3 .and. section == 'configurePresets') members = .true.
                case (JSON_OBJECT_START, JSON_ARRAY_START)
                    depth = depth + 1
                    if (depth == 3 .and. section == 'configurePresets') then
                        binary_dir = .false.
                        members = .false.
                    end if
                case (JSON_OBJECT_END, JSON_ARRAY_END)
                    if (event%event_type == JSON_OBJECT_END .and. depth == 3 .and. &
                        section == 'configurePresets' .and. .not. binary_dir) then
                        output = output//text(cursor:event%raw_start - 1)
                        if (members) output = output//','
                        output = output//'"binaryDir":"${sourceDir}/build"'
                        cursor = event%raw_start
                    end if
                    depth = depth - 1
                case (JSON_STRING)
                    if (depth == 3 .and. section == 'configurePresets' .and. &
                        key == 'binaryDir') then
                        output = output//text(cursor:event%raw_start - 1)// &
                            '"${sourceDir}/build"'
                        cursor = event%raw_end + 1
                        binary_dir = .true.
                    else if (depth == 2 .and. section == 'include') then
                        call append_path(includes, event%string_val)
                    end if
                end select
            end do
            do i = 1, size(includes)
                child = replace_str(includes(i)%s, '${sourceDir}', snapshot)
                child = replace_str(child, '${fileDir}', path_dirname(path))
                if (index(child, '$') > 0) then
                    status = 1
                    message = 'unsupported macro in staged CMake preset include: '//child
                    return
                end if
                if (len(child) > 0) then
                    if (child(1:1) /= '/') child = path_dirname(path)//'/'//child
                end if
                call adapt(child, level + 1)
                if (status /= 0) return
            end do
            output = output//text(cursor:)
            open(newunit=unit, file=path, status='replace', action='write', iostat=ios)
            if (ios == 0) then
                write(unit, '(a)', iostat=ios) output
                close(unit)
            end if
            status = ios
            if (status /= 0) message = 'cannot adapt staged CMake preset: '//path
        end subroutine adapt
    end subroutine hook_stage_presets
    subroutine append_path(paths, value)
        type(string_t), allocatable, intent(inout) :: paths(:)
        character(len=*), intent(in) :: value
        type(string_t), allocatable :: grown(:)
        integer :: i, n

        n = size(paths)
        allocate(grown(n + 1))
        do i = 1, n
            call move_alloc(paths(i)%s, grown(i)%s)
        end do
        grown(n + 1)%s = value
        call move_alloc(grown, paths)
    end subroutine append_path
end module fo_hook_presets
