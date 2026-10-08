module fo_cmake_context
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: iso_c_binding, only: c_long_long
    use fo_fs, only: fs_make_dir, fs_collect_files, fs_write_text, fs_identity
    use fx_json_parse, only: json_parser_t, json_event_t, &
        json_parser_init_strict, json_parser_next, JSON_OBJECT_START, &
        JSON_OBJECT_END, JSON_ARRAY_START, JSON_ARRAY_END, JSON_KEY, &
        JSON_STRING, JSON_BOOL, JSON_ERROR, JSON_END_OF_INPUT
    implicit none
    private

    type, public :: cmake_context_t
        character(:), allocatable :: source_root
        character(:), allocatable :: build_root
        character(:), allocatable :: generator
        character(:), allocatable :: configuration
        character(:), allocatable :: profile
        character(:), allocatable :: configure_preset
        character(:), allocatable :: build_preset
        character(:), allocatable :: test_preset
        character(:), allocatable :: request_token
        character(:), allocatable :: error
        character(:), allocatable :: extra_args(:)
        character(:), allocatable :: build_targets(:)
        character(:), allocatable :: reported_generator
        logical :: multi_config = .false.
        logical :: has_codemodel = .false.
        logical :: has_cache = .false.
        logical :: has_toolchains = .false.
        ! With a configure preset, this names the File API/cache location only.
        ! CMake still chooses its own binaryDir from the preset.
        logical :: build_root_hint = .false.
        logical :: request_seen = .false.
        logical :: valid = .true.
    end type cmake_context_t

    public :: cmake_context_init, cmake_context_query, cmake_context_read_reply
    public :: cmake_context_build_path, cmake_context_validate_hint

contains

    subroutine cmake_context_init(context, source_root, build_root, generator, &
            configuration, configure_preset, build_preset, extra_args, &
            test_preset, argv, build_targets)
        type(cmake_context_t), intent(out) :: context
        character(len=*), intent(in) :: source_root
        character(len=*), intent(in), optional :: build_root, generator
        character(len=*), intent(in), optional :: configuration
        character(len=*), intent(in), optional :: configure_preset, build_preset
        character(len=*), intent(in), optional :: extra_args
        character(len=*), intent(in), optional :: test_preset, argv(:), build_targets
        character(:), allocatable :: raw_args, target_error
        integer :: i, env_length, env_status
        integer(int64) :: clock_tick
        character(len=40) :: token_text

        context%profile = ''
        context%source_root = trim(source_root)
        context%build_root = 'build'
        context%build_root_hint = present(build_root)
        if (present(build_root)) context%build_root = trim(build_root)
        if (.not. present(build_root)) then
            call get_environment_variable('FO_CMAKE_BUILD_DIR', &
                length=env_length, status=env_status)
            context%build_root_hint = env_status == 0 .and. env_length > 0
            call env_value('FO_CMAKE_BUILD_DIR', context%build_root)
        end if
        context%generator = ''
        if (present(generator)) context%generator = trim(generator)
        if (.not. present(generator)) call env_value('FO_CMAKE_GENERATOR', &
            context%generator)
        context%configuration = ''
        if (present(configuration)) context%configuration = trim(configuration)
        if (.not. present(configuration)) call env_value('FO_CMAKE_CONFIG', &
            context%configuration)
        context%configure_preset = ''
        if (present(configure_preset)) &
            context%configure_preset = trim(configure_preset)
        if (.not. present(configure_preset)) call env_value( &
            'FO_CMAKE_CONFIGURE_PRESET', context%configure_preset)
        context%build_preset = ''
        if (present(build_preset)) context%build_preset = trim(build_preset)
        if (.not. present(build_preset)) &
            call env_value('FO_CMAKE_BUILD_PRESET', context%build_preset)
        context%test_preset = ''
        if (present(test_preset)) context%test_preset = trim(test_preset)
        if (.not. present(test_preset)) call env_value( &
            'FO_CMAKE_TEST_PRESET', context%test_preset)
        context%reported_generator = ''
        call system_clock(count=clock_tick)
        write (token_text, '(i0)') clock_tick
        context%request_token = trim(token_text)

        raw_args = ''
        if (present(extra_args)) raw_args = extra_args
        if (.not. present(extra_args)) call env_value('FO_CMAKE_ARGS', raw_args)
        call parse_argv(raw_args, context%extra_args, context%error)
        if (present(argv)) then
            do i = 1, size(argv)
                call append_arg(context%extra_args, trim(argv(i)))
            end do
        end if
        raw_args = ''
        if (present(build_targets)) then
            raw_args = build_targets
        else
            call env_value('FO_CMAKE_BUILD_TARGETS', raw_args)
        end if
        call parse_argv(raw_args, context%build_targets, target_error)
        if (allocated(target_error)) then
            context%error = 'FO_CMAKE_BUILD_TARGETS has an unterminated escape or quote'
        end if
        context%valid = .not. allocated(context%error)
        if (len_trim(context%source_root) == 0 .or. &
                len_trim(context%build_root) == 0) then
            context%valid = .false.
            context%error = 'CMake source and build directories must be non-empty'
        end if
        if (len_trim(context%configure_preset) > 0 .and. &
                len_trim(context%generator) > 0) then
            context%valid = .false.
            context%error = 'FO_CMAKE_GENERATOR cannot override a configure preset'
        end if
    end subroutine cmake_context_init

    subroutine cmake_context_query(context)
        type(cmake_context_t), intent(in) :: context
        character(:), allocatable :: query_dir

        query_dir = cmake_context_build_path(context)// &
            '/.cmake/api/v1/query/client-fo'
        call fs_make_dir(query_dir)
        call create_empty_file(query_dir//'/codemodel-v2')
        call create_empty_file(query_dir//'/cache-v2')
        call create_empty_file(query_dir//'/toolchains-v1')
        call create_empty_file(query_dir//'/cmakeFiles-v1')
        call fs_write_text(query_dir//'/query.json', &
            '{"requests":[{"kind":"codemodel","version":2},'// &
            '{"kind":"cache","version":2},'// &
            '{"kind":"toolchains","version":1}],"client":'// &
            '{"fo_request":"'//context%request_token//'"}}')
    end subroutine cmake_context_query

    subroutine create_empty_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios

        open (newunit=unit, file=path, status='replace', action='write', &
            iostat=ios)
        if (ios == 0) close (unit)
    end subroutine create_empty_file

    function cmake_context_build_path(context) result(path)
        type(cmake_context_t), intent(in) :: context
        character(:), allocatable :: path

        if (context%build_root(1:1) == '/') then
            path = context%build_root
        else
            path = trim(context%source_root)//'/'//trim(context%build_root)
        end if
    end function cmake_context_build_path

    subroutine append_arg(values, token)
        character(len=:), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: token
        character(len=:), allocatable :: grown(:)
        integer :: old_count, old_len, new_len

        if (len(token) == 0) return
        old_count = 0
        old_len = 0
        if (allocated(values)) then
            old_count = size(values)
            old_len = len(values)
        end if
        new_len = max(old_len, len(token))
        allocate (character(len=new_len) :: grown(old_count + 1))
        if (old_count > 0) grown(1:old_count) = values
        grown(old_count + 1) = token
        call move_alloc(grown, values)
    end subroutine append_arg

    subroutine cmake_context_read_reply(context)
        type(cmake_context_t), intent(inout) :: context
        character(len=4096) :: paths(64)
        character(:), allocatable :: input
        integer :: n_paths

        context%reported_generator = ''
        context%multi_config = .false.
        context%has_codemodel = .false.
        context%has_cache = .false.
        context%has_toolchains = .false.
        context%request_seen = .false.
        call fs_collect_files(cmake_context_build_path(context)// &
            '/.cmake/api/v1/reply', 'index-', '.json', &
            '', paths, n_paths, recursive=.false.)
        if (n_paths < 1) return
        call read_file(trim(paths(n_paths)), input)
        if (allocated(input)) call parse_reply(context, input)
    end subroutine cmake_context_read_reply

    subroutine parse_reply(context, input)
        type(cmake_context_t), intent(inout) :: context
        character(len=*), intent(in) :: input
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        character(:), allocatable :: key
        character(len=32) :: path(8)
        integer :: depth, generator_depth, objects_depth

        call json_parser_init_strict(parser, input)
        depth = 0
        generator_depth = -1
        objects_depth = -1
        key = ''
        path = ''
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_END_OF_INPUT) exit
            if (event%event_type == JSON_ERROR) return
            select case (event%event_type)
            case (JSON_KEY)
                key = event%string_val
            case (JSON_OBJECT_START)
                depth = depth + 1
                if (depth <= size(path)) path(depth) = key
                if (key == 'generator') generator_depth = depth
                key = ''
            case (JSON_ARRAY_START)
                depth = depth + 1
                if (depth <= size(path)) path(depth) = key
                if (key == 'objects') objects_depth = depth
                key = ''
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                if (depth == generator_depth) generator_depth = -1
                if (depth == objects_depth) objects_depth = -1
                if (depth > 0 .and. depth <= size(path)) path(depth) = ''
                depth = depth - 1
                key = ''
            case (JSON_STRING)
                if (depth == generator_depth) then
                    if (key == 'name') context%reported_generator = event%string_val
                    if (objects_depth > 0 .and. key == 'kind') then
                        select case (event%string_val)
                        case ('codemodel')
                            context%has_codemodel = .true.
                        case ('cache')
                            context%has_cache = .true.
                        case ('toolchains')
                            context%has_toolchains = .true.
                        end select
                    end if
                else if (objects_depth > 0 .and. key == 'kind') then
                    select case (event%string_val)
                    case ('codemodel')
                        context%has_codemodel = .true.
                    case ('cache')
                        context%has_cache = .true.
                    case ('toolchains')
                        context%has_toolchains = .true.
                    end select
                end if
                if (depth == 5 .and. key == 'fo_request' .and. &
                        trim(path(2)) == 'reply' .and. &
                        trim(path(3)) == 'client-fo' .and. &
                        trim(path(4)) == 'query.json' .and. &
                        trim(path(5)) == 'client' .and. &
                        event%string_val == context%request_token) then
                    context%request_seen = .true.
                end if
                key = ''
            case (JSON_BOOL)
                if (depth == generator_depth .and. key == 'multiConfig') &
                    context%multi_config = event%bool_val
                key = ''
            case default
                key = ''
            end select
        end do
    end subroutine parse_reply

    subroutine cmake_context_validate_hint(context, valid)
        type(cmake_context_t), intent(inout) :: context
        logical, intent(out) :: valid
        character(len=4096) :: line, cache_home, cache_generator
        character(:), allocatable :: cache_file
        integer :: unit, ios
        integer(c_long_long) :: home_device, home_inode, source_device, source_inode
        logical :: home_exists, source_exists

        valid = .false.
        if (allocated(context%error)) deallocate (context%error)
        cache_home = ''
        cache_generator = ''
        cache_file = cmake_context_build_path(context)//'/CMakeCache.txt'
        open (newunit=unit, file=cache_file, status='old', action='read', &
            iostat=ios)
        if (ios /= 0) then
            context%error = 'preset build-root hint has no CMakeCache.txt'
            return
        end if
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'CMAKE_HOME_DIRECTORY:INTERNAL=') == 1) &
                cache_home = line(len('CMAKE_HOME_DIRECTORY:INTERNAL=') + 1:)
            if (index(line, 'CMAKE_GENERATOR:INTERNAL=') == 1) &
                cache_generator = line(len('CMAKE_GENERATOR:INTERNAL=') + 1:)
        end do
        close (unit)

        call fs_identity(trim(cache_home), home_device, home_inode, home_exists)
        call fs_identity(context%source_root, source_device, source_inode, &
            source_exists)
        if (.not. home_exists .or. .not. source_exists .or. &
                home_device /= source_device .or. home_inode /= source_inode) then
            context%error = 'preset build-root hint points at a different source'
            return
        end if
        if (.not. context%request_seen) then
            context%error = 'preset File API reply does not match its request '// &
                'token'
            return
        end if
        if (len_trim(cache_generator) == 0 .or. &
                trim(cache_generator) /= trim(context%reported_generator)) then
            context%error = 'preset build-root hint generator differs from File API'
            return
        end if
        valid = .true.
    end subroutine cmake_context_validate_hint

    subroutine read_file(path, text)
        character(len=*), intent(in) :: path
        character(:), allocatable, intent(out) :: text
        integer :: unit, ios, file_size

        inquire (file=path, size=file_size, iostat=ios)
        if (ios /= 0 .or. file_size < 1) return
        allocate (character(len=file_size) :: text)
        open (newunit=unit, file=path, status='old', action='read', &
            access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            deallocate (text)
            return
        end if
        read (unit, iostat=ios) text
        close (unit)
        if (ios /= 0) deallocate (text)
    end subroutine read_file

    subroutine env_value(name, value)
        character(len=*), intent(in) :: name
        character(:), allocatable, intent(inout) :: value
        integer :: n, status

        call get_environment_variable(name, length=n, status=status)
        if (status /= 0 .or. n < 1) return
        deallocate (value)
        allocate (character(len=n) :: value)
        call get_environment_variable(name, value, status=status)
        if (status /= 0) value = ''
    end subroutine env_value

    subroutine parse_argv(text, values, error)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: values(:)
        character(len=:), allocatable, intent(out) :: error
        character(:), allocatable :: token
        character(len=1) :: quote, ch
        integer :: i, n, count, max_len
        logical :: escaped

        count = 0
        max_len = 1
        quote = achar(0)
        escaped = .false.
        token = ''
        n = len_trim(text)
        do i = 1, n
            ch = text(i:i)
            if (escaped) then
                token = token//ch
                escaped = .false.
            else if (ch == achar(92) .and. quote /= "'") then
                escaped = .true.
            else if (quote /= achar(0)) then
                if (ch == quote) then
                    quote = achar(0)
                else
                    token = token//ch
                end if
            else if (ch == "'" .or. ch == '"') then
                quote = ch
            else if (ch == ' ' .or. ch == achar(9)) then
                if (len(token) > 0) then
                    count = count + 1
                    max_len = max(max_len, len(token))
                    token = ''
                end if
            else
                token = token//ch
            end if
        end do
        if (escaped .or. quote /= achar(0)) then
            error = 'FO_CMAKE_ARGS has an unterminated escape or quote'
            allocate (character(len=1) :: values(0))
            return
        end if
        if (len(token) > 0) then
            count = count + 1
            max_len = max(max_len, len(token))
        end if
        allocate (character(len=max_len) :: values(count))
        count = 0
        quote = achar(0)
        escaped = .false.
        token = ''
        do i = 1, n
            ch = text(i:i)
            if (escaped) then
                token = token//ch
                escaped = .false.
            else if (ch == achar(92) .and. quote /= "'") then
                escaped = .true.
            else if (quote /= achar(0)) then
                if (ch == quote) then
                    quote = achar(0)
                else
                    token = token//ch
                end if
            else if (ch == "'" .or. ch == '"') then
                quote = ch
            else if (ch == ' ' .or. ch == achar(9)) then
                if (len(token) > 0) then
                    count = count + 1
                    values(count) = token
                    token = ''
                end if
            else
                token = token//ch
            end if
        end do
        if (len(token) > 0) then
            count = count + 1
            values(count) = token
        end if
    end subroutine parse_argv

end module fo_cmake_context
