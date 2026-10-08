module fo_cmake_generation
    !! Delegated CMake generation capture. CMake and CTest own configuration
    !! and registered-test semantics; no FPM manifest or source scanner is used.
    use fo_build_backend, only: backend_t, cmake_configure
    use fo_cmake_context, only: cmake_context_build_path
    use fo_input_inventory, only: input_inventory_t, input_inventory_discover_cmake
    use fo_fs, only: fs_make_dir, fs_write_text, fs_collect_files, fs_remove_tree, &
        fs_realpath
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file
    use fo_dep_resolve, only: normalize_path
    use fo_cache, only: cache_digest, HASH_LEN
    use fo_process, only: argv_push, process_run_argv_logged
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
               json_parser_next, JSON_KEY, JSON_STRING, JSON_ERROR, JSON_END_OF_INPUT, &
                    JSON_ARRAY_START, JSON_ARRAY_END, JSON_OBJECT_START, JSON_OBJECT_END
    implicit none
    private
    integer, parameter :: PATH_LEN = 4096, MAX_ROOTS = 60
    public :: cmake_capture_inventory, cmake_registered_names, cmake_capture_cleanup
contains
    subroutine cmake_capture_inventory(backend, inventory, ierr, message)
        type(backend_t), intent(inout) :: backend
        type(input_inventory_t), intent(out) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: roots(MAX_ROOTS), bundles(MAX_ROOTS)
        character(len=256) :: labels(MAX_ROOTS), names(MAX_ROOTS)
        character(len=PATH_LEN) :: log_file, metadata_dir, normalized
        character(len=HASH_LEN) :: digest
        character(len=:), allocatable :: cache_text
        character(:), allocatable :: line, key, kind, value, script, rewritten
        character(:), allocatable :: digest_parts(:)
        character(:), allocatable :: generator
        integer :: n_roots, cursor, i, cleanup_status
        logical :: exists, source_key, provider, physical
        character(len=PATH_LEN) :: project_path, build_path

        ierr = 1
        message = ''
        if (.not. backend%cmake%valid) then
            message = backend%cmake%error
            return
        end if
        if (backend%cmake%build_root(1:1) == '/') then
            message = 'resident CMake requires a relocatable build directory'
            return
        end if
        call make_tmpfile('fo-cmake-capture-configure', log_file)
        call cmake_configure(backend%cmake, '', log_file, ierr)
        if (ierr /= 0) then
            message = 'CMake configuration failed during generation capture; '// &
                      'diagnostic: '//trim(log_file)
            return
        end if
        call delete_tmpfile(log_file)
        if (backend%cmake%build_root(1:1) == '/') then
            ierr = 1
            message = 'resident CMake requires a relocatable build directory'
            return
        end if
        allocate(character(len=1048576) :: cache_text)
        call read_text_file(cmake_context_build_path(backend%cmake)// &
                            '/CMakeCache.txt', cache_text)
        if (len_trim(cache_text) == 0) then
            ierr = 1
            message = 'configured CMake cache is unavailable'
            return
        end if
        call fs_realpath(backend%project_dir, project_path, physical)
        if (.not. physical) then
            ierr = 1
            message = 'CMake project root is unavailable'
            return
        end if
        call canonical_directory(cmake_context_build_path(backend%cmake), build_path)
        n_roots = 0
        roots = ''
        bundles = ''
        labels = ''
        names = ''
        generator = ''
        cursor = 1
        do while (next_cache_line(cache_text, cursor, line))
            call cache_entry(line, key, kind, value)
            if (key == 'CMAKE_GENERATOR') generator = value
            if (len(key) == 0 .or. len(value) == 0) cycle
            provider = .false.
            if (len(key) > 6) then
                if (key(len(key) - 5:) == '_BUILD') then
                    inquire(file=value//'/CMakeCache.txt', exist=provider)
                end if
            end if
            source_key = index(key, 'FETCHCONTENT_SOURCE_DIR_') == 1 .or. provider
            if (len(key) > 11) then
                if (key(len(key) - 10:) == '_SOURCE_DIR') source_key = .true.
            end if
            if (.not. source_key) cycle
            call canonical_directory(value, normalized)
            if (trim(normalized) == trim(project_path)) cycle
            inquire (file=trim(normalized)//'/CMakeLists.txt', exist=exists)
            ! Populated FetchContent sources may be consumed without a subproject.
            if (.not. exists) inquire (file=trim(normalized), exist=exists)
            if (.not. exists) cycle
            if (provider) then
                key = 'provider-'//key
            else if (index(key, 'FETCHCONTENT_SOURCE_DIR_') == 1) then
                key = key(25:)
            else
                key = key(:len(key) - 11)
            end if
            call add_source(normalized, key, n_roots, roots, bundles, labels, &
                            names, ierr, message, provider)
            if (ierr /= 0) return
        end do
        call validate_cmake_paths(backend, roots(:n_roots), ierr, message)
        if (ierr /= 0) return
        script = '# Frozen declared CMake configuration; generated by Fo.'// &
                 new_line('a')
        cursor = 1
        do while (next_cache_line(cache_text, cursor, line))
            call cache_entry(line, key, kind, value)
            if (len(key) == 0) cycle
            if (kind /= 'BOOL' .and. kind /= 'STRING' .and. kind /= 'PATH' .and. &
                kind /= 'FILEPATH') cycle
            if (key == 'CMAKE_INSTALL_PREFIX') cycle
            if (index(key, 'CMAKE_FIND_PACKAGE_REDIRECTS_DIR') == 1) cycle
            if (kind == 'PATH' .and. len(value) > 0) then
                call canonical_directory(value, normalized)
                do i = 1, n_roots
                    if (trim(normalized) /= trim(roots(i))) cycle
                    value = trim(normalized)
                    exit
                end do
            end if
            call relocate_value(value, trim(project_path), &
                trim(build_path), roots(:n_roots), &
                bundles(:n_roots), rewritten)
            script = script//'set('//key//' "'//cmake_quote(rewritten)// &
                     '" CACHE '//kind//' "captured by Fo" FORCE)'//new_line('a')
        end do
        do i = 1, n_roots
            if (index(labels(i), 'cmake-provider:') == 1) cycle
            key = upper(trim(names(i)))
            script = script//'set(FETCHCONTENT_SOURCE_DIR_'//key// &
                     ' "${CMAKE_CURRENT_LIST_DIR}/dependencies/'//trim(names(i))// &
                     '" CACHE PATH "frozen dependency" FORCE)'//new_line('a')
        end do
        ! A captured dependency closure must never redownload a different tree.
        script = script//'set(FETCHCONTENT_FULLY_DISCONNECTED ON CACHE BOOL '// &
                 '"frozen dependency closure" FORCE)'//new_line('a')
        allocate (character(len=len(script)) :: digest_parts(1))
        digest_parts(1) = script
        digest = cache_digest(digest_parts, 1)
        call make_tmpfile('fo-cmake-context-'//digest(:16), log_file)
        metadata_dir = trim(log_file)//'.dir'
        call delete_tmpfile(log_file)
        call fs_make_dir(metadata_dir)
        call capture_provider_metadata(roots(:n_roots), names(:n_roots), &
            labels(:n_roots), metadata_dir, ierr, message)
        if (ierr /= 0) then
            call fs_remove_tree(metadata_dir, cleanup_status)
            return
        end if
        call fs_write_text(trim(metadata_dir)//'/cache.cmake', script)
        call fs_write_text(trim(metadata_dir)//'/generator.txt', generator)
        call fs_write_text(trim(metadata_dir)//'/build-directory.txt', &
                           backend%cmake%build_root)
        call input_inventory_discover_cmake(backend%project_dir, roots(:n_roots), &
                   bundles(:n_roots), labels(:n_roots), trim(metadata_dir), inventory, &
                                            ierr, message)
        if (ierr == 0) call validate_cmake_paths(backend, roots(:n_roots), &
                                               ierr, message, inventory)
        if (ierr /= 0) call fs_remove_tree(trim(metadata_dir), cleanup_status)
    end subroutine cmake_capture_inventory

    subroutine capture_provider_metadata(roots, names, labels, metadata, ierr, message)
        character(len=*), intent(in) :: roots(:), names(:), labels(:), metadata
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=1048576), allocatable :: cache
        character(len=PATH_LEN) :: log_file
        character(:), allocatable :: line, key, kind, value, source, frozen, packed
        integer :: i, cursor, n_args

        allocate(cache)
        ierr = 0
        message = ''
        do i = 1, size(roots)
            if (index(labels(i), 'cmake-provider:') /= 1) cycle
            call read_text_file(trim(roots(i))//'/CMakeCache.txt', cache)
            source = ''
            cursor = 1
            do while (next_cache_line(cache, cursor, line))
                call cache_entry(line, key, kind, value)
                if (key == 'CMAKE_HOME_DIRECTORY') source = value
            end do
            if (len(source) == 0) then
                ierr = 1
                message = 'declared CMake build provider has no source provenance'
                return
            end if
            call fs_make_dir(trim(metadata)//'/provider-git')
            call make_tmpfile('fo-cmake-provider-git', log_file)
            n_args = 0
            call argv_push(packed, n_args, 'git')
            call argv_push(packed, n_args, 'clone')
            call argv_push(packed, n_args, '--bare')
            call argv_push(packed, n_args, '--depth=1')
            call argv_push(packed, n_args, 'file://'//source)
            call argv_push(packed, n_args, trim(metadata)//'/provider-git/'//trim(names(i)))
            call process_run_argv_logged('.', packed, n_args, log_file, .true., 60, ierr)
            if (ierr /= 0) then
                message = 'cannot freeze Git source provenance for CMake provider; '// &
                    'diagnostic: '//trim(log_file)
                return
            end if
            call delete_tmpfile(log_file)
            frozen = ''
            cursor = 1
            do while (next_cache_line(cache, cursor, line))
                call cache_entry(line, key, kind, value)
                if (key == 'CMAKE_HOME_DIRECTORY') then
                    line = 'CMAKE_HOME_DIRECTORY:INTERNAL=.fo-cmake/provider-git/'// &
                        trim(names(i))
                end if
                frozen = frozen//line//new_line('a')
            end do
            call fs_make_dir(trim(metadata)//'/providers/'//trim(names(i)))
            call fs_write_text(trim(metadata)//'/providers/'//trim(names(i))// &
                '/CMakeCache.txt', frozen)
        end do
    end subroutine capture_provider_metadata

    subroutine cmake_capture_cleanup(inventory, ierr)
        type(input_inventory_t), intent(in) :: inventory
        integer, intent(out) :: ierr
        integer :: i
        ierr = 0
        do i = 1, inventory%root_count
            if (trim(inventory%roots(i)%canonical_alias) /= 'cmake-context') cycle
            call fs_remove_tree(trim(inventory%roots(i)%physical_path), ierr)
            return
        end do
    end subroutine cmake_capture_cleanup

    logical function next_cache_line(text, cursor, line) result(found)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: cursor
        character(:), allocatable, intent(out) :: line
        integer :: ending, n
        n = len_trim(text)
        found = cursor <= n
        line = ''
        if (.not. found) return
        ending = index(text(cursor:n), new_line('a'))
        if (ending == 0) then
            line = text(cursor:n)
            cursor = n + 1
        else
            line = text(cursor:cursor + ending - 2)
            cursor = cursor + ending
        end if
    end function next_cache_line

    subroutine cache_entry(line, key, kind, value)
        character(len=*), intent(in) :: line
        character(:), allocatable, intent(out) :: key, kind, value
        integer :: colon, equals
        key = ''
        kind = ''
        value = ''
        if (len(line) == 0) return
        if (line(1:1) == '#' .or. index(line, '//') == 1) return
        colon = index(line, ':')
        equals = index(line, '=')
        if (colon < 2 .or. equals <= colon) return
        key = line(:colon - 1)
        kind = line(colon + 1:equals - 1)
        value = line(equals + 1:)
    end subroutine cache_entry

    subroutine add_source(path, name, n, roots, bundles, labels, names, ierr, &
                          message, provider)
        character(len=*), intent(in) :: path, name
        logical, intent(in) :: provider
        integer, intent(inout) :: n
        character(len=*), intent(inout) :: roots(:), bundles(:), labels(:), names(:)
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: i
        ierr = 0
        message = ''
        do i = 1, n
            if (trim(roots(i)) == trim(path)) return
        end do
        if (n == size(roots)) then
            ierr = 1
            message = 'CMake dependency closure exceeds supported root capacity'
            return
        end if
        if (verify(name, 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'// &
                   '0123456789_-') /= 0) then
            ierr = 1
            message = 'unsupported CMake dependency name: '//name
            return
        end if
        n = n + 1
        roots(n) = path
        names(n) = name
        labels(n) = 'cmake-dependency:'//name
        bundles(n) = 'project/.fo-cmake/dependencies/'//name
        if (provider) then
            labels(n) = 'cmake-provider:'//name
            bundles(n) = 'project/.fo-cmake/providers/'//name
        end if
    end subroutine add_source

    subroutine relocate_value(value, project, build, roots, bundles, relocated)
        character(len=*), intent(in) :: value, project, build, roots(:), bundles(:)
        character(:), allocatable, intent(out) :: relocated
        integer :: i
        relocated = value
        do i = 1, size(roots)
            call replace_path(relocated, trim(roots(i)), &
                              '${CMAKE_CURRENT_LIST_DIR}/'//trim(bundles(i) (18:)))
        end do
        call replace_path(relocated, trim(build), '${CMAKE_BINARY_DIR}')
        call replace_path(relocated, trim(project), '${CMAKE_CURRENT_LIST_DIR}/..')
    end subroutine relocate_value

    subroutine replace_path(value, old, replacement)
        character(:), allocatable, intent(inout) :: value
        character(len=*), intent(in) :: old, replacement
        integer :: at, start
        if (len(old) == 0) return
        start = 1
        do
            at = index(value(start:), old)
            if (at == 0) return
            at = at + start - 1
            if (at + len(old) <= len(value)) then
                select case (value(at + len(old):at + len(old)))
                case ('/', ';', ' ', '"', "'")
                case default
                    start = at + len(old)
                    cycle
                end select
            end if
            value = value(:at - 1)//replacement//value(at + len(old):)
            start = at + len(replacement)
            if (start > len(value)) return
        end do
    end subroutine replace_path

    function cmake_quote(value) result(escaped)
        character(len=*), intent(in) :: value
        character(:), allocatable :: escaped
        integer :: i
        escaped = ''
        do i = 1, len(value)
            if (value(i:i) == '"' .or. iachar(value(i:i)) == 92) &
                escaped = escaped//achar(92)
            escaped = escaped//value(i:i)
        end do
    end function cmake_quote

    function upper(value) result(converted)
        character(len=*), intent(in) :: value
        character(len=len(value)) :: converted
        integer :: i, code
        converted = value
        do i = 1, len(value)
            code = iachar(value(i:i))
            if (code >= iachar('a') .and. code <= iachar('z')) &
                converted(i:i) = achar(code - 32)
        end do
    end function upper

    subroutine validate_cmake_paths(backend, roots, ierr, message, inventory)
        type(backend_t), intent(in) :: backend
        character(len=*), intent(in) :: roots(:)
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(input_inventory_t), intent(in), optional :: inventory
        character(len=PATH_LEN), allocatable :: files(:)
        character(len=PATH_LEN) :: normalized, project, build
        character(len=:), allocatable :: text
        character(:), allocatable :: key, path
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        integer :: n_files, i, j, depth
        character(len=64) :: ancestors(64)
        logical :: captured

        allocate(files(2048))
        allocate(character(len=1048576) :: text)
        ierr = 0
        message = ''
        call canonical_directory(backend%project_dir, project)
        call canonical_directory(cmake_context_build_path(backend%cmake), build)
        call fs_collect_files(trim(build)//'/.cmake/api/v1/reply', '', '.json', &
                              '', files, n_files, recursive=.false.)
        if (n_files == 0 .or. n_files == size(files)) then
            ierr = 1
            message = 'CMake input metadata is unavailable or exceeds capacity'
            return
        end if
        do i = 1, n_files
            if (index(files(i), '/cmakeFiles-') == 0 .and. &
                index(files(i), '/target-') == 0) cycle
            call read_text_file(trim(files(i)), text)
            call json_parser_init_strict(parser, trim(text))
            key = ''
            depth = 0
            ancestors = ''
            do
                call json_parser_next(parser, event)
                if (event%event_type == JSON_END_OF_INPUT) exit
                if (event%event_type == JSON_ERROR) then
                    ierr = 1
                    message = 'invalid CMake input metadata JSON'
                    return
                end if
                select case (event%event_type)
                case (JSON_ARRAY_START, JSON_OBJECT_START)
                    depth = depth + 1
                    if (depth > size(ancestors)) then
                        ierr = 1
                        message = 'CMake input metadata exceeds supported nesting'
                        return
                    end if
                    ancestors(depth) = key
                    key = ''
                    cycle
                case (JSON_ARRAY_END, JSON_OBJECT_END)
                    if (depth > 0) depth = depth - 1
                    key = ''
                    cycle
                case (JSON_KEY)
                    key = event%string_val
                    cycle
                end select
                if (event%event_type /= JSON_STRING) cycle
                ! Target artifacts are outputs relative to the build tree.
                if (any(ancestors(:depth) == 'artifacts')) cycle
                if (key /= 'path') cycle
                path = event%string_val
                if (len(path) == 0) cycle
                if (path(1:1) /= '/') then
                    call normalize_path(trim(project)//'/'//path, normalized)
                else
                    call normalize_path(path, normalized)
                end if
                call canonical_authored_path(normalized)
                captured = inside(normalized, project) .or. &
                           inside(normalized, build)
                do j = 1, size(roots)
                    if (inside(normalized, roots(j))) captured = .true.
                end do
                ! System compiler/CMake inputs retain the delegated toolchain
                ! boundary. Arbitrary external authored inputs must be frozen.
                if (inside(normalized, '/usr') .or. &
                    inside(normalized, '/opt')) captured = .true.
                if (.not. captured) then
                    ierr = 1
                    message = 'unsupported uncaptured external CMake input: '// &
                              trim(normalized)
                    return
                end if
                if (present(inventory)) then
                    if (.not. inside(normalized, build) .and. &
                        .not. inside(normalized, '/usr') .and. &
                        .not. inside(normalized, '/opt')) then
                        if (.not. inventory_has_path(inventory, normalized)) then
                            ierr = 1
                            message = 'referenced CMake input is missing or unsupported: '// &
                                      trim(normalized)
                            return
                        end if
                    end if
                end if
                key = ''
            end do
        end do
    end subroutine validate_cmake_paths

    logical function inventory_has_path(inventory, path) result(found)
        type(input_inventory_t), intent(in) :: inventory
        character(len=*), intent(in) :: path
        integer :: i, j
        character(len=:), allocatable :: relative
        character(len=PATH_LEN) :: root
        found = .false.
        do i = 1, inventory%root_count
            call canonical_directory(inventory%roots(i)%physical_path, root)
            if (.not. inside(path, root)) cycle
            if (trim(path) == trim(root)) then
                found = .true.
                return
            end if
            relative = path(len_trim(root) + 2:)
            do j = 1, inventory%entry_count
                if (inventory%entries(j)%root_alias /= &
                    inventory%roots(i)%canonical_alias) cycle
                if (trim(inventory%entries(j)%relative_path) /= relative) cycle
                found = .true.
                return
            end do
        end do
    end function inventory_has_path

    subroutine canonical_authored_path(path)
        character(len=*), intent(inout) :: path
        character(len=PATH_LEN) :: parent
        character(len=:), allocatable :: leaf
        integer :: separator
        logical :: ok

        ! Resolve directory aliases while retaining the authored leaf: the
        ! inventory must still reject a referenced unsafe file symlink.
        separator = index(trim(path), '/', back=.true.)
        if (separator <= 1) return
        leaf = trim(path(separator + 1:))
        call fs_realpath(path(:separator - 1), parent, ok)
        if (ok) path = trim(parent)//'/'//leaf
    end subroutine canonical_authored_path

    subroutine canonical_directory(path, normalized)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: normalized
        logical :: ok

        call fs_realpath(path, normalized, ok)
        if (.not. ok) call normalize_path(path, normalized)
    end subroutine canonical_directory

    logical function inside(path, root) result(matches)
        character(len=*), intent(in) :: path, root
        integer :: n
        n = len_trim(root)
        matches = trim(path) == trim(root)
        if (len_trim(path) <= n) return
        if (path(:n) /= trim(root)) return
        matches = path(n + 1:n + 1) == '/'
    end function inside

    subroutine cmake_registered_names(backend, names, count, ierr, message)
        type(backend_t), intent(in) :: backend
        character(len=*), intent(out) :: names(:)
        integer, intent(out) :: count, ierr
        character(len=*), intent(out) :: message
        character(:), allocatable :: packed, key
        character(len=PATH_LEN) :: log_file
        character(len=:), allocatable :: text
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        integer :: n_args, depth, tests_depth, test_depth

        names = ''
        count = 0
        message = ''
        call make_tmpfile('fo-cmake-registered-tests', log_file)
        packed = ''
        n_args = 0
        call argv_push(packed, n_args, 'ctest')
        if (len_trim(backend%cmake%test_preset) > 0) then
            call argv_push(packed, n_args, '--preset')
            call argv_push(packed, n_args, backend%cmake%test_preset)
        else
            call argv_push(packed, n_args, '--test-dir')
            call argv_push(packed, n_args, &
                           cmake_context_build_path(backend%cmake))
        end if
        if (len_trim(backend%cmake%configuration) > 0) then
            call argv_push(packed, n_args, '-C')
            call argv_push(packed, n_args, backend%cmake%configuration)
        end if
        call argv_push(packed, n_args, '--show-only=json-v1')
        call process_run_argv_logged(backend%project_dir, packed, n_args, &
                                     log_file, .false., 30, ierr)
        if (ierr /= 0) then
            message = 'cannot query registered CTest inventory: '//trim(log_file)
            return
        end if
        allocate(character(len=1048576) :: text)
        call read_text_file(log_file, text)
        call delete_tmpfile(log_file)
        call json_parser_init_strict(parser, trim(text))
        depth = 0
        tests_depth = -1
        test_depth = -1
        key = ''
        do
            call json_parser_next(parser, event)
            select case (event%event_type)
            case (JSON_ERROR)
                ierr = 1
                message = 'invalid CTest registered-test JSON'
                return
            case (JSON_END_OF_INPUT)
                exit
            case (JSON_KEY)
                key = event%string_val
            case (JSON_ARRAY_START)
                depth = depth + 1
                if (key == 'tests') tests_depth = depth
                key = ''
            case (JSON_OBJECT_START)
                depth = depth + 1
                if (depth == tests_depth + 1) test_depth = depth
                key = ''
            case (JSON_ARRAY_END, JSON_OBJECT_END)
                if (depth == test_depth) test_depth = -1
                if (depth == tests_depth) tests_depth = -1
                depth = depth - 1
                key = ''
            case (JSON_STRING)
                if (key /= 'name' .or. depth /= test_depth) cycle
                if (count == size(names) .or. &
                    len(event%string_val) > len(names)) then
                    ierr = 1
                    message = 'registered CTest inventory exceeds supported capacity'
                    return
                end if
                count = count + 1
                names(count) = event%string_val
                key = ''
            end select
        end do
        ierr = 0
    end subroutine cmake_registered_names
end module fo_cmake_generation
