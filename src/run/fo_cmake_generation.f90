module fo_cmake_generation
    !! Delegated CMake generation capture. CMake and CTest own configuration
    !! and registered-test semantics; no FPM manifest or source scanner is used.
    use fo_build_backend, only: backend_t, cmake_configure
    use fo_cmake_context, only: cmake_context_build_path
    use fo_cmake_native_config, only: native_cmake_selected, native_cmake_configure
    use fo_cmake_native_model, only: cm_plan_t
    use fo_cmake_native_snapshot, only: native_snapshot_write
    use fo_input_inventory, only: input_inventory_t, input_inventory_discover_cmake, &
        input_inventory_cmake_reference, input_inventory_cmake_missing_include, &
        input_inventory_cmake_include_search
    use fo_fs, only: fs_make_dir, fs_write_text, fs_collect_files, fs_remove_tree, &
        fs_realpath, fs_path_is_absolute
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file, &
        read_text_file_alloc
    use fo_dep_resolve, only: normalize_path
    use fo_cache, only: cache_digest, HASH_LEN
    use fo_process, only: argv_push, process_run_argv_logged
    use fo_cmake_model, only: cmake_model_load_cases
    use fo_project_model, only: project_model_t, model_init
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
        json_parser_next, JSON_KEY, JSON_STRING, JSON_BOOL, JSON_ERROR, &
        JSON_END_OF_INPUT, &
        JSON_ARRAY_START, JSON_ARRAY_END, JSON_OBJECT_START, JSON_OBJECT_END
    implicit none
    private
    integer, parameter :: PATH_LEN = 4096, MAX_ROOTS = 60
    type :: path_alias_t
        character(:), allocatable :: authored, replacement
    end type path_alias_t
    public :: cmake_capture_inventory, cmake_registered_names, cmake_capture_cleanup
contains
    subroutine cmake_capture_inventory(backend, inventory, ierr, message, flags, &
            configured)
        type(backend_t), intent(inout) :: backend
        !! Configure flags of the tree (default none): the same as the loaded
        !! model's, so capturing never reconfigures a profiled tree.
        character(len=*), intent(in), optional :: flags
        !! The caller configured the tree with these flags and this backend
        !! (its File API reply is current): capture does not configure again.
        logical, intent(in), optional :: configured
        type(input_inventory_t), intent(out) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: roots(MAX_ROOTS), bundles(MAX_ROOTS)
        character(len=256) :: labels(MAX_ROOTS), names(MAX_ROOTS)
        character(len=PATH_LEN) :: log_file, metadata_dir, normalized
        character(len=HASH_LEN) :: digest
        character(len=:), allocatable :: cache_text
        character(:), allocatable :: line, key, kind, value, script, rewritten
        character(:), allocatable :: help, pending_help
        character(:), allocatable :: digest_parts(:)
        character(:), allocatable :: generator
        type(path_alias_t), allocatable :: aliases(:)
        integer :: n_roots, cursor, i, cleanup_status
        logical :: exists, source_key, provider, physical
        logical :: reuse
        character(len=PATH_LEN) :: project_path, build_path, fetchcontent_base

        ierr = 1
        message = ''
        if (native_cmake_selected(backend%project_dir)) then
            call native_capture_inventory(backend, inventory, ierr, message)
            return
        end if
        if (.not. backend%cmake%valid) then
            message = backend%cmake%error
            return
        end if
        if (backend%cmake%build_root(1:1) == '/') then
            message = 'resident CMake requires a relocatable build directory'
            return
        end if
        reuse = .false.
        if (present(configured)) reuse = configured
        if (.not. reuse) then
            call make_tmpfile('fo-cmake-capture-configure', log_file)
            if (present(flags)) then
                call cmake_configure(backend%cmake, flags, log_file, ierr)
            else
                call cmake_configure(backend%cmake, '', log_file, ierr)
            end if
            if (ierr /= 0) then
                message = 'CMake configuration failed during generation capture; '// &
                    'diagnostic: '//trim(log_file)
                return
            end if
            call delete_tmpfile(log_file)
        end if
        if (backend%cmake%build_root(1:1) == '/') then
            ierr = 1
            message = 'resident CMake requires a relocatable build directory'
            return
        end if
        call read_text_file_alloc(cmake_context_build_path(backend%cmake)// &
            '/CMakeCache.txt', cache_text, ierr)
        if (ierr /= 0 .or. len_trim(cache_text) == 0) then
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
        fetchcontent_base = trim(build_path)//'/_deps'
        n_roots = 0
        roots = ''
        bundles = ''
        labels = ''
        names = ''
        generator = ''
        cursor = 1
        do while (next_cache_line(cache_text, cursor, line))
            call cache_entry(line, key, kind, value)
            if (key == 'FETCHCONTENT_BASE_DIR') &
                call canonical_directory(value, fetchcontent_base)
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
        ! Populate without add_subdirectory does not create a cached
        ! <name>_SOURCE_DIR. Freeze the populated roots actually used by targets.
        call capture_populated_sources(backend, fetchcontent_base, n_roots, &
            roots, bundles, labels, names, ierr, message)
        if (ierr /= 0) return
        call validate_cmake_paths(backend, roots, ierr, message, &
            n_roots=n_roots, bundles=bundles, labels=labels, names=names)
        if (ierr /= 0) return
        ! Authored modules and declared source roots can be siblings
        ! of a standalone subproject. Preserve their relative source paths.
        normalized = project_path(:index(trim(project_path), '/', back=.true.) - 1)
        do i = 1, n_roots
            if (index(labels(i), 'cmake-module:') /= 1 .and. &
                index(labels(i), 'cmake-dependency:') /= 1) cycle
            if (.not. inside(roots(i), normalized)) cycle
            if (inside(roots(i), project_path)) cycle
            bundles(i) = roots(i)(len_trim(normalized) + 2:)
        end do
        call capture_path_aliases(cache_text, trim(project_path), trim(build_path), &
            roots(:n_roots), bundles(:n_roots), aliases, ierr, message)
        if (ierr /= 0) return
        script = '# Frozen declared CMake configuration; generated by Fo.'// &
            new_line('a')
        cursor = 1
        pending_help = ''
        do while (next_cache_line(cache_text, cursor, line))
            ! CMake wraps help comments without adding a separator; an explicit
            ! //\n continuation represents an original newline instead.
            if (index(line, '//') == 1) then
                if (index(line, '//'//achar(92)//'n') == 1) then
                    pending_help = pending_help//new_line('a')//line(5:)
                else
                    pending_help = pending_help//line(3:)
                end if
                cycle
            end if
            help = pending_help
            pending_help = ''
            call cache_entry(line, key, kind, value)
            if (len(key) == 0) cycle
            if (kind /= 'BOOL' .and. kind /= 'STRING' .and. kind /= 'PATH' .and. &
                kind /= 'FILEPATH' .and. kind /= 'INTERNAL') cycle
            if (key == 'CMAKE_INSTALL_PREFIX') cycle
            if (index(key, 'CMAKE_FIND_PACKAGE_REDIRECTS_DIR') == 1) cycle
            if (kind == 'PATH' .and. fs_path_is_absolute(value)) then
                call canonical_directory(value, normalized)
                do i = 1, n_roots
                    if (trim(normalized) /= trim(roots(i))) cycle
                    value = trim(normalized)
                    exit
                end do
            end if
            call relocate_value(value, trim(project_path), &
                trim(build_path), roots(:n_roots), &
                bundles(:n_roots), aliases, rewritten)
            ! Find-package results such as Python's headers are often INTERNAL.
            ! Keep only those internal paths pointing into captured headers;
            ! compiler and CMake implementation state remains delegated.
            if (kind == 'INTERNAL') then
                if (index(rewritten, '/dependencies/include-') == 0) cycle
            end if
            script = script//'set('//key//' "'//cmake_quote(rewritten)// &
                '" CACHE '//kind//' "'//cmake_quote(help, literal=.true.)// &
                '" FORCE)'//new_line('a')
        end do
        do i = 1, n_roots
            if (index(labels(i), 'cmake-dependency:') /= 1) cycle
            key = upper(trim(names(i)))
            script = script//'set(FETCHCONTENT_SOURCE_DIR_'//key// &
                ' "${CMAKE_CURRENT_LIST_DIR}/../../'//trim(bundles(i))// &
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
        call capture_project_git(trim(project_path), metadata_dir, script, ierr, message)
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
            ierr, message, freeze_inputs=.true.)
        if (ierr == 0) call validate_cmake_paths(backend, roots(:n_roots), &
            ierr, message, inventory)
        if (ierr /= 0) call fs_remove_tree(trim(metadata_dir), cleanup_status)
    end subroutine cmake_capture_inventory

    subroutine capture_populated_sources(backend, base, n, roots, bundles, &
            labels, names, ierr, message)
        type(backend_t), intent(in) :: backend
        character(len=*), intent(in) :: base
        integer, intent(inout) :: n
        character(len=*), intent(inout) :: roots(:), bundles(:), labels(:), names(:)
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN), allocatable :: files(:)
        character(len=PATH_LEN) :: normalized, physical
        character(:), allocatable :: text, key, path, component
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        integer :: n_files, i, slash
        logical :: resolved
        allocate(files(2048))
        allocate(character(len=1048576) :: text)
        ierr = 0
        message = ''
        call fs_collect_files(cmake_context_build_path(backend%cmake)// &
            '/.cmake/api/v1/reply', '', '.json', '', files, n_files, &
            recursive=.false.)
        if (n_files == 0 .or. n_files == size(files)) then
            ierr = 1
            message = 'CMake input metadata is unavailable or exceeds capacity'
            return
        end if
        do i = 1, n_files
            if (index(files(i), '/target-') == 0) cycle
            call read_text_file(trim(files(i)), text)
            call json_parser_init_strict(parser, trim(text))
            key = ''
            do
                call json_parser_next(parser, event)
                if (event%event_type == JSON_END_OF_INPUT) exit
                if (event%event_type == JSON_ERROR) then
                    ierr = 1
                    message = 'invalid CMake input metadata JSON'
                    return
                end if
                if (event%event_type == JSON_KEY) then
                    key = event%string_val
                    cycle
                end if
                if (event%event_type /= JSON_STRING .or. key /= 'path') cycle
                path = event%string_val
                if (len(path) == 0) cycle
                if (path(1:1) /= '/') path = backend%project_dir//'/'//path
                call normalize_path(path, normalized)
                call canonical_authored_path(normalized)
                if (.not. inside(normalized, base)) cycle
                if (len_trim(normalized) <= len_trim(base) + 1) cycle
                component = normalized(len_trim(base) + 2:)
                slash = index(component, '/')
                if (slash > 0) component = component(:slash - 1)
                if (len(component) <= 4) cycle
                if (component(len(component) - 3:) /= '-src') cycle
                call fs_realpath(trim(base)//'/'//component, physical, resolved)
                if (.not. resolved) cycle
                call add_source(physical, component(:len(component) - 4), n, &
                    roots, bundles, labels, names, ierr, message, .false.)
                if (ierr /= 0) return
            end do
        end do
    end subroutine capture_populated_sources

    subroutine native_capture_inventory(backend, inventory, ierr, message)
        type(backend_t), intent(in) :: backend
        type(input_inventory_t), intent(out) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(cm_plan_t) :: plan
        character(:), allocatable :: text, path, git_script
        character(len=PATH_LEN) :: temporary, metadata, physical
        integer :: i, j, cleanup_status
        logical :: resolved
        call native_cmake_configure(backend%cmake, plan)
        ierr = 1
        message = plan%error
        if (len(plan%error) > 0) return
        if (backend%cmake%build_root(1:1) == '/' .or. &
            index(backend%cmake%build_root, ':') == 2) then
            message = 'native resident CMake requires a relocatable build directory'
            return
        end if
        call native_snapshot_write(backend%cmake, plan, text)
        call make_tmpfile('fo-native-cmake-context', temporary)
        metadata = trim(temporary)//'.dir'
        call delete_tmpfile(temporary)
        call fs_make_dir(trim(metadata))
        call fs_write_text(trim(metadata)//'/native-context.json', text)
        call fs_write_text(trim(metadata)//'/build-directory.txt', &
            backend%cmake%build_root)
        call fs_realpath(backend%project_dir, physical, resolved)
        if (resolved) then
            git_script = ''
            call capture_project_git(trim(physical), trim(metadata), git_script, &
                ierr, message)
            if (ierr /= 0) then
                call fs_remove_tree(trim(metadata), cleanup_status)
                return
            end if
        end if
        call input_inventory_discover_cmake(backend%project_dir, &
            [character(len=PATH_LEN) ::], [character(len=PATH_LEN) ::], &
            [character(len=256) ::], trim(metadata), inventory, ierr, message)
        if (ierr == 0) then
            do i = 1, size(plan%inputs)
                path = plan%inputs(i)%text
                call fs_realpath(path, physical, resolved)
                if (resolved) resolved = inventory_has_path(inventory, physical)
                if (.not. resolved) then
                    ierr = 1
                    message = 'native CMake: input is outside frozen inventory: '//path
                    exit
                end if
            end do
        end if
        if (ierr == 0) then
            do i = 1, size(plan%targets)
                do j = 1, size(plan%targets(i)%sources)
                    path = plan%targets(i)%sources(j)%text
                    call fs_realpath(path, physical, resolved)
                    if (resolved) resolved = inventory_has_path(inventory, physical)
                    if (.not. resolved) then
                        ierr = 1
                        message = 'native CMake: source is outside frozen inventory: '// &
                            path
                        exit
                    end if
                end do
                if (ierr /= 0) exit
            end do
        end if
        if (ierr /= 0) call fs_remove_tree(trim(metadata), cleanup_status)
    end subroutine native_capture_inventory

    subroutine git_command(directory, arguments, log_file, ierr)
        character(len=*), intent(in) :: directory, arguments(:), log_file
        integer, intent(out) :: ierr
        character(:), allocatable :: packed
        integer :: i, n_args
        n_args = 0
        call argv_push(packed, n_args, 'git')
        do i = 1, size(arguments)
            call argv_push(packed, n_args, trim(arguments(i)))
        end do
        call process_run_argv_logged(directory, packed, n_args, log_file, .false., &
            60, ierr)
    end subroutine git_command

    subroutine capture_git_repository(source, destination, log_file, ierr)
        character(len=*), intent(in) :: source, destination, log_file
        integer, intent(out) :: ierr
        character(len=PATH_LEN) :: arguments(5)
        arguments(1) = 'clone'
        arguments(2) = '--bare'
        arguments(3) = '--depth=1'
        arguments(4) = 'file://'//source
        arguments(5) = destination
        call git_command('.', arguments, log_file, ierr)
    end subroutine capture_git_repository

    subroutine capture_project_git(source, metadata, script, ierr, message)
        character(len=*), intent(in) :: source, metadata
        character(:), allocatable, intent(inout) :: script
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: log_file, root, physical
        character(:), allocatable :: repository
        integer :: newline_at
        character(len=32) :: arguments(3)

        message = ''
        call make_tmpfile('fo-cmake-project-git', log_file)
        arguments(1) = 'rev-parse'
        arguments(2) = '--show-toplevel'
        call git_command(source, arguments(:2), log_file, ierr)
        if (ierr /= 0) then
            call delete_tmpfile(log_file)
            ierr = 0
            return
        end if
        call read_text_file(log_file, root)
        newline_at = index(root, new_line('a'))
        if (newline_at > 0) root = root(:newline_at - 1)
        call canonical_directory(trim(root), physical)
        root = physical
        if (trim(root) /= source) then
            call delete_tmpfile(log_file)
            ierr = 0
            return
        end if
        repository = trim(metadata)//'/project-git'
        call capture_git_repository(source, repository, log_file, ierr)
        arguments(1) = 'config'
        arguments(2) = '--remove-section'
        arguments(3) = 'remote.origin'
        if (ierr == 0) call git_command(repository, arguments, log_file, ierr)
        arguments(2) = 'core.bare'
        arguments(3) = 'false'
        if (ierr == 0) call git_command(repository, arguments, log_file, ierr)
        arguments(2) = 'core.worktree'
        arguments(3) = '../..'
        if (ierr == 0) call git_command(repository, arguments, log_file, ierr)
        arguments(1) = 'read-tree'
        arguments(2) = 'HEAD'
        if (ierr == 0) call git_command(repository, arguments(:2), log_file, ierr)
        if (ierr /= 0) then
            message = 'cannot freeze project Git build provenance; diagnostic: '// &
                trim(log_file)
            return
        end if
        call delete_tmpfile(log_file)
        script = script//'file(WRITE "${CMAKE_CURRENT_LIST_DIR}/../.git" '// &
            '"gitdir: .fo-cmake/project-git\n")'//new_line('a')
    end subroutine capture_project_git

    subroutine capture_provider_metadata(roots, names, labels, metadata, ierr, message)
        character(len=*), intent(in) :: roots(:), names(:), labels(:), metadata
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=1048576), allocatable :: cache
        character(len=PATH_LEN) :: log_file
        character(:), allocatable :: line, key, kind, value, source, frozen
        integer :: i, cursor

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
            call capture_git_repository(source, &
                trim(metadata)//'/provider-git/'//trim(names(i)), log_file, ierr)
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

    subroutine capture_path_aliases(cache, project, build, roots, bundles, &
            aliases, ierr, message)
        character(len=*), intent(in) :: cache, project, build, roots(:), bundles(:)
        type(path_alias_t), allocatable, intent(out) :: aliases(:)
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(path_alias_t), allocatable :: grown(:)
        character(:), allocatable :: line, key, kind, value, path, prefix
        character(:), allocatable :: replacement
        character(len=PATH_LEN) :: physical
        integer :: cursor, first, last, at, i, j, position
        logical :: resolved

        ierr = 0
        message = ''
        allocate(aliases(0))
        cursor = 1
        do while (next_cache_line(cache, cursor, line))
            call cache_entry(line, key, kind, value)
            if (kind /= 'PATH' .and. kind /= 'FILEPATH' .and. kind /= 'INTERNAL' .and. &
                key /= 'CMAKE_HOME_DIRECTORY') cycle
            first = 1
            do while (first <= len(value))
                last = index(value(first:), ';')
                if (last == 0) then
                    last = len(value) + 1
                else
                    last = first + last - 1
                end if
                path = value(first:last - 1)
                first = last + 1
                if (len(path) == 0) cycle
                if (path(1:1) /= '/' .or. index(path, '$') /= 0) cycle
                ! Probe directory prefixes, retaining the authored suffix. A
                ! known root may have aliases even when its suffix is absent.
                do at = len(path) + 1, 2, -1
                    if (at <= len(path)) then
                        if (path(at:at) /= '/') cycle
                    end if
                    prefix = path(:at - 1)
                    call fs_realpath(prefix, physical, resolved)
                    if (.not. resolved .or. trim(physical) == prefix) cycle
                    replacement = ''
                    if (trim(physical) == build) then
                        replacement = '${CMAKE_BINARY_DIR}'
                    else if (trim(physical) == project) then
                        replacement = '${CMAKE_CURRENT_LIST_DIR}/..'
                    else
                        do j = 1, size(roots)
                            if (trim(physical) /= trim(roots(j))) cycle
                            if (len_trim(bundles(j)) == 0) then
                                ierr = 1
                                message = 'captured CMake alias has no destination: '//prefix
                                return
                            end if
                            replacement = '${CMAKE_CURRENT_LIST_DIR}/../../'// &
                                trim(bundles(j))
                            exit
                        end do
                    end if
                    if (len(replacement) == 0) cycle
                    do i = 1, size(aliases)
                        if (aliases(i)%authored == prefix) exit
                    end do
                    if (i <= size(aliases)) cycle
                    ! Longer aliases must be replaced before their parents.
                    position = size(aliases) + 1
                    do i = 1, size(aliases)
                        if (len(aliases(i)%authored) >= len(prefix)) cycle
                        position = i
                        exit
                    end do
                    allocate(grown(size(aliases) + 1))
                    do i = 1, position - 1
                        grown(i) = aliases(i)
                    end do
                    grown(position)%authored = prefix
                    grown(position)%replacement = replacement
                    do i = position, size(aliases)
                        grown(i + 1) = aliases(i)
                    end do
                    call move_alloc(grown, aliases)
                end do
            end do
        end do
    end subroutine capture_path_aliases

    subroutine relocate_value(value, project, build, roots, bundles, aliases, relocated)
        character(len=*), intent(in) :: value, project, build, roots(:), bundles(:)
        type(path_alias_t), intent(in) :: aliases(:)
        character(:), allocatable, intent(out) :: relocated
        integer :: i
        relocated = value
        ! Resolve proven aliases before a containing physical root consumes
        ! them (for example a build alias inside a dependency source tree).
        do i = 1, size(aliases)
            call replace_path(relocated, aliases(i)%authored, &
                aliases(i)%replacement, alias=.true.)
        end do
        do i = 1, size(roots)
            call replace_path(relocated, trim(roots(i)), &
                '${CMAKE_CURRENT_LIST_DIR}/../../'//trim(bundles(i)))
        end do
        call replace_path(relocated, trim(build), '${CMAKE_BINARY_DIR}')
        call replace_path(relocated, trim(project), '${CMAKE_CURRENT_LIST_DIR}/..')
    end subroutine relocate_value

    subroutine replace_path(value, old, replacement, alias)
        character(:), allocatable, intent(inout) :: value
        character(len=*), intent(in) :: old, replacement
        logical, intent(in), optional :: alias
        integer :: at, start
        if (len(old) == 0) return
        start = 1
        do
            at = index(value(start:), old)
            if (at == 0) return
            at = at + start - 1
            if (present(alias) .and. at > 1) then
                select case (value(at - 1:at - 1))
                case (';', ' ', '"', "'", '=')
                case default
                    start = at + len(old)
                    cycle
                end select
            end if
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

    function cmake_quote(value, literal) result(escaped)
        character(len=*), intent(in) :: value
        logical, intent(in), optional :: literal
        character(:), allocatable :: escaped
        integer :: i
        logical :: quote_dollar
        quote_dollar = .false.
        if (present(literal)) quote_dollar = literal
        escaped = ''
        do i = 1, len(value)
            if (value(i:i) == '"' .or. iachar(value(i:i)) == 92 .or. &
                    (quote_dollar .and. value(i:i) == '$')) &
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

    subroutine validate_cmake_paths(backend, roots, ierr, message, inventory, &
            n_roots, bundles, labels, names)
        type(backend_t), intent(in) :: backend
        character(len=*), intent(inout) :: roots(:)
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(input_inventory_t), intent(inout), optional :: inventory
        integer, intent(inout), optional :: n_roots
        character(len=*), intent(inout), optional :: bundles(:), labels(:), names(:)
        character(len=PATH_LEN), allocatable :: files(:)
        character(len=PATH_LEN) :: normalized, authored, project, build, physical
        character(len=PATH_LEN) :: cmake_root, toolchain
        character(len=PATH_LEN) :: digest_parts(1)
        character(len=HASH_LEN) :: digest
        character(len=:), allocatable :: text
        character(:), allocatable :: key, path, line, kind, value, module_root
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        integer :: n_files, i, j, depth, count, cursor
        character(len=64) :: ancestors(64)
        character(len=PATH_LEN) :: pending_paths(64)
        logical :: generated(64), cmake_input(64), captured, exists, include_path
        logical :: delegated

        allocate(files(2048))
        allocate(character(len=1048576) :: text)
        ierr = 0
        message = ''
        count = size(roots)
        if (present(n_roots)) count = n_roots
        call canonical_directory(backend%project_dir, project)
        call canonical_directory(cmake_context_build_path(backend%cmake), build)
        cmake_root = ''
        toolchain = ''
        call read_text_file(trim(build)//'/CMakeCache.txt', text)
        cursor = 1
        do while (next_cache_line(text, cursor, line))
            call cache_entry(line, key, kind, value)
            if (key == 'CMAKE_ROOT' .and. len(value) > 0) &
                call canonical_directory(value, cmake_root)
            if (key == 'CMAKE_TOOLCHAIN_FILE' .and. len(value) > 0) then
                call normalize_path(value, toolchain)
                call canonical_authored_path(toolchain)
            end if
        end do
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
            pending_paths = ''
            generated = .false.
            cmake_input = .false.
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
                    pending_paths(depth) = ''
                    generated(depth) = .false.
                    cmake_input(depth) = .false.
                    key = ''
                    cycle
                case (JSON_ARRAY_END, JSON_OBJECT_END)
                    if (event%event_type == JSON_OBJECT_END .and. depth > 0) then
                        path = trim(pending_paths(depth))
                        if (len(path) > 0) then
                            if (path(1:1) /= '/') then
                                call normalize_path(trim(project)//'/'//path, normalized)
                            else
                                call normalize_path(path, normalized)
                            end if
                            ! CMake's codemodel also lists outputs and virtual
                            ! .rule entries of targets that have not been built.
                            ! They are not authored inputs to freeze yet.
                            if (index(files(i), '/target-') > 0 .and. &
                                generated(depth) .and. &
                                (inside(normalized, project) .or. &
                                inside(normalized, build))) then
                                inquire(file=trim(normalized), exist=exists)
                                if (.not. exists) then
                                    depth = depth - 1
                                    key = ''
                                    cycle
                                end if
                            end if
                            authored = normalized
                            call canonical_authored_path(normalized)
                            include_path = any(ancestors(:depth) == 'includes')
                            if (include_path) then
                                ! CMake permits absent search directories. The
                                ! project scan detects their later creation.
                                if (input_inventory_cmake_missing_include( &
                                        trim(project), trim(authored))) then
                                    if (present(inventory)) then
                                        call input_inventory_cmake_include_search( &
                                            inventory, trim(authored), ierr, message)
                                        if (ierr /= 0) return
                                    end if
                                    depth = depth - 1
                                    key = ''
                                    cycle
                                end if
                                call canonical_directory(normalized, physical)
                                normalized = physical
                            end if
                            if (include_path .and. present(n_roots) .and. &
                                .not. inside(normalized, build)) then
                                captured = trim(physical) == trim(project)
                                do j = 1, count
                                    if (inside(physical, roots(j))) captured = .true.
                                end do
                                if (.not. captured) then
                                    digest_parts(1) = physical
                                    digest = cache_digest(digest_parts, 1)
                                    call add_source(trim(physical), 'include-'//digest(:16), &
                                        n_roots, roots, bundles, labels, names, &
                                        ierr, message, .false.)
                                    if (ierr /= 0) return
                                    labels(n_roots) = 'cmake-include:'//digest(:16)
                                    count = n_roots
                                end if
                            end if
                            captured = inside(normalized, project) .or. &
                                inside(normalized, build)
                            do j = 1, count
                                if (inside(normalized, roots(j))) captured = .true.
                            end do
                            ! CMake's installation and exact toolchain stay delegated.
                            ! Authored files under /opt or /usr still need capture.
                            delegated = .false.
                            if (index(files(i), '/cmakeFiles-') > 0) then
                                delegated = cmake_input(depth)
                                if (trim(normalized) == trim(toolchain)) &
                                    delegated = .true.
                                ! Some CMake versions omit isCMake on templates.
                                if (len_trim(cmake_root) > 0) then
                                    if (inside(normalized, cmake_root)) &
                                        delegated = .true.
                                end if
                            end if
                            if (delegated) captured = .true.
                            ! Configure inputs declare authored module roots,
                            ! independently of target header/source directories.
                            if (.not. captured .and. present(n_roots) .and. &
                                index(files(i), '/cmakeFiles-') > 0 .and. &
                                .not. generated(depth)) then
                                module_root = normalized(:index(trim(normalized), &
                                    '/', back=.true.) - 1)
                                inquire(file=trim(normalized), exist=exists)
                                if (exists .and. .not. inside(project, module_root)) then
                                    digest_parts(1) = module_root
                                    digest = cache_digest(digest_parts, 1)
                                    call add_source(module_root, 'module-'//digest(:16), &
                                        n_roots, roots, bundles, labels, names, &
                                        ierr, message, .false.)
                                    if (ierr /= 0) return
                                    labels(n_roots) = 'cmake-module:'//digest(:16)
                                    count = n_roots
                                    captured = .true.
                                end if
                            end if
                            if (.not. captured) then
                                ierr = 1
                                message = 'unsupported uncaptured external CMake input: '// &
                                    trim(normalized)
                                return
                            end if
                            if (present(inventory)) then
                                if (.not. inside(normalized, build) .and. &
                                    .not. delegated) then
                                    call input_inventory_cmake_reference( &
                                        inventory, trim(authored), ierr, message)
                                    if (ierr /= 0) return
                                    if (.not. inventory_has_path(inventory, normalized)) then
                                        ierr = 1
                                        message = 'referenced CMake input is missing or unsupported: '// &
                                            trim(normalized)
                                        return
                                    end if
                                end if
                            end if
                        end if
                    end if
                    if (depth > 0) depth = depth - 1
                    key = ''
                    cycle
                case (JSON_KEY)
                    key = event%string_val
                    cycle
                case (JSON_BOOL)
                    if (key == 'isGenerated' .and. depth > 0) &
                        generated(depth) = event%bool_val
                    if (key == 'isCMake' .and. depth > 0 .and. &
                        index(files(i), '/cmakeFiles-') > 0) &
                        cmake_input(depth) = event%bool_val
                    key = ''
                    cycle
                end select
                if (event%event_type /= JSON_STRING) cycle
                ! Target artifacts are outputs relative to the build tree.
                if (any(ancestors(:depth) == 'artifacts')) cycle
                ! Target install destinations/prefix are output locations.
                if (any(ancestors(:depth) == 'install') .and. &
                    (any(ancestors(:depth) == 'destinations') .or. &
                    any(ancestors(:depth) == 'prefix'))) cycle
                if (key /= 'path') cycle
                path = event%string_val
                if (len(path) == 0) cycle
                pending_paths(depth) = path
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
        type(project_model_t) :: model
        character(len=:), allocatable :: detail
        type(cm_plan_t) :: native_plan
        integer :: n_args, i

        names = ''
        count = 0
        message = ''
        if (native_cmake_selected(backend%project_dir)) then
            call native_cmake_configure(backend%cmake, native_plan)
            ierr = 1
            message = native_plan%error
            if (len_trim(message) > 0) return
            if (size(native_plan%tests) > size(names)) then
                message = 'native registered test inventory exceeds capacity'
                return
            end if
            do n_args = 1, size(native_plan%tests)
                if (len(native_plan%tests(n_args)%name) > len(names)) then
                    message = 'native registered test name exceeds capacity'
                    return
                end if
                count = count + 1
                names(count) = native_plan%tests(n_args)%name
            end do
            ierr = 0
            return
        end if
        call model_init(model, 'cmake', '')
        call cmake_model_load_cases(backend%cmake, model, ierr, detail)
        if (ierr /= 0) then
            message = detail
            return
        end if
        do i = 1, model%n_cases
            ! Cases a test preset filters out are not part of the inventory.
            if (model%cases(i)%default_excluded) cycle
            if (count == size(names) .or. &
                len(model%cases(i)%name) > len(names)) then
                ierr = 1
                message = 'registered CTest inventory exceeds supported capacity'
                return
            end if
            count = count + 1
            names(count) = model%cases(i)%name
        end do
        ierr = 0
    end subroutine cmake_registered_names
end module fo_cmake_generation
