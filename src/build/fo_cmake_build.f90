module fo_cmake_build
    !! Native build of a configured CMake tree (CMake only configures).
    !!
    !! The File API codemodel and toolchains supply, for every target, its
    !! compile groups (language, compile command fragments, includes, defines,
    !! module directory), link and archive fragments and the compiler of each
    !! language. Fo compiles the C, C++ and Fortran sources itself on its own
    !! scheduler, scanner and action cache (module-interface cutoff included),
    !! archives static libraries and links executables with the exact
    !! fragments CMake reports.
    !!
    !! Route: a target is built natively when it is an executable or static
    !! library whose sources all exist before the build and are compiled by a
    !! known toolchain compiler. Everything else that the request needs
    !! (custom targets and commands, generated sources, shared, module and
    !! object libraries, a source compiled differently or a module defined
    !! twice across targets, other generators) is delegated to
    !! `cmake --build --target`, which runs first so that generated inputs
    !! exist. The decision is written to the build log.
    use fx_string, only: string_t, ends_with
    use fx_hash, only: fnv1a_string
    use fo_cmake_context, only: cmake_context_t, cmake_context_read_file, &
        cmake_context_build_path
    use fo_cmake_model, only: cmake_plan_t, cmake_target_t, cmake_group_t, &
        cmake_plan_load, cmake_split_fragment
    use fo_scan, only: scan_unit_t, scan_file_preprocessed, scan_file_regex, &
        MAX_UNITS
    use fo_gfortran_build, only: native_action_t, native_compile_actions, &
        native_link_exact, native_archive_target, make_obj_path
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t
    use fo_compdb, only: compdb_write_commands
    use fo_fs, only: fs_make_dir, fs_write_text
    use fo_process, only: argv_push, argv_push_split_nl
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    implicit none
    private

    public :: cmake_route_t, cmake_build_route, cmake_build_native

    integer, parameter :: MODE_SKIP = 0, MODE_NATIVE = 1, MODE_DELEGATED = 2
    character(len=1), parameter :: nl = achar(10)
    character(len=1024), save :: route_file = ''

    type :: target_actions_t
        type(native_action_t), allocatable :: a(:)
        integer, allocatable :: source(:)
        !! Objects of OBJECT_LIBRARY sources this target consumes
        !! ($<TARGET_OBJECTS>): providing target and its source index.
        integer, allocatable :: obj_target(:), obj_source(:)
    end type target_actions_t

    type, public :: cmake_route_t
        !! Which targets of the request are built natively or delegated.
        type(cmake_plan_t) :: plan
        logical :: usable = .false.
        character(len=:), allocatable :: reason
        integer, allocatable :: mode(:)
        type(target_actions_t), allocatable :: actions(:)
        character(len=:), allocatable :: ninja
        type(string_t), allocatable :: delegated(:)
        integer :: n_delegated = 0
        integer :: n_native = 0
    end type cmake_route_t

contains

    subroutine cmake_build_route(context, route, log_file)
        !! Classify the requested targets. A route that is not usable
        !! (forced delegation, stale reply, other generator) leaves the whole
        !! build to CMake; route%reason says why.
        type(cmake_context_t), intent(inout) :: context
        type(cmake_route_t), intent(out) :: route
        character(len=*), intent(in) :: log_file
        character(len=:), allocatable :: message
        character(len=16) :: forced
        logical, allocatable :: wanted(:)
        integer :: status, i, n

        route%usable = .false.
        route%reason = ''
        allocate (route%delegated(0))
        route_file = cmake_context_build_path(context)//'/.fo-native-cmake'
        call fs_make_dir(trim(route_file))
        route_file = trim(route_file)//'/route.log'
        call fs_write_text(trim(route_file), '')
        call get_environment_variable('FO_CMAKE_DELEGATE_BUILD', forced, &
            status=status)
        if (status == 0 .and. trim(forced) == '1') then
            route%reason = 'FO_CMAKE_DELEGATE_BUILD=1'
            call log_line(log_file, 'fo: delegated: all targets ('// &
                route%reason//')')
            return
        end if
        call cmake_plan_load(context, route%plan, message)
        if (len(message) > 0) then
            route%reason = message
        else if (.not. native_generator(context)) then
            route%reason = 'generator '//trim(context%reported_generator)// &
                ' is not single-configuration Ninja'
        else
            call cmake_context_read_file(route%plan%build_root//'/build.ninja', &
                route%ninja)
            if (.not. allocated(route%ninja)) then
                route%reason = 'cannot read build.ninja'
            else
                route%ninja = nl//route%ninja
            end if
        end if
        if (len(route%reason) > 0) then
            call log_line(log_file, 'fo: delegated: all targets ('// &
                route%reason//')')
            return
        end if
        n = size(route%plan%targets)
        allocate (route%mode(n), route%actions(n), wanted(n))
        route%mode = MODE_SKIP
        call wanted_targets(context, route%plan, route%ninja, wanted, message)
        if (len(message) > 0) then
            route%reason = message
            call log_line(log_file, 'fo: delegated: all targets ('//message//')')
            return
        end if
        do i = 1, n
            if (wanted(i)) call classify(route, i, log_file)
        end do
        call demote_conflicts(route, log_file)
        call demote_object_consumers(route, log_file)
        route%usable = .true.
        do i = 1, n
            if (route%mode(i) == MODE_NATIVE) then
                route%n_native = route%n_native + 1
                call log_line(log_file, 'fo: native CMake build: '// &
                    route%plan%targets(i)%name)
            end if
        end do
    end subroutine cmake_build_route

    logical function native_generator(context) result(ok)
        type(cmake_context_t), intent(in) :: context

        ok = .false.
        if (.not. allocated(context%reported_generator)) return
        ok = index(context%reported_generator, 'Ninja') == 1 .and. &
            .not. context%multi_config
        if (ok) ok = index(context%reported_generator, 'Multi-Config') == 0
    end function native_generator

    subroutine classify(route, t, log_file)
        type(cmake_route_t), intent(inout) :: route
        integer, intent(in) :: t
        character(len=*), intent(in) :: log_file
        character(len=:), allocatable :: reason
        type(string_t), allocatable :: grown(:)
        integer :: n

        reason = ''
        select case (route%plan%targets(t)%type)
        case ('INTERFACE_LIBRARY')
            return
        case ('EXECUTABLE', 'STATIC_LIBRARY', 'OBJECT_LIBRARY')
            call native_eligible(route%plan, route%ninja, t, route%actions(t), reason)
        case ('UTILITY')
            reason = 'custom or utility target'
        case default
            reason = 'unsupported target type '//route%plan%targets(t)%type
        end select
        if (len(reason) == 0) then
            route%mode(t) = MODE_NATIVE
            return
        end if
        route%mode(t) = MODE_DELEGATED
        n = route%n_delegated
        allocate (grown(n + 1))
        if (n > 0) grown(1:n) = route%delegated(1:n)
        grown(n + 1)%s = route%plan%targets(t)%name
        call move_alloc(grown, route%delegated)
        route%n_delegated = n + 1
        call log_line(log_file, 'fo: delegated: '//route%plan%targets(t)%name// &
            ' ('//reason//')')
    end subroutine classify

    subroutine native_eligible(plan, ninja, t, built, reason)
        !! Empty reason: target t is buildable natively and `built` holds its
        !! compile actions.
        type(cmake_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: ninja
        integer, intent(in) :: t
        type(target_actions_t), intent(out) :: built
        character(len=:), allocatable, intent(out) :: reason
        integer :: s, n, group, provider, provider_source
        logical :: exists

        reason = ''
        allocate (built%obj_target(0), built%obj_source(0))
        if (has_custom_commands(plan, ninja, t)) then
            reason = 'custom command attached to the target'
            return
        end if
        associate (tgt => plan%targets(t))
            if (tgt%n_sources == 0 .or. len(tgt%artifact) == 0) then
                reason = 'no sources or artifact'
                return
            end if
            allocate (built%a(tgt%n_sources), built%source(tgt%n_sources))
            n = 0
            do s = 1, tgt%n_sources
                group = tgt%src_group(s)
                if (tgt%src_generated(s)) then
                    call object_library_source(plan, tgt%sources(s)%s, &
                        provider, provider_source)
                    if (provider == 0) then
                        reason = 'generated source '//tgt%sources(s)%s
                        return
                    end if
                    built%obj_target = [built%obj_target, provider]
                    built%obj_source = [built%obj_source, provider_source]
                    cycle
                end if
                if (group == 0) cycle
                inquire (file=tgt%sources(s)%s, exist=exists)
                if (.not. exists) then
                    reason = 'source not present before the build '// &
                        tgt%sources(s)%s
                    return
                end if
                n = n + 1
                call make_action(plan, t, group, built%a(n), reason)
                if (len(reason) > 0) return
                if (built%a(n)%language == 'Fortran') then
                    call source_preprocessing(plan, ninja, t, s, built%a(n), reason)
                    if (len(reason) > 0) return
                end if
                built%source(n) = s
            end do
            if (n == 0 .and. size(built%obj_target) == 0) then
                reason = 'no compiled sources'
                return
            end if
            built%a = built%a(:n)
            built%source = built%source(:n)
            if (tgt%type == 'EXECUTABLE') then
                if (len(tool(plan, tgt%link_language)) == 0) &
                    reason = 'no toolchain compiler for link language '// &
                        tgt%link_language
            end if
        end associate
    end subroutine native_eligible

    subroutine source_preprocessing(plan, ninja, t, s, action, reason)
        !! The File API omits Fortran_PREPROCESS. Ninja records the resolved
        !! per-source choice as PREPROCESS_SCAN versus SCAN, even when ON and
        !! OFF sources share one compile group. Compile the original source
        !! with the same choice instead of losing CMake's preprocessing step.
        type(cmake_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: ninja
        integer, intent(in) :: t, s
        type(native_action_t), intent(inout) :: action
        character(len=:), allocatable, intent(out) :: reason
        character(len=:), allocatable :: prefix, source, line
        integer :: start, finish, at

        reason = ''
        prefix = plan%targets(t)%build_dir
        if (prefix == plan%build_root) then
            prefix = ''
        else if (index(prefix, plan%build_root//'/') == 1) then
            prefix = prefix(len(plan%build_root) + 2:)//'/'
        end if
        prefix = nl//'build '//ninja_path(prefix//'CMakeFiles/'// &
            plan%targets(t)%name//'.dir/')
        source = ' '//ninja_path(plan%targets(t)%sources(s)%s)
        start = 1
        do
            at = index(ninja(start:), prefix)
            if (at == 0) exit
            start = start + at
            finish = start + index(ninja(start:), nl) - 2
            if (finish < start) exit
            line = ninja(start:finish)
            start = finish + 1
            at = index(line, source)
            if (at == 0) cycle
            at = at + len(source)
            if (at <= len(line)) then
                if (line(at:at) /= ' ') cycle
            end if
            if (index(line, ': Fortran_PREPROCESS_SCAN_') > 0) then
                call add_token(action%argv, '-cpp')
                return
            else if (index(line, ': Fortran_SCAN_') > 0) then
                call add_token(action%argv, '-nocpp')
                return
            end if
        end do
        reason = 'cannot resolve Fortran preprocessing for '// &
            plan%targets(t)%sources(s)%s
    end subroutine source_preprocessing

    function ninja_path(path) result(escaped)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: escaped
        integer :: i

        escaped = ''
        do i = 1, len(path)
            if (index(' $:', path(i:i)) > 0) escaped = escaped//'$'
            escaped = escaped//path(i:i)
        end do
    end function ninja_path

    logical function has_custom_commands(plan, ninja, t) result(has)
        !! True when the target's link statement in build.ninja carries
        !! PRE_LINK, PRE_BUILD or POST_BUILD commands, or cannot be found
        !! (for an OBJECT_LIBRARY: when it is not a plain phony).
        type(cmake_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: ninja
        integer, intent(in) :: t
        character(len=:), allocatable :: artifact, statement
        character(len=16), parameter :: names(2) = &
            [character(len=16) :: 'POST_BUILD', 'PRE_LINK']
        integer :: start, finish, at, eol, k

        has = .true.
        artifact = plan%targets(t)%artifact
        if (index(artifact, plan%build_root//'/') == 1) &
            artifact = artifact(len(plan%build_root) + 2:)
        if (plan%targets(t)%type == 'OBJECT_LIBRARY') then
            ! No link statement: the target is a phony over its objects
            ! unless commands are attached to it.
            at = index(artifact, 'CMakeFiles/'//plan%targets(t)%name//'.dir/')
            if (at == 0) return
            has = index(ninja, nl//'build '//artifact(:at - 1)// &
                plan%targets(t)%name//': phony ') == 0
            return
        end if
        start = index(ninja, nl//'build '//artifact//': ')
        if (start == 0) return
        start = start + 1
        finish = index(ninja(start:), nl//nl)
        finish = merge(len(ninja), start + finish - 1, finish == 0)
        statement = ninja(start:finish)
        do k = 1, size(names)
            at = index(statement, nl//'  '//trim(names(k))//' = ')
            if (at == 0) cycle
            at = at + len(nl//'  '//trim(names(k))//' = ')
            eol = index(statement(at:), nl)
            if (eol == 0) eol = len(statement) - at + 2
            if (statement(at:at + eol - 2) /= ':') return
        end do
        has = .false.
    end function has_custom_commands

    function tool(plan, language) result(compiler)
        type(cmake_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: language
        character(len=:), allocatable :: compiler
        integer :: i

        compiler = ''
        do i = 1, size(plan%tool_languages)
            if (plan%tool_languages(i)%s /= language) cycle
            compiler = plan%tool_compilers(i)%s
            return
        end do
    end function tool

    function absolute(path, base) result(r)
        character(len=*), intent(in) :: path, base
        character(len=:), allocatable :: r

        r = path
        if (len(path) > 0) then
            if (path(1:1) == '/') return
        end if
        r = base//'/'//path
    end function absolute

    subroutine add_token(argv, token)
        character(len=:), allocatable, intent(inout) :: argv
        character(len=*), intent(in) :: token

        if (len(token) == 0) return
        if (len(argv) > 0) then
            argv = argv//nl//token
        else
            argv = token
        end if
    end subroutine add_token

    logical function takes_module_directory(token) result(takes)
        character(len=*), intent(in) :: token

        select case (token)
        case ('-J', '-module', '-module-dir', '-mdir')
            takes = .true.
        case default
            takes = .false.
        end select
    end function takes_module_directory

    subroutine make_action(plan, t, group, action, reason)
        !! The compile action of one compile group: defines, includes and
        !! fragments as CMake reports them, plus, for Fortran, the module
        !! output directory and the module directories of the targets it
        !! depends on.
        type(cmake_plan_t), intent(in) :: plan
        integer, intent(in) :: t, group
        type(native_action_t), intent(out) :: action
        character(len=:), allocatable, intent(inout) :: reason
        type(compiler_dialect_t) :: dialect
        type(string_t), allocatable :: words(:)
        character(len=:), allocatable :: argv, search, mod_dir
        integer :: i, j, k, n_words
        logical :: has_module_flag

        argv = ''
        search = ''
        mod_dir = ''
        has_module_flag = .false.
        associate (grp => plan%targets(t)%groups(group))
            action%language = grp%language
            select case (grp%language)
            case ('Fortran', 'C', 'CXX')
            case default
                reason = 'unsupported language '//grp%language
                return
            end select
            action%compiler = tool(plan, grp%language)
            if (len(action%compiler) == 0) then
                reason = 'no toolchain compiler for '//grp%language
                return
            end if
            do i = 1, grp%n_defines
                call add_token(argv, '-D'//grp%defines(i)%s)
            end do
            do i = 1, grp%n_includes
                ! Fortran module lookup needs -I even for SYSTEM includes;
                ! -isystem only supplies a preprocessor/header search path.
                if (grp%include_system(i) .and. grp%language /= 'Fortran') then
                    call add_token(argv, '-isystem')
                    call add_token(argv, grp%includes(i)%s)
                else
                    call add_token(argv, '-I'//grp%includes(i)%s)
                end if
                call add_token(search, grp%includes(i)%s)
            end do
            do i = 1, grp%n_fragments
                call cmake_split_fragment(grp%fragments(i)%s, words, n_words)
                j = 1
                do while (j <= n_words)
                    if (takes_module_directory(words(j)%s) .and. j < n_words) then
                        mod_dir = absolute(words(j + 1)%s, plan%build_root)
                        call add_token(argv, words(j)%s)
                        call add_token(argv, mod_dir)
                        has_module_flag = .true.
                        j = j + 2
                    else if (index(words(j)%s, '-J') == 1 .and. &
                             len(words(j)%s) > 2) then
                        mod_dir = absolute(words(j)%s(3:), plan%build_root)
                        call add_token(argv, '-J'//mod_dir)
                        has_module_flag = .true.
                        j = j + 1
                    else
                        call add_token(argv, words(j)%s)
                        j = j + 1
                    end if
                end do
            end do
        end associate
        if (action%language == 'Fortran') then
            if (.not. has_module_flag) then
                mod_dir = plan%targets(t)%build_dir
                dialect = compiler_dialect(action%compiler)
                call add_token(argv, dialect%module_flags(mod_dir))
            end if
            call add_token(search, mod_dir)
            do i = 1, plan%targets(t)%n_deps
                k = target_index(plan, plan%targets(t)%dep_names(i)%s)
                if (k == 0) cycle
                if (.not. has_fortran(plan%targets(k))) cycle
                call add_token(argv, '-I'//fortran_module_dir(plan, k))
                call add_token(search, fortran_module_dir(plan, k))
            end do
            action%mod_dir = mod_dir
        else
            action%mod_dir = ''
        end if
        action%argv = argv
        action%search = search
    end subroutine make_action

    logical function has_fortran(tgt) result(found)
        type(cmake_target_t), intent(in) :: tgt
        integer :: g

        found = .false.
        do g = 1, tgt%n_groups
            if (tgt%groups(g)%language == 'Fortran') found = .true.
        end do
    end function has_fortran

    function fortran_module_dir(plan, t) result(dir)
        !! Where target t writes its Fortran modules.
        type(cmake_plan_t), intent(in) :: plan
        integer, intent(in) :: t
        character(len=:), allocatable :: dir
        type(string_t), allocatable :: words(:)
        integer :: g, i, j, n_words

        dir = plan%targets(t)%build_dir
        do g = 1, plan%targets(t)%n_groups
            if (plan%targets(t)%groups(g)%language /= 'Fortran') cycle
            do i = 1, plan%targets(t)%groups(g)%n_fragments
                call cmake_split_fragment( &
                    plan%targets(t)%groups(g)%fragments(i)%s, words, n_words)
                do j = 1, n_words
                    if (takes_module_directory(words(j)%s) .and. j < n_words) then
                        dir = absolute(words(j + 1)%s, plan%build_root)
                        return
                    else if (index(words(j)%s, '-J') == 1 .and. &
                             len(words(j)%s) > 2) then
                        dir = absolute(words(j)%s(3:), plan%build_root)
                        return
                    end if
                end do
            end do
            return
        end do
    end function fortran_module_dir

    integer function target_index(plan, name) result(at)
        type(cmake_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: name

        do at = 1, size(plan%targets)
            if (plan%targets(at)%name == name) return
        end do
        at = 0
    end function target_index

    subroutine wanted_targets(context, plan, ninja, wanted, message)
        !! The requested targets (all when none are named) and everything
        !! they depend on.
        type(cmake_context_t), intent(in) :: context
        type(cmake_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: ninja
        logical, intent(out) :: wanted(:)
        character(len=:), allocatable, intent(out) :: message
        integer :: i, j, at
        logical :: changed, any_named

        message = ''
        wanted = .false.
        any_named = .false.
        if (allocated(context%build_targets)) any_named = &
            size(context%build_targets) > 0
        if (any_named) then
            do i = 1, size(context%build_targets)
                if (trim(context%build_targets(i)) == 'all') then
                    call all_members(plan, ninja, wanted, message)
                    if (len(message) > 0) return
                    cycle
                end if
                at = target_index(plan, trim(context%build_targets(i)))
                if (at == 0) then
                    message = 'unknown target '//trim(context%build_targets(i))
                    return
                end if
                wanted(at) = .true.
            end do
        else
            call all_members(plan, ninja, wanted, message)
            if (len(message) > 0) return
        end if
        changed = .true.
        do while (changed)
            changed = .false.
            do i = 1, size(plan%targets)
                if (.not. wanted(i)) cycle
                do j = 1, plan%targets(i)%n_deps
                    at = target_index(plan, plan%targets(i)%dep_names(j)%s)
                    if (at == 0) cycle
                    if (wanted(at)) cycle
                    wanted(at) = .true.
                    changed = .true.
                end do
            end do
        end do
    end subroutine wanted_targets

    recursive subroutine all_members(plan, ninja, wanted, message, phony)
        !! Targets of the generator's default `all` target, read from the
        !! `build all: phony ...` line of build.ninja (CMake offers no
        !! EXCLUDE_FROM_ALL flag in the File API). A subdirectory entry
        !! `DIR/all` expands through its own phony line; any other entry that
        !! matches no target by name, artifact or `DIR/name` (CMake target names
        !! are global) makes the membership unknown.
        type(cmake_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: ninja
        logical, intent(inout) :: wanted(:)
        character(len=:), allocatable, intent(inout) :: message
        character(len=*), intent(in), optional :: phony
        character(len=:), allocatable :: key, word, raw, artifact
        integer :: start, last, i, k
        logical :: matched
        character(len=1) :: ch

        key = nl//'build all: phony'
        if (present(phony)) key = nl//'build '//phony//': phony'
        start = index(ninja, key//' ')
        if (start == 0) start = index(ninja, key//nl)
        if (start == 0) then
            message = 'build.ninja has no default target'
            if (present(phony)) message = 'build.ninja has no phony '//phony
            return
        end if
        start = start + len(key)
        last = start + index(ninja(start:), nl) - 2
        if (last < start - 1) last = len(ninja)
        word = ''
        raw = ''
        i = start
        do while (i <= last + 1)
            ch = ' '
            if (i <= last) ch = ninja(i:i)
            if (ch == '$' .and. i < last) then
                if (ninja(i + 1:i + 1) == ' ' .or. ninja(i + 1:i + 1) == ':' .or. &
                    ninja(i + 1:i + 1) == '$') then
                    word = word//ninja(i + 1:i + 1)
                    raw = raw//ninja(i:i + 1)
                    i = i + 2
                    cycle
                end if
            end if
            if (ch == ' ') then
                if (len(word) > 0) then
                    matched = .false.
                    do k = 1, size(plan%targets)
                        artifact = plan%targets(k)%artifact
                        if (index(artifact, plan%build_root//'/') == 1) &
                            artifact = artifact(len(plan%build_root) + 2:)
                        if (plan%targets(k)%name == word .or. artifact == word .or. &
                            ends_with(word, '/'//plan%targets(k)%name)) then
                            wanted(k) = .true.
                            matched = .true.
                        end if
                    end do
                    if (.not. matched .and. len(word) > 4) then
                        if (word(len(word) - 3:) == '/all') then
                            call all_members(plan, ninja, wanted, message, raw)
                            if (len(message) > 0) return
                            matched = .true.
                        end if
                    end if
                    if (.not. matched) then
                        message = 'default target entry '//word// &
                            ' belongs to no known target'
                        return
                    end if
                    word = ''
                    raw = ''
                end if
            else
                word = word//ch
                raw = raw//ch
            end if
            i = i + 1
        end do
    end subroutine all_members

    subroutine object_library_source(plan, object, provider, source)
        !! The OBJECT_LIBRARY target and source index whose object CMake
        !! names `object` (`<dir>/CMakeFiles/<name>.dir/<path>.o`), or 0.
        type(cmake_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: object
        integer, intent(out) :: provider, source
        character(len=:), allocatable :: marker, rest, path
        integer :: t, s, at, n_found

        provider = 0
        source = 0
        if (.not. ends_with(object, '.o') .and. .not. ends_with(object, '.obj')) &
            return
        n_found = 0
        do t = 1, size(plan%targets)
            if (plan%targets(t)%type /= 'OBJECT_LIBRARY') cycle
            marker = '/CMakeFiles/'//plan%targets(t)%name//'.dir/'
            at = index(object, marker, back=.true.)
            if (at == 0) cycle
            rest = object(at + len(marker):index(object, '.', back=.true.) - 1)
            do while (index(rest, './') == 1)
                rest = rest(3:)
            end do
            do s = 1, plan%targets(t)%n_sources
                if (plan%targets(t)%src_generated(s)) cycle
                path = plan%targets(t)%sources(s)%s
                if (.not. ends_with(path, '/'//rest) .and. path /= rest) cycle
                n_found = n_found + 1
                provider = t
                source = s
            end do
        end do
        if (n_found /= 1) then
            provider = 0
            source = 0
        end if
    end subroutine object_library_source

    subroutine demote_object_consumers(route, log_file)
        !! A native target that consumes objects of an OBJECT_LIBRARY which is
        !! not built natively goes to CMake as well.
        type(cmake_route_t), intent(inout) :: route
        character(len=*), intent(in) :: log_file
        integer :: t, k
        logical :: changed, ok

        changed = .true.
        do while (changed)
            changed = .false.
            do t = 1, size(route%plan%targets)
                if (route%mode(t) /= MODE_NATIVE) cycle
                ok = .true.
                do k = 1, size(route%actions(t)%obj_target)
                    if (route%mode(route%actions(t)%obj_target(k)) /= MODE_NATIVE) &
                        ok = .false.
                end do
                if (ok) cycle
                route%mode(t) = MODE_DELEGATED
                route%n_delegated = route%n_delegated + 1
                route%delegated = [route%delegated(1:route%n_delegated - 1), &
                    string_t(route%plan%targets(t)%name)]
                call log_line(log_file, 'fo: delegated: '// &
                    route%plan%targets(t)%name// &
                    ' (consumes objects of a delegated object library)')
                changed = .true.
            end do
        end do
    end subroutine demote_object_consumers

    subroutine demote_conflicts(route, log_file)
        !! A source compiled with different actions by two native targets, or
        !! a module defined by two different sources, cannot share one native
        !! module scheduler: those targets go to CMake.
        type(cmake_route_t), intent(inout) :: route
        character(len=*), intent(in) :: log_file
        type(string_t), allocatable :: owner_file(:), owner_text(:), mod_names(:)
        integer, allocatable :: owner_target(:), mod_source(:), mod_target(:)
        logical, allocatable :: demote(:)
        type(scan_unit_t) :: unit
        integer :: t, k, slot, ntab, n_seen, mask, ierr, m
        character(len=:), allocatable :: text, source, lower
        integer(int64) :: h

        n_seen = 0
        do t = 1, size(route%plan%targets)
            if (route%mode(t) == MODE_NATIVE) n_seen = n_seen + &
                size(route%actions(t)%a)
        end do
        if (n_seen == 0) return
        ntab = 64
        do while (ntab < 4*n_seen)
            ntab = 2*ntab
        end do
        mask = ntab - 1
        allocate (owner_file(ntab), owner_text(ntab), owner_target(ntab))
        allocate (mod_names(ntab), mod_source(ntab), mod_target(ntab))
        owner_target = 0
        mod_target = 0
        allocate (demote(size(route%plan%targets)))
        demote = .false.
        do t = 1, size(route%plan%targets)
            if (route%mode(t) /= MODE_NATIVE) cycle
            do k = 1, size(route%actions(t)%a)
                source = route%plan%targets(t)%sources(route%actions(t)%source(k))%s
                text = action_text(route%actions(t)%a(k))
                h = fnv1a_string(source)
                slot = int(iand(h, int(mask, int64))) + 1
                do
                    if (owner_target(slot) == 0) then
                        owner_target(slot) = t
                        owner_file(slot)%s = source
                        owner_text(slot)%s = text
                        exit
                    end if
                    if (owner_file(slot)%s == source) then
                        if (owner_text(slot)%s /= text) then
                            demote(t) = .true.
                            demote(owner_target(slot)) = .true.
                        end if
                        exit
                    end if
                    slot = iand(slot, mask) + 1
                end do
                if (route%actions(t)%a(k)%language /= 'Fortran') cycle
                call scan_file_regex(source, unit, ierr)
                if (ierr /= 0) cycle
                call register_module(unit%module_name)
                if (allocated(unit%additional_modules)) then
                    do m = 1, size(unit%additional_modules)
                        call register_module(unit%additional_modules(m))
                    end do
                end if
            end do
        end do
        do t = 1, size(route%plan%targets)
            if (.not. demote(t)) cycle
            route%mode(t) = MODE_DELEGATED
            route%n_delegated = route%n_delegated + 1
            route%delegated = [route%delegated(1:route%n_delegated - 1), &
                string_t(route%plan%targets(t)%name)]
            call log_line(log_file, 'fo: delegated: '// &
                route%plan%targets(t)%name// &
                ' (source built differently or module defined twice across '// &
                'targets)')
        end do

    contains

        subroutine register_module(name)
            character(len=*), intent(in) :: name
            integer :: probe

            if (len_trim(name) == 0) return
            lower = lowercase(trim(name))
            probe = int(iand(fnv1a_string(lower), int(mask, int64))) + 1
            do
                if (mod_target(probe) == 0) then
                    mod_target(probe) = t
                    mod_names(probe)%s = lower
                    mod_source(probe) = k
                    exit
                end if
                if (mod_names(probe)%s == lower) then
                    if (mod_target(probe) /= t .or. mod_source(probe) /= k) then
                        demote(t) = .true.
                        demote(mod_target(probe)) = .true.
                    end if
                    exit
                end if
                probe = iand(probe, mask) + 1
            end do
        end subroutine register_module

    end subroutine demote_conflicts

    function lowercase(text) result(lower)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lower
        integer :: i, code

        lower = text
        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code >= 65 .and. code <= 90) lower(i:i) = achar(code + 32)
        end do
    end function lowercase

    function action_text(action) result(text)
        type(native_action_t), intent(in) :: action
        character(len=:), allocatable :: text

        text = action%language//nl//action%compiler//nl//action%mod_dir//nl// &
            action%argv//nl//action%search
    end function action_text

    subroutine cmake_build_native(route, log_file, exitcode)
        !! Compile, archive and link the natively routed targets. Delegated
        !! targets have been built before.
        type(cmake_route_t), intent(in) :: route
        character(len=*), intent(in) :: log_file
        integer, intent(out) :: exitcode
        type(scan_unit_t), allocatable :: units(:)
        type(native_action_t), allocatable :: actions(:)
        type(string_t), allocatable :: unit_files(:), unit_commands(:)
        character(len=512), allocatable :: objects(:)
        character(len=:), allocatable :: obj_dir, root, packed
        character(len=512) :: obj
        integer :: t, k, n, s, n_units, compiled, ierr, n_args, slot, ntab, mask
        integer, allocatable :: table(:)
        character(len=12) :: count_text
        logical :: found

        exitcode = 0
        if (route%n_native == 0) return
        root = route%plan%source_root
        obj_dir = route%plan%build_root//'/.fo-native-cmake/obj'
        call fs_make_dir(obj_dir)
        n = 0
        do t = 1, size(route%plan%targets)
            if (route%mode(t) == MODE_NATIVE) n = n + size(route%actions(t)%a)
        end do
        if (n > MAX_UNITS) then
            call native_failure(log_file, 'declared source capacity exceeded')
            exitcode = 1
            return
        end if
        allocate (units(n), actions(n), unit_files(n), unit_commands(n))
        ntab = 64
        do while (ntab < 4*n)
            ntab = 2*ntab
        end do
        mask = ntab - 1
        allocate (table(ntab))
        table = 0
        n_units = 0
        do t = 1, size(route%plan%targets)
            if (route%mode(t) /= MODE_NATIVE) cycle
            do k = 1, size(route%actions(t)%a)
                s = route%actions(t)%source(k)
                associate (source => route%plan%targets(t)%sources(s)%s)
                    slot = int(iand(fnv1a_string(source), int(mask, int64))) + 1
                    found = .false.
                    do while (table(slot) /= 0)
                        if (trim(units(table(slot))%filename) == source) then
                            found = .true.
                            exit
                        end if
                        slot = iand(slot, mask) + 1
                    end do
                    if (found) cycle
                    n_units = n_units + 1
                    table(slot) = n_units
                    actions(n_units) = route%actions(t)%a(k)
                    if (len(source) > len(units(n_units)%filename)) then
                        call native_failure(log_file, 'source path too long '//source)
                        exitcode = 1
                        return
                    end if
                    units(n_units)%filename = source
                    unit_files(n_units)%s = source
                    if (actions(n_units)%language /= 'Fortran') then
                        call name_unit(units(n_units), n_units)
                        cycle
                    end if
                    packed = ''
                    n_args = 0
                    call argv_push(packed, n_args, actions(n_units)%compiler)
                    call argv_push(packed, n_args, '-E')
                    call argv_push(packed, n_args, '-cpp')
                    call argv_push(packed, n_args, '-P')
                    call argv_push_split_nl(packed, n_args, actions(n_units)%argv)
                    if (index(nl//actions(n_units)%argv//nl, &
                        nl//'-nocpp'//nl) > 0) n_args = 0
                    ! The compiler, not the optional AST frontend, decides
                    ! what is valid Fortran: a source the frontend cannot
                    ! parse is scanned for module statements instead.
                    call scan_file_preprocessed(source, units(n_units), ierr, &
                        packed, n_args, allow_regex_fallback=.true.)
                    if (ierr /= 0) then
                        call native_failure(log_file, &
                            'cannot scan Fortran source '//source)
                        exitcode = 1
                        return
                    end if
                    units(n_units)%is_test = .false.
                    call name_unit(units(n_units), n_units)
                end associate
            end do
        end do
        call native_compile_actions(root, units(1:n_units), actions(1:n_units), &
            obj_dir, log_file, exitcode, compiled)
        if (exitcode /= 0) return
        write (count_text, '(i0)') compiled
        call log_line(log_file, 'fo: native CMake compiled '//trim(count_text)// &
            ' source file(s)')
        do k = 1, n_units
            call make_obj_path(unit_files(k)%s, root, obj_dir, obj)
            unit_commands(k)%s = actions(k)%compiler//nl//'-c'//nl// &
                actions(k)%argv//nl//'-o'//nl//trim(obj)//nl//unit_files(k)%s
        end do
        call compdb_write_commands(route%plan%build_root//'/compile_commands.json', &
            root, unit_files(1:n_units), unit_commands(1:n_units))
        do t = 1, size(route%plan%targets)
            if (route%mode(t) /= MODE_NATIVE) cycle
            if (route%plan%targets(t)%type /= 'STATIC_LIBRARY') cycle
            call target_objects(route, t, root, obj_dir, objects)
            call make_parent(route%plan%targets(t)%artifact)
            call native_archive_target(root, objects, &
                route%plan%targets(t)%artifact, log_file, exitcode)
            if (exitcode /= 0) return
        end do
        do t = 1, size(route%plan%targets)
            if (route%mode(t) /= MODE_NATIVE) cycle
            if (route%plan%targets(t)%type /= 'EXECUTABLE') cycle
            call link_executable(route, t, root, obj_dir, log_file, exitcode)
            if (exitcode /= 0) return
        end do
    end subroutine cmake_build_native

    subroutine name_unit(unit, index)
        !! The dependency graph has a node only for a unit that defines a
        !! module, procedure or program; a C source or a Fortran source that
        !! defines nothing gets a private node name so it is still compiled.
        type(scan_unit_t), intent(inout) :: unit
        integer, intent(in) :: index
        character(len=24) :: label

        if (len_trim(unit%module_name) > 0 .or. unit%is_program) return
        write (label, '(a,i0)') 'unit-', index
        unit%module_name = label
    end subroutine name_unit

    subroutine native_failure(log_file, message)
        character(len=*), intent(in) :: log_file, message

        write (error_unit, '(a)') 'native CMake: '//message
        call log_line(log_file, 'native CMake: '//message)
    end subroutine native_failure

    subroutine make_parent(path)
        character(len=*), intent(in) :: path
        integer :: slash

        slash = index(path, '/', back=.true.)
        if (slash > 1) call fs_make_dir(path(1:slash - 1))
    end subroutine make_parent

    subroutine target_objects(route, t, root, obj_dir, objects)
        type(cmake_route_t), intent(in) :: route
        integer, intent(in) :: t
        character(len=*), intent(in) :: root, obj_dir
        character(len=512), allocatable, intent(out) :: objects(:)
        integer :: k

        integer :: n_own

        n_own = size(route%actions(t)%a)
        allocate (objects(n_own + size(route%actions(t)%obj_target)))
        do k = 1, n_own
            call make_obj_path(route%plan%targets(t)%sources( &
                route%actions(t)%source(k))%s, root, obj_dir, objects(k))
        end do
        do k = 1, size(route%actions(t)%obj_target)
            call make_obj_path(route%plan%targets(route%actions(t)%obj_target(k))% &
                sources(route%actions(t)%obj_source(k))%s, root, obj_dir, &
                objects(n_own + k))
        end do
    end subroutine target_objects

    subroutine link_executable(route, t, root, obj_dir, log_file, exitcode)
        !! Link with the compiler of the link language and the target's link
        !! fragments: flags first, then the objects, then libraries and paths
        !! in CMake's order. Relative library files are relative to the build
        !! directory.
        type(cmake_route_t), intent(in) :: route
        integer, intent(in) :: t
        character(len=*), intent(in) :: root, obj_dir, log_file
        integer, intent(out) :: exitcode
        character(len=512), allocatable :: objects(:)
        character(len=:), allocatable :: argv, inputs, candidate
        type(string_t), allocatable :: words(:)
        integer :: i, j, n_words, provider, source
        character(len=512) :: native_object
        logical :: exists

        call target_objects(route, t, root, obj_dir, objects)
        argv = ''
        inputs = ''
        associate (tgt => route%plan%targets(t))
            do i = 1, tgt%n_link
                if (tgt%link_roles(i)%s /= 'flags') cycle
                call cmake_split_fragment(tgt%link_fragments(i)%s, words, n_words)
                do j = 1, n_words
                    call add_token(argv, words(j)%s)
                end do
            end do
            do i = 1, size(objects)
                call add_token(argv, trim(objects(i)))
                call add_token(inputs, trim(objects(i)))
            end do
            do i = 1, tgt%n_link
                if (tgt%link_roles(i)%s == 'flags') cycle
                call cmake_split_fragment(tgt%link_fragments(i)%s, words, n_words)
                do j = 1, n_words
                    candidate = words(j)%s
                    if (candidate(1:1) /= '-') then
                        candidate = absolute(candidate, route%plan%build_root)
                        ! Object-library inputs can occur in link fragments,
                        ! not only as generated sources. Use the same native
                        ! object vector as archives and source-list consumers.
                        call object_library_source(route%plan, candidate, &
                            provider, source)
                        if (provider > 0) then
                            if (route%mode(provider) == MODE_NATIVE) then
                                call make_obj_path(route%plan%targets(provider)% &
                                    sources(source)%s, root, obj_dir, native_object)
                                candidate = trim(native_object)
                                if (any(objects == candidate)) cycle
                            end if
                        end if
                        inquire (file=candidate, exist=exists)
                        if (exists) then
                            call add_token(inputs, candidate)
                        else
                            candidate = words(j)%s
                        end if
                    end if
                    call add_token(argv, candidate)
                end do
            end do
            call make_parent(tgt%artifact)
            call native_link_exact(route%plan%build_root, &
                tool(route%plan, tgt%link_language), argv, inputs, &
                tgt%artifact, log_file, exitcode)
        end associate
    end subroutine link_executable

    subroutine log_line(log_file, text)
        !! Append to the build log and to the route record of the build tree,
        !! which a later reader can inspect without the transient log.
        character(len=*), intent(in) :: log_file, text
        integer :: unit, ios

        open (newunit=unit, file=log_file, status='unknown', position='append', &
            action='write', iostat=ios)
        if (ios == 0) then
            write (unit, '(a)') text
            close (unit)
        end if
        if (len_trim(route_file) == 0) return
        open (newunit=unit, file=trim(route_file), status='unknown', &
            position='append', action='write', iostat=ios)
        if (ios /= 0) return
        write (unit, '(a)') text
        close (unit)
    end subroutine log_line

end module fo_cmake_build
