module fo_cmake_model
    !! CMake adapter for the backend-neutral project model, part 1: targets.
    !!
    !! Reads the File API reply of a configured tree (index -> codemodel-v2 ->
    !! target-*.json) into a project_model_t. A missing or stale reply (no
    !! codemodel object, or a reply that does not carry the request token of
    !! the context, i.e. CMake has not re-run since fo wrote its query) is an
    !! explicit error; the model is then left unusable.
    !!
    !! Targets:
    !!   STATIC/SHARED/MODULE/OBJECT/INTERFACE_LIBRARY -> TARGET_LIBRARY,
    !!   EXECUTABLE -> TARGET_EXECUTABLE, everything else (UTILITY, ...) ->
    !!   TARGET_OTHER. link_deps are the target `dependencies` (link and
    !!   add_dependencies edges between codemodel targets, as CMake reports
    !!   them, i.e. the transitive link closure; imported targets
    !!   are not in the codemodel). The artifact is the first artifact path
    !!   made absolute against the build directory. Sources are absolute.
    !!
    !! Origin rule: a target is ORIGIN_DEPENDENCY unless its source directory
    !! (paths.source of the target, resolved against the top-level source
    !! directory) is inside the top-level source directory and outside the
    !! build directory. FetchContent trees (build/_deps/...) and
    !! add_subdirectory(<outside>) therefore are dependencies; a vendored copy
    !! inside the source tree is project code.
    !!
    !! Layers: every project library is a layer named like the target, whose
    !! `uses` are its project library dependencies (transitive, as above).
    !! Layers are only built for project targets. The model is marked complete
    !! when the codemodel was read without gaps.
    !!
    !! Cases (cmake_model_load_cases, `ctest --show-only=json-v1`, no size
    !! limit): name, LABELS as labels, every other property as an attr (array
    !! values joined with ';'). command_target is the target whose artifact
    !! equals a command word (mapping exact); a script or unknown command
    !! leaves it unknown, which widens the plan. Default exclusions: with a
    !! test preset the preset listing is authoritative and cases absent from
    !! it (listed via `--test-dir`) are flagged default_excluded with their
    !! labels kept; without a preset the model default_exclude is [slow].
    !! CTest reports a test command only once its executable exists. For the
    !! cases without a reported command the generated CTestTestfile.cmake
    !! files (CTest's own add_test declarations, followed through subdirs()
    !! and include()) supply the command, so an unbuilt tree maps its cases
    !! to targets as well. A command that names no target artifact (a script)
    !! stays unknown, which widens the plan.
    use fx_string, only: string_t
    use fx_hash, only: fnv1a_string
    use fx_json_parse, only: json_parser_t, json_event_t, &
        json_parser_init_strict, json_parser_next, JSON_OBJECT_START, &
        JSON_OBJECT_END, JSON_ARRAY_START, JSON_ARRAY_END, JSON_KEY, &
        JSON_STRING, JSON_INTEGER, JSON_REAL, JSON_BOOL, JSON_ERROR, &
        JSON_END_OF_INPUT
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_process, only: argv_push, process_run_argv_logged
    use fo_cmake_context, only: cmake_context_t, cmake_context_read_reply, &
        cmake_context_read_file, cmake_context_build_path
    use fo_project_model, only: project_model_t, model_init, model_add_target, &
        model_add_layer, model_add_case, model_find_artifact, model_find_layer, &
        model_find_target, model_find_case, &
        model_attr_t, model_finalize, MAP_EXACT, TARGET_OTHER, TARGET_LIBRARY, &
        TARGET_EXECUTABLE, ORIGIN_PROJECT, ORIGIN_DEPENDENCY
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    private

    integer, parameter :: dp = real64

    public :: cmake_model_load, cmake_model_load_cases, cmake_model_target_origin
    public :: cmake_plan_load, cmake_plan_t, cmake_target_t, cmake_group_t
    public :: cmake_split_fragment

    type :: walker_t
        !! Tracks the JSON path ('configurations/#/targets/#/name').
        character(len=:), allocatable :: path
        integer, allocatable :: marks(:)
        logical, allocatable :: is_array(:)
        integer :: depth = 0
        character(len=:), allocatable :: key
        logical :: pop_pending = .false.
    end type walker_t

    type :: cmake_group_t
        !! One codemodel compile group: the compile command fragments,
        !! include directories, defines and language shared by some sources.
        character(len=:), allocatable :: language
        type(string_t), allocatable :: fragments(:), includes(:), defines(:)
        logical, allocatable :: include_system(:)
        integer :: n_fragments = 0, n_includes = 0, n_defines = 0
    end type cmake_group_t

    type :: cmake_target_t
        character(len=:), allocatable :: id, name, json_file, type
        character(len=:), allocatable :: source_dir, artifact
        type(string_t), allocatable :: sources(:), dep_ids(:)
        integer :: n_sources = 0, n_deps = 0
        !! Native-build facts: per source its compile group (0 = not compiled)
        !! and generated flag, link/archive fragments with their roles.
        character(len=:), allocatable :: build_dir, link_language
        integer, allocatable :: src_group(:)
        logical, allocatable :: src_generated(:)
        type(cmake_group_t), allocatable :: groups(:)
        integer :: n_groups = 0
        type(string_t), allocatable :: link_fragments(:), link_roles(:)
        type(string_t), allocatable :: archive_fragments(:), dep_names(:)
        integer :: n_link = 0, n_archive = 0
    end type cmake_target_t

    type :: cmake_plan_t
        !! The configured tree as the native builder needs it.
        character(len=:), allocatable :: source_root, build_root
        type(cmake_target_t), allocatable :: targets(:)
        type(string_t), allocatable :: tool_languages(:), tool_compilers(:)
    end type cmake_plan_t

contains

    subroutine cmake_model_load(context, model, ierr, message, input_digest)
        !! ierr 0 = model finalized and complete; otherwise message says why.
        type(cmake_context_t), intent(inout) :: context
        type(project_model_t), intent(out) :: model
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        character(len=*), intent(in), optional :: input_digest
        character(len=:), allocatable :: toolchains_file
        character(len=:), allocatable :: top_source, top_build, digest
        type(cmake_target_t), allocatable :: recs(:)
        integer :: n, i, k, j, slot, ntab
        integer, allocatable :: table(:)
        type(string_t), allocatable :: deps(:), uses(:), names(:)
        integer :: origin, kind, nd, nu
        integer, allocatable :: origins(:), kinds(:)

        ierr = 1
        message = ''
        digest = ''
        if (present(input_digest)) digest = input_digest
        call model_init(model, 'cmake', digest, &
            build_dir=cmake_context_build_path(context), &
            preset=trim(context%configure_preset), &
            configuration=trim(context%configuration))
        call read_records(context, recs, n, top_source, top_build, &
            toolchains_file, message)
        if (len(message) > 0) return

        ! id -> record index (open addressing; ids are unique in a codemodel).
        ntab = 64
        do while (ntab < 4*max(n, 1))
            ntab = 2*ntab
        end do
        allocate (table(ntab))
        table = 0
        do i = 1, n
            slot = id_slot(table, recs, recs(i)%id)
            if (table(slot) /= 0) then
                message = 'duplicate CMake target id '//recs(i)%id
                return
            end if
            table(slot) = i
        end do

        allocate (origins(n), kinds(n))
        do i = 1, n
            origins(i) = cmake_model_target_origin(recs(i)%source_dir, top_source, &
                top_build)
            kinds(i) = kind_of(recs(i)%type)
        end do

        do i = 1, n
            nd = recs(i)%n_deps
            allocate (deps(nd))
            k = 0
            do j = 1, nd
                slot = table(id_slot(table, recs, recs(i)%dep_ids(j)%s))
                if (slot == 0) cycle
                k = k + 1
                deps(k)%s = recs(slot)%name
            end do
            call model_add_target(model, recs(i)%name, kinds(i), &
                sources=recs(i)%sources(1:recs(i)%n_sources), &
                link_deps=deps(1:k), artifact=recs(i)%artifact, origin=origins(i))
            deallocate (deps)
        end do

        do i = 1, n
            if (origins(i) /= ORIGIN_PROJECT .or. kinds(i) /= TARGET_LIBRARY) cycle
            nd = recs(i)%n_deps
            allocate (uses(nd))
            nu = 0
            do j = 1, nd
                slot = table(id_slot(table, recs, recs(i)%dep_ids(j)%s))
                if (slot == 0) cycle
                if (origins(slot) /= ORIGIN_PROJECT .or. &
                    kinds(slot) /= TARGET_LIBRARY) cycle
                nu = nu + 1
                uses(nu)%s = recs(slot)%name
            end do
            allocate (names(1))
            names(1)%s = recs(i)%name
            call model_add_layer(model, recs(i)%name, uses=uses(1:nu), &
                targets=names)
            deallocate (uses, names)
        end do

        call cmake_model_load_cases(context, model, ierr, message)
        if (ierr /= 0) return
        call assign_case_layers(model)
        model%complete = .true.
        call model_finalize(model, ierr, message)
        if (ierr /= 0) model%complete = .false.
    end subroutine cmake_model_load

    subroutine read_records(context, recs, n, top_source, top_build, &
            toolchains_file, message)
        !! The codemodel target records of the current configuration, fully
        !! parsed. A non-empty message says why the reply is unusable.
        type(cmake_context_t), intent(inout) :: context
        type(cmake_target_t), allocatable, intent(out) :: recs(:)
        integer, intent(out) :: n
        character(len=:), allocatable, intent(out) :: top_source, top_build
        character(len=:), allocatable, intent(out) :: toolchains_file, message
        character(len=:), allocatable :: reply_dir, codemodel_file, text
        integer :: i
        logical :: ok

        message = ''
        n = 0
        call cmake_context_read_reply(context)
        if (.not. allocated(context%reply_index)) then
            message = 'CMake File API reply is missing; configure the tree first'
            return
        end if
        if (len(context%reply_index) == 0 .or. .not. context%has_codemodel) then
            message = 'CMake File API reply has no codemodel; configure the tree first'
            return
        end if
        if (.not. context%request_seen) then
            message = 'CMake File API reply is stale (CMake has not re-run since '// &
                'the fo query was written); reconfigure the tree'
            return
        end if
        reply_dir = context%reply_index(1:index(context%reply_index, '/', &
            back=.true.) - 1)

        call cmake_context_read_file(context%reply_index, text)
        if (.not. allocated(text)) then
            message = 'cannot read CMake File API index '//context%reply_index
            return
        end if
        call parse_index(text, codemodel_file, toolchains_file, ok)
        if (.not. ok) then
            message = 'CMake File API index has no codemodel-v2 object'
            return
        end if
        toolchains_file = reply_dir//'/'//toolchains_file
        call cmake_context_read_file(reply_dir//'/'//codemodel_file, text)
        if (.not. allocated(text)) then
            message = 'cannot read CMake codemodel '//codemodel_file
            return
        end if
        call parse_codemodel(text, context%configuration, top_source, top_build, &
            recs, n, ok)
        if (.not. ok) then
            message = 'CMake codemodel '//codemodel_file//' is malformed'
            return
        end if
        top_source = strip_slash(top_source)
        top_build = strip_slash(top_build)

        do i = 1, n
            call cmake_context_read_file(reply_dir//'/'//recs(i)%json_file, text)
            if (.not. allocated(text)) then
                message = 'cannot read CMake target file '//recs(i)%json_file
                return
            end if
            call parse_target(text, top_source, top_build, recs(i), ok)
            if (.not. ok) then
                message = 'CMake target file '//recs(i)%json_file//' is malformed'
                return
            end if
        end do
    end subroutine read_records

    subroutine cmake_plan_load(context, plan, message)
        !! Targets with compile groups and link fragments plus the toolchain
        !! compilers, for building natively. message is empty on success.
        type(cmake_context_t), intent(inout) :: context
        type(cmake_plan_t), intent(out) :: plan
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: top_source, top_build, toolchains_file, text
        integer :: n, i, j, k
        type(string_t), allocatable :: names(:)

        call read_records(context, plan%targets, n, top_source, top_build, &
            toolchains_file, message)
        if (len(message) > 0) return
        plan%source_root = top_source
        plan%build_root = top_build
        plan%targets = plan%targets(1:n)
        allocate (plan%tool_languages(0), plan%tool_compilers(0))
        call cmake_context_read_file(toolchains_file, text)
        if (allocated(text)) call parse_toolchains(text, plan%tool_languages, &
            plan%tool_compilers)
        do i = 1, n
            allocate (names(plan%targets(i)%n_deps))
            k = 0
            do j = 1, plan%targets(i)%n_deps
                do k = 1, n
                    if (plan%targets(k)%id == plan%targets(i)%dep_ids(j)%s) exit
                end do
                names(j)%s = ''
                if (k <= n) names(j)%s = plan%targets(k)%name
            end do
            call move_alloc(names, plan%targets(i)%dep_names)
        end do
    end subroutine cmake_plan_load

    subroutine parse_toolchains(text, languages, compilers)
        character(len=*), intent(in) :: text
        type(string_t), allocatable, intent(inout) :: languages(:), compilers(:)
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        type(walker_t) :: w
        character(len=:), allocatable :: path, language, compiler
        integer :: n

        n = 0
        language = ''
        compiler = ''
        call json_parser_init_strict(parser, text)
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_END_OF_INPUT) exit
            if (event%event_type == JSON_ERROR) return
            call walker_step(w, event, path)
            select case (event%event_type)
            case (JSON_OBJECT_START)
                if (path == 'toolchains/#') then
                    language = ''
                    compiler = ''
                end if
            case (JSON_STRING)
                if (path == 'toolchains/#/language') language = event%string_val
                if (path == 'toolchains/#/compiler/path') compiler = event%string_val
            case (JSON_OBJECT_END)
                if (path == 'toolchains/#' .and. len(language) > 0 .and. &
                    len(compiler) > 0) then
                    call push_string(compilers, n, compiler)
                    n = n - 1
                    call push_string(languages, n, language)
                end if
            end select
        end do
        languages = copy_strings(languages, n)
        compilers = copy_strings(compilers, n)
    end subroutine parse_toolchains

    function copy_strings(items, n) result(copy)
        type(string_t), intent(in) :: items(:)
        integer, intent(in) :: n
        type(string_t), allocatable :: copy(:)
        integer :: i

        allocate (copy(n))
        do i = 1, n
            copy(i)%s = items(i)%s
        end do
    end function copy_strings

    subroutine cmake_split_fragment(text, tokens, n)
        !! Split a compile or link command fragment into argv words, honouring
        !! the double quotes and backslash escapes CMake writes into it.
        character(len=*), intent(in) :: text
        type(string_t), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: n
        character(len=:), allocatable :: word
        character(len=1) :: ch, quote
        logical :: escaped, in_word
        integer :: i

        n = 0
        allocate (tokens(8))
        word = ''
        quote = ' '
        escaped = .false.
        in_word = .false.
        do i = 1, len(text) + 1
            ch = ' '
            if (i <= len(text)) ch = text(i:i)
            if (escaped) then
                word = word//ch
                escaped = .false.
            else if (ch == achar(92) .and. i <= len(text)) then
                escaped = .true.
                in_word = .true.
            else if (quote /= ' ') then
                if (ch == quote) then
                    quote = ' '
                else
                    word = word//ch
                end if
            else if (ch == '"' .or. ch == "'") then
                quote = ch
                in_word = .true.
            else if (ch == ' ' .or. ch == achar(9) .or. ch == achar(10)) then
                if (in_word) call push_string(tokens, n, word)
                word = ''
                in_word = .false.
            else
                word = word//ch
                in_word = .true.
            end if
        end do
    end subroutine cmake_split_fragment

    subroutine assign_case_layers(model)
        !! Layer of a case, as in the fpm adapter: the highest-rank layer
        !! (rank 0 without `uses`, else 1 + the highest rank used; ties go to
        !! the layer declared first) among the project libraries in the link
        !! closure of the case's command target. Runs before finalize.
        type(project_model_t), intent(inout) :: model
        integer, allocatable :: rank(:), stack(:), target_layer(:)
        logical, allocatable :: seen(:)
        integer :: i, j, k, l, u, top, best, t, changed

        if (model%n_layers == 0) return
        allocate (rank(model%n_layers))
        rank = 0
        do changed = 1, model%n_layers
            do l = 1, model%n_layers
                do k = 1, size(model%layers(l)%use_names)
                    u = model_find_layer(model, model%layers(l)%use_names(k)%s)
                    if (u > 0) rank(l) = max(rank(l), rank(u) + 1)
                end do
            end do
        end do
        allocate (seen(model%n_targets), stack(model%n_targets))
        allocate (target_layer(model%n_targets))
        target_layer = -1
        do i = 1, model%n_cases
            if (model%cases(i)%command_target == 0) cycle
            if (target_layer(model%cases(i)%command_target) >= 0) then
                model%cases(i)%layer = target_layer(model%cases(i)%command_target)
                cycle
            end if
            seen = .false.
            top = 1
            stack(1) = model%cases(i)%command_target
            seen(stack(1)) = .true.
            best = 0
            do while (top > 0)
                t = stack(top)
                top = top - 1
                l = model_find_layer(model, model%targets(t)%name)
                if (l > 0) then
                    if (best == 0) then
                        best = l
                    else if (rank(l) > rank(best)) then
                        best = l
                    else if (rank(l) == rank(best) .and. l < best) then
                        best = l
                    end if
                end if
                do j = 1, size(model%targets(t)%link_dep_names)
                    u = model_find_target(model, model%targets(t)%link_dep_names(j)%s)
                    if (u == 0) cycle
                    if (seen(u)) cycle
                    seen(u) = .true.
                    top = top + 1
                    stack(top) = u
                end do
            end do
            target_layer(model%cases(i)%command_target) = best
            model%cases(i)%layer = best
        end do
    end subroutine assign_case_layers

    subroutine cmake_model_load_cases(context, model, ierr, message)
        !! Add the registered CTest cases to a model that is not finalized.
        !! Targets already in the model are used to map command words; with
        !! none every case is mapped unknown. Needs no File API reply.
        type(cmake_context_t), intent(in) :: context
        type(project_model_t), intent(inout) :: model
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: text
        logical :: has_preset

        ierr = 1
        message = ''
        has_preset = len_trim(context%test_preset) > 0
        call run_ctest(context, has_preset, text, message)
        if (.not. allocated(text)) return
        call parse_ctest(text, model, .false., .false., message)
        if (len(message) > 0) return
        if (has_preset) then
            call run_ctest(context, .false., text, message)
            if (.not. allocated(text)) return
            call parse_ctest(text, model, .true., .true., message)
            if (len(message) > 0) return
        else
            deallocate (model%default_exclude)
            allocate (model%default_exclude(1))
            model%default_exclude(1)%s = 'slow'
        end if
        call map_declared_commands(context, model)
        ierr = 0
    end subroutine cmake_model_load_cases

    subroutine map_declared_commands(context, model)
        !! Map cases that CTest listed without a command through the
        !! add_test declarations of the generated CTestTestfile.cmake tree.
        type(cmake_context_t), intent(in) :: context
        type(project_model_t), intent(inout) :: model
        integer :: k
        logical :: unmapped

        unmapped = .false.
        do k = 1, model%n_cases
            if (model%cases(k)%command_target == 0) unmapped = .true.
        end do
        if (.not. unmapped) return
        call scan_testfile(cmake_context_build_path(context)// &
            '/CTestTestfile.cmake', model)
    end subroutine map_declared_commands

    recursive subroutine scan_testfile(path, model)
        !! Walk one CTestTestfile.cmake: add_test maps a case, subdirs() and
        !! include() are followed. Missing files are skipped silently (an
        !! include of a not yet generated discovery file).
        character(len=*), intent(in) :: path
        type(project_model_t), intent(inout) :: model
        character(len=:), allocatable :: text, command, dir
        type(string_t), allocatable :: args(:)
        integer :: pos, n_args, k, slash, t, target_index

        call cmake_context_read_file(path, text)
        if (.not. allocated(text)) return
        slash = index(path, '/', back=.true.)
        dir = '.'
        if (slash > 1) dir = path(:slash - 1)
        pos = 1
        do
            call next_command(text, pos, command, args, n_args)
            if (len(command) == 0) exit
            select case (command)
            case ('add_test')
                if (n_args < 2) cycle
                k = model_find_case(model, args(1)%s)
                if (k == 0) cycle
                if (model%cases(k)%command_target /= 0) cycle
                do t = 2, n_args
                    target_index = model_find_artifact(model, args(t)%s)
                    if (target_index == 0) cycle
                    model%cases(k)%command_target = target_index
                    model%cases(k)%mapping = MAP_EXACT
                    exit
                end do
            case ('subdirs')
                do t = 1, n_args
                    if (len(args(t)%s) == 0) cycle
                    if (args(t)%s(1:1) == '/') then
                        call scan_testfile(args(t)%s//'/CTestTestfile.cmake', model)
                    else
                        call scan_testfile(dir//'/'//args(t)%s// &
                            '/CTestTestfile.cmake', model)
                    end if
                end do
            case ('include')
                if (n_args < 1) cycle
                if (len(args(1)%s) == 0) cycle
                if (args(1)%s(1:1) == '/') then
                    call scan_testfile(args(1)%s, model)
                else
                    call scan_testfile(dir//'/'//args(1)%s, model)
                end if
            end select
        end do
    end subroutine scan_testfile

    subroutine next_command(text, pos, command, args, n_args)
        !! Next command invocation of a CMake script: its name (empty at the
        !! end of the text) and its top-level arguments (quoted, bracket and
        !! unquoted forms, nested parentheses skipped). pos advances.
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        character(len=:), allocatable, intent(out) :: command
        type(string_t), allocatable, intent(out) :: args(:)
        integer, intent(out) :: n_args
        character(len=:), allocatable :: word, value
        integer :: n, start, depth, level, close_at
        character(len=1) :: c

        n = len(text)
        n_args = 0
        command = ''
        do while (pos <= n)
            c = text(pos:pos)
            if (c == '#') then
                call skip_line(text, pos)
                cycle
            end if
            if (.not. is_name_char(c)) then
                pos = pos + 1
                cycle
            end if
            start = pos
            do while (pos <= n)
                if (.not. is_name_char(text(pos:pos))) exit
                pos = pos + 1
            end do
            word = text(start:pos - 1)
            do while (pos <= n)
                if (.not. is_space(text(pos:pos))) exit
                pos = pos + 1
            end do
            if (pos > n) return
            if (text(pos:pos) /= '(') cycle
            pos = pos + 1
            depth = 0
            do while (pos <= n)
                c = text(pos:pos)
                if (is_space(c)) then
                    pos = pos + 1
                else if (c == '#') then
                    call skip_line(text, pos)
                else if (c == '(') then
                    depth = depth + 1
                    pos = pos + 1
                else if (c == ')') then
                    pos = pos + 1
                    if (depth == 0) exit
                    depth = depth - 1
                else if (c == '"') then
                    call quoted_argument(text, pos, value)
                    if (depth == 0) call push_string(args, n_args, value)
                else if (c == '[' .and. bracket_level(text, pos) >= 0) then
                    level = bracket_level(text, pos)
                    close_at = index(text(pos:), ']'//repeat('=', level)//']')
                    if (close_at == 0) then
                        pos = n + 1
                        exit
                    end if
                    start = pos + level + 2
                    if (start <= n) then
                        if (text(start:start) == new_line('a')) start = start + 1
                    end if
                    if (depth == 0) call push_string(args, n_args, &
                        text(start:pos + close_at - 2))
                    pos = pos + close_at + level + 1
                else
                    start = pos
                    do while (pos <= n)
                        c = text(pos:pos)
                        if (is_space(c) .or. c == '(' .or. c == ')' .or. &
                            c == '"' .or. c == '#') exit
                        pos = pos + 1
                    end do
                    if (pos == start) then
                        pos = pos + 1
                    else if (depth == 0) then
                        call push_string(args, n_args, text(start:pos - 1))
                    end if
                end if
            end do
            command = word
            return
        end do
    end subroutine next_command

    subroutine skip_line(text, pos)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos

        do while (pos <= len(text))
            if (text(pos:pos) == new_line('a')) return
            pos = pos + 1
        end do
    end subroutine skip_line

    subroutine quoted_argument(text, pos, value)
        !! Quoted CMake argument starting at the opening quote.
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        character(len=:), allocatable, intent(out) :: value
        character(len=1) :: c
        integer :: n

        n = len(text)
        value = ''
        pos = pos + 1
        do while (pos <= n)
            c = text(pos:pos)
            if (c == '"') then
                pos = pos + 1
                return
            end if
            if (c == achar(92) .and. pos < n) then
                pos = pos + 1
                c = text(pos:pos)
                if (c == new_line('a')) then
                    pos = pos + 1
                    cycle
                end if
                if (c == 'n') c = new_line('a')
                if (c == 't') c = achar(9)
            end if
            value = value//c
            pos = pos + 1
        end do
    end subroutine quoted_argument

    integer function bracket_level(text, pos) result(level)
        !! Number of '=' of a bracket opening [==[ at pos, -1 if none.
        character(len=*), intent(in) :: text
        integer, intent(in) :: pos
        integer :: k

        level = -1
        k = pos + 1
        do while (k <= len(text))
            if (text(k:k) /= '=') exit
            k = k + 1
        end do
        if (k > len(text)) return
        if (text(k:k) == '[') level = k - pos - 1
    end function bracket_level

    logical function is_name_char(c) result(yes)
        character(len=1), intent(in) :: c
        yes = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. &
            (c >= '0' .and. c <= '9') .or. c == '_'
    end function is_name_char

    logical function is_space(c) result(yes)
        character(len=1), intent(in) :: c
        yes = c == ' ' .or. c == achar(9) .or. c == new_line('a') .or. c == achar(13)
    end function is_space

    subroutine run_ctest(context, with_preset, text, message)
        !! Output of `ctest --show-only=json-v1` selected by the test preset
        !! or by --test-dir (and -C), unallocated on failure.
        type(cmake_context_t), intent(in) :: context
        logical, intent(in) :: with_preset
        character(len=:), allocatable, intent(out) :: text
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: packed
        character(len=4096) :: log_file
        integer :: n_args, ierr

        message = ''
        call make_tmpfile('fo-cmake-registered-tests', log_file)
        packed = ''
        n_args = 0
        call argv_push(packed, n_args, 'ctest')
        if (with_preset) then
            call argv_push(packed, n_args, '--preset')
            call argv_push(packed, n_args, trim(context%test_preset))
        else
            call argv_push(packed, n_args, '--test-dir')
            call argv_push(packed, n_args, cmake_context_build_path(context))
        end if
        if (len_trim(context%configuration) > 0) then
            call argv_push(packed, n_args, '-C')
            call argv_push(packed, n_args, trim(context%configuration))
        end if
        call argv_push(packed, n_args, '--show-only=json-v1')
        call process_run_argv_logged(trim(context%source_root), packed, n_args, &
            log_file, .false., 30, ierr)
        if (ierr == 0) call cmake_context_read_file(trim(log_file), text)
        if (ierr /= 0 .or. .not. allocated(text)) then
            message = 'cannot query registered CTest inventory: '//trim(log_file)
            if (allocated(text)) deallocate (text)
            return
        end if
        call delete_tmpfile(log_file)
    end subroutine run_ctest

    subroutine parse_ctest(text, model, flag_cases, skip_known, message)
        !! Add the tests of a json-v1 listing. skip_known: ignore names the
        !! model already has. flag_cases: set default_excluded on added cases.
        !! message is empty on success.
        character(len=*), intent(in) :: text
        type(project_model_t), intent(inout) :: model
        logical, intent(in) :: flag_cases, skip_known
        character(len=:), allocatable, intent(out) :: message
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        type(walker_t) :: w
        character(len=:), allocatable :: path, name, prop, value, word
        type(string_t), allocatable :: labels(:)
        type(string_t), allocatable :: argv(:), items(:)
        type(model_attr_t), allocatable :: attrs(:)
        integer :: n_labels, n_attrs, n_argv, n_items, command_target, hit, k
        logical :: in_value

        message = ''
        call json_parser_init_strict(parser, text)
        name = ''
        prop = ''
        value = ''
        in_value = .false.
        n_labels = 0
        n_attrs = 0
        n_argv = 0
        n_items = 0
        command_target = 0
        allocate (labels(0), attrs(0), argv(0), items(0))
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_END_OF_INPUT) exit
            if (event%event_type == JSON_ERROR) then
                message = 'invalid CTest registered-test JSON'
                return
            end if
            call walker_step(w, event, path)
            select case (event%event_type)
            case (JSON_OBJECT_START)
                if (path == 'tests/#') then
                    name = ''
                    n_labels = 0
                    n_attrs = 0
                    n_argv = 0
                    command_target = 0
                    deallocate (labels, attrs, argv)
                    allocate (labels(8), attrs(8), argv(8))
                else if (path == 'tests/#/properties/#') then
                    prop = ''
                    value = ''
                    in_value = .false.
                    n_items = 0
                end if
            case (JSON_STRING, JSON_INTEGER, JSON_REAL, JSON_BOOL)
                word = scalar_text(event)
                select case (path)
                case ('tests/#/name')
                    name = word
                case ('tests/#/command/#')
                    call push_string(argv, n_argv, word)
                    if (command_target == 0) &
                        command_target = model_find_artifact(model, word)
                case ('tests/#/properties/#/name')
                    prop = word
                case ('tests/#/properties/#/value')
                    value = word
                    in_value = .true.
                    n_items = 0
                    call push_string(items, n_items, word)
                case ('tests/#/properties/#/value/#')
                    call push_string(items, n_items, word)
                    if (prop == 'LABELS') then
                        call push_string(labels, n_labels, word)
                    else if (in_value) then
                        value = value//';'//word
                    else
                        value = word
                        in_value = .true.
                    end if
                end select
            case (JSON_ARRAY_START)
                if (path == 'tests/#/properties/#/value') then
                    value = ''
                    in_value = .false.
                end if
            case (JSON_OBJECT_END)
                if (path == 'tests/#/properties/#') then
                    if (len(prop) > 0 .and. prop /= 'LABELS') &
                        call push_attr(attrs, n_attrs, prop, value, items(1:n_items))
                    if (prop == 'LABELS' .and. in_value) then
                        ! A scalar LABELS value is a CMake list.
                        k = 1
                        do while (k <= len(value))
                            hit = index(value(k:), ';')
                            if (hit == 0) hit = len(value) - k + 2
                            if (hit > 1) &
                                call push_string(labels, n_labels, &
                                value(k:k + hit - 2))
                            k = k + hit
                        end do
                    end if
                else if (path == 'tests/#') then
                    if (len(name) == 0) then
                        message = 'CTest listing has a test without a name'
                        return
                    end if
                    if (skip_known) then
                        if (model_find_case(model, name) > 0) cycle
                    end if
                    call model_add_case(model, name, &
                        command_target=command_target, command=argv(1:n_argv), &
                        labels=labels(1:n_labels), attrs=attrs(1:n_attrs), &
                        default_excluded=flag_cases)
                end if
            end select
        end do
    end subroutine parse_ctest

    function scalar_text(event) result(r)
        type(json_event_t), intent(in) :: event
        character(len=:), allocatable :: r
        character(len=64) :: buffer

        select case (event%event_type)
        case (JSON_INTEGER)
            write (buffer, '(i0)') event%int_val
            r = trim(buffer)
        case (JSON_REAL)
            if (abs(event%real_val) < 1.0e15_dp .and. &
                event%real_val == anint(event%real_val)) then
                write (buffer, '(i0)') nint(event%real_val, int64)
            else
                write (buffer, '(g0)') event%real_val
            end if
            r = trim(buffer)
        case (JSON_BOOL)
            r = merge('true ', 'false', event%bool_val)
            r = trim(r)
        case default
            r = event%string_val
        end select
    end function scalar_text

    subroutine push_attr(arr, n, key, value, items)
        type(model_attr_t), allocatable, intent(inout) :: arr(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: key, value
        type(string_t), intent(in) :: items(:)
        type(model_attr_t), allocatable :: grown(:)

        if (n + 1 > size(arr)) then
            allocate (grown(2*size(arr)))
            grown(1:n) = arr(1:n)
            call move_alloc(grown, arr)
        end if
        n = n + 1
        arr(n)%key = key
        arr(n)%value = value
        arr(n)%items = items
    end subroutine push_attr

    function cmake_model_target_origin(source_dir, top_source, top_build) &
            result(origin)
        !! ORIGIN_PROJECT iff source_dir is inside top_source and not inside
        !! top_build (see the module header).
        character(len=*), intent(in) :: source_dir, top_source, top_build
        integer :: origin

        origin = ORIGIN_DEPENDENCY
        if (is_within(source_dir, top_build)) return
        if (is_within(source_dir, top_source)) origin = ORIGIN_PROJECT
    end function cmake_model_target_origin

    logical function is_within(path, root)
        character(len=*), intent(in) :: path, root

        is_within = .false.
        if (len(root) == 0) return
        if (len(path) < len(root)) return
        if (path(1:len(root)) /= root) return
        if (len(path) == len(root)) then
            is_within = .true.
        else
            is_within = path(len(root) + 1:len(root) + 1) == '/'
        end if
    end function is_within

    function strip_slash(path) result(r)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: r
        integer :: n

        n = len(path)
        do while (n > 1)
            if (path(n:n) /= '/') exit
            n = n - 1
        end do
        r = path(1:n)
    end function strip_slash

    function absolute_path(path, base) result(r)
        character(len=*), intent(in) :: path, base
        character(len=:), allocatable :: r

        if (len(path) > 0 .and. path(1:1) == '/') then
            r = strip_slash(path)
        else if (len(path) == 0 .or. path == '.') then
            r = base
        else
            r = base//'/'//path
        end if
    end function absolute_path

    integer function kind_of(type_name) result(kind)
        character(len=*), intent(in) :: type_name

        select case (type_name)
        case ('STATIC_LIBRARY', 'SHARED_LIBRARY', 'MODULE_LIBRARY', &
                'OBJECT_LIBRARY', 'INTERFACE_LIBRARY')
            kind = TARGET_LIBRARY
        case ('EXECUTABLE')
            kind = TARGET_EXECUTABLE
        case default
            kind = TARGET_OTHER
        end select
    end function kind_of

    integer function id_slot(table, recs, id) result(slot)
        !! Slot holding id, or the empty slot where it belongs.
        integer, intent(in) :: table(:)
        type(cmake_target_t), intent(in) :: recs(:)
        character(len=*), intent(in) :: id
        integer :: mask

        mask = size(table) - 1
        slot = int(iand(fnv1a_string(id), int(mask, int64))) + 1
        do
            if (table(slot) == 0) return
            if (recs(table(slot))%id == id) return
            slot = iand(slot, mask) + 1
        end do
    end function id_slot

    ! ---- JSON walking -------------------------------------------------

    subroutine walker_step(w, event, path)
        !! Update w for event; path = location of the event (for scalars and
        !! keys the enclosing container path plus the key or '#').
        type(walker_t), intent(inout) :: w
        type(json_event_t), intent(in) :: event
        character(len=:), allocatable, intent(out) :: path
        character(len=:), allocatable :: name
        integer, allocatable :: im(:)
        logical, allocatable :: la(:)

        if (.not. allocated(w%path)) then
            w%path = ''
            w%key = ''
            allocate (w%marks(16), w%is_array(16))
        end if
        if (w%pop_pending) then
            w%path = w%path(1:w%marks(w%depth))
            w%depth = w%depth - 1
            w%pop_pending = .false.
        end if
        path = w%path
        select case (event%event_type)
        case (JSON_KEY)
            w%key = event%string_val
            path = join(w%path, w%key)
        case (JSON_OBJECT_START, JSON_ARRAY_START)
            if (w%depth > 0) then
                if (w%is_array(w%depth)) then
                    name = '#'
                else
                    name = w%key
                end if
            else
                name = ''
            end if
            if (w%depth + 1 > size(w%marks)) then
                allocate (im(2*size(w%marks)), la(2*size(w%marks)))
                im(1:w%depth) = w%marks(1:w%depth)
                la(1:w%depth) = w%is_array(1:w%depth)
                call move_alloc(im, w%marks)
                call move_alloc(la, w%is_array)
            end if
            w%depth = w%depth + 1
            w%marks(w%depth) = len(w%path)
            w%is_array(w%depth) = event%event_type == JSON_ARRAY_START
            if (w%depth > 1) w%path = join(w%path, name)
            path = w%path
        case (JSON_OBJECT_END, JSON_ARRAY_END)
            w%pop_pending = .true.
            path = w%path
        case default
            if (w%depth > 0) then
                if (w%is_array(w%depth)) path = join(w%path, '#')
                if (.not. w%is_array(w%depth)) path = join(w%path, w%key)
            end if
        end select
    end subroutine walker_step

    function join(a, b) result(r)
        character(len=*), intent(in) :: a, b
        character(len=:), allocatable :: r

        if (len(a) == 0) then
            r = b
        else
            r = a//'/'//b
        end if
    end function join

    subroutine parse_index(text, codemodel_file, toolchains_file, ok)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: codemodel_file, toolchains_file
        logical, intent(out) :: ok
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        type(walker_t) :: w
        character(len=:), allocatable :: path, kind, file

        ok = .false.
        codemodel_file = ''
        toolchains_file = ''
        kind = ''
        file = ''
        call json_parser_init_strict(parser, text)
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_END_OF_INPUT) exit
            if (event%event_type == JSON_ERROR) return
            call walker_step(w, event, path)
            select case (event%event_type)
            case (JSON_OBJECT_START)
                if (path == 'objects/#') then
                    kind = ''
                    file = ''
                end if
            case (JSON_STRING)
                if (path == 'objects/#/kind') kind = event%string_val
                if (path == 'objects/#/jsonFile') file = event%string_val
            case (JSON_OBJECT_END)
                if (path == 'objects/#' .and. kind == 'codemodel' .and. &
                    len(file) > 0) then
                    codemodel_file = file
                    ok = .true.
                end if
                if (path == 'objects/#' .and. kind == 'toolchains' .and. &
                    len(file) > 0) toolchains_file = file
            end select
        end do
    end subroutine parse_index

    subroutine parse_codemodel(text, configuration, top_source, top_build, recs, &
            n, ok)
        !! Targets of the configuration named `configuration` (the first one
        !! when it is empty or unknown, as in single-config trees).
        character(len=*), intent(in) :: text, configuration
        character(len=:), allocatable, intent(out) :: top_source, top_build
        type(cmake_target_t), allocatable, intent(out) :: recs(:)
        integer, intent(out) :: n
        logical, intent(out) :: ok
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        type(walker_t) :: w
        character(len=:), allocatable :: path, cfg_name
        type(cmake_target_t), allocatable :: cur(:), best(:)
        integer :: n_cur, n_best, config_index
        type(cmake_target_t) :: rec

        ok = .false.
        top_source = ''
        top_build = ''
        n = 0
        allocate (cur(16), best(0))
        n_cur = 0
        n_best = 0
        config_index = 0
        cfg_name = ''
        call json_parser_init_strict(parser, text)
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_END_OF_INPUT) exit
            if (event%event_type == JSON_ERROR) return
            call walker_step(w, event, path)
            select case (event%event_type)
            case (JSON_OBJECT_START)
                if (path == 'configurations/#') then
                    config_index = config_index + 1
                    n_cur = 0
                    cfg_name = ''
                else if (path == 'configurations/#/targets/#') then
                    rec = cmake_target_t()
                    rec%id = ''
                    rec%name = ''
                    rec%json_file = ''
                end if
            case (JSON_STRING)
                select case (path)
                case ('paths/source')
                    top_source = event%string_val
                case ('paths/build')
                    top_build = event%string_val
                case ('configurations/#/name')
                    cfg_name = event%string_val
                case ('configurations/#/targets/#/id')
                    rec%id = event%string_val
                case ('configurations/#/targets/#/name')
                    rec%name = event%string_val
                case ('configurations/#/targets/#/jsonFile')
                    rec%json_file = event%string_val
                end select
            case (JSON_OBJECT_END)
                if (path == 'configurations/#/targets/#') then
                    if (len(rec%id) == 0 .or. len(rec%name) == 0 .or. &
                        len(rec%json_file) == 0) return
                    call push_rec(cur, n_cur, rec)
                else if (path == 'configurations/#') then
                    if (config_index == 1 .or. (len(configuration) > 0 .and. &
                        cfg_name == configuration)) then
                        call move_alloc(cur, best)
                        n_best = n_cur
                        allocate (cur(16))
                    end if
                    n_cur = 0
                end if
            end select
        end do
        if (len(top_source) == 0 .or. len(top_build) == 0 .or. config_index == 0) return
        n = n_best
        allocate (recs(n))
        recs(1:n) = best(1:n)
        ok = .true.
    end subroutine parse_codemodel

    subroutine push_rec(arr, n, rec)
        type(cmake_target_t), allocatable, intent(inout) :: arr(:)
        integer, intent(inout) :: n
        type(cmake_target_t), intent(in) :: rec
        type(cmake_target_t), allocatable :: grown(:)

        if (n + 1 > size(arr)) then
            allocate (grown(2*size(arr)))
            grown(1:n) = arr(1:n)
            call move_alloc(grown, arr)
        end if
        n = n + 1
        arr(n) = rec
    end subroutine push_rec

    subroutine push_string(arr, n, s)
        type(string_t), allocatable, intent(inout) :: arr(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: s
        type(string_t), allocatable :: grown(:)

        if (.not. allocated(arr)) allocate (arr(8))
        if (n + 1 > size(arr)) then
            allocate (grown(max(8, 2*size(arr))))
            grown(1:n) = arr(1:n)
            call move_alloc(grown, arr)
        end if
        n = n + 1
        arr(n)%s = s
    end subroutine push_string

    subroutine parse_target(text, top_source, top_build, rec, ok)
        character(len=*), intent(in) :: text, top_source, top_build
        type(cmake_target_t), intent(inout) :: rec
        logical, intent(out) :: ok
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        type(walker_t) :: w
        character(len=:), allocatable :: path, rel_source, rel_build, fragment
        type(cmake_group_t), allocatable :: grown(:)
        integer :: pending_group, g
        logical :: pending_generated, pending_system

        ok = .false.
        rec%type = ''
        rec%artifact = ''
        rec%link_language = ''
        rel_source = '.'
        rel_build = '.'
        fragment = ''
        pending_group = 0
        pending_generated = .false.
        pending_system = .false.
        rec%n_sources = 0
        rec%n_deps = 0
        rec%n_groups = 0
        rec%n_link = 0
        rec%n_archive = 0
        if (allocated(rec%sources)) deallocate (rec%sources)
        if (allocated(rec%dep_ids)) deallocate (rec%dep_ids)
        if (allocated(rec%groups)) deallocate (rec%groups)
        allocate (rec%groups(4), rec%src_group(0), rec%src_generated(0))
        call json_parser_init_strict(parser, text)
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_END_OF_INPUT) exit
            if (event%event_type == JSON_ERROR) return
            call walker_step(w, event, path)
            g = rec%n_groups
            select case (event%event_type)
            case (JSON_OBJECT_START)
                select case (path)
                case ('sources/#')
                    pending_group = 0
                    pending_generated = .false.
                case ('compileGroups/#')
                    if (g + 1 > size(rec%groups)) then
                        allocate (grown(2*size(rec%groups)))
                        grown(1:g) = rec%groups(1:g)
                        call move_alloc(grown, rec%groups)
                    end if
                    rec%n_groups = g + 1
                    rec%groups(g + 1)%language = ''
                case ('compileGroups/#/includes/#')
                    pending_system = .false.
                case ('link/commandFragments/#')
                    fragment = ''
                end select
            case (JSON_BOOL)
                select case (path)
                case ('sources/#/isGenerated')
                    pending_generated = event%bool_val
                case ('compileGroups/#/includes/#/isSystem')
                    pending_system = event%bool_val
                end select
            case (JSON_INTEGER)
                if (path == 'sources/#/compileGroupIndex') &
                    pending_group = event%int_val + 1
            case (JSON_STRING)
                select case (path)
                case ('type')
                    rec%type = event%string_val
                case ('paths/source')
                    rel_source = event%string_val
                case ('paths/build')
                    rel_build = event%string_val
                case ('artifacts/#/path')
                    if (len(rec%artifact) == 0) &
                        rec%artifact = absolute_path(event%string_val, top_build)
                case ('sources/#/path')
                    call push_string(rec%sources, rec%n_sources, &
                        absolute_path(event%string_val, top_source))
                    rec%src_group = [rec%src_group, pending_group]
                    rec%src_generated = [rec%src_generated, pending_generated]
                case ('dependencies/#/id')
                    call push_string(rec%dep_ids, rec%n_deps, event%string_val)
                case ('compileGroups/#/language')
                    rec%groups(g)%language = event%string_val
                case ('compileGroups/#/compileCommandFragments/#/fragment')
                    call push_string(rec%groups(g)%fragments, &
                        rec%groups(g)%n_fragments, event%string_val)
                case ('compileGroups/#/defines/#/define')
                    call push_string(rec%groups(g)%defines, &
                        rec%groups(g)%n_defines, event%string_val)
                case ('compileGroups/#/includes/#/path')
                    call push_string(rec%groups(g)%includes, &
                        rec%groups(g)%n_includes, event%string_val)
                    if (.not. allocated(rec%groups(g)%include_system)) &
                        allocate (rec%groups(g)%include_system(0))
                    rec%groups(g)%include_system = &
                        [rec%groups(g)%include_system, pending_system]
                case ('link/language')
                    rec%link_language = event%string_val
                case ('link/commandFragments/#/fragment')
                    fragment = event%string_val
                case ('link/commandFragments/#/role')
                    call push_string(rec%link_fragments, rec%n_link, fragment)
                    rec%n_link = rec%n_link - 1
                    call push_string(rec%link_roles, rec%n_link, event%string_val)
                case ('archive/commandFragments/#/fragment')
                    call push_string(rec%archive_fragments, rec%n_archive, &
                        event%string_val)
                end select
            end select
        end do
        if (len(rec%type) == 0) return
        if (.not. allocated(rec%sources)) allocate (rec%sources(0))
        if (.not. allocated(rec%dep_ids)) allocate (rec%dep_ids(0))
        rec%source_dir = absolute_path(rel_source, top_source)
        rec%build_dir = absolute_path(rel_build, top_build)
        ok = .true.
    end subroutine parse_target

end module fo_cmake_model
