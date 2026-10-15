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
    !! Layers are only built for project targets. Cases are added by the CTest
    !! adapter; the model is marked complete when the codemodel was read
    !! without gaps.
    use fx_string, only: string_t
    use fx_hash, only: fnv1a_string
    use fx_json_parse, only: json_parser_t, json_event_t, &
        json_parser_init_strict, json_parser_next, JSON_OBJECT_START, &
        JSON_OBJECT_END, JSON_ARRAY_START, JSON_ARRAY_END, JSON_KEY, &
        JSON_STRING, JSON_ERROR, JSON_END_OF_INPUT
    use fo_cmake_context, only: cmake_context_t, cmake_context_read_reply, &
        cmake_context_read_file, cmake_context_build_path
    use fo_project_model, only: project_model_t, model_init, model_add_target, &
        model_add_layer, model_finalize, TARGET_OTHER, TARGET_LIBRARY, &
        TARGET_EXECUTABLE, ORIGIN_PROJECT, ORIGIN_DEPENDENCY
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    public :: cmake_model_load, cmake_model_target_origin

    type :: walker_t
        !! Tracks the JSON path ('configurations/#/targets/#/name').
        character(len=:), allocatable :: path
        integer, allocatable :: marks(:)
        logical, allocatable :: is_array(:)
        integer :: depth = 0
        character(len=:), allocatable :: key
        logical :: pop_pending = .false.
    end type walker_t

    type :: target_rec_t
        character(len=:), allocatable :: id, name, json_file, type
        character(len=:), allocatable :: source_dir, artifact
        type(string_t), allocatable :: sources(:), dep_ids(:)
        integer :: n_sources = 0, n_deps = 0
    end type target_rec_t

contains

    subroutine cmake_model_load(context, model, ierr, message, input_digest)
        !! ierr 0 = model finalized and complete; otherwise message says why.
        type(cmake_context_t), intent(inout) :: context
        type(project_model_t), intent(out) :: model
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        character(len=*), intent(in), optional :: input_digest
        character(len=:), allocatable :: reply_dir, codemodel_file, text
        character(len=:), allocatable :: top_source, top_build, digest
        type(target_rec_t), allocatable :: recs(:)
        integer :: n, i, k, j, slot, last_slash, ntab
        integer, allocatable :: table(:)
        logical :: ok
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
        last_slash = index(context%reply_index, '/', back=.true.)
        reply_dir = context%reply_index(1:last_slash - 1)

        call cmake_context_read_file(context%reply_index, text)
        if (.not. allocated(text)) then
            message = 'cannot read CMake File API index '//context%reply_index
            return
        end if
        call parse_index(text, codemodel_file, ok)
        if (.not. ok) then
            message = 'CMake File API index has no codemodel-v2 object'
            return
        end if
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

        model%complete = .true.
        call model_finalize(model, ierr, message)
        if (ierr /= 0) model%complete = .false.
    end subroutine cmake_model_load

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
        is_within = len(path) == len(root) .or. path(len(root) + 1:len(root) + 1) == '/'
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
        type(target_rec_t), intent(in) :: recs(:)
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

    subroutine parse_index(text, codemodel_file, ok)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: codemodel_file
        logical, intent(out) :: ok
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        type(walker_t) :: w
        character(len=:), allocatable :: path, kind, file

        ok = .false.
        codemodel_file = ''
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
            end select
        end do
    end subroutine parse_index

    subroutine parse_codemodel(text, configuration, top_source, top_build, recs, &
            n, ok)
        !! Targets of the configuration named `configuration` (the first one
        !! when it is empty or unknown, as in single-config trees).
        character(len=*), intent(in) :: text, configuration
        character(len=:), allocatable, intent(out) :: top_source, top_build
        type(target_rec_t), allocatable, intent(out) :: recs(:)
        integer, intent(out) :: n
        logical, intent(out) :: ok
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        type(walker_t) :: w
        character(len=:), allocatable :: path, cfg_name
        type(target_rec_t), allocatable :: cur(:), best(:)
        integer :: n_cur, n_best, config_index
        type(target_rec_t) :: rec

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
                    rec = target_rec_t()
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
        type(target_rec_t), allocatable, intent(inout) :: arr(:)
        integer, intent(inout) :: n
        type(target_rec_t), intent(in) :: rec
        type(target_rec_t), allocatable :: grown(:)

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
            allocate (grown(2*size(arr)))
            grown(1:n) = arr(1:n)
            call move_alloc(grown, arr)
        end if
        n = n + 1
        arr(n)%s = s
    end subroutine push_string

    subroutine parse_target(text, top_source, top_build, rec, ok)
        character(len=*), intent(in) :: text, top_source, top_build
        type(target_rec_t), intent(inout) :: rec
        logical, intent(out) :: ok
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        type(walker_t) :: w
        character(len=:), allocatable :: path, rel_source

        ok = .false.
        rec%type = ''
        rec%artifact = ''
        rel_source = '.'
        rec%n_sources = 0
        rec%n_deps = 0
        if (allocated(rec%sources)) deallocate (rec%sources)
        if (allocated(rec%dep_ids)) deallocate (rec%dep_ids)
        call json_parser_init_strict(parser, text)
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_END_OF_INPUT) exit
            if (event%event_type == JSON_ERROR) return
            call walker_step(w, event, path)
            if (event%event_type /= JSON_STRING) cycle
            select case (path)
            case ('type')
                rec%type = event%string_val
            case ('paths/source')
                rel_source = event%string_val
            case ('artifacts/#/path')
                if (len(rec%artifact) == 0) &
                    rec%artifact = absolute_path(event%string_val, top_build)
            case ('sources/#/path')
                call push_string(rec%sources, rec%n_sources, &
                    absolute_path(event%string_val, top_source))
            case ('dependencies/#/id')
                call push_string(rec%dep_ids, rec%n_deps, event%string_val)
            end select
        end do
        if (len(rec%type) == 0) return
        if (.not. allocated(rec%sources)) allocate (rec%sources(0))
        if (.not. allocated(rec%dep_ids)) allocate (rec%dep_ids(0))
        rec%source_dir = absolute_path(rel_source, top_source)
        ok = .true.
    end subroutine parse_target

end module fo_cmake_model
