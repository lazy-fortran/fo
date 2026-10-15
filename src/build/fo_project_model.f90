module fo_project_model
    !! Backend-neutral project model (#167, #205).
    !!
    !! An adapter (CMake File API + CTest, or fpm manifest + scan) fills one
    !! project_model_t: targets with sources, link dependencies, artifact and
    !! origin (project or dependency); layers (named tiers of project targets
    !! or sources with allowed uses); test cases with their target mapping,
    !! labels, layer and opaque adapter attributes; the labels excluded by
    !! default; and the identity of the adapter inputs. Selection
    !! (model_select) and build planning (model_plan) are pure operations on
    !! that model, shared by `fo test`, MCP and Gremlin on every backend.
    !! Selection semantics live only here: backends receive exact case names
    !! (model_selected_names), never patterns.
    !!
    !! Construction: model_init, model_add_target/layer/case in any order,
    !! then model_finalize, which resolves name references, rejects duplicate
    !! or dangling names and computes the model identity. Name and artifact
    !! lookups are hashed; selection is linear in cases times patterns and a
    !! plan is O(targets + link edges).
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_string, only: string_t, builder_t, builder_append, builder_to_string
    use fx_hash, only: sha256_string, fnv1a_string
    use fo_pattern, only: pattern_t, pattern_compile, pattern_compile_glob, &
        pattern_matches, pattern_is_glob
    implicit none
    private

    integer, parameter, public :: TARGET_OTHER = 0
    integer, parameter, public :: TARGET_LIBRARY = 1
    integer, parameter, public :: TARGET_EXECUTABLE = 2

    !! Where a target or case comes from. Dependency targets (FetchContent,
    !! external packages, fpm dependencies) are planned through but are never
    !! selected, layered or layering-checked.
    integer, parameter, public :: ORIGIN_PROJECT = 1
    integer, parameter, public :: ORIGIN_DEPENDENCY = 2

    !! How a case was mapped to the targets it needs.
    integer, parameter, public :: MAP_UNKNOWN = 0
    !! Its command runs a known target artifact.
    integer, parameter, public :: MAP_EXACT = 1
    !! An explicit declaration lists its targets (declared_target_names).
    integer, parameter, public :: MAP_DECLARED = 2

    !! model_selection_t%status
    integer, parameter, public :: SELECT_OK = 0
    !! Valid request that selects no case; consumers decide whether that fails.
    integer, parameter, public :: SELECT_EMPTY = 1
    !! An exact name (not a glob) names no selectable case.
    integer, parameter, public :: SELECT_UNKNOWN_NAME = 2
    !! An exactly named case is default-excluded and --all was not given.
    integer, parameter, public :: SELECT_NAMED_EXCLUDED = 3
    integer, parameter, public :: SELECT_UNKNOWN_LAYER = 4
    integer, parameter, public :: SELECT_BAD_PATTERN = 5

    !! model_selection_t%disposition, one per model case, first failing step.
    integer, parameter, public :: CASE_SELECTED = 1
    integer, parameter, public :: CASE_NOT_NAMED = 2
    integer, parameter, public :: CASE_LABEL_FILTERED = 3
    integer, parameter, public :: CASE_LABEL_EXCLUDED = 4
    integer, parameter, public :: CASE_LAYER_FILTERED = 5
    integer, parameter, public :: CASE_DEFAULT_EXCLUDED = 6
    integer, parameter, public :: CASE_DEPENDENCY = 7

    type, public :: model_attr_t
        !! Adapter property carried opaquely (e.g. CTest FIXTURES_REQUIRED).
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
    end type model_attr_t

    type, public :: model_target_t
        character(len=:), allocatable :: name
        integer :: kind = TARGET_OTHER
        integer :: origin = ORIGIN_PROJECT
        type(string_t), allocatable :: sources(:)
        !! Direct link dependencies by name; resolved into link_deps.
        type(string_t), allocatable :: link_dep_names(:)
        integer, allocatable :: link_deps(:)
        !! Absolute artifact path, empty when the target has none.
        character(len=:), allocatable :: artifact
    end type model_target_t

    type, public :: model_layer_t
        character(len=:), allocatable :: name
        !! Directly allowed layers by name; resolved into uses.
        type(string_t), allocatable :: use_names(:)
        integer, allocatable :: uses(:)
        !! Member project targets by name; resolved into targets.
        type(string_t), allocatable :: target_names(:)
        integer, allocatable :: targets(:)
        !! Member sources when a layer is not a whole target (fpm).
        type(string_t), allocatable :: sources(:)
    end type model_layer_t

    type, public :: model_case_t
        character(len=:), allocatable :: name
        integer :: origin = ORIGIN_PROJECT
        !! Target whose artifact runs the case; 0 = none known.
        integer :: command_target = 0
        integer :: mapping = MAP_UNKNOWN
        !! Explicitly declared required targets (MAP_DECLARED), by name;
        !! resolved into declared_targets.
        type(string_t), allocatable :: declared_target_names(:)
        integer, allocatable :: declared_targets(:)
        type(string_t), allocatable :: labels(:)
        !! Derived layer index; 0 = none.
        integer :: layer = 0
        type(model_attr_t), allocatable :: attrs(:)
        !! Adapter-level default exclusion independent of labels (a case the
        !! selected CTest preset filters out).
        logical :: default_excluded = .false.
    end type model_case_t

    type, public :: name_index_t
        !! Open-addressing hash from a key to an entity index (1..n).
        integer, allocatable :: slots(:)
        type(string_t), allocatable :: keys(:)
        integer :: count = 0
    end type name_index_t

    type, public :: project_model_t
        character(len=:), allocatable :: backend
        character(len=:), allocatable :: build_dir
        character(len=:), allocatable :: preset
        character(len=:), allocatable :: configuration
        !! Digest of the adapter inputs (manifest, CMake/preset/cache roles).
        character(len=:), allocatable :: input_digest
        !! model_cache_key of the fields above: reuse key of a loaded model.
        character(len=64) :: cache_key = ''
        !! Digest of cache_key and the whole model content.
        character(len=64) :: identity = ''
        !! False when the adapter could not see every target or case; plans
        !! then widen to all targets.
        logical :: complete = .false.
        logical :: finalized = .false.
        integer :: n_targets = 0
        integer :: n_layers = 0
        integer :: n_cases = 0
        type(model_target_t), allocatable :: targets(:)
        type(model_layer_t), allocatable :: layers(:)
        type(model_case_t), allocatable :: cases(:)
        !! Cases carrying any of these labels are excluded unless --all.
        type(string_t), allocatable :: default_exclude(:)
        type(name_index_t) :: target_index, layer_index, case_index
        type(name_index_t) :: artifact_index
        !! First duplicate name seen while adding; reported by finalize.
        character(len=:), allocatable :: duplicate
    end type project_model_t

    type, public :: model_request_t
        !! Exact case names or globs; none = every case.
        type(string_t), allocatable :: names(:)
        !! CTest -L regexes (AND) and -LE regexes (AND).
        type(string_t), allocatable :: include_labels(:)
        type(string_t), allocatable :: exclude_labels(:)
        !! Layer names (union); none = no layer filter.
        type(string_t), allocatable :: layers(:)
        logical :: all = .false.
    end type model_request_t

    type, public :: model_selection_t
        integer :: status = SELECT_OK
        character(len=:), allocatable :: message
        !! Selected case indices in model order.
        integer, allocatable :: cases(:)
        !! Per model case: CASE_* and a short human-readable reason.
        integer, allocatable :: disposition(:)
        type(string_t), allocatable :: reasons(:)
        !! Unknown exact names, unknown layers or invalid patterns.
        type(string_t), allocatable :: unknown(:)
        !! Globs that matched no case (not an error by themselves).
        type(string_t), allocatable :: unmatched(:)
    end type model_selection_t

    type, public :: model_plan_t
        !! Target indices to build, in model order.
        integer, allocatable :: targets(:)
        !! True when the plan fell back to every target; reasons say why
        !! (incomplete model, cases with MAP_UNKNOWN).
        logical :: widened = .false.
        type(string_t), allocatable :: reasons(:)
    end type model_plan_t

    public :: model_init, model_add_target, model_add_layer, model_add_case
    public :: model_finalize, model_select, model_plan, model_selected_names
    public :: model_find_target, model_find_artifact, model_find_layer
    public :: model_find_case, model_cache_key

contains

    function model_cache_key(backend, build_dir, preset, configuration, &
            input_digest) result(key)
        !! Reuse key of a loaded model: backend, build tree, preset,
        !! configuration and the adapter input digest.
        character(len=*), intent(in) :: backend, build_dir, preset, configuration
        character(len=*), intent(in) :: input_digest
        character(len=64) :: key
        type(builder_t) :: b

        call field(b, 'fo-project-model-key-v1')
        call field(b, backend)
        call field(b, build_dir)
        call field(b, preset)
        call field(b, configuration)
        call field(b, input_digest)
        key = sha256_string(builder_to_string(b))
    end function model_cache_key

    subroutine model_init(model, backend, input_digest, build_dir, preset, &
            configuration)
        type(project_model_t), intent(out) :: model
        character(len=*), intent(in) :: backend, input_digest
        character(len=*), intent(in), optional :: build_dir, preset, configuration

        model%backend = backend
        model%input_digest = input_digest
        model%build_dir = ''
        if (present(build_dir)) model%build_dir = build_dir
        model%preset = ''
        if (present(preset)) model%preset = preset
        model%configuration = ''
        if (present(configuration)) model%configuration = configuration
        model%cache_key = model_cache_key(model%backend, model%build_dir, &
            model%preset, model%configuration, model%input_digest)
        model%duplicate = ''
        allocate(model%targets(16), model%layers(4), model%cases(16))
        allocate(model%default_exclude(0))
    end subroutine model_init

    subroutine model_add_target(model, name, kind, sources, link_deps, artifact, &
            origin, index)
        type(project_model_t), intent(inout) :: model
        character(len=*), intent(in) :: name
        integer, intent(in) :: kind
        type(string_t), intent(in), optional :: sources(:), link_deps(:)
        character(len=*), intent(in), optional :: artifact
        integer, intent(in), optional :: origin
        integer, intent(out), optional :: index
        type(model_target_t), allocatable :: grown(:)
        integer :: n, existing

        n = model%n_targets + 1
        if (n > size(model%targets)) then
            allocate(grown(2*size(model%targets)))
            grown(1:n - 1) = model%targets(1:n - 1)
            call move_alloc(grown, model%targets)
        end if
        model%n_targets = n
        associate (t => model%targets(n))
            t%name = name
            t%kind = kind
            t%origin = ORIGIN_PROJECT
            if (present(origin)) t%origin = origin
            call copy_strings(sources, t%sources)
            call copy_strings(link_deps, t%link_dep_names)
            t%artifact = ''
            if (present(artifact)) t%artifact = artifact
            call index_insert(model%target_index, name, n, existing)
            if (existing /= 0) call note_duplicate(model, 'target', name)
            if (len(t%artifact) > 0) &
                call index_insert(model%artifact_index, t%artifact, n, existing)
        end associate
        model%finalized = .false.
        if (present(index)) index = n
    end subroutine model_add_target

    subroutine model_add_layer(model, name, uses, targets, sources, index)
        type(project_model_t), intent(inout) :: model
        character(len=*), intent(in) :: name
        type(string_t), intent(in), optional :: uses(:), targets(:), sources(:)
        integer, intent(out), optional :: index
        type(model_layer_t), allocatable :: grown(:)
        integer :: n, existing

        n = model%n_layers + 1
        if (n > size(model%layers)) then
            allocate(grown(2*size(model%layers)))
            grown(1:n - 1) = model%layers(1:n - 1)
            call move_alloc(grown, model%layers)
        end if
        model%n_layers = n
        associate (l => model%layers(n))
            l%name = name
            call copy_strings(uses, l%use_names)
            call copy_strings(targets, l%target_names)
            call copy_strings(sources, l%sources)
        end associate
        call index_insert(model%layer_index, name, n, existing)
        if (existing /= 0) call note_duplicate(model, 'layer', name)
        model%finalized = .false.
        if (present(index)) index = n
    end subroutine model_add_layer

    subroutine model_add_case(model, name, command_target, labels, layer, attrs, &
            default_excluded, declared_targets, mapping, origin, index)
        !! mapping defaults to MAP_DECLARED with declared_targets, else
        !! MAP_EXACT with a command target, else MAP_UNKNOWN.
        type(project_model_t), intent(inout) :: model
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: command_target, layer, mapping, origin
        type(string_t), intent(in), optional :: labels(:), declared_targets(:)
        type(model_attr_t), intent(in), optional :: attrs(:)
        logical, intent(in), optional :: default_excluded
        integer, intent(out), optional :: index
        type(model_case_t), allocatable :: grown(:)
        integer :: n, existing

        n = model%n_cases + 1
        if (n > size(model%cases)) then
            allocate(grown(2*size(model%cases)))
            grown(1:n - 1) = model%cases(1:n - 1)
            call move_alloc(grown, model%cases)
        end if
        model%n_cases = n
        associate (c => model%cases(n))
            c%name = name
            c%origin = ORIGIN_PROJECT
            if (present(origin)) c%origin = origin
            c%command_target = 0
            if (present(command_target)) c%command_target = command_target
            c%layer = 0
            if (present(layer)) c%layer = layer
            call copy_strings(labels, c%labels)
            call copy_strings(declared_targets, c%declared_target_names)
            if (present(attrs)) then
                allocate(c%attrs(size(attrs)))
                c%attrs(:) = attrs
            else
                allocate(c%attrs(0))
            end if
            c%default_excluded = .false.
            if (present(default_excluded)) c%default_excluded = default_excluded
            if (present(mapping)) then
                c%mapping = mapping
            else if (size(c%declared_target_names) > 0) then
                c%mapping = MAP_DECLARED
            else if (c%command_target > 0) then
                c%mapping = MAP_EXACT
            else
                c%mapping = MAP_UNKNOWN
            end if
        end associate
        call index_insert(model%case_index, name, n, existing)
        if (existing /= 0) call note_duplicate(model, 'test case', name)
        model%finalized = .false.
        if (present(index)) index = n
    end subroutine model_add_case

    subroutine model_finalize(model, ierr, message)
        !! Trim storage, check names, resolve references, compute identity.
        type(project_model_t), intent(inout) :: model
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        type(model_target_t), allocatable :: targets(:)
        type(model_layer_t), allocatable :: layers(:)
        type(model_case_t), allocatable :: cases(:)
        integer :: i, k

        ierr = 0
        message = ''
        model%finalized = .false.
        if (size(model%targets) /= model%n_targets) then
            allocate(targets(model%n_targets))
            targets(:) = model%targets(1:model%n_targets)
            call move_alloc(targets, model%targets)
        end if
        if (size(model%layers) /= model%n_layers) then
            allocate(layers(model%n_layers))
            layers(:) = model%layers(1:model%n_layers)
            call move_alloc(layers, model%layers)
        end if
        if (size(model%cases) /= model%n_cases) then
            allocate(cases(model%n_cases))
            cases(:) = model%cases(1:model%n_cases)
            call move_alloc(cases, model%cases)
        end if
        if (len(model%duplicate) > 0) then
            call fail(model%duplicate)
            return
        end if

        do i = 1, model%n_targets
            associate (t => model%targets(i))
                if (len(t%name) == 0) then
                    call fail('empty target name')
                    return
                end if
                call resolve_targets(t%link_dep_names, t%link_deps, &
                    'link dependency of target '//t%name)
                if (ierr /= 0) return
            end associate
        end do
        do i = 1, model%n_layers
            associate (l => model%layers(i))
                if (len(l%name) == 0) then
                    call fail('empty layer name')
                    return
                end if
                if (allocated(l%uses)) deallocate(l%uses)
                allocate(l%uses(size(l%use_names)))
                do k = 1, size(l%use_names)
                    l%uses(k) = model_find_layer(model, l%use_names(k)%s)
                    if (l%uses(k) == 0) then
                        call fail('unknown layer '//l%use_names(k)%s// &
                            ' used by layer '//l%name)
                        return
                    end if
                end do
                call resolve_targets(l%target_names, l%targets, &
                    'member of layer '//l%name)
                if (ierr /= 0) return
                do k = 1, size(l%targets)
                    if (model%targets(l%targets(k))%origin == ORIGIN_PROJECT) cycle
                    call fail('layer '//l%name//' contains dependency target '// &
                        l%target_names(k)%s)
                    return
                end do
            end associate
        end do
        do i = 1, model%n_cases
            associate (c => model%cases(i))
                if (len(c%name) == 0) then
                    call fail('empty test case name')
                    return
                end if
                if (c%command_target < 0 .or. c%command_target > model%n_targets) then
                    call fail('test case '//c%name//' has an invalid command target')
                    return
                end if
                if (c%layer < 0 .or. c%layer > model%n_layers) then
                    call fail('test case '//c%name//' has an invalid layer')
                    return
                end if
                call resolve_targets(c%declared_target_names, c%declared_targets, &
                    'declared target of test case '//c%name)
                if (ierr /= 0) return
                if (.not. mapping_consistent(c)) then
                    call fail('test case '//c%name//' has an inconsistent mapping')
                    return
                end if
            end associate
        end do
        model%identity = model_digest(model)
        model%finalized = .true.

    contains

        subroutine fail(text)
            character(len=*), intent(in) :: text

            ierr = 1
            message = text
        end subroutine fail

        subroutine resolve_targets(names, indices, context)
            type(string_t), intent(in) :: names(:)
            integer, allocatable, intent(inout) :: indices(:)
            character(len=*), intent(in) :: context
            integer :: j

            if (allocated(indices)) deallocate(indices)
            allocate(indices(size(names)))
            do j = 1, size(names)
                indices(j) = model_find_target(model, names(j)%s)
                if (indices(j) == 0) then
                    call fail('unknown target '//names(j)%s//' as '//context)
                    return
                end if
            end do
        end subroutine resolve_targets

    end subroutine model_finalize

    logical function mapping_consistent(c) result(ok)
        type(model_case_t), intent(in) :: c

        select case (c%mapping)
        case (MAP_EXACT)
            ok = c%command_target > 0
        case (MAP_DECLARED)
            ok = size(c%declared_targets) > 0
        case (MAP_UNKNOWN)
            ok = .true.
        case default
            ok = .false.
        end select
    end function mapping_consistent

    integer function model_find_target(model, name) result(index)
        type(project_model_t), intent(in) :: model
        character(len=*), intent(in) :: name

        index = index_lookup(model%target_index, name)
    end function model_find_target

    integer function model_find_artifact(model, path) result(index)
        !! First target whose artifact path equals path; 0 when none.
        type(project_model_t), intent(in) :: model
        character(len=*), intent(in) :: path

        index = 0
        if (len(path) == 0) return
        index = index_lookup(model%artifact_index, path)
    end function model_find_artifact

    integer function model_find_layer(model, name) result(index)
        type(project_model_t), intent(in) :: model
        character(len=*), intent(in) :: name

        index = index_lookup(model%layer_index, name)
    end function model_find_layer

    integer function model_find_case(model, name) result(index)
        type(project_model_t), intent(in) :: model
        character(len=*), intent(in) :: name

        index = index_lookup(model%case_index, name)
    end function model_find_case

    subroutine model_select(model, request, selection)
        !! Select project cases: names/globs, then -L (AND), -LE (AND), layers,
        !! then default exclusion. -LE always wins. Default exclusion is lifted
        !! by --all, or for a label exclusion by an -L regex matching that
        !! label. Dependency cases are never selected.
        type(project_model_t), intent(in) :: model
        type(model_request_t), intent(in) :: request
        type(model_selection_t), intent(out) :: selection
        type(string_t), allocatable :: names(:), include(:), exclude(:), layers(:)
        type(pattern_t), allocatable :: include_re(:), exclude_re(:)
        type(pattern_t) :: glob
        logical, allocatable :: named(:), exact(:), layer_wanted(:)
        type(string_t), allocatable :: origin(:)
        integer :: i, k, n_selected
        logical :: any_match
        character(len=:), allocatable :: reason, lifted

        if (.not. model%finalized) error stop 'model_select: model not finalized'
        call copy_strings(request%names, names)
        call copy_strings(request%include_labels, include)
        call copy_strings(request%exclude_labels, exclude)
        call copy_strings(request%layers, layers)
        allocate(selection%cases(0), selection%unknown(0), selection%unmatched(0))
        allocate(selection%disposition(model%n_cases), selection%reasons(model%n_cases))
        selection%disposition = CASE_NOT_NAMED
        do i = 1, model%n_cases
            selection%reasons(i)%s = ''
        end do
        selection%message = ''

        call compile_all(include, include_re)
        call compile_all(exclude, exclude_re)
        if (size(selection%unknown) > 0) then
            selection%status = SELECT_BAD_PATTERN
            selection%message = joined(selection%unknown)
            return
        end if

        allocate(layer_wanted(0:model%n_layers))
        layer_wanted = size(layers) == 0
        do k = 1, size(layers)
            i = model_find_layer(model, layers(k)%s)
            if (i == 0) then
                call append_string(selection%unknown, layers(k)%s)
            else
                layer_wanted(i) = .true.
            end if
        end do
        if (size(selection%unknown) > 0) then
            selection%status = SELECT_UNKNOWN_LAYER
            selection%message = 'unknown layer: '//joined(selection%unknown)
            return
        end if

        allocate(named(model%n_cases), exact(model%n_cases), origin(model%n_cases))
        named = size(names) == 0
        exact = .false.
        do i = 1, model%n_cases
            origin(i)%s = 'all cases'
        end do
        do k = 1, size(names)
            i = model_find_case(model, names(k)%s)
            if (i /= 0) then
                if (model%cases(i)%origin == ORIGIN_PROJECT) then
                    named(i) = .true.
                    exact(i) = .true.
                    origin(i)%s = 'name '//names(k)%s
                else
                    call append_string(selection%unknown, names(k)%s// &
                        ' (dependency test)')
                end if
                cycle
            end if
            if (.not. pattern_is_glob(names(k)%s)) then
                call append_string(selection%unknown, names(k)%s)
                cycle
            end if
            call pattern_compile_glob(names(k)%s, glob)
            any_match = .false.
            do i = 1, model%n_cases
                if (model%cases(i)%origin /= ORIGIN_PROJECT) cycle
                if (.not. pattern_matches(glob, model%cases(i)%name)) cycle
                any_match = .true.
                if (named(i)) cycle
                named(i) = .true.
                origin(i)%s = 'glob '//names(k)%s
            end do
            if (.not. any_match) call append_string(selection%unmatched, names(k)%s)
        end do

        n_selected = 0
        do i = 1, model%n_cases
            associate (c => model%cases(i))
                if (c%origin /= ORIGIN_PROJECT) then
                    call mark(i, CASE_DEPENDENCY, 'dependency test')
                    cycle
                end if
                if (.not. named(i)) cycle
                k = first_unmatched(include_re, c%labels)
                if (k > 0) then
                    call mark(i, CASE_LABEL_FILTERED, 'no label matches -L '// &
                        include(k)%s)
                    cycle
                end if
                if (size(exclude_re) > 0) then
                    if (first_unmatched(exclude_re, c%labels) == 0) then
                        call mark(i, CASE_LABEL_EXCLUDED, 'labels match -LE '// &
                            joined(exclude))
                        cycle
                    end if
                end if
                if (.not. layer_wanted(c%layer)) then
                    call mark(i, CASE_LAYER_FILTERED, 'not in the requested layers')
                    cycle
                end if
                call default_exclusion(c, model%default_exclude, include, &
                    include_re, request%all, reason, lifted)
                if (len(reason) > 0) then
                    call mark(i, CASE_DEFAULT_EXCLUDED, reason)
                    cycle
                end if
                call mark(i, CASE_SELECTED, origin(i)%s//lifted)
                n_selected = n_selected + 1
            end associate
        end do
        deallocate(selection%cases)
        allocate(selection%cases(n_selected))
        n_selected = 0
        do i = 1, model%n_cases
            if (selection%disposition(i) /= CASE_SELECTED) cycle
            n_selected = n_selected + 1
            selection%cases(n_selected) = i
        end do

        if (size(selection%unknown) > 0) then
            selection%status = SELECT_UNKNOWN_NAME
            selection%message = 'unknown test: '//joined(selection%unknown)
            return
        end if
        do i = 1, model%n_cases
            if (.not. exact(i)) cycle
            if (selection%disposition(i) /= CASE_DEFAULT_EXCLUDED) cycle
            selection%status = SELECT_NAMED_EXCLUDED
            selection%message = 'test '//model%cases(i)%name//' requires --all ('// &
                selection%reasons(i)%s//')'
            return
        end do
        if (n_selected == 0) then
            selection%status = SELECT_EMPTY
            selection%message = 'no test case matches the selection'
        end if

    contains

        subroutine mark(case_index, disposition, text)
            integer, intent(in) :: case_index, disposition
            character(len=*), intent(in) :: text

            selection%disposition(case_index) = disposition
            selection%reasons(case_index)%s = text
        end subroutine mark

        subroutine compile_all(regexes, compiled)
            type(string_t), intent(in) :: regexes(:)
            type(pattern_t), allocatable, intent(out) :: compiled(:)
            integer :: j, ierr
            character(len=:), allocatable :: text

            allocate(compiled(size(regexes)))
            do j = 1, size(regexes)
                call pattern_compile(regexes(j)%s, compiled(j), ierr, text)
                if (ierr /= 0) call append_string(selection%unknown, text)
            end do
        end subroutine compile_all

    end subroutine model_select

    subroutine model_selected_names(model, selection, names)
        !! Exact names of the selected cases, for backend run primitives.
        type(project_model_t), intent(in) :: model
        type(model_selection_t), intent(in) :: selection
        type(string_t), allocatable, intent(out) :: names(:)
        integer :: k

        allocate(names(size(selection%cases)))
        do k = 1, size(selection%cases)
            names(k)%s = model%cases(selection%cases(k))%name
        end do
    end subroutine model_selected_names

    subroutine default_exclusion(c, defaults, include, include_re, all, reason, &
            lifted)
        !! reason: why c stays default-excluded ('' when it does not);
        !! lifted: suffix recording a lifted exclusion for the selection reason.
        type(model_case_t), intent(in) :: c
        type(string_t), intent(in) :: defaults(:), include(:)
        type(pattern_t), intent(in) :: include_re(:)
        logical, intent(in) :: all
        character(len=:), allocatable, intent(out) :: reason, lifted
        integer :: j, k
        character(len=:), allocatable :: label_hit

        reason = ''
        lifted = ''
        label_hit = ''
        outer: do j = 1, size(c%labels)
        do k = 1, size(defaults)
            if (c%labels(j)%s == defaults(k)%s) then
                label_hit = c%labels(j)%s
                exit outer
            end if
        end do
    end do outer
    if (.not. c%default_excluded .and. len(label_hit) == 0) return
    if (all) then
        lifted = '; default exclusion lifted by --all'
        return
    end if
    if (c%default_excluded) then
        reason = 'excluded by default: test preset filter'
        return
    end if
    do k = 1, size(include_re)
        if (pattern_matches(include_re(k), label_hit)) then
            lifted = '; default exclusion lifted by -L '//include(k)%s
            return
        end if
    end do
    reason = 'excluded by default: label '//label_hit
end subroutine default_exclusion

subroutine model_plan(model, cases, plan)
    !! Targets needed to run cases: their mapped targets (command target,
    !! declared targets) and the closure of their link dependencies,
    !! dependency-origin targets included; O(targets + link edges). A case
    !! with an unknown mapping or an incomplete model widens the plan to
    !! every target.
    type(project_model_t), intent(in) :: model
    integer, intent(in) :: cases(:)
    type(model_plan_t), intent(out) :: plan
    logical, allocatable :: needed(:)
    integer, allocatable :: stack(:)
    integer :: i, top, t, k, n

    if (.not. model%finalized) error stop 'model_plan: model not finalized'
    allocate(plan%reasons(0))
    if (.not. model%complete) &
        call append_string(plan%reasons, 'project model is incomplete')
    do i = 1, size(cases)
        if (cases(i) < 1 .or. cases(i) > model%n_cases) then
            call append_string(plan%reasons, 'invalid case index')
            cycle
        end if
        if (model%cases(cases(i))%mapping /= MAP_UNKNOWN) cycle
        call append_string(plan%reasons, 'test case '// &
            model%cases(cases(i))%name//': target mapping unknown')
    end do
    plan%widened = size(plan%reasons) > 0
    allocate(needed(model%n_targets), stack(max(1, model%n_targets)))
    needed = plan%widened
    top = 0
    if (.not. plan%widened) then
        do i = 1, size(cases)
            associate (c => model%cases(cases(i)))
                if (c%command_target > 0) call push(c%command_target)
                do k = 1, size(c%declared_targets)
                    call push(c%declared_targets(k))
                end do
            end associate
        end do
    end if
    do while (top > 0)
        t = stack(top)
        top = top - 1
        do k = 1, size(model%targets(t)%link_deps)
            call push(model%targets(t)%link_deps(k))
        end do
    end do
    allocate(plan%targets(count(needed)))
    n = 0
    do i = 1, model%n_targets
        if (.not. needed(i)) cycle
        n = n + 1
        plan%targets(n) = i
    end do

contains

    subroutine push(target_index)
        integer, intent(in) :: target_index

        if (needed(target_index)) return
        needed(target_index) = .true.
        top = top + 1
        stack(top) = target_index
    end subroutine push

end subroutine model_plan

function model_digest(model) result(digest)
    !! Length-prefixed canonical serialization hashed with SHA-256.
    type(project_model_t), intent(in) :: model
    character(len=64) :: digest
    type(builder_t) :: b
    integer :: i, k

    call field(b, 'fo-project-model-v1')
    call field(b, model%cache_key)
    call field(b, merge('complete  ', 'incomplete', model%complete))
    call list('default-exclude', model%default_exclude)
    do i = 1, model%n_targets
        associate (t => model%targets(i))
            call field(b, 'target')
            call field(b, t%name)
            call field(b, int_text(t%kind))
            call field(b, int_text(t%origin))
            call field(b, t%artifact)
            call list('sources', t%sources)
            call list('link', t%link_dep_names)
        end associate
    end do
    do i = 1, model%n_layers
        associate (l => model%layers(i))
            call field(b, 'layer')
            call field(b, l%name)
            call list('uses', l%use_names)
            call list('targets', l%target_names)
            call list('sources', l%sources)
        end associate
    end do
    do i = 1, model%n_cases
        associate (c => model%cases(i))
            call field(b, 'case')
            call field(b, c%name)
            call field(b, int_text(c%origin))
            call field(b, int_text(c%command_target))
            call field(b, int_text(c%mapping))
            call list('declared', c%declared_target_names)
            call field(b, int_text(c%layer))
            call field(b, merge('excluded', 'default ', c%default_excluded))
            call list('labels', c%labels)
            call field(b, int_text(size(c%attrs)))
            do k = 1, size(c%attrs)
                call field(b, c%attrs(k)%key)
                call field(b, c%attrs(k)%value)
            end do
        end associate
    end do
    digest = sha256_string(builder_to_string(b))

contains

    subroutine list(tag, items)
        character(len=*), intent(in) :: tag
        type(string_t), intent(in) :: items(:)
        integer :: j

        call field(b, tag)
        call field(b, int_text(size(items)))
        do j = 1, size(items)
            call field(b, items(j)%s)
        end do
    end subroutine list

end function model_digest

subroutine field(b, text)
    type(builder_t), intent(inout) :: b
    character(len=*), intent(in) :: text

    call builder_append(b, int_text(len(text))//':'//text)
end subroutine field

integer function first_unmatched(patterns, labels) result(index)
    !! First regex matching none of labels (CTest AND semantics); 0 = all match.
    type(pattern_t), intent(in) :: patterns(:)
    type(string_t), intent(in) :: labels(:)
    integer :: k, j
    logical :: hit

    index = 0
    do k = 1, size(patterns)
        hit = .false.
        do j = 1, size(labels)
            if (pattern_matches(patterns(k), labels(j)%s)) then
                hit = .true.
                exit
            end if
        end do
        if (.not. hit) then
            index = k
            return
        end if
    end do
end function first_unmatched

subroutine note_duplicate(model, what, name)
    type(project_model_t), intent(inout) :: model
    character(len=*), intent(in) :: what, name

    if (len(model%duplicate) == 0) &
        model%duplicate = 'duplicate '//what//' name: '//name
end subroutine note_duplicate

subroutine index_insert(index, key, value, existing)
    !! Map key to value unless already present; existing = previous value.
    type(name_index_t), intent(inout) :: index
    character(len=*), intent(in) :: key
    integer, intent(in) :: value
    integer, intent(out) :: existing
    integer :: slot

    if (.not. allocated(index%slots)) then
        allocate(index%slots(64), index%keys(64))
        index%slots = 0
    end if
    existing = index_lookup(index, key)
    if (existing /= 0) return
    if (2*(index%count + 1) > size(index%slots)) call index_grow(index)
    if (value > size(index%keys)) call grow_keys(index%keys, value)
    index%keys(value)%s = key
    slot = index_probe(index, key)
    index%slots(slot) = value
    index%count = index%count + 1
end subroutine index_insert

integer function index_lookup(index, key) result(value)
    type(name_index_t), intent(in) :: index
    character(len=*), intent(in) :: key

    value = 0
    if (.not. allocated(index%slots)) return
    value = index%slots(index_probe(index, key))
end function index_lookup

integer function index_probe(index, key) result(slot)
    !! Slot holding key, or the empty slot where it belongs.
    type(name_index_t), intent(in) :: index
    character(len=*), intent(in) :: key
    integer :: mask

    mask = size(index%slots) - 1
    slot = int(iand(fnv1a_string(key), int(mask, int64))) + 1
    do
        if (index%slots(slot) == 0) return
        if (index%keys(index%slots(slot))%s == key) return
        slot = iand(slot, mask) + 1
    end do
end function index_probe

subroutine index_grow(index)
    type(name_index_t), intent(inout) :: index
    integer, allocatable :: old(:)
    integer :: k

    call move_alloc(index%slots, old)
    allocate(index%slots(2*size(old)))
    index%slots = 0
    do k = 1, size(old)
        if (old(k) == 0) cycle
        index%slots(index_probe(index, index%keys(old(k))%s)) = old(k)
    end do
end subroutine index_grow

subroutine grow_keys(keys, needed)
    type(string_t), allocatable, intent(inout) :: keys(:)
    integer, intent(in) :: needed
    type(string_t), allocatable :: grown(:)

    allocate(grown(max(needed, 2*size(keys))))
    grown(1:size(keys)) = keys
    call move_alloc(grown, keys)
end subroutine grow_keys

subroutine copy_strings(items, copy)
    type(string_t), intent(in), optional :: items(:)
    type(string_t), allocatable, intent(out) :: copy(:)

    if (present(items)) then
        allocate(copy(size(items)))
        copy(:) = items
    else
        allocate(copy(0))
    end if
end subroutine copy_strings

subroutine append_string(items, text)
    type(string_t), allocatable, intent(inout) :: items(:)
    character(len=*), intent(in) :: text
    type(string_t), allocatable :: grown(:)
    integer :: n

    n = size(items)
    allocate(grown(n + 1))
    grown(1:n) = items
    grown(n + 1)%s = text
    call move_alloc(grown, items)
end subroutine append_string

function joined(items) result(text)
    type(string_t), intent(in) :: items(:)
    character(len=:), allocatable :: text
    integer :: k

    text = ''
    do k = 1, size(items)
        if (k > 1) text = text//', '
        text = text//items(k)%s
    end do
end function joined

function int_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=16) :: buffer

    write (buffer, '(i0)') value
    text = trim(buffer)
end function int_text

end module fo_project_model
