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
    use fx_string, only: string_t, builder_t, builder_append, builder_to_string, to_lower
    use fx_hash, only: sha256_string, fnv1a_string
    use fo_pattern, only: pattern_t, pattern_compile, pattern_compile_glob, &
        pattern_matches, pattern_is_glob
    use fo_util, only: int_text
    use fo_case_filter, only: case_filter_t, filter_parse, filter_eval
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
    !! Selected, but no changed file affects it (model_affected).
    integer, parameter, public :: CASE_NOT_AFFECTED = 8
    !! Selected by name and labels, but the filter expression rejects it.
    integer, parameter, public :: CASE_FILTERED = 9

    type, public :: model_attr_t
        !! Adapter property carried opaquely (e.g. CTest FIXTURES_REQUIRED).
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
        !! The value as list elements, kept apart where value joins them with
        !! semicolons (CTest list properties; an element may hold one).
        type(string_t), allocatable :: items(:)
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
        !! Full argv of the case where the adapter knows one (CTest).
        type(string_t), allocatable :: command(:)
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
        !! Runs only when named exactly: never selected by globs, labels,
        !! layers, --all or an empty name list (the fpm dispatcher program).
        logical :: explicit_only = .false.
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
        !! Filter expression (fo_case_filter); empty = none. affected and
        !! failed are per model case, supplied by the caller when the
        !! expression uses affected() or failed().
        character(len=:), allocatable :: filter
        logical, allocatable :: affected(:), failed(:)
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

    type, public :: model_unit_t
        !! One scanned project source for model_layering: its path spelled as
        !! the model spells it (a target or layer source) and the modules it
        !! defines and uses (compared case-insensitively).
        character(len=:), allocatable :: path
        type(string_t), allocatable :: provides(:)
        type(string_t), allocatable :: uses(:)
        !! The file also holds program units outside its modules (external
        !! procedures, a program): it links like a source no unit uses as a
        !! module even when another unit uses one of its modules.
        logical :: other_units = .false.
    end type model_unit_t

    type, public :: model_violation_t
        !! source (in layer) uses module, defined by provider (in
        !! provider_layer), and layer does not reach provider_layer.
        character(len=:), allocatable :: source, module, layer
        character(len=:), allocatable :: provider, provider_layer
    end type model_violation_t

    type, public :: model_layering_t
        !! Sources that belong to at least one layer.
        integer :: checked = 0
        type(model_violation_t), allocatable :: violations(:)
        !! Units in no layer, in unit order (empty when there are no layers).
        type(string_t), allocatable :: unlayered(:)
    end type model_layering_t

    !! How a project file can affect test cases (model_affected).
    integer, parameter :: FILE_OTHER = 0
    integer, parameter :: FILE_CLOSURE = 1
    integer, parameter :: FILE_BUILD_DESCRIPTION = 2
    integer, parameter :: FILE_UNOWNED_SOURCE = 3

    type :: affect_graph_t
        !! Units and target sources indexed by relative path; module name ->
        !! providing units; source -> owning targets; closure stamps.
        type(name_index_t) :: unit_index, module_index, source_index
        type(string_t), allocatable :: roots(:)
        integer, allocatable :: module_head(:), provider_next(:), provider_unit(:)
        integer, allocatable :: owner_head(:), owner_next(:), owner_target(:)
        integer, allocatable :: tstamp(:), ustamp(:), queue(:)
        logical, allocatable :: plain_unit(:)
        integer :: nu = 0, n_modules = 0, n_providers = 0
        integer :: n_sources = 0, n_owners = 0, token = 0
    end type affect_graph_t

    public :: model_layering, model_layering_sources

    public :: model_init, model_add_target, model_add_layer, model_add_case
    public :: model_finalize, model_select, model_plan, model_selected_names
    public :: model_find_target, model_find_artifact, model_find_layer
    public :: model_find_case, model_cache_key
    public :: model_affected, model_case_digests
    public :: index_insert, index_lookup

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
            default_excluded, declared_targets, mapping, origin, explicit_only, index, &
            command)
        !! mapping defaults to MAP_DECLARED with declared_targets, else
        !! MAP_EXACT with a command target, else MAP_UNKNOWN.
        type(project_model_t), intent(inout) :: model
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: command_target, layer, mapping, origin
        type(string_t), intent(in), optional :: labels(:), declared_targets(:)
        type(string_t), intent(in), optional :: command(:)
        type(model_attr_t), intent(in), optional :: attrs(:)
        logical, intent(in), optional :: default_excluded, explicit_only
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
            call copy_strings(command, c%command)
            if (present(attrs)) then
                allocate(c%attrs(size(attrs)))
                c%attrs(:) = attrs
            else
                allocate(c%attrs(0))
            end if
            c%default_excluded = .false.
            if (present(default_excluded)) c%default_excluded = default_excluded
            c%explicit_only = .false.
            if (present(explicit_only)) c%explicit_only = explicit_only
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
        !! label. Dependency cases are never selected; explicit-only cases only
        !! when named exactly.
        type(project_model_t), intent(in) :: model
        type(model_request_t), intent(in) :: request
        type(model_selection_t), intent(out) :: selection
        type(string_t), allocatable :: names(:), include(:), exclude(:), layers(:)
        type(pattern_t), allocatable :: include_re(:), exclude_re(:)
        type(case_filter_t) :: expression
        logical :: filtered
        integer :: filter_error
        character(len=:), allocatable :: filter_message
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

        filtered = .false.
        if (allocated(request%filter)) filtered = len_trim(request%filter) > 0
        if (filtered) then
            call filter_parse(trim(request%filter), expression, filter_error, &
                filter_message)
            if (filter_error == 0 .and. expression%uses_affected .and. &
                .not. allocated(request%affected)) then
                filter_error = 1
                filter_message = 'affected() needs the Git delta of the project, '// &
                    'which this command does not supply'
            end if
            if (filter_error == 0 .and. expression%uses_failed .and. &
                .not. allocated(request%failed)) then
                filter_error = 1
                filter_message = 'failed() needs the last recorded run, '// &
                    'which this command does not supply'
            end if
            if (filter_error /= 0) then
                selection%status = SELECT_BAD_PATTERN
                selection%message = 'invalid filter: '//filter_message
                return
            end if
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
            if (model%cases(i)%explicit_only) named(i) = .false.
        end do
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
                if (model%cases(i)%explicit_only) cycle
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
                if (filtered) then
                    if (.not. filter_eval(expression, c%name, c%labels, &
                            layer_of(c), flag(request%affected, i), &
                            flag(request%failed, i))) then
                        call mark(i, CASE_FILTERED, 'excluded by filter '// &
                            trim(request%filter))
                        cycle
                    end if
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
        call sort_by_name(selection%cases)

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

        function layer_of(c) result(name)
            type(model_case_t), intent(in) :: c
            character(len=:), allocatable :: name

            name = ''
            if (c%layer > 0) name = model%layers(c%layer)%name
        end function layer_of

        logical function flag(mask, k)
            logical, intent(in), allocatable :: mask(:)
            integer, intent(in) :: k

            flag = .false.
            if (allocated(mask)) flag = mask(k)
        end function flag

        subroutine sort_by_name(idx)
            !! Bottom-up merge sort of case indices by name: O(n log n), stable.
            integer, intent(inout) :: idx(:)
            integer, allocatable :: tmp(:)
            integer :: n, width, lo, mid, hi, a, b, o

            n = size(idx)
            allocate(tmp(n))
            width = 1
            do while (width < n)
                lo = 1
                do while (lo <= n)
                    mid = min(lo + width - 1, n)
                    hi = min(lo + 2*width - 1, n)
                    a = lo
                    b = mid + 1
                    o = lo
                    do while (a <= mid .or. b <= hi)
                        if (b > hi) then
                            tmp(o) = idx(a)
                            a = a + 1
                        else if (a > mid) then
                            tmp(o) = idx(b)
                            b = b + 1
                        else if (model%cases(idx(b))%name < &
                                model%cases(idx(a))%name) then
                            tmp(o) = idx(b)
                            b = b + 1
                        else
                            tmp(o) = idx(a)
                            a = a + 1
                        end if
                        o = o + 1
                    end do
                    lo = lo + 2*width
                end do
                idx = tmp
                width = 2*width
            end do
        end subroutine sort_by_name

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

subroutine model_layering_sources(model, extra, paths)
    !! Project sources a layering check covers: the sources of every
    !! project-origin target and of every layer, then extra, each once in
    !! first-seen order. Dependency-origin targets contribute nothing.
    type(project_model_t), intent(in) :: model
    type(string_t), intent(in) :: extra(:)
    type(string_t), allocatable, intent(out) :: paths(:)
    type(string_t), allocatable :: trimmed(:)
    type(name_index_t) :: seen
    integer :: n, i, k

    n = 0
    allocate(paths(16))
    do i = 1, model%n_targets
        if (model%targets(i)%origin /= ORIGIN_PROJECT) cycle
        if (.not. allocated(model%targets(i)%sources)) cycle
        do k = 1, size(model%targets(i)%sources)
            call add(model%targets(i)%sources(k)%s)
        end do
    end do
    do i = 1, model%n_layers
        if (.not. allocated(model%layers(i)%sources)) cycle
        do k = 1, size(model%layers(i)%sources)
            call add(model%layers(i)%sources(k)%s)
        end do
    end do
    do k = 1, size(extra)
        call add(extra(k)%s)
    end do
    allocate(trimmed(n))
    trimmed(:) = paths(1:n)
    call move_alloc(trimmed, paths)

contains

    subroutine add(path)
        character(len=*), intent(in) :: path
        type(string_t), allocatable :: grown(:)
        integer :: existing

        if (len(path) == 0) return
        call index_insert(seen, path, n + 1, existing)
        if (existing /= 0) return
        n = n + 1
        if (n > size(paths)) then
            allocate(grown(2*size(paths)))
            grown(1:n - 1) = paths(1:n - 1)
            call move_alloc(grown, paths)
        end if
        paths(n)%s = path
    end subroutine add

end subroutine model_layering_sources

subroutine model_layering(model, units, report)
    !! Layering check, the same for every backend. A source in layer A that
    !! uses a module defined by a source in layer B violates the layering
    !! when B is outside the uses closure of A (A included). On CMake the
    !! layers are the project libraries and their uses the link graph.
    !!
    !! A source's layers are the layers listing it as a source or containing
    !! a target that lists it. Only modules defined by units are checked:
    !! intrinsic, external and dependency modules are ignored, as is a
    !! module the source defines itself or one with a provider in no layer.
    !! With several providers, any reachable one satisfies the use. Duplicate
    !! unit paths count once (the first). Cost: hashed lookups, linear in
    !! units, layer memberships and module uses, plus one closure walk per
    !! layer that has member sources.
    type(project_model_t), intent(in) :: model
    type(model_unit_t), intent(in) :: units(:)
    type(model_layering_t), intent(out) :: report
    type(name_index_t) :: path_index, module_index
    logical, allocatable :: active(:)
    integer, allocatable :: layer_first(:), layer_units(:)
    integer, allocatable :: unit_first(:), unit_layers(:), fill(:)
    integer, allocatable :: module_head(:), provider_next(:), provider_unit(:)
    integer, allocatable :: stamp(:), mark(:), queue(:), module_seen(:)
    integer :: nu, nl, n_pairs, n_modules, n_providers, n_violations
    integer :: u, l, k, j, t, s, m, e, p, existing, token, a, best, head, tail
    logical :: reached, unlayered_provider, other_provider

    if (.not. model%finalized) error stop 'model_layering: model not finalized'
    nu = size(units)
    nl = model%n_layers
    allocate(active(nu))
    do u = 1, nu
        call index_insert(path_index, units(u)%path, u, existing)
        active(u) = existing == 0
    end do

    ! Layer -> member units (CSR in layer order, each unit once per layer).
    allocate(layer_first(nl + 1), layer_units(max(16, nu)), stamp(nu))
    stamp = 0
    n_pairs = 0
    do l = 1, nl
        layer_first(l) = n_pairs + 1
        associate (ly => model%layers(l))
            if (allocated(ly%sources)) then
                do k = 1, size(ly%sources)
                    call add_member(ly%sources(k)%s, l)
                end do
            end if
            if (allocated(ly%targets)) then
                do k = 1, size(ly%targets)
                    t = ly%targets(k)
                    do s = 1, size(model%targets(t)%sources)
                        call add_member(model%targets(t)%sources(s)%s, l)
                    end do
                end do
            end if
        end associate
    end do
    layer_first(nl + 1) = n_pairs + 1

    ! Unit -> layers (CSR, ascending layer index) by counting.
    allocate(unit_first(nu + 1), unit_layers(max(1, n_pairs)), fill(nu))
    fill = 0
    do j = 1, n_pairs
        fill(layer_units(j)) = fill(layer_units(j)) + 1
    end do
    unit_first(1) = 1
    do u = 1, nu
        unit_first(u + 1) = unit_first(u) + fill(u)
        fill(u) = unit_first(u)
    end do
    do l = 1, nl
        do j = layer_first(l), layer_first(l + 1) - 1
            u = layer_units(j)
            unit_layers(fill(u)) = l
            fill(u) = fill(u) + 1
        end do
    end do

    ! Module name -> providing units (linked lists over provider entries).
    allocate(module_head(16), provider_next(16), provider_unit(16))
    n_modules = 0
    n_providers = 0
    do u = 1, nu
        if (.not. active(u)) cycle
        if (.not. allocated(units(u)%provides)) cycle
        do k = 1, size(units(u)%provides)
            m = index_lookup(module_index, to_lower(units(u)%provides(k)%s))
            if (m == 0) then
                n_modules = n_modules + 1
                call index_insert(module_index, &
                    to_lower(units(u)%provides(k)%s), n_modules, existing)
                call grow_ints(module_head, n_modules)
                module_head(n_modules) = 0
                m = n_modules
            end if
            n_providers = n_providers + 1
            call grow_ints(provider_next, n_providers)
            call grow_ints(provider_unit, n_providers)
            provider_unit(n_providers) = u
            provider_next(n_providers) = module_head(m)
            module_head(m) = n_providers
        end do
    end do

    allocate(report%violations(8))
    n_violations = 0
    allocate(mark(max(1, nl)), queue(max(1, nl)), module_seen(max(1, n_modules)))
    mark = 0
    module_seen = 0
    token = 0
    do a = 1, nl
        if (layer_first(a) == layer_first(a + 1)) cycle
        ! Uses closure of a: mark(x) == a.
        mark(a) = a
        queue(1) = a
        head = 1
        tail = 1
        do while (head <= tail)
            l = queue(head)
            head = head + 1
            do k = 1, size(model%layers(l)%uses)
                if (mark(model%layers(l)%uses(k)) == a) cycle
                mark(model%layers(l)%uses(k)) = a
                tail = tail + 1
                queue(tail) = model%layers(l)%uses(k)
            end do
        end do
        do j = layer_first(a), layer_first(a + 1) - 1
            u = layer_units(j)
            if (.not. allocated(units(u)%uses)) cycle
            token = token + 1
            do k = 1, size(units(u)%uses)
                m = index_lookup(module_index, to_lower(units(u)%uses(k)%s))
                if (m == 0) cycle
                if (module_seen(m) == token) cycle
                module_seen(m) = token
                call judge(u, m)
                if (reached .or. unlayered_provider .or. .not. other_provider) cycle
                call add_violation(u, units(u)%uses(k)%s, a, best)
            end do
        end do
    end do
    if (nl > 0) then
        allocate(report%unlayered(count(active .and. &
            unit_first(2:nu + 1) == unit_first(1:nu))))
    else
        allocate(report%unlayered(0))
    end if
    j = 0
    do u = 1, nu
        if (.not. active(u)) cycle
        if (unit_first(u + 1) > unit_first(u)) then
            report%checked = report%checked + 1
        else if (nl > 0) then
            j = j + 1
            report%unlayered(j)%s = units(u)%path
        end if
    end do
    call shrink_violations()

contains

    subroutine add_member(path, layer)
        character(len=*), intent(in) :: path
        integer, intent(in) :: layer
        integer :: unit

        unit = index_lookup(path_index, path)
        if (unit == 0) return
        if (stamp(unit) == layer) return
        stamp(unit) = layer
        n_pairs = n_pairs + 1
        call grow_ints(layer_units, n_pairs)
        layer_units(n_pairs) = unit
    end subroutine add_member

    subroutine judge(user, module)
        !! Providers of module seen from user in the layer being checked.
        integer, intent(in) :: user, module
        integer :: i

        reached = .false.
        unlayered_provider = .false.
        other_provider = .false.
        best = 0
        e = module_head(module)
        do while (e /= 0)
            p = provider_unit(e)
            e = provider_next(e)
            if (p == user) then
                reached = .true.
                cycle
            end if
            other_provider = .true.
            if (unit_first(p + 1) == unit_first(p)) then
                unlayered_provider = .true.
                cycle
            end if
            do i = unit_first(p), unit_first(p + 1) - 1
                if (mark(unit_layers(i)) == a) reached = .true.
            end do
            if (best == 0) then
                best = p
            else if (p < best) then
                best = p
            end if
        end do
    end subroutine judge

    subroutine add_violation(user, module, layer, provider)
        integer, intent(in) :: user, layer, provider
        character(len=*), intent(in) :: module
        type(model_violation_t), allocatable :: grown(:)

        n_violations = n_violations + 1
        if (n_violations > size(report%violations)) then
            allocate(grown(2*size(report%violations)))
            grown(1:n_violations - 1) = report%violations(1:n_violations - 1)
            call move_alloc(grown, report%violations)
        end if
        associate (v => report%violations(n_violations))
            v%source = units(user)%path
            v%module = module
            v%layer = model%layers(layer)%name
            v%provider = units(provider)%path
            v%provider_layer = model%layers(unit_layers(unit_first(provider)))%name
        end associate
    end subroutine add_violation

    subroutine shrink_violations()
        type(model_violation_t), allocatable :: kept(:)

        allocate(kept(n_violations))
        kept(:) = report%violations(1:n_violations)
        call move_alloc(kept, report%violations)
    end subroutine shrink_violations

end subroutine model_layering

subroutine model_affected(model, units, roots, changed, selection, widen_reasons, &
        force_reason)
    !! Restrict a selection to the cases affected by changed project files,
    !! the same for every backend (#189, #197). changed holds paths relative
    !! to the project root; model and unit paths are made relative by
    !! stripping one of roots (the project root as the model spells it).
    !! units are the scanned Fortran sources of the project.
    !!
    !! A case is affected when a changed file is
    !!  * a declared input of the case (fpm `input` attribute from a test
    !!    group, CTest REQUIRED_FILES), or a file below such a directory;
    !!  * a source in the module-use closure of the sources of its command
    !!    (or declared) targets; the closure follows `use` to every unit
    !!    providing the module, on both backends;
    !!  * a source of a target in the link closure of those targets that is
    !!    not reached through modules: a source providing no module that any
    !!    unit uses (programs, submodules, external procedures, unscanned
    !!    C sources), which the case links without a `use` edge; a file with
    !!    a used module and also such units counts the same (other_units).
    !! A case without a known command target is affected by every relevant
    !! change. Every selected case is affected (widened) when a build
    !! description (fpm.toml, CMakeLists.txt, *.cmake, CMake presets)
    !! changes, when a changed Fortran/C source belongs to no target and is
    !! no module some unit uses, or when force_reason is given. Other files
    !! (documentation, undeclared data) affect nothing. Unselected cases stay
    !! unselected; the affected subset keeps the selection order.
    !! model_case_digests hashes exactly these per-case file sets.
    type(project_model_t), intent(in) :: model
    type(model_unit_t), intent(in) :: units(:)
    type(string_t), intent(in) :: roots(:), changed(:)
    type(model_selection_t), intent(inout) :: selection
    type(string_t), allocatable, intent(out) :: widen_reasons(:)
    character(len=*), intent(in), optional :: force_reason
    type(affect_graph_t) :: g
    integer, allocatable :: kinds(:), change_unit(:), change_source(:), kept(:)
    type(string_t), allocatable :: cause(:)
    integer :: n_kept, k, c, i, ci
    logical :: relevant, hit, declared

    if (.not. model%finalized) error stop 'model_affected: model not finalized'
    allocate(widen_reasons(0))
    if (selection%status /= SELECT_OK) return
    call graph_build(g, model, units, roots)

    ! Classify the changes.
    allocate(kinds(size(changed)), change_unit(size(changed)), &
        change_source(size(changed)), cause(model%n_cases))
    do i = 1, model%n_cases
        cause(i)%s = ''
    end do
    relevant = .false.
    if (present(force_reason)) call widen(force_reason)
    do i = 1, size(changed)
        call graph_classify(g, changed(i)%s, kinds(i), change_unit(i), &
            change_source(i))
        select case (kinds(i))
        case (FILE_BUILD_DESCRIPTION)
            call widen('build description changed: '//changed(i)%s)
            cycle
        case (FILE_UNOWNED_SOURCE)
            call widen('changed source has no owning target: '//changed(i)%s)
        case (FILE_CLOSURE)
            relevant = .true.
        end select
        if (kinds(i) == FILE_UNOWNED_SOURCE) cycle
        declared = .false.
        do ci = 1, model%n_cases
            if (.not. case_input_covers(model, ci, roots, changed(i)%s)) cycle
            declared = .true.
            relevant = .true.
            if (len(cause(ci)%s) == 0) cause(ci)%s = &
                'affected: declared input '//changed(i)%s//' changed'
        end do
        if (kinds(i) == FILE_OTHER .and. .not. declared .and. &
                .not. documentation_like(changed(i)%s)) &
            call widen('changed file may be read by tests: '//changed(i)%s)
    end do

    ! Per selected case: declared inputs (above), else its closures.
    allocate(kept(size(selection%cases)))
    n_kept = 0
    do k = 1, size(selection%cases)
        c = selection%cases(k)
        if (size(widen_reasons) > 0) then
            call keep(c, 'affected: widened, '//widen_reasons(1)%s)
            cycle
        end if
        if (len(cause(c)%s) > 0) then
            call keep(c, cause(c)%s)
            cycle
        end if
        if (.not. relevant) then
            call drop(c)
            cycle
        end if
        if (.not. case_mapped(model, c)) then
            call keep(c, 'affected: no known command target')
            cycle
        end if
        call graph_closure(g, model, units, c)
        hit = .false.
        do i = 1, size(changed)
            if (kinds(i) /= FILE_CLOSURE) cycle
            if (graph_reached(g, change_unit(i), change_source(i))) then
                call keep(c, 'affected: '//changed(i)%s//' changed')
                hit = .true.
                exit
            end if
        end do
        if (.not. hit) call drop(c)
    end do
    selection%cases = kept(1:n_kept)
    if (n_kept == 0) then
        selection%status = SELECT_EMPTY
        selection%message = 'no test case is affected by the changes'
    end if

contains

    subroutine widen(reason)
        character(len=*), intent(in) :: reason

        call append_string(widen_reasons, reason)
        relevant = .true.
    end subroutine widen

    subroutine keep(ci, text)
        integer, intent(in) :: ci
        character(len=*), intent(in) :: text

        n_kept = n_kept + 1
        kept(n_kept) = ci
        selection%reasons(ci)%s = text
    end subroutine keep

    subroutine drop(ci)
        integer, intent(in) :: ci

        selection%disposition(ci) = CASE_NOT_AFFECTED
        selection%reasons(ci)%s = 'not affected by the changes'
    end subroutine drop

end subroutine model_affected

subroutine model_case_digests(model, units, roots, cases, files, contents, digests)
    !! Digest of the input closure of each case: the contents of every file
    !! whose change model_affected counts against the case (its declared
    !! inputs, its module-use and link closure sources, and the files that
    !! widen every case: build descriptions and sources owned by no
    !! target), plus the case name, labels and attributes. files are paths
    !! relative to the project root (as for model_affected's changed), with
    !! contents(i) the content identity of files(i); the digest follows the
    !! order of files. A case without a known command target covers every
    !! relevant file. Equal digests mean that no change between the two
    !! file sets affects the case.
    type(project_model_t), intent(in) :: model
    type(model_unit_t), intent(in) :: units(:)
    type(string_t), intent(in) :: roots(:), cases(:)
    type(string_t), intent(in) :: files(:), contents(:)
    character(len=64), allocatable, intent(out) :: digests(:)
    type(affect_graph_t) :: g
    type(builder_t) :: b
    integer, allocatable :: kinds(:), file_unit(:), file_source(:)
    logical, allocatable :: any_input(:), covered(:)
    integer :: f, k, c, j
    logical :: mapped

    if (.not. model%finalized) error stop 'model_case_digests: model not finalized'
    if (size(files) /= size(contents)) error stop 'model_case_digests: size mismatch'
    allocate(digests(size(cases)))
    call graph_build(g, model, units, roots)
    allocate(kinds(size(files)), file_unit(size(files)), &
        file_source(size(files)), any_input(size(files)), covered(size(files)))
    any_input = .false.
    do f = 1, size(files)
        call graph_classify(g, files(f)%s, kinds(f), file_unit(f), file_source(f))
    end do
    do c = 1, model%n_cases
        if (.not. has_inputs(model%cases(c))) cycle
        do f = 1, size(files)
            if (any_input(f)) cycle
            if (kinds(f) == FILE_BUILD_DESCRIPTION) cycle
            if (kinds(f) == FILE_UNOWNED_SOURCE) cycle
            any_input(f) = case_input_covers(model, c, roots, files(f)%s)
        end do
    end do
    do k = 1, size(cases)
        c = model_find_case(model, cases(k)%s)
        b = builder_t()
        call field(b, 'fo-case-closure-v1')
        call field(b, cases(k)%s)
        if (c == 0) then
            ! Not a model case: no closure is known, nothing can match it.
            digests(k) = ''
            cycle
        end if
        associate (mc => model%cases(c))
            call field(b, int_text(size(mc%labels)))
            do j = 1, size(mc%labels)
                call field(b, mc%labels(j)%s)
            end do
            call field(b, int_text(size(mc%attrs)))
            do j = 1, size(mc%attrs)
                call field(b, mc%attrs(j)%key)
                call field(b, mc%attrs(j)%value)
            end do
        end associate
        mapped = case_mapped(model, c)
        if (mapped) call graph_closure(g, model, units, c)
        covered = .false.
        if (has_inputs(model%cases(c))) then
            do f = 1, size(files)
                if (kinds(f) == FILE_BUILD_DESCRIPTION) cycle
                if (kinds(f) == FILE_UNOWNED_SOURCE) cycle
                covered(f) = case_input_covers(model, c, roots, files(f)%s)
            end do
        end if
        do f = 1, size(files)
            select case (kinds(f))
            case (FILE_BUILD_DESCRIPTION, FILE_UNOWNED_SOURCE)
                covered(f) = .true.
            case (FILE_OTHER)
                if (.not. any_input(f) .and. .not. documentation_like(files(f)%s)) &
                    covered(f) = .true.
            case (FILE_CLOSURE)
                if (.not. mapped) then
                    covered(f) = .true.
                else if (graph_reached(g, file_unit(f), file_source(f))) then
                    covered(f) = .true.
                end if
            end select
            if (.not. mapped .and. any_input(f)) covered(f) = .true.
            if (.not. covered(f)) cycle
            call field(b, files(f)%s)
            call field(b, contents(f)%s)
        end do
        digests(k) = sha256_string(builder_to_string(b))
    end do

contains

    logical function has_inputs(mc)
        type(model_case_t), intent(in) :: mc
        integer :: a

        has_inputs = .false.
        if (.not. allocated(mc%attrs)) return
        do a = 1, size(mc%attrs)
            if (mc%attrs(a)%key == 'input' .or. &
                mc%attrs(a)%key == 'REQUIRED_FILES') has_inputs = .true.
        end do
    end function has_inputs

end subroutine model_case_digests

subroutine graph_build(g, model, units, roots)
    !! Index the units (by relative path, provided modules) and the target
    !! sources (by relative path, owning targets) for model_affected.
    type(affect_graph_t), intent(out) :: g
    type(project_model_t), intent(in) :: model
    type(model_unit_t), intent(in) :: units(:)
    type(string_t), intent(in) :: roots(:)
    logical, allocatable :: module_used(:)
    integer :: u, k, m, t, s, existing, nu
    character(len=:), allocatable :: rel

    g%roots = roots
    nu = size(units)
    g%nu = nu
    allocate(g%module_head(max(16, nu)), g%provider_next(max(16, nu)), &
        g%provider_unit(max(16, nu)), g%plain_unit(nu))
    g%n_modules = 0
    g%n_providers = 0
    do u = 1, nu
        call index_insert(g%unit_index, graph_relative(roots, units(u)%path), u, &
            existing)
        do k = 1, size(units(u)%provides)
            m = graph_module_id(g, units(u)%provides(k)%s, .true.)
            g%n_providers = g%n_providers + 1
            call grow_ints(g%provider_next, g%n_providers)
            call grow_ints(g%provider_unit, g%n_providers)
            g%provider_unit(g%n_providers) = u
            g%provider_next(g%n_providers) = g%module_head(m)
            g%module_head(m) = g%n_providers
        end do
    end do
    allocate(module_used(g%n_modules))
    module_used = .false.
    do u = 1, nu
        do k = 1, size(units(u)%uses)
            m = index_lookup(g%module_index, to_lower(units(u)%uses(k)%s))
            if (m > 0) module_used(m) = .true.
        end do
    end do
    do u = 1, nu
        g%plain_unit(u) = .true.
        do k = 1, size(units(u)%provides)
            m = index_lookup(g%module_index, to_lower(units(u)%provides(k)%s))
            if (module_used(m)) g%plain_unit(u) = .false.
        end do
        if (units(u)%other_units) g%plain_unit(u) = .true.
    end do

    ! Target sources by relative path -> owning targets.
    allocate(g%owner_head(16), g%owner_next(16), g%owner_target(16))
    g%n_sources = 0
    g%n_owners = 0
    do t = 1, model%n_targets
        if (.not. allocated(model%targets(t)%sources)) cycle
        do k = 1, size(model%targets(t)%sources)
            rel = graph_relative(roots, model%targets(t)%sources(k)%s)
            s = index_lookup(g%source_index, rel)
            if (s == 0) then
                g%n_sources = g%n_sources + 1
                call index_insert(g%source_index, rel, g%n_sources, existing)
                call grow_ints(g%owner_head, g%n_sources)
                g%owner_head(g%n_sources) = 0
                s = g%n_sources
            end if
            g%n_owners = g%n_owners + 1
            call grow_ints(g%owner_next, g%n_owners)
            call grow_ints(g%owner_target, g%n_owners)
            g%owner_target(g%n_owners) = t
            g%owner_next(g%n_owners) = g%owner_head(s)
            g%owner_head(s) = g%n_owners
        end do
    end do
    allocate(g%tstamp(model%n_targets), g%ustamp(nu), g%queue(max(16, nu)))
    g%tstamp = 0
    g%ustamp = 0
    g%token = 0
end subroutine graph_build

subroutine graph_classify(g, path, kind, unit_id, source_id)
    !! How a project file (relative path) can affect cases.
    type(affect_graph_t), intent(in) :: g
    character(len=*), intent(in) :: path
    integer, intent(out) :: kind, unit_id, source_id

    unit_id = index_lookup(g%unit_index, path)
    source_id = index_lookup(g%source_index, path)
    kind = FILE_CLOSURE
    if (build_description(path)) then
        kind = FILE_BUILD_DESCRIPTION
    else if (header_like(path)) then
        ! Includers are not scanned: a header may be read by any case.
        kind = FILE_UNOWNED_SOURCE
    else if (unit_id == 0 .and. source_id == 0) then
        kind = FILE_OTHER
        if (source_like(path)) kind = FILE_UNOWNED_SOURCE
    else if (source_id == 0) then
        if (g%plain_unit(unit_id)) kind = FILE_UNOWNED_SOURCE
    end if
end subroutine graph_classify

subroutine graph_closure(g, model, units, ci)
    !! tstamp: link closure of the case targets; ustamp: module-use
    !! closure of their sources (both marked with a new token).
    type(affect_graph_t), intent(inout) :: g
    type(project_model_t), intent(in) :: model
    type(model_unit_t), intent(in) :: units(:)
    integer, intent(in) :: ci
    integer :: q, p, r, t, u, m, e, head, tail, n_case

    g%token = g%token + 1
    head = 1
    tail = 0
    if (model%cases(ci)%command_target > 0) &
        call push_target(model%cases(ci)%command_target)
    if (allocated(model%cases(ci)%declared_targets)) then
        do q = 1, size(model%cases(ci)%declared_targets)
            call push_target(model%cases(ci)%declared_targets(q))
        end do
    end if
    n_case = tail
    do while (head <= tail)
        t = g%queue(head)
        head = head + 1
        if (.not. allocated(model%targets(t)%link_deps)) cycle
        do q = 1, size(model%targets(t)%link_deps)
            call push_target(model%targets(t)%link_deps(q))
        end do
    end do
    ! Roots of the module walk: every source of the case targets, and the
    ! units of the other linked targets no module use leads to (programs,
    ! submodules, external procedures), whose own `use` statements count.
    do q = 1, tail
        associate (tg => model%targets(g%queue(q)))
            if (.not. allocated(tg%sources)) cycle
            do p = 1, size(tg%sources)
                r = index_lookup(g%unit_index, graph_relative(g%roots, tg%sources(p)%s))
                if (r == 0) cycle
                if (q > n_case .and. .not. g%plain_unit(r)) cycle
                g%ustamp(r) = -g%token
            end do
        end associate
    end do
    ! Module-use closure over the root units.
    head = 1
    tail = 0
    do r = 1, g%nu
        if (g%ustamp(r) == -g%token) then
            g%ustamp(r) = g%token
            tail = tail + 1
            g%queue(tail) = r
        end if
    end do
    do while (head <= tail)
        u = g%queue(head)
        head = head + 1
        do q = 1, size(units(u)%uses)
            m = index_lookup(g%module_index, to_lower(units(u)%uses(q)%s))
            if (m == 0) cycle
            e = g%module_head(m)
            do while (e > 0)
                r = g%provider_unit(e)
                if (g%ustamp(r) /= g%token) then
                    g%ustamp(r) = g%token
                    tail = tail + 1
                    g%queue(tail) = r
                end if
                e = g%provider_next(e)
            end do
        end do
    end do

contains

    subroutine push_target(tg)
        integer, intent(in) :: tg

        if (tg < 1) return
        if (g%tstamp(tg) == g%token) return
        g%tstamp(tg) = g%token
        tail = tail + 1
        call grow_ints(g%queue, tail)
        g%queue(tail) = tg
    end subroutine push_target

end subroutine graph_closure

logical function graph_reached(g, unit_id, source_id) result(reached)
    !! A file (unit_id, source_id) is in the module closure of the last
    !! graph_closure, or a source without a used module in a linked target.
    type(affect_graph_t), intent(in) :: g
    integer, intent(in) :: unit_id, source_id
    integer :: o

    reached = .false.
    if (unit_id > 0) then
        if (g%ustamp(unit_id) == g%token) then
            reached = .true.
            return
        end if
        if (.not. g%plain_unit(unit_id)) return
    end if
    if (source_id == 0) return
    o = g%owner_head(source_id)
    do while (o > 0)
        if (g%tstamp(g%owner_target(o)) == g%token) then
            reached = .true.
            return
        end if
        o = g%owner_next(o)
    end do
end function graph_reached

integer function graph_module_id(g, name, add) result(id)
    type(affect_graph_t), intent(inout) :: g
    character(len=*), intent(in) :: name
    logical, intent(in) :: add
    integer :: existing

    id = index_lookup(g%module_index, to_lower(name))
    if (id > 0 .or. .not. add) return
    g%n_modules = g%n_modules + 1
    call index_insert(g%module_index, to_lower(name), g%n_modules, existing)
    call grow_ints(g%module_head, g%n_modules)
    g%module_head(g%n_modules) = 0
    id = g%n_modules
end function graph_module_id

function graph_relative(roots, path) result(out)
    !! path without the first of roots it lies below.
    type(string_t), intent(in) :: roots(:)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: out
    integer :: r, n

    out = path
    do r = 1, size(roots)
        n = len(roots(r)%s)
        if (n == 0) cycle
        if (len(path) <= n + 1) cycle
        if (path(1:n + 1) /= roots(r)%s//'/') cycle
        out = path(n + 2:)
        return
    end do
end function graph_relative

logical function case_mapped(model, ci) result(mapped)
    type(project_model_t), intent(in) :: model
    integer, intent(in) :: ci

    mapped = model%cases(ci)%command_target > 0
    if (allocated(model%cases(ci)%declared_targets)) &
        mapped = mapped .or. size(model%cases(ci)%declared_targets) > 0
end function case_mapped

logical function case_input_covers(model, ci, roots, path) result(found)
    !! path is a declared input of case ci (fpm `input`, CTest
    !! REQUIRED_FILES) or lies below a declared directory.
    type(project_model_t), intent(in) :: model
    integer, intent(in) :: ci
    type(string_t), intent(in) :: roots(:)
    character(len=*), intent(in) :: path
    integer :: a, pos, next
    character(len=:), allocatable :: list, item

    found = .false.
    if (.not. allocated(model%cases(ci)%attrs)) return
    do a = 1, size(model%cases(ci)%attrs)
        associate (attr => model%cases(ci)%attrs(a))
            if (attr%key /= 'input' .and. attr%key /= 'REQUIRED_FILES') cycle
            list = attr%value
        end associate
        pos = 1
        do while (pos <= len(list))
            next = index(list(pos:), ';')
            if (next == 0) next = len(list) - pos + 2
            item = graph_relative(roots, list(pos:pos + next - 2))
            pos = pos + next
            if (len(item) == 0) cycle
            if (item /= path) then
                if (len(path) <= len(item)) cycle
                if (path(1:len(item) + 1) /= item//'/') cycle
            end if
            found = .true.
            return
        end do
    end do
end function case_input_covers

logical function build_description(path) result(yes)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: base

    base = path(index(path, '/', back=.true.) + 1:)
    yes = base == 'fpm.toml' .or. base == 'CMakeLists.txt' .or. &
        base == 'CMakePresets.json' .or. base == 'CMakeUserPresets.json'
    if (yes) return
    if (len(base) > 6) yes = to_lower(base(len(base) - 5:)) == '.cmake'
end function build_description

logical function source_like(path) result(yes)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: ext
    integer :: dot

    yes = .false.
    dot = index(path, '.', back=.true.)
    if (dot == 0 .or. dot < index(path, '/', back=.true.)) return
    ext = to_lower(path(dot + 1:))
    select case (ext)
    case ('f', 'for', 'ftn', 'f77', 'f90', 'f95', 'f03', 'f08', 'f18', &
            'fpp', 'fi', 'inc', 'h', 'hh', 'hpp', 'hxx', 'c', 'cc', 'cpp', 'cxx', &
            'fypp', 'pf')
        yes = .true.
    end select
end function source_like

logical function header_like(path) result(yes)
    !! A C or Fortran include file: its includers are not tracked.
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: ext
    integer :: dot

    yes = .false.
    dot = index(path, '.', back=.true.)
    if (dot == 0 .or. dot < index(path, '/', back=.true.)) return
    ext = to_lower(path(dot + 1:))
    yes = any(ext == ['h  ', 'hh ', 'hpp', 'hxx', 'inc', 'fi '])
end function header_like

logical function documentation_like(path) result(yes)
    !! A file no test reads: prose documentation, license files, repository
    !! metadata and the doc directories.
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: base, low, ext
    integer :: dot, slash

    slash = index(path, '/', back=.true.)
    base = to_lower(path(slash + 1:))
    yes = .true.
    if (path(1:min(len(path), 5)) == 'docs/' .or. &
        path(1:min(len(path), 4)) == 'doc/') return
    if (base(1:min(len(base), 4)) == '.git') return
    if (path(1:min(len(path), 8)) == '.github/') return
    dot = index(base, '.', back=.true.)
    ext = ''
    if (dot > 0) ext = base(dot + 1:)
    if (ext == 'md' .or. ext == 'rst') return
    low = base
    if (low(1:min(len(low), 6)) == 'readme' .or. &
        low(1:min(len(low), 7)) == 'license' .or. &
        low(1:min(len(low), 7)) == 'copying' .or. &
        low(1:min(len(low), 9)) == 'changelog') return
    yes = .false.
end function documentation_like

subroutine grow_ints(items, needed)
    !! Ensure size(items) >= needed, doubling.
    integer, allocatable, intent(inout) :: items(:)
    integer, intent(in) :: needed
    integer, allocatable :: grown(:)

    if (needed <= size(items)) return
    allocate(grown(max(needed, 2*size(items))))
    grown(1:size(items)) = items
    call move_alloc(grown, items)
end subroutine grow_ints

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
            call list('command', c%command)
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

end module fo_project_model
