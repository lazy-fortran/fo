module fo_input_inventory
    !! Canonical, typed declaration of files and roots that can affect an fo run.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, &
        c_long_long
    use fo_cache, only: HASH_LEN, cache_digest
    use fx_immutable_store, only: immutable_store_hash_file, IMMUTABLE_OK
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, fpm_config_allocate, &
        fpm_exe_t, &
        fpm_input_t, dep_kind, DEP_PATH, DEP_GIT, DEP_REGISTRY, &
        absolute_dependency_destination
    use fo_dep_resolve, only: normalize_path, join_path, resolved_src_t, &
        resolve_dev_dep_srcs, MAX_RESOLVED
    use fo_registry, only: registry_resolve, registry_config_path
    use fo_fs, only: fs_identity, fs_path_is_absolute
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file
    use fx_action_result_store, only: action_result_file_mode, ACTION_RESULT_OK
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096
    integer, parameter :: ALIAS_LEN = 256
    integer, parameter :: MAX_ROOTS = 64
    integer, parameter :: MAX_ALIASES = 16

    integer, parameter, public :: INPUT_FILE = 1
    integer, parameter, public :: INPUT_DIRECTORY = 2
    integer, parameter, public :: INPUT_SYMLINK = 3

    type, public :: input_root_t
        character(len=ALIAS_LEN) :: canonical_alias = ''
        character(len=PATH_LEN) :: physical_path = ''
        character(len=PATH_LEN) :: bundle_path = ''
        integer(c_long_long) :: device = 0_c_long_long
        integer(c_long_long) :: inode = 0_c_long_long
        integer :: alias_count = 0
        character(len=ALIAS_LEN) :: aliases(MAX_ALIASES) = ''
        character(len=PATH_LEN) :: bundle_paths(MAX_ALIASES) = ''
    end type input_root_t

    type, public :: input_declaration_t
        character(len=ALIAS_LEN) :: root_alias = ''
        character(len=PATH_LEN) :: relative_path = ''
        character(len=64) :: role = ''
        integer :: expected_kind = INPUT_FILE
        integer :: expected_mode = -1
        logical :: writable_at_execution = .false.
    end type input_declaration_t

    type, public :: input_entry_t
        character(len=ALIAS_LEN) :: root_alias = ''
        character(len=PATH_LEN) :: relative_path = ''
        character(len=64) :: role = ''
        integer :: kind = 0
        integer :: mode = -1
        logical :: writable_at_execution = .false.
        character(len=PATH_LEN) :: link_target = ''
        character(len=HASH_LEN) :: content_digest = ''
    end type input_entry_t

    type, public :: input_inventory_t
        type(input_root_t), allocatable :: roots(:)
        type(input_entry_t), allocatable :: entries(:)
        type(input_declaration_t), allocatable :: declarations(:)
        integer :: root_count = 0
        integer :: entry_count = 0
        integer :: entry_capacity = 0
        character(len=HASH_LEN) :: digest = ''
        character(len=1024) :: diagnostic = ''
        logical :: valid = .true.
        logical :: complete = .false.
    end type input_inventory_t

    public :: input_inventory_discover, input_inventory_declarations_from_config
    public :: input_inventory_revalidate, input_inventory_discover_cmake
    public :: input_inventory_rediscover

    interface
        integer(c_int) function c_list_input_tree(root, manifest, &
                exclude_root_outputs) bind(C, name='fo_c_generation_list_input_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), manifest(*)
            integer(c_int), value :: exclude_root_outputs
        end function c_list_input_tree
        integer(c_int) function c_list_cmake_input_tree(root, manifest, excluded) &
                bind(C, name='fo_c_generation_list_cmake_input_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), manifest(*), excluded(*)
        end function c_list_cmake_input_tree
    end interface

contains

    subroutine input_inventory_discover_cmake(project_dir, sources, bundles, &
            labels, metadata_dir, inventory, ierr, message)
        character(len=*), intent(in) :: project_dir, sources(:), bundles(:)
        character(len=*), intent(in) :: labels(:), metadata_dir
        type(input_inventory_t), intent(out) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: root_index, i, j, newline_at
        character(len=PATH_LEN) :: build_directory
        logical :: provider_dir_exists
        type(input_entry_t) :: provider_dir
        character(len=7), parameter :: provider_dirs(2) = &
            [character(len=7) :: 'include', 'lib']

        inventory = input_inventory_t()
        allocate(inventory%roots(MAX_ROOTS), inventory%entries(64), &
            inventory%declarations(0))
        inventory%entry_capacity = 64
        inventory%complete = .true.
        call add_root(inventory, 'project', project_dir, root_index, ierr, &
            message, 'project')
        if (ierr /= 0) return
        build_directory = ''
        call read_text_file(trim(metadata_dir)//'/build-directory.txt', build_directory)
        newline_at = index(build_directory, new_line('a'))
        if (newline_at > 0) build_directory = build_directory(:newline_at - 1)
        call scan_tree(inventory, root_index, 'project', project_dir, '', &
            'cmake-project-input', .false., ierr, message, cmake_scan=.true., &
            excluded_directory=trim(build_directory))
        if (ierr /= 0) return
        do i = 1, size(sources)
            call add_root(inventory, labels(i), sources(i), root_index, ierr, &
                message, bundles(i))
            if (ierr /= 0) return
            if (index(labels(i), 'cmake-provider:') == 1) then
                do j = 1, size(provider_dirs)
                    inquire(file=trim(sources(i))//'/'//trim(provider_dirs(j)), &
                            exist=provider_dir_exists)
                    if (.not. provider_dir_exists) cycle
                    provider_dir = input_entry_t()
                    provider_dir%root_alias = labels(i)
                    provider_dir%relative_path = provider_dirs(j)
                    provider_dir%role = 'cmake-provider-input'
                    provider_dir%kind = INPUT_DIRECTORY
                    provider_dir%mode = 0
                    call append_entry(inventory, provider_dir, ierr, message)
                    if (ierr /= 0) return
                end do
                call scan_configured_dir(sources(i), labels(i), 'include', &
                    'cmake-provider-input', .false., root_index, inventory, ierr, message)
                if (ierr /= 0) return
                call scan_configured_dir(sources(i), labels(i), 'lib', &
                    'cmake-provider-input', .false., root_index, inventory, ierr, message)
            else
                call scan_tree(inventory, root_index, labels(i), sources(i), '', &
                    'cmake-dependency-input', .false., ierr, message, cmake_scan=.true.)
            end if
            if (ierr /= 0) return
        end do
        call add_root(inventory, 'cmake-context', metadata_dir, root_index, &
            ierr, message, 'project/.fo-cmake')
        if (ierr /= 0) return
        call scan_tree(inventory, root_index, 'cmake-context', metadata_dir, '', &
            'cmake-configuration', .false., ierr, message)
        if (ierr /= 0) return
        call canonicalize_inventory(inventory, ierr, message)
        if (ierr /= 0) return
        inventory%valid = .true.
    end subroutine input_inventory_discover_cmake

    subroutine input_inventory_discover(project_dir, declarations, inventory, &
            ierr, message)
        character(len=*), intent(in) :: project_dir
        type(input_declaration_t), intent(in) :: declarations(:)
        type(input_inventory_t), intent(out) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(fpm_config_t) :: config
        type(resolved_src_t) :: resolved_dev_deps(MAX_RESOLVED)
        character(len=PATH_LEN) :: project_root
        character(len=:), allocatable :: dependency_root
        integer :: project_index, i, j, status, n_resolved_dev
        logical :: is_local

        inventory = input_inventory_t()
        inventory%declarations = declarations
        allocate(inventory%roots(MAX_ROOTS), inventory%entries(64))
        inventory%entry_capacity = 64
        inventory%complete = .true.
        ierr = 1
        message = ''
        call normalize_path(project_dir, project_root)
        if (len_trim(project_root) == 0 .or. len_trim(project_root) >= PATH_LEN) then
            message = 'project root is empty or exceeds the supported path length'
            call record_failure(inventory, message)
            return
        end if
        call fpm_config_parse(trim(project_root), config, status)
        if (status /= 0) then
            message = 'cannot parse required project manifest "fpm.toml"'
            call record_failure(inventory, message)
            return
        end if
        call mark_unmodeled_config(config, 'project', inventory, project_root)
        call add_root(inventory, 'project', trim(project_root), project_index, &
            ierr, message, 'project')
        if (ierr /= 0) then
            call record_failure(inventory, message)
            return
        end if
        call add_file_entry(inventory, project_index, 'project', 'fpm.toml', &
            'project-manifest', .false., ierr, message)
        if (ierr /= 0) then
            call record_failure(inventory, message)
            return
        end if
        call add_optional_manifest(inventory, project_index, 'project', &
            trim(project_root)//'/fpm.lock', 'dependency-lock', ierr, message)
        if (ierr /= 0) then
            call record_failure(inventory, message)
            return
        end if
        call discover_project_dirs(trim(project_root), config, inventory, ierr, &
            message)
        if (ierr /= 0) then
            call record_failure(inventory, message)
            return
        end if
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) /= DEP_PATH) cycle
            call discover_path_dependency(trim(project_root), config%deps(i)%path, &
                'dependency:'//trim(config%deps(i)%name), .true., inventory, &
                ierr, message, 0, 'project', trim(project_root), 'project')
            if (ierr /= 0) then
                call record_failure(inventory, message)
                return
            end if
        end do
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) cycle
            dependency_root = trim(project_root)//'/build/dependencies/'// &
                trim(config%deps(i)%name)
            if (dep_kind(config%deps(i)) == DEP_REGISTRY) then
                call registry_source(config%deps(i), project_root, dependency_root, &
                    inventory, ierr, message)
                if (ierr /= 0) return
            end if
            if (alias_root(inventory, 'dependency:'// &
                    trim(config%deps(i)%name)) /= 0) cycle
            call discover_acquired_dependency(trim(dependency_root), &
                'dependency:'//trim(config%deps(i)%name), &
                'project/build/dependencies/'//trim(config%deps(i)%name), &
                inventory, ierr, message, 0, trim(project_root), 'project')
            if (ierr /= 0) then
                call record_failure(inventory, message)
                return
            end if
        end do
        call scan_configured_dir(trim(project_root), 'project', &
            'build/dependencies/.fo-git-identities', 'dependency-git-identity', &
            .false., project_index, inventory, ierr, message)
        if (ierr /= 0) then
            call record_failure(inventory, message)
            return
        end if
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) /= DEP_PATH) cycle
            call discover_path_dependency(trim(project_root), &
                config%dev_deps(i)%path, &
                'dependency:'//trim(config%dev_deps(i)%name), .true., inventory, &
                ierr, message, 0, 'project', trim(project_root), 'project')
            if (ierr /= 0) then
                call record_failure(inventory, message)
                return
            end if
        end do
        call resolve_dev_dep_srcs(trim(project_root), resolved_dev_deps, &
            n_resolved_dev, status)
        if (status /= 0) then
            call mark_incomplete(inventory, &
                'cannot resolve configured development dependency roots')
        else
            do i = 1, n_resolved_dev
                is_local = .false.
                do j = 1, config%n_dev_deps
                    if (trim(config%dev_deps(j)%name) /= &
                            trim(resolved_dev_deps(i)%name)) cycle
                    is_local = dep_kind(config%dev_deps(j)) == DEP_PATH
                    exit
                end do
                if (is_local) cycle
                call discover_acquired_dependency(trim(resolved_dev_deps(i)%dir), &
                    'dependency:'//trim(resolved_dev_deps(i)%name), &
                    'project/build/dependencies/'// &
                    trim(resolved_dev_deps(i)%name), inventory, ierr, message, 0, &
                    trim(project_root), 'project')
                if (ierr /= 0) then
                    call record_failure(inventory, message)
                    return
                end if
            end do
        end if
        do i = 1, size(declarations)
            if (declaration_has_conflicting_intent(declarations, i)) then
                ierr = 1
                message = 'conflicting declarations for input: '// &
                    trim(declarations(i)%root_alias)//':'// &
                    trim(declarations(i)%relative_path)
                call record_failure(inventory, message)
                return
            end if
            if (declaration_is_duplicate(declarations, i)) then
                ierr = 1
                message = 'duplicate declared input role: '// &
                    trim(declarations(i)%root_alias)//':'// &
                    trim(declarations(i)%relative_path)
                call record_failure(inventory, message)
                return
            end if
            call add_declared_entry(inventory, declarations(i), ierr, message)
            if (ierr /= 0) then
                call record_failure(inventory, message)
                return
            end if
        end do
        call canonicalize_inventory(inventory, ierr, message)
        if (ierr /= 0) then
            call record_failure(inventory, message)
            return
        end if
        message = trim(inventory%diagnostic)
        ierr = 0
    end subroutine input_inventory_discover

    logical function declaration_is_duplicate(declarations, current)
        type(input_declaration_t), intent(in) :: declarations(:)
        integer, intent(in) :: current
        integer :: i

        declaration_is_duplicate = .false.
        do i = 1, current - 1
            if (trim(declarations(i)%root_alias) /= &
                    trim(declarations(current)%root_alias)) cycle
            if (trim(declarations(i)%relative_path) /= &
                    trim(declarations(current)%relative_path)) cycle
            if (trim(declarations(i)%role) /= &
                    trim(declarations(current)%role)) cycle
            declaration_is_duplicate = .true.
            return
        end do
    end function declaration_is_duplicate

    logical function declaration_has_conflicting_intent(declarations, current)
        type(input_declaration_t), intent(in) :: declarations(:)
        integer, intent(in) :: current
        integer :: i

        declaration_has_conflicting_intent = .false.
        do i = 1, current - 1
            if (trim(declarations(i)%root_alias) /= &
                    trim(declarations(current)%root_alias)) cycle
            if (trim(declarations(i)%relative_path) /= &
                    trim(declarations(current)%relative_path)) cycle
            if ((declarations(i)%writable_at_execution .neqv. &
                    declarations(current)%writable_at_execution) .or. &
                    declarations(i)%expected_kind /= &
                    declarations(current)%expected_kind .or. &
                    declarations(i)%expected_mode /= &
                    declarations(current)%expected_mode) then
                declaration_has_conflicting_intent = .true.
                return
            end if
        end do
    end function declaration_has_conflicting_intent

    subroutine input_inventory_declarations_from_config(project_dir, declarations, &
            ierr, message)
        character(len=*), intent(in) :: project_dir
        type(input_declaration_t), allocatable, intent(out) :: declarations(:)
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(fpm_config_t) :: config
        integer :: i

        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            message = 'cannot parse fixture declarations from fpm.toml'
            return
        end if
        allocate(declarations(config%n_fo_inputs))
        do i = 1, config%n_fo_inputs
            if (.not. config%fo_inputs(i)%path_seen .or. &
                    .not. config%fo_inputs(i)%role_seen) then
                ierr = 1
                message = 'each [[extra.fo.inputs]] row requires path and role'
                return
            end if
            select case (trim(config%fo_inputs(i)%role))
            case ('test-fixture', 'runtime-fixture')
            case default
                ierr = 1
                message = 'unsupported fixture role in [[extra.fo.inputs]]: '// &
                    trim(config%fo_inputs(i)%role)
                return
            end select
            declarations(i)%root_alias = 'project'
            if (config%fo_inputs(i)%root_seen) &
                declarations(i)%root_alias = trim(config%fo_inputs(i)%root)
            declarations(i)%relative_path = trim(config%fo_inputs(i)%path)
            declarations(i)%role = trim(config%fo_inputs(i)%role)
            select case (trim(config%fo_inputs(i)%kind))
            case ('file')
                declarations(i)%expected_kind = INPUT_FILE
            case ('directory')
                declarations(i)%expected_kind = INPUT_DIRECTORY
            case default
                ierr = 1
                message = 'unsupported fixture input kind: '// &
                    trim(config%fo_inputs(i)%kind)
                return
            end select
            declarations(i)%writable_at_execution = &
                config%fo_inputs(i)%writable_at_execution
        end do
        ierr = 0
        message = ''
    end subroutine input_inventory_declarations_from_config

    subroutine input_inventory_rediscover(project_dir, declarations, expected, &
            observed, ierr, message, materialized)
        character(len=*), intent(in) :: project_dir
        type(input_declaration_t), intent(in) :: declarations(:)
        type(input_inventory_t), intent(in) :: expected
        type(input_inventory_t), intent(out) :: observed
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        logical, intent(in), optional :: materialized
        character(len=PATH_LEN), allocatable :: sources(:), bundles(:)
        character(len=ALIAS_LEN), allocatable :: labels(:)
        character(len=PATH_LEN) :: metadata_dir, base, path
        integer :: i, n, metadata_index, slash
        logical :: from_bundle

        metadata_index = alias_root(expected, 'cmake-context')
        if (metadata_index == 0) then
            call input_inventory_discover(project_dir, declarations, observed, &
                ierr, message)
            return
        end if
        from_bundle = .false.
        if (present(materialized)) from_bundle = materialized
        base = project_dir
        slash = index(trim(base), '/', back=.true.)
        if (slash > 0) base = base(:slash - 1)
        allocate(sources(expected%root_count), bundles(expected%root_count), &
            labels(expected%root_count))
        metadata_dir = ''
        n = 0
        do i = 1, expected%root_count
            path = expected%roots(i)%physical_path
            if (from_bundle) path = trim(base)//'/'// &
                trim(expected%roots(i)%bundle_path)
            if (i == metadata_index) then
                metadata_dir = path
                cycle
            end if
            if (trim(expected%roots(i)%canonical_alias) == 'project') cycle
            n = n + 1
            sources(n) = path
            bundles(n) = expected%roots(i)%bundle_path
            labels(n) = expected%roots(i)%canonical_alias
        end do
        call input_inventory_discover_cmake(project_dir, sources(:n), bundles(:n), &
            labels(:n), trim(metadata_dir), observed, ierr, message)
    end subroutine input_inventory_rediscover

    subroutine input_inventory_revalidate(project_dir, declarations, expected, &
            ierr, message)
        character(len=*), intent(in) :: project_dir
        type(input_declaration_t), intent(in) :: declarations(:)
        type(input_inventory_t), intent(in) :: expected
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(input_inventory_t) :: observed
        integer :: i, j, observed_root

        call input_inventory_rediscover(project_dir, declarations, expected, &
            observed, ierr, message)
        if (ierr /= 0) return
        if (.not. expected%valid) then
            ierr = 1
            message = 'cannot revalidate an invalid expected input inventory'
            return
        end if
        if (observed%digest /= expected%digest .or. &
                observed%root_count /= expected%root_count .or. &
                (observed%complete .neqv. expected%complete) .or. &
                (observed%valid .neqv. expected%valid)) then
            ierr = 1
            message = 'input inventory changed during generation capture'
            return
        end if
        do i = 1, expected%root_count
            observed_root = alias_root(observed, &
                trim(expected%roots(i)%canonical_alias))
            if (observed_root == 0) then
                ierr = 1
                message = 'input root alias changed during generation capture: '// &
                    trim(expected%roots(i)%canonical_alias)
                return
            end if
            if (observed%roots(observed_root)%device /= &
                    expected%roots(i)%device .or. &
                    observed%roots(observed_root)%inode /= expected%roots(i)%inode) then
                ierr = 1
                message = 'input root identity changed during generation capture: '// &
                    trim(expected%roots(i)%canonical_alias)
                return
            end if
            do j = 1, expected%roots(i)%alias_count
                observed_root = alias_root(observed, &
                    trim(expected%roots(i)%aliases(j)))
                if (observed_root == 0) then
                    ierr = 1
                    message = 'input root alias mapping changed during capture'
                    return
                end if
                if (bundle_path_for_alias(observed%roots(observed_root), &
                        trim(expected%roots(i)%aliases(j))) /= &
                        bundle_path_for_alias(expected%roots(i), &
                        trim(expected%roots(i)%aliases(j)))) then
                    ierr = 1
                    message = 'input root bundle mapping changed during capture'
                    return
                end if
            end do
        end do
        ierr = 0
        message = ''
    end subroutine input_inventory_revalidate

    recursive subroutine discover_acquired_dependency(dependency_root, alias, &
            bundle_path, inventory, ierr, message, depth, acquisition_root, &
            acquisition_bundle)
        character(len=*), intent(in) :: dependency_root, alias, bundle_path
        character(len=*), intent(in) :: acquisition_root, acquisition_bundle
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer, intent(in) :: depth
        type(fpm_config_t) :: config
        character(len=PATH_LEN) :: manifest
        character(len=:), allocatable :: child_root, child_bundle, child_alias
        integer :: root_index, i, status
        logical :: exists

        ierr = 0
        message = ''
        if (depth > 16) then
            call mark_incomplete(inventory, trim(alias)// &
                ' exceeds acquired-dependency closure depth 16')
            return
        end if
        if (len_trim(dependency_root) >= PATH_LEN .or. &
                len_trim(bundle_path) >= PATH_LEN .or. &
                len_trim(alias) > ALIAS_LEN) then
            call mark_incomplete(inventory, trim(alias)// &
                ' exceeds supported acquired-dependency path or alias length')
            return
        end if
        if (alias_root(inventory, alias) /= 0) return
        manifest = trim(dependency_root)//'/fpm.toml'
        inquire(file=trim(manifest), exist=exists)
        if (.not. exists) then
            call mark_incomplete(inventory, trim(alias)// &
                ' is not present in the existing FPM resolved-dependency tree')
            return
        end if
        call fpm_config_parse(trim(dependency_root), config, status)
        if (status /= 0) then
            call mark_incomplete(inventory, trim(alias)// &
                ' has an unreadable resolved FPM manifest')
            return
        end if
        call mark_unmodeled_config(config, alias, inventory, acquisition_root)
        call add_root(inventory, alias, dependency_root, root_index, ierr, message, &
            trim(bundle_path))
        if (ierr /= 0) return
        call add_file_entry(inventory, root_index, alias, 'fpm.toml', &
            'dependency-manifest', .false., ierr, message)
        if (ierr /= 0) return
        call scan_configured_dir(trim(dependency_root), alias, config%source_dir, &
            'dependency-source', .false., root_index, inventory, ierr, message)
        if (ierr /= 0) return
        call scan_library_include_dirs(trim(dependency_root), alias, config, &
            root_index, inventory, ierr, message)
        if (ierr /= 0) return
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) /= DEP_PATH) cycle
            call discover_path_dependency(trim(dependency_root), &
                config%deps(i)%path, trim(alias)//'/'//trim(config%deps(i)%name), &
                .true., inventory, ierr, message, depth + 1, trim(bundle_path), &
                acquisition_root, acquisition_bundle)
            if (ierr /= 0) return
        end do
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) cycle
            ! FPM acquires transitive Git packages in the root's flat tree.
            child_root = trim(acquisition_root)//'/build/dependencies/'// &
                trim(config%deps(i)%name)
            if (dep_kind(config%deps(i)) == DEP_REGISTRY) then
                call registry_source(config%deps(i), acquisition_root, child_root, &
                    inventory, ierr, message)
                if (ierr /= 0) return
            end if
            child_alias = trim(alias)//'/dependency:'//trim(config%deps(i)%name)
            child_bundle = trim(acquisition_bundle)//'/build/dependencies/'// &
                trim(config%deps(i)%name)
            call discover_acquired_dependency(trim(child_root), trim(child_alias), &
                trim(child_bundle), inventory, ierr, message, depth + 1, &
                acquisition_root, acquisition_bundle)
            if (ierr /= 0) return
        end do
    end subroutine discover_acquired_dependency

    subroutine record_failure(inventory, message)
        type(input_inventory_t), intent(inout) :: inventory
        character(len=*), intent(in) :: message

        inventory%complete = .false.
        inventory%valid = .false.
        inventory%diagnostic = trim(message)
    end subroutine record_failure

    subroutine discover_project_dirs(project_root, config, inventory, ierr, message)
        character(len=*), intent(in) :: project_root
        type(fpm_config_t), intent(in) :: config
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        integer :: root_index, i
        ierr = 0
        message = ''
        root_index = alias_root(inventory, 'project')
        call scan_configured_dir(project_root, 'project', config%source_dir, &
            'build-source', .false., root_index, inventory, ierr, message)
        if (ierr /= 0) return
        if (config%auto_executables) then
            call scan_configured_dir(project_root, 'project', config%app_dir, &
                'executable-source', .false., root_index, inventory, ierr, message)
            if (ierr /= 0) return
        end if
        if (config%auto_tests) then
            call scan_configured_dir(project_root, 'project', config%test_dir, &
                'test-oracle', .false., root_index, inventory, ierr, message)
            if (ierr /= 0) return
        end if
        if (config%auto_examples) then
            call scan_configured_dir(project_root, 'project', config%example_dir, &
                'example-source', .false., root_index, inventory, ierr, message)
            if (ierr /= 0) return
        end if
        call scan_configured_dir(project_root, 'project', 'test-support', &
            'test-support', .false., root_index, inventory, ierr, message)
        if (ierr /= 0) return
        call scan_library_include_dirs(project_root, 'project', config, &
            root_index, inventory, ierr, message)
        if (ierr /= 0) return
        do i = 1, config%n_exes
            call scan_configured_dir(project_root, 'project', &
                config%exes(i)%source_dir, 'executable-source', .true., &
                root_index, inventory, ierr, message)
            if (ierr /= 0) return
            call add_target_main(project_root, 'project', config%exes(i), &
                'executable-main', inventory, ierr, message)
            if (ierr /= 0) return
        end do
        do i = 1, config%n_tests
            call scan_configured_dir(project_root, 'project', &
                config%tests(i)%source_dir, 'test-oracle', .true., root_index, &
                inventory, ierr, message)
            if (ierr /= 0) return
            call add_target_main(project_root, 'project', config%tests(i), &
                'test-main', inventory, ierr, message)
            if (ierr /= 0) return
        end do
        do i = 1, config%n_examples
            call scan_configured_dir(project_root, 'project', &
                config%examples(i)%source_dir, 'example-source', .true., &
                root_index, inventory, ierr, message)
            if (ierr /= 0) return
            call add_target_main(project_root, 'project', config%examples(i), &
                'example-main', inventory, ierr, message)
            if (ierr /= 0) return
        end do
    end subroutine discover_project_dirs

    subroutine scan_library_include_dirs(project_root, alias, config, root_index, &
            inventory, ierr, message)
        character(len=*), intent(in) :: project_root, alias
        type(fpm_config_t), intent(in) :: config
        integer, intent(in) :: root_index
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: i

        ierr = 0
        message = ''
        do i = 1, config%n_include_dirs
            call scan_configured_dir(project_root, alias, config%include_dirs(i), &
                'include', .false., root_index, inventory, ierr, message)
            if (ierr /= 0) return
        end do
    end subroutine scan_library_include_dirs

    subroutine scan_configured_dir(project_root, alias, relative_dir, role, &
            required, root_index, inventory, ierr, message)
        character(len=*), intent(in) :: project_root, alias, relative_dir, role
        logical, intent(in) :: required
        integer, intent(in) :: root_index
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: rel, source_dir
        logical :: exists

        ierr = 0
        message = ''
        if (trim(relative_dir) == '.') then
            rel = ''
        else
            call validate_relative_path(relative_dir, rel, ierr, message)
            if (ierr /= 0) return
        end if
        source_dir = trim(project_root)
        if (len_trim(rel) > 0) source_dir = trim(source_dir)//'/'//trim(rel)
        inquire(file=trim(source_dir), exist=exists)
        if (.not. exists) then
            if (required) then
                ierr = 1
                message = 'declared FPM source directory is missing: '//trim(rel)
            end if
            return
        end if
        call scan_tree(inventory, root_index, alias, trim(project_root), trim(rel), &
            role, .false., ierr, message)
    end subroutine scan_configured_dir

    subroutine add_target_main(project_root, alias, target, role, inventory, &
            ierr, message)
        character(len=*), intent(in) :: project_root, alias, role
        type(fpm_exe_t), intent(in) :: target
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: rel
        character(len=:), allocatable :: path

        call validate_relative_path(trim(target%source_dir)//'/'// &
            trim(target%main), rel, ierr, message)
        if (ierr /= 0) return
        path = trim(project_root)//'/'//trim(rel)
        call add_file_entry(inventory, alias_root(inventory, alias), alias, &
            trim(rel), role, .false., ierr, message)
        if (ierr /= 0) message = trim(message)//' ('//trim(path)//')'
    end subroutine add_target_main

    recursive subroutine discover_path_dependency(parent_root, dependency_path, alias, &
            follow_regular_deps, inventory, ierr, message, depth, parent_bundle, &
            acquisition_root, acquisition_bundle)
        character(len=*), intent(in) :: parent_root, dependency_path, alias
        character(len=*), intent(in) :: parent_bundle
        character(len=*), intent(in) :: acquisition_root, acquisition_bundle
        logical, intent(in) :: follow_regular_deps
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer, intent(in) :: depth

        type(fpm_config_t) :: config
        character(len=PATH_LEN) :: dependency_root, bundle_path
        character(len=:), allocatable :: child_root, child_bundle, child_alias
        integer :: root_index, i, status
        logical :: exists

        ierr = 1
        message = ''
        if (depth > 16) then
            message = 'path dependency closure exceeds depth 16 at '//trim(alias)
            return
        end if
        if (len_trim(dependency_path) == 0) then
            message = 'empty FPM path dependency: '//trim(alias)
            return
        end if
        if (fs_path_is_absolute(dependency_path)) then
            call normalize_path(dependency_path, dependency_root)
            bundle_path = 'project/'// &
                absolute_dependency_destination(dependency_path)
        else
            call validate_relative_path(dependency_path, dependency_root, &
                ierr, message, allow_parent=.true.)
            if (ierr /= 0) return
            call bundle_destination(parent_bundle, dependency_path, bundle_path, &
                ierr, message)
            if (ierr /= 0) return
            call join_path(parent_root, dependency_path, dependency_root)
        end if
        inquire(file=trim(dependency_root)//'/fpm.toml', exist=exists)
        if (.not. exists) then
            ierr = 1
            message = 'declared path dependency has no fpm.toml: '//trim(alias)
            return
        end if
        call fpm_config_parse(trim(dependency_root), config, status)
        if (status /= 0) then
            ierr = 1
            message = 'cannot parse path dependency manifest: '//trim(alias)
            return
        end if
        call mark_unmodeled_config(config, alias, inventory, acquisition_root)
        call add_root(inventory, alias, trim(dependency_root), root_index, &
            ierr, message, trim(bundle_path))
        if (ierr /= 0) return
        call add_file_entry(inventory, root_index, alias, 'fpm.toml', &
            'dependency-manifest', .false., ierr, message)
        if (ierr /= 0) return
        call scan_configured_dir(trim(dependency_root), alias, config%source_dir, &
            'dependency-source', .false., root_index, inventory, ierr, message)
        if (ierr /= 0) return
        call scan_library_include_dirs(trim(dependency_root), alias, config, &
            root_index, inventory, ierr, message)
        if (ierr /= 0) return
        if (.not. follow_regular_deps) return
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) /= DEP_PATH) cycle
            call discover_path_dependency(trim(dependency_root), &
                config%deps(i)%path, &
                trim(alias)//'/'//trim(config%deps(i)%name), .true., &
                inventory, ierr, message, depth + 1, trim(bundle_path), &
                acquisition_root, acquisition_bundle)
            if (ierr /= 0) return
        end do
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) cycle
            child_root = trim(acquisition_root)//'/build/dependencies/'// &
                trim(config%deps(i)%name)
            if (dep_kind(config%deps(i)) == DEP_REGISTRY) then
                call registry_source(config%deps(i), acquisition_root, child_root, &
                    inventory, ierr, message)
                if (ierr /= 0) return
            end if
            child_alias = trim(alias)//'/dependency:'//trim(config%deps(i)%name)
            child_bundle = trim(acquisition_bundle)//'/build/dependencies/'// &
                trim(config%deps(i)%name)
            call discover_acquired_dependency(trim(child_root), trim(child_alias), &
                trim(child_bundle), inventory, ierr, message, depth + 1, &
                acquisition_root, acquisition_bundle)
            if (ierr /= 0) return
        end do
        ierr = 0
    end subroutine discover_path_dependency

    subroutine registry_source(dep, project_root, directory, inventory, ierr, message)
        use fo_fpm_config, only: fpm_dep_t
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: project_root
        character(len=:), allocatable, intent(inout) :: directory
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: source, config_path
        integer :: slash, root_index, i
        type(fpm_config_t), allocatable :: root_config
        logical :: exists, selected

        call fpm_config_allocate(root_config)
        call fpm_config_parse(project_root, root_config, ierr)
        if (ierr /= 0) return
        selected = .false.
        do i = 1, root_config%n_deps
            if (trim(root_config%deps(i)%name) /= trim(dep%name)) cycle
            selected = .true.
            select case (dep_kind(root_config%deps(i)))
            case (DEP_PATH)
                call join_path(project_root, root_config%deps(i)%path, source)
            case (DEP_GIT)
                source = trim(project_root)//'/build/dependencies/'//trim(dep%name)
            case (DEP_REGISTRY)
                call registry_resolve(root_config%deps(i), project_root, source, ierr)
            end select
            exit
        end do
        if (.not. selected) call registry_resolve(dep, project_root, source, ierr)
        if (ierr /= 0) then
            message = 'cannot resolve registry dependency '//trim(dep%name)
            return
        end if
        directory = trim(source)
        call registry_config_path(config_path, ierr)
        if (ierr /= 0) return
        inquire(file=trim(config_path), exist=exists)
        if (.not. exists) return
        if (alias_root(inventory, 'registry-config') /= 0) return
        slash = index(trim(config_path), '/', back=.true.)
        call add_root(inventory, 'registry-config', config_path(:slash - 1), &
            root_index, ierr, message, 'registry-config')
        if (ierr /= 0) return
        call add_file_entry(inventory, root_index, 'registry-config', &
            trim(config_path(slash + 1:)), &
            'registry-configuration', .false., ierr, message)
    end subroutine registry_source

    subroutine mark_unmodeled_config(config, alias, inventory, resolved_root)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: alias
        type(input_inventory_t), intent(inout) :: inventory
        character(len=*), intent(in), optional :: resolved_root
        integer :: i
        logical :: acquired

        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) cycle
            if (dep_kind(config%deps(i)) == DEP_REGISTRY) cycle
            acquired = .false.
            if (present(resolved_root)) then
                inquire(file=trim(resolved_root)//'/build/dependencies/'// &
                    trim(config%deps(i)%name)//'/fpm.toml', exist=acquired)
            end if
            if (acquired) cycle
            call mark_incomplete(inventory, trim(alias)//' dependency '// &
                trim(config%deps(i)%name)// &
                ' is not available in the existing acquired-dependency tree')
        end do
        if (config%n_external_modules > 0) then
            call mark_incomplete(inventory, trim(alias)// &
                ' declares external compiler modules')
        end if
        if (config%n_link_libs > 0) then
            call mark_incomplete(inventory, trim(alias)// &
                ' declares external link libraries')
        end if
        if (config%openmp .or. config%blas .or. config%mpi .or. &
                config%hdf5 .or. config%netcdf .or. config%stdlib .or. &
                config%minpack) then
            call mark_incomplete(inventory, trim(alias)// &
                ' declares an external FPM metapackage')
        end if
        do i = 1, config%n_flags
            if (index(config%flags(i), '-I') == 0) cycle
            call mark_incomplete(inventory, trim(alias)// &
                ' has an unmodeled custom include path flag')
            exit
        end do
    end subroutine mark_unmodeled_config

    subroutine mark_incomplete(inventory, reason)
        type(input_inventory_t), intent(inout) :: inventory
        character(len=*), intent(in) :: reason

        inventory%complete = .false.
        if (len_trim(inventory%diagnostic) == 0) then
            inventory%diagnostic = 'inventory closure is incomplete: '//trim(reason)
        else if (len_trim(inventory%diagnostic) < len(inventory%diagnostic) - 4) then
            inventory%diagnostic = trim(inventory%diagnostic)//'; '//trim(reason)
        end if
    end subroutine mark_incomplete

    subroutine add_root(inventory, alias, physical_path, root_index, ierr, message, &
            bundle_path)
        type(input_inventory_t), intent(inout) :: inventory
        character(len=*), intent(in) :: alias, physical_path
        integer, intent(out) :: root_index, ierr
        character(len=*), intent(out) :: message
        character(len=*), intent(in) :: bundle_path
        character(len=PATH_LEN) :: normalized, checked_bundle_path
        integer(c_long_long) :: device, inode
        integer :: i
        logical :: ok

        root_index = 0
        ierr = 1
        message = ''
        if (.not. valid_alias(alias)) then
            message = 'invalid logical input-root alias: '//trim(alias)
            return
        end if
        call validate_relative_path(bundle_path, checked_bundle_path, ierr, message)
        if (ierr /= 0) then
            message = 'invalid bundle destination for input-root alias '// &
                trim(alias)//': '//trim(message)
            return
        end if
        call normalize_path(physical_path, normalized)
        if (len_trim(normalized) == 0 .or. len_trim(normalized) >= PATH_LEN) then
            message = 'input root path is empty or too long for alias '//trim(alias)
            return
        end if
        call fs_identity(trim(normalized), device, inode, ok)
        if (.not. ok) then
            message = 'declared input root does not exist: '//trim(alias)
            return
        end if
        do i = 1, inventory%root_count
            if (.not. root_has_alias(inventory%roots(i), alias)) cycle
            if (inventory%roots(i)%device /= device .or. &
                    inventory%roots(i)%inode /= inode) then
                message = 'ambiguous logical input-root alias: '//trim(alias)
                return
            end if
        end do
        do i = 1, inventory%root_count
            if (inventory%roots(i)%device /= device .or. &
                    inventory%roots(i)%inode /= inode) cycle
            root_index = i
            call add_root_alias(inventory%roots(i), alias, checked_bundle_path, ierr, &
                message)
            if (ierr /= 0) return
            if (trim(alias) == 'project' .or. &
                    (trim(inventory%roots(i)%canonical_alias) /= 'project' .and. &
                    trim(alias) < trim(inventory%roots(i)%canonical_alias))) then
                inventory%roots(i)%canonical_alias = trim(alias)
                inventory%roots(i)%physical_path = trim(normalized)
                inventory%roots(i)%bundle_path = trim(checked_bundle_path)
            end if
            ierr = 0
            return
        end do
        if (inventory%root_count >= MAX_ROOTS) then
            message = 'input inventory exceeds 64 distinct physical roots'
            return
        end if
        inventory%root_count = inventory%root_count + 1
        root_index = inventory%root_count
        inventory%roots(root_index)%device = device
        inventory%roots(root_index)%inode = inode
        inventory%roots(root_index)%physical_path = trim(normalized)
        inventory%roots(root_index)%canonical_alias = trim(alias)
        inventory%roots(root_index)%bundle_path = trim(checked_bundle_path)
        call add_root_alias(inventory%roots(root_index), alias, &
            checked_bundle_path, ierr, &
            message)
        if (ierr /= 0) return
        ierr = 0
    end subroutine add_root

    subroutine add_root_alias(root, alias, bundle_path, ierr, message)
        type(input_root_t), intent(inout) :: root
        character(len=*), intent(in) :: alias, bundle_path
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: i

        ierr = 0
        message = ''
        do i = 1, root%alias_count
            if (trim(root%aliases(i)) /= trim(alias)) cycle
            if (trim(root%bundle_paths(i)) /= trim(bundle_path)) then
                ierr = 1
                message = 'ambiguous bundle mapping for input-root alias: '// &
                    trim(alias)
            end if
            return
        end do
        if (root%alias_count >= MAX_ALIASES) then
            ierr = 1
            message = 'input root has more than 16 aliases: '// &
                trim(root%canonical_alias)
            return
        end if
        root%alias_count = root%alias_count + 1
        root%aliases(root%alias_count) = trim(alias)
        root%bundle_paths(root%alias_count) = trim(bundle_path)
        call sort_aliases(root)
    end subroutine add_root_alias

    subroutine sort_aliases(root)
        type(input_root_t), intent(inout) :: root
        character(len=ALIAS_LEN) :: value
        character(len=PATH_LEN) :: path_value
        integer :: i, j

        do i = 2, root%alias_count
            value = root%aliases(i)
            path_value = root%bundle_paths(i)
            j = i - 1
            do while (j >= 1)
                if (llt(trim(value), trim(root%aliases(j)))) then
                    root%aliases(j + 1) = root%aliases(j)
                    root%bundle_paths(j + 1) = root%bundle_paths(j)
                    j = j - 1
                else
                    exit
                end if
            end do
            root%aliases(j + 1) = value
            root%bundle_paths(j + 1) = path_value
        end do
        if (root%alias_count > 0) then
            j = 1
            do i = 1, root%alias_count
                if (trim(root%aliases(i)) == 'project') j = i
            end do
            root%canonical_alias = root%aliases(j)
            root%bundle_path = root%bundle_paths(j)
        end if
    end subroutine sort_aliases

    integer function alias_root(inventory, alias) result(index_root)
        type(input_inventory_t), intent(in) :: inventory
        character(len=*), intent(in) :: alias
        integer :: i, j

        index_root = 0
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                if (trim(inventory%roots(i)%aliases(j)) == trim(alias)) then
                    index_root = i
                    return
                end if
            end do
        end do
    end function alias_root

    function bundle_path_for_alias(root, alias) result(path)
        type(input_root_t), intent(in) :: root
        character(len=*), intent(in) :: alias
        character(len=PATH_LEN) :: path
        integer :: i

        path = ''
        do i = 1, root%alias_count
            if (trim(root%aliases(i)) /= trim(alias)) cycle
            path = root%bundle_paths(i)
            return
        end do
    end function bundle_path_for_alias

    subroutine bundle_destination(parent_bundle, dependency_path, destination, &
            ierr, message)
        character(len=*), intent(in) :: parent_bundle, dependency_path
        character(len=*), intent(out) :: destination
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: joined, normalized

        joined = trim(parent_bundle)//'/'//trim(dependency_path)
        call normalize_path(trim(joined), normalized)
        destination = ''
        ierr = 1
        message = ''
        if (len_trim(normalized) == 0 .or. normalized(1:1) == '/' .or. &
                trim(normalized) == '..' .or. index(trim(normalized), '../') == 1 .or. &
                len_trim(normalized) >= PATH_LEN) then
            message = 'path dependency escapes the frozen bundle: '// &
                trim(dependency_path)
            return
        end if
        destination = trim(normalized)
        ierr = 0
    end subroutine bundle_destination

    subroutine add_optional_manifest(inventory, root_index, alias, path, role, &
            ierr, message)
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(in) :: root_index
        character(len=*), intent(in) :: alias, path, role
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        logical :: exists

        inquire(file=trim(path), exist=exists)
        ierr = 0
        message = ''
        if (.not. exists) return
        call add_file_entry(inventory, root_index, alias, 'fpm.lock', role, &
            .false., ierr, message)
    end subroutine add_optional_manifest

    subroutine add_file_entry(inventory, root_index, alias, relative_path, role, &
            writable, ierr, message)
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(in) :: root_index
        character(len=*), intent(in) :: alias, relative_path, role
        logical, intent(in) :: writable
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(input_entry_t) :: entry
        character(len=PATH_LEN) :: path
        integer :: mode, mode_status

        call validate_relative_path(relative_path, entry%relative_path, ierr, &
            message)
        if (ierr /= 0) return
        if (root_index <= 0 .or. root_index > inventory%root_count) then
            ierr = 1
            message = 'input entry refers to an unknown physical root'
            return
        end if
        if (.not. root_has_alias(inventory%roots(root_index), alias)) then
            ierr = 1
            message = 'input entry refers to an ambiguous root alias: '//trim(alias)
            return
        end if
        path = trim(inventory%roots(root_index)%physical_path)//'/'// &
            trim(entry%relative_path)
        call action_result_file_mode(trim(path), mode, mode_status)
        if (mode_status /= ACTION_RESULT_OK) then
            ierr = 1
            message = 'required input file is missing or unsupported: '// &
                trim(alias)//':'//trim(entry%relative_path)
            return
        end if
        call immutable_store_hash_file(trim(path), entry%content_digest, &
            mode_status)
        if (mode_status /= IMMUTABLE_OK .or. &
            len_trim(entry%content_digest) /= HASH_LEN) then
            ierr = 1
            message = 'cannot hash required input file: '// &
                trim(alias)//':'//trim(entry%relative_path)
            return
        end if
        entry%root_alias = trim(alias)
        entry%role = trim(role)
        entry%kind = INPUT_FILE
        entry%mode = iand(mode, 511)
        entry%writable_at_execution = writable
        call append_entry(inventory, entry, ierr, message)
    end subroutine add_file_entry

    logical function root_has_alias(root, alias)
        type(input_root_t), intent(in) :: root
        character(len=*), intent(in) :: alias
        integer :: i

        root_has_alias = .false.
        do i = 1, root%alias_count
            if (trim(root%aliases(i)) /= trim(alias)) cycle
            root_has_alias = .true.
            return
        end do
    end function root_has_alias

    subroutine scan_tree(inventory, root_index, alias, physical_root, prefix, &
            role, writable, ierr, message, cmake_scan, excluded_directory)
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(in) :: root_index
        character(len=*), intent(in) :: alias, physical_root, prefix, role
        logical, intent(in) :: writable
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: manifest, line, relative, target
        character(len=PATH_LEN) :: symlink_parts(1)
        character(len=HASH_LEN) :: hash
        integer :: rc, unit, ios, kind, decode_status
        type(input_entry_t) :: entry
        character(kind=c_char, len=:), allocatable :: c_root, c_manifest
        logical :: whole_root_scan
        logical, intent(in), optional :: cmake_scan
        character(len=*), intent(in), optional :: excluded_directory
        character(kind=c_char, len=:), allocatable :: c_excluded
        integer(c_int) :: exclude_root_outputs

        ierr = 1
        message = ''
        call make_tmpfile('fo-input-inventory', manifest)
        whole_root_scan = len_trim(prefix) == 0 .or. trim(prefix) == '.'
        if (whole_root_scan) then
            c_root = trim(physical_root)//c_null_char
            exclude_root_outputs = 1_c_int
        else
            c_root = trim(physical_root)//'/'//trim(prefix)//c_null_char
            exclude_root_outputs = 0_c_int
        end if
        if (present(cmake_scan)) then
            if (cmake_scan .and. whole_root_scan) exclude_root_outputs = 2_c_int
        end if
        c_manifest = trim(manifest)//c_null_char
        if (present(excluded_directory)) then
            c_excluded = trim(excluded_directory)//c_null_char
            rc = c_list_cmake_input_tree(c_root, c_manifest, c_excluded)
        else
            rc = c_list_input_tree(c_root, c_manifest, exclude_root_outputs)
        end if
        if (rc /= 0) then
            call delete_tmpfile(trim(manifest))
            message = 'cannot enumerate declared input root: '//trim(alias)
            return
        end if
        open (newunit=unit, file=trim(manifest), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) then
            call delete_tmpfile(trim(manifest))
            message = 'cannot read input-root enumeration: '//trim(alias)
            return
        end if
        ierr = 0
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) < 7) cycle
            kind = 0
            select case (line(1:1))
            case ('F')
                kind = INPUT_FILE
                relative = trim(line(7:))
            case ('D')
                kind = INPUT_DIRECTORY
                relative = trim(line(7:))
            case ('L')
                call decode_link_record(line, relative, target, decode_status)
                if (decode_status /= 0) then
                    ierr = 1
                    message = 'malformed symlink record from input enumerator'
                    exit
                end if
                kind = INPUT_SYMLINK
            case default
                ierr = 1
                message = 'unknown record from input enumerator'
                exit
            end select
            if (.not. whole_root_scan) then
                relative = trim(prefix)//'/'//trim(relative)
            end if
            if (alias == 'cmake-context') then
                if (relative == 'dependencies' .or. &
                    index(relative, 'dependencies/') == 1) cycle
                ! Provider artifacts have their own authored input roots.
                if (index(relative, 'providers/') == 1) then
                    if (index(relative(11:), '/') > 0 .and. &
                        index(relative, '/CMakeCache.txt', back=.true.) == 0) cycle
                end if
            end if
            if (present(cmake_scan)) then
                if (cmake_scan) then
                    if (inside_cmake_output(physical_root, relative)) cycle
                end if
            end if
            if (ignored_input_path(trim(relative), kind == INPUT_DIRECTORY, &
                    whole_root_scan)) cycle
            if (.not. under_prefix(trim(relative), trim(prefix))) cycle
            if (trim(relative) == trim(prefix)) cycle
            entry = input_entry_t()
            entry%root_alias = trim(alias)
            entry%relative_path = trim(relative)
            entry%role = trim(role)
            entry%kind = kind
            entry%writable_at_execution = writable
            select case (kind)
            case (INPUT_DIRECTORY)
                entry%mode = 0
            case (INPUT_FILE)
                call action_result_file_mode(trim(physical_root)//'/'// &
                    trim(relative), entry%mode, decode_status)
                if (decode_status /= ACTION_RESULT_OK) then
                    ierr = 1
                    message = 'input file changed during enumeration: '// &
                        trim(alias)//':'//trim(relative)
                    exit
                end if
                entry%mode = iand(entry%mode, 511)
                call immutable_store_hash_file(trim(physical_root)//'/'// &
                    trim(relative), hash, decode_status)
                if (decode_status /= IMMUTABLE_OK .or. &
                    len_trim(hash) /= HASH_LEN) then
                    ierr = 1
                    message = 'cannot hash enumerated input: '//trim(relative)
                    exit
                end if
                entry%content_digest = hash
            case (INPUT_SYMLINK)
                entry%mode = 0
                entry%link_target = trim(target)
                if (len_trim(target) > len(symlink_parts(1)) - &
                    len('symlink:')) then
                    ierr = 1
                    message = 'symlink target is too long: '//trim(relative)
                    exit
                end if
                symlink_parts(1) = 'symlink:'//trim(target)
                hash = cache_digest(symlink_parts, 1)
                entry%content_digest = hash
            end select
            call append_entry(inventory, entry, ierr, message)
            if (ierr /= 0) exit
        end do
        close (unit)
        call delete_tmpfile(trim(manifest))
        if (ios > 0) then
            ierr = 1
            message = 'failed while reading input-root enumeration'
        end if
    end subroutine scan_tree

    subroutine decode_link_record(line, relative, target, ierr)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: relative, target
        integer, intent(out) :: ierr
        integer :: path_length, target_length, ios, start, i, hi, lo

        relative = ''
        target = ''
        ierr = 1
        if (len_trim(line) < 28) return
        read (line(7:14), '(i8)', iostat=ios) path_length
        if (ios /= 0) return
        read (line(16:23), '(i8)', iostat=ios) target_length
        if (ios /= 0) return
        if (path_length < 1 .or. target_length < 1) return
        if (path_length > len(relative) .or. &
            target_length > len(target)) return
        start = 25
        if (start - 1 + path_length + 2 * target_length /= len_trim(line)) return
        relative = line(start:start + path_length - 1)
        do i = 1, target_length
            hi = hex_value(line(start + path_length + 2 * i - 2:start + &
                path_length + 2 * i - 2))
            lo = hex_value(line(start + path_length + 2 * i - 1:start + &
                path_length + 2 * i - 1))
            if (hi < 0 .or. lo < 0) return
            target(i:i) = achar(16 * hi + lo)
        end do
        ierr = 0
    end subroutine decode_link_record

    integer function hex_value(character) result(value)
        character(len=1), intent(in) :: character
        integer :: code

        code = iachar(character)
        select case (code)
        case (iachar('0'):iachar('9'))
            value = code - iachar('0')
        case (iachar('a'):iachar('f'))
            value = code - iachar('a') + 10
        case (iachar('A'):iachar('F'))
            value = code - iachar('A') + 10
        case default
            value = -1
        end select
    end function hex_value

    logical function under_prefix(path, prefix)
        character(len=*), intent(in) :: path, prefix
        integer :: n

        n = len_trim(prefix)
        under_prefix = .false.
        if (n == 0) then
            under_prefix = .true.
        else if (len_trim(path) > n) then
            if (path(1:n) == prefix) then
                under_prefix = path(n + 1:n + 1) == '/'
            end if
        end if
    end function under_prefix

    logical function inside_cmake_output(root, relative) result(ignored)
        character(len=*), intent(in) :: root, relative
        integer :: slash, start
        logical :: exists
        ignored = .false.
        if (relative == '.fo-cmake' .or. index(relative, '.fo-cmake/') == 1) then
            ignored = .true.
            return
        end if
        start = 1
        do
            slash = index(relative(start:), '/')
            if (slash == 0) exit
            slash = slash + start - 1
            inquire(file=trim(root)//'/'//relative(:slash - 1)// &
                '/CMakeCache.txt', exist=exists)
            if (exists) then
                ignored = .true.
                return
            end if
            start = slash + 1
        end do
    end function inside_cmake_output

    logical function ignored_input_path(path, is_directory, whole_root_scan)
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_directory
        logical, intent(in) :: whole_root_scan
        integer :: start, stop, component_index
        character(len=128) :: component

        ignored_input_path = .false.
        if (len_trim(path) == 0) return
        start = 1
        component_index = 0
        do
            stop = index(path(start:), '/')
            if (stop == 0) then
                component = path(start:)
            else
                component = path(start:start + stop - 2)
            end if
            component_index = component_index + 1
            select case (trim(component))
            case ('.git', '.hg', '.svn', '.bzr', '.gremlin', '.fo')
                if (stop /= 0 .or. is_directory) then
                    ignored_input_path = .true.
                    return
                end if
            case ('.cache', 'build', 'cache', 'caches', 'session', &
                    'sessions', 'log', 'logs')
                if (whole_root_scan .and. component_index == 1 .and. &
                        (stop /= 0 .or. is_directory)) then
                    ignored_input_path = .true.
                    return
                end if
            end select
            if (stop == 0) exit
            start = start + stop
        end do
    end function ignored_input_path

    subroutine validate_relative_path(path, normalized, ierr, message, &
            allow_parent)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: normalized
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        logical, intent(in), optional :: allow_parent
        integer :: i, start, n
        logical :: permit_parent, saw_normal

        normalized = ''
        message = ''
        ierr = 1
        n = len_trim(path)
        permit_parent = .false.
        if (present(allow_parent)) permit_parent = allow_parent
        saw_normal = .false.
        if (n == 0 .or. n >= len(normalized)) then
            message = 'input path is empty or too long'
            return
        end if
        if (path(1:1) == '/' .or. index(path(:n), achar(10)) > 0 .or. &
                index(path(:n), achar(13)) > 0 .or. index(path(:n), '|') > 0) then
            message = 'input path must be relative and contain no delimiters'
            return
        end if
        start = 1
        do i = 1, n + 1
            if (i <= n) then
                if (path(i:i) /= '/') cycle
            end if
            if (i == start) then
                message = 'input path contains an empty component'
                return
            end if
            if (path(start:i - 1) == '..') then
                if (.not. permit_parent .or. saw_normal) then
                    message = 'input path contains a parent traversal'
                    return
                end if
            else if (path(start:i - 1) == '.') then
                message = 'input path contains an ambiguous dot component'
                return
            else
                saw_normal = .true.
            end if
            start = i + 1
        end do
        normalized = path(:n)
        ierr = 0
    end subroutine validate_relative_path

    logical function valid_alias(alias)
        character(len=*), intent(in) :: alias
        integer :: i, code

        valid_alias = .false.
        if (len_trim(alias) == 0 .or. len_trim(alias) > ALIAS_LEN) return
        do i = 1, len_trim(alias)
            code = iachar(alias(i:i))
            if ((code >= iachar('a') .and. code <= iachar('z')) .or. &
                    (code >= iachar('A') .and. code <= iachar('Z')) .or. &
                    (code >= iachar('0') .and. code <= iachar('9')) .or. &
                    index('_-:/', alias(i:i)) > 0) cycle
            return
        end do
        valid_alias = .true.
    end function valid_alias

    logical function valid_role(role)
        character(len=*), intent(in) :: role
        integer :: i, code

        valid_role = .false.
        if (len_trim(role) == 0 .or. len_trim(role) > 64) return
        do i = 1, len_trim(role)
            code = iachar(role(i:i))
            if ((code >= iachar('a') .and. code <= iachar('z')) .or. &
                    (code >= iachar('A') .and. code <= iachar('Z')) .or. &
                    (code >= iachar('0') .and. code <= iachar('9')) .or. &
                    index('_-:', role(i:i)) > 0) cycle
            return
        end do
        valid_role = .true.
    end function valid_role

    subroutine add_declared_entry(inventory, declaration, ierr, message)
        type(input_inventory_t), intent(inout) :: inventory
        type(input_declaration_t), intent(in) :: declaration
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(input_entry_t) :: directory_entry
        character(len=PATH_LEN) :: relative_path
        integer :: root_index, i

        root_index = alias_root(inventory, declaration%root_alias)
        if (root_index == 0) then
            ierr = 1
            message = 'declared input uses unknown root alias: '// &
                trim(declaration%root_alias)
            return
        end if
        if (.not. valid_role(declaration%role)) then
            ierr = 1
            message = 'declared input has an empty or invalid role'
            return
        end if
        if (declaration%expected_kind == INPUT_DIRECTORY) then
            call validate_relative_path(declaration%relative_path, relative_path, &
                ierr, message)
            if (ierr /= 0) return
            directory_entry = input_entry_t()
            directory_entry%root_alias = trim(declaration%root_alias)
            directory_entry%relative_path = trim(relative_path)
            directory_entry%role = trim(declaration%role)
            directory_entry%kind = INPUT_DIRECTORY
            directory_entry%mode = 0
            directory_entry%writable_at_execution = &
                declaration%writable_at_execution
            call append_entry(inventory, directory_entry, ierr, message)
            if (ierr /= 0) return
            call scan_tree(inventory, root_index, declaration%root_alias, &
                trim(inventory%roots(root_index)%physical_path), &
                trim(relative_path), declaration%role, &
                declaration%writable_at_execution, ierr, message)
            return
        end if
        if (declaration%expected_kind /= INPUT_FILE) then
            ierr = 1
            message = 'only regular file and directory declarations are supported'
            return
        end if
        call add_file_entry(inventory, root_index, declaration%root_alias, &
            declaration%relative_path, declaration%role, &
            declaration%writable_at_execution, ierr, message)
        if (ierr /= 0) return
        if (declaration%expected_mode < 0) return
        do i = 1, inventory%entry_count
            if (trim(inventory%entries(i)%root_alias) /= &
                    trim(declaration%root_alias)) cycle
            if (trim(inventory%entries(i)%relative_path) /= &
                    trim(declaration%relative_path)) cycle
            if (inventory%entries(i)%mode /= declaration%expected_mode) then
                ierr = 1
                message = 'declared input mode does not match: '// &
                    trim(declaration%relative_path)
            end if
            return
        end do
    end subroutine add_declared_entry

    subroutine append_entry(inventory, entry, ierr, message)
        type(input_inventory_t), intent(inout) :: inventory
        type(input_entry_t), intent(in) :: entry
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(input_entry_t), allocatable :: grown(:)
        integer :: i, capacity

        ierr = 0
        message = ''
        do i = 1, inventory%entry_count
            if (trim(inventory%entries(i)%root_alias) /= trim(entry%root_alias)) cycle
            if (trim(inventory%entries(i)%relative_path) /= &
                    trim(entry%relative_path)) cycle
            if (trim(inventory%entries(i)%role) /= trim(entry%role)) cycle
            if (inventory%entries(i)%kind /= entry%kind .or. &
                    inventory%entries(i)%mode /= entry%mode .or. &
                    inventory%entries(i)%content_digest /= entry%content_digest &
                    .or. inventory%entries(i)%link_target /= entry%link_target) then
                ierr = 1
                message = 'input changed or conflicts during inventory: '// &
                    trim(entry%root_alias)//':'//trim(entry%relative_path)
                return
            end if
            if (inventory%entries(i)%writable_at_execution .neqv. &
                    entry%writable_at_execution) then
                ierr = 1
                message = 'input mutability conflicts during inventory: '// &
                    trim(entry%root_alias)//':'//trim(entry%relative_path)
            end if
            return
        end do
        if (inventory%entry_count == inventory%entry_capacity) then
            capacity = max(64, 2 * inventory%entry_capacity)
            allocate(grown(capacity))
            if (inventory%entry_count > 0) then
                grown(:inventory%entry_count) = &
                    inventory%entries(:inventory%entry_count)
            end if
            call move_alloc(grown, inventory%entries)
            inventory%entry_capacity = capacity
        end if
        inventory%entry_count = inventory%entry_count + 1
        inventory%entries(inventory%entry_count) = entry
    end subroutine append_entry

    subroutine canonicalize_inventory(inventory, ierr, message)
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(input_entry_t) :: value
        character(len=2 * PATH_LEN + ALIAS_LEN + 3 * 64 + HASH_LEN + 64) :: part
        character(len=:), allocatable :: parts(:)
        integer :: i, j, capacity, root_index, retained, n_parts

        ierr = 0
        message = ''
        do i = 1, inventory%entry_count
            root_index = alias_root(inventory, inventory%entries(i)%root_alias)
            if (root_index == 0) then
                ierr = 1
                message = 'input entry lost its physical root alias'
                return
            end if
            inventory%entries(i)%root_alias = &
                inventory%roots(root_index)%canonical_alias
        end do
        do i = 2, inventory%entry_count
            value = inventory%entries(i)
            j = i - 1
            do while (j >= 1)
                if (.not. entry_after(inventory%entries(j), value)) exit
                inventory%entries(j + 1) = inventory%entries(j)
                j = j - 1
            end do
            inventory%entries(j + 1) = value
        end do
        retained = 0
        do i = 1, inventory%entry_count
            if (retained > 0) then
                if (same_entry_key(inventory%entries(retained), &
                        inventory%entries(i))) then
                    if (.not. same_entry_value(inventory%entries(retained), &
                            inventory%entries(i))) then
                        ierr = 1
                        message = 'conflicting duplicate input after '// &
                            'root canonicalization'
                        return
                    end if
                    cycle
                end if
            end if
            retained = retained + 1
            inventory%entries(retained) = inventory%entries(i)
        end do
        inventory%entry_count = retained
        capacity = inventory%entry_count
        do i = 1, inventory%root_count
            capacity = capacity + inventory%roots(i)%alias_count
        end do
        capacity = max(1, capacity)
        allocate(character(len=len(part)) :: parts(capacity))
        n_parts = 0
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                n_parts = n_parts + 1
                write (part, '("root|",a,"|",a)') &
                    trim(inventory%roots(i)%aliases(j)), &
                    trim(inventory%roots(i)%bundle_paths(j))
                parts(n_parts) = trim(part)
            end do
        end do
        do i = 1, inventory%entry_count
            root_index = alias_root(inventory, &
                trim(inventory%entries(i)%root_alias))
            if (root_index == 0) then
                ierr = 1
                message = 'input entry lost its physical root while hashing'
                return
            end if
            n_parts = n_parts + 1
            write (part, '(a,"|",a,"|",a,"|",i0,"|",i0,"|",l1,"|",a)') &
                'entry|'//trim(inventory%entries(i)%root_alias)//'|'// &
                trim(inventory%roots(root_index)%bundle_path), &
                trim(inventory%entries(i)%relative_path), &
                trim(inventory%entries(i)%role), inventory%entries(i)%kind, &
                inventory%entries(i)%mode, &
                inventory%entries(i)%writable_at_execution, &
                trim(inventory%entries(i)%content_digest)
            parts(n_parts) = trim(part)
        end do
        do i = 2, n_parts
            part = parts(i)
            j = i - 1
            do while (j >= 1)
                if (.not. llt(trim(part), trim(parts(j)))) exit
                parts(j + 1) = parts(j)
                j = j - 1
            end do
            parts(j + 1) = part
        end do
        inventory%digest = cache_digest(parts(:n_parts), n_parts)
    end subroutine canonicalize_inventory

    logical function same_entry_key(left, right)
        type(input_entry_t), intent(in) :: left, right

        same_entry_key = trim(left%root_alias) == trim(right%root_alias) .and. &
            trim(left%relative_path) == trim(right%relative_path) .and. &
            trim(left%role) == trim(right%role)
    end function same_entry_key

    logical function same_entry_value(left, right)
        type(input_entry_t), intent(in) :: left, right

        same_entry_value = left%kind == right%kind .and. left%mode == right%mode &
            .and. (left%writable_at_execution .eqv. right%writable_at_execution)
        if (.not. same_entry_value) return
        same_entry_value = left%link_target == right%link_target .and. &
            left%content_digest == right%content_digest
    end function same_entry_value

    logical function entry_after(left, right)
        type(input_entry_t), intent(in) :: left, right

        if (trim(left%root_alias) /= trim(right%root_alias)) then
            entry_after = lgt(trim(left%root_alias), trim(right%root_alias))
        else if (trim(left%relative_path) /= trim(right%relative_path)) then
            entry_after = lgt(trim(left%relative_path), trim(right%relative_path))
        else
            entry_after = lgt(trim(left%role), trim(right%role))
        end if
    end function entry_after

end module fo_input_inventory
