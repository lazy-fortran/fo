module fo_input_inventory
    !! Canonical, typed declaration of files and roots that can affect an fo run.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, &
        c_long_long
    use fo_cache, only: HASH_LEN, cache_digest, cache_file_digest
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, fpm_exe_t, &
        dep_kind, DEP_PATH
    use fo_dep_resolve, only: normalize_path
    use fo_fs, only: fs_identity
    use fo_util, only: make_tmpfile, delete_tmpfile
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
        integer(c_long_long) :: device = 0_c_long_long
        integer(c_long_long) :: inode = 0_c_long_long
        integer :: alias_count = 0
        character(len=ALIAS_LEN) :: aliases(MAX_ALIASES) = ''
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
        integer :: root_count = 0
        integer :: entry_count = 0
        integer :: entry_capacity = 0
        character(len=HASH_LEN) :: digest = ''
        character(len=1024) :: diagnostic = ''
        logical :: complete = .false.
    end type input_inventory_t

    public :: input_inventory_discover

    interface
        integer(c_int) function c_list_tree(root, manifest) &
                bind(C, name='fo_c_generation_list_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), manifest(*)
        end function c_list_tree
    end interface

contains

    subroutine input_inventory_discover(project_dir, declarations, inventory, &
            ierr, message)
        character(len=*), intent(in) :: project_dir
        type(input_declaration_t), intent(in) :: declarations(:)
        type(input_inventory_t), intent(out) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(fpm_config_t) :: config
        character(len=PATH_LEN) :: project_root
        integer :: project_index, i, status

        inventory = input_inventory_t()
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
        call mark_unmodeled_config(config, 'project', inventory)
        call add_root(inventory, 'project', trim(project_root), project_index, &
            ierr, message)
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
                ierr, message, 0)
            if (ierr /= 0) then
                call record_failure(inventory, message)
                return
            end if
        end do
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) /= DEP_PATH) cycle
            call discover_path_dependency(trim(project_root), &
                config%dev_deps(i)%path, &
                'dependency:'//trim(config%dev_deps(i)%name), .true., inventory, &
                ierr, message, 0)
            if (ierr /= 0) then
                call record_failure(inventory, message)
                return
            end if
        end do
        do i = 1, size(declarations)
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

    subroutine record_failure(inventory, message)
        type(input_inventory_t), intent(inout) :: inventory
        character(len=*), intent(in) :: message

        inventory%complete = .false.
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
        call scan_configured_dir(project_root, 'project', 'include', 'include', &
            .false., root_index, inventory, ierr, message)
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
        call validate_relative_path(relative_dir, rel, ierr, message)
        if (ierr /= 0) return
        source_dir = trim(project_root)//'/'//trim(rel)
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

    subroutine discover_path_dependency(parent_root, dependency_path, alias, &
            follow_regular_deps, inventory, ierr, message, depth)
        character(len=*), intent(in) :: parent_root, dependency_path, alias
        logical, intent(in) :: follow_regular_deps
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer, intent(in) :: depth

        type(fpm_config_t) :: config
        character(len=PATH_LEN) :: dependency_root
        integer :: root_index, i, status
        logical :: exists

        ierr = 1
        message = ''
        if (depth > 16) then
            message = 'path dependency closure exceeds depth 16 at '//trim(alias)
            return
        end if
        if (len_trim(dependency_path) == 0 .or. &
                dependency_path(1:1) == '/') then
            message = 'unsupported absolute or empty FPM path dependency: '// &
                trim(alias)
            return
        end if
        call validate_relative_path(dependency_path, dependency_root, ierr, message, &
            allow_parent=.true.)
        if (ierr /= 0) return
        if (dependency_root(1:1) /= '/') then
            dependency_root = trim(parent_root)//'/'//trim(dependency_root)
            call normalize_path(trim(dependency_root), dependency_root)
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
        call mark_unmodeled_config(config, alias, inventory)
        call add_root(inventory, alias, trim(dependency_root), root_index, &
            ierr, message)
        if (ierr /= 0) return
        call add_file_entry(inventory, root_index, alias, 'fpm.toml', &
            'dependency-manifest', .false., ierr, message)
        if (ierr /= 0) return
        call scan_configured_dir(trim(dependency_root), alias, config%source_dir, &
            'dependency-source', .false., root_index, inventory, ierr, message)
        if (ierr /= 0) return
        call scan_configured_dir(trim(dependency_root), alias, 'include', &
            'include', .false., root_index, inventory, ierr, message)
        if (ierr /= 0) return
        if (.not. follow_regular_deps) return
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) /= DEP_PATH) cycle
            call discover_path_dependency(trim(dependency_root), &
                config%deps(i)%path, &
                trim(alias)//'/'//trim(config%deps(i)%name), .true., &
                inventory, ierr, message, depth + 1)
            if (ierr /= 0) return
        end do
        ierr = 0
    end subroutine discover_path_dependency

    subroutine mark_unmodeled_config(config, alias, inventory)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: alias
        type(input_inventory_t), intent(inout) :: inventory
        integer :: i

        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) cycle
            call mark_incomplete(inventory, trim(alias)//' dependency '// &
                trim(config%deps(i)%name)//' is not a local path dependency')
        end do
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) == DEP_PATH) cycle
            call mark_incomplete(inventory, trim(alias)//' dev-dependency '// &
                trim(config%dev_deps(i)%name)//' is not a local path dependency')
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

    subroutine add_root(inventory, alias, physical_path, root_index, ierr, message)
        type(input_inventory_t), intent(inout) :: inventory
        character(len=*), intent(in) :: alias, physical_path
        integer, intent(out) :: root_index, ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: normalized
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
            call add_root_alias(inventory%roots(i), alias, ierr, message)
            if (ierr /= 0) return
            if (trim(alias) == 'project' .or. &
                    (trim(inventory%roots(i)%canonical_alias) /= 'project' .and. &
                    trim(alias) < trim(inventory%roots(i)%canonical_alias))) then
                inventory%roots(i)%canonical_alias = trim(alias)
                inventory%roots(i)%physical_path = trim(normalized)
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
        call add_root_alias(inventory%roots(root_index), alias, ierr, message)
        if (ierr /= 0) return
        ierr = 0
    end subroutine add_root

    subroutine add_root_alias(root, alias, ierr, message)
        type(input_root_t), intent(inout) :: root
        character(len=*), intent(in) :: alias
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: i

        ierr = 0
        message = ''
        do i = 1, root%alias_count
            if (trim(root%aliases(i)) == trim(alias)) return
        end do
        if (root%alias_count >= MAX_ALIASES) then
            ierr = 1
            message = 'input root has more than 16 aliases: '// &
                trim(root%canonical_alias)
            return
        end if
        root%alias_count = root%alias_count + 1
        root%aliases(root%alias_count) = trim(alias)
        call sort_aliases(root)
    end subroutine add_root_alias

    subroutine sort_aliases(root)
        type(input_root_t), intent(inout) :: root
        character(len=ALIAS_LEN) :: value
        integer :: i, j

        do i = 2, root%alias_count
            value = root%aliases(i)
            j = i - 1
            do while (j >= 1)
                if (llt(trim(value), trim(root%aliases(j)))) then
                    root%aliases(j + 1) = root%aliases(j)
                    j = j - 1
                else
                    exit
                end if
            end do
            root%aliases(j + 1) = value
        end do
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
        call cache_file_digest(trim(path), entry%content_digest)
        if (len_trim(entry%content_digest) /= HASH_LEN) then
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
            role, writable, ierr, message)
        type(input_inventory_t), intent(inout) :: inventory
        integer, intent(in) :: root_index
        character(len=*), intent(in) :: alias, physical_root, prefix, role
        logical, intent(in) :: writable
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: manifest, line, relative, target
        character(len=HASH_LEN) :: hash
        integer :: rc, unit, ios, kind, decode_status
        type(input_entry_t) :: entry
        character(kind=c_char, len=:), allocatable :: c_root, c_manifest

        ierr = 1
        message = ''
        call make_tmpfile('fo-input-inventory', manifest)
        c_root = trim(physical_root)//'/'//trim(prefix)//c_null_char
        c_manifest = trim(manifest)//c_null_char
        rc = c_list_tree(c_root, c_manifest)
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
            relative = trim(prefix)//'/'//trim(relative)
            if (ignored_input_path(trim(relative), kind == INPUT_DIRECTORY)) cycle
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
                call cache_file_digest(trim(physical_root)//'/'// &
                    trim(relative), hash)
                if (len_trim(hash) /= HASH_LEN) then
                    ierr = 1
                    message = 'cannot hash enumerated input: '//trim(relative)
                    exit
                end if
                entry%content_digest = hash
            case (INPUT_SYMLINK)
                entry%mode = 0
                entry%link_target = trim(target)
                hash = cache_digest([character(len=PATH_LEN) :: &
                    'symlink:'//trim(target)], 1)
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
        start = 25
        if (start + path_length + 2 * target_length > len_trim(line)) return
        relative = line(start:start + path_length - 1)
        do i = 1, target_length
            hi = hex_value(line(start + path_length + 2 * i - 1:start + &
                path_length + 2 * i - 1))
            lo = hex_value(line(start + path_length + 2 * i:start + &
                path_length + 2 * i))
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

    logical function ignored_input_path(path, is_directory)
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_directory
        integer :: start, stop
        character(len=128) :: component

        ignored_input_path = .false.
        start = 1
        do
            stop = index(path(start:), '/')
            if (stop == 0) then
                component = path(start:)
            else
                component = path(start:start + stop - 2)
            end if
            select case (trim(component))
            case ('.git', '.hg', '.svn', '.bzr', '.gremlin', '.fo', &
                    '.cache', 'build', 'cache', 'caches', 'session', &
                    'sessions', 'log', 'logs')
                if (stop /= 0 .or. is_directory) then
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
        if (declaration%expected_kind /= INPUT_FILE) then
            ierr = 1
            message = 'only regular file declarations are currently supported'
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
        character(len=PATH_LEN + ALIAS_LEN + 3 * 64 + HASH_LEN) :: part
        character(len=:), allocatable :: parts(:)
        integer :: i, j, capacity, root_index, retained

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
        capacity = max(1, 6 * inventory%entry_count)
        allocate(character(len=len(part)) :: parts(capacity))
        do i = 1, inventory%entry_count
            write (part, '(a,"|",a,"|",a,"|",i0,"|",i0,"|",l1,"|",a)') &
                trim(inventory%entries(i)%root_alias), &
                trim(inventory%entries(i)%relative_path), &
                trim(inventory%entries(i)%role), inventory%entries(i)%kind, &
                inventory%entries(i)%mode, &
                inventory%entries(i)%writable_at_execution, &
                trim(inventory%entries(i)%content_digest)
            parts(i) = trim(part)
        end do
        inventory%digest = cache_digest(parts(:inventory%entry_count), &
            inventory%entry_count)
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
