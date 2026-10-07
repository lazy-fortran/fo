module fo_gremlin_execution_view
    !! Private per-invocation working directories over frozen Gremlin inputs.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_fs, only: fs_collect_files, fs_copy_exec, fs_make_dir, &
        fs_mkdir_excl, fs_remove_tree
    use fo_input_inventory, only: input_entry_t, input_inventory_t, INPUT_FILE, &
        INPUT_DIRECTORY
    use fo_process, only: process_getpid
    use fo_util, only: make_tmpfile, delete_tmpfile
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096

    type, public :: execution_view_t
        character(len=PATH_LEN) :: root = ''
        character(len=PATH_LEN) :: cwd = ''
        character(len=PATH_LEN) :: tmpdir = ''
        character(len=PATH_LEN) :: owner_key = ''
        character(len=128) :: case_id = ''
        character(len=64) :: generation_id = ''
        character(len=64) :: inventory_digest = ''
        logical :: active = .false.
        logical :: complete = .false.
        logical :: retain_on_failure = .true.
        character(len=1024) :: diagnostic = ''
    end type execution_view_t

    public :: execution_view_create, execution_view_release
    public :: execution_view_copy_app_outputs

    interface
        integer(c_int) function generation_copy_tree_ephemeral( &
                source, destination, manifest) &
                bind(C, name='fo_c_generation_copy_tree_ephemeral')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: source(*), destination(*), manifest(*)
        end function generation_copy_tree_ephemeral

        integer(c_int) function copy_declared_file(source, destination, writable) &
                bind(C, name='fo_c_generation_copy_declared_file')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: source(*), destination(*)
            integer(c_int), value :: writable
        end function copy_declared_file
    end interface

contains

    subroutine execution_view_copy_app_outputs(build_project_root, &
            execution_project_root, app_output_dir, legacy_bin_dir, ierr, message)
        character(len=*), intent(in) :: build_project_root, execution_project_root
        character(len=*), intent(in) :: app_output_dir, legacy_bin_dir
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: app_files(256), source, destination
        character(len=PATH_LEN) :: relative, legacy_source, legacy_dest
        integer :: n_files, i, root_len, source_len, last_slash, rc
        logical :: exists

        ierr = 0
        message = ''
        if (len_trim(build_project_root) == 0 .or. &
                len_trim(execution_project_root) == 0 .or. &
                len_trim(app_output_dir) == 0 .or. len_trim(legacy_bin_dir) == 0) then
            ierr = 1
            message = 'application output roots must be non-empty'
            return
        end if
        if (trim(build_project_root) == trim(execution_project_root)) return

        call fs_collect_files(trim(app_output_dir), '', '', '', app_files, &
            n_files, .false.)
        if (n_files >= size(app_files)) then
            ierr = 1
            message = 'application output inventory exceeds 256 files'
            return
        end if
        root_len = len_trim(build_project_root)
        do i = 1, n_files
            source = trim(app_files(i))
            source_len = len_trim(source)
            if (source_len <= root_len + 1) cycle
            if (source(:root_len) /= trim(build_project_root)) cycle
            if (source(root_len + 1:root_len + 1) /= '/') cycle
            relative = source(root_len + 2:source_len)
            if (index(trim(relative), 'build/') /= 1) cycle
            last_slash = index(trim(source), '/', back=.true.)
            if (last_slash == 0) cycle

            destination = trim(execution_project_root)//'/'//trim(relative)
            call make_parent_directory(trim(destination))
            rc = fs_copy_exec(trim(source), trim(destination))
            if (rc /= 0) then
                ierr = 1
                message = 'cannot copy application output into execution view: '// &
                    trim(relative)
                return
            end if

            legacy_source = trim(legacy_bin_dir)//'/'// &
                source(last_slash + 1:source_len)
            inquire (file=trim(legacy_source), exist=exists)
            if (.not. exists) cycle
            legacy_dest = trim(execution_project_root)//'/'// &
                trim(legacy_source(root_len + 2:len_trim(legacy_source)))
            call make_parent_directory(trim(legacy_dest))
            rc = fs_copy_exec(trim(legacy_source), trim(legacy_dest))
            if (rc /= 0) then
                ierr = 1
                message = 'cannot copy legacy application output into execution view: '// &
                    trim(legacy_dest)
                return
            end if
        end do
    end subroutine execution_view_copy_app_outputs

    subroutine execution_view_create(scratch_parent, generation_id, owner_key, &
            case_id, inventory, inventory_ready, inventory_complete, view, ierr, &
            message, candidate_bundle_root)
        character(len=*), intent(in) :: scratch_parent, generation_id
        character(len=*), intent(in) :: owner_key, case_id
        type(input_inventory_t), intent(in) :: inventory
        logical, intent(in) :: inventory_ready, inventory_complete
        type(execution_view_t), intent(out) :: view
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=*), optional, intent(in) :: candidate_bundle_root

        character(len=PATH_LEN) :: source, destination, alias_root, bundle_path
        character(len=PATH_LEN) :: candidate, copy_manifest
        integer :: root_index, i, j, attempt, copy_rc, clock_count
        integer(c_int) :: writable, tree_rc
        logical :: already_materialized

        view = execution_view_t()
        ierr = 1
        message = ''
        view%case_id = case_id
        view%generation_id = generation_id
        view%owner_key = owner_key
        view%inventory_digest = inventory%digest
        view%complete = inventory_ready .and. inventory_complete .and. &
            inventory%complete
        if (len_trim(scratch_parent) == 0 .or. &
                len_trim(scratch_parent) >= PATH_LEN) then
            message = 'execution view scratch parent is empty or too long'
            return
        end if
        call fs_make_dir(trim(scratch_parent))
        if (len_trim(scratch_parent) + 48 >= PATH_LEN) then
            message = 'execution view path exceeds the supported length'
            return
        end if
        call system_clock(clock_count)
        attempt = 0
        do
            attempt = attempt + 1
            write(candidate, '(a,"/execution-",i0,"-",i0,"-",i0)') &
                trim(scratch_parent), process_getpid(), clock_count, attempt
            if (len_trim(candidate) >= PATH_LEN) then
                message = 'execution view path exceeds the supported length'
                return
            end if
            if (fs_mkdir_excl(trim(candidate)) == 0) exit
            if (attempt >= 32) then
                message = 'cannot reserve a unique owned execution view'
                return
            end if
        end do
        view%root = trim(candidate)
        view%cwd = trim(candidate)
        view%active = .true.
        if (.not. inventory_ready) then
            call reject_view(view, 'canonical input inventory unavailable', &
                ierr, message)
            return
        end if
        if (.not. inventory%valid) then
            if (len_trim(inventory%diagnostic) > 0) then
                call reject_view(view, trim(inventory%diagnostic), ierr, message)
            else
                call reject_view(view, &
                    'canonical input inventory has invalid declarations', &
                    ierr, message)
            end if
            return
        end if
        if (.not. inventory_complete .or. .not. inventory%complete) then
            view%complete = .false.
            view%diagnostic = trim(inventory%diagnostic)
            if (len_trim(view%diagnostic) == 0) &
                view%diagnostic = &
                    'canonical input inventory has unmodeled ambient inputs'
        end if

        if (present(candidate_bundle_root)) then
            if (len_trim(candidate_bundle_root) == 0 .or. &
                    len_trim(candidate_bundle_root) >= PATH_LEN) then
                call reject_view(view, &
                    'candidate bundle root is empty or too long', ierr, message)
                return
            end if
            if (len_trim(view%root) + len('/project') >= PATH_LEN) then
                call reject_view(view, &
                    'candidate build project path exceeds the supported length', &
                    ierr, message)
                return
            end if
            call make_tmpfile('fo-build-view-manifest', copy_manifest)
            tree_rc = generation_copy_tree_ephemeral( &
                trim(candidate_bundle_root)//c_null_char, &
                trim(view%root)//c_null_char, trim(copy_manifest)//c_null_char)
            call delete_tmpfile(copy_manifest)
            if (tree_rc /= 0) then
                call reject_view(view, &
                    'cannot copy frozen generation into candidate build view', &
                    ierr, message)
                return
            end if
            view%cwd = trim(view%root)//'/project'
            inquire (file=trim(view%cwd), exist=already_materialized)
            if (.not. already_materialized) then
                call reject_view(view, &
                    'candidate bundle does not contain a project root', ierr, message)
                return
            end if
            call create_private_scratch(view, ierr, message)
            if (ierr /= 0) return
            ierr = 0
            message = trim(view%diagnostic)
            return
        end if

        do i = 1, inventory%entry_count
            if (.not. execution_input_role(inventory%entries(i)%role)) cycle
            if (inventory%entries(i)%kind == INPUT_DIRECTORY) then
                root_index = find_root(inventory, inventory%entries(i)%root_alias)
                if (root_index == 0) then
                    call reject_view(view, &
                        'declared runtime fixture has an unknown logical root', &
                        ierr, message)
                    return
                end if
                if (.not. safe_relative_path( &
                        inventory%entries(i)%relative_path)) then
                    call reject_view(view, &
                        'declared runtime fixture path escapes its logical root', &
                        ierr, message)
                    return
                end if
                alias_root = ''
                if (trim(inventory%entries(i)%root_alias) /= 'project') &
                    alias_root = '.fo-inputs/'// &
                        trim(inventory%entries(i)%root_alias)
                destination = trim(view%root)
                if (len_trim(alias_root) > 0) &
                    destination = trim(destination)//'/'//trim(alias_root)
                destination = trim(destination)//'/'// &
                    trim(inventory%entries(i)%relative_path)
                if (len_trim(destination) >= PATH_LEN) then
                    call reject_view(view, &
                        'declared runtime fixture path exceeds the supported length', &
                        ierr, message)
                    return
                end if
                call fs_make_dir(trim(destination))
                inquire(file=trim(destination), exist=already_materialized)
                if (.not. already_materialized) then
                    call reject_view(view, &
                        'cannot materialize declared runtime fixture directory: '// &
                        trim(inventory%entries(i)%root_alias)//':'// &
                        trim(inventory%entries(i)%relative_path), ierr, message)
                    return
                end if
                cycle
            end if
            if (inventory%entries(i)%kind /= INPUT_FILE) then
                call reject_view(view, &
                    'declared runtime fixture is not a regular file or directory', &
                    ierr, message)
                return
            end if
            root_index = find_root(inventory, inventory%entries(i)%root_alias)
            if (root_index == 0) then
                call reject_view(view, &
                    'declared runtime fixture has an unknown logical root', &
                    ierr, message)
                return
            end if
            if (.not. safe_relative_path(inventory%entries(i)%relative_path)) then
                call reject_view(view, &
                    'declared runtime fixture path escapes its logical root', &
                    ierr, message)
                return
            end if
            already_materialized = .false.
            do j = 1, i - 1
                if (.not. execution_input_role(inventory%entries(j)%role)) cycle
                if (trim(inventory%entries(j)%root_alias) /= &
                        trim(inventory%entries(i)%root_alias)) cycle
                if (trim(inventory%entries(j)%relative_path) /= &
                        trim(inventory%entries(i)%relative_path)) cycle
                if ((inventory%entries(j)%writable_at_execution .neqv. &
                        inventory%entries(i)%writable_at_execution) .or. &
                        inventory%entries(j)%kind /= inventory%entries(i)%kind .or. &
                        inventory%entries(j)%mode /= inventory%entries(i)%mode .or. &
                        inventory%entries(j)%content_digest /= &
                        inventory%entries(i)%content_digest) then
                    call reject_view(view, &
                        'conflicting declarations for runtime fixture: '// &
                        trim(inventory%entries(i)%relative_path), ierr, message)
                    return
                end if
                already_materialized = .true.
                exit
            end do
            if (already_materialized) cycle
            bundle_path = ''
            do j = 1, inventory%roots(root_index)%alias_count
                if (trim(inventory%roots(root_index)%aliases(j)) /= &
                        trim(inventory%entries(i)%root_alias)) cycle
                bundle_path = inventory%roots(root_index)%bundle_paths(j)
                exit
            end do
            if (.not. safe_relative_path(bundle_path) .or. &
                    inventory%roots(root_index)%physical_path(1:1) /= '/') then
                call reject_view(view, &
                    'declared runtime fixture has no safe generation bundle path', &
                    ierr, message)
                return
            end if
            if (trim(inventory%entries(i)%root_alias) == 'project') then
                alias_root = ''
            else
                ! Keep distinct logical roots distinct in the view. Callers
                ! needing an absolute/outside-root runtime path remain unsupported.
                alias_root = '.fo-inputs/'//trim(inventory%entries(i)%root_alias)
            end if
            source = trim(inventory%roots(root_index)%physical_path)//'/'// &
                trim(bundle_path)//'/'// &
                trim(inventory%entries(i)%relative_path)
            destination = trim(view%root)
            if (len_trim(alias_root) > 0) &
                destination = trim(destination)//'/'//trim(alias_root)
            destination = trim(destination)//'/'// &
                trim(inventory%entries(i)%relative_path)
            if (len_trim(source) >= PATH_LEN .or. &
                    len_trim(destination) >= PATH_LEN) then
                call reject_view(view, &
                    'declared runtime fixture path exceeds the supported length', &
                    ierr, message)
                return
            end if
            call make_parent_directory(trim(destination))
            writable = 0_c_int
            if (inventory%entries(i)%writable_at_execution) writable = 1_c_int
            copy_rc = copy_declared_file(trim(source)//c_null_char, &
                trim(destination)//c_null_char, writable)
            if (copy_rc /= 0) then
                call reject_view(view, &
                    'cannot materialize declared runtime fixture: '// &
                    trim(inventory%entries(i)%root_alias)//':'// &
                    trim(inventory%entries(i)%relative_path), ierr, message)
                return
            end if
        end do
        call create_private_scratch(view, ierr, message)
        if (ierr /= 0) return
        ierr = 0
        message = trim(view%diagnostic)
    end subroutine execution_view_create

    subroutine create_private_scratch(view, ierr, message)
        type(execution_view_t), intent(inout) :: view
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        ierr = 0
        message = ''
        if (len_trim(view%root) + len('/.fo-tmp') >= PATH_LEN) then
            call reject_view(view, 'execution view scratch path is too long', &
                ierr, message)
            return
        end if
        view%tmpdir = trim(view%root)//'/.fo-tmp'
        if (fs_mkdir_excl(trim(view%tmpdir)) /= 0) &
            call reject_view(view, 'cannot create private execution scratch', &
                ierr, message)
    end subroutine create_private_scratch

    subroutine reject_view(view, diagnostic, ierr, message)
        type(execution_view_t), intent(inout) :: view
        character(len=*), intent(in) :: diagnostic
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: cleanup_status
        character(len=PATH_LEN) :: cleanup_message

        view%diagnostic = trim(diagnostic)
        call execution_view_release(view, .false., cleanup_status, cleanup_message)
        ierr = 1
        message = trim(diagnostic)
        if (cleanup_status /= 0) message = trim(message)//'; '// &
            trim(cleanup_message)
    end subroutine reject_view

    subroutine execution_view_release(view, retain, ierr, message)
        type(execution_view_t), intent(inout) :: view
        logical, intent(in) :: retain
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        ierr = 0
        message = ''
        if (.not. view%active .or. len_trim(view%root) == 0) return
        if (retain) then
            if (len_trim(view%tmpdir) > 0) then
                call fs_remove_tree(trim(view%tmpdir), ierr)
                if (ierr /= 0) then
                    message = 'cannot remove retained view scratch: '// &
                        trim(view%tmpdir)
                    return
                end if
            end if
            view%retain_on_failure = .true.
            view%active = .false.
            return
        end if
        call fs_remove_tree(trim(view%root), ierr)
        if (ierr /= 0) then
            message = 'cannot remove owned execution view: '//trim(view%root)
            return
        end if
        view%active = .false.
    end subroutine execution_view_release

    logical function execution_input_role(role)
        character(len=*), intent(in) :: role

        execution_input_role = trim(role) == 'runtime-fixture' .or. &
            trim(role) == 'test-fixture'
    end function execution_input_role

    integer function find_root(inventory, alias) result(index_root)
        type(input_inventory_t), intent(in) :: inventory
        character(len=*), intent(in) :: alias
        integer :: i

        index_root = 0
        do i = 1, inventory%root_count
            if (trim(inventory%roots(i)%canonical_alias) == trim(alias)) then
                index_root = i
                return
            end if
            if (any(inventory%roots(i)%aliases(: &
                    inventory%roots(i)%alias_count) == alias)) then
                index_root = i
                return
            end if
        end do
    end function find_root

    logical function safe_relative_path(path) result(safe)
        character(len=*), intent(in) :: path
        integer :: i, start, n

        safe = .false.
        n = len_trim(path)
        if (n == 0) return
        if (path(1:1) == '/') return
        start = 1
        do i = 1, n + 1
            if (i <= n) then
                if (path(i:i) /= '/') cycle
            end if
            if (i == start) return
            if (path(start:i - 1) == '.' .or. path(start:i - 1) == '..') return
            start = i + 1
        end do
        safe = .true.
    end function safe_relative_path

    subroutine make_parent_directory(path)
        character(len=*), intent(in) :: path
        integer :: slash

        slash = index(trim(path), '/', back=.true.)
        if (slash <= 1) return
        call fs_make_dir(path(:slash - 1))
    end subroutine make_parent_directory

end module fo_gremlin_execution_view
