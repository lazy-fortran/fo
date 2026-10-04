module fo_gremlin_execution_view
    !! Private per-invocation working directories over frozen Gremlin inputs.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_fs, only: fs_make_dir, fs_mkdir_excl, fs_remove_tree
    use fo_input_inventory, only: input_entry_t, input_inventory_t, INPUT_FILE
    use fo_process, only: process_getpid
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096

    type, public :: execution_view_t
        character(len=PATH_LEN) :: root = ''
        character(len=PATH_LEN) :: cwd = ''
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

    interface
        integer(c_int) function copy_declared_file(source, destination, writable) &
                bind(C, name='fo_c_generation_copy_declared_file')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: source(*), destination(*)
            integer(c_int), value :: writable
        end function copy_declared_file
    end interface

contains

    subroutine execution_view_create(scratch_parent, generation_id, owner_key, &
            case_id, inventory, inventory_ready, inventory_complete, view, ierr, &
            message)
        character(len=*), intent(in) :: scratch_parent, generation_id
        character(len=*), intent(in) :: owner_key, case_id
        type(input_inventory_t), intent(in) :: inventory
        logical, intent(in) :: inventory_ready, inventory_complete
        type(execution_view_t), intent(out) :: view
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: source, destination, alias_root
        character(len=PATH_LEN) :: candidate
        integer :: root_index, i, attempt, copy_rc, clock_count
        integer(c_int) :: writable

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

        do i = 1, inventory%entry_count
            if (.not. execution_input_role(inventory%entries(i)%role)) cycle
            if (inventory%entries(i)%kind /= INPUT_FILE) then
                call reject_view(view, &
                    'declared runtime fixture is not a regular file', ierr, message)
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
            if (trim(inventory%entries(i)%root_alias) == 'project') then
                alias_root = ''
            else
                ! Keep distinct logical roots distinct in the view. Callers
                ! needing an absolute/outside-root runtime path remain unsupported.
                alias_root = '.fo-inputs/'//trim(inventory%entries(i)%root_alias)
            end if
            source = trim(inventory%roots(root_index)%physical_path)//'/'// &
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
        ierr = 0
        message = trim(view%diagnostic)
    end subroutine execution_view_create

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
            view%retain_on_failure = .true.
            view%active = .false.
            return
        end if
        call fs_remove_tree(trim(view%root))
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
