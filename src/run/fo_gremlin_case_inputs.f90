module fo_gremlin_case_inputs
    !! Per-generation case inputs for Gremlin (#189, #140): the changed
    !! files between two generations as model_affected sees them, and the
    !! digest of each case's input closure, which keys PASS receipt reuse.
    !! Both read the generation's frozen input inventory and the project
    !! model (fo_project_model): project files by path relative to the
    !! project root, every other input (dependency trees, CMake context,
    !! manifests and other global roles) as a widening/global input.
    use fo_cache, only: HASH_LEN
    use fo_fs, only: fs_realpath
    use fx_hash, only: sha256_string
    use fx_string, only: string_t
    use fo_input_inventory, only: input_inventory_t, input_entry_t
    use fo_gremlin_generation, only: generation_t
    use fo_project_model, only: name_index_t, index_insert, index_lookup, &
        model_case_digests, model_find_case
    use fo_project_load, only: project_model_units
    implicit none
    private

    character(len=*), parameter :: PROJECT_ALIAS = 'project'

    public :: generation_model_roots, generation_units_ensure
    public :: generation_case_digests_ensure, generation_case_digest
    public :: generation_inventory_changes

contains

    subroutine generation_model_roots(generation, project, roots)
        !! The spellings of the project root in the generation's model and
        !! units: its campaign project and the tree the model was loaded
        !! from (a reused model), each also resolved.
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: project
        type(string_t), allocatable, intent(out) :: roots(:)

        allocate (roots(0))
        call add(project)
        call add(trim(generation%model_root))

    contains

        subroutine add(path)
            character(len=*), intent(in) :: path
            character(len=4096) :: physical
            logical :: ok

            if (len_trim(path) == 0) return
            call push(trim(path))
            call fs_realpath(trim(path), physical, ok)
            if (ok) call push(trim(physical))
        end subroutine add

        subroutine push(path)
            character(len=*), intent(in) :: path
            type(string_t), allocatable :: grown(:)
            integer :: k

            do k = 1, size(roots)
                if (roots(k)%s == path) return
            end do
            allocate (grown(size(roots) + 1))
            grown(1:size(roots)) = roots
            grown(size(grown))%s = path
            call move_alloc(grown, roots)
        end subroutine push

    end subroutine generation_model_roots

    subroutine generation_units_ensure(generation, project)
        !! Scan this generation's sources of the model once (a reused model
        !! spells its sources below the tree it was loaded from).
        type(generation_t), intent(inout) :: generation
        character(len=*), intent(in) :: project
        type(string_t), allocatable :: extra(:)

        if (allocated(generation%units)) return
        if (.not. allocated(generation%model)) return
        if (allocated(generation%model_extra)) then
            extra = generation%model_extra
        else
            allocate (extra(0))
        end if
        call project_model_units(generation%model, extra, generation%units, &
            trim(generation%model_root), trim(project))
    end subroutine generation_units_ensure

    subroutine generation_case_digests_ensure(generation, project)
        !! Input closure digest of every model case (model_case_digests over
        !! the project files of the inventory) bound to the global inputs:
        !! the toolchain, the fo driver and every input outside the project
        !! files. Empty (no reuse) without a model, a complete inventory or
        !! a toolchain identity.
        type(generation_t), intent(inout) :: generation
        character(len=*), intent(in) :: project
        type(string_t), allocatable :: files(:), contents(:), names(:), roots(:)
        character(len=64), allocatable :: digests(:)
        character(len=:), allocatable :: global
        integer :: i, n, k
        logical :: complete

        if (allocated(generation%case_digests)) return
        if (.not. allocated(generation%model)) return
        allocate (generation%case_digests(generation%model%n_cases))
        generation%case_digests = ''
        complete = generation%input_inventory_ready .and. &
            generation%input_inventory_complete .and. &
            len_trim(generation%toolchain_key) == HASH_LEN
        if (.not. complete) return
        call generation_units_ensure(generation, project)
        call generation_model_roots(generation, project, roots)
        associate (inv => generation%input_inventory)
            allocate (files(inv%entry_count), contents(inv%entry_count))
            global = 'fo-case-global-v1|'//generation%toolchain_key//'|'// &
                generation%driver_digest
            n = 0
            do i = 1, inv%entry_count
                if (project_file(inv%entries(i))) then
                    n = n + 1
                    files(n)%s = trim(inv%entries(i)%relative_path)
                    contents(n)%s = entry_text(inv%entries(i))
                else
                    global = global//new_line('a')// &
                        trim(inv%entries(i)%root_alias)//':'// &
                        trim(inv%entries(i)%relative_path)//'|'// &
                        entry_text(inv%entries(i))
                end if
            end do
        end associate
        allocate (names(generation%model%n_cases))
        do k = 1, size(names)
            names(k)%s = generation%model%cases(k)%name
        end do
        call model_case_digests(generation%model, generation%units, roots, names, &
            files(1:n), contents(1:n), digests)
        global = sha256_string(global)
        do k = 1, size(names)
            if (len_trim(digests(k)) == 0) cycle
            generation%case_digests(k) = sha256_string(global//digests(k))
        end do
    end subroutine generation_case_digests_ensure

    function generation_case_digest(generation, name) result(digest)
        !! The input closure digest of a case; '' when unknown.
        type(generation_t), intent(in) :: generation
        character(len=*), intent(in) :: name
        character(len=HASH_LEN) :: digest
        integer :: k

        digest = ''
        if (.not. allocated(generation%model)) return
        if (.not. allocated(generation%case_digests)) return
        k = model_find_case(generation%model, trim(name))
        if (k == 0) return
        digest = generation%case_digests(k)
    end function generation_case_digest

    subroutine generation_inventory_changes(baseline, candidate, changed, reasons)
        !! Project files that differ between two inventories (content, kind,
        !! mode, link; added or removed), as paths relative to the project
        !! root. A differing input outside the project files and an
        !! incomplete inventory are widening reasons instead.
        type(input_inventory_t), intent(in) :: baseline, candidate
        type(string_t), allocatable, intent(out) :: changed(:)
        type(string_t), allocatable, intent(out) :: reasons(:)
        type(name_index_t) :: before
        logical, allocatable :: seen(:), new_differs(:), old_differs(:)
        integer :: a, b, existing

        allocate (reasons(0))
        if (.not. baseline%complete) call append(reasons, &
            'baseline inventory is incomplete')
        if (.not. candidate%complete) call append(reasons, &
            'candidate inventory is incomplete')
        do a = 1, baseline%entry_count
            call index_insert(before, entry_key(baseline%entries(a)), a, existing)
        end do
        allocate (seen(baseline%entry_count), old_differs(baseline%entry_count), &
            new_differs(candidate%entry_count))
        seen = .false.
        new_differs = .false.
        do b = 1, candidate%entry_count
            a = index_lookup(before, entry_key(candidate%entries(b)))
            if (a > 0) then
                seen(a) = .true.
                if (entry_text(baseline%entries(a)) == &
                    entry_text(candidate%entries(b))) cycle
            end if
            new_differs(b) = .true.
        end do
        old_differs = .not. seen
        allocate (changed(0))
        call collect(candidate, new_differs)
        call collect(baseline, old_differs)

    contains

        subroutine collect(inventory, mask)
            !! Changed project paths of the masked entries; other entries
            !! become widening reasons.
            type(input_inventory_t), intent(in) :: inventory
            logical, intent(in) :: mask(:)
            type(string_t), allocatable :: grown(:)
            integer :: k, n

            n = size(changed)
            allocate (grown(n + count(mask)))
            grown(1:n) = changed
            do k = 1, size(mask)
                if (.not. mask(k)) cycle
                associate (entry => inventory%entries(k))
                    if (project_file(entry)) then
                        n = n + 1
                        grown(n)%s = trim(entry%relative_path)
                    else
                        call append(reasons, 'shared or global input changed: '// &
                            trim(entry%root_alias)//':'//trim(entry%relative_path))
                    end if
                end associate
            end do
            changed = grown(1:n)
        end subroutine collect

    end subroutine generation_inventory_changes

    logical function project_file(entry)
        !! A file of the project tree that model_affected classifies; other
        !! roots and global roles (manifests, include trees, toolchain,
        !! environment, test support, locks) affect every case.
        type(input_entry_t), intent(in) :: entry
        character(len=:), allocatable :: role

        project_file = trim(entry%root_alias) == PROJECT_ALIAS
        if (.not. project_file) return
        role = trim(adjustl(entry%role))
        project_file = .not. (index(role, 'manifest') > 0 .or. &
            index(role, 'include') > 0 .or. index(role, 'harness') > 0 .or. &
            index(role, 'runtime') > 0 .or. index(role, 'toolchain') > 0 .or. &
            index(role, 'environment') > 0 .or. index(role, 'test-support') > 0 &
            .or. index(role, 'dependency-lock') > 0)
    end function project_file

    function entry_key(entry) result(key)
        type(input_entry_t), intent(in) :: entry
        character(len=:), allocatable :: key

        key = trim(entry%root_alias)//':'//trim(entry%relative_path)//'|'// &
            trim(entry%role)
    end function entry_key

    function entry_text(entry) result(text)
        type(input_entry_t), intent(in) :: entry
        character(len=:), allocatable :: text
        character(len=48) :: numbers

        write (numbers, '(i0,"|",i0,"|",l1)') entry%kind, entry%mode, &
            entry%writable_at_execution
        text = trim(entry%role)//'|'//trim(numbers)//'|'// &
            trim(entry%link_target)//'|'//trim(entry%content_digest)
    end function entry_text

    subroutine append(items, text)
        type(string_t), allocatable, intent(inout) :: items(:)
        character(len=*), intent(in) :: text
        type(string_t), allocatable :: grown(:)

        allocate (grown(size(items) + 1))
        grown(1:size(items)) = items
        grown(size(grown))%s = text
        call move_alloc(grown, items)
    end subroutine append

end module fo_gremlin_case_inputs
