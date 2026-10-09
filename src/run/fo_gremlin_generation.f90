module fo_gremlin_generation
    !! Immutable, content-addressed input snapshots for Gremlin campaigns.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_cache, only: HASH_LEN, cache_digest, cache_store_root
    use fo_fs, only: fs_make_dir, fs_mkdir_excl
    use fo_process, only: process_getpid
    use fx_immutable_store, only: immutable_store_t, immutable_store_init, &
        IMMUTABLE_OK
    use fo_input_inventory, only: input_inventory_t, input_inventory_revalidate, &
        input_inventory_rediscover, input_entry_t, INPUT_FILE, INPUT_DIRECTORY
    use fo_generation_manifest, only: generation_manifest_metadata_t, &
        generation_manifest_capture, generation_manifest_load, &
        generation_manifest_materialize, generation_manifest_execution_identity, &
        generation_manifest_release
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096
    integer, parameter :: PART_LEN = 4096

    type, public :: generation_context_t
        character(len=:), allocatable :: toolchain
        character(len=:), allocatable :: flags
        character(len=:), allocatable :: environment
        character(len=:), allocatable :: base_commit
        character(len=:), allocatable :: patch_digest
        character(len=PATH_LEN) :: driver_path = ''
        character(len=HASH_LEN) :: driver_digest = ''
        integer(int64) :: driver_size = 0_int64
        type(input_inventory_t) :: input_inventory
    end type generation_context_t

    type, public :: generation_t
        character(len=HASH_LEN) :: identity = ''
        character(len=HASH_LEN) :: manifest_id = ''
        character(len=PATH_LEN) :: store_root = ''
        character(len=PATH_LEN) :: driver_path = ''
        character(len=HASH_LEN) :: driver_digest = ''
        integer(int64) :: driver_size = 0_int64
        character(len=PATH_LEN) :: root = ''
        character(len=PATH_LEN) :: project_root = ''
        ! Writable materialization locator; deliberately excluded from identity.
        character(len=PATH_LEN) :: build_project_root = ''
        character(len=:), allocatable :: base_commit
        character(len=:), allocatable :: patch_digest
        type(input_inventory_t) :: input_inventory
        logical :: input_inventory_ready = .false.
        logical :: input_inventory_complete = .false.
        character(len=1024) :: input_inventory_diagnostic = ''
    end type generation_t

    public :: generation_capture, generation_driver_identity, &
        generation_load_inventory

    interface
        integer(c_int) function fo_c_generation_validate_root(root) &
                bind(C, name='fo_c_generation_validate_root')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
        end function fo_c_generation_validate_root

        integer(c_int) function fo_c_generation_freeze_tree(root) &
                bind(C, name='fo_c_generation_freeze_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
        end function fo_c_generation_freeze_tree

        integer(c_int) function fo_c_generation_lock(path) &
                bind(C, name='fo_c_generation_lock')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_generation_lock

        subroutine fo_c_generation_unlock(fd) &
                bind(C, name='fo_c_generation_unlock')
            import :: c_int
            integer(c_int), value :: fd
        end subroutine fo_c_generation_unlock

        integer(c_int) function fo_c_generation_publish(stage, target) &
                bind(C, name='fo_c_generation_publish')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: stage(*), target(*)
        end function fo_c_generation_publish

        integer(c_int) function fo_c_generation_remove_stage(path) &
                bind(C, name='fo_c_generation_remove_stage')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_generation_remove_stage

        integer(c_int) function fo_c_generation_remove_empty_stage(path) &
                bind(C, name='fo_c_generation_remove_empty_stage')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_generation_remove_empty_stage
    end interface

contains

    subroutine generation_capture(project_root, cas_root, context, generation, &
            ierr, message)
        character(len=*), intent(in) :: project_root, cas_root
        type(generation_context_t), intent(in) :: context
        type(generation_t), intent(out) :: generation
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        ierr = 1
        message = ''
        generation = generation_t()
        if (len_trim(project_root) == 0 .or. len_trim(cas_root) == 0) then
            message = 'project_root and cas_root are required'
            return
        end if
        if (.not. valid_driver_digest(context%driver_digest) .or. &
            context%driver_size <= 0_int64) then
            message = 'generation requires a pinned fo driver digest and size'
            return
        end if
        if (len_trim(project_root) >= PATH_LEN .or. &
            len_trim(cas_root) + len('/gremlin/generations-v2') + 1 + HASH_LEN + &
            len('/bundle/project') >= PATH_LEN) then
            message = 'generation root path exceeds the supported length'
            return
        end if
        if (index(project_root, achar(0)) /= 0 .or. &
            index(cas_root, achar(0)) /= 0) then
            message = 'generation paths cannot contain a null byte'
            return
        end if
        if (fo_c_generation_validate_root(trim(project_root)//c_null_char) /= 0) then
            message = 'generation project root must be a directory, not a symlink'
            return
        end if
        if (.not. identity_field_fits(context%toolchain) .or. &
            .not. identity_field_fits(context%flags) .or. &
            .not. identity_field_fits(context%environment) .or. &
            .not. identity_field_fits(context%base_commit) .or. &
            .not. identity_field_fits(context%patch_digest)) then
            message = 'generation identity field exceeds the supported length'
            return
        end if
        if (.not. context%input_inventory%valid) then
            message = 'generation requires a valid declared input inventory'
            return
        end if
        if (context%input_inventory%root_count <= 0 .or. &
            .not. allocated(context%input_inventory%roots) .or. &
            .not. allocated(context%input_inventory%entries)) then
            message = 'generation requires declared project inputs'
            return
        end if
        call generation_capture_inventory(project_root, cas_root, context, &
            generation, ierr, message)
    end subroutine generation_capture

    subroutine generation_capture_inventory(project_root, cas_root, context, &
            generation, ierr, message)
        character(len=*), intent(in) :: project_root, cas_root
        type(generation_context_t), intent(in) :: context
        type(generation_t), intent(out) :: generation
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(generation_manifest_metadata_t) :: metadata
        type(input_inventory_t) :: restored_inventory
        type(immutable_store_t) :: store
        character(len=PATH_LEN) :: store_root, base, cache, capture_dir, stage
        character(len=PATH_LEN) :: lock_path, manifest_path
        character(len=PATH_LEN) :: owner_path(1)
        character(len=HASH_LEN) :: manifest_id, root_owner
        character(len=32) :: clock_text
        integer :: status, attempt, clock_count, fd
        integer(c_int) :: crc
        logical :: exists

        generation = generation_t()
        ierr = 1
        message = ''
        metadata%toolchain = value_or_empty(context%toolchain)
        metadata%flags = value_or_empty(context%flags)
        metadata%environment = value_or_empty(context%environment)
        metadata%driver_digest = trim(context%driver_digest)
        metadata%driver_size = context%driver_size
        metadata%base_commit = value_or_empty(context%base_commit)
        metadata%patch_digest = value_or_empty(context%patch_digest)
        metadata%execution_identity = generation_manifest_execution_identity( &
            metadata, context%input_inventory)
        call cache_store_root(store_root)
        if (len_trim(store_root) == 0 .or. len_trim(store_root) >= PATH_LEN) then
            message = 'cannot resolve the shared immutable object store'
            return
        end if
        base = trim(cas_root)//'/gremlin/generations-v2'
        cache = trim(base)//'/'//metadata%execution_identity
        if (len_trim(base) + len('/.capture/manifest-') + 64 >= PATH_LEN .or. &
            len_trim(cache) + len('/bundle/project') >= PATH_LEN .or. &
            len_trim(base) + len('/.locks/') + HASH_LEN >= PATH_LEN) then
            message = 'manifest generation path exceeds the supported length'
            return
        end if
        generation%identity = metadata%execution_identity
        generation%root = cache
        owner_path(1) = trim(cache)
        root_owner = cache_digest(owner_path, 1)
        generation%project_root = trim(cache)//'/bundle/project'
        generation%driver_path = context%driver_path
        generation%driver_digest = context%driver_digest
        generation%driver_size = context%driver_size
        lock_path = trim(base)//'/.locks/'//generation%identity
        call fs_make_dir(trim(base))
        call fs_make_dir(trim(base)//'/.capture')
        call fs_make_dir(trim(base)//'/.locks')
        fd = int(fo_c_generation_lock(trim(lock_path)//c_null_char))
        if (fd < 0) then
            message = 'cannot acquire manifest generation publication lock'
            return
        end if

        inquire (file=trim(cache), exist=exists)
        if (exists) then
            call read_manifest_store(trim(cache)//'/store.root', store_root, &
                status, message)
            if (status /= 0) then
                call fo_c_generation_unlock(int(fd, c_int))
                return
            end if
            manifest_path = trim(cache)//'/manifest.id'
            call read_manifest_id(trim(manifest_path), manifest_id, status, message)
            if (status == 0) then
                call generation_manifest_load(trim(store_root), manifest_id, &
                    metadata, restored_inventory, status, message)
            end if
            if (status == 0 .and. metadata%execution_identity /= &
                generation%identity) then
                status = 1
                message = 'stored manifest identity differs from its generation path'
            end if
            if (status == 0) then
                call validate_manifest_bundle(generation%project_root, &
                    restored_inventory, status, message)
            end if
            if (status == 0) then
                call input_inventory_revalidate(project_root, &
                    context%input_inventory%declarations, &
                    context%input_inventory, status, message)
            end if
            if (status == 0) then
                call bind_inventory_to_bundle(restored_inventory, trim(cache), &
                    status, message)
            end if
            if (status == 0) then
                generation%manifest_id = manifest_id
                generation%store_root = store_root
                generation%input_inventory = restored_inventory
                generation%input_inventory_ready = .true.
                generation%input_inventory_complete = &
                    restored_inventory%complete
                generation%input_inventory_diagnostic = &
                    restored_inventory%diagnostic
                generation%base_commit = metadata%base_commit
                generation%patch_digest = metadata%patch_digest
                ierr = 0
            end if
            call fo_c_generation_unlock(int(fd, c_int))
            return
        end if

        call immutable_store_init(store, trim(store_root), status)
        if (status /= IMMUTABLE_OK) then
            message = 'cannot initialize the shared immutable object store'
            call fo_c_generation_unlock(int(fd, c_int))
            return
        end if
        if (len_trim(store%root_dir) == 0 .or. &
            len_trim(store%root_dir) >= PATH_LEN) then
            message = 'canonical immutable object store path is invalid'
            call fo_c_generation_unlock(int(fd, c_int))
            return
        end if
        store_root = trim(store%root_dir)

        call generation_manifest_capture(trim(store_root), root_owner, metadata, &
            context%input_inventory, manifest_id, status, message)
        if (status /= 0) then
            call fo_c_generation_unlock(int(fd, c_int))
            return
        end if
        call system_clock(clock_count)
        do attempt = 1, 32
            write (clock_text, '(i0)') clock_count + attempt
            capture_dir = trim(base)//'/.capture/manifest-'// &
                trim(int_text(process_getpid()))//'-'//trim(clock_text)
            status = fs_mkdir_excl(trim(capture_dir))
            if (status == 0) exit
        end do
        if (attempt > 32) then
            message = 'cannot allocate manifest generation staging directory'
            call release_failed_manifest(store_root, generation%identity, &
                root_owner, message)
            call fo_c_generation_unlock(int(fd, c_int))
            return
        end if
        stage = trim(capture_dir)//'/generation'
        status = fs_mkdir_excl(trim(stage))
        if (status /= 0) then
            message = 'cannot create manifest generation stage'
            crc = fo_c_generation_remove_stage(trim(capture_dir)//c_null_char)
            if (crc == 0) call release_failed_manifest(store_root, &
                generation%identity, root_owner, message)
            call fo_c_generation_unlock(int(fd, c_int))
            return
        end if
        call fs_make_dir(trim(stage)//'/bundle')
        call generation_manifest_materialize(trim(store_root), manifest_id, &
            trim(stage)//'/bundle', metadata, restored_inventory, status, message)
        if (status == 0) then
            call write_manifest_id(trim(stage)//'/manifest.id', manifest_id, status)
            if (status /= 0) message = 'cannot write generation manifest locator'
        end if
        if (status == 0) then
            call write_manifest_store(trim(stage)//'/store.root', store_root, status)
            if (status /= 0) message = 'cannot write generation store locator'
        end if
        if (status == 0) then
            call write_manifest_id(trim(stage)//'/root.owner', root_owner, status)
            if (status /= 0) message = 'cannot write generation root owner locator'
        end if
        if (status == 0) then
            call write_manifest_identity(trim(stage)//'/identity.txt', metadata, &
                status)
            if (status /= 0) message = 'cannot write generation identity record'
        end if
        if (status == 0) then
            ! Keep the staging directory writable until its atomic rename.
            ! The publisher freezes that directory after publication.
            crc = fo_c_generation_freeze_tree( &
                trim(stage)//'/bundle'//c_null_char)
            if (crc /= 0) status = int(crc)
            if (status == 0) then
                crc = fo_c_generation_freeze_tree( &
                    trim(stage)//'/manifest.id'//c_null_char)
                if (crc /= 0) status = int(crc)
            end if
            if (status == 0) then
                crc = fo_c_generation_freeze_tree( &
                    trim(stage)//'/store.root'//c_null_char)
                if (crc /= 0) status = int(crc)
            end if
            if (status == 0) then
                crc = fo_c_generation_freeze_tree( &
                    trim(stage)//'/root.owner'//c_null_char)
                if (crc /= 0) status = int(crc)
            end if
            if (status == 0) then
                crc = fo_c_generation_freeze_tree( &
                    trim(stage)//'/identity.txt'//c_null_char)
                if (crc /= 0) status = int(crc)
            end if
            if (status /= 0) message = 'cannot freeze manifest generation content'
        end if
        if (status == 0) then
            crc = fo_c_generation_publish(trim(stage)//c_null_char, &
                trim(cache)//c_null_char)
            if (crc /= 0) status = int(crc)
            if (status /= 0) message = &
                'cannot publish manifest generation stage ('// &
                trim(int_text(status))//')'
        end if
        if (status /= 0) then
            if (len_trim(message) == 0) &
                message = 'cannot publish immutable manifest generation'
            crc = fo_c_generation_remove_stage(trim(capture_dir)//c_null_char)
            inquire (file=trim(cache), exist=exists)
            if (crc == 0 .and. .not. exists) call release_failed_manifest( &
                store_root, generation%identity, root_owner, message)
            call fo_c_generation_unlock(int(fd, c_int))
            return
        end if
        crc = fo_c_generation_remove_empty_stage(trim(capture_dir)//c_null_char)
        if (crc /= 0) then
            message = 'cannot remove published manifest generation staging directory'
            call fo_c_generation_unlock(int(fd, c_int))
            return
        end if
        call fo_c_generation_unlock(int(fd, c_int))
        generation%manifest_id = manifest_id
        generation%store_root = store_root
        generation%input_inventory = context%input_inventory
        call bind_inventory_to_bundle(generation%input_inventory, trim(cache), &
            status, message)
        if (status /= 0) then
            ierr = 1
            return
        end if
        generation%input_inventory_ready = .true.
        generation%input_inventory_complete = context%input_inventory%complete
        generation%input_inventory_diagnostic = context%input_inventory%diagnostic
        generation%base_commit = value_or_empty(context%base_commit)
        generation%patch_digest = value_or_empty(context%patch_digest)
        ierr = 0
    end subroutine generation_capture_inventory

    subroutine release_failed_manifest(store_root, generation_id, root_owner, message)
        character(len=*), intent(in) :: store_root, generation_id, root_owner
        character(len=*), intent(inout) :: message
        integer :: status

        call generation_manifest_release(trim(store_root), trim(generation_id), &
            trim(root_owner), status)
        if (status /= IMMUTABLE_OK) message = trim(message)// &
            '; failed generation root could not be released'
    end subroutine release_failed_manifest

    subroutine generation_load_inventory(generation, ierr, message)
        type(generation_t), intent(inout) :: generation
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(generation_manifest_metadata_t) :: metadata
        type(input_inventory_t) :: inventory
        character(len=PATH_LEN) :: store_root, manifest_path
        character(len=HASH_LEN) :: manifest_id
        integer :: status

        ierr = 1
        message = ''
        if (len_trim(generation%root) == 0 .or. &
            len_trim(generation%identity) /= HASH_LEN) then
            message = 'generation root or execution identity is unavailable'
            return
        end if
        call read_manifest_store(trim(generation%root)//'/store.root', &
            store_root, status, message)
        if (status /= 0) return
        generation%store_root = store_root
        manifest_path = trim(generation%root)//'/manifest.id'
        call read_manifest_id(trim(manifest_path), manifest_id, status, message)
        if (status /= 0) return
        call generation_manifest_load(trim(store_root), manifest_id, metadata, &
            inventory, status, message)
        if (status /= 0) return
        if (metadata%execution_identity /= generation%identity) then
            message = 'generation manifest does not match its execution identity'
            return
        end if
        call bind_inventory_to_bundle(inventory, trim(generation%root), &
            status, message)
        if (status /= 0) return
        generation%manifest_id = manifest_id
        generation%input_inventory = inventory
        generation%input_inventory_ready = .true.
        generation%input_inventory_complete = inventory%complete
        generation%input_inventory_diagnostic = inventory%diagnostic
        generation%base_commit = metadata%base_commit
        generation%patch_digest = metadata%patch_digest
        generation%driver_digest = metadata%driver_digest
        generation%driver_size = metadata%driver_size
        message = trim(inventory%diagnostic)
        ierr = 0
    end subroutine generation_load_inventory

    subroutine bind_inventory_to_bundle(inventory, generation_root, ierr, message)
        type(input_inventory_t), intent(inout) :: inventory
        character(len=*), intent(in) :: generation_root
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: i

        ierr = 1
        message = ''
        if (.not. allocated(inventory%roots) .or. &
            len_trim(generation_root) == 0) then
            message = 'cannot bind input inventory to its generation bundle'
            return
        end if
        if (len_trim(generation_root) + len('/bundle') >= PATH_LEN) then
            message = 'generation bundle path exceeds the supported length'
            return
        end if
        do i = 1, inventory%root_count
            inventory%roots(i)%physical_path = &
                trim(generation_root)//'/bundle'
        end do
        ierr = 0
    end subroutine bind_inventory_to_bundle

    subroutine validate_manifest_bundle(project_root, expected, ierr, message)
        character(len=*), intent(in) :: project_root
        type(input_inventory_t), intent(in) :: expected
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(input_inventory_t) :: observed

        ierr = 1
        message = ''
        if (.not. allocated(expected%declarations)) then
            message = 'stored generation manifest omits typed declarations'
            return
        end if
        call input_inventory_rediscover(project_root, expected%declarations, &
            expected, observed, ierr, message, materialized=.true.)
        if (ierr /= 0) return
        if ((observed%complete .neqv. expected%complete) .or. &
            (observed%valid .neqv. expected%valid)) then
            ierr = 1
            message = 'materialized generation differs from its canonical manifest'
            return
        end if
        call compare_materialized_inventory(expected, observed, ierr, message)
        if (ierr /= 0) return
        ierr = 0
        message = ''
    end subroutine validate_manifest_bundle

    subroutine compare_materialized_inventory(expected, observed, ierr, message)
        type(input_inventory_t), intent(in) :: expected, observed
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: i, j, k, l, matches, expected_root, observed_root
        logical :: found

        ierr = 1
        message = 'materialized generation differs from its canonical manifest'
        matches = 0
        do i = 1, expected%root_count
            do j = 1, expected%roots(i)%alias_count
                found = .false.
                do k = 1, observed%root_count
                    do l = 1, observed%roots(k)%alias_count
                        if (trim(observed%roots(k)%aliases(l)) /= &
                            trim(expected%roots(i)%aliases(j))) cycle
                        if (trim(observed%roots(k)%bundle_paths(l)) /= &
                            trim(expected%roots(i)%bundle_paths(j))) return
                        found = .true.
                        matches = matches + 1
                        exit
                    end do
                    if (found) exit
                end do
                if (.not. found) return
            end do
        end do
        if (matches /= total_alias_count(observed)) return
        do i = 1, expected%entry_count
            expected_root = root_with_alias(expected, &
                trim(expected%entries(i)%root_alias))
            if (expected_root == 0) return
            do j = 1, expected%roots(expected_root)%alias_count
                found = .false.
                do k = 1, observed%entry_count
                    if (.not. matching_materialized_entry( &
                            expected%entries(i), observed%entries(k))) cycle
                    observed_root = root_with_alias(observed, &
                        trim(observed%entries(k)%root_alias))
                    if (observed_root == 0) return
                    do l = 1, observed%roots(observed_root)%alias_count
                        if (trim(observed%roots(observed_root)%aliases(l)) /= &
                            trim(expected%roots(expected_root)%aliases(j))) cycle
                        if (trim(observed%roots(observed_root)%bundle_paths(l)) /= &
                            trim(expected%roots(expected_root)%bundle_paths(j))) cycle
                        found = .true.
                        exit
                    end do
                    if (found) exit
                end do
                if (.not. found) then
                    message = 'materialized entry is missing: '// &
                        trim(expected%roots(expected_root)%aliases(j))//':'// &
                        trim(expected%entries(i)%relative_path)
                    return
                end if
            end do
        end do
        do i = 1, observed%entry_count
            observed_root = root_with_alias(observed, &
                trim(observed%entries(i)%root_alias))
            if (observed_root == 0) return
            found = .false.
            do k = 1, expected%entry_count
                if (.not. matching_materialized_entry( &
                        expected%entries(k), observed%entries(i))) cycle
                expected_root = root_with_alias(expected, &
                    trim(expected%entries(k)%root_alias))
                if (expected_root == 0) return
                do j = 1, expected%roots(expected_root)%alias_count
                    do l = 1, observed%roots(observed_root)%alias_count
                        if (trim(expected%roots(expected_root)%aliases(j)) /= &
                            trim(observed%roots(observed_root)%aliases(l))) cycle
                        if (trim(expected%roots(expected_root)%bundle_paths(j)) /= &
                            trim(observed%roots(observed_root)%bundle_paths(l))) cycle
                        found = .true.
                        exit
                    end do
                    if (found) exit
                end do
                if (found) exit
            end do
            if (.not. found) then
                message = 'materialized entry is unexpected: '// &
                    trim(observed%entries(i)%root_alias)//':'// &
                    trim(observed%entries(i)%relative_path)
                return
            end if
        end do
        ierr = 0
        message = ''
    end subroutine compare_materialized_inventory

    logical function matching_materialized_entry(expected, observed)
        type(input_entry_t), intent(in) :: expected, observed
        integer :: expected_mode

        matching_materialized_entry = .false.
        if (trim(expected%relative_path) /= trim(observed%relative_path)) return
        if (trim(expected%role) /= trim(observed%role)) return
        if (expected%kind /= observed%kind) return
        if (expected%writable_at_execution .neqv. &
                observed%writable_at_execution) return
        if (expected%content_digest /= observed%content_digest) return
        if (expected%link_target /= observed%link_target) return
        expected_mode = expected%mode
        select case (expected%kind)
        case (INPUT_FILE)
            if (iand(expected_mode, 73) == 0) then
                expected_mode = 292
            else
                expected_mode = 365
            end if
        case (INPUT_DIRECTORY)
            expected_mode = 0
        end select
        if (observed%mode /= expected_mode) return
        matching_materialized_entry = .true.
    end function matching_materialized_entry

    integer function root_with_alias(inventory, alias)
        type(input_inventory_t), intent(in) :: inventory
        character(len=*), intent(in) :: alias
        integer :: i, j

        root_with_alias = 0
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                if (trim(inventory%roots(i)%aliases(j)) /= trim(alias)) cycle
                root_with_alias = i
                return
            end do
        end do
    end function root_with_alias

    integer function total_alias_count(inventory)
        type(input_inventory_t), intent(in) :: inventory
        integer :: i

        total_alias_count = 0
        do i = 1, inventory%root_count
            total_alias_count = total_alias_count + inventory%roots(i)%alias_count
        end do
    end function total_alias_count

    subroutine generation_driver_identity(generation_root, digest, size, ierr, message)
        character(len=*), intent(in) :: generation_root
        character(len=HASH_LEN), intent(out) :: digest
        integer(int64), intent(out) :: size
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: identity_path
        character(len=PATH_LEN) :: line
        integer :: unit, io_status, parse_status
        logical :: found_digest, found_size

        digest = ''
        size = 0_int64
        ierr = 1
        message = ''
        if (len_trim(generation_root) == 0 .or. &
            len_trim(generation_root) + len('/identity.txt') >= len(identity_path)) then
            message = 'generation root cannot hold its identity path'
            return
        end if
        identity_path = trim(generation_root)//'/identity.txt'
        open (newunit=unit, file=trim(identity_path), status='old', action='read', &
            iostat=io_status)
        if (io_status /= 0) then
            message = 'generation identity metadata is unavailable'
            return
        end if
        found_digest = .false.
        found_size = .false.
        do
            read (unit, '(a)', iostat=io_status) line
            if (io_status /= 0) exit
            if (index(line, 'driver_digest=') == 1) then
                if (found_digest) then
                    close (unit)
                    message = 'generation identity repeats its driver digest'
                    return
                end if
                if (len_trim(line) - len('driver_digest=') /= HASH_LEN) then
                    close (unit)
                    message = 'generation identity has an invalid driver digest length'
                    return
                end if
                digest = line(len('driver_digest=') + 1:)
                found_digest = .true.
            else if (index(line, 'driver_size=') == 1) then
                if (found_size) then
                    close (unit)
                    message = 'generation identity repeats its driver size'
                    return
                end if
                read (line(len('driver_size=') + 1:), *, iostat=parse_status) size
                if (parse_status /= 0) then
                    close (unit)
                    message = 'generation identity has an invalid driver size'
                    return
                end if
                found_size = .true.
            end if
        end do
        close (unit)
        if (io_status > 0) then
            message = 'generation identity metadata cannot be read'
            return
        end if
        if (.not. found_digest .or. .not. found_size .or. size <= 0_int64 .or. &
            .not. valid_driver_digest(digest)) then
            message = 'generation identity has no valid pinned fo driver'
            return
        end if
        ierr = 0
    end subroutine generation_driver_identity

    subroutine read_manifest_store(path, store_root, ierr, message)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: store_root
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: unit, ios
        character(len=PATH_LEN) :: line

        ierr = 1
        store_root = ''
        message = 'generation immutable store locator is unavailable'
        line = ''
        open (newunit=unit, file=trim(path), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) line
        close (unit)
        if (ios /= 0 .or. len_trim(line) == 0 .or. &
            len_trim(line) >= len(store_root) .or. line(1:1) /= '/') then
            message = 'generation immutable store locator is malformed'
            return
        end if
        if (index(line, achar(0)) /= 0 .or. index(line, achar(10)) /= 0 .or. &
            index(line, achar(13)) /= 0) then
            message = 'generation immutable store locator is malformed'
            return
        end if
        store_root = trim(line)
        ierr = 0
        message = ''
    end subroutine read_manifest_store

    subroutine write_manifest_store(path, store_root, ierr)
        character(len=*), intent(in) :: path, store_root
        integer, intent(out) :: ierr
        integer :: unit, ios

        ierr = 1
        if (len_trim(store_root) == 0 .or. len_trim(store_root) >= PATH_LEN .or. &
            store_root(1:1) /= '/') return
        if (index(store_root, achar(0)) /= 0 .or. &
            index(store_root, achar(10)) /= 0 .or. &
            index(store_root, achar(13)) /= 0) return
        open (newunit=unit, file=trim(path), status='new', action='write', &
            iostat=ios)
        if (ios /= 0) return
        write (unit, '(a)', iostat=ios) trim(store_root)
        if (ios == 0) flush (unit, iostat=ios)
        close (unit, iostat=ierr)
        if (ios /= 0) ierr = ios
    end subroutine write_manifest_store

    subroutine read_manifest_id(path, manifest_id, ierr, message)
        character(len=*), intent(in) :: path
        character(len=HASH_LEN), intent(out) :: manifest_id
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: unit, ios
        character(len=HASH_LEN + 1) :: line

        ierr = 1
        manifest_id = ''
        message = 'generation manifest reference is unavailable'
        open (newunit=unit, file=trim(path), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) line
        if (ios /= 0) then
            message = 'generation manifest reference is malformed'
            close (unit)
            return
        end if
        if (valid_driver_digest(trim(line))) then
            manifest_id = trim(line)
            ierr = 0
            message = ''
        else
            message = 'generation manifest reference is malformed'
        end if
        close (unit)
    end subroutine read_manifest_id

    subroutine write_manifest_id(path, manifest_id, ierr)
        character(len=*), intent(in) :: path, manifest_id
        integer, intent(out) :: ierr
        integer :: unit, ios

        ierr = 1
        if (.not. valid_driver_digest(manifest_id)) return
        open (newunit=unit, file=trim(path), status='new', action='write', &
            iostat=ios)
        if (ios /= 0) return
        write (unit, '(a)', iostat=ios) trim(manifest_id)
        if (ios == 0) flush (unit, iostat=ios)
        close (unit, iostat=ierr)
        if (ios /= 0) ierr = ios
    end subroutine write_manifest_id

    subroutine write_manifest_identity(path, metadata, ierr)
        character(len=*), intent(in) :: path
        type(generation_manifest_metadata_t), intent(in) :: metadata
        integer, intent(out) :: ierr
        character(len=:), allocatable :: record
        character(len=32) :: driver_size_text

        write (driver_size_text, '(i0)') metadata%driver_size
        record = 'schema=fo-gremlin-manifest-v1'//new_line('a')// &
            'execution_identity='//metadata%execution_identity//new_line('a')// &
            'driver_digest='//metadata%driver_digest//new_line('a')// &
            'driver_size='//trim(driver_size_text)//new_line('a')
        call write_identity(path, record, ierr)
    end subroutine write_manifest_identity

    logical function valid_driver_digest(digest)
        character(len=*), intent(in) :: digest
        integer :: i, value

        valid_driver_digest = .false.
        if (len_trim(digest) /= HASH_LEN) return
        do i = 1, HASH_LEN
            value = iachar(digest(i:i))
            if (.not. ((value >= iachar('0') .and. value <= iachar('9')) .or. &
                (value >= iachar('a') .and. value <= iachar('f')))) return
        end do
        valid_driver_digest = .true.
    end function valid_driver_digest

    subroutine write_identity(path, record, ierr)
        character(len=*), intent(in) :: path, record
        integer, intent(out) :: ierr
        integer :: unit, ios

        ierr = 1
        open (newunit=unit, file=trim(path), status='new', action='write', &
            iostat=ios)
        if (ios /= 0) return
        write (unit, '(a)', iostat=ios) record
        if (ios == 0) flush (unit, iostat=ios)
        close (unit, iostat=ierr)
        if (ios /= 0) ierr = ios
    end subroutine write_identity

    function value_or_empty(value) result(text)
        character(len=:), allocatable, intent(in) :: value
        character(len=:), allocatable :: text
        text = ''
        if (allocated(value)) text = value
    end function value_or_empty

    logical function identity_field_fits(value)
        character(len=:), allocatable, intent(in) :: value

        identity_field_fits = .true.
        if (.not. allocated(value)) return
        identity_field_fits = len(value) <= PART_LEN
    end function identity_field_fits

    subroutine remove_stage(path)
        character(len=*), intent(in) :: path
        integer(c_int) :: ignored
        ignored = fo_c_generation_remove_stage(trim(path)//c_null_char)
    end subroutine remove_stage

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
        text = trim(adjustl(text))
    end function int_text

end module fo_gremlin_generation
