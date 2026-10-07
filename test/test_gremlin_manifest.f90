program test_gremlin_manifest
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long, c_null_char, &
        c_size_t
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use fo_cache, only: HASH_LEN, cache_file_digest, cache_digest
    use fo_fs, only: fs_make_dir, fs_write_text
    use fo_input_inventory, only: input_inventory_t, input_declaration_t, &
        input_inventory_discover, INPUT_FILE
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_generation_manifest, only: generation_manifest_metadata_t, &
        generation_manifest_materialize
    use fo_gremlin_state, only: gremlin_generation_register_at, &
        gremlin_generation_pin_at, gremlin_generation_prune_at, &
        gremlin_generation_prune
    use fx_immutable_store, only: immutable_store_t, immutable_lease_t, &
        immutable_store_init, immutable_store_graph_read_lease_acquire, &
        immutable_store_lease_release, IMMUTABLE_OK, IMMUTABLE_MISSING
    use fo_process, only: process_getpid
    implicit none

    interface
        integer(c_int) function c_setenv(name, value, overwrite) &
                bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
        integer(c_int) function c_access(path, mode) bind(C, name='access')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_access
        integer(c_int) function c_chmod(path, mode) bind(C, name='chmod')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_chmod
        integer(c_long) function copy_sync_count(reset) &
                bind(C, name='fo_c_generation_copy_sync_count')
            import :: c_int, c_long
            integer(c_int), value :: reset
        end function copy_sync_count
        integer(c_int) function c_symlink(target, path) bind(C, name='symlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: target(*), path(*)
        end function c_symlink
        integer(c_long) function c_readlink(path, target, capacity) &
                bind(C, name='readlink')
            import :: c_char, c_long, c_size_t
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: target(*)
            integer(c_size_t), value :: capacity
        end function c_readlink
        integer(c_int) function remove_tree(path) &
                bind(C, name='fo_c_generation_remove_stage')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function remove_tree
        integer(c_int) function c_unlink(path) bind(C, name='unlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_unlink
        integer(c_int) function c_mark_release(path, store, owner) &
                bind(C, name='fo_gremlin_generation_mark_release')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*), store(*), owner(*)
        end function c_mark_release
        integer(c_int) function c_raw_prune(path) &
                bind(C, name='fo_gremlin_generation_prune_at')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_raw_prune
    end interface

    character(len=512) :: root, project, dependency, nested_dependency
    character(len=512) :: cache, cache_b, cache_other, cache_failed, driver
    character(len=512) :: cache_legacy, cache_crash
    character(len=512) :: first_file, second_file, mutated_file, message
    character(len=HASH_LEN) :: driver_digest
    integer(int64) :: driver_size
    integer :: ierr, status, unit, ios, i, shared_root, conflict_entry
    type(input_declaration_t) :: declarations(1)
    type(input_inventory_t) :: inventory, changed, conflicting
    type(generation_context_t) :: context
    type(generation_t) :: first, reused, mutated, corrupted_reuse, conflict
    type(generation_t) :: other_worktree, failed_capture
    type(generation_t) :: legacy_worktree, crash_worktree
    logical :: found_aliases

    root = '/var/tmp/fo-generation-manifest-'//int_text(process_getpid())
    project = trim(root)//'/project'
    dependency = trim(root)//'/shared'
    nested_dependency = trim(project)//'/test-support/fo_test_os'
    cache = trim(root)//'/cache'
    cache_b = trim(root)//'/cache-b'
    cache_other = trim(root)//'/other-worktree-cache'
    cache_failed = trim(root)//'/failed-generation-cache'
    cache_legacy = trim(root)//'/legacy-worktree-cache'
    cache_crash = trim(root)//'/crash-worktree-cache'
    driver = trim(root)//'/driver-image'
    call remove_fixture(trim(root))
    call fs_make_dir(trim(project)//'/src')
    call fs_make_dir(trim(nested_dependency)//'/src')
    call fs_make_dir(trim(dependency)//'/src')
    status = c_symlink('shared'//c_null_char, &
        trim(root)//'/shared-alias'//c_null_char)
    call require(status == 0, 'fixture creates a second path to one dependency root')
    status = c_setenv('FO_CACHE_DIR'//c_null_char, &
        trim(cache)//c_null_char, 1_c_int)
    call require(status == 0, 'isolated Fx cache is configured')
    call write(trim(project)//'/fpm.toml', &
        'name = "manifest-fixture"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
            'shared-one = { path = "../shared" }'//new_line('a')// &
            'shared-two = { path = "../shared-alias" }'//new_line('a')// &
            '[dev-dependencies]'//new_line('a')// &
            'fo_test_os = { path = "test-support/fo_test_os" }')
    call write(trim(dependency)//'/fpm.toml', 'name = "shared"')
    call write(trim(nested_dependency)//'/fpm.toml', 'name = "fo_test_os"')
    call write(trim(nested_dependency)//'/src/fo_test_os.f90', &
        'module fo_test_os')
    call write(trim(project)//'/src/main.f90', 'program main')
    call write(trim(project)//'/fixture.dat', 'source-one')
    status = c_chmod(trim(project)//'/fixture.dat'//c_null_char, 493_c_int)
    call require(status == 0, 'fixture payload is executable')
    call write(trim(project)//'/src/target.dat', 'link-target')
    status = c_symlink('target.dat'//c_null_char, &
        trim(project)//'/src/alias.dat'//c_null_char)
    call require(status == 0, 'fixture creates a contained input symlink')
    call write(trim(dependency)//'/src/shared.f90', 'module shared')
    call write(trim(driver), 'verified test driver image')

    declarations(1)%root_alias = 'project'
    declarations(1)%relative_path = 'fixture.dat'
    declarations(1)%role = 'test-fixture'
    declarations(1)%expected_kind = INPUT_FILE
    call discover(inventory)
    found_aliases = .false.
    shared_root = 0
    do i = 1, inventory%root_count
        if (inventory%roots(i)%alias_count < 2) cycle
        shared_root = i
        found_aliases = .true.
        exit
    end do
    call require(found_aliases, 'same physical dependency has both logical aliases')
    if (found_aliases) then
        call require(len_trim(inventory%roots(shared_root)%bundle_paths(1)) > 0 &
            .and. len_trim(inventory%roots(shared_root)%bundle_paths(2)) > 0, &
            'each logical alias has a bundle destination')
        call require(trim(inventory%roots(shared_root)%bundle_paths(1)) /= &
            trim(inventory%roots(shared_root)%bundle_paths(2)), &
            'aliases preserve distinct bundle destinations for one physical root')
    end if

    call cache_file_digest(trim(driver), driver_digest)
    inquire (file=trim(driver), size=driver_size)
    context%toolchain = 'test-toolchain'
    context%flags = '-O0'
    context%environment = 'test-env'
    context%base_commit = 'base-a'
    context%patch_digest = 'patch-a'
    context%driver_path = trim(driver)
    context%driver_digest = driver_digest
    context%driver_size = driver_size
    context%input_inventory = inventory
    status = int(copy_sync_count(1_c_int), c_int)
    call generation_capture(trim(project), trim(cache), &
        context, first, ierr, message)
    call require(ierr == 0, 'canonical manifest generation capture: '//trim(message))
    call require(copy_sync_count(0_c_int) == 0_c_long, &
        'manifest capture payloads perform no per-file sync')
    if (ierr == 0) then
        first_file = trim(first%project_root)//'/fixture.dat'
        call require(file_equals(trim(first_file), 'source-one'), &
            'captured project file reconstructs from its blob')
        call require(c_access(trim(first_file)//c_null_char, 1_c_int) == 0, &
            'captured file preserves executable mode')
        first_file = trim(first%project_root)//'/src/alias.dat'
        call require(link_points_to(trim(first_file), 'target.dat'), &
            'manifest preserves literal symlink target')
        call require(file_exists(trim(first%root)//'/manifest.id'), &
            'generation keeps a reference to its immutable manifest')
        if (found_aliases) then
            first_file = trim(first%root)//'/bundle/'// &
                trim(inventory%roots(shared_root)%bundle_paths(1))//'/src/shared.f90'
            second_file = trim(first%root)//'/bundle/'// &
                trim(inventory%roots(shared_root)%bundle_paths(2))//'/src/shared.f90'
            call require(file_equals(trim(first_file), 'module shared'), &
                'first alias materializes dependency content')
            call require(file_equals(trim(second_file), 'module shared'), &
                'second alias reuses payload at its own destination')
        end if
        call require(file_equals(trim(first%project_root)// &
            '/test-support/fo_test_os/fpm.toml', 'name = "fo_test_os"'), &
            'nested path dev dependency materializes its manifest once')
        call require(file_equals(trim(first%project_root)// &
            '/test-support/fo_test_os/src/fo_test_os.f90', &
            'module fo_test_os'), &
            'nested path dev dependency materializes source bytes')
    end if
    if (ierr == 0) call verify_bundle_rebuild(first)

    conflicting = inventory
    conflict_entry = 0
    do i = 1, conflicting%entry_count
        if (trim(conflicting%entries(i)%root_alias) /= &
                'dependency:fo_test_os' .or. &
            trim(conflicting%entries(i)%relative_path) /= 'fpm.toml') cycle
        conflict_entry = i
        conflicting%entries(i)%mode = &
            modulo(conflicting%entries(i)%mode + 1, 512)
        exit
    end do
    call require(conflict_entry > 0, &
        'nested dependency inventory includes its fpm.toml record')
    if (conflict_entry > 0) then
        context%flags = 'conflicting-input-records'
        context%input_inventory = conflicting
        call generation_capture(trim(project), trim(cache), context, conflict, &
            ierr, message)
        call require(ierr /= 0 .and. &
            index(trim(message), &
                'logical path has conflicting generation input records') > 0, &
            'overlapping destination rejects conflicting mode metadata')
        context%flags = '-O0'
        context%input_inventory = inventory
    end if

    context%base_commit = 'base-b'
    context%patch_digest = 'patch-b'
    status = c_setenv('FO_CACHE_DIR'//c_null_char, &
        trim(cache_b)//c_null_char, 1_c_int)
    call require(status == 0, 'second Fx cache environment is configured')
    call generation_capture(trim(project), trim(cache), &
        context, reused, ierr, message)
    call require(ierr == 0, 'generation reuse with provenance change: '//trim(message))
    if (ierr == 0) then
        call require(reused%identity == first%identity, &
            'provenance-only change preserves execution identity')
        call require(reused%base_commit == 'base-a', &
            'reused generation keeps its original provenance')
        call require(reused%store_root == first%store_root, &
            'recovery follows the generation-pinned Fx store')
    end if

    call write(trim(project)//'/fixture.dat', 'source-two')
    call discover(changed)
    context%input_inventory = changed
    call generation_capture(trim(project), trim(cache), &
        context, mutated, ierr, message)
    call require(ierr == 0, 'generation capture after source mutation: '//trim(message))
    if (ierr == 0) then
        call require(mutated%identity /= first%identity, &
            'source mutation changes the execution identity')
        call require(mutated%store_root /= first%store_root, &
            'new generation records its own current Fx store')
        call require(file_equals(trim(first%project_root)//'/fixture.dat', &
            'source-one'), 'prior generation remains immutable after mutation')
        call require(file_equals(trim(mutated%project_root)//'/fixture.dat', &
            'source-two'), 'new generation contains the changed input')
        mutated_file = trim(mutated%project_root)//'/fixture.dat'
        status = c_chmod(trim(mutated%project_root)//c_null_char, 493_c_int)
        status = c_chmod(trim(mutated_file)//c_null_char, 420_c_int)
        call write(trim(mutated_file), 'cache-corruption')
        call require(file_equals(trim(mutated_file), 'cache-corruption'), &
            'fixture changes bytes in the cached generation')
        status = c_chmod(trim(mutated_file)//c_null_char, 292_c_int)
        status = c_chmod(trim(mutated%project_root)//c_null_char, 365_c_int)
        call generation_capture(trim(project), trim(cache), context, &
            corrupted_reuse, ierr, message)
        call require(ierr /= 0, &
            'reuse rejects changed bytes in the materialized generation')
    end if

    call write(trim(project)//'/fixture.dat', 'source-one')
    call discover(inventory)
    context%input_inventory = inventory
    if (len_trim(first%root) > 0) call verify_root_lifecycle()
    call remove_fixture(trim(root))
    stop

contains

    subroutine verify_root_lifecycle()
        type(immutable_store_t) :: store
        character(len=512) :: failed_stage
        character(len=512) :: owner_path(1)
        character(len=HASH_LEN) :: failed_owner
        integer :: local_status

        local_status = c_setenv('FO_CACHE_DIR'//c_null_char, &
            trim(cache)//c_null_char, 1_c_int)
        call require(local_status == 0, 'root oracle uses the first Fx store')
        local_status = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(root)//'/state'//c_null_char, 1_c_int)
        call require(local_status == 0, 'root oracle isolates generation sidecars')
        call immutable_store_init(store, trim(first%store_root), local_status)
        call require(local_status == IMMUTABLE_OK, &
            'shared Fx store opens for lifecycle oracle')
        if (local_status /= IMMUTABLE_OK) return
        call gremlin_generation_register_at(trim(first%root), local_status, message)
        call require(local_status == 0, 'first generation registers for pruning')
        call assert_generation_root(store, first, .true., &
            'first generation owns durable Fx roots')

        call generation_capture(trim(project), trim(cache_other), context, &
            other_worktree, local_status, message)
        call require(local_status == 0, &
            'second worktree captures same generation: '//trim(message))
        if (local_status /= 0) return
        call require(other_worktree%identity == first%identity .and. &
            other_worktree%root /= first%root, &
            'two worktrees have separate materializations of the same version')
        call assert_generation_root(store, other_worktree, .true., &
            'second worktree independently owns Fx roots')

        call gremlin_generation_pin_at(trim(first%root), .true., &
            local_status, message)
        call require(local_status == 0, 'first generation can be pinned')
        call gremlin_generation_prune_at(trim(first%root), local_status, message)
        call require(local_status /= 0, 'pin prevents first generation prune')
        call assert_generation_root(store, first, .true., &
            'blocked prune retains first generation roots')
        call gremlin_generation_pin_at(trim(first%root), .false., &
            local_status, message)
        call require(local_status == 0, 'first generation can be unpinned')
        call gremlin_generation_prune_at(trim(first%root), local_status, message)
        call require(local_status == 0, &
            'unprotected first generation prunes: '//trim(message))
        call assert_generation_root(store, first, .false., &
            'pruned generation releases its Fx roots')
        call assert_generation_root(store, other_worktree, .true., &
            'pruning one worktree preserves the other worktree roots')
        call gremlin_generation_register_at(trim(other_worktree%root), &
            local_status, message)
        call require(local_status == 0, 'second worktree generation registers')

        call fs_make_dir(trim(cache_failed)//'/gremlin/generations/.capture')
        failed_stage = trim(cache_failed)//'/gremlin/generations/.capture'
        local_status = c_chmod(trim(failed_stage)//c_null_char, 365_c_int)
        call require(local_status == 0, 'failure fixture makes capture stage read-only')
        context%flags = 'failed-publication-oracle'
        call generation_capture(trim(project), trim(cache_failed), context, &
            failed_capture, local_status, message)
        call require(local_status /= 0, &
            'failed stage prevents generation publication')
        owner_path(1) = trim(failed_capture%root)
        failed_owner = cache_digest(owner_path, 1)
        call assert_root_owner(store, failed_owner, &
            .false., 'failed publication releases its durable roots')
        context%flags = '-O0'
        local_status = c_chmod(trim(failed_stage)//c_null_char, 493_c_int)
        call require(local_status == 0, 'failure fixture restores capture stage')

        call gremlin_generation_prune_at(trim(other_worktree%root), &
            local_status, message)
        call require(local_status == 0, 'second worktree generation prunes')
        call assert_generation_root(store, other_worktree, .false., &
            'second worktree releases its own roots')

        call generation_capture(trim(project), trim(cache_legacy), context, &
            legacy_worktree, local_status, message)
        call require(local_status == 0, 'legacy-shaped generation captures')
        if (local_status /= 0) return
        call gremlin_generation_register_at(trim(legacy_worktree%root), &
            local_status, message)
        call require(local_status == 0, 'legacy-shaped generation registers')
        local_status = c_chmod(trim(legacy_worktree%root)//c_null_char, 493_c_int)
        call require(local_status == 0, 'legacy fixture permits locator removal')
        local_status = c_unlink( &
            trim(legacy_worktree%root)//'/root.owner'//c_null_char)
        call require(local_status == 0, 'legacy fixture lacks owner locator')
        local_status = c_chmod(trim(legacy_worktree%root)//c_null_char, 365_c_int)
        call require(local_status == 0, 'legacy fixture restores frozen root')
        call gremlin_generation_prune_at(trim(legacy_worktree%root), &
            local_status, message)
        call require(local_status == 0 .and. index(message, 'legacy') > 0, &
            'legacy manifest materialization prunes with explicit root retention')
        call assert_generation_root(store, legacy_worktree, .true., &
            'legacy owner remains protected after materialization prune')

        call generation_capture(trim(project), trim(cache_crash), context, &
            crash_worktree, local_status, message)
        call require(local_status == 0, 'crash recovery generation captures')
        if (local_status /= 0) return
        call gremlin_generation_register_at(trim(crash_worktree%root), &
            local_status, message)
        call require(local_status == 0, 'crash recovery generation registers')
        call read_root_owner(crash_worktree, failed_owner, local_status)
        call require(local_status == 0, 'crash fixture reads durable owner')
        local_status = c_mark_release(trim(crash_worktree%root)//c_null_char, &
            trim(crash_worktree%store_root)//c_null_char, &
            trim(failed_owner)//c_null_char)
        call require(local_status == 0, 'prune records pending Fx release')
        local_status = c_raw_prune(trim(crash_worktree%root)//c_null_char)
        call require(local_status == 0, 'simulated crash follows materialization prune')
        call assert_root_owner(store, failed_owner, .true., &
            'crash window retains pending durable root')
        call gremlin_generation_prune(crash_worktree%identity, local_status, message)
        call require(local_status == 0, &
            'repeated prune completes interrupted Fx release: '//trim(message))
        call assert_root_owner(store, failed_owner, .false., &
            'recovery releases orphaned Fx root')
    end subroutine verify_root_lifecycle

    subroutine assert_generation_root(store, generation, expected, label)
        type(immutable_store_t), intent(in) :: store
        type(generation_t), intent(in) :: generation
        logical, intent(in) :: expected
        character(len=*), intent(in) :: label
        character(len=HASH_LEN) :: owner
        integer :: local_status

        call read_root_owner(generation, owner, local_status)
        call assert_root_owner(store, owner, expected, label)
    end subroutine assert_generation_root

    subroutine read_root_owner(generation, owner, ierr)
        type(generation_t), intent(in) :: generation
        character(len=HASH_LEN), intent(out) :: owner
        integer, intent(out) :: ierr
        integer :: unit

        owner = ''
        open (newunit=unit, file=trim(generation%root)//'/root.owner', &
            status='old', action='read', iostat=ierr)
        if (ierr == 0) then
            read (unit, '(a)', iostat=ierr) owner
            close (unit)
        end if
        if (ierr /= 0) owner = cache_digest([generation%root], 1)
    end subroutine read_root_owner

    subroutine assert_root_owner(store, root_owner, expected, label)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: root_owner, label
        logical, intent(in) :: expected
        type(immutable_lease_t) :: lease
        integer :: local_status, release_status

        call immutable_store_graph_read_lease_acquire(store, 'fo-generation', &
            root_owner, 'oracle', lease, local_status)
        if (expected) then
            call require(local_status == IMMUTABLE_OK, label)
        else
            call require(local_status == IMMUTABLE_MISSING, label)
        end if
        if (local_status == IMMUTABLE_OK) then
            call immutable_store_lease_release(store, lease, release_status)
            call require(release_status == IMMUTABLE_OK, &
                'oracle releases its temporary Fx read lease')
        end if
    end subroutine assert_root_owner

    subroutine verify_bundle_rebuild(generation)
        type(generation_t), intent(in) :: generation
        type(generation_manifest_metadata_t) :: rebuilt_metadata
        type(input_inventory_t) :: rebuilt_inventory
        character(len=512) :: bundle_root, payload_path
        integer :: local_status, remove_status

        bundle_root = trim(root)//'/rebuildable-bundle'
        payload_path = trim(bundle_root)//'/project/fixture.dat'
        call fs_make_dir(trim(bundle_root))
        call generation_manifest_materialize(trim(generation%store_root), &
            generation%manifest_id, trim(bundle_root), rebuilt_metadata, &
            rebuilt_inventory, local_status, message)
        call require(local_status == 0, &
            'materialize bundle from durable generation manifest: '//trim(message))
        if (local_status /= 0) return
        call require(file_equals(trim(payload_path), 'source-one'), &
            'rebuilt bundle contains the manifest payload bytes')
        call require(c_access(trim(payload_path)//c_null_char, 1_c_int) == 0, &
            'rebuilt bundle preserves the manifest executable mode')

        remove_status = remove_tree(trim(bundle_root)//c_null_char)
        call require(remove_status == 0, 'remove rebuildable bundle')
        call fs_make_dir(trim(bundle_root))
        call generation_manifest_materialize(trim(generation%store_root), &
            generation%manifest_id, trim(bundle_root), rebuilt_metadata, &
            rebuilt_inventory, local_status, message)
        call require(local_status == 0, &
            'rebuild deleted bundle from durable manifest: '//trim(message))
        if (local_status /= 0) return
        call require(file_equals(trim(payload_path), 'source-one'), &
            'recovered bundle contains the manifest payload bytes')
        call require(c_access(trim(payload_path)//c_null_char, 1_c_int) == 0, &
            'recovered bundle preserves the manifest executable mode')
        remove_status = remove_tree(trim(bundle_root)//c_null_char)
        call require(remove_status == 0, 'remove temporary rebuilt bundle')
    end subroutine verify_bundle_rebuild

    subroutine discover(result)
        type(input_inventory_t), intent(out) :: result
        call input_inventory_discover(trim(project), declarations, result, &
            ierr, message)
        call require(ierr == 0, 'canonical inventory discovery: '//trim(message))
    end subroutine discover

    subroutine write(path, value)
        character(len=*), intent(in) :: path, value
        call fs_write_text(path, value)
    end subroutine write

    logical function file_exists(path)
        character(len=*), intent(in) :: path
        file_exists = c_access(trim(path)//c_null_char, 0_c_int) == 0
    end function file_exists

    logical function file_equals(path, expected)
        character(len=*), intent(in) :: path, expected
        character(len=128) :: actual
        integer :: read_unit, read_status
        file_equals = .false.
        open (newunit=read_unit, file=trim(path), status='old', action='read', &
            iostat=read_status)
        if (read_status /= 0) return
        read (read_unit, '(a)', iostat=read_status) actual
        close (read_unit)
        if (read_status /= 0) return
        file_equals = trim(actual) == expected
    end function file_equals

    logical function link_points_to(path, expected)
        character(len=*), intent(in) :: path, expected
        character(kind=c_char) :: value(128)
        integer(c_long) :: count
        integer :: k

        link_points_to = .false.
        if (len(expected) > size(value)) return
        count = c_readlink(trim(path)//c_null_char, value, &
            int(size(value), c_size_t))
        if (count /= len(expected)) return
        do k = 1, len(expected)
            if (value(k) /= expected(k:k)) return
        end do
        link_points_to = .true.
    end function link_points_to

    subroutine remove_fixture(path)
        character(len=*), intent(in) :: path
        integer(c_int) :: rc
        rc = remove_tree(trim(path)//c_null_char)
    end subroutine remove_fixture

    subroutine require(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label
        if (condition) then
            write (*, '(a)') 'PASS: '//trim(label)
        else
            write (error_unit, '(a)') 'FAIL: '//trim(label)
            error stop 1
        end if
    end subroutine require

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
    end function int_text
end program test_gremlin_manifest
