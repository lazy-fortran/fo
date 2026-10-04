program test_gremlin_manifest
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long, c_null_char, &
        c_size_t
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use fo_cache, only: HASH_LEN, cache_file_digest
    use fo_fs, only: fs_make_dir, fs_write_text
    use fo_input_inventory, only: input_inventory_t, input_declaration_t, &
        input_inventory_discover, INPUT_FILE
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
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
    end interface

    character(len=512) :: root, project, dependency, cache, cache_b, driver
    character(len=512) :: first_file, second_file, mutated_file, message
    character(len=HASH_LEN) :: driver_digest
    integer(int64) :: driver_size
    integer :: ierr, status, unit, ios, i, shared_root
    type(input_declaration_t) :: declarations(1)
    type(input_inventory_t) :: inventory, changed
    type(generation_context_t) :: context
    type(generation_t) :: first, reused, mutated, corrupted_reuse
    logical :: found_aliases

    root = '/var/tmp/fo-generation-manifest-'//int_text(process_getpid())
    project = trim(root)//'/project'
    dependency = trim(root)//'/shared'
    cache = trim(root)//'/cache'
    cache_b = trim(root)//'/cache-b'
    driver = trim(root)//'/driver-image'
    call remove_fixture(trim(root))
    call fs_make_dir(trim(project)//'/src')
    call fs_make_dir(trim(dependency)//'/src')
    status = c_setenv('FO_CACHE_DIR'//c_null_char, &
        trim(cache)//c_null_char, 1_c_int)
    call require(status == 0, 'isolated Fx cache is configured')
    call write(trim(project)//'/fpm.toml', &
        'name = "manifest-fixture"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'shared-one = { path = "../shared" }'//new_line('a')// &
        'shared-two = { path = "../shared" }'//new_line('a'))
    call write(trim(dependency)//'/fpm.toml', 'name = "shared"')
    call write(trim(project)//'/src/main.f90', 'program main')
    call write(trim(project)//'/fixture.dat', 'source-one')
    call write(trim(project)//'/target.dat', 'link-target')
    status = c_symlink('target.dat'//c_null_char, &
        trim(project)//'/alias.dat'//c_null_char)
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
    call generation_capture(trim(project), trim(cache), &
        context, first, ierr, message)
    call require(ierr == 0, 'canonical manifest generation capture: '//trim(message))
    if (ierr == 0) then
        first_file = trim(first%project_root)//'/fixture.dat'
        call require(file_equals(trim(first_file), 'source-one'), &
            'captured project file reconstructs from its blob')
        first_file = trim(first%project_root)//'/alias.dat'
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

    call remove_fixture(trim(root))
    stop

contains

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
