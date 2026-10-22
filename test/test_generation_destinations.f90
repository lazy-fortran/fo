program test_generation_destinations
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_generation_manifest, only: generation_manifest_metadata_t, &
        generation_manifest_capture, generation_manifest_materialize, &
        generation_manifest_execution_identity
    use fo_input_inventory, only: input_inventory_t, input_declaration_t, &
        input_inventory_discover
    use fo_test_harness, only: make_scratch, make_directory, write_text, &
        read_text, remove_tree, assert_true, assert_equal_integer, &
        finish_assertions
    use fo_util, only: int_text
    implicit none
    type(input_inventory_t) :: inventory, restored
    type(input_declaration_t) :: declarations(0)
    type(generation_manifest_metadata_t) :: metadata, loaded
    character(len=:), allocatable :: scratch, project, shared, bundle, path
    character(len=HASH_LEN) :: manifest_id, owner
    character(len=4096) :: message
    character(len=32) :: argument
    integer :: i, shared_root, ierr, count, bad_reads
    real(dp) :: start_cpu, end_cpu
    character, parameter :: nl = new_line('a')

    count = 64
    if (command_argument_count() > 0) then
        call get_command_argument(1, argument)
        read (argument, *) count
    end if
    call make_scratch('fo-generation-destinations', scratch)
    project = scratch//'/project'
    shared = scratch//'/shared'
    bundle = scratch//'/bundle'
    call make_directory(project//'/src')
    call make_directory(shared//'/src')
    call write_text(project//'/fpm.toml', 'name="destinations"'//nl// &
        '[dependencies]'//nl//'one={path="../shared"}'//nl// &
        'two={path="../shared"}'//nl)
    call write_text(project//'/src/main.f90', 'program main'//nl//'end program')
    call write_text(shared//'/fpm.toml', 'name="shared"'//nl)
    do i = 1, count
        path = shared//'/src/payload_'//int_text(i)//'.f90'
        call write_text(path, 'module payload_'//int_text(i)//nl//'end module')
    end do
    call input_inventory_discover(project, declarations, inventory, ierr, message)
    call assert_equal_integer(ierr, 0, 'discover shared-root fixture: '//trim(message))
    shared_root = 0
    do i = 1, inventory%root_count
        if (inventory%roots(i)%alias_count < 2) cycle
        shared_root = i
    end do
    call assert_true(shared_root > 0, 'two dependencies share a physical root')
    if (shared_root == 0) call finish_assertions()
    ! Distinct logical records intentionally occupy the same frozen paths.
    inventory%roots(shared_root)%bundle_paths(2) = &
        inventory%roots(shared_root)%bundle_paths(1)
    metadata%toolchain = 'synthetic'
    metadata%driver_digest = cache_digest(['synthetic-driver'], 1)
    metadata%driver_size = 1
    metadata%execution_identity = &
        generation_manifest_execution_identity(metadata, inventory)
    owner = cache_digest([scratch], 1)
    call generation_manifest_capture(scratch//'/store', owner, metadata, &
        inventory, manifest_id, ierr, message)
    call assert_equal_integer(ierr, 0, 'capture aliased inputs: '//trim(message))
    call cpu_time(start_cpu)
    call generation_manifest_materialize(scratch//'/store', manifest_id, bundle, &
        loaded, restored, ierr, message)
    call cpu_time(end_cpu)
    print '(a,f12.6)', 'materialization CPU seconds: ', end_cpu - start_cpu
    call assert_equal_integer(ierr, 0, 'materialize aliases: '//trim(message))
    bad_reads = 0
    do i = 1, count
        path = bundle//'/'// &
            trim(inventory%roots(shared_root)%bundle_paths(1))// &
            '/src/payload_'//int_text(i)//'.f90'
        if (read_text(path) /= 'module payload_'//int_text(i)//nl//'end module') &
            bad_reads = bad_reads + 1
    end do
    call assert_equal_integer(bad_reads, 0, 'every distinct payload survives aliases')
    call remove_tree(scratch)
    call finish_assertions()
end program test_generation_destinations
