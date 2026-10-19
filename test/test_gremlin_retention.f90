program test_gremlin_retention
    !! Retained generations, bytes, pins, live owners and views are status data.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use fo_util, only: temporary_root, int_text
    use fo_cache, only: HASH_LEN, cache_file_digest
    use fo_fs, only: fs_make_dir, fs_write_text
    use fo_input_inventory, only: input_inventory_t, input_declaration_t, &
        input_inventory_discover
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_gremlin_retention, only: gremlin_retention_json, gremlin_trim_cache
    use fo_gremlin_state, only: gremlin_generation_pin_at, &
        gremlin_generation_register_lease_at, gremlin_lease_t, &
        gremlin_lease_release, gremlin_client_identity
    use fo_process, only: process_getpid
    use fo_test_harness, only: tree_remove => remove_tree, set_mode, set_mtime, file_exists
    implicit none

    interface
        integer(c_int) function remove_tree(path) &
                bind(C, name='fo_c_generation_remove_stage')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function remove_tree
        integer(c_int) function c_setenv(name, value, overwrite) &
                bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
        integer(c_int) function scratch_create(view, parent, output, capacity) &
                bind(C, name='fo_c_execution_scratch_create')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: view(*), parent(*)
            character(kind=c_char), intent(out) :: output(*)
            integer(c_int), value :: capacity
        end function scratch_create
    end interface

    character(len=512) :: root, project, cache, driver, message
    character(len=HASH_LEN) :: driver_digest
    integer(int64) :: driver_size
    integer :: ierr
    type(input_declaration_t) :: declarations(0)
    type(input_inventory_t) :: inventory
    type(generation_context_t) :: context
    type(generation_t) :: first, second
    type(gremlin_lease_t) :: lease
    character(len=:), allocatable :: json

    root = temporary_root()//'/fo-retention-'//trim(int_text(process_getpid()))
    project = trim(root)//'/project'
    cache = trim(root)//'/cache'
    driver = trim(root)//'/driver-image'
    ierr = remove_tree(trim(root)//c_null_char)
    call fs_make_dir(trim(project)//'/src')
    ierr = c_setenv('FO_CACHE_DIR'//c_null_char, trim(cache)//c_null_char, 1_c_int)
    call fs_write_text(trim(project)//'/fpm.toml', 'name = "retain"')
    call fs_write_text(trim(project)//'/src/a.f90', repeat('a', 3000))
    call fs_write_text(trim(driver), 'verified test driver image')
    call cache_file_digest(trim(driver), driver_digest)
    inquire (file=trim(driver), size=driver_size)
    context%toolchain = 'test-toolchain'
    context%flags = '-O0'
    context%environment = 'test-env'
    context%base_commit = 'base'
    context%patch_digest = 'patch'
    context%driver_path = trim(driver)
    context%driver_digest = driver_digest
    context%driver_size = driver_size

    json = gremlin_retention_json('', '')
    call require(value_of(json, 'generations') == 0 .and. &
        value_of(json, 'bytes') == 0, 'no store reports empty retention')

    call capture(first)
    json = gremlin_retention_json(first%root, trim(root)//'/views')
    call require(value_of(json, 'generations') == 1, 'one generation is retained')
    call require(value_of(json, 'bytes') >= 3000, &
        'generation bytes include the captured file')
    call require(value_of(json, 'pinned') == 0 .and. value_of(json, 'owners') == 0, &
        'an idle generation has no pin and no owner')

    call fs_write_text(trim(project)//'/src/a.f90', repeat('b', 5000))
    call capture(second)
    json = gremlin_retention_json(second%root, trim(root)//'/views')
    call require(value_of(json, 'generations') == 2, 'two generations are retained')
    call require(value_of(json, 'bytes') >= 8000, 'bytes grow with the new generation')

    call gremlin_generation_register_lease_at(trim(first%root), lease, ierr, &
        message)
    call require(ierr == 0, 'register a generation: '//trim(message))
    call gremlin_lease_release(lease, ierr, message)
    call gremlin_generation_pin_at(trim(first%root), .true., ierr, message)
    call require(ierr == 0, 'pin a generation: '//trim(message))
    call gremlin_generation_register_lease_at(trim(second%root), lease, ierr, &
        message)
    call require(ierr == 0, 'lease a generation: '//trim(message))
    json = gremlin_retention_json(second%root, trim(root)//'/views')
    call require(value_of(json, 'pinned') == 1, 'the pin is reported')
    call require(value_of(json, 'owners') == 1, 'the live lease is reported as owner')
    call gremlin_lease_release(lease, ierr, message)
    json = gremlin_retention_json(second%root, trim(root)//'/views')
    call require(value_of(json, 'owners') == 0, 'a released lease is no longer an owner')

    call fs_make_dir(trim(root)//'/views/.building-execution-1/project')
    call fs_write_text(trim(root)//'/views/.building-execution-1/project/f', &
        repeat('v', 700))
    json = gremlin_retention_json(second%root, trim(root)//'/views')
    call require(value_of(json, 'views') == 1 .and. value_of(json, 'view_bytes') &
        >= 700, 'views and their bytes are reported')
    call require(index(json, '"view_bytes_exact":true') > 0, &
        'the small view walk finished inside its budget')

    call gremlin_generation_pin_at(trim(first%root), .false., ierr, message)
    call test_view_budget()
    call test_orphan_budget()
    call tree_remove(trim(root))
    write (*, '(a)') 'test_gremlin_retention: PASS'

contains

    subroutine test_orphan_budget()
        character(:), allocatable :: state, view, scratch_path
        character(kind=c_char, len=4096) :: scratch_buffer
        integer :: rc, ending

        state = trim(root)//'/orphan/gremlin/projects/project/lane'
        view = state//'/views/execution-old'
        call fs_make_dir(view)
        call set_mode(view, 448)
        call fs_write_text(view//'/data', repeat('x', 999))
        rc = scratch_create(view//c_null_char, trim(root)//c_null_char, &
            scratch_buffer, len(scratch_buffer))
        call require(rc == 0, 'create a validated orphan scratch reference')
        ending = index(scratch_buffer, c_null_char)
        call require(ending > 1, 'scratch has an observable private pathname')
        scratch_path = scratch_buffer(:ending - 1)
        call fs_make_dir(state//'/async-processes/dead-owner')
        call fs_write_text(state//'/async-processes/dead-owner/pending', '?')
        call set_mtime(view, 1_int64)
        rc = gremlin_trim_cache('', state, 1_int64, 0, 60)
        call require(rc == 0 .and. file_exists(view//'/data'), &
            'a pending payload record protects every view of a dead lane')
        call require(file_exists(scratch_path), 'pending payload retains its scratch')
        call tree_remove(state//'/async-processes/dead-owner')
        rc = gremlin_trim_cache('', state, 1_int64, 0, 60)
        call require(rc == 1, 'a drained dead lane can retire its orphan view')
        call require(.not. file_exists(view), 'orphan view is gone')
        call require(.not. file_exists(scratch_path), 'validated orphan scratch is gone')
    end subroutine test_orphan_budget

    subroutine test_view_budget()
        character(:), allocatable :: state, old, live, invalid, young, path
        character(len=64) :: start
        integer :: pid, unit, rc, i
        character(len=1), parameter :: names(3) = ['a', 'b', 'c']

        state = trim(root)//'/trim/gremlin/projects/project/lane'
        old = state//'/views'
        live = trim(root)//'/trim/gremlin/projects/project/live'
        invalid = trim(root)//'/trim/gremlin/projects/project/invalid'
        young = old//'/execution-young'
        call fs_make_dir(old)
        do i = 1, 3
            path = old//'/execution-'//names(i)
            call fs_make_dir(path)
            call set_mode(path, 448)
            call fs_write_text(path//'/data', repeat('x', 999))
            call set_mtime(path, int(i, int64)*1000000000_int64)
        end do
        call fs_make_dir(young)
        call set_mode(young, 448)
        call fs_write_text(young//'/data', repeat('y', 999))
        call fs_make_dir(live//'/views/execution-live')
        call set_mode(live//'/views/execution-live', 448)
        call fs_write_text(live//'/views/execution-live/data', repeat('l', 999))
        call set_mtime(live//'/views/execution-live', 1_int64)
        call gremlin_client_identity(.false., pid, start, rc)
        call require(rc == 0, 'capture the independent live owner identity')
        open(newunit=unit, file=live//'/owner', status='replace')
        write(unit, '(a)') 'live-session'
        write(unit, '(i0)') pid
        write(unit, '(a)') trim(start)
        close(unit)
        call fs_make_dir(invalid//'/views/execution-invalid')
        call set_mode(invalid//'/views/execution-invalid', 448)
        call fs_write_text(invalid//'/views/execution-invalid/data', repeat('i', 999))
        call fs_write_text(invalid//'/views/execution-invalid/.fo-tmp-ref', '?')
        call set_mtime(invalid//'/views/execution-invalid', 1_int64)

        rc = gremlin_trim_cache('', state, 6000_int64, 300, 60)
        call require(rc == 2, 'budget evicts the two oldest eligible views')
        call require(.not. file_exists(old//'/execution-a/data') .and. &
            .not. file_exists(old//'/execution-b/data'), 'oldest views go first')
        call require(file_exists(old//'/execution-c/data'), 'newer eligible view stays')
        rc = gremlin_trim_cache('', state, 1_int64, 300, 60)
        call require(rc == 0 .and. file_exists(old//'/execution-c/data'), &
            'a shared stamp bounds the frequency of budget walks')
        rc = gremlin_trim_cache('', state, 1_int64, 0, 60)
        call require(rc == 1, 'a later sweep retires the remaining old view')
        call require(file_exists(young//'/data'), 'grace protects a young view')
        call require(file_exists(live//'/views/execution-live/data'), &
            'live owner protects completed build views without scratch')
        call require(file_exists(invalid//'/views/execution-invalid/data'), &
            'an invalid scratch reference cannot authorize deletion')
        rc = gremlin_trim_cache('', state, 0_int64, 0, 0)
        call require(rc == 0 .and. file_exists(young//'/data'), &
            'zero disables the retained-view budget')
    end subroutine test_view_budget

    subroutine capture(generation)
        type(generation_t), intent(out) :: generation

        call input_inventory_discover(trim(project), declarations, inventory, &
            ierr, message)
        call require(ierr == 0, 'inventory discovery: '//trim(message))
        context%input_inventory = inventory
        call generation_capture(trim(project), trim(cache), context, generation, &
            ierr, message)
        call require(ierr == 0, 'generation capture: '//trim(message))
    end subroutine capture

    integer(int64) function value_of(text, key) result(number)
        character(len=*), intent(in) :: text, key
        integer :: at, finish, ios

        number = -1
        at = index(text, '"'//key//'":')
        if (at == 0) return
        at = at + len(key) + 3
        finish = at
        do while (finish <= len(text))
            if (text(finish:finish) < '0' .or. text(finish:finish) > '9') exit
            finish = finish + 1
        end do
        read (text(at:finish - 1), *, iostat=ios) number
        if (ios /= 0) number = -1
    end function value_of

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

end program test_gremlin_retention
