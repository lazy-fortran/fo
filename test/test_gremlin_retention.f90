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
    use fo_gremlin_retention, only: gremlin_retention_json
    use fo_gremlin_state, only: gremlin_generation_pin_at, &
        gremlin_generation_register_lease_at, gremlin_lease_t, &
        gremlin_lease_release
    use fo_process, only: process_getpid
    use fo_test_harness, only: tree_remove => remove_tree
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
    call tree_remove(trim(root))
    write (*, '(a)') 'test_gremlin_retention: PASS'

contains

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
