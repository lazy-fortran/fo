program test_gremlin_warm_generation
    !! A warm generation after a one-file edit must write O(changed file) bytes,
    !! not O(tree): unchanged inputs are neither re-copied for capture nor
    !! re-written for the bundle or the build view.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_util, only: temporary_root, int_text
    use fo_cache, only: HASH_LEN, cache_file_digest
    use fo_fs, only: fs_make_dir, fs_write_text
    use fo_input_inventory, only: input_inventory_t, input_declaration_t, &
        input_inventory_discover
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_generation_manifest, only: generation_manifest_materialize_stats
    use fo_gremlin_execution_view, only: execution_view_t, &
        execution_view_create, execution_view_release
    use fo_process, only: process_getpid
    use fo_test_harness, only: tree_remove => remove_tree
    implicit none

    integer, parameter :: FILE_COUNT = 200, PAYLOAD_LEN = 4000

    interface
        integer(c_long) function copy_bytes(reset) &
                bind(C, name='fo_c_generation_copy_bytes')
            import :: c_int, c_long
            integer(c_int), value :: reset
        end function copy_bytes
        integer(c_long) function clone_count(reset) &
                bind(C, name='fo_c_generation_clone_count')
            import :: c_int, c_long
            integer(c_int), value :: reset
        end function clone_count
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
    character(len=PAYLOAD_LEN) :: payload
    integer(kind(1_8)) :: driver_size
    integer :: ierr, i, cloned, copied
    integer(c_long) :: bytes, clones
    type(input_declaration_t) :: declarations(0)
    type(input_inventory_t) :: inventory
    type(generation_context_t) :: context
    type(generation_t) :: first, second
    type(execution_view_t) :: view
    logical :: clone_capable

    root = temporary_root()//'/fo-warm-generation-'//trim(int_text(process_getpid()))
    project = trim(root)//'/project'
    cache = trim(root)//'/cache'
    driver = trim(root)//'/driver-image'
    ierr = remove_tree(trim(root)//c_null_char)
    call fs_make_dir(trim(project)//'/src')
    ierr = c_setenv('FO_CACHE_DIR'//c_null_char, trim(cache)//c_null_char, 1_c_int)
    call require(ierr == 0, 'isolated Fx cache is configured')
    call fs_write_text(trim(project)//'/fpm.toml', 'name = "warm"')
    do i = 1, FILE_COUNT
        call fs_write_text(file_path(i), unique_payload(i, 'a'))
    end do
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

    call discover()
    call generation_capture(trim(project), trim(cache), context, first, ierr, &
        message)
    call require(ierr == 0, 'cold generation capture: '//trim(message))
    bytes = copy_bytes(1_c_int)
    clones = clone_count(1_c_int)
    call generation_manifest_materialize_stats(.true., cloned, copied)
    call require(bytes >= int(FILE_COUNT, c_long) * PAYLOAD_LEN / 2_c_long .or. &
        cloned >= FILE_COUNT, 'cold capture pays for the tree once')

    call fs_write_text(file_path(7), unique_payload(7, 'b'))
    call discover()
    call generation_capture(trim(project), trim(cache), context, second, ierr, &
        message)
    call require(ierr == 0, 'warm generation capture: '//trim(message))
    call require(trim(second%identity) /= trim(first%identity), &
        'edit produces a new generation')
    bytes = copy_bytes(1_c_int)
    call generation_manifest_materialize_stats(.true., cloned, copied)
    write (*, '(a,i0,a,i0,a,i0)') 'warm capture copied bytes=', bytes, &
        ' cloned blobs=', cloned, ' copied blobs=', copied
    call require(bytes <= 2_c_long * PAYLOAD_LEN, &
        'warm capture copies at most the changed file for payload capture')
    ! Whatever the filesystem offers, the bundle must not be re-copied more
    ! than the clone-incapable fallback allows; when clone works it is free.
    clone_capable = cloned > 0
    if (clone_capable) call require(copied == 0, &
        'warm bundle materialization clones every blob')

    clones = clone_count(1_c_int)
    bytes = copy_bytes(1_c_int)
    call execution_view_create(trim(root)//'/views', second%identity, &
        'warm-owner', 'case', second%input_inventory, .true., .true., view, &
        ierr, message, candidate_bundle_root=trim(second%root)//'/bundle')
    call require(ierr == 0, 'build view over the warm bundle: '//trim(message))
    bytes = copy_bytes(1_c_int)
    clones = clone_count(1_c_int)
    write (*, '(a,i0,a,i0)') 'build view copied bytes=', bytes, &
        ' cloned files=', clones
    if (clone_capable) then
        call require(bytes == 0_c_long .and. clones >= int(FILE_COUNT, c_long), &
            'build view shares blocks with the frozen bundle')
    else
        call require(bytes >= int(FILE_COUNT, c_long) * PAYLOAD_LEN / 2_c_long, &
            'filesystem without clone support falls back to byte copy')
    end if
    call require(file_has(trim(view%cwd)//'/src/f007.f90', 'b'), &
        'view holds the edited file content')
    call require(file_has(trim(view%cwd)//'/src/f008.f90', 'a'), &
        'view holds unchanged file content')
    call execution_view_release(view, .false., ierr, message)
    call tree_remove(trim(root))
    write (*, '(a)') 'test_gremlin_warm_generation: PASS'

contains

    function file_path(index) result(path)
        integer, intent(in) :: index
        character(len=:), allocatable :: path
        character(len=8) :: text
        write (text, '(i3.3)') index
        path = trim(project)//'/src/f'//trim(text)//'.f90'
    end function file_path

    function unique_payload(index, fill) result(text)
        integer, intent(in) :: index
        character(len=1), intent(in) :: fill
        character(len=PAYLOAD_LEN) :: text
        character(len=16) :: tag
        text = repeat(fill, PAYLOAD_LEN)
        write (tag, '(a,i0)') '! unique ', index
        text(1:len_trim(tag)) = trim(tag)
    end function unique_payload

    subroutine discover()
        call input_inventory_discover(trim(project), declarations, inventory, &
            ierr, message)
        call require(ierr == 0, 'canonical inventory discovery: '//trim(message))
        context%input_inventory = inventory
    end subroutine discover

    logical function file_has(path, fill)
        character(len=*), intent(in) :: path
        character(len=1), intent(in) :: fill
        character(len=PAYLOAD_LEN) :: text
        integer :: unit, ios
        file_has = .false.
        open (newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) text
        close (unit)
        if (ios /= 0) return
        file_has = text(PAYLOAD_LEN:PAYLOAD_LEN) == fill
    end function file_has

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

end program test_gremlin_warm_generation
