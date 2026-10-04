program test_cache
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_cache, only: cache_t, cache_init, cache_key_for, cache_lookup, &
        cache_store_action, cache_restore_action, cache_schema, &
        cache_store_root, cache_debug_write_action_record, &
        HASH_LEN
    use fo_process, only: process_getpid
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree, fs_write_text
    use fx_immutable_store, only: immutable_store_t, immutable_store_init, &
        immutable_store_hash_file, immutable_store_blob_path
    implicit none

    integer :: n_pass, n_fail
    character(len=512) :: test_cache_root, shared_sentinel_path
    character(len=:), allocatable :: prior_cache_override
    character(len=22), parameter :: SENTINEL_TEXT = 'outside-cache-sentinel'
    logical :: had_cache_override

    interface
        integer(c_int) function c_setenv(name, value, overwrite) &
                bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv

        integer(c_int) function c_unsetenv(name) bind(C, name='unsetenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
        end function c_unsetenv

        integer(c_int) function c_chmod(path, mode) bind(C, name='chmod')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_chmod
    end interface

    n_pass = 0
    n_fail = 0
    call begin_test_cache_scope()

    call test_init()
    call test_action_store_restore()
    call test_parallel_cache_lookup()
    call test_miss()
    call test_action_persistence()
    call test_corrupt_payload_misses()
    call test_stale_action_record_misses()
    call test_wrong_schema_misses()
    call test_partial_temp_ignored()
    call test_large_file_hashes_full_source()
    call test_schema_and_root()
    call test_distinct_roots_are_isolated()
    call end_test_cache_scope()

    write (output_unit, '(a,i0,a,i0,a)') 'cache: ', n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine begin_test_cache_scope()
        integer :: env_len, env_status, rc
        logical :: exists

        call get_environment_variable('FO_CACHE_DIR', length=env_len, &
            status=env_status)
        had_cache_override = env_status == 0
        if (had_cache_override) then
            allocate (character(len=env_len) :: prior_cache_override)
            call get_environment_variable('FO_CACHE_DIR', prior_cache_override)
        end if

        call make_tmp_path('fo_cache_test_root', test_cache_root, '')
        ! Install the per-process override before any cache_init call. If a
        ! runner interrupts this executable, only this unique /var/tmp root can
        ! be left behind; the caller's cache and default CAS stay untouched.
        call fs_remove_tree(test_cache_root)
        rc = c_setenv('FO_CACHE_DIR'//c_null_char, &
            trim(test_cache_root)//c_null_char, 1_c_int)
        if (rc /= 0) error stop 'cannot install isolated FO_CACHE_DIR'
        call fs_make_dir(test_cache_root)
        inquire (file=trim(test_cache_root), exist=exists)
        if (.not. exists) then
            call restore_parent_cache_environment()
            error stop 'cannot create isolated cache root'
        end if

        ! Keep the sentinel outside the active store, but inside this owned
        ! root so interruption can leave no residue in a shared cache.
        shared_sentinel_path = trim(test_cache_root)//'/.outside-cache-sentinel'
        call fs_write_text(shared_sentinel_path, SENTINEL_TEXT)
        inquire (file=trim(shared_sentinel_path), exist=exists)
        if (.not. exists .or. .not. file_contains(shared_sentinel_path, &
            SENTINEL_TEXT)) then
            call restore_parent_cache_environment()
            call fs_remove_file(shared_sentinel_path)
            call fs_remove_tree(test_cache_root)
            error stop 'cannot establish shared cache sentinel'
        end if
    end subroutine begin_test_cache_scope

    subroutine end_test_cache_scope()
        logical :: exists

        call assert(file_contains(shared_sentinel_path, SENTINEL_TEXT), &
            'unrelated shared cache sentinel is unchanged')
        call restore_parent_cache_environment()
        call fs_remove_file(shared_sentinel_path)
        inquire (file=trim(shared_sentinel_path), exist=exists)
        call assert(.not. exists, 'shared cache sentinel cleanup succeeds')
        call fs_remove_tree(test_cache_root)
        inquire (file=trim(test_cache_root), exist=exists)
        call assert(.not. exists, 'isolated cache root cleanup succeeds')
    end subroutine end_test_cache_scope

    subroutine restore_parent_cache_environment()
        integer :: rc, env_len, env_status
        character(len=:), allocatable :: observed_override

        if (had_cache_override) then
            rc = c_setenv('FO_CACHE_DIR'//c_null_char, &
                trim(prior_cache_override)//c_null_char, 1_c_int)
        else
            rc = c_unsetenv('FO_CACHE_DIR'//c_null_char)
        end if
        if (rc /= 0) error stop 'cannot restore original FO_CACHE_DIR'
        call get_environment_variable('FO_CACHE_DIR', length=env_len, &
            status=env_status)
        if (had_cache_override) then
            call assert(env_status == 0, 'original FO_CACHE_DIR is restored')
            if (env_status == 0) then
                allocate (character(len=env_len) :: observed_override)
                call get_environment_variable('FO_CACHE_DIR', observed_override)
                call assert(observed_override == prior_cache_override, &
                    'original FO_CACHE_DIR value is preserved')
            end if
        else
            call assert(env_status /= 0, 'originally unset FO_CACHE_DIR stays unset')
        end if
    end subroutine restore_parent_cache_environment

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_init()
        type(cache_t) :: c
        integer :: ierr

        call cache_init(c, ierr)
        call assert(ierr == 0, 'cache init succeeds')
        call assert(c%initialized, 'cache dir set')
    end subroutine test_init

    subroutine test_action_store_restore()
        type(cache_t) :: c
        integer :: ierr
        character(len=512) :: obj_path, mod_dir, mod_path
        character(len=HASH_LEN) :: action_id, output_id
        logical :: restored

        call cache_init(c, ierr)
        call make_tmp_path('fo_cache_obj', obj_path, '.o')
        call make_tmp_path('fo_cache_moddir', mod_dir, '')
        call fs_make_dir(trim(mod_dir))
        mod_path = trim(mod_dir)//'/m.mod'
        call write_text(obj_path, 'object payload')
        call write_text(mod_path, 'module payload')

        action_id = repeat('a', HASH_LEN)
        call cache_store_action(c, action_id, obj_path, mod_dir, 'm', output_id, ierr)
        call assert(ierr == 0, 'store action succeeds')
        call assert(cache_lookup(c, action_id), 'action lookup hits')

        call fs_remove_file(trim(obj_path))
        call fs_remove_file(trim(mod_path))
        call cache_restore_action(c, action_id, obj_path, mod_dir, restored)
        call assert(restored, 'action restore reports success')
        call assert(file_contains(obj_path, 'object payload'), &
            'object restored from action cache')
        call assert(file_contains(mod_path, 'module payload'), &
            'module restored from action cache')

        call fs_remove_file(trim(obj_path))
        call fs_remove_file(trim(mod_path))
        call fs_remove_tree(trim(mod_dir))
    end subroutine test_action_store_restore

    subroutine test_parallel_cache_lookup()
        type(cache_t) :: c
        integer :: ierr, i
        character(len=512) :: root, obj_path, mod_dir, mod_path
        character(len=HASH_LEN) :: action_id, output_id
        logical, allocatable :: hits(:)

        call make_tmp_path('fo_cache_parallel', root, '')
        call fs_remove_tree(trim(root))
        call fs_make_dir(trim(root))
        call cache_init(c, ierr)
        call assert(ierr == 0, 'parallel cache init')

        obj_path = trim(root)//'/payload.o'
        mod_dir = trim(root)//'/mods'
        mod_path = trim(mod_dir)//'/m.mod'
        call fs_make_dir(trim(mod_dir))
        call write_text(obj_path, 'object payload')
        call write_text(mod_path, 'module payload')

        action_id = repeat('f', HASH_LEN)
        call cache_store_action(c, action_id, obj_path, mod_dir, 'm', output_id, &
            ierr)
        call assert(ierr == 0, 'parallel cache store')

        allocate (hits(64))
        hits = .false.
        !$omp parallel do schedule(dynamic) num_threads(8) private(i)
        do i = 1, size(hits)
            hits(i) = cache_lookup(c, action_id)
        end do
        !$omp end parallel do

        call assert(all(hits), 'parallel cache lookup hits')
        call fs_remove_tree(trim(root))
    end subroutine test_parallel_cache_lookup

    subroutine test_miss()
        type(cache_t) :: c
        integer :: ierr

        call cache_init(c, ierr)
        call assert(.not. cache_lookup(c, '0000000000000000'), &
            'lookup nonexistent misses')
    end subroutine test_miss

    subroutine test_action_persistence()
        type(cache_t) :: c
        integer :: ierr
        character(len=512) :: obj_path, mod_dir
        character(len=HASH_LEN) :: action_id, output_id

        call cache_init(c, ierr)
        call make_tmp_path('fo_cache_persist_obj', obj_path, '.o')
        call make_tmp_path('fo_cache_persist_moddir', mod_dir, '')
        call fs_make_dir(trim(mod_dir))
        call write_text(obj_path, 'persist object')
        call write_text(trim(mod_dir)//'/persist_mod.mod', 'persist mod')
        action_id = repeat('b', HASH_LEN)
        call cache_store_action(c, action_id, obj_path, mod_dir, 'persist_mod', &
            output_id, ierr)

        call cache_init(c, ierr)
        call assert(cache_lookup(c, action_id), &
            'action cache persists across init')
        call fs_remove_file(trim(obj_path))
        call fs_remove_tree(trim(mod_dir))
    end subroutine test_action_persistence

    subroutine test_corrupt_payload_misses()
        type(cache_t) :: c
        type(immutable_store_t) :: store
        integer :: ierr, file_status
        character(len=512) :: obj_path, mod_dir, store_root
        character(len=HASH_LEN) :: action_id, output_id
        character(len=HASH_LEN) :: object_id, recovered_id
        character(len=:), allocatable :: payload_path
        logical :: restored

        call cache_init(c, ierr)
        call make_tmp_path('fo_cache_corrupt_obj', obj_path, '.o')
        call make_tmp_path('fo_cache_corrupt_moddir', mod_dir, '')
        call fs_make_dir(trim(mod_dir))
        call write_text(obj_path, 'valid object')
        call write_text(trim(mod_dir)//'/badmod.mod', 'valid mod')
        action_id = repeat('c', HASH_LEN)
        call cache_store_action(c, action_id, obj_path, mod_dir, 'badmod', &
            output_id, ierr)
        call cache_store_root(store_root)
        call immutable_store_init(store, trim(store_root), ierr)
        call assert(ierr == 0, 'initialize active immutable cache store')
        call immutable_store_hash_file(trim(obj_path), object_id, ierr)
        call assert(ierr == 0, 'hash active object payload')
        payload_path = immutable_store_blob_path(store, object_id)
        call assert(index(payload_path, trim(test_cache_root)//'/') == 1, &
            'corruption target stays inside the owned test cache')
        file_status = c_chmod(payload_path//c_null_char, 384_c_int)
        call assert(file_status == 0, 'make immutable payload writable for corruption')
        if (file_status == 0) then
            call write_text(payload_path, 'corrupt payload')
            file_status = c_chmod(payload_path//c_null_char, 292_c_int)
            call assert(file_status == 0, 'restore immutable payload permissions')
        end if
        call assert(.not. cache_lookup(c, action_id), &
            'corrupt immutable payload does not restore as a hit')
        call fs_remove_file(trim(obj_path))
        call fs_remove_file(trim(mod_dir)//'/badmod.mod')
        call cache_restore_action(c, action_id, obj_path, mod_dir, restored)
        call assert(.not. restored, 'restore rejects corrupt immutable payload')

        call fs_remove_file(payload_path)
        call write_text(obj_path, 'valid object')
        call write_text(trim(mod_dir)//'/badmod.mod', 'valid mod')
        call cache_store_action(c, action_id, obj_path, mod_dir, 'badmod', &
            output_id, ierr)
        call assert(ierr == 0, 'valid action republishes its missing payload')
        call fs_remove_file(trim(obj_path))
        call fs_remove_file(trim(mod_dir)//'/badmod.mod')
        call cache_restore_action(c, action_id, obj_path, mod_dir, restored)
        call assert(restored, 'repaired immutable payload restores successfully')
        call assert(file_contains(obj_path, 'valid object'), &
            'repaired object bytes are restored')
        call assert(file_contains(trim(mod_dir)//'/badmod.mod', 'valid mod'), &
            'repaired module bytes are restored')
        call immutable_store_hash_file(trim(obj_path), recovered_id, ierr)
        call assert(ierr == 0 .and. recovered_id == object_id, &
            'repaired payload digest matches its original identity')

        call fs_remove_file(trim(obj_path))
        call fs_remove_tree(trim(mod_dir))
    end subroutine test_corrupt_payload_misses

    subroutine test_stale_action_record_misses()
        type(cache_t) :: c
        integer :: ierr
        character(len=HASH_LEN) :: action_id
        character(len=512) :: record

        call cache_init(c, ierr)
        action_id = repeat('d', HASH_LEN)
        record = 'schema 1'//achar(10)//'kind compile'//achar(10)// &
            'output '//repeat('1', HASH_LEN)//achar(10)// &
            'object '//repeat('2', HASH_LEN)//' 1'//achar(10)
        call cache_debug_write_action_record(c, action_id, record, ierr)
        call assert(ierr == 0, 'debug stale action write succeeds')
        call assert(.not. cache_lookup(c, action_id), &
            'stale action record with missing payload misses')
    end subroutine test_stale_action_record_misses

    subroutine test_wrong_schema_misses()
        type(cache_t) :: c
        integer :: ierr
        character(len=HASH_LEN) :: action_id
        character(len=512) :: record

        call cache_init(c, ierr)
        action_id = repeat('e', HASH_LEN)
        record = 'schema 999'//achar(10)//'kind compile'//achar(10)// &
            'output '//repeat('3', HASH_LEN)//achar(10)// &
            'object '//repeat('4', HASH_LEN)//' 1'//achar(10)
        call cache_debug_write_action_record(c, action_id, record, ierr)
        call assert(ierr == 0, 'debug wrong schema write succeeds')
        call assert(.not. cache_lookup(c, action_id), &
            'wrong schema action record misses')
    end subroutine test_wrong_schema_misses

    subroutine test_partial_temp_ignored()
        type(cache_t) :: c
        integer :: ierr
        character(len=512) :: root, shard, temp_path
        character(len=HASH_LEN) :: action_id

        call cache_init(c, ierr)
        action_id = 'fa'//repeat('0', HASH_LEN - 2)
        call cache_store_root(root)
        shard = trim(root)//'/fa'
        call fs_make_dir(trim(shard))
        temp_path = trim(shard)//'/.tmp.'//trim(action_id)//'-a'
        call write_text(temp_path, 'partial action')
        call assert(.not. cache_lookup(c, action_id), &
            'partial temp file is ignored by lookup')
        call fs_remove_file(trim(temp_path))
    end subroutine test_partial_temp_ignored

    subroutine test_large_file_hashes_full_source()
        character(len=HASH_LEN) :: key_a, key_b
        character(len=HASH_LEN) :: dep_keys(1)
        character(len=512) :: path_a, path_b

        call make_tmp_path('fo_cache_large_a', path_a, '.f90')
        call make_tmp_path('fo_cache_large_b', path_b, '.f90')
        call write_large_source(path_a, '1')
        call write_large_source(path_b, '2')

        key_a = cache_key_for(path_a, 'compiler', '', dep_keys, 0)
        key_b = cache_key_for(path_b, 'compiler', '', dep_keys, 0)

        call assert(key_a /= key_b, &
            'cache key includes source content after fixed buffer boundary')
        call fs_remove_file(trim(path_a))
        call fs_remove_file(trim(path_b))
    end subroutine test_large_file_hashes_full_source

    subroutine test_schema_and_root()
        character(len=512) :: text, override

        call cache_schema(text)
        call assert(trim(text) == 'action-output-v2', 'cache schema is reported')
        call cache_store_root(text)
        ! FO_CACHE_DIR overrides the immutable action-result store root; the
        ! legacy action-record store remains under store/v1.
        call get_environment_variable('FO_CACHE_DIR', override)
        if (len_trim(override) > 0) then
            call assert(trim(text) == trim(override)//'/store/v2', &
                'FO_CACHE_DIR override points the store root at it')
        else
            call assert(index(text, '/.cache/fo/store/v2') > 0, &
                'default cache root points at store v2')
        end if
    end subroutine test_schema_and_root

    subroutine test_distinct_roots_are_isolated()
        type(cache_t) :: cache_a, cache_b
        integer :: ierr, env_len, env_status, rc
        character(len=512) :: root_a, root_b, observed_root
        character(len=512) :: object_a, object_b, mods_a, mods_b
        character(len=HASH_LEN) :: action_id, output_id
        character(len=:), allocatable :: prior_override
        logical :: had_override, restored

        call get_environment_variable('FO_CACHE_DIR', length=env_len, &
            status=env_status)
        had_override = env_status == 0
        if (had_override) then
            allocate (character(len=env_len) :: prior_override)
            call get_environment_variable('FO_CACHE_DIR', prior_override)
        end if
        call make_tmp_path('fo_cache_root_a', root_a, '')
        call make_tmp_path('fo_cache_root_b', root_b, '')
        call make_tmp_path('fo_cache_root_obj_a', object_a, '.o')
        call make_tmp_path('fo_cache_root_obj_b', object_b, '.o')
        call make_tmp_path('fo_cache_root_mod_a', mods_a, '')
        call make_tmp_path('fo_cache_root_mod_b', mods_b, '')
        action_id = repeat('f', HASH_LEN)
        call fs_remove_tree(trim(root_a))
        call fs_remove_tree(trim(root_b))

        rc = c_setenv('FO_CACHE_DIR'//c_null_char, trim(root_a)//c_null_char, 1_c_int)
        call assert(rc == 0, 'set first isolated cache root')
        call cache_init(cache_a, ierr)
        call assert(ierr == 0, 'initialize first isolated cache root')
        call fs_make_dir(trim(mods_a))
        call write_text(object_a, 'root a object')
        call write_text(trim(mods_a)//'/root_mod.mod', 'root a module')
        call cache_store_action(cache_a, action_id, object_a, mods_a, 'root_mod', &
            output_id, ierr)
        call assert(ierr == 0, 'store first root action')

        rc = c_setenv('FO_CACHE_DIR'//c_null_char, trim(root_b)//c_null_char, 1_c_int)
        call assert(rc == 0, 'switch to second isolated cache root')
        call cache_store_root(observed_root)
        call assert(trim(observed_root) == trim(root_b)//'/store/v2', &
            'cache store root follows the current environment')
        call cache_init(cache_b, ierr)
        call assert(ierr == 0, 'initialize second isolated cache root')
        call fs_make_dir(trim(mods_b))
        call write_text(object_b, 'root b object')
        call write_text(trim(mods_b)//'/root_mod.mod', 'root b module')
        call cache_store_action(cache_b, action_id, object_b, mods_b, 'root_mod', &
            output_id, ierr)
        call assert(ierr == 0, 'store same action independently in second root')
        call assert(cache_lookup(cache_a, action_id), 'first root retains its action')
        call assert(cache_lookup(cache_b, action_id), 'second root retains its action')

        call fs_remove_file(trim(object_a))
        call fs_remove_file(trim(object_b))
        call fs_remove_file(trim(mods_a)//'/root_mod.mod')
        call fs_remove_file(trim(mods_b)//'/root_mod.mod')
        call cache_restore_action(cache_a, action_id, object_a, mods_a, restored)
        call assert(restored, 'first root restores independently')
        call assert(file_contains(object_a, 'root a object'), &
            'first root never reads second root payload')
        call cache_restore_action(cache_b, action_id, object_b, mods_b, restored)
        call assert(restored, 'second root restores independently')
        call assert(file_contains(object_b, 'root b object'), &
            'second root never reads first root payload')

        if (had_override) then
            rc = c_setenv('FO_CACHE_DIR'//c_null_char, &
                trim(prior_override)//c_null_char, 1_c_int)
        else
            rc = c_unsetenv('FO_CACHE_DIR'//c_null_char)
        end if
        call assert(rc == 0, 'restore original FO_CACHE_DIR environment')
        call fs_remove_file(trim(object_a))
        call fs_remove_file(trim(object_b))
        call fs_remove_tree(trim(mods_a))
        call fs_remove_tree(trim(mods_b))
        call fs_remove_tree(trim(root_a))
        call fs_remove_tree(trim(root_b))
    end subroutine test_distinct_roots_are_isolated

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: u, ios, close_status

        open (newunit=u, file=trim(path), status='replace', iostat=ios)
        if (ios /= 0) then
            call assert(.false., 'open test cache fixture for writing')
            return
        end if
        write (u, '(a)', iostat=ios) trim(text)
        close (u, iostat=close_status)
        call assert(ios == 0 .and. close_status == 0, &
            'write test cache fixture')
    end subroutine write_text

    logical function file_contains(path, needle)
        character(len=*), intent(in) :: path, needle
        character(len=512) :: line
        integer :: u, iostat

        file_contains = .false.
        open (newunit=u, file=trim(path), status='old', iostat=iostat)
        if (iostat /= 0) return
        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, trim(needle)) > 0) then
                file_contains = .true.
                exit
            end if
        end do
        close (u)
    end function file_contains

    subroutine write_large_source(filename, marker)
        character(len=*), intent(in) :: filename, marker

        integer :: u, i, ios, close_status

        open (newunit=u, file=filename, status='replace', iostat=ios)
        if (ios /= 0) then
            call assert(.false., 'open large source fixture for writing')
            return
        end if
        write (u, '(a)', iostat=ios) 'module fo_cache_large'
        if (ios == 0) write (u, '(a)', iostat=ios) 'contains'
        do i = 1, 180
            if (ios /= 0) exit
            write (u, '(a,i0,a)', iostat=ios) 'subroutine filler_', i, '()'
            if (ios == 0) write (u, '(a)', iostat=ios) 'end subroutine'
        end do
        if (ios == 0) write (u, '(a)', iostat=ios) &
            'integer, parameter :: marker = '//marker
        if (ios == 0) write (u, '(a)', iostat=ios) 'end module fo_cache_large'
        close (u, iostat=close_status)
        call assert(ios == 0 .and. close_status == 0, &
            'write large source fixture')
    end subroutine write_large_source

    subroutine make_tmp_path(prefix, path, suffix)
        character(len=*), intent(in) :: prefix, suffix
        character(len=*), intent(out) :: path

        integer :: count, pid
        integer, save :: serial = 0

        serial = serial + 1
        pid = process_getpid()
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a,i0,a)') '/var/tmp/', trim(prefix), '-', &
            pid, '-', count, '-', serial, trim(suffix)
    end subroutine make_tmp_path

end program test_cache
