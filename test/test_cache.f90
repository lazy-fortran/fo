program test_cache
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_cache, only: cache_t, cache_init, cache_key_for, cache_lookup, &
                        cache_store_action, cache_restore_action, cache_schema, &
                        cache_store_root, cache_debug_write_action_record, &
                        cache_debug_corrupt_object_payload, HASH_LEN
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_init()
    call test_action_store_restore()
    call test_miss()
    call test_action_persistence()
    call test_corrupt_payload_misses()
    call test_stale_action_record_misses()
    call test_wrong_schema_misses()
    call test_partial_temp_ignored()
    call test_large_file_hashes_full_source()
    call test_schema_and_root()

    write (output_unit, '(a,i0,a,i0,a)') 'cache: ', n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

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
        call execute_command_line('mkdir -p '//trim(mod_dir), wait=.true.)
        mod_path = trim(mod_dir)//'/m.mod'
        call write_text(obj_path, 'object payload')
        call write_text(mod_path, 'module payload')

        action_id = repeat('a', HASH_LEN)
        call cache_store_action(c, action_id, obj_path, mod_dir, 'm', output_id, ierr)
        call assert(ierr == 0, 'store action succeeds')
        call assert(cache_lookup(c, action_id), 'action lookup hits')

        call execute_command_line('rm -f '//trim(obj_path)//' '//trim(mod_path), &
                                  wait=.true.)
        call cache_restore_action(c, action_id, obj_path, mod_dir, restored)
        call assert(restored, 'action restore reports success')
        call assert(file_contains(obj_path, 'object payload'), &
                    'object restored from action cache')
        call assert(file_contains(mod_path, 'module payload'), &
                    'module restored from action cache')

        call execute_command_line('rm -f '//trim(obj_path)//' '//trim(mod_path), &
                                  wait=.true.)
        call execute_command_line('rm -rf '//trim(mod_dir), wait=.true.)
    end subroutine test_action_store_restore

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
        call execute_command_line('mkdir -p '//trim(mod_dir), wait=.true.)
        call write_text(obj_path, 'persist object')
        call write_text(trim(mod_dir)//'/persist_mod.mod', 'persist mod')
        action_id = repeat('b', HASH_LEN)
        call cache_store_action(c, action_id, obj_path, mod_dir, 'persist_mod', &
                                output_id, ierr)

        call cache_init(c, ierr)
        call assert(cache_lookup(c, action_id), &
                    'action cache persists across init')
        call execute_command_line('rm -f '//trim(obj_path), wait=.true.)
        call execute_command_line('rm -rf '//trim(mod_dir), wait=.true.)
    end subroutine test_action_persistence

    subroutine test_corrupt_payload_misses()
        type(cache_t) :: c
        integer :: ierr
        character(len=512) :: obj_path, mod_dir
        character(len=HASH_LEN) :: action_id, output_id
        logical :: restored

        call cache_init(c, ierr)
        call make_tmp_path('fo_cache_corrupt_obj', obj_path, '.o')
        call make_tmp_path('fo_cache_corrupt_moddir', mod_dir, '')
        call execute_command_line('mkdir -p '//trim(mod_dir), wait=.true.)
        call write_text(obj_path, 'valid object')
        call write_text(trim(mod_dir)//'/badmod.mod', 'valid mod')
        action_id = repeat('c', HASH_LEN)
        call cache_store_action(c, action_id, obj_path, mod_dir, 'badmod', &
                                output_id, ierr)
        call cache_debug_corrupt_object_payload(c, action_id, ierr)
        call assert(ierr == 0, 'debug corrupt object payload succeeds')
        call execute_command_line('rm -f '//trim(obj_path)//' '// &
                                  trim(mod_dir)//'/badmod.mod', wait=.true.)
        call cache_restore_action(c, action_id, obj_path, mod_dir, restored)
        call assert(.not. restored, 'corrupt payload does not restore as hit')

        call execute_command_line('rm -f '//trim(obj_path), wait=.true.)
        call execute_command_line('rm -rf '//trim(mod_dir), wait=.true.)
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
        call execute_command_line('mkdir -p '//trim(shard), wait=.true.)
        temp_path = trim(shard)//'/.tmp.'//trim(action_id)//'-a'
        call write_text(temp_path, 'partial action')
        call assert(.not. cache_lookup(c, action_id), &
                    'partial temp file is ignored by lookup')
        call execute_command_line('rm -f '//trim(temp_path), wait=.true.)
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
        call execute_command_line('rm -f '//trim(path_a)//' '//trim(path_b))
    end subroutine test_large_file_hashes_full_source

    subroutine test_schema_and_root()
        character(len=512) :: text

        call cache_schema(text)
        call assert(trim(text) == 'action-output-v1', 'cache schema is reported')
        call cache_store_root(text)
        call assert(index(text, '/.cache/fo/store/v1') > 0, &
                    'cache root points at store v1')
    end subroutine test_schema_and_root

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: u

        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') trim(text)
        close (u)
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

        integer :: u, i

        open (newunit=u, file=filename, status='replace')
        write (u, '(a)') 'module fo_cache_large'
        write (u, '(a)') 'contains'
        do i = 1, 180
            write (u, '(a,i0,a)') 'subroutine filler_', i, '()'
            write (u, '(a)') 'end subroutine'
        end do
        write (u, '(a)') 'integer, parameter :: marker = '//marker
        write (u, '(a)') 'end module fo_cache_large'
        close (u)
    end subroutine write_large_source

    subroutine make_tmp_path(prefix, path, suffix)
        character(len=*), intent(in) :: prefix, suffix
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a)') '/tmp/', trim(prefix), '-', &
            count, '-', serial, trim(suffix)
    end subroutine make_tmp_path

end program test_cache
