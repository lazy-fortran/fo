program test_cache
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_cache, only: cache_t, cache_init, cache_key_for, cache_lookup, &
                        cache_store, HASH_LEN
    use fo_dag, only: dag_t
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_init()
    call test_store_and_lookup()
    call test_miss()
    call test_update()
    call test_persistence()
    call test_large_file_hashes_full_source()

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
        call assert(len_trim(c%dir) > 0, 'cache dir set')
    end subroutine test_init

    subroutine test_store_and_lookup()
        type(cache_t) :: c
        integer :: ierr

        call cache_init(c, ierr)
        call cache_store(c, 'test_module', 'ABCDEF1234567890')
        call assert(cache_lookup(c, 'test_module', 'ABCDEF1234567890'), &
                    'lookup after store hits')
    end subroutine test_store_and_lookup

    subroutine test_miss()
        type(cache_t) :: c
        integer :: ierr

        call cache_init(c, ierr)
        call assert(.not. cache_lookup(c, 'nonexistent', '0000000000000000'), &
                    'lookup nonexistent misses')
    end subroutine test_miss

    subroutine test_update()
        type(cache_t) :: c
        integer :: ierr

        call cache_init(c, ierr)
        call cache_store(c, 'mod_a', '1111111111111111')
        call cache_store(c, 'mod_a', '2222222222222222')
        call assert(.not. cache_lookup(c, 'mod_a', '1111111111111111'), &
                    'old key misses after update')
        call assert(cache_lookup(c, 'mod_a', '2222222222222222'), &
                    'new key hits after update')
    end subroutine test_update

    subroutine test_persistence()
        type(cache_t) :: c1, c2
        integer :: ierr

        call cache_init(c1, ierr)
        call cache_store(c1, 'persist_mod', 'AAAA111122223333')

        ! reinit loads from disk
        call cache_init(c2, ierr)
        call assert(cache_lookup(c2, 'persist_mod', 'AAAA111122223333'), &
                    'cache persists across init')
    end subroutine test_persistence

    subroutine test_large_file_hashes_full_source()
        type(dag_t) :: dag
        character(len=HASH_LEN) :: key_a, key_b
        character(len=HASH_LEN) :: dep_keys(1)

        call write_large_source('/tmp/fo_cache_large_a.f90', '1')
        call write_large_source('/tmp/fo_cache_large_b.f90', '2')

        key_a = cache_key_for('/tmp/fo_cache_large_a.f90', 'compiler', '', &
                              dag, dep_keys, 0)
        key_b = cache_key_for('/tmp/fo_cache_large_b.f90', 'compiler', '', &
                              dag, dep_keys, 0)

        call assert(key_a /= key_b, &
                    'cache key includes source content after fixed buffer boundary')
        call execute_command_line('rm -f /tmp/fo_cache_large_a.f90 '// &
                                  '/tmp/fo_cache_large_b.f90')
    end subroutine test_large_file_hashes_full_source

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

end program test_cache
