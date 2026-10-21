module fo_cache
    !! fo's binding over the shared fx action cache: it owns fo's cache
    !! namespace (FO_CACHE_DIR override, $HOME/.cache/fo default) and re-exports
    !! the action/keying API that lives in fx_action_cache.
    use fx_action_cache, only: cache_t, HASH_LEN, &
        action_cache_root, action_cache_store_root, action_cache_schema, &
        action_cache_init, cache_key_for, fx_cache_lookup => cache_lookup, &
        fx_cache_restore_action => cache_restore_action, &
        fx_cache_store_action => cache_store_action, &
        fx_cache_action_mod_key => cache_action_mod_key, &
        fx_cache_store_binary => cache_store_binary, &
        fx_cache_restore_binary => cache_restore_binary, &
        fx_cache_binary_matches => cache_binary_matches, cache_digest, &
        cache_file_digest, hash_mod_file
    use fx_action_result_store, only: action_result_store_policy
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    character(len=*), parameter :: FO_CACHE_ENV = 'FO_CACHE_DIR'
    character(len=*), parameter :: FO_CACHE_SUBDIR = 'fo'
    logical, save :: policy_set = .false.

    public :: cache_t, HASH_LEN
    public :: cache_init, cache_root, cache_store_root, cache_schema
    public :: cache_key_for, cache_lookup, cache_restore_action, &
        cache_store_action, cache_action_mod_key
    public :: cache_store_binary, cache_restore_binary, cache_binary_matches
    public :: cache_digest, cache_file_digest, hash_mod_file
    public :: cache_env_size

contains

    subroutine cache_root(root)
        !! FO_CACHE_DIR overrides the location; the parallel test runner sets it
        !! per test process so concurrent builds never share one cache. The
        !! default is $HOME/.cache/fo.
        character(len=*), intent(out) :: root

        call action_cache_root(FO_CACHE_ENV, FO_CACHE_SUBDIR, root)
    end subroutine cache_root

    subroutine cache_store_root(root)
        character(len=*), intent(out) :: root

        call action_cache_store_root(FO_CACHE_ENV, FO_CACHE_SUBDIR, root)
    end subroutine cache_store_root

    subroutine cache_schema(schema)
        character(len=*), intent(out) :: schema

        call action_cache_schema(schema)
    end subroutine cache_schema

    subroutine cache_init(c, ierr)
        !! FO_CACHE_MAX_SIZE caps the shared store (default 10G; K/M/G/T
        !! suffixes, 0 disables). FO_CACHE_GRACE (seconds, default 3600)
        !! protects entries used or published that recently from eviction.
        type(cache_t), intent(out) :: c
        integer, intent(out) :: ierr
        integer(int64) :: max_bytes, grace
        logical :: ok

        call action_cache_init(c, FO_CACHE_ENV, FO_CACHE_SUBDIR, ierr)
        !$omp critical (fo_cache_policy)
        if (.not. policy_set) then
            max_bytes = 10_int64 * 1024_int64**3
            grace = 3600_int64
            call cache_env_size('FO_CACHE_MAX_SIZE', max_bytes, ok)
            call cache_env_size('FO_CACHE_GRACE', grace, ok)
            call action_result_store_policy(max_bytes=max_bytes, &
                grace_seconds=grace)
            policy_set = .true.
        end if
        !$omp end critical (fo_cache_policy)
    end subroutine cache_init

    ! GCC 12 emits a shared deferred-result length for Fx's
    ! immutable_store_init -> path_normalize call (fx_immutable_store.f90:118).
    ! Serialize store access at this adapter boundary; compiler processes and
    ! test execution remain parallel. The upstream store stays authoritative.
    function cache_lookup(c, key) result(hit)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        logical :: hit

        !$omp critical (fo_cache_store)
        hit = fx_cache_lookup(c, key)
        !$omp end critical (fo_cache_store)
    end function cache_lookup

    subroutine cache_restore_action(c, action_id, obj_path, mod_dir, restored, &
            output_id, required_mod_names, required_smod_names)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, obj_path, mod_dir
        logical, intent(out) :: restored
        character(len=HASH_LEN), intent(out), optional :: output_id
        character(len=*), intent(in), optional :: required_mod_names(:), &
            required_smod_names(:)

        !$omp critical (fo_cache_store)
        call fx_cache_restore_action(c, action_id, obj_path, mod_dir, restored, &
            output_id, required_mod_names, required_smod_names)
        !$omp end critical (fo_cache_store)
    end subroutine cache_restore_action

    subroutine cache_store_action(c, action_id, obj_path, mod_dir, module_names, &
            output_id, ierr, smod_names)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, obj_path, mod_dir, module_names(:)
        character(len=HASH_LEN), intent(out) :: output_id
        integer, intent(out) :: ierr
        character(len=*), intent(in), optional :: smod_names(:)

        !$omp critical (fo_cache_store)
        call fx_cache_store_action(c, action_id, obj_path, mod_dir, module_names, &
            output_id, ierr, smod_names)
        !$omp end critical (fo_cache_store)
    end subroutine cache_store_action

    subroutine cache_action_mod_key(c, action_id, mod_key, found)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN), intent(out) :: mod_key
        logical, intent(out) :: found

        !$omp critical (fo_cache_store)
        call fx_cache_action_mod_key(c, action_id, mod_key, found)
        !$omp end critical (fo_cache_store)
    end subroutine cache_action_mod_key

    subroutine cache_store_binary(c, action_id, bin_path, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, bin_path
        integer, intent(out) :: ierr

        !$omp critical (fo_cache_store)
        call fx_cache_store_binary(c, action_id, bin_path, ierr)
        !$omp end critical (fo_cache_store)
    end subroutine cache_store_binary

    subroutine cache_restore_binary(c, action_id, dest_path, restored)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, dest_path
        logical, intent(out) :: restored

        !$omp critical (fo_cache_store)
        call fx_cache_restore_binary(c, action_id, dest_path, restored)
        !$omp end critical (fo_cache_store)
    end subroutine cache_restore_binary

    subroutine cache_binary_matches(c, action_id, bin_path, matches)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, bin_path
        logical, intent(out) :: matches

        !$omp critical (fo_cache_store)
        call fx_cache_binary_matches(c, action_id, bin_path, matches)
        !$omp end critical (fo_cache_store)
    end subroutine cache_binary_matches

    subroutine cache_env_size(name, value, ok)
        !! Read a non-negative integer with an optional K/M/G/T (1024-based)
        !! suffix; an unset or malformed variable keeps the default.
        character(len=*), intent(in) :: name
        integer(int64), intent(inout) :: value
        logical, intent(out) :: ok
        character(len=64) :: text
        integer(int64) :: number, scale
        integer :: n, ios

        ok = .false.
        call get_environment_variable(name, text)
        text = adjustl(text)
        n = len_trim(text)
        if (n == 0) return
        scale = 1_int64
        select case (text(n:n))
        case ('K', 'k')
            scale = 1024_int64
        case ('M', 'm')
            scale = 1024_int64**2
        case ('G', 'g')
            scale = 1024_int64**3
        case ('T', 't')
            scale = 1024_int64**4
        end select
        if (scale > 1_int64) n = n - 1
        if (n == 0) return
        if (verify(text(1:n), '0123456789') /= 0) return
        read (text(1:n), *, iostat=ios) number
        if (ios /= 0 .or. number < 0_int64) return
        if (number > huge(number) / scale) return
        value = number * scale
        ok = .true.
    end subroutine cache_env_size

end module fo_cache
