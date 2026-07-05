module fo_cache
    !! fo's binding over the shared fx action cache: it owns fo's cache
    !! namespace (FO_CACHE_DIR override, $HOME/.cache/fo default) and re-exports
    !! the action/keying API that lives in fx_action_cache.
    use fx_action_cache, only: cache_t, HASH_LEN, &
        action_cache_root, action_cache_store_root, action_cache_schema, &
        action_cache_init, cache_key_for, cache_lookup, cache_restore_action, &
        cache_store_action, cache_action_mod_key, cache_store_binary, &
        cache_restore_binary, cache_binary_matches, cache_digest, &
        cache_file_digest, hash_mod_file, cache_debug_write_action_record, &
        cache_debug_corrupt_object_payload
    implicit none
    private

    character(len=*), parameter :: FO_CACHE_ENV = 'FO_CACHE_DIR'
    character(len=*), parameter :: FO_CACHE_SUBDIR = 'fo'

    public :: cache_t, HASH_LEN
    public :: cache_init, cache_root, cache_store_root, cache_schema
    public :: cache_key_for, cache_lookup, cache_restore_action, &
        cache_store_action, cache_action_mod_key
    public :: cache_store_binary, cache_restore_binary, cache_binary_matches
    public :: cache_digest, cache_file_digest, hash_mod_file
    public :: cache_debug_write_action_record, cache_debug_corrupt_object_payload

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
        type(cache_t), intent(out) :: c
        integer, intent(out) :: ierr

        call action_cache_init(c, FO_CACHE_ENV, FO_CACHE_SUBDIR, ierr)
    end subroutine cache_init

end module fo_cache
