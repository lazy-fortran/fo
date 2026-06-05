module fo_cache
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_cache, only: cache_t, fx_cache_init => cache_init, &
                        fx_cache_has => cache_has, &
                        fx_cache_store_bytes => cache_store_bytes, &
                        fx_cache_key => cache_key
    use fx_hash, only: fnv1a_file, hash_to_hex
    implicit none
    private
    public :: cache_t, cache_init, cache_lookup, cache_store, cache_key_for
    public :: hash_mod_file, HASH_LEN

    integer, parameter :: HASH_LEN = 64
    integer, parameter :: MAX_PARTS = 132

contains

    subroutine cache_init(c, ierr)
        type(cache_t), intent(out) :: c
        integer, intent(out) :: ierr

        character(len=512) :: home

        ierr = 0
        call get_environment_variable('HOME', home)
        if (len_trim(home) == 0) call get_environment_variable('USERPROFILE', home)
        call fx_cache_init(c, trim(home)//'/.cache/fo/modules')
        if (.not. c%initialized) ierr = 1
    end subroutine cache_init

    function cache_key_for(filename, compiler, flags, dep_keys, &
                           n_dep_keys) result(key)
        character(len=*), intent(in) :: filename, compiler, flags
        character(len=HASH_LEN), intent(in) :: dep_keys(:)
        integer, intent(in) :: n_dep_keys
        character(len=HASH_LEN) :: key

        integer(int64) :: h
        character(len=16) :: file_hash
        integer :: i, n_parts, ierr
        character(len=HASH_LEN) :: parts(MAX_PARTS)

        call fnv1a_file(filename, h, ierr)
        file_hash = hash_to_hex(h)

        parts(1) = file_hash
        parts(2) = trim(compiler)
        parts(3) = trim(flags)
        n_parts = 3
        do i = 1, min(n_dep_keys, MAX_PARTS - 3)
            n_parts = n_parts + 1
            parts(n_parts) = dep_keys(i)
        end do
        key = fx_cache_key(parts, n_parts)
    end function cache_key_for

    function cache_lookup(c, name, key) result(hit)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: name, key
        logical :: hit

        if (len_trim(name) < 0) return  ! name reserved; content hash is sufficient
        hit = fx_cache_has(c, trim(key))
    end function cache_lookup

    subroutine cache_store(c, name, key)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: name, key

        character(len=1) :: marker(1)
        integer :: ierr

        if (len_trim(name) < 0) return  ! name reserved; content hash is sufficient
        marker(1) = '0'
        call fx_cache_store_bytes(c, trim(key), marker, 1, ierr)
    end subroutine cache_store

    subroutine hash_mod_file(modpath, key)
        character(len=*), intent(in) :: modpath
        character(len=HASH_LEN), intent(out) :: key

        integer(int64) :: h
        character(len=16) :: hex
        character(len=16) :: parts(1)
        integer :: ierr

        call fnv1a_file(modpath, h, ierr)
        if (ierr /= 0) h = 0_int64
        hex = hash_to_hex(h)
        parts(1) = hex
        key = fx_cache_key(parts, 1)
    end subroutine hash_mod_file

end module fo_cache
