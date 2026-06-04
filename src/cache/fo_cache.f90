module fo_cache
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_scan, only: MAX_NAME
    use fo_dag, only: MAX_NODES
    implicit none
    private
    public :: cache_t, cache_init, cache_lookup, cache_store, cache_key_for
    public :: hash_mod_file, HASH_LEN

    integer, parameter :: HASH_LEN = 16

    type :: cache_entry_t
        character(len=MAX_NAME) :: name = ''
        character(len=HASH_LEN) :: key = ''
        logical :: valid = .false.
    end type cache_entry_t

    type :: cache_t
        character(len=512) :: dir = ''
        integer :: n_entries = 0
        type(cache_entry_t) :: entries(MAX_NODES)
    end type cache_t

contains

    subroutine cache_init(c, ierr)
        type(cache_t), intent(out) :: c
        integer, intent(out) :: ierr

        character(len=512) :: home
        integer :: cmdstat

        ierr = 0
        call get_environment_variable('HOME', home)
        if (len_trim(home) == 0) then
            call get_environment_variable('USERPROFILE', home)
        end if

        c%dir = trim(home)//'/.cache/fo'
        call execute_command_line('mkdir -p '//trim(c%dir), exitstat=ierr, &
                                  cmdstat=cmdstat, wait=.true.)
        if (cmdstat /= 0) ierr = 1

        call load_index(c)
    end subroutine cache_init

    function cache_key_for(filename, compiler, flags, dep_keys, &
                           n_dep_keys) result(key)
        character(len=*), intent(in) :: filename, compiler, flags
        character(len=HASH_LEN), intent(in) :: dep_keys(:)
        integer, intent(in) :: n_dep_keys
        character(len=HASH_LEN) :: key

        integer(int64) :: h
        integer :: i
        ! hash: source + compiler + flags + dependency keys
        h = fnv1a_init()
        call fnv1a_update_file(h, filename)
        h = fnv1a_update(h, trim(compiler))
        h = fnv1a_update(h, trim(flags))
        do i = 1, n_dep_keys
            h = fnv1a_update(h, dep_keys(i))
        end do

        call hash_to_hex(h, key)
    end function cache_key_for

    function cache_lookup(c, name, key) result(hit)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: name, key
        logical :: hit

        integer :: i

        hit = .false.
        do i = 1, c%n_entries
            if (trim(c%entries(i)%name) == trim(name) .and. &
                trim(c%entries(i)%key) == trim(key) .and. &
                c%entries(i)%valid) then
                hit = .true.
                return
            end if
        end do
    end function cache_lookup

    subroutine cache_store(c, name, key)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: name, key

        integer :: i

        ! update existing entry
        do i = 1, c%n_entries
            if (trim(c%entries(i)%name) == trim(name)) then
                c%entries(i)%key = key
                c%entries(i)%valid = .true.
                call save_index(c)
                return
            end if
        end do

        ! add new entry
        if (c%n_entries < MAX_NODES) then
            c%n_entries = c%n_entries + 1
            c%entries(c%n_entries)%name = name
            c%entries(c%n_entries)%key = key
            c%entries(c%n_entries)%valid = .true.
            call save_index(c)
        end if
    end subroutine cache_store

    ! --- FNV-1a hash (64-bit, good distribution, fast) ---

    function fnv1a_init() result(h)
        integer(int64) :: h
        h = -3750763034362895579_int64  ! 14695981039346656037 as signed
    end function fnv1a_init

    function fnv1a_update(h, str) result(h_out)
        integer(int64), intent(in) :: h
        character(len=*), intent(in) :: str
        integer(int64) :: h_out

        integer :: i

        h_out = h
        do i = 1, len_trim(str)
            h_out = ieor(h_out, int(iachar(str(i:i)), int64))
            h_out = h_out*1099511628211_int64
        end do
    end function fnv1a_update

    subroutine hash_to_hex(h, hex)
        integer(int64), intent(in) :: h
        character(len=HASH_LEN), intent(out) :: hex

        write (hex, '(z16.16)') h
    end subroutine hash_to_hex

    subroutine hash_mod_file(modpath, key)
        character(len=*), intent(in) :: modpath
        character(len=HASH_LEN), intent(out) :: key

        integer(int64) :: h

        h = fnv1a_init()
        call fnv1a_update_file(h, modpath)
        call hash_to_hex(h, key)
    end subroutine hash_mod_file

    subroutine fnv1a_update_file(h, filename)
        integer(int64), intent(inout) :: h
        character(len=*), intent(in) :: filename

        integer :: u, iostat
        character(len=4096) :: line

        open (newunit=u, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            h = fnv1a_update(h, trim(line))
            h = fnv1a_update(h, char(10))
        end do
        close (u)
    end subroutine fnv1a_update_file

    ! --- index persistence ---

    subroutine load_index(c)
        type(cache_t), intent(inout) :: c

        character(len=512) :: indexfile, line
        integer :: u, iostat
        logical :: exists

        indexfile = trim(c%dir)//'/index'
        inquire (file=indexfile, exist=exists)
        if (.not. exists) return

        open (newunit=u, file=indexfile, status='old', iostat=iostat)
        if (iostat /= 0) return

        c%n_entries = 0
        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle
            c%n_entries = c%n_entries + 1
            if (c%n_entries > MAX_NODES) then
                c%n_entries = MAX_NODES
                exit
            end if
            call parse_index_line(line, c%entries(c%n_entries))
        end do
        close (u)
    end subroutine load_index

    subroutine save_index(c)
        type(cache_t), intent(in) :: c

        character(len=512) :: indexfile
        integer :: u, i

        indexfile = trim(c%dir)//'/index'
        open (newunit=u, file=indexfile, status='replace')
        do i = 1, c%n_entries
            if (c%entries(i)%valid) then
                write (u, '(a,1x,a)') trim(c%entries(i)%key), trim(c%entries(i)%name)
            end if
        end do
        close (u)
    end subroutine save_index

    subroutine parse_index_line(line, entry)
        character(len=*), intent(in) :: line
        type(cache_entry_t), intent(out) :: entry

        integer :: sp

        sp = index(line, ' ')
        if (sp > 0 .and. sp <= HASH_LEN + 1) then
            entry%key = line(1:sp - 1)
            entry%name = adjustl(line(sp + 1:))
            entry%valid = .true.
        end if
    end subroutine parse_index_line

end module fo_cache
