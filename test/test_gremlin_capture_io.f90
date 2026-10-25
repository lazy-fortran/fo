! A generation capture reads each input payload at most twice (once to copy it
! into the store while hashing, once when the inventory is revalidated) and
! writes it once. Earlier captures read every new payload about nine times,
! which stalled Gremlin generations on large CMake dependency trees.
program test_gremlin_capture_io
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use fo_util, only: temporary_root
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_fs, only: fs_make_dir, fs_write_text, fs_remove_tree
    use fo_process, only: process_getpid
    use fo_input_inventory, only: input_inventory_t, &
        input_inventory_discover_cmake, INPUT_FILE
    use fo_generation_manifest, only: generation_manifest_metadata_t, &
        generation_manifest_capture, generation_manifest_execution_identity
    implicit none

    integer, parameter :: N_SMALL = 40, N_LARGE = 4
    integer, parameter :: LARGE_BYTES = 2 * 1024 * 1024
    ! Generation manifest, object tree and lease rows for ~50 entries.
    integer(int64), parameter :: METADATA_ALLOWANCE = 256 * 1024
    type(input_inventory_t) :: inventory
    type(generation_manifest_metadata_t) :: metadata
    character(len=:), allocatable :: root, project, deps, meta, store
    character(len=1024) :: message
    character(len=HASH_LEN) :: manifest_id, owner
    character(len=4096) :: sources(1), bundles(1), labels(1)
    character(len=32) :: number
    integer(int64) :: payload, read0, read1, write0, write1, size_bytes
    integer :: ierr, i, pass

    write (number, '(i0)') process_getpid()
    root = temporary_root()//'/fo-capture-io-'//trim(number)
    call fs_remove_tree(root, ierr)
    project = root//'/project'
    deps = project//'/build/_deps/big-src'
    meta = root//'/meta'
    store = root//'/store'
    call fs_make_dir(project//'/src')
    call fs_make_dir(deps)
    call fs_make_dir(meta)
    call fs_write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.20)'//new_line('a')// &
        'project(capture_io Fortran)'//new_line('a'))
    do i = 1, N_SMALL
        write (number, '(i0)') i
        call fs_write_text(project//'/src/m'//trim(number)//'.f90', &
            'module m'//trim(number)//new_line('a')//repeat('!', 64 * i)// &
            new_line('a')//'end module'//new_line('a'))
    end do
    do i = 1, N_LARGE
        write (number, '(i0)') i
        call write_pseudo_random(deps//'/blob'//trim(number)//'.bin', i)
    end do
    call fs_write_text(deps//'/CMakeLists.txt', 'project(big)'//new_line('a'))
    call fs_write_text(meta//'/build-directory.txt', project//'/build'// &
        new_line('a'))

    sources(1) = deps
    bundles(1) = 'project/.fo-cmake/dependencies/big'
    labels(1) = 'cmake-dependency:big'
    call input_inventory_discover_cmake(project, sources, bundles, labels, meta, &
        inventory, ierr, message, freeze_inputs=.true.)
    call require(ierr == 0, 'synthetic CMake inventory discovers: '//trim(message))
    payload = 0
    do i = 1, inventory%entry_count
        if (inventory%entries(i)%kind /= INPUT_FILE) cycle
        size_bytes = entry_size(i)
        if (size_bytes < 0) call require(.false., 'inventoried file has a size')
        payload = payload + size_bytes
    end do
    call require(payload > int(N_LARGE, int64) * LARGE_BYTES, &
        'fixture payload is dominated by dependency files')

    metadata%toolchain = 'gfortran'
    metadata%flags = ''
    metadata%environment = ''
    metadata%driver_digest = repeat('a', HASH_LEN)
    metadata%driver_size = 1
    metadata%base_commit = ''
    metadata%patch_digest = ''
    metadata%execution_identity = generation_manifest_execution_identity( &
        metadata, inventory)
    owner = cache_digest([store], 1)

    call proc_io(read0, write0)
    if (read0 < 0) then
        write (*, '(a)') 'SKIP: /proc/self/io is unavailable; byte bound not checked'
        call capture('generation capture without I/O accounting')
        call fs_remove_tree(root, ierr)
        stop
    end if
    do pass = 1, 2
        call proc_io(read0, write0)
        call capture('generation capture')
        call proc_io(read1, write1)
        write (*, '(a,i0,a,i0,a,i0,a,i0)') 'INFO: pass ', pass, ' read ', &
            read1 - read0, ' written ', write1 - write0, ' payload ', payload
        call require(read1 - read0 <= 2 * payload + METADATA_ALLOWANCE, &
            'capture reads each payload at most twice')
        if (pass == 1) call require(write1 - write0 <= payload + &
            METADATA_ALLOWANCE, 'cold capture writes each payload once')
        if (pass == 2) call require(write1 - write0 <= METADATA_ALLOWANCE, &
            'warm capture writes no payload')
    end do
    call fs_remove_tree(root, ierr)

contains

    subroutine capture(label)
        character(len=*), intent(in) :: label
        integer :: status

        call generation_manifest_capture(store, owner, metadata, inventory, &
            manifest_id, status, message)
        call require(status == 0, label//': '//trim(message))
    end subroutine capture

    integer(int64) function entry_size(index) result(bytes)
        integer, intent(in) :: index
        integer :: r

        bytes = -1
        do r = 1, inventory%root_count
            if (.not. any(inventory%roots(r)%aliases(: &
                inventory%roots(r)%alias_count) == &
                inventory%entries(index)%root_alias)) cycle
            inquire (file=trim(inventory%roots(r)%physical_path)//'/'// &
                trim(inventory%entries(index)%relative_path), size=bytes)
            return
        end do
    end function entry_size

    subroutine write_pseudo_random(path, seed)
        character(len=*), intent(in) :: path
        integer, intent(in) :: seed
        character(len=:), allocatable :: bytes
        integer(int64) :: state
        integer :: unit, k

        allocate (character(len=LARGE_BYTES) :: bytes)
        state = 2654435761_int64 * seed + 1
        do k = 1, LARGE_BYTES
            state = modulo(6364136223_int64 * state + 1442695_int64, &
                2147483648_int64)
            bytes(k:k) = achar(int(modulo(state / 65536_int64, 256_int64)))
        end do
        open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='write')
        write (unit) bytes
        close (unit)
    end subroutine write_pseudo_random

    subroutine proc_io(rchar, wchar)
        integer(int64), intent(out) :: rchar, wchar
        character(len=64) :: key
        integer(int64) :: value
        integer :: unit, ios

        rchar = -1
        wchar = -1
        open (newunit=unit, file='/proc/self/io', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (unit, *, iostat=ios) key, value
            if (ios /= 0) exit
            if (key == 'rchar:') rchar = value
            if (key == 'wchar:') wchar = value
        end do
        close (unit)
    end subroutine proc_io

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

end program test_gremlin_capture_io
