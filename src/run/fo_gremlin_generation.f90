module fo_gremlin_generation
    !! Immutable, content-addressed input snapshots for Gremlin campaigns.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_cache, only: HASH_LEN, cache_digest, cache_file_digest
    use fo_fs, only: fs_make_dir, fs_mkdir_excl
    use fo_process, only: process_getpid
    use fo_input_inventory, only: input_inventory_t
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096
    integer, parameter :: HASH_BLOCK = 128
    integer, parameter :: PART_LEN = 4096

    type, public :: generation_input_t
        character(len=:), allocatable :: label
        character(len=:), allocatable :: source_root
        character(len=:), allocatable :: destination
    end type generation_input_t

    type, public :: generation_context_t
        character(len=:), allocatable :: toolchain
        character(len=:), allocatable :: flags
        character(len=:), allocatable :: environment
        character(len=:), allocatable :: base_commit
        character(len=:), allocatable :: patch_digest
        character(len=PATH_LEN) :: driver_path = ''
        character(len=HASH_LEN) :: driver_digest = ''
        integer(int64) :: driver_size = 0_int64
        type(generation_input_t), allocatable :: inputs(:)
        type(input_inventory_t) :: input_inventory
    end type generation_context_t

    type, public :: generation_t
        character(len=HASH_LEN) :: identity = ''
        character(len=PATH_LEN) :: driver_path = ''
        character(len=HASH_LEN) :: driver_digest = ''
        integer(int64) :: driver_size = 0_int64
        character(len=PATH_LEN) :: root = ''
        character(len=PATH_LEN) :: project_root = ''
        character(len=:), allocatable :: base_commit
        character(len=:), allocatable :: patch_digest
        type(input_inventory_t) :: input_inventory
        logical :: input_inventory_ready = .false.
        logical :: input_inventory_complete = .false.
        character(len=1024) :: input_inventory_diagnostic = ''
    end type generation_t

    public :: generation_capture, generation_driver_identity

    interface
        integer(c_int) function fo_c_generation_list_tree(root, manifest) &
                bind(C, name='fo_c_generation_list_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), manifest(*)
        end function fo_c_generation_list_tree

        integer(c_int) function fo_c_generation_copy_tree(root, dest, manifest) &
                bind(C, name='fo_c_generation_copy_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), dest(*), manifest(*)
        end function fo_c_generation_copy_tree

        integer(c_int) function fo_c_generation_freeze_tree(root) &
                bind(C, name='fo_c_generation_freeze_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
        end function fo_c_generation_freeze_tree

        integer(c_int) function fo_c_generation_freeze_directory(path) &
                bind(C, name='fo_c_generation_freeze_directory')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_generation_freeze_directory

        integer(c_int) function fo_c_generation_lock(path) &
                bind(C, name='fo_c_generation_lock')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_generation_lock

        subroutine fo_c_generation_unlock(fd) &
                bind(C, name='fo_c_generation_unlock')
            import :: c_int
            integer(c_int), value :: fd
        end subroutine fo_c_generation_unlock

        integer(c_int) function fo_c_generation_publish(stage, target) &
                bind(C, name='fo_c_generation_publish')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: stage(*), target(*)
        end function fo_c_generation_publish

        integer(c_int) function fo_c_generation_remove_stage(path) &
                bind(C, name='fo_c_generation_remove_stage')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_generation_remove_stage
    end interface

contains

    subroutine generation_capture(project_root, cas_root, context, generation, &
            ierr, message)
        character(len=*), intent(in) :: project_root, cas_root
        type(generation_context_t), intent(in) :: context
        type(generation_t), intent(out) :: generation
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(generation_input_t), allocatable :: roots(:)
        character(len=PATH_LEN), allocatable :: manifests(:)
        character(len=HASH_LEN), allocatable :: tree_hashes(:)
        character(len=PATH_LEN) :: cache, base, capture_dir, stage, lock_path
        character(len=PATH_LEN) :: verification, verify_dest
        character(len=PATH_LEN) :: second_manifest
        character(len=PATH_LEN) :: source, dest
        character(len=HASH_LEN) :: verify_hash
        character(len=32) :: driver_size_text
        character(len=PART_LEN), allocatable :: identity_parts(:)
        character(len=:), allocatable :: record
        integer :: n_roots, i, rc, fd, attempt, clock_count, clock_rate
        integer(c_int) :: crc
        logical :: exists, matched

        ierr = 1
        message = ''
        generation = generation_t()
        if (len_trim(project_root) == 0 .or. len_trim(cas_root) == 0) then
            message = 'project_root and cas_root are required'
            return
        end if
        if (.not. valid_driver_digest(context%driver_digest) .or. &
            context%driver_size <= 0_int64) then
            message = 'generation requires a pinned fo driver digest and size'
            return
        end if
        if (len_trim(project_root) >= PATH_LEN .or. &
            len_trim(cas_root) + len('/gremlin/generations') + 1 + HASH_LEN + &
            len('/bundle/project') >= PATH_LEN) then
            message = 'generation root path exceeds the supported length'
            return
        end if
        if (index(project_root, achar(0)) /= 0 .or. &
            index(cas_root, achar(0)) /= 0) then
            message = 'generation paths cannot contain a null byte'
            return
        end if
        if (.not. identity_field_fits(context%toolchain) .or. &
            .not. identity_field_fits(context%flags) .or. &
            .not. identity_field_fits(context%environment) .or. &
            .not. identity_field_fits(context%base_commit) .or. &
            .not. identity_field_fits(context%patch_digest)) then
            message = 'generation identity field exceeds the supported length'
            return
        end if
        n_roots = 1
        if (allocated(context%inputs)) n_roots = n_roots + size(context%inputs)
        allocate (roots(n_roots), manifests(n_roots), tree_hashes(n_roots))
        roots(1)%label = 'project'
        roots(1)%source_root = trim(project_root)
        roots(1)%destination = '.'
        do i = 2, n_roots
            roots(i) = context%inputs(i - 1)
            if (.not. allocated(roots(i)%label)) then
                message = 'each generation input needs label, source_root, and '// &
                    'destination'
                return
            end if
            if (.not. allocated(roots(i)%source_root)) then
                message = 'each generation input needs label, source_root, and '// &
                    'destination'
                return
            end if
            if (.not. allocated(roots(i)%destination)) then
                message = 'each generation input needs label, source_root, and '// &
                    'destination'
                return
            end if
            if (len_trim(roots(i)%label) == 0) then
                message = 'generation input label, source_root, and destination '// &
                    'must be nonempty'
                return
            end if
            if (len_trim(roots(i)%source_root) == 0) then
                message = 'generation input label, source_root, and destination '// &
                    'must be nonempty'
                return
            end if
            if (len_trim(roots(i)%destination) == 0) then
                message = 'generation input label, source_root, and destination '// &
                    'must be nonempty'
                return
            end if
            if (len_trim(roots(i)%label) > PART_LEN) then
                message = 'generation input path or label exceeds the supported length'
                return
            end if
            if (len_trim(roots(i)%source_root) >= PATH_LEN) then
                message = 'generation input path or label exceeds the supported length'
                return
            end if
            if (len_trim(roots(i)%destination) > PART_LEN) then
                message = 'generation input path or label exceeds the supported length'
                return
            end if
            if (index(roots(i)%source_root, achar(0)) /= 0 .or. &
                index(roots(i)%destination, achar(0)) /= 0) then
                message = 'generation input paths cannot contain a null byte'
                return
            end if
            if (.not. valid_destination(roots(i)%destination)) then
                message = 'generation input destination escapes the bundle'
                return
            end if
        end do

        call fs_make_dir(trim(cas_root)//'/gremlin/generations')
        base = trim(cas_root)//'/gremlin/generations'
        call fs_make_dir(trim(base)//'/.capture')
        call system_clock(clock_count, clock_rate)
        do attempt = 1, 32
            capture_dir = trim(base)//'/.capture/capture-'// &
                int_text(process_getpid())//'-'//int_text(clock_count + attempt)
            rc = fs_mkdir_excl(trim(capture_dir))
            if (rc == 0) exit
        end do
        if (attempt > 32) then
            message = 'cannot allocate unique generation capture directory'
            return
        end if
        stage = trim(capture_dir)//'/stage'
        rc = fs_mkdir_excl(trim(stage))
        if (rc /= 0) then
            message = 'cannot allocate unique generation staging directory'
            call remove_capture(manifests, n_roots, capture_dir)
            return
        end if
        verification = trim(capture_dir)//'/verify'
        call fs_make_dir(trim(stage)//'/bundle/project')
        call fs_make_dir(trim(verification)//'/bundle/project')
        do i = 1, n_roots
            source = trim(roots(i)%source_root)
            if (i == 1) then
                dest = trim(stage)//'/bundle/project'
                verify_dest = trim(verification)//'/bundle/project'
            else
                dest = trim(stage)//'/bundle/project/'//trim(roots(i)%destination)
                verify_dest = trim(verification)//'/bundle/project/'// &
                    trim(roots(i)%destination)
            end if
            call temp_manifest(capture_dir, i, 'first', manifests(i))
            crc = fo_c_generation_copy_tree(trim(source)//c_null_char, &
                trim(dest)//c_null_char, trim(manifests(i))//c_null_char)
            rc = 1
            if (crc == 0) call hash_tree(trim(dest), manifests(i), &
                tree_hashes(i), rc, message)
            if (crc /= 0 .or. rc /= 0) then
                message = 'cannot capture input tree '//trim(roots(i)%label)
                call remove_capture(manifests, n_roots, capture_dir)
                return
            end if
            call temp_manifest(capture_dir, i, 'verify', second_manifest)
            crc = fo_c_generation_copy_tree(trim(source)//c_null_char, &
                trim(verify_dest)//c_null_char, trim(second_manifest)//c_null_char)
            rc = 1
            if (crc == 0) call hash_tree(trim(verify_dest), second_manifest, &
                verify_hash, rc, message)
            matched = crc == 0 .and. rc == 0
            if (matched) matched = verify_hash == tree_hashes(i)
            call remove_one(second_manifest)
            if (.not. matched) then
                message = 'input changed during descriptor-rooted capture; retry'
                call remove_capture(manifests, n_roots, capture_dir)
                return
            end if
        end do
        crc = fo_c_generation_remove_stage(trim(verification)//c_null_char)
        if (crc /= 0) then
            message = 'cannot remove generation verification copy'
            call remove_capture(manifests, n_roots, capture_dir)
            return
        end if

        allocate (identity_parts(8 + 3 * n_roots))
        identity_parts(1) = 'fo-gremlin-generation-v2'
        identity_parts(2) = value_or_empty(context%toolchain)
        identity_parts(3) = value_or_empty(context%flags)
        identity_parts(4) = value_or_empty(context%environment)
        identity_parts(5) = value_or_empty(context%base_commit)
        identity_parts(6) = value_or_empty(context%patch_digest)
        identity_parts(7) = trim(context%driver_digest)
        write (driver_size_text, '(i0)') context%driver_size
        identity_parts(8) = trim(driver_size_text)
        do i = 1, n_roots
            identity_parts(8 + 3 * i - 2) = trim(roots(i)%label)
            identity_parts(8 + 3 * i - 1) = trim(roots(i)%destination)
            identity_parts(8 + 3 * i) = tree_hashes(i)
        end do
        generation%identity = cache_digest(identity_parts, size(identity_parts))
        cache = trim(base)//'/'//generation%identity
        generation%root = cache
        generation%project_root = trim(cache)//'/bundle/project'
        generation%base_commit = value_or_empty(context%base_commit)
        generation%patch_digest = value_or_empty(context%patch_digest)
        generation%driver_path = context%driver_path
        generation%driver_digest = context%driver_digest
        generation%driver_size = context%driver_size

        inquire (file=trim(cache), exist=exists)
        if (exists) then
            call validate_materialized(roots, tree_hashes, trim(cache), &
                trim(capture_dir), matched, message)
            if (matched) then
                call freeze_generation_inputs(trim(cache), roots, crc)
                if (crc == 0) crc = fo_c_generation_freeze_tree( &
                    trim(cache)//'/identity.txt'//c_null_char)
                if (crc == 0) crc = fo_c_generation_freeze_directory( &
                    trim(cache)//c_null_char)
                if (crc == 0) then
                    ierr = 0
                    call remove_capture(manifests, n_roots, capture_dir)
                    return
                end if
                message = 'existing generation could not be made immutable'
            else
                message = 'existing generation content does not match '// &
                    'its identity: '//trim(cache)
            end if
            call remove_capture(manifests, n_roots, capture_dir)
            return
        end if

        do i = 1, n_roots
            if (i == 1) then
                dest = trim(stage)//'/bundle/project'
            else
                dest = trim(stage)//'/bundle/project/'//trim(roots(i)%destination)
            end if
            call fs_make_dir(trim(dest)//'/build')
        end do
        call freeze_generation_inputs(trim(stage), roots, crc)
        if (crc /= 0) then
            message = 'could not make frozen input bundle immutable'
            call remove_capture(manifests, n_roots, capture_dir)
            return
        end if

        call validate_materialized(roots, tree_hashes, trim(stage), &
            trim(capture_dir), matched, message)
        if (.not. matched) then
            if (len_trim(message) == 0) then
                message = 'input changed while frozen copy was being made; retry'
            end if
            call remove_capture(manifests, n_roots, capture_dir)
            return
        end if

        record = identity_record(context, roots, tree_hashes)
        call write_identity(trim(stage)//'/identity.txt', record, rc)
        if (rc /= 0) then
            message = 'cannot record generation identity metadata'
            call remove_capture(manifests, n_roots, capture_dir)
            return
        end if
        crc = fo_c_generation_freeze_tree( &
            trim(stage)//'/identity.txt'//c_null_char)
        if (crc /= 0) then
            message = 'could not make generation identity metadata immutable'
            call remove_capture(manifests, n_roots, capture_dir)
            return
        end if
        call fs_make_dir(trim(base)//'/.locks')
        lock_path = trim(base)//'/.locks/'//generation%identity
        fd = int(fo_c_generation_lock(trim(lock_path)//c_null_char))
        if (fd < 0) then
            message = 'cannot acquire generation publication lock'
            call remove_capture(manifests, n_roots, capture_dir)
            return
        end if
        inquire (file=trim(cache), exist=exists)
        if (exists) then
            call validate_materialized(roots, tree_hashes, trim(cache), &
                trim(capture_dir), matched, message)
            if (matched) then
                call freeze_generation_inputs(trim(cache), roots, crc)
                if (crc == 0) crc = fo_c_generation_freeze_tree( &
                    trim(cache)//'/identity.txt'//c_null_char)
                if (crc == 0) crc = fo_c_generation_freeze_directory( &
                    trim(cache)//c_null_char)
                if (crc == 0) then
                    ierr = 0
                else
                    message = 'concurrent generation could not be made immutable'
                end if
                call remove_stage(stage)
            else
                message = 'concurrent generation at target failed validation'
                call remove_stage(stage)
            end if
        else
            crc = fo_c_generation_publish(trim(stage)//c_null_char, &
                trim(cache)//c_null_char)
            if (crc == 0) then
                ierr = 0
            else
                message = 'atomic generation publication failed ('// &
                    trim(int_text(int(crc)))//')'
                call remove_stage(stage)
            end if
        end if
        call fo_c_generation_unlock(int(fd, c_int))
        call remove_capture(manifests, n_roots, capture_dir)
    end subroutine generation_capture

    subroutine freeze_generation_inputs(generation_root, roots, ierr)
        character(len=*), intent(in) :: generation_root
        type(generation_input_t), intent(in) :: roots(:)
        integer(c_int), intent(out) :: ierr

        character(len=PATH_LEN) :: input_root
        integer :: i

        ierr = 0
        do i = 1, size(roots)
            if (i == 1) then
                input_root = trim(generation_root)//'/bundle/project'
            else
                input_root = trim(generation_root)//'/bundle/project/'// &
                    trim(roots(i)%destination)
            end if
            ierr = fo_c_generation_freeze_tree(trim(input_root)//c_null_char)
            if (ierr /= 0) return
        end do
        ierr = fo_c_generation_freeze_directory( &
            trim(generation_root)//'/bundle'//c_null_char)
    end subroutine freeze_generation_inputs

    subroutine hash_tree(root, manifest, digest, ierr, message)
        character(len=*), intent(in) :: root, manifest
        character(len=HASH_LEN), intent(out) :: digest
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: rel, full, record
        character(len=8) :: executable_text
        character(len=HASH_LEN) :: file_hash
        character(len=HASH_LEN) :: empty_parts(1)
        character(len=PATH_LEN) :: link_part(1)
        character(len=PART_LEN) :: parts(HASH_BLOCK * 2)
        character(len=HASH_LEN), allocatable :: blocks(:)
        character(len=HASH_LEN), allocatable :: grown(:)
        integer :: unit, ios, n_parts, n_blocks, capacity, executable
        integer :: path_length, target_length, record_length

        ierr = 1
        message = ''
        digest = ''
        capacity = 32
        allocate (blocks(capacity))
        n_blocks = 0
        n_parts = 0
        open (newunit=unit, file=trim(manifest), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) then
            message = 'cannot read generation file inventory'
            return
        end if
        do
            read (unit, '(a)', iostat=ios) record
            if (ios < 0) exit
            if (ios /= 0) then
                close (unit)
                message = 'invalid path in generation inventory'
                return
            end if
            record_length = len_trim(record)
            if (record_length < 7) then
                close (unit)
                message = 'invalid path in generation inventory'
                return
            end if
            read (record(3:5), '(i3)', iostat=ios) executable
            if (ios /= 0) then
                close (unit)
                message = 'invalid mode in generation inventory'
                return
            end if
            if (executable /= 0 .and. executable /= 1) then
                close (unit)
                message = 'invalid executable mode in generation inventory'
                return
            end if
            if (record(1:1) /= 'D' .and. record(1:1) /= 'F' .and. &
                record(1:1) /= 'L') then
                close (unit)
                message = 'invalid entry type in generation inventory'
                return
            end if
            if (record(1:1) == 'L') then
                if (executable /= 0) then
                    close (unit)
                    message = 'invalid symlink mode in generation inventory'
                    return
                end if
                if (record_length < 24) then
                    close (unit)
                    message = 'invalid symlink in generation inventory'
                    return
                end if
                read (record(7:14), '(i8)', iostat=ios) path_length
                if (ios /= 0) then
                    close (unit)
                    message = 'invalid symlink path length in inventory'
                    return
                end if
                read (record(16:23), '(i8)', iostat=ios) target_length
                if (ios /= 0) then
                    close (unit)
                    message = 'invalid symlink target length in inventory'
                    return
                end if
                if (path_length <= 0 .or. target_length <= 0) then
                    close (unit)
                    message = 'invalid symlink lengths in generation inventory'
                    return
                end if
                if (24 + path_length + 2 * target_length /= record_length .or. &
                    path_length >= PATH_LEN .or. 2 * target_length > PATH_LEN) then
                    close (unit)
                    message = 'invalid symlink record size in inventory'
                    return
                end if
                rel = record(25:24 + path_length)
                link_part(1) = record(25 + path_length: &
                    24 + path_length + 2 * target_length)
                file_hash = cache_digest(link_part, 1)
            else
                rel = record(7:)
            end if
            if (len_trim(rel) >= PATH_LEN) then
                close (unit)
                message = 'generation input path exceeds supported length'
                return
            end if
            if (record(1:1) == 'D') then
                file_hash = 'directory'
            else if (record(1:1) == 'F') then
                full = trim(root)//'/'//trim(rel)
                call cache_file_digest(trim(full), file_hash)
                if (len_trim(file_hash) == 0) then
                    close (unit)
                    message = 'cannot hash generation input '//trim(rel)
                    return
                end if
            end if
            write (executable_text, '(i0)') executable
            n_parts = n_parts + 1
            parts(n_parts * 2 - 1) = trim(rel)
            parts(n_parts * 2) = trim(file_hash)//':x='// &
                trim(executable_text)
            if (n_parts == HASH_BLOCK) then
                call append_block(blocks, n_blocks, capacity, &
                    cache_digest(parts, 2 * n_parts))
                n_parts = 0
            end if
        end do
        close (unit)
        if (n_parts > 0) then
            call append_block(blocks, n_blocks, capacity, &
                cache_digest(parts, 2 * n_parts))
        end if
        if (n_blocks == 0) then
            empty_parts(1) = 'empty-tree'
            digest = cache_digest(empty_parts, 1)
        else
            digest = cache_digest(blocks, n_blocks)
        end if
        ierr = 0
    end subroutine hash_tree

    subroutine append_block(blocks, n_blocks, capacity, block_hash)
        character(len=HASH_LEN), allocatable, intent(inout) :: blocks(:)
        integer, intent(inout) :: n_blocks, capacity
        character(len=HASH_LEN), intent(in) :: block_hash
        character(len=HASH_LEN), allocatable :: grown(:)

        if (n_blocks == capacity) then
            allocate (grown(capacity * 2))
            grown(:capacity) = blocks
            call move_alloc(grown, blocks)
            capacity = capacity * 2
        end if
        n_blocks = n_blocks + 1
        blocks(n_blocks) = block_hash
    end subroutine append_block

    subroutine validate_materialized(roots, expected, generation_root, &
            manifest_root, matched, message)
        type(generation_input_t), intent(in) :: roots(:)
        character(len=HASH_LEN), intent(in) :: expected(:)
        character(len=*), intent(in) :: generation_root, manifest_root
        logical, intent(out) :: matched
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: target, manifest
        character(len=HASH_LEN) :: got
        integer :: i, rc
        integer(c_int) :: crc

        matched = .false.
        message = ''
        do i = 1, size(roots)
            if (i == 1) then
                target = trim(generation_root)//'/bundle/project'
            else
                target = trim(generation_root)//'/bundle/project/'// &
                    trim(roots(i)%destination)
            end if
            manifest = trim(manifest_root)//'/check-'//int_text(i)
            crc = fo_c_generation_list_tree(trim(target)//c_null_char, &
                trim(manifest)//c_null_char)
            if (crc /= 0) then
                message = 'generation input tree is missing or unreadable'
                call remove_one(manifest)
                return
            end if
            call hash_tree(target, manifest, got, rc, message)
            call remove_one(manifest)
            if (rc /= 0) return
            if (got /= expected(i)) then
                message = 'frozen generation content does not match source identity'
                return
            end if
        end do
        matched = .true.
    end subroutine validate_materialized

    function identity_record(context, roots, hashes) result(record)
        type(generation_context_t), intent(in) :: context
        type(generation_input_t), intent(in) :: roots(:)
        character(len=HASH_LEN), intent(in) :: hashes(:)
        character(len=:), allocatable :: record
        character(len=32) :: driver_size_text
        integer :: i

        write (driver_size_text, '(i0)') context%driver_size
        record = 'schema=fo-gremlin-generation-v2'//new_line('a')// &
            'toolchain='//value_or_empty(context%toolchain)//new_line('a')// &
            'flags='//value_or_empty(context%flags)//new_line('a')// &
            'environment='//value_or_empty(context%environment)//new_line('a')// &
            'base_commit='//value_or_empty(context%base_commit)//new_line('a')// &
            'patch_digest='//value_or_empty(context%patch_digest)//new_line('a')// &
            'driver_digest='//trim(context%driver_digest)//new_line('a')// &
            'driver_size='//trim(driver_size_text)//new_line('a')
        do i = 1, size(roots)
            record = record//'input='//trim(roots(i)%label)//'|'// &
                trim(roots(i)%destination)//'|'//trim(hashes(i))//new_line('a')
        end do
    end function identity_record

    subroutine generation_driver_identity(generation_root, digest, size, ierr, message)
        character(len=*), intent(in) :: generation_root
        character(len=HASH_LEN), intent(out) :: digest
        integer(int64), intent(out) :: size
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: identity_path
        character(len=PATH_LEN) :: line
        integer :: unit, io_status, parse_status
        logical :: found_digest, found_size

        digest = ''
        size = 0_int64
        ierr = 1
        message = ''
        if (len_trim(generation_root) == 0 .or. &
            len_trim(generation_root) + len('/identity.txt') >= len(identity_path)) then
            message = 'generation root cannot hold its identity path'
            return
        end if
        identity_path = trim(generation_root)//'/identity.txt'
        open (newunit=unit, file=trim(identity_path), status='old', action='read', &
            iostat=io_status)
        if (io_status /= 0) then
            message = 'generation identity metadata is unavailable'
            return
        end if
        found_digest = .false.
        found_size = .false.
        do
            read (unit, '(a)', iostat=io_status) line
            if (io_status /= 0) exit
            if (index(line, 'driver_digest=') == 1) then
                if (found_digest) then
                    close (unit)
                    message = 'generation identity repeats its driver digest'
                    return
                end if
                if (len_trim(line) - len('driver_digest=') /= HASH_LEN) then
                    close (unit)
                    message = 'generation identity has an invalid driver digest length'
                    return
                end if
                digest = line(len('driver_digest=') + 1:)
                found_digest = .true.
            else if (index(line, 'driver_size=') == 1) then
                if (found_size) then
                    close (unit)
                    message = 'generation identity repeats its driver size'
                    return
                end if
                read (line(len('driver_size=') + 1:), *, iostat=parse_status) size
                if (parse_status /= 0) then
                    close (unit)
                    message = 'generation identity has an invalid driver size'
                    return
                end if
                found_size = .true.
            end if
        end do
        close (unit)
        if (io_status > 0) then
            message = 'generation identity metadata cannot be read'
            return
        end if
        if (.not. found_digest .or. .not. found_size .or. size <= 0_int64 .or. &
            .not. valid_driver_digest(digest)) then
            message = 'generation identity has no valid pinned fo driver'
            return
        end if
        ierr = 0
    end subroutine generation_driver_identity

    logical function valid_driver_digest(digest)
        character(len=*), intent(in) :: digest
        integer :: i, value

        valid_driver_digest = .false.
        if (len_trim(digest) /= HASH_LEN) return
        do i = 1, HASH_LEN
            value = iachar(digest(i:i))
            if (.not. ((value >= iachar('0') .and. value <= iachar('9')) .or. &
                (value >= iachar('a') .and. value <= iachar('f')))) return
        end do
        valid_driver_digest = .true.
    end function valid_driver_digest

    subroutine write_identity(path, record, ierr)
        character(len=*), intent(in) :: path, record
        integer, intent(out) :: ierr
        integer :: unit, ios

        ierr = 1
        open (newunit=unit, file=trim(path), status='new', action='write', &
            iostat=ios)
        if (ios /= 0) return
        write (unit, '(a)', iostat=ios) record
        if (ios == 0) flush (unit, iostat=ios)
        close (unit, iostat=ierr)
        if (ios /= 0) ierr = ios
    end subroutine write_identity

    function value_or_empty(value) result(text)
        character(len=:), allocatable, intent(in) :: value
        character(len=:), allocatable :: text
        text = ''
        if (allocated(value)) text = value
    end function value_or_empty

    logical function identity_field_fits(value)
        character(len=:), allocatable, intent(in) :: value

        identity_field_fits = .true.
        if (.not. allocated(value)) return
        identity_field_fits = len(value) <= PART_LEN
    end function identity_field_fits

    logical function valid_destination(destination)
        character(len=*), intent(in) :: destination
        integer :: depth, start, stop
        character(len=PATH_LEN) :: component

        valid_destination = .false.
        if (len_trim(destination) == 0) return
        if (destination(1:1) == '/') return
        depth = 1
        start = 1
        do while (start <= len_trim(destination))
            stop = index(destination(start:), '/')
            if (stop == 0) then
                component = destination(start:len_trim(destination))
                start = len_trim(destination) + 1
            else
                component = destination(start:start + stop - 2)
                start = start + stop
            end if
            select case (trim(component))
            case ('', '.')
                cycle
            case ('..')
                depth = depth - 1
                if (depth < 0) return
            case default
                depth = depth + 1
            end select
        end do
        valid_destination = .true.
    end function valid_destination

    subroutine temp_manifest(workdir, index, phase, path)
        character(len=*), intent(in) :: workdir, phase
        integer, intent(in) :: index
        character(len=*), intent(out) :: path
        path = trim(workdir)//'/manifest-'//int_text(index)//'-'//trim(phase)
    end subroutine temp_manifest

    subroutine remove_manifests(paths, n)
        character(len=*), intent(in) :: paths(:)
        integer, intent(in) :: n
        integer :: i
        do i = 1, min(n, size(paths))
            call remove_one(paths(i))
        end do
    end subroutine remove_manifests

    subroutine remove_capture(paths, n, capture_dir)
        character(len=*), intent(in) :: paths(:), capture_dir
        integer, intent(in) :: n

        call remove_manifests(paths, n)
        call remove_stage(capture_dir)
    end subroutine remove_capture

    subroutine remove_one(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        open (newunit=unit, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) return
        close (unit, status='delete', iostat=ios)
    end subroutine remove_one

    subroutine remove_stage(path)
        character(len=*), intent(in) :: path
        integer(c_int) :: ignored
        ignored = fo_c_generation_remove_stage(trim(path)//c_null_char)
    end subroutine remove_stage

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
        text = trim(adjustl(text))
    end function int_text

end module fo_gremlin_generation
