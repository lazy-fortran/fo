module fo_generation_manifest
    !! Compact, versioned Gremlin generation manifests over the shared Fx blob store.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, &
        c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_immutable_store, only: immutable_store_t, immutable_lease_t, &
        immutable_store_init, immutable_store_put_blob, &
        immutable_store_hash_file, immutable_store_publication_lease_acquire, &
        immutable_store_root_set, immutable_store_lease_release, &
        immutable_store_materialize_blob, IMMUTABLE_MATERIALIZE_COPY, &
        IMMUTABLE_OK
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_input_inventory, only: input_inventory_t, input_root_t, &
        input_entry_t, input_declaration_t, INPUT_FILE, INPUT_DIRECTORY, &
        INPUT_SYMLINK, input_inventory_revalidate
    use fo_fs, only: fs_make_dir
    use fo_util, only: make_tmpfile, delete_tmpfile
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096
    integer, parameter :: MANIFEST_VERSION = 2
    character(len=*), parameter :: HEADER = 'fo-generation-manifest'

    type, public :: generation_manifest_metadata_t
        character(len=:), allocatable :: execution_identity
        character(len=:), allocatable :: toolchain
        character(len=:), allocatable :: flags
        character(len=:), allocatable :: environment
        character(len=:), allocatable :: driver_digest
        integer(int64) :: driver_size = 0_int64
        character(len=:), allocatable :: base_commit
        character(len=:), allocatable :: patch_digest
    end type generation_manifest_metadata_t

    public :: generation_manifest_capture, generation_manifest_load
    public :: generation_manifest_materialize
    public :: generation_manifest_execution_identity

    interface
        integer(c_int) function fo_c_generation_create_link(path, target) &
                bind(C, name='fo_c_generation_create_link')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*), target(*)
        end function fo_c_generation_create_link

        integer(c_int) function fo_c_generation_capture_file(root, relative, &
                destination, expected_device, expected_inode) &
                bind(C, name='fo_c_generation_capture_file')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: root(*), relative(*)
            character(kind=c_char), intent(in) :: destination(*)
            integer(c_long_long), value :: expected_device, expected_inode
        end function fo_c_generation_capture_file
    end interface

contains

    subroutine generation_manifest_capture(store_root, metadata, inventory, &
            manifest_id, ierr, message)
        character(len=*), intent(in) :: store_root
        type(generation_manifest_metadata_t), intent(in) :: metadata
        type(input_inventory_t), intent(in) :: inventory
        character(len=HASH_LEN), intent(out) :: manifest_id
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(immutable_store_t) :: store
        type(immutable_lease_t) :: publication
        character(len=HASH_LEN), allocatable :: ids(:)
        character(len=HASH_LEN) :: object_id
        character(len=4), allocatable :: kinds(:)
        character(len=PATH_LEN) :: manifest_path, source_path, temp_payload
        integer :: status, i, j, n_ids, lease_status, root_index
        logical :: seen
        integer(c_int) :: capture_status

        manifest_id = ''
        ierr = 1
        message = ''
        if (len_trim(store_root) == 0) then
            message = 'an immutable object store root is required'
            return
        end if
        if (.not. allocated(metadata%execution_identity)) then
            message = 'generation manifest requires an execution identity'
            return
        end if
        if (.not. inventory%valid .or. inventory%root_count <= 0 .or. &
            .not. allocated(inventory%roots) .or. &
            .not. allocated(inventory%entries)) then
            message = 'generation manifest requires a valid canonical inventory'
            return
        end if
        if (.not. allocated(metadata%driver_digest)) then
            message = 'generation manifest requires a pinned driver digest'
            return
        end if
        if (len_trim(metadata%execution_identity) == 0 .or. &
            .not. is_hex_digest(trim(metadata%driver_digest)) .or. &
            .not. is_hex_digest(trim(inventory%digest))) then
            message = 'generation manifest identity or pinned driver digest is invalid'
            return
        end if
        if (metadata%execution_identity /= &
            generation_manifest_execution_identity(metadata, inventory)) then
            message = 'generation manifest execution identity is inconsistent'
            return
        end if
        call immutable_store_init(store, trim(store_root), status)
        if (status /= IMMUTABLE_OK) then
            message = 'cannot initialize immutable generation object store'
            return
        end if
        call make_tmpfile('fo-generation-manifest', manifest_path)
        call write_manifest_file(trim(manifest_path), metadata, inventory, status, &
            message)
        if (status /= 0) then
            call delete_tmpfile(trim(manifest_path))
            return
        end if
        call immutable_store_hash_file(trim(manifest_path), manifest_id, &
            status)
        if (status /= IMMUTABLE_OK) then
            manifest_id = ''
            message = 'cannot hash serialized generation manifest'
            call delete_tmpfile(trim(manifest_path))
            return
        end if

        allocate (ids(inventory%entry_count + 1), &
            kinds(inventory%entry_count + 1))
        n_ids = 0
        do i = 1, inventory%entry_count
            if (inventory%entries(i)%kind /= INPUT_FILE) cycle
            object_id = inventory%entries(i)%content_digest
            if (len_trim(object_id) /= HASH_LEN) then
                message = 'canonical file entry has no content digest'
                call delete_tmpfile(trim(manifest_path))
                manifest_id = ''
                return
            end if
            seen = .false.
            do j = 1, n_ids
                if (ids(j) == object_id) seen = .true.
            end do
            if (seen) cycle
            n_ids = n_ids + 1
            ids(n_ids) = object_id
            kinds(n_ids) = 'blob'
        end do
        do j = 1, n_ids
            if (ids(j) == manifest_id) then
                message = 'manifest and payload unexpectedly have the same object id'
                call delete_tmpfile(trim(manifest_path))
                manifest_id = ''
                return
            end if
        end do
        n_ids = n_ids + 1
        ids(n_ids) = manifest_id
        kinds(n_ids) = 'blob'
        call immutable_store_publication_lease_acquire(store, &
            'fo-generation-capture', store%writer_start, &
            'generation-'//trim(metadata%execution_identity), &
            kinds(:n_ids), ids(:n_ids), publication, status)
        if (status /= IMMUTABLE_OK) then
            message = 'cannot protect generation objects during publication'
            call delete_tmpfile(trim(manifest_path))
            manifest_id = ''
            return
        end if

        do i = 1, inventory%entry_count
            if (inventory%entries(i)%kind /= INPUT_FILE) cycle
            call inventory_source_path(inventory, inventory%entries(i), &
                root_index, source_path, status, message)
            if (status /= 0) exit
            call make_tmpfile('fo-generation-payload', temp_payload)
            capture_status = fo_c_generation_capture_file( &
                trim(inventory%roots(root_index)%physical_path)//c_null_char, &
                trim(inventory%entries(i)%relative_path)//c_null_char, &
                trim(temp_payload)//c_null_char, &
                inventory%roots(root_index)%device, &
                inventory%roots(root_index)%inode)
            if (capture_status /= 0) then
                status = 1
                message = 'descriptor-rooted input capture failed: '// &
                    trim(inventory%entries(i)%root_alias)//':'// &
                    trim(inventory%entries(i)%relative_path)
                exit
            end if
            call immutable_store_put_blob(store, trim(temp_payload), object_id, status)
            call delete_tmpfile(trim(temp_payload))
            if (status /= IMMUTABLE_OK .or. &
                object_id /= inventory%entries(i)%content_digest) then
                status = 1
                message = 'input changed while its content blob was captured: '// &
                    trim(inventory%entries(i)%root_alias)//':'// &
                    trim(inventory%entries(i)%relative_path)
                exit
            end if
        end do
        if (status == IMMUTABLE_OK) then
            call immutable_store_put_blob(store, trim(manifest_path), object_id, &
                status)
            if (status == IMMUTABLE_OK .and. object_id /= manifest_id) then
                status = 1
                message = 'serialized generation manifest changed during publication'
            end if
        end if
        if (status == IMMUTABLE_OK) then
            if (len(project_source_root(inventory)) == 0) then
                status = 1
                message = 'canonical inventory has no project source root'
            else
                call input_inventory_revalidate(project_source_root(inventory), &
                    inventory%declarations, inventory, status, message)
            end if
        end if
        if (status == IMMUTABLE_OK) then
            call immutable_store_root_set(store, 'fo-generation', &
                trim(metadata%execution_identity), &
                'generation-'//trim(metadata%execution_identity), &
                kinds(:n_ids), ids(:n_ids), status)
        end if
        call immutable_store_lease_release(store, publication, lease_status)
        call delete_tmpfile(trim(manifest_path))
        if (status /= IMMUTABLE_OK .or. lease_status /= IMMUTABLE_OK) then
            if (len_trim(message) == 0) &
                message = 'generation manifest publication or root commit failed'
            manifest_id = ''
            return
        end if
        ierr = 0
    end subroutine generation_manifest_capture

    function generation_manifest_execution_identity(metadata, inventory) &
            result(identity)
        type(generation_manifest_metadata_t), intent(in) :: metadata
        type(input_inventory_t), intent(in) :: inventory
        character(len=HASH_LEN) :: identity
        character(len=32) :: driver_size
        character(len=PATH_LEN * 4) :: parts(7)

        write (driver_size, '(i0)') metadata%driver_size
        parts(1) = 'fo-gremlin-execution-v3'
        parts(2) = value_or_empty(metadata%toolchain)
        parts(3) = value_or_empty(metadata%flags)
        parts(4) = value_or_empty(metadata%environment)
        parts(5) = value_or_empty(metadata%driver_digest)
        parts(6) = trim(driver_size)
        parts(7) = inventory%digest
        identity = cache_digest(parts, size(parts))
    end function generation_manifest_execution_identity

    subroutine write_manifest_file(path, metadata, inventory, ierr, message)
        character(len=*), intent(in) :: path
        type(generation_manifest_metadata_t), intent(in) :: metadata
        type(input_inventory_t), intent(in) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: unit, ios, i, j, k, canonical_count, declaration_count
        character(len=32) :: number
        character(len=32) :: driver_size
        character(len=1) :: flag, valid_flag
        character(len=HASH_LEN) :: content_id

        ierr = 1
        message = ''
        if (.not. allocated(inventory%roots) .or. &
            .not. allocated(inventory%entries)) then
            message = 'canonical input inventory has no allocated roots or entries'
            return
        end if
        if (inventory%root_count < 1 .or. &
            inventory%root_count > size(inventory%roots) .or. &
            inventory%entry_count < 0 .or. &
            inventory%entry_count > size(inventory%entries)) then
            message = 'canonical input inventory counts are invalid'
            return
        end if
        declaration_count = 0
        if (allocated(inventory%declarations)) &
            declaration_count = size(inventory%declarations)
        do i = 1, inventory%root_count
            if (inventory%roots(i)%alias_count < 1 .or. &
                inventory%roots(i)%alias_count > &
                    size(inventory%roots(i)%aliases)) then
                message = 'canonical input root has an invalid alias count'
                return
            end if
            if (len_trim(inventory%roots(i)%canonical_alias) == 0) then
                message = 'canonical input root has no canonical alias'
                return
            end if
            canonical_count = 0
            do j = 1, inventory%roots(i)%alias_count
                if (len_trim(inventory%roots(i)%aliases(j)) == 0 .or. &
                    .not. safe_bundle_path( &
                        trim(inventory%roots(i)%bundle_paths(j)))) then
                    message = 'canonical input root has an invalid alias mapping'
                    return
                end if
                if (trim(inventory%roots(i)%aliases(j)) == &
                    trim(inventory%roots(i)%canonical_alias)) &
                    canonical_count = canonical_count + 1
            end do
            if (canonical_count /= 1) then
                message = 'canonical input root alias is missing or duplicated'
                return
            end if
        end do
        do i = 1, declaration_count
            canonical_count = 0
            do j = 1, inventory%root_count
                do k = 1, inventory%roots(j)%alias_count
                    if (trim(inventory%declarations(i)%root_alias) == &
                            trim(inventory%roots(j)%aliases(k))) &
                        canonical_count = canonical_count + 1
                end do
            end do
            if (canonical_count /= 1 .or. &
                .not. safe_entry_path( &
                    trim(inventory%declarations(i)%relative_path))) then
                message = 'canonical input declaration has an invalid root or path'
                return
            end if
        end do
        open (newunit=unit, file=trim(path), status='replace', action='write', &
            iostat=ios)
        if (ios /= 0) then
            message = 'cannot create temporary generation manifest'
            return
        end if
        write (unit, '(A)', iostat=ios) HEADER//achar(9)//int_text(MANIFEST_VERSION)
        if (ios == 0) then
            write (driver_size, '(i0)') metadata%driver_size
            write (unit, '(A)', iostat=ios) 'M'//achar(9)// &
                hex_encode(value_or_empty(metadata%execution_identity))//achar(9)// &
                hex_encode(value_or_empty(metadata%toolchain))//achar(9)// &
                hex_encode(value_or_empty(metadata%flags))//achar(9)// &
                hex_encode(value_or_empty(metadata%environment))//achar(9)// &
                hex_encode(value_or_empty(metadata%driver_digest))//achar(9)// &
                trim(driver_size)//achar(9)// &
                hex_encode(value_or_empty(metadata%base_commit))//achar(9)// &
                hex_encode(value_or_empty(metadata%patch_digest))
        end if
        if (ios == 0) then
        if (inventory%complete) then
            flag = '1'
        else
            flag = '0'
        end if
        if (inventory%valid) then
            valid_flag = '1'
        else
            valid_flag = '0'
        end if
        write (number, '(i0)') inventory%root_count
        write (unit, '(A)', iostat=ios) 'S'//achar(9)//trim(number)//achar(9)// &
            int_text(inventory%entry_count)//achar(9)// &
            int_text(declaration_count)//achar(9)//valid_flag//achar(9)// &
            flag//achar(9)// &
            hex_encode(trim(inventory%diagnostic))//achar(9)// &
            trim(inventory%digest)
        end if
        if (allocated(inventory%declarations)) then
            do i = 1, declaration_count
                if (ios /= 0) exit
                write (unit, '(A)', iostat=ios) 'D'//achar(9)// &
                    hex_encode( &
                    trim(inventory%declarations(i)%root_alias))//achar(9)// &
                    hex_encode( &
                    trim(inventory%declarations(i)%relative_path))//achar(9)// &
                    hex_encode(trim(inventory%declarations(i)%role))//achar(9)// &
                    int_text(inventory%declarations(i)%expected_kind)//achar(9)// &
                    int_text(inventory%declarations(i)%expected_mode)//achar(9)// &
                    bool_text(inventory%declarations(i)%writable_at_execution)
            end do
        end if
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                if (ios /= 0) exit
                write (unit, '(A)', iostat=ios) 'R'//achar(9)// &
                    int_text(i)//achar(9)// &
                    hex_encode(trim(inventory%roots(i)%aliases(j)))//achar(9)// &
                    hex_encode(trim(inventory%roots(i)%bundle_paths(j)))//achar(9)// &
                    bool_text(trim(inventory%roots(i)%aliases(j)) == &
                    trim(inventory%roots(i)%canonical_alias))
            end do
        end do
        do i = 1, inventory%entry_count
            if (ios /= 0) exit
            content_id = inventory%entries(i)%content_digest
            if (len_trim(content_id) == 0) content_id = '-'
            write (unit, '(A)', iostat=ios) 'E'//achar(9)// &
                hex_encode(trim(inventory%entries(i)%root_alias))//achar(9)// &
                hex_encode(trim(inventory%entries(i)%relative_path))//achar(9)// &
                hex_encode(trim(inventory%entries(i)%role))//achar(9)// &
                int_text(inventory%entries(i)%kind)//achar(9)// &
                int_text(inventory%entries(i)%mode)//achar(9)// &
                bool_text(inventory%entries(i)%writable_at_execution)//achar(9)// &
                hex_encode(trim(inventory%entries(i)%link_target))//achar(9)// &
                trim(content_id)
        end do
        close (unit, iostat=j)
        if (ios /= 0 .or. j /= 0) then
            message = 'failed while writing temporary generation manifest'
            return
        end if
        ierr = 0
    end subroutine write_manifest_file

    function hex_encode(value) result(encoded)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: encoded
        character(len=16), parameter :: hex = '0123456789abcdef'
        integer :: i, byte

        if (len(value) == 0) then
            encoded = '-'
            return
        end if
        allocate (character(len=2 * len(value)) :: encoded)
        do i = 1, len(value)
            byte = iachar(value(i:i))
            encoded(2 * i - 1:2 * i - 1) = hex(byte / 16 + 1:byte / 16 + 1)
            encoded(2 * i:2 * i) = hex(mod(byte, 16) + 1:mod(byte, 16) + 1)
        end do
    end function hex_encode

    function value_or_empty(value) result(text)
        character(len=:), allocatable, intent(in) :: value
        character(len=:), allocatable :: text

        text = ''
        if (allocated(value)) text = value
    end function value_or_empty

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=32) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function int_text

    function bool_text(value) result(text)
        logical, intent(in) :: value
        character(len=1) :: text

        text = '0'
        if (value) text = '1'
    end function bool_text

    subroutine generation_manifest_load(store_root, manifest_id, metadata, &
            inventory, ierr, message)
        character(len=*), intent(in) :: store_root, manifest_id
        type(generation_manifest_metadata_t), intent(out) :: metadata
        type(input_inventory_t), intent(out) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(immutable_store_t) :: store
        character(len=PATH_LEN) :: temp_path
        character(len=32768) :: line
        character(len=32768) :: fields(10)
        integer :: status, unit, ios, n_fields
        integer :: got_aliases, got_entries, got_declarations
        integer :: declaration_count, root_index, complete, valid, i, j
        integer(int64) :: driver_size
        character(len=:), allocatable :: decoded
        logical :: used_clone

        metadata = generation_manifest_metadata_t()
        inventory = input_inventory_t()
        ierr = 1
        message = ''
        if (len_trim(store_root) == 0 .or. len_trim(manifest_id) /= HASH_LEN) then
            message = 'manifest store root or object id is invalid'
            return
        end if
        call immutable_store_init(store, trim(store_root), status)
        if (status /= IMMUTABLE_OK) then
            message = 'cannot initialize immutable generation object store'
            return
        end if
        call make_tmpfile('fo-generation-manifest-read', temp_path)
        call immutable_store_materialize_blob(store, manifest_id, trim(temp_path), &
            384, IMMUTABLE_MATERIALIZE_COPY, used_clone, status)
        if (status /= IMMUTABLE_OK) then
            message = 'cannot read immutable generation manifest blob'
            call delete_tmpfile(trim(temp_path))
            return
        end if
        open (newunit=unit, file=trim(temp_path), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) then
            message = 'cannot open materialized generation manifest'
            call delete_tmpfile(trim(temp_path))
            return
        end if
        read (unit, '(A)', iostat=ios) line
        if (ios /= 0) then
            message = 'generation manifest header is missing'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        if (trim(line) /= HEADER//achar(9)//int_text(MANIFEST_VERSION)) then
            message = 'unsupported or malformed generation manifest header'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        read (unit, '(A)', iostat=ios) line
        if (ios /= 0) then
            message = 'generation manifest metadata is missing'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        call split_fields(trim(line), fields, n_fields)
        if (n_fields /= 9 .or. trim(fields(1)) /= 'M') then
            message = 'generation manifest metadata record is malformed'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        call hex_decode(trim(fields(2)), metadata%execution_identity, status)
        if (status == 0) call hex_decode(trim(fields(3)), metadata%toolchain, status)
        if (status == 0) call hex_decode(trim(fields(4)), metadata%flags, status)
        if (status == 0) call hex_decode(trim(fields(5)), metadata%environment, status)
        if (status == 0) call hex_decode(trim(fields(6)), &
            metadata%driver_digest, status)
        if (status /= 0) then
            message = 'generation manifest metadata contains invalid text encoding'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        read (fields(7), *, iostat=ios) driver_size
        if (ios /= 0) then
            message = 'generation manifest driver size is malformed'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        metadata%driver_size = driver_size
        call hex_decode(trim(fields(8)), metadata%base_commit, status)
        if (status == 0) &
            call hex_decode(trim(fields(9)), metadata%patch_digest, status)
        if (status /= 0 .or. len(metadata%driver_digest) /= HASH_LEN) then
            message = 'generation manifest provenance metadata is malformed'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        if (.not. is_hex_digest(trim(metadata%driver_digest)) .or. &
            driver_size <= 0_int64) then
            message = 'generation manifest driver identity is malformed'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        read (unit, '(A)', iostat=ios) line
        if (ios /= 0) then
            message = 'generation manifest inventory header is missing'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        call split_fields(trim(line), fields, n_fields)
        if (n_fields /= 8 .or. trim(fields(1)) /= 'S') then
            message = 'generation manifest inventory header is malformed'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        inventory%root_count = 0
        inventory%entry_count = -1
        declaration_count = -1
        valid = -1
        complete = -1
        read (fields(2), *, iostat=ios) inventory%root_count
        if (ios == 0) read (fields(3), *, iostat=ios) inventory%entry_count
        if (ios == 0) read (fields(4), *, iostat=ios) declaration_count
        read (fields(5), *, iostat=status) valid
        if (status == 0) read (fields(6), *, iostat=status) complete
        if (ios /= 0 .or. status /= 0 .or. inventory%root_count < 1 .or. &
            inventory%root_count > 64 .or. inventory%entry_count < 0 .or. &
            declaration_count < 0 .or. &
            (valid /= 0 .and. valid /= 1) .or. &
            (complete /= 0 .and. complete /= 1)) then
            message = 'generation manifest inventory counts are invalid'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        inventory%valid = valid == 1
        inventory%complete = complete == 1
        call hex_decode(trim(fields(7)), decoded, status)
        if (status /= 0 .or. len_trim(fields(8)) /= HASH_LEN) then
            message = 'generation manifest inventory summary is malformed'
            close (unit)
            call delete_tmpfile(trim(temp_path))
            return
        end if
        inventory%diagnostic = decoded
        inventory%digest = fields(8)(1:HASH_LEN)
        allocate (inventory%roots(inventory%root_count), &
            inventory%entries(inventory%entry_count), &
            inventory%declarations(declaration_count))
        inventory%entry_capacity = inventory%entry_count
        do i = 1, inventory%root_count
            inventory%roots(i)%aliases = ''
            inventory%roots(i)%bundle_paths = ''
            inventory%roots(i)%alias_count = 0
        end do
        got_aliases = 0
        got_entries = 0
        got_declarations = 0
        do
            read (unit, '(A)', iostat=ios) line
            if (ios < 0) exit
            if (ios /= 0) then
                message = 'cannot read generation manifest inventory records'
                exit
            end if
            call split_fields(trim(line), fields, n_fields)
            if (n_fields == 0) then
                message = 'generation manifest contains an empty record'
                exit
            end if
            select case (trim(fields(1)))
            case ('D')
                if (n_fields /= 7 .or. &
                    got_declarations >= declaration_count) then
                    message = 'generation manifest declaration is malformed'
                    exit
                end if
                got_declarations = got_declarations + 1
                call decode_loaded_declaration(fields, &
                    inventory%declarations(got_declarations), status, message)
                if (status /= 0) exit
            case ('R')
                if (n_fields /= 5) then
                    message = 'generation manifest root mapping is malformed'
                    exit
                end if
                read (fields(2), *, iostat=status) root_index
                if (status /= 0 .or. root_index < 1 .or. &
                    root_index > inventory%root_count) then
                    message = 'generation manifest root index is invalid'
                    exit
                end if
                call append_loaded_alias(inventory%roots(root_index), fields(3), &
                    fields(4), fields(5), status, message)
                if (status /= 0) exit
                got_aliases = got_aliases + 1
            case ('E')
                if (n_fields /= 9 .or. got_entries >= inventory%entry_count) then
                    message = 'generation manifest input entry is malformed'
                    exit
                end if
                got_entries = got_entries + 1
                call decode_loaded_entry(fields, inventory%entries(got_entries), &
                    status, message)
                if (status /= 0) exit
            case default
                message = 'generation manifest contains an unknown record'
                exit
            end select
            if (len_trim(message) /= 0) exit
        end do
        close (unit)
        call delete_tmpfile(trim(temp_path))
        if (len_trim(message) /= 0) return
        if (got_entries /= inventory%entry_count .or. got_aliases == 0 .or. &
            got_declarations /= declaration_count) then
            message = 'generation manifest inventory record counts do not match'
            return
        end if
        if (.not. inventory%valid) then
            message = 'generation manifest records an invalid canonical inventory'
            return
        end if
        do i = 1, inventory%root_count
            if (inventory%roots(i)%alias_count == 0 .or. &
                len_trim(inventory%roots(i)%canonical_alias) == 0) then
                message = 'generation manifest omits a canonical root alias'
                return
            end if
            do j = 1, inventory%roots(i)%alias_count
                if (.not. safe_bundle_path( &
                    trim(inventory%roots(i)%bundle_paths(j)))) then
                    message = 'generation manifest has an unsafe bundle mapping'
                    return
                end if
            end do
        end do
        do i = 1, declaration_count
            status = 0
            do j = 1, inventory%root_count
                do k = 1, inventory%roots(j)%alias_count
                    if (trim(inventory%declarations(i)%root_alias) == &
                            trim(inventory%roots(j)%aliases(k))) &
                        status = status + 1
                end do
            end do
            if (status /= 1 .or. .not. safe_entry_path( &
                    trim(inventory%declarations(i)%relative_path))) then
                message = 'generation manifest declaration has an unknown root or path'
                return
            end if
        end do
        call validate_loaded_aliases(inventory, status, message)
        if (status /= 0) return
        if (metadata%execution_identity /= &
            generation_manifest_execution_identity(metadata, inventory)) then
            message = 'generation manifest execution identity does not match inventory'
            return
        end if
        ierr = 0
    end subroutine generation_manifest_load

    subroutine generation_manifest_materialize(store_root, manifest_id, bundle_root, &
            metadata, inventory, ierr, message)
        character(len=*), intent(in) :: store_root, manifest_id, bundle_root
        type(generation_manifest_metadata_t), intent(out) :: metadata
        type(input_inventory_t), intent(out) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(immutable_store_t) :: store
        character(len=PATH_LEN) :: destination
        integer :: status, i, j, root_index
        logical :: used_clone, already_materialized

        ierr = 1
        message = ''
        if (len_trim(bundle_root) == 0) then
            message = 'generation bundle destination is empty'
            return
        end if
        call generation_manifest_load(store_root, manifest_id, metadata, &
            inventory, status, message)
        if (status /= 0) return
        call immutable_store_init(store, trim(store_root), status)
        if (status /= IMMUTABLE_OK) then
            message = 'cannot initialize immutable generation object store'
            return
        end if
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                if (.not. safe_bundle_path( &
                    trim(inventory%roots(i)%bundle_paths(j)))) then
                    message = 'generation manifest has an escaping bundle root'
                    return
                end if
                call fs_make_dir(join_path(trim(bundle_root), &
                    trim(inventory%roots(i)%bundle_paths(j))))
            end do
        end do
        do i = 1, inventory%entry_count
            call entry_destination(inventory, inventory%entries(i), &
                trim(bundle_root), destination, root_index, status, message)
            if (status /= 0) return
            already_materialized = .false.
            call duplicate_materialized_entry(inventory, i, already_materialized, &
                status, message)
            if (status /= 0) return
            if (already_materialized) cycle
            select case (inventory%entries(i)%kind)
            case (INPUT_DIRECTORY)
                call fs_make_dir(trim(destination))
            case (INPUT_FILE)
                call fs_make_dir(parent_path(trim(destination)))
                call immutable_store_materialize_blob(store, &
                    trim(inventory%entries(i)%content_digest), trim(destination), &
                    inventory%entries(i)%mode, IMMUTABLE_MATERIALIZE_COPY, &
                    used_clone, status)
                if (status /= IMMUTABLE_OK) then
                    message = 'cannot materialize generation input blob: '// &
                        trim(inventory%entries(i)%root_alias)//':'// &
                        trim(inventory%entries(i)%relative_path)
                    return
                end if
            case (INPUT_SYMLINK)
                call fs_make_dir(parent_path(trim(destination)))
                status = int(fo_c_generation_create_link( &
                    trim(destination)//c_null_char, &
                    trim(inventory%entries(i)%link_target)//c_null_char))
                if (status /= 0) then
                    message = 'cannot materialize literal generation symlink: '// &
                        trim(inventory%entries(i)%root_alias)//':'// &
                        trim(inventory%entries(i)%relative_path)
                    return
                end if
            case default
                message = 'generation manifest contains an unsupported input kind'
                return
            end select
        end do
        ierr = 0
    end subroutine generation_manifest_materialize

    subroutine split_fields(line, fields, count)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: fields(:)
        integer, intent(out) :: count
        integer :: start, finish, limit, i

        fields = ''
        count = 0
        limit = len_trim(line)
        start = 1
        do i = 1, limit + 1
            if (i <= limit) then
                if (line(i:i) /= achar(9)) cycle
            end if
            if (count >= size(fields)) return
            finish = i - 1
            count = count + 1
            if (finish >= start) fields(count) = line(start:finish)
            start = i + 1
        end do
    end subroutine split_fields

    subroutine hex_decode(encoded, decoded, ierr)
        character(len=*), intent(in) :: encoded
        character(len=:), allocatable, intent(out) :: decoded
        integer, intent(out) :: ierr
        integer :: i, high, low, value

        ierr = 1
        if (trim(encoded) == '-') then
            allocate (character(len=0) :: decoded)
            ierr = 0
            return
        end if
        if (mod(len_trim(encoded), 2) /= 0) then
            allocate (character(len=0) :: decoded)
            return
        end if
        allocate (character(len=len_trim(encoded) / 2) :: decoded)
        do i = 1, len(decoded)
            high = hex_value(encoded(2 * i - 1:2 * i - 1))
            low = hex_value(encoded(2 * i:2 * i))
            if (high < 0 .or. low < 0) then
                decoded = ''
                return
            end if
            value = high * 16 + low
            decoded(i:i) = achar(value)
        end do
        ierr = 0
    end subroutine hex_decode

    integer function hex_value(character_value)
        character(len=1), intent(in) :: character_value
        integer :: value

        value = iachar(character_value)
        hex_value = -1
        if (value >= iachar('0') .and. value <= iachar('9')) then
            hex_value = value - iachar('0')
        else if (value >= iachar('a') .and. value <= iachar('f')) then
            hex_value = value - iachar('a') + 10
        else if (value >= iachar('A') .and. value <= iachar('F')) then
            hex_value = value - iachar('A') + 10
        end if
    end function hex_value

    subroutine append_loaded_alias(root, encoded_alias, encoded_path, &
            encoded_canonical, ierr, message)
        type(input_root_t), intent(inout) :: root
        character(len=*), intent(in) :: encoded_alias, encoded_path
        character(len=*), intent(in) :: encoded_canonical
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=:), allocatable :: alias, bundle_path
        integer :: i, canonical, status

        ierr = 1
        message = ''
        canonical = -1
        call hex_decode(trim(encoded_alias), alias, status)
        if (status == 0) call hex_decode(trim(encoded_path), bundle_path, status)
        if (status /= 0 .or. len(alias) == 0 .or. &
            index(alias, achar(0)) /= 0 .or. &
            .not. safe_bundle_path(bundle_path)) then
            message = 'generation manifest has an invalid logical root mapping'
            return
        end if
        read (encoded_canonical, *, iostat=status) canonical
        if (status /= 0 .or. (canonical /= 0 .and. canonical /= 1)) then
            message = 'generation manifest canonical root marker is invalid'
            return
        end if
        do i = 1, root%alias_count
            if (trim(root%aliases(i)) == alias) then
                message = 'generation manifest repeats a logical root alias'
                return
            end if
        end do
        if (root%alias_count >= size(root%aliases)) then
            message = 'generation manifest exceeds the supported aliases per root'
            return
        end if
        root%alias_count = root%alias_count + 1
        root%aliases(root%alias_count) = alias
        root%bundle_paths(root%alias_count) = bundle_path
        if (canonical == 1) then
            if (len_trim(root%canonical_alias) /= 0) then
                message = 'generation manifest repeats the canonical root marker'
                return
            end if
            root%canonical_alias = alias
            root%bundle_path = bundle_path
        end if
        ierr = 0
    end subroutine append_loaded_alias

    subroutine decode_loaded_entry(fields, entry, ierr, message)
        character(len=*), intent(in) :: fields(:)
        type(input_entry_t), intent(out) :: entry
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=:), allocatable :: value
        integer :: status, logical_value

        ierr = 1
        message = ''
        entry%kind = 0
        entry%mode = -1
        logical_value = -1
        call hex_decode(trim(fields(2)), value, status)
        if (status /= 0 .or. len(value) == 0) goto 900
        entry%root_alias = value
        call hex_decode(trim(fields(3)), value, status)
        if (status /= 0 .or. .not. safe_entry_path(value)) goto 900
        entry%relative_path = value
        call hex_decode(trim(fields(4)), value, status)
        if (status /= 0 .or. len(value) == 0) goto 900
        entry%role = value
        read (fields(5), *, iostat=status) entry%kind
        if (status /= 0) goto 900
        read (fields(6), *, iostat=status) entry%mode
        if (status /= 0 .or. entry%mode < 0 .or. entry%mode > 511) goto 900
        read (fields(7), *, iostat=status) logical_value
        if (status /= 0 .or. (logical_value /= 0 .and. logical_value /= 1)) &
            goto 900
        entry%writable_at_execution = logical_value == 1
        call hex_decode(trim(fields(8)), value, status)
        if (status /= 0) goto 900
        entry%link_target = value
        if (trim(fields(9)) /= '-') then
            if (len_trim(fields(9)) /= HASH_LEN) goto 900
            entry%content_digest = fields(9)(:HASH_LEN)
        end if
        select case (entry%kind)
        case (INPUT_FILE)
            if (.not. is_hex_digest(trim(entry%content_digest))) goto 900
        case (INPUT_DIRECTORY)
            if (trim(fields(9)) /= '-') goto 900
        case (INPUT_SYMLINK)
            if (len_trim(entry%link_target) == 0 .or. &
                len_trim(fields(9)) /= HASH_LEN .or. &
                index(entry%link_target, achar(0)) /= 0 .or. &
                index(entry%link_target, '/') /= 0 .or. &
                trim(entry%link_target) == '.' .or. &
                trim(entry%link_target) == '..') goto 900
            if (cache_digest([character(len=PATH_LEN) :: &
                'symlink:'//trim(entry%link_target)], 1) /= &
                    entry%content_digest) goto 900
        case default
            goto 900
        end select
        ierr = 0
        return
900     message = 'generation manifest contains an invalid input entry'
    end subroutine decode_loaded_entry

    subroutine decode_loaded_declaration(fields, declaration, ierr, message)
        character(len=*), intent(in) :: fields(:)
        type(input_declaration_t), intent(out) :: declaration
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=:), allocatable :: value
        integer :: status, logical_value

        ierr = 1
        message = ''
        declaration%expected_kind = 0
        declaration%expected_mode = -2
        logical_value = -1
        call hex_decode(trim(fields(2)), value, status)
        if (status /= 0 .or. len(value) == 0 .or. &
            index(value, achar(0)) /= 0) goto 900
        declaration%root_alias = value
        call hex_decode(trim(fields(3)), value, status)
        if (status /= 0 .or. .not. safe_entry_path(value)) goto 900
        declaration%relative_path = value
        call hex_decode(trim(fields(4)), value, status)
        if (status /= 0 .or. len(value) == 0 .or. &
            index(value, achar(0)) /= 0) goto 900
        declaration%role = value
        read (fields(5), *, iostat=status) declaration%expected_kind
        if (status /= 0) goto 900
        read (fields(6), *, iostat=status) declaration%expected_mode
        if (status /= 0 .or. declaration%expected_mode < -1 .or. &
            declaration%expected_mode > 511) goto 900
        read (fields(7), *, iostat=status) logical_value
        if (status /= 0 .or. (logical_value /= 0 .and. logical_value /= 1)) &
            goto 900
        if (declaration%expected_kind /= INPUT_FILE .and. &
            declaration%expected_kind /= INPUT_DIRECTORY .and. &
            declaration%expected_kind /= INPUT_SYMLINK) goto 900
        declaration%writable_at_execution = logical_value == 1
        ierr = 0
        return
900     message = 'generation manifest contains an invalid declaration'
    end subroutine decode_loaded_declaration

    subroutine inventory_source_path(inventory, entry, root_index, path, ierr, &
            message)
        type(input_inventory_t), intent(in) :: inventory
        type(input_entry_t), intent(in) :: entry
        integer, intent(out) :: root_index, ierr
        character(len=*), intent(out) :: path, message
        integer :: i, j

        root_index = 0
        ierr = 1
        path = ''
        message = 'input manifest entry names an unknown root alias'
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                if (trim(inventory%roots(i)%aliases(j)) /= &
                    trim(entry%root_alias)) cycle
                root_index = i
                exit
            end do
            if (root_index /= 0) exit
        end do
        if (root_index == 0) return
        if (len_trim(inventory%roots(root_index)%physical_path) == 0 .or. &
            .not. safe_entry_path(trim(entry%relative_path))) then
            message = 'input manifest entry has an invalid source path'
            return
        end if
        path = join_path(trim(inventory%roots(root_index)%physical_path), &
            trim(entry%relative_path))
        if (len_trim(path) >= len(path)) then
            message = 'input manifest source path exceeds the supported length'
            return
        end if
        ierr = 0
        message = ''
    end subroutine inventory_source_path

    subroutine entry_destination(inventory, entry, bundle_root, destination, &
            root_index, ierr, message)
        type(input_inventory_t), intent(in) :: inventory
        type(input_entry_t), intent(in) :: entry
        character(len=*), intent(in) :: bundle_root
        character(len=*), intent(out) :: destination
        integer, intent(out) :: root_index, ierr
        character(len=*), intent(out) :: message
        integer :: i, j
        character(len=PATH_LEN) :: bundle_path

        destination = ''
        root_index = 0
        ierr = 1
        message = 'generation manifest entry names an unknown root alias'
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                if (trim(inventory%roots(i)%aliases(j)) /= &
                    trim(entry%root_alias)) cycle
                root_index = i
                bundle_path = inventory%roots(i)%bundle_paths(j)
                exit
            end do
            if (root_index /= 0) exit
        end do
        if (root_index == 0) return
        if (.not. safe_bundle_path(trim(bundle_path)) .or. &
            .not. safe_entry_path(trim(entry%relative_path))) then
            message = 'generation manifest contains an escaping logical path'
            return
        end if
        destination = join_path(join_path(bundle_root, trim(bundle_path)), &
            trim(entry%relative_path))
        if (len_trim(destination) >= len(destination)) then
            message = 'generation materialization path exceeds the supported length'
            return
        end if
        ierr = 0
        message = ''
    end subroutine entry_destination

    subroutine duplicate_materialized_entry(inventory, current, duplicate, ierr, &
            message)
        type(input_inventory_t), intent(in) :: inventory
        integer, intent(in) :: current
        logical, intent(out) :: duplicate
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: current_path, prior_path
        integer :: ignored, status, i

        duplicate = .false.
        ierr = 0
        message = ''
        call entry_destination(inventory, inventory%entries(current), '', &
            current_path, ignored, status, message)
        if (status /= 0) then
            ierr = status
            return
        end if
        do i = 1, current - 1
            call entry_destination(inventory, inventory%entries(i), '', &
                prior_path, ignored, status, message)
            if (status /= 0) then
                ierr = status
                return
            end if
            if (trim(prior_path) /= trim(current_path)) cycle
            if (inventory%entries(i)%kind /= inventory%entries(current)%kind .or. &
                inventory%entries(i)%mode /= inventory%entries(current)%mode .or. &
                inventory%entries(i)%content_digest /= &
                    inventory%entries(current)%content_digest .or. &
                inventory%entries(i)%link_target /= &
                    inventory%entries(current)%link_target) then
                ierr = 1
                message = 'logical path has conflicting generation input records'
                return
            end if
            duplicate = .true.
            return
        end do
    end subroutine duplicate_materialized_entry

    subroutine validate_loaded_aliases(inventory, ierr, message)
        type(input_inventory_t), intent(in) :: inventory
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: ignored_path
        integer :: i, j, k, l, root_index, status

        ierr = 1
        message = ''
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                do k = i, inventory%root_count
                    do l = 1, inventory%roots(k)%alias_count
                        if (k == i .and. l <= j) cycle
                        if (trim(inventory%roots(i)%aliases(j)) /= &
                            trim(inventory%roots(k)%aliases(l))) cycle
                        message = 'generation manifest repeats a logical root alias'
                        return
                    end do
                end do
            end do
        end do
        do i = 1, inventory%entry_count
            call entry_destination(inventory, inventory%entries(i), '', &
                ignored_path, root_index, status, message)
            if (status /= 0) return
        end do
        do i = 1, size(inventory%declarations)
            root_index = 0
            do j = 1, inventory%root_count
                do k = 1, inventory%roots(j)%alias_count
                    if (trim(inventory%roots(j)%aliases(k)) /= &
                        trim(inventory%declarations(i)%root_alias)) cycle
                    root_index = j
                    exit
                end do
                if (root_index /= 0) exit
            end do
            if (root_index == 0 .or. &
                .not. safe_entry_path( &
                    trim(inventory%declarations(i)%relative_path))) then
                message = 'generation manifest declaration names an invalid root path'
                return
            end if
        end do
        ierr = 0
    end subroutine validate_loaded_aliases

    function join_path(parent, child) result(path)
        character(len=*), intent(in) :: parent, child
        character(len=:), allocatable :: path

        if (len_trim(child) == 0 .or. trim(child) == '.') then
            path = trim(parent)
        else if (len_trim(parent) == 0) then
            path = trim(child)
        else
            path = trim(parent)//'/'//trim(child)
        end if
    end function join_path

    function parent_path(path) result(parent)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: parent
        integer :: slash

        slash = index(trim(path), '/', back=.true.)
        if (slash <= 0) then
            parent = '.'
        else if (slash == 1) then
            parent = '/'
        else
            parent = path(:slash - 1)
        end if
    end function parent_path

    logical function safe_bundle_path(path)
        character(len=*), intent(in) :: path

        safe_bundle_path = safe_relative_path(path, .true.)
    end function safe_bundle_path

    logical function safe_entry_path(path)
        character(len=*), intent(in) :: path

        safe_entry_path = safe_relative_path(path, .false.)
    end function safe_entry_path

    logical function safe_relative_path(path, allow_dot)
        character(len=*), intent(in) :: path
        logical, intent(in) :: allow_dot
        integer :: start, finish, n

        safe_relative_path = .false.
        n = len_trim(path)
        if (n == 0) return
        if (index(path, achar(0)) /= 0) return
        if (path(1:1) == '/') return
        if (allow_dot .and. trim(path) == '.') then
            safe_relative_path = .true.
            return
        end if
        start = 1
        do
            finish = index(path(start:n), '/')
            if (finish == 0) then
                finish = n + 1
            else
                finish = start + finish - 1
            end if
            if (finish == start) return
            if (path(start:finish - 1) == '.' .or. &
                path(start:finish - 1) == '..') return
            if (finish > n) exit
            start = finish + 1
            if (start > n) return
        end do
        safe_relative_path = .true.
    end function safe_relative_path

    logical function is_hex_digest(digest)
        character(len=*), intent(in) :: digest
        integer :: i

        is_hex_digest = .false.
        if (len_trim(digest) /= HASH_LEN) return
        do i = 1, HASH_LEN
            if (hex_value(digest(i:i)) < 0) return
        end do
        is_hex_digest = .true.
    end function is_hex_digest

    function project_source_root(inventory) result(path)
        type(input_inventory_t), intent(in) :: inventory
        character(len=:), allocatable :: path
        integer :: i, j

        path = ''
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                if (trim(inventory%roots(i)%aliases(j)) /= 'project') cycle
                path = trim(inventory%roots(i)%physical_path)
                return
            end do
        end do
    end function project_source_root

end module fo_generation_manifest
