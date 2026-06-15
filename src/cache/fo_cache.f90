module fo_cache
    use fx_cache, only: cache_t, fx_cache_init => cache_init, &
        fx_cache_has => cache_has, fx_cache_store => cache_store, &
        fx_cache_restore => cache_restore, &
        fx_cache_store_bytes => cache_store_bytes, &
        fx_cache_restore_bytes => cache_restore_bytes
    use fx_hash, only: sha256_file, sha256_string
    use fo_util, only: make_tmpfile, delete_tmpfile
    implicit none
    private
    public :: cache_t, cache_init, cache_lookup, cache_key_for, &
        cache_restore_action, cache_store_action, cache_root, &
        cache_store_root, cache_schema, cache_action_mod_key
    public :: cache_debug_write_action_record
    public :: cache_debug_corrupt_object_payload
    public :: hash_mod_file, HASH_LEN

    integer, parameter :: HASH_LEN = 64
    integer, parameter :: MAX_PARTS = 132
    integer, parameter :: CACHE_SCHEMA_VERSION = 1
    integer, parameter :: MAX_RECORD_BYTES = 8192

contains

    subroutine cache_root(root)
        character(len=*), intent(out) :: root

        character(len=512) :: home

        call get_environment_variable('HOME', home)
        if (len_trim(home) == 0) call get_environment_variable('USERPROFILE', home)
        root = trim(home)//'/.cache/fo'
    end subroutine cache_root

    subroutine cache_store_root(root)
        character(len=*), intent(out) :: root

        character(len=512) :: base

        call cache_root(base)
        root = trim(base)//'/store/v1'
    end subroutine cache_store_root

    subroutine cache_schema(schema)
        character(len=*), intent(out) :: schema

        schema = 'action-output-v1'
    end subroutine cache_schema

    subroutine cache_init(c, ierr)
        type(cache_t), intent(out) :: c
        integer, intent(out) :: ierr

        character(len=512) :: root

        ierr = 0
        call cache_store_root(root)
        call fx_cache_init(c, trim(root))
        if (.not. c%initialized) ierr = 1
    end subroutine cache_init

    function cache_key_for(filename, compiler, flags, dep_keys, &
            n_dep_keys) result(key)
        character(len=*), intent(in) :: filename, compiler, flags
        character(len=HASH_LEN), intent(in) :: dep_keys(:)
        integer, intent(in) :: n_dep_keys
        character(len=HASH_LEN) :: key

        character(len=HASH_LEN) :: file_hash
        integer :: i, n_parts
        character(len=512) :: parts(MAX_PARTS)

        call source_tree_hash(filename, file_hash)

        parts(1) = 'fo-cache-schema-1'
        parts(2) = file_hash
        parts(3) = trim(compiler)
        parts(4) = trim(flags)
        n_parts = 4
        do i = 1, min(n_dep_keys, MAX_PARTS - 4)
            n_parts = n_parts + 1
            parts(n_parts) = dep_keys(i)
        end do
        key = digest_parts(parts, n_parts)
    end function cache_key_for

    subroutine source_tree_hash(filename, hash)
        !! Hash a source file together with every file it (recursively) pulls in
        !! via Fortran `include` statements. Without this, editing an .inc file
        !! leaves the including .f90's content hash unchanged, so the compile
        !! cache serves a stale object and the edit silently never takes effect.
        character(len=*), intent(in) :: filename
        character(len=HASH_LEN), intent(out) :: hash

        character(len=512) :: parts(MAX_PARTS)
        integer :: n_parts

        n_parts = 0
        call accumulate_source_hashes(filename, parts, n_parts, 0)
        if (n_parts == 0) then
            hash = ''
        else
            hash = digest_parts(parts, n_parts)
        end if
    end subroutine source_tree_hash

    recursive subroutine accumulate_source_hashes(filename, parts, n_parts, depth)
        character(len=*), intent(in) :: filename
        character(len=512), intent(inout) :: parts(:)
        integer, intent(inout) :: n_parts
        integer, intent(in) :: depth

        character(len=HASH_LEN) :: fh
        character(len=512) :: line, incfile, dir
        integer :: u, ios, ierr

        if (depth > 16 .or. n_parts >= size(parts)) return

        call sha256_file(filename, fh, ierr)
        if (ierr == 0) then
            n_parts = n_parts + 1
            parts(n_parts) = fh
        end if

        dir = dirname_of(filename)
        open (newunit=u, file=trim(filename), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            call parse_include_path(line, incfile)
            if (len_trim(incfile) == 0) cycle
            if (incfile(1:1) /= '/') incfile = trim(dir)//trim(incfile)
            call accumulate_source_hashes(trim(incfile), parts, n_parts, depth + 1)
        end do
        close (u)
    end subroutine accumulate_source_hashes

    subroutine parse_include_path(line, incfile)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: incfile

        character(len=512) :: t
        character(len=7) :: head
        character(len=1) :: qc
        integer :: i, q1, q2

        incfile = ''
        t = adjustl(line)
        if (len_trim(t) < 9) return
        head = t(1:7)
        do i = 1, 7
            if (head(i:i) >= 'A' .and. head(i:i) <= 'Z') &
                head(i:i) = achar(iachar(head(i:i)) + 32)
        end do
        if (head /= 'include') return
        if (t(8:8) /= ' ' .and. t(8:8) /= '''' .and. t(8:8) /= '"') return
        q1 = scan(t, '''"')
        if (q1 == 0) return
        qc = t(q1:q1)
        q2 = index(t(q1 + 1:), qc)
        if (q2 == 0) return
        incfile = t(q1 + 1:q1 + q2 - 1)
    end subroutine parse_include_path

    function dirname_of(path) result(d)
        character(len=*), intent(in) :: path
        character(len=512) :: d
        integer :: s

        s = index(path, '/', back=.true.)
        if (s == 0) then
            d = './'
        else
            d = path(1:s)
        end if
    end function dirname_of

    function cache_lookup(c, key) result(hit)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        logical :: hit

        character(len=HASH_LEN) :: out_id, object_key, mod_key
        character(len=MAX_PARTS) :: mod_label
        integer :: ierr, obj_size, mod_size, n_rec, i
        logical :: has_mod
        character(len=1) :: rec_bytes(MAX_RECORD_BYTES)
        character(len=MAX_RECORD_BYTES) :: rec_text

        hit = .false.
        if (.not. fx_cache_has(c, trim(key)//'-a')) return

        ! Restore and parse the action record in memory. cache_lookup runs from
        ! an OpenMP parallel region; the previous temp-file round-trip
        ! (make_tmpfile + restore-to-file + read + delete) raced under
        ! concurrency and corrupted results. In-memory restore touches no
        ! per-thread scratch files.
        call fx_cache_restore_bytes(c, trim(key)//'-a', rec_bytes, n_rec, ierr)
        if (ierr /= 0 .or. n_rec <= 0 .or. n_rec > MAX_RECORD_BYTES) return
        rec_text = ''
        do i = 1, n_rec
            rec_text(i:i) = rec_bytes(i)
        end do
        call parse_action_record(rec_text(1:n_rec), out_id, object_key, &
            obj_size, mod_label, mod_key, mod_size, &
            has_mod, ierr)
        if (ierr /= 0) return
        hit = fx_cache_has(c, trim(out_id)//'-d') .and. &
            fx_cache_has(c, trim(object_key)//'-d')
        if (hit .and. has_mod) hit = fx_cache_has(c, trim(mod_key)//'-d')
    end function cache_lookup

    subroutine cache_restore_action(c, action_id, obj_path, mod_dir, restored, &
            output_id)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, obj_path, mod_dir
        logical, intent(out) :: restored
        character(len=HASH_LEN), intent(out), optional :: output_id

        character(len=512) :: record_path
        character(len=HASH_LEN) :: out_id, object_key, mod_key
        character(len=MAX_PARTS) :: mod_label
        integer :: ierr, obj_size, mod_size
        logical :: has_mod, local_ok

        restored = .false.
        if (present(output_id)) output_id = ''
        if (.not. c%initialized) return
        if (.not. fx_cache_has(c, trim(action_id)//'-a')) return

        call make_tmpfile('fo_action_record', record_path)
        call fx_cache_restore(c, trim(action_id)//'-a', record_path, ierr)
        if (ierr /= 0) then
            call delete_tmpfile(record_path)
            return
        end if

        call read_action_record(record_path, out_id, object_key, obj_size, &
            mod_label, mod_key, mod_size, has_mod, ierr)
        call delete_tmpfile(record_path)
        if (ierr /= 0) return
        if (present(output_id)) output_id = out_id
        if (.not. fx_cache_has(c, trim(out_id)//'-d')) return

        local_ok = local_outputs_match(obj_path, mod_dir, mod_label, object_key, &
            mod_key, has_mod)
        if (local_ok) then
            restored = .true.
            return
        end if

        call fx_cache_restore(c, trim(object_key)//'-d', obj_path, ierr)
        if (ierr /= 0) return
        if (has_mod) then
            call fx_cache_restore(c, trim(mod_key)//'-d', &
                trim(mod_dir)//'/'//trim(mod_label)//'.mod', &
                ierr)
            if (ierr /= 0) return
        end if

        restored = local_outputs_match(obj_path, mod_dir, mod_label, object_key, &
            mod_key, has_mod)
    end subroutine cache_restore_action

    subroutine cache_store_action(c, action_id, obj_path, mod_dir, mod_name, &
            output_id, ierr)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: action_id, obj_path, mod_dir, mod_name
        character(len=HASH_LEN), intent(out) :: output_id
        integer, intent(out) :: ierr

        character(len=HASH_LEN) :: object_key, mod_key
        character(len=512) :: parts(4)
        character(len=512) :: record_path, mod_path, lower_name
        character(len=1) :: marker(1)
        integer :: obj_size, mod_size, u, ios, store_ierr
        logical :: has_mod

        output_id = ''
        ierr = 0
        if (.not. c%initialized) then
            ierr = 1
            return
        end if

        call file_content_key(obj_path, 'object', object_key, obj_size, ierr)
        if (ierr /= 0) return

        lower_name = lowercase(trim(mod_name))
        mod_path = trim(mod_dir)//'/'//trim(lower_name)//'.mod'
        inquire (file=trim(mod_path), exist=has_mod)
        mod_key = ''
        mod_size = 0
        if (has_mod) then
            call file_content_key(mod_path, 'mod', mod_key, mod_size, ierr)
            if (ierr /= 0) return
        end if

        parts(1) = 'fo-output-schema-1'
        parts(2) = object_key
        parts(3) = mod_key
        parts(4) = trim(lower_name)
        output_id = digest_parts(parts, 4)

        call fx_cache_store(c, trim(object_key)//'-d', obj_path, store_ierr)
        if (store_ierr /= 0) then
            ierr = store_ierr
            return
        end if
        if (has_mod) then
            call fx_cache_store(c, trim(mod_key)//'-d', mod_path, store_ierr)
            if (store_ierr /= 0) then
                ierr = store_ierr
                return
            end if
        end if

        marker(1) = '1'
        call fx_cache_store_bytes(c, trim(output_id)//'-d', marker, 1, ierr)
        if (ierr /= 0) return

        call make_tmpfile('fo_action_record', record_path)
        open (newunit=u, file=trim(record_path), status='replace', iostat=ios)
        if (ios /= 0) then
            ierr = 1
            call delete_tmpfile(record_path)
            return
        end if
        write (u, '(a,i0)') 'schema ', CACHE_SCHEMA_VERSION
        write (u, '(a)') 'kind compile'
        write (u, '(a,a)') 'output ', trim(output_id)
        write (u, '(a,a,1x,i0)') 'object ', trim(object_key), obj_size
        if (has_mod) write (u, '(a,a,1x,a,1x,i0)') 'mod ', trim(lower_name), &
            trim(mod_key), mod_size
        close (u)

        call fx_cache_store(c, trim(action_id)//'-a', record_path, ierr)
        call delete_tmpfile(record_path)
    end subroutine cache_store_action

    subroutine hash_mod_file(modpath, key)
        character(len=*), intent(in) :: modpath
        character(len=HASH_LEN), intent(out) :: key

        integer :: size_bytes, ierr

        call file_content_key(modpath, 'mod', key, size_bytes, ierr)
        if (ierr /= 0) key = ''
    end subroutine hash_mod_file

    subroutine cache_action_mod_key(c, action_id, mod_key, found)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN), intent(out) :: mod_key
        logical, intent(out) :: found

        character(len=512) :: record_path
        character(len=HASH_LEN) :: out_id, object_key
        character(len=MAX_PARTS) :: mod_label
        integer :: ierr, obj_size, mod_size
        logical :: has_mod

        mod_key = ''
        found = .false.
        if (.not. c%initialized) return
        if (.not. fx_cache_has(c, trim(action_id)//'-a')) return

        call make_tmpfile('fo_action_mod_key', record_path)
        call fx_cache_restore(c, trim(action_id)//'-a', record_path, ierr)
        if (ierr /= 0) then
            call delete_tmpfile(record_path)
            return
        end if
        call read_action_record(record_path, out_id, object_key, obj_size, &
            mod_label, mod_key, mod_size, has_mod, ierr)
        call delete_tmpfile(record_path)
        if (ierr /= 0 .or. .not. has_mod) return
        found = fx_cache_has(c, trim(mod_key)//'-d')
        if (.not. found) mod_key = ''
    end subroutine cache_action_mod_key

    subroutine cache_debug_write_action_record(c, action_id, record_text, ierr)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: action_id, record_text
        integer, intent(out) :: ierr

        character(len=1), allocatable :: bytes(:)
        integer :: i, n

        ierr = 0
        n = len_trim(record_text)
        allocate (bytes(max(n, 0)))
        do i = 1, n
            bytes(i) = record_text(i:i)
        end do
        call fx_cache_store_bytes(c, trim(action_id)//'-a', bytes, n, ierr)
    end subroutine cache_debug_write_action_record

    subroutine cache_debug_corrupt_object_payload(c, action_id, ierr)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: action_id
        integer, intent(out) :: ierr

        character(len=512) :: record_path
        character(len=HASH_LEN) :: out_id, object_key, mod_key
        character(len=MAX_PARTS) :: mod_label
        character(len=1) :: bad(7)
        integer :: obj_size, mod_size, i
        logical :: has_mod

        ierr = 1
        if (.not. c%initialized) return
        call make_tmpfile('fo_action_corrupt', record_path)
        call fx_cache_restore(c, trim(action_id)//'-a', record_path, ierr)
        if (ierr /= 0) then
            call delete_tmpfile(record_path)
            return
        end if
        call read_action_record(record_path, out_id, object_key, obj_size, &
            mod_label, mod_key, mod_size, has_mod, ierr)
        call delete_tmpfile(record_path)
        if (ierr /= 0) return
        bad = ''
        do i = 1, size(bad)
            bad(i) = achar(iachar('0') + modulo(i, 10))
        end do
        call fx_cache_store_bytes(c, trim(object_key)//'-d', bad, size(bad), ierr)
    end subroutine cache_debug_corrupt_object_payload

    subroutine file_content_key(path, kind, key, size_bytes, ierr)
        character(len=*), intent(in) :: path, kind
        character(len=HASH_LEN), intent(out) :: key
        integer, intent(out) :: size_bytes, ierr

        character(len=HASH_LEN) :: hex
        character(len=512) :: parts(3)

        call sha256_file(path, hex, ierr)
        if (ierr /= 0) then
            key = ''
            size_bytes = 0
            return
        end if
        call file_size(path, size_bytes)
        parts(1) = 'fo-payload-schema-1'
        parts(2) = trim(kind)
        parts(3) = hex
        key = digest_parts(parts, 3)
    end subroutine file_content_key

    function digest_parts(parts, n_parts) result(key)
        character(len=*), intent(in) :: parts(:)
        integer, intent(in) :: n_parts
        character(len=HASH_LEN) :: key

        character(len=:), allocatable :: text
        character(len=32) :: len_text
        integer :: i

        text = ''
        do i = 1, n_parts
            write (len_text, '(i0)') len_trim(parts(i))
            text = text//trim(len_text)//':'//trim(parts(i))//achar(10)
        end do
        key = sha256_string(text)
    end function digest_parts

    logical function local_outputs_match(obj_path, mod_dir, mod_name, object_key, &
            mod_key, has_mod) result(ok)
        character(len=*), intent(in) :: obj_path, mod_dir, mod_name
        character(len=*), intent(in) :: object_key, mod_key
        logical, intent(in) :: has_mod

        character(len=HASH_LEN) :: actual_key
        integer :: size_bytes, ierr

        ok = .false.
        call file_content_key(obj_path, 'object', actual_key, size_bytes, ierr)
        if (ierr /= 0 .or. trim(actual_key) /= trim(object_key)) return
        if (has_mod) then
            call file_content_key(trim(mod_dir)//'/'//trim(mod_name)//'.mod', &
                'mod', actual_key, size_bytes, ierr)
            if (ierr /= 0 .or. trim(actual_key) /= trim(mod_key)) return
        end if
        ok = .true.
    end function local_outputs_match

    subroutine read_action_record(path, output_id, object_key, obj_size, mod_name, &
            mod_key, mod_size, has_mod, ierr)
        character(len=*), intent(in) :: path
        character(len=HASH_LEN), intent(out) :: output_id, object_key, mod_key
        character(len=*), intent(out) :: mod_name
        integer, intent(out) :: obj_size, mod_size, ierr
        logical, intent(out) :: has_mod

        character(len=MAX_RECORD_BYTES) :: text
        character(len=512) :: line
        integer :: u, ios, n

        ierr = 1
        text = ''
        n = 0
        open (newunit=u, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (n + len_trim(line) + 1 > MAX_RECORD_BYTES) exit
            text(n + 1:n + len_trim(line)) = line(1:len_trim(line))
            n = n + len_trim(line)
            text(n + 1:n + 1) = new_line('a')
            n = n + 1
        end do
        close (u)

        call parse_action_record(text(1:n), output_id, object_key, obj_size, &
            mod_name, mod_key, mod_size, has_mod, ierr)
    end subroutine read_action_record

    subroutine parse_action_record(text, output_id, object_key, obj_size, &
            mod_name, mod_key, mod_size, has_mod, ierr)
        !! Parse a newline-separated action record from memory. Shared by the
        !! file-based read_action_record and the in-memory cache_lookup path so
        !! both agree on the record grammar.
        character(len=*), intent(in) :: text
        character(len=HASH_LEN), intent(out) :: output_id, object_key, mod_key
        character(len=*), intent(out) :: mod_name
        integer, intent(out) :: obj_size, mod_size, ierr
        logical, intent(out) :: has_mod

        character(len=512) :: line, tag
        integer :: ios, schema, p, q, n

        output_id = ''
        object_key = ''
        mod_key = ''
        mod_name = ''
        obj_size = 0
        mod_size = 0
        has_mod = .false.
        ierr = 1
        schema = -1

        n = len(text)
        p = 1
        do while (p <= n)
            q = index(text(p:n), new_line('a'))
            if (q == 0) then
                line = text(p:n)
                p = n + 1
            else
                line = text(p:p + q - 2)
                p = p + q
            end if
            if (len_trim(line) == 0) cycle
            read (line, *, iostat=ios) tag
            if (ios /= 0) cycle
            select case (trim(tag))
            case ('schema')
                read (line, *, iostat=ios) tag, schema
            case ('output')
                read (line, *, iostat=ios) tag, output_id
            case ('object')
                read (line, *, iostat=ios) tag, object_key, obj_size
            case ('mod')
                read (line, *, iostat=ios) tag, mod_name, mod_key, mod_size
                if (ios == 0) has_mod = .true.
            end select
        end do

        if (schema /= CACHE_SCHEMA_VERSION) return
        if (len_trim(output_id) == 0 .or. len_trim(object_key) == 0) return
        ierr = 0
    end subroutine parse_action_record

    subroutine file_size(path, size_bytes)
        character(len=*), intent(in) :: path
        integer, intent(out) :: size_bytes

        character(len=4096) :: cmd
        character(len=512) :: tmpfile, text
        integer :: u, ios

        size_bytes = 0
        call make_tmpfile('fo_size', tmpfile)
        cmd = 'wc -c < '//sq(trim(path))//' > '//sq(trim(tmpfile))
        call execute_command_line(trim(cmd), wait=.true.)
        open (newunit=u, file=trim(tmpfile), status='old', iostat=ios)
        if (ios == 0) then
            read (u, '(a)', iostat=ios) text
            if (ios == 0) read (text, *, iostat=ios) size_bytes
            close (u)
        end if
        call delete_tmpfile(tmpfile)
    end subroutine file_size

    pure function lowercase(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len_trim(s)) :: out
        integer :: i

        out = s(1:len_trim(s))
        do i = 1, len_trim(out)
            if (out(i:i) >= 'A' .and. out(i:i) <= 'Z') then
                out(i:i) = achar(iachar(out(i:i)) + 32)
            end if
        end do
    end function lowercase

    pure function sq(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len_trim(s) + 2) :: r
        r = "'"//trim(s)//"'"
    end function sq

end module fo_cache
