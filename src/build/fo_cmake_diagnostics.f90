module fo_cmake_diagnostics
    !! Bounded configure evidence, copied before disposable build views vanish.
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_cache, only: cache_file_digest, HASH_LEN
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_realpath
    use fx_json_build, only: json_escape_string
    use fx_action_result_store, only: action_result_file_mode, ACTION_RESULT_OK
    implicit none
    private
    integer, parameter :: MAX_MEMBERS = 64, LOG_BYTES = 262144
    integer, parameter :: CACHE_BYTES = 2097152, TOTAL_BYTES = 8388608
    type, public :: cmake_diagnostic_t
        character(:), allocatable :: root, cwd, build, command, preload, rows
        character(len=HASH_LEN) :: preload_sha = ''
        character(len=32) :: started = ''
        integer :: count = 0, retained_bytes = 0, omitted = 0
        logical :: active = .false.
    end type cmake_diagnostic_t
    public :: cmake_diagnostic_begin, cmake_diagnostic_finish
    public :: cmake_diagnostic_retain
    interface
        integer(c_int) function diagnostic_files(root, output, capacity, limit, &
                json_only, count) bind(C, name='fo_cmake_diagnostic_files')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            character(kind=c_char), intent(out) :: output(*)
            integer(c_int), value :: capacity, limit, json_only
            integer(c_int), intent(out) :: count
        end function diagnostic_files
    end interface
contains
    subroutine cmake_diagnostic_begin(d, cwd, build, packed, n_args, log_file, preload)
        type(cmake_diagnostic_t), intent(out) :: d
        character(len=*), intent(in) :: cwd, build, packed, log_file, preload
        integer, intent(in) :: n_args
        integer :: i, first, count, status
        character(len=HASH_LEN) :: digest
        logical :: exists

        d%root = log_file//'.cmake-diagnostics'
        d%cwd = cwd
        d%build = build
        d%preload = preload
        d%rows = ''
        d%command = '['
        first = 1
        count = 0
        do i = 1, len(packed)
            if (packed(i:i) /= achar(0)) cycle
            if (count > 0) d%command = d%command//','
            d%command = d%command//quote(packed(first:i - 1))
            count = count + 1
            first = i + 1
            if (count == n_args) exit
        end do
        d%command = d%command//']'
        call timestamp(d%started)
        inquire(file=d%root, exist=exists)
        if (exists) return
        call fs_make_dir(d%root, status)
        if (status /= 0) return
        d%active = .true.
        if (len(preload) > 0) then
            call capture(d, preload, 'preload-before', .true., digest)
            d%preload_sha = digest
        end if
    end subroutine cmake_diagnostic_begin

    subroutine cmake_diagnostic_finish(d, exitcode, log_file, configure_exit)
        type(cmake_diagnostic_t), intent(inout) :: d
        integer, intent(in) :: exitcode
        integer, intent(in), optional :: configure_exit
        character(len=*), intent(in) :: log_file
        character(len=4096) :: paths(MAX_MEMBERS)
        character(len=HASH_LEN) :: post_sha
        character(len=32) :: finished
        character(:), allocatable :: text
        integer :: count, i, status, unit, ios, process_exit

        if (.not. d%active) return
        process_exit = exitcode
        if (present(configure_exit)) process_exit = configure_exit
        if (exitcode == 0) then
            call fs_remove_tree(d%root, status)
            return
        end if
        post_sha = ''
        if (len(d%preload) > 0) then
            call capture(d, d%preload, 'preload-after', .true., post_sha)
        end if
        call capture(d, log_file, 'configure-output', .false.)
        call capture(d, d%build//'/CMakeCache.txt', 'configured-cache', .true.)
        call capture(d, d%build//'/CMakeFiles/CMakeError.log', 'configure-error', .false.)
        call capture(d, d%build//'/CMakeFiles/CMakeOutput.log', 'configure-output-log', .false.)
        call capture(d, d%build//'/CMakeFiles/CMakeConfigureLog.yaml', 'configure-events', .false.)
        call bounded_files(d%build//'/.cmake/api/v1/reply', paths, &
            MAX_MEMBERS - d%count, .true., count, status)
        if (status == 0 .or. status == 1) then
            do i = 1, count
                call capture(d, trim(paths(i)), 'file-api-reply', .false.)
            end do
        end if
        text = '{"schema":"fo-cmake-configure-diagnostics-v1",'// &
            '"cwd":'//quote(d%cwd)//',"build_root":'//quote(d%build)// &
            ',"argv":'//d%command//',"command_selected_preload":'//quote(d%preload)// &
            ',"preload_before_sha256":'//quote(trim(d%preload_sha))// &
            ',"preload_after_sha256":'//quote(trim(post_sha))// &
            ',"preload_unchanged":'//boolean(len_trim(post_sha) > 0 .and. &
                post_sha == d%preload_sha)// &
            ',"started":'//quote(trim(d%started))//',"exit_code":'//number(process_exit)// &
            ',"gate_exit_code":'//number(exitcode)// &
            ',"capture_stage":"after-configure-before-view-release",'// &
            '"loader_observation":"raw configure-output member only",'// &
            '"file_api_collection_status":'//number(status)// &
            ',"omitted_member_requests":'//number(d%omitted)// &
            ',"limits":{"members":64,"total_bytes":8388608,'// &
            '"log_member_bytes":262144,"complete_cache_bytes":2097152}'
        call timestamp(finished)
        text = text//',"finished":'//quote(trim(finished))//',"members":['//d%rows//']}'
        open(newunit=unit, file=d%root//'/manifest.json', status='replace', &
            access='stream', form='unformatted', action='write', iostat=ios)
        if (ios == 0) then
            write(unit, iostat=ios) text
            close(unit)
        end if
    end subroutine cmake_diagnostic_finish

    subroutine capture(d, path, role, complete, full_digest)
        type(cmake_diagnostic_t), intent(inout) :: d
        character(len=*), intent(in) :: path, role
        logical, intent(in) :: complete
        character(len=*), intent(out), optional :: full_digest
        character(:), allocatable :: content, member, state
        character(len=HASH_LEN) :: digest
        integer(int64) :: size_bytes
        integer :: source, target, ios, kept, cap, mode, status
        character(len=4096) :: physical
        logical :: exists, resolved

        if (present(full_digest)) full_digest = ''
        if (d%count >= MAX_MEMBERS) then
            d%omitted = d%omitted + 1
            return
        end if
        d%count = d%count + 1
        member = 'member-'//number(d%count)//'.bin'
        state = 'absent'
        digest = ''
        physical = ''
        kept = 0
        size_bytes = 0
        inquire(file=path, exist=exists, size=size_bytes, iostat=ios)
        if (ios /= 0) then
            state = 'stat-error'
        else if (exists) then
            call fs_realpath(path, physical, resolved)
            call action_result_file_mode(path, mode, status)
            if (.not. resolved .or. status /= ACTION_RESULT_OK) then
                state = 'not-regular'
                call append_row()
                return
            end if
            cap = LOG_BYTES
            if (complete) cap = CACHE_BYTES
            if (size_bytes < 0 .or. (complete .and. size_bytes > cap)) then
                state = 'size-limit'
            else
                kept = int(min(size_bytes, int(cap, int64)))
                kept = min(kept, TOTAL_BYTES - d%retained_bytes)
                if (complete .and. int(kept, int64) /= size_bytes) then
                    state = 'total-limit'
                    kept = 0
                else
                    allocate(character(len=kept) :: content)
                    open(newunit=source, file=path, status='old', action='read', &
                        access='stream', form='unformatted', iostat=ios)
                    if (ios == 0) then
                        if (kept > 0) read(source, pos=max(1_int64, &
                            size_bytes - kept + 1_int64), iostat=ios) content
                        close(source)
                    end if
                    if (ios == 0) then
                        open(newunit=target, file=d%root//'/'//member, status='new', &
                            access='stream', form='unformatted', action='write', iostat=ios)
                        if (ios == 0) then
                            write(target, iostat=ios) content
                            close(target)
                        end if
                    end if
                    state = 'io-error'
                    if (ios == 0) then
                        state = 'complete'
                        if (int(kept, int64) < size_bytes) state = 'truncated-tail'
                        call cache_file_digest(d%root//'/'//member, digest)
                        d%retained_bytes = d%retained_bytes + kept
                        if (present(full_digest) .and. state == 'complete') &
                            full_digest = digest
                    end if
                end if
            end if
        end if
        call append_row()
    contains
        subroutine append_row()
        if (len(d%rows) > 0) d%rows = d%rows//','
        d%rows = d%rows//'{"original_path":'//quote(path)// &
            ',"resolved_path":'//quote(trim(physical))//',"role":'//quote(role)// &
            ',"member":'//quote(member)//',"status":'//quote(state)// &
            ',"original_bytes":'//number64(size_bytes)//',"retained_bytes":'// &
            number(kept)//',"retained_sha256":'//quote(trim(digest))//'}'
        end subroutine append_row
    end subroutine capture

    subroutine cmake_diagnostic_retain(source_log, lane_log, generation, session, view)
        character(len=*), intent(in) :: source_log, lane_log, generation, session, view
        character(len=4096) :: paths(MAX_MEMBERS + 1)
        character(len=HASH_LEN) :: digest
        character(:), allocatable :: root, destination, content, member, copies
        integer :: count, i, status, source, target, ios, size_bytes
        logical :: exists

        copies = ''
        root = source_log//'.cmake-diagnostics'
        inquire(file=root//'/manifest.json', exist=exists)
        if (.not. exists) return
        destination = lane_log//'.cmake-diagnostics'
        call fs_make_dir(destination, status)
        if (status /= 0) return
        call bounded_files(root, paths, MAX_MEMBERS + 1, .false., count, status)
        if (status < 0 .or. status == 2) return
        do i = 1, count
            member = trim(paths(i)(len(root) + 2:))
            if (member /= 'manifest.json' .and. index(member, 'member-') /= 1) cycle
            inquire(file=trim(paths(i)), size=size_bytes, iostat=ios)
            if (ios /= 0 .or. size_bytes < 0 .or. size_bytes > CACHE_BYTES) cycle
            allocate(character(len=size_bytes) :: content)
            open(newunit=source, file=trim(paths(i)), access='stream', &
                form='unformatted', status='old', action='read', iostat=ios)
            if (ios == 0) then
                read(source, iostat=ios) content
                close(source)
            end if
            if (ios == 0) then
                open(newunit=target, file=destination//'/'//member, access='stream', &
                    form='unformatted', status='new', action='write', iostat=ios)
                if (ios == 0) then
                    write(target, iostat=ios) content
                    close(target)
                end if
            end if
            digest = ''
            if (ios == 0) call cache_file_digest(destination//'/'//member, digest)
            if (len(copies) > 0) copies = copies//','
            copies = copies//'{"member":'//quote(member)//',"copy_iostat":'// &
                number(ios)//',"bytes":'//number(size_bytes)// &
                ',"sha256":'//quote(trim(digest))//'}'
            deallocate(content)
        end do
        call cache_file_digest(destination//'/manifest.json', digest)
        content = '{"schema":"fo-cmake-diagnostic-owner-v1","generation":'// &
            quote(generation)//',"session":'//quote(session)//',"view_root":'// &
            quote(view)//',"lane_build_log":'//quote(lane_log)// &
            ',"original_configure_log":'//quote(source_log)// &
            ',"manifest_sha256":'//quote(trim(digest))// &
            ',"retained_members":['//copies//']}'
        open(newunit=target, file=destination//'/owner.json', access='stream', &
            form='unformatted', status='new', action='write', iostat=ios)
        if (ios == 0) then
            write(target, iostat=ios) content
            close(target)
        end if
    end subroutine cmake_diagnostic_retain

    subroutine bounded_files(root, paths, limit, json_only, count, status)
        character(len=*), intent(in) :: root
        character(len=*), intent(out) :: paths(:)
        integer, intent(in) :: limit
        logical, intent(in) :: json_only
        integer, intent(out) :: count, status
        character(kind=c_char) :: buffer((MAX_MEMBERS + 1)*4097)
        integer(c_int) :: found
        integer :: i, first, row, json_flag
        buffer = c_null_char
        json_flag = 0
        if (json_only) json_flag = 1
        status = diagnostic_files(root//c_null_char, buffer, int(size(buffer), c_int), &
            int(min(limit, size(paths)), c_int), int(json_flag, c_int), found)
        count = int(found)
        paths = ''
        row = 1
        first = 1
        do i = 1, size(buffer)
            if (row > count) exit
            if (buffer(i) /= c_null_char) cycle
            block
                integer :: j
                do j = first, i - 1
                    paths(row)(j - first + 1:j - first + 1) = buffer(j)
                end do
            end block
            row = row + 1
            first = i + 1
        end do
    end subroutine bounded_files

    function quote(text) result(value)
        character(len=*), intent(in) :: text
        character(:), allocatable :: value
        value = '"'//json_escape_string(text)//'"'
    end function quote

    function number(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        text = number64(int(value, int64))
    end function number

    function number64(value) result(text)
        integer(int64), intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: buffer
        write(buffer, '(i0)') value
        text = trim(buffer)
    end function number64

    function boolean(value) result(text)
        logical, intent(in) :: value
        character(:), allocatable :: text
        text = 'false'
        if (value) text = 'true'
    end function boolean

    subroutine timestamp(text)
        character(len=*), intent(out) :: text
        character(len=8) :: date
        character(len=10) :: time
        character(len=5) :: zone
        call date_and_time(date, time, zone)
        text = date//'T'//time//zone
    end subroutine timestamp
end module fo_cmake_diagnostics
