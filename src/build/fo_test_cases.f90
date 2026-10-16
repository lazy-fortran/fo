module fo_test_cases
    !! Cases inside a test executable (#217): the listing protocol, its
    !! adapters and the cache of an executable's case list.
    !!
    !! fo protocol (any executable that reads the variables below):
    !!  * FO_TEST_LIST=1: print one `fo-case NAME` line per case (NAME has no
    !!    blank, control character or `;`; an optional TAB and a `;`-separated
    !!    list of project-relative sources follow it: the files the case's own
    !!    code lives in, which narrow that case's input closure) and exit 0.
    !!    Other output lines are ignored.
    !!  * FO_TEST_CASE=NAME: run only that case.
    !! Adapters need no change of the executable; only test-drive has a
    !! listing and selection command line that was checked against its source
    !! (`exe SUITE TEST`; an unknown suite or test prints the available names
    !! to standard error and exits non-zero). veggies, pFUnit and Fortuno are
    !! not adapted: their listing and filter options could not be verified
    !! here, and an unverified adapter would run suites as a side effect of
    !! listing. Their executables stay one case, or implement the fo protocol.
    !!
    !! An executable without a protocol, or whose listing fails, is one case
    !! with a reason. The list is cached beside the executable, keyed by its
    !! content digest, so listing runs once per build of the executable.
    use fx_string, only: string_t
    use fo_cache, only: HASH_LEN
    use fo_fs, only: fs_rename
    use fo_pattern, only: pattern_glob
    use fo_process, only: process_run_argv_logged, argv_push, process_getpid
    use fo_stat_memo, only: memo_hash_file
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file_alloc
    implicit none
    private

    character(len=*), parameter, public :: CASE_SEP = '::'
    integer, parameter, public :: ADAPTER_NONE = 0, ADAPTER_FO = 1, &
        ADAPTER_TESTDRIVE = 2
    integer, parameter :: LIST_TIMEOUT_S = 60
    !! The reason of an executable that speaks no listing protocol: one case,
    !! nothing to report.
    character(len=*), parameter, public :: NO_PROTOCOL_REASON = 'no listing protocol'

    type, public :: case_listing_t
        integer :: adapter = ADAPTER_NONE
        !! Why the executable is one case (empty when it listed cases).
        character(len=:), allocatable :: reason
        type(string_t), allocatable :: names(:)
        !! Per case: `;`-separated project-relative sources, or empty.
        type(string_t), allocatable :: sources(:)
    end type case_listing_t

    public :: case_list, case_split, case_matches, case_run_setup
    public :: case_valid_name, case_filter_add

contains

    subroutine case_split(name, exe, case_name)
        !! name = exe // '::' // case_name; case_name is empty without '::'.
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: exe, case_name
        integer :: p

        p = index(name, CASE_SEP)
        if (p == 0) then
            exe = trim(name)
            case_name = ''
        else
            exe = name(1:p - 1)
            case_name = trim(name(p + len(CASE_SEP):))
        end if
    end subroutine case_split

    logical function case_valid_name(name) result(ok)
        character(len=*), intent(in) :: name
        integer :: k

        ok = len(name) > 0
        do k = 1, len(name)
            if (iachar(name(k:k)) <= 32 .or. iachar(name(k:k)) == 127 .or. &
                name(k:k) == ';') ok = .false.
        end do
    end function case_valid_name

    subroutine case_filter_add(filter, pattern)
        !! Filters are newline-joined glob patterns of one executable.
        character(len=:), allocatable, intent(inout) :: filter
        character(len=*), intent(in) :: pattern

        if (.not. allocated(filter)) filter = ''
        if (len(filter) > 0) then
            if (index(new_line('a')//filter//new_line('a'), &
                new_line('a')//pattern//new_line('a')) > 0) return
            filter = filter//new_line('a')
        end if
        filter = filter//pattern
    end subroutine case_filter_add

    logical function case_matches(filter, case_name) result(hit)
        !! Whether case_name satisfies a newline-joined glob filter; an empty
        !! filter matches every case.
        character(len=*), intent(in) :: filter, case_name
        integer :: pos, next

        hit = len(filter) == 0
        pos = 1
        do while (.not. hit .and. pos <= len(filter))
            next = index(filter(pos:), new_line('a'))
            if (next == 0) next = len(filter) - pos + 2
            hit = pattern_glob(filter(pos:pos + next - 2), case_name)
            pos = pos + next
        end do
    end function case_matches

    subroutine case_run_setup(adapter, case_name, args, env)
        !! Extra argv lines (newline-joined) and per-process environment
        !! assignments (`;`-joined) that make the executable run one case.
        integer, intent(in) :: adapter
        character(len=*), intent(in) :: case_name
        character(len=:), allocatable, intent(out) :: args, env
        character(len=:), allocatable :: suite, test
        integer :: p

        args = ''
        env = ''
        select case (adapter)
        case (ADAPTER_FO)
            env = 'FO_TEST_CASE='//case_name
        case (ADAPTER_TESTDRIVE)
            p = index(case_name, CASE_SEP)
            suite = case_name(1:p - 1)
            test = case_name(p + len(CASE_SEP):)
            args = suite//new_line('a')//test
        end select
    end subroutine case_run_setup

    subroutine case_list(bin, cwd, listing)
        !! The case list of the built executable bin, run from cwd. Cached
        !! beside the executable by its content digest.
        character(len=*), intent(in) :: bin, cwd
        type(case_listing_t), intent(out) :: listing
        character(len=HASH_LEN) :: digest
        character(len=:), allocatable :: binary, sidecar
        integer :: ierr

        allocate (listing%names(0), listing%sources(0))
        listing%reason = NO_PROTOCOL_REASON
        call memo_hash_file(bin, digest, ierr)
        if (ierr /= 0) then
            listing%reason = 'cannot read the executable'
            return
        end if
        sidecar = cache_path(bin)
        if (cache_read(sidecar, digest, listing)) return
        call read_text_file_alloc(bin, binary, ierr)
        if (ierr /= 0) then
            listing%reason = 'cannot read the executable'
            return
        end if
        if (index(binary, marker('TSIL_TSET_OF')) > 0) then
            listing%adapter = ADAPTER_FO
            call list_fo(bin, cwd, listing)
        else if (index(binary, marker(':stset elbaliavA')) > 0 .and. &
            index(binary, marker('setiustset elbaliavA')) > 0) then
            listing%adapter = ADAPTER_TESTDRIVE
            call list_testdrive(bin, cwd, listing)
        end if
        if (size(listing%names) == 0) listing%adapter = ADAPTER_NONE
        call cache_write(sidecar, digest, listing)
    end subroutine case_list

    function marker(reversed) result(text)
        !! A string to look for in executables, given reversed and turned
        !! around at run time: fo's own executables (which contain this
        !! module) must not contain it.
        character(len=*), intent(in) :: reversed
        character(len=:), allocatable :: text
        integer :: k, n

        n = len(reversed)
        allocate (character(len=n) :: text)
        do k = 1, n
            text(k:k) = reversed(n - k + 1:n - k + 1)
        end do
    end function marker

    function cache_path(bin) result(path)
        !! A hidden file beside the executable, so listings of the output
        !! directory still show only executables.
        character(len=*), intent(in) :: bin
        character(len=:), allocatable :: path
        integer :: slash

        slash = index(bin, '/', back=.true.)
        path = bin(1:slash)//'.'//bin(slash + 1:)//'.fo-cases'
    end function cache_path

    subroutine run_capture(bin, cwd, args, env, output, exitcode)
        !! Run bin with extra argv tokens (one per element) and the
        !! environment env; the merged output and the exit code.
        character(len=*), intent(in) :: bin, cwd, env
        type(string_t), intent(in) :: args(:)
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exitcode
        character(len=:), allocatable :: packed
        character(len=512) :: log
        integer :: n, k, status

        packed = ''
        n = 0
        call argv_push(packed, n, bin)
        do k = 1, size(args)
            call argv_push(packed, n, args(k)%s)
        end do
        call make_tmpfile('fo-case-list', log)
        call process_run_argv_logged(cwd, packed, n, trim(log), .false., &
            LIST_TIMEOUT_S, exitcode, env_extra=env)
        call read_text_file_alloc(trim(log), output, status)
        if (status /= 0) output = ''
        call delete_tmpfile(trim(log))
    end subroutine run_capture

    subroutine list_fo(bin, cwd, listing)
        character(len=*), intent(in) :: bin, cwd
        type(case_listing_t), intent(inout) :: listing
        character(len=:), allocatable :: output, line, text, source
        character(len=48) :: code_text
        integer :: code, pos, next, tab
        type(string_t) :: none(0)

        call run_capture(bin, cwd, none, 'FO_TEST_LIST=1', output, code)
        if (code /= 0) then
            write (code_text, '(a,i0)') 'listing exited with code ', code
            listing%reason = trim(code_text)
            return
        end if
        pos = 1
        do while (pos <= len(output))
            next = index(output(pos:), new_line('a'))
            if (next == 0) next = len(output) - pos + 2
            line = output(pos:pos + next - 2)
            pos = pos + next
            if (len(line) < 9) cycle
            if (line(1:8) /= 'fo-case ') cycle
            text = trim(line(9:))
            source = ''
            tab = index(text, achar(9))
            if (tab > 0) then
                source = trim(text(tab + 1:))
                text = text(1:tab - 1)
            end if
            if (.not. case_valid_name(text)) then
                listing%reason = 'invalid case name in the listing'
                deallocate (listing%names, listing%sources)
                allocate (listing%names(0), listing%sources(0))
                return
            end if
            call push(listing, text, source)
        end do
        if (size(listing%names) == 0) listing%reason = 'the listing named no case'
        if (size(listing%names) > 0) listing%reason = ''
    end subroutine list_fo

    subroutine list_testdrive(bin, cwd, listing)
        !! `exe NAME` with an unknown suite prints `- suite` lines below
        !! "Available testsuites"; `exe SUITE NAME` with an unknown test
        !! prints `- test` lines below "Available tests:".
        character(len=*), intent(in) :: bin, cwd
        type(case_listing_t), intent(inout) :: listing
        type(string_t), allocatable :: suites(:), tests(:)
        character(len=:), allocatable :: output
        integer :: code, k, j

        call run_capture(bin, cwd, [string_t('fo-list-suites')], '', output, code)
        call dash_items(output, marker('setiustset elbaliavA'), suites)
        if (code == 0 .or. size(suites) == 0) then
            listing%reason = 'test-drive executable does not list its suites'
            return
        end if
        do k = 1, size(suites)
            call run_capture(bin, cwd, [suites(k), string_t('fo-list-tests')], '', &
                output, code)
            call dash_items(output, marker(':stset elbaliavA'), tests)
            if (code == 0 .or. size(tests) == 0) then
                listing%reason = 'test-drive suite '//suites(k)%s// &
                    ' does not list its tests'
                deallocate (listing%names, listing%sources)
                allocate (listing%names(0), listing%sources(0))
                return
            end if
            do j = 1, size(tests)
                if (.not. case_valid_name(suites(k)%s//CASE_SEP//tests(j)%s)) then
                    listing%reason = 'invalid case name in the listing'
                    deallocate (listing%names, listing%sources)
                    allocate (listing%names(0), listing%sources(0))
                    return
                end if
                call push(listing, suites(k)%s//CASE_SEP//tests(j)%s, '')
            end do
        end do
        listing%reason = ''
    end subroutine list_testdrive

    subroutine dash_items(output, header, items)
        !! The names of the `- name` lines that follow the header line.
        character(len=*), intent(in) :: output, header
        type(string_t), allocatable, intent(out) :: items(:)
        character(len=:), allocatable :: line
        integer :: pos, next, dash
        logical :: seen

        allocate (items(0))
        seen = .false.
        pos = 1
        do while (pos <= len(output))
            next = index(output(pos:), new_line('a'))
            if (next == 0) next = len(output) - pos + 2
            line = output(pos:pos + next - 2)
            pos = pos + next
            if (index(line, header) > 0) then
                seen = .true.
                cycle
            end if
            if (.not. seen) cycle
            line = adjustl(line)
            if (len(line) > 0) then
                if (line(1:1) == '#') line = adjustl(line(2:))
            end if
            if (len_trim(line) < 3) cycle
            if (line(1:2) /= '- ') cycle
            dash = 3
            items = [items, string_t(trim(adjustl(line(dash:))))]
        end do
    end subroutine dash_items

    subroutine push(listing, name, source)
        type(case_listing_t), intent(inout) :: listing
        character(len=*), intent(in) :: name, source

        listing%names = [listing%names, string_t(name)]
        listing%sources = [listing%sources, string_t(source)]
    end subroutine push

    logical function cache_read(path, digest, listing) result(hit)
        character(len=*), intent(in) :: path, digest
        type(case_listing_t), intent(inout) :: listing
        character(len=:), allocatable :: text, line
        integer :: status, pos, next, tab, adapter, ios
        logical :: first

        hit = .false.
        call read_text_file_alloc(path, text, status)
        if (status /= 0) return
        first = .true.
        adapter = ADAPTER_NONE
        pos = 1
        do while (pos <= len(text))
            next = index(text(pos:), new_line('a'))
            if (next == 0) next = len(text) - pos + 2
            line = text(pos:pos + next - 2)
            pos = pos + next
            if (first) then
                if (line /= 'fo-cases-v1 '//digest) return
                first = .false.
            else if (line(1:min(8, len(line))) == 'adapter ') then
                read (line(9:), *, iostat=ios) adapter
                if (ios /= 0) return
            else if (line(1:min(7, len(line))) == 'reason ') then
                listing%reason = line(8:)
            else if (line(1:min(5, len(line))) == 'case ') then
                tab = index(line, achar(9))
                if (tab == 0) return
                call push(listing, line(6:tab - 1), line(tab + 1:))
            end if
        end do
        if (first) return
        listing%adapter = adapter
        hit = .true.
    end function cache_read

    subroutine cache_write(path, digest, listing)
        !! Atomic replace; a failed write only loses the cache.
        character(len=*), intent(in) :: path, digest
        type(case_listing_t), intent(in) :: listing
        character(len=len(path) + 32) :: temporary
        integer :: u, ios, k

        write (temporary, '(a,a,i0)') path, '.tmp', process_getpid()
        open (newunit=u, file=trim(temporary), status='replace', action='write', &
            iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') 'fo-cases-v1 '//digest
        write (u, '(a,i0)') 'adapter ', listing%adapter
        if (len(listing%reason) > 0) write (u, '(a)') 'reason '//listing%reason
        do k = 1, size(listing%names)
            write (u, '(a)') 'case '//listing%names(k)%s//achar(9)// &
                listing%sources(k)%s
        end do
        close (u)
        if (fs_rename(trim(temporary), path) /= 0) call delete_tmpfile(trim(temporary))
    end subroutine cache_write

end module fo_test_cases
