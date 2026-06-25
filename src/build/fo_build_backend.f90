module fo_build_backend
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_mkdir_excl, fs_sleep_ms, &
        fs_pid_alive, fs_collect_files, fs_remove_file
    use fo_process, only: process_detect_nproc, process_fpm_build, &
        process_fpm_test_list, process_fpm_test_all, &
        process_fpm_test_names, process_cmake_build, &
        process_ctest, process_getpid
    use fo_gfortran_build, only: gfortran_build, gfortran_test, &
        gfortran_test_names
    implicit none
    private
    public :: backend_t, detect_backend, detect_nproc, detect_jobs
    public :: backend_build, backend_test, backend_test_names, backend_clean
    public :: BACKEND_FPM, BACKEND_CMAKE, BACKEND_NONE, BACKEND_GFORTRAN

    integer, parameter :: BACKEND_NONE = 0
    integer, parameter :: BACKEND_FPM = 1
    integer, parameter :: BACKEND_CMAKE = 2
    integer, parameter :: BACKEND_GFORTRAN = 3
    integer, parameter :: MAX_TEST_TARGETS = 512

    type :: backend_t
        integer :: kind = BACKEND_NONE
        character(len=512) :: project_dir = '.'
    end type backend_t

contains

    function detect_backend(dir) result(b)
        character(len=*), intent(in) :: dir
        type(backend_t) :: b
        logical :: has_fpm, has_cmake
        character(len=512) :: current, parent
        integer :: depth

        current = absolute_dir(dir)

        do depth = 1, 64
            b%project_dir = current

            inquire (file=trim(current)//'/fpm.toml', exist=has_fpm)
            inquire (file=trim(current)//'/CMakeLists.txt', exist=has_cmake)

            if (has_cmake) then
                b%kind = BACKEND_CMAKE
                return
            else if (has_fpm) then
                b%kind = BACKEND_GFORTRAN
                return
            end if

            call parent_dir(current, parent)
            if (trim(parent) == trim(current)) exit
            current = parent
        end do

        b%kind = BACKEND_NONE
    end function detect_backend

    function absolute_dir(dir) result(absdir)
        character(len=*), intent(in) :: dir
        character(len=512) :: absdir

        character(len=512) :: pwd

        if (len_trim(dir) == 0) then
            absdir = '.'
        else if (dir(1:1) == '/') then
            absdir = trim(dir)
        else
            call get_environment_variable('PWD', pwd)
            if (len_trim(pwd) > 0) then
                if (trim(dir) == '.') then
                    absdir = trim(pwd)
                else
                    absdir = trim(pwd)//'/'//trim(dir)
                end if
            else
                absdir = trim(dir)
            end if
        end if
    end function absolute_dir

    subroutine parent_dir(path, parent)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: parent

        character(len=512) :: clean
        integer :: n, last

        clean = trim(path)
        n = len_trim(clean)
        do while (n > 1 .and. clean(n:n) == '/')
            clean(n:n) = ' '
            n = n - 1
        end do

        if (trim(clean) == '/') then
            parent = '/'
            return
        end if

        last = index(trim(clean), '/', back=.true.)
        if (last <= 1) then
            parent = '/'
        else
            parent = clean(1:last - 1)
        end if
    end subroutine parent_dir

    function detect_nproc() result(np)
        integer :: np

        np = process_detect_nproc()
        if (np < 1) np = 1
    end function detect_nproc

    function detect_jobs() result(jobs)
        integer :: jobs

        character(len=32) :: buf
        integer :: status, iostat

        jobs = detect_nproc()
        call get_environment_variable('FO_JOBS', buf, status=status)
        if (status /= 0 .or. len_trim(buf) == 0) return

        read (buf, *, iostat=iostat) jobs
        if (iostat /= 0 .or. jobs < 1) jobs = detect_nproc()
    end function detect_jobs

    subroutine backend_build(self, exitcode, flags, log_file, with_tests)
        type(backend_t), intent(in) :: self
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: flags
        character(len=*), intent(in), optional :: log_file
        ! with_tests: also compile and link the test binaries (gfortran backend)
        ! so build/fo/bin/test_* stay current after a plain `fo build` instead of
        ! going stale until the next `fo test`. fpm/cmake build tests anyway.
        logical, intent(in), optional :: with_tests

        integer :: np, lock_ierr
        character(len=512) :: log_path, flag_text, lock_dir
        logical :: want_tests

        np = detect_jobs()
        log_path = ''
        if (present(log_file)) log_path = log_file
        flag_text = ''
        if (present(flags)) flag_text = flags
        want_tests = .false.
        if (present(with_tests)) want_tests = with_tests
        call acquire_project_lock(self%project_dir, lock_dir, lock_ierr)
        if (lock_ierr /= 0) then
            exitcode = 1
            return
        end if

        select case (self%kind)
        case (BACKEND_GFORTRAN)
            call gfortran_build(self%project_dir, log_path, exitcode, &
                flags=flag_text)
            if (exitcode == 0 .and. want_tests) &
                call gfortran_test(self%project_dir, log_path, exitcode, &
                build_only=.true.)
        case (BACKEND_FPM)
            call process_fpm_build(self%project_dir, flag_text, np, log_path, &
                exitcode)
            if (exitcode /= 0 .and. exitcode /= 124 .and. len_trim(log_path) > 0) then
                if (log_has_vtable_mismatch(log_path)) then
                    write (error_unit, '(a)') &
                        'fo: WARNING: stale .mod files detected (vtable mismatch);' // &
                        ' clearing module interface cache and retrying'
                    call clear_fpm_mod_cache(self%project_dir)
                    call process_fpm_build(self%project_dir, flag_text, np, log_path, &
                        exitcode)
                end if
            end if
        case (BACKEND_CMAKE)
            call process_cmake_build(self%project_dir, flag_text, np, log_path, &
                exitcode)
            if (exitcode /= 0 .and. exitcode /= 124 .and. len_trim(log_path) > 0) then
                if (log_has_vtable_mismatch(log_path)) then
                    write (error_unit, '(a)') &
                        'fo: WARNING: stale CMake module interfaces detected;' // &
                        ' clearing build tree and retrying'
                    call clear_cmake_build_tree(self%project_dir)
                    call process_cmake_build(self%project_dir, flag_text, np, log_path, &
                        exitcode)
                else if (log_has_cmake_fetchcontent_unstash(log_path)) then
                    write (error_unit, '(a)') &
                        'fo: WARNING: dirty CMake FetchContent checkout detected;' // &
                        ' clearing build tree and retrying'
                    call clear_cmake_build_tree(self%project_dir)
                    call process_cmake_build(self%project_dir, flag_text, np, log_path, &
                        exitcode)
                end if
            end if
        case default
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            call release_project_lock(lock_dir)
            return
        end select
        call release_project_lock(lock_dir)
        if (exitcode == 124) then
            write (error_unit, '(a)') &
                'fo: WARNING: build timed out (FO_BUILD_TIMEOUT exceeded);' // &
                ' set FO_BUILD_TIMEOUT env var or investigate slow build'
        end if
    end subroutine backend_build

    subroutine backend_test(self, exitcode, include_slow, log_file)
        type(backend_t), intent(in) :: self
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file

        integer :: list_ierr, n_names, jobs, lock_ierr
        character(len=128) :: names(MAX_TEST_TARGETS)
        character(len=512) :: log_path, lock_dir
        logical :: has_tests, slow

        slow = .false.
        if (present(include_slow)) slow = include_slow
        jobs = detect_jobs()
        log_path = ''
        if (present(log_file)) log_path = log_file
        call acquire_project_lock(self%project_dir, lock_dir, lock_ierr)
        if (lock_ierr /= 0) then
            exitcode = 1
            return
        end if

        select case (self%kind)
        case (BACKEND_GFORTRAN)
            call gfortran_test(self%project_dir, log_path, exitcode, &
                include_slow=slow)
        case (BACKEND_FPM)
            if (slow) then
                call process_fpm_test_all(self%project_dir, jobs, log_path, exitcode)
            else
                call fpm_list_tests(self%project_dir, names, n_names, &
                    list_ierr, log_path)
                if (list_ierr /= 0) then
                    exitcode = 1
                    call release_project_lock(lock_dir)
                    return
                end if
                call filter_slow_tests(names, n_names)
                if (n_names == 0) then
                    exitcode = 0
                    call release_project_lock(lock_dir)
                    return
                end if
                call fpm_run_tests(self%project_dir, names, n_names, &
                    exitcode, log_path)
            end if
        case (BACKEND_CMAKE)
            inquire (file=trim(self%project_dir)//'/build/CTestTestfile.cmake', &
                exist=has_tests)
            if (.not. has_tests) then
                exitcode = 0
                call release_project_lock(lock_dir)
                return
            end if
            call process_ctest(self%project_dir, jobs, '', slow, log_path, exitcode)
        case default
            write (error_unit, '(a)') 'fo: no build backend detected'
            exitcode = 1
            call release_project_lock(lock_dir)
            return
        end select
        call release_project_lock(lock_dir)
        if (exitcode == 124) then
            write (error_unit, '(a)') &
                'fo: WARNING: tests timed out (FO_TEST_TIMEOUT exceeded);' // &
                ' set FO_TEST_TIMEOUT env var or mark slow tests with _slow suffix'
        end if
    end subroutine backend_test

    subroutine backend_test_names(self, names, n_names, exitcode, include_slow, &
            log_file)
        use fo_scan, only: is_slow_test
        type(backend_t), intent(in) :: self
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file

        integer :: i, lock_ierr
        character(len=128) :: fast_names(MAX_TEST_TARGETS)
        logical :: slow
        integer :: n_fast, jobs
        character(len=1024) :: regex
        character(len=512) :: log_path, lock_dir

        slow = .false.
        if (present(include_slow)) slow = include_slow
        exitcode = 0
        jobs = detect_jobs()
        log_path = ''
        if (present(log_file)) log_path = log_file

        n_fast = 0
        do i = 1, n_names
            if (.not. slow .and. is_slow_test(names(i))) cycle
            if (n_fast < MAX_TEST_TARGETS) then
                n_fast = n_fast + 1
                fast_names(n_fast) = names(i)
            end if
        end do
        if (n_fast == 0) return
        call acquire_project_lock(self%project_dir, lock_dir, lock_ierr)
        if (lock_ierr /= 0) then
            exitcode = 1
            return
        end if

        if (self%kind == BACKEND_GFORTRAN) then
            call gfortran_test_names(self%project_dir, fast_names, n_fast, &
                log_path, exitcode, include_slow=slow)
            call release_project_lock(lock_dir)
            return
        end if

        if (self%kind == BACKEND_FPM) then
            call fpm_run_tests(self%project_dir, fast_names, n_fast, &
                exitcode, log_path)
            call release_project_lock(lock_dir)
            return
        end if

        if (self%kind == BACKEND_CMAKE) then
            call names_to_ctest_regex(fast_names, n_fast, regex)
            call process_ctest(self%project_dir, jobs, regex, slow, log_path, &
                exitcode)
            call release_project_lock(lock_dir)
            return
        end if

        exitcode = 1
        call release_project_lock(lock_dir)
    end subroutine backend_test_names

    subroutine acquire_project_lock(project_dir, lock_dir, ierr)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(out) :: lock_dir
        integer, intent(out) :: ierr

        character(len=:), allocatable :: base, pid_file
        integer :: state, u, ios, owner

        base = trim(project_dir)//'/build/fo'
        lock_dir = trim(base)//'/.lock'
        pid_file = trim(lock_dir)//'/pid'
        call fs_make_dir(base)
        ierr = 0

        ! Spin on an atomic exclusive mkdir of the lock directory. When the
        ! lock is held, reclaim it only if the recorded owner pid is gone.
        do
            state = fs_mkdir_excl(lock_dir)
            if (state == 0) exit
            if (state < 0) then
                ierr = 1
                return
            end if
            owner = 0
            open (newunit=u, file=pid_file, status='old', action='read', &
                iostat=ios)
            if (ios == 0) then
                read (u, *, iostat=ios) owner
                close (u)
            end if
            if (owner > 0 .and. .not. fs_pid_alive(owner)) then
                call fs_remove_tree(lock_dir)
                cycle
            end if
            call fs_sleep_ms(50)
        end do

        open (newunit=u, file=pid_file, status='replace', action='write', &
            iostat=ios)
        if (ios == 0) then
            write (u, '(i0)') process_getpid()
            close (u)
        end if
    end subroutine acquire_project_lock

    subroutine release_project_lock(lock_dir)
        character(len=*), intent(in) :: lock_dir

        if (len_trim(lock_dir) == 0) return
        call fs_remove_tree(trim(lock_dir))
    end subroutine release_project_lock

    pure function sq(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len_trim(s) + 2) :: r
        r = "'"//trim(s)//"'"
    end function sq

    subroutine names_to_ctest_regex(names, n_names, regex)
        character(len=128), intent(in) :: names(MAX_TEST_TARGETS)
        integer, intent(in) :: n_names
        character(len=*), intent(out) :: regex

        integer :: i

        regex = '^('
        do i = 1, n_names
            if (i > 1) regex = trim(regex)//'|'
            call append_ctest_regex_name(regex, names(i))
        end do
        regex = trim(regex)//')$'
    end subroutine names_to_ctest_regex

    subroutine append_ctest_regex_name(regex, name)
        character(len=*), intent(inout) :: regex
        character(len=*), intent(in) :: name

        integer :: i
        character(len=1) :: ch

        do i = 1, len_trim(name)
            ch = name(i:i)
            select case (ch)
            case ('.', '^', '$', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|')
                regex = trim(regex)//achar(92)//ch
            case (achar(92))
                regex = trim(regex)//achar(92)//achar(92)
            case default
                regex = trim(regex)//ch
            end select
        end do
    end subroutine append_ctest_regex_name

    subroutine fpm_list_tests(project_dir, names, n_names, exitcode, log_file)
        character(len=*), intent(in) :: project_dir
        character(len=128), intent(out) :: names(MAX_TEST_TARGETS)
        integer, intent(out) :: n_names, exitcode
        character(len=*), intent(in), optional :: log_file

        character(len=512) :: list_file, parse_file

        names = ''
        n_names = 0
        exitcode = 0

        if (present(log_file) .and. len_trim(log_file) > 0) then
            list_file = log_file
            parse_file = log_file
        else
            call make_tmpfile('fo-fpm-tests', list_file)
            parse_file = list_file
        end if

        call process_fpm_test_list(project_dir, list_file, exitcode)
        if (exitcode == 0) call parse_fpm_test_list(parse_file, names, n_names)
        if (.not. present(log_file) .or. len_trim(log_file) == 0) then
            call delete_tmpfile(list_file)
        end if
    end subroutine fpm_list_tests

    subroutine parse_fpm_test_list(path, names, n_names)
        character(len=*), intent(in) :: path
        character(len=128), intent(inout) :: names(MAX_TEST_TARGETS)
        integer, intent(inout) :: n_names

        character(len=512) :: line
        integer :: u, iostat, colon
        logical :: in_names

        in_names = .false.
        open (newunit=u, file=path, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            colon = index(line, 'Matched names:')
            if (colon > 0) then
                in_names = .true.
                line = line(colon + len('Matched names:'):)
            else if (.not. in_names) then
                cycle
            end if
            call parse_words(line, names, n_names)
        end do
        close (u)
    end subroutine parse_fpm_test_list

    subroutine parse_words(line, names, n_names)
        character(len=*), intent(in) :: line
        character(len=128), intent(inout) :: names(MAX_TEST_TARGETS)
        integer, intent(inout) :: n_names

        integer :: pos, start, finish, n

        n = len_trim(line)
        pos = 1
        do while (pos <= n)
            do while (pos <= n .and. line(pos:pos) == ' ')
                pos = pos + 1
            end do
            if (pos > n) exit

            start = pos
            do while (pos <= n .and. line(pos:pos) /= ' ')
                pos = pos + 1
            end do
            finish = pos - 1

            if (n_names < MAX_TEST_TARGETS) then
                n_names = n_names + 1
                names(n_names) = line(start:finish)
            end if
        end do
    end subroutine parse_words

    subroutine filter_slow_tests(names, n_names)
        use fo_scan, only: is_slow_test
        character(len=128), intent(inout) :: names(MAX_TEST_TARGETS)
        integer, intent(inout) :: n_names

        character(len=128) :: fast_names(MAX_TEST_TARGETS)
        integer :: i, n_fast

        fast_names = ''
        n_fast = 0
        do i = 1, n_names
            if (is_slow_test(names(i))) cycle
            n_fast = n_fast + 1
            fast_names(n_fast) = names(i)
        end do

        names = fast_names
        n_names = n_fast
    end subroutine filter_slow_tests

    subroutine fpm_run_tests(project_dir, names, n_names, exitcode, log_file)
        character(len=*), intent(in) :: project_dir
        character(len=128), intent(in) :: names(MAX_TEST_TARGETS)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: log_file

        integer :: jobs

        if (n_names == 0) then
            exitcode = 0
            return
        end if

        jobs = detect_jobs()

        if (present(log_file)) then
            call process_fpm_test_names(project_dir, names, n_names, jobs, log_file, &
                exitcode)
        else
            call process_fpm_test_names(project_dir, names, n_names, jobs, '', &
                exitcode)
        end if
    end subroutine fpm_run_tests

    ! Returns true if the build log contains a gfortran vtable mismatch error.
    ! These occur when .mod files compiled under different dependency sets are mixed.
    logical function log_has_vtable_mismatch(log_file)
        character(len=*), intent(in) :: log_file

        integer :: u, iostat
        character(len=512) :: line

        log_has_vtable_mismatch = .false.
        open (newunit=u, file=trim(log_file), status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, 'Mismatch in components of derived type') > 0 .or. &
                index(line, '__vtype_') > 0) then
                log_has_vtable_mismatch = .true.
                exit
            end if
        end do
        close (u)
    end function log_has_vtable_mismatch

    logical function log_has_cmake_fetchcontent_unstash(log_file)
        character(len=*), intent(in) :: log_file

        integer :: u, iostat
        character(len=512) :: line

        log_has_cmake_fetchcontent_unstash = .false.
        open (newunit=u, file=trim(log_file), status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, 'Failed to unstash changes in:') > 0) then
                log_has_cmake_fetchcontent_unstash = .true.
                exit
            end if
        end do
        close (u)
    end function log_has_cmake_fetchcontent_unstash

    ! Delete all .mod files from fpm build directories to clear stale module interfaces.
    ! Keeps dependency build artifacts (.o files) intact to avoid full dependency rebuild.
    subroutine clear_fpm_mod_cache(project_dir)
        character(len=*), intent(in) :: project_dir

        character(len=:), allocatable :: build_root
        character(len=512), allocatable :: hits(:)
        integer :: n, k, base_depth

        build_root = trim(project_dir)//'/build'
        base_depth = path_depth(build_root)
        allocate (hits(4096))

        ! Project module interfaces at depth <= 2 under build, never a
        ! dependency's. Replaces find -maxdepth 2 -name '*.mod' -delete.
        call fs_collect_files(build_root, '', '.mod', '', hits, n)
        do k = 1, n
            if (index(hits(k), '/dependencies/') > 0) cycle
            if (path_depth(trim(hits(k))) - base_depth > 2) cycle
            call fs_remove_file(trim(hits(k)))
        end do

        ! Project objects at exactly depth 3 (src_*.o), to force recompilation
        ! with fresh interfaces. Replaces find -mindepth 3 -maxdepth 3.
        call fs_collect_files(build_root, 'src_', '.o', '', hits, n)
        do k = 1, n
            if (index(hits(k), '/dependencies/') > 0) cycle
            if (.not. basename_starts(trim(hits(k)), 'src_')) cycle
            if (path_depth(trim(hits(k))) - base_depth /= 3) cycle
            call fs_remove_file(trim(hits(k)))
        end do
        deallocate (hits)
    end subroutine clear_fpm_mod_cache

    integer function path_depth(path) result(d)
        !! Number of path components (count of '/' separators) in a path, used
        !! to emulate find's -maxdepth/-mindepth without a shell.
        character(len=*), intent(in) :: path
        integer :: i
        d = 0
        do i = 1, len_trim(path)
            if (path(i:i) == '/') d = d + 1
        end do
    end function path_depth

    logical function basename_starts(path, prefix) result(yes)
        !! True when the final path component begins with prefix.
        character(len=*), intent(in) :: path, prefix
        integer :: slash, n
        n = len_trim(path)
        slash = index(path(1:n), '/', back=.true.)
        yes = .false.
        if (n - slash < len(prefix)) return
        yes = path(slash + 1:slash + len(prefix)) == prefix
    end function basename_starts

    subroutine clear_cmake_build_tree(project_dir)
        character(len=*), intent(in) :: project_dir

        call fs_remove_tree(trim(project_dir)//'/build')
    end subroutine clear_cmake_build_tree

    subroutine backend_clean(project_dir, purge_store, build_removed, &
            store_removed)
        !! Project-scoped clean. Always drops the project's build/ tree (a
        !! disposable view fo regenerates from the cache). The shared
        !! content-addressed store under cache_root is removed only when
        !! purge_store is set: it is the cross-project source of truth, so a
        !! per-project clean must not cold-start every other project.
        use fo_cache, only: cache_root
        use fo_util, only: clean_root_build_artifacts
        character(len=*), intent(in) :: project_dir
        logical, intent(in) :: purge_store
        logical, intent(out) :: build_removed, store_removed

        character(len=512) :: root
        integer :: n_removed

        build_removed = .false.
        store_removed = .false.
        if (len_trim(project_dir) > 0) then
            call fs_remove_tree(trim(project_dir)//'/build')
            call clean_root_build_artifacts(trim(project_dir), n_removed)
            build_removed = .true.
        end if
        if (purge_store) then
            call cache_root(root)
            call fs_remove_tree(trim(root))
            store_removed = .true.
        end if
    end subroutine backend_clean

end module fo_build_backend
