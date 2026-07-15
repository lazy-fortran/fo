module fo_build_backend
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_mkdir_excl, fs_sleep_ms, &
        fs_pid_alive
    use fo_process, only: process_detect_nproc, process_getpid, &
        process_run_argv_logged, argv_push, argv_push_split
    use fo_gfortran_build, only: gfortran_build, gfortran_test, &
        gfortran_test_names
    use fo_compiler_flags, only: append_array_temporary_warning_flag
    implicit none
    private
    public :: backend_t, detect_backend, detect_nproc, detect_jobs
    public :: backend_build, backend_test, backend_test_names
    public :: backend_test_affected, backend_clean
    public :: profile_flags
    public :: BACKEND_NONE, BACKEND_NATIVE, BACKEND_CMAKE

    integer, parameter :: BACKEND_NONE = 0
    integer, parameter :: BACKEND_NATIVE = 1
    integer, parameter :: BACKEND_CMAKE = 2
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
                b%kind = BACKEND_NATIVE
                return
            end if

            call parent_dir(current, parent)
            if (trim(parent) == trim(current)) exit
            current = parent
        end do

        b%kind = BACKEND_NONE
        b%project_dir = ''
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

    subroutine backend_build(self, exitcode, flags, log_file, with_tests, use_cache)
        type(backend_t), intent(in) :: self
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: flags
        character(len=*), intent(in), optional :: log_file
        logical, intent(in), optional :: with_tests
        logical, intent(in), optional :: use_cache

        character(len=512) :: log_path, flag_text, lock_dir
        integer :: lock_ierr
        logical :: want_tests

        log_path = ''
        if (present(log_file)) log_path = log_file
        flag_text = ''
        if (present(flags)) flag_text = flags
        want_tests = .false.
        if (present(with_tests)) want_tests = with_tests

        if (self%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            return
        end if

        call acquire_project_lock(self%project_dir, lock_dir, lock_ierr)
        if (lock_ierr /= 0) then
            exitcode = 1
            return
        end if

        select case (self%kind)
        case (BACKEND_NATIVE)
            if (want_tests) then
                call gfortran_test(self%project_dir, log_path, exitcode, &
                    flags=flag_text, build_only=.true., use_cache=use_cache)
            else
                call gfortran_build(self%project_dir, log_path, exitcode, &
                    flags=flag_text, use_cache=use_cache)
            end if
        case (BACKEND_CMAKE)
            call cmake_build(self%project_dir, flag_text, log_path, exitcode)
        end select

        call release_project_lock(lock_dir)
        if (exitcode == 124) then
            write (error_unit, '(a)') &
                'fo: WARNING: build timed out (FO_BUILD_TIMEOUT exceeded);' // &
                ' set FO_BUILD_TIMEOUT env var or investigate slow build'
        end if
    end subroutine backend_build

    function profile_flags(name) result(flags)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: flags
        select case (trim(name))
        case ('debug')
            flags = '-g -O0 -fcheck=all -fbacktrace'
        case ('asan')
            flags = '-g -O0 -fcheck=all -fbacktrace '// &
                '-fsanitize=address,undefined'
        case default
            flags = ''
        end select
    end function profile_flags

    subroutine backend_test(self, exitcode, include_slow, log_file, flags, use_cache)
        type(backend_t), intent(in) :: self
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache

        character(len=512) :: log_path, lock_dir, flag_text
        integer :: lock_ierr
        logical :: slow

        slow = .false.
        if (present(include_slow)) slow = include_slow
        log_path = ''
        if (present(log_file)) log_path = log_file
        flag_text = ''
        if (present(flags)) flag_text = flags

        if (self%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            return
        end if

        call acquire_project_lock(self%project_dir, lock_dir, lock_ierr)
        if (lock_ierr /= 0) then
            exitcode = 1
            return
        end if

        select case (self%kind)
        case (BACKEND_NATIVE)
            call gfortran_test(self%project_dir, log_path, exitcode, &
                include_slow=slow, flags=flag_text, use_cache=use_cache)
        case (BACKEND_CMAKE)
            call cmake_build(self%project_dir, flag_text, log_path, exitcode)
            if (exitcode == 0) &
                call cmake_test(self%project_dir, '', slow, log_path, exitcode)
        end select

        call release_project_lock(lock_dir)
        if (exitcode == 124) then
            write (error_unit, '(a)') &
                'fo: WARNING: tests timed out (FO_TEST_TIMEOUT exceeded);' // &
                ' set FO_TEST_TIMEOUT env var or mark slow tests with _slow suffix'
        end if
    end subroutine backend_test

    subroutine backend_test_names(self, names, n_names, exitcode, include_slow, &
            log_file, flags, use_cache)
        use fo_scan, only: is_slow_test
        type(backend_t), intent(in) :: self
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache

        integer :: i, lock_ierr
        character(len=128) :: fast_names(MAX_TEST_TARGETS)
        logical :: slow
        integer :: n_fast
        character(len=512) :: log_path, lock_dir, flag_text

        slow = .false.
        if (present(include_slow)) slow = include_slow
        exitcode = 0
        log_path = ''
        if (present(log_file)) log_path = log_file
        flag_text = ''
        if (present(flags)) flag_text = flags

        n_fast = 0
        do i = 1, n_names
            if (.not. slow .and. is_slow_test(names(i))) cycle
            if (n_fast < MAX_TEST_TARGETS) then
                n_fast = n_fast + 1
                fast_names(n_fast) = names(i)
            end if
        end do
        if (n_fast == 0) return

        if (self%kind == BACKEND_NONE) then
            exitcode = 1
            return
        end if

        call acquire_project_lock(self%project_dir, lock_dir, lock_ierr)
        if (lock_ierr /= 0) then
            exitcode = 1
            return
        end if

        select case (self%kind)
        case (BACKEND_NATIVE)
            call gfortran_test_names(self%project_dir, fast_names, n_fast, &
                log_path, exitcode, include_slow=slow, flags=flag_text, &
                use_cache=use_cache)
        case (BACKEND_CMAKE)
            block
                character(len=1024) :: regex
                call cmake_build(self%project_dir, flag_text, log_path, exitcode)
                if (exitcode == 0) then
                    call names_to_ctest_regex(fast_names, n_fast, regex)
                    call cmake_test(self%project_dir, regex, slow, log_path, &
                        exitcode)
                end if
            end block
        end select
        call release_project_lock(lock_dir)
    end subroutine backend_test_names

    subroutine backend_test_affected(self, names, n_names, exitcode, &
            include_slow, log_file, flags, use_cache)
        type(backend_t), intent(in) :: self
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache

        if (self%kind == BACKEND_CMAKE) then
            call backend_test(self, exitcode, include_slow, log_file, flags, &
                use_cache)
        else
            call backend_test_names(self, names, n_names, exitcode, &
                include_slow, log_file, flags, use_cache)
        end if
    end subroutine backend_test_affected

    subroutine cmake_build(project_dir, flags, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, flags, log_file
        integer, intent(out) :: exitcode

        character(len=:), allocatable :: packed
        character(len=32) :: jobs_text
        character(len=512) :: compiler, extra_args
        character(len=1024) :: effective_flags
        integer :: n_args

        write (jobs_text, '(i0)') detect_jobs()
        compiler = ''
        call get_environment_variable('FC', compiler)
        if (len_trim(compiler) == 0) compiler = 'gfortran'
        effective_flags = flags
        call append_array_temporary_warning_flag(compiler, effective_flags)

        n_args = 0
        call argv_push(packed, n_args, 'cmake')
        call argv_push(packed, n_args, '-S')
        call argv_push(packed, n_args, '.')
        call argv_push(packed, n_args, '-B')
        call argv_push(packed, n_args, 'build')
        call argv_push(packed, n_args, '-G')
        call argv_push(packed, n_args, 'Ninja')
        call argv_push(packed, n_args, '-DCMAKE_Fortran_COMPILER='//trim(compiler))
        if (len_trim(effective_flags) > 0) call argv_push(packed, n_args, &
            '-DCMAKE_Fortran_FLAGS='//trim(effective_flags))
        extra_args = ''
        call get_environment_variable('FO_CMAKE_ARGS', extra_args)
        call argv_push_split(packed, n_args, extra_args)
        call process_run_argv_logged(project_dir, packed, n_args, log_file, &
            .false., environment_timeout('FO_BUILD_TIMEOUT', 300), exitcode)
        if (exitcode /= 0) return

        deallocate (packed)
        n_args = 0
        call argv_push(packed, n_args, 'cmake')
        call argv_push(packed, n_args, '--build')
        call argv_push(packed, n_args, 'build')
        call argv_push(packed, n_args, '-j')
        call argv_push(packed, n_args, jobs_text)
        call process_run_argv_logged(project_dir, packed, n_args, log_file, &
            .true., environment_timeout('FO_BUILD_TIMEOUT', 300), exitcode)
    end subroutine cmake_build

    subroutine cmake_test(project_dir, regex, include_slow, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, regex, log_file
        logical, intent(in) :: include_slow
        integer, intent(out) :: exitcode

        character(len=:), allocatable :: packed
        character(len=32) :: jobs_text
        logical :: has_tests
        integer :: n_args

        inquire (file=trim(project_dir)//'/build/CTestTestfile.cmake', &
            exist=has_tests)
        if (.not. has_tests) then
            exitcode = 0
            return
        end if

        write (jobs_text, '(i0)') detect_jobs()
        n_args = 0
        call argv_push(packed, n_args, 'ctest')
        call argv_push(packed, n_args, '--test-dir')
        call argv_push(packed, n_args, 'build')
        call argv_push(packed, n_args, '--output-on-failure')
        call argv_push(packed, n_args, '--no-tests=error')
        call argv_push(packed, n_args, '-j')
        call argv_push(packed, n_args, jobs_text)
        if (len_trim(regex) > 0) then
            call argv_push(packed, n_args, '-R')
            call argv_push(packed, n_args, regex)
        end if
        if (.not. include_slow) then
            call argv_push(packed, n_args, '-LE')
            call argv_push(packed, n_args, &
                'slow|regression|performance|scalability')
        end if
        call process_run_argv_logged(project_dir, packed, n_args, log_file, &
            .false., environment_timeout('FO_TEST_TIMEOUT', 600), exitcode)
    end subroutine cmake_test

    integer function environment_timeout(name, fallback) result(timeout)
        character(len=*), intent(in) :: name
        integer, intent(in) :: fallback

        character(len=32) :: value
        integer :: iostat, status

        timeout = fallback
        call get_environment_variable(name, value, status=status)
        if (status /= 0 .or. len_trim(value) == 0) return
        read (value, *, iostat=iostat) timeout
        if (iostat /= 0 .or. timeout < 1) timeout = fallback
    end function environment_timeout

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

        character(len=1) :: ch
        integer :: i

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

    subroutine backend_clean(project_dir, purge_store, build_removed, &
            store_removed)
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
