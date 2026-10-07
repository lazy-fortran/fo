module fo_build_backend
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_mkdir_excl, fs_sleep_ms, &
        fs_pid_alive
    use fo_process, only: process_detect_nproc, process_getpid, &
        process_getcwd, process_run_argv_logged, argv_push
    use fo_gfortran_build, only: gfortran_build, gfortran_test, &
        gfortran_test_names, gfortran_run_tests
    use fo_test_budget, only: test_timeout_seconds, test_wall_cap_seconds
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t, &
        selected_compiler_command
    use fo_cmake_context, only: cmake_context_t, cmake_context_init, &
        cmake_context_query, cmake_context_read_reply, &
        cmake_context_build_path, cmake_context_validate_hint
    implicit none
    private
    public :: backend_t, detect_backend, detect_nproc, detect_jobs
    public :: backend_build, backend_test, backend_test_names
    public :: backend_test_affected, backend_clean
    public :: profile_flags, backend_profile
    public :: BACKEND_NONE, BACKEND_NATIVE, BACKEND_CMAKE

    integer, parameter :: BACKEND_NONE = 0
    integer, parameter :: BACKEND_NATIVE = 1
    integer, parameter :: BACKEND_CMAKE = 2
    integer, parameter :: MAX_TEST_TARGETS = 512

    type :: backend_t
        integer :: kind = BACKEND_NONE
        character(len=512) :: project_dir = '.'
        type(cmake_context_t) :: cmake
    end type backend_t

contains

    function detect_backend(dir) result(b)
        character(len=*), intent(in) :: dir
        type(backend_t) :: b
        logical :: has_fpm, has_cmake, has_cmake_tests
        logical :: force_cmake, force_fpm
        character(len=16) :: preference
        character(len=512) :: current, parent
        integer :: depth, status

        current = absolute_dir(dir)
        preference = ''
        call get_environment_variable('FO_BACKEND', preference, status=status)
        force_cmake = status == 0 .and. trim(adjustl(preference)) == 'cmake'
        force_fpm = status == 0 .and. trim(adjustl(preference)) == 'fpm'

        do depth = 1, 64
            b%project_dir = current
            inquire (file=trim(current)//'/fpm.toml', exist=has_fpm)
            inquire (file=trim(current)//'/CMakeLists.txt', exist=has_cmake)
            has_cmake_tests = .false.
            if (has_cmake) has_cmake_tests = cmake_declares_tests( &
                trim(current)//'/CMakeLists.txt')
            if (force_cmake .and. has_cmake) then
                b%kind = BACKEND_CMAKE
                call cmake_context_init(b%cmake, trim(current))
                return
            else if (force_fpm .and. has_fpm) then
                b%kind = BACKEND_NATIVE
                return
            else if (.not. force_cmake .and. .not. force_fpm .and. &
                    has_fpm .and. has_cmake_tests) then
                b%kind = BACKEND_CMAKE
                call cmake_context_init(b%cmake, trim(current))
                return
            else if (.not. force_cmake .and. .not. force_fpm .and. has_fpm) then
                b%kind = BACKEND_NATIVE
                return
            else if (.not. force_cmake .and. .not. force_fpm .and. has_cmake) then
                b%kind = BACKEND_CMAKE
                call cmake_context_init(b%cmake, trim(current))
                return
            end if

            call parent_dir(current, parent)
            if (trim(parent) == trim(current)) exit
            current = parent
        end do

        b%kind = BACKEND_NONE
        b%project_dir = ''
    end function detect_backend

    logical function cmake_declares_tests(filename) result(declares_tests)
        character(len=*), intent(in) :: filename

        character(len=2048) :: line, lowered
        integer :: unit, iostat, i, code

        declares_tests = .false.
        open (newunit=unit, file=filename, status='old', action='read', &
            iostat=iostat)
        if (iostat /= 0) return
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            lowered = line
            do i = 1, len_trim(lowered)
                code = iachar(lowered(i:i))
                if (code >= iachar('A') .and. code <= iachar('Z')) &
                    lowered(i:i) = achar(code + iachar('a') - iachar('A'))
            end do
            if (index(lowered, 'include(ctest') > 0 .or. &
                index(lowered, 'enable_testing') > 0) then
                declares_tests = .true.
                exit
            end if
        end do
        close (unit)
    end function cmake_declares_tests

    function absolute_dir(dir) result(absdir)
        character(len=*), intent(in) :: dir
        character(len=512) :: absdir

        character(len=512) :: pwd
        integer :: cwd_ierr

        if (len_trim(dir) == 0) then
            absdir = '.'
        else if (dir(1:1) == '/') then
            absdir = trim(dir)
        else
            call process_getcwd(pwd, cwd_ierr)
            if (cwd_ierr /= 0) pwd = ''
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
        type(backend_t), intent(inout) :: self
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
                ! `fo exec` must be able to resolve either an application or a
                ! test target from a clean tree. Build applications first;
                ! gfortran_test then adds test binaries without discarding the
                ! current application stamp.
                call gfortran_build(self%project_dir, log_path, exitcode, &
                    flags=flag_text, use_cache=use_cache)
                if (exitcode /= 0) then
                    call release_project_lock(lock_dir)
                    return
                end if
                call gfortran_test(self%project_dir, log_path, exitcode, &
                    flags=flag_text, build_only=.true., use_cache=use_cache)
            else
                call gfortran_build(self%project_dir, log_path, exitcode, &
                    flags=flag_text, use_cache=use_cache)
            end if
        case (BACKEND_CMAKE)
            call cmake_build(self%cmake, flag_text, log_path, exitcode)
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
        type(compiler_dialect_t) :: dialect

        dialect = compiler_dialect(selected_compiler_command())
        flags = dialect%profile_flags(name)
    end function profile_flags

    subroutine backend_profile(backend, name)
        type(backend_t), intent(inout) :: backend
        character(len=*), intent(in) :: name
        character(len=len(name)) :: lowered
        integer :: i, code

        if (backend%kind /= BACKEND_CMAKE) return
        if (len_trim(backend%cmake%configuration) > 0) return
        lowered = name
        do i = 1, len(name)
            code = iachar(name(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) &
                lowered(i:i) = achar(code + iachar('a') - iachar('A'))
        end do
        select case (trim(lowered))
        case ('debug')
            backend%cmake%configuration = 'Debug'
        case ('release')
            backend%cmake%configuration = 'Release'
        case ('asan')
            backend%cmake%configuration = 'ASan'
        end select
    end subroutine backend_profile

    subroutine backend_test(self, exitcode, include_slow, log_file, flags, use_cache)
        type(backend_t), intent(inout) :: self
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache

        character(len=128) :: no_names(1)
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
                include_slow=slow, flags=flag_text, build_only=.true., &
                use_cache=use_cache)
        case (BACKEND_CMAKE)
            call cmake_build(self%cmake, flag_text, log_path, exitcode)
        end select

        call release_project_lock(lock_dir)
        if (exitcode == 0) then
            select case (self%kind)
            case (BACKEND_NATIVE)
                no_names = ''
                call gfortran_run_tests(self%project_dir, log_path, exitcode, &
                    slow, no_names, 0, flags=flag_text)
            case (BACKEND_CMAKE)
                call cmake_test(self%cmake, '', slow, log_path, exitcode)
            end select
        end if
        if (exitcode == 124) then
            write (error_unit, '(a)') &
                'fo: WARNING: tests timed out (CPU budget FO_TEST_TIMEOUT or' // &
                ' wall cap FO_TEST_WALL_TIMEOUT exceeded); raise them, set' // &
                ' [extra.fo] test-timeout, or mark slow tests with _slow suffix'
        end if
    end subroutine backend_test

    subroutine backend_test_names(self, names, n_names, exitcode, include_slow, &
            log_file, flags, use_cache)
        use fo_scan, only: is_slow_test, MAX_PATH
        type(backend_t), intent(inout) :: self
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache

        integer :: i, lock_ierr
        character(len=MAX_PATH) :: fast_names(MAX_TEST_TARGETS)
        logical :: slow
        integer :: n_fast
        character(len=512) :: log_path, lock_dir, flag_text
        character(len=1024) :: regex

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
                use_cache=use_cache, build_only=.true.)
        case (BACKEND_CMAKE)
            call cmake_build(self%cmake, flag_text, log_path, exitcode)
        end select
        call release_project_lock(lock_dir)
        if (exitcode /= 0) return

        select case (self%kind)
        case (BACKEND_NATIVE)
            call gfortran_run_tests(self%project_dir, log_path, exitcode, slow, &
                fast_names, n_fast, flags=flag_text)
        case (BACKEND_CMAKE)
            call names_to_ctest_regex(fast_names, n_fast, regex)
            call cmake_test(self%cmake, regex, slow, log_path, exitcode)
        end select
    end subroutine backend_test_names

    subroutine backend_test_affected(self, names, n_names, exitcode, &
            include_slow, log_file, flags, use_cache)
        type(backend_t), intent(inout) :: self
        character(len=*), intent(in) :: names(:)
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

    subroutine cmake_build(context, flags, log_file, exitcode)
        type(cmake_context_t), intent(inout) :: context
        character(len=*), intent(in) :: flags, log_file
        integer, intent(out) :: exitcode

        character(len=:), allocatable :: packed, cache_file, configuration
        character(len=32) :: jobs_text
        logical :: has_cache, hint_valid
        integer :: n_args, i

        if (.not. context%valid) then
            write (error_unit, '(a,a)') 'fo: invalid CMake context: ', &
                context%error
            exitcode = 1
            return
        end if
        if (len_trim(flags) > 0 .and. len_trim(context%configuration) == 0) then
            write (error_unit, '(a)') 'fo: CMake compiler flags require '// &
                '--profile or FO_CMAKE_CONFIG to select their configuration'
            exitcode = 1
            return
        end if
        if (len_trim(context%configure_preset) > 0 .and. &
                .not. context%build_root_hint .and. &
                len_trim(context%build_preset) == 0) then
            write (error_unit, '(a)') &
                'fo: configure preset build root is unknown; supply '// &
                'FO_CMAKE_BUILD_DIR as a locator hint or select a build preset'
            exitcode = 1
            return
        end if

        write (jobs_text, '(i0)') detect_jobs()
        if (len_trim(context%configure_preset) == 0 .or. &
                context%build_root_hint) &
            call cmake_context_query(context)
        cache_file = cmake_context_build_path(context)//'/CMakeCache.txt'
        inquire (file=cache_file, exist=has_cache)

        n_args = 0
        call argv_push(packed, n_args, 'cmake')
        if (len_trim(context%configure_preset) > 0) then
            call argv_push(packed, n_args, '--preset')
            call argv_push(packed, n_args, context%configure_preset)
        else
            call argv_push(packed, n_args, '-S')
            call argv_push(packed, n_args, context%source_root)
            call argv_push(packed, n_args, '-B')
            call argv_push(packed, n_args, context%build_root)
            if (len_trim(context%generator) > 0) then
                call argv_push(packed, n_args, '-G')
                call argv_push(packed, n_args, context%generator)
            else if (.not. has_cache) then
                call argv_push(packed, n_args, '-G')
                call argv_push(packed, n_args, 'Ninja')
            end if
        end if
        if (len_trim(context%configuration) > 0 .and. &
                .not. context%multi_config .and. &
                .not. generator_is_multi(context)) then
            call argv_push(packed, n_args, &
                '-DCMAKE_BUILD_TYPE='//context%configuration)
        end if
        if (len_trim(flags) > 0) then
            configuration = context%configuration
            do i = 1, len(configuration)
                if (configuration(i:i) >= 'a' .and. configuration(i:i) <= 'z') &
                    configuration(i:i) = achar(iachar(configuration(i:i)) - 32)
            end do
            call argv_push(packed, n_args, &
                '-DCMAKE_Fortran_FLAGS_'//configuration//'='//trim(flags))
        end if
        if (allocated(context%extra_args)) then
            do i = 1, size(context%extra_args)
                call argv_push(packed, n_args, context%extra_args(i))
            end do
        end if
        call process_run_argv_logged(context%source_root, packed, n_args, &
            log_file, .false., environment_timeout('FO_BUILD_TIMEOUT', 300), &
            exitcode)
        if (exitcode /= 0) return
        if (len_trim(context%configure_preset) > 0) then
            if (context%build_root_hint) then
                call cmake_context_read_reply(context)
                call cmake_context_validate_hint(context, hint_valid)
                if (.not. hint_valid) then
                    write (error_unit, '(a,a)') &
                        'fo: invalid CMake preset build-root hint: ', &
                        context%error
                    exitcode = 1
                    return
                end if
            end if
        else
            call cmake_context_read_reply(context)
        end if

        deallocate (packed)
        n_args = 0
        call argv_push(packed, n_args, 'cmake')
        call argv_push(packed, n_args, '--build')
        if (len_trim(context%build_preset) > 0) then
            call argv_push(packed, n_args, '--preset')
            call argv_push(packed, n_args, context%build_preset)
        else
            if (len_trim(context%configure_preset) > 0 .and. &
                    .not. context%build_root_hint) then
                write (error_unit, '(a)') &
                    'fo: CMake build needs a preset or build-root hint'
                exitcode = 1
                return
            end if
            call argv_push(packed, n_args, context%build_root)
        end if
        if (len_trim(context%configuration) > 0) then
            call argv_push(packed, n_args, '--config')
            call argv_push(packed, n_args, context%configuration)
        end if
        call argv_push(packed, n_args, '-j')
        call argv_push(packed, n_args, jobs_text)
        call process_run_argv_logged(context%source_root, packed, n_args, &
            log_file, .true., environment_timeout('FO_BUILD_TIMEOUT', 300), &
            exitcode)
    end subroutine cmake_build

    subroutine cmake_test(context, regex, include_slow, log_file, exitcode)
        type(cmake_context_t), intent(in) :: context
        character(len=*), intent(in) :: regex, log_file
        logical, intent(in) :: include_slow
        integer, intent(out) :: exitcode

        character(len=:), allocatable :: packed
        character(len=32) :: jobs_text, timeout_text
        logical :: has_tests
        integer :: n_args

        if (len_trim(context%configure_preset) > 0 .and. &
                .not. context%build_root_hint .and. &
                len_trim(context%test_preset) == 0) then
            write (error_unit, '(a)') &
                'fo: CTest root for configure preset is unknown; supply '// &
                'FO_CMAKE_BUILD_DIR as a locator hint or select a test preset'
            exitcode = 1
            return
        end if
        has_tests = len_trim(context%test_preset) > 0
        if (.not. has_tests) inquire ( &
            file=cmake_context_build_path(context)//'/CTestTestfile.cmake', &
            exist=has_tests)
        if (.not. has_tests) then
            exitcode = 0
            return
        end if

        write (jobs_text, '(i0)') detect_jobs()
        n_args = 0
        call argv_push(packed, n_args, 'ctest')
        if (len_trim(context%test_preset) > 0) then
            call argv_push(packed, n_args, '--preset')
            call argv_push(packed, n_args, context%test_preset)
        else
            call argv_push(packed, n_args, '--test-dir')
            call argv_push(packed, n_args, context%build_root)
        end if
        if (len_trim(context%configuration) > 0) then
            call argv_push(packed, n_args, '-C')
            call argv_push(packed, n_args, context%configuration)
        end if
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
        ! FO_TEST_TIMEOUT is a per-test limit. It used to bound the whole ctest
        ! run, so a suite of individually fast tests failed once their sum
        ! passed ten seconds. ctest can only enforce wall time per test, so it
        ! gets the generous wall-clock cap that accompanies the CPU budget. The
        ! whole run keeps only a day-long backstop, which also keeps the
        ! still-running heartbeats flowing.
        write (timeout_text, '(i0)') &
            test_wall_cap_seconds(budget=test_timeout_seconds())
        call argv_push(packed, n_args, '--timeout')
        call argv_push(packed, n_args, timeout_text)
        call process_run_argv_logged(context%source_root, packed, n_args, log_file, &
            .false., 86400, exitcode)
    end subroutine cmake_test

    logical function generator_is_multi(context) result(is_multi)
        type(cmake_context_t), intent(in) :: context
        character(len=512) :: generator, cache_file, line
        integer :: unit, ios

        generator = context%generator
        if (len_trim(context%reported_generator) > 0) &
            generator = context%reported_generator
        if (index(generator, 'Visual Studio') == 1 .or. &
                trim(generator) == 'Xcode' .or. &
                trim(generator) == 'Ninja Multi-Config') then
            is_multi = .true.
            return
        end if
        cache_file = trim(context%source_root)//'/'//trim(context%build_root)// &
            '/CMakeCache.txt'
        open (newunit=unit, file=trim(cache_file), status='old', action='read', &
            iostat=ios)
        if (ios == 0) then
            do
                read (unit, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (index(line, 'CMAKE_GENERATOR:INTERNAL=') == 1) then
                    is_multi = index(line, 'Visual Studio') > 0 .or. &
                        index(line, 'Xcode') > 0 .or. &
                        index(line, 'Ninja Multi-Config') > 0
                    close (unit)
                    return
                end if
            end do
            close (unit)
        end if
        is_multi = .false.
    end function generator_is_multi

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
        character(len=*), intent(in) :: names(MAX_TEST_TARGETS)
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
