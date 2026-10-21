module fo_build_backend
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_mkdir_excl, fs_sleep_ms, &
        fs_pid_alive, fs_native_path, fs_is_windows, fs_realpath, &
        fs_path_is_absolute, fs_parent_path
    use fo_process, only: process_detect_nproc, process_getpid, &
        process_getcwd, process_run_argv_logged, argv_push
    use fo_gfortran_build, only: gfortran_build, gfortran_test, &
        gfortran_run_tests
    use fo_util, only: read_text_file, make_tmpfile, delete_tmpfile
    use fo_ctest_run, only: ctest_import_junit
    use fx_string, only: string_t
    use fo_test_budget, only: test_timeout_seconds, test_wall_cap_seconds
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t, &
        selected_compiler_command, COMPILER_UNKNOWN
    use fo_cmake_context, only: cmake_context_t, cmake_context_init, &
        cmake_context_query, cmake_context_read_reply, &
        cmake_context_build_path, cmake_context_validate_hint
    use fo_cmake_native_config, only: native_cmake_selected, native_cmake_configure
    use fo_cmake_native_model, only: cm_plan_t
    use fo_cmake_native, only: native_cmake_build, native_cmake_test
    use fo_cmake_build, only: cmake_route_t, cmake_build_route, cmake_build_native
    use fo_input_inventory, only: cmake_headers_revalidate
    implicit none
    private
    public :: backend_t, detect_backend, detect_nproc, detect_jobs
    public :: backend_build, backend_test, backend_test_names
    public :: backend_test_cases, backend_set_build_targets
    public :: backend_clean
    public :: profile_flags, backend_profile, cmake_configure
    public :: BACKEND_NONE, BACKEND_NATIVE, BACKEND_CMAKE

    integer, parameter :: BACKEND_NONE = 0
    integer, parameter :: BACKEND_NATIVE = 1
    integer, parameter :: BACKEND_CMAKE = 2

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
        if (len_trim(current) == 0) then
            b%project_dir = ''
            return
        end if
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

            call fs_parent_path(current, parent)
            if (trim(parent) == trim(current)) exit
            current = parent
        end do

        b%kind = BACKEND_NONE
        b%project_dir = ''
    end function detect_backend

    logical function cmake_declares_tests(filename) result(declares_tests)
        !! True when the top-level file unconditionally declares a CTest contract.
        !! A declaration nested in if()/foreach()/function()/macro()/while()/block()
        !! (for example behind an optional feature) does not make CMake the test
        !! authority: with the option off it registers no tests at all.
        character(len=*), intent(in) :: filename

        character(len=2048) :: line, lowered
        integer :: unit, iostat, i, code, depth, hash

        declares_tests = .false.
        open (newunit=unit, file=filename, status='old', action='read', &
            iostat=iostat)
        if (iostat /= 0) return
        depth = 0
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            lowered = line
            do i = 1, len_trim(lowered)
                code = iachar(lowered(i:i))
                if (code >= iachar('A') .and. code <= iachar('Z')) &
                    lowered(i:i) = achar(code + iachar('a') - iachar('A'))
            end do
            hash = index(lowered, '#')
            if (hash > 0) lowered(hash:) = ''
            lowered = adjustl(lowered)
            if (command_is(lowered, 'if') .or. command_is(lowered, 'foreach') .or. &
                command_is(lowered, 'while') .or. command_is(lowered, 'macro') .or. &
                command_is(lowered, 'function') .or. &
                command_is(lowered, 'block')) then
                depth = depth + 1
            else if (command_is(lowered, 'endif') .or. &
                    command_is(lowered, 'endforeach') .or. &
                    command_is(lowered, 'endwhile') .or. &
                    command_is(lowered, 'endmacro') .or. &
                    command_is(lowered, 'endfunction') .or. &
                    command_is(lowered, 'endblock')) then
                depth = max(0, depth - 1)
            else if (depth == 0) then
                if (command_is(lowered, 'enable_testing') .or. &
                    (command_is(lowered, 'include') .and. &
                    index(lowered, 'include(ctest') > 0)) then
                    declares_tests = .true.
                    exit
                end if
            end if
        end do
        close (unit)
    end function cmake_declares_tests

    logical function command_is(text, name) result(matches)
        !! True when the (lowercased, left-adjusted) line invokes CMake command name.
        character(len=*), intent(in) :: text, name
        integer :: n

        matches = .false.
        n = len(name)
        if (len_trim(text) <= n) return
        if (text(1:n) /= name) return
        matches = text(n + 1:n + 1) == '(' .or. text(n + 1:n + 1) == ' '
        if (matches) matches = index(adjustl(text(n + 1:)), '(') == 1
    end function command_is

    function absolute_dir(dir) result(absdir)
        character(len=*), intent(in) :: dir
        character(len=512) :: absdir, pwd
        character(len=:), allocatable :: native
        integer :: cwd_ierr
        logical :: resolved

        native = fs_native_path(dir)
        if (fs_is_windows()) then
            call fs_realpath(native, absdir, resolved)
            if (.not. resolved) absdir = ''
            return
        end if
        if (len(native) == 0) then
            absdir = '.'
        else if (fs_path_is_absolute(native)) then
            absdir = native
        else
            call process_getcwd(pwd, cwd_ierr)
            if (cwd_ierr /= 0) pwd = ''
            pwd = fs_native_path(pwd)
            absdir = native
            if (len_trim(pwd) > 0) then
                absdir = trim(pwd)
                if (native /= '.') absdir = trim(pwd)//'/'//native
            end if
        end if
    end function absolute_dir

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

    subroutine backend_build(self, exitcode, flags, log_file, with_tests, use_cache, &
            configured, include_slow)
        !! configured: the CMake tree was configured by this command already
        !! (its model is loaded), so the build does not configure again.
        !! include_slow: with_tests also builds the slow fpm tests.
        type(backend_t), intent(inout) :: self
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: flags
        character(len=*), intent(in), optional :: log_file
        logical, intent(in), optional :: with_tests
        logical, intent(in), optional :: use_cache
        logical, intent(in), optional :: configured
        logical, intent(in), optional :: include_slow

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
                    include_slow=include_slow, flags=flag_text, &
                    use_cache=use_cache)
            else
                call gfortran_build(self%project_dir, log_path, exitcode, &
                    flags=flag_text, use_cache=use_cache)
            end if
        case (BACKEND_CMAKE)
            call cmake_build(self%cmake, flag_text, log_path, exitcode, configured)
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
        backend%cmake%profile = trim(name)
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

    subroutine backend_test(self, exitcode, include_slow, log_file, flags, use_cache, &
            no_build)
        !! Whole-suite run without a project selection (fpm, frozen native
        !! CMake). Model-free CMake callers get the default `slow` label
        !! exclusion; `fo test` selects on the model (backend_test_cases).
        type(backend_t), intent(inout) :: self
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache
        logical, intent(in), optional :: no_build

        character(len=128) :: no_names(1)
        character(len=512) :: log_path, lock_dir, flag_text
        integer :: lock_ierr
        logical :: slow, skip_build

        slow = .false.
        if (present(include_slow)) slow = include_slow
        skip_build = .false.
        if (present(no_build)) skip_build = no_build
        log_path = ''
        if (present(log_file)) log_path = log_file
        flag_text = ''
        if (present(flags)) flag_text = flags

        if (self%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            return
        end if

        exitcode = 0
        if (.not. skip_build) then
            call acquire_project_lock(self%project_dir, lock_dir, lock_ierr)
            if (lock_ierr /= 0) then
                exitcode = 1
                return
            end if
            select case (self%kind)
            case (BACKEND_NATIVE)
                call gfortran_test(self%project_dir, log_path, exitcode, &
                    include_slow=slow, flags=flag_text, &
                    use_cache=use_cache)
            case (BACKEND_CMAKE)
                call cmake_build(self%cmake, flag_text, log_path, exitcode)
            end select
            call release_project_lock(lock_dir)
        end if
        if (exitcode == 0) then
            select case (self%kind)
            case (BACKEND_NATIVE)
                no_names = ''
                call gfortran_run_tests(self%project_dir, log_path, exitcode, &
                    slow, no_names, 0, flags=flag_text)
            case (BACKEND_CMAKE)
                if (native_cmake_selected(self%project_dir)) then
                    call native_cmake_test(self%cmake, [character(len=1) ::], &
                        log_path, exitcode)
                else
                    call cmake_test(self%cmake, '', .false., .not. slow, &
                        log_path, .false., exitcode)
                end if
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
            log_file, flags, use_cache, no_build, build_only)
        !! Exact named fpm tests (the model's selection, run by
        !! project_run_cases) and the registered cases of the frozen
        !! FO_CMAKE_NATIVE route, which has no project model. Slow handling
        !! belongs to the model's `slow` label, not to this runner.
        type(backend_t), intent(inout) :: self
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache
        logical, intent(in), optional :: no_build
        !! Build what the named tests need and return, without running.
        logical, intent(in), optional :: build_only

        integer :: lock_ierr
        logical :: slow, skip_build
        character(len=512) :: log_path, lock_dir, flag_text
        character(len=:), allocatable :: regex

        slow = .false.
        if (present(include_slow)) slow = include_slow
        skip_build = .false.
        if (present(no_build)) skip_build = no_build
        exitcode = 0
        log_path = ''
        if (present(log_file)) log_path = log_file
        flag_text = ''
        if (present(flags)) flag_text = flags
        if (n_names == 0) return

        if (self%kind == BACKEND_NONE) then
            exitcode = 1
            return
        end if

        if (.not. skip_build) then
            call acquire_project_lock(self%project_dir, lock_dir, lock_ierr)
            if (lock_ierr /= 0) then
                exitcode = 1
                return
            end if
            select case (self%kind)
            case (BACKEND_NATIVE)
                call gfortran_test(self%project_dir, log_path, exitcode, &
                    include_slow=slow, flags=flag_text, use_cache=use_cache, &
                    names=names, n_names=n_names)
            case (BACKEND_CMAKE)
                call cmake_build(self%cmake, flag_text, log_path, exitcode)
            end select
            call release_project_lock(lock_dir)
            if (exitcode /= 0) return
        end if
        if (present(build_only)) then
            if (build_only) return
        end if

        select case (self%kind)
        case (BACKEND_NATIVE)
            call gfortran_run_tests(self%project_dir, log_path, exitcode, slow, &
                names, n_names, flags=flag_text)
        case (BACKEND_CMAKE)
            if (native_cmake_selected(self%project_dir)) then
                call native_cmake_test(self%cmake, names(:n_names), &
                    log_path, exitcode)
            else
                call names_to_ctest_regex(names, n_names, regex)
                call cmake_test(self%cmake, regex, .false., .false., log_path, &
                    .false., exitcode)
            end if
        end select
    end subroutine backend_test_names

    subroutine backend_set_build_targets(self, targets, widened)
        !! CMake build targets of a plan: the planned targets merged with any
        !! FO_CMAKE_BUILD_TARGETS the user set. A widened plan (a case whose
        !! needs are unknown) builds the user's targets, or the default
        !! target set when there are none, plus the targets it does know.
        type(backend_t), intent(inout) :: self
        type(string_t), intent(in) :: targets(:)
        logical, intent(in) :: widened
        type(string_t), allocatable :: merged(:)
        character(len=:), allocatable :: kept(:)
        integer :: i, j, n
        logical :: seen

        if (self%kind /= BACKEND_CMAKE) return
        if (.not. allocated(self%cmake%build_targets)) &
            allocate (character(len=1) :: self%cmake%build_targets(0))
        if (widened .and. size(targets) == 0) return
        if (widened .and. size(self%cmake%build_targets) == 0) then
            ! The default set has a name only when other targets join it.
            call append_arg_list(self%cmake%build_targets, default_target_name())
        end if
        allocate (merged(size(targets) + size(self%cmake%build_targets)))
        n = 0
        do i = 1, size(targets)
            n = n + 1
            merged(n)%s = targets(i)%s
        end do
        if (allocated(self%cmake%build_targets)) then
            do i = 1, size(self%cmake%build_targets)
                seen = .false.
                do j = 1, n
                    if (merged(j)%s == trim(self%cmake%build_targets(i))) seen = .true.
                end do
                if (seen) cycle
                n = n + 1
                merged(n)%s = trim(self%cmake%build_targets(i))
            end do
        end if
        j = 1
        do i = 1, n
            j = max(j, len(merged(i)%s))
        end do
        allocate (character(len=j) :: kept(n))
        do i = 1, n
            kept(i) = merged(i)%s
        end do
        call move_alloc(kept, self%cmake%build_targets)

    contains

        function default_target_name() result(name)
            !! CMake's name of the default build target for the generator.
            character(len=:), allocatable :: name

            name = 'all'
            if (.not. allocated(self%cmake%reported_generator)) return
            if (index(self%cmake%reported_generator, 'Visual Studio') == 1 .or. &
                self%cmake%reported_generator == 'Xcode') name = 'ALL_BUILD'
        end function default_target_name

        subroutine append_arg_list(values, token)
            character(len=:), allocatable, intent(inout) :: values(:)
            character(len=*), intent(in) :: token
            character(len=:), allocatable :: grown(:)

            allocate (character(len=max(len(values), len(token))) :: &
                grown(size(values) + 1))
            grown(:size(values)) = values
            grown(size(values) + 1) = token
            call move_alloc(grown, values)
        end subroutine append_arg_list

    end subroutine backend_set_build_targets

    subroutine backend_test_cases(self, names, n_names, exitcode, log_file, &
            flags, no_build, configured, lift_preset_filter, build_only, append_log)
        !! Run exact CTest cases of a configured CMake project: build (the
        !! caller has set the plan targets, backend_set_build_targets) unless
        !! no_build, then ctest in batches with exact anchored names. A batch
        !! is one ctest invocation, so fixtures run once per batch.
        !! lift_preset_filter: a selected case is excluded by the test preset
        !! filter; override the preset's label filter for this run.
        !! build_only: stop after the build (native CTest execution follows).
        !! append_log: the log already holds results; ctest appends to it.
        type(backend_t), intent(inout) :: self
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: log_file
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: no_build, configured, lift_preset_filter
        logical, intent(in), optional :: build_only, append_log

        integer, parameter :: batch_bytes = 32000
        character(len=512) :: log_path, lock_dir, flag_text
        character(len=:), allocatable :: regex
        integer :: lock_ierr, first, last, code
        logical :: skip_build, lift, after_results

        exitcode = 0
        log_path = ''
        if (present(log_file)) log_path = log_file
        flag_text = ''
        if (present(flags)) flag_text = flags
        skip_build = .false.
        if (present(no_build)) skip_build = no_build
        lift = .false.
        if (present(lift_preset_filter)) lift = lift_preset_filter
        after_results = .false.
        if (present(append_log)) after_results = append_log
        if (self%kind /= BACKEND_CMAKE .or. n_names < 1) then
            exitcode = 1
            return
        end if

        if (.not. skip_build) then
            call acquire_project_lock(self%project_dir, lock_dir, lock_ierr)
            if (lock_ierr /= 0) then
                exitcode = 1
                return
            end if
            call cmake_build(self%cmake, flag_text, log_path, exitcode, configured)
            call release_project_lock(lock_dir)
            if (exitcode /= 0) then
                if (exitcode == 124) write (error_unit, '(a)') &
                    'fo: WARNING: build timed out (FO_BUILD_TIMEOUT exceeded)'
                return
            end if
        end if

        if (present(build_only)) then
            if (build_only) return
        end if

        first = 1
        do while (first <= n_names)
            last = first
            call ctest_batch_end(names, n_names, first, batch_bytes, last)
            call names_to_ctest_regex(names(first:last), last - first + 1, regex)
            call cmake_test(self%cmake, regex, lift, .false., log_path, &
                first > 1 .or. after_results, code)
            if (code /= 0) exitcode = code
            first = last + 1
        end do
        if (exitcode == 124) then
            write (error_unit, '(a)') &
                'fo: WARNING: tests timed out (CPU budget FO_TEST_TIMEOUT or' // &
                ' wall cap FO_TEST_WALL_TIMEOUT exceeded); raise them or set' // &
                ' [extra.fo] test-timeout'
        end if
    end subroutine backend_test_cases

    subroutine ctest_batch_end(names, n_names, first, limit, last)
        !! Last name of the batch that starts at first: the escaped regex
        !! stays below limit bytes, a batch holds at least one name.
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names, first, limit
        integer, intent(out) :: last
        integer :: i, k, used, width

        used = 0
        last = first - 1
        do i = first, n_names
            width = len_trim(names(i)) + 1
            do k = 1, len_trim(names(i))
                select case (names(i)(k:k))
                case ('.', '^', '$', '*', '+', '?', '(', ')', '[', ']', '{', '}', &
                        '|', achar(92))
                    width = width + 1
                end select
            end do
            if (i > first .and. used + width > limit) exit
            used = used + width
            last = i
        end do
    end subroutine ctest_batch_end

    recursive subroutine cmake_configure(context, flags, log_file, exitcode)
        type(cmake_context_t), intent(inout) :: context
        character(len=*), intent(in) :: flags, log_file
        integer, intent(out) :: exitcode

        character(len=:), allocatable :: packed, cache_file, configuration
        character(:), allocatable :: effective_flags, selected_flags, compiler
        type(compiler_dialect_t) :: dialect
        type(cm_plan_t) :: native_plan
        logical :: has_cache, hint_valid, has_frozen_context
        character(len=4096) :: captured_generator
        character(len=4096) :: header_message
        integer :: n_args, i, line_end

        call cmake_headers_revalidate(context%source_root, exitcode, header_message)
        if (exitcode /= 0) then
            write(error_unit, '(a)') trim(header_message)
            return
        end if
        if (native_cmake_selected(context%source_root)) then
            call native_cmake_configure(context, native_plan)
            exitcode = 0
            if (len(native_plan%error) > 0) then
                write(error_unit, '(a)') native_plan%error
                exitcode = 1
            else
                context%configuration = native_plan%configuration
            end if
            return
        end if

        effective_flags = flags
        if (allocated(context%profile)) then
            if (len_trim(context%profile) > 0 .and. len_trim(flags) > 0) then
                selected_flags = profile_flags(context%profile)
                if (len(selected_flags) > 0) then
                    if (index(flags, selected_flags) == 1) then
                        ! CMake owns compiler selection, including inherited presets.
                        ! Configure first so Fo's defaults follow the real compiler;
                        ! retain separately supplied project/user flags unchanged.
                        call cmake_configure(context, '', log_file, exitcode)
                        if (exitcode /= 0) return
                        call cmake_cache_compiler(context, compiler)
                        if (len(compiler) > 0) then
                            dialect = compiler_dialect(compiler)
                            if (dialect%kind == COMPILER_UNKNOWN) then
                                write (error_unit, '(a,a)') &
                                    'fo: unsupported CMake profile compiler: ', compiler
                                exitcode = 1
                                return
                            end if
                            effective_flags = dialect%profile_flags(context%profile)// &
                                flags(len(selected_flags) + 1:)
                        end if
                    end if
                end if
            end if
        end if
        if (len_trim(context%configure_preset) == 0 .and. &
            len_trim(context%generator) == 0) then
            inquire(file=context%source_root//'/.fo-cmake/generator.txt', &
                exist=has_frozen_context)
            if (has_frozen_context) then
                call read_text_file(context%source_root// &
                    '/.fo-cmake/generator.txt', captured_generator)
                line_end = index(captured_generator, new_line('a'))
                if (line_end > 0) captured_generator = &
                    captured_generator(:line_end - 1)
                context%generator = trim(captured_generator)
            end if
        end if
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
                '-DCMAKE_Fortran_FLAGS_'//configuration//'='//trim(effective_flags))
        end if
        if (allocated(context%extra_args)) then
            do i = 1, size(context%extra_args)
                call argv_push(packed, n_args, context%extra_args(i))
            end do
        end if
        inquire(file=context%source_root//'/.fo-cmake/cache.cmake', &
            exist=has_frozen_context)
        if (has_frozen_context) then
            call argv_push(packed, n_args, '-C')
            call argv_push(packed, n_args, context%source_root// &
                '/.fo-cmake/cache.cmake')
        end if
        call process_run_argv_logged(context%source_root, packed, n_args, &
            log_file, .false., environment_timeout('FO_BUILD_TIMEOUT', 300), &
            exitcode)
        if (exitcode /= 0) return
        if (len_trim(context%configure_preset) > 0) then
            if (.not. context%build_root_hint) then
                ! Let CMake expand inherited presets and choose binaryDir.
                ! Its final configure record locates the File API request;
                ! the second configure validates that request and source.
                call locate_preset_build(context, log_file, hint_valid)
                if (.not. hint_valid) then
                    write (error_unit, '(a)') &
                        'fo: CMake did not report its preset build directory'
                    exitcode = 1
                    return
                end if
                call cmake_configure(context, flags, log_file, exitcode)
                return
            end if
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

    end subroutine cmake_configure

    subroutine cmake_cache_compiler(context, compiler)
        type(cmake_context_t), intent(in) :: context
        character(:), allocatable, intent(out) :: compiler
        character(len=4096) :: line
        integer :: unit, ios, colon, equals

        compiler = ''
        open (newunit=unit, file=cmake_context_build_path(context)// &
            '/CMakeCache.txt', status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            colon = index(line, ':')
            if (colon < 1) cycle
            if (line(:colon - 1) /= 'CMAKE_Fortran_COMPILER') cycle
            equals = index(line, '=')
            if (equals > colon) compiler = trim(line(equals + 1:))
            exit
        end do
        close (unit)
    end subroutine cmake_cache_compiler

    subroutine locate_preset_build(context, log_file, found)
        type(cmake_context_t), intent(inout) :: context
        character(len=*), intent(in) :: log_file
        logical, intent(out) :: found
        character(len=*), parameter :: marker = &
            '-- Build files have been written to: '
        character(len=4096) :: line
        character(:), allocatable :: build, prefix
        integer :: unit, ios

        found = .false.
        build = ''
        open (newunit=unit, file=log_file, status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, marker) /= 1) cycle
            if (len_trim(line) == len(line)) cycle
            build = trim(line(len(marker) + 1:))
        end do
        close (unit)
        if (len(build) == 0) return
        if (build(1:1) /= '/') return
        prefix = trim(context%source_root)//'/'
        context%build_root = build
        if (index(build, prefix) == 1) &
            context%build_root = build(len(prefix) + 1:)
        context%build_root_hint = .true.
        found = .true.
    end subroutine locate_preset_build

    subroutine cmake_build(context, flags, log_file, exitcode, configured)
        type(cmake_context_t), intent(inout) :: context
        character(len=*), intent(in) :: flags, log_file
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: configured
        character(len=4096) :: message

        ! Project code can force cache hints back to absolute SDK paths. Those
        ! delegated inputs must still match the captured bytes, even on a hit.
        call cmake_headers_revalidate(context%source_root, exitcode, message)
        if (exitcode == 0) then
            call cmake_build_impl(context, flags, log_file, exitcode, configured)
        end if
        if (exitcode == 0) &
            call cmake_headers_revalidate(context%source_root, exitcode, message)
        if (len_trim(message) > 0) write(error_unit, '(a)') trim(message)
    end subroutine cmake_build

    subroutine cmake_build_impl(context, flags, log_file, exitcode, configured)
        type(cmake_context_t), intent(inout) :: context
        character(len=*), intent(in) :: flags, log_file
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: configured
        logical :: skip_configure
        type(cmake_route_t) :: route
        character(len=:), allocatable :: saved_targets(:)
        integer :: i, width

        if (native_cmake_selected(context%source_root)) then
            if (len_trim(flags) > 0) then
                if (len(context%profile) == 0) then
                    write(error_unit, '(a)') &
                        'native CMake: declare compiler flags through CMake inputs'
                    exitcode = 1
                    return
                end if
                if (trim(flags) /= trim(profile_flags(context%profile))) then
                    write(error_unit, '(a)') &
                        'native CMake: custom CLI compiler flags unsupported'
                    exitcode = 1
                    return
                end if
            end if
            call native_cmake_build(context, log_file, exitcode)
            return
        end if

        skip_configure = .false.
        if (present(configured)) skip_configure = configured
        exitcode = 0
        if (.not. skip_configure) call cmake_configure(context, flags, log_file, exitcode)
        if (exitcode /= 0) return
        ! CMake only configured. Fo builds the targets it can compile itself;
        ! targets with custom commands or generated sources, which must run
        ! first, are the only ones left to `cmake --build --target`.
        call cmake_build_route(context, route, log_file)
        if (.not. route%usable) then
            call cmake_delegate_build(context, log_file, exitcode)
            return
        end if
        if (route%n_delegated > 0) then
            call move_alloc(context%build_targets, saved_targets)
            width = 1
            do i = 1, route%n_delegated
                width = max(width, len(route%delegated(i)%s))
            end do
            allocate (character(len=width) :: &
                context%build_targets(route%n_delegated))
            do i = 1, route%n_delegated
                context%build_targets(i) = route%delegated(i)%s
            end do
            call cmake_delegate_build(context, log_file, exitcode)
            call move_alloc(saved_targets, context%build_targets)
            if (exitcode /= 0) return
        end if
        call cmake_build_native(route, log_file, exitcode)
    end subroutine cmake_build_impl

    subroutine cmake_delegate_build(context, log_file, exitcode)
        !! `cmake --build` for the context's build targets (all when none).
        type(cmake_context_t), intent(inout) :: context
        character(len=*), intent(in) :: log_file
        integer, intent(out) :: exitcode
        character(len=:), allocatable :: packed
        character(len=32) :: jobs_text
        integer :: n_args, i

        write (jobs_text, '(i0)') detect_jobs()
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
        if (allocated(context%build_targets)) then
            if (size(context%build_targets) > 0) then
                call argv_push(packed, n_args, '--target')
                do i = 1, size(context%build_targets)
                    call argv_push(packed, n_args, context%build_targets(i))
                end do
            end if
        end if
        call argv_push(packed, n_args, '-j')
        call argv_push(packed, n_args, jobs_text)
        call process_run_argv_logged(context%source_root, packed, n_args, &
            log_file, .true., environment_timeout('FO_BUILD_TIMEOUT', 300), &
            exitcode)
    end subroutine cmake_delegate_build

    subroutine cmake_test(context, regex, lift_preset_filter, exclude_slow, &
            log_file, append, exitcode)
        !! exclude_slow: the model-free default exclusion, label `slow` only.
        type(cmake_context_t), intent(in) :: context
        character(len=*), intent(in) :: regex, log_file
        logical, intent(in) :: lift_preset_filter, exclude_slow, append
        integer, intent(out) :: exitcode

        character(len=:), allocatable :: packed
        character(len=32) :: jobs_text, timeout_text
        character(len=4096) :: junit_file
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
        if (exclude_slow) then
            call argv_push(packed, n_args, '-LE')
            call argv_push(packed, n_args, '^slow$')
        else if (lift_preset_filter .and. len_trim(context%test_preset) > 0) then
            ! A command-line label filter replaces the preset's, and this
            ! one excludes no label.
            call argv_push(packed, n_args, '-LE')
            call argv_push(packed, n_args, '^$')
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
        ! Verdicts come from the JUnit file; the console stays the fallback.
        call make_tmpfile('fo-ctest-junit', junit_file)
        call argv_push(packed, n_args, '--output-junit')
        call argv_push(packed, n_args, junit_file)
        call process_run_argv_logged(context%source_root, packed, n_args, log_file, &
            append, 86400, exitcode)
        call ctest_import_junit(trim(junit_file), log_file)
        call delete_tmpfile(junit_file)
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
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        character(len=:), allocatable, intent(out) :: regex

        integer :: i

        regex = '^('
        do i = 1, n_names
            if (i > 1) regex = regex//"|"
            call append_ctest_regex_name(regex, names(i))
        end do
        regex = regex//")$"
    end subroutine names_to_ctest_regex

    subroutine append_ctest_regex_name(regex, name)
        character(len=:), allocatable, intent(inout) :: regex
        character(len=*), intent(in) :: name

        character(len=1) :: ch
        integer :: i

        do i = 1, len_trim(name)
            ch = name(i:i)
            select case (ch)
            case ('.', '^', '$', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|')
                regex = regex//achar(92)//ch
            case (achar(92))
                regex = regex//achar(92)//achar(92)
            case default
                regex = regex//ch
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
