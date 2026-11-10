module fo_hook
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, dp => real64
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE, BACKEND_CMAKE, &
        cmake_configured_build_dir, cmake_configure
    use fo_project_model, only: project_model_t, model_request_t, model_selection_t
    use fo_project_load, only: project_affected, project_resolve, configure_error_excerpt, &
        project_cmake_delta
    use fo_cmake_delta, only: cmake_delta_t
    use fo_gremlin_context, only: case_identity_capture
    use fo_gremlin_generation, only: generation_t
    use fo_gremlin_request, only: gremlin_json_text_valid
    use fo_exec_service, only: exec_job_t, exec_start, exec_poll, EXEC_UNCANCELLED
    use fo_cmake_generation, only: cmake_stage_git, cmake_populated_dependency_args, &
        cmake_unpopulated_dependency, cmake_cache_compiler_args
    use fo_cmake_context, only: cmake_context_build_path
    use fo_hook_presets, only: hook_stage_presets
    use fo_hook_evidence, only: hook_input_key, hook_receipt_read, &
        hook_receipt_write, hook_slow_case, hook_prepared_key
    use fo_test_results, only: test_result_entry_t, parse_test_results
    use fo_fmt, only: fmt_sources, FMT_SELECT_FILES, fmt_check_lines, fmt_diff_lines
    use fo_process, only: process_run_argv_logged, argv_push, process_self_executable, &
        process_setenv_default, &
        process_getpid, process_exit, process_has_fortran_source_ext, TIMEOUT_WALL
    use fo_fs, only: fs_mkdir_excl, fs_remove_tree, fs_canonical_path, fs_sleep_ms, &
        fs_make_dir, fs_remove_file
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file_alloc, &
        wall_time_seconds, int_text
    use fx_hash, only: sha256_file, sha256_string
    use fx_string, only: string_t
    use fx_path, only: path_dirname, path_basename
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_key_string, json_key_int, json_key_bool, &
        json_key, json_array_start, json_array_end, json_value_string, json_to_string
    implicit none
    private
    public :: hook_cli
    real(dp), save :: commit_deadline = 0.0_dp
    interface
        integer(c_int) function c_prepare(root, view, cap, reuse, offline) &
                bind(C, name='fo_c_hook_prepare')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), view(*), reuse(*)
            integer(c_int), value :: cap, offline
        end function c_prepare
        ! A blocking close-on-exec flock: it ends with the process, so a
        ! killed commit never blocks the next one.
        integer(c_int) function c_hook_lock(path) bind(C, name='fo_sys_lock_fixture')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_hook_lock
        subroutine c_hook_unlock(fd) bind(C, name='fo_sys_unlock')
            import :: c_int
            integer(c_int), value :: fd
        end subroutine c_hook_unlock
        integer(c_int) function c_unsetenv(name) bind(C, name='fo_c_unsetenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
        end function c_unsetenv
    end interface
contains
    subroutine hook_cli()
        character(len=4096) :: argument, owned_view
        logical :: json, bounded, prepare, view_given
        integer :: i, code

        json = .false.
        bounded = .false.
        prepare = .false.
        view_given = .false.
        owned_view = ''
        call get_command_argument(2, argument)
        if (trim(argument) /= 'pre-commit') then
            write(error_unit, '(a)') 'usage: fo hook pre-commit [--json] [--bounded | --prepare]'
            call process_exit(2)
        end if
        do i = 3, command_argument_count()
            call get_command_argument(i, argument)
            select case (trim(argument))
            case ('--json')
                json = .true.
            case ('--bounded')
                bounded = .true.
            case ('--prepare')
                prepare = .true.
            case default
                if (index(argument, '--view=') == 1) then
                    view_given = .true.
                    owned_view = argument(8:)
                    cycle
                end if
                write(error_unit, '(a)') 'fo hook: unknown option '//trim(argument)
                call process_exit(2)
            end select
        end do
        if (prepare .and. (bounded .or. view_given)) then
            write(error_unit, '(a)') 'fo hook: --prepare cannot be combined with --bounded or --view'
            call process_exit(2)
        end if
        if (bounded) then
            call bounded_check(json, code)
        else
            call staged_check(json, code, trim(owned_view), prepare)
        end if
        call process_exit(code)
    end subroutine hook_cli

    subroutine bounded_check(json, code)
        logical, intent(in) :: json
        integer, intent(out) :: code
        character(len=4096) :: driver, log
        character(len=:), allocatable :: root, snapshot, packed, text, message
        type(string_t), allocatable :: empty(:)
        integer :: status, n, attempt, cleanup
        logical :: found, owned, done
        type(exec_job_t) :: job

        ! Reserve two seconds of the approximate ten-second commit budget
        ! for owned-process drainage and snapshot cleanup after cancellation.
        commit_deadline = wall_time_seconds() + 8.0_dp
        code = 1
        owned = .false.
        allocate(empty(0))
        message = 'staged verification incomplete; prepare with: fo hook pre-commit --prepare'
        call make_tmpfile('fo-bounded-hook', log)
        call process_self_executable(driver, found)
        validation: block
            if (.not. found) exit validation
            call git_text('.', git_words('rev-parse', '--show-toplevel'), root, status)
            if (status /= 0) exit validation
            root = fs_canonical_path(root)
            do attempt = 1, 100
                snapshot = path_dirname(root)//'/.fo-hook-'// &
                    int_text(process_getpid())//'-'//int_text(attempt)
                if (fs_mkdir_excl(snapshot) /= 0) cycle
                owned = .true.
                exit
            end do
            if (.not. owned) exit validation
            n = 0
            packed = ''
            call argv_push(packed, n, trim(driver))
            call argv_push(packed, n, 'hook')
            call argv_push(packed, n, 'pre-commit')
            call argv_push(packed, n, '--view='//snapshot)
            if (json) call argv_push(packed, n, '--json')
            if (wall_time_seconds() >= commit_deadline) exit validation
            ! An owned session cancels the whole tree at the deadline; a
            ! process group would leave nested fo commands running in the
            ! view removed below.
            call exec_start(job, root, packed, n, trim(log), status, &
                timeout_seconds=max(1, int(commit_deadline - wall_time_seconds())))
            if (status /= 0) exit validation
            do
                call exec_poll(job, done, cleanup)
                if (done) exit
                call fs_sleep_ms(20)
            end do
            status = job%exitcode
            if (status == EXEC_UNCANCELLED) then
                owned = .false.
                message = message//'; process drainage failed; retained '//snapshot
                exit validation
            end if
            if (job%timed_out) exit validation
            call read_text_file_alloc(trim(log), text, cleanup)
            if (cleanup /= 0 .or. len_trim(text) == 0) exit validation
            if (json) then
                if (.not. gremlin_json_text_valid(text)) exit validation
            end if
            code = status
            message = ''
        end block validation
        if (owned) then
            call remove_owned_view(snapshot, cleanup)
            if (cleanup /= 0) then
                code = 1
                message = 'cannot remove owned staged view: '//snapshot
            end if
        end if
        if (len(message) == 0) then
            write(output_unit, '(a)') trim(text)
        else
            call report(json, code, '', '', .false., .false., 0, empty, empty, message)
        end if
        call delete_tmpfile(trim(log))
    end subroutine bounded_check

    subroutine staged_check(json, code, owned_view, prepare)
        logical, intent(in) :: json, prepare
        integer, intent(out) :: code
        character(len=*), intent(in) :: owned_view
        type(backend_t) :: backend
        type(project_model_t) :: model
        type(model_request_t) :: request
        type(model_selection_t) :: selection
        type(generation_t) :: generation
        type(cmake_delta_t) :: delta
        type(test_result_entry_t), allocatable :: results(:)
        type(string_t), allocatable :: changed(:), reasons(:), deferred(:), warnings(:)
        character(len=4096) :: driver, logfile, resultfile, dependency_message
        character(len=64) :: driver_digest, key, prepared_key
        character(len=:), allocatable :: root, snapshot, tree, text, message, packed, child_env
        character(len=:), allocatable :: reuse, populated, backend_name, refresh_message
        type(string_t), allocatable :: excluded(:), included(:)
        character(len=4096), allocatable :: files(:)
        character(len=:), allocatable :: formatting
        integer :: status, n, i, j, fast, passed, nfiles, timeout_kind, cap, budget
        integer :: cleanup, attempts, nresults, r, test_cap
        real :: seconds
        real(dp) :: started, elapsed, threshold
        logical :: found, snapshot_owned, hit, built, identity_ok, populated_found, has_base
        logical :: fortran, refreshed, has_fpm, shared
        logical, allocatable :: added(:)
        integer(c_int) :: lock_fd
        character(len=:), allocatable :: state, previous

        code = 1
        message = ''
        tree = ''
        snapshot = ''
        key = ''
        hit = .false.
        built = .false.
        snapshot_owned = .false.
        shared = .false.
        lock_fd = -1
        fast = 0
        passed = 0
        allocate(deferred(0), warnings(0), changed(0))
        call make_tmpfile('fo-hook-log', logfile)
        call make_tmpfile('fo-hook-result', resultfile)
        call process_self_executable(driver, found)
        reuse = ''
        child_env = ''
        backend_name = 'native'
        started = wall_time_seconds()
        validation: block
            if (.not. found) then
                message = 'cannot resolve the running Fo image'
                exit validation
            end if
            call git_text('.', git_words('rev-parse', '--show-toplevel'), root, status)
            if (status /= 0) then
                message = 'not a Git working tree'
                exit validation
            end if
            root = fs_canonical_path(root)
            call git_text(root, git_words('write-tree'), tree, status)
            if (status /= 0) then
                message = 'cannot snapshot the index (resolve conflicts first)'
                exit validation
            end if
            ! A CMake project whose configured build enables no Fortran keeps
            ! its .f90 files as data, such as a compiler's test inputs, and
            ! its build is not one Fo can stage. A linked worktree without its
            ! own build stands on the configuration of its main checkout.
            call git_text(root, git_words('cat-file', '-e', ':fpm.toml'), text, status)
            has_fpm = status == 0
            if (status /= 0) then
                call hook_cmake_build_dir(root, populated, populated_found, fortran)
                if (populated_found .and. .not. fortran) then
                    message = 'configured CMake build enables no Fortran: '// &
                        populated//'; nothing to check'
                    code = 0
                    exit validation
                end if
            end if
            call git_text(root, git_words('diff-tree', '--no-commit-id', &
                '--name-only', '-r', '-z', 'HEAD', tree), text, status, strip=.false.)
            has_base = status == 0
            if (status /= 0) call git_text(root, git_words(&
                'ls-tree', '-r', '-z', '--name-only', tree), text, status, strip=.false.)
            if (status /= 0) then
                message = 'cannot enumerate staged paths'
                exit validation
            end if
            deallocate(changed)
            call split_paths(text, changed)
            ! Documentation that no build script or source names cannot change
            ! what is built or tested, so such a commit needs no build.
            if (has_base .and. inert_paths(root, tree, changed)) then
                message = 'fo hook: only documentation staged; nothing to build or test'
                code = 0
                exit validation
            end if
            budget = config_seconds(root, 'fo.hookBudget', 10)
            ! A cold build in a fresh checkout routinely exceeds a minute; incremental
            ! builds reuse the shared cache, so the cap only stops runaway builds.
            cap = config_seconds(root, 'fo.hookBuildCap', 900)
            threshold = real(config_seconds(root, 'fo.hookFastThreshold', 1), dp)
            if (len(owned_view) > 0) then
                if (index(owned_view, path_dirname(root)//'/.fo-hook-') /= 1) then
                    message = 'invalid owned staged view'
                    exit validation
                end if
                snapshot = owned_view
            else if (.not. has_fpm .and. .not. prepare .and. &
                    shared_view(root, snapshot, state)) then
                ! A CMake build is not relocatable, so incremental reuse needs one
                ! fixed staged view per repository, shared by its worktrees.
                ! Preparation keeps the offline view whose receipts the bounded
                ! hook reuses.
                call fs_make_dir(state, status)
                if (status == 0) lock_fd = c_hook_lock(state//'/lock'//c_null_char)
                if (lock_fd < 0) then
                    message = 'cannot lock the shared staged build in '//state
                    exit validation
                end if
                shared = .true.
            else
                do attempts = 1, 100
                    snapshot = path_dirname(root)//'/.fo-hook-'//int_text(process_getpid())// &
                        '-'//int_text(attempts)
                    if (fs_mkdir_excl(snapshot) == 0) then
                        snapshot_owned = .true.
                        exit
                    end if
                end do
            end if
            if (.not. snapshot_owned .and. .not. shared .and. len(owned_view) == 0) then
                message = 'cannot create an isolated staged view beside the repository'
                exit validation
            end if
            if (shared) then
                call update_view(root, snapshot, tree, state//'/index', status)
            else
                call checkout_tree(root, snapshot, tree, status)
            end if
            if (status /= 0) then
                message = 'cannot materialize staged tree or pinned submodule (initialize it first)'
                exit validation
            end if
            allocate(files(size(changed)))
            nfiles = 0
            do i = 1, size(changed)
                if (.not. process_has_fortran_source_ext(changed(i)%s)) cycle
                inquire(file=snapshot//'/'//changed(i)%s, exist=found)
                if (.not. found) cycle
                nfiles = nfiles + 1
                files(nfiles) = changed(i)%s
            end do
            if (has_base) then
                ! Only lines this commit adds or changes are checked, so legacy
                ! formatting elsewhere in a touched file never blocks a commit.
                formatting = ''
                status = 0
                do i = 1, nfiles
                    call git_text(root, git_words('diff-tree', '-p', '-U0', 'HEAD', tree, &
                        '--', ':(literal)'//trim(files(i))), text, j, strip=.false.)
                    if (j /= 0) then
                        message = 'cannot diff staged '//trim(files(i))
                        exit validation
                    end if
                    call fmt_diff_lines(text, .false., added)
                    call merge_parent_lines(root, tree, trim(files(i)), added, j)
                    if (j /= 0) then
                        message = 'cannot diff staged '//trim(files(i))//' against MERGE_HEAD'
                        exit validation
                    end if
                    call fmt_check_lines(snapshot, trim(files(i)), added, formatting, status)
                end do
            else
                formatting = repeat(' ', 16384)
                call fmt_sources(snapshot, FMT_SELECT_FILES, .true., .false., files, &
                    nfiles, formatting, status)
            end if
            if (status /= 0) then
                message = trim(formatting)
                if (index(message, ': needs formatting') > 0) message = message// &
                    'run: fo fmt --changed, then stage the reformatted files'
                exit validation
            end if
            ! A project already configured with CMake keeps its populated
            ! dependency trees: the staged copy is pointed at them, because its own
            ! configure is offline. This runs before the backend
            ! is initialized so the effective configure arguments include it.
            call hook_cmake_build_dir(root, populated, populated_found)
            if (shared) then
                ! The shared build fetches what the staged commit pins into its own
                ! _deps and keeps it across commits; FetchContent updates a
                ! dependency exactly when its pinned reference changes. Roots of
                ! a developer build may hold other revisions, so only its
                ! compilers are reused.
                if (populated_found) reuse = cmake_cache_compiler_args(populated)
                call reset_offline_build(snapshot//'/build')
            else if (.not. populated_found .and. len(owned_view) == 0) then
                ! The bounded hook keeps its deadline and never acquires anything.
                call populate_cmake_dependencies(root, populated, populated_found, &
                    message)
                if (len(message) > 0) exit validation
            end if
            if (populated_found .and. .not. shared) then
                call cmake_populated_dependency_args(populated, &
                    'build/_deps', reuse, status, dependency_message)
                if (status /= 0) then
                    message = trim(dependency_message)
                    exit validation
                end if
            end if
            status = c_prepare(root//c_null_char, snapshot//c_null_char, int(cap, c_int), &
                reuse//c_null_char, merge(0_c_int, 1_c_int, shared))
            if (status /= 0) then
                message = 'cannot prepare private build/dependency view'
                exit validation
            end if
            backend = detect_backend(snapshot, authority_dir=root)
            if (backend%kind == BACKEND_NONE) then
                message = 'no supported staged Fortran project'
                exit validation
            end if
            if (backend%kind == BACKEND_CMAKE) then
                backend_name = 'cmake'
                if (len_trim(backend%cmake%configure_preset) + &
                    len_trim(backend%cmake%build_preset) + &
                    len_trim(backend%cmake%test_preset) > 0) then
                    call hook_stage_presets(snapshot, status, message)
                    if (status /= 0) exit validation
                end if
                ! Always configure/build this index view, even when the caller
                ! has an absolute FO_CMAKE_BUILD_DIR or a preset binaryDir.
                backend%cmake%build_root = 'build'
                backend%cmake%build_root_hint = .true.
                ! Preserve the project's CMake build rules in the staged view.
                child_env = 'FO_CMAKE_BUILD_DIR=build;FO_CMAKE_DELEGATE_BUILD=1'
                if (shared) then
                    ! Provenance of the previous commit; this one is captured anew.
                    call fs_remove_file(snapshot//'/.git')
                    call fs_remove_tree(snapshot//'/.fo-cmake/project-git')
                end if
                call cmake_stage_git(root, snapshot, status, message)
                if (status /= 0) exit validation
            end if
            allocate(request%names(0), request%include_labels(0), &
                request%exclude_labels(0), request%layers(0))
            request%all = .true.
            ! A commit never runs a comparison that needs a reference checkout or
            ! a network. The project names those cases with its own CTest labels;
            ! each comma-separated entry of fo.hookExcludeLabels is one pattern
            ! handled exactly as `fo test -LE` handles one `-LE` argument.
            call config_labels(root, 'fo.hookExcludeLabels', excluded)
            call config_labels(root, 'fo.hookIncludeLabels', included)
            if (size(excluded) > 0) request%exclude_labels = excluded
            if (size(included) > 0) request%include_labels = included
            ! A listfile edit that only registers tests runs those tests, not all.
            if (has_base) then
                call project_cmake_delta(root, 'HEAD', snapshot, changed, delta)
            else
                call project_cmake_delta(root, '', snapshot, [string_t ::], delta)
            end if
            refreshed = .not. populated_found .or. len(owned_view) > 0 .or. &
                backend%kind /= BACKEND_CMAKE .or. shared
            do
                if (prepare) then
                    call project_resolve(backend, '', request, model, selection, status, &
                        message)
                else
                    call project_affected(backend, '', request, changed, model, selection, &
                        reasons, status, message, delta=delta)
                end if
                if (refreshed .or. status == 124) exit
                ! The reused build may predate a dependency this commit declares:
                ! the offline staged configure then leaves its source empty.
                ! Configure this checkout once with network access and retry
                ! with the dependency roots it populates.
                refresh_message = cmake_unpopulated_dependency(snapshot//'/build')
                if (len(refresh_message) == 0) exit
                refreshed = .true.
                previous = populated
                call populate_cmake_dependencies(root, populated, populated_found, &
                    refresh_message, refresh=.true., compilers_from=previous)
                if (len(refresh_message) > 0 .or. .not. populated_found) then
                    status = 1
                    message = 'cannot fetch a dependency the reused CMake build '// &
                        'lacks'//new_line('a')//refresh_message
                    exit
                end if
                call cmake_populated_dependency_args(populated, 'build/_deps', reuse, &
                    status, dependency_message)
                if (status /= 0) then
                    message = trim(dependency_message)
                    exit
                end if
                ! The shared build reconfigures onto the new roots in place.
                if (.not. shared) call fs_remove_tree(snapshot//'/build', status)
                status = c_prepare(root//c_null_char, snapshot//c_null_char, &
                    int(cap, c_int), reuse//c_null_char, 1_c_int)
                if (status /= 0) then
                    message = 'cannot prepare private build/dependency view'
                    exit
                end if
                ! The backend parsed the earlier CMake arguments; take the new roots.
                backend = detect_backend(snapshot, authority_dir=root)
                backend%cmake%build_root = 'build'
                backend%cmake%build_root_hint = .true.
            end do
            if (backend%kind == BACKEND_CMAKE .and. .not. json) &
                write(error_unit, '(a)') toolchain_line(snapshot//'/build', reuse)
            if (status /= 0) then
                if (status == 124) message = &
                    'staged build exceeded hard cap during CMake configure'
                if (shared .and. status /= 124 .and. index(message, 'FetchContent') > 0) then
                    ! FetchContent reports a failed population as "Build step for
                    ! <name> failed"; older layouts name the _deps directory.
                    text = dependency_names(message)
                    j = index(message, 'Build step for ')
                    if (j > 0) then
                        i = index(message(j + 15:), ' failed')
                        if (i > 1) text = message(j + 15:j + 13 + i)
                    end if
                    if (len(text) > 0) message = 'cannot fetch the CMake dependencies '// &
                        'this commit pins; missing dependency: '//text//new_line('a')//message
                end if
                ! The hook allows no network fetch, so say how to populate them.
                if (trim(message) == 'cannot acquire dependencies') then
                    inquire(file=root//'/CMakeLists.txt', exist=found)
                    if (found) then
                        message = 'dependencies are not populated in this checkout '// &
                            'and a commit never fetches them; configure its CMake '// &
                            'build directory or run `fo build` here once, then commit again'
                    else
                        message = 'dependencies are not populated in this checkout '// &
                            'and a commit never fetches them; run `fo build` here once, '// &
                            'then commit again'
                    end if
                end if
                exit validation
            end if
            call sha256_file(trim(driver), driver_digest, status)
            identity_ok = .false.
            if (status == 0) call case_identity_capture(backend, driver_digest, '', &
                generation, identity_ok, configured=.true.)
            if (identity_ok) key = hook_input_key(generation)
            prepared_key = hook_prepared_key(trim(key), model, request)
            if (identity_ok .and. model%complete) then
                hit = hook_receipt_read(trim(key)) .or. hook_receipt_read(trim(prepared_key))
            end if
            do i = 1, size(selection%cases)
                j = selection%cases(i)
                if (hook_slow_case(model%cases(j)) .or. &
                    model%cases(j)%default_excluded) then
                    call push(deferred, model%cases(j)%name)
                else
                    fast = fast + 1
                end if
            end do
            if (hit) then
                passed = fast
                built = .true.
                code = 0
                exit validation
            end if
            packed = ''
            n = 0
            call argv_push(packed, n, trim(driver))
            call argv_push(packed, n, 'build')
            call process_run_argv_logged(snapshot, packed, n, logfile, .false., cap, &
                status, env_extra=child_env, heartbeat_s=0, timeout_kind=timeout_kind, &
                cpu_budget_s=86400)
            if (status /= 0) then
                message = 'staged build failed'
                if (timeout_kind == TIMEOUT_WALL .or. status == 124) &
                    message = 'staged build exceeded hard cap'
                if (populated_found .and. len(trim(reuse)) > 0) then
                    message = trim(message)//'; this commit reused the '// &
                        'populated CMake dependencies from the build tree, '// &
                        'which CMake never re-fetches - run `fo update` and '// &
                        'rebuild if they are stale'
                end if
                do i = 1, size(selection%cases)
                    j = selection%cases(i)
                    if (hook_slow_case(model%cases(j))) cycle
                    if (model%cases(j)%default_excluded) cycle
                    call push(deferred, model%cases(j)%name)
                end do
                exit validation
            end if
            built = .true.
            do i = 1, size(selection%cases)
                j = selection%cases(i)
                if (hook_slow_case(model%cases(j))) cycle
                if (model%cases(j)%default_excluded) cycle
                elapsed = wall_time_seconds() - started
                if (.not. prepare .and. elapsed >= real(budget, dp)) then
                    call push(deferred, model%cases(j)%name)
                    cycle
                end if
                packed = ''
                n = 0
                call argv_push(packed, n, trim(driver))
                call argv_push(packed, n, 'test')
                call argv_push(packed, n, '--rerun')
                call argv_push(packed, n, '--retries')
                call argv_push(packed, n, '0')
                call argv_push(packed, n, '--')
                call argv_push(packed, n, model%cases(j)%name)
                test_cap = cap
                if (.not. prepare) test_cap = max(1, int(real(budget, dp) - elapsed))
                call process_run_argv_logged(snapshot, packed, n, resultfile, .false., &
                    test_cap, status, heartbeat_s=0, &
                    timeout_kind=timeout_kind, wall_seconds=seconds, &
                    env_extra=child_env//';FO_TEST_REPORT_NAMES=1', cpu_budget_s=86400)
                call parse_test_results(trim(resultfile), results, nresults, r)
                if (timeout_kind == TIMEOUT_WALL) then
                    call push(warnings, model%cases(j)%name)
                else
                    do r = 1, nresults
                        if (real(results(r)%seconds, dp) > threshold) &
                            call push(warnings, results(r)%name)
                    end do
                end if
                if (timeout_kind == TIMEOUT_WALL) then
                    call push(deferred, model%cases(j)%name)
                    cycle
                end if
                if (status /= 0) then
                    message = 'fast test failed: '//model%cases(j)%name
                    call copy_log(resultfile, logfile)
                    do r = i + 1, size(selection%cases)
                        if (hook_slow_case(model%cases(selection%cases(r)))) cycle
                        if (model%cases(selection%cases(r))%default_excluded) cycle
                        call push(deferred, model%cases(selection%cases(r))%name)
                    end do
                    exit
                end if
                if (nresults < 1) then
                    message = 'fast test produced no result: '//model%cases(j)%name
                    call copy_log(resultfile, logfile)
                    exit
                end if
                if (any(results(:nresults)%status /= 'PASS')) then
                    call push(deferred, model%cases(j)%name)
                    cycle
                end if
                passed = passed + 1
            end do
            if (len(message) > 0) exit validation
            if (prepare) then
                if (len_trim(prepared_key) /= 64 .or. .not. model%complete .or. passed /= fast) then
                    message = 'fast test preparation incomplete; no receipt written'
                    exit validation
                end if
                call hook_receipt_write(trim(prepared_key))
            end if
            if (len(owned_view) > 0) then
                if (.not. identity_ok .or. .not. model%complete .or. passed /= fast) then
                    message = 'staged verification incomplete; prepare with: '// &
                        'fo hook pre-commit --prepare'
                    exit validation
                end if
            end if
            ! Only a complete fast tier is a reusable receipt. An affected
            ! subset is useful evidence, but cannot stand for every fast test.
            if (.not. prepare .and. model%complete .and. passed == count_model_fast(model)) &
                call hook_receipt_write(trim(key))
            code = 0
        end block validation
        if (snapshot_owned) then
            call remove_owned_view(snapshot, cleanup)
            if (cleanup /= 0) then
                code = 1
                message = 'cannot remove owned staged view: '//snapshot
            end if
        end if
        if (lock_fd >= 0) call c_hook_unlock(lock_fd)
        if (code /= 0) then
            call read_text_file_alloc(trim(logfile), text, status)
            if (status == 0) then
                if (len(text) > 2000) text = text(len(text) - 1999:)
                message = message//achar(10)//text
            end if
        end if
        call report(json, code, tree, key, hit, built, passed, deferred, warnings, &
            message, backend_name, excluded)
        call delete_tmpfile(trim(logfile))
        call delete_tmpfile(trim(resultfile))
    end subroutine staged_check

    integer function count_model_fast(model) result(n)
        type(project_model_t), intent(in) :: model
        integer :: i

        n = 0
        do i = 1, model%n_cases
            if (model%cases(i)%explicit_only .or. model%cases(i)%expanded) cycle
            if (hook_slow_case(model%cases(i))) cycle
            n = n + 1
        end do
    end function count_model_fast

    recursive subroutine checkout_tree(source, destination, tree, status)
        !! A private index pins the tree even during a partially staged commit.
        !! Initialized submodules are read at their index gitlink, never HEAD.
        character(len=*), intent(in) :: source, destination, tree
        integer, intent(out) :: status
        character(len=4096) :: indexfile, log
        character(len=:), allocatable :: text, env, entry, path, commit
        type(string_t), allocatable :: entries(:)
        integer :: i, tab

        call make_tmpfile('fo-hook-index', indexfile)
        call make_tmpfile('fo-hook-checkout', log)
        env = 'GIT_INDEX_FILE='//trim(indexfile)
        call git_run(source, git_words('read-tree', tree), log, status, env)
        if (status == 0) call git_run(source, git_words(&
            'checkout-index', '--all', '--prefix='//destination//'/'), log, status, env)
        call delete_tmpfile(trim(indexfile))
        call delete_tmpfile(trim(log))
        if (status /= 0) return
        call git_text(source, git_words('ls-tree', '-r', '-z', tree), &
            text, status, strip=.false.)
        if (status /= 0) return
        call split_paths(text, entries)
        do i = 1, size(entries)
            entry = entries(i)%s
            if (index(entry, '160000 commit ') /= 1) cycle
            tab = index(entry, achar(9))
            if (tab == 0) then
                status = 1
                return
            end if
            commit = entry(15:tab - 1)
            path = entry(tab + 1:)
            call checkout_tree(source//'/'//path, destination//'/'//path, commit, status)
            if (status /= 0) return
        end do
    end subroutine checkout_tree

    subroutine reset_offline_build(build_dir)
        !! A shared build configured offline against a developer build's
        !! dependency roots keeps those roots in its cache; start it afresh.
        character(len=*), intent(in) :: build_dir
        character(len=:), allocatable :: cache
        integer :: ierr

        call read_text_file_alloc(build_dir//'/CMakeCache.txt', cache, ierr)
        if (ierr /= 0) return
        if (index(cache, 'FETCHCONTENT_FULLY_DISCONNECTED:BOOL=ON') > 0) &
            call fs_remove_tree(build_dir)
    end subroutine reset_offline_build

    function toolchain_line(build_dir, reuse) result(line)
        !! The compilers the staged build uses and where each choice came from.
        character(len=*), intent(in) :: build_dir, reuse
        character(len=:), allocatable :: line
        character(len=*), parameter :: languages(3) = [character(len=7) :: &
            'Fortran', 'C', 'CXX']
        character(len=*), parameter :: variables(3) = [character(len=3) :: 'FC', 'CC', 'CXX']
        character(len=:), allocatable :: cache, entry, origin
        integer :: k, at, finish, length, ierr

        line = 'fo hook: toolchain'
        call read_text_file_alloc(build_dir//'/CMakeCache.txt', cache, ierr)
        if (ierr /= 0) cache = ''
        do k = 1, 3
            ! Position p of the newline in the prefixed text is where the entry
            ! starts in the cache itself.
            at = index(new_line('a')//cache, new_line('a')//'CMAKE_'// &
                trim(languages(k))//'_COMPILER:')
            if (at == 0) cycle
            finish = index(cache(at:), new_line('a'))
            if (finish == 0) finish = len(cache) - at + 2
            entry = cache(at:at + finish - 2)
            call get_environment_variable(trim(variables(k)), length=length)
            if (length > 0) then
                origin = trim(variables(k))
            else if (index(reuse, '-DCMAKE_'//trim(languages(k))//'_COMPILER=') > 0) then
                origin = 'reused build'
            else
                origin = 'CMake'
            end if
            line = line//' '//trim(languages(k))//' '//entry(index(entry, '=') + 1:)// &
                ' ('//origin//')'
        end do
    end function toolchain_line

    logical function inert_paths(root, tree, paths) result(inert)
        !! Documentation, images and hosted CI configuration (.github/,
        !! .gitlab/, .forgejo/, .gitlab-ci.yml: run by the forge, never by a
        !! local build) are inert unless a file other than these names them, as
        !! a build script installing a page would.
        character(len=*), intent(in) :: root, tree
        type(string_t), intent(in) :: paths(:)
        character(len=*), parameter :: documents(*) = [character(len=9) :: &
            'md', 'markdown', 'rst', 'adoc', 'png', 'jpg', 'jpeg', 'gif', 'svg', 'pdf']
        character(len=:), allocatable :: packed, text, extension
        integer :: i, k, n, dot, status

        inert = .false.
        if (size(paths) > 256) return
        packed = ''
        n = 0
        call argv_push(packed, n, 'grep')
        call argv_push(packed, n, '-q')
        call argv_push(packed, n, '-F')
        do i = 1, size(paths)
            if (.not. hosted_ci(paths(i)%s)) then
                dot = index(paths(i)%s, '.', back=.true.)
                if (dot == 0 .or. dot < index(paths(i)%s, '/', back=.true.)) return
                extension = paths(i)%s(dot + 1:)
                do k = 1, len(extension)
                    if (extension(k:k) >= 'A' .and. extension(k:k) <= 'Z') &
                        extension(k:k) = achar(iachar(extension(k:k)) + 32)
                end do
                if (.not. any(documents == extension)) return
            end if
            call argv_push(packed, n, '-e')
            call argv_push(packed, n, path_basename(paths(i)%s))
        end do
        call argv_push(packed, n, tree)
        call argv_push(packed, n, '--')
        call argv_push(packed, n, '.')
        do k = 1, size(documents)
            call argv_push(packed, n, ':(exclude)*.'//trim(documents(k)))
            call argv_push(packed, n, ':(exclude)*.'//upper(trim(documents(k))))
        end do
        call argv_push(packed, n, ':(exclude).github')
        call argv_push(packed, n, ':(exclude).gitlab')
        call argv_push(packed, n, ':(exclude).forgejo')
        call argv_push(packed, n, ':(exclude).gitlab-ci.yml')
        ! git grep exits 1 when nothing matches and higher on errors.
        call git_text(root, packed, text, status)
        inert = status == 1
    contains
        logical function hosted_ci(path)
            character(len=*), intent(in) :: path
            hosted_ci = index(path, '.github/') == 1 .or. index(path, '.gitlab/') == 1 .or. &
                index(path, '.forgejo/') == 1 .or. path == '.gitlab-ci.yml'
        end function hosted_ci

        function upper(word) result(out)
            character(len=*), intent(in) :: word
            character(len=len(word)) :: out
            integer :: j
            out = word
            do j = 1, len(word)
                if (word(j:j) >= 'a' .and. word(j:j) <= 'z') &
                    out(j:j) = achar(iachar(word(j:j)) - 32)
            end do
        end function upper
    end function inert_paths

    logical function shared_view(root, view, state) result(found)
        !! One staged view per repository, beside its main checkout so that
        !! relative sibling paths keep their meaning, and its private index and
        !! lock in the common Git directory.
        character(len=*), intent(in) :: root
        character(len=:), allocatable, intent(out) :: view, state
        character(len=:), allocatable :: common, main
        integer :: status

        found = .false.
        view = ''
        state = ''
        call git_text(root, git_words('rev-parse', '--path-format=absolute', &
            '--git-common-dir'), common, status)
        if (status /= 0 .or. len(common) <= 5) return
        if (common(len(common) - 4:) /= '/.git') return
        common = fs_canonical_path(common)
        main = path_dirname(common)
        view = path_dirname(main)//'/.fo-staged-'//path_basename(main)
        state = common//'/fo-hook'
        found = .true.
    end function shared_view

    recursive subroutine update_view(source, view, tree, index_file, status)
        !! Bring a persistent view to `tree`, rewriting only files whose content
        !! differs so the build tool recompiles exactly what changed. Initialized
        !! submodules follow their staged gitlinks, as in checkout_tree.
        character(len=*), intent(in) :: source, view, tree, index_file
        integer, intent(out) :: status
        character(len=4096) :: log
        character(len=:), allocatable :: text, entry, path, commit
        type(string_t), allocatable :: entries(:)
        integer :: i, tab
        logical :: known

        ! Without its index the view's content is unknown; start it afresh.
        inquire(file=index_file, exist=known)
        if (.not. known) call fs_remove_tree(view)
        call fs_make_dir(view, status)
        if (status /= 0) return
        call make_tmpfile('fo-hook-view', log)
        call git_run(source, git_words('--work-tree='//view, 'read-tree', '--reset', &
            '-u', tree), log, status, 'GIT_INDEX_FILE='//index_file)
        call delete_tmpfile(trim(log))
        if (status /= 0) then
            call fs_remove_file(index_file)
            return
        end if
        call git_text(source, git_words('ls-tree', '-r', '-z', tree), &
            text, status, strip=.false.)
        if (status /= 0) return
        call split_paths(text, entries)
        do i = 1, size(entries)
            entry = entries(i)%s
            if (index(entry, '160000 commit ') /= 1) cycle
            tab = index(entry, achar(9))
            if (tab == 0) then
                status = 1
                return
            end if
            commit = entry(15:tab - 1)
            path = entry(tab + 1:)
            call update_view(source//'/'//path, view//'/'//path, commit, &
                index_file//'-'//sha256_string(path), status)
            if (status /= 0) return
        end do
    end subroutine update_view

    subroutine hook_cmake_build_dir(root, build_dir, found, fortran)
        !! The configured build of `root`, else that of its main checkout: a
        !! linked worktree without its own build stands on that configuration.
        character(len=*), intent(in) :: root
        character(len=:), allocatable, intent(out) :: build_dir
        logical, intent(out) :: found
        logical, intent(out), optional :: fortran
        character(len=:), allocatable :: common
        integer :: status

        call cmake_configured_build_dir(root, build_dir, found, fortran)
        if (found) return
        call git_text(root, git_words('rev-parse', '--path-format=absolute', &
            '--git-common-dir'), common, status)
        if (status /= 0 .or. len(common) <= 5) return
        if (common(len(common) - 4:) /= '/.git') return
        call cmake_configured_build_dir(path_dirname(common), build_dir, found, fortran)
    end subroutine hook_cmake_build_dir

    subroutine populate_cmake_dependencies(root, build_dir, found, message, refresh, &
            compilers_from)
        !! A fresh CMake checkout has never fetched what it fetches at configure
        !! time (FetchContent, ExternalProject), and the staged configure is
        !! offline. Configure the checkout's own build directory once, with
        !! network access, as `cmake -B build` would; later commits reuse it.
        !! `refresh` does the same when a reused build predates a dependency.
        character(len=*), intent(in) :: root
        character(len=:), allocatable, intent(out) :: build_dir, message
        logical, intent(out) :: found
        logical, intent(in), optional :: refresh
        !! A configured build whose compilers this configure keeps.
        character(len=*), intent(in), optional :: compilers_from
        type(backend_t) :: backend
        character(len=1024) :: log_file
        character(len=:), allocatable :: text, names
        integer :: exitcode, status, length, i
        character(len=*), parameter :: git_context(*) = [character(len=34) :: &
            'GIT_DIR', 'GIT_WORK_TREE', 'GIT_INDEX_FILE', 'GIT_COMMON_DIR', &
            'GIT_OBJECT_DIRECTORY', 'GIT_ALTERNATE_OBJECT_DIRECTORIES', 'GIT_PREFIX', &
            'GIT_ALLOW_PROTOCOL', 'FO_CMAKE_ARGS']
        type :: saved_value_t
            character(len=:), allocatable :: text
        end type saved_value_t
        type(saved_value_t) :: saved(size(git_context))
        logical :: kept(size(git_context))

        message = ''
        build_dir = ''
        found = .false.
        ! `git commit` exports its repository and index to the hook, and the
        ! staged view restricts Git to file transport and CMake to offline
        ! arguments. This online configure, including backend detection, must
        ! see none of them; the hook needs them back afterwards.
        do i = 1, size(git_context)
            call get_environment_variable(trim(git_context(i)), length=length, &
                status=status)
            kept(i) = status == 0
            if (.not. kept(i)) cycle
            allocate(character(len=length) :: saved(i)%text)
            if (length > 0) call get_environment_variable(trim(git_context(i)), &
                saved(i)%text)
            status = c_unsetenv(trim(git_context(i))//c_null_char)
        end do
        if (present(compilers_from)) then
            call process_setenv_default('FO_CMAKE_ARGS', &
                cmake_cache_compiler_args(compilers_from))
        end if
        call populate()
        status = c_unsetenv('FO_CMAKE_ARGS'//c_null_char)
        do i = 1, size(git_context)
            if (kept(i)) call process_setenv_default(trim(git_context(i)), saved(i)%text)
        end do
    contains
        subroutine populate()
            backend = detect_backend(root, authority_dir=root)
            if (backend%kind /= BACKEND_CMAKE) return
            call get_environment_variable('FO_CMAKE_BUILD_DIR', length=length, status=status)
            if ((status /= 0 .or. length == 0) .and. &
                len_trim(backend%cmake%configure_preset) == 0) then
                backend%cmake%build_root = 'build'
                backend%cmake%build_root_hint = .true.
            end if
            if (present(refresh)) then
                write (error_unit, '(a)') 'fo hook: the reused CMake build lacks a '// &
                    'dependency of this commit; configuring this checkout to fetch it'
            else
                write (error_unit, '(a)') 'fo hook: no configured CMake build; configuring '// &
                    'this checkout once to fetch its dependencies'
            end if
            call make_tmpfile('fo-hook-populate', log_file)
            call cmake_configure(backend%cmake, '', trim(log_file), exitcode)
            if (exitcode == 0) then
                call delete_tmpfile(trim(log_file))
                build_dir = cmake_context_build_path(backend%cmake)
                found = .true.
                return
            end if
            call read_text_file_alloc(trim(log_file), text, status)
            call delete_tmpfile(trim(log_file))
            if (status /= 0) text = ''
            names = dependency_names(text)
            message = 'cannot fetch the CMake dependencies of this checkout'
            if (len(names) > 0) message = message//'; missing dependency: '//names
            message = message//new_line('a')//trim(configure_error_excerpt(text))
        end subroutine populate
    end subroutine populate_cmake_dependencies

    function dependency_names(text) result(names)
        !! Dependencies a failed configure names by their FetchContent or
        !! ExternalProject directories (<base>/_deps/<name>-src and the like).
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: names
        character(len=:), allocatable :: name
        character(len=*), parameter :: delimiters = ' "''/'//new_line('a')
        integer :: at, finish, dash

        names = ''
        at = index(text, '/_deps/')
        do while (at > 0)
            at = at + 7
            finish = at - 1
            do while (finish < len(text))
                if (index(delimiters, text(finish + 1:finish + 1)) > 0) exit
                finish = finish + 1
            end do
            name = text(at:finish)
            dash = index(name, '-', back=.true.)
            if (dash > 1) then
                select case (name(dash + 1:))
                case ('src', 'build', 'subbuild', 'populate')
                    name = name(:dash - 1)
                    if (index(', '//names//',', ', '//name//',') == 0) then
                        if (len(names) > 0) names = names//', '
                        names = names//name
                    end if
                end select
            end if
            if (finish >= len(text)) exit
            dash = index(text(finish + 1:), '/_deps/')
            if (dash == 0) exit
            at = finish + dash
        end do
    end function dependency_names

    subroutine copy_log(source, destination)
        character(len=*), intent(in) :: source, destination
        character(len=:), allocatable :: text
        integer :: ierr
        call read_text_file_alloc(trim(source), text, ierr)
        if (ierr == 0) then
            block
                use fo_fs, only: fs_write_text
                call fs_write_text(trim(destination), text)
            end block
        end if
    end subroutine copy_log

    subroutine config_labels(root, name, labels)
        !! One regular expression per comma-separated entry of a Git config key.
        character(len=*), intent(in) :: root, name
        type(string_t), allocatable, intent(out) :: labels(:)
        character(len=:), allocatable :: text
        character(len=:), allocatable :: piece
        integer :: status, comma

        allocate (labels(0))
        call git_text(root, git_words('config', '--get-all', name), text, status)
        if (status /= 0) return
        text = adjustl(text)
        if (len_trim(text) == 0) return
        do
            comma = index(text, ',')
            if (comma == 0) then
                piece = trim(text)
            else
                piece = trim(text(:comma - 1))
            end if
            if (len_trim(piece) > 0) call append(labels, piece)
            if (comma == 0) exit
            text = adjustl(text(comma + 1:))
            if (len_trim(text) == 0) exit
        end do
    end subroutine config_labels

    subroutine append(labels, value)
        type(string_t), allocatable, intent(inout) :: labels(:)
        character(len=*), intent(in) :: value
        type(string_t), allocatable :: grown(:)
        integer :: n

        n = size(labels)
        allocate (grown(n + 1))
        if (n > 0) grown(:n) = labels
        grown(n + 1)%s = trim(value)
        call move_alloc(grown, labels)
    end subroutine append

    subroutine remove_owned_view(snapshot, status)
        !! The view belongs to this hook, so a child that is still leaving it
        !! is waited for rather than reported as a failed verification.
        character(len=*), intent(in) :: snapshot
        integer, intent(out) :: status
        integer :: attempt
        logical :: owned_left

        do attempt = 1, 20
            call fs_remove_tree(snapshot, status)
            if (status == 0) then
                inquire (file=snapshot, exist=owned_left)
                if (.not. owned_left) return
            end if
            if (attempt < 20) call fs_sleep_ms(250)
        end do
    end subroutine remove_owned_view

    integer function config_seconds(root, name, fallback) result(value)
        character(len=*), intent(in) :: root, name
        integer, intent(in) :: fallback
        character(len=:), allocatable :: text
        integer :: status, parsed

        value = fallback
        call git_text(root, git_words('config', '--int', name), text, status)
        if (status /= 0) return
        read(text, *, iostat=status) parsed
        if (status == 0) then
            if (parsed > 0) value = parsed
        end if
    end function config_seconds

    function git_words(first, second, third, fourth, fifth, sixth, seventh) result(words)
        character(len=*), intent(in) :: first
        character(len=*), intent(in), optional :: second, third, fourth, fifth, sixth, seventh
        character(len=:), allocatable :: words
        integer :: n

        words = ''
        n = 0
        call argv_push(words, n, first)
        if (present(second)) call argv_push(words, n, second)
        if (present(third)) call argv_push(words, n, third)
        if (present(fourth)) call argv_push(words, n, fourth)
        if (present(fifth)) call argv_push(words, n, fifth)
        if (present(sixth)) call argv_push(words, n, sixth)
        if (present(seventh)) call argv_push(words, n, seventh)
    end function git_words

    subroutine git_run(root, args, log, status, env)
        character(len=*), intent(in) :: root, args, log
        integer, intent(out) :: status
        character(len=*), intent(in), optional :: env
        character(len=:), allocatable :: packed
        integer :: n, i, cap

        packed = ''
        n = 0
        call argv_push(packed, n, 'git')
        call argv_push(packed, n, '-C')
        call argv_push(packed, n, root)
        if (root /= '.') then
            call argv_push(packed, n, '--git-dir='//root//'/.git')
            call argv_push(packed, n, '--work-tree='//root)
        end if
        packed = trim(packed)//trim(args)
        do i = 1, len(args)
            if (args(i:i) == achar(0)) n = n + 1
        end do
        cap = 10
        if (commit_deadline > 0.0_dp) then
            cap = int(commit_deadline - wall_time_seconds())
            if (cap < 1) then
                status = 124
                return
            end if
        end if
        call process_run_argv_logged(root, packed, n, log, .false., cap, status, &
            env_extra=env, heartbeat_s=0)
    end subroutine git_run

    subroutine merge_parent_lines(root, tree, path, added, status)
        !! A merge commit owns only the lines that differ from every parent, its
        !! conflict resolutions; lines arriving unchanged from a merged branch
        !! carry that branch's formatting and were judged by its own commits.
        character(len=*), intent(in) :: root, tree, path
        logical, intent(inout) :: added(:)
        integer, intent(out) :: status
        character(len=:), allocatable :: text, heads, merge_head
        logical, allocatable :: other(:)
        integer :: pos, eol, k, ierr

        status = 0
        call git_text(root, git_words('rev-parse', '--git-path', 'MERGE_HEAD'), merge_head, k)
        if (k /= 0 .or. len(merge_head) == 0) return
        if (merge_head(1:1) /= '/') merge_head = root//'/'//merge_head
        call read_text_file_alloc(merge_head, heads, ierr)
        if (ierr /= 0) return
        pos = 1
        do while (pos <= len(heads))
            eol = index(heads(pos:), achar(10))
            if (eol == 0) eol = len(heads) - pos + 2
            if (eol > 1) then
                call git_text(root, git_words('diff-tree', '-p', '-U0', heads(pos:pos + eol - 2), &
                    tree, '--', ':(literal)'//path), text, status, strip=.false.)
                if (status /= 0) return
                call fmt_diff_lines(text, .false., other)
                do k = 1, size(added)
                    added(k) = added(k) .and. k <= size(other)
                    if (added(k)) added(k) = other(k)
                end do
            end if
            pos = pos + eol
        end do
    end subroutine merge_parent_lines

    subroutine git_text(root, args, text, status, strip)
        character(len=*), intent(in) :: root, args
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        logical, intent(in), optional :: strip
        character(len=4096) :: log
        integer :: ierr

        call make_tmpfile('fo-hook-git', log)
        call git_run(root, args, log, status)
        call read_text_file_alloc(trim(log), text, ierr)
        if (ierr /= 0) text = ''
        call delete_tmpfile(trim(log))
        if (present(strip)) then
            if (.not. strip) return
        end if
        text = trim(text)
        if (len(text) > 0) then
            if (text(len(text):) == achar(10)) text = text(:len(text) - 1)
        end if
    end subroutine git_text

    subroutine push(items, text)
        type(string_t), allocatable, intent(inout) :: items(:)
        character(len=*), intent(in) :: text
        type(string_t), allocatable :: grown(:)
        integer :: i, n

        n = size(items)
        allocate(grown(n + 1))
        do i = 1, n
            grown(i)%s = items(i)%s
        end do
        grown(n + 1)%s = text
        call move_alloc(grown, items)
    end subroutine push

    subroutine split_paths(text, paths)
        character(len=*), intent(in) :: text
        type(string_t), allocatable, intent(out) :: paths(:)
        integer :: at, last

        allocate(paths(0))
        at = 1
        do while (at <= len(text))
            last = index(text(at:), achar(0))
            if (last == 0) exit
            call push(paths, text(at:at + last - 2))
            at = at + last
        end do
    end subroutine split_paths

    subroutine report(json, code, tree, key, hit, built, passed, deferred, warnings, &
            message, backend, excluded)
        logical, intent(in) :: json, hit, built
        integer, intent(in) :: code, passed
        character(len=*), intent(in) :: tree, key, message
        character(len=*), intent(in), optional :: backend
        type(string_t), intent(in), optional :: excluded(:)
        type(string_t), intent(in) :: deferred(:), warnings(:)
        type(json_builder_t) :: jb
        character(len=:), allocatable :: summary

        summary = 'fo hook: '
        if (code == 0) then
            summary = summary//'OK'
        else
            summary = summary//'FAIL'
        end if
        if (hit) summary = summary//' cache hit'
        if (built) summary = summary//' build PASS'
        summary = summary//'; fast PASS '//int_text(passed)//'; DEFERRED '// &
            int_text(size(deferred))//' to merge_group CI'
        if (size(warnings) > 0) summary = summary//'; slow fast-tier warnings '// &
            int_text(size(warnings))
        if (json) then
            jb = json_new()
            call json_object_start(jb)
            call json_key_string(jb, 'summary', summary)
            call json_key_string(jb, 'tree', tree)
            call json_key_string(jb, 'input_key', trim(key))
            call json_key_bool(jb, 'cache_hit', hit)
            call json_key_string(jb, 'build', trim(merge('PASS    ', 'UNPROVEN', built)))
            call json_key_int(jb, 'exit_code', code)
            call json_key_int(jb, 'fast_passed', passed)
            if (present(backend)) call json_key_string(jb, 'backend', backend)
            if (present(excluded)) then
                call json_key_int(jb, 'exclude_label_count', size(excluded))
                call names('exclude_labels', excluded)
            end if
            call json_key_int(jb, 'deferred_count', size(deferred))
            call names('deferred', deferred)
            call names('slow_fast_tests', warnings)
            call json_key_string(jb, 'diagnostic', message(:min(len(message), 2500)))
            call json_object_end(jb)
            write(output_unit, '(a)') json_to_string(jb)
        else
            write(output_unit, '(a)') summary
            if (len(message) > 0) write(error_unit, '(a)') message
            if (size(warnings) > 0) write(error_unit, '(a)') &
                'Mark these tests slow or speed them up (see doc/PRECOMMIT_PLAN.md):'
            block
                integer :: i
                do i = 1, min(32, size(warnings))
                    write(error_unit, '(a)') '  '//warnings(i)%s
                end do
            end block
        end if
    contains
        subroutine names(label, items)
            character(len=*), intent(in) :: label
            type(string_t), intent(in) :: items(:)
            integer :: k
            call json_key(jb, label)
            call json_array_start(jb)
            do k = 1, min(32, size(items))
                call json_value_string(jb, items(k)%s)
            end do
            call json_array_end(jb)
            call json_key_bool(jb, label//'_truncated', size(items) > 32)
        end subroutine names
    end subroutine report
end module fo_hook
