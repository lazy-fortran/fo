module fo_hook
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, dp => real64
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE, BACKEND_CMAKE, &
        cmake_configured_build_dir, backend_build
    use fo_cmake_context, only: cmake_context_adopt_query, cmake_context_read_reply
    use fo_project_model, only: project_model_t, model_request_t, model_selection_t
    use fo_project_load, only: project_affected
    use fo_gremlin_context, only: case_identity_capture
    use fo_gremlin_generation, only: generation_t
    use fo_gremlin_request, only: gremlin_json_text_valid
    use fo_cmake_generation, only: cmake_stage_git, cmake_populated_dependency_args
    use fo_hook_evidence, only: hook_input_key, hook_receipt_read, &
        hook_receipt_write, hook_slow_case
    use fo_test_results, only: test_result_entry_t, parse_test_results
    use fo_fmt, only: fmt_sources, FMT_SELECT_FILES
    use fo_process, only: process_run_argv_logged, argv_push, process_self_executable, &
        process_getpid, process_exit, process_has_fortran_source_ext, TIMEOUT_WALL
    use fo_fs, only: fs_mkdir_excl, fs_remove_tree, fs_canonical_path, fs_sleep_ms
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file_alloc, &
        wall_time_seconds, int_text
    use fx_hash, only: sha256_file
    use fx_string, only: string_t
    use fx_path, only: path_dirname
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_key_string, json_key_int, json_key_bool, &
        json_key, json_array_start, json_array_end, json_value_string, json_to_string
    implicit none
    private
    public :: hook_cli
    real(dp), save :: commit_deadline = 0.0_dp
    interface
        integer(c_int) function c_prepare(root, view, cap, reuse) bind(C, name='fo_c_hook_prepare')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), view(*), reuse(*)
            integer(c_int), value :: cap
        end function c_prepare
    end interface
contains
    subroutine hook_cli()
        character(len=4096) :: argument, owned_view
        logical :: json, bounded
        integer :: i, code

        json = .false.
        bounded = .false.
        owned_view = ''
        call get_command_argument(2, argument)
        if (trim(argument) == 'build-view') then
            call get_command_argument(3, argument)
            call build_configured_view(trim(argument), code)
            call process_exit(code)
        end if
        if (trim(argument) /= 'pre-commit') then
            write(error_unit, '(a)') 'usage: fo hook pre-commit [--json] [--bounded]'
            call process_exit(2)
        end if
        do i = 3, command_argument_count()
            call get_command_argument(i, argument)
            select case (trim(argument))
            case ('--json')
                json = .true.
            case ('--bounded')
                bounded = .true.
            case default
                if (index(argument, '--view=') == 1) then
                    owned_view = argument(8:)
                    cycle
                end if
                write(error_unit, '(a)') 'fo hook: unknown option '//trim(argument)
                call process_exit(2)
            end select
        end do
        if (bounded) then
            call bounded_check(json, code)
        else
            call staged_check(json, code, trim(owned_view))
        end if
        call process_exit(code)
    end subroutine hook_cli

    subroutine build_configured_view(token, code)
        !! The parent configured this private view already. Its exact File API
        !! request authorizes reuse while the child retains the build wall cap.
        character(len=*), intent(in) :: token
        integer, intent(out) :: code
        type(backend_t) :: backend
        character(len=4096) :: logfile
        character(len=:), allocatable :: text
        integer :: status

        code = 1
        if (len_trim(token) == 0 .or. command_argument_count() /= 3) return
        backend = detect_backend('.')
        if (backend%kind /= BACKEND_CMAKE) return
        call cmake_context_adopt_query(backend%cmake)
        if (backend%cmake%request_token /= token) return
        call cmake_context_read_reply(backend%cmake)
        if (.not. backend%cmake%request_seen .or. &
            .not. backend%cmake%has_codemodel .or. &
            .not. backend%cmake%has_toolchains) return
        call make_tmpfile('fo-hook-build', logfile)
        call backend_build(backend, code, log_file=trim(logfile), configured=.true.)
        if (code /= 0) then
            call read_text_file_alloc(trim(logfile), text, status)
            if (status == 0) write(error_unit, '(a)') text
        end if
        call delete_tmpfile(trim(logfile))
    end subroutine build_configured_view

    subroutine bounded_check(json, code)
        logical, intent(in) :: json
        integer, intent(out) :: code
        character(len=4096) :: driver, log
        character(len=:), allocatable :: root, snapshot, packed, text, message
        type(string_t), allocatable :: empty(:)
        integer :: status, n, attempt, cleanup, kind
        logical :: found, owned

        ! Reserve two seconds of the approximate ten-second commit budget
        ! for owned-process drainage and snapshot cleanup after cancellation.
        commit_deadline = wall_time_seconds() + 8.0_dp
        code = 1
        owned = .false.
        allocate(empty(0))
        message = 'staged verification incomplete; prepare with: fo hook pre-commit'
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
            call process_run_argv_logged(root, packed, n, log, .false., &
                max(1, int(commit_deadline - wall_time_seconds())), status, &
                heartbeat_s=0, cpu_budget_s=86400, timeout_kind=kind)
            if (status == 125) then
                owned = .false.
                message = message//'; process drainage failed; retained '//snapshot
                exit validation
            end if
            if (kind == TIMEOUT_WALL .or. status == 124) exit validation
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

    subroutine staged_check(json, code, owned_view)
        logical, intent(in) :: json
        integer, intent(out) :: code
        character(len=*), intent(in) :: owned_view
        type(backend_t) :: backend
        type(project_model_t) :: model
        type(model_request_t) :: request
        type(model_selection_t) :: selection
        type(generation_t) :: generation
        type(test_result_entry_t), allocatable :: results(:)
        type(string_t), allocatable :: changed(:), reasons(:), deferred(:), warnings(:)
        character(len=4096) :: driver, logfile, resultfile, dependency_message
        character(len=64) :: driver_digest, key
        character(len=:), allocatable :: root, snapshot, tree, text, message, packed, child_env
        character(len=:), allocatable :: reuse, populated, backend_name
        type(string_t), allocatable :: excluded(:), included(:)
        character(len=4096), allocatable :: files(:)
        character(len=16384) :: formatting
        integer :: status, n, i, j, fast, passed, nfiles, timeout_kind, cap, budget
        integer :: cleanup, attempts, nresults, r
        real :: seconds
        real(dp) :: started, elapsed, threshold
        logical :: found, snapshot_owned, hit, built, identity_ok, populated_found

        code = 1
        message = ''
        tree = ''
        snapshot = ''
        key = ''
        hit = .false.
        built = .false.
        snapshot_owned = .false.
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
            budget = config_seconds(root, 'fo.hookBudget', 10)
            cap = config_seconds(root, 'fo.hookBuildCap', 60)
            threshold = real(config_seconds(root, 'fo.hookFastThreshold', 1), dp)
            if (len(owned_view) > 0) then
                if (index(owned_view, path_dirname(root)//'/.fo-hook-') /= 1) then
                    message = 'invalid owned staged view'
                    exit validation
                end if
                snapshot = owned_view
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
            if (.not. snapshot_owned .and. len(owned_view) == 0) then
                message = 'cannot create an isolated staged view beside the repository'
                exit validation
            end if
            call checkout_tree(root, snapshot, tree, status)
            if (status /= 0) then
                message = 'cannot materialize staged tree or pinned submodule (initialize it first)'
                exit validation
            end if
            call git_text(root, git_words('diff-tree', '--no-commit-id', &
                '--name-only', '-r', '-z', 'HEAD', tree), text, status, strip=.false.)
            if (status /= 0) call git_text(root, git_words(&
                'ls-tree', '-r', '-z', '--name-only', tree), text, status, strip=.false.)
            if (status /= 0) then
                message = 'cannot enumerate staged paths'
                exit validation
            end if
            deallocate(changed)
            call split_paths(text, changed)
            allocate(files(size(changed)))
            nfiles = 0
            do i = 1, size(changed)
                if (.not. process_has_fortran_source_ext(changed(i)%s)) cycle
                inquire(file=snapshot//'/'//changed(i)%s, exist=found)
                if (.not. found) cycle
                nfiles = nfiles + 1
                files(nfiles) = changed(i)%s
            end do
            call fmt_sources(snapshot, FMT_SELECT_FILES, .true., .false., files, &
                nfiles, formatting, status)
            if (status /= 0) then
                message = trim(formatting)
                exit validation
            end if
            ! A project already configured with CMake keeps its populated
            ! dependency trees: the staged copy is pointed at them, because a
            ! commit never acquires a dependency. This runs before the backend
            ! is initialized so the effective configure arguments include it.
            call cmake_configured_build_dir(root, populated, populated_found)
            if (populated_found) then
                call cmake_populated_dependency_args(populated, &
                    'build/_deps', reuse, status, dependency_message)
                if (status /= 0) then
                    message = trim(dependency_message)
                    exit validation
                end if
            end if
            status = c_prepare(root//c_null_char, snapshot//c_null_char, int(cap, c_int), &
                reuse//c_null_char)
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
                ! Always configure/build this index view, even when the caller
                ! has an absolute FO_CMAKE_BUILD_DIR or a preset binaryDir.
                backend%cmake%build_root = 'build'
                backend%cmake%build_root_hint = .true.
                child_env = 'FO_CMAKE_BUILD_DIR=build'
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
            call project_affected(backend, '', request, changed, model, selection, &
                reasons, status, message)
            if (status /= 0) exit validation
            call sha256_file(trim(driver), driver_digest, status)
            identity_ok = .false.
            if (status == 0) call case_identity_capture(backend, driver_digest, '', &
                generation, identity_ok, configured=.true.)
            if (identity_ok) key = hook_input_key(generation)
            hit = hook_receipt_read(trim(key))
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
            if (backend%kind == BACKEND_CMAKE) then
                call argv_push(packed, n, 'hook')
                call argv_push(packed, n, 'build-view')
                call argv_push(packed, n, backend%cmake%request_token)
            else
                call argv_push(packed, n, 'build')
            end if
            call process_run_argv_logged(snapshot, packed, n, logfile, .false., cap, &
                status, env_extra=child_env, heartbeat_s=0, timeout_kind=timeout_kind, &
                cpu_budget_s=86400)
            if (status /= 0) then
                message = 'staged build failed'
                if (timeout_kind == TIMEOUT_WALL .or. status == 124) &
                    message = 'staged build exceeded hard cap'
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
                if (elapsed >= real(budget, dp)) then
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
                call process_run_argv_logged(snapshot, packed, n, resultfile, .false., &
                    max(1, int(real(budget, dp) - elapsed)), status, heartbeat_s=0, &
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
            if (len(owned_view) > 0) then
                if (.not. identity_ok .or. .not. model%complete .or. passed /= fast) then
                    message = 'staged verification incomplete; prepare with: '// &
                        'fo hook pre-commit'
                    exit validation
                end if
            end if
            ! Only a complete fast tier is a reusable receipt. An affected
            ! subset is useful evidence, but cannot stand for every fast test.
            if (model%complete .and. passed == count_model_fast(model)) &
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
