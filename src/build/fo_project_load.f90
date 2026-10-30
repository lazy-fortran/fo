module fo_project_load
    !! Load the project model of the detected backend and resolve a test
    !! selection on it. This is the one selection path shared by `fo test`,
    !! `fo test --list` and the MCP test tool (#167, #193): backend detection
    !! -> adapter (fpm manifest + scan, or CMake configure + File API + CTest)
    !! -> project_model_t -> model_select. Backends then receive exact
    !! case indices, which project_run_cases builds and runs.
    use fx_string, only: string_t
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_array_start, json_array_end, json_key, &
        json_value_string, json_key_string, json_key_bool, &
        json_to_string
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file_alloc
    use fo_dep_resolve, only: resolved_src_t, resolve_dep_srcs, &
        resolve_dev_dep_srcs, MAX_RESOLVED
    use fo_fpm_config, only: DEP_PATH
    use fo_build_backend, only: backend_t, BACKEND_NATIVE, BACKEND_CMAKE, &
        cmake_configure, backend_test_names, backend_test_cases, backend_test, &
        backend_set_build_targets, backend_build
    use fo_cmake_native_config, only: native_cmake_selected
    use fo_cmake_model, only: cmake_model_load
    use fo_cmake_context, only: cmake_context_adopt_query
    use fo_fpm_model, only: fpm_model_load
    use fo_project_model, only: project_model_t, model_request_t, &
        model_selection_t, model_plan_t, model_select, model_plan, &
        model_find_case, SELECT_OK, MAP_UNKNOWN, &
        SELECT_UNKNOWN_NAME, SELECT_EMPTY, SELECT_NAMED_EXCLUDED, &
        CASE_DEFAULT_EXCLUDED
    use fo_project_model, only: model_unit_t, model_layering_t, model_layering, &
        model_layering_sources, model_affected, CASE_SELECTED, ORIGIN_PROJECT, &
        model_listing_t, model_expand_cases, model_collapse_cases
    use fo_build_tree, only: native_output_dir
    use fo_test_cases, only: CASE_SEP, case_list, case_listing_t, case_matches
    use fo_process, only: argv_push, process_run_argv_logged
    use fo_fs, only: fs_realpath
    use fo_ctest_run, only: ctest_native_select, ctest_native_run, ctest_log_note
    use fo_scan, only: scan_unit_t, scan_file
    use fo_process, only: process_has_fortran_source_ext
    use fx_json_build, only: json_key_int
    use fo_case_filter, only: case_filter_t, filter_parse
    use fo_test_index, only: test_index_t, test_index_dir, test_index_load, &
        test_index_find
    implicit none
    private

    public :: project_has_model, project_load, project_resolve, project_run_cases
    public :: project_affected, project_changed_files, project_run_affected
    public :: project_select, project_list_expand, project_expand_cases
    public :: project_load_configured, project_inventory_cases, project_build_cases
    public :: project_model_units, project_affected_cases
    public :: project_list_text, project_list_json
    public :: project_layering, project_layering_text, project_layering_json
    public :: configure_error_excerpt

contains

    logical function project_has_model(b) result(has)
        !! False for the frozen FO_CMAKE_NATIVE route, which has no model.
        type(backend_t), intent(in) :: b

        has = .false.
        if (b%kind == BACKEND_NATIVE) then
            has = .true.
        else if (b%kind == BACKEND_CMAKE) then
            has = .not. native_cmake_selected(b%project_dir)
        end if
    end function project_has_model

    subroutine project_load(b, flags, model, ierr, message, scanned)
        !! Load a finalized model. CMake is configured first (File API reply
        !! and CTest registry); nothing is built. ierr /= 0 with message.
        !! scanned: project sources the adapter scanned beyond the target
        !! sources (fpm test helpers); empty on CMake.
        type(backend_t), intent(inout) :: b
        character(len=*), intent(in) :: flags
        type(project_model_t), intent(out) :: model
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        type(string_t), allocatable, intent(out), optional :: scanned(:)
        character(len=1024) :: log_file
        character(len=:), allocatable :: text
        integer :: exitcode, status

        ierr = 0
        message = ''
        if (.not. project_has_model(b)) then
            ierr = 1
            message = 'no project model for this backend configuration'
            return
        end if
        if (b%kind == BACKEND_NATIVE) then
            call fpm_model_load(trim(b%project_dir), model, ierr, message, &
                scanned=scanned)
            return
        end if
        if (present(scanned)) allocate (scanned(0))
        call make_tmpfile('fo-model-configure', log_file)
        call cmake_configure(b%cmake, flags, trim(log_file), exitcode)
        if (exitcode /= 0) then
            ierr = 1
            message = 'CMake configure failed'
            call read_text_file_alloc(trim(log_file), text, status)
            if (status == 0) message = message//new_line('a')// &
                trim(configure_error_excerpt(text))
            call delete_tmpfile(trim(log_file))
            return
        end if
        call delete_tmpfile(trim(log_file))
        call cmake_model_load(b%cmake, model, ierr, message)
        if (ierr /= 0) message = 'CMake project model unavailable: '//message
    end subroutine project_load

    function configure_error_excerpt(text) result(excerpt)
        !! CMake reports an error where it occurs and keeps configuring, so the
        !! end of a long log rarely holds the cause: keep every error block.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: excerpt
        integer, parameter :: limit = 8000
        integer :: start, finish, next
        logical :: in_error

        excerpt = ''
        in_error = .false.
        start = 1
        do while (start <= len(text))
            next = index(text(start:), new_line('a'))
            if (next == 0) then
                finish = len(text)
                next = len(text) + 1
            else
                finish = start + next - 2
                next = start + next
            end if
            if (index(text(start:finish), 'CMake Error') == 1) then
                in_error = .true.
            else if (in_error .and. finish >= start) then
                ! A block continues with indented text and its call stack.
                in_error = text(start:start) == ' ' .or. &
                    index(text(start:finish), 'Call Stack') == 1
            end if
            if (in_error .and. len(excerpt) <= limit) &
                excerpt = excerpt//text(start:finish)//new_line('a')
            start = next
        end do
        if (len(excerpt) == 0) then
            excerpt = text
            if (len(excerpt) > 2000) excerpt = '...'//excerpt(len(excerpt) - 1999:)
        else if (len(excerpt) > limit) then
            excerpt = excerpt(:limit)//'...'
        end if
    end function configure_error_excerpt

    subroutine project_load_configured(b, model, ierr, message, input_digest, &
            scanned)
        !! Load the model of a tree an earlier fo command configured (the
        !! Gremlin generation build, same flags): CMake reads that configure's
        !! File API reply and CTest registry and does not configure again,
        !! except for a configure preset without a build-root hint, whose
        !! build tree only a configure run reveals; fpm scans.
        type(backend_t), intent(inout) :: b
        type(project_model_t), intent(out) :: model
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        character(len=*), intent(in), optional :: input_digest
        !! As for project_load (fpm test helpers); empty on CMake.
        type(string_t), allocatable, intent(out), optional :: scanned(:)

        ierr = 0
        message = ''
        if (.not. project_has_model(b)) then
            ierr = 1
            message = 'no project model for this backend configuration'
            return
        end if
        if (b%kind == BACKEND_NATIVE) then
            call fpm_model_load(trim(b%project_dir), model, ierr, message, &
                scanned=scanned)
            return
        end if
        if (present(scanned)) allocate (scanned(0))
        if (len_trim(b%cmake%configure_preset) > 0 .and. &
            .not. b%cmake%build_root_hint) then
            ! The preset's build tree is only known from a configure run.
            call project_load(b, '', model, ierr, message)
            return
        end if
        call cmake_context_adopt_query(b%cmake)
        call cmake_model_load(b%cmake, model, ierr, message, input_digest)
        if (ierr /= 0) message = 'CMake project model unavailable: '//message
    end subroutine project_load_configured

    subroutine project_inventory_cases(model, cases)
        !! The cases a continuous campaign may run: every case except those a
        !! test preset filters out, explicit-only dispatchers and expanded parents.
        !! Label exclusions (slow) stay in; their runs pass --all.
        type(project_model_t), intent(in) :: model
        integer, allocatable, intent(out) :: cases(:)
        integer :: k, n

        n = 0
        do k = 1, model%n_cases
            if (included(k)) n = n + 1
        end do
        allocate (cases(n))
        n = 0
        do k = 1, model%n_cases
            if (.not. included(k)) cycle
            n = n + 1
            cases(n) = k
        end do

    contains

        logical function included(i)
            integer, intent(in) :: i

            included = .not. (model%cases(i)%default_excluded .or. &
                model%cases(i)%explicit_only .or. model%cases(i)%expanded)
        end function included

    end subroutine project_inventory_cases

    subroutine project_build_cases(b, model, cases, flags, log_file, exitcode)
        !! Build what the cases need without running them. The model is
        !! loaded (CMake is configured): CMake builds the plan targets of the
        !! cases; fpm builds the named tests and their dependency closures.
        type(backend_t), intent(inout) :: b
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        character(len=*), intent(in) :: flags, log_file
        integer, intent(out) :: exitcode
        type(model_plan_t) :: plan
        type(string_t), allocatable :: targets(:)
        character(len=:), allocatable :: names(:)
        integer :: k, width

        if (b%kind == BACKEND_CMAKE) then
            call plan_names(model, cases, plan, targets)
            call backend_set_build_targets(b, targets, plan%widened)
            call backend_build(b, exitcode, flags, log_file, with_tests=.true., &
                configured=.true.)
        else
            width = 1
            do k = 1, size(cases)
                width = max(width, len(model%cases(cases(k))%name))
            end do
            allocate (character(len=width) :: names(size(cases)))
            do k = 1, size(cases)
                names(k) = model%cases(cases(k))%name
            end do
            call backend_test_names(b, names, size(names), exitcode, .true., &
                log_file, flags=flags, build_only=.true.)
        end if
    end subroutine project_build_cases

    subroutine project_resolve(b, flags, request, model, selection, ierr, message, &
            configured)
        !! Load the model and select. ierr = 0: the selection is usable (an
        !! empty selection has selection%status = SELECT_EMPTY and the caller
        !! decides). ierr /= 0: message is the diagnostic, without prefix.
        type(backend_t), intent(inout) :: b
        character(len=*), intent(in) :: flags
        type(model_request_t), intent(in) :: request
        type(project_model_t), intent(out) :: model
        type(model_selection_t), intent(out) :: selection
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        logical, intent(in), optional :: configured
        type(model_request_t) :: local
        logical :: reuse

        reuse = .false.
        if (present(configured)) reuse = configured
        if (reuse) then
            call project_load_configured(b, model, ierr, message)
        else
            call project_load(b, flags, model, ierr, message)
        end if
        if (ierr /= 0) return
        call attach_recorded_failures(b, model, request, local)
        call resolve_loaded(model, local, selection, ierr, message)
    end subroutine project_resolve

    subroutine filter_flags(request, affected, failed)
        !! Whether the request's filter expression uses affected() / failed().
        type(model_request_t), intent(in) :: request
        logical, intent(out) :: affected, failed
        type(case_filter_t) :: expression
        character(len=:), allocatable :: message
        integer :: ierr

        affected = .false.
        failed = .false.
        if (.not. allocated(request%filter)) return
        if (len_trim(request%filter) == 0) return
        call filter_parse(trim(request%filter), expression, ierr, message)
        if (ierr /= 0) return
        affected = expression%uses_affected
        failed = expression%uses_failed
    end subroutine filter_flags

    subroutine attach_recorded_failures(b, model, request, local)
        !! local = request plus, when its filter uses failed(), the cases the
        !! last recorded run of this project failed (fo_test_index).
        type(backend_t), intent(in) :: b
        type(project_model_t), intent(in) :: model
        type(model_request_t), intent(in) :: request
        type(model_request_t), intent(out) :: local
        type(test_index_t) :: recorded
        logical :: uses_affected, uses_failed
        integer :: i, k

        local = request
        call filter_flags(request, uses_affected, uses_failed)
        if (.not. uses_failed) return
        call test_index_load(test_index_dir(model, trim(b%project_dir)), recorded)
        allocate (local%failed(model%n_cases))
        local%failed = .false.
        do i = 1, model%n_cases
            k = test_index_find(recorded, model%cases(i)%name)
            if (k == 0) cycle
            local%failed(i) = recorded%status(k)%s == 'FAIL' .or. &
                recorded%status(k)%s == 'TIMEOUT'
        end do
    end subroutine attach_recorded_failures

    subroutine resolve_loaded(model, request, selection, ierr, message)
        !! model_select with the diagnostics of project_resolve.
        type(project_model_t), intent(in) :: model
        type(model_request_t), intent(in) :: request
        type(model_selection_t), intent(out) :: selection
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        integer :: i, k

        message = ''
        call model_select(model, request, selection)
        select case (selection%status)
        case (SELECT_OK, SELECT_EMPTY)
            ierr = 0
        case (SELECT_NAMED_EXCLUDED)
            ierr = 1
            message = selection%message
            ! Keep the established wording for the `slow` label rule.
            do i = 1, size(request%names)
                k = model_find_case(model, request%names(i)%s)
                if (k == 0) cycle
                if (selection%disposition(k) /= CASE_DEFAULT_EXCLUDED) cycle
                if (has_label(model, k, 'slow')) message = 'slow test '// &
                    request%names(i)%s//' requires --all'
                exit
            end do
        case default
            ierr = 1
            message = selection%message
        end select
    end subroutine resolve_loaded

    logical function has_label(model, k, label) result(found)
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: k
        character(len=*), intent(in) :: label
        integer :: j

        found = .false.
        do j = 1, size(model%cases(k)%labels)
            if (model%cases(k)%labels(j)%s == label) found = .true.
        end do
    end function has_label

    subroutine project_select(b, flags, request, only_changed, model, selection, &
            widen_reasons, ierr, message, configured)
        !! The one selection entry of `fo test`, `fo test --list`, the MCP test
        !! tool and `fo check`: project_resolve, or with only_changed the
        !! cases the Git delta of the project and its path dependencies
        !! affects (no baseline widens to the whole selection).
        type(backend_t), intent(inout) :: b
        character(len=*), intent(in) :: flags
        type(model_request_t), intent(in) :: request
        logical, intent(in) :: only_changed
        type(project_model_t), intent(out) :: model
        type(model_selection_t), intent(out) :: selection
        type(string_t), allocatable, intent(out) :: widen_reasons(:)
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        !! Reuse an existing configuration for --no-build. Parallel Gremlin
        !! children must not rewrite queries or replies in their shared tree.
        logical, intent(in), optional :: configured
        type(string_t), allocatable :: changed(:)
        character(len=:), allocatable :: reason
        logical :: git_ok, uses_affected, uses_failed

        call filter_flags(request, uses_affected, uses_failed)
        if (.not. only_changed .and. .not. uses_affected) then
            allocate (widen_reasons(0))
            call project_resolve(b, flags, request, model, selection, ierr, message, &
                configured=configured)
            return
        end if
        call project_changed_files(trim(b%project_dir), changed, git_ok, reason)
        if (git_ok) then
            call project_affected(b, flags, request, changed, model, selection, &
                widen_reasons, ierr, message, restrict=only_changed, &
                configured=configured)
        else
            call project_affected(b, flags, request, changed, model, selection, &
                widen_reasons, ierr, message, force_reason=reason, &
                restrict=only_changed, configured=configured)
        end if
    end subroutine project_select

    subroutine project_affected(b, flags, request, changed, model, selection, &
            widen_reasons, ierr, message, force_reason, restrict, configured)
        !! project_resolve restricted by model_affected to the cases the
        !! changed files affect (`fo test --only-changed`, MCP only_changed,
        !! `fo check`). changed: paths below the project root (relative, or
        !! absolute with the project root as prefix); paths
        !! below the build trees are ignored. force_reason widens to the whole
        !! selection (no change baseline). The units are the scanned Fortran
        !! sources of the targets plus the adapter's other scanned sources.
        type(backend_t), intent(inout) :: b
        character(len=*), intent(in) :: flags
        type(model_request_t), intent(in) :: request
        type(string_t), intent(in) :: changed(:)
        type(project_model_t), intent(out) :: model
        type(model_selection_t), intent(out) :: selection
        type(string_t), allocatable, intent(out) :: widen_reasons(:)
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        character(len=*), intent(in), optional :: force_reason
        !! .false.: keep the whole selection (only the filter expression
        !! sees the delta, through affected()). Default .true.
        logical, intent(in), optional :: restrict
        logical, intent(in), optional :: configured
        type(model_request_t) :: local
        integer, allocatable :: project_cases(:), hit(:)
        type(string_t), allocatable :: more(:)
        logical :: uses_affected, uses_failed
        type(string_t), allocatable :: extra(:), paths(:), unscanned(:), roots(:)
        type(string_t), allocatable :: kept(:), ignored(:)
        type(model_unit_t), allocatable :: units(:)
        character(len=4096) :: physical
        logical :: resolved, reuse
        integer :: k, n

        allocate (widen_reasons(0))
        reuse = .false.
        if (present(configured)) reuse = configured
        if (reuse) then
            call project_load_configured(b, model, ierr, message, scanned=extra)
        else
            call project_load(b, flags, model, ierr, message, scanned=extra)
        end if
        if (ierr /= 0) return
        allocate (roots(1))
        roots(1)%s = trim(b%project_dir)
        call fs_realpath(trim(b%project_dir), physical, resolved)
        if (resolved) then
            if (trim(physical) /= roots(1)%s) call push_text(roots, trim(physical))
        end if
        ! Build trees hold generated files, never project changes.
        allocate (ignored(1))
        ignored(1)%s = 'build'
        if (len(model%build_dir) > 0) &
            call push_text(ignored, strip_root(model%build_dir, roots))
        allocate (kept(size(changed)))
        n = 0
        do k = 1, size(changed)
            n = n + 1
            kept(n)%s = strip_root(changed(k)%s, roots)
            if (below_any(kept(n)%s, ignored)) n = n - 1
        end do
        call model_layering_sources(model, extra, paths)
        call scan_units(paths, units, unscanned)
        call attach_recorded_failures(b, model, request, local)
        call filter_flags(request, uses_affected, uses_failed)
        if (uses_affected) then
            project_cases = pack([(k, k=1, model%n_cases)], &
                [(model%cases(k)%origin == ORIGIN_PROJECT, k=1, model%n_cases)])
            call project_affected_cases(model, units, roots, project_cases, &
                kept(1:n), hit, widen_reasons, force_reason)
            allocate (local%affected(model%n_cases))
            local%affected = .false.
            local%affected(hit) = .true.
        end if
        call resolve_loaded(model, local, selection, ierr, message)
        if (ierr /= 0) return
        if (present(restrict)) then
            if (.not. restrict) return
        end if
        call model_affected(model, units, roots, kept(1:n), selection, &
            more, force_reason)
        widen_reasons = [widen_reasons, more]
    end subroutine project_affected

    subroutine project_model_units(model, extra, units, from_root, to_root)
        !! Scan the sources of a loaded model (target and layer sources) and
        !! extra, the adapter's other scanned sources, into model units.
        !! A path below from_root is read below to_root instead and spelled
        !! so: the sources of a model loaded from an earlier copy of the
        !! tree (a reused Gremlin generation model). Unreadable sources are
        !! left out; model_affected treats them as unscanned.
        type(project_model_t), intent(in) :: model
        type(string_t), intent(in) :: extra(:)
        type(model_unit_t), allocatable, intent(out) :: units(:)
        character(len=*), intent(in) :: from_root, to_root
        type(string_t), allocatable :: paths(:), unscanned(:)
        integer :: k, n

        call model_layering_sources(model, extra, paths)
        n = len(from_root)
        if (n > 0 .and. from_root /= to_root) then
            do k = 1, size(paths)
                if (len(paths(k)%s) <= n + 1) cycle
                if (paths(k)%s(1:n + 1) /= from_root//'/') cycle
                paths(k)%s = to_root//paths(k)%s(n + 1:)
            end do
        end if
        call scan_units(paths, units, unscanned)
    end subroutine project_model_units

    subroutine project_affected_cases(model, units, roots, cases, changed, &
            affected, widen_reasons, force_reason)
        !! model_affected on an already loaded model and scanned units:
        !! affected = the cases (model indices, in the given order) that the
        !! changed files (relative to the project root, which roots spell)
        !! affect. force_reason widens to every case.
        type(project_model_t), intent(in) :: model
        type(model_unit_t), intent(in) :: units(:)
        type(string_t), intent(in) :: roots(:), changed(:)
        integer, intent(in) :: cases(:)
        integer, allocatable, intent(out) :: affected(:)
        type(string_t), allocatable, intent(out) :: widen_reasons(:)
        character(len=*), intent(in), optional :: force_reason
        type(model_selection_t) :: selection
        integer :: k

        selection%status = SELECT_OK
        selection%message = ''
        selection%cases = cases
        allocate (selection%disposition(model%n_cases), &
            selection%reasons(model%n_cases), selection%unknown(0), &
            selection%unmatched(0))
        selection%disposition = CASE_SELECTED
        do k = 1, model%n_cases
            selection%reasons(k)%s = ''
        end do
        if (size(cases) == 0) then
            allocate (affected(0), widen_reasons(0))
            return
        end if
        call model_affected(model, units, roots, changed, selection, &
            widen_reasons, force_reason)
        affected = selection%cases
    end subroutine project_affected_cases

    subroutine project_run_affected(b, flags, include_all, log_file, exitcode, &
            n_run)
        !! Build and run the cases the Git delta affects (the default
        !! selection; include_all lifts default exclusions), for `fo check`
        !! and the `fo` pipeline: the selection of `fo test --only-changed`.
        !! Without a project model (FO_CMAKE_NATIVE) every registered test
        !! runs (n_run = -1). n_run = 0: no case is affected and nothing ran.
        !! A model that cannot be loaded fails (exitcode 1) with the
        !! diagnostic appended to log_file.
        type(backend_t), intent(inout) :: b
        character(len=*), intent(in) :: flags
        logical, intent(in) :: include_all
        character(len=*), intent(in) :: log_file
        integer, intent(out) :: exitcode, n_run
        type(model_request_t) :: request
        type(project_model_t) :: model
        type(model_selection_t) :: selection
        type(string_t), allocatable :: widen_reasons(:)
        character(len=:), allocatable :: message
        integer :: ierr, unit, ios

        n_run = 0
        exitcode = 0
        if (.not. project_has_model(b)) then
            n_run = -1
            call backend_test(b, exitcode, include_all, log_file, flags=flags)
            return
        end if
        allocate (request%names(0), request%include_labels(0), &
            request%exclude_labels(0), request%layers(0))
        request%all = include_all
        call project_select(b, flags, request, .true., model, selection, &
            widen_reasons, ierr, message)
        if (ierr /= 0) then
            exitcode = 1
            open (newunit=unit, file=log_file, position='append', &
                action='write', iostat=ios)
            if (ios == 0) then
                write (unit, '(a)') 'fo: '//message
                close (unit)
            end if
            return
        end if
        if (selection%status == SELECT_EMPTY) return
        n_run = size(selection%cases)
        call project_run_cases(b, model, selection%cases, flags, .false., log_file, &
            exitcode)
    end subroutine project_run_affected

    subroutine project_changed_files(project_dir, changed, ok, reason)
        !! Files that differ from the Git HEAD of the repository holding
        !! project_dir (see git_changed_files), relative to project_dir, plus
        !! those of every fpm path dependency outside project_dir, spelled
        !! absolute: each dependency is compared against its own repository.
        !! ok = .false. with reason when a baseline is missing for the
        !! project or for such a dependency (the selection then widens).
        character(len=*), intent(in) :: project_dir
        type(string_t), allocatable, intent(out) :: changed(:)
        logical, intent(out) :: ok
        character(len=:), allocatable, intent(out) :: reason
        type(resolved_src_t), allocatable :: deps(:), devs(:)
        type(resolved_src_t) :: dep
        type(string_t), allocatable :: roots(:)
        type(string_t), allocatable :: more(:)
        character(len=4096) :: physical
        logical :: resolved, dep_ok
        character(len=:), allocatable :: dep_reason
        integer :: n_deps, n_devs, n_unresolved, ierr, k

        call git_changed_files(project_dir, changed, ok, reason)
        if (.not. ok) return
        inquire (file=trim(project_dir)//'/fpm.toml', exist=resolved)
        if (.not. resolved) return
        allocate (deps(MAX_RESOLVED), devs(MAX_RESOLVED))
        call resolve_dep_srcs(project_dir, deps, n_deps, n_unresolved, ierr)
        if (ierr /= 0) n_deps = 0
        call resolve_dev_dep_srcs(project_dir, devs, n_devs, ierr)
        if (ierr /= 0) n_devs = 0
        call fs_realpath(project_dir, physical, resolved)
        if (.not. resolved) physical = project_dir
        ! Assign components directly: GNU Fortran 12 -O2 builds
        ! string_t(trim(physical)) with a wrong length.
        allocate (roots(2))
        roots(1)%s = project_dir
        roots(2)%s = trim(physical)
        do k = 1, n_deps + n_devs
            if (k <= n_deps) then
                dep = deps(k)
            else
                dep = devs(k - n_deps)
            end if
            if (dep%kind /= DEP_PATH) cycle
            if (below_any(trim(dep%dir)//'/', roots)) cycle
            call git_changed_files(trim(dep%dir), more, dep_ok, dep_reason)
            if (.not. dep_ok) then
                ok = .false.
                reason = 'path dependency '//trim(dep%name)//': '//dep_reason
                return
            end if
            call add_prefixed(changed, trim(dep%dir)//'/', more)
        end do
    end subroutine project_changed_files

    subroutine add_prefixed(changed, prefix, more)
        type(string_t), allocatable, intent(inout) :: changed(:)
        character(len=*), intent(in) :: prefix
        type(string_t), intent(in) :: more(:)
        integer :: k

        do k = 1, size(more)
            call push_text(changed, prefix//more(k)%s)
        end do
    end subroutine add_prefixed

    subroutine git_changed_files(project_dir, changed, ok, reason)
        !! Files that differ from the Git HEAD of the repository holding
        !! project_dir: tracked changes (staged or not, deletions included)
        !! and untracked files that are not ignored, relative to project_dir
        !! and limited to it. ok = .false. with reason when there is no Git
        !! baseline (no repository, no commit, git unavailable).
        character(len=*), intent(in) :: project_dir
        type(string_t), allocatable, intent(out) :: changed(:)
        logical, intent(out) :: ok
        character(len=:), allocatable, intent(out) :: reason
        character(len=1024) :: out_file
        character(len=:), allocatable :: packed, text
        integer :: n_args, exitcode, status

        ok = .false.
        reason = ''
        allocate (changed(0))
        call make_tmpfile('fo-changed', out_file)
        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'git')
        call argv_push(packed, n_args, '-C')
        call argv_push(packed, n_args, trim(project_dir))
        call argv_push(packed, n_args, 'rev-parse')
        call argv_push(packed, n_args, '--verify')
        call argv_push(packed, n_args, '--quiet')
        call argv_push(packed, n_args, 'HEAD')
        call process_run_argv_logged('', packed, n_args, trim(out_file), .false., &
            0, exitcode)
        if (exitcode /= 0) then
            reason = 'no Git HEAD to compare against'
            call delete_tmpfile(trim(out_file))
            return
        end if
        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'git')
        call argv_push(packed, n_args, '-C')
        call argv_push(packed, n_args, trim(project_dir))
        call argv_push(packed, n_args, 'diff')
        call argv_push(packed, n_args, '--name-only')
        call argv_push(packed, n_args, '--no-renames')
        call argv_push(packed, n_args, '--relative')
        call argv_push(packed, n_args, '-z')
        call argv_push(packed, n_args, 'HEAD')
        call argv_push(packed, n_args, '--')
        call process_run_argv_logged('', packed, n_args, trim(out_file), .false., &
            0, exitcode)
        if (exitcode == 0) then
            n_args = 0
            packed = ''
            call argv_push(packed, n_args, 'git')
            call argv_push(packed, n_args, '-C')
            call argv_push(packed, n_args, trim(project_dir))
            call argv_push(packed, n_args, 'ls-files')
            call argv_push(packed, n_args, '--others')
            call argv_push(packed, n_args, '--exclude-standard')
            call argv_push(packed, n_args, '-z')
            call process_run_argv_logged('', packed, n_args, trim(out_file), &
                .true., 0, exitcode)
        end if
        if (exitcode /= 0) then
            reason = 'git could not list the changed files'
            call delete_tmpfile(trim(out_file))
            return
        end if
        call read_text_file_alloc(trim(out_file), text, status)
        call delete_tmpfile(trim(out_file))
        if (status /= 0) then
            reason = 'git could not list the changed files'
            return
        end if
        call split_nul(text, changed)
        ok = .true.
    end subroutine git_changed_files

    subroutine split_nul(text, items)
        !! NUL-terminated items of text, each once.
        character(len=*), intent(in) :: text
        type(string_t), allocatable, intent(inout) :: items(:)
        integer :: pos, next, k
        logical :: seen

        pos = 1
        do while (pos <= len(text))
            next = index(text(pos:), achar(0))
            if (next == 0) next = len(text) - pos + 2
            if (next > 1) then
                seen = .false.
                do k = 1, size(items)
                    if (items(k)%s == text(pos:pos + next - 2)) seen = .true.
                end do
                if (.not. seen) call push_text(items, text(pos:pos + next - 2))
            end if
            pos = pos + next
        end do
    end subroutine split_nul

    subroutine push_text(items, text)
        type(string_t), allocatable, intent(inout) :: items(:)
        character(len=*), intent(in) :: text
        type(string_t), allocatable :: grown(:)
        integer :: n

        n = size(items)
        allocate (grown(n + 1))
        if (n > 0) grown(1:n) = items
        grown(n + 1)%s = text
        call move_alloc(grown, items)
    end subroutine push_text

    function strip_root(path, roots) result(rel)
        character(len=*), intent(in) :: path
        type(string_t), intent(in) :: roots(:)
        character(len=:), allocatable :: rel
        integer :: r, n

        rel = path
        do r = 1, size(roots)
            n = len(roots(r)%s)
            if (len(path) <= n + 1) cycle
            if (path(1:n + 1) /= roots(r)%s//'/') cycle
            rel = path(n + 2:)
            return
        end do
    end function strip_root

    logical function below_any(path, dirs) result(below)
        character(len=*), intent(in) :: path
        type(string_t), intent(in) :: dirs(:)
        integer :: d, n

        below = .false.
        do d = 1, size(dirs)
            n = len(dirs(d)%s)
            if (n == 0) cycle
            if (len(path) <= n) cycle
            if (path(1:n + 1) == dirs(d)%s//'/') below = .true.
        end do
    end function below_any

    subroutine scan_units(paths, units, unscanned)
        !! Scan the Fortran sources among paths into model units; sources
        !! that cannot be read or parsed are returned in unscanned.
        type(string_t), intent(in) :: paths(:)
        type(model_unit_t), allocatable, intent(out) :: units(:)
        type(string_t), allocatable, intent(out) :: unscanned(:)
        type(model_unit_t), allocatable :: all(:)
        type(scan_unit_t) :: scanned
        logical, allocatable :: failed(:)
        integer :: k, n, status, m, n_extra, k_extra

        allocate (all(size(paths)), failed(size(paths)))
        failed = .false.
        n = 0
        do k = 1, size(paths)
            if (.not. process_has_fortran_source_ext(paths(k)%s)) cycle
            call scan_file(paths(k)%s, scanned, status)
            if (status /= 0) then
                failed(k) = .true.
                cycle
            end if
            n = n + 1
            all(n)%path = paths(k)%s
            all(n)%other_units = scanned%has_other_units
            n_extra = 0
            if (allocated(scanned%additional_modules)) &
                n_extra = size(scanned%additional_modules)
            m = 0
            if (len_trim(scanned%module_name) > 0) m = 1
            allocate (all(n)%provides(m + n_extra))
            if (m == 1) all(n)%provides(1)%s = trim(scanned%module_name)
            do k_extra = 1, n_extra
                all(n)%provides(m + k_extra)%s = &
                    trim(scanned%additional_modules(k_extra))
            end do
            allocate (all(n)%uses(scanned%n_deps))
            do m = 1, scanned%n_deps
                all(n)%uses(m)%s = trim(scanned%deps(m))
            end do
        end do
        allocate (units(n))
        if (n > 0) units(:) = all(1:n)
        allocate (unscanned(count(failed)))
        m = 0
        do k = 1, size(paths)
            if (.not. failed(k)) cycle
            m = m + 1
            unscanned(m) = paths(k)
        end do
    end subroutine scan_units

    subroutine project_run_cases(b, model, cases, flags, no_build, log_file, &
            exitcode, selection)
        !! Build what the cases need, then run exactly them. The model is
        !! already loaded (CMake is configured), so nothing configures again.
        !! CMake: build the plan targets only, then ctest with exact anchored
        !! names in batches. fpm: its named-test path. no_build runs without
        !! building. selection: the `exe::glob` filters of an executable not
        !! listed yet are applied where it is run (the listing needs the build).
        type(backend_t), intent(inout) :: b
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        character(len=*), intent(in) :: flags
        logical, intent(in) :: no_build
        character(len=*), intent(in) :: log_file
        integer, intent(out) :: exitcode
        type(model_selection_t), intent(in), optional :: selection
        character(len=:), allocatable :: names(:)
        type(model_plan_t) :: plan
        type(string_t), allocatable :: targets(:), filters(:), selected(:)
        integer :: k, width
        logical :: lift

        allocate (filters(size(cases)))
        do k = 1, size(cases)
            filters(k)%s = ''
            if (.not. present(selection)) cycle
            if (allocated(selection%filters)) filters(k)%s = &
                selection%filters(cases(k))%s
        end do
        width = 1
        do k = 1, size(cases)
            width = max(width, len(model%cases(cases(k))%name))
        end do
        allocate (character(len=width) :: names(size(cases)))
        lift = .false.
        do k = 1, size(cases)
            names(k) = model%cases(cases(k))%name
            if (model%cases(cases(k))%default_excluded) lift = .true.
        end do
        if (b%kind == BACKEND_CMAKE) then
            call plan_names(model, cases, plan, targets)
            call backend_set_build_targets(b, targets, plan%widened)
            call run_cmake_cases(b, model, cases, names, flags, no_build, lift, &
                log_file, exitcode, filters)
        else
            call filtered_names(names, filters, selected)
            width = 1
            do k = 1, size(selected)
                width = max(width, len(selected(k)%s))
            end do
            deallocate (names)
            allocate (character(len=width) :: names(size(selected)))
            do k = 1, size(selected)
                names(k) = selected(k)%s
            end do
            call backend_test_names(b, names, size(names), exitcode, .true., &
                log_file, flags=flags, no_build=no_build)
        end if
    end subroutine project_run_cases

    subroutine filtered_names(names, filters, selected)
        !! The names a runner receives: a case with filter globs once per
        !! glob as `name::glob`, any other case as its name.
        character(len=*), intent(in) :: names(:)
        type(string_t), intent(in) :: filters(:)
        type(string_t), allocatable, intent(out) :: selected(:)
        integer :: k, pos, next

        allocate (selected(0))
        do k = 1, size(names)
            if (len(filters(k)%s) == 0) then
                selected = [selected, string_t(trim(names(k)))]
                cycle
            end if
            pos = 1
            do while (pos <= len(filters(k)%s))
                next = index(filters(k)%s(pos:), new_line('a'))
                if (next == 0) next = len(filters(k)%s) - pos + 2
                selected = [selected, string_t(trim(names(k))//CASE_SEP// &
                    filters(k)%s(pos:pos + next - 2))]
                pos = pos + next
            end do
        end do
    end subroutine filtered_names

    subroutine project_list_expand(b, flags, request, model, selection, ierr, &
            message, build)
        !! Split the selected executables that list their cases (built ones
        !! only: the listing runs the executable) into their cases and select
        !! again, so `exe::case` names and globs resolve on the cases and each
        !! case has its own digest and verdict. build: build the selected
        !! cases first (`fo test`); a failing build leaves the selection as is
        !! for the run to report.
        type(backend_t), intent(inout) :: b
        character(len=*), intent(in) :: flags
        type(model_request_t), intent(in) :: request
        type(project_model_t), intent(inout) :: model
        type(model_selection_t), intent(inout) :: selection
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        logical, intent(in), optional :: build
        character(len=1024) :: log_file
        integer :: n, code

        ierr = 0
        message = ''
        if (present(build)) then
            if (build) then
                call make_tmpfile('fo-expand-build', log_file)
                call project_build_cases(b, model, selection%cases, flags, &
                    trim(log_file), code)
                call delete_tmpfile(trim(log_file))
                if (code /= 0) return
            end if
        end if
        call expand_listed(b, flags, model, selection%cases, n, ierr, message)
        if (ierr /= 0 .or. n == 0) return
        call resolve_loaded(model, request, selection, ierr, message)
    end subroutine project_list_expand

    subroutine project_expand_cases(b, flags, model, n_expanded, ierr, message, &
            from_root, to_root)
        !! Gremlin: after a generation build, replace the executable cases of
        !! every built executable that lists its cases by its cases (an
        !! earlier expansion is dropped first, so a rebuilt executable lists
        !! again). Executables that list nothing stay one case.
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: flags
        type(project_model_t), intent(inout) :: model
        integer, intent(out) :: n_expanded, ierr
        character(len=:), allocatable, intent(out) :: message
        !! A model reused from a tree that is gone names its executables
        !! below from_root: they are looked up below to_root instead.
        character(len=*), intent(in), optional :: from_root, to_root
        integer :: k
        integer, allocatable :: all(:)

        n_expanded = 0
        call model_collapse_cases(model, ierr, message)
        if (ierr /= 0) return
        all = [(k, k=1, model%n_cases)]
        call expand_listed(b, flags, model, all, n_expanded, ierr, message, &
            from_root, to_root)
    end subroutine project_expand_cases

    subroutine expand_listed(b, flags, model, cases, n_expanded, ierr, message, &
            from_root, to_root)
        !! Expand the given model cases whose executable is built and lists
        !! cases: a case runs one executable when its target artifact (fpm: the
        !! executable of that name) or its single-token CTest command is one.
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: flags
        type(project_model_t), intent(inout) :: model
        integer, intent(in) :: cases(:)
        integer, intent(out) :: n_expanded, ierr
        character(len=:), allocatable, intent(out) :: message
        character(len=*), intent(in), optional :: from_root, to_root
        type(model_listing_t), allocatable :: listings(:)
        type(case_listing_t) :: listing
        character(len=:), allocatable :: exe
        logical :: exists
        integer :: k

        allocate (listings(0))
        n_expanded = 0
        ierr = 0
        message = ''
        do k = 1, size(cases)
            exe = case_executable(b, flags, model, cases(k))
            if (len(exe) == 0) cycle
            if (present(from_root) .and. present(to_root)) &
                exe = rebased(exe, from_root, to_root)
            inquire (file=exe, exist=exists)
            if (.not. exists) cycle
            call case_list(exe, trim(b%project_dir), listing)
            if (size(listing%names) == 0) cycle
            listings = [listings, model_listing_t(cases(k), listing%names, &
                listing%sources)]
        end do
        n_expanded = size(listings)
        if (n_expanded == 0) return
        call model_expand_cases(model, listings, ierr, message)
    end subroutine expand_listed

    function rebased(path, from_root, to_root) result(out)
        !! path below from_root, spelled below to_root. The CMake File API
        !! reports resolved paths (/private/var for /var on Darwin), so the
        !! two roots are compared without a leading /private.
        character(len=*), intent(in) :: path, from_root, to_root
        character(len=:), allocatable :: out, plain, root

        out = path
        if (len(from_root) == 0) return
        plain = unprivate(path)
        root = unprivate(from_root)
        if (len(plain) <= len(root)) return
        if (plain(1:len(root) + 1) /= root//'/') return
        out = to_root//plain(len(root) + 1:)
    end function rebased

    function unprivate(path) result(out)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: out

        out = path
        if (len(path) > 9) then
            if (path(1:9) == '/private/') out = path(9:)
        end if
    end function unprivate

    function case_executable(b, flags, model, k) result(exe)
        !! The executable that runs model case k as a whole, or ''.
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: flags
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: k
        character(len=:), allocatable :: exe
        integer :: t

        exe = ''
        associate (c => model%cases(k))
            if (c%explicit_only .or. c%expanded) return
            if (allocated(c%parent)) return
            t = c%command_target
            if (b%kind == BACKEND_NATIVE) then
                if (t < 1) return
                if (model%targets(t)%name /= c%name) return
                exe = native_output_dir(trim(b%project_dir), flags, 'bin')//'/'// &
                    c%name
            else
                if (.not. allocated(c%command)) return
                if (size(c%command) /= 1) return
                exe = c%command(1)%s
                if (len(exe) == 0) return
                if (exe(1:1) /= '/') then
                    exe = ''
                else if (ordered(model, k)) then
                    exe = ''
                end if
            end if
        end associate
    end function case_executable

    logical function ordered(model, k) result(yes)
        !! A CTest case that takes part in an ordering the model cannot
        !! carry over to its cases: a fixture setup or cleanup, or a DEPENDS
        !! target of another case. It is never split.
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: k
        integer :: j, a, m

        yes = .false.
        do a = 1, size(model%cases(k)%attrs)
            select case (model%cases(k)%attrs(a)%key)
            case ('FIXTURES_SETUP', 'FIXTURES_CLEANUP')
                yes = .true.
                return
            end select
        end do
        do j = 1, model%n_cases
            do a = 1, size(model%cases(j)%attrs)
                if (model%cases(j)%attrs(a)%key /= 'DEPENDS') cycle
                if (.not. allocated(model%cases(j)%attrs(a)%items)) cycle
                do m = 1, size(model%cases(j)%attrs(a)%items)
                    if (model%cases(j)%attrs(a)%items(m)%s == &
                        model%cases(k)%name) yes = .true.
                end do
            end do
        end do
    end function ordered

    subroutine run_cmake_cases(b, model, cases, names, flags, no_build, lift, &
            log_file, exitcode, filters)
        !! Build once, run the cases whose CTest properties fo implements
        !! natively, and delegate the rest to ctest (FO_CTEST_DELEGATE=1
        !! delegates all). Delegated cases are named in the log. The native
        !! cases run per listed case: each executable that lists its cases
        !! becomes its cases (the `filters` select among them).
        type(backend_t), intent(inout) :: b
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        character(len=*), intent(in) :: names(:), flags, log_file
        logical, intent(in) :: no_build, lift
        integer, intent(out) :: exitcode
        type(string_t), intent(in) :: filters(:)
        type(project_model_t) :: listed
        type(string_t), allocatable :: reasons(:)
        logical, allocatable :: native(:)
        integer, allocatable :: run_cases(:)
        character(len=8) :: forced
        character(len=:), allocatable :: message
        integer :: k, status, code

        allocate (native(size(cases)), reasons(size(cases)))
        call get_environment_variable('FO_CTEST_DELEGATE', forced, status=status)
        if (status == 0 .and. len_trim(forced) > 0 .and. forced(1:1) /= '0') then
            native = .false.
            do k = 1, size(cases)
                reasons(k)%s = 'FO_CTEST_DELEGATE is set'
            end do
        else
            call ctest_native_select(model, cases, native, reasons)
        end if
        if (.not. any(native)) then
            call refuse_filters(filters, log_file, exitcode)
            if (exitcode /= 0) return
            call backend_test_cases(b, names, size(names), exitcode, &
                log_file=log_file, flags=flags, no_build=no_build, &
                configured=.true., lift_preset_filter=lift, model=model)
            return
        end if
        exitcode = 0
        if (.not. no_build) then
            call backend_test_cases(b, names, size(names), exitcode, &
                log_file=log_file, flags=flags, configured=.true., build_only=.true., model=model)
            if (exitcode /= 0) return
        end if
        call listed_cases(b, flags, model, pack(cases, native), &
            pack(filters, native), listed, run_cases, message)
        if (len(message) > 0) then
            call ctest_log_failure(log_file, message)
            exitcode = 1
            return
        end if
        call ctest_native_run(listed, run_cases, log_file, &
            .not. no_build, exitcode)
        if (all(native)) return
        call refuse_filters(pack(filters, .not. native), log_file, code)
        if (code /= 0) then
            exitcode = code
            return
        end if
        do k = 1, size(cases)
            if (native(k)) cycle
            call ctest_log_note(log_file, 'fo: delegated to ctest: '// &
                trim(names(k))//' ('//reasons(k)%s//')')
        end do
        call backend_test_cases(b, pack_names(names, .not. native), &
            count(.not. native), code, log_file=log_file, flags=flags, &
            no_build=.true., configured=.true., lift_preset_filter=lift, &
            append_log=.true., model=model)
        if (code /= 0) exitcode = code
    end subroutine run_cmake_cases

    subroutine ctest_log_failure(log_file, message)
        !! A selection that cannot be applied, as a failed result line.
        character(len=*), intent(in) :: log_file, message
        integer :: u, ios

        open (newunit=u, file=trim(log_file), position='append', status='old', &
            action='write', iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') 'TEST_REASON '//message
        write (u, '(a)') 'TEST_RESULT fo-case-selection FAIL 1 0.00'
        close (u)
    end subroutine ctest_log_failure

    subroutine refuse_filters(filters, log_file, exitcode)
        !! Cases of a test that ctest itself runs cannot be selected.
        type(string_t), intent(in) :: filters(:)
        character(len=*), intent(in) :: log_file
        integer, intent(out) :: exitcode
        integer :: k

        exitcode = 0
        do k = 1, size(filters)
            if (len(filters(k)%s) == 0) cycle
            call ctest_log_failure(log_file, 'cannot select cases of a test that '// &
                'ctest runs: '//filters(k)%s)
            exitcode = 1
            return
        end do
    end subroutine refuse_filters

    subroutine listed_cases(b, flags, model, cases, filters, listed, run_cases, &
            message)
        !! listed: the model with the listed executables of cases expanded;
        !! run_cases: the cases to run in it (each executable's cases that
        !! match its filter, or the executable). message: why a filter fails.
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: flags
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        type(string_t), intent(in) :: filters(:)
        type(project_model_t), intent(out) :: listed
        integer, allocatable, intent(out) :: run_cases(:)
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: reason
        integer :: k, j, i, n, ierr

        listed = model
        message = ''
        allocate (run_cases(0))
        call expand_listed(b, flags, listed, cases, n, ierr, reason)
        if (ierr /= 0) then
            message = 'cannot expand the listed cases: '//reason
            return
        end if
        do k = 1, size(cases)
            i = model_find_case(listed, model%cases(cases(k))%name)
            if (.not. listed%cases(i)%expanded) then
                if (len(filters(k)%s) > 0) then
                    message = 'cannot select cases of '// &
                        model%cases(cases(k))%name//': it does not list cases'
                    return
                end if
                run_cases = [run_cases, i]
                cycle
            end if
            j = 0
            do n = 1, listed%n_cases
                if (.not. allocated(listed%cases(n)%parent)) cycle
                if (listed%cases(n)%parent /= listed%cases(i)%name) cycle
                if (.not. case_matches(filters(k)%s, listed%cases(n)%name( &
                    len(listed%cases(i)%name) + len(CASE_SEP) + 1:))) cycle
                run_cases = [run_cases, n]
                j = j + 1
            end do
            if (j == 0) then
                message = 'no case of '//listed%cases(i)%name//' matches '// &
                    filters(k)%s
                return
            end if
        end do
    end subroutine listed_cases

    function pack_names(names, keep) result(kept)
        character(len=*), intent(in) :: names(:)
        logical, intent(in) :: keep(:)
        character(len=len(names)) :: kept(count(keep))

        kept = pack(names, keep)
    end function pack_names

    subroutine plan_names(model, cases, plan, names)
        !! Target names of the plan. A widened plan still names the exact
        !! targets of its mapped cases, so a build widened by a script case
        !! also builds them (EXCLUDE_FROM_ALL executables included).
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        type(model_plan_t), intent(out) :: plan
        type(string_t), allocatable, intent(out) :: names(:)
        type(model_plan_t) :: mapped_plan
        integer, allocatable :: mapped(:)
        integer :: k

        call model_plan(model, cases, plan)
        if (.not. plan%widened) then
            allocate (names(size(plan%targets)))
            do k = 1, size(plan%targets)
                names(k)%s = model%targets(plan%targets(k))%name
            end do
            return
        end if
        allocate (names(0))
        mapped = pack(cases, [(valid_mapped(cases(k)), k=1, size(cases))])
        call model_plan(model, mapped, mapped_plan)
        if (mapped_plan%widened) return
        deallocate (names)
        allocate (names(size(mapped_plan%targets)))
        do k = 1, size(mapped_plan%targets)
            names(k)%s = model%targets(mapped_plan%targets(k))%name
        end do

    contains

        logical function valid_mapped(i)
            integer, intent(in) :: i

            valid_mapped = .false.
            if (i < 1 .or. i > model%n_cases) return
            valid_mapped = model%cases(i)%mapping /= MAP_UNKNOWN
        end function valid_mapped

    end subroutine plan_names

    function joined_labels(model, i) result(text)
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: i
        character(len=:), allocatable :: text
        integer :: k

        text = ''
        do k = 1, size(model%cases(i)%labels)
            if (k > 1) text = text//','
            text = text//model%cases(i)%labels(k)%s
        end do
    end function joined_labels

    function layer_name(model, i) result(text)
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: i
        character(len=:), allocatable :: text

        text = ''
        if (model%cases(i)%layer > 0) text = model%layers(model%cases(i)%layer)%name
    end function layer_name

    subroutine project_list_text(model, selection, text)
        !! Selected cases, plan targets and the widened flag with its reasons.
        type(project_model_t), intent(in) :: model
        type(model_selection_t), intent(in) :: selection
        character(len=:), allocatable, intent(out) :: text
        type(model_plan_t) :: plan
        type(string_t), allocatable :: targets(:)
        character(len=*), parameter :: nl = new_line('a')
        character(len=24) :: count
        integer :: k, i

        call plan_names(model, selection%cases, plan, targets)
        write (count, '(i0)') size(selection%cases)
        text = 'backend: '//model%backend//nl//'selected: '//trim(count)//nl
        do k = 1, size(selection%cases)
            i = selection%cases(k)
            text = text//'  '//model%cases(i)%name
            if (size(model%cases(i)%labels) > 0) &
                text = text//'  labels='//joined_labels(model, i)
            if (model%cases(i)%layer > 0) text = text//'  layer='//layer_name(model, i)
            text = text//'  ('//selection%reasons(i)%s//')'//nl
        end do
        text = text//'plan targets:'
        if (size(targets) == 0) text = text//' none'
        do k = 1, size(targets)
            text = text//' '//targets(k)%s
        end do
        text = text//nl
        if (plan%widened) then
            text = text//'widened: yes'//nl
            do k = 1, size(plan%reasons)
                text = text//'  reason: '//plan%reasons(k)%s//nl
            end do
        else
            text = text//'widened: no'//nl
        end if
    end subroutine project_list_text

    subroutine project_list_json(model, selection, text)
        type(project_model_t), intent(in) :: model
        type(model_selection_t), intent(in) :: selection
        character(len=:), allocatable, intent(out) :: text
        type(model_plan_t) :: plan
        type(string_t), allocatable :: targets(:)
        type(json_builder_t) :: jb
        integer :: k, j, i

        call plan_names(model, selection%cases, plan, targets)
        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'backend', model%backend)
        call json_key_string(jb, 'identity', trim(model%identity))
        call json_key(jb, 'cases')
        call json_array_start(jb)
        do k = 1, size(selection%cases)
            i = selection%cases(k)
            call json_object_start(jb)
            call json_key_string(jb, 'name', model%cases(i)%name)
            call json_key(jb, 'labels')
            call json_array_start(jb)
            do j = 1, size(model%cases(i)%labels)
                call json_value_string(jb, model%cases(i)%labels(j)%s)
            end do
            call json_array_end(jb)
            call json_key_string(jb, 'layer', layer_name(model, i))
            call json_key_string(jb, 'reason', selection%reasons(i)%s)
            call json_object_end(jb)
        end do
        call json_array_end(jb)
        call json_key(jb, 'targets')
        call json_array_start(jb)
        do k = 1, size(targets)
            call json_value_string(jb, targets(k)%s)
        end do
        call json_array_end(jb)
        call json_key_bool(jb, 'widened', plan%widened)
        call json_key(jb, 'widen_reasons')
        call json_array_start(jb)
        do k = 1, size(plan%reasons)
            call json_value_string(jb, plan%reasons(k)%s)
        end do
        call json_array_end(jb)
        call json_object_end(jb)
        text = json_to_string(jb)
    end subroutine project_list_json

    subroutine project_layering(b, flags, model, report, unscanned, ierr, message)
        !! Load the model and run the layering check (`fo graph --layers`).
        !! The covered sources are the Fortran sources of the project targets
        !! and layers (fpm: also the scanned sources in no layer), each scanned
        !! by fo's source scanner on both backends. Sources the scanner cannot
        !! read (a generated source not yet generated) are returned in
        !! unscanned and not checked. ierr /= 0: model unavailable.
        type(backend_t), intent(inout) :: b
        character(len=*), intent(in) :: flags
        type(project_model_t), intent(out) :: model
        type(model_layering_t), intent(out) :: report
        type(string_t), allocatable, intent(out) :: unscanned(:)
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        type(string_t), allocatable :: extra(:), paths(:)
        type(model_unit_t), allocatable :: units(:)
        integer :: k

        allocate (extra(0))
        if (b%kind == BACKEND_NATIVE) then
            ! fpm_model_load reports the scanned sources in no layer; they
            ! are no target source when they are test helpers.
            call fpm_model_load(trim(b%project_dir), model, ierr, message, &
                unlayered=extra)
            if (ierr /= 0) return
            do k = 1, size(extra)
                if (len(extra(k)%s) == 0) cycle
                if (extra(k)%s(1:1) == '/') cycle
                extra(k)%s = trim(b%project_dir)//'/'//extra(k)%s
            end do
        else
            call project_load(b, flags, model, ierr, message)
            if (ierr /= 0) return
        end if
        call model_layering_sources(model, extra, paths)
        call scan_units(paths, units, unscanned)
        call model_layering(model, units, report)
    end subroutine project_layering

    function project_relative(project_dir, path) result(rel)
        !! path relative to project_dir when it lies below it.
        character(len=*), intent(in) :: project_dir, path
        character(len=:), allocatable :: rel, prefix

        prefix = trim(project_dir)//'/'
        rel = path
        if (len(path) <= len(prefix)) return
        if (path(1:len(prefix)) == prefix) rel = path(len(prefix) + 1:)
    end function project_relative

    subroutine project_layering_text(project_dir, model, report, unscanned, text)
        !! One line per violation (source, module, both layers, missing edge),
        !! then the unlayered and unscanned sources and a verdict line.
        character(len=*), intent(in) :: project_dir
        type(project_model_t), intent(in) :: model
        type(model_layering_t), intent(in) :: report
        type(string_t), intent(in) :: unscanned(:)
        character(len=:), allocatable, intent(out) :: text
        character(len=*), parameter :: nl = new_line('a')
        character(len=24) :: count
        integer :: k

        write (count, '(i0)') model%n_layers
        text = 'backend: '//model%backend//nl//'layers: '//trim(count)
        write (count, '(i0)') report%checked
        text = text//'  checked sources: '//trim(count)//nl
        do k = 1, size(report%violations)
            associate (v => report%violations(k))
                text = text//'violation: '//project_relative(project_dir, v%source)// &
                    ' (layer '//v%layer//') uses module '//v%module//' from '// &
                    project_relative(project_dir, v%provider)//' (layer '// &
                    v%provider_layer//'); missing edge '//v%layer//' -> '// &
                    v%provider_layer//nl
            end associate
        end do
        do k = 1, size(report%unlayered)
            text = text//'unlayered: '// &
                project_relative(project_dir, report%unlayered(k)%s)//nl
        end do
        do k = 1, size(unscanned)
            text = text//'unscanned: '//project_relative(project_dir, unscanned(k)%s)//nl
        end do
        write (count, '(i0)') size(report%violations)
        if (size(report%violations) == 0) then
            text = text//'layering: ok'//nl
        else
            text = text//'layering: '//trim(count)//' violation(s)'//nl
        end if
    end subroutine project_layering_text

    subroutine project_layering_json(project_dir, model, report, unscanned, text)
        character(len=*), intent(in) :: project_dir
        type(project_model_t), intent(in) :: model
        type(model_layering_t), intent(in) :: report
        type(string_t), intent(in) :: unscanned(:)
        character(len=:), allocatable, intent(out) :: text
        type(json_builder_t) :: jb
        integer :: k, j

        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'backend', model%backend)
        call json_key_bool(jb, 'ok', size(report%violations) == 0)
        call json_key_int(jb, 'checked', report%checked)
        call json_key(jb, 'layers')
        call json_array_start(jb)
        do k = 1, model%n_layers
            call json_object_start(jb)
            call json_key_string(jb, 'name', model%layers(k)%name)
            call json_key(jb, 'uses')
            call json_array_start(jb)
            do j = 1, size(model%layers(k)%uses)
                call json_value_string(jb, model%layers(model%layers(k)%uses(j))%name)
            end do
            call json_array_end(jb)
            call json_object_end(jb)
        end do
        call json_array_end(jb)
        call json_key(jb, 'violations')
        call json_array_start(jb)
        do k = 1, size(report%violations)
            associate (v => report%violations(k))
                call json_object_start(jb)
                call json_key_string(jb, 'source', project_relative(project_dir, &
                    v%source))
                call json_key_string(jb, 'module', v%module)
                call json_key_string(jb, 'layer', v%layer)
                call json_key_string(jb, 'provider', project_relative(project_dir, &
                    v%provider))
                call json_key_string(jb, 'provider_layer', v%provider_layer)
                call json_key_string(jb, 'missing_edge', v%layer//' -> '// &
                    v%provider_layer)
                call json_object_end(jb)
            end associate
        end do
        call json_array_end(jb)
        call json_key(jb, 'unlayered')
        call json_array_start(jb)
        do k = 1, size(report%unlayered)
            call json_value_string(jb, project_relative(project_dir, &
                report%unlayered(k)%s))
        end do
        call json_array_end(jb)
        call json_key(jb, 'unscanned')
        call json_array_start(jb)
        do k = 1, size(unscanned)
            call json_value_string(jb, project_relative(project_dir, unscanned(k)%s))
        end do
        call json_array_end(jb)
        call json_object_end(jb)
        text = json_to_string(jb)
    end subroutine project_layering_json

end module fo_project_load
