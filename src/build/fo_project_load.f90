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
    use fo_scan, only: is_slow_test
    use fo_build_backend, only: backend_t, BACKEND_NATIVE, BACKEND_CMAKE, &
        cmake_configure, backend_test_names, backend_test_cases, &
        backend_set_build_targets
    use fo_cmake_native_config, only: native_cmake_selected
    use fo_cmake_model, only: cmake_model_load
    use fo_fpm_model, only: fpm_model_load
    use fo_project_model, only: project_model_t, model_request_t, &
        model_selection_t, model_plan_t, model_select, model_plan, &
        model_find_case, SELECT_OK, &
        SELECT_UNKNOWN_NAME, SELECT_EMPTY, SELECT_NAMED_EXCLUDED, &
        CASE_DEFAULT_EXCLUDED
    use fo_project_model, only: model_unit_t, model_layering_t, model_layering, &
        model_layering_sources
    use fo_scan, only: scan_unit_t, scan_file
    use fo_process, only: process_has_fortran_source_ext
    use fx_json_build, only: json_key_int
    implicit none
    private

    public :: project_has_model, project_load, project_resolve, project_run_cases
    public :: project_list_text, project_list_json
    public :: project_layering, project_layering_text, project_layering_json

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

    subroutine project_load(b, flags, model, ierr, message)
        !! Load a finalized model. CMake is configured first (File API reply
        !! and CTest registry); nothing is built. ierr /= 0 with message.
        type(backend_t), intent(inout) :: b
        character(len=*), intent(in) :: flags
        type(project_model_t), intent(out) :: model
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
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
            call fpm_model_load(trim(b%project_dir), model, ierr, message)
            return
        end if
        call make_tmpfile('fo-model-configure', log_file)
        call cmake_configure(b%cmake, flags, trim(log_file), exitcode)
        if (exitcode /= 0) then
            ierr = 1
            message = 'CMake configure failed'
            call read_text_file_alloc(trim(log_file), text, status)
            if (status == 0) then
                if (len(text) > 2000) text = '...'//text(len(text) - 1999:)
                message = message//new_line('a')//trim(text)
            end if
            call delete_tmpfile(trim(log_file))
            return
        end if
        call delete_tmpfile(trim(log_file))
        call cmake_model_load(b%cmake, model, ierr, message)
        if (ierr /= 0) message = 'CMake project model unavailable: '//message
    end subroutine project_load

    subroutine project_resolve(b, flags, request, model, selection, ierr, message)
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
        integer :: i, k

        call project_load(b, flags, model, ierr, message)
        if (ierr /= 0) return
        call model_select(model, request, selection)
        select case (selection%status)
        case (SELECT_OK, SELECT_EMPTY)
            ierr = 0
        case (SELECT_NAMED_EXCLUDED)
            ierr = 1
            message = selection%message
            ! Keep the established wording for the slow-name rule.
            do i = 1, size(request%names)
                k = model_find_case(model, request%names(i)%s)
                if (k == 0) cycle
                if (selection%disposition(k) /= CASE_DEFAULT_EXCLUDED) cycle
                if (is_slow_test(request%names(i)%s)) message = 'slow test '// &
                    request%names(i)%s//' requires --all'
                exit
            end do
        case default
            ierr = 1
            message = selection%message
        end select
        if (ierr /= 0) return
    end subroutine project_resolve

    subroutine project_run_cases(b, model, cases, flags, no_build, log_file, &
            exitcode)
        !! Build what the cases need, then run exactly them. The model is
        !! already loaded (CMake is configured), so nothing configures again.
        !! CMake: build the plan targets only, then ctest with exact anchored
        !! names in batches. fpm: its named-test path. no_build runs without
        !! building.
        type(backend_t), intent(inout) :: b
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        character(len=*), intent(in) :: flags
        logical, intent(in) :: no_build
        character(len=*), intent(in) :: log_file
        integer, intent(out) :: exitcode
        character(len=:), allocatable :: names(:)
        type(model_plan_t) :: plan
        type(string_t), allocatable :: targets(:)
        integer :: k, width
        logical :: lift

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
            call backend_test_cases(b, names, size(names), exitcode, &
                log_file=log_file, flags=flags, no_build=no_build, &
                configured=.true., lift_preset_filter=lift)
        else
            call backend_test_names(b, names, size(names), exitcode, .true., &
                log_file, flags=flags, no_build=no_build)
        end if
    end subroutine project_run_cases

    subroutine plan_names(model, cases, plan, names)
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        type(model_plan_t), intent(out) :: plan
        type(string_t), allocatable, intent(out) :: names(:)
        integer :: k

        call model_plan(model, cases, plan)
        allocate (names(size(plan%targets)))
        do k = 1, size(plan%targets)
            names(k)%s = model%targets(plan%targets(k))%name
        end do
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
        type(scan_unit_t) :: scanned
        logical, allocatable :: failed(:)
        integer :: k, n, status, m, n_extra, k_extra

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
        allocate (units(size(paths)), failed(size(paths)))
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
            units(n)%path = paths(k)%s
            n_extra = 0
            if (allocated(scanned%additional_modules)) &
                n_extra = size(scanned%additional_modules)
            m = 0
            if (len_trim(scanned%module_name) > 0) m = 1
            allocate (units(n)%provides(m + n_extra))
            if (m == 1) units(n)%provides(1)%s = trim(scanned%module_name)
            do k_extra = 1, n_extra
                units(n)%provides(m + k_extra)%s = &
                    trim(scanned%additional_modules(k_extra))
            end do
            allocate (units(n)%uses(scanned%n_deps))
            do m = 1, scanned%n_deps
                units(n)%uses(m)%s = trim(scanned%deps(m))
            end do
        end do
        allocate (unscanned(count(failed)))
        m = 0
        do k = 1, size(paths)
            if (.not. failed(k)) cycle
            m = m + 1
            unscanned(m) = paths(k)
        end do
        call model_layering(model, units(1:n), report)
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
