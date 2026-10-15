module fo_project_load
    !! Load the project model of the detected backend and resolve a test
    !! selection on it. This is the one selection path shared by `fo test`,
    !! `fo test --list` and the MCP test tool (#167, #193): backend detection
    !! -> adapter (fpm manifest + scan, or CMake configure + File API + CTest)
    !! -> project_model_t -> model_select. Backends then receive exact case
    !! names (model_selected_names).
    use fx_string, only: string_t
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_array_start, json_array_end, json_key, &
        json_value_string, json_key_string, json_key_bool, &
        json_to_string
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file_alloc
    use fo_scan, only: is_slow_test
    use fo_build_backend, only: backend_t, BACKEND_NATIVE, BACKEND_CMAKE, &
        cmake_configure
    use fo_cmake_native_config, only: native_cmake_selected
    use fo_cmake_model, only: cmake_model_load
    use fo_fpm_model, only: fpm_model_load
    use fo_gfortran_build, only: gfortran_named_test_exists
    use fo_project_model, only: project_model_t, model_request_t, &
        model_selection_t, model_plan_t, model_select, model_plan, &
        model_find_case, model_selected_names, SELECT_OK, &
        SELECT_UNKNOWN_NAME, SELECT_EMPTY, SELECT_NAMED_EXCLUDED, &
        CASE_DEFAULT_EXCLUDED
    implicit none
    private

    public :: project_has_model, project_load, project_resolve, project_run_names
    public :: project_list_text, project_list_json

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

    subroutine project_resolve(b, flags, request, model, selection, ierr, message, &
            extra)
        !! Load the model and select. ierr = 0: the selection is usable (an
        !! empty selection has selection%status = SELECT_EMPTY and the caller
        !! decides). ierr /= 0: message is the diagnostic, without prefix.
        !! extra: names the native inventory accepts although they are no
        !! model case (a dispatcher program named explicitly); they run as
        !! named and belong to no plan.
        type(backend_t), intent(inout) :: b
        character(len=*), intent(in) :: flags
        type(model_request_t), intent(in) :: request
        type(project_model_t), intent(out) :: model
        type(model_selection_t), intent(out) :: selection
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        type(string_t), allocatable, intent(out), optional :: extra(:)
        integer :: i, k
        logical :: all_legacy

        if (present(extra)) allocate (extra(0))
        call project_load(b, flags, model, ierr, message)
        if (ierr /= 0) return
        call model_select(model, request, selection)
        if (selection%status == SELECT_UNKNOWN_NAME .and. b%kind == BACKEND_NATIVE &
            .and. present(extra)) then
            all_legacy = .true.
            do i = 1, size(selection%unknown)
                if (gfortran_named_test_exists(trim(b%project_dir), &
                    selection%unknown(i)%s)) cycle
                all_legacy = .false.
                exit
            end do
            if (all_legacy) then
                extra = selection%unknown
                selection%status = SELECT_OK
                if (size(selection%cases) == 0) selection%status = SELECT_EMPTY
            end if
        end if
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
        ! Explicit slow names stay gated until the backends stop filtering
        ! by name (CMake names that only look slow, for instance).
        if (.not. request%all) then
            do i = 1, size(request%names)
                if (model_find_case(model, request%names(i)%s) == 0) cycle
                if (.not. is_slow_test(request%names(i)%s)) cycle
                ierr = 1
                message = 'slow test '//request%names(i)%s//' requires --all'
                return
            end do
        end if
    end subroutine project_resolve

    subroutine project_run_names(model, selection, extra, names)
        !! Exact names handed to the backend: selected cases, then extras.
        type(project_model_t), intent(in) :: model
        type(model_selection_t), intent(in) :: selection
        type(string_t), intent(in) :: extra(:)
        character(len=:), allocatable, intent(out) :: names(:)
        type(string_t), allocatable :: selected(:)
        integer :: k, width

        call model_selected_names(model, selection, selected)
        width = 1
        do k = 1, size(selected)
            width = max(width, len(selected(k)%s))
        end do
        do k = 1, size(extra)
            width = max(width, len(extra(k)%s))
        end do
        allocate (character(len=width) :: names(size(selected) + size(extra)))
        do k = 1, size(selected)
            names(k) = selected(k)%s
        end do
        do k = 1, size(extra)
            names(size(selected) + k) = extra(k)%s
        end do
    end subroutine project_run_names

    subroutine plan_names(model, selection, plan, names)
        type(project_model_t), intent(in) :: model
        type(model_selection_t), intent(in) :: selection
        type(model_plan_t), intent(out) :: plan
        type(string_t), allocatable, intent(out) :: names(:)
        integer :: k

        call model_plan(model, selection%cases, plan)
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

        call plan_names(model, selection, plan, targets)
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

        call plan_names(model, selection, plan, targets)
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

end module fo_project_load
