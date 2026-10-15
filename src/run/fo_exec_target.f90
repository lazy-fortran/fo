module fo_exec_target
    use fo_build_backend, only: backend_t, BACKEND_CMAKE, BACKEND_NATIVE, &
        cmake_configure
    use fo_build_tree, only: native_output_dir
    use fo_cmake_context, only: cmake_context_t
    use fo_cmake_model, only: cmake_model_load
    use fo_cmake_native, only: native_cmake_resolve
    use fo_cmake_native_config, only: native_cmake_selected
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    use fo_project_model, only: project_model_t, model_find_target, TARGET_EXECUTABLE
    use fo_gfortran_build, only: gfortran_app_source_name, gfortran_test_source_name, &
        scan_project_app_units
    use fo_scan, only: scan_unit_t, scan_dir
    implicit none
    private

    public :: resolve_exec_target, exec_target_is_app
    public :: exec_uses_cmake_model, resolve_cmake_exec
    public :: exec_args_t, parse_exec_args

    type :: exec_args_t
        !! Parsed `fo exec` / `fo run` command line. to_program marks the
        !! arguments handed to the executable, in order.
        character(len=512) :: cwd = ''
        character(len=256) :: target = ''
        integer :: target_index = 0
        logical :: skip_build = .false.
        logical :: help = .false.
        character(len=512) :: flags = ''
        character(len=64) :: profile = ''
        character(len=256) :: error = ''
        logical, allocatable :: to_program(:)
    end type exec_args_t

contains

    subroutine parse_exec_args(args, run_style, parsed)
        !! args are the words after the subcommand. For `fo exec` the first
        !! non-option word is the target and everything after it belongs to
        !! the program, so `fo exec app --no-build` passes --no-build to app
        !! instead of silently skipping the build. `fo run` (run_style) follows
        !! fpm: fo options may also follow the target, and only words after
        !! `--` or not recognised as fo options go to the program.
        character(len=*), intent(in) :: args(:)
        logical, intent(in) :: run_style
        type(exec_args_t), intent(out) :: parsed

        character(len=:), allocatable :: arg
        integer :: i, n, eq
        logical :: literal

        n = size(args)
        allocate (parsed%to_program(n))
        parsed%to_program = .false.
        literal = .false.
        i = 1
        do while (i <= n)
            arg = trim(args(i))
            if (literal) then
                parsed%to_program(i) = .true.
            else if (parsed%target_index > 0 .and. .not. run_style) then
                parsed%to_program(i) = .true.
            else if (arg == '--') then
                literal = parsed%target_index > 0
            else if (option_takes_value(arg)) then
                if (i == n) then
                    parsed%error = arg//' requires a value'
                    return
                end if
                i = i + 1
                call apply_option(arg, trim(args(i)), parsed)
                if (arg == '--target') parsed%target_index = i
            else if (keyed_option(arg)) then
                eq = index(arg, '=')
                call apply_option(arg(:eq - 1), arg(eq + 1:), parsed)
                if (arg(:eq - 1) == '--target') parsed%target_index = i
            else if (apply_switch(arg, parsed)) then
                continue
            else if (parsed%target_index == 0) then
                parsed%target = arg
                parsed%target_index = i
            else
                parsed%to_program(i) = .true.
            end if
            i = i + 1
        end do
    end subroutine parse_exec_args

    logical function keyed_option(arg) result(keyed)
        !! --name=value form of an option that takes a value.
        character(len=*), intent(in) :: arg
        integer :: eq

        keyed = .false.
        eq = index(arg, '=')
        if (eq > 1) keyed = option_takes_value(arg(:eq - 1))
    end function keyed_option

    logical function option_takes_value(arg) result(takes)
        character(len=*), intent(in) :: arg

        select case (arg)
        case ('--cwd', '--profile', '--flag', '--target')
            takes = .true.
        case default
            takes = .false.
        end select
    end function option_takes_value

    subroutine apply_option(name, value, parsed)
        character(len=*), intent(in) :: name, value
        type(exec_args_t), intent(inout) :: parsed

        select case (name)
        case ('--cwd')
            parsed%cwd = value
        case ('--profile')
            parsed%profile = value
        case ('--flag')
            parsed%flags = value
        case ('--target')
            parsed%target = value
        end select
    end subroutine apply_option

    logical function apply_switch(arg, parsed) result(known)
        character(len=*), intent(in) :: arg
        type(exec_args_t), intent(inout) :: parsed

        known = .true.
        select case (arg)
        case ('--no-build')
            parsed%skip_build = .true.
        case ('--example')
            ! Examples share fo's executable namespace; the fpm selector is
            ! accepted without changing resolution.
        case ('--release', '--debug', '--asan')
            parsed%profile = arg(3:)
        case ('-h', '--help')
            parsed%help = .true.
        case default
            known = .false.
        end select
    end function apply_switch

    logical function exec_target_is_app(b, target) result(is_app)
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: target
        type(fpm_config_t), allocatable :: config
        integer :: ierr, i, n_units
        type(scan_unit_t), allocatable :: units(:)

        is_app = .false.
        if (b%kind /= BACKEND_NATIVE) return
        allocate (config)
        call fpm_config_parse(b%project_dir, config, ierr)
        if (ierr /= 0) return
        call scan_project_app_units(config%project_dir, config, units, n_units, ierr)
        if (ierr /= 0) return
        do i = 1, n_units
            if (.not. units(i)%is_program .or. units(i)%is_test) cycle
            if (gfortran_app_source_name(config, units(i)%filename) /= target) cycle
            is_app = .true.
            exit
        end do
        if (.not. is_app) then
            if (config%auto_examples .or. config%n_examples > 0) then
                is_app = source_dir_has_target(config, config%example_dir, &
                    target, .false.)
            end if
        end if
        if (.not. is_app) is_app = source_dir_has_target(config, &
            config%source_dir, target, .false.)
        if (is_app) then
            ! Combined builds materialize tests last. Preserve that precedence
            ! when an application and a test share the same public name.
            is_app = .not. source_dir_has_target(config, config%test_dir, &
                target, .true.)
        end if
    end function exec_target_is_app

    logical function source_dir_has_target(config, source_dir, target, tests) &
            result(found)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: source_dir, target
        logical, intent(in) :: tests
        type(scan_unit_t), allocatable :: units(:)
        character(len=128) :: public_name
        integer :: i, n_units, ierr

        found = .false.
        call scan_dir(trim(config%project_dir)//'/'//trim(source_dir), &
            units, n_units, ierr)
        if (ierr /= 0) return
        do i = 1, n_units
            if (.not. units(i)%is_program) cycle
            if (tests) then
                public_name = gfortran_test_source_name(config, source_dir, &
                    units(i)%filename)
            else
                if (units(i)%is_test) cycle
                public_name = gfortran_app_source_name(config, units(i)%filename)
            end if
            if (public_name /= target) cycle
            found = .true.
            return
        end do
    end function source_dir_has_target

    logical function exec_uses_cmake_model(b) result(uses)
        !! CMake projects resolve `fo exec` targets from the File API project
        !! model. FO_CMAKE_NATIVE keeps its own resolver.
        type(backend_t), intent(in) :: b

        uses = b%kind == BACKEND_CMAKE
        if (uses) uses = .not. native_cmake_selected(b%project_dir)
    end function exec_uses_cmake_model

    subroutine resolve_exec_target(b, target, bin_path, found, flags)
        !! Locate the built executable for target of a fpm or native CMake
        !! backend. The requested flags select the build tree (see fo_build_tree):
        !! a binary built with other flags is never substituted. Non-native CMake
        !! targets are resolved by resolve_cmake_exec instead.
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: target
        character(len=*), intent(out) :: bin_path
        logical, intent(out) :: found
        character(len=*), intent(in), optional :: flags

        if (b%kind /= BACKEND_CMAKE) then
            if (present(flags)) then
                bin_path = native_output_dir(b%project_dir, flags, 'bin')//'/'// &
                    trim(target)
            else
                bin_path = trim(b%project_dir)//'/build/fo/bin/'//trim(target)
            end if
            inquire (file=trim(bin_path), exist=found)
            return
        end if

        found = .false.
        bin_path = ''
        if (native_cmake_selected(b%project_dir)) &
            call native_cmake_resolve(b%cmake, target, bin_path, found)
    end subroutine resolve_exec_target

    subroutine resolve_cmake_exec(context, target, flags, log_file, bin_path, &
            found, configured, message)
        !! Configure the CMake project (File API query included), then resolve
        !! target from the project model. bin_path is the target's own artifact,
        !! so the build is never searched for a same-named file. configured is
        !! false when CMake failed (details in log_file). found is false with
        !! message set for an unknown or non-executable target. The artifact is
        !! not checked for existence: the caller builds the target first.
        type(cmake_context_t), intent(inout) :: context
        character(len=*), intent(in) :: target, flags, log_file
        character(len=*), intent(out) :: bin_path
        logical, intent(out) :: found, configured
        character(:), allocatable, intent(out) :: message

        type(project_model_t) :: model
        character(:), allocatable :: load_message
        integer :: exitcode, ierr, target_index

        found = .false.
        configured = .false.
        bin_path = ''
        message = ''
        call cmake_configure(context, flags, log_file, exitcode)
        if (exitcode /= 0) return
        configured = .true.

        call cmake_model_load(context, model, ierr, load_message)
        if (ierr /= 0) then
            message = 'CMake project model unavailable: '//load_message
            return
        end if
        target_index = model_find_target(model, trim(target))
        if (target_index == 0) then
            message = 'no such target: '//trim(target)//executable_hints(model, target)
            return
        end if
        if (model%targets(target_index)%kind /= TARGET_EXECUTABLE .or. &
            .not. allocated(model%targets(target_index)%artifact)) then
            message = 'not an executable target: '//trim(target)// &
                executable_hints(model, target)
            return
        end if
        if (len(model%targets(target_index)%artifact) > len(bin_path)) then
            message = 'executable path too long: '//model%targets(target_index)%artifact
            return
        end if
        bin_path = model%targets(target_index)%artifact
        found = .true.
    end subroutine resolve_cmake_exec

    function executable_hints(model, target) result(text)
        !! Executable targets whose names resemble target, or all executable
        !! targets when none do. Empty when the project has no executables.
        type(project_model_t), intent(in) :: model
        character(len=*), intent(in) :: target
        character(:), allocatable :: text
        integer :: i, n_listed, n_exec
        logical :: near_only

        n_exec = 0
        near_only = .false.
        do i = 1, model%n_targets
            if (model%targets(i)%kind /= TARGET_EXECUTABLE) cycle
            n_exec = n_exec + 1
            if (near_name(model%targets(i)%name, target)) near_only = .true.
        end do
        text = ''
        if (n_exec == 0) then
            text = ' (the project defines no executable targets)'
            return
        end if
        n_listed = 0
        text = ' (executable targets'
        if (near_only) text = ' (similar executable targets'
        text = text//': '
        do i = 1, model%n_targets
            if (model%targets(i)%kind /= TARGET_EXECUTABLE) cycle
            if (near_only .and. .not. near_name(model%targets(i)%name, target)) cycle
            if (n_listed >= 12) exit
            if (n_listed > 0) text = text//', '
            text = text//model%targets(i)%name
            n_listed = n_listed + 1
        end do
        text = text//')'
    end function executable_hints

    logical function near_name(name, target) result(near)
        character(len=*), intent(in) :: name, target

        near = index(name, trim(target)) > 0 .or. index(trim(target), name) > 0
        if (.not. near) near = edit_distance(name, trim(target)) <= &
            max(2, len_trim(target) / 3)
    end function near_name

    integer function edit_distance(a, b) result(distance)
        !! Levenshtein distance between two short names.
        character(len=*), intent(in) :: a, b
        integer, allocatable :: previous(:), current(:)
        integer :: i, j, cost

        allocate (previous(0:len(b)), current(0:len(b)))
        do j = 0, len(b)
            previous(j) = j
        end do
        do i = 1, len(a)
            current(0) = i
            do j = 1, len(b)
                cost = 1
                if (a(i:i) == b(j:j)) cost = 0
                current(j) = min(previous(j) + 1, current(j - 1) + 1, &
                    previous(j - 1) + cost)
            end do
            previous = current
        end do
        distance = previous(len(b))
    end function edit_distance

end module fo_exec_target
