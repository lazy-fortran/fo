module fo_exec_target
    use fo_build_backend, only: backend_t, BACKEND_CMAKE, BACKEND_NATIVE
    use fo_build_tree, only: native_output_dir
    use fo_fs, only: fs_collect_files
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    use fo_gfortran_build, only: gfortran_app_source_name, gfortran_test_source_name
    use fo_scan, only: scan_unit_t, scan_dir
    implicit none
    private

    public :: resolve_exec_target, exec_target_is_app
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
        integer :: ierr

        is_app = .false.
        if (b%kind /= BACKEND_NATIVE) return
        allocate (config)
        call fpm_config_parse(b%project_dir, config, ierr)
        if (ierr /= 0) return
        is_app = source_dir_has_target(config, config%app_dir, target, .false.)
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

    subroutine resolve_exec_target(b, target, bin_path, found, flags)
        !! Locate the built executable for target. For the native backend the
        !! requested flags select the build tree (see fo_build_tree): a binary
        !! built with other flags is never substituted.
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: target
        character(len=*), intent(out) :: bin_path
        logical, intent(out) :: found
        character(len=*), intent(in), optional :: flags

        character(len=1024) :: candidates(64)
        integer :: i, last_slash, n_candidates, n_exact

        if (present(flags)) then
            bin_path = native_output_dir(b%project_dir, flags, 'bin')//'/'// &
                trim(target)
        else
            bin_path = trim(b%project_dir)//'/build/fo/bin/'//trim(target)
        end if
        inquire (file=trim(bin_path), exist=found)
        if (found .or. b%kind /= BACKEND_CMAKE) return

        bin_path = trim(b%project_dir)//'/build/'//trim(target)
        inquire (file=trim(bin_path), exist=found)
        if (found) return

        ! CMake's conventional output directory, checked before the recursive
        ! scan below. A project that has also been built with fpm keeps a
        ! second copy under build/<compiler-hash>/app, and the scan rejects
        ! anything it finds twice. Without this probe a target that is present
        ! at the canonical path is reported as missing purely because a stale
        ! sibling build tree exists.
        bin_path = trim(b%project_dir)//'/build/bin/'//trim(target)
        inquire (file=trim(bin_path), exist=found)
        if (found) return

        last_slash = index(trim(target), '/', back=.true.)
        if (last_slash > 0) return

        call fs_collect_files(trim(b%project_dir)//'/build', '', trim(target), '', &
            candidates, n_candidates)
        n_exact = 0
        do i = 1, n_candidates
            last_slash = index(trim(candidates(i)), '/', back=.true.)
            if (trim(candidates(i)(last_slash + 1:)) /= trim(target)) cycle
            n_exact = n_exact + 1
            bin_path = candidates(i)
        end do
        found = n_exact == 1
        if (.not. found) bin_path = ''
    end subroutine resolve_exec_target

end module fo_exec_target
