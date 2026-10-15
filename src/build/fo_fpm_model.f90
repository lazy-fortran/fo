module fo_fpm_model
    !! fpm adapter for the backend-neutral project model (#167).
    !!
    !! Baseline: the library, the app and test executables and the dependency
    !! packages as targets; every discovered test as a case whose command
    !! target is the executable that runs it (the dispatcher for marked
    !! tests); the `slow` label from is_slow_test, excluded by default.
    !! Case names are the `fo test` inventory (same eligibility, naming and
    !! dispatcher rules as the native test runner). No layers yet.
    use fx_string, only: string_t
    use fx_hash, only: sha256_string
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, fpm_config_allocate
    use fo_scan, only: scan_unit_t, scan_dir, is_slow_test, MAX_PATH
    use fo_util, only: read_text_file_alloc
    use fo_build_tree, only: native_output_dir
    use fo_dep_resolve, only: resolved_src_t, resolve_dep_srcs, &
        resolve_dev_dep_srcs, MAX_RESOLVED
    use fo_gfortran_build, only: scan_project_test_units, scan_project_app_units, &
        gfortran_app_source_name, gfortran_test_source_name, &
        test_source_eligible, scanned_dispatcher_available, &
        unit_has_dispatcher_marker
    use fo_project_model, only: project_model_t, model_init, model_add_target, &
        model_add_case, model_finalize, model_find_target, model_find_case, &
        TARGET_LIBRARY, TARGET_EXECUTABLE, ORIGIN_DEPENDENCY
    implicit none
    private

    public :: fpm_model_load

contains

    subroutine fpm_model_load(project_dir, model, ierr, message)
        !! Load the model of the fpm project in project_dir. ierr /= 0 with a
        !! message when the manifest, a source scan or the model is invalid.
        character(len=*), intent(in) :: project_dir
        type(project_model_t), intent(out) :: model
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message

        type(fpm_config_t), allocatable :: config
        type(scan_unit_t), allocatable :: tests(:), apps(:), lib(:)
        type(resolved_src_t), allocatable :: deps(:), devs(:)
        type(string_t), allocatable :: app_deps(:), test_deps(:), lib_deps(:)
        type(string_t) :: label(1)
        character(len=:), allocatable :: bin_dir, name
        character(len=MAX_PATH) :: case_name
        integer :: n_tests, n_apps, n_lib, n_deps, n_devs, n_unresolved
        integer :: i, dispatcher, lib_index, target_index
        logical :: has_dispatcher, has_library

        message = ''
        call fpm_config_allocate(config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            message = 'cannot parse fpm.toml in '//trim(project_dir)
            return
        end if
        call scan_project_test_units(project_dir, config, tests, n_tests, ierr)
        if (ierr /= 0) then
            message = 'cannot scan test sources'
            return
        end if
        call scan_project_app_units(project_dir, config, apps, n_apps, ierr)
        if (ierr /= 0) then
            message = 'cannot scan executable sources'
            return
        end if
        call scan_library(project_dir, config, lib, n_lib, has_library, ierr)
        if (ierr /= 0) then
            message = 'cannot scan library sources'
            return
        end if
        allocate (deps(MAX_RESOLVED), devs(MAX_RESOLVED))
        call resolve_dep_srcs(project_dir, deps, n_deps, n_unresolved, ierr)
        if (ierr /= 0) then
            message = 'cannot resolve dependencies'
            return
        end if
        call resolve_dev_dep_srcs(project_dir, devs, n_devs, ierr)
        if (ierr /= 0) then
            message = 'cannot resolve dev-dependencies'
            return
        end if

        call model_init(model, 'fpm', input_digest(project_dir, tests, n_tests, &
            apps, n_apps, lib, n_lib))
        bin_dir = native_output_dir(project_dir, '', 'bin')

        ! Dependency packages first: project targets link them by name.
        do i = 1, n_deps
            call model_add_target(model, trim(deps(i)%name), TARGET_LIBRARY, &
                origin=ORIGIN_DEPENDENCY)
        end do
        do i = 1, n_devs
            if (model_find_target(model, trim(devs(i)%name)) > 0) cycle
            call model_add_target(model, trim(devs(i)%name), TARGET_LIBRARY, &
                origin=ORIGIN_DEPENDENCY)
        end do
        lib_index = 0
        if (has_library) then
            call model_add_target(model, library_name(config), TARGET_LIBRARY, &
                sources=unit_files(lib, n_lib), &
                link_deps=dep_names(deps, n_deps), index=lib_index)
        end if
        lib_deps = library_deps(config, has_library, deps, n_deps)
        app_deps = lib_deps
        test_deps = merge_names(lib_deps, dep_names(devs, n_devs))

        do i = 1, n_apps
            if (.not. apps(i)%is_program) cycle
            name = trim(gfortran_app_source_name(config, apps(i)%filename))
            if (len(name) == 0) cycle
            if (model_find_target(model, name) > 0) cycle
            call model_add_target(model, name, TARGET_EXECUTABLE, &
                sources=[string_t(trim(apps(i)%filename))], link_deps=app_deps, &
                artifact=bin_dir//'/'//name)
        end do

        has_dispatcher = scanned_dispatcher_available(config, config%test_dir, &
            tests, n_tests)
        dispatcher = 0
        ! Test executables: the dispatcher and every unmarked program. A marked
        ! program runs inside the dispatcher and has no executable of its own.
        do i = 1, n_tests
            if (.not. eligible(i)) cycle
            case_name = gfortran_test_source_name(config, config%test_dir, &
                tests(i)%filename)
            if (len_trim(case_name) == 0) cycle
            name = trim(case_name)
            if (has_dispatcher) then
                if (name == trim(config%dispatcher)) then
                    call add_test_exe(i, name, dispatcher)
                    cycle
                end if
            end if
            if (.not. tests(i)%is_program) cycle
            if (marked(i)) cycle
            call add_test_exe(i, name, target_index)
        end do

        ! Cases in inventory order. The dispatcher itself is infrastructure,
        ! not a case, as in the native test runner.
        label(1)%s = 'slow'
        do i = 1, n_tests
            if (.not. eligible(i)) cycle
            case_name = gfortran_test_source_name(config, config%test_dir, &
                tests(i)%filename)
            if (len_trim(case_name) == 0) cycle
            name = trim(case_name)
            if (len_trim(config%dispatcher) > 0 .and. &
                name == trim(config%dispatcher)) cycle
            if (model_find_case(model, name) > 0) cycle
            if (marked(i)) then
                target_index = dispatcher
            else
                target_index = model_find_target(model, name)
            end if
            if (is_slow_test(name)) then
                call model_add_case(model, name, command_target=target_index, &
                    labels=label)
            else
                call model_add_case(model, name, command_target=target_index)
            end if
        end do

        model%default_exclude = [string_t('slow')]
        ! Every target and case is visible to the scan; an unresolved
        ! dependency checkout hides targets the project links.
        model%complete = n_unresolved == 0
        call model_finalize(model, ierr, message)

    contains

        logical function eligible(k)
            integer, intent(in) :: k

            eligible = test_source_eligible(tests(k)%filename, &
                tests(k)%is_program, has_dispatcher, &
                tests(k)%is_program .or. len_trim(tests(k)%module_name) > 0)
        end function eligible

        logical function marked(k)
            integer, intent(in) :: k

            marked = .false.
            if (.not. has_dispatcher) return
            marked = unit_has_dispatcher_marker(tests(k), project_dir, &
                config%test_dir)
        end function marked

        subroutine add_test_exe(k, exe, index)
            integer, intent(in) :: k
            character(len=*), intent(in) :: exe
            integer, intent(out) :: index

            index = model_find_target(model, exe)
            if (index > 0) return
            call model_add_target(model, exe, TARGET_EXECUTABLE, &
                sources=[string_t(trim(tests(k)%filename))], &
                link_deps=test_deps, artifact=bin_dir//'/'//exe, index=index)
        end subroutine add_test_exe

    end subroutine fpm_model_load

    subroutine scan_library(project_dir, config, units, n_units, present_, ierr)
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(in) :: config
        type(scan_unit_t), allocatable, intent(out) :: units(:)
        integer, intent(out) :: n_units, ierr
        logical, intent(out) :: present_
        logical :: exists

        n_units = 0
        ierr = 0
        allocate (units(0))
        inquire (file=trim(project_dir)//'/'//trim(config%source_dir), exist=exists)
        present_ = .false.
        if (.not. exists) return
        call scan_dir(trim(project_dir)//'/'//trim(config%source_dir), units, &
            n_units, ierr)
        if (ierr /= 0) return
        present_ = n_units > 0 .and. len_trim(config%name) > 0
    end subroutine scan_library

    function library_name(config) result(name)
        !! fpm names the default executable after the package, so the library
        !! target carries a `lib` prefix to keep target names unique.
        type(fpm_config_t), intent(in) :: config
        character(len=:), allocatable :: name

        name = 'lib'//trim(config%name)
    end function library_name

    function library_deps(config, has_library, deps, n_deps) result(names)
        !! Executables link the project library, or its dependencies directly.
        type(fpm_config_t), intent(in) :: config
        logical, intent(in) :: has_library
        type(resolved_src_t), intent(in) :: deps(:)
        integer, intent(in) :: n_deps
        type(string_t), allocatable :: names(:)

        if (has_library) then
            allocate (names(1))
            names(1)%s = library_name(config)
        else
            names = dep_names(deps, n_deps)
        end if
    end function library_deps

    function merge_names(first, second) result(names)
        !! first followed by the entries of second not already present.
        type(string_t), intent(in) :: first(:), second(:)
        type(string_t), allocatable :: names(:)
        integer :: i, j

        names = first
        do i = 1, size(second)
            do j = 1, size(names)
                if (names(j)%s == second(i)%s) exit
            end do
            if (j > size(names)) names = [names, second(i)]
        end do
    end function merge_names

    function dep_names(deps, n_deps) result(names)
        type(resolved_src_t), intent(in) :: deps(:)
        integer, intent(in) :: n_deps
        type(string_t), allocatable :: names(:)
        integer :: i

        allocate (names(n_deps))
        do i = 1, n_deps
            names(i)%s = trim(deps(i)%name)
        end do
    end function dep_names

    function unit_files(units, n_units) result(files)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n_units
        type(string_t), allocatable :: files(:)
        integer :: i

        allocate (files(n_units))
        do i = 1, n_units
            files(i)%s = trim(units(i)%filename)
        end do
    end function unit_files

    function input_digest(project_dir, tests, n_tests, apps, n_apps, lib, n_lib) &
            result(digest)
        !! Digest of the adapter inputs: the manifest text and every scanned
        !! source path.
        character(len=*), intent(in) :: project_dir
        type(scan_unit_t), intent(in) :: tests(:), apps(:), lib(:)
        integer, intent(in) :: n_tests, n_apps, n_lib
        character(len=:), allocatable :: digest, text, manifest
        integer :: i, ios

        call read_text_file_alloc(trim(project_dir)//'/fpm.toml', manifest, ios)
        if (ios /= 0) manifest = ''
        text = manifest
        do i = 1, n_tests
            text = text//new_line('a')//'t:'//trim(tests(i)%filename)
        end do
        do i = 1, n_apps
            text = text//new_line('a')//'a:'//trim(apps(i)%filename)
        end do
        do i = 1, n_lib
            text = text//new_line('a')//'l:'//trim(lib(i)%filename)
        end do
        digest = sha256_string(text)
    end function input_digest

end module fo_fpm_model
