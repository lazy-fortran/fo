module fo_fpm_model
    !! fpm adapter for the backend-neutral project model (#167).
    !!
    !! Baseline: the library, the app and test executables and the dependency
    !! packages as targets; every discovered test as a case whose command
    !! target is the executable that runs it (the dispatcher for marked
    !! tests); the `slow` label from is_slow_test, excluded by default.
    !! Case names are the `fo test` inventory (same eligibility, naming and
    !! dispatcher rules as the native test runner).
    !!
    !! [extra.fo] mapping (parsed by fo_fpm_config):
    !!  * layers: the members of a layer are the scanned project sources (library,
    !!    app and test directories) matched by its `source` globs (fo_pattern
    !!    glob, matched against the project-relative path; `*` also crosses `/`)
    !!    or defining one of its `modules`. A glob or module matching no source
    !!    is an error. A source may belong to several layers. Layer order is the
    !!    declaration order; `uses` becomes the model layer uses.
    !!  * layer rank: 0 without `uses`, else 1 + the highest rank it uses. The
    !!    layer of a case is the highest-rank layer among the sources in the
    !!    module-use closure of the case source (the source included); ties
    !!    go to the layer declared first. A case reaching no layered source
    !!    has layer 0.
    !!  * test groups: the group name becomes a label of every case whose name
    !!        !!    matches (by name or by project-relative source path) one of its
    !!    `tests` globs. Declared group `inputs` are
    !!    recorded on those cases as attributes with key `input`.
    !!  * test-defaults: an explicit `exclude` list (also an empty one)
    !!    replaces the default [slow] exclusion. The `slow` label itself is
    !!    still attached to slow-named cases.
    !!  * sources of no layer are returned in `unlayered` (project-relative)
    !!    when layers are declared, never dropped silently.
    use fx_string, only: string_t, to_lower
    use fx_hash, only: sha256_string
    use fo_fpm_config, only: fpm_config_t, fpm_string_t, fpm_config_parse, &
        fpm_config_allocate
    use fo_pattern, only: pattern_glob
    use fo_scan, only: scan_unit_t, scan_dir, is_slow_test, MAX_PATH
    use fo_util, only: read_text_file_alloc
    use fo_build_tree, only: native_output_dir
    use fo_dep_resolve, only: resolved_src_t, resolve_dep_srcs, &
        resolve_dev_dep_srcs, MAX_RESOLVED
    use fo_gfortran_build, only: scan_project_test_units, scan_project_app_units, &
        gfortran_app_source_name, gfortran_test_source_name, &
        test_source_eligible, scanned_dispatcher_available, &
        unit_has_dispatcher_marker
    use fo_project_model, only: project_model_t, model_attr_t, model_init, &
        model_add_target, model_add_layer, model_add_case, model_finalize, &
        model_find_target, model_find_case, &
        TARGET_LIBRARY, TARGET_EXECUTABLE, ORIGIN_DEPENDENCY
    implicit none
    private

    public :: fpm_model_load

    type :: layering_t
        !! Resolved [extra.fo.layers] over the scanned project sources.
        integer :: n_units = 0
        type(string_t), allocatable :: files(:)
        !! Project-relative path of each unit.
        type(string_t), allocatable :: relative(:)
        logical, allocatable :: member(:, :)
        !! Layer rank (0 = uses nothing).
        integer, allocatable :: rank(:)
        !! Providers of each unit's used modules, CSR over units.
        integer, allocatable :: first(:), provider(:)
    end type layering_t

contains

    subroutine fpm_model_load(project_dir, model, ierr, message, unlayered, &
            scanned)
        !! Load the model of the fpm project in project_dir. ierr /= 0 with a
        !! message when the manifest, a source scan, a layer or the model is
        !! invalid. With layers declared, unlayered lists the project sources
        !! (relative paths) that belong to no layer. scanned lists every
        !! scanned project source (library, executables, tests, also helpers
        !! that are no target source), spelled like the target sources.
        character(len=*), intent(in) :: project_dir
        type(project_model_t), intent(out) :: model
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        type(string_t), allocatable, intent(out), optional :: unlayered(:)
        type(string_t), allocatable, intent(out), optional :: scanned(:)

        type(fpm_config_t), allocatable :: config
        type(scan_unit_t), allocatable :: tests(:), apps(:), lib(:)
        type(resolved_src_t), allocatable :: deps(:), devs(:)
        type(string_t), allocatable :: app_deps(:), test_deps(:), lib_deps(:)
        type(string_t), allocatable :: labels(:)
        type(model_attr_t), allocatable :: attrs(:)
        type(layering_t) :: layering
        type(scan_unit_t), allocatable :: all_units(:)
        character(len=:), allocatable :: bin_dir, name
        character(len=MAX_PATH) :: case_name
        integer :: n_tests, n_apps, n_lib, n_deps, n_devs, n_unresolved
        integer :: i, dispatcher, lib_index, target_index, layer
        logical :: has_dispatcher, has_library, is_dispatcher

        message = ''
        if (present(unlayered)) allocate (unlayered(0))
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

        if (present(scanned)) then
            allocate (scanned(n_lib + n_apps + n_tests))
            do i = 1, n_lib
                scanned(i)%s = trim(lib(i)%filename)
            end do
            do i = 1, n_apps
                scanned(n_lib + i)%s = trim(apps(i)%filename)
            end do
            do i = 1, n_tests
                scanned(n_lib + n_apps + i)%s = trim(tests(i)%filename)
            end do
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

        if (config%n_layers > 0) then
            allocate (all_units(n_lib + n_apps + n_tests))
            all_units(1:n_lib) = lib(1:n_lib)
            all_units(n_lib + 1:n_lib + n_apps) = apps(1:n_apps)
            all_units(n_lib + n_apps + 1:) = tests(1:n_tests)
            call build_layering(project_dir, config, all_units, layering, &
                ierr, message)
            if (ierr /= 0) return
            do i = 1, config%n_layers
                call model_add_layer(model, config%layers(i)%name, &
                    uses=strings_of(config%layers(i)%uses), &
                    sources=member_files(layering, i))
            end do
            if (present(unlayered)) unlayered = unlayered_files(layering)
        end if

        ! Cases in inventory order. The dispatcher is an explicit-only case:
        ! it runs when named exactly and is never selected otherwise.
        do i = 1, n_tests
            if (.not. eligible(i)) cycle
            case_name = gfortran_test_source_name(config, config%test_dir, &
                tests(i)%filename)
            if (len_trim(case_name) == 0) cycle
            name = trim(case_name)
            if (model_find_case(model, name) > 0) cycle
            is_dispatcher = .false.
            if (has_dispatcher) is_dispatcher = name == trim(config%dispatcher)
            if (marked(i)) then
                target_index = dispatcher
            else
                target_index = model_find_target(model, name)
            end if
            call case_labels(config, name, relative_path(project_dir, &
                trim(tests(i)%filename)), is_slow_test(name), labels, attrs)
            layer = 0
            if (config%n_layers > 0) layer = unit_layer(layering, n_lib + n_apps + i)
            call model_add_case(model, name, command_target=target_index, &
                labels=labels, layer=layer, attrs=attrs, &
                explicit_only=is_dispatcher)
        end do

        if (config%default_exclude_seen) then
            model%default_exclude = strings_of(config%default_exclude)
        else
            model%default_exclude = [string_t('slow')]
        end if
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

    function strings_of(items) result(out)
        type(fpm_string_t), intent(in) :: items(:)
        type(string_t), allocatable :: out(:)
        integer :: i

        allocate (out(size(items)))
        do i = 1, size(items)
            out(i)%s = items(i)%s
        end do
    end function strings_of

    subroutine case_labels(config, name, path, slow, labels, attrs)
        !! Labels of a case: `slow`, then every test group whose globs match
        !! the case name; attrs record the declared inputs of those groups.
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: name, path
        logical, intent(in) :: slow
        type(string_t), allocatable, intent(out) :: labels(:)
        type(model_attr_t), allocatable, intent(out) :: attrs(:)
        integer :: g, k, m
        logical :: hit

        allocate (labels(0), attrs(0))
        if (slow) labels = [string_t('slow')]
        do g = 1, config%n_test_groups
            hit = .false.
            do k = 1, size(config%test_groups(g)%tests)
                if (pattern_glob(config%test_groups(g)%tests(k)%s, name) .or. &
                    pattern_glob(config%test_groups(g)%tests(k)%s, path)) then
                    hit = .true.
                    exit
                end if
            end do
            if (.not. hit) cycle
            if (.not. has_name(labels, config%test_groups(g)%name)) &
                labels = [labels, string_t(config%test_groups(g)%name)]
            do m = 1, size(config%test_groups(g)%inputs)
                if (has_input(attrs, config%test_groups(g)%inputs(m)%s)) cycle
                attrs = [attrs, model_attr_t('input', &
                    config%test_groups(g)%inputs(m)%s)]
            end do
        end do
    end subroutine case_labels

    logical function has_name(names, name) result(found)
        type(string_t), intent(in) :: names(:)
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        do i = 1, size(names)
            if (names(i)%s == name) found = .true.
        end do
    end function has_name

    logical function has_input(attrs, path) result(found)
        type(model_attr_t), intent(in) :: attrs(:)
        character(len=*), intent(in) :: path
        integer :: i

        found = .false.
        do i = 1, size(attrs)
            if (attrs(i)%value == path) found = .true.
        end do
    end function has_input

    subroutine build_layering(project_dir, config, units, lay, ierr, message)
        !! Resolve every layer of config over units: membership, rank and the
        !! module providers each unit uses.
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(in) :: config
        type(scan_unit_t), intent(in) :: units(:)
        type(layering_t), intent(out) :: lay
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        integer, allocatable :: order(:), mod_unit(:)
        type(string_t), allocatable :: mod_name(:)
        integer :: n, nl, u, l, k, j, a, changed, p, pos, n_mod
        logical :: found

        ierr = 0
        message = ''
        n = size(units)
        nl = config%n_layers
        lay%n_units = n
        allocate (lay%files(n), lay%relative(n), lay%member(n, nl), lay%rank(nl))
        lay%member = .false.
        do u = 1, n
            lay%files(u)%s = trim(units(u)%filename)
            lay%relative(u)%s = relative_path(project_dir, lay%files(u)%s)
        end do

        ! Sorted (module name, unit) table for provider lookup.
        n_mod = 0
        do u = 1, n
            if (len_trim(units(u)%module_name) > 0) n_mod = n_mod + 1
            if (allocated(units(u)%additional_modules)) &
                n_mod = n_mod + size(units(u)%additional_modules)
        end do
        allocate (mod_name(n_mod), mod_unit(n_mod), order(n_mod))
        n_mod = 0
        do u = 1, n
            if (len_trim(units(u)%module_name) > 0) then
                n_mod = n_mod + 1
                mod_name(n_mod)%s = to_lower(trim(units(u)%module_name))
                mod_unit(n_mod) = u
            end if
            if (.not. allocated(units(u)%additional_modules)) cycle
            do k = 1, size(units(u)%additional_modules)
                n_mod = n_mod + 1
                mod_name(n_mod)%s = to_lower(trim(units(u)%additional_modules(k)))
                mod_unit(n_mod) = u
            end do
        end do
        call sort_order(mod_name, order)

        do l = 1, nl
            associate (ly => config%layers(l))
                do k = 1, size(ly%source)
                    found = .false.
                    do u = 1, n
                        if (.not. pattern_glob(ly%source(k)%s, lay%relative(u)%s)) &
                            cycle
                        lay%member(u, l) = .true.
                        found = .true.
                    end do
                    if (.not. found) then
                        ierr = 1
                        message = 'layer '//ly%name//': source glob '// &
                            ly%source(k)%s//' matches no project source'
                        return
                    end if
                end do
                do k = 1, size(ly%modules)
                    p = find_sorted(mod_name, order, to_lower(ly%modules(k)%s))
                    if (p == 0) then
                        ierr = 1
                        message = 'layer '//ly%name//': module '// &
                            ly%modules(k)%s//' is defined by no project source'
                        return
                    end if
                    do while (p <= n_mod)
                        if (mod_name(order(p))%s /= to_lower(ly%modules(k)%s)) exit
                        lay%member(mod_unit(order(p)), l) = .true.
                        p = p + 1
                    end do
                end do
            end associate
        end do

        ! Rank by relaxation; layers are acyclic (checked by the parser).
        lay%rank = 0
        do j = 1, nl + 1
            changed = 0
            do l = 1, nl
                do k = 1, size(config%layers(l)%uses)
                    do a = 1, nl
                        if (config%layers(a)%name /= config%layers(l)%uses(k)%s) &
                            cycle
                        if (lay%rank(a) + 1 > lay%rank(l)) then
                            lay%rank(l) = lay%rank(a) + 1
                            changed = 1
                        end if
                    end do
                end do
            end do
            if (changed == 0) exit
        end do

        ! Providers of the modules each unit uses (project sources only).
        allocate (lay%first(n + 1))
        allocate (lay%provider(max(16, 4*n)))
        pos = 1
        do u = 1, n
            lay%first(u) = pos
            do k = 1, units(u)%n_deps
                p = find_sorted(mod_name, order, to_lower(trim(units(u)%deps(k))))
                if (p == 0) cycle
                do while (p <= n_mod)
                    if (mod_name(order(p))%s /= to_lower(trim(units(u)%deps(k)))) &
                        exit
                    if (mod_unit(order(p)) /= u) then
                        if (pos > size(lay%provider)) call grow(lay%provider)
                        lay%provider(pos) = mod_unit(order(p))
                        pos = pos + 1
                    end if
                    p = p + 1
                end do
            end do
        end do
        lay%first(n + 1) = pos
    end subroutine build_layering

    subroutine grow(a)
        integer, allocatable, intent(inout) :: a(:)
        integer, allocatable :: bigger(:)

        allocate (bigger(2*size(a)))
        bigger(1:size(a)) = a
        call move_alloc(bigger, a)
    end subroutine grow

    function relative_path(project_dir, file) result(rel)
        character(len=*), intent(in) :: project_dir, file
        character(len=:), allocatable :: rel, prefix

        prefix = trim(project_dir)//'/'
        rel = file
        if (len(file) > len(prefix)) then
            if (file(1:len(prefix)) == prefix) rel = file(len(prefix) + 1:)
        end if
    end function relative_path

    subroutine sort_order(keys, order)
        !! Stable merge sort of the indices of keys by string value.
        type(string_t), intent(in) :: keys(:)
        integer, intent(out) :: order(:)
        integer, allocatable :: work(:)
        integer :: n, width, lo, mid, hi, i, j, k

        n = size(keys)
        do i = 1, n
            order(i) = i
        end do
        allocate (work(n))
        width = 1
        do while (width < n)
            lo = 1
            do while (lo <= n)
                mid = min(lo + width, n + 1)
                hi = min(lo + 2*width, n + 1)
                i = lo
                j = mid
                k = lo
                do while (i < mid .or. j < hi)
                    if (j >= hi) then
                        work(k) = order(i)
                        i = i + 1
                    else if (i >= mid) then
                        work(k) = order(j)
                        j = j + 1
                    else if (keys(order(j))%s < keys(order(i))%s) then
                        work(k) = order(j)
                        j = j + 1
                    else
                        work(k) = order(i)
                        i = i + 1
                    end if
                    k = k + 1
                end do
                lo = lo + 2*width
            end do
            order(1:n) = work(1:n)
            width = 2*width
        end do
    end subroutine sort_order

    integer function find_sorted(keys, order, key) result(first)
        !! Position in order of the first entry equal to key, else 0.
        type(string_t), intent(in) :: keys(:)
        integer, intent(in) :: order(:)
        character(len=*), intent(in) :: key
        integer :: lo, hi, mid

        first = 0
        lo = 1
        hi = size(order) + 1
        do while (lo < hi)
            mid = (lo + hi)/2
            if (keys(order(mid))%s < key) then
                lo = mid + 1
            else
                hi = mid
            end if
        end do
        if (lo > size(order)) return
        if (keys(order(lo))%s == key) first = lo
    end function find_sorted

    function member_files(lay, l) result(files)
        type(layering_t), intent(in) :: lay
        integer, intent(in) :: l
        type(string_t), allocatable :: files(:)
        integer :: u

        allocate (files(0))
        do u = 1, lay%n_units
            if (lay%member(u, l)) files = [files, lay%files(u)]
        end do
    end function member_files

    function unlayered_files(lay) result(files)
        type(layering_t), intent(in) :: lay
        type(string_t), allocatable :: files(:)
        integer :: u

        allocate (files(0))
        do u = 1, lay%n_units
            if (.not. any(lay%member(u, :))) files = [files, lay%relative(u)]
        end do
    end function unlayered_files

    integer function unit_layer(lay, start) result(best)
        !! Highest-rank layer (ties: first declared) among the units in the
        !! module-use closure of unit start, start included; 0 if none.
        type(layering_t), intent(in) :: lay
        integer, intent(in) :: start
        logical, allocatable :: seen(:)
        integer, allocatable :: stack(:)
        integer :: top, u, l, k

        best = 0
        allocate (seen(lay%n_units), stack(lay%n_units))
        seen = .false.
        seen(start) = .true.
        stack(1) = start
        top = 1
        do while (top > 0)
            u = stack(top)
            top = top - 1
            do l = 1, size(lay%rank)
                if (.not. lay%member(u, l)) cycle
                if (best == 0) then
                    best = l
                else if (lay%rank(l) > lay%rank(best)) then
                    best = l
                end if
            end do
            do k = lay%first(u), lay%first(u + 1) - 1
                if (seen(lay%provider(k))) cycle
                seen(lay%provider(k)) = .true.
                top = top + 1
                stack(top) = lay%provider(k)
            end do
        end do
    end function unit_layer

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
