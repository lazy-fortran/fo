program test_project_model
    !! In-memory oracle for the backend-neutral project model: selection by
    !! exact name/glob, -L/-LE, layers and default exclusion, dependency
    !! origin, test-to-target mapping, build plans over the link closure,
    !! identity inputs and scaling to thousands of targets and cases.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, int64
    use fx_string, only: string_t
    use fo_pattern, only: pattern_search, pattern_glob
    use fo_project_model, only: project_model_t, model_request_t, &
        model_selection_t, model_plan_t, model_attr_t, model_init, &
        model_add_target, model_add_layer, model_add_case, model_finalize, &
        model_select, model_plan, model_find_case, model_find_target, &
        model_find_artifact, model_find_layer, model_cache_key, &
        model_selected_names, TARGET_LIBRARY, TARGET_EXECUTABLE, &
        ORIGIN_DEPENDENCY, MAP_DECLARED, MAP_EXACT, MAP_UNKNOWN, SELECT_OK, &
        SELECT_EMPTY, SELECT_UNKNOWN_NAME, SELECT_NAMED_EXCLUDED, &
        SELECT_UNKNOWN_LAYER, SELECT_BAD_PATTERN, CASE_DEFAULT_EXCLUDED, &
        CASE_LABEL_EXCLUDED, CASE_SELECTED, CASE_DEPENDENCY
    use fo_project_model, only: model_unit_t, model_affected, CASE_NOT_AFFECTED
    implicit none

    integer :: n_pass, n_fail
    type(project_model_t) :: model

    n_pass = 0
    n_fail = 0

    call test_regex()
    call build_model(model, 'base', '')
    call test_identity()
    call test_names()
    call test_labels()
    call test_layers()
    call test_origin_and_mapping()
    call test_plan()
    call test_finalize_errors()
    call test_scale()
    call test_affected()

    write (output_unit, '(a,i0,a,i0,a)') 'project model: ', n_pass, ' pass, ', &
        n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//msg
        end if
    end subroutine assert

    subroutine test_regex()
        integer :: ierr

        call assert(pattern_search('^ab+d$', 'abbd', ierr), 're: ^ab+d$ abbd')
        call assert(.not. pattern_search('^ab+d$', 'ababd', ierr), 're: ^ab+d$ ababd')
        call assert(pattern_search('^(ab|cd)$', 'cd', ierr), 're: alternation group')
        call assert(.not. pattern_search('^(ab|cd)$', 'abd', ierr), 're: anchored alt')
        call assert(pattern_search('phys', 'astrophysics', ierr), 're: unanchored')
        call assert(pattern_search('[]x]', 'a]b', ierr), 're: ] first in class')
        call assert(.not. pattern_search('^[^a-c]+$', 'abd', ierr), 're: negated range')
        call assert(pattern_search('^[^a-c]+$', 'xyz', ierr), 're: negated class')
        call assert(pattern_search('^[+*/-]$', '-', ierr), 're: literal - last')
        call assert(pattern_search('^a\.b$', 'a.b', ierr), 're: escaped dot')
        call assert(.not. pattern_search('^a\.b$', 'axb', ierr), 're: escaped dot neg')
        call assert(pattern_search('^(a*)*$', '', ierr), 're: nested empty loop')
        call assert(pattern_search('^colou?r$', 'color', ierr), 're: optional')
        call assert(.not. pattern_search('(ab', 'ab', ierr), 're: unbalanced')
        call assert(ierr /= 0, 're: unbalanced ( is an error')
        call assert(pattern_glob('test_*_slow', 'test_io_slow'), 'glob: star')
        call assert(.not. pattern_glob('test_?', 'test_ab'), 'glob: question')
        call assert(pattern_glob('t[!x]st.f90', 'test.f90'), 'glob: negated set')
        call assert(.not. pattern_glob('t*', 'xt'), 'glob: whole-name match')
    end subroutine test_regex

    subroutine build_model(m, variant, preset)
        !! dep_lib <- core <- model <- (app, t_phys); core <- t_core;
        !! unrelated alone; dep_lib comes from a dependency. Layers:
        !! core_layer <- model_layer.
        type(project_model_t), intent(out) :: m
        character(len=*), intent(in) :: variant, preset
        integer :: t_core, t_phys, l_core, l_model, ierr
        character(len=:), allocatable :: message
        type(model_attr_t) :: attrs(1)
        type(string_t), allocatable :: labels(:), declared(:)

        call model_init(m, 'test', 'inputs-1', build_dir='/b', preset=preset, &
            configuration='Debug')
        call add_target(m, 'app', TARGET_EXECUTABLE, '', 'model', '/b/app')
        call add_target(m, 'model', TARGET_LIBRARY, 'src/m.f90', 'core', &
            '/b/libmodel.a')
        call add_target(m, 'core', TARGET_LIBRARY, 'src/c.f90', 'dep_lib', &
            '/b/libcore.a')
        call add_target(m, 'unrelated', TARGET_LIBRARY, '', '', '/b/libu.a')
        call add_target(m, 't_core', TARGET_EXECUTABLE, 'test/t_core.f90', 'core', &
            '/b/t_core', index=t_core)
        call add_target(m, 't_phys', TARGET_EXECUTABLE, '', 'model', '/b/t_phys', &
            index=t_phys)
        call add_target(m, 'dep_lib', TARGET_LIBRARY, '', '', '/b/_deps/libdep.a', &
            origin=ORIGIN_DEPENDENCY)
        call split_words('core_layer', labels)
        call split_words('model', declared)
        call model_add_layer(m, 'model_layer', uses=labels, targets=declared, &
            index=l_model)
        call split_words('core', declared)
        call model_add_layer(m, 'core_layer', targets=declared, index=l_core)
        call split_words('unit', labels)
        call model_add_case(m, 'test_core', command_target=t_core, labels=labels, &
            layer=l_core)
        attrs(1) = model_attr_t('FIXTURES_REQUIRED', 'db')
        call split_words('physics unit', labels)
        call model_add_case(m, 'test_phys', command_target=t_phys, labels=labels, &
            layer=l_model, attrs=attrs)
        call split_words('physics '//variant, labels)
        call model_add_case(m, 'test_phys_slow', command_target=t_phys, &
            labels=labels, layer=l_model)
        call split_words('regression', labels)
        call model_add_case(m, 'script_case', labels=labels)
        call model_add_case(m, 'preset_off', command_target=t_core, &
            default_excluded=.true.)
        call split_words('integration', labels)
        call split_words('app', declared)
        call model_add_case(m, 'declared_case', labels=labels, &
            declared_targets=declared)
        call model_add_case(m, 'dep_test', origin=ORIGIN_DEPENDENCY)
        call split_words('slow', m%default_exclude)
        m%complete = .true.
        call model_finalize(m, ierr, message)
        call assert(ierr == 0, 'finalize: '//message)
    end subroutine build_model

    subroutine test_identity()
        type(project_model_t) :: same, changed

        call assert(len_trim(model%identity) == 64, 'identity is a SHA-256 digest')
        call build_model(same, 'base', '')
        call assert(same%identity == model%identity, 'identity is deterministic')
        call build_model(changed, 'slow', '')
        call assert(changed%identity /= model%identity, 'identity covers labels')
        call build_model(changed, 'base', 'ci')
        call assert(changed%cache_key /= model%cache_key, 'cache key covers preset')
        call assert(changed%identity /= model%identity, 'identity covers preset')
        call assert(model_cache_key('cmake', '/b', '', 'Debug', 'd') /= &
            model_cache_key('cmake', '/b', '', 'Release', 'd'), &
            'cache key covers configuration')
        call assert(model_cache_key('cmake', '/b', '', 'Debug', 'd') /= &
            model_cache_key('cmake', '/c', '', 'Debug', 'd'), &
            'cache key covers build directory')
        call assert(model_cache_key('cmake', '/b', '', 'Debug', 'd') /= &
            model_cache_key('fpm', '/b', '', 'Debug', 'd'), 'cache key covers backend')
        call assert(model_cache_key('cmake', '/b', '', 'Debug', 'd') /= &
            model_cache_key('cmake', '/b', '', 'Debug', 'e'), &
            'cache key covers input digest')
        call assert(model_find_artifact(model, '/b/t_phys') == &
            model_find_target(model, 't_phys'), 'artifact lookup')
        call assert(size(model%cases) == model%n_cases, 'finalize trims storage')
    end subroutine test_identity

    subroutine test_names()
        type(model_selection_t) :: sel
        type(project_model_t) :: m
        type(string_t), allocatable :: names(:)

        call build_model(m, 'slow', '')
        call expect(m, '', SELECT_OK, &
            'declared_case script_case test_core test_phys', 'default selection')
        call run(m, '', sel)
        call assert(sel%disposition(model_find_case(m, 'test_phys_slow')) &
            == CASE_DEFAULT_EXCLUDED, 'slow label is default-excluded')
        call assert(sel%disposition(model_find_case(m, 'preset_off')) &
            == CASE_DEFAULT_EXCLUDED, 'adapter flag is default-excluded')
        call model_selected_names(m, sel, names)
        call assert(size(names) == 4, 'backend receives exact names')
        call expect(m, 'all', SELECT_OK, 'declared_case preset_off script_case test_core '// &
            'test_phys test_phys_slow', '--all')
        call expect(m, 'n=test_core', SELECT_OK, 'test_core', 'exact name')
        call expect(m, 'n=test_phys*', SELECT_OK, 'test_phys', &
            'glob keeps default exclusion')
        call expect(m, 'n=test_phys* all', SELECT_OK, 'test_phys test_phys_slow', &
            'glob with --all')
        call expect(m, 'n=test_phys_slow', SELECT_NAMED_EXCLUDED, '', &
            'exact slow name needs --all')
        call expect(m, 'n=test_phys_slow all', SELECT_OK, 'test_phys_slow', &
            'exact slow name with --all')
        call run(m, 'n=test_core n=nope', sel)
        call assert(sel%status == SELECT_UNKNOWN_NAME, 'unknown exact name')
        call assert(size(sel%unknown) == 1, 'one unknown name reported')
        if (size(sel%unknown) == 1) call assert(sel%unknown(1)%s == 'nope', &
            'unknown name is reported verbatim')
        call run(m, 'n=zz*', sel)
        call assert(sel%status == SELECT_EMPTY, 'unmatched glob: empty selection')
        call assert(size(sel%unmatched) == 1, 'unmatched glob reported')
    end subroutine test_names

    subroutine test_labels()
        type(project_model_t) :: m
        type(model_selection_t) :: sel

        call build_model(m, 'slow', '')
        call expect(m, 'L=physics', SELECT_OK, 'test_phys', '-L')
        call expect(m, 'L=slow', SELECT_OK, 'test_phys_slow', &
            '-L on a default-excluded label lifts it')
        call expect(m, 'L=physics L=unit', SELECT_OK, 'test_phys', &
            '-L repeated is AND')
        call expect(m, 'L=^unit$', SELECT_OK, 'test_core test_phys', &
            '-L anchored regex')
        call expect(m, 'E=unit', SELECT_OK, 'declared_case script_case', '-LE')
        call expect(m, 'E=physics E=unit', SELECT_OK, &
            'declared_case script_case test_core', '-LE repeated is AND')
        call expect(m, 'E=unit|regression|integration', SELECT_EMPTY, '', &
            '-LE alternation')
        call expect(m, 'L=physics E=phys', SELECT_EMPTY, '', '-LE wins over -L')
        call expect(m, 'E=slow all', SELECT_OK, &
            'declared_case preset_off script_case test_core test_phys', &
            '-LE applies under --all')
        call run(m, 'n=test_phys E=unit', sel)
        call assert(sel%status == SELECT_EMPTY, 'exact name filtered by -LE')
        call assert(sel%disposition(model_find_case(m, 'test_phys')) == &
            CASE_LABEL_EXCLUDED, '-LE disposition recorded')
        call expect(m, 'L=(', SELECT_BAD_PATTERN, '', 'bad regex')
    end subroutine test_labels

    subroutine test_layers()
        type(project_model_t) :: m

        call build_model(m, 'slow', '')
        call expect(m, 'y=model_layer', SELECT_OK, 'test_phys', '--layer')
        call expect(m, 'y=model_layer y=core_layer', SELECT_OK, &
            'test_core test_phys', '--layer union')
        call expect(m, 'y=nope', SELECT_UNKNOWN_LAYER, '', 'unknown layer')
        call assert(m%layers(model_find_layer(m, 'model_layer'))%uses(1) == &
            model_find_layer(m, 'core_layer'), 'layer uses resolved')
    end subroutine test_layers

    subroutine test_origin_and_mapping()
        type(model_selection_t) :: sel

        call run(model, 'all', sel)
        call assert(sel%disposition(model_find_case(model, 'dep_test')) == &
            CASE_DEPENDENCY, 'dependency test never selected')
        call expect(model, 'n=dep_*', SELECT_EMPTY, '', 'glob skips dependency tests')
        call expect(model, 'n=dep_test', SELECT_UNKNOWN_NAME, '', &
            'exact dependency test name is not selectable')
        call assert(model%cases(model_find_case(model, 'test_core'))%mapping == &
            MAP_EXACT, 'command target maps exactly')
        call assert(model%cases(model_find_case(model, 'declared_case'))%mapping == &
            MAP_DECLARED, 'declared targets map as declared')
        call assert(model%cases(model_find_case(model, 'script_case'))%mapping == &
            MAP_UNKNOWN, 'script command maps as unknown')
    end subroutine test_origin_and_mapping

    subroutine test_plan()
        type(project_model_t) :: partial

        call expect_plan(model, 'test_core', .false., 'core t_core dep_lib', &
            'plan: closure includes dependency targets')
        call expect_plan(model, 'test_phys test_core', .false., &
            'model core t_core t_phys dep_lib', 'plan: transitive link closure')
        call expect_plan(model, '', .false., '', 'plan: no cases, no targets')
        call expect_plan(model, 'declared_case', .false., 'app model core dep_lib', &
            'plan: declared targets')
        call expect_plan(model, 'script_case', .true., &
            'app model core unrelated t_core t_phys dep_lib', &
            'plan: unknown mapping widens to all')
        call build_model(partial, 'base', '')
        partial%complete = .false.
        call expect_plan(partial, 'test_core', .true., &
            'app model core unrelated t_core t_phys dep_lib', &
            'plan: incomplete model widens')
    end subroutine test_plan

    subroutine test_finalize_errors()
        type(project_model_t) :: m
        integer :: ierr
        character(len=:), allocatable :: message
        type(string_t), allocatable :: words(:)

        call model_init(m, 'test', '')
        call add_target(m, 'exe', TARGET_EXECUTABLE, '', 'missing', '')
        call model_finalize(m, ierr, message)
        call assert(ierr /= 0, 'finalize: dangling link dependency rejected')
        call model_init(m, 'test', '')
        call model_add_case(m, 'dup')
        call model_add_case(m, 'dup')
        call model_finalize(m, ierr, message)
        call assert(ierr /= 0, 'finalize: duplicate case rejected')
        call model_init(m, 'test', '')
        call split_words('b', words)
        call model_add_layer(m, 'a', uses=words)
        call model_finalize(m, ierr, message)
        call assert(ierr /= 0, 'finalize: dangling layer use rejected')
        call model_init(m, 'test', '')
        call add_target(m, 'ext', TARGET_LIBRARY, '', '', '', &
            origin=ORIGIN_DEPENDENCY)
        call split_words('ext', words)
        call model_add_layer(m, 'a', targets=words)
        call model_finalize(m, ierr, message)
        call assert(ierr /= 0, 'finalize: dependency target in a layer rejected')
        call model_init(m, 'test', '')
        call model_add_case(m, 'c', mapping=MAP_EXACT)
        call model_finalize(m, ierr, message)
        call assert(ierr /= 0, 'finalize: exact mapping without target rejected')
    end subroutine test_finalize_errors

    subroutine test_scale()
        !! 10000 targets and 5000 cases: hashed lookups and a linear closure
        !! keep load, selection and planning far below a second.
        integer, parameter :: n = 5000
        type(project_model_t) :: m
        type(model_selection_t) :: sel
        type(model_plan_t) :: plan
        type(string_t), allocatable :: deps(:), slow_labels(:), unit_labels(:)
        integer(int64) :: start, finish, rate
        integer :: i, exe, ierr
        character(len=16) :: id
        character(len=:), allocatable :: message
        real :: seconds

        call system_clock(start, rate)
        call model_init(m, 'test', 'scale')
        call split_words('slow', slow_labels)
        call split_words('unit', unit_labels)
        allocate(deps(1))
        do i = 1, n
            write (id, '(i0)') i
            ! deps(1) still names lib_<i-1> from the previous iteration.
            if (i > 1) then
                call model_add_target(m, 'lib_'//trim(id), TARGET_LIBRARY, &
                    link_deps=deps, artifact='/b/lib_'//trim(id)//'.a')
            else
                call model_add_target(m, 'lib_1', TARGET_LIBRARY, artifact='/b/lib_1.a')
            end if
            deps(1)%s = 'lib_'//trim(id)
            call model_add_target(m, 'exe_'//trim(id), TARGET_EXECUTABLE, &
                link_deps=deps, artifact='/b/exe_'//trim(id), index=exe)
            if (mod(i, 2) == 0) then
                call model_add_case(m, 'case_'//trim(id), command_target=exe, &
                    labels=slow_labels)
            else
                call model_add_case(m, 'case_'//trim(id), command_target=exe, &
                    labels=unit_labels)
            end if
        end do
        call split_words('slow', m%default_exclude)
        m%complete = .true.
        call model_finalize(m, ierr, message)
        call assert(ierr == 0, 'scale: finalize '//message)
        call run(m, 'n=case_4999', sel)
        call assert(size(sel%cases) == 1, 'scale: exact name')
        call run(m, 'n=case_1* L=unit', sel)
        call assert(size(sel%cases) == 556, 'scale: glob and label')
        call run(m, 'all', sel)
        call assert(size(sel%cases) == n, 'scale: all cases')
        call model_plan(m, sel%cases, plan)
        call assert(size(plan%targets) == 2*n, 'scale: plan closes all targets')
        call assert(model_find_artifact(m, '/b/exe_2500') == &
            model_find_target(m, 'exe_2500'), 'scale: artifact lookup')
        call system_clock(finish)
        seconds = real(finish - start)/real(rate)
        call assert(seconds < 1.0, 'scale: load, select and plan under a second')
    end subroutine test_scale

    subroutine test_affected()
        !! model_affected: module closure, link closure for sources no unit
        !! uses as a module (external procedures), declared inputs (file and
        !! directory), cases without a command target, widening, ignored
        !! files, and unselected cases staying unselected.
        type(project_model_t) :: m
        type(model_unit_t) :: units(7)
        type(model_attr_t) :: required(1), input(1)
        integer :: ierr, t1, t2, t3
        character(len=:), allocatable :: message

        call model_init(m, 'unit', 'digest')
        call add_target(m, 'lib_a', TARGET_LIBRARY, &
            '/p/src/a.f90 /p/src/ext.f90 /p/src/mixed.f90', '', '')
        call add_target(m, 'lib_b', TARGET_LIBRARY, '/p/src/b.f90', 'lib_a', '')
        call add_target(m, 't1', TARGET_EXECUTABLE, '/p/test/t1.f90', 'lib_a', &
            '/p/build/t1', index=t1)
        call add_target(m, 't2', TARGET_EXECUTABLE, '/p/test/t2.f90', 'lib_b', &
            '/p/build/t2', index=t2)
        call add_target(m, 't3', TARGET_EXECUTABLE, '/p/test/t3.f90', '', &
            '/p/build/t3', index=t3)
        call model_add_case(m, 'c1', command_target=t1)
        call model_add_case(m, 'c2', command_target=t2)
        required(1) = model_attr_t('REQUIRED_FILES', '/p/ref/x.dat;/p/ref/y.dat')
        call model_add_case(m, 'c3', command_target=t3, attrs=required)
        input(1) = model_attr_t('input', 'data')
        call model_add_case(m, 'c4', attrs=input)
        call model_finalize(m, ierr, message)
        call assert(ierr == 0, 'affected model finalizes: '//message)
        call set_unit(units(1), '/p/src/a.f90', 'ma', '')
        call set_unit(units(2), '/p/src/ext.f90', 'ext', '')
        call set_unit(units(3), '/p/src/b.f90', 'mb', 'ma mm')
        call set_unit(units(4), '/p/test/t1.f90', '', 'ma')
        call set_unit(units(5), '/p/test/t2.f90', '', 'mb')
        call set_unit(units(6), '/p/test/t3.f90', '', '')
        ! A used module plus external procedures in one file: the procedures
        ! link into every case linking lib_a, also one that never uses mm.
        call set_unit(units(7), '/p/src/mixed.f90', 'mm', '')
        units(7)%other_units = .true.

        call expect_affected(m, units, '', 'src/b.f90', 'c2 c4', .false., 'used module')
        call expect_affected(m, units, '', 'src/a.f90', 'c1 c2 c4', .false., &
            'base module')
        call expect_affected(m, units, '', 'src/ext.f90', 'c1 c2 c4', .false., &
            'external procedure in a linked library')
        call expect_affected(m, units, '', 'src/mixed.f90', 'c1 c2 c4', .false., &
            'used module with external procedures in a linked library')
        call expect_affected(m, units, '', 'test/t3.f90', 'c3 c4', .false., &
            'test program')
        call expect_affected(m, units, '', 'ref/y.dat', 'c3 c4', .false., &
            'REQUIRED_FILES')
        call expect_affected(m, units, '', 'data/sub/z.txt', 'c4', .false., &
            'input directory')
        call expect_affected(m, units, '', 'README.md docs/x.txt', '', .false., &
            'unrelated files')
        call expect_affected(m, units, '', 'src/new.f90', 'c1 c2 c3 c4', .true., &
            'source of no target')
        call expect_affected(m, units, '', 'sub/CMakeLists.txt', &
            'c1 c2 c3 c4', .true., &
            'build description')
        call expect_affected(m, units, 'n=c1', 'src/b.f90', '', .false., &
            'unselected case stays unselected')
    end subroutine test_affected

    subroutine set_unit(u, path, provides, uses)
        type(model_unit_t), intent(out) :: u
        character(len=*), intent(in) :: path, provides, uses

        u%path = path
        call split_words(provides, u%provides)
        call split_words(uses, u%uses)
    end subroutine set_unit

    subroutine expect_affected(m, units, spec, changed, names, widened, what)
        type(project_model_t), intent(in) :: m
        type(model_unit_t), intent(in) :: units(:)
        character(len=*), intent(in) :: spec, changed, names, what
        logical, intent(in) :: widened
        type(model_selection_t) :: sel
        type(string_t), allocatable :: paths(:), reasons(:)
        type(string_t) :: roots(1)
        character(len=:), allocatable :: got
        integer :: k

        call run(m, spec, sel)
        call split_words(changed, paths)
        roots(1)%s = '/p'
        call model_affected(m, units, roots, paths, sel, reasons)
        got = ''
        do k = 1, size(sel%cases)
            if (k > 1) got = got//' '
            got = got//m%cases(sel%cases(k))%name
            call assert(sel%reasons(sel%cases(k))%s(1:9) == 'affected:', &
                what//': affected reason')
        end do
        call assert(got == names, what//': got "'//got//'"')
        call assert((size(reasons) > 0) .eqv. widened, what//': widening')
        call assert(size(sel%cases) == count(sel%disposition == CASE_SELECTED), &
            what//': dispositions agree')
        call assert((len(names) == 0) .eqv. (sel%status == SELECT_EMPTY), &
            what//': empty status')
        if (.not. widened) call assert(count(sel%disposition == CASE_NOT_AFFECTED) &
            > 0, what//': unaffected cases are marked')
    end subroutine expect_affected

    subroutine expect(m, spec, status, names, what)
        type(project_model_t), intent(in) :: m
        character(len=*), intent(in) :: spec, names, what
        integer, intent(in) :: status
        type(model_selection_t) :: sel
        character(len=:), allocatable :: got
        integer :: k

        call run(m, spec, sel)
        got = ''
        do k = 1, size(sel%cases)
            if (k > 1) got = got//' '
            got = got//m%cases(sel%cases(k))%name
        end do
        call assert(sel%status == status, what//': status')
        call assert(got == names, what//': got "'//got//'"')
    end subroutine expect

    subroutine run(m, spec, sel)
        !! spec: space-separated n=NAME L=RE E=RE y=LAYER all.
        type(project_model_t), intent(in) :: m
        character(len=*), intent(in) :: spec
        type(model_selection_t), intent(out) :: sel
        type(model_request_t) :: request
        type(string_t), allocatable :: words(:)
        integer :: k

        allocate(request%names(0), request%include_labels(0), &
            request%exclude_labels(0), request%layers(0))
        call split_words(spec, words)
        do k = 1, size(words)
            associate (w => words(k)%s)
                if (w == 'all') then
                    request%all = .true.
                else if (w(1:2) == 'n=') then
                    call push_word(request%names, w(3:))
                else if (w(1:2) == 'L=') then
                    call push_word(request%include_labels, w(3:))
                else if (w(1:2) == 'E=') then
                    call push_word(request%exclude_labels, w(3:))
                else if (w(1:2) == 'y=') then
                    call push_word(request%layers, w(3:))
                end if
            end associate
        end do
        call model_select(m, request, sel)
        call assert(size(sel%cases) == count(sel%disposition == CASE_SELECTED), &
            'selected indices agree with dispositions')
    end subroutine run

    subroutine expect_plan(m, case_names, widened, targets, what)
        type(project_model_t), intent(in) :: m
        character(len=*), intent(in) :: case_names, targets, what
        logical, intent(in) :: widened
        type(model_plan_t) :: plan
        type(string_t), allocatable :: words(:)
        integer, allocatable :: indices(:)
        character(len=:), allocatable :: got
        integer :: k

        call split_words(case_names, words)
        allocate(indices(size(words)))
        do k = 1, size(words)
            indices(k) = model_find_case(m, words(k)%s)
        end do
        call model_plan(m, indices, plan)
        got = ''
        do k = 1, size(plan%targets)
            if (k > 1) got = got//' '
            got = got//m%targets(plan%targets(k))%name
        end do
        call assert(plan%widened .eqv. widened, what//': widened flag')
        call assert(widened .eqv. size(plan%reasons) > 0, what//': widening reasons')
        call assert(got == targets, what//': got "'//got//'"')
    end subroutine expect_plan

    subroutine add_target(m, name, kind, sources, deps, artifact, origin, index)
        type(project_model_t), intent(inout) :: m
        character(len=*), intent(in) :: name, sources, deps, artifact
        integer, intent(in) :: kind
        integer, intent(in), optional :: origin
        integer, intent(out), optional :: index
        type(string_t), allocatable :: source_list(:), dep_list(:)

        call split_words(sources, source_list)
        call split_words(deps, dep_list)
        call model_add_target(m, name, kind, sources=source_list, &
            link_deps=dep_list, artifact=artifact, origin=origin, index=index)
    end subroutine add_target

    subroutine split_words(words, items)
        !! Space-separated words as a string list.
        character(len=*), intent(in) :: words
        type(string_t), allocatable, intent(out) :: items(:)
        integer :: start, k

        allocate(items(0))
        start = 1
        do k = 1, len(words) + 1
            if (k <= len(words)) then
                if (words(k:k) /= ' ') cycle
            end if
            if (k > start) call push_word(items, words(start:k - 1))
            start = k + 1
        end do
    end subroutine split_words

    subroutine push_word(items, word)
        type(string_t), allocatable, intent(inout) :: items(:)
        character(len=*), intent(in) :: word
        type(string_t), allocatable :: grown(:)
        integer :: n

        n = size(items)
        allocate(grown(n + 1))
        grown(1:n) = items
        grown(n + 1)%s = word
        call move_alloc(grown, items)
    end subroutine push_word

end program test_project_model
