program test_fpm_project_model
    !! fpm adapter baseline: a generated project with fast, slow, dispatched
    !! and module-only dispatched tests, an app, a library and a dependency.
    !! Case names must equal the native `fo test` inventory, which is
    !! computed here by the production selection function.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fx_dag, only: dag_t, MAX_NODES
    use fo_util, only: temporary_root
    use fo_fs, only: fs_make_dir, fs_write_text, fs_remove_tree
    use fo_scan, only: MAX_PATH
    use fo_check, only: fo_changed_modules
    use fo_gfortran_build, only: gfortran_selected_test_names
    use fx_string, only: string_t
    use fo_fpm_model, only: fpm_model_load
    use fo_project_model, only: model_find_layer
    use fo_project_model, only: project_model_t, model_request_t, &
        model_selection_t, model_plan_t, model_select, model_plan, &
        model_selected_names, model_find_case, model_find_target, &
        model_find_artifact, TARGET_LIBRARY, TARGET_EXECUTABLE, &
        ORIGIN_DEPENDENCY, MAP_EXACT, SELECT_OK, SELECT_NAMED_EXCLUDED, SELECT_EMPTY
    implicit none

    integer :: n_pass, n_fail
    character(len=512) :: root

    n_pass = 0
    n_fail = 0
    root = trim(temporary_root())//'/fo_fpm_project_model_probe'
    call fs_remove_tree(root)
    call write_project()
    call test_model()
    call test_inventory()
    call test_layers()
    call fs_remove_tree(root)

    write (output_unit, '(a,i0,a,i0,a)') 'fpm project model: ', n_pass, &
        ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine check(cond, name)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', name
        end if
    end subroutine check

    subroutine put(path, text)
        character(len=*), intent(in) :: path, text

        call fs_write_text(trim(root)//'/'//path, text//new_line('a'))
    end subroutine put

    subroutine write_project()
        call fs_make_dir(trim(root))
        call fs_make_dir(trim(root)//'/src')
        call fs_make_dir(trim(root)//'/app')
        call fs_make_dir(trim(root)//'/test')
        call put('fpm.toml', 'name = "m5probe"'//new_line('a')// &
            '[extra.fo]'//new_line('a')//'dispatcher = "m5_suite"')
        call put('src/m5lib.f90', 'module m5lib'//new_line('a')// &
            'implicit none'//new_line('a')//'end module m5lib')
        call put('app/main.f90', 'program m5app'//new_line('a')// &
            'end program m5app')
        call put('test/test_fast.f90', 'program test_fast'//new_line('a')// &
            'end program test_fast')
        call put('test/test_m5_slow_case.f90', 'program test_m5_slow_case'// &
            new_line('a')//'end program test_m5_slow_case')
        call put('test/m5_suite.f90', 'program m5_suite'//new_line('a')// &
            'end program m5_suite')
        call put('test/test_routed.f90', '! fo: dispatcher'//new_line('a')// &
            'program test_routed'//new_line('a')//'end program test_routed')
        call put('test/test_routed_mod.f90', '! fo: dispatcher'//new_line('a')// &
            'module test_routed_mod'//new_line('a')// &
            'implicit none'//new_line('a')//'end module test_routed_mod')
        call put('test/m5_helper.f90', 'module m5_helper'//new_line('a')// &
            'implicit none'//new_line('a')//'end module m5_helper')
    end subroutine write_project

    subroutine test_model()
        type(project_model_t) :: model
        type(model_request_t) :: request
        type(model_selection_t) :: selection
        type(model_plan_t) :: plan
        type(string_t), allocatable :: names(:)
        character(len=:), allocatable :: message
        integer :: ierr, c, t

        call fpm_model_load(trim(root), model, ierr, message)
        call check(ierr == 0, 'model loads: '//message)
        if (ierr /= 0) return
        call check(model%complete, 'model is complete')
        call check(model%backend == 'fpm', 'backend is fpm')
        call check(size(model%default_exclude) == 1, 'one default exclusion')
        call check(model%default_exclude(1)%s == 'slow', 'default exclusion is slow')

        t = model_find_target(model, 'libm5probe')
        call check(t > 0, 'library target')
        if (t > 0) call check(model%targets(t)%kind == TARGET_LIBRARY, &
            'library kind')
        t = model_find_target(model, 'm5probe')
        call check(t > 0, 'app executable target')
        if (t > 0) call check(model%targets(t)%kind == TARGET_EXECUTABLE, &
            'app executable kind')
        call check(model_find_target(model, 'test_fast') > 0, 'fast test exe')
        call check(model_find_target(model, 'm5_suite') > 0, 'dispatcher exe')
        call check(model_find_target(model, 'test_routed') == 0, &
            'routed program has no executable of its own')
        call check(model_find_artifact(model, &
            trim(root)//'/build/fo/bin/test_fast') > 0, 'artifact lookup')

        c = model_find_case(model, 'test_fast')
        call check(c > 0, 'fast case')
        if (c > 0) then
            call check(model%cases(c)%mapping == MAP_EXACT, 'fast case maps exactly')
            call check(model%targets(model%cases(c)%command_target)%name == &
                'test_fast', 'fast case runs its exe')
            call check(size(model%cases(c)%labels) == 0, 'fast case is unlabelled')
        end if
        c = model_find_case(model, 'test_m5_slow_case')
        call check(c > 0, 'slow case')
        if (c > 0) then
            call check(size(model%cases(c)%labels) == 1, 'slow case has a label')
            if (size(model%cases(c)%labels) == 1) call check( &
                model%cases(c)%labels(1)%s == 'slow', 'label is slow')
        end if
        c = model_find_case(model, 'm5_suite')
        call check(c > 0, 'dispatcher is a case')
        if (c > 0) then
            call check(model%cases(c)%explicit_only, 'dispatcher is explicit-only')
            call check(model%targets(model%cases(c)%command_target)%name == &
                'm5_suite', 'dispatcher case runs the dispatcher exe')
        end if
        call check(model_find_case(model, 'm5_helper') == 0, &
            'helper module is not a case')
        do c = 1, model%n_cases
            if (model%cases(c)%name /= 'test_routed' .and. &
                model%cases(c)%name /= 'test_routed_mod') cycle
            call check(model%targets(model%cases(c)%command_target)%name == &
                'm5_suite', trim(model%cases(c)%name)//' maps to dispatcher')
        end do

        ! Default selection drops the slow case; naming it needs --all.
        call model_select(model, request, selection)
        call check(selection%status == SELECT_OK, 'default selection ok')
        call model_selected_names(model, selection, names)
        call check(size(names) == 3, 'default selection has three fast cases')
        call model_plan(model, selection%cases, plan)
        call check(.not. plan%widened, 'complete model does not widen')
        call check(has_target(model, plan, 'm5_suite'), 'plan builds dispatcher')
        call check(.not. has_target(model, plan, 'm5probe'), 'plan skips the app')
        allocate (request%names(1))
        request%names(1)%s = 'test_m5_slow_case'
        call model_select(model, request, selection)
        call check(selection%status == SELECT_NAMED_EXCLUDED, &
            'named slow case requires --all')

        ! The dispatcher is explicit-only: a matching glob and --all skip it,
        ! its exact name runs it.
        request%names(1)%s = 'm5*'
        call model_select(model, request, selection)
        call check(selection%status == SELECT_EMPTY, &
            'glob does not select dispatcher')
        request%names(1)%s = '*suite'
        request%all = .true.
        call model_select(model, request, selection)
        call check(selection%status == SELECT_EMPTY, '--all glob skips dispatcher')
        deallocate (request%names)
        allocate (request%names(0))
        call model_select(model, request, selection)
        call model_selected_names(model, selection, names)
        call check(size(names) == 4, '--all selects the four inventory cases')
        do c = 1, size(names)
            call check(names(c)%s /= 'm5_suite', '--all skips dispatcher')
        end do
        deallocate (request%names)
        allocate (request%names(1))
        request%names(1)%s = 'm5_suite'
        call model_select(model, request, selection)
        call check(selection%status == SELECT_OK .and. size(selection%cases) == 1, &
            'dispatcher runs when named')
    end subroutine test_model

    logical function has_target(model, plan, name) result(found)
        type(project_model_t), intent(in) :: model
        type(model_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        do i = 1, size(plan%targets)
            if (model%targets(plan%targets(i))%name == name) found = .true.
        end do
    end function has_target

    subroutine test_inventory()
        !! The model's case names are exactly the native inventory, slow
        !! tests included.
        type(project_model_t) :: model
        type(dag_t) :: dag
        character(len=MAX_PATH), allocatable :: filenames(:), inventory(:)
        integer, allocatable :: changed(:), affected(:), ids(:)
        integer :: n_changed, n_affected, n_cached, ierr, n_inventory, i
        character(len=:), allocatable :: message

        allocate (filenames(MAX_NODES), changed(MAX_NODES), affected(MAX_NODES))
        allocate (ids(MAX_NODES), inventory(MAX_NODES))
        call fo_changed_modules(trim(root), dag, changed, n_changed, affected, &
            n_affected, n_cached, ierr, filenames=filenames)
        call check(ierr == 0, 'native scan succeeds')
        do i = 1, dag%n_nodes
            ids(i) = i
        end do
        call gfortran_selected_test_names(trim(root), filenames, ids, &
            dag%n_nodes, .true., inventory, n_inventory)
        call fpm_model_load(trim(root), model, ierr, message)
        call check(ierr == 0, 'model loads for inventory')
        call check(n_inventory == 4, 'inventory has four cases')
        call check(model%n_cases == n_inventory + 1, &
            'model has the inventory plus the dispatcher')
        do i = 1, n_inventory
            call check(model_find_case(model, trim(inventory(i))) > 0, &
                'model has inventory case '//trim(inventory(i)))
        end do
    end subroutine test_inventory

    subroutine write_layered(tail)
        !! Three layers (base <- mid <- top), an unlayered source, five tests.
        character(len=*), intent(in) :: tail
        character(len=1) :: nl

        nl = new_line('a')
        call fs_remove_tree(trim(root)//'_layers')
        call fs_make_dir(trim(root)//'_layers')
        call fs_make_dir(trim(root)//'_layers/src')
        call fs_make_dir(trim(root)//'_layers/test')
        call lput('fpm.toml', 'name = "lay"'//nl//'[extra.fo]'//nl// &
            '[[extra.fo.inputs]]'//nl//'path = "data/cfg.nml"'//nl//tail)
        call lput('src/l_base.f90', 'module l_base'//nl//'end module l_base')
        call lput('src/l_mid.f90', 'module l_mid'//nl//'use l_base'//nl// &
            'end module l_mid')
        call lput('src/l_top.f90', 'module l_top'//nl//'use l_mid'//nl// &
            'end module l_top')
        call lput('src/l_orphan.f90', 'module l_orphan'//nl//'end module l_orphan')
        call lput('test/test_base.f90', 'program test_base'//nl//'use l_base'//nl// &
            'end program test_base')
        call lput('test/test_mid.f90', 'program test_mid'//nl//'use l_mid'//nl// &
            'end program test_mid')
        call lput('test/test_top.f90', 'program test_top'//nl//'use l_top'//nl// &
            'end program test_top')
        call lput('test/test_free.f90', 'program test_free'//nl// &
            'end program test_free')
        call lput('test/test_lay_slow.f90', 'program test_lay_slow'//nl// &
            'use l_base'//nl//'end program test_lay_slow')
    end subroutine write_layered

    subroutine lput(path, text)
        character(len=*), intent(in) :: path, text

        call fs_write_text(trim(root)//'_layers/'//path, text//new_line('a'))
    end subroutine lput

    function layers_toml() result(text)
        character(len=:), allocatable :: text
        character(len=1) :: nl

        nl = new_line('a')
        text = '[extra.fo.layers]'//nl// &
            'base = { source = ["src/l_base.f90"] }'//nl// &
            "mid = { modules = ['l_mid'], uses = ['base'] }"//nl// &
            'top = { source = ["src/l_t*.f90"], uses = ["mid"] }'//nl// &
            '[extra.fo.test-groups]'//nl// &
            'physics = { tests = ["test_mid", "test/test_top*", '// &
            '"test_lay_slow"], '// &
            'inputs = ["data/cfg.nml"] }'//nl// &
            'smoke = { tests = ["test_f*", "test_mid"], '// &
            'inputs = ["data/cfg.nml"] }'//nl
    end function layers_toml

    integer function case_layer_name_check(model, case_name, layer_name) result(ok)
        type(project_model_t), intent(in) :: model
        character(len=*), intent(in) :: case_name, layer_name
        integer :: c

        ok = 0
        c = model_find_case(model, case_name)
        if (c == 0) return
        if (len(layer_name) == 0) then
            if (model%cases(c)%layer == 0) ok = 1
        else if (model%cases(c)%layer > 0) then
            if (model%layers(model%cases(c)%layer)%name == layer_name) ok = 1
        end if
    end function case_layer_name_check

    logical function has_label(model, case_name, label) result(found)
        type(project_model_t), intent(in) :: model
        character(len=*), intent(in) :: case_name, label
        integer :: c, i

        found = .false.
        c = model_find_case(model, case_name)
        if (c == 0) return
        do i = 1, size(model%cases(c)%labels)
            if (model%cases(c)%labels(i)%s == label) found = .true.
        end do
    end function has_label

    logical function listed(names, text) result(found)
        type(string_t), intent(in) :: names(:)
        character(len=*), intent(in) :: text
        integer :: i

        found = .false.
        do i = 1, size(names)
            if (names(i)%s == text) found = .true.
        end do
    end function listed

    subroutine test_layers()
        type(project_model_t) :: model
        type(string_t), allocatable :: unlayered(:)
        character(len=:), allocatable :: message
        integer :: ierr, c, l

        ! Layers, groups, default exclusion untouched (slow).
        call write_layered(layers_toml())
        call fpm_model_load(trim(root)//'_layers', model, ierr, message, unlayered)
        call check(ierr == 0, 'layered model loads: '//message)
        if (ierr /= 0) return
        call check(model%n_layers == 3, 'three layers')
        call check(model_find_layer(model, 'base') == 1, 'layers keep their order')
        l = model_find_layer(model, 'mid')
        call check(l == 2, 'mid layer')
        if (l > 0) then
            call check(size(model%layers(l)%uses) == 1, 'mid uses one layer')
            if (size(model%layers(l)%uses) == 1) call check( &
                model%layers(l)%uses(1) == 1, 'mid uses base')
            call check(size(model%layers(l)%sources) == 1, 'module list resolves')
        end if
        call check(case_layer_name_check(model, 'test_base', 'base') == 1, &
            'test_base is in base')
        call check(case_layer_name_check(model, 'test_mid', 'mid') == 1, &
            'closure through l_mid reaches base, own source is mid')
        call check(case_layer_name_check(model, 'test_top', 'top') == 1, &
            'highest layer of the closure wins')
        call check(case_layer_name_check(model, 'test_free', '') == 1, &
            'case reaching no layered source has no layer')
        call check(case_layer_name_check(model, 'test_lay_slow', 'base') == 1, &
            'slow case still layered')
        call check(size(unlayered) == 6, 'unlayered sources are reported')
        call check(listed(unlayered, 'src/l_orphan.f90'), 'orphan is reported')
        call check(listed(unlayered, 'test/test_free.f90'), 'test source reported')
        call check(.not. listed(unlayered, 'src/l_mid.f90'), &
            'module-list member is not reported')

        call check(has_label(model, 'test_mid', 'physics'), 'group by name')
        call check(has_label(model, 'test_top', 'physics'), 'group by path glob')
        call check(.not. has_label(model, 'test_base', 'physics'), &
            'group does not leak')
        call check(has_label(model, 'test_free', 'smoke'), 'second group')
        call check(has_label(model, 'test_lay_slow', 'slow'), 'slow label kept')
        call check(has_label(model, 'test_lay_slow', 'physics'), &
            'group label follows existing slow label')
        c = model_find_case(model, 'test_mid')
        call check(has_label(model, 'test_mid', 'smoke'), &
            'case retains both matching group labels')
        call check(size(model%cases(c)%labels) == 2, 'two distinct group labels')
        call check(size(model%cases(c)%attrs) == 1, 'group input recorded')
        if (size(model%cases(c)%attrs) == 1) call check( &
            model%cases(c)%attrs(1)%key == 'input' .and. &
            model%cases(c)%attrs(1)%value == 'data/cfg.nml', 'input value')
        c = model_find_case(model, 'test_base')
        call check(size(model%cases(c)%attrs) == 0, 'ungrouped case has no inputs')
        call check(size(model%default_exclude) == 1, 'default stays [slow]')

        ! Explicit exclude replaces [slow]; slow cases then run by default.
        call write_layered(layers_toml()// &
            '[extra.fo.test-defaults]'//new_line('a')//'exclude = ["smoke"]')
        call fpm_model_load(trim(root)//'_layers', model, ierr, message)
        call check(ierr == 0, 'explicit exclude loads: '//message)
        call check(size(model%default_exclude) == 1, 'one explicit exclusion')
        if (size(model%default_exclude) == 1) call check( &
            model%default_exclude(1)%s == 'smoke', 'explicit exclusion replaces slow')
        call check(excluded_by_default(model, 'test_free'), 'smoke case excluded')
        call check(.not. excluded_by_default(model, 'test_lay_slow'), &
            'slow case no longer excluded')

        ! An explicitly empty list means no exclusions at all.
        call write_layered(layers_toml()// &
            '[extra.fo.test-defaults]'//new_line('a')//'exclude = []')
        call fpm_model_load(trim(root)//'_layers', model, ierr, message)
        call check(ierr == 0, 'empty exclude loads: '//message)
        call check(size(model%default_exclude) == 0, 'empty exclude: none')
        call check(.not. excluded_by_default(model, 'test_lay_slow'), &
            'empty exclude keeps slow cases')
        call check(has_label(model, 'test_lay_slow', 'slow'), &
            'slow label survives an empty exclude')

        ! No layers: nothing reported, cases unlayered.
        call write_layered('')
        call fpm_model_load(trim(root)//'_layers', model, ierr, message, unlayered)
        call check(ierr == 0, 'plain project loads')
        call check(model%n_layers == 0 .and. size(unlayered) == 0, &
            'no layers, no report')

        ! A layer naming nothing is an error, not an empty layer.
        call write_layered('[extra.fo.layers]'//new_line('a')// &
            "base = { modules = ['no_such_mod'] }")
        call fpm_model_load(trim(root)//'_layers', model, ierr, message)
        call check(ierr /= 0 .and. index(message, 'no_such_mod') > 0, &
            'unknown layer module is rejected')
        call write_layered('[extra.fo.layers]'//new_line('a')// &
            'base = { source = ["nowhere/*.f90"] }')
        call fpm_model_load(trim(root)//'_layers', model, ierr, message)
        call check(ierr /= 0 .and. index(message, 'nowhere') > 0, &
            'glob matching nothing is rejected')
        call fs_remove_tree(trim(root)//'_layers')
    end subroutine test_layers

    logical function excluded_by_default(model, case_name) result(excluded)
        type(project_model_t), intent(in) :: model
        character(len=*), intent(in) :: case_name
        type(model_request_t) :: request
        type(model_selection_t) :: selection
        type(string_t), allocatable :: names(:)

        call model_select(model, request, selection)
        call model_selected_names(model, selection, names)
        excluded = .not. listed(names, case_name)
    end function excluded_by_default

end program test_fpm_project_model
