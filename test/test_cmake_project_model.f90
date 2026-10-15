program test_cmake_project_model
    !! CMake File API -> project model: targets, link dependencies, artifact
    !! paths, layers and origin marking on a generated project
    !! (core <- model <- exe) with one target from a separate source root.
    use fo_fs, only: fs_realpath
    use fo_test_harness, only: string_list_t, process_result_t, make_scratch, &
        join_path, write_lines, list_add, remove_tree, run_process, &
        assert_process_ok, assert_true, assert_equal_string, &
        assert_equal_integer, finish_assertions
    use fo_cmake_context, only: cmake_context_t, cmake_context_init, &
        cmake_context_query
    use fo_cmake_model, only: cmake_model_load, cmake_model_target_origin
    use fo_project_model, only: project_model_t, model_find_target, &
        model_find_case, MAP_EXACT, MAP_UNKNOWN, &
        model_find_layer, TARGET_LIBRARY, TARGET_EXECUTABLE, ORIGIN_PROJECT, &
        ORIGIN_DEPENDENCY
    implicit none
    character(len=4096) :: resolved
    character(:), allocatable :: scratch, root, project, dependency, build
    character(:), allocatable :: message
    type(cmake_context_t) :: context
    type(project_model_t) :: model
    type(process_result_t) :: result
    integer :: ierr
    logical :: ok

    call make_scratch('fo-cmake-model', scratch)
    call fs_realpath(scratch, resolved, ok)
    root = trim(resolved)
    project = join_path(root, 'project')
    dependency = join_path(root, 'dependency')
    build = join_path(root, 'build')
    call write_fixture()

    ! No reply yet: explicit error, not an empty model.
    call cmake_context_init(context, project, build_root=build)
    call cmake_model_load(context, model, ierr, message)
    call assert_true(ierr /= 0 .and. len(message) > 0, 'missing reply is an error')
    call assert_true(.not. model%complete, 'missing reply leaves model incomplete')

    call cmake_context_query(context)
    call configure()
    call build_exe()
    call cmake_model_load(context, model, ierr, message, input_digest='d1')
    call assert_equal_integer(ierr, 0, 'load ok: '//message)
    call assert_true(model%complete, 'complete is set')
    call assert_true(model%finalized, 'model is finalized')
    call assert_equal_string(model%backend, 'cmake', 'backend')
    call assert_equal_integer(model%n_targets, 4, 'four targets')
    call check_targets()
    call check_layers()
    call check_cases()
    call check_preset_cases()

    ! A new request token without a re-run of CMake is a stale reply.
    call cmake_context_init(context, project, build_root=build)
    call cmake_model_load(context, model, ierr, message)
    call assert_true(ierr /= 0 .and. index(message, 'stale') > 0, &
        'stale reply is an error: '//message)

    call assert_equal_integer(cmake_model_target_origin('/s/a', '/s', '/s/build'), &
        ORIGIN_PROJECT, 'origin: inside source')
    call assert_equal_integer(cmake_model_target_origin('/s/build/_deps/x-src', &
        '/s', '/s/build'), ORIGIN_DEPENDENCY, 'origin: FetchContent in build dir')
    call assert_equal_integer(cmake_model_target_origin('/other', '/s', '/s/build'), &
        ORIGIN_DEPENDENCY, 'origin: outside source')
    call assert_equal_integer(cmake_model_target_origin('/sx', '/s', '/s/build'), &
        ORIGIN_DEPENDENCY, 'origin: sibling prefix')

    call remove_tree(root)
    call finish_assertions()
    print '(a)', 'CMake project model PASS'
contains
    subroutine write_fixture()
        call write_lines(join_path(project, 'CMakeLists.txt'), [character(len=160) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(pm Fortran)', &
            'add_subdirectory("'//dependency//'" dep_build)', &
            'add_subdirectory(core)', &
            'add_subdirectory(model)', &
            'add_executable(exe main.f90)', &
            'target_link_libraries(exe PRIVATE model)', &
            'enable_testing()', &
            'add_test(NAME unit COMMAND exe)', &
            'set_tests_properties(unit PROPERTIES LABELS "fast" TIMEOUT 30 '// &
            'FIXTURES_REQUIRED fx)', &
            'add_test(NAME crunch COMMAND exe)', &
            'set_tests_properties(crunch PROPERTIES LABELS "slow;perf")', &
            'add_test(NAME script COMMAND ${CMAKE_COMMAND} -P '// &
            '${CMAKE_CURRENT_SOURCE_DIR}/check.cmake)', &
            'set_tests_properties(script PROPERTIES FIXTURES_SETUP fx '// &
            'FIXTURES_REQUIRED "a;b")'])
        call write_lines(join_path(project, 'check.cmake'), &
            [character(len=40) :: 'message(STATUS ok)'])
        call write_lines(join_path(project, 'CMakePresets.json'), &
            [character(len=200) :: '{"version":3,"configurePresets":[{"name":"c",'// &
            '"generator":"Ninja","binaryDir":"'//build//'"}],"testPresets":'// &
            '[{"name":"t","configurePreset":"c","filter":{"exclude":'// &
            '{"label":"perf"}}}]}'])
        call write_lines(join_path(project, 'core/CMakeLists.txt'), &
            [character(len=160) :: 'add_library(core STATIC core.f90)', &
            'target_link_libraries(core PUBLIC ext)'])
        call write_lines(join_path(project, 'core/core.f90'), [character(len=80) :: &
            'module core', 'contains', 'subroutine c()', 'end subroutine', &
            'end module'])
        call write_lines(join_path(project, 'model/CMakeLists.txt'), &
            [character(len=160) :: 'add_library(model STATIC model.f90)', &
            'target_link_libraries(model PUBLIC core)'])
        call write_lines(join_path(project, 'model/model.f90'), [character(len=80) :: &
            'module model', 'contains', 'subroutine m()', 'end subroutine', &
            'end module'])
        call write_lines(join_path(project, 'main.f90'), [character(len=80) :: &
            'program main', 'end program'])
        call write_lines(join_path(dependency, 'CMakeLists.txt'), &
            [character(len=160) :: 'add_library(ext STATIC ext.f90)'])
        call write_lines(join_path(dependency, 'ext.f90'), [character(len=80) :: &
            'module ext', 'contains', 'subroutine e()', 'end subroutine', &
            'end module'])
    end subroutine write_fixture

    subroutine configure()
        type(string_list_t) :: arguments
        call list_add(arguments, 'cmake')
        call list_add(arguments, '-S')
        call list_add(arguments, project)
        call list_add(arguments, '-B')
        call list_add(arguments, build)
        call run_process(arguments, root, result)
        call assert_process_ok(result, 'configure generated project')
    end subroutine configure

    subroutine build_exe()
        type(string_list_t) :: arguments
        ! CTest lists a command only once its executable exists.
        call list_add(arguments, 'cmake')
        call list_add(arguments, '--build')
        call list_add(arguments, build)
        call list_add(arguments, '--target')
        call list_add(arguments, 'exe')
        call run_process(arguments, root, result)
        call assert_process_ok(result, 'build exe')
    end subroutine build_exe

    subroutine check_targets()
        integer :: core, mdl, exe, ext

        core = model_find_target(model, 'core')
        mdl = model_find_target(model, 'model')
        exe = model_find_target(model, 'exe')
        ext = model_find_target(model, 'ext')
        call assert_true(min(core, mdl, exe, ext) > 0, 'all targets found')
        if (min(core, mdl, exe, ext) < 1) return
        call assert_equal_integer(model%targets(core)%kind, TARGET_LIBRARY, 'core lib')
        call assert_equal_integer(model%targets(mdl)%kind, TARGET_LIBRARY, 'model lib')
        call assert_equal_integer(model%targets(exe)%kind, TARGET_EXECUTABLE, &
            'exe kind')
        call assert_equal_integer(model%targets(core)%origin, ORIGIN_PROJECT, &
            'core is project')
        call assert_equal_integer(model%targets(exe)%origin, ORIGIN_PROJECT, &
            'exe is project')
        call assert_equal_integer(model%targets(ext)%origin, ORIGIN_DEPENDENCY, &
            'ext in a separate source root is a dependency')
        call assert_equal_string(model%targets(core)%artifact, &
            join_path(build, 'core/libcore.a'), 'core artifact')
        call assert_equal_string(model%targets(exe)%artifact, join_path(build, 'exe'), &
            'exe artifact')
        ! CMake reports the transitive link closure as target dependencies.
        call assert_true(has_dep(exe, mdl) .and. has_dep(exe, core) .and. &
            has_dep(exe, ext), 'exe -> model, core, ext')
        call assert_true(has_dep(mdl, core) .and. has_dep(mdl, ext), &
            'model -> core, ext')
        call assert_true(has_dep(core, ext) .and. .not. has_dep(core, exe) .and. &
            .not. has_dep(core, mdl), 'core -> ext only')
        call assert_equal_integer(size(model%targets(ext)%link_deps), 0, 'ext leaf')
        call assert_equal_integer(size(model%targets(core)%sources), 1, 'core sources')
        call assert_equal_string(model%targets(core)%sources(1)%s, &
            join_path(project, 'core/core.f90'), 'core source is absolute')
        call assert_equal_integer(model_find_target(model, 'nosuch'), 0, 'unknown')
    end subroutine check_targets

    logical function has_dep(from, to)
        integer, intent(in) :: from, to

        has_dep = any(model%targets(from)%link_deps == to)
    end function has_dep

    subroutine check_layers()
        integer :: core, mdl

        core = model_find_layer(model, 'core')
        mdl = model_find_layer(model, 'model')
        call assert_true(core > 0 .and. mdl > 0, 'library layers exist')
        call assert_equal_integer(model_find_layer(model, 'exe'), 0, 'exe is no layer')
        call assert_equal_integer(model_find_layer(model, 'ext'), 0, &
            'dependency is no layer')
        if (min(core, mdl) < 1) return
        call assert_equal_integer(size(model%layers(core)%uses), 0, &
            'core layer does not use the dependency')
        call assert_equal_integer(size(model%layers(mdl)%uses), 1, 'model uses one')
        call assert_equal_integer(model%layers(mdl)%uses(1), core, 'model uses core')
        call assert_equal_integer(size(model%layers(mdl)%targets), 1, 'layer target')
    end subroutine check_layers

    subroutine check_cases()
        integer :: unit_case, crunch, script, exe

        call assert_equal_integer(model%n_cases, 3, 'three cases')
        call assert_equal_integer(size(model%default_exclude), 1, 'slow excluded')
        unit_case = model_find_case(model, 'unit')
        crunch = model_find_case(model, 'crunch')
        script = model_find_case(model, 'script')
        exe = model_find_target(model, 'exe')
        call assert_true(min(unit_case, crunch, script) > 0, 'all cases found')
        if (min(unit_case, crunch, script) < 1) return
        call assert_equal_integer(model%cases(unit_case)%command_target, exe, &
            'unit runs exe')
        call assert_equal_integer(model%cases(unit_case)%mapping, MAP_EXACT, &
            'unit mapping exact')
        call assert_equal_integer(model%cases(script)%command_target, 0, &
            'script command has no target')
        call assert_equal_integer(model%cases(script)%mapping, MAP_UNKNOWN, &
            'script mapping unknown')
        call assert_equal_integer(size(model%cases(crunch)%labels), 2, 'two labels')
        call assert_equal_string(model%cases(crunch)%labels(1)%s, 'perf', 'label 1')
        call assert_equal_string(model%cases(crunch)%labels(2)%s, 'slow', 'label 2')
        call assert_equal_string(attr(unit_case, 'FIXTURES_REQUIRED'), 'fx', &
            'fixture attr')
        call assert_equal_string(attr(unit_case, 'TIMEOUT'), '30', 'numeric attr')
        call assert_equal_string(attr(script, 'FIXTURES_REQUIRED'), 'a;b', &
            'list attr is joined')
        call assert_equal_string(attr(unit_case, 'LABELS'), '', 'labels are no attr')
        call assert_true(.not. any(model%cases%default_excluded), 'no flag w/o preset')
    end subroutine check_cases

    subroutine check_preset_cases()
        integer :: crunch, unit_case

        call cmake_context_init(context, project, build_root=build, test_preset='t')
        call cmake_context_query(context)
        call configure()
        call cmake_model_load(context, model, ierr, message)
        call assert_equal_integer(ierr, 0, 'preset load ok: '//message)
        call assert_equal_integer(model%n_cases, 3, 'preset: three cases')
        call assert_equal_integer(size(model%default_exclude), 0, &
            'preset replaces the slow rule')
        crunch = model_find_case(model, 'crunch')
        unit_case = model_find_case(model, 'unit')
        call assert_true(min(crunch, unit_case) > 0, 'preset cases found')
        if (min(crunch, unit_case) < 1) return
        call assert_true(model%cases(crunch)%default_excluded, &
            'preset-filtered case is flagged')
        call assert_equal_integer(size(model%cases(crunch)%labels), 2, &
            'filtered case keeps labels')
        call assert_true(.not. model%cases(unit_case)%default_excluded, &
            'listed case is not flagged')
    end subroutine check_preset_cases

    function attr(case_index, key) result(value)
        integer, intent(in) :: case_index
        character(len=*), intent(in) :: key
        character(:), allocatable :: value
        integer :: i

        value = ''
        do i = 1, size(model%cases(case_index)%attrs)
            if (model%cases(case_index)%attrs(i)%key == key) &
                value = model%cases(case_index)%attrs(i)%value
        end do
    end function attr
end program test_cmake_project_model
