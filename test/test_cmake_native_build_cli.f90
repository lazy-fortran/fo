program test_cmake_native_build_cli
    !! CMake only configures; fo compiles and links natively from the File API.
    !! Oracles (all through the public `fo build`/`fo test` process boundary):
    !!  - the twin CMake fixture gives the same test verdicts built natively and
    !!    through `cmake --build` (FO_CMAKE_DELEGATE_BUILD=1);
    !!  - a mixed C/Fortran project (per-target include directories, compile
    !!    definitions that select a `use` and one that would form a module cycle
    !!    if the inactive branch were scanned, two Fortran targets with separate
    !!    module directories, a static library and an executable) produces the
    !!    output of the cmake-built reference, without `cmake --build` (a
    !!    compiled Fortran `cmake` shim on PATH records and refuses it), and a
    !!    declared header-only inputs are not compiled and header/include edits
    !!    reach their C and Fortran objects; a fresh build tree restores the C
    !!    object and still compiles it after a header edit;
    !!  - a target with a generated source is delegated, built before its
    !!    dependents, and only that target reaches `cmake --build`;
    !!  - interface cutoff, observed with a compiler shim that records every
    !!    compile: a body-only edit of a low-level module compiles exactly that
    !!    file and relinks, an interface edit compiles its dependents;
    !!  - a copy of a project at a path of another length reuses every compile result
    !!    with FO_CACHE_RELOCATABLE=1 (debug objects compiled at two paths are
    !!    byte-identical), stays
    !!    location-keyed without it, and a body edit compiles one file.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_lines, write_text
    use fo_test_harness, only: read_text, remove_tree, remove_path, environment_value
    use fo_test_harness, only: run_process, assert_true, assert_contains
    use fo_test_harness, only: assert_not_contains, assert_equal_string
    use fo_test_harness, only: assert_equal_integer, assert_process_ok
    use fo_test_harness, only: finish_assertions, file_exists
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value
    use fo_test_twin, only: twin_write, twin_case_count, twin_case_name
    use fo_link_resolve, only: is_macos
    use fo_fs, only: fs_find_executable
    use fo_fake_tool, only: fake_tool_dispatch, install_fake_tool
    implicit none

    character(:), allocatable :: driver, scratch, cache, fortran, shim_dir
    character(:), allocatable :: cmake_log, compile_log, ar_read_log
    character(len=1), parameter :: nl = achar(10)

    call fake_tool_dispatch()
    call resolve_driver(driver)
    call make_scratch('fo-cmake-native-build', scratch)
    cache = join_path(scratch, 'fo-cache')
    shim_dir = join_path(scratch, 'shim')
    cmake_log = join_path(scratch, 'cmake-build.log')
    compile_log = join_path(scratch, 'compile.log')
    ar_read_log = join_path(scratch, 'ar-read.log')
    fortran = find_tool('gfortran')
    call assert_true(len(fortran) > 0, 'a gfortran is on PATH')
    if (len(fortran) == 0) call finish_assertions()
    call build_shims()

    call twin_verdicts()
    call mixed_project()
    call source_preprocessing()
    call system_module_include()
    call generated_source_is_delegated()
    call duplicate_modules_are_delegated()
    call interface_cutoff()
    call relocated_tree_reuses_results()
    call shared_dependency_relocation()
    call shared_library_keeps_its_runtime_path()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'cmake-native-build-cli: native build matches cmake --build'

contains

    subroutine system_module_include()
        character(:), allocatable :: root, modules
        type(process_result_t) :: observed
        type(string_list_t) :: command, args, environment

        root = join_path(scratch, 'system-module')
        modules = join_path(root, 'system')
        call write_lines(join_path(modules, 'external.f90'), [character(len=80) :: &
            'module external_config', 'implicit none', &
            'integer, parameter :: expected = 31', 'end module external_config'])
        ! An independently compiled module, like an installed MPI module, has
        ! no provider target whose module directory Fo could add implicitly.
        call list_add(command, fortran)
        call list_add(command, '-c')
        call list_add(command, '-J'//modules)
        call list_add(command, '-o')
        call list_add(command, join_path(modules, 'external.o'))
        call list_add(command, join_path(modules, 'external.f90'))
        call run_process(command, root, observed)
        call assert_process_ok(observed, 'independent external module compiles')
        call write_lines(join_path(root, 'CMakeLists.txt'), [character(len=180) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(system_module Fortran)', 'enable_testing()', &
            'add_executable(probe main.f90)', &
            'target_include_directories(probe SYSTEM PRIVATE system)', &
            'add_test(NAME system_module COMMAND probe)'])
        call write_lines(join_path(root, 'main.f90'), [character(len=80) :: &
            'program probe', 'use external_config, only: expected', &
            'implicit none', 'if (expected /= 31) stop 1', 'end program probe'])
        environment = fo_environment(.false., .true.)
        call list_add(environment, 'FO_CMAKE_ARGS=-DCMAKE_Fortran_COMPILER='//fortran)
        call list_add(args, 'test')
        call remove_path(cmake_log)
        call run_fo(driver, args, root, cache, observed, environment)
        call assert_process_ok(observed, 'SYSTEM include finds an external Fortran module')
        call assert_true(.not. file_exists(cmake_log), &
            'external module includes do not require delegation')
    end subroutine system_module_include

    subroutine source_preprocessing()
        character(:), allocatable :: root, reference, dir
        type(process_result_t) :: native, delegated
        type(string_list_t) :: args
        integer :: i

        root = join_path(scratch, 'preprocess native')
        reference = join_path(scratch, 'preprocess-reference')
        do i = 1, 2
            dir = root
            if (i == 2) dir = reference
            call write_lines(join_path(dir, 'CMakeLists.txt'), [character(len=180) :: &
                'cmake_minimum_required(VERSION 3.20)', &
                'project(preprocessing Fortran)', 'enable_testing()', &
                'add_subdirectory(parts)', &
                'add_executable(probe main.f90)', &
                'target_link_libraries(probe PRIVATE parts)', &
                'add_test(NAME preprocessing COMMAND probe)'])
            call write_lines(join_path(dir, 'parts/CMakeLists.txt'), &
                [character(len=180) :: &
                'add_library(parts STATIC enabled.f90 disabled.F90)', &
                'target_compile_definitions(parts PRIVATE picked=31)', &
                'set_source_files_properties(enabled.f90 PROPERTIES Fortran_PREPROCESS ON)', &
                'set_source_files_properties(disabled.F90 PROPERTIES Fortran_PREPROCESS OFF)'])
            call write_lines(join_path(dir, 'parts/enabled.f90'), [character(len=80) :: &
                'integer function enabled()', '#ifdef UNAVAILABLE_MPI', &
                'use unavailable_mpi', '#endif', 'implicit none', &
                'enabled = picked', 'end function enabled'])
            call write_lines(join_path(dir, 'parts/disabled.F90'), [character(len=80) :: &
                'integer function disabled()', 'implicit none', &
                'integer :: picked', 'picked = 11', &
                'disabled = picked', 'end function disabled'])
            call write_lines(join_path(dir, 'main.f90'), [character(len=80) :: &
                'program probe', 'implicit none', &
                'integer, external :: enabled, disabled', &
                'if (enabled() /= 31 .or. disabled() /= 11) stop 1', &
                'end program probe'])
        end do
        call list_add(args, 'test')
        call list_add(args, '--rerun')
        call list_add(args, '--retries')
        call list_add(args, '0')
        call remove_path(cmake_log)
        call run_fo(driver, args, root, cache, native, fo_environment(.false., .true.))
        call assert_process_ok(native, 'per-source preprocessing ON/OFF runs natively')
        call assert_true(.not. file_exists(cmake_log), &
            'preprocessing does not delegate any target')
        call run_fo(driver, args, reference, cache, delegated, &
            fo_environment(.true., .false.))
        call assert_process_ok(delegated, 'CMake reference agrees on ON/OFF behavior')
        call write_lines(join_path(root, 'parts/CMakeLists.txt'), &
            [character(len=180) :: &
            'add_library(parts STATIC enabled.f90 disabled.F90)', &
            'target_compile_definitions(parts PRIVATE picked=31)', &
            'set_source_files_properties(enabled.f90 disabled.F90 '// &
            'PROPERTIES Fortran_PREPROCESS ON)'])
        call run_fo(driver, args, root, cache, native, fo_environment(.false., .true.))
        call assert_true(native%exit_code /= 0, &
            'changing OFF to ON invalidates the warm successful compile')
    end subroutine source_preprocessing

    function find_tool(name) result(path)
        character(len=*), intent(in) :: name
        character(:), allocatable :: path
        character(len=4096) :: found
        logical :: located

        path = ''
        call fs_find_executable(name, found, located)
        if (located) path = trim(found)
    end function find_tool

    subroutine build_shims()
        !! Fake tools: `cmake` records and refuses `--build` (passing everything
        !! else to the real cmake) and `gfortran-count` records each compile, then
        !! runs the real gfortran.
        call install_fake_tool(join_path(shim_dir, 'cmake'), &
            'if $ARG1 = --build append $FO_TEST_CMAKE_LOG " $ARGS\n"' // nl // &
            'if $ARG1 = --build if ${FO_TEST_CMAKE_PASS:-} = "" exit 97' // nl // &
            'exec $FO_TEST_REAL_CMAKE $@')
        call install_fake_tool(join_path(shim_dir, 'ar'), &
            'if $ARG1 = p append $FO_TEST_AR_READ_LOG "read member\n"' // nl // &
            'exec $FO_TEST_REAL_AR $@')
        call install_fake_tool(join_path(shim_dir, 'gfortran-count'), &
            'if " $ARGS " ~ " -c " if ${FO_TEST_FC_LOG:-} != "" append $FO_TEST_FC_LOG ' // &
            '"COMPILE $LAST\n"' // nl // &
            'if " $ARGS " !~ " -c " if " $ARGS " ~ "top.f90.o" ' // &
            'if ${FO_TEST_FC_LOG:-} != "" append $FO_TEST_FC_LOG "LINK top\n"' // nl // &
            'exec $FO_TEST_REAL_FC $@')
    end subroutine build_shims

    function fo_environment(delegate, with_shims) result(environment)
        logical, intent(in) :: delegate, with_shims
        type(string_list_t) :: environment

        call list_add(environment, 'FO_BACKEND=cmake')
        call list_add(environment, 'FO_JOBS=2')
        ! These fixtures choose their compiler, including the counting wrapper.
        ! A host toolchain can otherwise override CMAKE_Fortran_COMPILER.
        call list_add(environment, 'CMAKE_TOOLCHAIN_FILE=')
        if (delegate) call list_add(environment, 'FO_CMAKE_DELEGATE_BUILD=1')
        if (.not. with_shims) return
        call list_add(environment, 'PATH='//shim_dir//':'//environment_value('PATH'))
        call list_add(environment, 'FO_TEST_REAL_CMAKE='//find_tool('cmake'))
        call list_add(environment, 'FO_TEST_CMAKE_LOG='//cmake_log)
        call list_add(environment, 'FO_TEST_REAL_AR='//find_tool('ar'))
        call list_add(environment, 'FO_TEST_AR_READ_LOG='//ar_read_log)
    end function fo_environment

    function line_count(text, fragment) result(n)
        character(len=*), intent(in) :: text, fragment
        integer :: n, start, at

        n = 0
        start = 1
        do
            at = index(text(start:), fragment)
            if (at == 0) exit
            n = n + 1
            start = start + at - 1 + len(fragment)
        end do
    end function line_count

    function route_record(root) result(text)
        character(len=*), intent(in) :: root
        character(:), allocatable :: text

        text = read_text(join_path(root, 'build/.fo-native-cmake/route.log'))
    end function route_record

    function run_text(path) result(text)
        character(len=*), intent(in) :: path
        character(:), allocatable :: text
        type(string_list_t) :: command
        type(process_result_t) :: observed

        call list_add(command, path)
        call run_process(command, scratch, observed)
        text = observed%stdout
        if (observed%exit_code /= 0) text = 'exit '//trim(text)
    end function run_text

    subroutine twin_verdicts()
        character(:), allocatable :: native_dir, delegated_dir
        type(process_result_t) :: native, delegated
        type(string_list_t) :: args
        type(json_value_t) :: native_report, delegated_report, native_tests
        type(json_value_t) :: delegated_tests, entry, field
        logical :: valid
        character(:), allocatable :: error_message, name
        integer :: i, j

        native_dir = join_path(scratch, 'twin-native')
        delegated_dir = join_path(scratch, 'twin-delegated')
        call twin_write(native_dir, 'cmake')
        call twin_write(delegated_dir, 'cmake')
        call list_add(args, 'test')
        call list_add(args, '--all')
        call list_add(args, '--json=full')
        call run_fo(driver, args, native_dir, cache, native, fo_environment(.false., .false.))
        call run_fo(driver, args, delegated_dir, cache, delegated, &
            fo_environment(.true., .false.))
        call assert_process_ok(native, 'twin tests run natively')
        call assert_process_ok(delegated, 'twin tests run through cmake --build')
        call json_parse(native%stdout, native_report, valid, error_message)
        call assert_true(valid, 'native twin report is JSON')
        call json_parse(delegated%stdout, delegated_report, valid, error_message)
        call assert_true(valid, 'delegated twin report is JSON')
        native_tests = json_member(native_report, 'tests')
        delegated_tests = json_member(delegated_report, 'tests')
        call assert_equal_integer(json_size(native_tests), twin_case_count(), &
            'every twin case has a native verdict')
        call assert_equal_integer(json_size(delegated_tests), json_size(native_tests), &
            'both routes report the same cases')
        do i = 1, json_size(native_tests)
            entry = json_element(native_tests, i)
            name = json_string_value(json_member(entry, 'name'))
            do j = 1, json_size(delegated_tests)
                entry = json_element(delegated_tests, j)
                if (json_string_value(json_member(entry, 'name')) /= name) cycle
                field = json_member(entry, 'status')
                entry = json_element(native_tests, i)
                call assert_equal_string(json_string_value(json_member(entry, 'status')), &
                    json_string_value(field), 'same verdict for '//name)
            end do
        end do
        call assert_contains(route_record(native_dir), &
            'native CMake build: test_core_a', 'twin targets build natively')
        call assert_not_contains(route_record(native_dir), 'delegated:', &
            'nothing of the twin is delegated')
        call assert_contains(route_record(delegated_dir), 'FO_CMAKE_DELEGATE_BUILD=1', &
            'forced delegation is recorded')
        call assert_true(file_exists(join_path(native_dir, 'build/test_core_a')), &
            'native build produced the test executable')
    end subroutine twin_verdicts

    subroutine write_mixed(dir)
        character(len=*), intent(in) :: dir

        call write_lines(join_path(dir, 'CMakeLists.txt'), [character(len=180) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(mixed C Fortran)', &
            'enable_testing()', &
            'add_library(core STATIC core/core.F90 core/gate.f90 core/gate_alt.f90 '// &
            'core/include/offset.inc core/cyc_x.F90 core/cyc_a.f90 core/scale.c '// &
            'core/include/scale.h)', &
            'set_source_files_properties(core/include/offset.inc core/include/scale.h '// &
            'PROPERTIES HEADER_FILE_ONLY TRUE)', &
            'set_target_properties(core PROPERTIES Fortran_MODULE_DIRECTORY '// &
            '${CMAKE_BINARY_DIR}/mod_core)', &
            'target_include_directories(core PUBLIC core/include '// &
            '${CMAKE_BINARY_DIR}/mod_core)', &
            'target_compile_definitions(core PRIVATE CORE_SCALE=3 GATE_ALT)', &
            'target_link_libraries(core PUBLIC m)', &
            'add_library(util STATIC util/util.f90)', &
            'set_target_properties(util PROPERTIES Fortran_MODULE_DIRECTORY '// &
            '${CMAKE_BINARY_DIR}/mod_util)', &
            'target_include_directories(util PUBLIC ${CMAKE_BINARY_DIR}/mod_util)', &
            'target_link_libraries(util PUBLIC core)', &
            'add_executable(app app/banner.inc app/main.f90)', &
            'set_source_files_properties(app/banner.inc PROPERTIES HEADER_FILE_ONLY TRUE)', &
            'target_include_directories(app PRIVATE app)', &
            'target_link_libraries(app PRIVATE util)', &
            'add_test(NAME app_runs COMMAND app)', &
            'set_tests_properties(app_runs PROPERTIES PASS_REGULAR_EXPRESSION '// &
            '"mixed: ")'])
        call write_lines(join_path(dir, 'core/gate.f90'), [character(len=60) :: &
            'module gate_mod', 'implicit none', 'contains', &
            'integer function gate_offset()', 'gate_offset = 0', 'end function', &
            'end module'])
        call write_lines(join_path(dir, 'core/gate_alt.f90'), [character(len=60) :: &
            'module gate_alt', 'implicit none', 'contains', &
            'integer function gate_offset()', 'gate_offset = 100', 'end function', &
            'end module'])
        call write_lines(join_path(dir, 'core/cyc_x.F90'), [character(len=60) :: &
            'module cyc_x', '#ifdef WITH_A', 'use cyc_a', '#endif', &
            'implicit none', 'integer, parameter :: cyc_base = 4', 'end module'])
        call write_lines(join_path(dir, 'core/cyc_a.f90'), [character(len=60) :: &
            'module cyc_a', 'use cyc_x, only: cyc_base', 'implicit none', &
            'integer, parameter :: cyc_a_value = cyc_base + 3', 'end module'])
        call write_lines(join_path(dir, 'core/core.F90'), [character(len=100) :: &
            'module core_mod', '#ifdef GATE_ALT', 'use gate_alt, only: gate_offset', &
            '#else', 'use gate_mod, only: gate_offset', '#endif', &
            'use, intrinsic :: iso_c_binding, only: c_int', 'implicit none', &
            'include "offset.inc"', &
            'interface', '  integer(c_int) function core_scale(x) bind(C, name="core_scale")', &
            '    import :: c_int', '    integer(c_int), value :: x', &
            '  end function', 'end interface', 'contains', &
            'integer function core_value(x)', '  integer, intent(in) :: x', &
            '  core_value = int(core_scale(int(x, c_int))) + gate_offset() + header_offset', &
            'end function', 'end module'])
        call write_lines(join_path(dir, 'core/scale.c'), [character(len=60) :: &
            '#include "scale.h"', 'int core_scale(int x) {', &
            '  return x * CORE_SCALE + SCALE_BASE;', '}'])
        call write_lines(join_path(dir, 'core/include/scale.h'), [character(len=40) :: &
            '#define SCALE_BASE 5'])
        call write_lines(join_path(dir, 'core/include/offset.inc'), [character(len=50) :: &
            'integer, parameter :: header_offset = 0'])
        call write_lines(join_path(dir, 'app/banner.inc'), [character(len=50) :: &
            'integer, parameter :: banner_offset = 0'])
        call write_lines(join_path(dir, 'util/util.f90'), [character(len=60) :: &
            'module util_mod', 'use core_mod, only: core_value', 'implicit none', &
            'contains', 'integer function util_value()', &
            '  util_value = core_value(2) * 2', 'end function', 'end module'])
        call write_lines(join_path(dir, 'app/main.f90'), [character(len=100) :: &
            'program app', 'use util_mod, only: util_value', &
            'use cyc_a, only: cyc_a_value', 'implicit none', 'include "banner.inc"', &
            'print "(a,i0,1x,i0)", "mixed: ", util_value(), cyc_a_value + banner_offset', &
            'end program'])
    end subroutine write_mixed

    subroutine mixed_project()
        character(:), allocatable :: source, reference, expected, native_run
        character(:), allocatable :: reference_run
        type(string_list_t) :: command, args, environment
        type(process_result_t) :: observed

        source = join_path(scratch, 'mixed')
        reference = join_path(scratch, 'mixed-reference')
        call write_mixed(source)
        ! The reference: CMake builds the same tree itself.
        call list_add(command, find_tool('cmake'))
        call list_add(command, '-S')
        call list_add(command, source)
        call list_add(command, '-B')
        call list_add(command, reference)
        call list_add(command, '-G')
        call list_add(command, 'Ninja')
        call list_add(command, '-DCMAKE_Fortran_COMPILER='//fortran)
        call run_process(command, scratch, observed)
        call assert_process_ok(observed, 'reference configure')
        command = string_list_t()
        call list_add(command, find_tool('cmake'))
        call list_add(command, '--build')
        call list_add(command, reference)
        call run_process(command, scratch, observed)
        call assert_process_ok(observed, 'reference cmake --build')
        reference_run = run_text(join_path(reference, 'app'))
        ! gate_alt 100 + core_scale(2) = 2*3+5 = 11 -> 111, doubled, cyc 7.
        expected = 'mixed: 222 7'//nl
        call assert_equal_string(reference_run, expected, 'reference output')

        environment = fo_environment(.false., .true.)
        call list_add(environment, 'FO_CMAKE_ARGS=-DCMAKE_Fortran_COMPILER='//fortran)
        call list_add(args, 'build')
        call run_fo(driver, args, source, cache, observed, environment)
        call assert_process_ok(observed, 'native build of the mixed project')
        native_run = run_text(join_path(source, 'build/app'))
        call assert_equal_string(native_run, reference_run, &
            'native executable prints what the cmake-built one prints')
        call assert_true(.not. file_exists(cmake_log), &
            'cmake --build is never invoked for native targets')
        call assert_contains(route_record(source), 'native CMake build: app', &
            'the executable is built natively')
        call assert_not_contains(route_record(source), 'delegated:', &
            'nothing of the mixed project is delegated')

        ! A header edit must reach the C object (header dependency in the key).
        call write_lines(join_path(source, 'core/include/scale.h'), [character(len=40) :: &
            '#define SCALE_BASE 9'])
        call run_fo(driver, args, source, cache, observed, environment)
        call assert_process_ok(observed, 'rebuild after the header edit')
        call assert_equal_string(run_text(join_path(source, 'build/app')), &
            'mixed: 230 7'//nl, 'the header edit changed the C object')
        call write_lines(join_path(source, 'core/include/offset.inc'), [character(len=50) :: &
            'integer, parameter :: header_offset = 2'])
        call run_fo(driver, args, source, cache, observed, environment)
        call assert_process_ok(observed, 'rebuild after the Fortran include edit')
        call assert_equal_string(run_text(join_path(source, 'build/app')), &
            'mixed: 234 7'//nl, 'the include edit changed the Fortran object')
        call remove_path(ar_read_log)
        call run_fo(driver, args, source, cache, observed, environment)
        call assert_process_ok(observed, 'warm rebuild')
        call assert_true(.not. file_exists(ar_read_log), &
            'a warm archive reuses validated bytes without spawning ar per member')
        call assert_equal_string(run_text(join_path(source, 'build/app')), &
            'mixed: 234 7'//nl, 'warm rebuild keeps the output')

        ! A warm receipt must not accept corrupted archive bytes.
        call write_text(join_path(source, 'build/libcore.a'), 'broken archive')
        call run_fo(driver, args, source, cache, observed, environment)
        call assert_process_ok(observed, 'rebuild repairs a corrupted native archive')
        call assert_equal_string(run_text(join_path(source, 'build/app')), &
            'mixed: 234 7'//nl, 'repaired archive preserves the compiled result')

        ! A fresh build tree has no dependency files: the C action is keyed
        ! by the headers a preprocessing scan lists, so it is restored too.
        call remove_tree(join_path(source, 'build'))
        call run_fo(driver, args, source, cache, observed, environment)
        call assert_process_ok(observed, 'build in a fresh build tree')
        call assert_contains(route_record(source), 'compiled 0 source file(s)', &
            'a fresh build tree restores every action, the C one included')
        call assert_equal_string(run_text(join_path(source, 'build/app')), &
            'mixed: 234 7'//nl, 'restored objects give the same output')
        ! A header edit seen first by a fresh build tree still misses.
        call remove_tree(join_path(source, 'build'))
        call write_lines(join_path(source, 'core/include/scale.h'), &
            [character(len=40) :: '#define SCALE_BASE 7'])
        call run_fo(driver, args, source, cache, observed, environment)
        call assert_process_ok(observed, 'fresh build tree after a header edit')
        call assert_contains(route_record(source), 'compiled 1 source file(s)', &
            'the header edit compiles exactly the C source')
        call assert_equal_string(run_text(join_path(source, 'build/app')), &
            'mixed: 230 7'//nl, 'the header edit reached the C object')

        ! Positive control: the trap sees the delegated route.
        environment = fo_environment(.true., .true.)
        call list_add(environment, 'FO_CMAKE_ARGS=-DCMAKE_Fortran_COMPILER='//fortran)
        call run_fo(driver, args, source, cache, observed, environment)
        call assert_true(observed%exit_code /= 0, 'the shim refuses cmake --build')
        call assert_true(file_exists(cmake_log), &
            'forced delegation reaches cmake --build through the shim')
        call remove_path(cmake_log)
    end subroutine mixed_project

    subroutine generated_source_is_delegated()
        character(:), allocatable :: root, recorded
        type(string_list_t) :: args, environment
        type(process_result_t) :: observed

        root = join_path(scratch, 'generated')
        call write_lines(join_path(root, 'CMakeLists.txt'), [character(len=200) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(generated Fortran)', &
            'add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/gen_mod.f90 '// &
            'COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/gen_mod.in '// &
            '${CMAKE_BINARY_DIR}/gen_mod.f90 DEPENDS ${CMAKE_SOURCE_DIR}/gen_mod.in)', &
            'add_library(gen STATIC ${CMAKE_BINARY_DIR}/gen_mod.f90)', &
            'add_executable(user user.f90)', &
            'target_link_libraries(user gen)', &
            'add_executable(plain plain.f90)', &
            'add_executable(stamped plain.f90)', &
            'add_custom_command(TARGET stamped POST_BUILD COMMAND ${CMAKE_COMMAND} '// &
            '-E touch ${CMAKE_BINARY_DIR}/post.stamp)'])
        call write_lines(join_path(root, 'gen_mod.in'), [character(len=60) :: &
            'module gen_mod', 'implicit none', 'contains', &
            'integer function gen_value()', 'gen_value = 41', 'end function', &
            'end module'])
        call write_lines(join_path(root, 'user.f90'), [character(len=60) :: &
            'program user', 'use gen_mod, only: gen_value', 'implicit none', &
            'print "(i0)", gen_value() + 1', 'end program'])
        call write_lines(join_path(root, 'plain.f90'), [character(len=40) :: &
            'program plain', 'print "(i0)", 7', 'end program'])
        environment = fo_environment(.false., .true.)
        call list_add(environment, 'FO_CMAKE_ARGS=-DCMAKE_Fortran_COMPILER='//fortran)
        call list_add(environment, 'FO_TEST_CMAKE_PASS=1')
        call list_add(args, 'build')
        call run_fo(driver, args, root, cache, observed, environment)
        call assert_process_ok(observed, 'build with a generated source')
        call assert_contains(route_record(root), 'delegated: gen (generated source', &
            'the generated-source target is delegated with its reason')
        call assert_contains(route_record(root), 'native CMake build: user', &
            'its dependent builds natively')
        call assert_contains(route_record(root), 'native CMake build: plain', &
            'an unrelated target builds natively')
        call assert_contains(route_record(root), &
            'delegated: stamped (custom command attached', &
            'a target with a post-build command is delegated')
        call assert_true(file_exists(join_path(root, 'build/post.stamp')), &
            'the post-build command ran')
        call assert_equal_string(run_text(join_path(root, 'build/user')), '42'//nl, &
            'the dependent used the generated module')
        recorded = read_text(cmake_log)
        call assert_contains(recorded, '--target gen stamped', &
            'exactly the delegated targets reach cmake --build')
        call assert_not_contains(recorded, ' user', 'user is not built by cmake')
        call assert_not_contains(recorded, ' plain', 'plain is not built by cmake')
        call remove_path(cmake_log)
    end subroutine generated_source_is_delegated

    subroutine duplicate_modules_are_delegated()
        !! Two libraries define the same module name in separate module
        !! directories: one native scheduler cannot hold both, so both go to
        !! CMake while their executables still build natively and link them.
        character(:), allocatable :: root
        type(string_list_t) :: args, environment
        type(process_result_t) :: observed
        integer :: i
        character(len=1) :: digit
        character(len=60) :: value_line, program_line

        root = join_path(scratch, 'duplicates')
        call write_lines(join_path(root, 'CMakeLists.txt'), [character(len=160) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(duplicates Fortran)', &
            'foreach(n 1 2)', &
            '  add_library(lib${n} STATIC dup${n}.f90)', &
            '  set_target_properties(lib${n} PROPERTIES Fortran_MODULE_DIRECTORY '// &
            '${CMAKE_BINARY_DIR}/mod${n})', &
            '  target_include_directories(lib${n} PUBLIC ${CMAKE_BINARY_DIR}/mod${n})', &
            '  add_executable(exe${n} exe${n}.f90)', &
            '  target_link_libraries(exe${n} lib${n})', &
            'endforeach()'])
        do i = 1, 2
            write (digit, '(i1)') i
            value_line = 'dup_value = '//digit
            program_line = 'program exe'//digit
            call write_lines(join_path(root, 'dup'//digit//'.f90'), &
                [character(len=60) :: 'module dup_mod', 'implicit none', 'contains', &
                'integer function dup_value()', value_line, 'end function', &
                'end module'])
            call write_lines(join_path(root, 'exe'//digit//'.f90'), &
                [character(len=60) :: program_line, 'use dup_mod, only: dup_value', &
                'implicit none', 'print "(i0)", dup_value()', 'end program'])
        end do
        environment = fo_environment(.false., .false.)
        call list_add(environment, 'FO_CMAKE_ARGS=-DCMAKE_Fortran_COMPILER='//fortran)
        call list_add(args, 'build')
        call run_fo(driver, args, root, cache, observed, environment)
        call assert_process_ok(observed, 'build with a module defined twice')
        call assert_contains(route_record(root), 'delegated: lib1 (source built '// &
            'differently or module defined twice', 'the first duplicate is delegated')
        call assert_contains(route_record(root), 'delegated: lib2', &
            'the second duplicate is delegated')
        call assert_contains(route_record(root), 'native CMake build: exe1', &
            'its executable is native')
        call assert_equal_string(run_text(join_path(root, 'build/exe1')), '1'//nl, &
            'exe1 linked the first library')
        call assert_equal_string(run_text(join_path(root, 'build/exe2')), '2'//nl, &
            'exe2 linked the second library')
    end subroutine duplicate_modules_are_delegated

    subroutine write_chain(root, body, interface_extra)
        character(len=*), intent(in) :: root, body, interface_extra
        character(:), allocatable :: value_line

        value_line = '  low_value = ' // body
        call write_lines(join_path(root, 'low.f90'), [character(len=70) :: &
            'module low_mod', 'implicit none', interface_extra, 'contains', &
            'integer function low_value()', value_line, &
            'end function', 'end module'])
    end subroutine write_chain

    subroutine interface_cutoff()
        character(:), allocatable :: root
        type(string_list_t) :: args, environment
        type(process_result_t) :: observed

        root = join_path(scratch, 'cutoff')
        call write_lines(join_path(root, 'CMakeLists.txt'), [character(len=100) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(cutoff Fortran)', &
            'add_library(chain STATIC low.f90 mid.f90)', &
            'add_executable(top top.f90)', &
            'target_link_libraries(top chain)'])
        call write_chain(root, '1', 'integer, parameter :: marker = 1')
        call write_lines(join_path(root, 'mid.f90'), [character(len=60) :: &
            'module mid_mod', 'use low_mod, only: low_value', 'implicit none', &
            'contains', 'integer function mid_value()', &
            '  mid_value = low_value() + 10', 'end function', 'end module'])
        call write_lines(join_path(root, 'top.f90'), [character(len=60) :: &
            'program top', 'use mid_mod, only: mid_value', 'implicit none', &
            'print "(i0)", mid_value()', 'end program'])
        environment = fo_environment(.false., .false.)
        call list_add(environment, 'FO_CMAKE_ARGS=-DCMAKE_Fortran_COMPILER='// &
            join_path(shim_dir, 'gfortran-count'))
        call list_add(environment, 'FO_TEST_REAL_FC='//fortran)
        call list_add(environment, 'FO_TEST_FC_LOG='//compile_log)
        call list_add(args, 'build')
        call run_fo(driver, args, root, cache, observed, environment)
        call assert_process_ok(observed, 'cold build of the chain')
        call assert_equal_string(run_text(join_path(root, 'build/top')), '11'//nl, &
            'cold chain output')
        call assert_equal_integer(line_count(read_text(compile_log), 'low.f90'), 1, &
            'cold build compiles low.f90 once')
        call assert_equal_integer(line_count(read_text(compile_log), 'mid.f90'), 1, &
            'cold build compiles mid.f90 once')
        call assert_equal_integer(line_count(read_text(compile_log), 'top.f90'), 1, &
            'cold build compiles top.f90 once')

        ! Body-only edit: one compile, relink, new output.
        call remove_path(compile_log)
        call write_chain(root, '2', 'integer, parameter :: marker = 1')
        call run_fo(driver, args, root, cache, observed, environment)
        call assert_process_ok(observed, 'build after a body-only edit')
        call assert_equal_integer(line_count(read_text(compile_log), 'low.f90'), 1, &
            'the edited module is compiled')
        call assert_equal_integer(line_count(read_text(compile_log), 'mid.f90'), 0, &
            'its dependent is not compiled for a body-only edit')
        call assert_equal_integer(line_count(read_text(compile_log), 'top.f90'), 0, &
            'the program is not compiled for a body-only edit')
        call assert_equal_string(run_text(join_path(root, 'build/top')), '12'//nl, &
            'the relinked executable has the new body')

        ! Interface edit: dependents are compiled again.
        call remove_path(compile_log)
        call write_chain(root, '2', 'integer, parameter :: marker = 2')
        call run_fo(driver, args, root, cache, observed, environment)
        call assert_process_ok(observed, 'build after an interface edit')
        call assert_equal_integer(line_count(read_text(compile_log), 'low.f90'), 1, &
            'the edited interface is compiled')
        call assert_equal_integer(line_count(read_text(compile_log), 'mid.f90'), 1, &
            'a dependent of a changed interface is compiled')
    end subroutine interface_cutoff

    subroutine write_relocation_project(root, low_body)
        !! A subdirectory with an OBJECT library consumed by a static library,
        !! a generic interface (`module procedure`, no .smod) and a source
        !! that defines a module and the program using it; debug information
        !! and `-mtune=native`, which keys the host CPU after the source.
        character(len=*), intent(in) :: root, low_body
        character(len=100), allocatable :: lines(:)
        character(len=100) :: value_line

        value_line = '  part = '//low_body
        lines = [character(len=100) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(relocate Fortran)', &
            'set(CMAKE_Fortran_FLAGS "-O1 -g -mtune=native")', &
            'add_subdirectory(sub)', &
            'add_executable(top top.f90)', &
            'target_link_libraries(top low parts)']
        call write_lines(join_path(root, 'CMakeLists.txt'), lines)
        lines = [character(len=100) :: &
            'add_library(parts OBJECT parts.f90)', &
            'add_library(low STATIC low.f90 $<TARGET_OBJECTS:parts>)', &
            'add_dependencies(low parts)']
        call write_lines(join_path(root, 'sub/CMakeLists.txt'), lines)
        lines = [character(len=100) :: &
            'module parts_mod', 'implicit none', 'contains', &
            'integer function part()', value_line, 'end function', &
            'end module']
        call write_lines(join_path(root, 'sub/parts.f90'), lines)
        lines = [character(len=100) :: &
            'module low_mod', 'use parts_mod, only: part', 'implicit none', &
            'interface low_value', '  module procedure low_int, low_real', &
            'end interface', 'contains', 'integer function low_int(k)', &
            '  integer, intent(in) :: k', '  low_int = k + part()', &
            'end function', 'real function low_real(x)', &
            '  real, intent(in) :: x', '  low_real = x', 'end function', &
            'end module']
        call write_lines(join_path(root, 'sub/low.f90'), lines)
        lines = [character(len=100) :: &
            'module top_helper', 'use low_mod, only: low_value', 'implicit none', &
            'contains', 'integer function helper()', '  helper = low_value(10)', &
            'end function', 'end module', 'program top', &
            'use top_helper, only: helper', 'implicit none', &
            'print "(i0)", helper()', 'end program']
        call write_lines(join_path(root, 'top.f90'), lines)
    end subroutine write_relocation_project

    integer function project_compiles() result(n)
        !! Compiles of the relocation project's sources (configure-time
        !! compiler checks excluded).
        character(:), allocatable :: text

        text = read_text(compile_log)
        n = line_count(text, 'parts.f90') + line_count(text, 'low.f90') + &
            line_count(text, 'top.f90')
    end function project_compiles

    subroutine shared_dependency_relocation()
        character(:), allocatable :: first, second, shared
        type(string_list_t) :: args, environment
        type(process_result_t) :: observed
        integer :: i
        character(:), allocatable :: tree

        first = join_path(scratch, 'external-consumer-a')
        second = join_path(scratch, 'external-consumer-longer-b')
        shared = join_path(scratch, 'external/parts.f90')
        call write_lines(shared, [character(len=60) :: &
            'module parts_mod', 'contains', 'integer function part()', &
            'part = 1', 'end function', 'end module'])
        do i = 1, 2
            tree = first
            if (i == 2) tree = second
            call write_lines(tree//'/CMakeLists.txt', [character(len=1024) :: &
                'cmake_minimum_required(VERSION 3.20)', 'project(external Fortran)', &
                'set(CMAKE_Fortran_FLAGS "-O1 -g")', &
                'add_executable(top top.f90 "'//shared//'")'])
            call write_lines(tree//'/top.f90', [character(len=60) :: &
                'program top', 'use parts_mod', 'print "(i0)", part()', 'end program'])
        end do
        environment = fo_environment(.false., .false.)
        call list_add(environment, 'FO_CACHE_RELOCATABLE=1')
        call list_add(environment, 'FO_CMAKE_ARGS=-DCMAKE_Fortran_COMPILER='// &
            join_path(shim_dir, 'gfortran-count'))
        call list_add(environment, 'FO_TEST_REAL_FC='//fortran)
        call list_add(environment, 'FO_TEST_FC_LOG='//compile_log)
        call list_add(args, 'build')
        call remove_path(compile_log)
        call run_fo(driver, args, first, cache, observed, environment)
        call assert_process_ok(observed, 'build a consumer of a shared external source')
        call assert_equal_integer(project_compiles(), 2, 'two cold external actions')
        call remove_path(compile_log)
        call run_fo(driver, args, second, cache, observed, environment)
        call assert_process_ok(observed, 'build the relocated external consumer')
        call assert_equal_integer(project_compiles(), 0, &
            'a shared external source reuses debug objects after its consumer moves')
        call assert_equal_string(run_text(second//'/build/top'), '1'//nl, &
            'relocated external consumer keeps its result')
        call assert_equal_string(read_text(first//'/build/top'), &
            read_text(second//'/build/top'), 'external debug executables are identical')
    end subroutine shared_dependency_relocation

    subroutine shared_library_keeps_its_runtime_path()
        character(:), allocatable :: first, second, tree
        type(string_list_t) :: args, environment
        type(process_result_t) :: observed
        integer :: i

        ! Mach-O records its install name in the library bytes themselves.
        if (is_macos()) return
        first = join_path(scratch, 'shared-a')
        second = join_path(scratch, 'shared-copy-b')
        environment = fo_environment(.false., .false.)
        call list_add(environment, 'FO_CACHE_RELOCATABLE=1')
        call list_add(environment, 'FO_CMAKE_ARGS=-DCMAKE_Fortran_COMPILER='// &
            join_path(shim_dir, 'gfortran-count'))
        call list_add(environment, 'FO_TEST_REAL_FC='//fortran)
        call list_add(environment, 'FO_TEST_FC_LOG='//compile_log)
        call list_add(args, 'build')
        do i = 1, 2
            tree = first
            if (i == 2) tree = second
            call write_lines(tree//'/CMakeLists.txt', [character(len=160) :: &
                'cmake_minimum_required(VERSION 3.20)', 'project(shared Fortran)', &
                'set(CMAKE_SKIP_RPATH TRUE)', 'set(CMAKE_Fortran_FLAGS "-O1 -g0")', &
                'add_library(dep SHARED dep.f90)', &
                'set_target_properties(dep PROPERTIES NO_SONAME TRUE '// &
                'Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/mod")', &
                'target_include_directories(dep PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/mod")', &
                'add_executable(top top.f90)', 'target_link_libraries(top dep)'])
            call write_lines(tree//'/dep.f90', [character(len=64) :: &
                'module dep_mod', 'contains', 'integer function value()', &
                'value = 41', 'end function', 'end module'])
            call write_lines(tree//'/top.f90', [character(len=64) :: &
                'program top', 'use dep_mod', 'print "(i0)", value()', 'end program'])
            call remove_path(compile_log)
            call run_fo(driver, args, tree, cache, observed, environment)
            call assert_process_ok(observed, 'build with a shared library without SONAME')
        end do
        call assert_equal_string(read_text(first//'/build/libdep.so'), &
            read_text(second//'/build/libdep.so'), 'shared-library bytes are identical')
        call assert_equal_integer(line_count(read_text(compile_log), 'LINK top'), 1, &
            'a shared-library path remains part of the link identity')
        call remove_tree(first)
        call assert_equal_string(run_text(second//'/build/top'), '41'//nl, &
            'the copied executable uses its own surviving shared library')
    end subroutine shared_library_keeps_its_runtime_path

    subroutine relocated_tree_reuses_results()
        !! A copy of a project at another path reuses every compile result
        !! with FO_CACHE_RELOCATABLE=1, and objects compiled at different paths
        !! are byte-identical; without it results stay keyed by location.
        !! A body edit then compiles exactly the edited file.
        character(:), allocatable :: first, second, third, fresh, objects
        type(string_list_t) :: args, environment, plain
        type(process_result_t) :: observed

        first = join_path(scratch, 'relocate-a')
        ! A path of another length: the key must not depend on it.
        second = join_path(scratch, 'relocate-copy-b')
        third = join_path(scratch, 'relocate-c')
        fresh = join_path(scratch, 'relocate-d')
        call write_relocation_project(first, '1')
        call write_relocation_project(second, '1')
        call write_relocation_project(third, '1')
        plain = fo_environment(.false., .false.)
        call list_add(plain, 'FO_CMAKE_ARGS=-DCMAKE_Fortran_COMPILER='// &
            join_path(shim_dir, 'gfortran-count'))
        call list_add(plain, 'FO_TEST_REAL_FC='//fortran)
        call list_add(plain, 'FO_TEST_FC_LOG='//compile_log)
        environment = plain
        call list_add(environment, 'FO_CACHE_RELOCATABLE=1')
        call list_add(args, 'build')
        call remove_path(compile_log)
        call run_fo(driver, args, first, cache, observed, environment)
        call assert_process_ok(observed, 'cold build of the relocation project')
        call assert_not_contains(route_record(first), 'delegated', &
            'subdirectory, object library and module+program sources are native')
        call assert_equal_string(run_text(join_path(first, 'build/top')), '11'//nl, &
            'cold relocation project output')
        call assert_equal_integer(project_compiles(), 3, &
            'cold build compiles the three sources once each')

        call remove_path(compile_log)
        call run_fo(driver, args, second, cache, observed, environment)
        call assert_process_ok(observed, 'build of the copied project')
        call assert_equal_integer(project_compiles(), 0, &
            'the copy reuses every compile result')
        call assert_equal_integer(line_count(read_text(compile_log), 'LINK top'), 0, &
            'the copy restores its executable without invoking the linker')
        call assert_equal_string(run_text(join_path(second, 'build/top')), '11'//nl, &
            'copied project output')
        ! Independent oracle: the same sources compiled at a third path with
        ! an empty cache give byte-identical debug objects.
        call write_relocation_project(fresh, '1')
        call run_fo(driver, args, fresh, join_path(scratch, 'fo-cache-fresh'), &
            observed, environment)
        call assert_process_ok(observed, 'independent build at another path')
        objects = 'build/.fo-native-cmake/obj/'
        call assert_equal_string(read_text(join_path(fresh, objects//'sub_low.f90.o')), &
            read_text(join_path(first, objects//'sub_low.f90.o')), &
            'debug objects compiled at two paths are identical')
        call assert_equal_string(read_text(join_path(fresh, objects//'top.f90.o')), &
            read_text(join_path(second, objects//'top.f90.o')), &
            'a restored object equals an independently compiled one')

        call remove_path(compile_log)
        call run_fo(driver, args, third, cache, observed, plain)
        call assert_process_ok(observed, 'build of a copy keyed by location')
        call assert_equal_integer(project_compiles(), 3, &
            'without FO_CACHE_RELOCATABLE a copy compiles at its own location')

        call remove_path(compile_log)
        call write_relocation_project(second, '2')
        call run_fo(driver, args, second, cache, observed, environment)
        call assert_process_ok(observed, 'build after a body edit in the copy')
        call assert_equal_integer(project_compiles(), 1, &
            'a body edit compiles exactly the edited file')
        call assert_equal_integer(line_count(read_text(compile_log), 'parts.f90'), 1, &
            'the edited file is the one compiled')
        call assert_equal_string(run_text(join_path(second, 'build/top')), '12'//nl, &
            'edited copy output')

        call remove_path(compile_log)
        call remove_path(second//'/build/top')
        call run_fo(driver, args, second, cache, observed, environment)
        call assert_process_ok(observed, 'restore a deleted native CMake executable')
        call assert_equal_integer(line_count(read_text(compile_log), 'LINK top'), 0, &
            'a deleted executable is restored without relinking')
        call assert_equal_string(run_text(second//'/build/top'), '12'//nl, &
            'the restored executable has the edited implementation')

        call remove_path(compile_log)
        call list_add(environment, &
            'FO_CMAKE_ARGS=-DCMAKE_Fortran_COMPILER='// &
            join_path(shim_dir, 'gfortran-count')//' -DCMAKE_EXE_LINKER_FLAGS=-g')
        call run_fo(driver, args, second, cache, observed, environment)
        call assert_process_ok(observed, 'build after changing declared link flags')
        call assert_equal_integer(line_count(read_text(compile_log), 'LINK top'), 1, &
            'changed link flags invalidate the cached executable')
    end subroutine relocated_tree_reuses_results

end program test_cmake_native_build_cli
