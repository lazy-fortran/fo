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
    !!    header edit reaches the C object;
    !!  - a target with a generated source is delegated, built before its
    !!    dependents, and only that target reaches `cmake --build`;
    !!  - interface cutoff, observed with a compiler shim that records every
    !!    compile: a body-only edit of a low-level module compiles exactly that
    !!    file and relinks, an interface edit compiles its dependents.
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
    implicit none

    character(:), allocatable :: driver, scratch, cache, fortran, shim_dir
    character(:), allocatable :: cmake_log, compile_log
    character(len=1), parameter :: nl = achar(10)

    call resolve_driver(driver)
    call make_scratch('fo-cmake-native-build', scratch)
    cache = join_path(scratch, 'fo-cache')
    shim_dir = join_path(scratch, 'shim')
    cmake_log = join_path(scratch, 'cmake-build.log')
    compile_log = join_path(scratch, 'compile.log')
    fortran = find_tool('gfortran')
    call assert_true(len(fortran) > 0, 'a gfortran is on PATH')
    if (len(fortran) == 0) call finish_assertions()
    call build_shims()

    call twin_verdicts()
    call mixed_project()
    call generated_source_is_delegated()
    call duplicate_modules_are_delegated()
    call interface_cutoff()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'cmake-native-build-cli: native build matches cmake --build'

contains

    function find_tool(name) result(path)
        character(len=*), intent(in) :: name
        character(:), allocatable :: path
        type(string_list_t) :: command
        type(process_result_t) :: observed

        call list_add(command, 'sh')
        call list_add(command, '-c')
        call list_add(command, 'command -v '//name)
        call run_process(command, scratch, observed)
        path = ''
        if (observed%exit_code /= 0) return
        path = trim(observed%stdout)
        if (index(path, nl) > 0) path = path(:index(path, nl) - 1)
    end function find_tool

    subroutine build_shims()
        !! One compiled Fortran program, installed as `cmake` (refuses and
        !! records `--build`, passes everything else to the real cmake) and as
        !! `gfortran-count` (records each compile, then runs the real gfortran).
        type(string_list_t) :: command
        type(process_result_t) :: observed
        character(:), allocatable :: source

        source = join_path(scratch, 'shim.f90')
        call write_lines(source, [character(len=100) :: &
            'program shim', &
            'implicit none', &
            'character(len=4096) :: arg, program_name, real_tool, log_name', &
            'character(len=:), allocatable :: command, last, joined', &
            'integer :: i, status, unit, slash', &
            'logical :: compiling, is_cmake', &
            'call get_command_argument(0, program_name)', &
            'slash = index(trim(program_name), "/", back=.true.)', &
            'is_cmake = trim(program_name(slash + 1:)) == "cmake"', &
            'if (is_cmake) then', &
            '  call get_environment_variable("FO_TEST_REAL_CMAKE", real_tool)', &
            '  call get_environment_variable("FO_TEST_CMAKE_LOG", log_name)', &
            'else', &
            '  call get_environment_variable("FO_TEST_REAL_FC", real_tool)', &
            '  call get_environment_variable("FO_TEST_FC_LOG", log_name)', &
            'end if', &
            'command = "''"//trim(real_tool)//"''"', &
            'joined = ""', &
            'compiling = .false.', &
            'last = ""', &
            'do i = 1, command_argument_count()', &
            '  call get_command_argument(i, arg)', &
            '  if (trim(arg) == "-c") compiling = .true.', &
            '  last = trim(arg)', &
            '  joined = joined//" "//trim(arg)', &
            '  command = command//" ''"//quoted(trim(arg))//"''"', &
            'end do', &
            'if (is_cmake .and. command_argument_count() > 0) then', &
            '  call get_command_argument(1, arg)', &
            '  if (trim(arg) == "--build") then', &
            '    open (newunit=unit, file=trim(log_name), position="append", &', &
            '        status="unknown")', &
            '    write (unit, "(a)") trim(joined)', &
            '    close (unit)', &
            '    call get_environment_variable("FO_TEST_CMAKE_PASS", arg)', &
            '    if (len_trim(arg) == 0) call exit(97)', &
            '  end if', &
            'else if (.not. is_cmake .and. compiling .and. len_trim(log_name) > 0) then', &
            '  open (newunit=unit, file=trim(log_name), position="append", &', &
            '      status="unknown")', &
            '  write (unit, "(a)") "COMPILE "//last', &
            '  close (unit)', &
            'end if', &
            'call execute_command_line(command, exitstat=status)', &
            'call exit(status)', &
            'contains', &
            'function quoted(text) result(r)', &
            '  character(len=*), intent(in) :: text', &
            '  character(len=:), allocatable :: r', &
            '  integer :: k', &
            '  r = ""', &
            '  do k = 1, len(text)', &
            '    if (text(k:k) == "''") then', &
            '      r = r//"''"//achar(92)//"''''"', &
            '    else', &
            '      r = r//text(k:k)', &
            '    end if', &
            '  end do', &
            'end function quoted', &
            'end program shim'])
        call list_add(command, fortran)
        call list_add(command, source)
        call list_add(command, '-o')
        call list_add(command, join_path(shim_dir, 'cmake'))
        call write_text(join_path(shim_dir, '.keep'), '')
        call run_process(command, scratch, observed)
        call assert_process_ok(observed, 'compile the cmake shim')
        command = string_list_t()
        call list_add(command, fortran)
        call list_add(command, source)
        call list_add(command, '-o')
        call list_add(command, join_path(shim_dir, 'gfortran-count'))
        call run_process(command, scratch, observed)
        call assert_process_ok(observed, 'compile the compiler shim')
    end subroutine build_shims

    function fo_environment(delegate, with_shims) result(environment)
        logical, intent(in) :: delegate, with_shims
        type(string_list_t) :: environment

        call list_add(environment, 'FO_BACKEND=cmake')
        call list_add(environment, 'FO_JOBS=2')
        if (delegate) call list_add(environment, 'FO_CMAKE_DELEGATE_BUILD=1')
        if (.not. with_shims) return
        call list_add(environment, 'PATH='//shim_dir//':'//environment_value('PATH'))
        call list_add(environment, 'FO_TEST_REAL_CMAKE='//find_tool('cmake'))
        call list_add(environment, 'FO_TEST_CMAKE_LOG='//cmake_log)
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

        call write_lines(join_path(dir, 'CMakeLists.txt'), [character(len=120) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(mixed C Fortran)', &
            'enable_testing()', &
            'add_library(core STATIC core/core.F90 core/gate.f90 core/gate_alt.f90 '// &
            'core/cyc_x.F90 core/cyc_a.f90 core/scale.c)', &
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
            'add_executable(app app/main.f90)', &
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
        call write_lines(join_path(dir, 'core/core.F90'), [character(len=70) :: &
            'module core_mod', '#ifdef GATE_ALT', 'use gate_alt, only: gate_offset', &
            '#else', 'use gate_mod, only: gate_offset', '#endif', &
            'use, intrinsic :: iso_c_binding, only: c_int', 'implicit none', &
            'interface', '  integer(c_int) function core_scale(x) bind(C, name="core_scale")', &
            '    import :: c_int', '    integer(c_int), value :: x', &
            '  end function', 'end interface', 'contains', &
            'integer function core_value(x)', '  integer, intent(in) :: x', &
            '  core_value = int(core_scale(int(x, c_int))) + gate_offset()', &
            'end function', 'end module'])
        call write_lines(join_path(dir, 'core/scale.c'), [character(len=60) :: &
            '#include "scale.h"', 'int core_scale(int x) {', &
            '  return x * CORE_SCALE + SCALE_BASE;', '}'])
        call write_lines(join_path(dir, 'core/include/scale.h'), [character(len=40) :: &
            '#define SCALE_BASE 5'])
        call write_lines(join_path(dir, 'util/util.f90'), [character(len=60) :: &
            'module util_mod', 'use core_mod, only: core_value', 'implicit none', &
            'contains', 'integer function util_value()', &
            '  util_value = core_value(2) * 2', 'end function', 'end module'])
        call write_lines(join_path(dir, 'app/main.f90'), [character(len=70) :: &
            'program app', 'use util_mod, only: util_value', &
            'use cyc_a, only: cyc_a_value', 'implicit none', &
            'print "(a,i0,1x,i0)", "mixed: ", util_value(), cyc_a_value', &
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
        call run_fo(driver, args, source, cache, observed, environment)
        call assert_process_ok(observed, 'warm rebuild')
        call assert_equal_string(run_text(join_path(source, 'build/app')), &
            'mixed: 230 7'//nl, 'warm rebuild keeps the output')

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

        call write_lines(join_path(root, 'low.f90'), [character(len=70) :: &
            'module low_mod', 'implicit none', interface_extra, 'contains', &
            'integer function low_value()', '  low_value = '//body, &
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

end program test_cmake_native_build_cli
