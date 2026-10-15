program test_native_cmake_gorilla_cli
    !! A GORILLA-shaped CMake tree (the command shapes of GORILLA's default
    !! profile: host conditionals, BLAS/LAPACK providers, a nested static
    !! library with a module directory, $ENV conditionals) configures, builds
    !! and runs through the opt-in native interpreter with trap cmake/ctest on
    !! PATH. The GORILLA_COVERAGE branch needs pFUnit and must say so precisely.
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
                          make_scratch, make_directory, write_text, environment_value, &
                    run_process, assert_process_ok, assert_true, assert_equal_integer, &
          assert_equal_string, assert_file_exists, assert_file_absent, finish_assertions
    use fo_test_harness, only: resolve_test_executable
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size, &
                            json_string_value
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, self
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: args, env, base_env
    type(process_result_t) :: process
    type(json_value_t) :: report, tests, entry
    logical :: executable_ok

    call resolve_test_executable(self, executable_ok)
    call assert_true(executable_ok, 'resolves the native test executable')
    if (.not. executable_ok) call finish_assertions()
    if (index(self, '/cmake') > 0 .or. index(self, '/ctest') > 0) then
        call write_text(environment_value('FO_FORBIDDEN_TOOL_MARKER'), self)
        error stop 'native route invoked a forbidden delegated tool'
    end if
    call resolve_driver(driver)
    call make_scratch('fo-native-cmake-gorilla', scratch)
    project = scratch//'/project'
    cache = scratch//'/cache'
    call make_directory(project//'/SRC/contrib')
    call make_directory(scratch//'/shadow')
    call write_project()
    call shadow_tool('cmake')
    call shadow_tool('ctest')
    call list_add(base_env, 'FO_BACKEND=cmake')
    call list_add(base_env, 'FO_CMAKE_NATIVE=1')
    call list_add(base_env, 'FO_FC=gfortran')
    call list_add(base_env, 'FO_CMAKE_BUILD_DIR=native')
    call list_add(base_env, 'FO_CMAKE_CONFIG=Release')
    call list_add(base_env, 'FO_CMAKE_CONFIGURE_PRESET=')
    call list_add(base_env, 'FO_CMAKE_BUILD_PRESET=')
    call list_add(base_env, 'FO_CMAKE_TEST_PRESET=')
    call list_add(base_env, 'FO_CMAKE_ARGS=')
    call list_add(base_env, 'FO_FORBIDDEN_TOOL_MARKER='//scratch//'/delegated.ran')
    call list_add(base_env, 'PATH='//scratch//'/shadow:'//environment_value('PATH'))

    ! Default profile: no GORILLA_COVERAGE and no registered tests.
    env = base_env
    call list_add(env, 'GORILLA_COVERAGE=')
    call list_add(env, 'FX_TESTING=')
    args = string_list_t()
    call list_add(args, 'build')
    call run_fo(driver, args, project, cache, process, env, 120000)
    call assert_process_ok(process, 'GORILLA-shaped default profile builds natively')
    call assert_file_exists(project//'/native/SRC/libgorillafx.a', &
                            'nested static library lands in its binary directory')
    call assert_file_exists(project//'/native/test_fx.x', &
                            'executable lands in the root binary directory')
    call assert_file_absent(scratch//'/delegated.ran', &
                            'default profile invokes neither cmake nor ctest')
    args = string_list_t()
    call list_add(args, 'exec')
    call list_add(args, '--no-build')
    call list_add(args, 'test_fx.x')
    call list_add(args, '42')
    call run_fo(driver, args, project, cache, process, env, 60000)
    call assert_process_ok(process, 'built executable runs')
    call assert_true(index(process%stdout, 'fx-contrib 7 arg 42') > 0, &
                     'executable combines contrib module and argument')
    args = string_list_t()
    call list_add(args, 'test')
    call run_fo(driver, args, project, cache, process, env, 60000)
    call assert_process_ok(process, 'no registered tests is not a failure')
    call assert_true(index(process%stdout, 'project registers no tests') > 0, &
                     'no-test verdict matches the delegated route')

    ! Registered test: provider variables and exact verdict.
    env = base_env
    call list_add(env, 'GORILLA_COVERAGE=')
    call list_add(env, 'FX_TESTING=TRUE')
    args = string_list_t()
    call list_add(args, 'test')
    call list_add(args, 'fx_registered')
    call list_add(args, '--json')
    call run_fo(driver, args, project, cache, process, env, 120000)
    call assert_process_ok(process, 'registered GORILLA-shaped test passes natively')
    call parse_json_report(process, report, 'native registered test report')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), 1, 'one registered test')
    entry = json_element(tests, 1)
    call assert_equal_string(json_string_value(json_member(entry, 'status')), &
                             'pass', 'registered test verdict')
    call assert_file_absent(scratch//'/delegated.ran', &
                            'registered test invokes neither cmake nor ctest')

    ! Coverage profile needs pFUnit: explicit diagnostic, no silent skip.
    env = base_env
    call list_add(env, 'GORILLA_COVERAGE=TRUE')
    call list_add(env, 'FX_TESTING=')
    args = string_list_t()
    call list_add(args, 'build')
    call run_fo(driver, args, project, cache, process, env, 120000)
    call assert_true(process%exit_code /= 0, 'pFUnit profile is not silently skipped')
    call assert_true(index(process%stdout//process%stderr, &
                     'unsupported package provider PFUNIT') > 0, &
                     'pFUnit profile reports the missing provider')
    call assert_file_absent(scratch//'/delegated.ran', &
                            'unsupported provider never falls back to cmake')
    call finish_assertions()
contains
    subroutine write_project()
        character(:), allocatable :: expect_blas, expect_lapack
        expect_blas = '.false.'
        expect_lapack = '.false.'
        if (links('blas')) expect_blas = '.true.'
        if (links('lapack')) expect_lapack = '.true.'
        call write_text(project//'/CMakeLists.txt', &
            'cmake_minimum_required(VERSION 3.12)'//nl// &
            'project (GORILLAFX'//nl//'  VERSION 1.0.0)'//nl// &
            'enable_language(Fortran)'//nl// &
            'include_directories(/usr/local/include)'//nl// &
            'if(UNIX AND NOT APPLE)'//nl//'	include_directories(/usr/include)'//nl// &
            'elseif(APPLE AND ${CMAKE_HOST_SYSTEM_PROCESSOR} STREQUAL "arm64")'//nl// &
            '	include_directories(/opt/homebrew/include)'//nl//'endif()'//nl// &
            'set(MARK_BLAS missing)'//nl//'set(MARK_LAPACK missing)'//nl// &
            'find_package(BLAS)'//nl//'find_package(LAPACK)'//nl// &
            'if(BLAS_FOUND)'//nl//'	set(MARK_BLAS found)'//nl//'endif()'//nl// &
            'if(LAPACK_FOUND)'//nl//'	set(MARK_LAPACK found)'//nl//'endif()'//nl// &
            'if ("$ENV{GORILLA_COVERAGE}" STREQUAL "TRUE")'//nl// &
            '	find_package(PFUNIT REQUIRED)'//nl//'endif()'//nl// &
            'if ("$ENV{FX_TESTING}" STREQUAL "TRUE")'//nl//'	enable_testing()'//nl// &
            'endif()'//nl// &
            'add_compile_options(-g -fbacktrace -ffpe-trap=zero,overflow,invalid '// &
            '-fbounds-check -fopenmp)'//nl// &
            'add_link_options(-g -fbacktrace -ffpe-trap=zero,overflow,invalid '// &
            '-fbounds-check -fopenmp)'//nl// &
            'add_link_options(-L/usr/local/lib)'//nl// &
            'add_subdirectory(SRC)'//nl// &
            'add_executable(test_fx.x'//nl//'  SRC/test_fx.f90'//nl//')'//nl// &
            'target_link_libraries(test_fx.x gorillafx)'//nl// &
            'if ("$ENV{FX_TESTING}" STREQUAL "TRUE")'//nl// &
            '	add_test(NAME fx_registered COMMAND test_fx.x 42 ${MARK_BLAS} '// &
            '${MARK_LAPACK})'//nl//'endif()'//nl// &
            'message(${CMAKE_HOST_SYSTEM_PROCESSOR})'//nl)
        call write_text(project//'/SRC/CMakeLists.txt', &
            'add_library(gorillafx STATIC'//nl//'	contrib/fx_contrib.f90'//nl// &
            '	fx_mod.f90'//nl//'	)'//nl// &
            'set_target_properties (gorillafx PROPERTIES'//nl// &
            '  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})'//nl// &
            'target_include_directories(gorillafx PUBLIC ${CMAKE_CURRENT_BINARY_DIR})'// &
            nl)
        call write_text(project//'/SRC/contrib/fx_contrib.f90', &
            'module fx_contrib'//nl//'contains'//nl// &
            'integer function seven()'//nl//'seven = 7'//nl//'end function'//nl// &
            'end module'//nl)
        call write_text(project//'/SRC/fx_mod.f90', &
            'module fx_mod'//nl//'use fx_contrib, only: seven'//nl//'contains'//nl// &
            'integer function contrib_value()'//nl//'contrib_value = seven()'//nl// &
            'end function'//nl//'end module'//nl)
        ! A directly linkable library must be reported FOUND; a host may also
        ! provide one through pkg-config or Accelerate, so absence is not checked.
        call write_text(project//'/SRC/test_fx.f90', &
            'program test_fx'//nl//'use fx_mod, only: contrib_value'//nl// &
            'implicit none'//nl//'character(32) :: argument, blas, lapack'//nl// &
            'call get_command_argument(1, argument)'//nl// &
            'print "(a,i0,a,a)", "fx-contrib ", contrib_value(), " arg ", &'//nl// &
            '  trim(argument)'//nl// &
            'if (command_argument_count() >= 3) then'//nl// &
            '  call get_command_argument(2, blas)'//nl// &
            '  call get_command_argument(3, lapack)'//nl// &
            '  if ('//expect_blas//' .and. trim(blas) /= "found") &'//nl// &
            '    error stop "BLAS provider missed a linkable library"'//nl// &
            '  if ('//expect_lapack//' .and. trim(lapack) /= "found") &'//nl// &
            '    error stop "LAPACK provider missed a linkable library"'//nl// &
            'end if'//nl//'end program'//nl)
    end subroutine

    logical function links(library) result(ok)
        !! Independent oracle: does the compiler link -l<library> directly?
        character(len=*), intent(in) :: library
        type(string_list_t) :: command
        type(process_result_t) :: observed
        call write_text(scratch//'/probe.f90', 'program p'//nl//'end program'//nl)
        call list_add(command, 'gfortran')
        call list_add(command, scratch//'/probe.f90')
        call list_add(command, '-o')
        call list_add(command, scratch//'/probe.x')
        call list_add(command, '-l'//library)
        call run_process(command, scratch, observed)
        ok = observed%exit_code == 0
    end function

    subroutine shadow_tool(name)
        character(len=*), intent(in) :: name
        type(string_list_t) :: command
        type(process_result_t) :: observed
        call list_add(command, 'cp')
        call list_add(command, self)
        call list_add(command, scratch//'/shadow/'//name)
        call run_process(command, project, observed)
        call assert_process_ok(observed, 'prepare fail-on-execution delegated tool guard')
    end subroutine
end program test_native_cmake_gorilla_cli
