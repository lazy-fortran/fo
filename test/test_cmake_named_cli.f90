program test_cmake_named_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, current_directory
    use fo_test_harness, only: write_lines, remove_path, remove_tree
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_file_exists, assert_file_absent
    use fo_test_harness, only: assert_process_ok, assert_true
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report, exercise_harness
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value
    use fo_test_harness, only: finish_assertions
    use fo_test_harness, only: run_process
    implicit none

    character(:), allocatable :: driver, project, scratch, selected, neighbor
    character(:), allocatable :: selected_receipt, neighbor_receipt
    type(string_list_t) :: arguments, environment
    type(process_result_t) :: result
    type(json_value_t) :: report, tests, entry, field

    selected = 'solovev_axis_regularity'
    neighbor = selected // '_extra'
    call resolve_driver(driver)
    call current_directory(project)
    call make_scratch('fo-cmake-named-cli', scratch)
    selected_receipt = join_path(scratch, 'build/' // selected // '.ran')
    neighbor_receipt = join_path(scratch, 'build/' // neighbor // '.ran')
    call exercise_harness()

    call write_lines(join_path(scratch, 'CMakeLists.txt'), [character(len=192) :: &
        'cmake_minimum_required(VERSION 3.20)', &
        'project(fo_cmake_named_cli NONE)', &
        'enable_testing()', &
        'add_test(NAME solovev_axis_regularity COMMAND "${CMAKE_COMMAND}" -E touch ' // &
        '"${CMAKE_BINARY_DIR}/solovev_axis_regularity.ran")', &
        'add_test(NAME solovev_axis_regularity_extra COMMAND "${CMAKE_COMMAND}" -E ' // &
        'touch "${CMAKE_BINARY_DIR}/solovev_axis_regularity_extra.ran")'])
    call list_add(environment, 'FO_BACKEND=cmake')
    call list_add(environment, 'FO_JOBS=2')
    call list_add(arguments, 'test')
    call list_add(arguments, selected)
    call list_add(arguments, '--json')
    call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
    call assert_process_ok(result, 'named CTest request')
    call parse_json_report(result, report, 'named CTest report')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), 1, &
        'report retains only the exact requested CTest ID')
    entry = json_element(tests, 1)
    field = json_member(entry, 'name')
    call assert_equal_string(json_string_value(field), selected, 'reported CTest ID')
    field = json_member(entry, 'status')
    call assert_equal_string(json_string_value(field), 'pass', 'requested CTest passes')
    call assert_file_exists(selected_receipt, 'requested CTest case executes')
    call assert_file_absent(neighbor_receipt, 'similarly named CTest case stays unrun')

    call remove_path(selected_receipt)
    arguments = string_list_t()
    call list_add(arguments, 'test')
    call list_add(arguments, 'not_a_registered_ctest_id')
    call list_add(arguments, '--json')
    call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
    call assert_equal_integer(result%term_signal, 0, 'unknown CTest ID exits normally')
    call assert_true(result%exit_code /= 0, 'unknown CTest ID fails instead of reporting zero tests')
    call assert_file_absent(selected_receipt, 'no-match does not run a registered case')
    call assert_file_absent(neighbor_receipt, 'no-match does not run a similar case')

    call remove_tree(scratch)
    call profile_isolation()
    call finish_assertions()
    write(*, '(a)') 'cmake-named-cli: exact CTest ID ran alone; no-match failed'
contains
    subroutine profile_isolation()
        character(:), allocatable :: root
        type(string_list_t) :: args, env
        type(process_result_t) :: observed

        call make_scratch('fo-cmake-profile', root)
        call write_lines(join_path(root, 'CMakeLists.txt'), [character(len=160) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(profile_isolation Fortran)', &
            'enable_testing()', &
            'add_executable(numeric numeric.f90)', &
            'target_compile_definitions(numeric PRIVATE '// &
            '"ACTIVE_CONFIGURATION=\"${CMAKE_BUILD_TYPE}\"")', &
            'add_test(NAME numeric COMMAND numeric "${CMAKE_BUILD_TYPE}")'])
        call write_lines(join_path(root, 'CMakePresets.json'), [character(len=192) :: &
            '{"version":3,"configurePresets":[{"name":"cpu","generator":"Ninja",', &
            '"binaryDir":"${sourceDir}/build","cacheVariables":{', &
            '"CMAKE_BUILD_TYPE":"Release",', &
            '"CMAKE_Fortran_COMPILER":"gfortran",', &
            '"CMAKE_Fortran_FLAGS":"-cpp -DKEEP_PROJECT",', &
            '"CMAKE_Fortran_FLAGS_RELEASE":"-O2"}}],', &
            '"buildPresets":[{"name":"cpu","configurePreset":"cpu"}]}'])
        call write_lines(join_path(root, 'numeric.f90'), [character(len=160) :: &
            'program numeric', &
            'use iso_fortran_env, only: compiler_options, real64', &
            'implicit none', &
            'character(32)::configuration', &
            'character(:),allocatable::options', &
            'real(real64)::a(2,2),x(2),y(2)', &
            'real(real64),allocatable::bounds_probe(:)', &
            'integer::requested_index', &
            'call get_command_argument(1,configuration)', &
            'if(trim(configuration)/=ACTIVE_CONFIGURATION) '// &
            'error stop "executable configured for another profile"', &
            'options=compiler_options()', &
            '#ifndef KEEP_PROJECT', &
            'error stop "project global flags were overwritten"', &
            '#endif', &
            'select case(trim(configuration))', &
            'case("Debug","Checked")', &
            'if(index(options,"-fcheck=all")==0) error stop "missing Debug checks"', &
            'case("Release")', &
            'if(index(options,"-fcheck")/=0) '// &
            'error stop "Release inherited Debug checks"', &
            'if(index(options,"-fbacktrace")/=0) '// &
            'error stop "Release inherited backtrace"', &
            'if(index(options,"-O0")/=0) '// &
            'error stop "Release inherited Debug optimization"', &
            'if(index(options,"-O2")==0) error stop "Release preset flags lost"', &
            'case default', &
            'error stop "wrong profile configuration"', &
            'end select', &
            'a=reshape([2d0,-1d0,1d0,2d0],[2,2]);x=[.4d0,2.2d0]', &
            'y=matmul(a,x)', &
            'if(maxval(abs(y-[3d0,4d0]))>1d-14) error stop "numeric oracle"', &
            'if(command_argument_count()>1)then', &
            'call get_command_argument(2,configuration)', &
            'read(configuration,*) requested_index', &
            'allocate(bounds_probe(1));bounds_probe=1', &
            'print *,bounds_probe(requested_index)', &
            'end if', &
            'end program'])
        call list_add(env, 'FO_BACKEND=cmake')
        ! Fo's native compiler preference must not override the CMake preset.
        call list_add(env, 'FO_FC=flang')
        call list_add(env, 'FO_CMAKE_CONFIGURE_PRESET=cpu')
        call list_add(env, 'FO_CMAKE_BUILD_PRESET=cpu')
        call list_add(env, 'FO_CMAKE_BUILD_DIR=build')
        call list_add(env, 'FO_CMAKE_CONFIG=')
        call list_add(env, 'FO_JOBS=2')
        call list_add(args, 'test')
        call list_add(args, '--profile')
        call list_add(args, 'debug')
        call list_add(args, 'numeric')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_process_ok(observed, &
            'public Debug profile uses checked Debug executable')

        args = string_list_t()
        call list_add(args, join_path(root, 'build/numeric'))
        call list_add(args, 'Debug')
        call list_add(args, '2')
        call run_process(args, root, observed)
        call assert_true(observed%exit_code /= 0, &
            'Debug executable rejects a dynamic out-of-bounds index')
        call assert_true(index(observed%stderr, 'above upper bound') > 0, &
            'Debug failure is the independent runtime bounds check')

        ! Native preset reconfiguration shares the exact build directory.
        args = string_list_t()
        call list_add(args, 'cmake')
        call list_add(args, '--preset')
        call list_add(args, 'cpu')
        call run_process(args, root, observed)
        call assert_process_ok(observed, 'native Release preset after Fo Debug')
        args = string_list_t()
        call list_add(args, 'cmake')
        call list_add(args, '--build')
        call list_add(args, 'build')
        call run_process(args, root, observed)
        call assert_process_ok(observed, 'native Release rebuild')
        args = string_list_t()
        call list_add(args, 'ctest')
        call list_add(args, '--test-dir')
        call list_add(args, 'build')
        call list_add(args, '--output-on-failure')
        call run_process(args, root, observed)
        call assert_process_ok(observed, &
            'Release numeric executable excludes Debug flags')

        args = string_list_t()
        call list_add(args, 'build')
        call list_add(args, '--flag')
        call list_add(args, '-fcheck=all')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_true(observed%exit_code /= 0, &
            'ambiguous CMake flag scope is refused')
        call assert_true(index(observed%stderr, '--profile or FO_CMAKE_CONFIG') > 0, &
            'ambiguous flag diagnostic names configuration choices')

        ! An explicit custom configuration is authoritative over profile inference.
        env = string_list_t()
        call list_add(env, 'FO_BACKEND=cmake')
        call list_add(env, 'FO_CMAKE_CONFIGURE_PRESET=cpu')
        call list_add(env, 'FO_CMAKE_BUILD_PRESET=cpu')
        call list_add(env, 'FO_CMAKE_BUILD_DIR=build')
        call list_add(env, 'FO_CMAKE_CONFIG=Checked')
        call list_add(env, 'FO_JOBS=2')
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, '--profile')
        call list_add(args, 'debug')
        call list_add(args, 'numeric')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_process_ok(observed, &
            'explicit custom configuration with Debug checks')
        args = string_list_t()
        call list_add(args, join_path(root, 'build/numeric'))
        call list_add(args, 'Checked')
        call run_process(args, root, observed)
        call assert_process_ok(observed, &
            'numeric executable retains explicit custom configuration')
        call remove_tree(root)
    end subroutine profile_isolation
end program test_cmake_named_cli
