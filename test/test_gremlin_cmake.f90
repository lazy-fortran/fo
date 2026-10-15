program test_gremlin_cmake
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
              make_directory, write_text, read_text, assert_true, assert_equal_string, &
                               assert_equal_integer, &
                               finish_assertions, run_process, assert_process_ok
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, &
                   gremlin_start_args, gremlin_wait_ms, gremlin_field, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, &
        json_string_value, json_size, json_element, json_number_value
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: owner, generation, previous, dependency
    character(:), allocatable :: provider, provider_source, revision, raw_archive
    character(:), allocatable :: child_builds, generation_builds, child_runs
    character(:), allocatable :: first_model, first_model_generation
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer :: rc
    interface
        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
        integer(c_int) function c_unlink(path) bind(C, name='unlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_unlink
        integer(c_int) function c_symlink(target, path) bind(C, name='symlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: target(*), path(*)
        end function c_symlink
    end interface

    call gremlin_setup(driver, scratch, project, cache, state)
    provider = scratch//'/provider-build'
    provider_source = scratch//'/provider-source'
    call make_provider()
    call make_populate_only_dependency()
    dependency = scratch//'/dependency'
    call make_directory(dependency)
    call write_text(dependency//'/CMakeLists.txt', &
                    'cmake_minimum_required(VERSION 3.22)'//new_line('a')// &
                    'project(value_dep LANGUAGES Fortran)'//new_line('a')// &
                    'add_library(value_dep package/leaf.f90)'//new_line('a'))
    call write_dependency(7)
    call make_directory(dependency//'/package')
    rc = c_symlink('../value.f90'//c_null_char, &
        dependency//'/package/leaf.f90'//c_null_char)
    call assert_true(rc == 0, 'internal parent-relative dependency link exists')
    rc = c_symlink('/unavailable-unused-source'//c_null_char, &
                   dependency//'/unused-alias'//c_null_char)
    call assert_true(rc == 0, 'unused invalid alias fixture exists')
    call write_text(project//'/CMakeLists.txt', &
                    'cmake_minimum_required(VERSION 3.22)'//new_line('a')// &
                    'project(cmake_resident LANGUAGES Fortran)'//new_line('a')// &
                    'include(CTest)'//new_line('a')// &
            'set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})'//new_line('a')// &
                    'set(VALUE_PROVIDER_BUILD "'//provider//'" CACHE PATH "provider")'// &
                    new_line('a')// &
                    'file(STRINGS "${VALUE_PROVIDER_BUILD}/CMakeCache.txt" origin '// &
                    'REGEX "^CMAKE_HOME_DIRECTORY:INTERNAL=")'//new_line('a')// &
                    'string(REPLACE "CMAKE_HOME_DIRECTORY:INTERNAL=" "" origin '// &
                    '"${origin}")'//new_line('a')// &
                    'execute_process(COMMAND git -C "${origin}" rev-parse HEAD '// &
                    'OUTPUT_VARIABLE actual OUTPUT_STRIP_TRAILING_WHITESPACE '// &
                    'RESULT_VARIABLE git_status)'//new_line('a')// &
                    'if(NOT git_status EQUAL 0 OR NOT actual STREQUAL "'//revision//'" )'// &
                    new_line('a')//'message(FATAL_ERROR "provider revision changed")'// &
                    new_line('a')//'endif()'//new_line('a')// &
                    'set(FETCHCONTENT_BASE_DIR "${CMAKE_BINARY_DIR}/_deps" '// &
                    'CACHE PATH "native dependency build directory")'//new_line('a')// &
                    'include(FetchContent)'//new_line('a')// &
                    'FetchContent_Declare(value_dep SOURCE_DIR "'//dependency//'" )'// &
                    new_line('a')// &
                    'FetchContent_MakeAvailable(value_dep)'//new_line('a')// &
                    'FetchContent_Declare(raw_dep URL "'//raw_archive//'" )'// &
                    new_line('a')// &
                    'FetchContent_GetProperties(raw_dep)'//new_line('a')// &
                    'if(NOT raw_dep_POPULATED)'//new_line('a')// &
                    'FetchContent_Populate(raw_dep)'//new_line('a')// &
                    'endif()'//new_line('a')// &
                    'add_library(raw_dep "${raw_dep_SOURCE_DIR}/raw.f90")'// &
                    new_line('a')// &
                    'add_custom_command(OUTPUT "${CMAKE_CURRENT_SOURCE_DIR}/'// &
                    'unselected_generated.f90" COMMAND "${CMAKE_COMMAND}" -E copy '// &
                    '"${CMAKE_CURRENT_SOURCE_DIR}/marker.f90" '// &
                    '"${CMAKE_CURRENT_SOURCE_DIR}/unselected_generated.f90")'// &
                    new_line('a')// &
                    'add_custom_target(unselected_generate DEPENDS '// &
                    '"${CMAKE_CURRENT_SOURCE_DIR}/unselected_generated.f90")'// &
                    new_line('a')// &
                    'add_executable(unselected_generated_target EXCLUDE_FROM_ALL '// &
                    'unselected_generated.f90)'//new_line('a')// &
                    'add_executable(marker marker.f90)'//new_line('a')// &
                    'add_executable(probe EXCLUDE_FROM_ALL probe.f90)'// &
                    new_line('a')// &
                    'add_test(NAME excluded_probe COMMAND probe)'//new_line('a')// &
                    'set_tests_properties(excluded_probe PROPERTIES '// &
                    'LABELS "probe;unit")'//new_line('a')// &
                    'install(TARGETS marker RUNTIME DESTINATION lib)'//new_line('a')// &
                    'add_executable(unselected broken.f90)'//new_line('a')// &
                    'target_link_libraries(marker PRIVATE value_dep raw_dep)'// &
                    new_line('a')// &
                  'target_include_directories(marker PRIVATE "${CMAKE_BINARY_DIR}" '// &
                    '"${VALUE_PROVIDER_BUILD}/include")'// &
                    new_line('a')// &
                 'file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/sandbox")'//new_line('a')// &
                    'add_test(NAME setup COMMAND "${CMAKE_COMMAND}" '// &
                    '-DSOURCE=${CMAKE_SOURCE_DIR} '// &
                    '-DOUTPUT=${CMAKE_BINARY_DIR}/sandbox/setup.ran '// &
                    '-P "${CMAKE_SOURCE_DIR}/git_check.cmake")'//new_line('a')// &
        'set_tests_properties(setup PROPERTIES FIXTURES_SETUP value)'//new_line('a')// &
                    'add_test(NAME registered_marker COMMAND marker)'//new_line('a')// &
        'set_tests_properties(registered_marker PROPERTIES FIXTURES_REQUIRED value '// &
                    'WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/sandbox" '// &
                    'PASS_REGULAR_EXPRESSION "marker passed")'//new_line('a')// &
                    'add_test(NAME regex_negative COMMAND marker)'//new_line('a')// &
           'set_tests_properties(regex_negative PROPERTIES FIXTURES_REQUIRED value '// &
                    'WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/sandbox" '// &
                    'PASS_REGULAR_EXPRESSION "text never printed")'//new_line('a'))
    call write_text(project//'/broken.f90', 'unselected target is intentionally invalid')
    call write_text(project//'/probe.f90', 'program probe'//new_line('a')// &
        'print *, "probe passed"'//new_line('a')//'end program probe'//new_line('a'))
    call write_marker(.false.)
    call write_text(project//'/CMakePresets.json', &
                    '{"version":3,"configurePresets":[{"name":"cpu",'// &
                    '"generator":"Ninja","binaryDir":"${sourceDir}/build",'// &
                    '"cacheVariables":{"CMAKE_BUILD_TYPE":"Release",'// &
                    '"CMAKE_INSTALL_PREFIX":"'//scratch// &
                    '/uncreated-install-prefix"}}],'// &
                    '"buildPresets":[{"name":"cpu","configurePreset":"cpu"}],'// &
                    '"testPresets":[{"name":"cpu","configurePreset":"cpu"}]}')
    call write_text(project//'/git_check.cmake', &
        'file(STRINGS "${SOURCE}/expected-head" expected)'//new_line('a')// &
        'execute_process(COMMAND git -C "${SOURCE}" rev-parse HEAD '// &
        'OUTPUT_VARIABLE actual OUTPUT_STRIP_TRAILING_WHITESPACE '// &
        'RESULT_VARIABLE status)'//new_line('a')// &
        'if(NOT status EQUAL 0 OR NOT actual STREQUAL expected)'//new_line('a')// &
        'message(FATAL_ERROR "frozen project Git revision changed")'//new_line('a')// &
        'endif()'//new_line('a')// &
        'execute_process(COMMAND git -C "${SOURCE}" status --porcelain '// &
        '--untracked-files=no OUTPUT_VARIABLE dirty RESULT_VARIABLE status)'// &
        new_line('a')// &
        'if(NOT status EQUAL 0 OR dirty STREQUAL "")'//new_line('a')// &
        'message(FATAL_ERROR "captured tracked edits lost Git dirty status")'// &
        new_line('a')//'endif()'//new_line('a')// &
        'file(TOUCH "${OUTPUT}")'//new_line('a'))
    call initialize_project_git()
    call install_tool_trap()
    ! The build counts below observe the delegated `cmake --build` through the
    ! trap; the native route (fo_cmake_build) never invokes it.
    rc = c_setenv('FO_CMAKE_DELEGATE_BUILD'//c_null_char, '1'//c_null_char, 1_c_int)
    rc = c_setenv('FO_CMAKE_CONFIGURE_PRESET'//c_null_char, 'cpu'//c_null_char, 1_c_int)
    rc = c_setenv('FO_CMAKE_BUILD_PRESET'//c_null_char, 'cpu'//c_null_char, 1_c_int)
    ! No FO_CMAKE_BUILD_TARGETS: the generation build plans the targets of the
    ! campaign's cases from the model, so the invalid `unselected` stays unbuilt.
    rc = c_setenv('FO_CMAKE_TEST_PRESET'//c_null_char, 'cpu'//c_null_char, 1_c_int)
    rc = c_setenv('FO_CMAKE_BUILD_DIR'//c_null_char, 'build'//c_null_char, 1_c_int)
    call gremlin_start_args(args, project, 'cmake', 'registered_marker')
    ! An EXCLUDE_FROM_ALL test executable outside FO_CMAKE_BUILD_TARGETS: the
    ! generation build plans it from the model.
    call list_add(args, '--target')
    call list_add(args, 'excluded_probe')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
    call assert_true(process%exit_code == 0, 'CMake resident request starts')
    owner = gremlin_field(reply, 'session_id')
    call wait_result('', 'PASS', generation)
    call assert_true(len(generation) == 64, 'CMake-only generation has an identity')
    call assert_true(.not. file_present(project//'/build/_deps/value_dep-build/'// &
        'libvalue_dep.a'), 'resident archive remains outside the live build directory')
    if (len(generation) /= 64) then
        call gremlin_stop_lane(driver, project, cache, state, 'cmake', owner, &
            allow_terminal_error=.true.)
        call finish_assertions()
    end if
    previous = generation
    call assert_probe_receipt(generation)
    first_model = status_field('active_model')
    first_model_generation = status_field('active_model_generation')
    call assert_true(len(first_model) == 64, &
        'the active generation has a project model')
    call assert_equal_string(first_model_generation, generation, &
        'the first generation loads its own project model')
    call assert_true(file_present(child_runs), 'case children run the CTest case')
    call assert_true(file_present(generation_builds), &
        'the generation build reaches cmake --build through the trap')
    rc = c_unlink(child_runs//c_null_char)
    rc = c_unlink(generation_builds//c_null_char)
    call scoped_lane_phase()
    call write_marker(.true.)
    call wait_result(previous, 'BUILD_FAIL', generation)
    call assert_equal_string(generation, previous, &
                         'invalid candidate retains the last compiled CMake generation')
    call gremlin_wait_ms(1000)
    call assert_build_failure_once()
    call assert_equal_integer(line_count(generation_builds), 1, &
        'the failed candidate is built once by the supervisor')
    call assert_true(.not. file_present(child_runs), &
        'no case child runs for the unbuilt candidate')
    call write_marker(.false.)
    call write_dependency(9)
    call wait_result(previous, 'FAIL', generation)
    call assert_true(generation /= previous, 'external dependency edit wakes resident')
    call assert_equal_string(status_field('active_model'), first_model, &
        'a source-only edit reuses the project model')
    call assert_equal_string(status_field('active_model_generation'), &
        first_model_generation, 'the reused model keeps its loading generation')
    previous = generation
    call write_dependency(7)
    call write_text(project//'/marker.f90', read_text(project//'/marker.f90')// &
                    '! restored candidate'//new_line('a'))
    call wait_result(previous, 'PASS', generation)
    previous = generation
    call write_provider(19)
    call wait_result(previous, 'FAIL', generation)
    call assert_true(generation /= previous, 'prebuilt provider edit wakes resident')
    previous = generation
    call write_provider(17)
    call wait_result(previous, 'PASS', generation)
    previous = generation
    call write_text(project//'/CMakeLists.txt', &
        read_text(project//'/CMakeLists.txt')//'# model input edit'//new_line('a'))
    call wait_result(previous, 'PASS', generation)
    call assert_equal_string(status_field('active_model_generation'), generation, &
        'a CMakeLists edit loads the model again')
    call assert_true(status_field('active_model') /= first_model, &
        'the refreshed model has a new identity')
    previous = generation
    call write_text(scratch//'/outside.f90', read_text(dependency//'/value.f90'))
    rc = c_unlink(dependency//'/package/leaf.f90'//c_null_char)
    call assert_true(rc == 0, 'referenced link can be changed in the fixture')
    rc = c_symlink(scratch//'/outside.f90'//c_null_char, &
                   dependency//'/package/leaf.f90'//c_null_char)
    call assert_true(rc == 0, 'referenced external alias fixture exists')
    call wait_result(previous, 'CAPTURE_FAIL', generation)
    call assert_equal_string(generation, previous, &
                            'unsupported referenced input retains active generation')
    call assert_true(.not. file_present(child_builds), &
        'case children never invoke cmake --build')
    call gremlin_stop_lane(driver, project, cache, state, 'cmake', owner, &
                           allow_terminal_error=.true.)
    call finish_assertions()
contains
    subroutine install_tool_trap()
        !! cmake and ctest on PATH log each call and exec the real tool. Case
        !! children carry FO_GREMLIN_EXECUTION_CWD; the generation build not.
        type(string_list_t) :: command
        type(process_result_t) :: observed
        character(:), allocatable :: shadow
        character(len=4096) :: path_value
        integer :: k
        character(len=5), parameter :: tools(2) = ['cmake', 'ctest']
        character(:), allocatable :: real_tool, body
        shadow = scratch//'/shadow'
        child_builds = scratch//'/child-cmake-build.log'
        generation_builds = scratch//'/generation-cmake-build.log'
        child_runs = scratch//'/child-ctest.log'
        call make_directory(shadow)
        do k = 1, 2
            command = string_list_t()
            call list_add(command, 'sh')
            call list_add(command, '-c')
            call list_add(command, 'command -v '//trim(tools(k)))
            call run_process(command, project, observed)
            call assert_process_ok(observed, 'real '//trim(tools(k))//' is on PATH')
            real_tool = trim(observed%stdout)
            if (index(real_tool, new_line('a')) > 0) &
                real_tool = real_tool(:index(real_tool, new_line('a')) - 1)
            if (k == 1) then
                body = 'if [ "$1" = "--build" ]; then'//new_line('a')// &
                    '  if [ -n "$FO_GREMLIN_EXECUTION_CWD" ]; then'//new_line('a')// &
                    '    echo "$*" >> "'//child_builds//'"'//new_line('a')// &
                    '  else'//new_line('a')// &
                    '    echo "$*" >> "'//generation_builds//'"'//new_line('a')// &
                    '  fi'//new_line('a')//'fi'//new_line('a')
            else
                body = 'if [ -n "$FO_GREMLIN_EXECUTION_CWD" ]; then'//new_line('a')// &
                    '  echo "$*" >> "'//child_runs//'"'//new_line('a')// &
                    'fi'//new_line('a')
            end if
            call write_text(shadow//'/'//trim(tools(k)), '#!/bin/sh'//new_line('a')// &
                body//'exec "'//real_tool//'" "$@"'//new_line('a'))
            command = string_list_t()
            call list_add(command, 'chmod')
            call list_add(command, '+x')
            call list_add(command, shadow//'/'//trim(tools(k)))
            call run_process(command, project, observed)
            call assert_process_ok(observed, 'tool trap is executable')
        end do
        call get_environment_variable('PATH', path_value)
        rc = c_setenv('PATH'//c_null_char, shadow//':'//trim(path_value)//c_null_char, &
            1_c_int)
    end subroutine install_tool_trap

    subroutine scoped_lane_phase()
        !! A lane scoped by -L probe: the generation builds the scope's plan
        !! targets only, only scoped cases run, and a request naming a case
        !! outside that build is served by building its targets.
        character(len=*), parameter :: scope_lane = 'scope'
        type(string_list_t) :: scoped, ask
        type(json_value_t) :: started, status, events, receipt, answer
        type(process_result_t) :: observed
        character(:), allocatable :: scope_owner, active, builds
        integer :: k, attempt, probe_receipts, other_receipts

        call list_add(scoped, 'gremlin')
        call list_add(scoped, 'start')
        call list_add(scoped, '--dir')
        call list_add(scoped, project)
        call list_add(scoped, '--lane')
        call list_add(scoped, scope_lane)
        call list_add(scoped, '-L')
        call list_add(scoped, 'probe')
        call list_add(scoped, '-LE')
        call list_add(scoped, 'nothing_has_this_label')
        call list_add(scoped, '--random')
        call list_add(scoped, '4')
        call list_add(scoped, '--campaign-seconds')
        call list_add(scoped, '60')
        call list_add(scoped, '--timeout-seconds')
        call list_add(scoped, '10')
        call gremlin_json(driver, project, cache, state, scoped, started, observed, &
            30000)
        scope_owner = gremlin_field(started, 'session_id')
        call assert_true(observed%exit_code == 0 .and. len(scope_owner) > 0, &
            'a lane scoped by -L and -LE starts')
        if (len(scope_owner) == 0) return
        probe_receipts = 0
        other_receipts = 0
        active = ''
        do attempt = 1, 3000
            ask = string_list_t()
            call list_add(ask, 'gremlin')
            call list_add(ask, 'status')
            call list_add(ask, '--detail')
            call list_add(ask, 'full')
            call list_add(ask, '--dir')
            call list_add(ask, project)
            call list_add(ask, '--lane')
            call list_add(ask, scope_lane)
            call list_add(ask, '--session')
            call list_add(ask, scope_owner)
            call list_add(ask, '--max-records')
            call list_add(ask, '128')
            call gremlin_json(driver, project, cache, state, ask, status, observed, &
                10000)
            active = gremlin_field(status, 'active_generation')
            events = json_member(status, 'events')
            probe_receipts = 0
            other_receipts = 0
            do k = 1, json_size(events)
                receipt = json_element(events, k)
                if (json_string_value(json_member(receipt, 'case_id')) == &
                    'excluded_probe') then
                    probe_receipts = probe_receipts + 1
                else if (len(json_string_value(json_member(receipt, 'case_id'))) > 0 &
                    .and. json_string_value(json_member(receipt, 'case_id')) /= &
                    '<build>') then
                    other_receipts = other_receipts + 1
                end if
            end do
            if (probe_receipts >= 1) exit
            call gremlin_wait_ms(50)
        end do
        call assert_true(probe_receipts >= 1, 'the scoped lane runs its scoped case')
        call assert_equal_integer(other_receipts, 0, &
            'the scoped lane runs no case outside its scope')
        builds = ''
        if (file_present(generation_builds)) builds = read_text(generation_builds)
        call assert_true(index(builds, 'probe') > 0, &
            'the scoped generation builds the scope target')
        call assert_true(index(builds, 'marker') == 0, &
            'the scoped generation does not build targets outside its scope')
        call assert_true(.not. file_present(child_builds), &
            'scoped case children never invoke cmake --build')
        ! A case outside the scope: reproduce builds its own target first.
        ask = string_list_t()
        call list_add(ask, 'gremlin')
        call list_add(ask, 'reproduce')
        call list_add(ask, 'registered_marker')
        call list_add(ask, '--dir')
        call list_add(ask, project)
        call list_add(ask, '--lane')
        call list_add(ask, scope_lane)
        call list_add(ask, '--session')
        call list_add(ask, scope_owner)
        call list_add(ask, '--generation')
        call list_add(ask, active)
        call gremlin_json(driver, project, cache, state, ask, answer, observed, &
            180000)
        call assert_equal_integer(observed%exit_code, 0, &
            'a request outside the scoped build builds its targets and runs')
        call assert_equal_string(gremlin_field(answer, 'state'), 'PASS', &
            'the out-of-scope case passes after its targets are built')
        call gremlin_stop_lane(driver, project, cache, state, scope_lane, &
            scope_owner, allow_terminal_error=.true.)
        rc = c_unlink(generation_builds//c_null_char)
        rc = c_unlink(child_builds//c_null_char)
        rc = c_unlink(child_runs//c_null_char)
    end subroutine scoped_lane_phase

    integer function line_count(path)
        character(len=*), intent(in) :: path
        character(:), allocatable :: text
        integer :: k
        line_count = 0
        if (.not. file_present(path)) return
        text = read_text(path)
        do k = 1, len(text)
            if (text(k:k) == new_line('a')) line_count = line_count + 1
        end do
    end function line_count

    function status_field(name) result(value)
        character(len=*), intent(in) :: name
        character(:), allocatable :: value
        type(string_list_t) :: query
        type(json_value_t) :: status
        type(process_result_t) :: observed

        query = string_list_t()
        call list_add(query, 'gremlin')
        call list_add(query, 'status')
        call list_add(query, '--detail')
        call list_add(query, 'full')
        call list_add(query, '--dir')
        call list_add(query, project)
        call list_add(query, '--lane')
        call list_add(query, 'cmake')
        call list_add(query, '--session')
        call list_add(query, owner)
        call gremlin_json(driver, project, cache, state, query, status, observed, 10000)
        value = gremlin_field(status, name)
    end function status_field

    subroutine assert_probe_receipt(active)
        !! The EXCLUDE_FROM_ALL case passed in the generation and its receipt
        !! carries the model's labels.
        character(len=*), intent(in) :: active
        type(string_list_t) :: query
        type(json_value_t) :: status, events, receipt, labels
        type(process_result_t) :: observed
        integer :: k, attempt
        logical :: found

        found = .false.
        do attempt = 1, 3000
            query = string_list_t()
            call list_add(query, 'gremlin')
            call list_add(query, 'status')
            call list_add(query, '--detail')
            call list_add(query, 'full')
            call list_add(query, '--dir')
            call list_add(query, project)
            call list_add(query, '--lane')
            call list_add(query, 'cmake')
            call list_add(query, '--session')
            call list_add(query, owner)
            call list_add(query, '--max-records')
            call list_add(query, '128')
            call gremlin_json(driver, project, cache, state, query, status, &
                observed, 10000)
            events = json_member(status, 'events')
            do k = 1, json_size(events)
                receipt = json_element(events, k)
                if (json_string_value(json_member(receipt, 'case_id')) /= &
                    'excluded_probe') cycle
                if (json_string_value(json_member(receipt, 'generation')) /= &
                    active) cycle
                found = .true.
                call assert_equal_string( &
                    json_string_value(json_member(receipt, 'status')), 'PASS', &
                    'the EXCLUDE_FROM_ALL case runs on the generation build')
                labels = json_member(receipt, 'labels')
                call assert_equal_integer(json_size(labels), 2, &
                    'receipt carries case labels')
                if (json_size(labels) == 2) call assert_equal_string( &
                    json_string_value(json_element(labels, 1))// &
                    json_string_value(json_element(labels, 2)), 'probeunit', &
                    'receipt labels are the CTest LABELS')
            end do
            if (found) exit
            call gremlin_wait_ms(20)
        end do
        call assert_true(found, 'the EXCLUDE_FROM_ALL case has a receipt')
    end subroutine assert_probe_receipt

    subroutine assert_build_failure_once()
        !! The failed candidate has exactly one BUILD_FAIL receipt and no case
        !! receipt: its build failure never reaches a case child.
        type(string_list_t) :: query
        type(json_value_t) :: status, events, receipt
        type(process_result_t) :: observed
        character(:), allocatable :: failed, cursor
        type(string_list_t) :: generations, cases, outcomes
        character(len=32) :: number
        integer :: k, n_failures, n_cases, n_rows, page
        cursor = '0'
        do page = 1, 1000
            query = string_list_t()
            call list_add(query, 'gremlin')
            call list_add(query, 'status')
            call list_add(query, '--detail')
            call list_add(query, 'full')
            call list_add(query, '--dir')
            call list_add(query, project)
            call list_add(query, '--lane')
            call list_add(query, 'cmake')
            call list_add(query, '--session')
            call list_add(query, owner)
            call list_add(query, '--cursor')
            call list_add(query, cursor)
            call list_add(query, '--max-records')
            call list_add(query, '128')
            call gremlin_json(driver, project, cache, state, query, status, observed, &
                10000)
            events = json_member(status, 'events')
            do k = 1, json_size(events)
                receipt = json_element(events, k)
                call list_add(generations, &
                    json_string_value(json_member(receipt, 'generation')))
                call list_add(cases, json_string_value(json_member(receipt, 'case_id')))
                call list_add(outcomes, json_string_value(json_member(receipt, 'status')))
            end do
            if (.not. json_boolean_value(json_member(status, 'has_more'))) exit
            write (number, '(i0)') int(json_number_value(json_member(status, &
                'next_cursor')), int64)
            cursor = trim(number)
        end do
        n_rows = 0
        if (allocated(outcomes%items)) n_rows = size(outcomes%items)
        failed = ''
        n_failures = 0
        do k = 1, n_rows
            if (outcomes%items(k)%value /= 'BUILD_FAIL') cycle
            n_failures = n_failures + 1
            failed = generations%items(k)%value
        end do
        call assert_equal_integer(n_failures, 1, 'the build failure is reported once')
        n_cases = 0
        do k = 1, n_rows
            if (generations%items(k)%value /= failed) cycle
            if (cases%items(k)%value == '<build>') cycle
            n_cases = n_cases + 1
        end do
        call assert_equal_integer(n_cases, 0, &
            'no case of the failed candidate is attempted')
    end subroutine assert_build_failure_once

    subroutine make_populate_only_dependency()
        type(string_list_t) :: command
        type(process_result_t) :: observed
        character(:), allocatable :: raw_source
        raw_source = scratch//'/raw-dependency'
        raw_archive = scratch//'/raw-dependency.tar'
        call make_directory(raw_source)
        call write_text(raw_source//'/raw.f90', &
            'module raw_values'//new_line('a')// &
            'integer, parameter :: raw_result = 11'//new_line('a')// &
            'end module raw_values'//new_line('a'))
        call list_add(command, 'cmake')
        call list_add(command, '-E')
        call list_add(command, 'tar')
        call list_add(command, 'cf')
        call list_add(command, raw_archive)
        call list_add(command, '--format=gnutar')
        call list_add(command, 'raw.f90')
        call run_process(command, raw_source, observed)
        call assert_process_ok(observed, 'populate-only dependency archive builds')
    end subroutine make_populate_only_dependency
    logical function file_present(path)
        character(len=*), intent(in) :: path
        inquire(file=path, exist=file_present)
    end function file_present
    subroutine initialize_project_git()
        type(string_list_t) :: command
        type(process_result_t) :: observed
        character(:), allocatable :: head
        integer :: newline_at
        call write_text(project//'/tracked.txt', 'committed state')
        call list_add(command, 'git')
        call list_add(command, 'init')
        call run_process(command, project, observed)
        call assert_process_ok(observed, 'CMake project Git initializes')
        command = string_list_t()
        call list_add(command, 'git')
        call list_add(command, 'add')
        call list_add(command, '.')
        call run_process(command, project, observed)
        command = string_list_t()
        call list_add(command, 'git')
        call list_add(command, '-c')
        call list_add(command, 'user.name=Fixture')
        call list_add(command, '-c')
        call list_add(command, 'user.email=fixture@example.invalid')
        call list_add(command, 'commit')
        call list_add(command, '-qm')
        call list_add(command, 'project source')
        call run_process(command, project, observed)
        call assert_process_ok(observed, 'CMake project source commit exists')
        command = string_list_t()
        call list_add(command, 'git')
        call list_add(command, 'rev-parse')
        call list_add(command, 'HEAD')
        call run_process(command, project, observed)
        head = trim(observed%stdout)
        newline_at = index(head, new_line('a'))
        if (newline_at > 0) head = head(:newline_at - 1)
        call write_text(project//'/expected-head', head//new_line('a'))
        call write_text(project//'/tracked.txt', 'captured dirty state')
    end subroutine initialize_project_git

    subroutine make_provider()
        type(string_list_t) :: command
        type(process_result_t) :: observed
        integer :: newline_at
        call make_directory(provider_source)
        call make_directory(provider//'/include')
        call make_directory(provider//'/lib')
        call write_text(provider_source//'/README', 'prebuilt provider provenance')
        call list_add(command, 'git')
        call list_add(command, 'init')
        call run_process(command, provider_source, observed)
        call assert_process_ok(observed, 'toy provider Git repository initializes')
        command = string_list_t()
        call list_add(command, 'git')
        call list_add(command, 'add')
        call list_add(command, 'README')
        call run_process(command, provider_source, observed)
        command = string_list_t()
        call list_add(command, 'git')
        call list_add(command, '-c')
        call list_add(command, 'user.name=Fixture')
        call list_add(command, '-c')
        call list_add(command, 'user.email=fixture@example.invalid')
        call list_add(command, 'commit')
        call list_add(command, '-m')
        call list_add(command, 'provider source')
        call run_process(command, provider_source, observed)
        call assert_process_ok(observed, 'toy provider source commit exists')
        command = string_list_t()
        call list_add(command, 'git')
        call list_add(command, 'rev-parse')
        call list_add(command, 'HEAD')
        call run_process(command, provider_source, observed)
        revision = trim(observed%stdout)
        newline_at = index(revision, new_line('a'))
        if (newline_at > 0) revision = revision(:newline_at - 1)
        call write_text(provider//'/CMakeCache.txt', &
            'CMAKE_HOME_DIRECTORY:INTERNAL='//provider_source//new_line('a'))
        call write_provider(17)
    end subroutine make_provider

    subroutine write_provider(value)
        integer, intent(in) :: value
        character(len=8) :: number
        write(number, '(i0)') value
        call write_text(provider//'/include/provider.inc', &
            'integer, parameter :: provider_header = '//trim(number))
    end subroutine write_provider

    subroutine write_dependency(value)
        integer, intent(in) :: value
        character(len=8) :: number
        write (number, '(i0)') value
        call write_text(dependency//'/value.f90', &
                      'module values'//new_line('a')//'implicit none'//new_line('a')// &
                      'integer, parameter :: result = '//trim(number)//new_line('a')// &
                        'end module values'//new_line('a'))
    end subroutine write_dependency
    subroutine write_marker(broken)
        logical, intent(in) :: broken
        if (broken) then
            call write_text(project//'/marker.f90', 'this is not Fortran')
            return
        end if
        call write_text(project//'/marker.f90', &
                        'program marker'//new_line('a')//'use values, only: result'// &
                        new_line('a')//'use raw_values, only: raw_result'// &
                        new_line('a')//'implicit none'//new_line('a')// &
                        'logical :: exists'//new_line('a')// &
                        'include "provider.inc"'//new_line('a')// &
                        'if (provider_header /= 17) error stop "provider changed"'// &
                        new_line('a')// &
                        'inquire(file="setup.ran", exist=exists)'//new_line('a')// &
           'if (.not. exists) error stop "CTest fixture or cwd lost"'//new_line('a')// &
            'if (result /= 7) error stop "dependency result changed"'//new_line('a')// &
            'if (raw_result /= 11) error stop "populated source lost"'//new_line('a')// &
                        'print *, "marker passed"'//new_line('a')//'end program marker')
    end subroutine write_marker
    subroutine wait_result(old, outcome, current)
        character(len=*), intent(in) :: old, outcome
        character(:), allocatable, intent(out) :: current
        integer :: i
        type(string_list_t) :: query
        type(json_value_t) :: status, failure
        type(process_result_t) :: observed
        current = old
        query = string_list_t()
        call list_add(query, 'gremlin')
        call list_add(query, 'status')
        call list_add(query, '--dir')
        call list_add(query, project)
        call list_add(query, '--lane')
        call list_add(query, 'cmake')
        call list_add(query, '--session')
        call list_add(query, owner)
        do i = 1, 2400
        call gremlin_json(driver, project, cache, state, query, status, observed, 10000)
            current = gremlin_field(status, 'active_generation')
            if (outcome == 'PASS' .and. current /= old) then
                if (json_boolean_value(json_member(status, 'local_gate_green'))) return
            end if
            if (outcome == 'FAIL' .and. current /= old) then
                failure = json_member(status, 'latest_failure')
                if (json_string_value(json_member(failure, 'generation')) == current &
                    .and. json_string_value(json_member(failure, 'status')) == 'FAIL' &
                    .and. json_string_value(json_member(failure, 'case_id')) == &
                    'registered_marker') return
            end if
            if (gremlin_field(status, 'last_outcome') == outcome) then
                if (outcome == 'BUILD_FAIL' .or. current /= old) return
            end if
            if (outcome == 'CAPTURE_FAIL' .and. &
                (gremlin_field(status, 'state') == 'capture_failed' .or. &
                 gremlin_field(status, 'state') == 'error')) then
                call assert_true(index(gremlin_field(status, 'diagnostic'), &
                    'referenced CMake input is missing or unsupported') > 0, &
                    'referenced unsafe alias has an explicit closure diagnostic')
                return
            end if
            if (gremlin_field(status, 'state') == 'error') exit
            if (gremlin_field(status, 'state') == 'capture_failed') exit
            call gremlin_wait_ms(20)
        end do
        write (*, '(a)') observed%stdout
        call assert_true(.false., 'resident CMake reaches '//outcome)
    end subroutine wait_result
end program test_gremlin_cmake
