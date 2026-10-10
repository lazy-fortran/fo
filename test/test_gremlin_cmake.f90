program test_gremlin_cmake
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
              make_directory, write_text, read_text, assert_true, assert_equal_string, &
                               finish_assertions, run_process, assert_process_ok
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, &
                   gremlin_start_args, gremlin_wait_ms, gremlin_field, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, json_string_value
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state
    character(:), allocatable :: owner, generation, previous, dependency
    character(:), allocatable :: provider, provider_source, revision, raw_archive
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
                    'add_executable(marker marker.f90)'//new_line('a')// &
                    'install(TARGETS marker RUNTIME DESTINATION lib)'//new_line('a')// &
                    'add_executable(unselected broken.f90)'//new_line('a')// &
                    'target_link_libraries(marker PRIVATE value_dep raw_dep)'// &
                    new_line('a')// &
                  'target_include_directories(marker PRIVATE "${CMAKE_BINARY_DIR}" '// &
                    '"${VALUE_PROVIDER_BUILD}/include")'// &
                    new_line('a')// &
                 'file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/sandbox")'//new_line('a')// &
                    'add_test(NAME setup COMMAND "${CMAKE_COMMAND}" -E touch '// &
                    '"${CMAKE_BINARY_DIR}/sandbox/setup.ran")'//new_line('a')// &
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
    call write_marker(.false.)
    call write_text(project//'/CMakePresets.json', &
                    '{"version":3,"configurePresets":[{"name":"cpu",'// &
                    '"generator":"Ninja","binaryDir":"${sourceDir}/build",'// &
                    '"cacheVariables":{"CMAKE_BUILD_TYPE":"Release",'// &
                    '"CMAKE_INSTALL_PREFIX":"'//scratch// &
                    '/uncreated-install-prefix"}}],'// &
                    '"buildPresets":[{"name":"cpu","configurePreset":"cpu"}],'// &
                    '"testPresets":[{"name":"cpu","configurePreset":"cpu"}]}')
    rc = c_setenv('FO_CMAKE_CONFIGURE_PRESET'//c_null_char, 'cpu'//c_null_char, 1_c_int)
    rc = c_setenv('FO_CMAKE_BUILD_PRESET'//c_null_char, 'cpu'//c_null_char, 1_c_int)
    rc = c_setenv('FO_CMAKE_BUILD_TARGETS'//c_null_char, 'marker'//c_null_char, 1_c_int)
    rc = c_setenv('FO_CMAKE_TEST_PRESET'//c_null_char, 'cpu'//c_null_char, 1_c_int)
    rc = c_setenv('FO_CMAKE_BUILD_DIR'//c_null_char, 'build'//c_null_char, 1_c_int)
    call gremlin_start_args(args, project, 'cmake', 'registered_marker')
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
    call write_marker(.true.)
    call wait_result(previous, 'BUILD_FAIL', generation)
    call assert_equal_string(generation, previous, &
                         'invalid candidate retains the last compiled CMake generation')
    call write_marker(.false.)
    call write_dependency(9)
    call wait_result(previous, 'FAIL', generation)
    call assert_true(generation /= previous, 'external dependency edit wakes resident')
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
    call write_text(scratch//'/outside.f90', read_text(dependency//'/value.f90'))
    rc = c_unlink(dependency//'/package/leaf.f90'//c_null_char)
    call assert_true(rc == 0, 'referenced link can be changed in the fixture')
    rc = c_symlink(scratch//'/outside.f90'//c_null_char, &
                   dependency//'/package/leaf.f90'//c_null_char)
    call assert_true(rc == 0, 'referenced external alias fixture exists')
    call wait_result(previous, 'CAPTURE_FAIL', generation)
    call assert_equal_string(generation, previous, &
                            'unsupported referenced input retains active generation')
    call gremlin_stop_lane(driver, project, cache, state, 'cmake', owner, &
                           allow_terminal_error=.true.)
    call finish_assertions()
contains
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
