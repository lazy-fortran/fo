program test_native_cmake_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
                          make_scratch, make_directory, write_text, environment_value, &
                    run_process, assert_process_ok, assert_true, assert_equal_integer, &
          assert_equal_string, assert_file_exists, assert_file_absent, finish_assertions
    use fo_test_harness, only: remove_tree, remove_path
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size, &
                            json_string_value
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, self, cmake_text
    character(len=4096) :: executable
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: args, env
    type(process_result_t) :: process
    type(json_value_t) :: report, tests, entry
    integer :: phase

    call get_command_argument(0, executable)
    self = trim(executable)
    if (index(self, '/cmake') > 0 .or. index(self, '/ctest') > 0 .or. &
        index(self, achar(92)//'cmake') > 0 .or. &
        index(self, achar(92)//'ctest') > 0) then
        call write_text(environment_value('FO_FORBIDDEN_TOOL_MARKER'), self)
        error stop 'native route invoked a forbidden delegated tool'
    end if
    call resolve_driver(driver)
    call make_scratch('fo-native-cmake-cli', scratch)
    project = scratch//'/project'
    cache = scratch//'/cache'
    call make_directory(project//'/support')
    call make_directory(scratch//'/shadow')
    cmake_text = 'cmake_minimum_required(VERSION 3.20)'//nl// &
                 'project(native_oracle VERSION 1.0.0 LANGUAGES Fortran)'//nl// &
                 'enable_testing()'//nl// &
                 'add_compile_options(-ffpe-trap=zero,overflow,invalid)'//nl// &
                 'add_link_options(-ffpe-trap=zero,overflow,invalid)'//nl// &
                 'add_subdirectory(support)'//nl// &
                 'add_executable(oracle.x main.f90)'//nl// &
                 'target_link_libraries(oracle.x PRIVATE payload)'//nl// &
                 'add_executable(unselected invalid.f90)'//nl// &
                 'add_test(NAME numerical_oracle COMMAND oracle.x 19)'//nl// &
                 'add_test(NAME numerical_oracle_neighbor COMMAND oracle.x 999)'//nl
    call write_text(project//'/CMakeLists.txt', cmake_text)
    call write_text(project//'/support/CMakeLists.txt', &
                    'add_library(payload STATIC payload.f90)'//nl// &
                'set_target_properties(payload PROPERTIES Fortran_MODULE_DIRECTORY '// &
                    '"${CMAKE_CURRENT_BINARY_DIR}")'//nl// &
         'target_include_directories(payload PUBLIC "${CMAKE_CURRENT_BINARY_DIR}")'//nl)
    call payload(19)
    call write_text(project//'/main.f90', 'program numerical'//nl// &
                    'use payload_value, only: value'//nl// &
                    'use iso_fortran_env, only: compiler_options'//nl// &
         'implicit none'//nl//'character(32)::argument'//nl//'integer::expected'//nl// &
         'call get_command_argument(1,argument)'//nl//'read(argument,*)expected'//nl// &
               'if(value()/=expected)error stop "independent arithmetic oracle"'//nl// &
  'if(index(compiler_options(),"-ffpe-trap=")==0)error stop "project flag lost"'//nl// &
        'if(index(compiler_options(),"-fcheck=")>0)error stop "invented checks"'//nl// &
                    'print *,"numerical-oracle",value()'//nl//'end program'//nl)
    call write_text(project//'/invalid.f90', 'unselected source must not compile')
    call reference()
    call shadow_tool('cmake')
    call shadow_tool('ctest')
    call list_add(env, 'FO_BACKEND=cmake')
    call list_add(env, 'FO_CMAKE_NATIVE=1')
    call list_add(env, 'FO_FC=gfortran')
    call list_add(env, 'FO_CMAKE_BUILD_DIR=native')
    call list_add(env, 'FO_CMAKE_CONFIG=Release')
    call list_add(env, 'FO_CMAKE_BUILD_TARGETS=oracle.x')
    call list_add(env, 'FO_CMAKE_CONFIGURE_PRESET=')
    call list_add(env, 'FO_CMAKE_BUILD_PRESET=')
    call list_add(env, 'FO_CMAKE_TEST_PRESET=')
    call list_add(env, 'FO_CMAKE_ARGS=')
    call list_add(env, 'FO_FORBIDDEN_TOOL_MARKER='//scratch//'/delegated.ran')
    if (environment_value('OS') == 'Windows_NT') then
        call list_add(env, 'PATH='//scratch//'/shadow;'//environment_value('PATH'))
    else
        call list_add(env, 'PATH='//scratch//'/shadow:'//environment_value('PATH'))
    end if
    do phase = 1, 4
        if (phase == 2) then
            call remove_tree(project//'/native/.fo-native/obj')
            call remove_path(project//'/native/support/payload_value.mod')
        end if
        if (phase == 3) call payload(23)
        if (phase == 4) call payload(19)
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, 'numerical_oracle')
        call list_add(args, '--json')
        call run_fo(driver, args, project, cache, process, env, 60000)
        if (phase == 3) then
            call assert_true(process%exit_code /= 0, &
                             'warm source edit invalidates the numerical result')
        else
            call assert_process_ok(process, 'cold, warm and restored numerical oracle')
        end if
        call parse_json_report(process, report, 'native selected test report')
        tests = json_member(report, 'tests')
        call assert_equal_integer(json_size(tests), 1, 'exact registered test scope')
        entry = json_element(tests, 1)
        call assert_equal_string(json_string_value(json_member(entry, 'name')), &
                       'numerical_oracle', 'native reports exact independent test name')
        if (phase == 3) then
            call assert_equal_string(json_string_value(json_member(entry, 'status')), &
                                  'fail', 'native reports the actual numerical failure')
        else
            call assert_equal_string(json_string_value(json_member(entry, 'status')), &
                                  'pass', 'native reports the actual numerical success')
        end if
        call assert_file_exists(project//'/native/support/libpayload.a', &
                              'declared static library uses its CMake binary directory')
        call assert_file_absent(scratch//'/delegated.ran', &
                      'native configure, build and test invoke neither CMake nor CTest')
        call assert_file_absent(project//'/native/CMakeCache.txt', &
                                'native execution creates no fabricated CMake cache')
    end do
    args = string_list_t()
    call list_add(args, 'exec')
    call list_add(args, '--no-build')
    call list_add(args, 'oracle.x')
    call list_add(args, '19')
    call run_fo(driver, args, project, cache, process, env, 60000)
    call assert_process_ok(process, 'public native executable resolver')
    call assert_true(index(process%stdout, 'numerical-oracle') > 0, &
                     'public exec reaches the built independent numerical program')
    args = string_list_t()
    call list_add(args, 'test')
    call list_add(args, 'missing_registered_test')
    call list_add(args, '--json')
    call run_fo(driver, args, project, cache, process, env, 60000)
    call assert_true(process%exit_code /= 0, 'unknown registered test fails explicitly')
    call write_text(project//'/CMakeLists.txt', cmake_text// &
                    'add_custom_target(unsupported ALL COMMAND never_execute_me)'//nl)
    args = string_list_t()
    call list_add(args, 'build')
    call run_fo(driver, args, project, cache, process, env, 60000)
    call assert_true(process%exit_code /= 0, 'unsupported commands fail explicitly')
    call assert_true(index(process%stdout//process%stderr, &
         'unsupported command add_custom_target') > 0, 'unsupported command diagnostic')
call assert_file_absent(scratch//'/delegated.ran', 'unsupported inputs never fall back')
    call write_text(project//'/CMakeLists.txt', cmake_text)
    call finish_assertions()
contains
    subroutine payload(number)
        integer, intent(in) :: number
        character(len=16) :: text
        write (text, '(i0)') number
        call write_text(project//'/support/payload.f90', &
                     'module payload_value'//nl//'implicit none'//nl//'contains'//nl// &
                        'integer function value()'//nl//'value='//trim(text)//nl// &
                        'end function'//nl//'end module'//nl)
    end subroutine

    subroutine reference()
        type(string_list_t) :: command
        type(process_result_t) :: observed
        call list_add(command, 'cmake')
        call list_add(command, '-S')
        call list_add(command, project)
        call list_add(command, '-B')
        call list_add(command, project//'/reference')
        call list_add(command, '-DCMAKE_BUILD_TYPE=Release')
        call list_add(command, '-DCMAKE_Fortran_COMPILER=gfortran')
        call run_process(command, project, observed, timeout_ms=60000)
        call assert_process_ok(observed, 'independent unchanged CMake configure')
        command = string_list_t()
        call list_add(command, 'cmake')
        call list_add(command, '--build')
        call list_add(command, project//'/reference')
        call list_add(command, '--target')
        call list_add(command, 'oracle.x')
        call run_process(command, project, observed, timeout_ms=60000)
        call assert_process_ok(observed, 'independent selected CMake build')
        command = string_list_t()
        call list_add(command, 'ctest')
        call list_add(command, '--test-dir')
        call list_add(command, project//'/reference')
        call list_add(command, '-R')
        call list_add(command, '^numerical_oracle$')
        call list_add(command, '--output-on-failure')
        call run_process(command, project, observed, timeout_ms=60000)
        call assert_process_ok(observed, 'independent numerical CTest authority')
    end subroutine

    subroutine shadow_tool(name)
        character(len=*), intent(in) :: name
        type(string_list_t) :: command
        type(process_result_t) :: observed
        character(:), allocatable :: destination
        destination = scratch//'/shadow/'//name
        if (environment_value('OS') == 'Windows_NT') destination = destination//'.exe'
        call list_add(command, 'cp')
        call list_add(command, self)
        call list_add(command, destination)
        call run_process(command, project, observed)
      call assert_process_ok(observed, 'prepare fail-on-execution delegated tool guard')
    end subroutine
end program test_native_cmake_cli
