program test_native_cmake_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
                          make_scratch, make_directory, write_text, environment_value, &
                    run_process, assert_process_ok, assert_true, assert_equal_integer, &
          assert_equal_string, assert_file_exists, assert_file_absent, finish_assertions
    use fo_test_harness, only: remove_tree, remove_path, read_text
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_run, &
                                      gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_element, json_size, &
                            json_string_value, json_boolean_value
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, self, cmake_text
    character(:), allocatable :: state, owner, first_generation, previous_generation
    character(len=4096) :: executable
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: args, env
    type(process_result_t) :: process
    type(json_value_t) :: report, tests, entry
    integer :: phase

    call get_command_argument(0, executable)
    self = trim(executable)
    if (self == 'cmake' .or. self == 'ctest' .or. &
        self == 'cmake.exe' .or. self == 'ctest.exe' .or. &
        index(self, '/cmake') > 0 .or. index(self, '/ctest') > 0 .or. &
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
                 'set(CMAKE_Fortran_FLAGS "$ENV{FO_NATIVE_ORACLE_FLAGS}")'//nl// &
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
        'if(index(compiler_options(),"-O3")==0)error stop "Release profile lost"'//nl// &
                    'if(index(compiler_options(),"170003")==0)'//nl// &
                    'error stop "captured environment/compiler flags lost"'//nl// &
                    'print *,"numerical-oracle",value()'//nl//'end program'//nl)
    call write_text(project//'/invalid.f90', 'unselected source must not compile')
    call reference()
    call shadow_tool('cmake')
    call shadow_tool('ctest')
    call list_add(env, 'FO_BACKEND=cmake')
    call list_add(env, 'FO_CMAKE_NATIVE=1')
    call list_add(env, 'FO_NATIVE_ORACLE_FLAGS=-fmax-stack-var-size=170003')
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
    call write_text(project//'/CMakeLists.txt', cmake_text// &
        'add_test(NAME cwd_oracle COMMAND oracle.x 19 WORKING_DIRECTORY support)'//nl)
    call run_fo(driver, args, project, cache, process, env, 60000)
    call assert_true(process%exit_code /= 0, 'unsupported test cwd fails explicitly')
    call assert_true(index(process%stdout//process%stderr, &
        'unsupported test option WORKING_DIRECTORY') > 0, &
        'unsupported test properties never become program arguments')
    call assert_file_absent(scratch//'/delegated.ran', &
        'unsupported test properties do not invoke delegated tools')
    call write_text(project//'/CMakeLists.txt', cmake_text)
    call resident_oracle()
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
        type(string_list_t) :: command, reference_env
        type(process_result_t) :: observed
        call list_add(command, 'cmake')
        call list_add(command, '-S')
        call list_add(command, project)
        call list_add(command, '-B')
        call list_add(command, project//'/reference')
        call list_add(command, '-DCMAKE_BUILD_TYPE=Release')
        call list_add(command, '-DCMAKE_Fortran_COMPILER=gfortran')
      call list_add(reference_env, 'FO_NATIVE_ORACLE_FLAGS=-fmax-stack-var-size=170003')
        call run_process(command, project, observed, reference_env, timeout_ms=60000)
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

    subroutine resident_oracle()
        type(string_list_t) :: request, replay_env
        type(json_value_t) :: reply
        integer :: phase, i
        state = scratch//'/state'
        call make_directory(state)
        call gremlin_start_args(request, project, 'native-numeric', 'numerical_oracle')
        call list_add(request, '--random')
        call list_add(request, '0')
        call gremlin_run(driver, project, cache, state, request, process, env, 20000)
        call assert_process_ok(process, 'native resident capture starts without CMake')
        call parse_json_report(process, reply, 'native resident start')
        owner = gremlin_field(reply, 'session_id')
        previous_generation = ''
        do phase = 1, 3
            if (phase == 2) call payload(23)
            if (phase == 3) call payload(19)
            call wait_settled(phase /= 2, reply)
            call assert_true(gremlin_field(reply, 'active_generation') /= &
              previous_generation, 'authored edit wakes a new frozen native generation')
            previous_generation = gremlin_field(reply, 'active_generation')
            if (phase == 1) first_generation = previous_generation
            call assert_file_absent(scratch//'/delegated.ran', &
               'native capture, discovery and resident campaigns invoke no CMake/CTest')
        end do
        call gremlin_stop_lane(driver, project, cache, state, 'native-numeric', owner)
        call payload(23)
        replay_env = env
        do i = 1, size(replay_env%items)
            if (index(replay_env%items(i)%value, 'FO_NATIVE_ORACLE_FLAGS=') == 1) &
        replay_env%items(i)%value = 'FO_NATIVE_ORACLE_FLAGS=-fmax-stack-var-size=990003'
            if (index(replay_env%items(i)%value, 'FO_CMAKE_NATIVE=') == 1) &
                replay_env%items(i)%value = 'FO_CMAKE_NATIVE=0'
            if (index(replay_env%items(i)%value, 'FO_CMAKE_CONFIG=') == 1) &
                replay_env%items(i)%value = 'FO_CMAKE_CONFIG=Debug'
        end do
        request = string_list_t()
        call list_add(request, 'gremlin')
        call list_add(request, 'reproduce')
        call list_add(request, '--dir')
        call list_add(request, project)
        call list_add(request, '--lane')
        call list_add(request, 'native-numeric')
        call list_add(request, '--generation')
        call list_add(request, first_generation)
        call list_add(request, '--case')
        call list_add(request, 'numerical_oracle')
    call gremlin_run(driver, project, cache, state, request, process, replay_env, 60000)
        call assert_process_ok(process, 'stopped owner replays frozen native inputs')
        call parse_json_report(process, reply, 'native frozen replay')
        call assert_equal_string(gremlin_field(reply, 'last_outcome'), 'PASS', &
            'frozen numerical runtime and compiler flags survive live source/env edits')
        call assert_file_absent(scratch//'/delegated.ran', &
             'frozen replay retains the captured native route despite changed selector')
    end subroutine

    subroutine wait_settled(green, reply)
        logical, intent(in) :: green
        type(json_value_t), intent(out) :: reply
        type(string_list_t) :: query
        logical :: settled
        integer :: attempt
        call list_add(query, 'gremlin')
        call list_add(query, 'status')
        call list_add(query, '--dir')
        call list_add(query, project)
        call list_add(query, '--lane')
        call list_add(query, 'native-numeric')
        call list_add(query, '--session')
        call list_add(query, owner)
        settled = .false.
        do attempt = 1, 600
            call gremlin_run(driver, project, cache, state, query, process, env, 10000)
            call parse_json_report(process, reply, 'native resident status')
            if (gremlin_field(reply, 'state') == 'error') exit
            if (gremlin_field(reply, 'state') == 'quiescent') then
                settled = json_boolean_value(json_member(reply, 'local_gate_green')) &
                          .eqv. green
                settled = settled .and. &
                        gremlin_field(reply, 'active_generation') /= previous_generation
                if (settled) exit
            end if
            call gremlin_wait_ms(50)
        end do
        if (.not. settled) write (*, '(a)') process%stdout//process%stderr
        call assert_true(settled, 'native resident numerical gate settles red/green')
    end subroutine

    subroutine shadow_tool(name)
        character(len=*), intent(in) :: name
        type(string_list_t) :: command, guard_env
        type(process_result_t) :: observed
        character(:), allocatable :: destination
        destination = scratch//'/shadow/'//name
        if (environment_value('OS') == 'Windows_NT') destination = destination//'.exe'
        call list_add(command, 'cp')
        call list_add(command, self)
        call list_add(command, destination)
        call run_process(command, project, observed)
      call assert_process_ok(observed, 'prepare fail-on-execution delegated tool guard')
        command = string_list_t()
        call list_add(command, name)
        if (environment_value('OS') == 'Windows_NT') then
            call list_add(guard_env, 'PATH='//scratch//'/shadow;'// &
                          environment_value('PATH'))
        else
            call list_add(guard_env, 'PATH='//scratch//'/shadow:'// &
                          environment_value('PATH'))
        end if
        call list_add(guard_env, 'FO_FORBIDDEN_TOOL_MARKER='//scratch//'/guard-probe.ran')
        call run_process(command, project, observed, guard_env, timeout_ms=1000)
        call assert_true(observed%exit_code /= 0, 'bare delegated tool guard rejects')
        call assert_file_exists(scratch//'/guard-probe.ran', &
                                'bare delegated tool reaches the guard')
        call assert_equal_string(trim(read_text(scratch//'/guard-probe.ran')), name, &
                                 'bare delegated tool argv0 is guarded')
        call remove_path(scratch//'/guard-probe.ran')
    end subroutine
end program test_native_cmake_cli
