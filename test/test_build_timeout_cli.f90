program test_build_timeout_cli
    use fo_test_harness, only: string_list_t, process_result_t, make_scratch, &
        write_lines, list_add, remove_tree, assert_equal_integer, &
        assert_contains, assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none
    character(:), allocatable :: driver, scratch, project
    type(string_list_t) :: arguments, environment
    type(process_result_t) :: result

    call resolve_driver(driver)
    call make_scratch('fo-build-timeout-cli', scratch)
    project = scratch//'/project'
    call fixture('sleep 3')
    call list_add(arguments, 'build')
    call list_add(environment, 'FO_BACKEND=cmake')
    call list_add(environment, 'FO_CMAKE_NATIVE=0')
    call list_add(environment, 'FO_BUILD_TIMEOUT=1')
    call run_fo(driver, arguments, project, scratch//'/cache', result, &
        environment, 20000)
    call assert_equal_integer(result%exit_code, 124, &
        'public build preserves the backend timeout status')
    call assert_contains(result%stdout//result%stderr, 'build timed out', &
        'the build timeout remains diagnosed')

    call fixture('false')
    call run_fo(driver, arguments, project, scratch//'/cache', result, &
        environment, 20000)
    call assert_equal_integer(result%exit_code, 1, &
        'an ordinary build failure keeps the public failure status')

    call fixture('true')
    call run_fo(driver, arguments, project, scratch//'/cache', result, &
        environment, 20000)
    call assert_process_ok(result, 'a successful build keeps its success status')

    call list_add(arguments, '--tests')
    call write_lines(project//'/CMakeLists.txt', [character(len=160) :: &
        'cmake_minimum_required(VERSION 3.20)', &
        'project(configure_timeout_status LANGUAGES NONE)', &
        'execute_process(COMMAND "${CMAKE_COMMAND}" -E sleep 3)'])
    call run_fo(driver, arguments, project, scratch//'/cache', result, &
        environment, 20000)
    call assert_equal_integer(result%exit_code, 124, &
        'building the test model preserves the configure timeout status')
    call assert_contains(result%stdout//result%stderr, 'configure timed out', &
        'the configure timeout remains diagnosed')

    call write_lines(project//'/CMakeLists.txt', [character(len=160) :: &
        'cmake_minimum_required(VERSION 3.20)', &
        'project(configure_failure_status LANGUAGES NONE)', &
        'message(FATAL_ERROR "synthetic configure failure")'])
    call run_fo(driver, arguments, project, scratch//'/cache', result, &
        environment, 20000)
    call assert_equal_integer(result%exit_code, 1, &
        'an ordinary configure failure keeps the public failure status')
    call remove_tree(scratch)
    call finish_assertions()
contains
    subroutine fixture(command)
        character(*), intent(in) :: command

        call write_lines(project//'/CMakeLists.txt', [character(len=160) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(timeout_status LANGUAGES NONE)', &
            'add_custom_target(work ALL COMMAND "${CMAKE_COMMAND}" -E '// &
            command//')'])
    end subroutine fixture
end program test_build_timeout_cli
