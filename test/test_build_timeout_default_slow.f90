program test_build_timeout_default_slow
    !! A legitimate delegated build exceeding the former five-minute default.
    use fo_test_harness, only: string_list_t, process_result_t, make_scratch, &
        write_lines, list_add, remove_tree, assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none
    character(:), allocatable :: driver, scratch, project
    type(string_list_t) :: arguments, environment
    type(process_result_t) :: result

    call resolve_driver(driver)
    call make_scratch('fo-build-timeout-default', scratch)
    project = scratch//'/project'
    call write_lines(project//'/CMakeLists.txt', [character(len=160) :: &
        'cmake_minimum_required(VERSION 3.20)', &
        'project(long_delegated_build LANGUAGES NONE)', &
        'add_custom_target(work ALL COMMAND "${CMAKE_COMMAND}" -E sleep 301)'])
    call list_add(arguments, 'build')
    call list_add(environment, 'FO_BACKEND=cmake')
    call list_add(environment, 'FO_CMAKE_NATIVE=0')
    call list_add(environment, 'FO_BUILD_TIMEOUT=')
    call run_fo(driver, arguments, project, scratch//'/cache', result, &
        environment, 330000)
    call assert_process_ok(result, 'delegated build survives the former 300 s default')
    call remove_tree(scratch)
    call finish_assertions()
end program test_build_timeout_default_slow
