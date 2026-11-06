program test_cmake_project_args
    !! Configure arguments persisted as Git config `fo.cmakeArgs` reach CMake
    !! from the checkout and from its linked worktrees, before FO_CMAKE_ARGS.
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, read_text, assert_true, assert_contains, assert_process_ok, &
        environment_value, run_git, finish_assertions
    use fo_test_cli, only: run_fo
    use fo_test_gremlin_oracle, only: gremlin_setup
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, linked
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: args, environment
    type(process_result_t) :: process
    integer :: status

    call gremlin_setup(driver, scratch, project, cache, state)
    call assert_true(len(environment_value('FO_CMAKE_PROJECT_ARGS')) == 0, &
        'the fixture starts without resolved project arguments')
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(project_args LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'if(NOT PROBE STREQUAL "checkout" OR NOT EXTRA STREQUAL "environment")'//nl// &
        '  message(FATAL_ERROR "PROBE=${PROBE} EXTRA=${EXTRA}")'//nl//'endif()'//nl// &
        'add_executable(probe probe.f90)'//nl//'add_test(NAME probe COMMAND probe)'//nl)
    call write_text(project//'/probe.f90', 'program probe'//nl//'end program probe'//nl)
    call write_text(project//'/.gitignore', 'build/'//nl)
    call git(project, [character(len=32) :: 'init', '-q'], 'fixture Git initializes')
    call git(project, [character(len=32) :: 'add', '.'], 'fixture files are staged')
    call git(project, [character(len=32) :: 'commit', '-qm', 'fixture'], &
        'fixture commit exists')
    call git(project, [character(len=32) :: 'config', 'fo.cmakeArgs', &
        '-DPROBE=checkout'], 'persists the checkout configure arguments')
    linked = scratch//'/linked'
    call git(project, [character(len=4096) :: 'worktree', 'add', '-q', linked], &
        'a linked worktree shares the checkout configuration')

    environment = string_list_t()
    call list_add(environment, 'FO_BACKEND=cmake')
    call list_add(environment, 'FO_CMAKE_BUILD_DIR=build')
    call list_add(environment, 'FO_CMAKE_CONFIGURE_PRESET=')
    call list_add(environment, 'FO_CMAKE_ARGS=-DEXTRA=environment')
    args = string_list_t()
    call list_add(args, 'test')
    call list_add(args, 'probe')
    call run_fo(driver, args, project, cache, process, environment, 120000)
    call assert_process_ok(process, 'the checkout configures with fo.cmakeArgs')
    call assert_contains(read_text(project//'/build/CMakeCache.txt'), &
        'PROBE:UNINITIALIZED=checkout', 'CMake records the persisted argument')
    call run_fo(driver, args, linked, cache, process, environment, 120000)
    call assert_process_ok(process, 'the linked worktree configures with fo.cmakeArgs')
    call finish_assertions()

contains

    subroutine git(directory, arguments, label)
        character(len=*), intent(in) :: directory, arguments(:), label

        call run_git(directory, arguments, status)
        call assert_true(status == 0, label)
    end subroutine git

end program test_cmake_project_args
