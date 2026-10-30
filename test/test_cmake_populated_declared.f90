program test_cmake_populated_declared
    !! A populated FetchContent dependency that never calls project() has no
    !! <project>_SOURCE_DIR cache entry; its declared name still finds it, so an
    !! offline reconfigure (the commit hook) reuses its source.
    use fo_test_harness, only: process_result_t, string_list_t, make_scratch, &
        write_text, list_add, assert_true, assert_contains, assert_process_ok, &
        finish_assertions
    use fo_test_cli, only: run_external
    use fo_cmake_generation, only: cmake_populated_dependency_args
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(len=:), allocatable :: scratch, args
    character(len=4096) :: message
    type(string_list_t) :: cmake_args
    type(process_result_t) :: result
    integer :: ierr

    call make_scratch('fo-cmake-populated-declared', scratch)
    call write_text(scratch//'/origin/CMakeLists.txt', 'set(rt_dep_ready TRUE PARENT_SCOPE)'//nl)
    call write_text(scratch//'/project/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(populated_declared LANGUAGES NONE)'//nl//'include(FetchContent)'//nl// &
        'FetchContent_Declare(rt_dep_runtime DOWNLOAD_COMMAND "${CMAKE_COMMAND}" -E '// &
        'copy_directory "'//scratch//'/origin" <SOURCE_DIR>)'//nl// &
        'FetchContent_MakeAvailable(rt_dep_runtime)'//nl)
    call list_add(cmake_args, '-S')
    call list_add(cmake_args, '.')
    call list_add(cmake_args, '-B')
    call list_add(cmake_args, 'build')
    call run_external('cmake', cmake_args, scratch//'/project', result, timeout_ms=180000)
    call assert_process_ok(result, 'configure the FetchContent fixture without File API')
    call cmake_populated_dependency_args(scratch//'/project/build', 'build/_deps', &
        args, ierr, message)
    call assert_true(ierr == 0, 'populated dependency arguments: '//trim(message))
    call assert_contains(args, '-DFETCHCONTENT_SOURCE_DIR_RT_DEP_RUNTIME=', &
        'declared dependency without project() is reused by its declared name')
    call assert_contains(args, '/_deps/rt_dep_runtime-src"', &
        'declared dependency points at its populated source')
    call finish_assertions()
end program test_cmake_populated_declared
