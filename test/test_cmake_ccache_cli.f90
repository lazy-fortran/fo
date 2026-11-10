program test_cmake_ccache_cli
    !! In a delegated CMake build (CMake's generator compiles, as in the
    !! pre-commit hook) C and C++ go through ccache when it is installed: the
    !! second build of the same sources in a fresh tree hits the cache, and
    !! FO_CCACHE=0 leaves the launcher out. Skipped without ccache.
    !! Runs the driver named by FO.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, read_text
    use fo_test_harness, only: remove_tree, assert_equal_integer, assert_true
    use fo_test_harness, only: assert_contains, assert_not_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(:), allocatable :: driver, scratch, project, cache_dir
    type(string_list_t) :: env, arguments, stats, stats_env
    type(process_result_t) :: result
    integer :: probe

    call execute_command_line('command -v ccache >/dev/null 2>&1', exitstat=probe)
    if (probe /= 0) then
        write (*, '(a)') 'cmake-ccache-cli: ccache not installed, skipped'
        stop
    end if
    call resolve_driver(driver)
    call make_scratch('fo-cmake-ccache', scratch)
    project = join_path(scratch, 'project')
    cache_dir = join_path(scratch, 'ccache')
    call write_text(join_path(project, 'CMakeLists.txt'), &
        'cmake_minimum_required(VERSION 3.20)'//nl//'project(cc LANGUAGES CXX)'//nl// &
        'add_executable(hello hello.cpp)'//nl)
    call write_text(join_path(project, 'hello.cpp'), &
        '#include <cstdio>'//nl//'int main() { std::puts("hi"); return 0; }'//nl)
    call list_add(env, 'FO_BACKEND=cmake')
    call list_add(env, 'CCACHE_DIR='//cache_dir)
    call list_add(env, 'FO_CMAKE_DELEGATE_BUILD=1')
    call list_add(arguments, 'build')

    call run_fo(driver, arguments, project, join_path(scratch, 'cache'), result, env, 240000)
    call assert_equal_integer(result%exit_code, 0, 'first build: '//result%stderr)
    call assert_contains(read_text(join_path(project, 'build/CMakeCache.txt')), &
        'CMAKE_CXX_COMPILER_LAUNCHER', 'ccache is the C++ compiler launcher')
    call remove_tree(join_path(project, 'build'))
    call run_fo(driver, arguments, project, join_path(scratch, 'cache'), result, env, 240000)
    call assert_equal_integer(result%exit_code, 0, 'second build: '//result%stderr)
    call list_add(stats, '--print-stats')
    call list_add(stats_env, 'CCACHE_DIR='//cache_dir)
    call run_external('ccache', stats, project, result, stats_env)
    call assert_true(index(result%stdout, 'direct_cache_hit'//achar(9)//'1') > 0, &
        'the rebuild in a fresh tree hits the cache: '//result%stdout)

    call remove_tree(join_path(project, 'build'))
    call list_add(env, 'FO_CCACHE=0')
    call run_fo(driver, arguments, project, join_path(scratch, 'cache'), result, env, 240000)
    call assert_equal_integer(result%exit_code, 0, 'build without ccache: '//result%stderr)
    call assert_not_contains(read_text(join_path(project, 'build/CMakeCache.txt')), &
        'CMAKE_CXX_COMPILER_LAUNCHER:', 'FO_CCACHE=0 leaves the launcher out')
    call remove_tree(scratch)
    call finish_assertions()
end program test_cmake_ccache_cli
