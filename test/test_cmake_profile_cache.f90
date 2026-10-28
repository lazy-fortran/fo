program test_cmake_profile_cache
    !! A Fo profile must not rewrite the cache of a build tree the user
    !! configured, because later plain CMake/CTest runs there inherit it.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_lines, read_text
    use fo_test_harness, only: remove_tree, run_process, assert_process_ok
    use fo_test_harness, only: assert_contains, assert_not_contains
    use fo_test_harness, only: finish_assertions, harness_probe_entry
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(:), allocatable :: driver, root, before, after
    type(string_list_t) :: args, env
    type(process_result_t) :: observed

    call harness_probe_entry()
    call resolve_driver(driver)
    call make_scratch('fo-cmake-profile-cache', root)
    call write_lines(join_path(root, 'CMakeLists.txt'), [character(len=160) :: &
        'cmake_minimum_required(VERSION 3.20)', &
        'project(fo_profile_cache Fortran)', &
        'enable_testing()', &
        'add_executable(checked checked.f90)', &
        'add_test(NAME checked COMMAND checked)'])
    ! Passes only when Fo's Debug profile flags reach the compiler.
    call write_lines(join_path(root, 'checked.f90'), [character(len=160) :: &
        'program checked', &
        'use, intrinsic :: iso_fortran_env, only: compiler_options', &
        'if (index(compiler_options(), "-fcheck") == 0) error stop "no checks"', &
        'end program checked'])

    call list_add(args, 'cmake')
    call list_add(args, '-S')
    call list_add(args, '.')
    call list_add(args, '-B')
    call list_add(args, 'build')
    call list_add(args, '-DCMAKE_Fortran_COMPILER=gfortran')
    call list_add(args, '-DCMAKE_BUILD_TYPE=Release')
    call list_add(args, '-DCMAKE_Fortran_FLAGS_DEBUG=-g')
    call run_process(args, root, observed)
    call assert_process_ok(observed, 'user configures the project')
    before = read_text(join_path(root, 'build/CMakeCache.txt'))
    call assert_contains(before, 'CMAKE_Fortran_FLAGS_DEBUG:STRING=-g' // &
        new_line('a'), 'user Debug flags recorded')

    call list_add(env, 'FO_BACKEND=cmake')
    call list_add(env, 'FO_JOBS=2')
    call list_add(env, 'FC=gfortran')
    args = string_list_t()
    call list_add(args, 'test')
    call list_add(args, '--profile')
    call list_add(args, 'debug')
    call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
    call assert_process_ok(observed, 'Fo Debug profile test with checks')

    after = read_text(join_path(root, 'build/CMakeCache.txt'))
    call assert_contains(after, 'CMAKE_Fortran_FLAGS_DEBUG:STRING=-g' // &
        new_line('a'), 'user Debug flags survive a Fo profile run')
    call assert_contains(after, 'CMAKE_BUILD_TYPE:STRING=Release' // &
        new_line('a'), 'user build type survives a Fo profile run')
    call assert_not_contains(after, '-fcheck', &
        'Fo profile flags stay out of the user cache')
    call remove_tree(root)
    call finish_assertions()
end program test_cmake_profile_cache
