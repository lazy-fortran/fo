program test_cmake_external_exec_cli
    use fo_test_harness, only: string_list_t, process_result_t, make_scratch, &
        join_path, write_lines, list_add, remove_tree, run_process, &
        assert_process_ok, assert_true, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none
    character(:), allocatable :: driver, scratch, project, external
    type(string_list_t) :: arguments, environment
    type(process_result_t) :: result

    call resolve_driver(driver)
    call make_scratch('fo-cmake-external-exec', scratch)
    project = join_path(scratch, 'project')
    external = join_path(scratch, 'selected output')
    call write_lines(join_path(project, 'CMakeLists.txt'), [character(len=160) :: &
        'cmake_minimum_required(VERSION 3.20)', &
        'project(external_exec Fortran)', &
        'set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")', &
        'add_executable(numeric numeric.f90)'])
    call write_lines(join_path(project, 'numeric.f90'), [character(len=96) :: &
        'program numeric', &
        'implicit none', &
        'integer :: n', &
        'character(16) :: argument', &
        'call get_command_argument(1,argument)', &
        'read(argument,*) n', &
        'if(n/=7) error stop 17', &
        'print "(a,i0)","independent numeric result=",n*n', &
        'end program'])
    ! Independent native configure/build/run validates the executable and input.
    call list_add(arguments, 'cmake')
    call list_add(arguments, '-S')
    call list_add(arguments, project)
    call list_add(arguments, '-B')
    call list_add(arguments, external)
    call run_process(arguments, scratch, result)
    call assert_process_ok(result, 'native external configure')
    arguments = string_list_t()
    call list_add(arguments, 'cmake')
    call list_add(arguments, '--build')
    call list_add(arguments, external)
    call run_process(arguments, scratch, result)
    call assert_process_ok(result, 'native external build')
    arguments = string_list_t()
    call list_add(arguments, join_path(external, 'bin/numeric'))
    call list_add(arguments, '7')
    call run_process(arguments, scratch, result)
    call numeric_ok(result, 'native numeric oracle')

    ! A stale default-tree candidate must not supersede the selected build root.
    call write_lines(join_path(project, 'build/bin/numeric'), [character(len=80) :: &
        '#!/bin/sh', 'echo WRONG_DEFAULT_EXECUTABLE', 'exit 91'])
    arguments = string_list_t()
    call list_add(arguments, 'chmod')
    call list_add(arguments, '+x')
    call list_add(arguments, join_path(project, 'build/bin/numeric'))
    call run_process(arguments, scratch, result)
    call assert_process_ok(result, 'make stale default candidate executable')
    call list_add(environment, 'FO_BACKEND=cmake')
    call list_add(environment, 'FO_JOBS=2')
    call list_add(environment, 'FO_CMAKE_BUILD_DIR='//external)
    arguments = string_list_t()
    call list_add(arguments, 'exec')
    call list_add(arguments, 'numeric')
    call list_add(arguments, '7')
    call run_fo(driver, arguments, project, join_path(scratch, 'cache'), &
                result, environment)
    call numeric_ok(result, 'public exec absolute external root')

    environment = string_list_t()
    call list_add(environment, 'FO_BACKEND=cmake')
    call list_add(environment, 'FO_JOBS=2')
    call list_add(environment, 'FO_CMAKE_BUILD_DIR=../selected output')
    arguments = string_list_t()
    call list_add(arguments, 'exec')
    call list_add(arguments, '--no-build')
    call list_add(arguments, 'bin/numeric')
    call list_add(arguments, '7')
    call run_fo(driver, arguments, project, join_path(scratch, 'cache'), &
                result, environment)
    call numeric_ok(result, 'public no-build relative external root')
    call remove_tree(scratch)
    call finish_assertions()
    print '(a)', 'CMake external exec numeric oracle PASS'
contains
    subroutine numeric_ok(observed, label)
        type(process_result_t), intent(in) :: observed
        character(*), intent(in) :: label

        call assert_process_ok(observed, label)
        call assert_true(index(observed%stdout, 'independent numeric result=49') > 0, &
                         label//' returns the independent numeric result')
        call assert_true(index(observed%stdout, 'WRONG_DEFAULT_EXECUTABLE') == 0, &
                         label//' excludes a stale default-root executable')
    end subroutine numeric_ok
end program test_cmake_external_exec_cli
