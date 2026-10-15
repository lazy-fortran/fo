program test_cmake_external_exec_cli
    use fo_test_harness, only: string_list_t, process_result_t, make_scratch, &
        join_path, write_lines, write_text, list_add, remove_tree, run_process, &
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
        'add_executable(numeric numeric.f90)', &
        'add_executable(broken broken.f90)'])
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
    ! An unrelated target that does not compile must never block exec numeric.
    call write_text(join_path(project, 'broken.f90'), &
        'program broken'//new_line('a')//'this is not valid fortran ('// &
        new_line('a')//'end program broken'//new_line('a'))
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
    call list_add(arguments, '--target')
    call list_add(arguments, 'numeric')
    call run_process(arguments, scratch, result)
    call assert_process_ok(result, 'native external build')
    arguments = string_list_t()
    call list_add(arguments, join_path(external, 'bin/numeric'))
    call list_add(arguments, '7')
    call run_process(arguments, scratch, result)
    call numeric_ok(result, 'native numeric oracle')

    ! Stale candidates at the canonical CMake root, in the default fpm-style
    ! tree and in a sibling build tree must not supersede the project model's
    ! artifact for the selected build root.
    call write_stale(join_path(external, 'numeric'), 'WRONG_CMAKE_ROOT_EXECUTABLE')
    call write_stale(join_path(project, 'build/bin/numeric'), &
        'WRONG_DEFAULT_EXECUTABLE')
    call write_stale(join_path(project, 'build-stale/bin/numeric'), &
        'WRONG_SIBLING_TREE_EXECUTABLE')

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
    call list_add(arguments, 'numeric')
    call list_add(arguments, '7')
    call run_fo(driver, arguments, project, join_path(scratch, 'cache'), &
        result, environment)
    call numeric_ok(result, 'public no-build relative external root')

    arguments = string_list_t()
    call list_add(arguments, 'exec')
    call list_add(arguments, 'nosuch')
    call run_fo(driver, arguments, project, join_path(scratch, 'cache'), &
        result, environment)
    call assert_true(result%exit_code /= 0, 'unknown target exits nonzero')
    call assert_true(index(result%stderr, 'no such target: nosuch') > 0, &
        'unknown target names the requested target')
    call assert_true(index(result%stderr, 'numeric') > 0, &
        'unknown target lists the nearby executable targets')
    call remove_tree(scratch)
    call finish_assertions()
    print '(a)', 'CMake external exec numeric oracle PASS'
contains
    subroutine write_stale(path, text)
        character(*), intent(in) :: path, text
        type(string_list_t) :: chmod_args
        type(process_result_t) :: chmod_result

        call write_text(path, '#!/bin/sh'//new_line('a')//'echo '//text// &
            new_line('a')//'exit 91'//new_line('a'))
        call list_add(chmod_args, 'chmod')
        call list_add(chmod_args, '+x')
        call list_add(chmod_args, path)
        call run_process(chmod_args, scratch, chmod_result)
        call assert_process_ok(chmod_result, 'make stale candidate executable')
    end subroutine write_stale

    subroutine numeric_ok(observed, label)
        type(process_result_t), intent(in) :: observed
        character(*), intent(in) :: label

        call assert_process_ok(observed, label)
        call assert_true(index(observed%stdout, 'independent numeric result=49') > 0, &
            label//' returns the independent numeric result')
        call assert_true(index(observed%stdout, 'WRONG_') == 0, &
            label//' excludes every stale candidate executable')
    end subroutine numeric_ok
end program test_cmake_external_exec_cli
