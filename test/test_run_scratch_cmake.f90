program test_run_scratch_cmake
    !! `fo run FILE.f90` builds a scratch program outside a CMake project
    !! against the project's libraries and modules and runs it. Its objects
    !! and modules stay out of the project and its build tree.
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, remove_tree, assert_process_ok, assert_true, &
        assert_contains, assert_not_contains, finish_assertions, file_exists
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project
    type(string_list_t) :: env
    type(process_result_t) :: result

    call resolve_driver(driver)
    call make_scratch('fo-run-scratch', scratch)
    project = scratch//'/project'
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(scratch_host LANGUAGES Fortran)'//nl// &
        'add_library(core src/core.f90)'//nl// &
        'set_target_properties(core PROPERTIES Fortran_MODULE_DIRECTORY '// &
        '${CMAKE_BINARY_DIR}/mod/core)'//nl// &
        'add_library(extra src/extra.f90)'//nl// &
        'target_link_libraries(extra PUBLIC core)'//nl// &
        'set_target_properties(extra PROPERTIES Fortran_MODULE_DIRECTORY '// &
        '${CMAKE_BINARY_DIR}/mod/extra)'//nl// &
        'add_executable(big app/big.f90)'//nl// &
        'target_link_libraries(big PRIVATE extra)'//nl// &
        'add_executable(small app/small.f90)'//nl// &
        'target_link_libraries(small PRIVATE core)'//nl// &
        'add_executable(other app/other.f90)'//nl)
    call write_text(project//'/src/core.f90', &
        'module core_mod'//nl//'implicit none'//nl//'contains'//nl// &
        'integer function base()'//nl//'base = 40'//nl//'end function base'//nl// &
        'end module core_mod'//nl)
    call write_text(project//'/src/extra.f90', &
        'module extra_mod'//nl//'use core_mod, only: base'//nl//'implicit none'//nl// &
        'contains'//nl//'integer function answer()'//nl//'answer = base() + 2'//nl// &
        'end function answer'//nl//'end module extra_mod'//nl)
    call write_text(project//'/app/big.f90', 'program big'//nl// &
        'use extra_mod, only: answer'//nl//'print *, answer()'//nl//'end program big'//nl)
    call write_text(project//'/app/small.f90', 'program small'//nl// &
        'use core_mod, only: base'//nl//'print *, base()'//nl//'end program small'//nl)
    call write_text(project//'/app/other.f90', 'program other'//nl// &
        'end program other'//nl)
    call write_text(scratch//'/probe.f90', &
        'module probe_helpers'//nl//'implicit none'//nl//'contains'//nl// &
        'integer function twice(x)'//nl//'integer, intent(in) :: x'//nl// &
        'twice = 2*x'//nl//'end function twice'//nl//'end module probe_helpers'//nl// &
        'program probe'//nl//'use extra_mod, only: answer'//nl// &
        'use probe_helpers, only: twice'//nl//'character(len=16) :: word'//nl// &
        'call get_command_argument(1, word)'//nl// &
        'print ''(a,i0)'', trim(word)//'' '', twice(answer())'//nl// &
        'end program probe'//nl)
    call write_text(scratch//'/core_probe.f90', 'program core_probe'//nl// &
        'use core_mod, only: base'//nl//'print ''(a,i0)'', ''base '', base()'//nl// &
        'end program core_probe'//nl)
    call list_add(env, 'FO_BACKEND=cmake')
    call list_add(env, 'FO_CMAKE_BUILD_DIR=build')
    call list_add(env, 'FO_CMAKE_CONFIGURE_PRESET=')

    call run(scratch//'/probe.f90', 'value')
    call assert_process_ok(result, 'the scratch program builds and runs')
    call assert_contains(result%stdout, 'value 84', &
        'it links the project libraries and passes arguments: '//result%stdout)
    call assert_contains(result%stdout//result%stderr, 'link line of big', &
        'the only executable linking extra donates its settings')
    call assert_true(.not. file_exists(project//'/build/probe_helpers.mod') .and. &
        .not. file_exists(project//'/build/mod/extra/probe_helpers.mod') .and. &
        .not. file_exists(project//'/build/mod/core/probe_helpers.mod') .and. &
        .not. file_exists(project//'/probe_helpers.mod') .and. &
        .not. file_exists(scratch//'/probe_helpers.mod'), &
        'the program''s own module stays private')
    call assert_true(.not. file_exists(scratch//'/probe') .and. &
        .not. file_exists(project//'/build/probe'), 'the program binary stays private')

    call run(scratch//'/core_probe.f90', '')
    call assert_process_ok(result, 'a program using only core runs')
    call assert_contains(result%stdout, 'base 40', 'it prints the core value')
    call assert_contains(result%stdout//result%stderr, 'link line of small', &
        'the smallest executable linking core donates its settings')
    call assert_not_contains(result%stdout//result%stderr, 'link line of big', &
        'a larger link closure is not chosen')
    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine run(file, argument)
        character(len=*), intent(in) :: file, argument
        type(string_list_t) :: args

        call list_add(args, 'run')
        call list_add(args, file)
        if (len(argument) > 0) call list_add(args, argument)
        call run_fo(driver, args, project, scratch//'/cache', result, env, &
            timeout_ms=180000)
    end subroutine run

end program test_run_scratch_cmake
