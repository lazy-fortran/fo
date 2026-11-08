program test_hook_cmake_registration
    !! A staged CMakeLists.txt edit that only registers a test runs that test
    !! and the cases the model cannot map (scripts), not every fast test; an
    !! edit that changes an existing setting still runs every fast test.
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, read_text, remove_tree, assert_process_ok, &
        assert_true, assert_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fx_string, only: replace_str
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project, listfile
    type(string_list_t) :: args, env
    type(process_result_t) :: result

    call resolve_driver(driver)
    call make_scratch('fo-hook-cmake-registration', scratch)
    project = scratch//'/project'
    listfile = &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(registration LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'set(SETTING 1)'//nl// &
        'add_executable(check check.f90)'//nl// &
        'foreach(case a b c)'//nl// &
        '    add_test(NAME check_${case} COMMAND check ${case})'//nl// &
        '    set_tests_properties(check_${case} PROPERTIES TIMEOUT 60)'//nl// &
        'endforeach()'//nl// &
        'add_test(NAME script COMMAND ${CMAKE_COMMAND} -E echo script)'//nl
    call write_text(project//'/CMakeLists.txt', listfile)
    call write_text(project//'/check.f90', 'program check'//nl// &
        '    implicit none'//nl//'end program check'//nl)
    call write_text(project//'/.gitignore', 'build/'//nl)
    call git('init', '-q')
    call git('config', 'fo.hookBudget', '600')
    call git('add', '.')
    call git('commit', '-qm', 'fixture')
    env = string_list_t()
    call list_add(env, 'FO_BACKEND=cmake')
    call list_add(env, 'FO_CMAKE_CONFIGURE_PRESET=')
    args = string_list_t()
    call list_add(args, '-S')
    call list_add(args, '.')
    call list_add(args, '-B')
    call list_add(args, 'build')
    call run_external('cmake', args, project, result, env, timeout_ms=90000)
    call assert_process_ok(result, 'populate the developer build')

    call write_text(project//'/CMakeLists.txt', &
        replace_str(listfile, 'foreach(case a b c)', 'foreach(case a b c d)'))
    call git('add', 'CMakeLists.txt')
    call hook()
    call assert_process_ok(result, 'registration commit passes')
    call assert_contains(result%stdout, '"fast_passed":2', &
        'only the registered test and the unmapped script run: '//result%stdout)
    call git('commit', '-qm', 'register check_d')

    call write_text(project//'/CMakeLists.txt', replace_str(replace_str(listfile, &
        'foreach(case a b c)', 'foreach(case a b c d)'), 'set(SETTING 1)', &
        'set(SETTING 2)'))
    call git('add', 'CMakeLists.txt')
    call hook()
    call assert_process_ok(result, 'setting commit passes')
    call assert_contains(result%stdout, '"fast_passed":5', &
        'a changed setting runs every fast test: '//result%stdout)
    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine git(first, second, third)
        character(len=*), intent(in) :: first
        character(len=*), intent(in), optional :: second, third
        type(string_list_t) :: git_args, git_env

        call list_add(git_args, first)
        if (present(second)) call list_add(git_args, second)
        if (present(third)) call list_add(git_args, third)
        call list_add(git_env, 'GIT_CONFIG_GLOBAL=')
        call list_add(git_env, 'GIT_AUTHOR_NAME=fo')
        call list_add(git_env, 'GIT_AUTHOR_EMAIL=fo@example.invalid')
        call list_add(git_env, 'GIT_COMMITTER_NAME=fo')
        call list_add(git_env, 'GIT_COMMITTER_EMAIL=fo@example.invalid')
        call run_external('git', git_args, project, result, git_env, timeout_ms=90000)
        call assert_process_ok(result, 'fixture git '//first)
    end subroutine git

    subroutine hook()
        args = string_list_t()
        call list_add(args, 'hook')
        call list_add(args, 'pre-commit')
        call list_add(args, '--json')
        call run_fo(driver, args, project, scratch//'/cache', result, env, &
            timeout_ms=180000)
    end subroutine hook

end program test_hook_cmake_registration
