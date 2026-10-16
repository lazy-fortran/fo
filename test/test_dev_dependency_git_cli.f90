program test_dev_dependency_git_cli
    !! `fo test` on a clean tree acquires a Git dev-dependency itself; no earlier
    !! `fo build` has to fetch it.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory, write_text
    use fo_test_harness, only: remove_tree, run_git, assert_equal_integer
    use fo_test_harness, only: assert_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, provider, consumer
    type(process_result_t) :: result
    type(string_list_t) :: args, environment
    integer :: code

    call resolve_driver(driver)
    call make_scratch('fo-dev-dependency-git', scratch)
    provider = join_path(scratch, 'provider')
    consumer = join_path(scratch, 'consumer')
    call make_directory(join_path(scratch, 'tmp'))
    call list_add(environment, 'GIT_CONFIG_NOSYSTEM=1')
    call list_add(environment, 'GIT_CONFIG_GLOBAL=/dev/null')
    call list_add(environment, 'GIT_TERMINAL_PROMPT=0')
    call list_add(environment, 'TMPDIR='//join_path(scratch, 'tmp'))

    call write_text(join_path(provider, 'fpm.toml'), 'name = "dev_provider"'//nl)
    call write_text(join_path(provider, 'src/provider.f90'), &
        'module dev_provider'//nl//'implicit none'//nl//'contains'//nl// &
        'integer function value()'//nl//'value = 41'//nl// &
        'end function value'//nl//'end module dev_provider'//nl)
    call run_git(provider, [character(len=32) :: 'init', '-q', '-b', 'main'], code)
    call assert_equal_integer(code, 0, 'git init')
    call run_git(provider, [character(len=32) :: 'add', 'fpm.toml', &
        'src/provider.f90'], code)
    call run_git(provider, [character(len=32) :: 'commit', '-q', '-m', 'provider'], code)
    call assert_equal_integer(code, 0, 'git commit')
    call run_git(provider, [character(len=32) :: 'tag', '--no-sign', '--no-annotate', &
        'v1'], code)
    call assert_equal_integer(code, 0, 'git tag')

    call write_text(join_path(consumer, 'fpm.toml'), 'name = "dev_consumer"'//nl// &
        '[dev-dependencies]'//nl//'dev_provider = { git = "file://'//provider// &
        '", tag = "v1" }'//nl)
    call write_text(join_path(consumer, 'test/test_value.f90'), &
        'program test_value'//nl//'use dev_provider, only: value'//nl// &
        'implicit none'//nl//'if (value() /= 41) error stop 1'//nl// &
        'end program test_value'//nl)

    call list_add(args, 'test')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, environment)
    call assert_equal_integer(result%exit_code, 0, &
        'fo test on a clean tree fetches the Git dev-dependency: '//result%stderr)
    call assert_contains(result%stdout, '1 pass', 'the test using the dependency passes')

    call remove_tree(scratch)
    call finish_assertions()
end program test_dev_dependency_git_cli
