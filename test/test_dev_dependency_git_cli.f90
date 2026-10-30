program test_dev_dependency_git_cli
    !! `fo test` on a clean tree acquires a Git dev-dependency itself; no earlier
    !! `fo build` has to fetch it.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory, write_text
    use fo_test_harness, only: remove_tree, run_git, assert_equal_integer
    use fo_test_harness, only: assert_contains, finish_assertions, assert_true
    use fo_test_harness, only: read_text, file_exists, resolve_test_executable
    use fo_dep_update, only: dep_acquire_sources
    use fo_process, only: process_exit
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, provider, consumer
    type(process_result_t) :: result
    type(string_list_t) :: args, environment
    integer :: code
    character(len=64) :: mode

    call get_command_argument(1, mode)
    if (trim(mode) == "--relative-acquire") then
        call dep_acquire_sources(".", code)
        call process_exit(code)
    end if
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

    call fpm_checkout()
    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine fpm_checkout()
        character(len=:), allocatable :: root, checkout, revision, fetched, marker, self
        type(string_list_t) :: settings
        logical :: found

        root = join_path(scratch, "fpm-consumer")
        checkout = root//"/build/dependencies/dev_provider"
        marker = root//"/build/dependencies/.fo-git-identities/dev_provider"
        args = string_list_t()
        call list_add(args, "rev-parse")
        call list_add(args, "HEAD")
        call run_external("git", args, provider, result, environment)
        call assert_equal_integer(result%exit_code, 0, "read provider revision")
        revision = result%stdout(:index(result%stdout, nl) - 1)
        call write_text(root//"/fpm.toml", 'name = "fpm_consumer"'//nl// &
            "[dependencies]"//nl// &
            'dev_provider = { git = "https://example.invalid/provider.git", rev = "'// &
            revision//'" }'//nl)
        call write_text(root//"/test/test_value.f90", &
            read_text(consumer//"/test/test_value.f90"))
        call make_directory(checkout)
        call run_git(checkout, [character(len=32) :: "init", "-q"], code)
        call assert_equal_integer(code, 0, "initialize fpm checkout")
        call run_git(checkout, [character(len=2048) :: "fetch", "--depth=1", &
            "file://"//provider//"/.git", revision], code)
        call assert_equal_integer(code, 0, "fetch pinned object as fpm does")
        call run_git(checkout, [character(len=32) :: "checkout", "-qf", "FETCH_HEAD"], code)
        call assert_equal_integer(code, 0, "checkout FETCH_HEAD as fpm does")
        fetched = read_text(checkout//"/.git/FETCH_HEAD")
        call write_text(scratch//"/mirror-config", &
            '[url "file://'//provider//'/.git"]'//nl// &
            "    insteadOf = https://example.invalid/provider.git"//nl)
        call list_add(settings, "GIT_CONFIG_NOSYSTEM=1")
        call list_add(settings, "GIT_CONFIG_GLOBAL="//scratch//"/mirror-config")
        call list_add(settings, "GIT_ALLOW_PROTOCOL=file")
        call list_add(settings, "TMPDIR="//scratch//"/tmp")
        args = string_list_t()
        call list_add(args, "build")
        call list_add(args, "--tests")
        call list_add(args, "--case")
        call list_add(args, "test_value")
        call write_text(checkout//"/.git/FETCH_HEAD", revision//achar(9)//achar(9)// &
            "'"//revision//"' of https://example.invalid/wrong"//nl)
        call run_fo(driver, args, root, scratch//"/fpm-cache", result, settings)
        call assert_true(result%exit_code /= 0, "wrong fetched URL is rejected")
        call assert_true(.not. file_exists(marker), "wrong URL is not blessed")
        call write_text(checkout//"/.git/FETCH_HEAD", fetched)
        call write_text(checkout//"/src/provider.f90", &
            read_text(provider//"/src/provider.f90")//"! dirty tracked source"//nl)
        call run_fo(driver, args, root, scratch//"/fpm-cache", result, settings)
        call assert_true(result%exit_code /= 0, "dirty fpm checkout is rejected")
        call assert_true(.not. file_exists(marker), "dirty fpm checkout is not blessed")
        call write_text(checkout//"/src/provider.f90", &
            read_text(provider//"/src/provider.f90"))
        block
            type(string_list_t) :: words
            character(len=:), allocatable :: later

            call run_git(provider, [character(len=32) :: "commit", "--allow-empty", &
                "-qm", "Later provider"], code)
            call assert_equal_integer(code, 0, "create different provider commit")
            call list_add(words, "rev-parse")
            call list_add(words, "HEAD")
            call run_external("git", words, provider, result, environment)
            later = result%stdout(:index(result%stdout, nl) - 1)
            call run_git(checkout, [character(len=2048) :: "fetch", "--depth=1", &
                "file://"//provider//"/.git", later], code)
            call assert_equal_integer(code, 0, "fetch another provider commit")
            call run_git(checkout, [character(len=128) :: "checkout", "-qf", later], code)
            call assert_equal_integer(code, 0, "select wrong provider HEAD")
            call write_text(checkout//"/.git/FETCH_HEAD", fetched)
            call run_fo(driver, args, root, scratch//"/fpm-cache", result, settings)
            call assert_true(result%exit_code /= 0, "wrong fetched HEAD is rejected")
            call assert_true(.not. file_exists(marker), "wrong fetched HEAD is not blessed")
            call run_git(checkout, [character(len=128) :: "checkout", "-qf", revision], code)
            call assert_equal_integer(code, 0, "restore pinned provider HEAD")
        end block
        call run_fo(driver, args, root, scratch//"/fpm-cache", result, settings)
        call assert_equal_integer(result%exit_code, 0, &
            "prepare tests from pinned fpm checkout: "//result%stderr)
        call assert_true(file_exists(marker), "pinned fetched checkout gains identity")
        call remove_tree(marker)
        call resolve_test_executable(self, found)
        call assert_true(found, "resolve relative acquisition probe")
        args = string_list_t()
        call list_add(args, "--relative-acquire")
        call list_add(settings, "GIT_DIR="//provider//"/.git")
        call list_add(settings, "GIT_WORK_TREE="//provider)
        call run_external(self, args, root, result, settings)
        call assert_equal_integer(result%exit_code, 0, &
            "relative checkout ignores ambient Git repository: "//result%stdout//result%stderr)
        call assert_true(file_exists(marker), "relative acquisition records identity")
    end subroutine fpm_checkout
end program test_dev_dependency_git_cli
