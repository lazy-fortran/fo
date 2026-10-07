program test_native_git_dependency_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory
    use fo_test_harness, only: make_symlink, move_path
    use fo_test_harness, only: write_text, remove_tree, file_exists
    use fo_test_harness, only: assert_process_ok, assert_equal_string, assert_true
    use fo_test_harness, only: assert_contains
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, tools, dependency, consumer
    character(:), allocatable :: offline_dependency
    character(:), allocatable :: path_dependency, nested_consumer
    character(:), allocatable :: git, compiler, assembler, linker, archiver
    character(:), allocatable :: commit, moved_commit, latest_commit, checkout
    type(process_result_t) :: result
    type(string_list_t) :: args, environment

    call resolve_driver(driver)
    call make_scratch('fo-native-git-dependency', scratch)
    call list_add(environment, 'GIT_CONFIG_NOSYSTEM=1')
    call list_add(environment, 'GIT_CONFIG_GLOBAL=/dev/null')
    call list_add(environment, 'GIT_TERMINAL_PROMPT=0')
    tools = join_path(scratch, 'tools')
    dependency = join_path(scratch, 'provider')
    consumer = join_path(scratch, 'consumer')
    call make_directory(tools)
    call find_command('git', git)
    call find_command('gfortran', compiler)
    call find_command('as', assembler)
    call find_command('ld', linker)
    call find_command('ar', archiver)
    call make_symlink(git, join_path(tools, 'git'))
    call make_symlink(compiler, join_path(tools, 'gfortran'))
    call make_symlink(assembler, join_path(tools, 'as'))
    call make_symlink(linker, join_path(tools, 'ld'))
    call make_symlink(archiver, join_path(tools, 'ar'))
    call write_text(join_path(dependency, 'fpm.toml'), &
        'name = "git_provider"' // new_line('a'))
    call write_text(join_path(dependency, 'src/provider.f90'), &
        'module git_provider' // new_line('a') // 'implicit none' // new_line('a') // &
        'contains' // new_line('a') // 'integer function value()' // new_line('a') // &
        'value = 41' // new_line('a') // 'end function value' // new_line('a') // &
        'end module git_provider' // new_line('a'))

    call git_command([character(len=32) :: 'init', '-q', '-b', 'main'], dependency)
    call git_command([character(len=32) :: 'config', 'user.name', 'Fo test'], &
        dependency)
    call git_command([character(len=32) :: 'config', 'user.email', &
        'fo-test@example.invalid'], dependency)
    call git_command([character(len=32) :: 'add', 'fpm.toml', &
        'src/provider.f90'], dependency)
    call git_command([character(len=32) :: 'commit', '-q', '-m', &
        'pinned provider'], dependency)
    call git_command([character(len=32) :: 'tag', '--no-sign', '--no-annotate', &
        'provider-v1'], dependency)
    args = string_list_t()
    call list_add(args, '-C')
    call list_add(args, dependency)
    call list_add(args, 'rev-parse')
    call list_add(args, 'HEAD')
    call run_external(git, args, scratch, result, environment)
    call assert_process_ok(result, 'read independent pinned Git revision')
    commit = without_line_ending(result%stdout)
    call write_text(join_path(dependency, 'src/provider.f90'), &
        'module git_provider' // new_line('a') // 'implicit none' // new_line('a') // &
        'contains' // new_line('a') // 'integer function value()' // new_line('a') // &
        'value = 99' // new_line('a') // 'end function value' // new_line('a') // &
        'end module git_provider' // new_line('a'))
    call git_command([character(len=32) :: 'add', 'src/provider.f90'], &
        dependency)
    call git_command([character(len=32) :: 'commit', '-q', '-m', &
        'move branch after pin'], dependency)

    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "native_git_probe"' // new_line('a') // '[dependencies]' // &
        new_line('a') // 'git_provider = { git = "file://' // dependency // &
        '", rev = "' // commit // '" }' // new_line('a'))
    call write_text(join_path(consumer, 'app/probe.f90'), &
        'program probe' // new_line('a') // &
        'use git_provider, only: value' // new_line('a') // 'implicit none' // &
        new_line('a') // "print '(i0)', value()" // new_line('a') // &
        'end program probe' // new_line('a'))
    call list_add(environment, 'PATH=' // tools)
    call list_add(environment, 'FO_JOBS=2')
    call list_add(environment, 'TMPDIR=' // join_path(scratch, 'tmp'))
    call make_directory(join_path(scratch, 'tmp'))
    call assert_true(.not. file_exists(join_path(tools, 'fpm')), &
        'FPM is absent from the Fo process PATH')

    args = string_list_t()
    call list_add(args, 'build')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
        environment)
    call assert_process_ok(result, &
        'Fo builds a missing pinned Git dependency without FPM')
    checkout = join_path(consumer, 'build/dependencies/git_provider')
    call assert_checkout_commit(commit)

    args = string_list_t()
    call list_add(args, 'exec')
    call list_add(args, 'probe')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
        environment)
    call assert_process_ok(result, &
        'Fo runs app linked to native Git dependency')
    call assert_equal_string(result%stdout, '41' // new_line('a'), &
        'runtime output matches the independently pinned source behavior')

    call remove_tree(checkout)
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "native_git_probe"' // new_line('a') // '[dependencies]' // &
        new_line('a') // 'git_provider = { git = "file://' // dependency // &
        '", tag = "provider-v1" }' // new_line('a'))
    args = string_list_t()
    call list_add(args, 'build')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
        environment)
    call assert_process_ok(result, 'Fo acquires a Git dependency pinned by tag')
    call assert_checkout_commit(commit)
    args = string_list_t()
    call list_add(args, 'exec')
    call list_add(args, 'probe')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
        environment)
    call assert_process_ok(result, 'Fo runs app linked to tag-pinned dependency')
    call assert_equal_string(result%stdout, '41' // new_line('a'), &
        'tag selects the same pinned source after the repository HEAD advances')

    call remove_tree(join_path(checkout, '.git'))
    call assert_true(.not. file_exists(join_path(checkout, '.git')), &
        'materialized Git dependency source has no Git metadata')
    offline_dependency = dependency//'.offline'
    call move_path(dependency, offline_dependency)
    args = string_list_t()
    call list_add(args, 'build')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
        environment)
    call assert_process_ok(result, &
        'Fo accepts a materialized Git dependency without Git metadata')
    args = string_list_t()
    call list_add(args, 'exec')
    call list_add(args, 'probe')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
        environment)
    call assert_process_ok(result, &
        'Fo runs app linked to materialized dependency without Git metadata')
    call assert_equal_string(result%stdout, '41' // new_line('a'), &
        'materialized dependency source preserves the pinned runtime behavior')
    call move_path(offline_dependency, dependency)

    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "native_git_probe"' // new_line('a') // '[dependencies]' // &
        new_line('a') // 'git_provider = { git = "file://' // dependency // &
        '", branch = "main" }' // new_line('a'))
    call run_build()
    moved_commit = current_commit()
    call assert_checkout_commit(moved_commit)
    call run_probe()
    call assert_equal_string(result%stdout, '99' // new_line('a'), &
        'moving branch initially builds its independently identified source')
    moved_commit = current_commit()

    call write_provider_value(137, 'advance remote while branch is cached')
    latest_commit = current_commit()
    call run_build()
    call assert_checkout_commit(moved_commit)
    call run_probe()
    call assert_equal_string(result%stdout, '99' // new_line('a'), &
        'ordinary build reuses the resolved branch commit without fetching')

    args = string_list_t()
    call list_add(args, 'update')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
        environment)
    call assert_process_ok(result, 'fo update refreshes the cached Git branch')
    call run_build()
    call assert_checkout_commit(latest_commit)
    call run_probe()
    call assert_equal_string(result%stdout, '137' // new_line('a'), &
        'fo update captures the new branch commit and consumer behavior')

    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "native_git_probe"' // new_line('a') // '[dependencies]' // &
        new_line('a') // 'git_provider = { git = "file://' // dependency // &
        '", rev = "' // commit // '" }' // new_line('a'))
    call run_build()
    call assert_checkout_commit(commit)
    call run_probe()
    call assert_equal_string(result%stdout, '41' // new_line('a'), &
        'editing branch pin to rev reacquires and invalidates the old checkout')

    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "native_git_probe"' // new_line('a') // '[dependencies]' // &
        new_line('a') // 'git_provider = { git = "file://' // dependency // &
        '", tag = "provider-v1" }' // new_line('a'))
    call run_build()
    call assert_checkout_commit(commit)
    call run_probe()
    call assert_equal_string(result%stdout, '41' // new_line('a'), &
        'editing rev pin to tag resolves the tag instead of reusing stale source')

    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "native_git_probe"' // new_line('a') // '[dependencies]' // &
        new_line('a') // 'git_provider = { git = "file://' // dependency // &
        '", rev = "missing-revision" }' // new_line('a'))
    args = string_list_t()
    call list_add(args, 'build')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
        environment)
    call assert_true(result%exit_code /= 0, &
        'missing pinned revision fails instead of reusing prior source')
    call assert_contains(result%stderr, 'cannot select Git ref', &
        'missing pinned revision reports an actionable acquisition error')

    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "native_git_probe"' // new_line('a') // '[dependencies]' // &
        new_line('a') // 'git_provider = { git = "file://' // dependency // &
        '", tag = "provider-v1" }' // new_line('a'))
    call run_build()
    call assert_checkout_commit(commit)
    call run_probe()
    call assert_equal_string(result%stdout, '41' // new_line('a'), &
        'valid pin recovers after missing revision failure')

    path_dependency = join_path(scratch, 'path_dependency')
    nested_consumer = join_path(scratch, 'nested_consumer')
    call write_text(join_path(path_dependency, 'fpm.toml'), &
        'name = "path_dependency"' // new_line('a') // '[dependencies]' // &
        new_line('a') // 'git_provider = { git = "../provider", rev = "' // &
        commit // '" }' // new_line('a'))
    call write_text(join_path(path_dependency, 'src/path_provider.f90'), &
        'module path_provider' // new_line('a') // &
        'use git_provider, only: value' // new_line('a') // 'implicit none' // &
        new_line('a') // 'contains' // new_line('a') // &
        'integer function nested_value()' // new_line('a') // &
        'nested_value = value()' // new_line('a') // &
        'end function nested_value' // new_line('a') // &
        'end module path_provider' // new_line('a'))
    call write_text(join_path(nested_consumer, 'fpm.toml'), &
        'name = "nested_git_probe"' // new_line('a') // '[dependencies]' // &
        new_line('a') // 'path_dependency = { path = "../path_dependency" }' // &
        new_line('a'))
    call write_text(join_path(nested_consumer, 'app/nested.f90'), &
        'program nested' // new_line('a') // &
        'use path_provider, only: nested_value' // new_line('a') // &
        'implicit none' // new_line('a') // "print '(i0)', nested_value()" // &
        new_line('a') // 'end program nested' // new_line('a'))
    args = string_list_t()
    call list_add(args, 'build')
    call run_fo(driver, args, nested_consumer, join_path(scratch, 'cache'), &
        result, environment)
    call assert_process_ok(result, &
        'nested relative Git source resolves from its declaring package')
    checkout = join_path(nested_consumer, 'build/dependencies/git_provider')
    call assert_checkout_commit(commit)
    args = string_list_t()
    call list_add(args, 'exec')
    call list_add(args, 'nested')
    call run_fo(driver, args, nested_consumer, join_path(scratch, 'cache'), &
        result, environment)
    call assert_process_ok(result, 'nested Git dependency app runs without FPM')
    call assert_equal_string(result%stdout, '41' // new_line('a'), &
        'nested relative Git dependency preserves its pinned behavior')

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') &
        'native-git-dependency-cli: exact rev and tag build and run without FPM'

contains

    subroutine assert_checkout_commit(expected)
        character(len=*), intent(in) :: expected

        args = string_list_t()
        call list_add(args, '-C')
        call list_add(args, checkout)
        call list_add(args, 'rev-parse')
        call list_add(args, 'HEAD')
        call run_external(git, args, scratch, result, environment)
        call assert_process_ok(result, 'inspect acquired checkout identity')
        call assert_equal_string(without_line_ending(result%stdout), expected, &
            'acquired checkout is the exact declared revision')
    end subroutine assert_checkout_commit

    subroutine run_build()
        args = string_list_t()
        call list_add(args, 'build')
        call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
            environment)
        call assert_process_ok(result, 'build native Git dependency consumer')
    end subroutine run_build

    subroutine run_probe()
        args = string_list_t()
        call list_add(args, 'exec')
        call list_add(args, 'probe')
        call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
            environment)
        call assert_process_ok(result, 'run native Git dependency consumer')
    end subroutine run_probe

    function current_commit() result(value)
        character(:), allocatable :: value

        args = string_list_t()
        call list_add(args, '-C')
        call list_add(args, dependency)
        call list_add(args, 'rev-parse')
        call list_add(args, 'HEAD')
        call run_external(git, args, scratch, result, environment)
        call assert_process_ok(result, 'read independent Git commit identity')
        value = without_line_ending(result%stdout)
    end function current_commit

    subroutine write_provider_value(number, subject)
        integer, intent(in) :: number
        character(len=*), intent(in) :: subject
        character(len=16) :: digits

        write (digits, '(i0)') number
        call write_text(join_path(dependency, 'src/provider.f90'), &
            'module git_provider' // new_line('a') // 'implicit none' // &
            new_line('a') // 'contains' // new_line('a') // &
            'integer function value()' // new_line('a') // &
            'value = '//trim(digits) // new_line('a') // &
            'end function value' // new_line('a') // &
            'end module git_provider' // new_line('a'))
        call git_command([character(len=48) :: 'add', 'src/provider.f90'], &
            dependency)
        call git_command([character(len=48) :: 'commit', '-q', '-m', subject], &
            dependency)
    end subroutine write_provider_value

    function without_line_ending(text) result(value)
        character(len=*), intent(in) :: text
        character(:), allocatable :: value
        integer :: n

        n = len_trim(text)
        if (n > 0) then
            if (text(n:n) == new_line('a')) n = n - 1
        end if
        if (n > 0) then
            value = text(:n)
        else
            value = ''
        end if
    end function without_line_ending

    subroutine find_command(name, path)
        character(len=*), intent(in) :: name
        character(:), allocatable, intent(out) :: path
        character(len=4096) :: search_path
        character(len=512) :: candidate
        integer :: length, status, start, finish, i

        call get_environment_variable('PATH', search_path, length, status)
        call assert_true(status == 0 .and. length > 0, &
            'read process PATH while locating '//name)
        start = 1
        do i = 1, length + 1
            if (i <= length) then
                if (search_path(i:i) /= ':') cycle
            end if
            finish = i - 1
            if (finish < start) then
                start = i + 1
                cycle
            end if
            candidate = trim(search_path(start:finish))//'/'//trim(name)
            if (file_exists(trim(candidate))) then
                path = trim(candidate)
                return
            end if
            start = i + 1
        end do
        call assert_true(.false., 'cannot locate required tool '//name)
        path = ''
    end subroutine find_command

    subroutine git_command(words, cwd)
        character(len=*), intent(in) :: words(:), cwd
        integer :: i

        args = string_list_t()
        call list_add(args, '-C')
        call list_add(args, cwd)
        do i = 1, size(words)
            call list_add(args, trim(words(i)))
        end do
        call run_external(git, args, cwd, result, environment)
        call assert_process_ok(result, 'create local Git fixture')
    end subroutine git_command

end program test_native_git_dependency_cli
