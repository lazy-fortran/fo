program test_native_git_dependency_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory
    use fo_test_harness, only: make_symlink, move_path, remove_path
    use fo_test_harness, only: write_text, remove_tree, file_exists
    use fo_test_harness, only: assert_process_ok, assert_equal_string, assert_true
    use fo_test_harness, only: assert_contains
    use fo_test_harness, only: assert_file_equals
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_fs, only: fs_stat, fs_identity
    use, intrinsic :: iso_c_binding, only: c_long_long
    implicit none

    character(:), allocatable :: driver, scratch, tools, dependency, consumer
    character(:), allocatable :: offline_dependency
    character(:), allocatable :: path_dependency, nested_consumer
    character(:), allocatable :: git, compiler, assembler, linker, archiver, helper
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
    ! Xcode's as wrapper script needs realpath and dirname to locate clang.
    call find_command('realpath', helper)
    call make_symlink(helper, join_path(tools, 'realpath'))
    call find_command('dirname', helper)
    call make_symlink(helper, join_path(tools, 'dirname'))
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

    call test_only_closure()

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') &
        'native-git-dependency-cli: exact rev and tag build and run without FPM'

contains

    subroutine test_only_closure()
        character(:), allocatable :: root, dev, nested, original_path_manifest
        character(:), allocatable :: binary
        character(len=1), parameter :: nl = new_line('a')
        integer(c_long_long) :: first_time, second_time, first_size, second_size
        integer(c_long_long) :: first_device, second_device, first_inode, second_inode
        logical :: valid

        root = join_path(scratch, 'test_only')
        dev = join_path(scratch, 'root_dev')
        nested = join_path(scratch, 'test_edge')
        original_path_manifest = 'name = "path_dependency"' // nl // &
            '[dependencies]' // nl // 'git_provider = { git = "../provider", '// &
            'rev = "' // commit // '" }' // nl
        call write_text(join_path(dev, 'fpm.toml'), 'name = "root_dev"' // nl)
        call write_text(join_path(dev, 'src/dev.f90'), &
            'module root_dev' // nl // 'implicit none' // nl // &
            'integer, parameter :: dev_value = 7' // nl // &
            'end module root_dev' // nl)
        call write_text(join_path(nested, 'fpm.toml'), &
            'name = "test_edge"' // nl // '[dependencies]' // nl // &
            'root_dev = { path = "../root_dev" }' // nl)
        call write_text(join_path(nested, 'src/edge.f90'), &
            'module test_edge' // nl // 'use root_dev, only: dev_value' // nl // &
            'implicit none' // nl // &
            'integer, parameter :: edge_value = dev_value + 1' // nl // &
            'end module test_edge' // nl)
        call write_text(join_path(root, 'fpm.toml'), &
            'name = "test_only"' // nl // '[build]' // nl // &
            'auto-tests = false' // nl // '[dependencies]' // nl // &
            'path_dependency = { path = "../path_dependency" }' // nl // &
            '[dev-dependencies]' // nl // &
            'root_dev = { path = "../root_dev" }' // nl // &
            '[[test]]' // nl // 'name = "closure_case"' // nl // &
            'source-dir = "checks"' // nl // 'main = "main.f90"' // nl // &
            '[test.dependencies]' // nl // &
            'test_edge = { path = "../test_edge" }' // nl)
        call write_text(join_path(root, 'checks/main.f90'), &
            'program closure_probe' // nl // &
            'use path_provider, only: nested_value' // nl // &
            'use root_dev, only: dev_value' // nl // &
            'use test_edge, only: edge_value' // nl // &
            'implicit none' // nl // 'integer :: unit' // nl // &
            "open(newunit=unit,file='closure.receipt',status='replace')" // nl // &
            "write(unit,'(i0)') nested_value() + dev_value + edge_value" // nl // &
            'close(unit)' // nl // 'end program closure_probe' // nl)
        call expect_test_value(root, '56', 'cold named test-only closure')
        call assert_true(.not. file_exists(join_path(root, 'src')), &
            'test-only consumer has no dummy library source')
        call assert_true(.not. file_exists(join_path(root, 'app')), &
            'test-only consumer has no dummy executable source')
        binary = join_path(root, 'build/fo/bin/closure_case')
        if (file_exists(binary // '.exe')) binary = binary // '.exe'
        call fs_stat(binary, first_time, first_size, valid)
        call assert_true(valid, 'cold test-only command publishes its named executable')
        call fs_identity(binary, first_device, first_inode, valid)
        call expect_test_value(root, '56', 'warm named test-only closure')
        call fs_stat(binary, second_time, second_size, valid)
        call assert_true(valid, 'warm test-only executable remains available')
        call fs_identity(binary, second_device, second_inode, valid)
        call assert_true(first_time == second_time .and. first_size == second_size .and. &
            first_device == second_device .and. first_inode == second_inode, &
            'unchanged valid named test executable is reused without rewriting')

        call write_text(join_path(dev, 'src/dev.f90'), &
            'module root_dev' // nl // 'implicit none' // nl // &
            'integer, parameter :: dev_value = 11' // nl // &
            'end module root_dev' // nl)
        call expect_test_value(root, '64', 'warm root dev dependency edit')
        call write_text(join_path(path_dependency, 'fpm.toml'), &
            'name = "path_dependency"' // nl // '[dependencies]' // nl // &
            'git_provider = { git = "../provider", rev = "' // &
            latest_commit // '" }' // nl)
        call expect_test_value(root, '160', 'warm nested Git pin change')

        call move_path(dev, dev // '.offline')
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, 'closure_case')
        call run_fo(driver, args, root, join_path(scratch, 'cache'), result, &
            environment)
        call assert_true(result%exit_code /= 0, &
            'missing root dev dependency rejects an otherwise warm test')
        call assert_contains(result%stderr, 'root_dev', &
            'missing test dependency names the unavailable package')
        call move_path(dev // '.offline', dev)
        call expect_test_value(root, '160', &
            'restored dependency recovers test-only closure')
        call write_text(join_path(path_dependency, 'fpm.toml'), original_path_manifest)
        call expect_test_value(root, '64', 'restored Git pin recovers earlier behavior')

    end subroutine test_only_closure

    subroutine expect_test_value(root, expected, context)
        character(len=*), intent(in) :: root, expected, context
        character(len=1), parameter :: nl = new_line('a')
        call remove_path(join_path(root, 'closure.receipt'))
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, 'closure_case')
        call run_fo(driver, args, root, join_path(scratch, 'cache'), result, &
            environment)
        call assert_process_ok(result, context)
        call assert_file_equals(join_path(root, 'closure.receipt'), expected // nl, &
            context // ' executes the current source closure')
    end subroutine expect_test_value

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
