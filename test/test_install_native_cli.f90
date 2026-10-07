program test_install_native_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, make_directory, join_path, write_text
    use fo_test_harness, only: read_text
    use fo_test_harness, only: file_exists, remove_tree, assert_true
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, prefix
    character(:), allocatable :: installed, sentinel, sentinel_text
    character(:), allocatable :: conflict_prefix, conflict_first, conflict_marker
    type(string_list_t) :: args, environment, run_args
    type(process_result_t) :: result
    character(len=1), parameter :: nl = new_line('a')

    call resolve_driver(driver)
    call make_scratch('fo-install-native', scratch)
    project = join_path(scratch, 'project')
    cache = join_path(scratch, 'cache')
    prefix = join_path(scratch, 'prefix')
    installed = join_path(join_path(prefix, 'bin'), 'install_probe')
    sentinel = join_path(join_path(prefix, 'bin'), 'unrelated.txt')
    sentinel_text = 'leave this prefix file alone'//nl

    call write_text(join_path(project, 'fpm.toml'), 'name = "install_probe"'//nl)
    call write_text(join_path(project, 'app/main.f90'), &
        'program install_probe'//nl//'implicit none'//nl// &
        "print '(a)', 'native-install-output'"//nl// &
        'end program install_probe'//nl)
    call write_text(join_path(project, 'app/second_probe.f90'), &
        'program second_probe'//nl//'implicit none'//nl// &
        "print '(a)', 'second-install-output'"//nl// &
        'end program second_probe'//nl)
    call write_text(join_path(project, 'example/example_probe.f90'), &
        'program example_probe'//nl//'implicit none'//nl// &
        "print '(a)', 'example-output'"//nl// &
        'end program example_probe'//nl)
    call write_text(sentinel, sentinel_text)
    call make_directory(join_path(scratch, 'tmp'))
    call list_add(environment, 'FO_JOBS=1')
    call list_add(environment, 'TMPDIR='//join_path(scratch, 'tmp'))
    call list_add(environment, 'PATH=/usr/bin:/bin')
    call list_add(args, 'install')
    call list_add(args, '--prefix')
    call list_add(args, prefix)
    call run_fo(driver, args, project, cache, result, environment, 120000)
    call assert_equal_integer(result%exit_code, 0, &
        'native explicit-prefix installation succeeds')
    call assert_true(file_exists(installed), 'application is installed in prefix/bin')
    call assert_true(file_exists(join_path(join_path(prefix, 'bin'), 'second_probe')), &
        'all app executables are installed')
    call assert_true(.not. file_exists(join_path(join_path(prefix, 'bin'), &
        'example_probe')), 'example executables are not installed')
    call assert_equal_string(read_text(sentinel), sentinel_text, &
        'unrelated prefix file is preserved')

    call run_external(installed, run_args, project, result)
    call assert_equal_integer(result%exit_code, 0, &
        'installed program runs successfully')
    call assert_equal_string(result%stdout, 'native-install-output'//nl, &
        'installed program has the expected runtime output')

    conflict_prefix = join_path(scratch, 'conflict-prefix')
    conflict_first = join_path(join_path(conflict_prefix, 'bin'), 'install_probe')
    conflict_marker = join_path(join_path(join_path(conflict_prefix, 'bin'), &
        'second_probe'), 'preserve.txt')
    call write_text(conflict_marker, 'directory must survive'//nl)
    args = string_list_t()
    call list_add(args, 'install')
    call list_add(args, '--prefix')
    call list_add(args, conflict_prefix)
    call run_fo(driver, args, project, cache, result, environment, 120000)
    call assert_true(result%exit_code /= 0, &
        'directory at an executable destination rejects installation')
    call assert_true(.not. file_exists(conflict_first), &
        'destination conflict publishes no earlier executable')
    call assert_equal_string(read_text(conflict_marker), 'directory must survive'//nl, &
        'destination conflict preserves existing directory contents')

    call write_text(join_path(project, 'app/second_probe.f90'), &
        'program second_probe'//nl//'implicit none'//nl// &
        'print *, missing_install_symbol'//nl// &
        'end program second_probe'//nl)
    args = string_list_t()
    call list_add(args, 'install')
    call list_add(args, '--prefix')
    call list_add(args, prefix)
    call run_fo(driver, args, project, cache, result, environment, 120000)
    call assert_true(result%exit_code /= 0, 'failed release build fails installation')
    call run_external(installed, string_list_t(), project, result)
    call assert_equal_integer(result%exit_code, 0, &
        'failed install leaves the previous executable usable')
    call assert_equal_string(result%stdout, 'native-install-output'//nl, &
        'failed install does not publish a partial replacement')
    call run_external(join_path(join_path(prefix, 'bin'), 'second_probe'), &
        run_args, project, result)
    call assert_equal_integer(result%exit_code, 0, &
        'failed multi-executable install keeps the prior second executable')
    call assert_equal_string(result%stdout, 'second-install-output'//nl, &
        'failed multi-executable install publishes no partial replacement')
    call assert_equal_string(read_text(sentinel), sentinel_text, &
        'failed install preserves unrelated prefix files')

    call write_text(join_path(project, 'fpm.toml'), 'name = "install_probe"'//nl// &
        '[install]'//nl//'library = true'//nl)
    call run_fo(driver, args, project, cache, result, environment, 120000)
    call assert_true(result%exit_code /= 0, &
        'deferred library installation fails clearly')
    call run_external(installed, run_args, project, result)
    call assert_equal_integer(result%exit_code, 0, &
        'unsupported library install leaves the application intact')
    call assert_equal_string(result%stdout, 'native-install-output'//nl, &
        'unsupported library install does not publish partial output')

    call remove_tree(scratch)
    call finish_assertions()
end program test_install_native_cli
