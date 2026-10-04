program test_targeted_materialization_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, make_directory, join_path, write_text
    use fo_test_harness, only: remove_path, remove_tree, register_scratch
    use fo_test_harness, only: assert_file_exists, assert_file_absent, assert_file_equals
    use fo_test_harness, only: assert_contains, assert_process_ok, assert_true
    use fo_test_harness, only: file_exists
    use fo_test_harness, only: current_directory
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_test_harness, only: finish_assertions
    implicit none

    character(:), allocatable :: driver, scratch, marker, env_scratch, env_cache, bin_dir
    character(:), allocatable :: home_dir, config_dir, xdg_cache, prefix, state_dir
    character(:), allocatable :: source, dependency, dependency_build, probe_log
    character(:), allocatable :: compiler_path, cwd, arg0
    character(:), allocatable :: path_value, fpm_wrapper_dir, fpm_script, fpm_binary
    character(:), allocatable :: fpm_call_log
    character(len=32), parameter :: names(2) = &
        [character(len=32) :: 'test_mcp_pass', 'test_mcp_fail']
    character(len=32), parameter :: values(2) = &
        [character(len=32) :: 'first target ran', 'second target ran']
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment, probe_environment
    integer :: i, arg0_length, arg_status

    call maybe_log_compiler_probe()
    call resolve_driver(driver)
    call make_scratch('fo-targeted-materialization', scratch)
    marker = scratch // '.marker'
    env_scratch = scratch // '.env'
    call register_scratch(env_scratch)
    home_dir = join_path(env_scratch, 'home')
    config_dir = join_path(env_scratch, 'xdg-config')
    xdg_cache = join_path(env_scratch, 'xdg-cache')
    prefix = join_path(env_scratch, 'prefix')
    env_cache = join_path(env_scratch, 'fo-cache')
    state_dir = join_path(env_scratch, 'gremlin-state')
    fpm_wrapper_dir = join_path(env_scratch, 'bin')
    fpm_script = join_path(fpm_wrapper_dir, 'fpm')
    fpm_call_log = join_path(env_scratch, 'fpm.calls')
    dependency = join_path(scratch, 'test-support/dependency')
    dependency_build = join_path(scratch, 'build/dependencies/probe_dependency')
    probe_log = join_path(env_scratch, 'compiler-probe.log')
    call make_directory(home_dir)
    call make_directory(config_dir)
    call make_directory(xdg_cache)
    call make_directory(prefix)
    call make_directory(env_cache)
    call make_directory(state_dir)
    call make_directory(fpm_wrapper_dir)
    call make_directory(join_path(dependency, 'src'))
    call get_environment_variable('PATH', length=arg0_length, status=arg_status)
    call assert_true(arg_status == 0 .and. arg0_length > 0, &
        'fixture has a process PATH for dependency bootstrap')
    allocate (character(len=arg0_length) :: path_value)
    call get_environment_variable('PATH', path_value)
    call list_add(environment, 'HOME=' // home_dir)
    call list_add(environment, 'XDG_CONFIG_HOME=' // config_dir)
    call list_add(environment, 'XDG_CACHE_HOME=' // xdg_cache)
    call list_add(environment, 'FO_PREFIX=' // prefix)
    call list_add(environment, 'FO_CACHE_DIR=' // env_cache)
    call list_add(environment, 'FO_GREMLIN_STATE_DIR=' // state_dir)
    call list_add(environment, 'FO_JOBS=2')
    call list_add(environment, 'PATH=' // fpm_wrapper_dir // ':' // path_value)
    call list_add(environment, 'FO_TEST_FPM_CALL_LOG=' // fpm_call_log)
    arguments = string_list_t()
    call list_add(arguments, '-c')
    call list_add(arguments, 'command -v fpm')
    call run_external('sh', arguments, scratch, result)
    call assert_process_ok(result, 'locate fpm for the command counter')
    fpm_binary = trim(result%stdout)
    if (len(fpm_binary) > 0) then
        arg0_length = index(fpm_binary, new_line('a'))
        if (arg0_length > 0) fpm_binary = fpm_binary(:arg0_length - 1)
    end if
    call assert_true(len(fpm_binary) > 0, 'resolve the absolute fpm executable')
    call list_add(environment, 'FO_TEST_REAL_FPM=' // fpm_binary)
    call write_text(fpm_script, '#!/bin/sh' // new_line('a') // &
        'printf "%s\\n" "$*" >> "$FO_TEST_FPM_CALL_LOG"' // new_line('a') // &
        'exec "$FO_TEST_REAL_FPM" "$@"' // new_line('a'))
    call run_external('chmod', [character(len=16) :: '+x', fpm_script], scratch, result)
    call assert_process_ok(result, 'make the fpm command counter executable')

    call write_text(join_path(dependency, 'fpm.toml'), &
        'name = "probe_dependency"' // new_line('a'))
    call write_text(join_path(dependency, 'src/probe_dependency.f90'), &
        'module probe_dependency_mod' // new_line('a') // 'implicit none' // &
        new_line('a') // 'contains' // new_line('a') // &
        'integer function dependency_value()' // new_line('a') // &
        'dependency_value = 17' // new_line('a') // &
        'end function dependency_value' // new_line('a') // &
        'end module probe_dependency_mod' // new_line('a'))
    call initialize_dependency_git(dependency)

    call write_text(join_path(scratch, 'fpm.toml'), &
        'name = "targeted_materialization_probe"' // new_line('a') // &
        'version = "0.1.0"' // new_line('a') // 'license = "MIT"' // new_line('a') // &
        'author = "test"' // new_line('a') // 'maintainer = "test"' // new_line('a') // &
        'description = "target materialization probe"' // new_line('a') // &
        '[dependencies]' // new_line('a') // &
        'probe_dependency = { git = "file://' // dependency // '" }' // new_line('a'))
    do i = 1, size(names)
        source = 'program ' // trim(names(i)) // new_line('a') // &
            'use probe_dependency_mod, only: dependency_value' // new_line('a') // &
            'implicit none' // new_line('a') // 'integer :: unit' // new_line('a') // &
            'if (dependency_value() /= 17) stop 1' // new_line('a') // &
            "open(newunit=unit, file='" // marker // "', status='replace')" // &
            new_line('a') // "write(unit, '(a)') '" // trim(values(i)) // "'" // &
            new_line('a') // 'close(unit)' // new_line('a') // &
            'end program ' // trim(names(i)) // new_line('a')
        call write_text(join_path(scratch, 'test/' // trim(names(i)) // '.f90'), source)
    end do

    call current_directory(cwd)
    call get_command_argument(0, length=arg0_length, status=arg_status)
    call assert_true(arg_status == 0 .and. arg0_length > 0, &
        'resolve current test executable for compiler probe')
    if (arg_status /= 0 .or. arg0_length < 1) then
        arg0 = ''
    else
        allocate(character(len=arg0_length) :: arg0)
        call get_command_argument(0, value=arg0, status=arg_status)
    end if
    if (len_trim(arg0) > 0) then
        if (arg0(1:1) == '/') then
            compiler_path = trim(arg0)
        else
            compiler_path = join_path(cwd, trim(arg0))
        end if
    else
        compiler_path = ''
    end if
    call assert_true(file_exists(compiler_path), &
        'current test executable is available as the compiler probe')
    probe_environment = environment
    call list_add(probe_environment, 'FC=' // compiler_path)
    call list_add(probe_environment, 'FO_FC=')
    call list_add(probe_environment, 'FO_TEST_COMPILER_PROBE_LOG=' // probe_log)

    call rejects_unknown_option('--list')
    call rejects_unknown_option('--verbsoe')

    call run_named(names(1), end_options=.true.)
    bin_dir = join_path(scratch, 'build/fo/bin')
    call assert_file_exists(join_path(bin_dir, trim(names(1))), &
        'first selected test is materialized')
    call assert_file_absent(join_path(bin_dir, trim(names(2))), &
        'unselected second test is not materialized')
    call assert_file_equals(marker, trim(values(1)) // new_line('a'), 'first target executes')
    call assert_file_exists(dependency_build, &
        'valid named test materializes its Git dependency')
    call assert_file_equals(fpm_call_log, 'update --fetch-only' // new_line('a'), &
        'test-only Git dependency fetch avoids a redundant root build')

    call run_named(names(2))
    call assert_file_exists(join_path(bin_dir, trim(names(2))), &
        'second selected test is materialized')
    call assert_file_equals(marker, trim(values(2)) // new_line('a'), 'second target executes')
    call assert_file_equals(fpm_call_log, 'update --fetch-only' // new_line('a'), &
        'warm dependency source reuse skips another fpm bootstrap')
    call remove_path(marker)
    call remove_tree(scratch)
    call remove_tree(env_scratch)
    call finish_assertions()
    write(*, '(a)') 'targeted-materialization-cli: sequential selected targets each build and run'

contains

    subroutine maybe_log_compiler_probe()
        character(:), allocatable :: log_path
        integer :: env_length, env_status, unit

        call get_environment_variable('FO_TEST_COMPILER_PROBE_LOG', &
            length=env_length, status=env_status)
        if (env_status /= 0 .or. env_length == 0) return
        allocate(character(len=env_length) :: log_path)
        call get_environment_variable('FO_TEST_COMPILER_PROBE_LOG', value=log_path)
        open (newunit=unit, file=trim(log_path), status='replace')
        write (unit, '(a)') 'compiler invoked'
        close (unit)
        stop 97
    end subroutine maybe_log_compiler_probe

    subroutine initialize_dependency_git(directory)
        character(len=*), intent(in) :: directory

        arguments = string_list_t()
        call list_add(arguments, 'init')
        call list_add(arguments, '-q')
        call run_external('git', arguments, directory, result)
        call assert_process_ok(result, 'initialize dependency Git repository')
        arguments = string_list_t()
        call list_add(arguments, 'add')
        call list_add(arguments, 'fpm.toml')
        call list_add(arguments, 'src/probe_dependency.f90')
        call run_external('git', arguments, directory, result)
        call assert_process_ok(result, 'stage dependency sources')
        arguments = string_list_t()
        call list_add(arguments, '-c')
        call list_add(arguments, 'user.name=fo-test')
        call list_add(arguments, '-c')
        call list_add(arguments, 'user.email=fo-test@example.invalid')
        call list_add(arguments, 'commit')
        call list_add(arguments, '-q')
        call list_add(arguments, '-m')
        call list_add(arguments, 'initial')
        call run_external('git', arguments, directory, result)
        call assert_process_ok(result, 'commit dependency sources')
    end subroutine initialize_dependency_git

    subroutine rejects_unknown_option(option)
        character(len=*), intent(in) :: option

        call remove_path(marker)
        call remove_path(probe_log)
        call remove_path(fpm_call_log)
        call remove_tree(join_path(scratch, 'build'))
        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, option)
        call run_fo(driver, arguments, scratch, env_cache, result, probe_environment)
        call assert_true(result%exit_code /= 0 .and. result%term_signal == 0, &
            'unknown option returns a usage failure: ' // trim(option))
        call assert_contains(result%stderr, 'fo test: unknown option', &
            'unknown option is diagnosed before test execution: ' // trim(option))
        call assert_contains(result%stderr, 'usage: fo test', &
            'unknown option returns test usage: ' // trim(option))
        call assert_file_absent(marker, 'unknown option does not run a fixture test')
        call assert_file_absent(probe_log, 'unknown option does not invoke the compiler')
        call assert_file_absent(fpm_call_log, 'unknown option does not invoke fpm')
        call assert_file_absent(dependency_build, &
            'unknown option does not materialize the Git dependency')
    end subroutine rejects_unknown_option

    subroutine run_named(name, end_options)
        character(len=*), intent(in) :: name
        logical, intent(in), optional :: end_options

        arguments = string_list_t()
        call list_add(arguments, 'test')
        if (present(end_options)) then
            if (end_options) call list_add(arguments, '--')
        end if
        call list_add(arguments, trim(name))
        call run_fo(driver, arguments, scratch, env_cache, result, environment)
        if (result%exit_code /= 0 .or. result%term_signal /= 0 .or. &
            result%runner_failed) then
            write (*, '(a)') 'candidate stdout:'
            write (*, '(a)') result%stdout(:min(len(result%stdout), 2048))
            write (*, '(a)') 'candidate stderr:'
            write (*, '(a)') result%stderr(:min(len(result%stderr), 2048))
        end if
        call assert_process_ok(result, 'targeted ' // trim(name))
        call assert_contains(result%stdout, trim(name), 'named test is reported')
        call assert_contains(result%stdout, 'PASS', 'named test reports a pass status')
    end subroutine run_named

end program test_targeted_materialization_cli
