program test_targeted_materialization_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, make_directory, join_path, write_text
    use fo_test_harness, only: remove_path, remove_tree
    use fo_test_harness, only: assert_file_exists, assert_file_absent, assert_file_equals
    use fo_test_harness, only: assert_contains, assert_process_ok
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(:), allocatable :: driver, scratch, marker, env_scratch, env_cache, bin_dir
    character(:), allocatable :: home_dir, config_dir, xdg_cache, prefix, state_dir
    character(:), allocatable :: source
    character(len=32), parameter :: names(2) = &
        [character(len=32) :: 'test_mcp_pass', 'test_mcp_fail']
    character(len=32), parameter :: values(2) = &
        [character(len=32) :: 'first target ran', 'second target ran']
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    integer :: i

    call resolve_driver(driver)
    call make_scratch('fo-targeted-materialization', scratch)
    marker = scratch // '.marker'
    env_scratch = scratch // '.env'
    home_dir = join_path(env_scratch, 'home')
    config_dir = join_path(env_scratch, 'xdg-config')
    xdg_cache = join_path(env_scratch, 'xdg-cache')
    prefix = join_path(env_scratch, 'prefix')
    env_cache = join_path(env_scratch, 'fo-cache')
    state_dir = join_path(env_scratch, 'gremlin-state')
    call make_directory(home_dir)
    call make_directory(config_dir)
    call make_directory(xdg_cache)
    call make_directory(prefix)
    call make_directory(env_cache)
    call make_directory(state_dir)
    call list_add(environment, 'HOME=' // home_dir)
    call list_add(environment, 'XDG_CONFIG_HOME=' // config_dir)
    call list_add(environment, 'XDG_CACHE_HOME=' // xdg_cache)
    call list_add(environment, 'FO_PREFIX=' // prefix)
    call list_add(environment, 'FO_CACHE_DIR=' // env_cache)
    call list_add(environment, 'FO_GREMLIN_STATE_DIR=' // state_dir)
    call list_add(environment, 'FO_JOBS=2')

    call write_text(join_path(scratch, 'fpm.toml'), &
        'name = "targeted_materialization_probe"' // new_line('a') // &
        'version = "0.1.0"' // new_line('a') // 'license = "MIT"' // new_line('a') // &
        'author = "test"' // new_line('a') // 'maintainer = "test"' // new_line('a') // &
        'description = "target materialization probe"' // new_line('a') // &
        '[build]' // new_line('a') // 'test-dir = "test"' // new_line('a') // &
        'source-dir = "src"' // new_line('a') // 'app-dir = "app"' // new_line('a'))
    do i = 1, size(names)
        source = 'program ' // trim(names(i)) // new_line('a') // 'implicit none' // &
            new_line('a') // 'integer :: unit' // new_line('a') // &
            "open(newunit=unit, file='" // marker // "', status='replace')" // &
            new_line('a') // "write(unit, '(a)') '" // trim(values(i)) // "'" // &
            new_line('a') // 'close(unit)' // new_line('a') // &
            'end program ' // trim(names(i)) // new_line('a')
        call write_text(join_path(scratch, 'test/' // trim(names(i)) // '.f90'), source)
    end do

    call run_named(names(1))
    bin_dir = join_path(scratch, 'build/fo/bin')
    call assert_file_exists(join_path(bin_dir, trim(names(1))), 'first selected test is materialized')
    call assert_file_absent(join_path(bin_dir, trim(names(2))), &
        'unselected second test is not materialized')
    call assert_file_equals(marker, trim(values(1)) // new_line('a'), 'first target executes')

    call run_named(names(2))
    call assert_file_exists(join_path(bin_dir, trim(names(2))), 'second selected test is materialized')
    call assert_file_equals(marker, trim(values(2)) // new_line('a'), 'second target executes')
    write(*, '(a)') 'targeted-materialization-cli: sequential selected targets each build and run'
    call remove_path(marker)
    call remove_tree(scratch)
    call remove_tree(env_scratch)

contains

    subroutine run_named(name)
        character(len=*), intent(in) :: name

        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, trim(name))
        call run_fo(driver, arguments, scratch, env_cache, result, environment)
        call assert_process_ok(result, 'targeted ' // trim(name))
        call assert_contains(result%stdout, trim(name), 'named test is reported')
        call assert_contains(result%stdout, 'PASS', 'named test reports a pass status')
    end subroutine run_named

end program test_targeted_materialization_cli
