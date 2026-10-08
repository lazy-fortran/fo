program test_install_help
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fx_hash, only: sha256_file
    use fo_fs, only: fs_identity, fs_stat, fs_tree_fingerprint, fs_remove_tree, fs_is_windows, fs_find_executable
    use fo_test_harness, only: string_list_t, process_result_t, list_add, run_process
    use fo_test_harness, only: make_scratch, join_path, write_text, make_directory
    use fo_test_harness, only: read_text, current_directory
    use fo_test_harness, only: file_exists, environment_value, assert_true
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    implicit none

    interface
        integer(c_int) function prepare_executable(path) bind(C, name='fo_test_prepare_executable')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function prepare_executable
        integer(c_int) function executable_permissions(path) bind(C, name='fo_test_executable_permissions')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function executable_permissions
        integer(c_int) function snapshot_permissions(path, buffer, capacity) &
                bind(C, name='fo_test_permissions_snapshot')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: capacity
        end function snapshot_permissions
    end interface

    character(:), allocatable :: run_oracle, driver, project, scratch, home
    character(:), allocatable :: prefix_override
    character(:), allocatable :: cache, prefix
    character(:), allocatable :: sentinel, installed, original_permissions, value, cwd, compiler, c_compiler
    character(:), allocatable :: shim, shim_dir, compiler_source, marker, separator
    character(len=4096) :: compiler_buffer, c_compiler_buffer
    character(len=64) :: original_hash, installed_hash
    type(string_list_t) :: args, env
    type(process_result_t) :: result
    integer(c_long_long) :: before_sum, before_mixed, before_count
    integer(c_long_long) :: after_sum, after_mixed, after_count
    integer(c_long_long) :: prefix_sum, prefix_mixed, prefix_count
    integer(c_long_long) :: home_sum, home_mixed, home_count
    integer(c_long_long) :: old_mtime, old_size, new_mtime, new_size
    integer(c_long_long) :: old_device, old_inode
    integer :: hash_status, chmod_status
    logical :: ok, before_ok, after_ok

    run_oracle = environment_value('FO_RUN_INSTALL_HELP_ORACLE')
    if (run_oracle /= '1') then
        write (output_unit, '(a)') &
            'SKIP: set FO_RUN_INSTALL_HELP_ORACLE=1 for the standalone CLI oracle'
        return
    end if

    driver = environment_value('FO_BIN')
    if (len_trim(driver) == 0) then
        write (output_unit, '(a)') 'FAIL: FO_BIN must name the worktree-local driver'
        error stop 1
    end if
    call assert_true(file_exists(driver), 'worktree-local fo driver exists')
    call make_scratch('fo-install-help', scratch)
    project = join_path(scratch, 'project')
    home = join_path(scratch, 'home')
    cache = join_path(scratch, 'cache')
    prefix_override = environment_value('FO_INSTALL_HELP_PREFIX')
    prefix = join_path(scratch, 'prefix')
    if (len_trim(prefix_override) > 0) prefix = prefix_override
    sentinel = join_path(join_path(prefix, 'bin'), 'installation_probe')
    installed = sentinel

    call make_directory(project)
    call make_directory(home)
    call make_directory(cache)
    call fs_remove_tree(prefix)
    call make_directory(join_path(prefix, 'bin'))
    call make_directory(join_path(scratch, 'tmp'))
    call make_directory(join_path(project, 'app'))
    call write_text(join_path(project, 'fpm.toml'), &
        'name = "install_probe"'//new_line('a')// &
        '[[executable]]'//new_line('a')//'name = "installation_probe"'//new_line('a')// &
        'source-dir = "app"'//new_line('a')//'main = "main.f90"'//new_line('a'))
    call write_text(join_path(project, 'app/main.f90'), &
        'program installation_probe'//new_line('a')// &
        'implicit none'//new_line('a')//'character(256) :: arg, value'//new_line('a')// &
        'integer :: status'//new_line('a')// &
        'call get_command_argument(1,arg)'//new_line('a')// &
        'if(trim(arg)/="--verify-install") error stop 41'//new_line('a')// &
        'call get_command_argument(2,arg)'//new_line('a')// &
        'if(trim(arg)/="literal ; [installed]") error stop 42'//new_line('a')// &
        'call get_environment_variable("FO_INSTALL_HELP_VALUE",value,status=status)'//new_line('a')// &
        'if(status/=0.or.trim(value)/="native;environment;value") error stop 43'//new_line('a')// &
        'print "(a)", "installed native application: 16903"'//new_line('a')// &
        'end program installation_probe'//new_line('a'))
    call write_text(sentinel, 'sentinel executable: leave these bytes intact'//new_line('a'))
    chmod_status = prepare_executable(sentinel//c_null_char)
    call assert_equal_integer(chmod_status, 0, 'sentinel is marked executable')

    call current_directory(cwd)
    compiler_source = join_path(cwd, 'test-fixtures/c/compiler_probe.c')
    shim_dir = join_path(scratch, 'compiler-bin')
    shim = join_path(shim_dir, 'gfortran')
    marker = join_path(scratch, 'compiler-invocations')
    separator = ':'
    call make_directory(shim_dir)
    call fs_find_executable('gfortran', compiler_buffer, ok)
    compiler = trim(compiler_buffer)
    call assert_true(ok, 'locates real native Fortran compiler')
    call fs_find_executable('cc', c_compiler_buffer, ok)
    if (fs_is_windows()) then
        shim = shim//'.exe'
        separator = ';'
        call fs_find_executable('gcc', c_compiler_buffer, ok)
    end if
    c_compiler = trim(c_compiler_buffer)
    call assert_true(ok, 'locates native C compiler for independent compiler observer')
    call list_add(args, c_compiler)
    if (fs_is_windows()) call list_add(args, '-municode')
    call list_add(args, compiler_source)
    call list_add(args, '-o')
    call list_add(args, shim)
    call run_process(args, project, result, timeout_ms=30000)
    call assert_equal_integer(result%exit_code, 0, 'builds the independent real-compiler observer')
    call add_command_env(env, scratch, home, cache)
    call list_add(env, 'PATH='//shim_dir//separator//environment_value('PATH'))
    call list_add(env, 'FO_FC='//shim)
    call list_add(env, 'FO_TEST_REAL_COMPILER='//compiler)
    call list_add(env, 'FO_TEST_COMPILER_MARKER='//marker)
    call sha256_file(sentinel, original_hash, hash_status)
    call assert_equal_integer(hash_status, 0, 'sentinel hash is available')
    call fs_stat(sentinel, old_mtime, old_size, ok)
    call fs_identity(sentinel, old_device, old_inode, ok)
    call assert_true(ok, 'sentinel metadata is available')
    call assert_true(executable_permissions(sentinel//c_null_char) == 1, &
        'sentinel has real owner read/write/execute and public read/execute permissions')
    original_permissions = permissions_of(sentinel)
    call fingerprint(project, before_sum, before_mixed, before_count, before_ok)
    call assert_true(before_ok, 'project tree can be fingerprinted')
    call fingerprint(prefix, prefix_sum, prefix_mixed, prefix_count, before_ok)
    call assert_true(before_ok, 'isolated installation tree can be fingerprinted')
    call fingerprint(home, home_sum, home_mixed, home_count, before_ok)
    call assert_true(before_ok .and. home_count == 0, 'isolated HOME starts empty')

    call run_cli([character(len=32) :: '--help'], result)
    call assert_equal_integer(result%exit_code, 0, 'global fo --help exits zero')
    call assert_true(index(result%stdout, 'fo - Fortran build driver') > 0, &
        'global help prints command overview')
    call assert_no_side_effects('global fo --help')
    call run_cli([character(len=32) :: 'clean', '--help'], result)
    call assert_equal_integer(result%exit_code, 0, 'fo clean --help exits zero')
    call assert_true(index(result%stdout, 'usage: fo clean') > 0, &
        'command-local help prints clean usage')
    call assert_no_side_effects('fo clean --help')

    call run_cli([character(len=32) :: 'help', 'install'], result)
    call assert_equal_integer(result%exit_code, 0, 'fo help install exits zero')
    call assert_contains_help(result, 'usage: fo install [--prefix PATH]')
    call assert_no_side_effects('fo help install')
    block
        character(:), allocatable :: expected
        expected = result%stdout
        call run_cli([character(len=32) :: 'install', '--help'], result)
        call assert_equal_integer(result%exit_code, 0, 'fo install --help exits zero')
        call assert_equal_string(result%stdout, expected, &
            'fo install --help matches fo help install byte for byte')
        call assert_no_side_effects('fo install --help')
        call run_cli([character(len=32) :: 'install', '-h'], result)
        call assert_equal_integer(result%exit_code, 0, 'fo install -h exits zero')
        call assert_equal_string(result%stdout, expected, &
            'fo install -h matches fo help install byte for byte')
        call assert_no_side_effects('fo install -h')
    end block

    call run_cli([character(len=40) :: 'install', '--not-an-install-option'], result)
    call assert_true(result%exit_code /= 0, 'invalid install option exits nonzero')
    call assert_true(index(result%stderr, 'unknown option') > 0, &
        'invalid install option explains the error')
    call assert_no_side_effects('invalid fo install option')
    call run_cli([character(len=32) :: 'install', '--prefix'], result)
    call assert_true(result%exit_code /= 0, 'missing install prefix exits nonzero')
    call assert_no_side_effects('missing fo install --prefix value')
    call run_cli([character(len=512) :: 'install', '--prefix', prefix], result)
    if (result%exit_code /= 0) then
        write (output_unit, '(a)') 'install exit='//integer_text(result%exit_code)// &
            ' stderr='//result%stderr
        write (output_unit, '(a)') 'scratch='//scratch//new_line('a')//result%stdout
        block
            integer :: log_start, log_end
            character(:), allocatable :: log_path
            log_start = index(result%stderr, 'fo: full log: ')
            if (log_start > 0) then
                log_start = log_start + len('fo: full log: ')
                log_end = index(result%stderr(log_start:), new_line('a'))
                if (log_end == 0) log_end = len(result%stderr) - log_start + 2
                log_path = result%stderr(log_start:log_start + log_end - 2)
                if (file_exists(log_path)) write(output_unit, '(a)') &
                    'install subprocess log: '//read_text(log_path)
            end if
        end block
    end if
    call assert_equal_integer(result%exit_code, 0, 'explicit isolated-prefix install succeeds')
    call assert_true(file_exists(marker), 'explicit native install invokes the observed real compiler')
    call sha256_file(installed, installed_hash, hash_status)
    call assert_equal_integer(hash_status, 0, 'installed application hash is available')
    call assert_true(installed_hash /= original_hash, &
        'native install replaces the original sentinel bytes')
    call fs_stat(installed, new_mtime, new_size, ok)
    call assert_true(ok .and. new_size > 1000_c_long_long, &
        'installed path contains a compiled application')
    call assert_true(executable_permissions(installed//c_null_char) == 1, &
        'installed application has real executable permissions')
    args = string_list_t()
    call list_add(args, installed)
    call list_add(args, '--verify-install')
    call list_add(args, 'literal ; [installed]')
    call list_add(env, 'FO_INSTALL_HELP_VALUE=native;environment;value')
    call run_process(args, home, result, env, timeout_ms=30000)
    call assert_equal_integer(result%exit_code, 0, 'installed application really executes outside its project')
    call assert_equal_string(result%stdout, 'installed native application: 16903'//new_line('a'), &
        'installed application runs authored code with exact argv and environment values')
    call fingerprint(project, after_sum, after_mixed, after_count, after_ok)
    call assert_true(after_ok .and. after_count == before_count .and. &
        after_sum == before_sum .and. after_mixed == before_mixed, &
        'explicit install does not alter project source files')

    call finish_assertions()

contains

    subroutine add_command_env(environment, root, user_home, cache_root)
        type(string_list_t), intent(inout) :: environment
        character(len=*), intent(in) :: root, user_home, cache_root

        call list_add(environment, 'HOME='//user_home)
        call list_add(environment, 'FO_CACHE_DIR='//cache_root)
        call list_add(environment, 'FO_DISABLE_SELF_REFRESH=1')
        call list_add(environment, 'FO_SELF_REFRESH=0')
        call list_add(environment, 'TMPDIR='//root//'/tmp')
        call list_add(environment, 'USERPROFILE='//user_home)
        call list_add(environment, 'APPDATA='//user_home)
        call list_add(environment, 'LOCALAPPDATA='//user_home)
    end subroutine add_command_env

    subroutine run_cli(arguments, child)
        character(len=*), intent(in) :: arguments(:)
        type(process_result_t), intent(out) :: child
        type(string_list_t) :: command
        integer :: j

        call list_add(command, driver)
        do j = 1, size(arguments)
            call list_add(command, trim(arguments(j)))
        end do
        call run_process(command, project, child, env, timeout_ms=30000)
    end subroutine run_cli

    subroutine assert_no_side_effects(context)
        character(len=*), intent(in) :: context
        integer(c_long_long) :: sum_value, mixed_value, count_value
        integer(c_long_long) :: mtime, size, device, inode
        character(len=64) :: hash
        integer :: ierr
        logical :: tree_ok, stat_ok

        call assert_true(.not. file_exists(marker), trim(context)//': no compiler subprocess launched')

        call assert_true(.not. file_exists(join_path(project, 'build')) .and. &
            .not. file_exists(join_path(project, '.fo')), &
            trim(context)//': no project build or lock tree')
        call assert_true(.not. file_exists(join_path(home, '.local')), &
            trim(context)//': isolated global install tree is untouched')
        call assert_true(.not. file_exists(join_path(home, '.cache')) .and. &
            .not. file_exists(join_path(home, '.config')) .and. &
            .not. file_exists(join_path(home, '.fo')), &
            trim(context)//': isolated HOME has no tool/cache writes')
        call fingerprint(home, sum_value, mixed_value, count_value, tree_ok)
        call assert_true(tree_ok .and. count_value == home_count .and. &
            sum_value == home_sum .and. mixed_value == home_mixed, &
            trim(context)//': isolated HOME tree is unchanged')
        call fingerprint(project, sum_value, mixed_value, count_value, tree_ok)
        call assert_true(tree_ok .and. count_value == before_count .and. &
            sum_value == before_sum .and. mixed_value == before_mixed, &
            trim(context)//': project tree is unchanged')
        call fingerprint(cache, sum_value, mixed_value, count_value, tree_ok)
        call assert_true(tree_ok .and. count_value == 0, &
            trim(context)//': isolated cache has no writes')
        call fs_stat(sentinel, mtime, size, stat_ok)
        call fs_identity(sentinel, device, inode, stat_ok)
        call assert_true(stat_ok .and. mtime == old_mtime .and. size == old_size .and. &
            device == old_device .and. inode == old_inode, &
            trim(context)//': sentinel metadata is byte-for-byte stable')
        call sha256_file(sentinel, hash, ierr)
        call assert_true(ierr == 0 .and. hash == original_hash, &
            trim(context)//': sentinel hash is unchanged')
        value = permissions_of(sentinel)
        call assert_equal_string(value, original_permissions, &
            trim(context)//': real owner and permissions are unchanged')
        call fingerprint(prefix, sum_value, mixed_value, count_value, tree_ok)
        call assert_true(tree_ok .and. count_value == prefix_count .and. &
            sum_value == prefix_sum .and. mixed_value == prefix_mixed, &
            trim(context)//': isolated install tree is unchanged')
    end subroutine assert_no_side_effects

    function permissions_of(path) result(snapshot)
        character(len=*), intent(in) :: path
        character(:), allocatable :: snapshot
        character(kind=c_char, len=32768) :: buffer
        integer :: status, terminator
        buffer = repeat(c_null_char, len(buffer))
        status = snapshot_permissions(path//c_null_char, buffer, int(len(buffer), c_int))
        call assert_equal_integer(status, 0, 'independent owner and permission descriptor is readable')
        terminator = index(buffer, c_null_char)
        snapshot = ''
        if (status /= 0 .or. terminator < 1) return
        snapshot = buffer(:terminator - 1)
    end function permissions_of

    subroutine assert_contains_help(child, fragment)
        type(process_result_t), intent(in) :: child
        character(len=*), intent(in) :: fragment
        call assert_true(index(child%stdout, fragment) > 0, &
            'install help includes its usage and options')
    end subroutine assert_contains_help

    subroutine fingerprint(path, sum_value, mixed_value, count_value, valid)
        character(len=*), intent(in) :: path
        integer(c_long_long), intent(out) :: sum_value, mixed_value, count_value
        logical, intent(out) :: valid
        call fs_tree_fingerprint(path, .false., sum_value, mixed_value, count_value, valid)
    end subroutine fingerprint

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
    end function integer_text

end program test_install_help
