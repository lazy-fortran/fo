program test_install_native_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, make_directory, join_path, write_text
    use fo_test_harness, only: read_text
    use fo_test_harness, only: current_directory, assert_process_ok, remove_path
    use fo_test_harness, only: file_exists, remove_tree, assert_true
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions, assert_contains
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_fs, only: fs_realpath, fs_find_executable
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, prefix
    character(:), allocatable :: installed, sentinel, sentinel_text
    character(:), allocatable :: conflict_prefix, conflict_first, conflict_marker
    type(string_list_t) :: args, environment, run_args
    type(process_result_t) :: result
    character(len=1), parameter :: nl = new_line('a')
    character(len=4096) :: physical_scratch
    logical :: scratch_resolved

    call resolve_driver(driver)
    call make_scratch('fo-install-native', scratch)
    call fs_realpath(scratch, physical_scratch, scratch_resolved)
    call assert_true(scratch_resolved, 'install fixture has a physical source root')
    if (.not. scratch_resolved) error stop 'install fixture root is unavailable'
    scratch = trim(physical_scratch)
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
    ! Keep the system tools minimal but include the compiler's own directory,
    ! which is not /usr/bin on every host (Homebrew on macOS).
    call list_add(environment, 'PATH='//compiler_directory()//':/usr/bin:/bin')
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

    ! A release compile failure reports the compiler diagnostic, its source
    ! location and the retained log, like fo build.
    call write_text(join_path(project, 'fpm.toml'), 'name = "install_probe"'//nl)
    call write_text(join_path(project, 'app/main.f90'), &
        'program install_probe'//nl//'this is not fortran'//nl// &
        'end program install_probe'//nl)
    call run_fo(driver, args, project, cache, result, environment, 120000)
    call assert_true(result%exit_code /= 0, 'failing release build fails install')
    call assert_contains(result%stderr, 'fo install: release build failed: ', &
        'install failure names the release build')
    call assert_contains(result%stderr, 'fo: at: '//join_path(project, 'app')//'/', &
        'install failure names the failing source location')
    call assert_contains(result%stderr, 'fo: full log: ', &
        'install failure points at the retained build log')

    call check_atomic_reinstall()
    call remove_tree(scratch)
    call finish_assertions()
contains

    subroutine check_atomic_reinstall()
        character(:), allocatable :: root, app, store, destination, first, second
        character(:), allocatable :: shim, marker, cwd, preload, mode
        type(string_list_t) :: command, env, install_args, no_args
        type(process_result_t) :: observed
        logical :: darwin

        call run_external('/usr/bin/uname', no_args, scratch, observed)
        call assert_process_ok(observed, 'identify install publication platform')
        darwin = observed%stdout == 'Darwin'//nl
        if (.not. darwin .and. observed%stdout /= 'Linux'//nl) then
            write (*, '(a)') &
                'SKIP: install rename interposition requires Darwin or Linux'
            return
        end if
        root = join_path(scratch, 'atomic')
        app = join_path(root, 'project')
        store = join_path(root, 'cache')
        destination = join_path(root, 'prefix')
        first = join_path(destination, 'bin/first')
        second = join_path(destination, 'bin/second')
        shim = join_path(root, 'observer.so')
        marker = join_path(root, 'publication.marker')
        call make_directory(root)
        call current_directory(cwd)
        if (darwin) then
            call list_add(command, '-dynamiclib')
            preload = 'DYLD_INSERT_LIBRARIES='
        else
            call list_add(command, '-shared')
            call list_add(command, '-fPIC')
            preload = 'LD_PRELOAD='
        end if
        call list_add(command, '-Wall')
        call list_add(command, '-Werror')
        call list_add(command, &
            join_path(cwd, 'test-fixtures/c/install_publication_observer.c'))
        if (.not. darwin) call list_add(command, '-ldl')
        call list_add(command, '-o')
        call list_add(command, shim)
        call run_external('/usr/bin/cc', command, root, observed)
        call assert_process_ok(observed, 'compile install publication observer')
        if (observed%exit_code /= 0) return

        call write_text(join_path(app, 'fpm.toml'), 'name = "atomic_install"'//nl)
        call write_atomic_apps(app, 'old')
        call list_add(install_args, 'install')
        call list_add(install_args, '--prefix')
        call list_add(install_args, destination)
        call run_fo(driver, install_args, app, store, observed, environment, 120000)
        call assert_process_ok(observed, 'initial atomic fixture install succeeds')
        env = environment
        call list_add(env, preload//shim)
        call list_add(env, 'FO_TEST_INSTALL_TARGET='//first)
        call list_add(env, 'FO_TEST_INSTALL_MARKER='//marker)
        call write_atomic_apps(app, 'new')
        call run_fo(driver, install_args, app, store, observed, env, 120000)
        call assert_process_ok(observed, 'atomic reinstall succeeds')
        call assert_equal_string(read_text(marker), 'present'//nl, &
            'installed pathname exists at the incoming publication boundary')
        call assert_atomic_outputs(first, second, app, 'new')

        command = string_list_t()
        call list_add(command, '700')
        call list_add(command, first)
        call run_external('/bin/chmod', command, root, observed)
        call assert_process_ok(observed, 'restrict old executable permissions')
        call list_add(env, 'FO_TEST_INSTALL_FAILURE_TARGET='//second)
        call remove_path(marker)
        call write_atomic_apps(app, 'failed')
        call run_fo(driver, install_args, app, store, observed, env, 120000)
        call assert_true(observed%exit_code /= 0, &
            'injected publication failure fails install')
        call assert_contains(observed%stderr, 'could not publish executable second', &
            'second publication fails after the first executable was replaced')
        call assert_equal_string(read_text(marker), 'present'//nl, &
            'first executable reaches publication before the injected failure')
        call assert_atomic_outputs(first, second, app, 'new')
        command = string_list_t()
        if (darwin) then
            call list_add(command, '-f')
            call list_add(command, '%Lp')
        else
            call list_add(command, '-c')
            call list_add(command, '%a')
        end if
        call list_add(command, first)
        call run_external('/usr/bin/stat', command, root, observed)
        call assert_process_ok(observed, 'read restored executable permissions')
        mode = observed%stdout
        call assert_equal_string(mode, '700'//nl, &
            'rollback preserves the old executable permissions')
    end subroutine check_atomic_reinstall

    subroutine write_atomic_apps(app, version)
        character(len=*), intent(in) :: app, version

        call write_text(join_path(app, 'app/first.f90'), &
            'program first'//nl//'print "(a)", "first '//version//'"'//nl// &
            'end program first'//nl)
        call write_text(join_path(app, 'app/second.f90'), &
            'program second'//nl//'print "(a)", "second '//version//'"'//nl// &
            'end program second'//nl)
    end subroutine write_atomic_apps

    subroutine assert_atomic_outputs(first, second, app, version)
        character(len=*), intent(in) :: first, second, app, version
        type(process_result_t) :: ran

        call run_external(first, string_list_t(), app, ran)
        call assert_process_ok(ran, 'first installed executable runs')
        call assert_equal_string(ran%stdout, 'first '//version//nl, &
            'first installed executable retains the expected output')
        call run_external(second, string_list_t(), app, ran)
        call assert_process_ok(ran, 'second installed executable runs')
        call assert_equal_string(ran%stdout, 'second '//version//nl, &
            'second installed executable retains the expected output')
    end subroutine assert_atomic_outputs

    function compiler_directory() result(directory)
        character(:), allocatable :: directory, path
        character(len=4096) :: found_path
        integer :: slash
        logical :: located

        directory = '/usr/bin'
        call fs_find_executable('gfortran', found_path, located)
        if (.not. located) return
        path = trim(found_path)
        slash = index(path, '/', back=.true.)
        if (slash > 1) directory = path(:slash - 1)
    end function compiler_directory

end program test_install_native_cli
