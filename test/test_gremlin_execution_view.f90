program test_gremlin_execution_view
    use fo_cache, only: HASH_LEN, cache_file_digest
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree, fs_write_text, fs_realpath, fs_rename
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release, execution_view_copy_app_outputs
    use fo_input_inventory, only: input_declaration_t, input_inventory_t, &
        input_inventory_discover, INPUT_FILE
    use fo_process, only: argv_push, process_getcwd, process_run_argv_logged
    use fo_util, only: delete_tmpfile, make_tmpfile, read_text_file, temporary_root
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    implicit none

    type(input_declaration_t) :: declarations(1)
    type(input_inventory_t) :: inventory, escaping, incomplete, invalid
    type(input_inventory_t) :: multirole, conflicting, aliased
    type(execution_view_t) :: first, second, partial, paired, rejected, alias_view, tampered
    type(execution_view_t) :: build_view, runtime_view
    type(execution_view_t) :: reproduction_build_view, reproduction_runtime_view
    type(string_list_t) :: arguments, environment
    type(process_result_t) :: build_result, run_result
    character(len=512) :: root, views, source_file, text, message
    character(len=512) :: first_fixture, second_output, escape_path, bundle_file
    character(len=512) :: build_source, build_binary, dependency_source
    character(len=HASH_LEN) :: source_digest, after_digest
    character(len=HASH_LEN) :: build_source_digest, build_source_after
    character(len=HASH_LEN) :: dependency_digest, dependency_after
    character(len=HASH_LEN) :: candidate_app_digest, candidate_app_after
    character(len=512) :: argument, executable, current_dir, probe_log
    character(len=512) :: runtime_output, unrelated_test, unrelated_build
    character(len=:), allocatable :: packed, driver
    integer :: n_args, probe_exit, cwd_status
    integer :: ierr, release_status, i
    character(len=4096) :: canonical_tmp, moved_scratch
    logical :: exists, path_ok

    call get_command_argument(1, argument)
    if (trim(argument) == '--execution-view-probe') then
        call runtime_probe()
        stop
    end if

    call make_tmpfile('fo-view-oracle', root)
    call fs_make_dir(trim(root)//'/fixtures')
    call fs_make_dir(trim(root)//'/views')
    call fs_write_text(trim(root)//'/fpm.toml', &
        'name = "execution_view_fixture"'//new_line('a')// &
        'version = "0.1.0"')
    source_file = trim(root)//'/fixtures/input.txt'
    call fs_write_text(trim(source_file), 'frozen fixture')

    declarations(1)%root_alias = 'project'
    declarations(1)%relative_path = 'fixtures/input.txt'
    declarations(1)%role = 'test-fixture'
    declarations(1)%expected_kind = INPUT_FILE
    declarations(1)%writable_at_execution = .true.
    call input_inventory_discover(trim(root), declarations, inventory, ierr, message)
    call check(ierr == 0 .and. inventory%complete, &
        'discover the fixture through the canonical inventory')
    bundle_file = trim(root)//'/bundle/project/fixtures/input.txt'
    call fs_make_dir(trim(root)//'/bundle/project/fixtures')
    call fs_write_text(trim(bundle_file), 'frozen fixture')
    do i = 1, inventory%root_count
        inventory%roots(i)%physical_path = trim(root)//'/bundle'
    end do
    call fs_write_text(trim(source_file), 'edited live fixture')
    call cache_file_digest(trim(source_file), source_digest)

    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-case-1', 'test_case', inventory, .true., .true., first, ierr, message)
    call check(ierr == 0 .and. first%active .and. first%complete, &
        'create a complete private fixture view')
    inquire(file=trim(first%tmpdir), exist=exists)
    call check(exists, 'execution view owns a private temporary directory')
    call fs_realpath(temporary_root(), canonical_tmp, path_ok)
    call check(path_ok, 'caller temporary root has a physical path')
    call check(index(first%tmpdir, trim(canonical_tmp)//'/fo-execution-tmp-') == 1, &
        'scratch belongs to the caller physical temporary root')
    call check(index(first%tmpdir, trim(first%root)//'/') /= 1, &
        'scratch is separate from retained execution evidence')
    call fs_write_text(trim(first%tmpdir)//'/owned.tmp', 'disposable scratch')
    first_fixture = trim(first%cwd)//'/fixtures/input.txt'
    call read_text_file(trim(first_fixture), text)
    call check(index(text, 'frozen fixture') > 0, 'materialize the declared fixture')
    call fs_write_text(trim(first_fixture), 'private mutation')
    call read_text_file(trim(first_fixture), text)
    call check(index(text, 'private mutation') > 0, &
        'writable intent permits a private edit')
    call cache_file_digest(trim(source_file), after_digest)
    call check(source_digest == after_digest, &
        'fixture writes leave source bytes unchanged')

    call get_command_argument(0, executable)
    if (len_trim(executable) > 0 .and. executable(1:1) /= '/') then
        call process_getcwd(current_dir, cwd_status)
        call check(cwd_status == 0, 'resolve the native test executable path')
        executable = trim(current_dir)//'/'//trim(executable)
    end if
    call make_tmpfile('fo-view-probe', probe_log)
    packed = ''
    n_args = 0
    call argv_push(packed, n_args, trim(executable))
    call argv_push(packed, n_args, '--execution-view-probe')
    call process_run_argv_logged(trim(first%cwd), packed, n_args, trim(probe_log), &
        .false., 10, probe_exit)
    call check(probe_exit == 0, &
        'native test reads the declared fixture and writes relative output in its view')
    call delete_tmpfile(trim(probe_log))
    call read_text_file(trim(first%cwd)//'/relative-output.txt', text)
    call check(index(text, 'written relative') > 0, &
        'relative output is created inside the private working directory')

    call fs_remove_file(trim(source_file))
    inquire(file=trim(source_file), exist=exists)
    call check(.not. exists, 'editable fixture can be removed after capture')
    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-case-2', 'test_case', inventory, .true., .true., second, ierr, message)
    call check(second%tmpdir /= first%tmpdir, &
        'concurrent cases own distinct disposable scratch')
    call check(ierr == 0 .and. second%cwd /= first%cwd, &
        'concurrent cases receive distinct working directories')
    call read_text_file(trim(second%cwd)//'/fixtures/input.txt', text)
    call check(index(text, 'frozen fixture') > 0, &
        'later view reads captured bytes after editable source deletion')
    second_output = trim(second%cwd)//'/relative-output.txt'
    call fs_write_text(trim(second_output), 'case two')
    call read_text_file(trim(first%cwd)//'/relative-output.txt', text)
    call check(index(text, 'written relative') > 0, &
        'relative outputs stay isolated between cases')

    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-tampered', 'test_case', inventory, .true., .true., tampered, ierr, message)
    call check(ierr == 0, 'create identity-validation fixture')
    moved_scratch = trim(tampered%tmpdir)//'-original'
    ierr = fs_rename(trim(tampered%tmpdir), trim(moved_scratch))
    call check(ierr == 0, 'move original scratch aside without changing its identity')
    call fs_make_dir(trim(tampered%tmpdir))
    call fs_write_text(trim(tampered%tmpdir)//'/unrelated.txt', 'must survive')
    call execution_view_release(tampered, .false., release_status, message)
    call check(release_status /= 0 .and. tampered%active, &
        'replacement scratch is rejected instead of removing an unrelated directory')
    inquire(file=trim(tampered%tmpdir)//'/unrelated.txt', exist=exists)
    call check(exists, 'failed cleanup preserves unrelated replacement bytes')
    call fs_remove_tree(trim(tampered%tmpdir), ierr)
    call check(ierr == 0, 'fixture removes its own unrelated replacement')
    ierr = fs_rename(trim(moved_scratch), trim(tampered%tmpdir))
    call check(ierr == 0, 'restore original owned scratch')
    call execution_view_release(tampered, .false., release_status, message)
    call check(release_status == 0, 'restored identity permits checked cleanup')

    call execution_view_release(first, .true., release_status, message)
    call check(release_status == 0, 'retained view releases checked scratch')
    inquire(file=trim(first_fixture), exist=exists)
    call check(exists, 'retained failure view keeps its fixture evidence')
    inquire(file=trim(first%tmpdir)//'/owned.tmp', exist=exists)
    call check(.not. exists, 'retained failure view drops temporary scratch')
    call execution_view_release(second, .false., release_status, message)
    call check(release_status == 0, 'ordinary view release succeeds')
    inquire(file=trim(second%tmpdir), exist=exists)
    call check(.not. exists, 'ordinary release removes external scratch directory')
    inquire(file=trim(second_output), exist=exists)
    call check(.not. exists, 'owned inactive view cleanup removes its scratch')

    incomplete = inventory
    incomplete%complete = .false.
    incomplete%diagnostic = 'unmodeled ambient dependency'
    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-partial', 'test_case', incomplete, .true., .false., &
        partial, ierr, message)
    call check(ierr == 0 .and. partial%active .and. .not. partial%complete, &
        'unknown ambient closure still permits a private declared fixture')
    call read_text_file(trim(partial%cwd)//'/fixtures/input.txt', text)
    call check(index(text, 'frozen fixture') > 0, &
        'incomplete inventory still materializes known declared bytes')
    call execution_view_release(partial, .false., release_status, message)

    invalid = inventory
    invalid%valid = .false.
    invalid%diagnostic = 'invalid declared fixture'
    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-invalid', 'test_case', invalid, .true., .false., &
        rejected, ierr, message)
    call check(ierr /= 0 .and. .not. rejected%active, &
        'invalid declaration rejects the execution before case launch')
    call check(index(message, 'invalid declared fixture') > 0, &
        'invalid declaration reports its discovery diagnostic')

    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-unavailable', 'test_case', inventory, .false., .false., &
        rejected, ierr, message)
    call check(ierr /= 0 .and. .not. rejected%active, &
        'unavailable inventory rejects execution without fixture knowledge')

    do i = 1, inventory%entry_count
        if (trim(inventory%entries(i)%role) == 'test-fixture') exit
    end do
    call check(i <= inventory%entry_count, 'fixture entry is present')
    multirole = inventory
    multirole%entry_count = multirole%entry_count + 1
    multirole%entries(multirole%entry_count) = inventory%entries(i)
    multirole%entries(multirole%entry_count)%role = 'runtime-fixture'
    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-multirole', 'test_case', multirole, .true., .true., &
        paired, ierr, message)
    call check(ierr == 0 .and. paired%active, &
        'compatible fixture roles materialize one private physical file')
    call read_text_file(trim(paired%cwd)//'/fixtures/input.txt', text)
    call check(index(text, 'frozen fixture') > 0, &
        'multi-role materialization preserves declared bytes')
    call execution_view_release(paired, .false., release_status, message)

    call check(trim(inventory%roots(1)%aliases(1)) == 'project', &
        'project bundle alias is available for the fixture')
    bundle_file = trim(root)//'/bundle/deps/fixture/nested/data.txt'
    call fs_make_dir(trim(root)//'/bundle/deps/fixture/nested')
    call fs_write_text(trim(bundle_file), 'frozen dependency fixture')
    aliased = inventory
    aliased%roots(1)%alias_count = 2
    aliased%roots(1)%aliases(2) = 'dependency:fixture'
    aliased%roots(1)%bundle_paths(2) = 'deps/fixture'
    call check(aliased%entry_count < aliased%entry_capacity, &
        'inventory has room for a second declared fixture')
    aliased%entry_count = aliased%entry_count + 1
    aliased%entries(aliased%entry_count) = inventory%entries(i)
    aliased%entries(aliased%entry_count)%root_alias = 'dependency:fixture'
    aliased%entries(aliased%entry_count)%relative_path = 'nested/data.txt'
    call cache_file_digest(trim(bundle_file), &
        aliased%entries(aliased%entry_count)%content_digest)
    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-alias', 'test_case', aliased, .true., .true., &
        alias_view, ierr, message)
    call check(ierr == 0 .and. alias_view%active, &
        'dependency alias materializes from its own bundle subtree')
    call read_text_file(trim(alias_view%cwd)// &
        '/.fo-inputs/dependency:fixture/nested/data.txt', text)
    call check(index(text, 'frozen dependency fixture') > 0, &
        'nested dependency fixture retains captured bytes and layout')
    call read_text_file(trim(alias_view%cwd)//'/fixtures/input.txt', text)
    call check(index(text, 'frozen fixture') > 0, &
        'dependency alias does not replace the project fixture')
    call execution_view_release(alias_view, .false., release_status, message)

    conflicting = multirole
    conflicting%entries(conflicting%entry_count)%writable_at_execution = .false.
    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-conflict', 'test_case', conflicting, .true., .true., &
        rejected, ierr, message)
    call check(ierr /= 0 .and. index(message, 'conflicting') > 0, &
        'conflicting fixture intent rejects the private view')

    escaping = inventory
    do i = 1, escaping%entry_count
        if (trim(escaping%entries(i)%role) /= 'test-fixture') cycle
        escaping%entries(i)%relative_path = '../escape.txt'
        exit
    end do
    escape_path = trim(root)//'/escape.txt'
    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-escape', 'test_case', escaping, .true., .true., rejected, ierr, &
        message)
    call check(ierr /= 0 .and. index(message, 'escapes') > 0, &
        'escaping declared paths receive an explicit rejection')
    inquire(file=trim(escape_path), exist=exists)
    call check(.not. exists, 'path rejection does not write outside the view')

    ! Candidate builds get a fresh writable project tree rooted in the frozen
    ! generation bundle; their compiler outputs cannot mutate that bundle.
    call fs_make_dir(trim(root)//'/bundle/project/app')
    call fs_make_dir(trim(root)//'/bundle/deps/provider/src')
    call fs_write_text(trim(root)//'/bundle/project/fpm.toml', &
        'name = "candidate_build_view"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'candidate_provider = { path = "../deps/provider" }'//new_line('a')// &
        '[build]'//new_line('a')//'auto-executables = false'//new_line('a')// &
        '[[executable]]'//new_line('a')//'name = "candidate_build_probe"'// &
        new_line('a')//'source-dir = "app"'//new_line('a')//'main = "main.f90"'// &
        new_line('a'))
    build_source = trim(root)//'/bundle/project/app/main.f90'
    dependency_source = trim(root)//'/bundle/deps/provider/src/provider.f90'
    call fs_write_text(trim(root)//'/bundle/deps/provider/fpm.toml', &
        'name = "candidate_provider"'//new_line('a'))
    call fs_write_text(trim(dependency_source), &
        'module candidate_provider_mod'//new_line('a')// &
        'integer, parameter :: candidate_provider_value = 73'//new_line('a')// &
        'end module candidate_provider_mod'//new_line('a'))
    call fs_write_text(trim(build_source), &
        'program candidate_build_probe'//new_line('a')// &
        'use candidate_provider_mod, only: candidate_provider_value'//new_line('a')// &
        'integer :: unit'//new_line('a')// &
        'print "(i0)", candidate_provider_value'//new_line('a')// &
        'open(newunit=unit,file="app-output.txt",status="replace")'//new_line('a')// &
        'write(unit,"(i0)") candidate_provider_value'//new_line('a')// &
        'close(unit)'//new_line('a')// &
        'end program candidate_build_probe'//new_line('a'))
    call fs_make_dir(trim(root)//'/bundle/project/test')
    call fs_write_text(trim(root)//'/bundle/project/test/test_app_execution_view.f90', &
        'program test_app_execution_view'//new_line('a')// &
        '    integer :: command_status, exit_status, io_status, unit, value'//new_line('a')// &
        '    logical :: exists'//new_line('a')// &
        '    inquire(file="build/fo/app/candidate_build_probe", exist=exists)'// &
        new_line('a')//'    if (.not. exists) error stop 11'//new_line('a')// &
        '    call execute_command_line("build/fo/app/candidate_build_probe", &'// &
        new_line('a')//'        cmdstat=command_status, exitstat=exit_status)'// &
        new_line('a')//'    if (command_status /= 0) error stop 12'//new_line('a')// &
        '    if (exit_status /= 0) error stop 13'//new_line('a')// &
        '    open(newunit=unit,file="app-output.txt",status="old",iostat=io_status)'// &
        new_line('a')//'    if (io_status /= 0) error stop 14'//new_line('a')// &
        '    read(unit,*,iostat=io_status) value'//new_line('a')// &
        '    close(unit)'//new_line('a')//'    if (io_status /= 0) error stop 15'// &
        new_line('a')//'    if (value /= 73) error stop 16'//new_line('a')// &
        'end program test_app_execution_view'//new_line('a'))
    call cache_file_digest(trim(build_source), build_source_digest)
    call cache_file_digest(trim(dependency_source), dependency_digest)
    call execution_view_create(trim(root)//'/views', repeat('b', HASH_LEN), &
        'session-candidate-build', 'build', inventory, .true., .true., &
        build_view, ierr, message, candidate_bundle_root=trim(root)//'/bundle')
    call check(ierr == 0 .and. build_view%active, &
        'candidate build view copies the frozen project')
    inquire(file=trim(build_view%tmpdir), exist=exists)
    call check(exists, 'candidate build view also owns private scratch')
    if (build_view%active) then
        call resolve_driver(driver)
        environment = string_list_t()
        call list_add(environment, 'FO_JOBS=1')
        arguments = string_list_t()
        call list_add(arguments, 'build')
        call run_fo(driver, arguments, trim(build_view%cwd), &
            trim(root)//'/build-view-cache', build_result, environment, 120000)
        call check(build_result%exit_code == 0, &
            'native Fo build succeeds in the candidate-owned view')
        build_binary = trim(build_view%cwd)//'/build/fo/bin/candidate_build_probe'
        inquire(file=trim(build_binary), exist=exists)
        call check(exists, 'native executable is published in the candidate view')
        if (exists) then
            arguments = string_list_t()
            call list_add(arguments, 'exec')
            call list_add(arguments, '--no-build')
            call list_add(arguments, 'candidate_build_probe')
            call run_fo(driver, arguments, trim(build_view%cwd), &
                trim(root)//'/build-view-cache', run_result, environment, 30000)
            call check(run_result%exit_code == 0, &
                'candidate executable exits successfully in the private view')
            if (allocated(run_result%stdout)) then
                call check(index(run_result%stdout, '73') > 0, &
                    'candidate executable resolves the logical dependency root')
            else
                call check(.false., 'candidate executable output is captured')
            end if
        end if
        call fs_remove_file(trim(build_view%cwd)//'/app-output.txt')
        unrelated_test = trim(build_view%cwd)// &
            '/build/fo/bin/unrelated_test_output'
        unrelated_build = trim(build_view%cwd)//'/build/fo/lib/unrelated.a'
        call fs_write_text(trim(unrelated_test), 'test executable')
        call fs_write_text(trim(unrelated_build), 'unrelated build output')
        call execution_view_create(trim(root)//'/views', repeat('c', HASH_LEN), &
            'session-runtime-apps', 'test_case', inventory, .true., .true., &
            runtime_view, ierr, message)
        call check(ierr == 0 .and. runtime_view%active, &
            'create a private test execution view for built applications')
        if (runtime_view%active) then
            call execution_view_copy_app_outputs(trim(build_view%cwd), &
                trim(runtime_view%cwd), trim(build_view%cwd)//'/build/fo/app', &
                trim(build_view%cwd)//'/build/fo/bin', ierr, message)
            call check(ierr == 0, 'copy app outputs into the test execution view')
            runtime_output = trim(runtime_view%cwd)//'/app-output.txt'
            inquire(file=trim(runtime_view%cwd)// &
                '/build/fo/app/candidate_build_probe', exist=exists)
            call check(exists, 'Fo app-only output is present in the execution view')
            inquire(file=trim(runtime_view%cwd)// &
                '/build/fo/bin/candidate_build_probe', exist=exists)
            call check(exists, 'legacy Fo app path is present in the execution view')
            inquire(file=trim(runtime_view%cwd)// &
                '/build/fo/bin/unrelated_test_output', exist=exists)
            call check(.not. exists, 'test binaries are not copied from the build view')
            inquire(file=trim(runtime_view%cwd)//'/build/fo/lib/unrelated.a', &
                exist=exists)
            call check(.not. exists, 'unrelated build outputs are not copied')
            packed = ''
            n_args = 0
            call argv_push(packed, n_args, trim(runtime_view%cwd)// &
                '/build/fo/app/candidate_build_probe')
            call make_tmpfile('fo-view-app-run', probe_log)
            call process_run_argv_logged(trim(runtime_view%cwd), packed, n_args, &
                trim(probe_log), .false., 10, probe_exit)
            call check(probe_exit == 0, &
                'built app runs from the private execution view')
            call read_text_file(trim(runtime_output), text)
            call check(index(text, '73') > 0, &
                'app writes its relative output in the view')
            inquire(file=trim(build_view%cwd)//'/app-output.txt', exist=exists)
            call check(.not. exists, 'app execution leaves the build view unchanged')
            inquire(file=trim(root)//'/bundle/project/app-output.txt', exist=exists)
            call check(.not. exists, 'app execution leaves the frozen bundle unchanged')
            call delete_tmpfile(trim(probe_log))

            call fs_remove_file(trim(runtime_output))
            environment = string_list_t()
            call list_add(environment, 'FO_JOBS=1')
            call list_add(environment, 'FO_GREMLIN_EXECUTION_CWD='// &
                trim(runtime_view%cwd))
            arguments = string_list_t()
            call list_add(arguments, 'test')
            call list_add(arguments, 'test_app_execution_view')
            call run_fo(driver, arguments, trim(build_view%cwd), &
                trim(root)//'/build-view-cache', run_result, environment, 120000)
            call check(run_result%exit_code == 0, &
                'campaign-style test runs its app from the execution view')
            inquire(file=trim(runtime_output), exist=exists)
            call check(exists, 'campaign-style app writes only in the execution view')
            inquire(file=trim(build_view%cwd)//'/app-output.txt', exist=exists)
            call check(.not. exists, 'campaign-style app does not write in build view')

            call execution_view_create(trim(root)//'/views', repeat('d', HASH_LEN), &
                'session-reproduction-build', 'build', inventory, .true., .true., &
                reproduction_build_view, ierr, message, &
                candidate_bundle_root=trim(root)//'/bundle')
            call check(ierr == 0 .and. reproduction_build_view%active, &
                'create a fresh reproduction build view')
            call execution_view_create(trim(root)//'/views', repeat('e', HASH_LEN), &
                'session-reproduction-runtime', 'test_case', inventory, .true., .true., &
                reproduction_runtime_view, ierr, message)
            call check(ierr == 0 .and. reproduction_runtime_view%active, &
                'create a fresh reproduction execution view')
            call check(reproduction_build_view%root /= reproduction_runtime_view%root, &
                'back-to-back build and runtime views publish to distinct directories')
            if (reproduction_build_view%active .and. &
                    reproduction_runtime_view%active) then
                environment = string_list_t()
                call list_add(environment, 'FO_JOBS=1')
                call list_add(environment, 'FO_GREMLIN_EXECUTION_CWD='// &
                    trim(reproduction_runtime_view%cwd))
                arguments = string_list_t()
                call list_add(arguments, 'test')
                call list_add(arguments, 'test_app_execution_view')
                call run_fo(driver, arguments, trim(reproduction_build_view%cwd), &
                    trim(root)//'/build-view-cache', run_result, environment, 120000)
                if (run_result%exit_code /= 0) then
                    if (allocated(run_result%stdout)) print '(a)', run_result%stdout
                    if (allocated(run_result%stderr)) print '(a)', run_result%stderr
                end if
                call check(run_result%exit_code == 0, &
                    'reproduction-style test builds and runs its app in the view')
                inquire(file=trim(reproduction_runtime_view%cwd)//'/app-output.txt', &
                    exist=exists)
                call check(exists, &
                    'reproduction-style app writes inside its private execution view')
                inquire(file=trim(reproduction_build_view%cwd)//'/app-output.txt', &
                    exist=exists)
                call check(.not. exists, &
                    'reproduction-style app leaves its build view unchanged')
            end if
            call execution_view_release(reproduction_runtime_view, .false., &
                release_status, message)
            call execution_view_release(reproduction_build_view, .false., &
                release_status, message)
            call cache_file_digest(trim(build_view%cwd)// &
                '/build/fo/app/candidate_build_probe', candidate_app_digest)
            call fs_write_text(trim(runtime_view%cwd)// &
                '/build/fo/app/candidate_build_probe', 'view-local replacement')
            call cache_file_digest(trim(build_view%cwd)// &
                '/build/fo/app/candidate_build_probe', candidate_app_after)
            call check(candidate_app_after == candidate_app_digest, &
                'changing the view-local app leaves the candidate artifact unchanged')
            call execution_view_release(runtime_view, .false., release_status, message)
        end if
        call cache_file_digest(trim(build_source), build_source_after)
        call check(build_source_digest == build_source_after, &
            'candidate build leaves frozen source bytes unchanged')
        call cache_file_digest(trim(dependency_source), dependency_after)
        call check(dependency_digest == dependency_after, &
            'candidate build leaves its dependency bundle unchanged')
        inquire(file=trim(root)//'/bundle/project/build/fo/bin/'// &
            'candidate_build_probe', exist=exists)
        call check(.not. exists, &
            'candidate build does not write outputs into its bundle')
        call execution_view_release(build_view, .false., release_status, message)
        call check(release_status == 0, 'candidate build view releases its owned tree')
    end if
    call fs_remove_tree(trim(root))
    print '(a)', 'Gremlin writable execution view behavioral oracle: PASS'

contains

    subroutine runtime_probe()
        character(len=256) :: line
        integer :: unit, status

        open (newunit=unit, file='fixtures/input.txt', status='old', &
            action='read', iostat=status)
        if (status /= 0) error stop 2
        read (unit, '(a)', iostat=status) line
        close (unit)
        if (status /= 0) error stop 2
        if (trim(line) /= 'private mutation') error stop 2
        open (newunit=unit, file='relative-output.txt', status='new', &
            action='write', iostat=status)
        if (status /= 0) error stop 2
        write (unit, '(a)', iostat=status) 'written relative'
        close (unit)
        if (status /= 0) error stop 2
    end subroutine runtime_probe

    subroutine check(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) return
        print '(a)', 'FAIL: '//description
        error stop 1
    end subroutine check
end program test_gremlin_execution_view
