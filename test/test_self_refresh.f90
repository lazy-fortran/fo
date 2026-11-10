program test_self_refresh
    !! Standalone behavior oracle: after `fo build`, run
    !! `FO_RUN_SELF_REFRESH_ORACLE=1 FO_BIN=<worktree>/build/fo/app/fo fo test
    !! test_self_refresh`. Child commands use a temporary HOME and cache.
    use fo_util, only: temporary_root, wall_time_seconds
    use fx_hash, only: sha256_file
    use fo_test_harness, only: set_mode
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, &
        c_null_char
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, dp => real64
    use fo_fs, only: fs_copy_exec, fs_identity, fs_make_dir, fs_remove_tree, &
        fs_stat, fs_write_text, fs_collect_files, fs_canonical_path
    use fo_process, only: argv_push, process_getcwd, process_getpid, &
        process_run_argv_logged
    implicit none

    integer :: n_pass, n_fail

    interface
        function c_setenv(name, value, overwrite) bind(C, name='setenv') result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: rc
        end function c_setenv
    end interface

    n_pass = 0
    n_fail = 0
    call run_oracle()
    if (n_fail > 0) error stop 1

contains

    subroutine run_oracle()
        character(len=4096) :: root, driver, copied_driver, source_root, fixture_root, frozen_root
        character(len=4096) :: home, cache, local_bin, global_fo, prefix
        character(len=4096) :: build_log, test_log, disabled_log, install_log
        character(len=4096) :: profile_a_log, profile_b_log, profile_a_hit_log
        character(len=4096) :: install_bin
        character(len=:), allocatable :: sentinel_before
        character(len=64) :: pid_text, env, profile_a_flags, profile_b_flags
        character(len=4096) :: private_driver
        character(len=4096) :: profile_a_driver, profile_b_driver
        character(len=4096) :: profile_a_hit_driver
        character(len=:), allocatable :: packed
        character(len=:), allocatable :: build_output, test_output
        character(len=:), allocatable :: disabled_output, install_output
        character(len=:), allocatable :: profile_output
        character(len=:), allocatable :: bytes_before, bytes_after
        integer(c_long_long) :: device_before, inode_before
        integer(c_long_long) :: device_after, inode_after, mtime_ns, size
        integer(c_long_long) :: profile_device_before, profile_inode_before
        integer(c_long_long) :: profile_device_after, profile_inode_after
        integer :: root_status, env_status, exitcode, n_args, pid
        integer(c_int) :: env_rc
        logical :: ok, exists, linked_fixture

        call get_environment_variable('FO_RUN_SELF_REFRESH_ORACLE', env, &
            status=env_status)
        if (env_status /= 0 .or. trim(env) /= '1') then
            write (output_unit, '(a)') &
                'SKIP: set FO_RUN_SELF_REFRESH_ORACLE=1 for the standalone oracle'
            return
        end if

        call process_getcwd(root, root_status)
        call check(root_status == 0, 'current worktree directory is available')
        if (root_status /= 0) return
        linked_fixture = .false.
        call get_environment_variable('FO_SELF_REFRESH_FIXTURE', fixture_root, &
            status=env_status)
        if (env_status == 0 .and. len_trim(fixture_root) > 0) linked_fixture = .true.
        if (linked_fixture) then
            call get_environment_variable('FO_GREMLIN_FROZEN_ROOT', frozen_root, &
                status=env_status)
            if (env_status == 0 .and. len_trim(frozen_root) > 0) root = frozen_root
        end if
        source_root = root
        inquire (file=trim(root)//'/src/build/fo_build_backend.f90', &
            exist=exists)
        call check(exists, 'oracle runs from the fo linked worktree')
        if (.not. exists) return

        call get_environment_variable('FO_BIN', driver, status=env_status)
        if (env_status /= 0) driver = ''
        if (len_trim(driver) == 0) driver = trim(source_root)//'/build/fo/app/fo'
        if (driver(1:1) /= '/') driver = trim(source_root)//'/'//trim(driver)
        call fs_stat(trim(driver), mtime_ns, size, ok)
        call check(ok, 'a current fo driver is available to start the build')
        if (.not. ok) return
        pid = process_getpid()
        write (pid_text, '(i0)') pid
        if (linked_fixture) then
            fixture_root = trim(fixture_root)//'-'//trim(pid_text)
            call prepare_linked_fixture(trim(source_root), trim(driver), &
                trim(fixture_root), ok)
            call check(ok, 'linked fixture source and native core identity are verified')
            if (.not. ok) return
            root = fixture_root
            linked_fixture = .true.
            ! The fixture declares its frozen core as an external library.
            env_rc = c_setenv('FO_LIBRARY_PATH'//c_null_char, &
                trim(root)//'/core'//c_null_char, 1_c_int)
            call check(env_rc == 0, 'linked fixture library path is applied')
            if (env_rc /= 0) return
        end if

        root = trim(root)
        home = temporary_root()//'/fo-self-refresh-oracle-'//trim(pid_text)//'/home'
        cache = temporary_root()//'/fo-self-refresh-oracle-'//trim(pid_text)//'/cache'
        local_bin = trim(home)//'/.local/bin'
        global_fo = trim(local_bin)//'/fo'
        prefix = trim(home)//'/.local'
        copied_driver = temporary_root()//'/fo-self-refresh-oracle-'//trim(pid_text)// &
            '/fo-driver-before-build'
        build_log = temporary_root()//'/fo-self-refresh-oracle-'//trim(pid_text)// &
            '/build.log'
        test_log = temporary_root()//'/fo-self-refresh-oracle-'//trim(pid_text)// &
            '/test.log'
        disabled_log = temporary_root()//'/fo-self-refresh-oracle-'//trim(pid_text)// &
            '/disabled.log'
        install_log = temporary_root()//'/fo-self-refresh-oracle-'//trim(pid_text)// &
            '/install.log'
        profile_a_log = temporary_root()//'/fo-self-refresh-oracle-'//trim(pid_text)// &
            '/profile-a.log'
        profile_b_log = temporary_root()//'/fo-self-refresh-oracle-'//trim(pid_text)// &
            '/profile-b.log'
        profile_a_hit_log = temporary_root()//'/fo-self-refresh-oracle-'// &
            trim(pid_text)//'/profile-a-hit.log'
        install_bin = trim(prefix)//'/bin/fo'
        ! Distinct profiles exercise A/B/A without discarding reusable actions.
        profile_a_flags = '-O3 -fmax-errors=1'
        profile_b_flags = '-O0 -fmax-errors=2'

        call fs_remove_tree(temporary_root()//'/fo-self-refresh-oracle-'// &
            trim(pid_text))
        call fs_make_dir(trim(local_bin))
        call fs_make_dir(trim(cache))
        call fs_write_text(trim(global_fo), &
            'protected-global-fo-sentinel'//new_line('a'))
        call read_file(trim(global_fo), bytes_before, ok)
        call fs_identity(trim(global_fo), device_before, inode_before, exists)
        call check(ok .and. exists, 'isolated global sentinel is created')
        sentinel_before = bytes_before

        call check(fs_copy_exec(trim(driver), trim(copied_driver)) == 0, &
            'starting driver is copied outside the output path')
        call set_test_environment(trim(home), trim(cache), .false., env_rc)
        call check(env_rc == 0, 'build uses an isolated HOME and cache')
        n_args = 0
        call argv_push(packed, n_args, trim(copied_driver))
        call argv_push(packed, n_args, 'build')
        call run_stage(trim(root), packed, n_args, trim(build_log), exitcode)
        call read_file(trim(build_log), build_output, ok)
        call check(exitcode == 0, 'plain fo build succeeds in the linked worktree')
        if (exitcode /= 0 .and. len(build_output) > 0) &
            write (output_unit, '(a)') 'build log tail: '// &
            build_output(max(1, len(build_output) - 1499):)
        call check(index(build_output, 'fo: worktree driver ready: ') > 0, &
            'self-build advertises its private driver path')
        if (index(build_output, 'fo: worktree driver ready: ') == 0 .and. &
            len(build_output) > 0) &
            write (output_unit, '(a)') 'build transcript: '// &
            build_output(max(1, len(build_output) - 1499):)
        private_driver = advertised_driver(build_output)
        call check(len_trim(private_driver) > 0, &
            'the advertised worktree driver path is exact')
        call check(index(trim(private_driver), trim(root)//'/build/') == 1, &
            'the advertised driver belongs to this worktree build tree')

        if (len_trim(private_driver) > 0) then
            call fs_stat(trim(private_driver), mtime_ns, size, exists)
            call check(exists .and. size > 100000, &
                'the advertised private driver is a built executable')
            if (allocated(packed)) deallocate (packed)
            n_args = 0
            call argv_push(packed, n_args, trim(private_driver))
            call argv_push(packed, n_args, 'test')
            call argv_push(packed, n_args, 'test_random_tests')
            call run_stage(trim(root), packed, n_args, trim(test_log), exitcode)
            call read_file(trim(test_log), test_output, ok)
            call check(exitcode == 0, &
                'plain fo test succeeds through the rebuilt private driver')

            call run_profile_build(trim(root), trim(home), trim(cache), &
                trim(private_driver), trim(copied_driver), trim(profile_a_flags), &
                trim(profile_a_log), profile_output, exitcode, env_rc)
            call check(env_rc == 0, 'first profile uses isolated test state')
            call check(exitcode == 0, 'first private profile build succeeds')
            profile_a_driver = advertised_driver(profile_output)
            call check(index(trim(profile_a_driver), &
                trim(root)//'/build/fo/profiles/') == 1, &
                'first profile advertises its selected profile tree')

            call run_profile_build(trim(root), trim(home), trim(cache), &
                trim(private_driver), trim(copied_driver), trim(profile_b_flags), &
                trim(profile_b_log), profile_output, exitcode, env_rc)
            call check(env_rc == 0, 'second profile uses isolated test state')
            call check(exitcode == 0, 'second private profile build succeeds')
            profile_b_driver = advertised_driver(profile_output)
            call check(index(trim(profile_b_driver), &
                trim(root)//'/build/fo/profiles/') == 1, &
                'second profile advertises its selected profile tree')
            call check(trim(profile_b_driver) /= trim(profile_a_driver), &
                'different flag profiles have different private drivers')
            call fs_identity(trim(profile_a_driver), profile_device_before, &
                profile_inode_before, exists)
            call check(exists, 'first profile driver remains available')

            call run_profile_build(trim(root), trim(home), trim(cache), &
                trim(private_driver), trim(copied_driver), trim(profile_a_flags), &
                trim(profile_a_hit_log), profile_output, exitcode, env_rc)
            call check(env_rc == 0, 'profile cache hit uses isolated test state')
            call check(exitcode == 0, 'first profile cache-hit build succeeds')
            profile_a_hit_driver = advertised_driver(profile_output)
            call check(trim(profile_a_hit_driver) == trim(profile_a_driver), &
                'cache hit advertises its exact profile, not the newest sibling')
            call fs_identity(trim(profile_a_driver), profile_device_after, &
                profile_inode_after, exists)
            call check(exists .and. profile_device_after == profile_device_before &
                .and. profile_inode_after == profile_inode_before, &
                'repeated profile build is a cache hit with unchanged inode')

            call set_test_environment(trim(home), trim(cache), .true., env_rc)
            call check(env_rc == 0, 'strict opt-out environment is applied')
            call check(fs_copy_exec(trim(private_driver), trim(copied_driver)) == 0, &
                'private driver can be staged outside its link output')
            if (allocated(packed)) deallocate (packed)
            n_args = 0
            call argv_push(packed, n_args, trim(copied_driver))
            call argv_push(packed, n_args, 'build')
            call run_stage(trim(root), packed, n_args, trim(disabled_log), &
                exitcode)
            call read_file(trim(disabled_log), disabled_output, ok)
            call check(exitcode == 0, 'strict self-refresh opt-out still builds')
            call check(index(disabled_output, 'worktree driver ready') == 0, &
                'FO_DISABLE_SELF_REFRESH=1 suppresses private refresh output')
        end if

        call read_file(trim(global_fo), bytes_after, ok)
        call fs_identity(trim(global_fo), device_after, inode_after, exists)
        call check(ok .and. exists, 'sentinel remains present after build and test')
        call check(bytes_after == sentinel_before, &
            'plain build and test preserve global sentinel bytes')
        call check(device_after == device_before .and. inode_after == inode_before, &
            'plain build and test preserve global sentinel inode')

        if (len_trim(private_driver) > 0) then
            call set_test_environment(trim(home), trim(cache), .true., env_rc)
            call check(env_rc == 0, 'install targets the isolated HOME')
            if (allocated(packed)) deallocate (packed)
            n_args = 0
            call argv_push(packed, n_args, trim(private_driver))
            call argv_push(packed, n_args, 'install')
            call run_stage(trim(root), packed, n_args, trim(install_log), &
                exitcode)
            call read_file(trim(install_log), install_output, ok)
            call check(exitcode == 0, &
                'explicit fo install publishes under the isolated HOME')
            call read_file(trim(global_fo), bytes_after, ok)
            call check(bytes_after /= sentinel_before, &
                'explicit fo install changes the isolated installed CLI')
            inquire (file=trim(install_bin), exist=exists)
            call check(exists, 'explicit install creates ~/.local/bin/fo')
        end if

        if (n_fail == 0) then
            if (linked_fixture) call fs_remove_tree(trim(root))
            call fs_remove_tree(temporary_root()//'/fo-self-refresh-oracle-'// &
                trim(pid_text))
        else
            write (error_unit, '(a)') 'self-refresh logs retained: '// &
                temporary_root()//'/fo-self-refresh-oracle-'//trim(pid_text)
        end if
        call report()
    end subroutine run_oracle

    subroutine prepare_linked_fixture(source_root, driver, fixture, ok)
        character(len=*), intent(in) :: source_root, driver, fixture
        logical, intent(out) :: ok
        character(len=4096) :: archive, module_dir, receipt, compiler, line, origin
        character(len=4096), allocatable :: paths(:), modules(:)
        character(len=64), allocatable :: hashes(:)
        character(len=64) :: actual
        character(len=:), allocatable :: path, name
        integer :: status, unit, n, i, n_modules, collect_status
        logical :: exists

        ok = .false.
        call get_environment_variable('FO_SELF_REFRESH_CORE_ARCHIVE', archive, &
            status=status)
        if (status /= 0 .or. len_trim(archive) == 0) return
        call get_environment_variable('FO_SELF_REFRESH_CORE_MODULE_DIR', module_dir, &
            status=status)
        if (status /= 0 .or. len_trim(module_dir) == 0) return
        call get_environment_variable('FO_SELF_REFRESH_IDENTITY_RECEIPT', receipt, &
            status=status)
        if (status /= 0 .or. len_trim(receipt) == 0) return
        call get_environment_variable('FO_SELF_REFRESH_COMPILER', compiler, status=status)
        if (status /= 0 .or. len_trim(compiler) == 0) return
        call get_environment_variable('FO_SELF_REFRESH_SOURCE_ROOT', origin, status=status)
        if (status /= 0 .or. len_trim(origin) == 0) origin = source_root
        allocate(paths(4096), hashes(4096), modules(4096))
        open(newunit=unit, file=trim(receipt), status='old', action='read', iostat=status)
        if (status /= 0) return
        n = 0
        do
            read(unit, '(a)', iostat=status) line
            if (status < 0) exit
            if (status /= 0 .or. len_trim(line) < 67 .or. n == size(paths)) then
                close(unit)
                return
            end if
            if (line(65:66) /= '  ') then
                close(unit)
                return
            end if
            path = trim(line(67:))
            call sha256_file(path, actual, status)
            if (status /= 0 .or. actual /= line(:64)) then
                write(error_unit, '(a)') 'fixture identity mismatch: '//path
                close(unit)
                return
            end if
            n = n + 1
            paths(n) = fs_canonical_path(path)
            hashes(n) = actual
        end do
        close(unit)
        origin = fs_canonical_path(trim(origin))
        do i = 1, n
            if (index(paths(i), trim(origin)//'/') /= 1) cycle
            name = trim(paths(i)(len_trim(origin) + 1:))
            if (.not. (index(name, '/src/') == 1 .or. index(name, '/app/') == 1 .or. &
                index(name, '/include/') == 1 .or. index(name, '/test-support/') == 1 .or. &
                name == '/fpm.toml' .or. name == '/fpm.lock' .or. &
                name == '/test/test_random_tests.f90')) cycle
            if (.not. source_receipt_has(trim(source_root)//name, source_root, &
                origin, paths, hashes, n)) return
        end do
        if (.not. receipt_has(archive, paths, n)) return
        if (.not. receipt_has(compiler, paths, n)) return
        call sha256_file(driver, actual, status)
        if (status /= 0) return
        exists = .false.
        do i = 1, n
            if (hashes(i) /= actual) cycle
            path = trim(paths(i))
            if (len(path) < 64) cycle
            if (path(len(path) - 63:) == actual) exists = .true.
        end do
        if (.not. exists) return
        if (.not. source_receipt_has(trim(source_root)//'/fpm.toml', source_root, origin, &
            paths, hashes, n)) return
        if (.not. source_receipt_has(trim(source_root)//'/test/test_random_tests.f90', &
            source_root, origin, paths, hashes, n)) return
        call require_receipt_tree(trim(source_root)//'/src', '', source_root, origin, paths, hashes, n, ok)
        if (.not. ok) return
        call require_receipt_tree(trim(source_root)//'/app', '', source_root, origin, paths, hashes, n, ok)
        if (.not. ok) return
        call require_receipt_tree(trim(source_root)//'/include', '', source_root, origin, paths, hashes, n, ok)
        if (.not. ok) return
        call require_receipt_tree(trim(source_root)//'/test-support', '', source_root, origin, paths, hashes, n, ok)
        if (.not. ok) return
        call require_receipt_tree(trim(source_root)//'/build/dependencies', &
            '/src/', source_root, origin, paths, hashes, n, ok)
        if (.not. ok) return
        call require_receipt_tree(trim(source_root)//'/build/dependencies', &
            '/include/', source_root, origin, paths, hashes, n, ok)
        if (.not. ok) return
        ok = .false.
        call fs_collect_files(trim(source_root)//'/build/dependencies', '', &
            'fpm.toml', '', modules, n_modules, ierr=collect_status)
        if (collect_status /= 0) return
        do i = 1, n_modules
            if (.not. source_receipt_has(trim(modules(i)), source_root, origin, &
                paths, hashes, n)) return
        end do
        inquire(file=trim(source_root)//'/fpm.lock', exist=exists)
        if (exists) then
            if (.not. source_receipt_has(trim(source_root)//'/fpm.lock', &
                source_root, origin, paths, hashes, n)) return
        end if
        inquire(file=trim(fixture), exist=exists)
        ok = .false.
        if (exists) return
        call fs_collect_files(trim(module_dir), '', '.mod', '', modules, n_modules, &
            recursive=.false., ierr=collect_status)
        if (collect_status /= 0 .or. n_modules == 0) return
        if (.not. receipt_has(trim(module_dir)//'/fo_cli_entry.mod', paths, n)) return
        if (.not. receipt_has(trim(module_dir)//'/fo_test_random.mod', paths, n)) return
        call fs_make_dir(trim(fixture)//'/core/mod')
        call fs_make_dir(trim(fixture)//'/app')
        call fs_make_dir(trim(fixture)//'/test')
        call fs_make_dir(trim(fixture)//'/fixture-src')
        call fs_make_dir(trim(fixture)//'/src/build')
        call copy_receipt_file(archive, trim(fixture)//'/core/libfo.a', &
            paths, hashes, n, ok)
        if (.not. ok) return
        do i = 1, n_modules
            path = trim(modules(i))
            name = path(index(path, '/', back=.true.) + 1:)
            call copy_receipt_file(path, trim(fixture)//'/core/mod/'//name, &
                paths, hashes, n, ok)
            if (.not. ok) return
        end do
        call copy_receipt_file(trim(origin)//'/app/main.f90', &
            trim(fixture)//'/app/main.f90', paths, hashes, n, ok)
        if (.not. ok) return
        call copy_receipt_file(trim(origin)//'/test/test_random_tests.f90', &
            trim(fixture)//'/test/test_random_tests.f90', paths, hashes, n, ok)
        if (.not. ok) return
        call copy_receipt_file(trim(origin)//'/src/build/fo_build_backend.f90', &
            trim(fixture)//'/src/build/fo_build_backend.f90', paths, hashes, n, ok)
        if (.not. ok) return
        call fs_write_text(trim(fixture)//'/fpm.toml', &
            'name="fo"'//new_line('a')//'[build]'//new_line('a')// &
            'external-modules=["fo_cli_entry","fo_test_random"]'//new_line('a')// &
            'link=["fo"]'//new_line('a')//'[library]'//new_line('a')// &
            'source-dir="fixture-src"'//new_line('a')// &
            'include-dir="core/mod"'//new_line('a')//'[dependencies]'//new_line('a')// &
            'openmp="*"'//new_line('a'))
    end subroutine prepare_linked_fixture

    logical function receipt_has(path, paths, n) result(found)
        character(len=*), intent(in) :: path, paths(:)
        integer, intent(in) :: n
        found = any(paths(:n) == fs_canonical_path(path))
        if (.not. found) write(error_unit, '(a)') 'fixture receipt omits: '//trim(path)
    end function receipt_has

    logical function source_receipt_has(path, root, origin, paths, hashes, n) result(found)
        character(len=*), intent(in) :: path, root, origin, paths(:), hashes(:)
        integer, intent(in) :: n
        character(len=64) :: actual
        integer :: status, i
        found = .false.
        call sha256_file(path, actual, status)
        if (status /= 0) return
        do i = 1, n
            if (paths(i) /= fs_canonical_path(trim(origin)//path(len_trim(root) + 1:))) cycle
            found = hashes(i) == actual
            return
        end do
    end function source_receipt_has

    subroutine require_receipt_tree(directory, needle, source_root, origin, &
            paths, hashes, n, ok)
        character(len=*), intent(in) :: directory, needle, source_root, origin
        character(len=*), intent(in) :: paths(:), hashes(:)
        integer, intent(in) :: n
        logical, intent(out) :: ok
        character(len=4096), allocatable :: files(:)
        integer :: count, i, status
        logical :: exists
        inquire(file=directory, exist=exists)
        ok = .true.
        if (.not. exists) return
        allocate(files(4096))
        call fs_collect_files(directory, '', '', needle, files, count, ierr=status)
        ok = status == 0
        if (.not. ok) return
        do i = 1, count
            ok = source_receipt_has(trim(files(i)), source_root, origin, paths, hashes, n)
            if (.not. ok) return
        end do
    end subroutine require_receipt_tree

    subroutine copy_receipt_file(source, destination, paths, hashes, n, ok)
        character(len=*), intent(in) :: source, destination, paths(:), hashes(:)
        integer, intent(in) :: n
        logical, intent(out) :: ok
        character(len=64) :: actual
        integer :: i, status
        ok = .false.
        do i = 1, n
            if (paths(i) /= fs_canonical_path(source)) cycle
            if (fs_copy_exec(source, destination) /= 0) return
            call sha256_file(destination, actual, status)
            ok = status == 0 .and. actual == hashes(i)
            if (ok) call set_mode(destination, int(o'444'))
            return
        end do
    end subroutine copy_receipt_file

    subroutine run_profile_build(root, home, cache, source_driver, copied_driver, &
            flags, log_path, output, exitcode, env_rc)
        character(len=*), intent(in) :: root, home, cache, source_driver
        character(len=*), intent(in) :: copied_driver, flags, log_path
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exitcode
        integer(c_int), intent(out) :: env_rc

        character(len=:), allocatable :: command
        integer :: n_args
        logical :: read_ok

        call set_test_environment(home, cache, .false., env_rc)
        exitcode = 1
        if (env_rc /= 0) return
        if (fs_copy_exec(source_driver, copied_driver) /= 0) return
        n_args = 0
        call argv_push(command, n_args, copied_driver)
        call argv_push(command, n_args, 'build')
        call argv_push(command, n_args, '--flag')
        call argv_push(command, n_args, flags)
        call run_stage(trim(root), command, n_args, log_path, exitcode)
        call read_file(log_path, output, read_ok)
    end subroutine run_profile_build

    subroutine run_stage(root, packed, n_args, log_path, exitcode)
        character(len=*), intent(in) :: root, packed, log_path
        integer, intent(in) :: n_args
        integer, intent(out) :: exitcode
        real(dp) :: started

        ! A cold 507-source release install took 317 s on GCC 12.2. This is
        ! a command safety cap, not a performance oracle. Cold profile builds
        ! also approach 180 s on CI; allow the same bounded headroom for them.
        write (output_unit, '(a)') 'self-refresh stage started: '//log_path
        flush (output_unit)
        started = wall_time_seconds()
        call process_run_argv_logged(root, packed, n_args, log_path, &
            .false., 900, exitcode)
        write (output_unit, '(a,i0,a,f8.2,a)') &
            'self-refresh stage finished: exit ', exitcode, ', ', &
            wall_time_seconds() - started, ' s; '//log_path
        flush (output_unit)
    end subroutine run_stage

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label

        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//label
        end if
    end subroutine check

    subroutine set_test_environment(home, cache, disable_refresh, rc)
        character(len=*), intent(in) :: home, cache
        logical, intent(in) :: disable_refresh
        integer(c_int), intent(out) :: rc

        character(len=1) :: disabled
        integer(c_int) :: one_rc, two_rc, three_rc, four_rc, five_rc

        disabled = '0'
        if (disable_refresh) disabled = '1'
        one_rc = c_setenv('HOME'//c_null_char, trim(home)//c_null_char, 1_c_int)
        two_rc = c_setenv('TMPDIR'//c_null_char, &
            temporary_root()//c_null_char, 1_c_int)
        three_rc = c_setenv('FO_CACHE_DIR'//c_null_char, &
            trim(cache)//c_null_char, 1_c_int)
        four_rc = c_setenv('FO_DISABLE_SELF_REFRESH'//c_null_char, &
            disabled//c_null_char, 1_c_int)
        five_rc = c_setenv('FO_SELF_REFRESH'//c_null_char, '0'//c_null_char, &
            1_c_int)
        rc = 0_c_int
        if (one_rc /= 0 .or. two_rc /= 0 .or. three_rc /= 0 .or. &
            four_rc /= 0 .or. five_rc /= 0) rc = 1_c_int
    end subroutine set_test_environment

    subroutine read_file(path, content, ok)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        logical, intent(out) :: ok
        integer :: unit, count, read_status

        allocate (character(len=0) :: content)
        ok = .false.
        inquire (file=path, size=count, iostat=read_status)
        if (read_status /= 0 .or. count < 0) return
        deallocate (content)
        allocate (character(len=count) :: content)
        open (newunit=unit, file=path, status='old', action='read', &
            access='stream', form='unformatted', iostat=read_status)
        if (read_status /= 0) return
        if (count > 0) read (unit, iostat=read_status) content
        close (unit)
        ok = (read_status == 0)
    end subroutine read_file

    function advertised_driver(output) result(path)
        character(len=*), intent(in) :: output
        character(len=4096) :: path
        character(len=*), parameter :: marker = 'fo: worktree driver ready: '
        integer :: start, finish

        path = ''
        start = index(output, marker)
        if (start == 0) return
        start = start + len(marker)
        finish = index(output(start:), new_line('a'))
        if (finish == 0) then
            path = output(start:)
        else if (finish > 1) then
            path = output(start:start + finish - 2)
        end if
    end function advertised_driver

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'self-refresh oracle: ', n_pass, &
            ' passed, ', n_fail
    end subroutine report

end program test_self_refresh
