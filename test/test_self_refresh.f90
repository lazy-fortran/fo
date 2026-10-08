program test_self_refresh
    !! Standalone behavior oracle: after `fo build`, run
    !! `FO_RUN_SELF_REFRESH_ORACLE=1 FO_BIN=<worktree>/build/fo/app/fo fo test
    !! test_self_refresh`. Child commands use a temporary HOME and cache.
    use fo_util, only: temporary_root
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, &
        c_null_char
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_fs, only: fs_copy_exec, fs_identity, fs_make_dir, fs_remove_tree, &
        fs_stat, fs_write_text
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
        character(len=4096) :: root, driver, copied_driver
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
        logical :: ok, exists

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
        inquire (file=trim(root)//'/src/build/fo_build_backend.f90', &
            exist=exists)
        call check(exists, 'oracle runs from the fo linked worktree')
        if (.not. exists) return

        call get_environment_variable('FO_BIN', driver, status=env_status)
        if (env_status /= 0) driver = ''
        if (len_trim(driver) == 0) driver = trim(root)//'/build/fo/app/fo'
        if (driver(1:1) /= '/') driver = trim(root)//'/'//trim(driver)
        call fs_stat(trim(driver), mtime_ns, size, ok)
        call check(ok, 'a current fo driver is available to start the build')
        if (.not. ok) return

        pid = process_getpid()
        write (pid_text, '(i0)') pid
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
        write (profile_a_flags, '(a,i0)') '-O3 -fmax-errors=', pid + 1
        write (profile_b_flags, '(a,i0)') '-O0 -fmax-errors=', pid + 2

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
        call process_run_argv_logged(trim(root), packed, n_args, trim(build_log), &
            .false., 180, exitcode)
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
            call process_run_argv_logged(trim(root), packed, n_args, trim(test_log), &
                .false., 180, exitcode)
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
            call process_run_argv_logged(trim(root), packed, n_args, &
                trim(disabled_log), .false., 180, exitcode)
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
            call process_run_argv_logged(trim(root), packed, n_args, &
                trim(install_log), .false., 180, exitcode)
            call read_file(trim(install_log), install_output, ok)
            call check(exitcode == 0, &
                'explicit fo install publishes under the isolated HOME')
            call read_file(trim(global_fo), bytes_after, ok)
            call check(bytes_after /= sentinel_before, &
                'explicit fo install changes the isolated installed CLI')
            inquire (file=trim(install_bin), exist=exists)
            call check(exists, 'explicit install creates ~/.local/bin/fo')
        end if

        call fs_remove_tree(temporary_root()//'/fo-self-refresh-oracle-'// &
            trim(pid_text))
        call report()
    end subroutine run_oracle

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
        call process_run_argv_logged(trim(root), command, n_args, log_path, &
            .false., 180, exitcode)
        call read_file(log_path, output, read_ok)
    end subroutine run_profile_build

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
