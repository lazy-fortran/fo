program test_stat_memo
    !! The persistent file-hash memo must return the true sha256, reuse it while
    !! (mtime,size) are unchanged, recompute when the file changes, and reload
    !! its persisted entries after a save.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    use fo_stat_memo, only: memo_hash_file, memo_save, memo_reset
    use fx_hash, only: sha256_file
    use fo_process, only: process_getpid, process_getcwd, process_start_argv_logged, &
        process_poll_pid, process_cancel_pid, process_exit, argv_push
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree, fs_sleep_ms, &
        fs_stat, fs_collect_files
    implicit none
    integer, parameter :: CROSS_FILES = 1024
    integer, parameter :: CRASH_FILES = 16383
    integer :: n_pass, n_fail
    character(len=512) :: temp_root, cache_root, arg
    character(len=:), allocatable :: prior_cache
    integer :: env_len, env_status, worker_status
    logical :: had_cache

    interface
        integer(c_int) function c_setenv(name, value, overwrite) &
                bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv

        integer(c_int) function c_unsetenv(name) bind(C, name='unsetenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
        end function c_unsetenv

        integer(c_int) function c_kill(pid, signal) bind(C, name='kill')
            import :: c_int
            integer(c_int), value :: pid, signal
        end function c_kill

    end interface

    n_pass = 0
    n_fail = 0

    call get_command_argument(1, arg)
    if (trim(arg) == '--worker') then
        call run_concurrent_worker(worker_status)
        call process_exit(worker_status)
    end if
    if (trim(arg) == '--crash-worker') then
        call run_crash_worker(worker_status)
        call process_exit(worker_status)
    end if

    call get_environment_variable('FO_CACHE_DIR', length=env_len, &
        status=env_status)
    had_cache = env_status == 0
    if (had_cache) then
        allocate (character(len=env_len) :: prior_cache)
        call get_environment_variable('FO_CACHE_DIR', value=prior_cache)
    end if
    call make_tmp_path(temp_root, '.root')
    call fs_remove_tree(trim(temp_root))
    call fs_make_dir(trim(temp_root))
    cache_root = trim(temp_root)//'/cache'
    call fs_make_dir(trim(cache_root))
    call set_cache_root(trim(cache_root))
    call memo_reset()

    call test_matches_direct_sha256()
    call test_recomputes_on_change()
    call test_recomputes_after_same_size_restored_mtime()
    call test_persists_across_reset()
    call test_cross_process_publication(trim(temp_root))
    call test_killed_writer_recovery(trim(temp_root))
    call test_failed_publication_retry(trim(temp_root))

    call memo_reset()
    call fs_remove_tree(trim(temp_root))
    if (had_cache) then
        call set_cache_root(trim(prior_cache))
    else
        env_status = c_unsetenv('FO_CACHE_DIR'//c_null_char)
    end if
    write (output_unit, '(a,i0,a,i0,a)') 'stat_memo: ', n_pass, ' pass, ', &
        n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            if (n_fail <= 12) write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_matches_direct_sha256()
        character(len=512) :: f
        character(len=64) :: memo_h, direct_h
        integer :: ierr

        call write_file(f, 'hello memo')
        call memo_hash_file(trim(f), memo_h, ierr)
        call sha256_file(trim(f), direct_h, ierr)
        call assert(trim(memo_h) == trim(direct_h), &
            'memo hash equals direct sha256')
        ! second call returns the same (served from memo)
        call memo_hash_file(trim(f), memo_h, ierr)
        call assert(trim(memo_h) == trim(direct_h), 'repeat hash stable')
        call fs_remove_file(trim(f))
    end subroutine test_matches_direct_sha256

    subroutine test_recomputes_on_change()
        character(len=512) :: f
        character(len=64) :: h1, h2, direct
        integer :: ierr

        call write_file(f, 'first contents')
        call memo_hash_file(trim(f), h1, ierr)
        ! change content (and size) so (mtime,size) differ
        call write_text(trim(f), 'second, longer contents that change the size')
        call memo_hash_file(trim(f), h2, ierr)
        call sha256_file(trim(f), direct, ierr)
        call assert(trim(h1) /= trim(h2), 'hash changes after edit')
        call assert(trim(h2) == trim(direct), 'post-edit hash is correct')
        call fs_remove_file(trim(f))
    end subroutine test_recomputes_on_change

    subroutine test_recomputes_after_same_size_restored_mtime()
        character(len=512) :: f, reference, command
        character(len=64) :: h1, h2, direct
        integer :: ierr, direct_ierr, status
        integer(c_long_long) :: before_mtime, before_size, after_mtime, after_size
        logical :: stat_ok

        call make_tmp_path(f, '.restored')
        call make_tmp_path(reference, '.reference')
        call write_text(trim(f), 'same length A')
        call memo_hash_file(trim(f), h1, ierr)
        call assert(ierr == 0, 'hash before same-size restored-mtime edit')

        command = 'touch -r '//trim(f)//' '//trim(reference)
        call execute_command_line(trim(command), exitstat=status)
        call assert(status == 0, 'capture original file mtime')
        call write_text(trim(f), 'same length B')
        command = 'touch -r '//trim(reference)//' '//trim(f)
        call execute_command_line(trim(command), exitstat=status)
        call assert(status == 0, 'restore original file mtime')

        call fs_stat(trim(f), before_mtime, before_size, stat_ok)
        call assert(stat_ok, 'stat restored-mtime file')
        call memo_hash_file(trim(f), h2, ierr)
        call sha256_file(trim(f), direct, direct_ierr)
        call fs_stat(trim(f), after_mtime, after_size, stat_ok)
        call assert(stat_ok .and. before_mtime == after_mtime .and. &
            before_size == after_size, 'edit preserves mtime and size')
        call assert(trim(h1) /= trim(h2), &
            'ctime invalidates same-size restored-mtime memo entry')
        call assert(trim(h2) == trim(direct), &
            'restored-mtime edit returns the new sha256')
        call assert(ierr == 0 .and. direct_ierr == 0, &
            'hashes succeed after restored-mtime edit')
        call fs_remove_file(trim(f))
        call fs_remove_file(trim(reference))
    end subroutine test_recomputes_after_same_size_restored_mtime

    subroutine test_persists_across_reset()
        character(len=512) :: f
        character(len=64) :: h1, h2, direct
        integer :: ierr

        call write_file(f, 'persist me')
        call memo_hash_file(trim(f), h1, ierr)
        call memo_save()
        call memo_reset() ! drop in-memory state; force reload
        call memo_hash_file(trim(f), h2, ierr)
        call sha256_file(trim(f), direct, ierr)
        call assert(trim(h2) == trim(direct), 'hash correct after reload')
        call assert(trim(h1) == trim(h2), 'reloaded hash matches pre-save')
        call fs_remove_file(trim(f))
    end subroutine test_persists_across_reset

    subroutine test_cross_process_publication(root)
        character(len=*), intent(in) :: root
        character(len=512) :: data_root, cache, gate, ready, executable, cwd
        character(len=512) :: log_file, source, alias, memo_path
        character(len=64) :: memo_h, direct
        character(len=32) :: group_text
        character(len=:), allocatable :: packed
        integer :: pids(2), exits(2), start_status(2), i, group, n_args
        integer :: ierr, direct_ierr, status, u, ios, record_count
        integer(c_long_long) :: mt, ct, sz
        logical :: done(2), exists

        data_root = trim(root)//'/cross-files'
        cache = trim(root)//'/cross-cache'
        gate = trim(root)//'/cross-save.go'
        call fs_make_dir(trim(data_root))
        call fs_make_dir(trim(cache))
        call write_text(trim(data_root)//'/payload-1.dat', 'worker one payload')
        call write_text(trim(data_root)//'/payload-2.dat', 'worker two payload')
        call set_cache_root(trim(cache))
        call memo_reset()
        call get_command_argument(0, executable)
        call process_getcwd(cwd, status)
        call assert(status == 0, 'get test executable working directory')

        pids = 0
        exits = -1
        start_status = -1
        do group = 1, 2
            write (group_text, '(i0)') group
            ready = trim(gate)//'.ready.'//trim(group_text)
            log_file = trim(root)//'/memo-worker-'//trim(group_text)//'.log'
            packed = ''
            n_args = 0
            call argv_push(packed, n_args, trim(executable))
            call argv_push(packed, n_args, '--worker')
            call argv_push(packed, n_args, trim(data_root))
            call argv_push(packed, n_args, trim(gate))
            call argv_push(packed, n_args, trim(group_text))
            call process_start_argv_logged(trim(cwd), packed, n_args, &
                trim(log_file), pids(group), start_status(group))
        end do
        call assert(all(start_status == 0) .and. all(pids > 0), &
            'start two independent memo processes')

        done = .false.
        if (all(pids > 0) .and. all(start_status == 0)) then
            do group = 1, 2
                write (group_text, '(i0)') group
                ready = trim(gate)//'.ready.'//trim(group_text)
                call wait_for_file(trim(ready), exists)
                call assert(exists, 'worker reached concurrent-save barrier')
            end do
            call write_text(trim(gate), 'release')
            call wait_for_child(pids(1), done(1), exits(1))
            call wait_for_child(pids(2), done(2), exits(2))
        else
            do group = 1, 2
                if (pids(group) > 0) call process_cancel_pid(pids(group), status)
            end do
        end if
        call assert(all(done), 'both concurrent memo processes terminate')
        call assert(all(exits == 0), 'both concurrent memo processes read correct hashes')

        memo_path = trim(cache)//'/stat/v2/memo'
        inquire (file=trim(memo_path), exist=exists)
        call assert(exists, 'concurrent writers publish a memo file')
        if (exists) then
            open (newunit=u, file=trim(memo_path), status='old', iostat=ios)
            record_count = 0
            if (ios == 0) then
                do
                    read (u, *, iostat=ios) mt, ct, sz, memo_h, alias
                    if (ios /= 0) exit
                    record_count = record_count + 1
                    call sha256_file(trim(alias), direct, ierr)
                    call assert(ierr == 0 .and. trim(memo_h) == trim(direct), &
                        'persisted concurrent entry has its true digest')
                end do
                close (u)
            end if
            call assert(ios < 0 .and. record_count >= CROSS_FILES .and. &
                record_count <= 2 * CROSS_FILES, &
                'concurrent publication leaves one complete process snapshot')
        end if

        call memo_reset()
        if (all(done) .and. all(exits == 0)) then
            do group = 1, 2
                do i = 1, CROSS_FILES, CROSS_FILES - 1
                    source = alias_path(trim(data_root), group, i)
                    call memo_hash_file(trim(source), memo_h, ierr)
                    call sha256_file(trim(source), direct, direct_ierr)
                    call assert(ierr == 0 .and. direct_ierr == 0 .and. &
                        trim(memo_h) == trim(direct), &
                        'separate process reload returns the known digest')
                end do
            end do
        end if
        call set_cache_root(trim(root)//'/cache')
        call memo_reset()
    end subroutine test_cross_process_publication

    subroutine test_killed_writer_recovery(root)
        character(len=*), intent(in) :: root
        character(len=512) :: data_root, cache, directory, gate, ready
        character(len=512) :: baseline, executable, cwd, log_file, memo_path
        character(len=:), allocatable :: packed
        character(len=64) :: memo_h, direct, stored_hash
        character(len=512) :: stored_path
        integer :: pid, start_status, n_args, status, exitcode, ierr
        integer :: u, ios, record_count, kill_status, attempt
        integer(c_long_long) :: temp_size, mt, ct, sz
        logical :: exists, done

        data_root = trim(root)//'/killed-files'
        cache = trim(root)//'/killed-cache'
        gate = trim(root)//'/killed-save.go'
        ready = trim(gate)//'.ready'
        directory = trim(cache)//'/stat/v2'
        baseline = trim(root)//'/baseline-source.dat'
        memo_path = trim(directory)//'/memo'
        log_file = trim(root)//'/killed-writer.log'
        call fs_make_dir(trim(data_root))
        call fs_make_dir(trim(cache))
        call write_text(trim(data_root)//'/payload.dat', 'crash writer payload')
        call write_text(trim(baseline), 'prior complete memo snapshot')
        call set_cache_root(trim(cache))
        call memo_reset()
        call memo_hash_file(trim(baseline), memo_h, ierr)
        call assert(ierr == 0, 'hash entry before killed writer')
        call memo_save()

        call get_command_argument(0, executable)
        call process_getcwd(cwd, status)
        packed = ''
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, '--crash-worker')
        call argv_push(packed, n_args, trim(data_root))
        call argv_push(packed, n_args, trim(gate))
        call process_start_argv_logged(trim(cwd), packed, n_args, &
            trim(log_file), pid, start_status)
        call assert(status == 0 .and. start_status == 0 .and. pid > 0, &
            'start writer that will be killed during publication')
        if (pid <= 0 .or. start_status /= 0) then
            call set_cache_root(trim(root)//'/cache')
            call memo_reset()
            return
        end if

        call wait_for_file(trim(ready), exists)
        call assert(exists, 'large writer is ready at save barrier')
        if (.not. exists) then
            call process_cancel_pid(pid, status)
            call set_cache_root(trim(root)//'/cache')
            call memo_reset()
            return
        end if
        call write_text(trim(gate), 'release')

        temp_size = -1_c_long_long
        done = .false.
        exitcode = -1
        do attempt = 1, 10000
            call memo_temp_size(trim(directory), temp_size)
            call process_poll_pid(pid, done, exitcode)
            if (done .or. temp_size > 0_c_long_long) exit
            call fs_sleep_ms(1)
        end do
        kill_status = -1
        if (.not. done .and. temp_size > 0_c_long_long) then
            kill_status = c_kill(int(pid, c_int), 9_c_int)
        end if
        if (.not. done) call wait_for_child(pid, done, exitcode)
        call assert(temp_size > 0_c_long_long, &
            'observe partially written unique temporary before killing writer')
        call assert(kill_status == 0 .and. done .and. exitcode /= 0, &
            'kill interrupts a memo writer before publication completes')

        inquire (file=trim(memo_path), exist=exists)
        call assert(exists, 'previous memo snapshot remains present after kill')
        record_count = 0
        if (exists) then
            open (newunit=u, file=trim(memo_path), status='old', iostat=ios)
            if (ios == 0) then
                do
                    read (u, *, iostat=ios) mt, ct, sz, stored_hash, stored_path
                    if (ios /= 0) exit
                    record_count = record_count + 1
                    call sha256_file(trim(stored_path), direct, ierr)
                    call assert(ierr == 0 .and. trim(direct) == trim(stored_hash), &
                        'surviving snapshot contains a known digest')
                end do
                close (u)
            end if
        end if
        call assert(ios < 0 .and. record_count == 1, &
            'killed writer leaves the prior complete snapshot intact')

        call memo_reset()
        call memo_hash_file(trim(baseline), memo_h, ierr)
        call sha256_file(trim(baseline), direct, status)
        call assert(ierr == 0 .and. status == 0 .and. &
            trim(memo_h) == trim(direct), 'reader recovers after killed writer')
        call set_cache_root(trim(root)//'/cache')
        call memo_reset()
    end subroutine test_killed_writer_recovery

    subroutine test_failed_publication_retry(root)
        character(len=*), intent(in) :: root
        character(len=512) :: cache, blocker, source, memo_path
        character(len=64) :: memo_h, direct
        integer :: ierr
        logical :: exists

        cache = trim(root)//'/retry-cache'
        blocker = trim(root)//'/not-a-directory'
        source = trim(root)//'/retry-source.dat'
        call fs_make_dir(trim(cache))
        call write_text(trim(blocker), 'blocks parent directory creation')
        call write_text(trim(source), 'retry after failed memo publication')
        call set_cache_root(trim(cache))
        call memo_reset()
        call memo_hash_file(trim(source), memo_h, ierr)
        call assert(ierr == 0, 'hash entry before failed publication')
        call set_cache_root(trim(blocker))
        call memo_save()
        call set_cache_root(trim(cache))
        call memo_save()

        memo_path = trim(cache)//'/stat/v2/memo'
        inquire (file=trim(memo_path), exist=exists)
        call assert(exists, 'dirty memo retries after failed publication')
        call memo_reset()
        call memo_hash_file(trim(source), memo_h, ierr)
        call sha256_file(trim(source), direct, ierr)
        call assert(ierr == 0 .and. trim(memo_h) == trim(direct), &
            'memo remains readable after failed-writer recovery')
        call set_cache_root(trim(root)//'/cache')
        call memo_reset()
    end subroutine test_failed_publication_retry

    subroutine run_concurrent_worker(exitcode)
        integer, intent(out) :: exitcode
        character(len=512) :: data_root, gate, group_text, ready, source
        character(len=64) :: memo_h, direct
        integer :: group, i, memo_ierr, direct_ierr, status
        logical :: exists

        exitcode = 1
        call get_command_argument(2, data_root)
        call get_command_argument(3, gate)
        call get_command_argument(4, group_text)
        read (group_text, *, iostat=status) group
        if (status /= 0 .or. group < 1 .or. group > 2) return

        call memo_reset()
        do i = 1, CROSS_FILES
            source = alias_path(trim(data_root), group, i)
            call memo_hash_file(trim(source), memo_h, memo_ierr)
            call sha256_file(trim(source), direct, direct_ierr)
            if (memo_ierr /= 0 .or. direct_ierr /= 0 .or. &
                trim(memo_h) /= trim(direct)) return
        end do
        ready = trim(gate)//'.ready.'//trim(group_text)
        call write_text(trim(ready), 'ready')
        call wait_for_file(trim(gate), exists)
        if (.not. exists) return

        call memo_save()
        call memo_reset()
        do i = 1, CROSS_FILES
            source = alias_path(trim(data_root), group, i)
            call memo_hash_file(trim(source), memo_h, memo_ierr)
            call sha256_file(trim(source), direct, direct_ierr)
            if (memo_ierr /= 0 .or. direct_ierr /= 0 .or. &
                trim(memo_h) /= trim(direct)) return
        end do
        call memo_save()
        exitcode = 0
    end subroutine run_concurrent_worker

    subroutine run_crash_worker(exitcode)
        integer, intent(out) :: exitcode
        character(len=512) :: data_root, gate, ready, source
        character(len=64) :: memo_h, direct
        integer :: i, memo_ierr, direct_ierr
        logical :: exists

        exitcode = 1
        call get_command_argument(2, data_root)
        call get_command_argument(3, gate)
        call memo_reset()
        do i = 1, CRASH_FILES
            source = large_alias_path(trim(data_root), i)
            call memo_hash_file(trim(source), memo_h, memo_ierr)
            call sha256_file(trim(source), direct, direct_ierr)
            if (memo_ierr /= 0 .or. direct_ierr /= 0 .or. &
                trim(memo_h) /= trim(direct)) return
        end do
        ready = trim(gate)//'.ready'
        call write_text(trim(ready), 'ready')
        call wait_for_file(trim(gate), exists)
        if (.not. exists) return
        call memo_save()
        exitcode = 0
    end subroutine run_crash_worker

    function alias_path(root, group, item) result(path)
        character(len=*), intent(in) :: root
        integer, intent(in) :: group, item
        character(len=512) :: path
        character(len=16) :: group_text
        integer :: bit

        path = trim(root)
        do bit = 1, 130
            if (bit <= 10) then
                if (btest(item, bit - 1)) then
                    path = trim(path)//'/./'
                else
                    path = trim(path)//'///'
                end if
            else
                path = trim(path)//'///'
            end if
        end do
        write (group_text, '(i0)') group
        path = trim(path)//'/payload-'//trim(group_text)//'.dat'
    end function alias_path

    function large_alias_path(root, item) result(path)
        character(len=*), intent(in) :: root
        integer, intent(in) :: item
        character(len=512) :: path
        integer :: bit

        path = trim(root)
        do bit = 1, 130
            if (bit <= 14) then
                if (btest(item, bit - 1)) then
                    path = trim(path)//'/./'
                else
                    path = trim(path)//'///'
                end if
            else
                path = trim(path)//'///'
            end if
        end do
        path = trim(path)//'/payload.dat'
    end function large_alias_path

    subroutine wait_for_file(path, found)
        character(len=*), intent(in) :: path
        logical, intent(out) :: found
        integer :: attempt

        found = .false.
        do attempt = 1, 6000
            inquire (file=trim(path), exist=found)
            if (found) return
            call fs_sleep_ms(10)
        end do
    end subroutine wait_for_file

    subroutine wait_for_child(pid, done, exitcode)
        integer, intent(in) :: pid
        logical, intent(out) :: done
        integer, intent(out) :: exitcode
        integer :: attempt, status

        done = .false.
        exitcode = -1
        do attempt = 1, 6000
            call process_poll_pid(pid, done, exitcode)
            if (done) return
            call fs_sleep_ms(10)
        end do
        call process_cancel_pid(pid, status)
    end subroutine wait_for_child

    subroutine memo_temp_size(directory, size_bytes)
        character(len=*), intent(in) :: directory
        integer(c_long_long), intent(out) :: size_bytes
        character(len=512) :: paths(8)
        integer :: n_paths, n_bytes, ios

        size_bytes = -1_c_long_long
        call fs_collect_files(trim(directory), 'memo.tmp.', '', '', paths, &
            n_paths, recursive=.false.)
        if (n_paths < 1) return
        inquire (file=trim(paths(1)), size=n_bytes, iostat=ios)
        if (ios == 0) size_bytes = int(n_bytes, c_long_long)
    end subroutine memo_temp_size

    subroutine set_cache_root(path)
        character(len=*), intent(in) :: path
        integer(c_int) :: status

        status = c_setenv('FO_CACHE_DIR'//c_null_char, &
            trim(path)//c_null_char, 1_c_int)
        call assert(status == 0, 'set isolated FO_CACHE_DIR')
    end subroutine set_cache_root

    subroutine make_tmp_path(path, suffix)
        character(len=*), intent(out) :: path
        character(len=*), intent(in) :: suffix
        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,i0,a,i0,a,i0,a)') '/tmp/fo_statmemo-', &
            process_getpid(), '-', count, '-', serial, trim(suffix)
    end subroutine make_tmp_path

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: u, ios

        open (newunit=u, file=trim(path), status='replace', action='write', &
            iostat=ios)
        if (ios /= 0) then
            call assert(.false., 'open test file for writing: '//trim(path))
            return
        end if
        write (u, '(a)', iostat=ios) text
        call assert(ios == 0, 'write test file: '//trim(path))
        close (u, iostat=ios)
        call assert(ios == 0, 'close test file: '//trim(path))
    end subroutine write_text

    subroutine write_file(path, text)
        character(len=*), intent(out) :: path
        character(len=*), intent(in) :: text
        integer :: u, ios

        call make_tmp_path(path, '.dat')
        open (newunit=u, file=trim(path), status='replace', iostat=ios)
        if (ios /= 0) return
        write (u, '(a)', iostat=ios) text
        close (u)
    end subroutine write_file

end program test_stat_memo
