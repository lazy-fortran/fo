module stat_memo_parallel_oracle
    use fx_hash, only: sha256_file
    use fo_fs, only: fs_sleep_ms
    implicit none
    private
    public :: parallel_provider, reset_parallel_oracle, max_parallel_hashes
    integer, save :: active_hashes = 0
    integer, save :: peak_hashes = 0
contains
    subroutine reset_parallel_oracle()
        active_hashes = 0
        peak_hashes = 0
    end subroutine reset_parallel_oracle

    integer function max_parallel_hashes() result(peak)
        peak = peak_hashes
    end function max_parallel_hashes

    subroutine parallel_provider(path, hash, ierr)
        character(len=*), intent(in) :: path
        character(len=64), intent(out) :: hash
        integer, intent(out) :: ierr
        integer :: current_active

        !$omp atomic update
        active_hashes = active_hashes + 1
        !$omp atomic read
        current_active = active_hashes
        !$omp critical (stat_memo_parallel_oracle)
        peak_hashes = max(peak_hashes, current_active)
        !$omp end critical (stat_memo_parallel_oracle)
        call fs_sleep_ms(150)
        call sha256_file(path, hash, ierr)
        !$omp atomic update
        active_hashes = active_hashes - 1
    end subroutine parallel_provider
end module stat_memo_parallel_oracle

program test_stat_memo
    !! The persistent file-hash memo must return the true sha256, reuse it while
    !! (mtime,size) are unchanged, recompute when the file changes, and reload
    !! its persisted entries after a save.
    use fo_util, only: temporary_root
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    use fo_stat_memo, only: memo_hash_file, memo_save, memo_reset, &
        memo_hash_file_for_test
    use fx_hash, only: sha256_file
    use fo_process, only: process_getpid, process_getcwd, process_start_argv_logged, &
        process_poll_pid, process_cancel_pid, process_exit, argv_push
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree, fs_sleep_ms, &
        fs_stat, fs_collect_files, fs_rename
    use stat_memo_parallel_oracle, only: parallel_provider, reset_parallel_oracle, &
        max_parallel_hashes
    implicit none
    integer, parameter :: CROSS_FILES = 1024
    integer, parameter :: CRASH_FILES = 16383
    integer :: n_pass, n_fail
    character(len=512) :: temp_root, cache_root, arg
    character(len=:), allocatable :: prior_cache
    integer :: env_len, env_status, worker_status
    logical :: had_cache

    interface
        subroutine hash_then_mutate(path, hash, ierr)
            character(len=*), intent(in) :: path
            character(len=64), intent(out) :: hash
            integer, intent(out) :: ierr
        end subroutine hash_then_mutate
        subroutine oracle_initialize() bind(C, name='stat_memo_oracle_initialize')
        end subroutine oracle_initialize
        subroutine arm_io_oracle(mode) bind(C, name='stat_memo_oracle_io')
            import :: c_int
            integer(c_int), value :: mode
        end subroutine arm_io_oracle
        integer(c_int) function oracle_state(which) &
                bind(C, name='stat_memo_oracle_state')
            import :: c_int
            integer(c_int), value :: which
        end function oracle_state
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

    call oracle_initialize()
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
    call test_concurrent_cold_misses(trim(temp_root))
    call test_recomputes_on_change()
    call test_recomputes_after_same_size_restored_mtime()
    call test_persists_across_reset()
    call test_malformed_rows_and_quoted_paths(trim(temp_root))
    call test_unstable_mutation(trim(temp_root))
    call test_cross_process_publication(trim(temp_root))
    call test_killed_writer_recovery(trim(temp_root))
    call test_failed_publication_retry(trim(temp_root))
    call test_io_failure_retry(trim(temp_root))

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

    subroutine test_concurrent_cold_misses(root)
        character(len=*), intent(in) :: root
        integer, parameter :: N = 4
        character(len=512) :: paths(N)
        character(len=64) :: hashes(N), direct
        integer :: ierrs(N), i, direct_ierr

        do i = 1, N
            paths(i) = trim(root)//'/parallel-miss-'//char(iachar('0') + i)
            call write_text(trim(paths(i)), repeat('parallel hash payload ', 128))
        end do
        call reset_parallel_oracle()
        !$omp parallel do num_threads(N) schedule(static)
        do i = 1, N
            call memo_hash_file_for_test(trim(paths(i)), hashes(i), ierrs(i), &
                parallel_provider)
        end do
        !$omp end parallel do

        call assert(max_parallel_hashes() > 1, &
            'independent cold misses hash concurrently')
        do i = 1, N
            call sha256_file(trim(paths(i)), direct, direct_ierr)
            call assert(ierrs(i) == 0 .and. direct_ierr == 0 .and. &
                hashes(i) == direct, 'parallel miss returns the file sha256')
            call fs_remove_file(trim(paths(i)))
        end do
    end subroutine test_concurrent_cold_misses

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

    subroutine test_malformed_rows_and_quoted_paths(root)
        character(len=*), intent(in) :: root
        character(len=512) :: cache, directory, memo_path, quoted_path
        character(len=512) :: second_path, scratch, line, stored_path
        character(len=64) :: memo_h, direct, stored_hash
        integer :: ierr, direct_ierr, u, v, ios, count
        integer(c_long_long) :: mt, ct, sz

        cache = trim(root)//'/quoted-cache'
        directory = trim(cache)//'/stat/v2'
        memo_path = trim(directory)//'/memo'
        quoted_path = trim(root)//"/it's a quoted path.dat"
        second_path = trim(root)//'/second-source.dat'
        scratch = trim(root)//'/memo-with-malformed-row'
        call fs_make_dir(trim(cache))
        call write_text(trim(quoted_path), 'quoted path payload')
        call write_text(trim(second_path), 'second path payload')
        call set_cache_root(trim(cache))
        call memo_reset()
        call memo_hash_file(trim(quoted_path), memo_h, ierr)
        call assert(ierr == 0, 'hash path with spaces and apostrophe')
        call memo_save()

        open (newunit=u, file=trim(memo_path), status='old', iostat=ios)
        open (newunit=v, file=trim(scratch), status='replace', &
            action='write', iostat=direct_ierr)
        call assert(ios == 0 .and. direct_ierr == 0, &
            'open memo files to insert malformed numeric row')
        if (ios == 0 .and. direct_ierr == 0) then
            write (v, '(a)', iostat=direct_ierr) &
                'bad-number 1 2 '//repeat('0', 64)//" 'malformed path'"
            do
                read (u, '(a)', iostat=ios) line
                if (ios /= 0) exit
                write (v, '(a)', iostat=direct_ierr) trim(line)
                if (direct_ierr /= 0) exit
            end do
            close (u)
            close (v)
            call assert(ios < 0 .and. direct_ierr == 0, &
                'write malformed row before the preserved valid row')
            ios = fs_rename(trim(scratch), trim(memo_path))
            call assert(ios == 0, 'replace memo with malformed-row fixture')
        else
            if (ios == 0) close (u)
            if (direct_ierr == 0) close (v)
        end if

        call memo_reset()
        call memo_hash_file(trim(second_path), memo_h, ierr)
        call assert(ierr == 0, 'hash another entry after malformed row')
        call memo_save()
        count = 0
        open (newunit=u, file=trim(memo_path), status='old', iostat=ios)
        if (ios == 0) then
            do
                read (u, *, iostat=ios) mt, ct, sz, stored_hash, stored_path
                if (ios /= 0) exit
                count = count + 1
                if (trim(stored_path) == trim(quoted_path)) then
                    call sha256_file(trim(quoted_path), direct, direct_ierr)
                    call assert(direct_ierr == 0 .and. &
                        trim(stored_hash) == trim(direct), &
                        'quoted path survives reload with known digest')
                end if
            end do
            close (u)
        end if
        call assert(ios < 0 .and. count == 2, &
            'malformed numeric row does not discard later valid rows')

        call memo_reset()
        call memo_hash_file(trim(quoted_path), memo_h, ierr)
        call sha256_file(trim(quoted_path), direct, direct_ierr)
        call assert(ierr == 0 .and. direct_ierr == 0 .and. &
            trim(memo_h) == trim(direct), 'quoted path reload remains correct')
        call set_cache_root(trim(root)//'/cache')
        call memo_reset()
    end subroutine test_malformed_rows_and_quoted_paths

    subroutine test_unstable_mutation(root)
        character(len=*), intent(in) :: root
        character(len=512) :: cache, source, memo_path
        character(len=64) :: hash, direct
        integer :: ierr, direct_ierr
        logical :: exists

        cache = trim(root)//'/unstable-cache'
        source = trim(root)//'/unstable-source.dat'
        memo_path = trim(cache)//'/stat/v2/memo'
        call fs_make_dir(trim(cache))
        call write_text(trim(source), repeat('A', 4096))
        call set_cache_root(trim(cache))
        call memo_reset()
        call arm_io_oracle(3_c_int)
        call memo_hash_file_for_test(trim(source), hash, ierr, hash_then_mutate)
        call arm_io_oracle(0_c_int)
        call assert(oracle_state(1_c_int) == 3 .and. &
            oracle_state(2_c_int) == 3 .and. oracle_state(3_c_int) == 0, &
            'each provider returns a real old digest after mutating the source')
        call assert(ierr /= 0 .and. len_trim(hash) == 0, &
            'unstable hash retries fail without exposing a digest')
        call memo_save()
        inquire (file=trim(memo_path), exist=exists)
        call assert(.not. exists, 'unstable digest was not persisted as an action')

        call memo_hash_file(trim(source), hash, ierr)
        call sha256_file(trim(source), direct, direct_ierr)
        call assert(ierr == 0 .and. direct_ierr == 0 .and. hash == direct, &
            'stable retry computes the actual post-mutation file digest')
        call memo_save()
        inquire (file=trim(memo_path), exist=exists)
        call assert(exists, 'stable retry can persist after exhausted hash attempts')
        call set_cache_root(trim(root)//'/cache')
        call memo_reset()
    end subroutine test_unstable_mutation

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
        if (.not. done) then
            kill_status = c_kill(int(pid, c_int), 9_c_int)
            if (kill_status /= 0) call process_cancel_pid(pid, status)
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
        character(len=512) :: cache, blocker, prior, source, later, memo_path
        character(len=512) :: directory, stored_path, paths(8)
        character(len=64) :: memo_h, direct, stored_hash
        integer :: ierr, direct_ierr, env_status, u, ios, count, n_paths
        integer(c_long_long) :: mt, ct, sz
        logical :: exists, found_prior

        cache = trim(root)//'/retry-cache'
        blocker = trim(root)//'/not-a-directory'
        prior = trim(root)//'/retry-prior.dat'
        source = trim(root)//'/retry-source.dat'
        later = trim(root)//'/retry-later.dat'
        directory = trim(cache)//'/stat/v2'
        memo_path = trim(directory)//'/memo'
        call fs_make_dir(trim(cache))
        call write_text(trim(blocker), 'blocks parent directory creation')
        call write_text(trim(prior), 'prior complete memo snapshot')
        call write_text(trim(source), 'retry after failed memo publication')
        call write_text(trim(later), 'retry after directory failure')
        call set_cache_root(trim(cache))
        call memo_reset()
        call memo_hash_file(trim(prior), memo_h, ierr)
        call assert(ierr == 0, 'hash entry for prior snapshot')
        call memo_save()

        call memo_hash_file(trim(source), memo_h, ierr)
        call assert(ierr == 0, 'hash entry before failed publication')
        env_status = c_setenv('FO_STAT_MEMO_TEST_FAIL_RENAME'//c_null_char, &
            '1'//c_null_char, 1_c_int)
        call assert(env_status == 0, 'enable post-write rename failure')
        call memo_save()
        env_status = c_unsetenv('FO_STAT_MEMO_TEST_FAIL_RENAME'//c_null_char)
        call assert(env_status == 0, 'disable post-write rename failure')

        inquire (file=trim(memo_path), exist=exists)
        call assert(exists, 'failed rename leaves prior memo file present')
        count = 0
        found_prior = .false.
        if (exists) then
            open (newunit=u, file=trim(memo_path), status='old', iostat=ios)
            if (ios == 0) then
                do
                    read (u, *, iostat=ios) mt, ct, sz, stored_hash, stored_path
                    if (ios /= 0) exit
                    count = count + 1
                    if (trim(stored_path) == trim(prior)) then
                        found_prior = .true.
                        call sha256_file(trim(prior), direct, direct_ierr)
                        call assert(direct_ierr == 0 .and. &
                            trim(stored_hash) == trim(direct), &
                            'failed rename preserves prior snapshot digest')
                    end if
                end do
                close (u)
            end if
        end if
        call assert(ios < 0 .and. count == 1 .and. found_prior, &
            'post-write rename failure preserves complete old snapshot')
        call fs_collect_files(trim(directory), 'memo.tmp.', '', '', paths, &
            n_paths, recursive=.false.)
        call assert(n_paths == 0, 'failed rename removes its completed temporary')

        call memo_save()
        call memo_hash_file(trim(later), memo_h, ierr)
        call assert(ierr == 0, 'hash another entry before parent-path failure')
        call set_cache_root(trim(blocker))
        call memo_save()
        call set_cache_root(trim(cache))
        call memo_save()

        inquire (file=trim(memo_path), exist=exists)
        call assert(exists, 'dirty memo retries after publication failures')
        count = 0
        if (exists) then
            open (newunit=u, file=trim(memo_path), status='old', iostat=ios)
            if (ios == 0) then
                do
                    read (u, *, iostat=ios) mt, ct, sz, stored_hash, stored_path
                    if (ios /= 0) exit
                    count = count + 1
                end do
                close (u)
            end if
        end if
        call assert(ios < 0 .and. count == 3, &
            'retries publish all entries after post-write and pre-open failures')
        call memo_reset()
        call memo_hash_file(trim(prior), memo_h, ierr)
        call sha256_file(trim(prior), direct, direct_ierr)
        call assert(ierr == 0 .and. direct_ierr == 0 .and. &
            trim(memo_h) == trim(direct), 'prior entry remains readable')
        call memo_hash_file(trim(source), memo_h, ierr)
        call sha256_file(trim(source), direct, direct_ierr)
        call assert(ierr == 0 .and. direct_ierr == 0 .and. &
            trim(memo_h) == trim(direct), 'first dirty entry survives retry')
        call memo_hash_file(trim(later), memo_h, ierr)
        call sha256_file(trim(later), direct, direct_ierr)
        call assert(ierr == 0 .and. direct_ierr == 0 .and. &
            trim(memo_h) == trim(direct), 'later dirty entry survives retry')
        call set_cache_root(trim(root)//'/cache')
        call memo_reset()
    end subroutine test_failed_publication_retry

    subroutine test_io_failure_retry(root)
        character(len=*), intent(in) :: root
        character(len=512) :: cache, prior, source, memo_path, directory, paths(8)
        character(len=64) :: hash, before, after, direct
        character(len=24) :: mode_text
        integer :: mode, ierr, direct_ierr, n_paths, u, ios, count
        integer(c_long_long) :: mt, ct, sz
        character(len=512) :: stored_path
        character(len=64) :: stored_hash

        do mode = 1, 2
            write (mode_text, '(i0)') mode
            cache = trim(root)//'/io-cache-'//trim(mode_text)
            prior = trim(root)//'/io-prior-'//trim(mode_text)
            source = trim(root)//'/io-source-'//trim(mode_text)
            directory = trim(cache)//'/stat/v2'
            memo_path = trim(directory)//'/memo'
            call fs_make_dir(trim(cache))
            call write_text(trim(prior), 'prior complete snapshot')
            call write_text(trim(source), 'dirty retry entry')
            call set_cache_root(trim(cache))
            call memo_reset()
            call memo_hash_file(trim(prior), hash, ierr)
            call assert(ierr == 0, 'hash prior entry for I/O failure oracle')
            call memo_save()
            call sha256_file(trim(memo_path), before, ierr)
            call assert(ierr == 0, 'read complete snapshot before I/O fault')
            call memo_hash_file(trim(source), hash, ierr)
            call assert(ierr == 0, 'hash dirty entry before I/O fault')
            call arm_io_oracle(mode)
            call memo_save()
            call arm_io_oracle(0)
            call assert(oracle_state(4_c_int) == 1, 'injected EIO reached owned write/close')
            call sha256_file(trim(memo_path), after, ierr)
            call assert(ierr == 0 .and. before == after, &
                'write/close failure preserves every byte of old snapshot')
            call fs_collect_files(trim(directory), 'memo.tmp.', '', '', paths, &
                n_paths, recursive=.false.)
            call assert(n_paths == 0, 'write/close failure removes owned temporary')

            ! No rehash/reset here: publication must retry the existing dirty state.
            call memo_save()
            open (newunit=u, file=trim(memo_path), status='old', iostat=ios)
            count = 0
            if (ios == 0) then
                do
                    read (u, *, iostat=ios) mt, ct, sz, stored_hash, stored_path
                    if (ios /= 0) exit
                    count = count + 1
                    call sha256_file(trim(stored_path), direct, direct_ierr)
                    call assert(direct_ierr == 0 .and. stored_hash == direct, &
                        'retried snapshot contains actual source digests')
                    call assert(trim(stored_path) == trim(prior) .or. &
                        trim(stored_path) == trim(source), &
                        'retried snapshot contains only the two expected paths')
                end do
                close (u)
            end if
            call assert(ios < 0 .and. count == 2, &
                'write/close failure retains dirty entries for a complete retry')
        end do
        call set_cache_root(trim(root)//'/cache')
        call memo_reset()
    end subroutine test_io_failure_retry

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
        call arm_io_oracle(4_c_int)
        call memo_save()
        exitcode = 0
    end subroutine run_crash_worker

    function alias_path(root, group, item) result(path)
        character(len=*), intent(in) :: root
        integer, intent(in) :: group, item
        character(len=512) :: path
        character(len=16) :: group_text
        character(len=32) :: suffix
        integer :: bit, components

        write (group_text, '(i0)') group
        suffix = '/payload-'//trim(group_text)//'.dat'
        ! Keep all distinguishing bits; only redundant slash padding may shrink
        ! when a private execution TMPDIR gives the fixture a longer root.
        components = min(130, &
            (len(path) - len_trim(root) - len_trim(suffix)) / 3)
        if (components < 10) error stop 'stat memo alias root leaves no room for 10 distinct bits'
        path = trim(root)
        do bit = 1, components
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
        path = trim(path)//trim(suffix)
    end function alias_path

    function large_alias_path(root, item) result(path)
        character(len=*), intent(in) :: root
        integer, intent(in) :: item
        character(len=512) :: path
        integer :: bit, components

        components = min(130, &
            (len(path) - len_trim(root) - len('/payload.dat')) / 3)
        if (components < 14) error stop 'stat memo alias root leaves no room for 14 distinct bits'
        path = trim(root)
        do bit = 1, components
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
        write (path, '(a,i0,a,i0,a,i0,a)') temporary_root()//'/fo_statmemo-', &
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

! Publication I/O interposition belongs only to this test executable.
! Production calls these functions directly; the SHA oracle uses a provider.
module stat_memo_io_oracle
    use fo_fs, only: fs_sleep_ms
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t, c_intptr_t, &
        c_ptr, c_funptr, c_null_char, c_associated, c_f_procpointer, c_f_pointer
    implicit none
    private
    public :: oracle_initialize, arm_io_oracle
    public :: oracle_write, oracle_close, oracle_mkstemp
    public :: hash_calls, hash_mutations, hash_errors
    integer :: hash_calls = 0, hash_mutations = 0, hash_errors = 0
    integer :: io_faults = 0
    integer :: fault_mode = 0
    integer(c_int) :: owned_fd = -1_c_int
    abstract interface
        function byte_io(fd, bytes, count) bind(C) result(n)
            import :: c_int, c_ptr, c_size_t, c_intptr_t
            integer(c_int), value :: fd
            type(c_ptr), value :: bytes
            integer(c_size_t), value :: count
            integer(c_intptr_t) :: n
        end function byte_io
        function fd_close(fd) bind(C) result(rc)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: rc
        end function fd_close
        function create_temp(path) bind(C) result(fd)
            import :: c_int, c_char
            character(kind=c_char), intent(inout) :: path(*)
            integer(c_int) :: fd
        end function create_temp
        function errno_address() bind(C) result(address)
            import :: c_ptr
            type(c_ptr) :: address
        end function errno_address
    end interface
    procedure(byte_io), pointer :: real_write => null()
    procedure(fd_close), pointer :: real_close => null()
    procedure(create_temp), pointer :: real_mkstemp => null()
    procedure(errno_address), pointer :: real_errno => null()
    interface
        function c_dlsym(handle, name) bind(C, name='dlsym') result(address)
            import :: c_ptr, c_char
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: name(*)
            type(c_ptr) :: address
        end function c_dlsym
    end interface
contains
    function resolve(name) result(address)
        character(len=*), intent(in) :: name
        type(c_funptr) :: address
        type(c_ptr) :: next, raw
        next = transfer(-1_c_intptr_t, next)
        raw = c_dlsym(next, name//c_null_char)
        if (.not. c_associated(raw)) error stop 'cannot resolve libc oracle symbol'
        address = transfer(raw, address)
    end function resolve

    subroutine oracle_initialize() bind(C, name='stat_memo_oracle_initialize')
        type(c_funptr) :: address
        if (associated(real_write)) return
        call c_f_procpointer(resolve('write'), real_write)
        call c_f_procpointer(resolve('close'), real_close)
        call c_f_procpointer(resolve('mkstemp'), real_mkstemp)
        ! errno has different names on Linux and macOS.
        block
            type(c_ptr) :: next, raw
            next = transfer(-1_c_intptr_t, next)
            raw = c_dlsym(next, '__errno_location'//c_null_char)
            if (.not. c_associated(raw)) raw = c_dlsym(next, '__error'//c_null_char)
            if (.not. c_associated(raw)) error stop 'cannot resolve libc errno'
            address = transfer(raw, address)
        end block
        call c_f_procpointer(address, real_errno)
    end subroutine oracle_initialize

    subroutine arm_io_oracle(mode) bind(C, name='stat_memo_oracle_io')
        integer(c_int), value :: mode
        fault_mode = mode
        owned_fd = -1_c_int
        if (mode /= 0) io_faults = 0
        if (mode == 3) then
            hash_calls = 0
            hash_mutations = 0
            hash_errors = 0
        end if
    end subroutine arm_io_oracle

    integer(c_int) function oracle_state(which) &
            bind(C, name='stat_memo_oracle_state') result(state)
        integer(c_int), value :: which
        select case (which)
        case (1)
            state = hash_calls
        case (2)
            state = hash_mutations
        case (3)
            state = hash_errors
        case (4)
            state = io_faults
        case default
            state = -1
        end select
    end function oracle_state

    subroutine set_io_error()
        integer(c_int), pointer :: value
        call c_f_pointer(real_errno(), value)
        value = 5_c_int ! POSIX EIO on the supported Linux/macOS platforms.
    end subroutine set_io_error

    function oracle_mkstemp(path) bind(C, name='mkstemp') result(fd)
        character(kind=c_char), intent(inout) :: path(*)
        integer(c_int) :: fd
        call oracle_initialize()
        fd = real_mkstemp(path)
        if (fault_mode /= 0) owned_fd = fd
    end function oracle_mkstemp

    function oracle_write(fd, buffer, count) bind(C, name='write') result(n)
        integer(c_int), value :: fd
        type(c_ptr), value :: buffer
        integer(c_size_t), value :: count
        integer(c_intptr_t) :: n
        call oracle_initialize()
        if (fault_mode == 4 .and. fd == owned_fd) then
            n = real_write(fd, buffer, min(count, 1_c_size_t))
            if (n > 0_c_intptr_t) then
                do
                    call fs_sleep_ms(10)
                end do
            end if
            return
        end if
        if (fault_mode == 1 .and. fd == owned_fd) then
            io_faults = io_faults + 1
            call set_io_error()
            n = -1_c_intptr_t
            return
        end if
        n = real_write(fd, buffer, count)
    end function oracle_write

    function oracle_close(fd) bind(C, name='close') result(rc)
        integer(c_int), value :: fd
        integer(c_int) :: rc
        call oracle_initialize()
        rc = real_close(fd)
        if (fault_mode == 2 .and. fd == owned_fd) then
            io_faults = io_faults + 1
            call set_io_error()
            rc = -1_c_int
        end if
    end function oracle_close
end module stat_memo_io_oracle

! Standalone provider avoids internal-procedure trampolines on both platforms.
subroutine hash_then_mutate(path, hash, ierr)
    !! Return an actual old-version SHA digest after the source has changed.
    !! This runs inside the production pre/post-stat bracket, with no libc
    !! interposition or timing assumptions on either Linux or macOS.
    use fx_hash, only: sha256_file
    use stat_memo_io_oracle, only: hash_calls, hash_mutations, hash_errors
    implicit none
    character(len=*), intent(in) :: path
    character(len=64), intent(out) :: hash
    integer, intent(out) :: ierr
    character(len=64) :: changed_hash
    integer :: u, ios, close_ios, changed_ierr

    hash_calls = hash_calls + 1
    call sha256_file(path, hash, ierr)
    if (ierr /= 0) then
        hash_errors = hash_errors + 1
        return
    end if
    open (newunit=u, file=path, status='old', access='stream', &
        form='unformatted', action='write', position='append', iostat=ios)
    if (ios /= 0) then
        hash_errors = hash_errors + 1
        ierr = 1
        return
    end if
    write (u, iostat=ios) 'B'
    close (u, iostat=close_ios)
    if (ios /= 0 .or. close_ios /= 0) then
        hash_errors = hash_errors + 1
        ierr = 1
        return
    end if
    hash_mutations = hash_mutations + 1
    call sha256_file(path, changed_hash, changed_ierr)
    if (changed_ierr /= 0 .or. changed_hash == hash) then
        hash_errors = hash_errors + 1
    end if
end subroutine hash_then_mutate
