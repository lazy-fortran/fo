module fo_gremlin_context
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: iso_c_binding, only: c_long_long
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_fpm_config, only: DEP_PATH, fpm_config_t, fpm_config_parse, dep_kind
    use fo_gremlin_generation, only: generation_context_t, generation_input_t, &
        generation_t, generation_capture
    use fo_gremlin_state, only: gremlin_generation_register_at
    use fo_change_watch, only: change_watch_t, change_watch_add_context
    use fo_process, only: argv_push, process_cancel_pid, process_poll_pid, &
        process_start_argv_logged
    use fo_util, only: make_tmpfile, make_sibling_tmpfile, delete_tmpfile, read_text_file
    use fo_fs, only: fs_find_executable, fs_sleep_ms, fs_tree_fingerprint, fs_rename
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096

    public :: capture_candidate

contains

    subroutine capture_candidate(project_dir, generation, ok, &
            registration_error, message, change_watch)
        character(len=*), intent(in) :: project_dir
        type(generation_t), intent(out) :: generation
        logical, intent(out) :: ok
        integer, intent(out) :: registration_error
        character(len=*), intent(out) :: message
        type(change_watch_t), intent(inout), optional :: change_watch

        type(generation_context_t) :: context
        character(len=PATH_LEN) :: cas_root
        character(len=PATH_LEN) :: watch_message
        integer :: ierr

        registration_error = 0
        call capture_context(project_dir, context, ierr, message)
        if (ierr /= 0) then
            ok = .false.
            return
        end if
        if (present(change_watch)) then
            call change_watch_add_context(change_watch, context, ierr, watch_message)
            if (ierr /= 0) then
                message = 'cannot watch Gremlin path dependencies: '// &
                    trim(watch_message)
                registration_error = ierr
                ok = .false.
                return
            end if
        end if
        call common_generation_cas_root(cas_root, ierr, message)
        if (ierr /= 0) then
            ok = .false.
            return
        end if
        call generation_capture(project_dir, trim(cas_root), context, generation, &
            ierr, message)
        ok = ierr == 0
        if (.not. ok) return
        call gremlin_generation_register_at(generation%root, ierr, message)
        ok = ierr == 0
        if (ierr /= 0) registration_error = ierr
        if (.not. ok) return
        if (.not. present(change_watch)) return
        change_watch%capture_count = change_watch%capture_count + 1
        call observe_test_capture(change_watch%capture_count, generation%identity)
    end subroutine capture_candidate

    subroutine observe_test_capture(count, identity)
        !! Explicit test opt-in: replace one bounded record atomically. Observer
        !! failure never changes capture success or the production journal.
        integer, intent(in) :: count
        character(len=*), intent(in) :: identity
        character(len=PATH_LEN) :: path, temporary
        integer :: status, unit, write_status

        call get_environment_variable('FO_GREMLIN_TEST_CAPTURE_COUNTER', path, &
            status=status)
        if (status /= 0 .or. len_trim(path) == 0) return
        call make_sibling_tmpfile(trim(path), temporary)
        open (newunit=unit, file=trim(temporary), status='new', &
            action='write', iostat=status)
        if (status /= 0) return
        write (unit, '(a,i0,a,a,a)', iostat=write_status) '{"count":', count, &
            ',"generation":"', identity, '"}'
        close (unit, iostat=status)
        if (status == 0 .and. write_status == 0) then
            status = fs_rename(trim(temporary), trim(path))
        end if
        call delete_tmpfile(trim(temporary))
    end subroutine observe_test_capture

    subroutine common_generation_cas_root(root, ierr, message)
        character(len=*), intent(out) :: root
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: base
        integer :: length, status

        root = ''
        base = ''
        call get_environment_variable('FO_GREMLIN_STATE_DIR', base, length, status)
        if (status == -1 .or. length >= len(base)) then
            ierr = 1
            message = 'FO_GREMLIN_STATE_DIR exceeds the supported path length'
            return
        end if
        if (length <= 0) then
            base = ''
            call get_environment_variable('XDG_CACHE_HOME', base, length, status)
            if (status == -1 .or. length >= len(base)) then
                ierr = 1
                message = 'XDG_CACHE_HOME exceeds the supported path length'
                return
            end if
        end if
        if (length <= 0) then
            base = ''
            call get_environment_variable('HOME', base, length, status)
            if (status /= 0 .or. length <= 0 .or. length >= len(base)) then
                ierr = 1
                message = 'no shared Gremlin state base directory is available'
                return
            end if
            base = trim(base(:length))//'/.cache'
            length = len_trim(base)
        end if
        if (length + len('/fo') > len(root)) then
            ierr = 1
            message = 'shared Gremlin generation CAS path exceeds the supported length'
            return
        end if
        root = trim(base(:length))//'/fo'
        ierr = 0
        message = ''
    end subroutine common_generation_cas_root

    subroutine capture_context(project_dir, context, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(generation_context_t), intent(out) :: context
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(fpm_config_t), allocatable :: config
        type(generation_input_t), allocatable :: inputs(:)
        character(len=PATH_LEN) :: compiler_path, command_path
        character(len=:), allocatable :: packed
        character(len=PATH_LEN) :: log_file, output_line
        character(len=128) :: base_commit
        character(len=131072) :: git_status
        character(len=256) :: fingerprint
        integer(c_long_long) :: tree_sum, tree_mixed, tree_count
        integer :: i, n_inputs, n_args, exitcode, git_exit
        logical :: found, git_found, fingerprint_ok

        ierr = 0
        message = ''
        context%toolchain = ''
        context%flags = ''
        context%environment = ''
        context%base_commit = ''
        context%patch_digest = ''
        allocate (config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) then
            message = 'cannot parse fpm.toml for generation inputs'
            return
        end if
        n_inputs = 0
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) n_inputs = n_inputs + 1
        end do
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) == DEP_PATH) n_inputs = n_inputs + 1
        end do
        allocate (inputs(n_inputs))
        n_inputs = 0
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) /= DEP_PATH) cycle
            call append_path_dependency(project_dir, config%deps(i)%path, &
                config%deps(i)%name, inputs, n_inputs, ierr, message)
            if (ierr /= 0) return
        end do
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) /= DEP_PATH) cycle
            call append_path_dependency(project_dir, config%dev_deps(i)%path, &
                config%dev_deps(i)%name, inputs, n_inputs, ierr, message)
            if (ierr /= 0) return
        end do
        context%inputs = inputs
        do i = 1, config%n_flags
            context%flags = context%flags//' '//trim(config%flags(i))
        end do
        call fs_find_executable('gfortran', compiler_path, found)
        if (.not. found) then
            context%toolchain = 'gfortran-unresolved'
        else
            call make_tmpfile('fo-gremlin-compiler', log_file)
            packed = ''
            n_args = 0
            output_line = ''
            call argv_push(packed, n_args, trim(compiler_path))
            call argv_push(packed, n_args, '--version')
            call process_start_argv_logged(project_dir, packed, n_args, log_file, i, exitcode)
            if (exitcode == 0) then
                call context_process_wait_bounded(i, 10, exitcode)
                if (exitcode == 0) then
                    call read_first_line(log_file, output_line)
                    context%toolchain = trim(compiler_path)//':'//trim(output_line)
                else
                    context%toolchain = trim(compiler_path)//':version-unavailable'
                end if
            else
                context%toolchain = trim(compiler_path)//':version-unavailable'
            end if
        end if
        call append_environment_value(context%environment, 'FC')
        call append_environment_value(context%environment, 'FFLAGS')
        call append_environment_value(context%environment, 'FPM_FC')
        call append_environment_value(context%environment, 'FPM_FFLAGS')
        call append_environment_value(context%environment, 'OMP_NUM_THREADS')
        call make_tmpfile('fo-gremlin-git-head', log_file)
        call fs_find_executable('git', command_path, found)
        git_found = found
        if (found) then
            packed = ''
            n_args = 0
            output_line = ''
            call argv_push(packed, n_args, trim(command_path))
            call argv_push(packed, n_args, 'rev-parse')
            call argv_push(packed, n_args, 'HEAD')
            call process_start_argv_logged(project_dir, packed, n_args, log_file, &
                i, git_exit)
            if (git_exit == 0) call context_process_wait_bounded(i, 10, git_exit)
            if (git_exit == 0) then
                call read_first_line(log_file, output_line)
                base_commit = trim(output_line)
            else
                base_commit = 'unavailable'
            end if
        else
            base_commit = 'unavailable'
        end if
        context%base_commit = trim(base_commit)
        call fs_tree_fingerprint(project_dir, .true., tree_sum, tree_mixed, &
            tree_count, fingerprint_ok)
        if (fingerprint_ok) then
            write (fingerprint, '(i0,":",i0,":",i0)') tree_sum, tree_mixed, tree_count
        else
            fingerprint = 'fingerprint-unavailable'
        end if
        git_status = ''
        call make_tmpfile('fo-gremlin-git-diff', log_file)
        if (git_found) then
            packed = ''
            n_args = 0
            output_line = ''
            call argv_push(packed, n_args, trim(command_path))
            call argv_push(packed, n_args, 'diff')
            call argv_push(packed, n_args, '--binary')
            call argv_push(packed, n_args, 'HEAD')
            call process_start_argv_logged(project_dir, packed, n_args, log_file, &
                i, git_exit)
            if (git_exit == 0) call context_process_wait_bounded(i, 20, git_exit)
            if (git_exit == 0) then
                call read_text_file(log_file, git_status)
                context%patch_digest = context_text_digest(trim(git_status)//'|'//trim(fingerprint))
            else
                context%patch_digest = context_text_digest('git-diff-unavailable|'//trim(fingerprint))
            end if
        else
            context%patch_digest = context_text_digest('git-unavailable|'//trim(fingerprint))
        end if
    end subroutine capture_context

    subroutine append_environment_value(environment, name)
        character(len=*), intent(inout) :: environment
        character(len=*), intent(in) :: name
        character(len=512) :: value
        integer :: length, status

        value = ''
        call get_environment_variable(name, value, length, status)
        if (status /= 0 .or. length <= 0) return
        if (len_trim(environment) > 0) environment = trim(environment)//';'
        environment = trim(environment)//trim(name)//'='//value(:min(length, len(value)))
    end subroutine append_environment_value

    subroutine append_path_dependency(project_dir, dep_path, dep_name, inputs, &
            n_inputs, ierr, message)
        character(len=*), intent(in) :: project_dir, dep_path, dep_name
        type(generation_input_t), intent(inout) :: inputs(:)
        integer, intent(inout) :: n_inputs
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        integer :: i

        ierr = 0
        message = ''
        if (len_trim(dep_path) == 0) return
        if (dep_path(1:1) == '/') then
            ierr = 1
            message = 'absolute fpm path dependencies cannot be frozen safely'
            return
        end if
        do i = 1, n_inputs
            if (trim(inputs(i)%destination) == trim(dep_path)) return
        end do
        if (n_inputs >= size(inputs)) then
            ierr = 1
            message = 'too many path dependencies to freeze'
            return
        end if
        n_inputs = n_inputs + 1
        inputs(n_inputs)%label = 'dependency:'//trim(dep_name)
        inputs(n_inputs)%source_root = trim(project_dir)//'/'//trim(dep_path)
        inputs(n_inputs)%destination = trim(dep_path)
    end subroutine append_path_dependency

    subroutine context_cancel_owned_process(pid, ierr)
        integer, intent(in) :: pid
        integer, intent(out) :: ierr
        integer :: attempt, exitcode
        logical :: done

        ierr = 0
        if (pid <= 0) return
        do attempt = 1, 3
            call process_cancel_pid(pid, ierr)
            if (ierr == 0) return
            call process_poll_pid(pid, done, exitcode)
            if (done) then
                ierr = 0
                return
            end if
            call fs_sleep_ms(50)
        end do
    end subroutine context_cancel_owned_process

    subroutine context_process_wait_bounded(pid, timeout_seconds, exitcode)
        integer, intent(in) :: pid, timeout_seconds
        integer, intent(out) :: exitcode
        integer :: cancel_status
        logical :: done
        real :: started

        call context_clock_seconds(started)
        do
            call process_poll_pid(pid, done, exitcode)
            if (done) return
            if (context_elapsed_seconds(started) >= real(timeout_seconds)) then
                call context_cancel_owned_process(pid, cancel_status)
                if (cancel_status == 0) then
                    exitcode = 124
                else
                    exitcode = 125
                end if
                return
            end if
            call fs_sleep_ms(50)
        end do
    end subroutine context_process_wait_bounded

    subroutine context_clock_seconds(seconds)
        real, intent(out) :: seconds
        integer(int64) :: count, rate

        call system_clock(count=count, count_rate=rate)
        seconds = 0.0
        if (rate > 0_int64) seconds = real(count)/real(rate)
    end subroutine context_clock_seconds

    real function context_elapsed_seconds(started)
        real, intent(in) :: started
        real :: now

        call context_clock_seconds(now)
        context_elapsed_seconds = max(0.0, now - started)
    end function context_elapsed_seconds

    function context_text_digest(text) result(digest)
        character(len=*), intent(in) :: text
        character(len=HASH_LEN) :: digest
        character(len=:), allocatable :: parts(:)

        allocate (character(len=max(1, len(text))) :: parts(1))
        parts(1) = text
        digest = cache_digest(parts, 1)
    end function context_text_digest

    subroutine read_first_line(path, line)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: line
        integer :: unit, ios

        line = ''
        open (newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) line
        close (unit)
        if (ios /= 0) line = ''
    end subroutine read_first_line
end module fo_gremlin_context
