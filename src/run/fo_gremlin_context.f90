module fo_gremlin_context
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: iso_c_binding, only: c_long_long
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_fpm_config, only: DEP_PATH, DEP_REGISTRY, fpm_config_t, fpm_config_parse, dep_kind
    use fo_dep_update, only: dep_acquire_sources
    use fo_dep_resolve, only: normalize_path, resolve_dev_dep_srcs, &
        resolved_src_t, resolve_dep_srcs, MAX_RESOLVED
    use fo_gremlin_generation, only: generation_context_t, generation_input_t, &
        generation_t, generation_capture, generation_load_inventory
    use fo_driver, only: driver_pin_t
    use fo_input_inventory, only: input_declaration_t, input_inventory_t, &
        input_inventory_discover, input_inventory_declarations_from_config
    use fo_gremlin_state, only: gremlin_generation_register_lease_at, gremlin_lease_t
    use fo_change_watch, only: change_watch_t, change_watch_add_context, change_watch_add_root
    use fo_process, only: argv_push, process_cancel_pid, process_poll_pid, &
        process_start_argv_logged
    use fo_util, only: make_tmpfile, make_sibling_tmpfile, delete_tmpfile, read_text_file
    use fo_fs, only: fs_find_executable, fs_sleep_ms, fs_tree_fingerprint, fs_rename
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096

    public :: capture_candidate, generation_inventory_restore
    public :: common_generation_cas_root

contains

    subroutine capture_candidate(project_dir, driver_pin, generation, lease, ok, &
            registration_error, message, change_watch)
        character(len=*), intent(in) :: project_dir
        type(driver_pin_t), intent(in) :: driver_pin
        type(generation_t), intent(out) :: generation
        type(gremlin_lease_t), intent(out) :: lease
        logical, intent(out) :: ok
        integer, intent(out) :: registration_error
        character(len=*), intent(out) :: message
        type(change_watch_t), intent(inout), optional :: change_watch

        type(generation_context_t) :: context
        type(input_inventory_t) :: input_inventory
        type(input_declaration_t), allocatable :: declarations(:)
        character(len=PATH_LEN) :: cas_root
        character(len=PATH_LEN) :: watch_message
        integer :: ierr

        registration_error = 0
        call dep_acquire_sources(project_dir, ierr)
        if (ierr /= 0) then
            message = 'cannot resolve declared dependencies before capture'
            registration_error = ierr
            ok = .false.
            return
        end if
        call capture_context(project_dir, context, ierr, message)
        if (ierr /= 0) then
            ok = .false.
            return
        end if
        call input_inventory_declarations_from_config(project_dir, declarations, &
            ierr, message)
        if (ierr /= 0) then
            registration_error = ierr
            ok = .false.
            return
        end if
        call input_inventory_discover(project_dir, declarations, input_inventory, &
            ierr, message)
        if (ierr /= 0 .or. .not. input_inventory%valid) then
            registration_error = max(1, ierr)
            ok = .false.
            return
        end if
        context%input_inventory = input_inventory
        context%driver_path = driver_pin%path
        context%driver_digest = driver_pin%digest
        context%driver_size = driver_pin%size
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
        if (present(change_watch)) then
            call watch_registry_versions(change_watch, project_dir, ierr, watch_message)
            if (ierr /= 0) then
                ok = .false.
                registration_error = ierr
                message = trim(watch_message)
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
        call generation_inventory_restore(generation, ierr, watch_message)
        ! Inventory failure does not invalidate immutable source/build evidence.
        ! Consumers must inspect input_inventory_complete and its diagnostic.
        call gremlin_generation_register_lease_at(generation%root, lease, ierr, message)
        ok = ierr == 0
        if (ierr /= 0) registration_error = ierr
        if (.not. ok) return
        if (.not. present(change_watch)) return
        change_watch%capture_count = change_watch%capture_count + 1
        call observe_test_capture(change_watch%capture_count, generation%identity)
    end subroutine capture_candidate

    subroutine watch_registry_versions(watch, project_dir, ierr, message)
        type(change_watch_t), intent(inout) :: watch
        character(len=*), intent(in) :: project_dir
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        type(resolved_src_t), allocatable :: deps(:), devs(:)
        integer :: i, slash, n_deps, n_devs, n_unresolved
        character(len=PATH_LEN) :: root

        ierr = 0
        message = ''
        allocate (deps(MAX_RESOLVED), devs(MAX_RESOLVED))
        call resolve_dep_srcs(project_dir, deps, n_deps, n_unresolved, ierr)
        if (ierr /= 0) return
        call resolve_dev_dep_srcs(project_dir, devs, n_devs, ierr)
        if (ierr /= 0) return
        do i = 1, n_deps + n_devs
            if (i <= n_deps) then
                if (.not. deps(i)%registry_latest) cycle
                root = deps(i)%dir
            else
                if (.not. devs(i - n_deps)%registry_latest) cycle
                root = devs(i - n_deps)%dir
            end if
            slash = index(trim(root), '/', back=.true.)
            if (slash <= 1) cycle
            call change_watch_add_root(watch, root(:slash - 1), ierr, message)
            if (ierr /= 0) return
        end do
    end subroutine watch_registry_versions

    subroutine generation_inventory_restore(generation, ierr, message)
        !! Recovery restores only the canonical inventory captured in the
        !! immutable generation manifest; it never scans a mutable checkout.
        type(generation_t), intent(inout) :: generation
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        ierr = 0
        message = ''
        if (generation%input_inventory_ready) then
            message = trim(generation%input_inventory_diagnostic)
            if (generation%input_inventory%valid) then
                ierr = 0
            else
                ierr = 1
            end if
            return
        end if
        call generation_load_inventory(generation, ierr, message)
        if (.not. generation%input_inventory_ready) then
            generation%input_inventory_ready = .true.
            generation%input_inventory%valid = .false.
            generation%input_inventory_complete = .false.
            generation%input_inventory_diagnostic = trim(message)
            if (len_trim(message) == 0) then
                generation%input_inventory_diagnostic = &
                    'generation manifest did not contain a restorable input inventory'
            end if
            ierr = 1
            return
        end if
        generation%input_inventory_complete = generation%input_inventory%complete
        generation%input_inventory_diagnostic = &
            trim(generation%input_inventory%diagnostic)
        if (ierr /= 0 .or. .not. generation%input_inventory%valid) then
            if (len_trim(generation%input_inventory_diagnostic) == 0) then
                generation%input_inventory_diagnostic = trim(message)
            end if
            ierr = 1
            message = trim(generation%input_inventory_diagnostic)
            return
        end if
        ierr = 0
        message = trim(generation%input_inventory_diagnostic)
    end subroutine generation_inventory_restore

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
        type(resolved_src_t) :: resolved_dev_deps(MAX_RESOLVED)
        type(resolved_src_t) :: resolved_deps(MAX_RESOLVED)
        character(len=PATH_LEN) :: compiler_path, command_path
        character(len=:), allocatable :: packed
        character(len=PATH_LEN) :: log_file, output_line
        character(len=128) :: base_commit
        character(len=131072) :: git_status
        character(len=256) :: fingerprint
        integer(c_long_long) :: tree_sum, tree_mixed, tree_count
        type(generation_input_t), allocatable :: captured_inputs(:)
        integer :: i, n_inputs, n_args, n_resolved_dev, resolve_status
        integer :: n_resolved, n_unresolved
        integer :: exitcode, git_exit
        logical :: found, git_found, fingerprint_ok, exists

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
        call resolve_dev_dep_srcs(project_dir, resolved_dev_deps, &
            n_resolved_dev, resolve_status)
        if (resolve_status /= 0) n_resolved_dev = 0
        call resolve_dep_srcs(project_dir, resolved_deps, n_resolved, &
            n_unresolved, ierr)
        if (ierr /= 0) return
        n_inputs = count(resolved_deps(:n_resolved)%kind == DEP_REGISTRY)
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) n_inputs = n_inputs + 1
        end do
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) == DEP_PATH) n_inputs = n_inputs + 1
        end do
        do i = 1, n_resolved_dev
            if (dev_dependency_is_path(config, &
                    trim(resolved_dev_deps(i)%name))) cycle
            inquire(file=trim(resolved_dev_deps(i)%dir)//'/fpm.toml', exist=exists)
            if (exists) n_inputs = n_inputs + 1
        end do
        allocate (inputs(n_inputs))
        n_inputs = 0
        do i = 1, n_resolved
            if (resolved_deps(i)%kind /= DEP_REGISTRY) cycle
            n_inputs = n_inputs + 1
            inputs(n_inputs)%label = 'dependency:'//trim(resolved_deps(i)%name)
            inputs(n_inputs)%source_root = trim(resolved_deps(i)%dir)
            context%environment = context%environment//'registry:'// &
                trim(resolved_deps(i)%name)//'='//trim(resolved_deps(i)%dir)//';'
            inputs(n_inputs)%destination = 'build/dependencies/'// &
                trim(resolved_deps(i)%name)
        end do
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
        do i = 1, n_resolved_dev
            if (dev_dependency_is_path(config, &
                    trim(resolved_dev_deps(i)%name))) cycle
            inquire(file=trim(resolved_dev_deps(i)%dir)//'/fpm.toml', exist=exists)
            if (.not. exists) cycle
            if (n_inputs >= size(inputs)) then
                ierr = 1
                message = 'too many resolved development dependencies to freeze'
                return
            end if
            n_inputs = n_inputs + 1
            inputs(n_inputs)%label = 'dependency:'// &
                trim(resolved_dev_deps(i)%name)
            inputs(n_inputs)%source_root = trim(resolved_dev_deps(i)%dir)
            if (resolved_dev_deps(i)%kind == DEP_REGISTRY) &
                context%environment = context%environment//'registry-dev:'// &
                trim(resolved_dev_deps(i)%name)//'='//trim(resolved_dev_deps(i)%dir)//';'
            inputs(n_inputs)%destination = 'build/dependencies/'// &
                trim(resolved_dev_deps(i)%name)
        end do
        if (n_inputs < size(inputs)) then
            allocate (captured_inputs(n_inputs))
            if (n_inputs > 0) captured_inputs = inputs(:n_inputs)
            call move_alloc(captured_inputs, inputs)
        end if
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
            call delete_tmpfile(log_file)
        end if
        call append_environment_value(context%environment, 'FC')
        call append_environment_value(context%environment, 'FFLAGS')
        call append_environment_value(context%environment, 'FPM_FC')
        call append_environment_value(context%environment, 'FPM_FFLAGS')
        call append_environment_value(context%environment, 'FO_FPM_CONFIG_FILE')
        call append_environment_value(context%environment, 'LIBRARY_PATH')
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
        call delete_tmpfile(log_file)
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
        call delete_tmpfile(log_file)
    end subroutine capture_context

    logical function dev_dependency_is_path(config, dependency_name)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: dependency_name
        integer :: i

        dev_dependency_is_path = .false.
        do i = 1, config%n_dev_deps
            if (trim(config%dev_deps(i)%name) /= trim(dependency_name)) cycle
            dev_dependency_is_path = dep_kind(config%dev_deps(i)) == DEP_PATH
            return
        end do
    end function dev_dependency_is_path

    subroutine append_environment_value(environment, name)
        character(len=:), allocatable, intent(inout) :: environment
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
        logical :: exists

        ierr = 0
        message = ''
        if (len_trim(dep_path) == 0) return
        if (dep_path(1:1) == '/') then
            ierr = 1
            message = 'absolute fpm path dependencies cannot be frozen safely'
            return
        end if
        ! The project-tree manifest already freezes internal path dependencies.
        ! Keep them out of the separate-root list so capture never overlays a
        ! second copy onto their existing project-tree destination.
        if (path_dependency_is_internal(project_dir, dep_path)) then
            inquire (file=trim(project_dir)//'/'//trim(dep_path), exist=exists)
            if (exists) return
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

    logical function path_dependency_is_internal(project_dir, dep_path)
        character(len=*), intent(in) :: project_dir, dep_path

        character(len=PATH_LEN) :: normalized_project, normalized_dependency
        integer :: project_length, dependency_length

        path_dependency_is_internal = .false.
        if (len_trim(project_dir) + len_trim(dep_path) + 1 >= PATH_LEN) return
        call normalize_path(project_dir, normalized_project)
        call normalize_path(trim(project_dir)//'/'//trim(dep_path), &
            normalized_dependency)
        project_length = len_trim(normalized_project)
        dependency_length = len_trim(normalized_dependency)
        if (project_length == 0 .or. dependency_length == 0) return

        if (trim(normalized_project) == '.') then
            if (normalized_dependency(1:1) == '/') return
            if (trim(normalized_dependency) == '..') return
            if (dependency_length >= 3) then
                if (normalized_dependency(:3) == '../') return
            end if
            path_dependency_is_internal = .true.
            return
        end if

        if (normalized_project(:project_length) == '/') then
            path_dependency_is_internal = normalized_dependency(1:1) == '/'
            return
        end if
        if (dependency_length < project_length) return
        if (normalized_dependency(:project_length) /= &
            normalized_project(:project_length)) return
        if (dependency_length == project_length) then
            path_dependency_is_internal = .true.
            return
        end if
        path_dependency_is_internal = &
            normalized_dependency(project_length + 1:project_length + 1) == '/'
    end function path_dependency_is_internal

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
