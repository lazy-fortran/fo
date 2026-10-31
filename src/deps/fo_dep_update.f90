module fo_dep_update
    !! Refresh git and registry dependencies.
    !!
    !! fo acquires those dependencies once and then reuses the
    !! compiled artifacts. Nothing in that path ever notices that a dependency
    !! tracking a branch has moved: the clone under `build/dependencies` and the
    !! objects under the fpm profile directories stay at whatever revision was
    !! fetched first. A project can then run for months against a stale
    !! dependency and see bugs that were fixed upstream long ago - which is
    !! exactly how fo kept running an old front end that could not parse
    !! constructs its own sources use.
    !!
    !! `fo update` drops both the clone and the compiled artifacts so the next
    !! build re-fetches and recompiles. `dep_update_missing_sources` reports
    !! declared git dependencies whose source tree is gone while their objects
    !! remain, which is the silent case the build must never accept.
    !!
    !! CMake FetchContent dependencies have the same disease: CMake populates
    !! `build/_deps/<name>-src` once and never re-fetches it, even when the
    !! declared tag moves, so a project keeps compiling against a dependency
    !! from the day its build tree was first configured. `fo update` drops the
    !! FetchContent source, subbuild and build directories of every dependency
    !! the build tree manages itself; a `FETCHCONTENT_SOURCE_DIR_<NAME>`
    !! override pointing outside the build tree is a deliberate pin and stays.
    use fo_fpm_config, only: fpm_config_t, fpm_dep_t, fpm_config_parse, &
        fpm_config_allocate, &
        dep_kind, DEP_PATH, DEP_GIT, DEP_REGISTRY
    use fo_registry, only: registry_resolve
    use fo_fs, only: fs_remove_tree, fs_remove_file, fs_stat, fs_make_dir, &
        fs_rename, fs_write_text, &
        fs_collect_git_checkouts, fs_mkdir_excl, fs_sleep_ms
    use fo_dep_resolve, only: normalize_path, join_path
    use fo_process, only: process_run_argv_logged, argv_push
    use fo_util, only: make_tmpfile, make_sibling_tmpfile, delete_tmpfile, &
        read_text_file, read_text_file_alloc
    use, intrinsic :: iso_c_binding, only: c_long_long
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    public :: dep_update_run
    public :: dep_update_cmake_fetchcontent
    public :: dep_update_missing_sources
    public :: dep_acquire_sources
    public :: MAX_UPDATE_NAMES

    integer, parameter :: MAX_UPDATE_NAMES = 64
    integer, parameter :: MAX_PROFILE_DIRS = 512
    integer, parameter :: MAX_CHECKOUTS = 512

contains

    subroutine dep_acquire_sources(project_dir, ierr)
        !! Acquire missing Git sources and validate local registry closure natively.
        !! Root test dependencies are included; dependencies' own dev edges
        !! are intentionally excluded from the consumer closure.
        character(len=*), intent(in) :: project_dir
        integer, intent(out) :: ierr

        character(len=512) :: root
        character(len=512) :: visited(MAX_CHECKOUTS)
        integer :: n_visited

        call normalize_path(project_dir, root)
        visited = ''
        n_visited = 0
        ierr = 0
        call acquire_package(root, root, .true., visited, n_visited, 0, ierr)
    end subroutine dep_acquire_sources

    recursive subroutine acquire_package(package_dir, root, include_dev, &
            visited, n_visited, depth, ierr)
        character(len=*), intent(in) :: package_dir, root
        logical, intent(in) :: include_dev
        character(len=*), intent(inout) :: visited(:)
        integer, intent(inout) :: n_visited
        integer, intent(in) :: depth
        integer, intent(out) :: ierr

        type(fpm_config_t), allocatable :: config
        character(len=512) :: package_key
        integer :: i, parse_ierr

        ierr = 0
        if (depth > 64 .or. n_visited >= size(visited)) then
            write (error_unit, '(a)') &
                'fo: dependency closure exceeds supported depth or size'
            ierr = 1
            return
        end if
        do i = 1, n_visited
            if (trim(visited(i)) == trim(package_dir)) return
        end do
        n_visited = n_visited + 1
        visited(n_visited) = trim(package_dir)

        call fpm_config_allocate(config)
        call fpm_config_parse(package_dir, config, parse_ierr)
        if (parse_ierr /= 0) then
            ierr = parse_ierr
            return
        end if
        package_key = trim(config%name)//'@'//trim(config%version)
        do i = 1, config%n_deps
            call acquire_edge(config%deps(i), package_dir, root, package_key, visited, &
                n_visited, depth, ierr)
            if (ierr /= 0) return
        end do
        if (.not. include_dev) return
        do i = 1, config%n_dev_deps
            if (any(config%deps(:config%n_deps)%name == &
                    config%dev_deps(i)%name)) cycle
            call acquire_edge(config%dev_deps(i), package_dir, root, package_key, &
                visited, n_visited, depth, ierr)
            if (ierr /= 0) return
        end do
    end subroutine acquire_package

    recursive subroutine acquire_edge(dep, package_dir, root, package_key, &
            visited, n_visited, depth, ierr)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: package_dir, root, package_key
        character(len=*), intent(inout) :: visited(:)
        integer, intent(inout) :: n_visited
        integer, intent(in) :: depth
        integer, intent(out) :: ierr

        character(len=512) :: dep_dir, root_key
        type(fpm_config_t), allocatable :: root_config
        integer :: i

        ierr = 0
        if (trim(package_dir) /= trim(root)) then
            call fpm_config_allocate(root_config)
            call fpm_config_parse(root, root_config, ierr)
            if (ierr /= 0) return
            do i = 1, root_config%n_deps
                if (trim(root_config%deps(i)%name) /= trim(dep%name)) cycle
                root_key = trim(root_config%name)//'@'//trim(root_config%version)
                call acquire_edge(root_config%deps(i), root, root, root_key, &
                    visited, n_visited, depth, ierr)
                return
            end do
        end if
        select case (dep_kind(dep))
        case (DEP_PATH)
            call join_path(package_dir, trim(dep%path), dep_dir)
            call acquire_package(trim(dep_dir), root, .false., visited, &
                n_visited, depth + 1, ierr)
        case (DEP_GIT)
            if (.not. safe_dependency_name(trim(dep%name))) then
                write (error_unit, '(a)') 'fo: unsafe Git dependency name: '// &
                    trim(dep%name)
                ierr = 1
                return
            end if
            dep_dir = trim(root)//'/build/dependencies/'//trim(dep%name)
            call acquire_git_checkout(dep, package_dir, package_key, dep_dir, ierr)
            if (ierr /= 0) return
            call acquire_package(trim(dep_dir), root, .false., visited, &
                n_visited, depth + 1, ierr)
        case (DEP_REGISTRY)
            call registry_resolve(dep, root, dep_dir, ierr)
            if (ierr /= 0) return
            call acquire_package(trim(dep_dir), root, .false., visited, &
                n_visited, depth + 1, ierr)
        end select
    end subroutine acquire_edge

    subroutine acquire_git_checkout(dep, package_dir, package_key, destination, ierr)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: package_dir, package_key, destination
        integer, intent(out) :: ierr

        character(len=512) :: lock_path, dependencies_dir
        integer(c_long_long) :: mtime, bytes
        integer :: lock_state, attempt
        logical :: current, lock_present

        call git_checkout_current(dep, package_key, destination, current)
        if (current) then
            ierr = 0
            return
        end if
        dependencies_dir = trim(destination(:index(trim(destination), '/', &
            back=.true.) - 1))
        call fs_make_dir(trim(dependencies_dir))
        lock_path = trim(destination)//'.fo-acquire-lock'
        lock_state = fs_mkdir_excl(trim(lock_path))
        if (lock_state == 1) then
            do attempt = 1, 3050
                call git_checkout_current(dep, package_key, destination, current)
                if (current) then
                    ierr = 0
                    return
                end if
                call fs_stat(trim(lock_path), mtime, bytes, lock_present)
                if (.not. lock_present) then
                    lock_state = fs_mkdir_excl(trim(lock_path))
                    if (lock_state == 0) exit
                end if
                call fs_sleep_ms(100)
            end do
        end if
        if (lock_state /= 0) then
            write (error_unit, '(a)') 'fo: timed out waiting for Git dependency '// &
                'acquisition lock at '//trim(lock_path)
            ierr = 1
            return
        end if
        call acquire_git_checkout_locked(dep, package_dir, package_key, destination, &
            ierr)
        call fs_remove_tree(trim(lock_path))
    end subroutine acquire_git_checkout

    subroutine git_checkout_current(dep, package_key, destination, current)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: package_key, destination
        logical, intent(out) :: current

        character(len=512) :: identity_path
        character(len=4096) :: selector, identity
        integer(c_long_long) :: mtime, bytes
        logical :: present, have_manifest, have_identity

        current = .false.
        call fs_stat(trim(destination), mtime, bytes, present)
        if (.not. present) return
        inquire (file=trim(destination)//'/fpm.toml', exist=have_manifest)
        if (.not. have_manifest) return
        identity_path = trim(destination(:index(trim(destination), '/', &
            back=.true.) - 1))// '/.fo-git-identities/'//trim(dep%name)
        inquire (file=trim(identity_path), exist=have_identity)
        if (.not. have_identity) return
        call read_text_file(trim(identity_path), identity)
        call git_selector(dep, package_key, selector)
        current = git_identity_matches(selector, identity)
    end subroutine git_checkout_current

    subroutine acquire_git_checkout_locked(dep, package_dir, package_key, &
            destination, ierr)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: package_dir, package_key, destination
        integer, intent(out) :: ierr

        character(len=512) :: stage, log_file, ref, backup
        character(len=512) :: dependencies_dir, identity_dir, identity_path
        character(len=512) :: source
        character(len=4096) :: output
        character(len=4096) :: selector, previous
        character(len=:), allocatable :: args
        integer(c_long_long) :: mtime, bytes
        integer :: n_args, exitcode, rename_rc, nl
        logical :: present, have_manifest, have_marker, valid

        ierr = 0
        dependencies_dir = trim(destination(:index(trim(destination), '/', &
            back=.true.) - 1))
        identity_dir = trim(dependencies_dir)//'/.fo-git-identities'
        identity_path = trim(identity_dir)//'/'//trim(dep%name)
        call fs_stat(trim(destination), mtime, bytes, present)
        if (present) then
            inquire (file=trim(destination)//'/fpm.toml', exist=have_manifest)
            if (have_manifest) then
                call git_selector(dep, package_key, selector)
                inquire (file=trim(identity_path), exist=have_marker)
                if (have_marker) then
                    call read_text_file(trim(identity_path), previous)
                    if (git_identity_matches(selector, previous)) return
                else
                    call validate_git_checkout(dep, package_dir, destination, &
                        output, valid)
                    if (.not. valid) then
                        write (error_unit, '(a)') 'fo: existing Git dependency '// &
                            trim(dep%name)//' does not match its declared origin, '// &
                            'revision or clean tracked contents; run fo update'
                        ierr = 1
                        return
                    end if
                    call fs_make_dir(trim(identity_dir))
                    call fs_write_text(trim(identity_path), &
                        trim(selector)//new_line('a')//trim(output))
                    call read_text_file(trim(identity_path), previous)
                    if (.not. git_identity_matches(selector, previous)) then
                        write (error_unit, '(a)') 'fo: cannot record Git identity for '// &
                            trim(dep%name)
                        ierr = 1
                    end if
                    return
                end if
            else
                write (error_unit, '(a)') &
                    'fo: incomplete Git dependency source at '//trim(destination)// &
                    '; remove it or run fo update'
                ierr = 1
                return
            end if
        end if
        call fs_make_dir(trim(dependencies_dir))
        call fs_make_dir(trim(identity_dir))
        call make_sibling_tmpfile(trim(destination), stage)
        call make_tmpfile('fo-git-log', log_file)
        n_args = 0
        call argv_push(args, n_args, 'git')
        call argv_push(args, n_args, 'clone')
        call argv_push(args, n_args, '--no-checkout')
        call argv_push(args, n_args, '--')
        call git_source(dep, package_dir, source)
        call argv_push(args, n_args, trim(source))
        call argv_push(args, n_args, trim(stage))
        call process_run_argv_logged('', args, n_args, trim(log_file), .false., &
            300, exitcode)
        if (exitcode /= 0) then
            call read_text_file(trim(log_file), output)
            write (error_unit, '(a)') 'fo: Git clone failed for dependency '// &
                trim(dep%name)//': '//trim(output)
            call fs_remove_tree(trim(stage))
            call delete_tmpfile(trim(log_file))
            ierr = exitcode
            return
        end if

        ref = 'HEAD'
        if (len_trim(dep%branch) > 0) ref = 'refs/remotes/origin/'//trim(dep%branch)
        if (len_trim(dep%tag) > 0) ref = 'refs/tags/'//trim(dep%tag)//'^{commit}'
        if (allocated(dep%rev)) then
            if (len_trim(dep%rev) > 0) ref = trim(dep%rev)
        end if
        args = ''
        n_args = 0
        call argv_push(args, n_args, 'git')
        call argv_push(args, n_args, '-C')
        call argv_push(args, n_args, trim(stage))
        call argv_push(args, n_args, 'checkout')
        call argv_push(args, n_args, '--detach')
        call argv_push(args, n_args, trim(ref))
        call process_run_argv_logged('', args, n_args, trim(log_file), .false., &
            60, exitcode)
        if (exitcode /= 0) then
            call read_text_file(trim(log_file), output)
            write (error_unit, '(a)') 'fo: cannot select Git ref '//trim(ref)// &
                ' for dependency '//trim(dep%name)//': '//trim(output)
            call fs_remove_tree(trim(stage))
            call delete_tmpfile(trim(log_file))
            ierr = exitcode
            return
        end if
        args = ''
        n_args = 0
        call argv_push(args, n_args, 'git')
        call argv_push(args, n_args, '-C')
        call argv_push(args, n_args, trim(stage))
        call argv_push(args, n_args, 'rev-parse')
        call argv_push(args, n_args, '--verify')
        call argv_push(args, n_args, 'HEAD^{commit}')
        call process_run_argv_logged('', args, n_args, trim(log_file), .false., &
            30, exitcode)
        if (exitcode /= 0) then
            call read_text_file(trim(log_file), output)
            write (error_unit, '(a)') 'fo: cannot resolve Git commit for '// &
                trim(dep%name)//': '//trim(output)
            call fs_remove_tree(trim(stage))
            call delete_tmpfile(trim(log_file))
            ierr = exitcode
            return
        end if
        call read_text_file(trim(log_file), output)
        nl = index(trim(output), new_line('a'))
        if (nl > 0) output = output(:nl - 1)
        call git_selector(dep, package_key, selector)
        if (present) then
            call make_sibling_tmpfile(trim(destination), backup)
            rename_rc = fs_rename(trim(destination), trim(backup))
            if (rename_rc /= 0) then
                call fs_remove_tree(trim(stage))
                call delete_tmpfile(trim(log_file))
                write (error_unit, '(a)') 'fo: cannot stage replacement Git '// &
                    'dependency '//trim(dep%name)
                ierr = 1
                return
            end if
        end if
        rename_rc = fs_rename(trim(stage), trim(destination))
        call delete_tmpfile(trim(log_file))
        if (rename_rc /= 0) then
            call fs_remove_tree(trim(stage))
            if (present) then
                rename_rc = fs_rename(trim(backup), trim(destination))
                if (rename_rc /= 0) then
                    write (error_unit, '(a)') 'fo: cannot restore prior Git '// &
                        'dependency; preserved backup at '//trim(backup)
                end if
            end if
            write (error_unit, '(a)') 'fo: cannot publish Git dependency '// &
                trim(dep%name)//' at '//trim(destination)
            ierr = 1
            return
        end if
        call fs_write_text(trim(identity_path), &
            trim(selector)//new_line('a')//trim(output))
        inquire (file=trim(identity_path), exist=have_marker)
        if (have_marker) call read_text_file(trim(identity_path), previous)
        if (have_marker) have_marker = git_identity_matches(selector, previous)
        if (.not. have_marker) then
            call fs_remove_tree(trim(destination))
            if (present) then
                rename_rc = fs_rename(trim(backup), trim(destination))
                if (rename_rc /= 0) then
                    write (error_unit, '(a)') 'fo: cannot restore prior Git '// &
                        'dependency; preserved backup at '//trim(backup)
                end if
            end if
            write (error_unit, '(a)') 'fo: cannot record Git identity for '// &
                trim(dep%name)
            ierr = 1
            return
        end if
        if (present) call fs_remove_tree(trim(backup))
    end subroutine acquire_git_checkout_locked

    subroutine git_source(dep, package_dir, source)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: package_dir
        character(len=*), intent(out) :: source
        character(len=512) :: local_source
        integer :: colon, slash, at_sign

        source = trim(dep%git)
        if (index(trim(source), 'file://') == 1) then
            local_source = source(8:)
            if (len_trim(local_source) > 0) then
                if (local_source(1:1) /= '/') then
                    call join_path(package_dir, trim(local_source), source)
                else
                    source = trim(local_source)
                end if
            end if
        else if (index(trim(source), '://') == 0) then
            colon = index(trim(source), ':')
            slash = index(trim(source), '/')
            at_sign = index(trim(source), '@')
            if (at_sign > 0 .and. colon > 0 .and. &
                (slash == 0 .or. colon < slash)) then
                ! Preserve SCP-style remote URLs such as git@host:repo.
            else if (source(1:1) /= '/') then
                call join_path(package_dir, trim(source), local_source)
                source = trim(local_source)
            end if
        end if
    end subroutine git_source

    subroutine validate_git_checkout(dep, package_dir, destination, commit, valid)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: package_dir, destination
        character(len=*), intent(out) :: commit
        logical, intent(out) :: valid

        character(len=512) :: source, ref
        character(len=4096) :: origin, selected, changes
        integer :: status
        logical :: have_source

        valid = .false.
        commit = ''
        call local_git_text(destination, &
            [character(len=64) :: 'config', '--get', 'remote.origin.url'], &
            origin, status)
        if (status == 1) then
            call fetched_checkout_matches(dep, destination, have_source)
            if (.not. have_source) return
        else if (status == 0) then
            call git_source(dep, package_dir, source)
            if (trim(origin) /= trim(dep%git) .and. &
                trim(origin) /= trim(source)) return
        else
            return
        end if
        ref = 'refs/remotes/origin/HEAD'
        if (len_trim(dep%branch) > 0) ref = 'refs/remotes/origin/'//trim(dep%branch)
        if (len_trim(dep%tag) > 0) ref = 'refs/tags/'//trim(dep%tag)
        if (allocated(dep%rev)) then
            if (len_trim(dep%rev) > 0) ref = trim(dep%rev)
        end if
        call local_git_text(destination, [character(len=1024) :: 'rev-parse', &
            '--verify', '--end-of-options', trim(ref)//'^{commit}'], selected, status)
        if (status /= 0 .and. len_trim(dep%branch) > 0) then
            if (trim(ref) == 'refs/remotes/origin/'//trim(dep%branch)) then
                call local_git_text(destination, [character(len=1024) :: &
                    'rev-parse', '--verify', '--end-of-options', &
                    'refs/heads/'//trim(dep%branch)//'^{commit}'], selected, status)
            end if
        end if
        if (status /= 0) return
        call local_git_text(destination, [character(len=64) :: 'rev-parse', &
            '--verify', 'HEAD^{commit}'], commit, status)
        if (status /= 0) return
        if (trim(commit) /= trim(selected)) return
        ! Compare both index and worktree with HEAD before recording provenance.
        call local_git_text(destination, [character(len=64) :: 'diff', &
            '--quiet', 'HEAD', '--'], changes, status)
        valid = status == 0
    end subroutine validate_git_checkout

    subroutine fetched_checkout_matches(dep, destination, valid)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: destination
        logical, intent(out) :: valid

        character(len=4096) :: fetched, expected, source
        character(len=:), allocatable :: prefix
        integer :: status, split, nl

        valid = .false.
        if (.not. allocated(dep%rev)) return
        if (len_trim(dep%rev) /= 40 .and. len_trim(dep%rev) /= 64) return
        if (verify(trim(dep%rev), '0123456789abcdefABCDEF') /= 0) return
        ! fpm fetches a pinned object directly, without creating an origin remote.
        call read_text_file(trim(destination)//'/.git/FETCH_HEAD', fetched)
        nl = index(fetched, new_line('a'))
        if (nl /= len_trim(fetched) .or. nl == 0) return
        fetched = fetched(:nl - 1)
        prefix = trim(dep%rev)//achar(9)//achar(9)//"'"//trim(dep%rev)//"' of "
        split = len(prefix)
        if (fetched(:split) /= prefix(:split)) return
        source = fetched(split + 1:)
        if (trim(source) == fetch_display_url(trim(dep%git))) then
            valid = .true.
            return
        end if
        ! Git expands configured mirror URLs locally; --get-url never fetches.
        call local_git_text(destination, [character(len=1024) :: 'ls-remote', &
            '--get-url', '--', trim(dep%git)], expected, status)
        if (status /= 0) return
        valid = trim(source) == fetch_display_url(trim(expected))
    end subroutine fetched_checkout_matches

    function fetch_display_url(url) result(display)
        character(len=*), intent(in) :: url
        character(len=:), allocatable :: display
        integer :: n

        display = url
        n = len_trim(display)
        if (n < 4) return
        if (display(n - 3:n) == '.git') display = display(:n - 4)
    end function fetch_display_url

    subroutine local_git_text(destination, words, output, status)
        character(len=*), intent(in) :: destination, words(:)
        character(len=*), intent(out) :: output
        integer, intent(out) :: status

        character(len=:), allocatable :: args
        character(len=512) :: log_file
        integer :: n_args, i, nl

        n_args = 0
        call argv_push(args, n_args, 'git')
        call argv_push(args, n_args, '-C')
        call argv_push(args, n_args, destination)
        call argv_push(args, n_args, '--git-dir=.git')
        call argv_push(args, n_args, '--work-tree=.')
        do i = 1, size(words)
            call argv_push(args, n_args, trim(words(i)))
        end do
        call make_tmpfile('fo-git-check', log_file)
        call process_run_argv_logged('', args, n_args, trim(log_file), .false., &
            30, status)
        call read_text_file(trim(log_file), output)
        call delete_tmpfile(trim(log_file))
        nl = index(output, new_line('a'))
        if (nl > 0) output = output(:nl - 1)
    end subroutine local_git_text

    subroutine git_selector(dep, package_key, selector)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: package_key
        character(len=*), intent(out) :: selector

        selector = 'declared-by='//trim(package_key)//new_line('a')// &
            'git='//trim(dep%git)//new_line('a')// &
            'branch='//trim(dep%branch)//new_line('a')// &
            'tag='//trim(dep%tag)//new_line('a')//'rev='
        if (allocated(dep%rev)) selector = trim(selector)//trim(dep%rev)
    end subroutine git_selector

    logical function git_identity_matches(selector, identity)
        character(len=*), intent(in) :: selector, identity
        character(len=64) :: commit
        integer :: start, n, i, code

        git_identity_matches = .false.
        if (index(trim(identity), trim(selector)//new_line('a')) /= 1) return
        start = len_trim(selector) + 2
        if (start > len_trim(identity)) return
        commit = identity(start:)
        n = len_trim(commit)
        if (n > 0) then
            if (commit(n:n) == new_line('a')) n = n - 1
        end if
        if (n /= 40 .and. n /= 64) return
        do i = 1, n
            code = iachar(commit(i:i))
            if (code >= iachar('0') .and. code <= iachar('9')) cycle
            if (code >= iachar('a') .and. code <= iachar('f')) cycle
            if (code >= iachar('A') .and. code <= iachar('F')) cycle
            return
        end do
        git_identity_matches = .true.
    end function git_identity_matches

    logical function safe_dependency_name(name)
        character(len=*), intent(in) :: name
        integer :: i

        safe_dependency_name = len_trim(name) > 0
        if (trim(name) == '.' .or. trim(name) == '..') then
            safe_dependency_name = .false.
            return
        end if
        do i = 1, len_trim(name)
            if (name(i:i) /= '/' .and. name(i:i) /= achar(92)) cycle
            safe_dependency_name = .false.
            return
        end do
    end function safe_dependency_name

    subroutine dep_update_missing_sources(project_dir, names, n_names)
        !! Names of declared git/registry dependencies with no source tree under
        !! `build/dependencies`.
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(out) :: names(:)
        integer, intent(out) :: n_names

        type(fpm_config_t), allocatable :: config
        character(len=512) :: root, dep_dir
        integer(c_long_long) :: mtime, bytes
        integer :: i, ierr
        logical :: ok

        n_names = 0
        call fpm_config_allocate(config)
        call normalize_path(project_dir, root)
        call fpm_config_parse(root, config, ierr)
        if (ierr /= 0) return
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) cycle
            dep_dir = trim(root)//'/build/dependencies/'// &
                trim(config%deps(i)%name)
            call fs_stat(trim(dep_dir), mtime, bytes, ok)
            if (ok) cycle
            if (n_names >= size(names)) return
            n_names = n_names + 1
            names(n_names) = trim(config%deps(i)%name)
        end do
    end subroutine dep_update_missing_sources

    subroutine dep_update_cmake_fetchcontent(project_dir, n_cmake, refreshed, &
            error_message)
        !! Drop the FetchContent dependency trees the project build tree
        !! manages itself so the next CMake configure re-fetches them at the
        !! declared refs. Sources pinned by a FETCHCONTENT_SOURCE_DIR_<NAME>
        !! override outside the build tree are deliberate and stay.
        character(len=*), intent(in) :: project_dir
        integer, intent(out) :: n_cmake
        logical, intent(inout) :: refreshed
        character(len=*), intent(out), optional :: error_message

        character(len=:), allocatable :: cache_text
        character(len=1024) :: build_dir, line, key, kind, value, base, dir, stem
        character(len=64) :: names(MAX_UPDATE_NAMES)
        integer :: cursor, n_names, i, ierr, k
        logical :: exists, listed

        n_cmake = 0
        if (present(error_message)) error_message = ''
        build_dir = trim(project_dir)//'/build'
        call get_environment_variable('FO_CMAKE_BUILD_DIR', value=value)
        if (len_trim(value) > 0) build_dir = trim(value)
        inquire(file=trim(build_dir)//'/CMakeCache.txt', exist=exists)
        if (.not. exists) return

        call read_text_file_alloc(trim(build_dir)//'/CMakeCache.txt', &
            cache_text, ierr)
        if (ierr /= 0) return
        if (len_trim(cache_text) == 0) return
        base = ''
        n_names = 0
        cursor = 1
        do while (update_cache_line(cache_text, cursor, line))
            call update_cache_entry(line, key, kind, value)
            if (trim(key) == 'FETCHCONTENT_BASE_DIR') base = trim(value)
            if (index(key, 'FETCHCONTENT_SOURCE_DIR_') == 1 .and. len(key) > 24) then
                if (n_names < MAX_UPDATE_NAMES) then
                    n_names = n_names + 1
                    names(n_names) = key(25:min(len(key), 24 + 64))
                end if
            end if
        end do
        if (n_names == 0) return
        if (len_trim(base) == 0) base = trim(build_dir)//'/_deps'

        listed = .false.
        do i = 1, n_names
            ! Resolve the source dir the same way the hook does: the cache
            ! value when set, otherwise the conventional _deps location.
            dir = ''
            cursor = 1
            do while (update_cache_line(cache_text, cursor, line))
                call update_cache_entry(line, key, kind, value)
                if (trim(key) == 'FETCHCONTENT_SOURCE_DIR_'//trim(names(i))) then
                    dir = trim(value)
                    exit
                end if
            end do
            stem = trim(base)//'/'//update_lower(trim(names(i)))
            if (len_trim(dir) == 0) dir = trim(stem)//'-src'
            ! Outside the FetchContent base the source is a deliberate pin.
            if (index(trim(dir), trim(stem)) /= 1) cycle
            inquire(file=trim(dir), exist=exists)
            if (exists) then
                call fs_remove_tree(trim(dir), ierr)
                if (ierr /= 0) then
                    if (present(error_message)) error_message = &
                        'cannot remove FetchContent source '//trim(dir)
                    return
                end if
                listed = .true.
            end if
            do k = 1, 3
                select case (k)
                case (1)
                    dir = trim(stem)//'-subbuild'
                case (2)
                    dir = trim(stem)//'-build'
                case default
                    dir = trim(stem)//'-populate'
                end select
                inquire(file=trim(dir), exist=exists)
                if (.not. exists) cycle
                call fs_remove_tree(trim(dir), ierr)
                if (ierr /= 0) then
                    if (present(error_message)) error_message = &
                        'cannot remove FetchContent artifact '//trim(dir)
                    return
                end if
                listed = .true.
            end do
            n_cmake = n_cmake + 1
        end do
        if (listed) refreshed = .true.
    end subroutine dep_update_cmake_fetchcontent

    logical function update_cache_line(cache, cursor, line) result(found)
        character(len=*), intent(in) :: cache
        integer, intent(inout) :: cursor
        character(len=*), intent(out) :: line
        integer :: nextn
        found = .false.
        line = ''
        if (cursor > len_trim(cache)) return
        nextn = index(cache(cursor:), new_line('a'))
        if (nextn == 0) then
            line = cache(cursor:)
            cursor = len_trim(cache) + 1
        else
            line = cache(cursor:cursor + nextn - 2)
            cursor = cursor + nextn
        end if
        found = .true.
    end function update_cache_line

    subroutine update_cache_entry(line, key, kind, value)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: key, kind, value
        integer :: colon, equals
        key = ''
        kind = ''
        value = ''
        colon = index(line, ':')
        equals = index(line, '=')
        if (equals <= colon .or. colon == 0) return
        key = line(:colon - 1)
        kind = line(colon + 1:equals - 1)
        value = line(equals + 1:)
    end subroutine update_cache_entry

    function update_lower(text) result(converted)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: converted
        integer :: i, code
        converted = text
        do i = 1, len_trim(converted)
            code = iachar(converted(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) &
                converted(i:i) = achar(code + 32)
        end do
    end function update_lower

    subroutine dep_update_run(project_dir, n_deps, refreshed, error_message)
        !! Remove the cached clones and the compiled dependency artifacts.
        !!
        !! The fpm profile directories are removed as well as the clone: leaving
        !! them behind lets the next build link objects compiled from the source
        !! that was just discarded. The CMake FetchContent trees are dropped
        !! first so a native build also re-fetches its dependencies.
        character(len=*), intent(in) :: project_dir
        integer, intent(out) :: n_deps
        logical, intent(out) :: refreshed
        character(len=*), intent(out), optional :: error_message

        type(fpm_config_t), allocatable :: config
        character(len=512) :: root
        integer :: i, ierr, n_fpm

        n_deps = 0
        refreshed = .false.
        if (present(error_message)) error_message = ''
        call normalize_path(project_dir, root)
        block
            integer :: n_cmake
            call dep_update_cmake_fetchcontent(root, n_cmake, refreshed, &
                error_message)
            if (present(error_message)) then
                if (len_trim(error_message) > 0) return
            end if
            n_deps = n_cmake
            n_fpm = 0
        end block
        call fpm_config_allocate(config)
        call fpm_config_parse(root, config, ierr)
        if (ierr /= 0) return
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) cycle
            n_fpm = n_fpm + 1
        end do
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) == DEP_PATH) cycle
            n_fpm = n_fpm + 1
        end do
        if (n_fpm == 0) return
        n_deps = n_deps + n_fpm

        block
            character(len=1024) :: problem
            call check_dependency_checkouts(trim(root)//'/build/dependencies', &
                problem)
            if (len_trim(problem) > 0) then
                if (present(error_message)) error_message = trim(problem)
                return
            end if
        end block

        call fs_remove_tree(trim(root)//'/build/dependencies')
        ! fpm records the acquired dependency tree in build/cache.toml. Leaving
        ! it behind after the clones are gone does not make fpm re-fetch: it
        ! makes fpm fail with "Error while retrieving commit information",
        ! because it looks for a checkout the cache still promises.
        call fs_remove_file(trim(root)//'/build/cache.toml')
        call remove_profile_trees(trim(root)//'/build')
        refreshed = .true.
    end subroutine dep_update_run

    subroutine check_dependency_checkouts(dependencies_dir, problem)
        !! Inspect every immediate git checkout before removing any artifact.
        !! This includes transitive dependencies not named in the root manifest.
        character(len=*), intent(in) :: dependencies_dir
        character(len=*), intent(out) :: problem

        character(len=1024) :: checkouts(MAX_CHECKOUTS), status_log
        character(len=:), allocatable :: args
        integer :: n_args, exitcode, n_checkouts, i
        integer(c_long_long) :: mtime, bytes
        logical :: present_file, listed

        problem = ''
        call fs_collect_git_checkouts(dependencies_dir, checkouts, &
            n_checkouts, listed)
        if (.not. listed) then
            problem = 'cannot inspect dependency checkouts: '// &
                trim(dependencies_dir)
            return
        end if
        do i = 1, n_checkouts
            call make_tmpfile('fo-update-status', status_log)
            args = ''
            n_args = 0
            call argv_push(args, n_args, 'git')
            call argv_push(args, n_args, '-C')
            call argv_push(args, n_args, trim(checkouts(i)))
            call argv_push(args, n_args, 'status')
            call argv_push(args, n_args, '--porcelain=v1')
            call argv_push(args, n_args, '--untracked-files=all')
            call argv_push(args, n_args, '--ignore-submodules=none')
            call process_run_argv_logged('', args, n_args, trim(status_log), &
                .false., 30, exitcode)
            call fs_stat(trim(status_log), mtime, bytes, present_file)
            call delete_tmpfile(trim(status_log))
            if (exitcode /= 0 .or. .not. present_file) then
                problem = 'cannot inspect dependency checkout: '// &
                    trim(checkouts(i))
                exit
            end if
            if (bytes > 0) then
                problem = 'dependency checkout has local changes: '// &
                    trim(checkouts(i))// &
                    '; commit or stash them, then rerun fo update'
                exit
            end if
        end do
    end subroutine check_dependency_checkouts

    subroutine remove_profile_trees(build_dir)
        !! Drop the fpm bootstrap profile trees that hold dependency objects.
        !! They are named `<compiler>_<hash>`, one per compiler and flag set,
        !! and are found through the module directories they contain.
        use fo_fs, only: fs_collect_mod_dirs
        character(len=*), intent(in) :: build_dir

        character(len=512) :: dirs(MAX_PROFILE_DIRS), profile
        integer :: i, n_dirs

        call fs_collect_mod_dirs(build_dir, dirs, n_dirs)
        do i = 1, n_dirs
            call first_component(trim(build_dir), dirs(i), profile)
            if (.not. is_profile_name(profile)) cycle
            call fs_remove_tree(trim(build_dir)//'/'//trim(profile))
        end do
    end subroutine remove_profile_trees

    subroutine first_component(build_dir, path, component)
        !! The first path component of `path` below `build_dir`.
        character(len=*), intent(in) :: build_dir, path
        character(len=*), intent(out) :: component
        character(len=512) :: rest
        integer :: cut

        component = ''
        if (index(trim(path), trim(build_dir)//'/') /= 1) return
        rest = trim(path(len_trim(build_dir) + 2:))
        cut = index(trim(rest), '/')
        if (cut > 1) then
            component = rest(:cut - 1)
        else if (cut == 0) then
            component = trim(rest)
        end if
    end subroutine first_component

    logical function is_profile_name(name) result(matches)
        !! `gfortran_1E049C6C`, `nvfortran_0738C102`, `ifx_...`: a compiler name,
        !! an underscore, and an uppercase hexadecimal profile hash.
        character(len=*), intent(in) :: name
        integer :: cut, i
        character(len=1) :: c

        matches = .false.
        cut = index(trim(name), '_', back=.true.)
        if (cut < 2 .or. cut >= len_trim(name)) return
        do i = cut + 1, len_trim(name)
            c = name(i:i)
            if (.not. ((c >= '0' .and. c <= '9') .or. &
                (c >= 'A' .and. c <= 'F'))) return
        end do
        matches = len_trim(name) - cut >= 8
    end function is_profile_name

end module fo_dep_update
