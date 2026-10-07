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
    use fo_fpm_config, only: fpm_config_t, fpm_dep_t, fpm_config_parse, &
        dep_kind, DEP_PATH, DEP_GIT
    use fo_fs, only: fs_remove_tree, fs_remove_file, fs_stat, fs_make_dir, &
        fs_rename, &
        fs_collect_git_checkouts
    use fo_dep_resolve, only: normalize_path, join_path
    use fo_process, only: process_run_argv_logged, argv_push
    use fo_util, only: make_tmpfile, make_sibling_tmpfile, delete_tmpfile, &
        read_text_file
    use, intrinsic :: iso_c_binding, only: c_long_long
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    public :: dep_update_run
    public :: dep_update_missing_sources
    public :: dep_acquire_missing_git
    public :: MAX_UPDATE_NAMES

    integer, parameter :: MAX_UPDATE_NAMES = 64
    integer, parameter :: MAX_PROFILE_DIRS = 512
    integer, parameter :: MAX_CHECKOUTS = 512

contains

    subroutine dep_acquire_missing_git(project_dir, ierr)
        !! Acquire missing Git dependencies without invoking FPM.
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
    end subroutine dep_acquire_missing_git

    recursive subroutine acquire_package(package_dir, root, include_dev, &
            visited, n_visited, depth, ierr)
        character(len=*), intent(in) :: package_dir, root
        logical, intent(in) :: include_dev
        character(len=*), intent(inout) :: visited(:)
        integer, intent(inout) :: n_visited
        integer, intent(in) :: depth
        integer, intent(out) :: ierr

        type(fpm_config_t), allocatable :: config
        integer :: i, parse_ierr

        ierr = 0
        if (depth > 64 .or. n_visited >= size(visited)) then
            write (error_unit, '(a)') &
                'fo: Git dependency closure exceeds supported depth or size'
            ierr = 1
            return
        end if
        do i = 1, n_visited
            if (trim(visited(i)) == trim(package_dir)) return
        end do
        n_visited = n_visited + 1
        visited(n_visited) = trim(package_dir)

        allocate (config)
        call fpm_config_parse(package_dir, config, parse_ierr)
        if (parse_ierr /= 0) then
            ierr = parse_ierr
            return
        end if
        do i = 1, config%n_deps
            call acquire_edge(config%deps(i), package_dir, root, visited, &
                n_visited, depth, ierr)
            if (ierr /= 0) return
        end do
        if (.not. include_dev) return
        do i = 1, config%n_dev_deps
            call acquire_edge(config%dev_deps(i), package_dir, root, visited, &
                n_visited, depth, ierr)
            if (ierr /= 0) return
        end do
    end subroutine acquire_package

    recursive subroutine acquire_edge(dep, package_dir, root, visited, &
            n_visited, depth, ierr)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: package_dir, root
        character(len=*), intent(inout) :: visited(:)
        integer, intent(inout) :: n_visited
        integer, intent(in) :: depth
        integer, intent(out) :: ierr

        character(len=512) :: dep_dir

        ierr = 0
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
            call acquire_git_checkout(dep, package_dir, dep_dir, ierr)
            if (ierr /= 0) return
            call acquire_package(trim(dep_dir), root, .false., visited, &
                n_visited, depth + 1, ierr)
        end select
    end subroutine acquire_edge

    subroutine acquire_git_checkout(dep, package_dir, destination, ierr)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: package_dir, destination
        integer, intent(out) :: ierr

        character(len=512) :: stage, log_file, ref
        character(len=512) :: dependencies_dir
        character(len=512) :: source, local_source
        character(len=4096) :: output
        character(len=:), allocatable :: args
        integer(c_long_long) :: mtime, bytes
        integer :: n_args, exitcode, rename_rc, colon, slash, at_sign
        logical :: present, have_manifest

        ierr = 0
        call fs_stat(trim(destination), mtime, bytes, present)
        if (present) then
            inquire (file=trim(destination)//'/fpm.toml', exist=have_manifest)
            if (have_manifest) return
            write (error_unit, '(a)') &
                'fo: incomplete Git dependency source at '//trim(destination)// &
                '; remove it or run fo update'
            ierr = 1
            return
        end if
        dependencies_dir = trim(destination(:index(trim(destination), '/', &
            back=.true.) - 1))
        call fs_make_dir(trim(dependencies_dir))
        call make_sibling_tmpfile(trim(destination), stage)
        call make_tmpfile('fo-git-log', log_file)
        n_args = 0
        call argv_push(args, n_args, 'git')
        call argv_push(args, n_args, 'clone')
        call argv_push(args, n_args, '--no-checkout')
        call argv_push(args, n_args, '--')
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
        if (len_trim(dep%tag) > 0) ref = trim(dep%tag)
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
        rename_rc = fs_rename(trim(stage), trim(destination))
        call delete_tmpfile(trim(log_file))
        if (rename_rc /= 0) then
            call fs_remove_tree(trim(stage))
            write (error_unit, '(a)') 'fo: cannot publish Git dependency '// &
                trim(dep%name)//' at '//trim(destination)
            ierr = 1
        end if
    end subroutine acquire_git_checkout

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
        allocate (config)
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

    subroutine dep_update_run(project_dir, n_deps, refreshed, error_message)
        !! Remove the cached clones and the compiled dependency artifacts.
        !!
        !! The fpm profile directories are removed as well as the clone: leaving
        !! them behind lets the next build link objects compiled from the source
        !! that was just discarded.
        character(len=*), intent(in) :: project_dir
        integer, intent(out) :: n_deps
        logical, intent(out) :: refreshed
        character(len=*), intent(out), optional :: error_message

        type(fpm_config_t), allocatable :: config
        character(len=512) :: root
        integer :: i, ierr

        n_deps = 0
        refreshed = .false.
        if (present(error_message)) error_message = ''
        allocate (config)
        call normalize_path(project_dir, root)
        call fpm_config_parse(root, config, ierr)
        if (ierr /= 0) return
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) == DEP_PATH) cycle
            n_deps = n_deps + 1
        end do
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) == DEP_PATH) cycle
            n_deps = n_deps + 1
        end do
        if (n_deps == 0) return

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
