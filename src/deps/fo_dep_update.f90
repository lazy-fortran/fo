module fo_dep_update
    !! Refresh git and registry dependencies.
    !!
    !! fo bootstraps those dependencies once, through fpm, and then reuses the
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
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, dep_kind, DEP_PATH
    use fo_fs, only: fs_remove_tree, fs_stat
    use fo_dep_resolve, only: normalize_path
    use, intrinsic :: iso_c_binding, only: c_long_long
    implicit none
    private

    public :: dep_update_run
    public :: dep_update_missing_sources
    public :: MAX_UPDATE_NAMES

    integer, parameter :: MAX_UPDATE_NAMES = 64
    integer, parameter :: MAX_PROFILE_DIRS = 512

contains

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

    subroutine dep_update_run(project_dir, n_deps, refreshed)
        !! Remove the cached clones and the compiled dependency artifacts.
        !!
        !! The fpm profile directories are removed as well as the clone: leaving
        !! them behind lets the next build link objects compiled from the source
        !! that was just discarded.
        character(len=*), intent(in) :: project_dir
        integer, intent(out) :: n_deps
        logical, intent(out) :: refreshed

        type(fpm_config_t), allocatable :: config
        character(len=512) :: root
        integer :: i, ierr

        n_deps = 0
        refreshed = .false.
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

        call fs_remove_tree(trim(root)//'/build/dependencies')
        call remove_profile_trees(trim(root)//'/build')
        refreshed = .true.
    end subroutine dep_update_run

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
