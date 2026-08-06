module fo_external_modules
    !! Locate Fortran module files that a project declares but does not define.
    !!
    !! `[build] external-modules` in fpm.toml names modules such as `hdf5` or
    !! `netcdf` that come from a system package.  fpm uses the list only to keep
    !! those names out of the dependency graph, but gfortran still has to be
    !! told where the `.mod` file lives: unlike C headers it does not search
    !! /usr/include for modules, and it ignores CPATH for them.  Without a -I
    !! the compile fails with "Cannot open module file", which reads like a
    !! missing library even though the package is installed.
    !!
    !! Rather than map module names onto package names, which differ across
    !! distributions, this searches the system module directories for the
    !! declared module file itself and returns the directories that hold one.

    implicit none
    private

    public :: collect_external_module_dirs

    integer, parameter :: MAX_SEARCH_DIRS = 16
    !> Colon-separated override, searched ahead of the built-in list.  Needed
    !> for module trees a package manager puts somewhere unusual, and for
    !> Spack, Nix, or module-file environments that install outside /usr.
    character(len=*), parameter :: SEARCH_PATH_ENV = 'FO_MODULE_PATH'

contains

    subroutine collect_external_module_dirs(names, n_names, dirs, n_dirs, &
            max_dirs)
        !! Append, to `dirs`, each distinct directory holding one of the named
        !! modules.  Names that are not found anywhere are skipped without
        !! error: a declared external module may legitimately be an include
        !! file rather than a module, or be unused on this build.
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        character(len=*), intent(inout) :: dirs(:)
        integer, intent(inout) :: n_dirs
        integer, intent(in) :: max_dirs

        character(len=512) :: search(MAX_SEARCH_DIRS)
        character(len=512) :: candidate
        character(len=128) :: lower_name
        integer :: n_search, i, j
        logical :: exists

        call build_search_path(search, n_search)

        do i = 1, n_names
            if (len_trim(names(i)) == 0) cycle
            call to_lower(trim(names(i)), lower_name)
            do j = 1, n_search
                candidate = trim(search(j))//'/'//trim(lower_name)//'.mod'
                inquire (file=trim(candidate), exist=exists)
                if (.not. exists) cycle
                call add_unique_dir(trim(search(j)), dirs, n_dirs, max_dirs)
                exit
            end do
        end do
    end subroutine collect_external_module_dirs

    subroutine build_search_path(search, n_search)
        !! FO_MODULE_PATH entries first, then the directories distributions
        !! actually install gfortran modules into.
        character(len=512), intent(out) :: search(MAX_SEARCH_DIRS)
        integer, intent(out) :: n_search

        character(len=4096) :: env_value
        integer :: length, status, start, pos

        n_search = 0

        call get_environment_variable(SEARCH_PATH_ENV, env_value, length, status)
        if (status == 0 .and. length > 0) then
            start = 1
            do pos = 1, length + 1
                if (pos <= length) then
                    if (env_value(pos:pos) /= ':') cycle
                end if
                if (pos > start) then
                    call add_search_dir(env_value(start:pos - 1), search, n_search)
                end if
                start = pos + 1
            end do
        end if

        call add_search_dir('/usr/include', search, n_search)
        call add_search_dir('/usr/local/include', search, n_search)
        call add_search_dir('/usr/lib/gfortran/modules', search, n_search)
        call add_search_dir('/usr/lib64/gfortran/modules', search, n_search)
        call add_search_dir('/usr/include/gfortran', search, n_search)
        call add_search_dir('/usr/include/x86_64-linux-gnu', search, n_search)
        call add_search_dir('/usr/include/aarch64-linux-gnu', search, n_search)
        call add_search_dir('/opt/homebrew/include', search, n_search)
    end subroutine build_search_path

    subroutine add_search_dir(dir, search, n_search)
        character(len=*), intent(in) :: dir
        character(len=512), intent(inout) :: search(MAX_SEARCH_DIRS)
        integer, intent(inout) :: n_search

        integer :: i

        if (len_trim(dir) == 0) return
        if (n_search >= MAX_SEARCH_DIRS) return
        do i = 1, n_search
            if (trim(search(i)) == trim(adjustl(dir))) return
        end do
        n_search = n_search + 1
        search(n_search) = trim(adjustl(dir))
    end subroutine add_search_dir

    subroutine add_unique_dir(dir, dirs, n_dirs, max_dirs)
        character(len=*), intent(in) :: dir
        character(len=*), intent(inout) :: dirs(:)
        integer, intent(inout) :: n_dirs
        integer, intent(in) :: max_dirs

        integer :: i

        do i = 1, n_dirs
            if (trim(dirs(i)) == trim(dir)) return
        end do
        if (n_dirs >= max_dirs) return
        n_dirs = n_dirs + 1
        dirs(n_dirs) = trim(dir)
    end subroutine add_unique_dir

    subroutine to_lower(text, lowered)
        character(len=*), intent(in) :: text
        character(len=*), intent(out) :: lowered

        integer :: i, code

        lowered = ''
        do i = 1, min(len_trim(text), len(lowered))
            code = iachar(text(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                lowered(i:i) = achar(code - iachar('A') + iachar('a'))
            else
                lowered(i:i) = text(i:i)
            end if
        end do
    end subroutine to_lower

end module fo_external_modules
