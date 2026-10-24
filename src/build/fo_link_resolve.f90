module fo_link_resolve
    !! Resolution of manifest `link` entries to the library files a native
    !! link uses. Both the link driver and the canonical input inventory share
    !! it, so recorded provenance names exactly the file Fo passes to the linker.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, &
        fpm_config_allocate, dep_kind, DEP_PATH
    use fo_dep_resolve, only: join_path
    implicit none
    private

    public :: resolve_link_token, build_lib_search_dirs, is_macos

contains

    subroutine resolve_link_token(project_dir, name, dirs, n_dirs, n_env, token)
        !! Resolve a linker token directly to a file when we can find it in the
        !! build-tree graph around the current project. This keeps local sibling
        !! dependencies resolvable even when callers do not configure LIBRARY_PATH.
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: name
        character(len=512), intent(in) :: dirs(:)
        integer, intent(in) :: n_dirs, n_env
        character(len=*), intent(out) :: token

        character(len=1024) :: cand
        character(len=8) :: ext1, ext2
        character(len=8) :: debug_links
        integer :: i, debug_status
        logical :: ex
        logical :: dbg
        logical :: use_local

        dbg = .false.
        call get_environment_variable('FO_DEBUG_LINKS', debug_links, status=debug_status)
        if (debug_status == 0 .and. len_trim(debug_links) > 0) dbg = .true.

        ! On macOS prefer the shared .dylib: its absolute install name pulls in
        ! transitive deps (CoreText, CoreGraphics, bz2, ...) that a static .a
        ! would leave unresolved at link time, and dylib install names are
        ! absolute so the binary needs no DYLD_LIBRARY_PATH at run time.
        !
        ! On Linux the preference is per directory. Env dirs (the first n_env,
        ! from LIBRARY_PATH/FO_LIBRARY_PATH) are non-default paths: prefer a
        ! static .a there so the binary does not need LIBRARY_PATH at run time.
        ! System dirs (/usr/lib, ...) prefer the shared .so: it is always on the
        ! default runtime search path, and a system static .a often needs a long
        ! chain of transitive deps (glib -> pcre2, ffi, sysprof, ...) that the
        ! project's `link` list does not enumerate, which would break the link.
        ! 1) Existing environment/system search: /path/lib{name}{.so|.a}
        do i = 1, n_dirs
            if (is_macos()) then
                ext1 = '.dylib'
                ext2 = '.a'
            else if (i <= n_env) then
                ext1 = '.a'
                ext2 = '.so'
            else
                ext1 = '.so'
                ext2 = '.a'
            end if
            cand = trim(dirs(i))//'/lib'//trim(name)//trim(ext1)
            inquire (file=trim(cand), exist=ex)
            if (ex) then
                token = trim(cand)
                if (dbg) write (error_unit, '(a,a)') 'fo link token found: ', trim(token)
                return
            end if
            cand = trim(dirs(i))//'/lib'//trim(name)//trim(ext2)
            inquire (file=trim(cand), exist=ex)
            if (ex) then
                token = trim(cand)
                if (dbg) write (error_unit, '(a,a)') 'fo link token found: ', trim(token)
                return
            end if
        end do

        ! 2) Local sibling projects (e.g. ../liric/build). This is common in
        ! lazy-fortran workspaces where LIRIC is built side-by-side, not in
        ! LIBRARY_PATH.
        call local_library_candidate(project_dir, trim(name), cand, use_local)
        if (use_local) then
            token = trim(cand)
            if (dbg) write (error_unit, '(a,a)') 'fo link token found: ', trim(token)
            return
        end if

        if (dbg) write (error_unit, '(a,a)') 'fo link token unresolved: ', trim(name)
        token = '-l'//trim(name)
    end subroutine resolve_link_token

    subroutine local_library_candidate(project_dir, name, candidate, found)
        !! Resolve a direct library artifact path from sibling checkout/build
        !! trees so local dependency builds do not require LIBRARY_PATH.
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: name
        character(len=1024), intent(out) :: candidate
        logical, intent(out) :: found

        character(len=1024) :: root_dir
        character(len=1024) :: local_root
        character(len=8) :: ext1, ext2
        integer :: slash, j
        logical :: exists

        found = .false.
        candidate = ''
        if (len_trim(project_dir) == 0) return

        call manifest_library_candidate(project_dir, name, candidate, found)
        if (found) return

        root_dir = trim(project_dir)
        if (root_dir(1:1) /= '/') then
            call get_environment_variable('PWD', root_dir)
            if (len_trim(root_dir) == 0) return
        end if

        if (len_trim(root_dir) > 1 .and. root_dir(len_trim(root_dir):len_trim(root_dir)) == '/') &
            root_dir = root_dir(1:len_trim(root_dir) - 1)
        slash = index(trim(root_dir), '/', back=.true.)
        if (slash > 0) root_dir = root_dir(1:slash - 1)

        if (is_macos()) then
            ext1 = '.dylib'
            ext2 = '.a'
        else
            ext1 = '.a'
            ext2 = '.so'
        end if

        do j = 1, 2
            if (j == 1) then
                local_root = trim(root_dir)//'/'//trim(name)//'/build'
            else
                local_root = trim(root_dir)//'/../'//trim(name)//'/build'
            end if

            candidate = trim(local_root)//'/lib'//trim(name)//trim(ext1)
            inquire (file=trim(candidate), exist=exists)
            if (exists) then
                found = .true.
                return
            end if

            candidate = trim(local_root)//'/'//trim(name)//trim(ext1)
            inquire (file=trim(candidate), exist=exists)
            if (exists) then
                found = .true.
                return
            end if

            candidate = trim(local_root)//'/lib'//trim(name)//trim(ext2)
            inquire (file=trim(candidate), exist=exists)
            if (exists) then
                found = .true.
                return
            end if

            candidate = trim(local_root)//'/'//trim(name)//trim(ext2)
            inquire (file=trim(candidate), exist=exists)
            if (exists) then
                found = .true.
                return
            end if
        end do
    end subroutine local_library_candidate

    subroutine manifest_library_candidate(project_dir, name, candidate, found)
        !! Resolve a link library from an explicit path dependency in the
        !! project's fpm.toml rather than guessing sibling directories. When a
        !! [dependencies] entry named `name` has a local path, its build tree is
        !! the authoritative artifact location.
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: name
        character(len=1024), intent(out) :: candidate
        logical, intent(out) :: found

        type(fpm_config_t), allocatable :: config
        character(len=512) :: dep_root
        character(len=8) :: ext1, ext2
        integer :: ierr, i

        found = .false.
        candidate = ''
        dep_root = ''

        call fpm_config_allocate(config)
        call fpm_config_parse(project_dir, config, ierr)
        if (ierr /= 0) return

        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) /= DEP_PATH) cycle
            if (trim(config%deps(i)%name) /= trim(name)) cycle
            call join_path(project_dir, trim(config%deps(i)%path), dep_root)
            exit
        end do
        if (len_trim(dep_root) == 0) return

        if (is_macos()) then
            ext1 = '.dylib'
            ext2 = '.a'
        else
            ext1 = '.a'
            ext2 = '.so'
        end if

        call probe_dep_artifact(dep_root, name, ext1, candidate, found)
        if (found) return
        call probe_dep_artifact(dep_root, name, ext2, candidate, found)
    end subroutine manifest_library_candidate

    subroutine probe_dep_artifact(dep_root, name, ext, candidate, found)
        character(len=*), intent(in) :: dep_root, name, ext
        character(len=1024), intent(out) :: candidate
        logical, intent(out) :: found

        found = .false.
        candidate = trim(dep_root)//'/build/lib'//trim(name)//trim(ext)
        inquire (file=trim(candidate), exist=found)
        if (found) return
        candidate = trim(dep_root)//'/build/'//trim(name)//trim(ext)
        inquire (file=trim(candidate), exist=found)
    end subroutine probe_dep_artifact

    logical function is_macos()
        !! True on macOS, detected by the always-present dynamic linker. Cached:
        !! the filesystem layout does not change within a run.
        logical, save :: checked = .false.
        logical, save :: mac = .false.

        if (.not. checked) then
            inquire (file='/usr/lib/dyld', exist=mac)
            checked = .true.
        end if
        is_macos = mac
    end function is_macos

    subroutine build_lib_search_dirs(dirs, n, n_env)
        !! Search dirs for external libs: LIBRARY_PATH and FO_LIBRARY_PATH
        !! (colon-separated, as understood by the toolchain) plus common system
        !! library locations. FO_LIBRARY_PATH lets a daemonized fo (MCP/LSP) that
        !! did not inherit LIBRARY_PATH still locate project-external archives.
        !! n_env returns how many leading dirs came from the environment; those
        !! are non-default paths where a static .a is preferred for runtime
        !! independence, while the trailing system dirs prefer the shared .so
        !! (always on the default runtime search path), avoiding broken links
        !! when a system .a needs transitive deps not listed in `link`.
        character(len=512), intent(out) :: dirs(:)
        integer, intent(out) :: n
        integer, intent(out) :: n_env

        n = 0
        call add_env_path_list('LIBRARY_PATH', dirs, n)
        call add_env_path_list('FO_LIBRARY_PATH', dirs, n)
        n_env = n
        if (is_macos()) call add_search_dir('/opt/homebrew/lib', dirs, n)
        call add_search_dir('/usr/local/lib', dirs, n)
        call add_search_dir('/usr/lib', dirs, n)
        call add_search_dir('/usr/lib64', dirs, n)
        call add_search_dir('/lib', dirs, n)
    end subroutine build_lib_search_dirs

    subroutine add_env_path_list(var, dirs, n)
        character(len=*), intent(in) :: var
        character(len=512), intent(inout) :: dirs(:)
        integer, intent(inout) :: n

        character(len=4096) :: val
        integer :: status, i, start

        call get_environment_variable(var, val, status=status)
        if (status /= 0 .or. len_trim(val) == 0) return
        start = 1
        do i = 1, len_trim(val) + 1
            if (i > len_trim(val) .or. val(i:i) == ':') then
                if (i > start) call add_search_dir(val(start:i - 1), dirs, n)
                start = i + 1
            end if
        end do
    end subroutine add_env_path_list

    subroutine add_search_dir(dir, dirs, n)
        character(len=*), intent(in) :: dir
        character(len=512), intent(inout) :: dirs(:)
        integer, intent(inout) :: n

        integer :: i

        if (len_trim(dir) == 0) return
        do i = 1, n
            if (trim(dirs(i)) == trim(dir)) return
        end do
        if (n >= size(dirs)) return
        n = n + 1
        dirs(n) = trim(dir)
    end subroutine add_search_dir

end module fo_link_resolve
