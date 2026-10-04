module fo_dep_resolve
    !! Resolve a project's dependency closure to the set of library source
    !! directories fo must compile alongside the project's own sources.
    !!
    !! Path dependencies and acquired Git dependencies are resolved
    !! transitively here. FPM stores the flattened Git closure in the root
    !! build/dependencies directory. Registry sources remain unresolved until
    !! a provider for FPM's registry cache is available.
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, dep_kind, &
        DEP_PATH, DEP_GIT, DEP_REGISTRY, add_link_lib
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private
    public :: resolved_src_t, resolve_dep_srcs, resolve_dev_dep_srcs, MAX_RESOLVED
    public :: normalize_path, join_path
    public :: merge_dep_link_libs

    integer, parameter :: MAX_RESOLVED = 256

    type :: resolved_src_t
        character(len=256) :: name = ''
        character(len=512) :: dir = '' ! absolute dep root (dedup key)
        character(len=512) :: src_dir = '' ! absolute dir holding library sources
        integer :: kind = DEP_PATH
    end type resolved_src_t

contains

    subroutine resolve_dep_srcs(project_dir, out, n_out, n_unresolved, ierr, &
            n_registry_out)
        !! Collect path deps and acquired Git source dirs for this project.
        !! Registry deps and missing Git checkouts are counted as unresolved.
        !! out excludes the root project's own sources.
        character(len=*), intent(in) :: project_dir
        type(resolved_src_t), intent(out) :: out(MAX_RESOLVED)
        integer, intent(out) :: n_out, n_unresolved, ierr
        integer, intent(out), optional :: n_registry_out
        integer :: n_registry

        type(fpm_config_t), allocatable :: root_config
        character(len=512) :: root

        n_out = 0
        n_unresolved = 0
        n_registry = 0
        ierr = 0
        call normalize_path(project_dir, root)
        allocate (root_config)
        call fpm_config_parse(root, root_config, ierr)
        if (ierr /= 0) return
        call walk(root, out, n_out, n_unresolved, n_registry, ierr, 0, &
            root_config)
        if (n_registry > 0) call drop_git_sources(out, n_out)
        if (present(n_registry_out)) n_registry_out = n_registry
    end subroutine resolve_dep_srcs

    subroutine resolve_dev_dep_srcs(project_dir, out, n_out, ierr)
        !! Collect dev-dependency library source dirs for the test build.
        !! Path dev-deps resolve to their local dir; acquired external deps
        !! resolve to the fpm bootstrapped layout build/dependencies/<name>.
        !! Each dev-dep's regular dependency closure is included for tests.
        character(len=*), intent(in) :: project_dir
        type(resolved_src_t), intent(out) :: out(MAX_RESOLVED)
        integer, intent(out) :: n_out, ierr

        type(fpm_config_t), allocatable :: cfg
        character(len=512) :: root, dep_dir
        integer :: i, k, kind, n_unresolved, n_registry
        logical :: seen

        n_out = 0
        ierr = 0
        allocate (cfg)
        call normalize_path(project_dir, root)
        call fpm_config_parse(root, cfg, ierr)
        if (ierr /= 0) return

        n_unresolved = 0
        n_registry = 0
        do i = 1, cfg%n_dev_deps
            kind = dep_kind(cfg%dev_deps(i))
            if (kind == DEP_PATH) then
                call resolve_path_dep(root, trim(cfg%dev_deps(i)%path), dep_dir)
            else
                dep_dir = trim(root)//'/build/dependencies/'// &
                    trim(cfg%dev_deps(i)%name)
            end if
            seen = .false.
            do k = 1, n_out
                if (trim(out(k)%dir) == trim(dep_dir)) then
                    seen = .true.
                    exit
                end if
            end do
            if (.not. seen) then
                call record_dep_src(cfg%dev_deps(i)%name, dep_dir, out, n_out, &
                    kind, ierr)
                if (ierr /= 0) return
                call walk(dep_dir, out, n_out, n_unresolved, n_registry, ierr, &
                    1, cfg)
                if (ierr /= 0) return
            end if
        end do
        if (n_unresolved > 0) then
            write (error_unit, '(a,i0,a)') 'fo: ', n_unresolved, &
                ' dev-dependency closure entries are unresolved'
            ierr = 1
        end if
    end subroutine resolve_dev_dep_srcs

    subroutine merge_dep_link_libs(project_dir, config)
        !! Add every dependency's own `link` entries to the root config.
        !!
        !! fo compiles a dependency's C and C++ sources into the same archive as
        !! the project's, so the final link line needs whatever those sources
        !! need: for a C++ shim that is at least stdc++, without which the link
        !! fails on __gxx_personality_v0 rather than on anything the caller
        !! wrote. Requiring the consumer to restate the dependency's link list
        !! makes the dependency's own manifest decorative and breaks as soon as
        !! the dependency adds a library.
        !!
        !! Dev-dependencies are included because their sources are compiled into
        !! the test build. Only path deps are walked; a git or registry dep that
        !! has not been acquired has no manifest to read.
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(inout) :: config

        type(resolved_src_t) :: deps(MAX_RESOLVED), devs(MAX_RESOLVED)
        type(fpm_config_t), allocatable :: dcfg
        integer :: n_deps, n_devs, n_unresolved, ierr, i, j

        call resolve_dep_srcs(project_dir, deps, n_deps, n_unresolved, ierr)
        if (ierr /= 0) n_deps = 0
        call resolve_dev_dep_srcs(project_dir, devs, n_devs, ierr)
        if (ierr /= 0) n_devs = 0

        allocate (dcfg)
        do i = 1, n_deps
            call fpm_config_parse(trim(deps(i)%dir), dcfg, ierr)
            if (ierr /= 0) cycle
            do j = 1, dcfg%n_link_libs
                call add_link_lib(config, trim(dcfg%link_libs(j)))
            end do
        end do
        do i = 1, n_devs
            call fpm_config_parse(trim(devs(i)%dir), dcfg, ierr)
            if (ierr /= 0) cycle
            do j = 1, dcfg%n_link_libs
                call add_link_lib(config, trim(dcfg%link_libs(j)))
            end do
        end do
    end subroutine merge_dep_link_libs

    recursive subroutine walk(dir, out, n_out, n_unresolved, n_registry, &
            ierr, depth, root_config)
        character(len=*), intent(in) :: dir
        type(resolved_src_t), intent(inout) :: out(MAX_RESOLVED)
        integer, intent(inout) :: n_out, n_unresolved, n_registry
        integer, intent(out) :: ierr
        integer, intent(in) :: depth
        type(fpm_config_t), intent(in) :: root_config

        type(fpm_config_t), allocatable :: cfg
        integer :: i, k, kind, root_kind
        character(len=512) :: dep_dir, dep_src
        logical :: seen, shadowed

        ierr = 0
        if (depth > 64) then
            write (error_unit, '(a)') 'fo: dependency nesting exceeds 64 at '// &
                trim(dir)
            ierr = 1
            return
        end if
        allocate (cfg)
        call fpm_config_parse(dir, cfg, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: invalid dependency manifest '// &
                trim(dir)//'/fpm.toml'
            return
        end if

        do i = 1, cfg%n_deps
            kind = dep_kind(cfg%deps(i))
            root_kind = kind
            if (kind == DEP_PATH) then
                ! If the root pins this package through Git, use that
                ! flattened checkout instead of a second path copy.
                shadowed = .false.
                do k = 1, root_config%n_deps
                    if (trim(root_config%deps(k)%name) /= &
                            trim(cfg%deps(i)%name)) cycle
                    root_kind = dep_kind(root_config%deps(k))
                    if (root_kind == DEP_PATH) cycle
                    shadowed = .true.
                    exit
                end do
                if (shadowed) then
                    if (root_kind /= DEP_GIT) then
                        n_unresolved = n_unresolved + 1
                        cycle
                    end if
                    dep_dir = trim(root_config%project_dir)// &
                        '/build/dependencies/'//trim(cfg%deps(i)%name)
                else
                    call resolve_path_dep(dir, trim(cfg%deps(i)%path), dep_dir)
                end if
            else if (kind == DEP_GIT) then
                dep_dir = trim(root_config%project_dir)// &
                    '/build/dependencies/'//trim(cfg%deps(i)%name)
            else
                n_unresolved = n_unresolved + 1
                n_registry = n_registry + 1
                cycle
            end if
            if (.not. has_manifest(dep_dir)) then
                if (root_kind == DEP_GIT) n_unresolved = n_unresolved + 1
                if (root_kind == DEP_PATH) then
                    write (error_unit, '(a)') 'fo: missing dependency manifest '// &
                        trim(dep_dir)//'/fpm.toml'
                    ierr = 1
                    return
                end if
                cycle
            end if
            seen = .false.
            do k = 1, n_out
                if (trim(out(k)%dir) == trim(dep_dir)) then
                    seen = .true.
                    exit
                end if
            end do
            ! Record the dep's library source dir (its own source-dir setting),
            ! then recurse into the dep for its transitive dependencies. Dedup
            ! on the resolved root so a diamond is compiled once.
            if (.not. seen) then
                call record_dep_src(cfg%deps(i)%name, dep_dir, out, n_out, &
                    root_kind, ierr)
                if (ierr /= 0) return
                call walk(dep_dir, out, n_out, n_unresolved, n_registry, &
                    ierr, depth + 1, root_config)
                if (ierr /= 0) return
            end if
        end do
    end subroutine walk

    subroutine drop_git_sources(out, n_out)
        type(resolved_src_t), intent(inout) :: out(MAX_RESOLVED)
        integer, intent(inout) :: n_out

        type(resolved_src_t) :: kept(MAX_RESOLVED)
        integer :: i, n_kept

        n_kept = 0
        do i = 1, n_out
            if (out(i)%kind == DEP_GIT) cycle
            n_kept = n_kept + 1
            kept(n_kept) = out(i)
        end do
        out = kept
        n_out = n_kept
    end subroutine drop_git_sources

    subroutine record_dep_src(name, dep_dir, out, n_out, kind, ierr)
        character(len=*), intent(in) :: name, dep_dir
        type(resolved_src_t), intent(inout) :: out(MAX_RESOLVED)
        integer, intent(inout) :: n_out
        integer, intent(in), optional :: kind
        integer, intent(out) :: ierr

        type(fpm_config_t), allocatable :: dcfg
        integer :: derr
        character(len=512) :: src

        ierr = 0
        if (n_out >= MAX_RESOLVED) then
            write (error_unit, '(a)') 'fo: dependency closure exceeds '// &
                'maximum of '//itoa(MAX_RESOLVED)//' entries'
            ierr = 1
            return
        end if
        allocate (dcfg)
        call fpm_config_parse(dep_dir, dcfg, derr)
        ierr = derr
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: invalid dependency manifest '// &
                trim(dep_dir)//'/fpm.toml'
            return
        end if
        if (len_trim(dcfg%source_dir) > 0) then
            src = trim(dep_dir)//'/'//trim(dcfg%source_dir)
        else
            src = trim(dep_dir)//'/src'
        end if
        n_out = n_out + 1
        out(n_out)%name = trim(name)
        out(n_out)%dir = trim(dep_dir)
        out(n_out)%src_dir = trim(src)
        out(n_out)%kind = DEP_PATH
        if (present(kind)) out(n_out)%kind = kind
    end subroutine record_dep_src

    function itoa(value) result(text)
        integer, intent(in) :: value
        character(len=16) :: text
        write (text, '(i0)') value
    end function itoa

    subroutine resolve_path_dep(base, rel, out)
        character(len=*), intent(in) :: base, rel
        character(len=*), intent(out) :: out
        character(len=512) :: primary_root, candidate
        logical :: found

        call join_path(base, rel, out)
        if (has_manifest(out)) return
        call linked_worktree_primary_root(base, primary_root, found)
        if (.not. found) return
        call join_path(primary_root, rel, candidate)
        if (has_manifest(candidate)) out = candidate
    end subroutine resolve_path_dep

    logical function has_manifest(dir)
        character(len=*), intent(in) :: dir

        inquire (file=trim(dir)//'/fpm.toml', exist=has_manifest)
    end function has_manifest

    subroutine linked_worktree_primary_root(project_dir, primary_root, found)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(out) :: primary_root
        logical, intent(out) :: found
        character(len=1024) :: line
        character(len=512) :: git_dir, normalized_git_dir
        integer :: unit, ios, marker
        logical :: git_dir_exists

        primary_root = ''
        found = .false.
        open (newunit=unit, file=trim(project_dir)//'/.git', status='old', &
            action='read', iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) line
        close (unit)
        if (ios /= 0) return
        if (index(line, 'gitdir: ') /= 1) return

        git_dir = adjustl(line(9:))
        if (git_dir(1:1) /= '/') then
            call join_path(project_dir, trim(git_dir), normalized_git_dir)
        else
            call normalize_path(git_dir, normalized_git_dir)
        end if
        marker = index(trim(normalized_git_dir), '/.git/worktrees/')
        if (marker <= 1) return
        if (len_trim(normalized_git_dir) < marker + 16) return
        inquire (file=trim(normalized_git_dir), exist=git_dir_exists)
        if (.not. git_dir_exists) return
        primary_root = normalized_git_dir(:marker - 1)
        found = .true.
    end subroutine linked_worktree_primary_root

    subroutine join_path(base, rel, out)
        !! Resolve rel against base (absolute base assumed) and normalize.
        character(len=*), intent(in) :: base, rel
        character(len=*), intent(out) :: out

        if (len_trim(rel) == 0) then
            call normalize_path(base, out)
        else if (rel(1:1) == '/') then
            call normalize_path(rel, out)
        else
            call normalize_path(trim(base)//'/'//trim(rel), out)
        end if
    end subroutine join_path

    subroutine normalize_path(path, out)
        !! Collapse '.' and 'a/b/..' segments so equivalent spellings of one
        !! directory compare equal for dedup. Leading '/' is preserved; a
        !! leading '..' that cannot be collapsed is kept verbatim.
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: out

        character(len=256) :: segs(128)
        integer :: nseg, i, start, n
        logical :: absolute, at_sep
        character(len=:), allocatable :: p

        p = trim(adjustl(path))
        n = len_trim(p)
        absolute = (n >= 1 .and. p(1:1) == '/')
        nseg = 0
        start = 1
        do i = 1, n + 1
            at_sep = i > n
            if (.not. at_sep) at_sep = p(i:i) == '/'
            if (at_sep) then
                if (i > start) then
                    call push_seg(p(start:i - 1), segs, nseg)
                end if
                start = i + 1
            end if
        end do

        out = ''
        if (absolute) out = '/'
        do i = 1, nseg
            if (i == 1) then
                out = trim(out)//trim(segs(i))
            else
                out = trim(out)//'/'//trim(segs(i))
            end if
        end do
        if (len_trim(out) == 0) out = '.'
    end subroutine normalize_path

    subroutine push_seg(seg, segs, nseg)
        character(len=*), intent(in) :: seg
        character(len=256), intent(inout) :: segs(128)
        integer, intent(inout) :: nseg

        if (trim(seg) == '.') return
        if (trim(seg) == '..') then
            if (nseg > 0) then
                if (trim(segs(nseg)) /= '..') then
                    nseg = nseg - 1
                    return
                end if
            end if
            ! cannot collapse: keep it (relative path escaping the base)
            if (nseg < 128) then
                nseg = nseg + 1
                segs(nseg) = '..'
            end if
            return
        end if
        if (nseg < 128) then
            nseg = nseg + 1
            segs(nseg) = trim(seg)
        end if
    end subroutine push_seg

end module fo_dep_resolve
