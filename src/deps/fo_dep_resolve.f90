module fo_dep_resolve
    !! Resolve a project's dependency closure to the set of library source
    !! directories fo must compile alongside the project's own sources.
    !!
    !! Path dependencies are resolved transitively here: each path dep's own
    !! fpm.toml is parsed for its source-dir and its further path deps, walked to
    !! a fixpoint with a visit guard. Git and registry deps are classified and
    !! reported separately (acquisition lives in fo_dep_fetch); a dep whose
    !! modules are never `use`d contributes no compiles because the module DAG
    !! only pulls in reachable units.
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, dep_kind, &
        DEP_PATH, DEP_GIT, DEP_REGISTRY
    implicit none
    private
    public :: resolved_src_t, resolve_dep_srcs, resolve_dev_dep_srcs, MAX_RESOLVED
    public :: normalize_path, join_path

    integer, parameter :: MAX_RESOLVED = 256

    type :: resolved_src_t
        character(len=256) :: name = ''
        character(len=512) :: dir = '' ! absolute dep root (dedup key)
        character(len=512) :: src_dir = '' ! absolute dir holding library sources
        integer :: kind = DEP_PATH
    end type resolved_src_t

contains

    subroutine resolve_dep_srcs(project_dir, out, n_out, n_unresolved, ierr)
        !! Collect the transitive path-dependency library source dirs of the
        !! project at project_dir. n_unresolved counts git/registry deps that
        !! were seen but not acquired here (the caller decides whether any are
        !! actually needed). out excludes the root project's own sources.
        character(len=*), intent(in) :: project_dir
        type(resolved_src_t), intent(out) :: out(MAX_RESOLVED)
        integer, intent(out) :: n_out, n_unresolved, ierr

        character(len=512) :: root

        n_out = 0
        n_unresolved = 0
        ierr = 0
        call normalize_path(project_dir, root)
        call walk(root, out, n_out, n_unresolved, ierr, 0)
    end subroutine resolve_dep_srcs

    subroutine resolve_dev_dep_srcs(project_dir, out, n_out, ierr)
        !! Collect dev-dependency library source dirs for the test build.
        !! Path dev-deps resolve to their local dir; git/registry dev-deps
        !! resolve to the fpm bootstrapped layout build/dependencies/<name>.
        !! Transitive regular deps of a dev-dep are not walked (deferred):
        !! the test build scans direct dev-dep sources only.
        character(len=*), intent(in) :: project_dir
        type(resolved_src_t), intent(out) :: out(MAX_RESOLVED)
        integer, intent(out) :: n_out, ierr

        type(fpm_config_t) :: cfg
        character(len=512) :: root, dep_dir
        integer :: i, k, kind
        logical :: seen

        n_out = 0
        ierr = 0
        call normalize_path(project_dir, root)
        call fpm_config_parse(root, cfg, ierr)
        if (ierr /= 0) return

        do i = 1, cfg%n_dev_deps
            kind = dep_kind(cfg%dev_deps(i))
            if (kind == DEP_PATH) then
                call join_path(root, trim(cfg%dev_deps(i)%path), dep_dir)
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
                call record_dep_src(cfg%dev_deps(i)%name, dep_dir, out, n_out)
            end if
        end do
    end subroutine resolve_dev_dep_srcs

    recursive subroutine walk(dir, out, n_out, n_unresolved, ierr, depth)
        character(len=*), intent(in) :: dir
        type(resolved_src_t), intent(inout) :: out(MAX_RESOLVED)
        integer, intent(inout) :: n_out, n_unresolved
        integer, intent(out) :: ierr
        integer, intent(in) :: depth

        type(fpm_config_t) :: cfg
        integer :: i, k, kind
        character(len=512) :: dep_dir, dep_src
        logical :: seen

        ierr = 0
        if (depth > 64) return
        call fpm_config_parse(dir, cfg, ierr)
        if (ierr /= 0) return

        do i = 1, cfg%n_deps
            kind = dep_kind(cfg%deps(i))
            if (kind /= DEP_PATH) then
                n_unresolved = n_unresolved + 1
                cycle
            end if
            call join_path(dir, trim(cfg%deps(i)%path), dep_dir)
            seen = .false.
            do k = 1, n_out
                if (trim(out(k)%dir) == trim(dep_dir)) then
                    seen = .true.
                    exit
                end if
            end do
            ! Record the dep's library source dir (its own source-dir setting),
            ! then recurse into the dep for its transitive path deps. Dedup keys
            ! on the resolved dep dir so a diamond is compiled once.
            if (.not. seen) then
                call record_dep_src(cfg%deps(i)%name, dep_dir, out, n_out)
                call walk(dep_dir, out, n_out, n_unresolved, ierr, depth + 1)
                ierr = 0
            end if
        end do
    end subroutine walk

    subroutine record_dep_src(name, dep_dir, out, n_out)
        character(len=*), intent(in) :: name, dep_dir
        type(resolved_src_t), intent(inout) :: out(MAX_RESOLVED)
        integer, intent(inout) :: n_out

        type(fpm_config_t) :: dcfg
        integer :: derr
        character(len=512) :: src

        if (n_out >= MAX_RESOLVED) return
        call fpm_config_parse(dep_dir, dcfg, derr)
        if (derr == 0 .and. len_trim(dcfg%source_dir) > 0) then
            src = trim(dep_dir)//'/'//trim(dcfg%source_dir)
        else
            src = trim(dep_dir)//'/src'
        end if
        n_out = n_out + 1
        out(n_out)%name = trim(name)
        out(n_out)%dir = trim(dep_dir)
        out(n_out)%src_dir = trim(src)
        out(n_out)%kind = DEP_PATH
    end subroutine record_dep_src

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
        logical :: absolute
        character(len=:), allocatable :: p

        p = trim(adjustl(path))
        n = len_trim(p)
        absolute = (n >= 1 .and. p(1:1) == '/')
        nseg = 0
        start = 1
        do i = 1, n + 1
            if (i > n .or. p(i:i) == '/') then
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
