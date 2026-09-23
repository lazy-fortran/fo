module fo_build_tree
    !! Where the native backend puts linked executables for a set of requested
    !! compiler flags.
    !!
    !! A default build (no --flag or --profile) links into build/fo/bin, as it
    !! always has. Every other flag set gets its own tree under
    !! build/fo/profiles/<slug>/, so `fo build --flag -O3` followed by a plain
    !! `fo` no longer overwrites the optimised binaries with -O0 ones while a
    !! `fo exec --no-build` loop is still running them. Modules and objects stay
    !! shared: they are restored from the content-addressed cache on a switch.
    use fo_cache, only: cache_digest, HASH_LEN
    use fo_fs, only: fs_collect_files, fs_make_dir, fs_write_text
    implicit none
    private

    public :: native_output_dir, native_profiles_dir, native_profile_slug
    public :: native_record_profile, native_other_builds

    character(len=*), parameter :: FLAGS_FILE = 'requested-flags'

contains

    function native_output_dir(project_dir, request_flags, kind) result(dir)
        !! kind is 'bin' (public executables) or 'app' (app-only copies).
        character(len=*), intent(in) :: project_dir, request_flags, kind
        character(len=:), allocatable :: dir

        if (len_trim(request_flags) == 0) then
            dir = trim(project_dir)//'/build/fo/'//trim(kind)
        else
            dir = native_profiles_dir(project_dir)//'/'// &
                native_profile_slug(request_flags)//'/'//trim(kind)
        end if
    end function native_output_dir

    function native_profiles_dir(project_dir) result(dir)
        character(len=*), intent(in) :: project_dir
        character(len=:), allocatable :: dir

        dir = trim(project_dir)//'/build/fo/profiles'
    end function native_profiles_dir

    function native_profile_slug(request_flags) result(slug)
        !! Readable, collision-free directory name for a flag set: the flags
        !! with unsafe characters replaced, then a digest of the exact text.
        character(len=*), intent(in) :: request_flags
        character(len=:), allocatable :: slug
        integer, parameter :: MAX_READABLE = 40
        character(len=1024) :: parts(1)
        character(len=MAX_READABLE) :: readable
        character(len=HASH_LEN) :: digest
        character(len=1) :: ch
        integer :: i, n

        readable = ''
        n = 0
        do i = 1, len_trim(request_flags)
            if (n >= MAX_READABLE) exit
            ch = request_flags(i:i)
            if (n == 0 .and. ch == ' ') cycle
            select case (ch)
            case ('a':'z', 'A':'Z', '0':'9', '.', '_', '=', '+')
                continue
            case ('-')
                if (n == 0) cycle
            case default
                ch = '_'
            end select
            if (ch == '-' .or. ch == '_') then
                if (n > 0) then
                    if (readable(n:n) == '_') cycle
                end if
            end if
            n = n + 1
            readable(n:n) = ch
        end do
        parts(1) = adjustl(request_flags)
        digest = cache_digest(parts, 1)
        slug = trim(readable)//'-'//digest(1:12)
    end function native_profile_slug

    subroutine native_record_profile(project_dir, request_flags)
        !! Note the flags a profile tree was built with, so a mismatch in
        !! `fo exec` can say which --flag selects it.
        character(len=*), intent(in) :: project_dir, request_flags
        character(len=:), allocatable :: root

        if (len_trim(request_flags) == 0) return
        root = native_profiles_dir(project_dir)//'/'//native_profile_slug(request_flags)
        call fs_make_dir(root)
        call fs_write_text(root//'/'//FLAGS_FILE, trim(adjustl(request_flags))// &
            new_line('a'))
    end subroutine native_record_profile

    subroutine native_other_builds(project_dir, request_flags, target, labels, &
            n_labels)
        !! Requested-flag sets, other than request_flags, whose tree holds an
        !! executable named target. The default tree is labelled ''.
        character(len=*), intent(in) :: project_dir, request_flags, target
        character(len=*), intent(out) :: labels(:)
        integer, intent(out) :: n_labels
        character(len=1024) :: found(256)
        character(len=:), allocatable :: profiles, own, dir
        integer :: i, n_found, cut
        logical :: exists

        n_labels = 0
        if (len_trim(request_flags) > 0) then
            inquire (file=trim(project_dir)//'/build/fo/bin/'//trim(target), &
                exist=exists)
            if (exists .and. size(labels) > 0) then
                n_labels = 1
                labels(1) = ''
            end if
        end if
        profiles = native_profiles_dir(project_dir)//'/'
        own = native_output_dir(project_dir, request_flags, 'bin')//'/'//trim(target)
        call fs_collect_files(native_profiles_dir(project_dir), trim(target), &
            trim(target), '', found, n_found)
        do i = 1, n_found
            if (n_labels >= size(labels)) exit
            if (index(found(i), profiles) /= 1) cycle
            if (trim(found(i)) == own) cycle
            dir = trim(found(i)(len(profiles) + 1:))
            cut = index(dir, '/')
            if (cut <= 1) cycle
            if (dir(cut:) /= '/bin/'//trim(target)) cycle
            n_labels = n_labels + 1
            labels(n_labels) = read_recorded_flags(profiles//dir(:cut - 1))
        end do
    end subroutine native_other_builds

    function read_recorded_flags(root) result(flags)
        character(len=*), intent(in) :: root
        character(len=512) :: flags
        integer :: u, ios

        flags = '?'
        open (newunit=u, file=root//'/'//FLAGS_FILE, status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) flags
        close (u)
        if (ios /= 0) flags = '?'
    end function read_recorded_flags

end module fo_build_tree
