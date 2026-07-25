module fo_build_stamp
    use fo_fs, only: fs_make_dir, fs_rename, fs_tree_fingerprint
    use fo_util, only: make_sibling_tmpfile, delete_tmpfile
    use fx_hash, only: fnv1a_string
    use, intrinsic :: iso_c_binding, only: c_long_long
    implicit none
    private

    integer, parameter :: TEXT_LEN = 4096
    character(len=16), parameter :: STAMP_MAGIC = 'fo-build-v5'

    type :: stamp_t
        character(len=512) :: project = ''
        character(len=TEXT_LEN) :: compiler = ''
        character(len=TEXT_LEN) :: flags = ''
        character(len=TEXT_LEN) :: request_flags = ''
        character(len=512) :: test_dir = ''
        character(len=512), allocatable :: roots(:)
        logical :: tests_ready = .false.
        logical :: apps_ready = .false.
        integer(c_long_long) :: in_sum = 0
        integer(c_long_long) :: in_mixed = 0
        integer(c_long_long) :: in_count = 0
        integer(c_long_long) :: out_sum = 0
        integer(c_long_long) :: out_mixed = 0
        integer(c_long_long) :: out_count = 0
    end type stamp_t

    public :: build_stamp_matches, build_stamp_quick_matches, build_stamp_save

contains

    subroutine build_stamp_matches(project_dir, compiler, flags, dep_roots, &
            n_dep_roots, matches, tests_ready, apps_ready, test_dir)
        character(len=*), intent(in) :: project_dir, compiler, flags
        character(len=*), intent(in) :: dep_roots(:)
        integer, intent(in) :: n_dep_roots
        logical, intent(out) :: matches
        logical, intent(out), optional :: tests_ready
        logical, intent(out), optional :: apps_ready
        character(len=*), intent(out), optional :: test_dir

        type(stamp_t) :: stamp
        integer(c_long_long) :: in_sum, in_mixed, in_count
        integer(c_long_long) :: out_sum, out_mixed, out_count
        integer :: i
        logical :: ok

        matches = .false.
        if (present(tests_ready)) tests_ready = .false.
        if (present(apps_ready)) apps_ready = .false.
        if (present(test_dir)) test_dir = ''
        call read_stamp(project_dir, stamp, ok)
        if (.not. ok) return
        if (trim(stamp%compiler) /= trim(compiler)) return
        if (trim(stamp%flags) /= trim(flags)) return
        if (size(stamp%roots) /= n_dep_roots) return
        do i = 1, n_dep_roots
            if (trim(stamp%roots(i)) /= trim(dep_roots(i))) return
        end do

        call input_fingerprint(project_dir, dep_roots, n_dep_roots, in_sum, &
            in_mixed, in_count, ok)
        if (.not. ok) return
        call fs_tree_fingerprint(trim(project_dir)//'/build/fo', .false., &
            out_sum, out_mixed, out_count, ok)
        if (.not. ok .or. out_count == 0) return
        call fingerprints_match(stamp, in_sum, in_mixed, in_count, out_sum, &
            out_mixed, out_count, matches)
        if (matches .and. present(tests_ready)) tests_ready = stamp%tests_ready
        if (matches .and. present(apps_ready)) apps_ready = stamp%apps_ready
        if (matches .and. present(test_dir)) test_dir = stamp%test_dir
    end subroutine build_stamp_matches

    subroutine build_stamp_quick_matches(project_dir, compiler, request_flags, &
            matches, tests_ready, apps_ready, test_dir)
        character(len=*), intent(in) :: project_dir, compiler, request_flags
        logical, intent(out) :: matches
        logical, intent(out), optional :: tests_ready
        logical, intent(out), optional :: apps_ready
        character(len=*), intent(out), optional :: test_dir

        type(stamp_t) :: stamp
        integer(c_long_long) :: in_sum, in_mixed, in_count
        integer(c_long_long) :: out_sum, out_mixed, out_count
        logical :: ok

        matches = .false.
        if (present(tests_ready)) tests_ready = .false.
        if (present(apps_ready)) apps_ready = .false.
        if (present(test_dir)) test_dir = ''
        call read_stamp(project_dir, stamp, ok)
        if (.not. ok) return
        if (trim(stamp%compiler) /= trim(compiler)) return
        if (trim(stamp%request_flags) /= trim(request_flags)) return
        call input_fingerprint(project_dir, stamp%roots, size(stamp%roots), &
            in_sum, in_mixed, in_count, ok)
        if (.not. ok) return
        call fs_tree_fingerprint(trim(project_dir)//'/build/fo', .false., &
            out_sum, out_mixed, out_count, ok)
        if (.not. ok .or. out_count == 0) return
        call fingerprints_match(stamp, in_sum, in_mixed, in_count, out_sum, &
            out_mixed, out_count, matches)
        if (matches .and. present(tests_ready)) tests_ready = stamp%tests_ready
        if (matches .and. present(apps_ready)) apps_ready = stamp%apps_ready
        if (matches .and. present(test_dir)) test_dir = stamp%test_dir
    end subroutine build_stamp_quick_matches

    subroutine build_stamp_save(project_dir, compiler, flags, request_flags, &
            dep_roots, n_dep_roots, tests_ready, apps_ready, test_dir)
        character(len=*), intent(in) :: project_dir, compiler, flags, request_flags
        character(len=*), intent(in) :: dep_roots(:)
        integer, intent(in) :: n_dep_roots
        logical, intent(in) :: tests_ready, apps_ready
        character(len=*), intent(in) :: test_dir

        type(stamp_t) :: stamp
        character(len=512) :: file, tmpfile
        integer(c_long_long) :: in_sum, in_mixed, in_count
        integer(c_long_long) :: out_sum, out_mixed, out_count
        integer :: u, ios, rc
        logical :: ok

        call input_fingerprint(project_dir, dep_roots, n_dep_roots, in_sum, &
            in_mixed, in_count, ok)
        if (.not. ok) return
        call fs_tree_fingerprint(trim(project_dir)//'/build/fo', .false., &
            out_sum, out_mixed, out_count, ok)
        if (.not. ok .or. out_count == 0) return
        call stamp_file(project_dir, file)
        if (len_trim(file) == 0) return
        call make_sibling_tmpfile(file, tmpfile)
        open (newunit=u, file=trim(tmpfile), access='stream', form='unformatted', &
            status='replace', action='write', iostat=ios)
        if (ios /= 0) return
        stamp%project = project_dir
        stamp%compiler = compiler
        stamp%flags = flags
        stamp%request_flags = request_flags
        stamp%tests_ready = tests_ready
        stamp%apps_ready = apps_ready
        stamp%test_dir = test_dir
        allocate (stamp%roots(n_dep_roots))
        if (n_dep_roots > 0) stamp%roots = dep_roots(1:n_dep_roots)
        stamp%in_sum = in_sum
        stamp%in_mixed = in_mixed
        stamp%in_count = in_count
        stamp%out_sum = out_sum
        stamp%out_mixed = out_mixed
        stamp%out_count = out_count
        write (u, iostat=ios) STAMP_MAGIC, stamp%project, stamp%compiler, &
            stamp%flags, stamp%request_flags, stamp%tests_ready, &
            stamp%apps_ready, stamp%test_dir, n_dep_roots
        if (ios == 0 .and. n_dep_roots > 0) write (u, iostat=ios) stamp%roots
        if (ios == 0) write (u, iostat=ios) in_sum, in_mixed, in_count, &
            out_sum, out_mixed, out_count
        close (u)
        if (ios /= 0) then
            call delete_tmpfile(tmpfile)
            return
        end if
        rc = fs_rename(tmpfile, file)
        if (rc /= 0) call delete_tmpfile(tmpfile)
    end subroutine build_stamp_save

    subroutine read_stamp(project_dir, stamp, ok)
        character(len=*), intent(in) :: project_dir
        type(stamp_t), intent(out) :: stamp
        logical, intent(out) :: ok

        character(len=512) :: file
        character(len=16) :: magic
        integer :: u, ios, n_roots

        ok = .false.
        call stamp_file(project_dir, file)
        if (len_trim(file) == 0) return
        open (newunit=u, file=trim(file), access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (u, iostat=ios) magic, stamp%project, stamp%compiler, stamp%flags, &
            stamp%request_flags, stamp%tests_ready, stamp%apps_ready, &
            stamp%test_dir, n_roots
        if (ios /= 0) then
            close (u)
            return
        end if
        if (magic /= STAMP_MAGIC .or. n_roots < 0 .or. n_roots > 1024) then
            close (u)
            return
        end if
        allocate (stamp%roots(n_roots))
        if (n_roots > 0) read (u, iostat=ios) stamp%roots
        if (ios == 0) read (u, iostat=ios) stamp%in_sum, stamp%in_mixed, &
            stamp%in_count, stamp%out_sum, stamp%out_mixed, stamp%out_count
        close (u)
        if (ios /= 0) return
        ok = trim(stamp%project) == trim(project_dir)
    end subroutine read_stamp

    subroutine fingerprints_match(stamp, in_sum, in_mixed, in_count, out_sum, &
            out_mixed, out_count, matches)
        type(stamp_t), intent(in) :: stamp
        integer(c_long_long), intent(in) :: in_sum, in_mixed, in_count
        integer(c_long_long), intent(in) :: out_sum, out_mixed, out_count
        logical, intent(out) :: matches

        matches = in_sum == stamp%in_sum .and. &
            in_mixed == stamp%in_mixed .and. in_count == stamp%in_count .and. &
            out_sum == stamp%out_sum .and. out_mixed == stamp%out_mixed .and. &
            out_count == stamp%out_count
    end subroutine fingerprints_match

    subroutine input_fingerprint(project_dir, dep_roots, n_dep_roots, sum, &
            mixed, count, ok)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(in) :: dep_roots(:)
        integer, intent(in) :: n_dep_roots
        integer(c_long_long), intent(out) :: sum, mixed, count
        logical, intent(out) :: ok

        integer(c_long_long) :: part_sum, part_mixed, part_count
        integer :: i
        logical :: part_ok

        call fs_tree_fingerprint(project_dir, .true., sum, mixed, count, ok)
        if (.not. ok) return
        do i = 1, n_dep_roots
            call fs_tree_fingerprint(dep_roots(i), .true., part_sum, part_mixed, &
                part_count, part_ok)
            if (.not. part_ok) then
                ok = .false.
                return
            end if
            sum = sum + part_sum
            mixed = ieor(mixed, part_mixed)
            count = count + part_count
        end do
    end subroutine input_fingerprint

    subroutine stamp_file(project_dir, path)
        character(len=*), intent(in) :: project_dir
        character(len=*), intent(out) :: path

        character(len=512) :: cache_root, directory
        character(len=16) :: key
        integer(8) :: hash

        path = ''
        call get_environment_variable('FO_CACHE_DIR', cache_root)
        if (len_trim(cache_root) == 0) then
            call get_environment_variable('HOME', cache_root)
            if (len_trim(cache_root) == 0) return
            cache_root = trim(cache_root)//'/.cache/fo'
        end if
        directory = trim(cache_root)//'/build-state/v1'
        call fs_make_dir(directory)
        hash = fnv1a_string(trim(project_dir))
        write (key, '(z16.16)') hash
        path = trim(directory)//'/'//key
    end subroutine stamp_file

end module fo_build_stamp
