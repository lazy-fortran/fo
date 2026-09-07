module fo_exec_target
    use fo_build_backend, only: backend_t, BACKEND_CMAKE, BACKEND_NATIVE
    use fo_fs, only: fs_collect_files
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    use fo_gfortran_build, only: gfortran_app_source_name, gfortran_test_source_name
    use fo_scan, only: scan_unit_t, scan_dir
    implicit none
    private

    public :: resolve_exec_target, exec_target_is_app

contains

    logical function exec_target_is_app(b, target) result(is_app)
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: target
        type(fpm_config_t), allocatable :: config
        integer :: ierr

        is_app = .false.
        if (b%kind /= BACKEND_NATIVE) return
        allocate (config)
        call fpm_config_parse(b%project_dir, config, ierr)
        if (ierr /= 0) return
        is_app = source_dir_has_target(config, config%app_dir, target, .false.)
        if (.not. is_app) then
            if (config%auto_examples .or. config%n_examples > 0) then
                is_app = source_dir_has_target(config, config%example_dir, &
                    target, .false.)
            end if
        end if
        if (.not. is_app) is_app = source_dir_has_target(config, &
            config%source_dir, target, .false.)
        if (is_app) then
            ! Combined builds materialize tests last. Preserve that precedence
            ! when an application and a test share the same public name.
            is_app = .not. source_dir_has_target(config, config%test_dir, &
                target, .true.)
        end if
    end function exec_target_is_app

    logical function source_dir_has_target(config, source_dir, target, tests) &
            result(found)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: source_dir, target
        logical, intent(in) :: tests
        type(scan_unit_t), allocatable :: units(:)
        character(len=128) :: public_name
        integer :: i, n_units, ierr

        found = .false.
        call scan_dir(trim(config%project_dir)//'/'//trim(source_dir), &
            units, n_units, ierr)
        if (ierr /= 0) return
        do i = 1, n_units
            if (.not. units(i)%is_program) cycle
            if (tests) then
                public_name = gfortran_test_source_name(config, source_dir, &
                    units(i)%filename)
            else
                if (units(i)%is_test) cycle
                public_name = gfortran_app_source_name(config, units(i)%filename)
            end if
            if (public_name /= target) cycle
            found = .true.
            return
        end do
    end function source_dir_has_target

    subroutine resolve_exec_target(b, target, bin_path, found)
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: target
        character(len=*), intent(out) :: bin_path
        logical, intent(out) :: found

        character(len=1024) :: candidates(64)
        integer :: i, last_slash, n_candidates, n_exact

        bin_path = trim(b%project_dir)//'/build/fo/bin/'//trim(target)
        inquire (file=trim(bin_path), exist=found)
        if (found .or. b%kind /= BACKEND_CMAKE) return

        bin_path = trim(b%project_dir)//'/build/'//trim(target)
        inquire (file=trim(bin_path), exist=found)
        if (found) return

        ! CMake's conventional output directory, checked before the recursive
        ! scan below. A project that has also been built with fpm keeps a
        ! second copy under build/<compiler-hash>/app, and the scan rejects
        ! anything it finds twice. Without this probe a target that is present
        ! at the canonical path is reported as missing purely because a stale
        ! sibling build tree exists.
        bin_path = trim(b%project_dir)//'/build/bin/'//trim(target)
        inquire (file=trim(bin_path), exist=found)
        if (found) return

        last_slash = index(trim(target), '/', back=.true.)
        if (last_slash > 0) return

        call fs_collect_files(trim(b%project_dir)//'/build', '', trim(target), '', &
            candidates, n_candidates)
        n_exact = 0
        do i = 1, n_candidates
            last_slash = index(trim(candidates(i)), '/', back=.true.)
            if (trim(candidates(i)(last_slash + 1:)) /= trim(target)) cycle
            n_exact = n_exact + 1
            bin_path = candidates(i)
        end do
        found = n_exact == 1
        if (.not. found) bin_path = ''
    end subroutine resolve_exec_target

end module fo_exec_target
