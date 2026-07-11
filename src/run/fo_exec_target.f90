module fo_exec_target
    use fo_build_backend, only: backend_t, BACKEND_CMAKE
    use fo_fs, only: fs_collect_files
    implicit none
    private

    public :: resolve_exec_target

contains

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
