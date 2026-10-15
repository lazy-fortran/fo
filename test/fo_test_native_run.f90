module fo_test_native_run
    !! Build the native test binaries, then run them: the sequence the backend
    !! performs, for tests that exercise the gfortran build and run path directly.
    use fo_gfortran_build, only: gfortran_test, gfortran_run_tests
    implicit none
    private
    public :: native_test_run

contains

    subroutine native_test_run(project_dir, log_file, exitcode, include_slow, &
            n_compiled, flags, use_cache, names, n_names)
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        integer, intent(out), optional :: n_compiled
        character(len=*), intent(in), optional :: flags
        logical, intent(in), optional :: use_cache
        character(len=*), intent(in), optional :: names(:)
        integer, intent(in), optional :: n_names

        character(len=128) :: no_names(1)
        logical :: slow
        integer :: n_sel

        slow = .false.
        if (present(include_slow)) slow = include_slow
        no_names = ''
        n_sel = 0
        if (present(names) .and. present(n_names)) n_sel = n_names
        call gfortran_test(project_dir, log_file, exitcode, include_slow=slow, &
            n_compiled=n_compiled, flags=flags, use_cache=use_cache, &
            names=names, n_names=n_names)
        if (exitcode /= 0) return
        if (n_sel > 0) then
            call gfortran_run_tests(project_dir, log_file, exitcode, slow, &
                names, n_sel, flags=flags)
        else
            call gfortran_run_tests(project_dir, log_file, exitcode, slow, &
                no_names, 0, flags=flags)
        end if
    end subroutine native_test_run

end module fo_test_native_run
