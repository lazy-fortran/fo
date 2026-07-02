module fo_exec_target
    use fo_build_backend, only: backend_t
    implicit none
    private

    public :: resolve_exec_target

contains

    subroutine resolve_exec_target(b, target, bin_path, found)
        type(backend_t), intent(in) :: b
        character(len=*), intent(in) :: target
        character(len=*), intent(out) :: bin_path
        logical, intent(out) :: found

        bin_path = trim(b%project_dir)//'/build/fo/bin/'//trim(target)
        inquire (file=trim(bin_path), exist=found)
    end subroutine resolve_exec_target

end module fo_exec_target
