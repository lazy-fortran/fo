module fo_test_random
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    public :: select_random_tests

contains

    subroutine select_random_tests( &
            names, n_names, requested, seed, selected, n_selected)
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names, requested, seed
        character(len=*), intent(out) :: selected(:)
        integer, intent(out) :: n_selected

        character(len=len(names)) :: temporary
        character(len=len(names)), allocatable :: shuffled(:)
        integer :: i, j
        integer(int64) :: state

        n_selected = 0
        if (n_names < 1 .or. requested < 1) return
        if (n_names > size(names) .or. &
            size(selected) < min(n_names, requested)) return
        allocate(shuffled(n_names))
        shuffled = names(:n_names)
        state = modulo(int(seed, int64), 2147483646_int64) + 1_int64
        do i = n_names, 2, -1
            call park_miller_step(state)
            j = 1 + int(modulo(state, int(i, int64)))
            temporary = shuffled(i)
            shuffled(i) = shuffled(j)
            shuffled(j) = temporary
        end do
        n_selected = min(n_names, requested)
        selected(:n_selected) = shuffled(:n_selected)
    end subroutine select_random_tests

    pure subroutine park_miller_step(state)
        integer(int64), intent(inout) :: state

        integer(int64) :: quotient

        quotient = state/127773_int64
        state = 16807_int64*(state - quotient*127773_int64) - &
            2836_int64*quotient
        if (state <= 0_int64) state = state + 2147483647_int64
    end subroutine park_miller_step

end module fo_test_random
