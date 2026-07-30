program test_random_tests
    use fo_test_random, only: select_random_tests
    implicit none

    character(len=16) :: names(8), first(8), replay(8), different(8)
    integer :: i, n_first, n_replay, n_different

    names = [character(len=16) :: &
        "alpha", "beta", "gamma", "delta", &
        "epsilon", "zeta", "eta", "theta"]
    call select_random_tests(names, 8, 4, 1729, first, n_first)
    call select_random_tests(names, 8, 4, 1729, replay, n_replay)
    call select_random_tests(names, 8, 4, 1730, different, n_different)
    if (n_first /= 4 .or. n_replay /= 4 .or. n_different /= 4) error stop 1
    if (any(first(:4) /= replay(:4))) error stop 2
    if (all(first(:4) == different(:4))) error stop 3
    do i = 1, 4
        if (count(first(:4) == first(i)) /= 1) error stop 4
        if (.not. any(names == first(i))) error stop 5
    end do

end program test_random_tests
