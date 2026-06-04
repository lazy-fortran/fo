program test_2
    use many_tests_lib, only: add
    implicit none
    if (add(2, 1) /= 3) error stop 'test_2 failed'
    print *, 'test_2: pass'
end program test_2
