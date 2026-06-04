program test_1
    use many_tests_lib, only: add
    implicit none
    if (add(1, 1) /= 2) error stop 'test_1 failed'
    print *, 'test_1: pass'
end program test_1
