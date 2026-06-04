program test_3
    use many_tests_lib, only: add
    implicit none
    if (add(3, 1) /= 4) error stop 'test_3 failed'
    print *, 'test_3: pass'
end program test_3
