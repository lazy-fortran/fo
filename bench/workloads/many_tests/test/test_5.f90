program test_5
    use many_tests_lib, only: add
    implicit none
    if (add(5, 1) /= 6) error stop 'test_5 failed'
    print *, 'test_5: pass'
end program test_5
