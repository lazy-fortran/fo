program test_6
    use many_tests_lib, only: add
    implicit none
    if (add(6, 1) /= 7) error stop 'test_6 failed'
    print *, 'test_6: pass'
end program test_6
