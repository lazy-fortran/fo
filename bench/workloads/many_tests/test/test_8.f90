program test_8
    use many_tests_lib, only: add
    implicit none
    if (add(8, 1) /= 9) error stop 'test_8 failed'
    print *, 'test_8: pass'
end program test_8
