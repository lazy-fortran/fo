program test_7
    use many_tests_lib, only: add
    implicit none
    if (add(7, 1) /= 8) error stop 'test_7 failed'
    print *, 'test_7: pass'
end program test_7
