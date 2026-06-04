program test_4
    use many_tests_lib, only: add
    implicit none
    if (add(4, 1) /= 5) error stop 'test_4 failed'
    print *, 'test_4: pass'
end program test_4
