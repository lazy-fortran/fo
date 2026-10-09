program test_test_os_link
    use fo_test_os_link, only: test_os_initialize
    implicit none

    if (test_os_initialize() /= 162) error stop 'test OS C shim did not link'
    write (*, '(a)') 'test OS C shim linkage passes'
end program test_test_os_link
