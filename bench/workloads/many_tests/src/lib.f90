module many_tests_lib
    implicit none
contains
    function add(a, b) result(c)
        integer, intent(in) :: a, b
        integer :: c
        c = a + b
    end function add
end module many_tests_lib
