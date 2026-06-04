module broken_mod
    implicit none
contains
    function broken_func(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x + undefined_var
    end function broken_func
end module broken_mod
