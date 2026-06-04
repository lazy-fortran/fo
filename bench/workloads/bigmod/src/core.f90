module bigmod_core
    implicit none
    integer, parameter :: CORE_VERSION = 1
contains
    function core_compute(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x * 2 + CORE_VERSION
    end function core_compute
end module bigmod_core
