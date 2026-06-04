module bigmod_leaf_6
    use bigmod_core, only: core_compute
    implicit none
contains
    function leaf_6_run(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = core_compute(x) + 6
    end function leaf_6_run
end module bigmod_leaf_6
