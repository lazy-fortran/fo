module bigmod_leaf_3
    use bigmod_core, only: core_compute
    implicit none
contains
    function leaf_3_run(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = core_compute(x) + 3
    end function leaf_3_run
end module bigmod_leaf_3
