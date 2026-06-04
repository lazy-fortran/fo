module bigmod_leaf_1
    use bigmod_core, only: core_compute
    implicit none
contains
    function leaf_1_run(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = core_compute(x) + 1
    end function leaf_1_run
end module bigmod_leaf_1
