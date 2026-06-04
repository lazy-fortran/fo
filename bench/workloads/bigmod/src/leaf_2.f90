module bigmod_leaf_2
    use bigmod_core, only: core_compute
    implicit none
contains
    function leaf_2_run(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = core_compute(x) + 2
    end function leaf_2_run
end module bigmod_leaf_2
