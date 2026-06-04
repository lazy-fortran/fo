module bigmod_leaf_5
    use bigmod_core, only: core_compute
    implicit none
contains
    function leaf_5_run(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = core_compute(x) + 5
    end function leaf_5_run
end module bigmod_leaf_5
