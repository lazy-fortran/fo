module bigmod_leaf_4
    use bigmod_core, only: core_compute
    implicit none
contains
    function leaf_4_run(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = core_compute(x) + 4
    end function leaf_4_run
end module bigmod_leaf_4
