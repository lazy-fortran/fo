program bigmod_main
    use bigmod_core, only: core_compute
    use bigmod_leaf_1, only: leaf_1_run
    implicit none
    print *, core_compute(1), leaf_1_run(1)
end program bigmod_main
