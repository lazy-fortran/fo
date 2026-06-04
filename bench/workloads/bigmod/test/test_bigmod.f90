program test_bigmod
    use bigmod_core, only: core_compute
    use bigmod_leaf_1, only: leaf_1_run
    use bigmod_leaf_2, only: leaf_2_run
    implicit none
    if (core_compute(1) /= 3) error stop 'core_compute failed'
    if (leaf_1_run(1) /= 4) error stop 'leaf_1 failed'
    if (leaf_2_run(1) /= 5) error stop 'leaf_2 failed'
    print *, 'bigmod: pass'
end program test_bigmod
