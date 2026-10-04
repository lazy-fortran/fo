program test_harness_failure_evidence
    use fo_test_harness, only: make_scratch, exercise_failure_cleanup_probe
    use fo_test_harness, only: finish_assertions
    implicit none

    character(:), allocatable :: scratch

    call make_scratch('fo-harness-evidence-oracle', scratch)
    call exercise_failure_cleanup_probe(scratch//'/default.marker')
    call exercise_failure_cleanup_probe(scratch//'/retained.marker', &
        retain_failed_scratch=.true.)
    call finish_assertions()
end program test_harness_failure_evidence
