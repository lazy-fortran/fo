program test_harness_failure_evidence
    use fo_test_harness, only: make_scratch, exercise_failure_cleanup_probe
    use fo_test_harness, only: finish_assertions
    use fo_test_harness, only: harness_probe_entry
    use fo_test_cli, only: exercise_harness
    implicit none

    character(:), allocatable :: scratch

    call harness_probe_entry()
    call make_scratch('fo-harness-evidence-oracle', scratch)
    call exercise_failure_cleanup_probe(scratch//'/default.marker')
    call exercise_failure_cleanup_probe(scratch//'/retained.marker', &
        retain_failed_scratch=.true.)
    call exercise_harness()
    call finish_assertions()
end program test_harness_failure_evidence
