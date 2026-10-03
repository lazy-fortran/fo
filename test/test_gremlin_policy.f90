program test_gremlin_policy
    use fo_gremlin_policy, only: GREMLIN_NONMANDATORY_LIMIT, &
        GREMLIN_POLICY_EMPTY_SELECTION, GREMLIN_POLICY_INVALID_COUNT, &
        GREMLIN_POLICY_INVALID_INVENTORY, GREMLIN_POLICY_OK, &
        GREMLIN_POLICY_OUTPUT_TOO_SMALL, GREMLIN_POLICY_UNKNOWN_NAME, &
        select_gremlin_tests, shuffle_gremlin_tests
    implicit none

    character(len=16) :: names(50), mandatory(3), impacted(2), history(3), debt(10)
    character(len=16) :: selected(50), replay(50), rotated(50)
    character(len=16) :: shuffled(50), shuffled_again(50), expected(18)
    character(len=16) :: expected_random(4), unknown(1), small(1), bad_names(50)
    integer :: i, n_selected, status, debt_cursor, replay_cursor
    integer :: n_mandatory_selected, replay_mandatory_count, prefix_for_shuffle

    do i = 1, size(names)
        write (names(i), '(a,i2.2)') 'case_', i
    end do
    mandatory = [character(len=16) :: 'case_01', 'case_01', 'case_02']
    impacted = [character(len=16) :: 'case_03', 'case_04']
    history = [character(len=16) :: 'case_03', 'case_45', 'case_46']
    do i = 1, size(debt)
        debt(i) = names(i + 40)
    end do
    expected = [character(len=16) :: &
        'case_01', 'case_02', 'case_03', 'case_04', 'case_45', 'case_46', &
        'case_41', 'case_42', 'case_43', 'case_44', 'case_47', 'case_48', &
        'case_08', 'case_19', 'case_34', 'case_32', 'case_40', 'case_13']
    expected_random = [character(len=16) :: &
        'case_38', 'case_46', 'case_03', 'case_21']

    ! The fixed result covers mandatory deduplication, impact/history priority,
    ! debt insertion, and seeded sampling without replacement.
    debt_cursor = 1
    call select_gremlin_tests( &
        names, size(names), mandatory, size(mandatory), impacted, size(impacted), &
        history, size(history), debt, size(debt), 6, 1729, selected, &
        n_selected, status, debt_cursor, n_mandatory_selected)
    if (status /= GREMLIN_POLICY_OK) error stop 1
    if (n_selected /= size(expected)) error stop 2
    if (any(selected(:n_selected) /= expected)) error stop 3
    if (debt_cursor /= 9) error stop 4
    if (n_mandatory_selected /= 2) error stop 37
    prefix_for_shuffle = n_mandatory_selected
    shuffled = selected
    shuffled_again = selected
    do i = 1, n_selected
        if (count(selected(:n_selected) == selected(i)) /= 1) error stop 5
    end do

    ! Replaying the same seed, inventory, policy, and initial cursor is exact.
    replay_cursor = 1
    call select_gremlin_tests( &
        names, size(names), mandatory, size(mandatory), impacted, size(impacted), &
        history, size(history), debt, size(debt), 6, 1729, replay, &
        n_selected, status, replay_cursor, replay_mandatory_count)
    if (status /= GREMLIN_POLICY_OK) error stop 6
    if (any(replay(:n_selected) /= expected)) error stop 7
    if (replay_cursor /= 9) error stop 35
    if (replay_mandatory_count /= 2) error stop 38

    ! The next request starts after the previous eight debt candidates.
    call select_gremlin_tests( &
        names, size(names), mandatory, size(mandatory), impacted, size(impacted), &
        history, size(history), debt, size(debt), 6, 1729, rotated, &
        n_selected, status, debt_cursor)
    if (status /= GREMLIN_POLICY_OK) error stop 8
    if (rotated(7) /= 'case_49' .or. rotated(8) /= 'case_50') error stop 9
    if (rotated(11) /= 'case_43' .or. rotated(12) /= 'case_44') error stop 10
    if (debt_cursor /= 7) error stop 11
    if (rotated(3) /= 'case_03' .or. rotated(4) /= 'case_04') error stop 12
    if (rotated(5) /= 'case_45' .or. rotated(6) /= 'case_46') error stop 13

    ! Random-only selection has a separate fixed oracle and seed.
    call select_gremlin_tests( &
        names, size(names), mandatory, 0, impacted, 0, history, 0, debt, 0, &
        4, 1729, selected, n_selected, status, &
        n_mandatory_selected=n_mandatory_selected)
    if (status /= GREMLIN_POLICY_OK) error stop 14
    if (n_selected /= size(expected_random)) error stop 15
    if (any(selected(:n_selected) /= expected_random)) error stop 16
    if (n_mandatory_selected /= 0) error stop 39

    ! Shuffle changes order reproducibly without changing membership.
    call shuffle_gremlin_tests( &
        shuffled, prefix_for_shuffle, size(expected), 1729, status)
    if (status /= GREMLIN_POLICY_OK) error stop 17
    call shuffle_gremlin_tests( &
        shuffled_again, prefix_for_shuffle, size(expected), 1729, status)
    if (status /= GREMLIN_POLICY_OK) error stop 18
    if (any(shuffled(:2) /= expected(:2))) error stop 19
    if (shuffled(3) == expected(3)) error stop 40
    if (any(shuffled(:size(expected)) /= shuffled_again(:size(expected)))) &
        error stop 20
    if (all(shuffled(3:size(expected)) == expected(3:size(expected)))) error stop 21
    do i = 1, size(expected)
        if (count(shuffled(:size(expected)) == expected(i)) /= 1) error stop 22
    end do

    unknown = [character(len=16) :: 'missing']
    n_mandatory_selected = 99
    call select_gremlin_tests( &
        names, size(names), unknown, size(unknown), impacted, 0, history, 0, &
        debt, 0, 0, 1, selected, n_selected, status, &
        n_mandatory_selected=n_mandatory_selected)
    if (status /= GREMLIN_POLICY_UNKNOWN_NAME) error stop 23
    if (n_mandatory_selected /= 0) error stop 41
    call select_gremlin_tests( &
        names, 0, mandatory, size(mandatory), impacted, 0, history, 0, debt, 0, &
        0, 1, selected, n_selected, status)
    if (status /= GREMLIN_POLICY_INVALID_COUNT) error stop 24
    call select_gremlin_tests( &
        names, size(names), mandatory, -1, impacted, 0, history, 0, debt, 0, &
        0, 1, selected, n_selected, status)
    if (status /= GREMLIN_POLICY_INVALID_COUNT) error stop 25
    call select_gremlin_tests( &
        names, size(names), mandatory, 0, impacted, 0, history, 0, debt, 0, &
        -1, 1, selected, n_selected, status)
    if (status /= GREMLIN_POLICY_INVALID_COUNT) error stop 26

    bad_names = names
    bad_names(3) = ''
    call select_gremlin_tests( &
        bad_names, size(bad_names), mandatory, 0, impacted, 0, history, 0, debt, 0, &
        1, 1, selected, n_selected, status)
    if (status /= GREMLIN_POLICY_INVALID_INVENTORY) error stop 27
    bad_names = names
    bad_names(2) = bad_names(1)
    call select_gremlin_tests( &
        bad_names, size(bad_names), mandatory, 0, impacted, 0, history, 0, debt, 0, &
        1, 1, selected, n_selected, status)
    if (status /= GREMLIN_POLICY_INVALID_INVENTORY) error stop 28

    call select_gremlin_tests( &
        names, size(names), mandatory, size(mandatory), impacted, 0, &
        history, 0, debt, 0, &
        0, 1, small, n_selected, status)
    if (status /= GREMLIN_POLICY_OUTPUT_TOO_SMALL) error stop 29
    call shuffle_gremlin_tests(selected, -1, 2, 1, status)
    if (status /= GREMLIN_POLICY_INVALID_COUNT) error stop 30
    call select_gremlin_tests( &
        names, size(names), mandatory, 0, impacted, 0, history, 0, debt, 0, &
        0, 1, selected, n_selected, status)
    if (status /= GREMLIN_POLICY_EMPTY_SELECTION) error stop 31

    debt_cursor = 0
    call select_gremlin_tests( &
        names, size(names), mandatory, 0, impacted, 0, history, 0, debt, &
        size(debt), 1, 1, selected, n_selected, status, debt_cursor)
    if (status /= GREMLIN_POLICY_INVALID_COUNT) error stop 32
    if (debt_cursor /= 0) error stop 33
    if (GREMLIN_NONMANDATORY_LIMIT /= 32) error stop 36
end program test_gremlin_policy
