program test_gremlin_admission_gc
    !! Admission directory hygiene: guardian markers and scope records of dead
    !! owners are collected on the next acquisition, while live entries and
    !! entries whose owner cannot be proven dead are left alone.
    use fo_test_harness, only: write_text, file_exists, make_directory, join_path
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup
    use fo_gremlin_state, only: gremlin_lease_group_t, &
        gremlin_host_lease_acquire_weighted, gremlin_host_lease_release
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, base
    character(:), allocatable :: dead_scope, junk_scope, dead_marker, junk_marker
    type(gremlin_lease_group_t) :: first, second
    character(len=512) :: message
    integer :: status
    logical :: busy

    call gremlin_setup(driver, scratch, project, cache, state)
    base = join_path(scratch, 'admission')

    call gremlin_host_lease_acquire_weighted('gremlin-heavy-work', 2, 1, first, &
        busy, status, message)
    call assert_true(status == 0 .and. .not. busy, 'live owner is admitted')
    if (status /= 0 .or. busy) call finish_assertions(retain_failed_scratch=.true.)

    ! Owner identity 999999 is beyond any pid and cannot match a process.
    dead_marker = join_path(join_path(base, 'guardians'), '999999-12345')
    junk_marker = join_path(join_path(base, 'guardians'), 'unrelated-entry')
    call write_text(dead_marker, '')
    call write_text(junk_marker, '')
    dead_scope = join_path(join_path(base, 'scopes'), '999999-DEAD00')
    junk_scope = join_path(join_path(base, 'scopes'), '999999-JUNK00')
    call make_directory(dead_scope)
    call write_text(join_path(dead_scope, 'gate'), '')
    call write_text(join_path(dead_scope, 'authority'), &
        '999999 12345 1 abcdef'//new_line('a')//'-'//new_line('a'))
    call make_directory(junk_scope)
    call write_text(join_path(junk_scope, 'authority'), 'not an owner record')

    call gremlin_host_lease_acquire_weighted('gremlin-heavy-work', 2, 1, second, &
        busy, status, message)
    call assert_true(status == 0 .and. .not. busy, 'a second owner is admitted')
    call assert_true(.not. file_exists(dead_marker), &
        'guardian marker of a dead owner is collected')
    call assert_true(.not. file_exists(join_path(dead_scope, 'authority')), &
        'scope of a dead owner is collected')
    call assert_true(file_exists(junk_marker), &
        'an entry that is not a guardian marker is kept')
    call assert_true(file_exists(join_path(junk_scope, 'authority')), &
        'a scope whose record cannot be read is kept')
    call assert_true(file_exists(join_path(first%scope, 'authority')), &
        'the scope of a live owner is kept')

    if (status == 0) call gremlin_host_lease_release(second, status, message)
    call gremlin_host_lease_release(first, status, message)
    call finish_assertions(retain_failed_scratch=.true.)
end program test_gremlin_admission_gc
