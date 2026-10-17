program test_gremlin_campaign_resolution
    !! Independent input mutations and durable receipt sequences distinguish
    !! an implementation repair from replacing the oracle that observed it.
    use fo_test_harness, only: assert_true, assert_equal_string, finish_assertions
    use fo_test_harness, only: make_directory
    use fo_fs, only: fs_remove_tree
    use fo_cache, only: HASH_LEN
    use fo_gremlin_case_inputs, only: generation_oracle_digests_ensure, &
        generation_oracle_digest, generation_case_digests_ensure, generation_case_digest
    use fo_gremlin_generation, only: generation_t
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_state_dir
    use fo_gremlin_session, only: gremlin_get_session_journal_path
    use fo_test_gremlin_oracle, only: gremlin_setup
    use fo_gremlin_receipts, only: campaign_failures, campaign_failure_t, &
        campaign_failure_resolution, prior_generation_case_passed
    use fo_gremlin_journal, only: journal_append, JOURNAL_OK
    use fo_gremlin_lifecycle, only: gremlin_lifecycle_event_t, &
        gremlin_lifecycle_append, LIFECYCLE_OK
    use fo_project_model, only: model_init, model_add_target, model_add_case, &
        model_finalize, TARGET_EXECUTABLE, TARGET_LIBRARY
    use fx_string, only: string_t
    implicit none

    type(generation_t) :: initial, changed
    type(gremlin_session_t) :: session, read_session
    type(campaign_failure_t), allocatable :: failures(:)
    character(len=512) :: message
    character(len=4096) :: lane_dir, journal_path
    character(len=:), allocatable :: driver, scratch, project, cache, state, lifecycle_path
    character(len=HASH_LEN) :: oracle, closure
    character(len=256) :: resolves
    character(len=HASH_LEN), parameter :: first = repeat('a', HASH_LEN)
    character(len=HASH_LEN), parameter :: second = repeat('b', HASH_LEN)
    character(len=HASH_LEN), parameter :: third = repeat('c', HASH_LEN)
    logical :: passed
    integer :: ierr, i, target
    character(len=:), allocatable :: detail

    call model_fixture(initial)
    call generation_oracle_digests_ensure(initial, '/fixture')
    call generation_case_digests_ensure(initial, '/fixture')
    oracle = generation_oracle_digest(initial, 'probe')
    closure = generation_case_digest(initial, 'probe')
    call assert_true(len_trim(oracle) == HASH_LEN, 'native oracle is identified')
    changed = initial
    changed%input_inventory%entries(1)%content_digest = repeat('9', HASH_LEN)
    call refresh(changed)
    call assert_equal_string(generation_oracle_digest(changed, 'probe'), oracle, &
        'implementation repair preserves the oracle identity')
    call assert_true(generation_case_digest(changed, 'probe') /= closure, &
        'implementation repair still invalidates PASS reuse')
    changed = initial
    changed%input_inventory%entries(3)%content_digest = repeat('9', HASH_LEN)
    call refresh(changed)
    call assert_equal_string(generation_oracle_digest(changed, 'probe'), oracle, &
        'an unrelated test edit leaves this oracle compatible')
    do i = 2, 4, 2
        changed = initial
        changed%input_inventory%entries(i)%content_digest = repeat('9', HASH_LEN)
        call refresh(changed)
        call assert_true(generation_oracle_digest(changed, 'probe') /= oracle, &
            'test or linked test helper replacement changes oracle identity')
    end do
    changed = initial
    changed%toolchain_key = repeat('9', HASH_LEN)
    call refresh(changed)
    call assert_true(generation_oracle_digest(changed, 'probe') /= oracle, &
        'compiler and configuration identity belongs to the oracle')
    changed = initial
    changed%model%backend = 'cmake'
    call refresh(changed)
    call assert_equal_string(generation_oracle_digest(changed, 'probe'), '', &
        'undifferentiated CMake roles remain conservatively unknown')

    call gremlin_setup(driver, scratch, project, cache, state)
    session%project_key = project
    session%lane_id = 'campaign-resolution'
    session%session_id = 'fixture-owner'
    call gremlin_session_state_dir(project, session%lane_id, lane_dir, ierr, message)
    call assert_true(ierr == 0, 'resolves the canonical campaign lane directory')
    if (ierr /= 0) call finish_assertions()
    session%state_dir = trim(lane_dir)
    call make_directory(session%state_dir)
    call gremlin_get_session_journal_path(project, session%lane_id, &
        session%session_id, journal_path, ierr, message)
    call assert_true(ierr == 0, 'resolves the separate canonical session journal')
    if (ierr /= 0) call finish_assertions()
    read_session = session
    read_session%state_dir = journal_path(:len_trim(journal_path) - len('/journal.jsonl'))
    call make_directory(read_session%state_dir)
    lifecycle_path = read_session%state_dir//'/lifecycle.jsonl'
    call assert_true(read_session%state_dir /= session%state_dir, &
        'owner and reader fixtures use the actual distinct journal layout')
    call lineage(first, second)
    call lineage(second, third)
    call receipt('failure', first, oracle, 'FAIL')
    call receipt('same-generation-pass', first, oracle, 'PASS')
    call campaign_failures(session, failures, ierr, message)
    call assert_true(ierr == JOURNAL_OK .and. size(failures) == 1, &
        'reads durable failure evidence')
    call assert_true(failures(1)%pending, 'same-generation PASS cannot claim a repair')
    call campaign_failure_resolution(session, second, 'probe', oracle, resolves, &
        ierr, message)
    call assert_equal_string(resolves, 'failure', &
        'a direct descendant with the same oracle links the exact failure')
    call campaign_failure_resolution(session, second, 'probe', repeat('9', HASH_LEN), &
        resolves, ierr, message)
    call assert_equal_string(resolves, '', 'a replaced oracle cannot claim repair')
    call receipt('changed-oracle-pass', second, repeat('9', HASH_LEN), 'PASS')
    call campaign_failures(session, failures, ierr, message)
    call assert_true(failures(1)%pending, &
        'incompatible PASS leaves old evidence pending')
    initial%identity = second
    call prior_generation_case_passed(read_session, initial, 'probe', passed, ierr, message)
    call assert_true(passed, 'compatible ancestral PASS permits regression attribution')
    changed = initial
    changed%oracle_digests = repeat('8', HASH_LEN)
    call prior_generation_case_passed(read_session, changed, 'probe', passed, ierr, message)
    call assert_true(.not. passed, &
        'incompatible earlier PASS cannot confirm regression')
    call receipt('repair', second, oracle, 'PASS')
    call campaign_failures(session, failures, ierr, message)
    call assert_true(.not. failures(1)%pending, &
        'compatible descendant PASS clears history')
    call receipt('late-old-failure', first, oracle, 'FAIL')
    call campaign_failures(session, failures, ierr, message)
    call assert_true(.not. failures(1)%pending, &
        'delayed ancestor FAIL cannot resurrect debt')
    call campaign_failure_resolution(session, third, 'probe', oracle, resolves, &
        ierr, message)
    call assert_equal_string(resolves, '', &
        'an already repaired failure is not relinked')
    call fs_remove_tree(scratch, ierr)
    call assert_true(ierr == 0, 'removes owned receipt fixture')
    call finish_assertions()

contains

    subroutine model_fixture(generation)
        type(generation_t), intent(out) :: generation

        allocate (generation%model, generation%units(4))
        call model_init(generation%model, 'fpm', 'fixture')
        call model_add_target(generation%model, 'core', TARGET_LIBRARY, &
            sources=[string_t('/fixture/src/core.f90')])
        call model_add_target(generation%model, 'helper', TARGET_LIBRARY, &
            sources=[string_t('/fixture/test/helper.f90')])
        call model_add_target(generation%model, 'probe', TARGET_EXECUTABLE, &
            sources=[string_t('/fixture/test/probe.f90')], &
            link_deps=[string_t('core'), string_t('helper')], index=target)
        call model_add_case(generation%model, 'probe', command_target=target)
        call model_add_target(generation%model, 'other', TARGET_EXECUTABLE, &
            sources=[string_t('/fixture/test/other.f90')], index=target)
        call model_add_case(generation%model, 'other', command_target=target)
        call model_finalize(generation%model, ierr, detail)
        call assert_true(ierr == 0, 'fixture model is valid')
        do i = 1, 4
            allocate (generation%units(i)%provides(0), generation%units(i)%uses(0))
        end do
        generation%units(1)%path = '/fixture/src/core.f90'
        generation%units(1)%provides = [string_t('core')]
        generation%units(2)%path = '/fixture/test/probe.f90'
        generation%units(2)%other_units = .true.
        generation%units(2)%uses = [string_t('core'), string_t('helper')]
        generation%units(3)%path = '/fixture/test/other.f90'
        generation%units(3)%other_units = .true.
        generation%units(4)%path = '/fixture/test/helper.f90'
        generation%units(4)%provides = [string_t('helper')]
        generation%model_root = '/fixture'
        generation%input_inventory_ready = .true.
        generation%input_inventory_complete = .true.
        generation%toolchain_key = repeat('d', HASH_LEN)
        generation%driver_digest = repeat('e', HASH_LEN)
        allocate (generation%input_inventory%entries(4))
        generation%input_inventory%entry_count = 4
        do i = 1, 4
            associate (entry => generation%input_inventory%entries(i))
                entry%root_alias = 'project'
                entry%relative_path = generation%units(i)%path(10:)
                entry%role = 'test-oracle'
                if (i == 1) entry%role = 'build-source'
                entry%content_digest = repeat('0', HASH_LEN)
            end associate
        end do
    end subroutine model_fixture

    subroutine refresh(generation)
        type(generation_t), intent(inout) :: generation

        if (allocated(generation%oracle_digests)) deallocate (generation%oracle_digests)
        if (allocated(generation%case_digests)) deallocate (generation%case_digests)
        call generation_oracle_digests_ensure(generation, '/fixture')
        call generation_case_digests_ensure(generation, '/fixture')
    end subroutine refresh

    subroutine lineage(parent, child)
        character(len=*), intent(in) :: parent, child
        type(gremlin_lifecycle_event_t) :: event

        event%event_id = child
        event%event_type = 'generation_superseded'
        event%generation = child
        event%previous_generation = parent
        call gremlin_lifecycle_append(lifecycle_path, event, &
            ierr, message)
        call assert_true(ierr == LIFECYCLE_OK, 'writes real generation lineage')
    end subroutine lineage

    subroutine receipt(id, generation, identity, verdict)
        character(len=*), intent(in) :: id, generation, identity, verdict
        character(len=:), allocatable :: record, outcome

        outcome = 'fail'
        if (verdict == 'PASS') outcome = 'pass'
        record = '{"completion_id":"'//id//'","case_id":"probe",'// &
            '"generation":"'//generation//'","oracle_identity":"'// &
            identity//'","status":"'//verdict//'","outcome":"'//outcome//'"}'
        call journal_append(session%state_dir//'/campaign-journal.jsonl', id, record, &
            ierr, message)
        call assert_true(ierr == JOURNAL_OK, 'writes actual durable receipt')
    end subroutine receipt

end program test_gremlin_campaign_resolution
