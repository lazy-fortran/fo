program test_gremlin_large_case_inputs
    use fo_cache, only: HASH_LEN
    use fo_gremlin_case_inputs, only: generation_case_digests_ensure, &
        generation_case_digest, generation_oracle_digests_ensure, generation_oracle_digest
    use fo_gremlin_generation, only: generation_t
    use fo_project_model, only: model_init, model_add_target, model_add_case, &
        model_finalize, TARGET_EXECUTABLE
    use fx_string, only: string_t
    implicit none

    type(generation_t) :: initial, changed
    character(len=HASH_LEN) :: case_digest, oracle_digest
    character(len=:), allocatable :: message
    integer :: i, ierr, target
    type(string_t) :: sources(1)

    allocate (initial%model, initial%units(1))
    call model_init(initial%model, 'fpm', 'fixture')
    sources(1)%s = '/fixture/test/probe.f90'
    call model_add_target(initial%model, 'probe', TARGET_EXECUTABLE, &
        sources=sources, index=target)
    call model_add_case(initial%model, 'probe', command_target=target)
    call model_finalize(initial%model, ierr, message)
    call require(ierr == 0, 'valid fixture model')
    initial%units(1)%path = sources(1)%s
    initial%units(1)%other_units = .true.
    allocate (initial%units(1)%provides(0), initial%units(1)%uses(0))
    initial%model_root = '/fixture'
    initial%input_inventory_ready = .true.
    initial%input_inventory_complete = .true.
    initial%toolchain_key = repeat('d', HASH_LEN)
    initial%driver_digest = repeat('e', HASH_LEN)
    allocate (initial%input_inventory%entries(1001))
    initial%input_inventory%entry_count = 1001
    associate (entry => initial%input_inventory%entries(1))
        entry%root_alias = 'project'
        entry%relative_path = 'test/probe.f90'
        entry%role = 'test-main'
        entry%content_digest = repeat('0', HASH_LEN)
    end associate
    do i = 2, 1001
        associate (entry => initial%input_inventory%entries(i))
            entry%root_alias = 'dependency:fixture'
            write (entry%relative_path, '(a,i0)') repeat('x', 100), i
            entry%role = 'build-source'
            entry%content_digest = repeat('0', HASH_LEN)
        end associate
    end do
    call generation_case_digests_ensure(initial, '/fixture')
    call generation_oracle_digests_ensure(initial, '/fixture')
    case_digest = generation_case_digest(initial, 'probe')
    oracle_digest = generation_oracle_digest(initial, 'probe')
    call require(len_trim(case_digest) == HASH_LEN, 'case closure has a digest')
    call require(len_trim(oracle_digest) == HASH_LEN, 'oracle closure has a digest')
    print '(a)', 'case='//case_digest
    print '(a)', 'oracle='//oracle_digest

    changed = initial
    deallocate (changed%case_digests, changed%oracle_digests)
    call generation_case_digests_ensure(changed, '/fixture')
    call generation_oracle_digests_ensure(changed, '/fixture')
    call require(generation_case_digest(changed, 'probe') == case_digest, &
        'unchanged global inputs preserve the case digest')
    call require(generation_oracle_digest(changed, 'probe') == oracle_digest, &
        'unchanged global inputs preserve the oracle digest')
    changed%input_inventory%entries(1001)%content_digest = repeat('9', HASH_LEN)
    deallocate (changed%case_digests, changed%oracle_digests)
    call generation_case_digests_ensure(changed, '/fixture')
    call generation_oracle_digests_ensure(changed, '/fixture')
    call require(generation_case_digest(changed, 'probe') /= case_digest, &
        'the final global input changes the case digest')
    call require(generation_oracle_digest(changed, 'probe') /= oracle_digest, &
        'the final global input changes the oracle digest')
    print '(a)', 'PASS: large case input closures'

contains

    subroutine require(condition, detail)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: detail

        if (condition) return
        print '(a)', 'FAIL: '//detail
        error stop 1
    end subroutine require

end program test_gremlin_large_case_inputs
