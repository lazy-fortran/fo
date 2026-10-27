program test_gremlin_case_inventory
    use fx_string, only: string_t
    use fo_project_model, only: project_model_t, model_listing_t, model_init, &
        model_add_case, model_expand_cases, model_find_case
    use fo_project_load, only: project_inventory_cases
    use fo_test_harness, only: assert_true, finish_assertions
    implicit none

    type(project_model_t) :: model
    type(model_listing_t) :: listing(1)
    integer, allocatable :: cases(:)
    character(:), allocatable :: message
    integer :: ierr

    call model_init(model, 'native', 'inventory-fixture')
    call model_add_case(model, 'multi')
    call model_add_case(model, 'ordinary', labels=[string_t('slow')])
    call model_add_case(model, 'dispatcher', explicit_only=.true.)
    call model_add_case(model, 'preset-excluded', default_excluded=.true.)
    listing(1)%parent = 1
    listing(1)%names = [string_t('one'), string_t('two')]
    listing(1)%sources = [string_t(''), string_t('')]
    call model_expand_cases(model, listing, ierr, message)
    call assert_true(ierr == 0, 'expand executable cases: '//message)
    call project_inventory_cases(model, cases)
    call assert_true(size(cases) == 3, 'campaign has two children and ordinary test')
    call assert_true(.not. any(cases == model_find_case(model, 'multi')), &
        'expanded executable is not scheduled beside its children')
    call assert_true(any(cases == model_find_case(model, 'multi::one')) .and. &
        any(cases == model_find_case(model, 'multi::two')), &
        'every listed child remains in the campaign')
    call assert_true(any(cases == model_find_case(model, 'ordinary')), &
        'slow labels remain eligible for continuous coverage')
    call assert_true(.not. any(cases == model_find_case(model, 'dispatcher')) .and. &
        .not. any(cases == model_find_case(model, 'preset-excluded')), &
        'explicit-only and preset exclusions remain excluded')
    call finish_assertions()
end program test_gremlin_case_inventory
