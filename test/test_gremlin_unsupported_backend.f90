program test_gremlin_unsupported_backend
    use fo_test_harness, only: make_scratch, write_text, assert_true, &
        finish_assertions, remove_tree
    use fo_gremlin_context, only: capture_candidate
    use fo_gremlin_generation, only: generation_t
    use fo_gremlin_state, only: gremlin_lease_t
    use fo_driver, only: driver_pin_t
    implicit none

    character(:), allocatable :: project
    character(len=4096) :: message
    type(driver_pin_t) :: driver
    type(generation_t) :: generation
    type(gremlin_lease_t) :: lease
    integer :: registration_error
    logical :: ok

    call make_scratch('fo-gremlin-unsupported', project)
    call write_text(project//'/Makefile', 'check-meta:'//new_line('a'))
    call capture_candidate(project, driver, generation, lease, ok, &
        registration_error, message)
    call assert_true(.not. ok, 'Makefile-only project cannot publish a generation')
    call assert_true(index(message, 'no supported project manifest') > 0, &
        'unsupported backend is distinguished from unavailable dependencies')
    call assert_true(index(message, 'fpm.toml or CMakeLists.txt') > 0, &
        'diagnostic names the supported project declarations')
    call remove_tree(project)
    call finish_assertions()
end program test_gremlin_unsupported_backend
