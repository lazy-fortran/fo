program test_gremlin_host_slots
    !! Host work-slot admission weighs lanes by their case children: parallel
    !! lanes share a pool of FO_GREMLIN_HOST_SLOTS slots instead of the first
    !! parallel lane excluding every other lane, a lane asking for more than
    !! the pool is clamped to it, and the pool defaults to the processor count.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup
    use fo_gremlin_state, only: gremlin_session_t, gremlin_lease_group_t, &
        LEASE_GROUP_MAX
    use fo_gremlin_campaign, only: acquire_heavy_work_slot, release_heavy_work_slot, &
        gremlin_host_slots
    use fo_process, only: process_detect_nproc
    implicit none

    interface
        integer(c_int) function setenv(name, value, overwrite) &
                bind(C, name='fo_test_setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function setenv
    end interface

    character(:), allocatable :: driver, scratch, project, cache, state
    type(gremlin_session_t) :: session
    type(gremlin_lease_group_t) :: first, second, third, wide
    logical :: have_first, have_second, have_third, have_wide
    logical :: stop_requested, waits
    character(len=512) :: message
    integer :: ierr

    call gremlin_setup(driver, scratch, project, cache, state)
    call set('FO_GREMLIN_HOST_SLOTS', '4')
    call assert_true(gremlin_host_slots() == 4, 'FO_GREMLIN_HOST_SLOTS sets the pool')

    have_first = .false.
    have_second = .false.
    have_third = .false.
    have_wide = .false.

    call take(2, first, have_first)
    call assert_true(have_first .and. first%count == 2, &
        'a two-job lane reserves two slots: '//trim(message))
    call take(2, second, have_second)
    call assert_true(have_second .and. second%count == 2, &
        'a second two-job lane runs beside the first: '//trim(message))
    call take(1, third, have_third)
    call assert_true(.not. have_third .and. waits .and. ierr == 0, &
        'a lane waits while the pool is full: '//trim(message))

    call release_heavy_work_slot(first, have_first, ierr, message, drain=.true.)
    call assert_true(ierr == 0 .and. .not. have_first, 'first lane releases')
    call take(1, third, have_third)
    call assert_true(have_third .and. third%count == 1, &
        'a released slot admits the waiting lane: '//trim(message))
    call release_heavy_work_slot(second, have_second, ierr, message, drain=.true.)
    call release_heavy_work_slot(third, have_third, ierr, message, drain=.true.)

    call take(16, wide, have_wide)
    call assert_true(have_wide .and. wide%count == 4, &
        'a lane wider than the pool is clamped to the pool: '//trim(message))
    call release_heavy_work_slot(wide, have_wide, ierr, message, drain=.true.)

    call set('FO_GREMLIN_HOST_SLOTS', '')
    call assert_true(gremlin_host_slots() == &
        max(1, min(LEASE_GROUP_MAX, process_detect_nproc())), &
        'the pool defaults to the processor count')
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine take(jobs, lease, held)
        !! Acquire as an independent lane: no inherited admission scope.
        integer, intent(in) :: jobs
        type(gremlin_lease_group_t), intent(inout) :: lease
        logical, intent(inout) :: held

        call set('FO_GREMLIN_ADMISSION_SCOPE', '')
        call acquire_heavy_work_slot(session, jobs, lease, held, .false., &
            stop_requested, ierr, message, waits)
        call set('FO_GREMLIN_ADMISSION_SCOPE', '')
    end subroutine take

    subroutine set(name, value)
        character(len=*), intent(in) :: name, value

        call assert_true(setenv(name//c_null_char, value//c_null_char, 1_c_int) == 0, &
            'sets '//name)
    end subroutine set

end program test_gremlin_host_slots
