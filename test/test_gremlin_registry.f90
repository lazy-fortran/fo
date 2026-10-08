program test_gremlin_registry
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, make_directory, make_symlink, write_text, read_text
    use fo_test_harness, only: file_exists, assert_true, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_gremlin_oracle, only: gremlin_json, gremlin_start_args
    use fo_test_gremlin_oracle, only: gremlin_wait_ms, gremlin_stop_lane, gremlin_field
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_boolean_value
    use fo_gremlin_state, only: gremlin_generation_lease_acquire_at, &
        gremlin_lease_t, gremlin_lease_release
    implicit none

    interface
        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
    end interface

    character(:), allocatable :: driver, scratch, project, cache, state, config
    character(:), allocatable :: registry, alternate, session, generation, runtime_value
    character(:), allocatable :: first_root, first_source, first_bytes, previous
    character(:), allocatable :: first_generation
    type(string_list_t) :: arguments, environment
    type(process_result_t) :: process
    type(json_value_t) :: started
    type(gremlin_lease_t) :: first_lease
    character(len=*), parameter :: lane = 'registry-resident-oracle'
    character(len=*), parameter :: case_id = 'test_registry_value'
    logical :: ready
    logical :: have_first_lease = .false.
    character(len=512) :: pin_message
    integer :: pin_error

    call resolve_driver(driver)
    call make_scratch('fo-gremlin-registry', scratch)
    ! Keep frozen/reproduction roots beyond the old 256-character manifest cap
    ! even when this fixture starts outside a resident execution view.
    if (len(scratch) < 160) then
        scratch = scratch//'/nested-'//repeat('r', 160 - len(scratch))
        call make_directory(scratch)
    end if
    project = scratch//'/project'
    cache = scratch//'/cache'
    state = scratch//'/state'
    config = scratch//'/config/config.toml'
    registry = scratch//'/registry'
    alternate = scratch//'/alternate'
    runtime_value = scratch//'/runtime-value.txt'
    call make_directory(project//'/test')
    call make_directory(cache)
    ! Exercise frozen roots through a directory alias on every supported host.
    call make_directory(scratch//'/state-real')
    call make_symlink(scratch//'/state-real', state)
    call make_directory(state//'/tmp')
    call set_env('FO_FPM_CONFIG_FILE', config)
    call set_env('FO_GREMLIN_STATE_DIR', state)
    call set_env('FO_CACHE_DIR', cache)
    call set_env('FO_PREFIX', scratch//'/prefix')
    call set_env('TMPDIR', state//'/tmp')
    call set_env('FO_TEST_TIMEOUT', '')
    call set_env('FO_TEST_WALL_TIMEOUT', '')
    call set_env('FO_SLOW_TEST_TIMEOUT', '')
    call set_env('FO_SLOW_TEST_WALL_TIMEOUT', '')
    call write_config(registry)
    call write_provider(registry, '1.0.0', 19)
    call write_provider(alternate, '3.0.0', 31)
    call write_text(project//'/fpm.toml', &
        'name = "registry_resident_probe"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'provider = { namespace = "demo" }'//new_line('a'))
    call write_text(project//'/test/'//case_id//'.f90', &
        'program '//case_id//new_line('a')// &
        'use provider, only: provider_value'//new_line('a')// &
        'implicit none'//new_line('a')// &
        'integer :: unit'//new_line('a')// &
        'print "(a,i0)", "REGISTRY_RUNTIME_VALUE=", provider_value()'//new_line('a')// &
        'select case(provider_value())'//new_line('a')// &
        'case(19,23,29,31)'//new_line('a')// &
        'open(newunit=unit, file="'//runtime_value// &
        '", status="replace", action="write")'//new_line('a')// &
        "write(unit, '(i0)') provider_value()"//new_line('a')// &
        'close(unit)'//new_line('a')// &
        'case default'//new_line('a')//'stop 1'//new_line('a')// &
        'end select'//new_line('a')//'end program'//new_line('a'))

    call gremlin_start_args(arguments, project, lane, case_id)
    call gremlin_json(driver, project, cache, state, arguments, started, process, 30000)
    session = gremlin_field(started, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'resident registry consumer starts through the public CLI')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)
    call await_value('', '19', generation, ready)
    if (.not. ready) call stop_failed()
    first_generation = generation
    first_root = state//'/fo/gremlin/generations-v2/'//generation
    call gremlin_generation_lease_acquire_at(first_root, first_lease, &
        pin_error, pin_message)
    call assert_true(pin_error == 0, 'lease first registry generation: '//trim(pin_message))
    have_first_lease = pin_error == 0
    if (.not. have_first_lease) call stop_failed()
    first_source = first_root// &
        '/bundle/project/build/dependencies/provider/src/provider.f90'
    call assert_true(file_exists(first_source), &
        'selected registry source has an immutable generation materialization')
    first_bytes = read_text(first_source)

    previous = generation
    call write_provider(registry, '1.0.0', 23)
    call await_value(previous, '23', generation, ready)
    if (.not. ready) call stop_failed()
    call assert_frozen('same-version source edit')

    previous = generation
    call write_provider(registry, '2.0.0', 29)
    call await_value(previous, '29', generation, ready)
    if (.not. ready) call stop_failed()
    call assert_frozen('new latest version')

    previous = generation
    call write_config(alternate)
    call await_value(previous, '31', generation, ready)
    if (.not. ready) call stop_failed()
    call assert_frozen('registry configuration path edit')
    call gremlin_stop_lane(driver, project, cache, state, lane, session)
    arguments = string_list_t()
    call list_add(arguments, 'gremlin')
    call list_add(arguments, 'reproduce')
    call list_add(arguments, case_id)
    call list_add(arguments, '--dir')
    call list_add(arguments, project)
    call list_add(arguments, '--lane')
    call list_add(arguments, lane)
    call list_add(arguments, '--session')
    call list_add(arguments, session)
    call list_add(arguments, '--generation')
    call list_add(arguments, first_generation)
    call gremlin_json(driver, project, cache, state, arguments, started, process, 30000)
    call assert_true(process%exit_code == 0, &
        'captured registry consumer reproduces: '//process%stdout//process%stderr)
    call assert_true(read_text(runtime_value) == '19'//new_line('a'), &
        'reproduction executes original registry source after live selection changes; '// &
        'actual='//read_text(runtime_value)//'; response='//process%stdout//process%stderr)
    call assert_frozen('old-generation reproduction')
    arguments = string_list_t()
    call list_add(arguments, 'test')
    call list_add(arguments, '--json')
    call list_add(arguments, case_id)
    call list_add(environment, 'FO_GREMLIN_FROZEN_ROOT='//first_root//'/bundle/project')
    call run_fo(driver, arguments, project, cache, process, environment, timeout_ms=30000)
    call assert_true(process%exit_code == 0, &
        'distinct mutable project resolves its own registry: '//process%stdout//process%stderr)
    call assert_true(read_text(runtime_value) == '31'//new_line('a'), &
        'unmatched frozen root preserves live registry resolution for another project')
    call assert_frozen('distinct mutable project')
    call unpin_first()
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine set_env(name, value)
        character(len=*), intent(in) :: name, value
        integer(c_int) :: status
        status = c_setenv(trim(name)//c_null_char, trim(value)//c_null_char, 1_c_int)
        call assert_true(status == 0, 'isolated registry fixture environment: '//name)
    end subroutine set_env

    subroutine write_config(root)
        character(len=*), intent(in) :: root
        call write_text(config, '[registry]'//new_line('a')// &
            'path = "'//root//'"'//new_line('a'))
    end subroutine write_config

    subroutine write_provider(root, version, value)
        character(len=*), intent(in) :: root, version
        integer, intent(in) :: value
        character(:), allocatable :: package
        character(len=16) :: numeral
        package = root//'/demo/provider/'//version
        write(numeral, '(i0)') value
        call write_text(package//'/fpm.toml', &
            'name = "provider"'//new_line('a')// &
            'version = "'//version//'"'//new_line('a'))
        call write_text(package//'/src/provider.f90', &
            'module provider'//new_line('a')//'implicit none'//new_line('a')// &
            'contains'//new_line('a')//'integer function provider_value()'// &
            new_line('a')//'provider_value = '//trim(numeral)//new_line('a')// &
            'end function'//new_line('a')//'end module'//new_line('a'))
    end subroutine write_provider

    subroutine await_value(old_generation, expected, current, found)
        character(len=*), intent(in) :: old_generation, expected
        character(:), allocatable, intent(out) :: current
        logical, intent(out) :: found
        type(string_list_t) :: args
        type(process_result_t) :: result
        type(json_value_t) :: status, receipts, receipt, green
        character(:), allocatable :: owner_state
        integer :: attempt, i

        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, session)
        call list_add(args, '--detail')
        call list_add(args, 'full')
        call list_add(args, '--cursor')
        call list_add(args, '0')
        found = .false.
        current = ''
        do attempt = 1, 300
            call gremlin_json(driver, project, cache, state, args, status, result, 10000)
            current = gremlin_field(status, 'active_generation')
            green = json_member(status, 'local_gate_green')
            if (len(current) > 0 .and. current /= old_generation .and. &
                    json_boolean_value(green)) then
                receipts = json_member(status, 'events')
                do i = 1, json_size(receipts)
                    receipt = json_element(receipts, i)
                    if (gremlin_field(receipt, 'generation') /= current) cycle
                    if (gremlin_field(receipt, 'case_id') /= case_id) cycle
                    if (gremlin_field(receipt, 'status') /= 'PASS') cycle
                    if (.not. file_exists(runtime_value)) cycle
                    if (read_text(runtime_value) /= expected//new_line('a')) cycle
                    found = .true.
                    return
                end do
            end if
            owner_state = gremlin_field(status, 'state')
            if (owner_state == 'stopped' .or. owner_state == 'error') exit
            call gremlin_wait_ms(100)
        end do
        call assert_true(found, 'new immutable generation has current PASS and value '// &
            expected//'; last state='//owner_state)
    end subroutine await_value

    subroutine assert_frozen(label)
        character(len=*), intent(in) :: label
        call assert_true(file_exists(first_source), 'old frozen source retained: '//label)
        call assert_true(read_text(first_source) == first_bytes, &
            'old frozen source bytes unchanged: '//label)
    end subroutine assert_frozen

    subroutine stop_failed()
        call gremlin_stop_lane(driver, project, cache, state, lane, session)
        call unpin_first()
        call finish_assertions(retain_failed_scratch=.true.)
    end subroutine stop_failed

    subroutine unpin_first()
        if (.not. have_first_lease) return
        call gremlin_lease_release(first_lease, pin_error, pin_message)
        call assert_true(pin_error == 0, 'release first registry generation: '//trim(pin_message))
        if (pin_error == 0) have_first_lease = .false.
    end subroutine unpin_first
end program test_gremlin_registry
