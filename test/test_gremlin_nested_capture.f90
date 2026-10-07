program test_gremlin_nested_capture
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, write_text, read_text
    use fo_test_harness, only: assert_true, assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, external
    character(:), allocatable :: lane, session, generation, next_generation
    character(:), allocatable :: identity, previous_identity, expected_diagnostic
    character(:), allocatable :: nested_source, external_source, nested_path, external_path
    type(process_result_t) :: process
    type(json_value_t) :: response, status
    integer :: attempt
    logical :: found

    call gremlin_setup(driver, scratch, project, cache, state)
    call use_absolute_driver()
    external = scratch//'/external_dep'
    nested_path = project//'/test-support/nested_dep/src/nested_dep.f90'
    external_path = external//'/src/external_dep.f90'
    call make_directory(project//'/test')
    call make_directory(project//'/test-support/nested_dep/src')
    call make_directory(external//'/src')
    call write_fixture('11', '21')
    lane = 'nested-capture'
    call start_lane(lane, session)
    call wait_for_pass(lane, session, '', generation, status)

    identity = generation_identity(generation)
    previous_identity = read_text(identity)
    call assert_true(len(identity_line(previous_identity, 'input=project|.|')) > 0, &
        'the primary project tree is one captured generation input')
    call assert_true(len(identity_line(previous_identity, &
        'input=dependency:nested_dep|')) == 0, &
        'the internal dependency is not duplicated as another generation root')
    call assert_true(len(identity_line(previous_identity, &
        'input=dependency:external_dep|')) > 0, &
        'the external sibling keeps its distinct dependency identity')
    call assert_equal_string(read_text(generation_project(generation)// &
        '/test-support/nested_dep/src/nested_dep.f90'), nested_source, &
        'the internal dependency is present in the frozen project tree')
    call assert_equal_string(read_text(generation_project(generation)// &
        '/../external_dep/src/external_dep.f90'), external_source, &
        'the external sibling is present at its bundled dependency root')

    previous_identity = identity_line(previous_identity, 'input=project|.|')
    call write_nested_value('12')
    call wait_for_pass(lane, session, generation, next_generation, status)
    call assert_true(next_generation /= generation, &
        'mutating an internal dependency changes the generation')
    identity = read_text(generation_identity(next_generation))
    call assert_true(identity_line(identity, 'input=project|.|') /= previous_identity, &
        'the project-tree digest records the internal dependency mutation once')
    call assert_equal_string(read_text(generation_project(next_generation)// &
        '/test-support/nested_dep/src/nested_dep.f90'), nested_source, &
        'the new generation freezes the changed internal dependency')

    generation = next_generation
    previous_identity = identity
    call write_external_value('22')
    call wait_for_pass(lane, session, generation, next_generation, status)
    call assert_true(next_generation /= generation, &
        'mutating an external sibling changes the generation')
    identity = read_text(generation_identity(next_generation))
    call assert_true(identity_line(identity, 'input=dependency:external_dep|') /= &
        identity_line(previous_identity, 'input=dependency:external_dep|'), &
        'the external dependency digest independently records its mutation')
    call assert_equal_string(identity_line(identity, 'input=project|.|'), &
        identity_line(previous_identity, 'input=project|.|'), &
        'an external dependency mutation leaves the project-tree digest unchanged')
    call assert_equal_string(read_text(generation_project(next_generation)// &
        '/../external_dep/src/external_dep.f90'), external_source, &
        'the new generation freezes the changed external sibling')
    call gremlin_stop_lane(driver, project, cache, state, lane, session)

    call write_text(project//'/fpm.toml', fixture_manifest(.true.))
    lane = 'capture-diagnostic'
    call start_lane(lane, session)
    expected_diagnostic = 'cannot capture input tree dependency:missing_dep'
    found = .false.
    do attempt = 1, 400
        call status_for(lane, session, status)
        if (gremlin_field(status, 'state') == 'capture_failed') then
            found = .true.
            exit
        end if
        call gremlin_wait_ms(50)
    end do
    call assert_true(found, 'a missing dependency produces public capture_failed status')
    call assert_equal_string(gremlin_field(status, 'diagnostic'), expected_diagnostic, &
        'public status exposes the exact bounded capture diagnostic')
    call assert_true(len(gremlin_field(status, 'active_generation')) == 0, &
        'a failed initial capture publishes no generation')
    call assert_capture_event(status, expected_diagnostic)
    call gremlin_stop_lane(driver, project, cache, state, lane, session)
    call finish_assertions()

contains

    subroutine use_absolute_driver()
        integer :: length, status
        character(:), allocatable :: value

        call get_environment_variable('FO_BIN', length=length, status=status)
        if (status /= 0 .or. length <= 0) return
        allocate (character(len=length) :: value)
        call get_environment_variable('FO_BIN', value, status=status)
        if (status == 0) driver = value
    end subroutine use_absolute_driver

    subroutine write_fixture(nested_value, outside_value)
        character(len=*), intent(in) :: nested_value, outside_value
        character(:), allocatable :: test_source

        call write_text(project//'/fpm.toml', fixture_manifest(.false.))
        call write_nested_value(nested_value)
        call write_external_value(outside_value)
        test_source = 'program test_path_dependencies'//new_line('a')// &
            'use nested_dep, only: nested_value'//new_line('a')// &
            'use external_dep, only: external_value'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'if (nested_value <= 0 .or. external_value <= 0) error stop 1'// &
            new_line('a')//'end program test_path_dependencies'//new_line('a')
        call write_text(project//'/test/test_path_dependencies.f90', test_source)
        call write_text(project//'/test-support/nested_dep/fpm.toml', &
            'name = "nested_dep"'//new_line('a'))
        call write_text(external//'/fpm.toml', 'name = "external_dep"'//new_line('a'))
    end subroutine write_fixture

    function fixture_manifest(force_failure) result(manifest)
        logical, intent(in) :: force_failure
        character(:), allocatable :: manifest

        manifest = 'name = "gremlin_nested_capture_probe"'//new_line('a')// &
            '[dev-dependencies]'//new_line('a')// &
            'nested_dep = { path = "test-support/nested_dep" }'//new_line('a')// &
            'external_dep = { path = "../external_dep" }'//new_line('a')
        if (force_failure) manifest = manifest// &
            'missing_dep = { path = "test-support/missing_dep" }'//new_line('a')
    end function fixture_manifest

    subroutine write_nested_value(value)
        character(len=*), intent(in) :: value
        nested_source = 'module nested_dep'//new_line('a')// &
            'integer, parameter :: nested_value = '//trim(value)//new_line('a')// &
            'end module nested_dep'//new_line('a')
        call write_text(nested_path, nested_source)
    end subroutine write_nested_value

    subroutine write_external_value(value)
        character(len=*), intent(in) :: value
        external_source = 'module external_dep'//new_line('a')// &
            'integer, parameter :: external_value = '//trim(value)//new_line('a')// &
            'end module external_dep'//new_line('a')
        call write_text(external_path, external_source)
    end subroutine write_external_value

    subroutine start_lane(lane_id, owner)
        character(len=*), intent(in) :: lane_id
        character(len=:), allocatable, intent(out) :: owner
        type(string_list_t) :: args

        call gremlin_start_args(args, project, lane_id, 'test_path_dependencies')
        call list_add(args, '--seed')
        call list_add(args, '1729')
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, response, process, 30000)
        call assert_true(process%exit_code == 0, 'starts the resident fixture lane')
        owner = gremlin_field(response, 'session_id')
        call assert_true(len(owner) > 0, 'resident fixture returns its owner ID')
    end subroutine start_lane

    subroutine wait_for_pass(lane_id, owner, old_generation, current_generation, document)
        character(len=*), intent(in) :: lane_id, owner, old_generation
        character(len=:), allocatable, intent(out) :: current_generation
        type(json_value_t), intent(out) :: document
        integer :: i

        current_generation = ''
        do i = 1, 900
            call status_for(lane_id, owner, document)
            current_generation = gremlin_field(document, 'active_generation')
            if (len(current_generation) == 64 .and. &
                current_generation /= old_generation) then
                if (has_pass(document, current_generation)) return
            end if
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'resident lane builds and passes its targeted test')
    end subroutine wait_for_pass

    subroutine status_for(lane_id, owner, document)
        character(len=*), intent(in) :: lane_id, owner
        type(json_value_t), intent(out) :: document
        type(string_list_t) :: args

        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--detail')
        call list_add(args, 'full')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane_id)
        call list_add(args, '--session')
        call list_add(args, owner)
        call list_add(args, '--json')
        call gremlin_json(driver, project, cache, state, args, document, process, 30000)
    end subroutine status_for

    logical function has_pass(document, wanted_generation)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: wanted_generation
        type(json_value_t) :: records, record
        integer :: i

        has_pass = .false.
        records = json_member(document, 'events')
        do i = 1, json_size(records)
            record = json_element(records, i)
            if (gremlin_field(record, 'case_id') /= 'test_path_dependencies') cycle
            if (gremlin_field(record, 'generation') /= wanted_generation) cycle
            if (gremlin_field(record, 'status') == 'PASS') then
                has_pass = .true.
                return
            end if
        end do
    end function has_pass

    subroutine assert_capture_event(document, expected)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: expected
        type(json_value_t) :: events, event
        integer :: i

        events = json_member(document, 'lifecycle_events')
        do i = 1, json_size(events)
            event = json_element(events, i)
            if (gremlin_field(event, 'event_type') /= 'capture_failed') cycle
            call assert_equal_string(gremlin_field(event, 'diagnostic'), expected, &
                'capture_failed lifecycle event carries the exact diagnostic')
            return
        end do
        call assert_true(.false., 'public lifecycle page includes capture_failed event')
    end subroutine assert_capture_event

    function generation_identity(value) result(path)
        character(len=*), intent(in) :: value
        character(:), allocatable :: path
        path = state//'/fo/gremlin/generations-v2/'//trim(value)//'/identity.txt'
    end function generation_identity

    function generation_project(value) result(path)
        character(len=*), intent(in) :: value
        character(:), allocatable :: path
        path = state//'/fo/gremlin/generations-v2/'//trim(value)//'/bundle/project'
    end function generation_project

    function identity_line(document, prefix) result(line)
        character(len=*), intent(in) :: document, prefix
        character(:), allocatable :: line
        integer :: start, finish

        line = ''
        start = index(document, prefix)
        if (start == 0) return
        finish = index(document(start:), new_line('a'))
        if (finish == 0) then
            line = document(start:)
        else
            line = document(start:start + finish - 2)
        end if
    end function identity_line

end program test_gremlin_nested_capture
