program test_default_gremlin_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_string, assert_contains
    use fo_test_harness, only: assert_process_ok, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_parse, json_member, json_string_value
    use fo_test_json, only: json_invalid
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, verify_dir
    character(:), allocatable :: first_session, second_session, message
    type(string_list_t) :: arguments
    type(process_result_t) :: first, second, verify_result, query_result
    type(json_value_t) :: first_json, second_json, query_json, field
    logical :: valid, compiled_module_exists
    integer :: summary_bytes, full_bytes

    call gremlin_setup(driver, scratch, project, cache, state)
    summary_bytes = 0
    full_bytes = 0
    call make_directory(join_path(project, 'test'))
    call write_text(join_path(project, 'fpm.toml'), 'name = "bare_gremlin_probe"'// &
        new_line('a'))
    call write_text(join_path(project, 'test/test_probe.f90'), &
        'program test_probe'//new_line('a')//'implicit none'//new_line('a')// &
        'print *, "probe passed"'//new_line('a')// &
        'end program test_probe'//new_line('a'))

    ! An empty argv invokes the default CLI path. It must own the same default
    ! lane as an explicit `fo gremlin start` in this project.
    arguments = string_list_t()
    call gremlin_run(driver, project, cache, state, arguments, first, timeout=30000)
    call assert_process_ok(first, 'bare fo starts Gremlin')
    call json_parse(first%stdout, first_json, valid, message)
    call assert_true(valid, 'bare fo returns Gremlin JSON: '//message)
    first_session = json_string_value(json_member(first_json, 'session_id'))
    call assert_true(len(first_session) > 0, 'bare fo returns its Gremlin owner')

    call list_add(arguments, 'gremlin')
    call list_add(arguments, 'start')
    call list_add(arguments, '--dir')
    call list_add(arguments, project)
    call list_add(arguments, '--lane')
    call list_add(arguments, 'default')
    call list_add(arguments, '--json')
    call gremlin_run(driver, project, cache, state, arguments, second, timeout=30000)
    call assert_process_ok(second, 'explicit Gremlin start attaches')
    call json_parse(second%stdout, second_json, valid, message)
    call assert_true(valid, 'explicit start returns Gremlin JSON: '//message)
    second_session = json_string_value(json_member(second_json, 'session_id'))
    call assert_equal_string(second_session, first_session, &
        'bare and explicit starts attach to the exact same owner')
    if (len(first_session) == 0) first_session = second_session

    call run_query('status', '', .false., -1, driver, project, cache, state, &
        query_result)
    call check_summary_response(query_result, first_session, 'default status', &
        query_json)
    summary_bytes = len_trim(query_result%stdout)

    call run_query('status', 'summary', .false., -1, driver, project, cache, &
        state, query_result)
    call check_summary_response(query_result, first_session, &
        'explicit summary status', query_json)

    call run_query('status', 'full', .false., -1, driver, project, cache, state, &
        query_result)
    call check_full_response(query_result, first_session, 'full status', query_json)
    full_bytes = len_trim(query_result%stdout)
    call assert_true(summary_bytes > 0 .and. summary_bytes < full_bytes, &
        'default summary emits fewer bytes than full status')
    call assert_true(summary_bytes <= 4096, &
        'default status summary stays within the 4 KiB response budget')
    write (*, '(a,i0,a,i0)') 'gremlin status bytes summary=', summary_bytes, &
        ' full=', full_bytes

    call run_query('status', '', .true., -1, driver, project, cache, state, &
        query_result)
    call check_summary_response(query_result, first_session, &
        'status with --json', query_json)

    call run_query('wait', '', .false., 0, driver, project, cache, state, &
        query_result)
    call check_summary_response(query_result, first_session, 'default wait', &
        query_json)
    field = json_member(query_json, 'action')
    call assert_equal_string(json_string_value(field), 'wait', &
        'wait retains its action marker')

    call run_query('wait', 'summary', .false., 0, driver, project, cache, state, &
        query_result)
    call check_summary_response(query_result, first_session, &
        'explicit summary wait', query_json)

    call run_query('wait', 'full', .false., 0, driver, project, cache, state, &
        query_result)
    call check_full_response(query_result, first_session, 'full wait', query_json)

    call run_query('events', '', .false., -1, driver, project, cache, state, &
        query_result)
    call assert_process_ok(query_result, 'default events query succeeds')
    call json_parse(query_result%stdout, query_json, valid, message)
    call assert_true(valid, 'events returns valid JSON: '//message)
    field = json_member(query_json, 'action')
    call assert_equal_string(json_string_value(field), 'events', &
        'events retains its action marker')
    call check_page_fields(query_json, 'default events')

    if (len(first_session) > 0) &
        call gremlin_stop_lane(driver, project, cache, state, 'default', first_session)

    verify_dir = join_path(scratch, 'verify-project')
    call make_directory(join_path(verify_dir, 'src'))
    call write_text(join_path(verify_dir, 'fpm.toml'), 'name = "verify_probe"'// &
        new_line('a'))
    call write_text(join_path(verify_dir, 'src/verify_module.f90'), &
        'module verify_module'//new_line('a')//'implicit none'//new_line('a')// &
        'integer, parameter :: verify_value = 19'//new_line('a')// &
        'end module verify_module'//new_line('a'))
    arguments = string_list_t()
    call list_add(arguments, 'verify')
    call gremlin_run(driver, verify_dir, cache, state, arguments, verify_result, &
        timeout=30000)
    call assert_process_ok(verify_result, 'fo verify runs the existing pipeline')
    call assert_contains(verify_result%stdout, 'Static: OK', &
        'verify executes the static pipeline stage')
    call assert_contains(verify_result%stdout, 'Build: OK', &
        'verify executes the native build stage')
    inquire (file=join_path(verify_dir, 'build/fo/mod/verify_module.mod'), &
        exist=compiled_module_exists)
    call assert_true(compiled_module_exists, &
        'verify produces the native module artifact')
    call assert_contains(verify_result%stdout, 'All stages passed', &
        'verify reaches the prior pipeline completion path')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'default-gremlin-cli: summary/full routing and verify pass'
contains

    subroutine run_query(action, detail, include_json, wait_ms, driver, project, &
            cache, state, result)
        character(len=*), intent(in) :: action, detail, driver, project, cache, state
        logical, intent(in) :: include_json
        integer, intent(in) :: wait_ms
        type(process_result_t), intent(out) :: result
        character(len=32) :: wait_text
        type(string_list_t) :: query_args

        query_args = string_list_t()
        call list_add(query_args, 'gremlin')
        call list_add(query_args, action)
        call list_add(query_args, '--dir')
        call list_add(query_args, project)
        call list_add(query_args, '--lane')
        call list_add(query_args, 'default')
        if (len_trim(detail) > 0) then
            call list_add(query_args, '--detail')
            call list_add(query_args, detail)
        end if
        if (wait_ms >= 0) then
            write (wait_text, '(i0)') wait_ms
            call list_add(query_args, '--wait-ms')
            call list_add(query_args, trim(wait_text))
        end if
        if (include_json) call list_add(query_args, '--json')
        call gremlin_run(driver, project, cache, state, query_args, result, &
            timeout=30000)
    end subroutine run_query

    subroutine check_summary_response(result, expected_session, label, parsed)
        type(process_result_t), intent(in) :: result
        character(len=*), intent(in) :: expected_session, label
        type(json_value_t), intent(out) :: parsed
        type(json_value_t) :: detail_field

        call assert_process_ok(result, label//' succeeds')
        call json_parse(result%stdout, parsed, valid, message)
        call assert_true(valid, label//' returns valid JSON: '//message)
        detail_field = json_member(parsed, 'detail')
        call assert_equal_string(json_string_value(detail_field), 'summary', &
            label//' reports summary detail')
        call check_identity(parsed, expected_session, label)
        call check_absent(parsed, 'events', label)
        call check_absent(parsed, 'lifecycle_events', label)
        call check_absent(parsed, 'next_cursor', label)
        call check_absent(parsed, 'next_lifecycle_cursor', label)
    end subroutine check_summary_response

    subroutine check_full_response(result, expected_session, label, parsed)
        type(process_result_t), intent(in) :: result
        character(len=*), intent(in) :: expected_session, label
        type(json_value_t), intent(out) :: parsed

        call assert_process_ok(result, label//' succeeds')
        call json_parse(result%stdout, parsed, valid, message)
        call assert_true(valid, label//' returns valid JSON: '//message)
        call check_identity(parsed, expected_session, label)
        call check_page_fields(parsed, label)
    end subroutine check_full_response

    subroutine check_identity(parsed, expected_session, label)
        type(json_value_t), intent(in) :: parsed
        character(len=*), intent(in) :: expected_session, label
        type(json_value_t) :: field_value

        field_value = json_member(parsed, 'lane_id')
        call assert_equal_string(json_string_value(field_value), 'default', &
            label//' retains lane identity')
        field_value = json_member(parsed, 'session_id')
        call assert_equal_string(json_string_value(field_value), expected_session, &
            label//' retains session identity')
        field_value = json_member(parsed, 'state')
        call assert_true(len(json_string_value(field_value)) > 0, &
            label//' retains state identity')
    end subroutine check_identity

    subroutine check_page_fields(parsed, label)
        type(json_value_t), intent(in) :: parsed
        character(len=*), intent(in) :: label

        call check_present(parsed, 'events', label)
        call check_present(parsed, 'lifecycle_events', label)
        call check_present(parsed, 'next_cursor', label)
        call check_present(parsed, 'next_lifecycle_cursor', label)
    end subroutine check_page_fields

    subroutine check_absent(parsed, name, label)
        type(json_value_t), intent(in) :: parsed
        character(len=*), intent(in) :: name, label
        type(json_value_t) :: field_value

        field_value = json_member(parsed, name)
        call assert_true(field_value%kind == json_invalid, &
            label//' omits '//name)
    end subroutine check_absent

    subroutine check_present(parsed, name, label)
        type(json_value_t), intent(in) :: parsed
        character(len=*), intent(in) :: name, label
        type(json_value_t) :: field_value

        field_value = json_member(parsed, name)
        call assert_true(field_value%kind /= json_invalid, &
            label//' retains '//name)
    end subroutine check_present
end program test_default_gremlin_cli
