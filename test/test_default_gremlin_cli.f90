program test_default_gremlin_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_string, assert_contains
    use fo_test_harness, only: assert_process_ok, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_parse, json_member, json_string_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, verify_dir
    character(:), allocatable :: first_session, second_session, message
    type(string_list_t) :: arguments
    type(process_result_t) :: first, second, verify_result
    type(json_value_t) :: first_json, second_json
    logical :: valid

    call gremlin_setup(driver, scratch, project, cache, state)
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
    call assert_contains(verify_result%stdout, 'All stages passed', &
        'verify reaches the prior pipeline completion path')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'default-gremlin-cli: bare start/attach and verify routing pass'
end program test_default_gremlin_cli
