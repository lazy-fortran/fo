program test_mcp_shutdown_drain
    !! A gremlin_wait still running when the client sends shutdown gets its
    !! response before the shutdown result; no request id goes unanswered.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_publish, gremlin_session_release
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_cli, only: resolve_driver
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_exchange
    use fo_test_json, only: json_value_t, json_member, json_number_value
    implicit none

    interface
        function c_setenv(name, value, overwrite) bind(C, name='setenv') result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: rc
        end function c_setenv
    end interface

    character(:), allocatable :: driver, scratch, project, state, input, status
    character(len=512) :: message
    type(gremlin_session_t) :: session
    type(json_value_t), allocatable :: responses(:)
    type(process_result_t) :: result
    type(string_list_t) :: environment
    integer :: ierr, i, wait_index, shutdown_index

    call resolve_driver(driver)
    call make_scratch('fo-mcp-shutdown-drain', scratch)
    project = join_path(scratch, 'project')
    state = join_path(scratch, 'state')
    call make_directory(project)
    call make_directory(state)
    call make_directory(join_path(scratch, 'cache'))
    ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, state//c_null_char, 1_c_int)
    call assert_true(ierr == 0, 'sets an isolated Gremlin state directory')
    ! This process owns a live lane that never becomes quiescent.
    call gremlin_session_acquire(project, 'drain', session, ierr, message)
    call assert_true(ierr == 0 .and. session%owner, 'owns an isolated live lane')
    if (ierr /= 0) call finish_assertions()
    status = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
        '","lane_id":"drain","state":"testing","active_generation":"",'// &
        '"candidate_generation":"","completed":0,"selected":1,"seed":1,'// &
        '"last_outcome":"NONE","last_exitcode":0}'
    call gremlin_session_publish(session, status, ierr, message)
    call assert_true(ierr == 0, 'publishes the live lane state')

    input = mcp_encode(mcp_request(1, 'initialize', '{"protocolVersion":'// &
        '"2024-11-05","capabilities":{},"clientInfo":{"name":"t","version":"1"}}'), &
        .false.)//mcp_encode(mcp_call(2, '{"action":"gremlin_wait","dir":"'// &
        project//'","lane_id":"drain","wait_until":"quiescent","wait_ms":1500}'), &
        .false.)//mcp_encode(mcp_request(3, 'shutdown'), .false.)
    call list_add(environment, 'FO_GREMLIN_STATE_DIR='//state)
    call mcp_exchange(driver, project, join_path(scratch, 'cache'), input, .false., &
        responses, result, environment)
    wait_index = 0
    shutdown_index = 0
    do i = 1, size(responses)
        select case (nint(json_number_value(json_member(responses(i), 'id'))))
        case (2)
            wait_index = i
        case (3)
            shutdown_index = i
        end select
    end do
    call assert_true(wait_index > 0, 'the pending wait is answered at shutdown')
    call assert_true(shutdown_index > wait_index, &
        'the wait response precedes the shutdown result')
    call gremlin_session_release(session, ierr, message)
    call finish_assertions()
end program test_mcp_shutdown_drain
