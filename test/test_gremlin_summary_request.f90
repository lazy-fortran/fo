program test_gremlin_summary_request
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_publish, gremlin_session_release
    use fo_gremlin_request, only: gremlin_request_t, parse_request
    use fo_gremlin_supervisor, only: gremlin_handle
    use fo_util, only: make_tmpfile
    implicit none

    interface
        function c_setenv(name, value, overwrite) bind(C, name='setenv') result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: rc
        end function c_setenv
    end interface

    type(gremlin_request_t) :: request
    type(gremlin_session_t) :: session
    character(len=256) :: message
    character(len=512) :: state_root, project_dir
    character(len=8192) :: status_text
    character(len=:), allocatable :: response
    integer :: ierr, failures, exitcode, release_error

    failures = 0
    call parse_request('status', '{}', request, ierr, message)
    call check(ierr == 0 .and. trim(request%detail) == 'full', &
        'status defaults to full detail for direct callers')
    call parse_request('status', '{"detail":"summary"}', request, ierr, message)
    call check(ierr == 0 .and. trim(request%detail) == 'summary', &
        'status accepts summary detail')
    call parse_request('wait', '{"detail":"summary","wait_ms":0}', &
        request, ierr, message)
    call check(ierr == 0 .and. trim(request%detail) == 'summary', &
        'wait accepts summary detail')
    call parse_request('status', '{"detail":"compact"}', request, ierr, message)
    call check(ierr /= 0 .and. index(message, 'summary or full') > 0, &
        'rejects unsupported detail values')
    call parse_request('events', '{"detail":"summary"}', request, ierr, message)
    call check(ierr /= 0 .and. index(message, 'unsupported') > 0, &
        'events retains its existing paging contract')
    call test_public_summary_shape()
    if (failures > 0) stop 1
    print '(a)', 'Gremlin summary request contract PASS'

contains

    subroutine test_public_summary_shape()
        call make_tmpfile('fo-gremlin-summary-state', state_root)
        call make_tmpfile('fo-gremlin-summary-project', project_dir)
        call fs_make_dir(trim(project_dir))
        ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(state_root)//c_null_char, 1_c_int)
        call check(ierr == 0, 'sets isolated state for public summary fixture')
        call gremlin_session_acquire(trim(project_dir), 'summary-test', session, &
            ierr, message)
        call check(ierr == 0 .and. session%owner, 'creates summary fixture session')
        if (ierr /= 0) return
        status_text = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
            '","lane_id":"summary-test","state":"quiescent",'// &
            '"active_generation":"","candidate_generation":"",'// &
            '"completed":0,"selected":0,"seed":0,"last_outcome":"NONE",'// &
            '"last_exitcode":0,"gate_required":0,"requirement_digest":"",'// &
            '"event_epoch":0,"input_changed":false,"diagnostic":""}'
        call gremlin_session_publish(session, trim(status_text), ierr, message)
        call check(ierr == 0, 'publishes summary fixture status')
        call gremlin_handle('status', trim(project_dir), &
            '{"lane_id":"summary-test","detail":"summary"}', response, exitcode)
        call check(exitcode == 0, 'public status accepts summary detail')
        call check(index(response, '"action":"status"') > 0 .and. &
            index(response, '"detail":"summary"') > 0 .and. &
            index(response, '"session_id":"'//trim(session%session_id)//'"') > 0 .and. &
            index(response, '"phase":') > 0 .and. &
            index(response, '"health":') > 0 .and. &
            index(response, '"local_gate_green":') > 0 .and. &
            index(response, '"gate_required":') > 0, &
            'summary returns its marker and compact readiness facts')
        call check(index(response, '"events"') == 0 .and. &
            index(response, '"next_cursor"') == 0 .and. &
            index(response, '"lifecycle_events"') == 0 .and. &
            index(response, '"next_lifecycle_cursor"') == 0, &
            'summary omits event data and paging cursors')
        call gremlin_session_release(session, release_error, message)
        call check(release_error == 0, 'releases summary fixture session')
        call fs_remove_tree(trim(state_root))
        call fs_remove_tree(trim(project_dir))
    end subroutine test_public_summary_shape

    subroutine check(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) return
        failures = failures + 1
        write (*, '(a)') 'FAIL: '//description
    end subroutine check

end program test_gremlin_summary_request
