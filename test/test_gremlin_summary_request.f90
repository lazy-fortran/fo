program test_gremlin_summary_request
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_publish, gremlin_session_release
    use fo_gremlin_request, only: gremlin_request_t, parse_request
    use fo_gremlin_session, only: gremlin_get_session_journal_path
    use fo_gremlin_supervisor, only: gremlin_handle
    use fo_gremlin_journal, only: journal_append
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
    character(len=64) :: failure_generation
    character(len=256) :: journal_path
    character(len=:), allocatable :: response, request_json, long_case, journal_record
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
    long_case = repeat('x', 1024)
    request_json = '{"targets":["'//long_case//'"]}'
    call parse_request('start', request_json, request, ierr, message)
    call check(ierr == 0, 'accepts a target at the retained request bound')
    if (ierr == 0) then
        call check(request%n_targets == 1 .and. &
            request%targets(1) == long_case, 'retains the complete target name')
    end if
    request_json = '{"targets":["'//long_case//'x"]}'
    call parse_request('start', request_json, request, ierr, message)
    call check(ierr /= 0 .and. index(message, '1024') > 0, &
        'rejects an overlong target with its retained request bound')
    request_json = '{"case_id":"'//long_case//'x"}'
    call parse_request('reproduce', request_json, request, ierr, message)
    call check(ierr /= 0 .and. index(message, 'case_id') > 0, &
        'reproduction rejects a name beyond the retained request bound')
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
            index(response, '"gate_required":0') > 0, &
            'summary returns its marker and compact readiness facts')
        call check(index(response, '"events"') == 0 .and. &
            index(response, '"next_cursor"') == 0 .and. &
            index(response, '"lifecycle_events"') == 0 .and. &
            index(response, '"next_lifecycle_cursor"') == 0, &
            'summary omits event data and paging cursors')
        call test_long_gate_receipts()
        failure_generation = repeat('a', len(failure_generation))
        journal_path = trim(session%state_dir)//'/campaign-journal.jsonl'
        journal_record = '{"completion_id":"build-failure","session_id":"'// &
            trim(session%session_id)//'","outcome":"fail",'// &
            '"status":"BUILD_FAIL","generation":"'//failure_generation// &
            '","case_id":"<build>","log_path":"/tmp/fo-build.log"}'
        call journal_append(trim(journal_path), 'build-failure', &
            trim(journal_record), ierr, message)
        call check(ierr == 0, 'records a current candidate build failure')
        journal_record = '{"completion_id":"old-build-failure",'// &
            '"session_id":"old-session","outcome":"fail",'// &
            '"status":"BUILD_FAIL","generation":"'//failure_generation// &
            '","case_id":"old-build","log_path":"/tmp/old-build.log"}'
        call journal_append(trim(journal_path), 'old-build-failure', &
            trim(journal_record), ierr, message)
        call check(ierr == 0, 'records another session failure on the same generation')
        status_text = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
            '","lane_id":"summary-test","state":"build_failed",'// &
            '"active_generation":"","candidate_generation":"'// &
            failure_generation//'","health":"failure","completed":0,'// &
            '"selected":0,"seed":0,"last_outcome":"BUILD_FAIL",'// &
            '"last_exitcode":1,"gate_required":0,"requirement_digest":"",'// &
            '"event_epoch":0,"input_changed":false,"diagnostic":""}'
        call gremlin_session_publish(session, trim(status_text), ierr, message)
        call check(ierr == 0, 'publishes a failed candidate generation')
        call gremlin_handle('status', trim(project_dir), &
            '{"lane_id":"summary-test","detail":"summary"}', response, exitcode)
        call check(exitcode == 0 .and. &
            index(response, '"latest_failure":{"generation":"'// &
            failure_generation//'","case_id":"<build>","status":"BUILD_FAIL",'// &
            '"log_path":"/tmp/fo-build.log"}') > 0, &
            'summary locates a failed candidate when no active generation exists: '// &
            trim(response))
        call gremlin_session_release(session, release_error, message)
        call check(release_error == 0, 'releases summary fixture session')
        call fs_remove_tree(trim(state_root))
        call fs_remove_tree(trim(project_dir))
    end subroutine test_public_summary_shape

    subroutine test_long_gate_receipts()
        character(len=64) :: generation, requirement
        character(len=16) :: completion
        character(len=206) :: case_name
        character(len=4096) :: readiness_journal
        integer :: i

        generation = repeat('a', 64)
        requirement = repeat('b', 64)
        call gremlin_get_session_journal_path(trim(project_dir), session%lane_id, &
            session%session_id, readiness_journal, ierr, message)
        call check(ierr == 0, 'locates the authoritative session receipt journal')
        if (ierr /= 0) return
        do i = 1, 2
            write (completion, '(a,i0)') 'long-gate-', i
            case_name = 'gate_'//repeat('n', 200)//achar(iachar('a') + i - 1)
            journal_record = '{"completion_id":"'//trim(completion)// &
                '","session_id":"'//trim(session%session_id)// &
                '","outcome":"pass","status":"PASS","generation":"'//generation// &
                '","case_id":"'//case_name// &
                '","gate_required":true,"requirement_digest":"'//requirement//'"}'
            call journal_append(trim(readiness_journal), trim(completion), &
                journal_record, ierr, message)
            call check(ierr == 0, &
                'records a distinct gate name beyond 128 characters: '//trim(message))
        end do
        status_text = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
            '","lane_id":"summary-test","state":"quiescent",'// &
            '"active_generation":"'//generation//'","candidate_generation":"",'// &
            '"gate_required":2,"requirement_digest":"'//requirement// &
            '","event_epoch":0,"input_changed":false,"diagnostic":""}'
        call gremlin_session_publish(session, trim(status_text), ierr, message)
        call check(ierr == 0, 'publishes a generation with long gate receipts')
        call gremlin_handle('status', trim(project_dir), &
            '{"lane_id":"summary-test","detail":"summary"}', response, exitcode)
        call check(exitcode == 0 .and. index(response, '"gate_required":2') > 0 .and. &
            index(response, '"gate_passed":2') > 0, &
            'public readiness counts distinct long gate names sharing '// &
            'a 128-character prefix: '//response)
    end subroutine test_long_gate_receipts

    subroutine check(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) return
        failures = failures + 1
        write (*, '(a)') 'FAIL: '//description
    end subroutine check

end program test_gremlin_summary_request
