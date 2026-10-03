program test_gremlin_supervisor
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, int64
    use fo_cache, only: cache_digest
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_gremlin_journal, only: journal_append
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_publish, gremlin_session_release, &
        gremlin_session_stop_requested
    use fo_gremlin_supervisor, only: gremlin_handle
    use fo_util, only: extract_json_field, make_tmpfile
    implicit none

    interface
        function c_setenv(name, value, overwrite) bind(C, name='setenv') result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: rc
        end function c_setenv

        function c_terminal_journal_path(project, lane, session, path, cap) &
                bind(C, name='fo_gremlin_terminal_journal_path') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project(*), lane(*), session(*)
            character(kind=c_char), intent(out) :: path(*)
            integer(c_int), value :: cap
            integer(c_int) :: ierr
        end function c_terminal_journal_path

        function c_terminal_publish(project, lane, session, status, status_len) &
                bind(C, name='fo_gremlin_terminal_publish') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project(*), lane(*), session(*)
            character(kind=c_char), intent(in) :: status(*)
            integer(c_int), value :: status_len
            integer(c_int) :: ierr
        end function c_terminal_publish
    end interface

    integer :: passed, failed

    passed = 0
    failed = 0
    call test_strict_request_validation()
    call test_same_place_attach_policy()
    call test_event_cursor_and_journal_errors()
    call test_terminal_session_history()
    write (output_unit, '(a,i0,a,i0,a)') 'Summary: ', passed, &
        ' passed, ', failed, ' failed'
    if (failed > 0) stop 1

contains

    subroutine check(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) then
            passed = passed + 1
        else
            failed = failed + 1
            write (error_unit, '(a,a)') 'FAIL: ', description
        end if
    end subroutine check

    subroutine test_strict_request_validation()
        character(len=:), allocatable :: response
        integer :: exitcode

        call gremlin_handle('start', '/tmp/project', '{"background":true}', &
            response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'unsupported') > 0, &
            'rejects unsupported background field')
        call gremlin_handle('status', '/tmp/project', &
            '{"lane_id":"a","lane_id":"b"}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'duplicate') > 0, &
            'rejects duplicate object keys')
        call gremlin_handle('start', '/tmp/project', '{"random_count":"2"}', &
            response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'wrong JSON type') > 0, &
            'rejects a string where an integer is required')
        call gremlin_handle('start', '/tmp/project', &
            '{"targets":["case_a","case_a"]}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'duplicate target') > 0, &
            'rejects duplicate target names')
        call gremlin_handle('status', '/tmp/project', &
            '{"cursor":-1}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'nonnegative') > 0, &
            'rejects a negative event cursor')
    end subroutine test_strict_request_validation

    subroutine test_same_place_attach_policy()
        type(gremlin_session_t) :: session
        character(len=512) :: state_root, project_dir, message
        character(len=64) :: policy_key
        character(len=8192) :: status_text
        character(len=:), allocatable :: parts(:), response_json
        integer :: ierr, exitcode, release_error

        call make_tmpfile('fo-gremlin-attach-state', state_root)
        call make_tmpfile('fo-gremlin-attach-project', project_dir)
        call fs_make_dir(trim(project_dir))
        ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(state_root)//c_null_char, 1_c_int)
        call check(ierr == 0, 'sets isolated state for attach policy')
        call gremlin_session_acquire(trim(project_dir), 'attach', session, ierr, message)
        call check(ierr == 0 .and. session%owner, 'creates active owner for attach policy')
        if (ierr /= 0) return

        allocate (character(len=96) :: parts(1))
        parts(1) = 'v1|r=32|s=0|c=60|t=5|j=1|changed=false|shuffle=false'
        policy_key = cache_digest(parts, 1)
        status_text = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
            '","lane_id":"attach","policy_key":"'//policy_key// &
            '","state":"testing"}'
        call gremlin_session_publish(session, trim(status_text), ierr, message)
        call check(ierr == 0, 'publishes active owner policy')

        call gremlin_handle('start', trim(project_dir), '{"lane_id":"attach"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, '"state":"attached"') > 0 &
            .and. index(response_json, trim(session%session_id)) > 0, &
            'same-place start attaches to a compatible owner')
        call gremlin_handle('start', trim(project_dir), &
            '{"lane_id":"attach","seed":1}', response_json, exitcode)
        call check(exitcode == 2 .and. index(response_json, 'incompatible policy') > 0, &
            'same-place start rejects a conflicting policy')

        call gremlin_session_release(session, release_error, message)
        call check(release_error == 0, 'releases attach policy owner')
        call fs_remove_tree(trim(state_root))
        call fs_remove_tree(trim(project_dir))
    end subroutine test_same_place_attach_policy

    subroutine test_event_cursor_and_journal_errors()
        type(gremlin_session_t) :: session
        character(len=512) :: state_root, project_dir, response
        character(len=512) :: journal_path
        character(len=1024) :: record
        character(len=64) :: cursor_text
        character(len=8192) :: status_text
        character(len=:), allocatable :: response_json
        integer(int64) :: cursor
        integer :: ierr, exitcode, ios, release_error, unit
        character(len=512) :: message
        character(len=64), parameter :: generation = &
            'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'

        call make_tmpfile('fo-gremlin-supervisor-state', state_root)
        call make_tmpfile('fo-gremlin-supervisor-project', project_dir)
        call fs_make_dir(trim(project_dir))
        cursor = 0_int64
        ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(state_root)//c_null_char, 1_c_int)
        call check(ierr == 0, 'sets an isolated Gremlin state directory')
        call gremlin_session_acquire(trim(project_dir), 'cursor-test', session, &
            ierr, message)
        call check(ierr == 0 .and. session%owner, 'creates isolated Gremlin owner')
        if (ierr /= 0) return

        status_text = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
            '","lane_id":"cursor-test","state":"testing",'// &
            '"active_generation":"'//generation//'","candidate_generation":"'// &
            generation//'","completed":0,"selected":2,"seed":1,'// &
            '"last_outcome":"NONE","last_exitcode":0}'
        call gremlin_session_publish(session, trim(status_text), ierr, message)
        call check(ierr == 0, 'publishes test owner state')
        call session_journal_file(trim(project_dir), 'cursor-test', &
            session%session_id, journal_path, ierr)
        call check(ierr == 0, 'resolves isolated session journal path')
        record = '{"completion_id":"'//trim(session%session_id)//'-one",'// &
            '"session_id":"'//trim(session%session_id)//'",'// &
            '"lane_id":"cursor-test","generation":"'//generation//'",'// &
            '"case_id":"case_alpha","outcome":"pass","status":"PASS"}'
        call journal_append(trim(journal_path), &
            trim(session%session_id)//'-one', trim(record), ierr, message)
        call check(ierr == 0, 'appends first valid receipt')
        record = '{"completion_id":"'//trim(session%session_id)//'-two",'// &
            '"session_id":"'//trim(session%session_id)//'",'// &
            '"lane_id":"cursor-test","generation":"'//generation//'",'// &
            '"case_id":"case_beta","outcome":"fail","status":"FAIL"}'
        call journal_append(trim(journal_path), &
            trim(session%session_id)//'-two', trim(record), ierr, message)
        call check(ierr == 0, 'appends second valid receipt')

        call gremlin_handle('status', trim(project_dir), &
            '{"lane_id":"cursor-test","max_records":1}', response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, 'case_alpha') > 0 .and. &
            index(response_json, '"has_more":true') > 0, &
            'status returns the first event and a continuation cursor')
        cursor_text = ''
        call extract_json_field(response_json, 'next_cursor', cursor_text)
        read (cursor_text, *, iostat=ios) cursor
        call check(ios == 0 .and. cursor > 0_int64, &
            'returns a byte cursor at the record boundary')
        call gremlin_handle('events', trim(project_dir), &
            '{"lane_id":"cursor-test","max_records":1,"cursor":'// &
            trim(cursor_text)//'}', response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, 'case_beta') > 0 .and. &
            index(response_json, '"status":"FAIL"') > 0 .and. &
            index(response_json, '"has_more":false') > 0, &
            'next cursor returns the next event without duplication')

        open (newunit=unit, file=trim(journal_path), &
            status='old', action='write', position='append', iostat=ios)
        if (ios == 0) then
            write (unit, '(a)', iostat=ios) 'not-json'
            close (unit)
        end if
        call check(ios == 0, 'writes malformed journal fixture')
        call gremlin_handle('events', trim(project_dir), &
            '{"lane_id":"cursor-test","cursor":'//trim(cursor_text)//'}', &
            response_json, exitcode)
        call check(exitcode /= 0 .and. index(response_json, 'journal') > 0, &
            'propagates malformed journal errors')

        call gremlin_session_release(session, release_error, message)
        call check(release_error == 0, 'releases test lane ownership')
        call fs_remove_tree(trim(state_root))
        call fs_remove_tree(trim(project_dir))
    end subroutine test_event_cursor_and_journal_errors

    subroutine test_terminal_session_history()
        type(gremlin_session_t) :: first, second
        character(len=512) :: state_root, project_dir, journal_path
        character(len=8192) :: status_text, final_status, record
        character(len=1024) :: response
        character(len=:), allocatable :: response_json
        integer :: ierr, exitcode, release_error
        integer(c_int) :: c_error
        character(len=512) :: message
        logical :: stop_requested
        character(len=64), parameter :: generation = &
            'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'

        call make_tmpfile('fo-gremlin-terminal-state', state_root)
        call make_tmpfile('fo-gremlin-terminal-project', project_dir)
        call fs_make_dir(trim(project_dir))
        ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(state_root)//c_null_char, 1_c_int)
        call check(ierr == 0, 'sets isolated state for terminal history')
        call gremlin_session_acquire(trim(project_dir), 'history', first, ierr, message)
        call check(ierr == 0 .and. first%owner, 'creates first historical lane owner')
        if (ierr /= 0) return
        status_text = '{"protocol":1,"session_id":"'//trim(first%session_id)// &
            '","lane_id":"history","state":"testing",'// &
            '"active_generation":"'//generation//'","candidate_generation":"'// &
            generation//'","completed":1,"selected":1,"seed":1,'// &
            '"last_outcome":"PASS","last_exitcode":0}'
        call gremlin_session_publish(first, trim(status_text), ierr, message)
        call check(ierr == 0, 'publishes first session state')
        call session_journal_file(trim(project_dir), 'history', first%session_id, &
            journal_path, ierr)
        call check(ierr == 0, 'locates first session journal')
        record = '{"completion_id":"'//trim(first%session_id)//'-one",'// &
            '"session_id":"'//trim(first%session_id)//'","lane_id":"history",'// &
            '"generation":"'//generation//'","case_id":"first_case",'// &
            '"outcome":"fail","status":"FAIL"}'
        call journal_append(trim(journal_path), trim(first%session_id)//'-one', &
            trim(record), ierr, message)
        call check(ierr == 0, 'appends first session failure receipt')

        call gremlin_handle('stop', trim(project_dir), &
            '{"lane_id":"history","session_id":"'//trim(first%session_id)//'"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, '"state":"stopping"') > 0, &
            'stop requests cancellation for the exact live session')
        final_status = '{"protocol":1,"session_id":"'//trim(first%session_id)// &
            '","lane_id":"history","state":"stopped",'// &
            '"active_generation":"'//generation//'","candidate_generation":"'// &
            generation//'","completed":1,"selected":1,"seed":1,'// &
            '"last_outcome":"CANCELLED","last_exitcode":0,"padding":"'// &
            repeat('x', 600)//'"}'
        call gremlin_session_publish(first, trim(final_status), ierr, message)
        call check(ierr == 0, 'publishes final state before owner release')
        c_error = c_terminal_publish(trim(project_dir)//c_null_char, &
            'history'//c_null_char, first%session_id//c_null_char, trim(final_status), &
            int(len_trim(final_status), c_int))
        call check(c_error == 0_c_int, 'atomically stores immutable terminal session')
        call gremlin_session_release(first, release_error, message)
        call check(release_error == 0, 'releases first session after snapshot')

        call gremlin_handle('status', trim(project_dir), &
            '{"lane_id":"history","session_id":"'//trim(first%session_id)//'"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, '"state":"stopped"') > 0 .and. &
            index(response_json, 'first_case') > 0 .and. &
            index(response_json, repeat('x', 600)) > 0, &
            'status resolves the full exact terminal snapshot after owner release')
        call gremlin_handle('events', trim(project_dir), &
            '{"lane_id":"history","session_id":"'//trim(first%session_id)//'"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, '"status":"FAIL"') > 0, &
            'events read frozen receipts after owner release')
        call gremlin_handle('failures', trim(project_dir), &
            '{"lane_id":"history","session_id":"'//trim(first%session_id)//'"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, 'first_case') > 0, &
            'failures reads the terminal session journal')
        call gremlin_handle('wait', trim(project_dir), &
            '{"lane_id":"history","session_id":"'//trim(first%session_id)// &
            '","fail_on_failure":true}', response_json, exitcode)
        call check(exitcode == 1 .and. index(response_json, 'first_case') > 0, &
            'wait reports failures from a completed exact session')
        call gremlin_handle('stop', trim(project_dir), &
            '{"lane_id":"history","session_id":"'//trim(first%session_id)//'"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, '"state":"stopped"') > 0, &
            'stop is idempotent for a terminal session')

        call gremlin_session_acquire(trim(project_dir), 'history', second, ierr, message)
        call check(ierr == 0 .and. second%owner, 'a second session starts on the same lane')
        if (ierr /= 0) return
        call check(trim(second%session_id) /= trim(first%session_id), &
            'same-lane replacement has a distinct session id')
        status_text = '{"protocol":1,"session_id":"'//trim(second%session_id)// &
            '","lane_id":"history","state":"testing",'// &
            '"active_generation":"'//generation//'","candidate_generation":"'// &
            generation//'","completed":0,"selected":1,"seed":2,'// &
            '"last_outcome":"NONE","last_exitcode":0}'
        call gremlin_session_publish(second, trim(status_text), ierr, message)
        call check(ierr == 0, 'publishes replacement session state')
        call session_journal_file(trim(project_dir), 'history', second%session_id, &
            journal_path, ierr)
        call check(ierr == 0, 'locates replacement session journal')
        record = '{"completion_id":"'//trim(second%session_id)//'-one",'// &
            '"session_id":"'//trim(second%session_id)//'","lane_id":"history",'// &
            '"generation":"'//generation//'","case_id":"second_case",'// &
            '"outcome":"pass","status":"PASS"}'
        call journal_append(trim(journal_path), trim(second%session_id)//'-one', &
            trim(record), ierr, message)
        call check(ierr == 0, 'appends replacement session receipt')
        call gremlin_handle('status', trim(project_dir), '{"lane_id":"history"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, trim(second%session_id)) > 0 .and. &
            index(response_json, 'second_case') > 0 .and. &
            index(response_json, 'first_case') == 0, &
            'implicit status resolves only the newer live owner')
        call gremlin_handle('events', trim(project_dir), &
            '{"lane_id":"history","session_id":"'//trim(first%session_id)//'"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, 'first_case') > 0 .and. &
            index(response_json, 'second_case') == 0, &
            'explicit old session never aliases the newer journal')
        call gremlin_handle('stop', trim(project_dir), &
            '{"lane_id":"history","session_id":"'//trim(first%session_id)//'"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, '"state":"stopped"') > 0, &
            'old terminal stop does not target the replacement owner')
        call gremlin_session_stop_requested(second, stop_requested, ierr, message)
        call check(ierr == 0 .and. .not. stop_requested, &
            'replacement owner has no stop request from the old session')
        call gremlin_session_release(second, release_error, message)
        call check(release_error == 0, 'releases replacement lane owner')
        call fs_remove_tree(trim(state_root))
        call fs_remove_tree(trim(project_dir))
    end subroutine test_terminal_session_history

    subroutine session_journal_file(project, lane, session, path, ierr)
        character(len=*), intent(in) :: project, lane, session
        character(len=*), intent(out) :: path
        integer, intent(out) :: ierr
        character(kind=c_char) :: c_path(4097)
        integer :: i, n

        c_path = c_null_char
        ierr = int(c_terminal_journal_path(trim(project)//c_null_char, &
            trim(lane)//c_null_char, trim(session)//c_null_char, c_path, &
            int(size(c_path), c_int)))
        path = ''
        n = 0
        do i = 1, size(c_path)
            if (c_path(i) == c_null_char) exit
            n = n + 1
        end do
        if (n <= len(path)) then
            do i = 1, n
                path(i:i) = c_path(i)
            end do
        else
            ierr = 1
        end if
    end subroutine session_journal_file

end program test_gremlin_supervisor
