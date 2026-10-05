program test_gremlin_supervisor
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, int64
    use fo_cache, only: cache_digest
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_sleep_ms
    use fo_gremlin_lifecycle, only: gremlin_freshness_update
    use fo_gremlin_journal, only: journal_append
    use fo_gremlin_coverage, only: coverage_epoch_t, coverage_open, &
        coverage_record, COVERAGE_OK
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_publish, gremlin_session_read, gremlin_session_release, &
        gremlin_session_stop_requested
    use fo_gremlin_supervisor, only: gremlin_handle
    use fo_gremlin_session, only: gremlin_release_stopped_session
    use fo_gremlin_request, only: gremlin_request_t, parse_request
    use fo_util, only: extract_json_field, make_tmpfile
    implicit none

    interface
        function c_fork() bind(C, name='fork') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_fork
        function c_kill(pid, signal) bind(C, name='kill') result(rc)
            import :: c_int
            integer(c_int), value :: pid, signal
            integer(c_int) :: rc
        end function c_kill
        function c_waitpid(pid, status, options) bind(C, name='waitpid') result(rc)
            import :: c_int
            integer(c_int), value :: pid, options
            integer(c_int), intent(out) :: status
            integer(c_int) :: rc
        end function c_waitpid
        subroutine c_exit(status) bind(C, name='_exit')
            import :: c_int
            integer(c_int), value :: status
        end subroutine c_exit
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

        function c_terminal_read(project, lane, session, status, status_cap, &
                journal, journal_cap) bind(C, name='fo_gremlin_terminal_read') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project(*), lane(*), session(*)
            character(kind=c_char), intent(out) :: status(*), journal(*)
            integer(c_int), value :: status_cap, journal_cap
            integer(c_int) :: ierr
        end function c_terminal_read

        function c_chmod(path, mode) bind(C, name='chmod') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
            integer(c_int) :: ierr
        end function c_chmod
    end interface

    integer :: passed, failed

    passed = 0
    failed = 0
    call test_strict_request_validation()
    call test_unknown_terminal_read_is_side_effect_free()
    call test_same_place_attach_policy()
    call test_event_cursor_and_journal_errors()
    call test_readiness_status_and_semantic_waits()
    call test_terminal_session_history()
    call test_failed_final_status_uses_constructed_terminal()
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
        type(gremlin_request_t) :: request
        character(len=:), allocatable :: deep_json
        character(len=256) :: message
        integer :: exitcode
        integer :: ierr

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
        call gremlin_handle('status', '/tmp/project', &
            '{"lane_id":"bad\q"}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'malformed') > 0, &
            'rejects malformed JSON string escapes')
        call gremlin_handle('status', '/tmp/project', &
            '{"lane_id":"bad\u0000"}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'control') > 0, &
            'rejects embedded NUL values')
        call gremlin_handle('status', '/tmp/project', &
            '{"lane_id":"valid"} trailing', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'trailing') > 0, &
            'rejects trailing input after the request object')
        call gremlin_handle('status', '/tmp/project', &
            '{"lane_id" "missing-colon"}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'malformed') > 0, &
            'rejects a missing object colon')
        call gremlin_handle('status', '/tmp/project', &
            '{,"lane_id":"extra-comma"}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'malformed') > 0, &
            'rejects an object comma without a member')
        call gremlin_handle('status', '/tmp/project', &
            '{"lane_id":"trailing-comma",}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'malformed') > 0, &
            'rejects an object trailing comma')
        call gremlin_handle('status', '/tmp/project', &
            '{"lane_id":"missing-separator" "cursor":0}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'malformed') > 0, &
            'rejects missing object separators')
        call gremlin_handle('status', '/tmp/project', '{"cursor":01}', &
            response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'malformed') > 0, &
            'rejects leading-zero integers')
        call gremlin_handle('start', '/tmp/project', &
            '{"targets":["case_a",]}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'malformed') > 0, &
            'rejects a target array trailing comma')
        call gremlin_handle('start', '/tmp/project', &
            '{"targets":["   "]}', response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'target names') > 0, &
            'rejects whitespace-only target names')
        deep_json = '{"targets":'//repeat('[', 65)//'0'//repeat(']', 65)//'}'
        call gremlin_handle('start', '/tmp/project', deep_json, response, exitcode)
        call check(exitcode /= 0 .and. index(response, 'malformed') > 0, &
            'rejects nesting beyond the guarded JSON parser depth')
        call parse_request('status', '{"cursor":2147483648}', request, ierr, message)
        call check(ierr == 0 .and. request%cursor == 2147483648_int64, &
            'preserves a cursor above the fx int32 range')
        call parse_request('wait', '{"cursor":2147483648,"wait_ms":0}', &
            request, ierr, message)
        call check(ierr == 0 .and. request%cursor == 2147483648_int64, &
            'preserves a lifecycle cursor above the fx int32 range')
        call parse_request('start', '{"random_count":0}', request, ierr, message)
        call check(ierr == 0 .and. request%random_count == 0, &
            'accepts a gate-only campaign with no random sample')
        call parse_request('status', '{"lane_\u00e9id":"safe"}', &
            request, ierr, message)
        call check(ierr /= 0 .and. index(message, 'unsupported') > 0, &
            'non-ASCII escaped key cannot alias lane_id')
        call parse_request('status', '{"lane_id":"caf\u00e9"}', &
            request, ierr, message)
        call check(ierr == 0 .and. trim(request%lane_id) == &
            'caf'//achar(195)//achar(169), 'decodes a Unicode value to UTF-8')
        call parse_request('start', '{"targets":["\u20ac","\ud83d\ude80"]}', &
            request, ierr, message)
        call check(ierr == 0 .and. request%n_targets == 2, &
            'accepts BMP and supplementary Unicode targets')
        call check(trim(request%targets(1)) == achar(226)//achar(130)//achar(172), &
            'decodes a three-byte Unicode value')
        call check(trim(request%targets(2)) == &
            achar(240)//achar(159)//achar(154)//achar(128), &
            'decodes surrogate pairs to four-byte UTF-8')
        call parse_request('start', '{"targets":["caf\u00e9","caf'// &
            achar(195)//achar(169)//'"]}', request, ierr, message)
        call check(ierr /= 0 .and. index(message, 'duplicate target') > 0, &
            'literal and escaped Unicode identify the same target')
        call parse_request('status', '{"lane_id":"bad\ud800"}', &
            request, ierr, message)
        call check(ierr /= 0 .and. index(message, 'malformed') > 0, &
            'rejects an unpaired high surrogate')
        call parse_request('status', '{"lane_id":"bad\udc00"}', &
            request, ierr, message)
        call check(ierr /= 0 .and. index(message, 'malformed') > 0, &
            'rejects an unpaired low surrogate')
    end subroutine test_strict_request_validation

    subroutine test_unknown_terminal_read_is_side_effect_free()
        type(gremlin_session_t) :: session
        character(len=512) :: state_root, project_dir, message
        character(len=2048) :: live_status
        character(kind=c_char) :: status(65536), journal(4097)
        integer(c_int) :: c_error
        character(len=:), allocatable :: response_json
        integer :: ierr, exitcode, release_error
        logical :: directory_exists

        call make_tmpfile('fo-gremlin-unknown-state', state_root)
        call make_tmpfile('fo-gremlin-unknown-project', project_dir)
        call fs_make_dir(trim(project_dir))
        ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(state_root)//c_null_char, 1_c_int)
        call check(ierr == 0, 'sets isolated state for unknown terminal lookup')
        inquire (file=trim(state_root)//'/fo/gremlin/.', exist=directory_exists)
        call check(.not. directory_exists, 'starts with no Gremlin state directories')

        call gremlin_handle('status', trim(project_dir), &
            '{"lane_id":"unknown","session_id":"unknown-session"}', &
            response_json, exitcode)
        call check(exitcode /= 0 .and. index(response_json, 'error') > 0, &
            'public status reports an unknown session as missing')
        inquire (file=trim(state_root)//'/fo/gremlin/.', exist=directory_exists)
        call check(.not. directory_exists, &
            'public unknown-session status leaves the state tree absent')

        status = c_null_char
        journal = c_null_char
        c_error = c_terminal_read(trim(project_dir)//c_null_char, 'unknown'//c_null_char, &
            'unknown-session'//c_null_char, status, int(size(status), c_int), journal, &
            int(size(journal), c_int))
        call check(c_error /= 0_c_int, 'unknown terminal session returns not found')
        inquire (file=trim(state_root)//'/fo/gremlin/.', exist=directory_exists)
        call check(.not. directory_exists, &
            'unknown terminal read leaves the state tree absent')

        call gremlin_session_acquire(trim(project_dir), 'unknown', session, &
            ierr, message)
        call check(ierr == 0 .and. session%owner, &
            'creates a real session after the missing lookup')
        if (ierr == 0) then
            live_status = '{"protocol":1,"session_id":"'// &
                trim(session%session_id)//'","lane_id":"unknown","state":"testing"}'
            call gremlin_session_publish(session, trim(live_status), ierr, message)
            call check(ierr == 0, 'publishes real session status')
            call gremlin_handle('status', trim(project_dir), &
                '{"lane_id":"unknown","session_id":"'// &
                trim(session%session_id)//'"}', response_json, exitcode)
            call check(exitcode == 0 .and. &
                index(response_json, trim(session%session_id)) > 0 .and. &
                index(response_json, '"state":"testing"') > 0, &
                'public status reads the existing session')
            call gremlin_session_release(session, release_error, message)
            call check(release_error == 0, 'releases real session after status read')
        end if
        call fs_remove_tree(trim(state_root))
        call fs_remove_tree(trim(project_dir))
    end subroutine test_unknown_terminal_read_is_side_effect_free

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
        parts(1) = 'v1|r=32|s=0|c=60|t=0|j=1|changed=false|shuffle=false'
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

    subroutine test_readiness_status_and_semantic_waits()
        type(gremlin_session_t) :: session
        type(coverage_epoch_t) :: coverage
        character(len=512) :: state_root, project_dir, message, journal_path
        character(len=512) :: coverage_path, freshness_path
        character(len=64) :: generation, pending_generation, cursor_text
        character(len=128) :: names(3), id
        character(len=8192) :: status_text, record
        character(len=:), allocatable :: response_json
        integer :: ierr, exitcode, release_error, status, digest_position, attempt
        integer(c_int) :: freshness_pid, child_status
        integer(int64) :: freshness_ticket, earlier_poll, later_request

        call make_tmpfile('fo-gremlin-readiness-state', state_root)
        call make_tmpfile('fo-gremlin-readiness-project', project_dir)
        call fs_make_dir(trim(project_dir))
        ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(state_root)//c_null_char, 1_c_int)
        call check(ierr == 0, 'sets isolated state for readiness API facts')
        call gremlin_session_acquire(trim(project_dir), 'readiness', session, &
            ierr, message)
        call check(ierr == 0 .and. session%owner, 'creates readiness API session')
        if (ierr /= 0) return

        ! This component fixture injects an immutable ledger and status. Its
        ! independent Fortran provider serves the owner acknowledgment boundary.
        ! Real filesystem watches are exercised by the public process oracle.
        freshness_path = trim(session%state_dir)//'/freshness-'//session%session_id
        freshness_ticket = 0_int64
        call gremlin_freshness_update(trim(freshness_path), 1, freshness_ticket, status)
        call gremlin_freshness_update(trim(freshness_path), 0, freshness_ticket, status)
        earlier_poll = freshness_ticket
        call gremlin_freshness_update(trim(freshness_path), 1, freshness_ticket, status)
        later_request = freshness_ticket
        call gremlin_freshness_update(trim(freshness_path), 2, earlier_poll, status)
        call gremlin_freshness_update(trim(freshness_path), 3, freshness_ticket, status)
        call check(status == 0 .and. freshness_ticket == earlier_poll .and. &
            freshness_ticket < later_request, &
            'an earlier provider poll cannot acknowledge a later consumer request')

        freshness_pid = c_fork()
        call check(freshness_pid >= 0, 'starts isolated component freshness provider')
        if (freshness_pid == 0) then
            do attempt = 1, 12000
                freshness_ticket = 0_int64
                call gremlin_freshness_update(trim(freshness_path), 0, &
                    freshness_ticket, status)
                if (status == 0 .and. freshness_ticket > 0_int64) &
                    call gremlin_freshness_update(trim(freshness_path), 2, &
                    freshness_ticket, status)
                call fs_sleep_ms(5)
            end do
            call c_exit(0_c_int)
        end if

        generation = repeat('c', 64)
        pending_generation = repeat('d', 64)
        status_text = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
            '","lane_id":"readiness","state":"testing",'// &
            '"active_generation":"'//generation// &
            '","candidate_generation":"'//generation// &
            '","requirement_digest":"'//repeat('e',64)//'","event_epoch":0,'// &
            '"gate_required":1,"completed":0,"selected":3,"seed":9,'// &
            '"last_outcome":"NONE","last_exitcode":0}'
        call gremlin_session_publish(session, trim(status_text), ierr, message)
        call check(ierr == 0, 'publishes exact active-generation status')
        call session_journal_file(trim(project_dir), 'readiness', &
            session%session_id, journal_path, ierr)
        call check(ierr == 0, 'resolves readiness receipt journal')

        id = trim(session%session_id)//'-build'
        record = '{"completion_id":"'//trim(id)//'","session_id":"'// &
            trim(session%session_id)//'","lane_id":"readiness",'// &
            '"generation":"'//generation//'","case_id":"<build>",'// &
            '"status":"BUILD_PASS","outcome":"pass"}'
        call journal_append(trim(journal_path), trim(id), trim(record), ierr, message)
        call check(ierr == 0, 'records the active generation build pass')

        names = ''
        names(1) = 'test_fast_one'
        names(2) = 'test_fast_two'
        names(3) = 'test_readiness_slow'
        coverage_path = trim(session%state_dir)//'/coverage-'//generation//'.state'
        call coverage_open(trim(coverage_path), generation, names, 3, 9, coverage, &
            status, message)
        call check(status == COVERAGE_OK, 'opens exact-generation coverage inventory')
        call coverage_record(coverage, 'test_fast_one', 'PASS', status, message)
        call check(status == COVERAGE_OK, 'records sampled ordinary case pass')

        id = trim(session%session_id)//'-gate'
        record = '{"completion_id":"'//trim(id)//'","session_id":"'// &
            trim(session%session_id)//'","lane_id":"readiness",'// &
            '"generation":"'//generation//'","case_id":"test_fast_one",'// &
            '"status":"PASS","outcome":"pass","gate_required":true,"requirement_digest":"'// &
            repeat('e',64)//'"}'
        call journal_append(trim(journal_path), trim(id), trim(record), ierr, message)
        call check(ierr == 0, 'records one explicitly required gate case')

        call gremlin_handle('status', trim(project_dir), '{"lane_id":"readiness"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. &
            index(response_json, '"local_gate_green":true') > 0 .and. &
            index(response_json, '"verification_level":"gate"') > 0 .and. &
            index(response_json, '"ordinary_required":2') > 0 .and. &
            index(response_json, '"ordinary_passed":1') > 0, &
            'status reports a green partial gate before ordinary completion')
        digest_position = index(status_text, repeat('e', 64))
        status_text(digest_position:digest_position + 63) = repeat('f', 64)
        call gremlin_session_publish(session, trim(status_text), ierr, message)
        call gremlin_handle('status', trim(project_dir), '{"lane_id":"readiness"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. &
            index(response_json, '"local_gate_green":false') > 0 .and. &
            index(response_json, '"gate_token":""') > 0, &
            'changed requirements invalidate receipts and clear the token before reuse')
        call gremlin_handle('wait', trim(project_dir), &
            '{"lane_id":"readiness","wait_until":"local-gate-green",'// &
            '"wait_ms":0}', response_json, exitcode)
        call check(exitcode == 0 .and. &
            index(response_json, '"wait_satisfied":false') > 0 .and. &
            index(response_json, '"gate_token":""') > 0, &
            'changed requirements prevent typed wait from returning historical green')

        status_text(digest_position:digest_position + 63) = repeat('e', 64)
        call gremlin_session_publish(session, trim(status_text), ierr, message)
        call gremlin_handle('wait', trim(project_dir), &
            '{"lane_id":"readiness","wait_until":"local-gate-green",'// &
            '"wait_ms":0}', response_json, exitcode)
        call check(exitcode == 0 .and. &
            index(response_json, '"wait_satisfied":true') > 0 .and. &
            index(response_json, '"wait_timed_out":false') > 0, &
            'zero-duration semantic wait succeeds when its fact is already true')
        call gremlin_handle('wait', trim(project_dir), &
            '{"lane_id":"readiness","wait_until":"ordinary-verified",'// &
            '"wait_ms":0}', response_json, exitcode)
        call check(exitcode == 0 .and. &
            index(response_json, '"wait_satisfied":false') > 0 .and. &
            index(response_json, '"wait_timed_out":true') > 0, &
            'zero-duration semantic wait reports an explicit timeout when false')

        call coverage_record(coverage, 'test_fast_two', 'PASS', status, message)
        call check(status == COVERAGE_OK, 'finishes ordinary inventory independently')
        call gremlin_handle('status', trim(project_dir), '{"lane_id":"readiness"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. &
            index(response_json, '"verification_level":"ordinary"') > 0 .and. &
            index(response_json, '"fully_verified":false') > 0, &
            'status advances to ordinary facts without implying full verification')

        call coverage_record(coverage, 'test_readiness_slow', 'PASS', status, message)
        call check(status == COVERAGE_OK, 'finishes slow full-inventory case')
        call gremlin_handle('wait', trim(project_dir), &
            '{"lane_id":"readiness","wait_until":"fully-verified",'// &
            '"wait_ms":0}', response_json, exitcode)
        call check(exitcode == 0 .and. &
            index(response_json, '"verification_level":"full"') > 0 .and. &
            index(response_json, '"fully_verified":true') > 0 .and. &
            index(response_json, '"wait_satisfied":true') > 0, &
            'full semantic wait observes every supported inventory pass')

        call gremlin_session_publish(session, &
            trim(status_text(:len_trim(status_text) - 1))//',"input_changed":true}', &
            ierr, message)
        call gremlin_handle('status', trim(project_dir), '{"lane_id":"readiness"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. &
            index(response_json, '"local_gate_green":false') > 0 .and. &
            index(response_json, '"fully_verified":false') > 0 .and. &
            index(response_json, '"verification_level":"none"') > 0, &
            'a watcher event invalidates even completed receipts before recapture')
        status_text = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
            '","lane_id":"readiness","state":"building",'// &
            '"active_generation":"'//generation// &
            '","candidate_generation":"'//pending_generation// &
            '","requirement_digest":"'//repeat('e',64)//'","event_epoch":0,'// &
            '"gate_required":1,"completed":3,"selected":3,"seed":9,'// &
            '"last_outcome":"NONE","last_exitcode":0}'
        call gremlin_session_publish(session, trim(status_text), ierr, message)
        call check(ierr == 0, 'publishes a newer pending candidate')
        call gremlin_handle('status', trim(project_dir), '{"lane_id":"readiness"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. &
            index(response_json, '"dirty":true') > 0 .and. &
            index(response_json, '"local_gate_green":false') > 0, &
            'pending candidate makes exact active-generation readiness stale')

        id = trim(session%session_id)//'-failure'
        record = '{"completion_id":"'//trim(id)//'","session_id":"'// &
            trim(session%session_id)//'","lane_id":"readiness",'// &
            '"generation":"'//generation//'","case_id":"test_fast_two",'// &
            '"status":"FAIL","outcome":"fail","gate_required":false}'
        call journal_append(trim(journal_path), trim(id), trim(record), ierr, message)
        call check(ierr == 0, 'records a durable active-generation failure')
        status_text = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
            '","lane_id":"readiness","state":"testing",'// &
            '"active_generation":"'//generation// &
            '","candidate_generation":"'//generation// &
            '","requirement_digest":"'//repeat('e',64)//'","event_epoch":0,'// &
            '"gate_required":1,"completed":4,"selected":3,"seed":9,'// &
            '"last_outcome":"FAIL","last_exitcode":1}'
        call gremlin_session_publish(session, trim(status_text), ierr, message)
        call check(ierr == 0, 'returns readiness status to active generation')
        call gremlin_handle('status', trim(project_dir), '{"lane_id":"readiness"}', &
            response_json, exitcode)
        call check(exitcode == 0 .and. &
            index(response_json, '"health":"failure"') > 0 .and. &
            index(response_json, '"local_gate_green":false') > 0, &
            'durable active-generation failure prevents gate green')
        cursor_text = ''
        call extract_json_field(response_json, 'next_cursor', cursor_text)
        call gremlin_handle('wait', trim(project_dir), &
            '{"lane_id":"readiness","cursor":'//trim(cursor_text)// &
            ',"fail_on_failure":true,"wait_ms":0}', response_json, exitcode)
        call check(exitcode == 1 .and. &
            index(response_json, '"failure_observed":true') > 0 .and. &
            index(response_json, '"durable_failure_report"') > 0 .and. &
            index(response_json, 'test_fast_two') > 0, &
            'failure wait returns durable receipt even after its cursor passed it')

        if (freshness_pid > 0) then
            status = c_kill(freshness_pid, 15_c_int)
            status = c_waitpid(freshness_pid, child_status, 0_c_int)
        end if
        call gremlin_session_release(session, release_error, message)
        call check(release_error == 0, 'releases readiness API session')
        call fs_remove_tree(trim(state_root))
        call fs_remove_tree(trim(project_dir))
    end subroutine test_readiness_status_and_semantic_waits

    subroutine test_terminal_session_history()
        type(gremlin_session_t) :: first, second
        character(len=512) :: state_root, project_dir, journal_path
        character(len=8192) :: status_text, final_status, record
        character(len=1024) :: response
        character(len=64) :: cursor_text
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
        cursor_text = ''
        call extract_json_field(response_json, 'next_cursor', cursor_text)
        call gremlin_handle('wait', trim(project_dir), &
            '{"lane_id":"history","session_id":"'//trim(first%session_id)// &
            '","cursor":'//trim(cursor_text)//',"fail_on_failure":true}', &
            response_json, exitcode)
        call check(exitcode == 1 .and. &
            index(response_json, '"durable_failure_report"') > 0 .and. &
            index(response_json, 'first_case') > 0, &
            'wait includes durable failures when the receipt cursor already passed them')
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

    subroutine test_failed_final_status_uses_constructed_terminal()
        type(gremlin_session_t) :: session
        character(len=512) :: state_root, project_dir, message
        character(len=2048) :: prior_status, stopped_status, live_status
        character(len=128) :: read_session_id, owner_start
        character(kind=c_char) :: terminal_status(8192), terminal_journal(4097)
        character(len=:), allocatable :: terminal_text, response_json
        integer(c_int) :: c_error
        integer :: ierr, read_error, release_error, exitcode, owner_pid

        call make_tmpfile('fo-gremlin-final-state', state_root)
        call make_tmpfile('fo-gremlin-final-project', project_dir)
        call fs_make_dir(trim(project_dir))
        ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, &
            trim(state_root)//c_null_char, 1_c_int)
        call check(ierr == 0, 'sets isolated state for failed final publication')
        call gremlin_session_acquire(trim(project_dir), 'final-write', session, &
            ierr, message)
        call check(ierr == 0 .and. session%owner, 'creates owner for final publication')
        if (ierr /= 0) then
            call fs_remove_tree(trim(state_root))
            call fs_remove_tree(trim(project_dir))
            return
        end if
        prior_status = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
            '","lane_id":"final-write","state":"testing"}'
        call gremlin_session_publish(session, trim(prior_status), ierr, message)
        call check(ierr == 0, 'publishes readable prior status')
        stopped_status = '{"protocol":1,"session_id":"'//trim(session%session_id)// &
            '","lane_id":"final-write","state":"stopped",'// &
            '"last_outcome":"CANCELLED"}'

        ierr = c_chmod(session%state_dir//c_null_char, 320_c_int)
        call check(ierr == 0, 'makes owner status directory read-only')
        call gremlin_session_publish(session, trim(stopped_status), ierr, message)
        call check(ierr /= 0, 'final stopped status write fails with prior status present')
        call gremlin_session_read(trim(project_dir), 'final-write', read_session_id, &
            owner_pid, owner_start, live_status, read_error, message)
        call check(read_error == 0 .and. trim(read_session_id) == &
            trim(session%session_id) .and. trim(live_status) == trim(prior_status), &
            'failed final write leaves the prior live status readable')

        call gremlin_release_stopped_session(trim(project_dir), session, &
            trim(stopped_status), release_error, message)
        call check(release_error /= 0, &
            'does not release ownership while the status directory blocks cleanup')
        terminal_status = c_null_char
        terminal_journal = c_null_char
        c_error = c_terminal_read(trim(project_dir)//c_null_char, &
            'final-write'//c_null_char, session%session_id//c_null_char, &
            terminal_status, int(size(terminal_status), c_int), terminal_journal, &
            int(size(terminal_journal), c_int))
        terminal_text = c_buffer_text(terminal_status)
        call check(c_error == 0_c_int .and. index(terminal_text, '"state":"stopped"') > 0 &
            .and. index(terminal_text, '"state":"testing"') == 0, &
            'terminal snapshot contains the constructed stopped status, never stale state')

        ierr = c_chmod(session%state_dir//c_null_char, 448_c_int)
        call check(ierr == 0, 'restores owner status directory permissions')
        call gremlin_session_release(session, release_error, message)
        call check(release_error == 0, 'releases owner after durable stopped snapshot')
        call gremlin_handle('status', trim(project_dir), &
            '{"lane_id":"final-write","session_id":"'// &
            trim(session%session_id)//'"}', response_json, exitcode)
        call check(exitcode == 0 .and. index(response_json, '"state":"stopped"') > 0 &
            .and. index(response_json, '"state":"testing"') == 0, &
            'released session reads the durable stopped terminal record')
        call fs_remove_tree(trim(state_root))
        call fs_remove_tree(trim(project_dir))
    end subroutine test_failed_final_status_uses_constructed_terminal

    function c_buffer_text(buffer) result(value)
        character(kind=c_char), intent(in) :: buffer(:)
        character(len=:), allocatable :: value
        integer :: i, n

        n = 0
        do i = 1, size(buffer)
            if (buffer(i) == c_null_char) exit
            n = n + 1
        end do
        allocate (character(len=n) :: value)
        do i = 1, n
            value(i:i) = buffer(i)
        end do
    end function c_buffer_text

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
