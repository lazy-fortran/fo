program test_gremlin_journal
    use, intrinsic :: iso_fortran_env, only: int64, output_unit
    use fo_gremlin_journal, only: journal_append, journal_read_page, &
        journal_record_t, JOURNAL_OK, JOURNAL_INVALID, JOURNAL_CONFLICT, &
        JOURNAL_TOO_LARGE
    use fo_process, only: process_getpid
    implicit none
    integer :: passed, failed

    passed = 0
    failed = 0
    call test_durable_receipts_and_reconnect()
    write (output_unit, '(a,i0,a,i0,a)') 'gremlin journal: ', passed, &
        ' pass, ', failed, ' fail'
    if (failed > 0) stop 1

contains

    subroutine assert(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label

        if (condition) then
            passed = passed + 1
        else
            failed = failed + 1
            write (output_unit, '(a,a)') 'FAIL: ', label
        end if
    end subroutine assert

    subroutine test_durable_receipts_and_reconnect()
        type(journal_record_t), allocatable :: records(:)
        character(len=256) :: path, message
        character(len=256) :: pass_record, fail_record, later_record
        character(len=256) :: conflict_record, cancelled_record, malformed_record
        character(len=256) :: nul_alias_record
        integer(int64) :: after_first, after_second, next_cursor
        integer :: status, unit, pid, ios

        pid = process_getpid()
        write (path, '(a,i0,a)') '/var/tmp/fo-gremlin-journal-', pid, '.jsonl'
        call remove_file(trim(path))
        call journal_read_page(trim(path), 0_int64, 10, 4096_int64, records, &
            next_cursor, status, message)
        call assert(status == JOURNAL_OK .and. size(records) == 0 .and. &
            next_cursor == 0_int64, 'a new journal starts with no known cases')
        pass_record = '{"completion_id":"run-a/pass","outcome":"pass",'// &
            '"test":"test_known_pass","stdout_ref":"artifacts/pass.out",'// &
            '"exit_code":0,"toolchain":{"name":"gfortran"}}'
        fail_record = '{"completion_id":"run-a/fail","outcome":"fail",'// &
            '"test":"test_known_failure","stderr_ref":"artifacts/fail.err",'// &
            '"exit_code":1}'
        later_record = '{"completion_id":"run-a/later","outcome":"pass",'// &
            '"test":"test_after_reconnect","exit_code":0}'
        conflict_record = '{"completion_id":"run-a/pass","outcome":"fail",'// &
            '"test":"test_known_pass","exit_code":1}'
        cancelled_record = '{"completion_id":"run-a/blocked",'// &
            '"outcome":"cancelled","test":"test_blocked"}'
        malformed_record = '{"completion_id":"run-a/bad",'// &
            '"outcome":"pass",}'
        nul_alias_record = '{"completion_id":"run-a/pass'//achar(92)// &
            'u0000suffix","outcome":"pass","test":"test_alias"}'

        call journal_append(trim(path), 'run-a/pass', trim(nul_alias_record), &
            status, message)
        call assert(status == JOURNAL_INVALID, &
            'escaped NUL cannot alias a different completion ID')

        call journal_append(trim(path), 'run-a/pass', trim(pass_record), status, &
            message)
        call assert(status == JOURNAL_OK, 'completed pass is appended')
        call journal_append(trim(path), 'run-a/fail', trim(fail_record), status, &
            message)
        call assert(status == JOURNAL_OK, 'completed failure is appended')
        call journal_append(trim(path), 'run-a/pass', trim(pass_record), status, &
            message)
        call assert(status == JOURNAL_OK, 'identical completion retry is idempotent')
        call journal_append(trim(path), 'run-a/pass', trim(conflict_record), &
            status, message)
        call assert(status == JOURNAL_CONFLICT, &
            'conflicting duplicate completion is rejected')
        call journal_append(trim(path), 'run-a/bad', trim(malformed_record), &
            status, message)
        call assert(status == JOURNAL_INVALID, 'malformed JSON is rejected')
        call journal_append(trim(path), 'run-a/blocked', trim(cancelled_record), &
            status, message)
        call assert(status == JOURNAL_INVALID, &
            'cancellation cannot be recorded as a completed verdict')

        ! Simulate a supervisor stopping while the third case is still running.
        open (newunit=unit, file=trim(path), access='stream', form='unformatted', &
            position='append', action='write', iostat=ios)
        call assert(ios == 0, 'open journal to model an interrupted writer')
        if (ios == 0) then
            write (unit, iostat=ios) '{"completion_id":"run-a/blocked",'// &
                '"outcome":"pass"'
            call assert(ios == 0, 'write an incomplete crash tail')
            close (unit)
        end if

        ! Reopening the provider models restart: only newline-terminated receipts count.
        call journal_read_page(trim(path), 0_int64, 10, 4096_int64, records, &
            after_second, status, message)
        call assert(status == JOURNAL_OK, 'reader tolerates an incomplete final line')
        call assert(size(records) == 2, 'restart recovers completed pass and failure')
        if (size(records) == 2) then
            call assert(index(records(1)%json, '"outcome":"pass"') > 0, &
                'first recovered result retains the pass oracle')
            call assert(index(records(1)%json, 'test_known_pass') > 0, &
                'first receipt retains its original test name')
            call assert(index(records(1)%json, 'artifacts/pass.out') > 0, &
                'first receipt retains its output artifact reference')
            call assert(index(records(2)%json, '"outcome":"fail"') > 0, &
                'second recovered result retains the failure oracle')
            call assert(index(records(2)%json, 'test_known_failure') > 0, &
                'second receipt retains its original test name')
        end if
        call assert(index(records_to_text(records), 'run-a/blocked') == 0, &
            'interrupted case remains unknown')

        call journal_read_page(trim(path), 0_int64, 1, 4096_int64, records, &
            after_first, status, message)
        call assert(status == JOURNAL_OK .and. size(records) == 1, &
            'page is bounded by requested record count')
        if (size(records) == 1) then
            call assert(index(records(1)%json, 'run-a/pass') > 0, &
                'first page begins with first durable receipt')
        end if
        call journal_read_page(trim(path), after_first, 1, 4096_int64, records, &
            next_cursor, status, message)
        call assert(status == JOURNAL_OK .and. size(records) == 1, &
            'reconnect page returns the next completed receipt')
        if (size(records) == 1) then
            call assert(index(records(1)%json, 'run-a/fail') > 0, &
                'reconnect does not repeat the first receipt')
        end if
        call assert(next_cursor == after_second, &
            'second page cursor lands after the second receipt')
        call journal_read_page(trim(path), after_second, 10, 4096_int64, records, &
            next_cursor, status, message)
        call assert(status == JOURNAL_OK .and. size(records) == 0, &
            'incomplete tail is withheld from a resumed reader')
        call assert(next_cursor == after_second, &
            'incomplete tail does not advance the reconnect cursor')

        call journal_read_page(trim(path), 1_int64, 10, 4096_int64, records, &
            next_cursor, status, message)
        call assert(status == JOURNAL_INVALID, &
            'reader rejects a cursor inside a receipt')
        call journal_read_page(trim(path), 0_int64, 10, 1_int64, records, &
            next_cursor, status, message)
        call assert(status == JOURNAL_TOO_LARGE .and. next_cursor == 0_int64, &
            'reader rejects a page limit smaller than its first receipt')

        call journal_append(trim(path), 'run-a/later', trim(later_record), status, &
            message)
        call assert(status == JOURNAL_OK, 'append recovers after an incomplete tail')
        call journal_read_page(trim(path), after_second, 10, 4096_int64, records, &
            next_cursor, status, message)
        call assert(status == JOURNAL_OK .and. size(records) == 1, &
            'reconnected reader sees the later completion')
        if (size(records) == 1) then
            call assert(index(records(1)%json, 'run-a/later') > 0, &
                'recovered tail is not concatenated with a new receipt')
        end if
        call assert(index(records_to_text(records), 'run-a/blocked') == 0, &
            'unfinished case remains unknown after tail recovery')
        call remove_file(trim(path))
    end subroutine test_durable_receipts_and_reconnect

    subroutine remove_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        logical :: exists

        inquire (file=path, exist=exists)
        if (.not. exists) return
        open (newunit=unit, file=path, status='old', iostat=ios)
        if (ios == 0) close (unit, status='delete', iostat=ios)
    end subroutine remove_file

    function records_to_text(records) result(text)
        type(journal_record_t), intent(in) :: records(:)
        character(len=:), allocatable :: text
        integer :: i

        text = ''
        do i = 1, size(records)
            text = text//records(i)%json
        end do
    end function records_to_text

end program test_gremlin_journal
