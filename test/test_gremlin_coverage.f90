program test_gremlin_coverage
    use fo_gremlin_coverage, only: coverage_epoch_t, coverage_open, &
        coverage_next_chunk, coverage_record, coverage_remaining, coverage_view, &
        coverage_begin_epoch, coverage_read_view_path, coverage_recover_running, &
        COVERAGE_OK, COVERAGE_INVALID
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    use fo_gremlin_policy, only: shuffle_gremlin_tests, GREMLIN_POLICY_OK
    implicit none

    integer, parameter :: n_cases = 40, chunk = 4
    character(len=16) :: names(n_cases), reversed(n_cases), selected(n_cases)
    character(len=16) :: priorities(2)
    logical :: visited(n_cases)
    type(coverage_epoch_t) :: state, resumed, replay
    type(gremlin_coverage_view_t) :: view
    character(len=256) :: message
    character(len=32) :: path
    integer :: i, n_selected, status, runs

    path = 'gremlin-coverage-test.state'
    call execute_command_line('rm -f '//trim(path)//' '//trim(path)//'.tmp '// &
        trim(path)//'.lock gremlin-coverage-replay.state '// &
        'gremlin-coverage-replay.state.lock')
    do i = 1, n_cases
        write(names(i), '(a,i2.2)') 'case_', i
        reversed(i) = names(n_cases - i + 1)
    end do
    priorities = [character(len=16) :: 'case_40', 'case_39']
    visited = .false.

    call coverage_open(trim(path), repeat('a', 64), names, n_cases, 173, &
        state, status, message)
    call check(status == COVERAGE_OK, 'initializes generation epoch')
    call check(state%epoch == 1 .and. state%seed == 173, 'stores epoch and seed')
    do i = 1, 4
        call coverage_next_chunk(state, priorities, 2, chunk, selected, n_selected, status)
        call check(status == COVERAGE_OK .and. n_selected == chunk, &
            'returns four-case campaign chunk')
        if (i == 1) then
            call check(selected(1) == 'case_40' .and. selected(2) == 'case_39', &
                'priority cases jump ahead')
        end if
        call record_chunk(state, selected, n_selected, visited)
    end do
    call coverage_next_chunk(state, priorities, 2, 1, selected, n_selected, status)
    call check(status == COVERAGE_OK .and. n_selected == 1, &
        'completes exactly seventeen receipts before restart')
    call record_chunk(state, selected, n_selected, visited)
    call check(coverage_remaining(state) == 23, &
        'seventeen durable outcomes leave twenty-three unknown')

    do i = 1, n_cases
        reversed(i) = names(n_cases - i + 1)
    end do
    call coverage_open(trim(path), repeat('a', 64), reversed, n_cases, 999, &
        resumed, status, message)
    call check(status == COVERAGE_OK, 'recovers interrupted epoch')
    call check(resumed%seed == 173 .and. resumed%epoch == 1, &
        'restart preserves seed and epoch')
    call check(resumed%cursor == state%cursor, 'restart preserves cursor')
    call check(resumed%inventory_digest == state%inventory_digest, &
        'inventory digest ignores input order')
    call coverage_open('gremlin-coverage-replay.state', repeat('a', 64), names, &
        n_cases, 173, replay, status, message)
    call coverage_next_chunk(replay, priorities, 2, chunk, selected, n_selected, status)
    call check(selected(1) == 'case_40' .and. selected(2) == 'case_39', &
        'same seed replays first priority prefix')

    runs = 17
    do while (coverage_remaining(resumed) > 0)
        call coverage_next_chunk(resumed, priorities, 2, chunk, selected, &
            n_selected, status)
        call check(status == COVERAGE_OK .and. n_selected > 0, &
            'campaign produces next coverage chunk')
        call record_chunk(resumed, selected, n_selected, visited)
        runs = runs + n_selected
    end do
    call check(runs == n_cases, 'no case repeats before epoch exhaustion')
    call check(all(visited), 'all forty eligible cases were observed')
    call coverage_view(resumed, view)
    call check(view%eligible_count == n_cases .and. view%completed_count == n_cases, &
        'typed coverage view reports exact counts')
    call check(view%unknown_count == 0 .and. view%full_coverage, &
        'full coverage requires every current-generation case')
    call coverage_next_chunk(resumed, priorities, 2, chunk, selected, n_selected, status)
    call check(resumed%epoch == 1 .and. n_selected == 0, &
        'does not restart epoch within the same loaded state')
    call coverage_open(trim(path), repeat('a', 64), names, n_cases, 300, replay, &
        status, message)
    call check(status == COVERAGE_OK .and. replay%epoch == 1 .and. &
        coverage_remaining(replay) == 0, 'opening a completed epoch does not roll it')
    call coverage_begin_epoch(replay, 301, status, message)
    call check(status == COVERAGE_OK .and. replay%epoch == 2 .and. &
        replay%seed == 301 .and. coverage_remaining(replay) == n_cases, &
        'a new epoch begins only through the explicit transition')

    call check_mixed_outcomes()
    call check_stale_writers()

    call execute_command_line('rm -f '//trim(path)//' '//trim(path)//'.tmp '// &
        trim(path)//'.lock gremlin-coverage-replay.state '// &
        'gremlin-coverage-replay.state.lock')
    write(*, '(a)') 'Gremlin coverage behavioral oracle: PASS'

contains

    subroutine check_stale_writers()
        type(coverage_epoch_t) :: first, second, latest
        character(len=16) :: transaction_names(3)
        character(len=256) :: transaction_path

        transaction_names = [character(len=16) :: 'transaction_1', &
            'transaction_2', 'transaction_3']
        transaction_path = 'gremlin-coverage-transaction.state'
        call execute_command_line('rm -f '//trim(transaction_path))
        call coverage_open(trim(transaction_path), repeat('d', 64), transaction_names, &
            3, 91, first, status, message)
        call check(status == COVERAGE_OK, 'creates transaction fixture')
        call coverage_open(trim(transaction_path), repeat('d', 64), transaction_names, &
            3, 91, second, status, message)
        call check(status == COVERAGE_OK, 'opens independent writer snapshot')
        call coverage_record(first, transaction_names(1), 'PASS', status, message)
        call check(status == COVERAGE_OK, 'first writer records pass')
        call coverage_record(second, transaction_names(2), 'FAIL', status, message)
        call check(status == COVERAGE_OK, 'stale second writer records different case')
        call coverage_record(first, transaction_names(3), 'TIMEOUT', status, message)
        call check(status == COVERAGE_OK, 'stale first writer records third case')
        call coverage_open(trim(transaction_path), repeat('d', 64), transaction_names, &
            3, 91, latest, status, message, persist=.false.)
        call check(status == COVERAGE_OK, 'reads committed transaction results')
        call check(all(latest%outcome == [character(len=16) :: 'PASS', 'FAIL', &
            'TIMEOUT']), 'independent stale writers cannot lose other completions')
        call coverage_begin_epoch(first, 92, status, message)
        call check(status == COVERAGE_OK, 'explicit epoch uses latest completed state')
        call coverage_record(second, transaction_names(2), 'PASS', status, message)
        call check(status == COVERAGE_INVALID, 'stale epoch cannot overwrite new epoch')
        call execute_command_line('rm -f '//trim(transaction_path)//' '// &
            trim(transaction_path)//'.lock')
    end subroutine check_stale_writers

    subroutine check_mixed_outcomes()
        character(len=16) :: mixed_names(8), mixed_selected(8), mixed_priorities(8)
        character(len=256) :: mixed_path
        type(coverage_epoch_t) :: mixed, mixed_recovered
        type(gremlin_coverage_view_t) :: mixed_view
        integer :: j, selected_count, priority_count, shuffle_status

        mixed_path = 'gremlin-coverage-outcomes.state'
        call execute_command_line('rm -f '//trim(mixed_path))
        do j = 1, size(mixed_names)
            write(mixed_names(j), '(a,i1)') 'mixed_', j
        end do
        mixed_priorities = mixed_names
        call coverage_open(trim(mixed_path), repeat('b', 64), mixed_names, &
            size(mixed_names), 91, mixed, status, message)
        call check(status == COVERAGE_OK, 'creates mixed-outcome epoch')
        call coverage_record(mixed, mixed_names(1), 'PASS', status, message)
        call check(status == COVERAGE_OK, 'records PASS outcome')
        call coverage_record(mixed, mixed_names(2), 'FAIL', status, message)
        call check(status == COVERAGE_OK, 'records FAIL outcome')
        call coverage_record(mixed, mixed_names(3), 'TIMEOUT', status, message)
        call check(status == COVERAGE_OK, 'records TIMEOUT outcome')
        call coverage_record(mixed, mixed_names(4), 'FLAKY', status, message)
        call check(status == COVERAGE_OK, 'records FLAKY outcome')
        call coverage_record(mixed, mixed_names(5), 'INFRA_ERROR', status, message)
        call check(status == COVERAGE_OK, 'records INFRA_ERROR outcome')
        call coverage_record(mixed, mixed_names(6), 'RUNNING', status, message)
        call check(status == COVERAGE_OK, 'records RUNNING as outstanding')
        call coverage_record(mixed, mixed_names(7), 'CANCELLED', status, message)
        call check(status == COVERAGE_OK, 'records CANCELLED as outstanding')
        call coverage_record(mixed, mixed_names(8), 'UNKNOWN', status, message)
        call check(status == COVERAGE_OK, 'records UNKNOWN as outstanding')
        call check(coverage_remaining(mixed) == 3, &
            'failures are attempted while running/cancelled remain outstanding')
        call coverage_view(mixed, mixed_view)
        call check(mixed_view%completed_count == 5 .and. &
            mixed_view%pass_count == 1 .and. mixed_view%fail_count == 1 .and. &
            mixed_view%timeout_count == 1 .and. mixed_view%flaky_count == 1 .and. &
            mixed_view%infra_count == 1, 'typed counts distinguish every attempted result')
        call check(mixed_view%running_count == 1 .and. &
            mixed_view%cancelled_count == 1 .and. mixed_view%unknown_count == 3 .and. &
            mixed_view%remaining_count == 3 .and. &
            .not. mixed_view%full_coverage, 'outstanding statuses cannot be green')
        ! Recovery is inspected before any selection or child launch can hide it.
        call coverage_recover_running(mixed, status, message)
        call check(status == COVERAGE_OK, 'recover a dead owner launch intent')
        call coverage_read_view_path(trim(mixed_path), repeat('b', 64), mixed_view, &
            status, message)
        call check(status == COVERAGE_OK, 'read durable pre-relaunch recovery state')
        call check(mixed_view%running_count == 0 .and. &
            mixed_view%cancelled_count == 1 .and. mixed_view%unknown_count == 3 .and. &
            mixed_view%remaining_count == 3 .and. mixed_view%completed_count == 5, &
            'recovery removes stale running without satisfying its obligation')
        call check(mixed_view%epoch == 1 .and. mixed_view%seed == 91, &
            'recovery preserves the same finite epoch identity')
        call coverage_next_chunk(mixed, mixed_priorities, size(mixed_priorities), &
            size(mixed_selected), mixed_selected, selected_count, status, priority_count)
        call check(status == COVERAGE_OK .and. selected_count == 3 .and. &
            priority_count == 3, 'resume reselects all outstanding labels')
        call shuffle_gremlin_tests(mixed_selected, priority_count, selected_count, &
            91, shuffle_status)
        call check(shuffle_status == GREMLIN_POLICY_OK .and. selected_count == 3, &
            'shuffle accepts only actually selected priority cases near exhaustion')
        do j = 1, selected_count
            call coverage_record(mixed, mixed_selected(j), 'PASS', status, message)
            call check(status == COVERAGE_OK, 'records recovered outstanding case')
        end do
        call check(coverage_remaining(mixed) == 0, 'mixed epoch reaches attempted completion')
        call coverage_view(mixed, mixed_view)
        call check(mixed_view%full_coverage .and. mixed_view%pass_count == 4 .and. &
            mixed_view%fail_count == 1 .and. .not. mixed_view%green, &
            'coverage completion is distinct from all-pass')
        call coverage_read_view_path(trim(mixed_path), repeat('b', 64), &
            mixed_view, status, message)
        call check(status == COVERAGE_OK .and. mixed_view%full_coverage .and. &
            mixed_view%pass_count == 4, 'production status query reads typed coverage counts')
        call coverage_record(mixed, mixed_names(8), 'BOGUS', status, message)
        call check(status == COVERAGE_INVALID, 'rejects invalid runtime outcome label')
        call corrupt_outcome_file(trim(mixed_path))
        call coverage_open(trim(mixed_path), repeat('b', 64), mixed_names, &
            size(mixed_names), 91, mixed_recovered, status, message)
        call check(status == COVERAGE_INVALID, 'rejects invalid persisted outcome label')
        call execute_command_line('rm -f '//trim(mixed_path)//' '//trim(mixed_path)//'.lock')
    end subroutine check_mixed_outcomes

    subroutine corrupt_outcome_file(filename)
        character(len=*), intent(in) :: filename
        character(len=256) :: lines(10)
        integer :: input_unit, output_unit, ios, j, split
        open(newunit=input_unit, file=filename, status='old', action='read', iostat=ios)
        call check(ios == 0, 'opens state file for invalid-label fixture')
        do j = 1, size(lines)
            read(input_unit, '(a)', iostat=ios) lines(j)
            call check(ios == 0, 'reads coverage state fixture')
        end do
        close(input_unit)
        split = index(lines(3), '|')
        lines(3) = lines(3)(:split)//'BOGUS'
        open(newunit=output_unit, file=filename, status='replace', &
            action='write', iostat=ios)
        call check(ios == 0, 'rewrites state file with an invalid label')
        do j = 1, size(lines)
            write(output_unit, '(a)') trim(lines(j))
        end do
        close(output_unit)
    end subroutine corrupt_outcome_file

    subroutine record_chunk(epoch, cases, count_cases, seen)
        type(coverage_epoch_t), intent(inout) :: epoch
        character(len=*), intent(in) :: cases(:)
        integer, intent(in) :: count_cases
        logical, intent(inout) :: seen(:)
        integer :: j, case_index
        do j = 1, count_cases
            case_index = 0
            read(cases(j)(6:7), *) case_index
            call check(.not. seen(case_index), 'case executes at most once per epoch')
            seen(case_index) = .true.
            call coverage_record(epoch, cases(j), 'PASS', status, message)
            call check(status == COVERAGE_OK, 'persists current-generation outcome')
        end do
    end subroutine record_chunk

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label
        if (condition) return
        write(*, '(a)') 'FAIL: '//trim(label)
        error stop 1
    end subroutine check

end program test_gremlin_coverage
