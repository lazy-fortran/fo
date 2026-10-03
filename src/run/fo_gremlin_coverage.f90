module fo_gremlin_coverage
    use, intrinsic :: iso_fortran_env, only: int64, iostat_end
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_size_t
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_gremlin_coverage_view, only: gremlin_coverage_view_t
    implicit none
    private

    integer, parameter, public :: COVERAGE_OK = 0
    integer, parameter, public :: COVERAGE_INVALID = 1
    integer, parameter, public :: COVERAGE_IO_ERROR = 2
    integer, parameter, public :: COVERAGE_NOT_FOUND = 3
    integer, parameter :: NAME_LEN = 128

    type, public :: coverage_epoch_t
        character(len=HASH_LEN) :: generation = ''
        character(len=HASH_LEN) :: inventory_digest = ''
        character(len=NAME_LEN), allocatable :: inventory(:)
        character(len=NAME_LEN), allocatable :: order(:)
        character(len=16), allocatable :: outcome(:)
        integer :: epoch = 1
        integer :: seed = 1
        integer :: cursor = 1
        character(len=:), allocatable :: path
    end type coverage_epoch_t

    public :: coverage_open, coverage_next_chunk, coverage_record
    public :: coverage_remaining, coverage_view, coverage_record_path
    public :: coverage_read_view_path, coverage_begin_epoch, coverage_recover_running

    interface
        integer(c_int) function coverage_lock(path) &
                bind(C, name='fo_c_gremlin_coverage_lock')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function coverage_lock
        subroutine coverage_unlock(fd) bind(C, name='fo_c_gremlin_coverage_unlock')
            import :: c_int
            integer(c_int), value :: fd
        end subroutine coverage_unlock
        integer(c_int) function coverage_publish(path, data, data_len) &
                bind(C, name='fo_c_gremlin_coverage_publish')
            import :: c_char, c_int, c_size_t
            character(kind=c_char), intent(in) :: path(*), data(*)
            integer(c_size_t), value :: data_len
        end function coverage_publish
    end interface

contains

    ! All mutations reload the latest state under the same lock as publication.
    ! A process-local snapshot must never overwrite another process's completion.
    subroutine coverage_open(path, generation, names, n_names, seed, state, status, &
            message, advance_completed, persist)
        character(len=*), intent(in) :: path, generation, names(:)
        integer, intent(in) :: n_names, seed
        type(coverage_epoch_t), intent(out) :: state
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        logical, intent(in), optional :: advance_completed, persist
        integer(c_int) :: fd

        call lock_state(path, fd, status, message)
        if (status /= COVERAGE_OK) return
        call coverage_open_locked(path, generation, names, n_names, seed, state, &
            status, message, advance_completed, persist)
        call coverage_unlock(fd)
    end subroutine coverage_open

    subroutine refresh_state(state, status, message)
        type(coverage_epoch_t), intent(inout) :: state
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        type(coverage_epoch_t) :: latest

        call coverage_open_locked(state%path, state%generation, state%inventory, &
            size(state%inventory), state%seed, latest, status, message, persist=.false.)
        if (status /= COVERAGE_OK) return
        if (latest%epoch /= state%epoch) then
            status = COVERAGE_INVALID
            message = 'coverage update belongs to a superseded epoch'
            return
        end if
        state = latest
    end subroutine refresh_state

    subroutine lock_state(path, fd, status, message)
        character(len=*), intent(in) :: path
        integer(c_int), intent(out) :: fd
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        fd = coverage_lock(trim(path)//c_null_char)
        status = COVERAGE_OK
        message = ''
        if (fd < 0) then
            status = COVERAGE_IO_ERROR
            message = 'cannot lock coverage state ('//int_text(-int(fd))//')'
        end if
    end subroutine lock_state

    subroutine coverage_record(state, case_name, outcome, status, message)
        type(coverage_epoch_t), intent(inout) :: state
        character(len=*), intent(in) :: case_name, outcome
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        integer(c_int) :: fd

        call lock_state(state%path, fd, status, message)
        if (status /= COVERAGE_OK) return
        call refresh_state(state, status, message)
        if (status == COVERAGE_OK) call coverage_record_locked(state, case_name, &
            outcome, status, message)
        call coverage_unlock(fd)
    end subroutine coverage_record

    ! The lane owner calls this only when no test child for this generation exists.
    ! A persisted launch intent is an outstanding obligation after owner loss.
    subroutine coverage_recover_running(state, status, message)
        type(coverage_epoch_t), intent(inout) :: state
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        integer(c_int) :: fd

        call lock_state(state%path, fd, status, message)
        if (status /= COVERAGE_OK) return
        call refresh_state(state, status, message)
        if (status == COVERAGE_OK) then
            if (any(state%outcome == 'RUNNING')) then
                where (state%outcome == 'RUNNING') state%outcome = 'UNKNOWN'
                call coverage_save(state, status, message)
            end if
        end if
        call coverage_unlock(fd)
    end subroutine coverage_recover_running

    subroutine coverage_begin_epoch(state, seed, status, message)
        type(coverage_epoch_t), intent(inout) :: state
        integer, intent(in) :: seed
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        integer(c_int) :: fd

        call lock_state(state%path, fd, status, message)
        if (status /= COVERAGE_OK) return
        call refresh_state(state, status, message)
        if (status == COVERAGE_OK) call coverage_begin_epoch_locked(state, seed, &
            status, message)
        call coverage_unlock(fd)
    end subroutine coverage_begin_epoch

    subroutine coverage_next_chunk(state, priorities, n_priorities, limit, selected, &
            n_selected, status, n_priorities_selected)
        type(coverage_epoch_t), intent(inout) :: state
        character(len=*), intent(in) :: priorities(:)
        integer, intent(in) :: n_priorities, limit
        character(len=*), intent(out) :: selected(:)
        integer, intent(out) :: n_selected, status
        integer, intent(out), optional :: n_priorities_selected
        character(len=256) :: message
        integer(c_int) :: fd

        selected = ''
        n_selected = 0
        if (present(n_priorities_selected)) n_priorities_selected = 0
        call lock_state(state%path, fd, status, message)
        if (status /= COVERAGE_OK) return
        call refresh_state(state, status, message)
        if (status == COVERAGE_OK) call coverage_next_chunk_locked(state, priorities, &
            n_priorities, limit, &
            selected, n_selected, status, n_priorities_selected)
        call coverage_unlock(fd)
    end subroutine coverage_next_chunk

    subroutine coverage_record_path(path, generation, case_name, outcome, &
            status, message)
        character(len=*), intent(in) :: path, generation, case_name, outcome
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        integer(c_int) :: fd

        call lock_state(path, fd, status, message)
        if (status /= COVERAGE_OK) return
        call coverage_record_path_locked(path, generation, case_name, outcome, &
            status, message)
        call coverage_unlock(fd)
    end subroutine coverage_record_path

    subroutine coverage_read_view_path(path, generation, view, status, message)
        character(len=*), intent(in) :: path, generation
        type(gremlin_coverage_view_t), intent(out) :: view
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        integer(c_int) :: fd

        call lock_state(path, fd, status, message)
        if (status /= COVERAGE_OK) return
        call coverage_read_view_path_locked(path, generation, view, status, message)
        call coverage_unlock(fd)
    end subroutine coverage_read_view_path

    subroutine coverage_open_locked(path, generation, names, n_names, seed, state, &
            status, message, advance_completed, persist)
        character(len=*), intent(in) :: path, generation, names(:)
        integer, intent(in) :: n_names, seed
        type(coverage_epoch_t), intent(out) :: state
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        logical, intent(in), optional :: advance_completed, persist

        character(len=NAME_LEN), allocatable :: canonical(:)
        character(len=HASH_LEN) :: digest
        character(len=256) :: text
        integer :: i, j, ios, unit, old_epoch, old_seed, old_cursor, old_count
        logical :: exists, advance, save_state

        advance = .false.
        save_state = .true.
        if (present(advance_completed)) advance = advance_completed
        if (present(persist)) save_state = persist

        status = COVERAGE_INVALID
        message = 'invalid coverage inventory or state path'
        if (n_names < 1 .or. n_names > size(names)) return
        if (len_trim(path) == 0 .or. len_trim(generation) == 0) return
        allocate(canonical(n_names))
        canonical = ''
        do i = 1, n_names
            if (len_trim(names(i)) == 0) return
            if (len_trim(names(i)) > NAME_LEN) return
            canonical(i) = trim(names(i))
        end do
        do i = 2, n_names
            j = i
            do while (j > 1)
                if (canonical(j) >= canonical(j - 1)) exit
                call swap_name(canonical(j), canonical(j - 1))
                j = j - 1
            end do
        end do
        do i = 2, n_names
            if (canonical(i) == canonical(i - 1)) return
        end do
        digest = inventory_hash(canonical)
        state%generation = generation
        state%inventory_digest = digest
        state%inventory = canonical
        state%order = canonical
        allocate(state%outcome(n_names))
        state%outcome = 'UNKNOWN'
        state%seed = max(1, seed)
        state%cursor = 1
        state%epoch = 1
        state%path = trim(path)

        inquire(file=trim(path), exist=exists, iostat=ios)
        if (ios /= 0) then
            status = COVERAGE_IO_ERROR
            message = 'cannot inspect coverage state'
            return
        end if
        if (exists) then
            open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
            if (ios /= 0) then
                status = COVERAGE_IO_ERROR
                message = 'cannot open coverage state'
                return
            end if
            read(unit, '(a)', iostat=ios) text
            if (ios == 0) then
                read(unit, *, iostat=ios) old_epoch, old_seed, old_cursor, old_count
            end if
            if (ios == 0) then
                if (trim(text) /= trim(generation)//'|'//trim(digest)) ios = 1
            end if
            if (ios == 0) then
                if (old_count /= n_names) ios = 1
                if (old_epoch < 1 .or. old_seed < 1 .or. old_cursor < 1) ios = 1
                if (old_cursor > n_names + 1) ios = 1
            end if
            if (ios == 0) then
                state%epoch = max(1, old_epoch)
                state%seed = old_seed
                state%cursor = min(max(1, old_cursor), n_names + 1)
                do i = 1, n_names
                    read(unit, '(a)', iostat=ios) text
                    if (ios /= 0) exit
                    call restore_case(text, state, i, ios)
                    if (ios /= 0) exit
                end do
                if (ios == 0) then
                    read(unit, '(a)', iostat=ios) text
                    if (ios == iostat_end) then
                        ios = 0
                    else
                        ios = 1
                    end if
                end if
            else
                ios = 1
            end if
            close(unit)
            if (ios /= 0) then
                status = COVERAGE_INVALID
                message = 'coverage state is malformed or belongs to another inventory'
                return
            end if
        end if
        if (advance .and. coverage_remaining(state) == 0) then
            state%epoch = state%epoch + 1
            state%seed = max(1, seed)
            state%cursor = 1
            state%outcome = 'UNKNOWN'
        end if
        call permute_epoch(state)
        call skip_seen(state)
        if (save_state) then
            call coverage_save(state, status, message)
        else
            status = COVERAGE_OK
            message = ''
        end if
    end subroutine coverage_open_locked

    subroutine coverage_next_chunk_locked(state, priorities, n_priorities, limit, &
            selected, n_selected, status, n_priorities_selected)
        type(coverage_epoch_t), intent(inout) :: state
        character(len=*), intent(in) :: priorities(:)
        integer, intent(in) :: n_priorities, limit
        character(len=*), intent(out) :: selected(:)
        integer, intent(out) :: n_selected, status
        integer, intent(out), optional :: n_priorities_selected
        integer :: i, at, scan_cursor, selected_priorities
        character(len=128) :: save_message

        selected = ''
        n_selected = 0
        status = COVERAGE_OK
        selected_priorities = 0
        if (present(n_priorities_selected)) n_priorities_selected = 0
        if (n_priorities < 0 .or. n_priorities > size(priorities) .or. &
            limit < 1 .or. size(selected) < limit) then
            status = COVERAGE_INVALID
            return
        end if
        selected_priorities = 0
        do i = 1, n_priorities
            at = inventory_index(state, priorities(i))
            if (at == 0) cycle
            if (outcome_attempted(state%outcome(at))) cycle
            if (n_selected < limit) then
                n_selected = n_selected + 1
                selected(n_selected) = state%inventory(at)
                selected_priorities = selected_priorities + 1
            end if
        end do
        call skip_seen(state)
        scan_cursor = state%cursor
        do while (n_selected < limit .and. scan_cursor <= size(state%order))
            at = inventory_index(state, state%order(scan_cursor))
            scan_cursor = scan_cursor + 1
            if (at == 0) cycle
            if (outcome_attempted(state%outcome(at))) cycle
            if (contains(selected, n_selected, state%inventory(at))) cycle
            n_selected = n_selected + 1
            selected(n_selected) = state%inventory(at)
        end do
        call skip_seen(state)
        call coverage_save(state, status, save_message)
        if (present(n_priorities_selected)) n_priorities_selected = selected_priorities
    end subroutine coverage_next_chunk_locked

    subroutine coverage_record_locked(state, case_name, outcome, status, message)
        type(coverage_epoch_t), intent(inout) :: state
        character(len=*), intent(in) :: case_name, outcome
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        integer :: at

        at = inventory_index(state, case_name)
        if (at == 0) then
            status = COVERAGE_INVALID
            message = 'coverage outcome references an ineligible case'
            return
        end if
        if (.not. outcome_valid(outcome)) then
            status = COVERAGE_INVALID
            message = 'coverage outcome label is invalid'
            return
        end if
        state%outcome(at) = outcome
        call skip_seen(state)
        call coverage_save(state, status, message)
    end subroutine coverage_record_locked

    subroutine coverage_record_path_locked(path, generation, case_name, outcome, &
            status, message)
        character(len=*), intent(in) :: path, generation, case_name, outcome
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        type(coverage_epoch_t) :: state
        character(len=256) :: header, line
        character(len=NAME_LEN), allocatable :: names(:)
        integer :: unit, ios, epoch, seed, cursor, n_names, i, separator

        status = COVERAGE_IO_ERROR
        message = 'cannot read current coverage inventory'
        open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(unit, '(a)', iostat=ios) header
        if (ios == 0) then
            separator = index(header, '|')
            if (separator < 1) ios = 1
        end if
        if (ios == 0) then
            if (trim(header(:separator - 1)) /= trim(generation)) ios = 1
        end if
        if (ios == 0) read(unit, *, iostat=ios) epoch, seed, cursor, n_names
        if (ios == 0) then
            if (n_names < 1 .or. n_names > 100000) ios = 1
            if (epoch < 1 .or. cursor < 1) ios = 1
        end if
        if (ios == 0) then
            allocate(names(n_names))
            do i = 1, n_names
                read(unit, '(a)', iostat=ios) line
                if (ios /= 0) exit
                separator = index(line, '|')
                if (separator < 2 .or. separator > NAME_LEN + 1) then
                    ios = 1
                    exit
                end if
                names(i) = line(:separator - 1)
            end do
        end if
        close(unit)
        if (ios /= 0) then
            message = 'coverage state is incomplete or malformed'
            return
        end if
        call coverage_open_locked(path, generation, names, n_names, seed, state, &
            status, message, persist=.false.)
        if (status /= COVERAGE_OK) return
        call coverage_record_locked(state, case_name, outcome, status, message)
    end subroutine coverage_record_path_locked

    subroutine coverage_read_view_path_locked(path, generation, view, status, message)
        character(len=*), intent(in) :: path, generation
        type(gremlin_coverage_view_t), intent(out) :: view
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(coverage_epoch_t) :: state
        character(len=256) :: header, line
        character(len=HASH_LEN) :: stored_digest
        character(len=NAME_LEN), allocatable :: names(:)
        integer :: unit, ios, epoch, seed, cursor, n_names, i, separator
        logical :: exists

        view = gremlin_coverage_view_t()
        inquire(file=trim(path), exist=exists, iostat=ios)
        if (ios /= 0) then
            status = COVERAGE_IO_ERROR
            message = 'cannot inspect coverage state for status query'
            return
        end if
        if (.not. exists) then
            status = COVERAGE_NOT_FOUND
            message = ''
            return
        end if
        open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            status = COVERAGE_IO_ERROR
            message = 'cannot open coverage state for status query'
            return
        end if
        read(unit, '(a)', iostat=ios) header
        if (ios == 0) then
            separator = index(header, '|')
            if (separator < 2 .or. separator + HASH_LEN > len_trim(header)) ios = 1
        end if
        if (ios == 0) then
            if (trim(header(:separator - 1)) /= trim(generation)) ios = 1
            stored_digest = header(separator + 1:separator + HASH_LEN)
        end if
        if (ios == 0) read(unit, *, iostat=ios) epoch, seed, cursor, n_names
        if (ios == 0) then
            if (n_names < 1 .or. n_names > 100000) ios = 1
        end if
        if (ios == 0) then
            allocate(names(n_names))
            do i = 1, n_names
                read(unit, '(a)', iostat=ios) line
                if (ios /= 0) exit
                separator = index(line, '|')
                if (separator < 2 .or. separator > NAME_LEN + 1) then
                    ios = 1
                    exit
                end if
                names(i) = line(:separator - 1)
            end do
        end if
        close(unit)
        if (ios /= 0) then
            status = COVERAGE_INVALID
            message = 'coverage state is malformed for status query'
            return
        end if
        call coverage_open_locked(path, generation, names, n_names, seed, state, &
            status, message, advance_completed=.false., persist=.false.)
        if (status /= COVERAGE_OK) return
        if (state%inventory_digest /= stored_digest) then
            status = COVERAGE_INVALID
            message = 'coverage state inventory digest changed during status query'
            return
        end if
        call coverage_view(state, view)
    end subroutine coverage_read_view_path_locked

    subroutine coverage_begin_epoch_locked(state, seed, status, message)
        type(coverage_epoch_t), intent(inout) :: state
        integer, intent(in) :: seed
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        if (coverage_remaining(state) /= 0 .or. state%epoch < 1) then
            status = COVERAGE_INVALID
            message = 'a new coverage epoch requires the prior epoch to be complete'
            return
        end if
        state%epoch = state%epoch + 1
        state%seed = max(1, seed)
        state%cursor = 1
        state%outcome = 'UNKNOWN'
        state%order = state%inventory
        call permute_epoch(state)
        call coverage_save(state, status, message)
    end subroutine coverage_begin_epoch_locked

    integer function coverage_remaining(state) result(remaining)
        type(coverage_epoch_t), intent(in) :: state
        integer :: i
        remaining = 0
        if (.not. allocated(state%outcome)) return
        do i = 1, size(state%outcome)
            if (.not. outcome_attempted(state%outcome(i))) remaining = remaining + 1
        end do
    end function coverage_remaining

    subroutine coverage_view(state, view)
        type(coverage_epoch_t), intent(in) :: state
        type(gremlin_coverage_view_t), intent(out) :: view
        integer :: i

        view = gremlin_coverage_view_t()
        view%generation_id = state%generation
        view%inventory_digest = state%inventory_digest
        view%epoch = state%epoch
        view%seed = state%seed
        view%cursor = state%cursor
        view%eligible_count = size(state%inventory)
        do i = 1, size(state%outcome)
            select case (trim(state%outcome(i)))
            case ('PASS')
                view%pass_count = view%pass_count + 1
            case ('FAIL')
                view%fail_count = view%fail_count + 1
            case ('TIMEOUT')
                view%timeout_count = view%timeout_count + 1
            case ('FLAKY')
                view%flaky_count = view%flaky_count + 1
            case ('INFRA', 'INFRA_ERROR')
                view%infra_count = view%infra_count + 1
            case ('RUNNING')
                view%running_count = view%running_count + 1
            case ('CANCELLED')
                view%cancelled_count = view%cancelled_count + 1
            case ('UNKNOWN')
                view%unknown_count = view%unknown_count + 1
            case default
                view%other_count = view%other_count + 1
            end select
        end do
        ! Unknown counts outstanding obligations, including running/cancelled cases.
        view%unknown_count = view%unknown_count + view%running_count + &
            view%cancelled_count
        view%remaining_count = coverage_remaining(state)
        view%completed_count = view%eligible_count - view%remaining_count
        view%ordinary_complete = view%remaining_count == 0
        view%full_coverage = view%eligible_count > 0 .and. view%ordinary_complete
        view%green = view%full_coverage .and. view%pass_count == view%eligible_count
    end subroutine coverage_view

    subroutine coverage_save(state, status, message)
        type(coverage_epoch_t), intent(in) :: state
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        character(len=256), allocatable :: lines(:)
        character(len=64) :: metadata
        character(kind=c_char), allocatable :: bytes(:)
        integer :: i, j, rc, offset, total, line_length

        allocate(lines(size(state%inventory) + 2))
        lines(1) = trim(state%generation)//'|'//trim(state%inventory_digest)
        write(metadata, '(4(i0,1x))') state%epoch, state%seed, state%cursor, &
            size(state%inventory)
        lines(2) = trim(metadata)
        do i = 1, size(state%inventory)
            lines(i + 2) = trim(state%inventory(i))//'|'//trim(state%outcome(i))
        end do
        total = 0
        do i = 1, size(lines)
            total = total + len_trim(lines(i)) + 1
        end do
        allocate(bytes(total))
        offset = 0
        do i = 1, size(lines)
            line_length = len_trim(lines(i))
            do j = 1, line_length
                bytes(offset + j) = lines(i)(j:j)
            end do
            offset = offset + line_length + 1
            bytes(offset) = achar(10)
        end do
        offset = int(coverage_publish(trim(state%path)//c_null_char, bytes, &
            int(size(bytes), c_size_t)))
        if (offset == 0) then
            status = COVERAGE_OK
            message = ''
        else
            status = COVERAGE_IO_ERROR
            message = 'cannot atomically publish coverage state update ('// &
                int_text(offset)//')'
        end if
    end subroutine coverage_save

    subroutine permute_epoch(state)
        type(coverage_epoch_t), intent(inout) :: state
        integer(int64) :: random_state
        integer :: i, j
        random_state = modulo(int(state%seed, int64), 2147483646_int64) + 1_int64
        do i = size(state%order), 2, -1
            call random_step(random_state)
            j = 1 + int(modulo(random_state, int(i, int64)))
            if (i /= j) call swap_name(state%order(i), state%order(j))
        end do
    end subroutine permute_epoch

    pure subroutine random_step(state)
        integer(int64), intent(inout) :: state
        integer(int64) :: quotient
        quotient = state/127773_int64
        state = 16807_int64*(state - quotient*127773_int64) - 2836_int64*quotient
        if (state <= 0_int64) state = state + 2147483647_int64
    end subroutine random_step

    function inventory_hash(names) result(digest)
        character(len=*), intent(in) :: names(:)
        character(len=HASH_LEN) :: digest
        character(len=:), allocatable :: canonical
        character(len=:), allocatable :: parts(:)
        integer :: i
        canonical = ''
        do i = 1, size(names)
            canonical = canonical//trim(names(i))//achar(10)
        end do
        allocate(character(len=max(1, len(canonical))) :: parts(1))
        parts(1) = canonical
        digest = cache_digest(parts, 1)
    end function inventory_hash

    integer function inventory_index(state, name) result(at)
        type(coverage_epoch_t), intent(in) :: state
        character(len=*), intent(in) :: name
        integer :: i
        at = 0
        do i = 1, size(state%inventory)
            if (state%inventory(i) == name) then
                at = i
                return
            end if
        end do
    end function inventory_index

    subroutine skip_seen(state)
        type(coverage_epoch_t), intent(inout) :: state
        integer :: at
        do while (state%cursor <= size(state%order))
            at = inventory_index(state, state%order(state%cursor))
            if (at == 0) exit
            if (.not. outcome_attempted(state%outcome(at))) exit
            state%cursor = state%cursor + 1
        end do
    end subroutine skip_seen

    logical function contains(names, count_names, name)
        character(len=*), intent(in) :: names(:), name
        integer, intent(in) :: count_names

    contains = .false.
        if (count_names < 1) return
    contains = any(names(:count_names) == name)
    end function contains

    subroutine swap_name(left, right)
        character(len=*), intent(inout) :: left, right
        character(len=len(left)) :: temporary
        temporary = left
        left = right
        right = temporary
    end subroutine swap_name

    subroutine restore_case(line, state, index_case, status)
        character(len=*), intent(in) :: line
        type(coverage_epoch_t), intent(inout) :: state
        integer, intent(in) :: index_case
        integer, intent(out) :: status
        integer :: split
        split = index(line, '|')
        status = 1
        if (split < 1) return
        if (trim(line(:split - 1)) /= trim(state%inventory(index_case))) return
        if (len_trim(line(split + 1:)) > len(state%outcome)) return
        if (.not. outcome_valid(trim(line(split + 1:)))) return
        state%outcome(index_case) = line(split + 1:)
        status = 0
    end subroutine restore_case

    pure logical function outcome_valid(outcome)
        character(len=*), intent(in) :: outcome
        select case (trim(outcome))
        case ('UNKNOWN', 'RUNNING', 'CANCELLED', 'PASS', 'FAIL', 'TIMEOUT', &
                'FLAKY', 'INFRA', 'INFRA_ERROR')
            outcome_valid = .true.
        case default
            outcome_valid = .false.
        end select
    end function outcome_valid

    pure logical function outcome_attempted(outcome)
        character(len=*), intent(in) :: outcome
        select case (trim(outcome))
        case ('PASS', 'FAIL', 'TIMEOUT', 'FLAKY', 'INFRA', 'INFRA_ERROR')
            outcome_attempted = .true.
        case default
            outcome_attempted = .false.
        end select
    end function outcome_attempted

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=16) :: text
        write(text, '(i0)') value
    end function int_text

end module fo_gremlin_coverage
