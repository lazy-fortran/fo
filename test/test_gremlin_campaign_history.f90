program test_gremlin_campaign_history
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, int64, output_unit
    use fo_util, only: make_tmpfile
    implicit none

    integer, parameter :: NAME_LEN = 128, MAX_EVENTS = 128, N_CASES = 40
    integer, parameter :: JSON_OBJECT_START = 1, JSON_OBJECT_END = 2
    integer, parameter :: JSON_ARRAY_START = 3, JSON_ARRAY_END = 4
    integer, parameter :: JSON_KEY = 5, JSON_STRING = 6, JSON_INTEGER = 7
    integer, parameter :: JSON_OTHER = 8, JSON_END_OF_INPUT = 9, JSON_ERROR = 10
    type :: receipt_t
        character(len=NAME_LEN) :: case_id = ''
        character(len=NAME_LEN) :: generation = ''
        character(len=NAME_LEN) :: inventory_digest = ''
        character(len=16) :: status = ''
        integer :: seed = 0
        integer :: epoch = 0
        integer :: order = 0
    end type receipt_t

    character(len=512) :: scratch_tmp, scratch, project, replay_project
    character(len=512) :: history_project, marker, replay_marker, history_marker
    character(len=512) :: cache_root, state_root, home_root, xdg_root, driver
    character(len=NAME_LEN) :: names(N_CASES), targets(4), marker_snapshot(MAX_EVENTS)
    type(receipt_t) :: first_session(MAX_EVENTS), resumed(MAX_EVENTS)
    type(receipt_t) :: replay(MAX_EVENTS)
    integer :: n_first, n_resumed, n_replay
    integer :: i, status, marker_boundary
    integer(c_int) :: env_status
    character(len=128) :: session
    logical :: ok, marker_ok

    interface
        integer(c_int) function set_environment(name, value, overwrite) &
                bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function set_environment
        integer(c_int) function signal_process(pid, signal_number) &
                bind(C, name='kill')
            import :: c_int
            integer(c_int), value :: pid, signal_number
        end function signal_process
        integer(c_int) function wait_microseconds(microseconds) &
                bind(C, name='usleep')
            import :: c_int
            integer(c_int), value :: microseconds
        end function wait_microseconds
    end interface

    env_status = set_environment('TMPDIR'//c_null_char, &
        '/var/tmp'//c_null_char, 1_c_int)
    call require(env_status == 0, 'sets private fixture temporary root')
    call make_tmpfile('fo-campaign-history-root', scratch_tmp)
    scratch = scratch_tmp(:len_trim(scratch_tmp) - 4)
    project = trim(scratch)//'/seed-project'
    replay_project = trim(scratch)//'/replay-project'
    history_project = trim(scratch)//'/history-project'
    marker = trim(scratch)//'/seed-markers'
    replay_marker = trim(scratch)//'/replay-markers'
    history_marker = trim(scratch)//'/history-markers'
    cache_root = trim(scratch)//'/fo-cache'
    state_root = trim(scratch)//'/gremlin-state'
    home_root = trim(scratch)//'/home'
    xdg_root = trim(scratch)//'/xdg-cache'
    call resolve_driver(driver)
    call make_directory(trim(scratch))
    call remove_file(trim(scratch_tmp))
    call make_directory(trim(cache_root))
    call make_directory(trim(state_root))
    call make_directory(trim(home_root))
    call make_directory(trim(xdg_root))

    do i = 1, N_CASES
        write(names(i), '(a,i2.2)') 'test_case_', i
    end do
    targets = ''
    call make_project(trim(project), names, marker, .false.)
    call make_project(trim(replay_project), names, replay_marker, .false.)

    ! A stop/restart in the same lane must resume the exact generation epoch.
    call start_session(trim(project), 'seed-oracle', 1729, 4, .true., &
        targets, 0, session)
    call wait_for_receipts(trim(project), 'seed-oracle', trim(session), 17, ok)
    call stop_session(trim(project), 'seed-oracle', trim(session), status)
    call require(status == 0, 'stops the first epoch owner')
    call require(ok, 'first epoch owner completes at least seventeen receipts')
    call fetch_events(trim(project), 'seed-oracle', trim(session), &
        first_session, n_first, ok)
    call require(ok .and. n_first >= 17 .and. n_first < N_CASES, &
        'first owner leaves an incomplete finite epoch')
    call read_markers(trim(marker), marker_snapshot, marker_boundary, marker_ok)
    call require(marker_ok .and. marker_boundary >= n_first .and. &
        marker_boundary <= n_first + 1, &
        'stopped owner leaves only its receipts and at most one in-flight marker')

    call start_session(trim(project), 'seed-oracle', 999, 4, .true., &
        targets, 0, session)
    call wait_for_receipts(trim(project), 'seed-oracle', trim(session), &
        N_CASES - n_first, ok)
    call stop_session(trim(project), 'seed-oracle', trim(session), status)
    call require(status == 0, 'stops the resumed epoch owner')
    call require(ok, 'resumed owner completes the remaining epoch receipts')
    call fetch_events(trim(project), 'seed-oracle', trim(session), &
        resumed, n_resumed, ok)
    call require(ok .and. n_resumed >= N_CASES - n_first, &
        'reads resumed epoch receipts from the process boundary')
    call verify_epoch(names, first_session, n_first, resumed, n_resumed)
    call verify_markers(trim(marker), marker_boundary, first_session, n_first, &
        resumed, n_resumed)

    ! Same inventory and seed in a fresh project must replay the same prefix.
    call start_session(trim(replay_project), 'seed-replay', 1729, 4, .true., &
        targets, 0, session)
    call wait_for_receipts(trim(replay_project), 'seed-replay', trim(session), &
        20, ok)
    call stop_session(trim(replay_project), 'seed-replay', trim(session), status)
    call require(status == 0, 'stops the replay owner')
    call require(ok, 'same-seed replay completes twenty receipts')
    call fetch_events(trim(replay_project), 'seed-replay', trim(session), &
        replay, n_replay, ok)
    call require(ok .and. n_replay >= 20, 'reads replay receipts')
    call verify_replay(first_session, n_first, resumed, n_resumed, replay, n_replay)
    call verify_legacy_oracle_divergence(names, first_session, n_first, resumed, &
        n_resumed)

    call verify_failure_history(trim(history_project), trim(history_marker))

    call clean_scratch(trim(scratch))
    write(output_unit, '(a)') 'Gremlin campaign history process oracle: PASS'

contains

    subroutine json_next_token(source, position, token_type, token_text)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: position
        integer, intent(out) :: token_type
        character(len=:), allocatable, intent(out) :: token_text
        character(len=1) :: character_value, escaped
        integer :: source_length, codepoint, digit, digit_index, token_start
        logical :: integer_number

        source_length = len(source)
        token_text = ''
        call skip_space(source, position)
        if (position > source_length) then
            token_type = JSON_END_OF_INPUT
            return
        end if
        character_value = source(position:position)
        position = position + 1
        select case (character_value)
        case ('{')
            token_type = JSON_OBJECT_START
        case ('}')
            token_type = JSON_OBJECT_END
        case ('[')
            token_type = JSON_ARRAY_START
        case (']')
            token_type = JSON_ARRAY_END
        case (',')
            token_type = JSON_OTHER
        case (':')
            token_type = JSON_OTHER
        case ('"')
            do while (position <= source_length)
                character_value = source(position:position)
                position = position + 1
                if (character_value == '"') then
                    call skip_space(source, position)
                    if (position <= source_length) then
                        if (source(position:position) == ':') then
                            position = position + 1
                            token_type = JSON_KEY
                            return
                        end if
                    end if
                    token_type = JSON_STRING
                    return
                end if
                if (iachar(character_value) < 32) then
                    token_type = JSON_ERROR
                    return
                end if
                if (character_value /= achar(92)) then
                    token_text = token_text//character_value
                    cycle
                end if
                if (position > source_length) then
                    token_type = JSON_ERROR
                    return
                end if
                escaped = source(position:position)
                position = position + 1
                select case (escaped)
                case ('"', achar(92), '/')
                    token_text = token_text//escaped
                case ('b')
                    token_text = token_text//achar(8)
                case ('f')
                    token_text = token_text//achar(12)
                case ('n')
                    token_text = token_text//achar(10)
                case ('r')
                    token_text = token_text//achar(13)
                case ('t')
                    token_text = token_text//achar(9)
                case ('u')
                    if (position + 3 > source_length) then
                        token_type = JSON_ERROR
                        return
                    end if
                    codepoint = 0
                    do digit_index = 1, 4
                        if (position > source_length) then
                            token_type = JSON_ERROR
                            return
                        end if
                        digit = hex_digit(source(position:position))
                        if (digit < 0) then
                            token_type = JSON_ERROR
                            return
                        end if
                        codepoint = 16*codepoint + digit
                        position = position + 1
                    end do
                    if (codepoint > 127) then
                        token_type = JSON_ERROR
                        return
                    end if
                    token_text = token_text//achar(codepoint)
                case default
                    token_type = JSON_ERROR
                    return
                end select
            end do
            token_type = JSON_ERROR
        case ('-', '0':'9')
            codepoint = position - 1
            do while (position <= source_length)
                character_value = source(position:position)
                if (index('0123456789+-.eE', character_value) == 0) exit
                position = position + 1
            end do
            token_text = source(codepoint:position - 1)
            integer_number = verify(token_text, '0123456789-') == 0
            if (integer_number) then
                token_type = JSON_INTEGER
            else
                token_type = JSON_OTHER
            end if
        case default
            token_start = position - 1
            do while (position <= source_length)
                character_value = source(position:position)
                if (index(' ,:]}', character_value) > 0) exit
                position = position + 1
            end do
            token_text = source(token_start:position - 1)
            if (token_text == 'true' .or. token_text == 'false' .or. &
                token_text == 'null') then
                token_type = JSON_OTHER
            else
                token_type = JSON_ERROR
            end if
        end select
    end subroutine json_next_token

    subroutine json_extract_string(source, field, value, found)
        character(len=*), intent(in) :: source, field
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found
        character(len=:), allocatable :: token_text, key
        integer :: position, token_type

        position = 1
        key = ''
        value = ''
        found = .false.
        do
            call json_next_token(source, position, token_type, token_text)
            if (token_type == JSON_END_OF_INPUT .or. token_type == JSON_ERROR) return
            if (token_type == JSON_KEY) then
                key = token_text
                cycle
            end if
            if (key == field) then
                if (token_type == JSON_STRING) then
                    value = token_text
                    found = .true.
                    return
                end if
                key = ''
            end if
        end do
    end subroutine json_extract_string

    subroutine skip_space(source, position)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: position
        integer :: source_length

        source_length = len(source)
        do while (position <= source_length)
            select case (source(position:position))
            case (' ', achar(9), achar(10), achar(13))
                position = position + 1
            case default
                return
            end select
        end do
    end subroutine skip_space

    integer function hex_digit(value) result(digit)
        character(len=1), intent(in) :: value

        digit = index('0123456789abcdef', value) - 1
        if (digit < 0) digit = index('0123456789ABCDEF', value) - 1
    end function hex_digit


    subroutine require(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) return
        write(error_unit, '(a)') 'FAIL: '//trim(description)
        error stop 1
    end subroutine require

    subroutine resolve_driver(path)
        character(len=*), intent(out) :: path
        integer :: length, env_status

        path = ''
        call get_environment_variable('FO', value=path, length=length, &
            status=env_status)
        if (env_status /= 0 .or. length < 1 .or. length > len(path)) path = 'fo'
    end subroutine resolve_driver

    subroutine make_directory(path)
        character(len=*), intent(in) :: path
        integer :: command_status, exit_status

        call execute_command_line('mkdir -p '//shell_quote(path), &
            cmdstat=command_status, exitstat=exit_status)
        call require(command_status == 0 .and. exit_status == 0, &
            'creates fixture directory '//trim(path))
    end subroutine make_directory

    subroutine make_project(root, inventory, marker_path, history_fixture)
        character(len=*), intent(in) :: root, inventory(:), marker_path
        logical, intent(in) :: history_fixture
        integer :: unit, i, ios

        call make_directory(trim(root)//'/test')
        open(newunit=unit, file=trim(root)//'/fpm.toml', status='replace', &
            action='write', iostat=ios)
        call require(ios == 0, 'creates fixture fpm.toml')
        write(unit, '(a)') 'name = "gremlin_campaign_history_probe"'
        close(unit)
        do i = 1, size(inventory)
            if (history_fixture) then
                call write_case(root, trim(inventory(i)), marker_path, &
                    trim(inventory(i)) == 'test_failure', &
                    trim(inventory(i)) == 'test_unknown')
            else
                call write_case(root, trim(inventory(i)), marker_path, .false., .false.)
            end if
        end do
    end subroutine make_project

    subroutine write_case(root, name, marker_path, should_fail, should_block)
        character(len=*), intent(in) :: root, name, marker_path
        logical, intent(in) :: should_fail, should_block
        integer :: unit, ios

        open(newunit=unit, file=trim(root)//'/test/'//trim(name)//'.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'creates fixture source '//trim(name))
        if (should_block) then
            write(unit, '(a)') 'program '//trim(name)
            write(unit, '(a)') 'use, intrinsic :: iso_c_binding, only: c_int'
            write(unit, '(a)') 'implicit none'
            write(unit, '(a)') 'interface'
            write(unit, '(a)') '  function c_pause() bind(C, name="pause") result(rc)'
            write(unit, '(a)') '    import :: c_int'
            write(unit, '(a)') '    integer(c_int) :: rc'
            write(unit, '(a)') '  end function c_pause'
            write(unit, '(a)') 'end interface'
            write(unit, '(a)') 'integer :: unit'
            write(unit, '(a)') 'integer(c_int) :: rc'
        else
            write(unit, '(a)') 'program '//trim(name)
            write(unit, '(a)') 'implicit none'
            write(unit, '(a)') 'integer :: unit'
        end if
        write(unit, '(a)') 'open(newunit=unit,file='//fortran_quote(marker_path)// &
            ',status="unknown",position="append")'
        write(unit, '(a)') 'write(unit,"(a)") '//fortran_quote(name)
        write(unit, '(a)') 'close(unit)'
        if (should_block) write(unit, '(a)') 'rc = c_pause()'
        if (should_fail) write(unit, '(a)') 'error stop 9'
        write(unit, '(a)') 'end program '//trim(name)
        close(unit)
    end subroutine write_case

    subroutine start_session(project_path, lane, seed, random_count, shuffle, &
            requested, n_requested, session_id)
        character(len=*), intent(in) :: project_path, lane
        integer, intent(in) :: seed, random_count, n_requested
        logical, intent(in) :: shuffle
        character(len=*), intent(in) :: requested(:)
        character(len=*), intent(out) :: session_id
        character(len=:), allocatable :: args, output, errors, parsed_id
        integer :: exit_status, i
        logical :: found

        args = 'gremlin start --dir '//shell_quote(project_path)// &
            ' --lane '//shell_quote(lane)//' --random-count '//int_text(random_count)// &
            ' --seed '//int_text(seed)//' --campaign-seconds 60'// &
            ' --timeout-seconds 5'
        if (shuffle) args = args//' --shuffle'
        do i = 1, n_requested
            args = args//' --target '//shell_quote(trim(requested(i)))
        end do
        args = args//' --json'
        call run_cli(project_path, args, output, errors, exit_status)
        call require(exit_status == 0, 'starts Gremlin session: '//trim(errors)// &
            ' '//trim(output))
        call json_extract_string(output, 'session_id', parsed_id, found)
        call require(found .and. len_trim(parsed_id) > 0, &
            'Gremlin start returns a session ID: '//trim(output))
        session_id = parsed_id
    end subroutine start_session

    subroutine stop_session(project_path, lane, session_id, exit_status)
        character(len=*), intent(in) :: project_path, lane, session_id
        integer, intent(out) :: exit_status
        character(len=:), allocatable :: args, output, errors, state
        integer :: command_status, attempt
        logical :: found, owner_exited

        args = 'gremlin stop --dir '//shell_quote(project_path)// &
            ' --lane '//shell_quote(lane)//' --session '//shell_quote(session_id)// &
            ' --json'
        call run_cli(project_path, args, output, errors, command_status)
        exit_status = 1
        if (command_status /= 0) return
        do attempt = 1, 200
            args = 'gremlin status --dir '//shell_quote(project_path)// &
                ' --lane '//shell_quote(lane)//' --session '//shell_quote(session_id)// &
                ' --json'
            call run_cli(project_path, args, output, errors, command_status)
            if (command_status == 0) then
                call json_extract_string(output, 'state', state, found)
                if (found .and. state == 'stopped') then
                    call wait_owner_exit(session_id, owner_exited)
                    if (owner_exited) exit_status = 0
                    return
                end if
            end if
            call pause_for(50000)
        end do
    end subroutine stop_session

    subroutine wait_owner_exit(session_id, success)
        character(len=*), intent(in) :: session_id
        logical, intent(out) :: success
        integer(c_int) :: owner_pid, signal_status
        integer :: separator, ios, attempt

        success = .false.
        separator = index(trim(session_id), '-')
        if (separator < 2) return
        read(session_id(:separator - 1), *, iostat=ios) owner_pid
        if (ios /= 0) return
        if (owner_pid < 1_c_int) return
        do attempt = 1, 200
            signal_status = signal_process(owner_pid, 0_c_int)
            if (signal_status /= 0_c_int) then
                success = .true.
                return
            end if
            call pause_for(50000)
        end do
    end subroutine wait_owner_exit

    subroutine wait_for_receipts(project_path, lane, session_id, wanted, success)
        character(len=*), intent(in) :: project_path, lane, session_id
        integer, intent(in) :: wanted
        logical, intent(out) :: success
        type(receipt_t) :: events(MAX_EVENTS)
        integer :: n_events, attempt
        logical :: page_ok

        success = .false.
        do attempt = 1, 1200
            call fetch_events(project_path, lane, session_id, events, n_events, page_ok)
            if (page_ok .and. n_events >= wanted) then
                success = .true.
                return
            end if
            call pause_for(50000)
        end do
    end subroutine wait_for_receipts

    subroutine fetch_events(project_path, lane, session_id, events, n_events, success)
        character(len=*), intent(in) :: project_path, lane, session_id
        type(receipt_t), intent(out) :: events(:)
        integer, intent(out) :: n_events
        logical, intent(out) :: success
        character(len=:), allocatable :: args, output, errors
        integer :: exit_status
        logical :: parsed

        args = 'gremlin events --dir '//shell_quote(project_path)// &
            ' --lane '//shell_quote(lane)//' --session '//shell_quote(session_id)// &
            ' --cursor 0 --max-records 128 --max-bytes 262144 --json'
        call run_cli(project_path, args, output, errors, exit_status)
        if (exit_status /= 0) then
            n_events = 0
            success = .false.
            return
        end if
        call parse_events(output, events, n_events, parsed)
        success = parsed
    end subroutine fetch_events

    subroutine run_cli(project_path, args, output, errors, exit_status)
        character(len=*), intent(in) :: project_path, args
        character(len=:), allocatable, intent(out) :: output, errors
        integer, intent(out) :: exit_status
        character(len=512) :: out_path, err_path
        character(len=8192) :: command
        integer :: command_status
        logical :: read_ok

        out_path = trim(scratch)//'/cli-stdout'
        err_path = trim(scratch)//'/cli-stderr'
        write(command, '(a)') 'cd '//shell_quote(project_path)//' && '// &
            'FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 FO_JOBS=1 '// &
            'FO_CACHE_DIR='//shell_quote(trim(cache_root))//' '// &
            'FO_GREMLIN_STATE_DIR='//shell_quote(trim(state_root))//' '// &
            'HOME='//shell_quote(trim(home_root))//' '// &
            'XDG_CACHE_HOME='//shell_quote(trim(xdg_root))//' TMPDIR=/var/tmp '// &
            shell_quote(trim(driver))//' '//trim(args)//' > '// &
            shell_quote(trim(out_path))//' 2> '//shell_quote(trim(err_path))
        call execute_command_line(trim(command), wait=.true., &
            cmdstat=command_status, exitstat=exit_status)
        if (command_status /= 0) exit_status = 127
        call read_text(trim(out_path), output, read_ok)
        if (.not. read_ok) output = ''
        call read_text(trim(err_path), errors, read_ok)
        if (.not. read_ok) errors = ''
        call remove_file(trim(out_path))
        call remove_file(trim(err_path))
    end subroutine run_cli

    subroutine parse_events(json, events, n_events, success)
        character(len=*), intent(in) :: json
        type(receipt_t), intent(out) :: events(:)
        integer, intent(out) :: n_events
        logical, intent(out) :: success
        type(receipt_t) :: current
        character(len=:), allocatable :: token_text
        character(len=64) :: key
        integer :: depth, position, token_type, integer_value, ios
        logical :: in_events, in_event, saw_events

        events = receipt_t()
        current = receipt_t()
        n_events = 0
        depth = 0
        key = ''
        in_events = .false.
        in_event = .false.
        saw_events = .false.
        position = 1
        do
            call json_next_token(json, position, token_type, token_text)
            select case (token_type)
            case (JSON_END_OF_INPUT)
                exit
            case (JSON_ERROR)
                success = .false.
                return
            case (JSON_OBJECT_START)
                depth = depth + 1
                if (in_events .and. depth == 3) then
                    current = receipt_t()
                    in_event = .true.
                end if
            case (JSON_ARRAY_START)
                if (depth == 1 .and. key == 'events') then
                    in_events = .true.
                    saw_events = .true.
                end if
                depth = depth + 1
            case (JSON_OBJECT_END)
                if (in_event .and. depth == 3) then
                    if (current%case_id /= '<build>' .and. &
                        len_trim(current%case_id) > 0 .and. &
                        n_events < size(events)) then
                        n_events = n_events + 1
                        events(n_events) = current
                    end if
                    in_event = .false.
                end if
                depth = depth - 1
            case (JSON_ARRAY_END)
                if (in_events .and. depth == 2) in_events = .false.
                depth = depth - 1
            case (JSON_KEY)
                key = token_text
            case (JSON_STRING)
                if (in_event .and. depth == 3) call set_event_string( &
                    current, trim(key), token_text)
                key = ''
            case (JSON_INTEGER)
                read(token_text, *, iostat=ios) integer_value
                if (ios /= 0) then
                    success = .false.
                    return
                end if
                if (in_event .and. depth == 3) call set_event_integer( &
                    current, trim(key), integer_value)
                key = ''
            case (JSON_OTHER)
                if (key /= '') key = ''
            end select
        end do
        success = saw_events
    end subroutine parse_events

    subroutine set_event_string(event, field, value)
        type(receipt_t), intent(inout) :: event
        character(len=*), intent(in) :: field, value

        select case (field)
        case ('case_id')
            event%case_id = value
        case ('generation')
            event%generation = value
        case ('inventory_digest')
            event%inventory_digest = value
        case ('status')
            event%status = value
        end select
    end subroutine set_event_string

    subroutine set_event_integer(event, field, value)
        type(receipt_t), intent(inout) :: event
        character(len=*), intent(in) :: field
        integer, intent(in) :: value

        select case (field)
        case ('seed')
            event%seed = value
        case ('coverage_epoch')
            event%epoch = value
        case ('order')
            event%order = value
        end select
    end subroutine set_event_integer

    subroutine verify_epoch(inventory, first, n_first, rest, n_rest)
        character(len=*), intent(in) :: inventory(:)
        type(receipt_t), intent(in) :: first(:), rest(:)
        integer, intent(in) :: n_first, n_rest
        type(receipt_t) :: event, baseline
        logical :: seen(size(inventory))
        integer :: i, at, total

        total = n_first + n_rest
        call require(total >= size(inventory), &
            'first finite epoch completes every eligible test')
        baseline = receipt_at(first, n_first, rest, n_rest, 1)
        seen = .false.
        do i = 1, size(inventory)
            event = receipt_at(first, n_first, rest, n_rest, i)
            at = name_index(inventory, event%case_id)
            call require(at > 0, 'epoch receipt belongs to the eligible inventory')
            call require(.not. seen(at), &
                'no case repeats before all forty epoch obligations complete')
            seen(at) = .true.
            call require(event%status == 'PASS', 'ordinary epoch case passes')
            call require(event%seed == 1729 .and. event%epoch == 1, &
                'restart preserves the original seed and epoch identity')
            if (i == 1) cycle
            call require(event%generation == baseline%generation, &
                'campaign chunks use one immutable generation')
            call require(event%inventory_digest == baseline%inventory_digest, &
                'campaign chunks use one canonical inventory')
        end do
        call require(all(seen), 'epoch receipts cover each eligible case exactly once')
        do i = 1, n_first
            call require(first(i)%order == modulo(i - 1, 4) + 1, &
                'first owner preserves four-case campaign positions')
        end do
        do i = 1, n_rest
            call require(rest(i)%order == modulo(i - 1, 4) + 1, &
                'restarted owner preserves four-case campaign positions')
        end do
    end subroutine verify_epoch

    subroutine verify_markers(path, marker_boundary, first, n_first, rest, n_rest)
        character(len=*), intent(in) :: path
        type(receipt_t), intent(in) :: first(:), rest(:)
        integer, intent(in) :: marker_boundary, n_first, n_rest
        character(len=NAME_LEN) :: markers(MAX_EVENTS)
        type(receipt_t) :: event
        integer :: n_markers, i
        logical :: found

        call read_markers(path, markers, n_markers, found)
        call require(found .and. n_markers >= marker_boundary + n_rest, &
            'test-written markers exist for both process sessions')
        do i = 1, n_first
            call require(markers(i) == first(i)%case_id, &
                'first-session receipts match test-written launch markers')
        end do
        do i = 1, n_rest
            event = rest(i)
            call require(markers(marker_boundary + i) == event%case_id, &
                'resumed-session receipts match test-written launch markers')
        end do
    end subroutine verify_markers

    subroutine verify_replay(first, n_first, rest, n_rest, replay, n_replay)
        type(receipt_t), intent(in) :: first(:), rest(:), replay(:)
        integer, intent(in) :: n_first, n_rest, n_replay
        type(receipt_t) :: expected
        integer :: i

        call require(n_replay >= 8, 'replay owner returned two full campaign chunks')
        do i = 1, 8
            expected = receipt_at(first, n_first, rest, n_rest, i)
            call require(replay(i)%case_id == expected%case_id, &
                'same seed replays two process-boundary campaign chunks')
            call require(replay(i)%seed == expected%seed .and. &
                replay(i)%epoch == expected%epoch .and. &
                replay(i)%order == expected%order, &
                'same seed replays epoch and campaign positions')
        end do
    end subroutine verify_replay

    subroutine verify_legacy_oracle_divergence(inventory, first, n_first, rest, n_rest)
        character(len=*), intent(in) :: inventory(:)
        type(receipt_t), intent(in) :: first(:), rest(:)
        integer, intent(in) :: n_first, n_rest
        type(receipt_t) :: event, prior(4)
        character(len=NAME_LEN) :: old_expected
        character(len=NAME_LEN) :: expected_epoch(size(inventory))
        integer :: i, chunk_start, chunk_end

        call require(n_first >= 5, 'reproducer spans two campaign chunks')
        expected_epoch = inventory
        call random_permutation(expected_epoch, size(expected_epoch), 1729)
        ! The first case establishes the default gate and stays at the front;
        ! later chunks shuffle every selected case.
        call random_permutation(expected_epoch(2:4), 3, 1729)
        chunk_start = 5
        chunk_end = min(chunk_start + 3, size(expected_epoch))
        call random_permutation(expected_epoch(chunk_start:chunk_end), &
            chunk_end - chunk_start + 1, 1729)
        do i = 1, 8
            event = receipt_at(first, n_first, rest, n_rest, i)
            call require(event%case_id == expected_epoch(i), &
                'seed 1729 follows the independent finite-epoch permutation')
        end do
        do i = 1, 4
            prior(i) = receipt_at(first, n_first, rest, n_rest, i)
        end do
        event = receipt_at(first, n_first, rest, n_rest, 5)
        old_expected = legacy_oracle_second_campaign(inventory, prior, 1729)
        call require(old_expected == 'test_case_20', &
            'legacy campaign-resampling oracle reproduces its stale case_20 prediction')
        call require(event%order == 1 .and. event%case_id == 'test_case_19', &
            'finite epoch resumes at its next unseen case, case_19')
        call require(old_expected /= event%case_id, &
            'negative control distinguishes the stale per-campaign oracle')
    end subroutine verify_legacy_oracle_divergence

    function legacy_oracle_second_campaign(inventory, previous, seed) result(expected)
        character(len=*), intent(in) :: inventory(:)
        type(receipt_t), intent(in) :: previous(:)
        integer, intent(in) :: seed
        character(len=NAME_LEN) :: expected
        character(len=NAME_LEN) :: selected(32), candidates(size(inventory))
        integer :: n_selected, n_candidates, start_at, at, i

        selected = ''
        candidates = ''
        n_selected = 0
        start_at = modulo(seed, size(previous))
        do i = 1, size(previous)
            at = modulo(start_at + i - 1, size(previous)) + 1
            call append_unique(previous(at)%case_id, selected, n_selected)
        end do
        n_candidates = 0
        do i = 1, size(inventory)
            if (any(selected(:n_selected) == inventory(i))) cycle
            n_candidates = n_candidates + 1
            candidates(n_candidates) = inventory(i)
        end do
        call random_permutation(candidates, n_candidates, seed)
        do i = 1, min(4, n_candidates)
            call append_unique(candidates(i), selected, n_selected)
        end do
        call random_permutation(selected, n_selected, seed)
        expected = selected(1)
    end function legacy_oracle_second_campaign

    subroutine random_permutation(values, count_values, seed)
        character(len=*), intent(inout) :: values(:)
        integer, intent(in) :: count_values, seed
        integer(int64) :: state, quotient
        integer :: i, j
        character(len=len(values)) :: temporary

        state = modulo(int(seed, int64), 2147483646_int64) + 1_int64
        do i = count_values, 2, -1
            quotient = state/127773_int64
            state = 16807_int64*(state - quotient*127773_int64) - &
                2836_int64*quotient
            if (state <= 0_int64) state = state + 2147483647_int64
            j = 1 + int(modulo(state, int(i, int64)))
            temporary = values(i)
            values(i) = values(j)
            values(j) = temporary
        end do
    end subroutine random_permutation

    subroutine append_unique(name, names, count_names)
        character(len=*), intent(in) :: name
        character(len=*), intent(inout) :: names(:)
        integer, intent(inout) :: count_names

        if (count_names > 0) then
            if (any(names(:count_names) == name)) return
        end if
        count_names = count_names + 1
        names(count_names) = name
    end subroutine append_unique

    function receipt_at(first, n_first, rest, n_rest, index_event) result(event)
        type(receipt_t), intent(in) :: first(:), rest(:)
        integer, intent(in) :: n_first, n_rest, index_event
        type(receipt_t) :: event

        call require(index_event >= 1 .and. index_event <= n_first + n_rest, &
            'selects an event within the two captured sessions')
        if (index_event <= n_first) then
            event = first(index_event)
        else
            event = rest(index_event - n_first)
        end if
    end function receipt_at

    integer function name_index(names_in, name) result(index_name)
        character(len=*), intent(in) :: names_in(:), name
        integer :: i

        index_name = 0
        do i = 1, size(names_in)
            if (trim(names_in(i)) == trim(name)) then
                index_name = i
                return
            end if
        end do
    end function name_index

    subroutine verify_failure_history(project_path, marker_path)
        character(len=*), intent(in) :: project_path, marker_path
        character(len=NAME_LEN) :: history_names(4), target(1), markers(MAX_EVENTS)
        character(len=128) :: first_session_id, second_session_id
        character(len=128) :: unknown_session_id, fourth_session_id
        type(receipt_t) :: first_events(MAX_EVENTS), second_events(MAX_EVENTS)
        type(receipt_t) :: unknown_events(MAX_EVENTS), fourth_events(MAX_EVENTS)
        integer :: n_first_events, n_second_events, n_unknown_events
        integer :: n_fourth_events, prior_markers, n_markers, stop_status
        logical :: ok_wait, ok_page, marker_found

        history_names = [character(len=NAME_LEN) :: 'test_failure', &
            'test_unknown', 'test_alpha', 'test_beta']
        call make_project(project_path, history_names, marker_path, .true.)

        target(1) = 'test_failure'
        call start_session(project_path, 'history', 11, 1, .false., target, 1, &
            first_session_id)
        call wait_for_receipts(project_path, 'history', trim(first_session_id), &
            1, ok_wait)
        call stop_session(project_path, 'history', trim(first_session_id), stop_status)
        call require(stop_status == 0 .and. ok_wait, &
            'targeted failure completes and its owner stops')
        call fetch_events(project_path, 'history', trim(first_session_id), &
            first_events, n_first_events, ok_page)
        call require(ok_page .and. n_first_events >= 1, &
            'reads the first failure receipt')
        call require(first_events(1)%case_id == 'test_failure' .and. &
            first_events(1)%status == 'FAIL', 'targeted failure is durably recorded')

        call append_generation_marker(project_path//'/test/test_alpha.f90')
        call start_session(project_path, 'history', 22, 1, .false., target, 0, &
            second_session_id)
        call wait_for_receipts(project_path, 'history', trim(second_session_id), &
            1, ok_wait)
        call stop_session(project_path, 'history', trim(second_session_id), stop_status)
        call require(stop_status == 0 .and. ok_wait, &
            'new-generation history campaign completes and stops')
        call fetch_events(project_path, 'history', trim(second_session_id), &
            second_events, n_second_events, ok_page)
        call require(ok_page .and. n_second_events >= 1, &
            'reads new-generation failure-priority receipt')
        call require(second_events(1)%case_id == 'test_failure' .and. &
            second_events(1)%status == 'FAIL', &
            'prior-generation failure is the first selected case')
        call require(second_events(1)%generation /= first_events(1)%generation, &
            'historical failure priority applies to a new immutable generation')

        call read_markers(marker_path, markers, prior_markers, marker_found)
        call require(marker_found, 'reads marker count before gated work')
        target(1) = 'test_unknown'
        call start_session(project_path, 'history', 33, 1, .false., target, 1, &
            unknown_session_id)
        call wait_for_marker_after(marker_path, 'test_unknown', prior_markers, &
            marker_found)
        call stop_session(project_path, 'history', trim(unknown_session_id), &
            stop_status)
        call require(stop_status == 0 .and. marker_found, &
            'stops the owner after the blocked case writes its marker')
        call fetch_events(project_path, 'history', trim(unknown_session_id), &
            unknown_events, n_unknown_events, ok_page)
        call require(ok_page, 'reads events after cancelling gated work')
        call require(count_named(unknown_events, n_unknown_events, 'test_unknown') == 0, &
            'cancelled gated work has no completion receipt')

        call append_generation_marker(project_path//'/test/test_beta.f90')
        call start_session(project_path, 'history', 44, 1, .false., target, 0, &
            fourth_session_id)
        call wait_for_receipts(project_path, 'history', trim(fourth_session_id), &
            1, ok_wait)
        call stop_session(project_path, 'history', trim(fourth_session_id), stop_status)
        call require(stop_status == 0 .and. ok_wait, &
            'post-cancellation priority campaign completes and stops')
        call fetch_events(project_path, 'history', trim(fourth_session_id), &
            fourth_events, n_fourth_events, ok_page)
        call require(ok_page .and. n_fourth_events >= 1, &
            'reads failure priority after unknown cancellation')
        call require(fourth_events(1)%case_id == 'test_failure' .and. &
            fourth_events(1)%status == 'FAIL', &
            'unknown work does not displace durable failure priority')
        call require(fourth_events(1)%generation /= second_events(1)%generation, &
            'failure priority is reevaluated for the latest generation')
    end subroutine verify_failure_history

    subroutine append_generation_marker(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios

        open(newunit=unit, file=trim(path), status='old', position='append', &
            action='write', iostat=ios)
        call require(ios == 0, 'opens a fixture source to change generation')
        write(unit, '(a)') '! source-only generation marker'
        close(unit)
    end subroutine append_generation_marker

    integer function count_named(events, n_events, name) result(n_matches)
        type(receipt_t), intent(in) :: events(:)
        integer, intent(in) :: n_events
        character(len=*), intent(in) :: name
        integer :: i

        n_matches = 0
        do i = 1, n_events
            if (events(i)%case_id == name) n_matches = n_matches + 1
        end do
    end function count_named

    subroutine wait_for_marker_after(path, name, old_count, found)
        character(len=*), intent(in) :: path, name
        integer, intent(in) :: old_count
        logical, intent(out) :: found
        character(len=NAME_LEN) :: markers(MAX_EVENTS)
        integer :: n_markers, attempt
        logical :: readable

        found = .false.
        do attempt = 1, 1200
            call read_markers(path, markers, n_markers, readable)
            if (readable .and. n_markers > old_count) then
                if (any(markers(old_count + 1:n_markers) == name)) then
                    found = .true.
                    return
                end if
            end if
            call pause_for(50000)
        end do
    end subroutine wait_for_marker_after

    subroutine read_markers(path, markers, n_markers, success)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: markers(:)
        integer, intent(out) :: n_markers
        logical, intent(out) :: success
        integer :: unit, ios

        markers = ''
        n_markers = 0
        open(newunit=unit, file=trim(path), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) then
            success = .false.
            return
        end if
        do while (n_markers < size(markers))
            read(unit, '(a)', iostat=ios) markers(n_markers + 1)
            if (ios /= 0) exit
            n_markers = n_markers + 1
        end do
        close(unit)
        success = .true.
    end subroutine read_markers

    subroutine pause_for(microseconds)
        integer, intent(in) :: microseconds
        integer(c_int) :: ignored

        ignored = wait_microseconds(int(microseconds, c_int))
    end subroutine pause_for

    subroutine read_text(path, contents, success)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: contents
        logical, intent(out) :: success
        integer :: unit, file_size, ios

        inquire(file=trim(path), size=file_size, iostat=ios)
        if (ios /= 0 .or. file_size < 0) then
            contents = ''
            success = .false.
            return
        end if
        allocate(character(len=file_size) :: contents)
        if (file_size == 0) then
            success = .true.
            return
        end if
        open(newunit=unit, file=trim(path), status='old', access='stream', &
            form='unformatted', action='read', iostat=ios)
        if (ios == 0) then
            read(unit, iostat=ios) contents
            close(unit)
        end if
        success = ios == 0
        if (.not. success) contents = ''
    end subroutine read_text

    subroutine remove_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios

        open(newunit=unit, file=trim(path), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        close(unit, status='delete', iostat=ios)
    end subroutine remove_file

    subroutine clean_scratch(path)
        character(len=*), intent(in) :: path
        integer :: command_status, exit_status

        call execute_command_line('chmod -R u+w '//shell_quote(path)// &
            ' && rm -rf '//shell_quote(path), cmdstat=command_status, &
            exitstat=exit_status)
        call require(command_status == 0 .and. exit_status == 0, &
            'cleans the isolated fixture scratch tree')
    end subroutine clean_scratch

    function shell_quote(value) result(quoted)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: quoted
        integer :: i

        quoted = achar(39)
        do i = 1, len_trim(value)
            if (value(i:i) == achar(39)) then
                quoted = quoted//achar(39)//achar(92)//achar(39)//achar(39)
            else
                quoted = quoted//value(i:i)
            end if
        end do
        quoted = quoted//achar(39)
    end function shell_quote

    function fortran_quote(value) result(quoted)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: quoted
        integer :: i

        quoted = achar(39)
        do i = 1, len_trim(value)
            quoted = quoted//value(i:i)
            if (value(i:i) == achar(39)) quoted = quoted//achar(39)
        end do
        quoted = quoted//achar(39)
    end function fortran_quote

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: buffer
        character(len=:), allocatable :: text

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function int_text
end program test_gremlin_campaign_history
