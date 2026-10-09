module fo_lint_deep
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_cache, only: cache_store_root
    use fo_fs, only: fs_find_executable
    use fo_process, only: process_run_argv_logged, argv_push, process_getcwd
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file_alloc, json_int
    use fx_hash, only: sha256_file, sha256_string
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
            json_parser_next, json_extract_string, json_extract_int, JSON_ARRAY_START, &
                         JSON_ARRAY_END, JSON_OBJECT_START, JSON_OBJECT_END, JSON_KEY, &
                             JSON_ERROR, JSON_END_OF_INPUT
    use fx_action_result_store, only: action_result_store_t, action_result_read_t, &
                                 action_result_store_init, action_result_read_acquire, &
                                      action_result_read_lookup, &
                           action_result_read_release, action_result_materialize_blob, &
                    action_result_publish_files, ACTION_RESULT_OK, ACTION_RESULT_MISSING
    use fx_immutable_store, only: immutable_tree_entry_t, IMMUTABLE_BLOB
    implicit none
    private
    public :: lint_deep_run, lint_deep_print

contains

    subroutine lint_deep_run(root, files, n_files, diagnostics, count, exitcode, error)
        character(len=*), intent(in) :: root, files(:)
        integer, intent(in) :: n_files
        character(len=:), allocatable, intent(out) :: diagnostics, error
        integer, intent(out) :: count, exitcode
        type(action_result_store_t) :: store
        type(immutable_tree_entry_t) :: entries(1)
        character(len=4096) :: tool, store_root
        character(len=512) :: temporary, paths(1)
        character(len=64) :: key, after, result_id
        character(len=:), allocatable :: action, packed, output, record
        integer :: status, n_args, tool_exit, unit, io
        logical :: found, hit, valid

        diagnostics = '[]'
        error = ''
        count = 0
        exitcode = 2
        call fs_find_executable('fluff', tool, found)
        if (.not. found) then
            error = 'fo lint --deep: fluff not found on PATH'
            return
        end if
        call input_key(root, files, n_files, trim(tool), key, error)
        if (len(error) > 0) return
        call cache_store_root(store_root)
        call action_result_store_init(store, trim(store_root), status)
        if (status /= ACTION_RESULT_OK) then
            error = 'fo lint --deep: cannot initialize diagnostics cache'
            return
        end if
        action = 'fo-lint-deep:'//key
        call make_tmpfile('fo-lint-deep', temporary)
        call restore_cached(store, action, temporary, output, hit, error)
        if (len(error) > 0) then
            call delete_tmpfile(temporary)
            return
        end if
        if (hit) then
            call decode_cached(output, diagnostics, tool_exit, valid)
            if (.not. valid) error = 'fo lint --deep: malformed cached diagnostics'
        else
            packed = ''
            n_args = 0
            call argv_push(packed, n_args, trim(tool))
            call argv_push(packed, n_args, 'check')
            call argv_push(packed, n_args, '--output-format')
            call argv_push(packed, n_args, 'json')
            do status = 1, n_files
                call argv_push(packed, n_args, trim(files(status)))
            end do
            ! With no selected sources, do not ask fluff to discover ambient files.
            tool_exit = 0
            output = '[]'
            if (n_files > 0) then
                call process_run_argv_logged('.', packed, n_args, temporary, &
                                             .false., 300, tool_exit, heartbeat_s=0)
                call read_text_file_alloc(temporary, output, status)
                if (status /= 0) error = 'fo lint --deep: cannot read fluff output'
            end if
            if (tool_exit < 0 .or. tool_exit > 1) then
                error = 'fo lint --deep: fluff execution failed (exit '// &
                        trim(json_int(tool_exit))//')'//new_line('a')//output
            end if
            if (len(error) == 0) diagnostics = trim_json_whitespace(output)
        end if
        valid = .false.
        if (len(error) == 0) call validate_diagnostics(diagnostics, count, valid)
        if (len(error) == 0 .and. .not. valid) &
            error = 'fo lint --deep: malformed fluff JSON diagnostics'// &
                    new_line('a')//diagnostics
        call input_key(root, files, n_files, trim(tool), after, record)
        if (len(record) > 0 .or. key /= after) &
            error = 'fo lint --deep: inputs changed during analysis'
        if (len(error) == 0 .and. .not. hit) then
            record = '{"schema":1,"exit_code":'//trim(json_int(tool_exit))// &
                     ',"diagnostics":'//diagnostics//'}'
            open (newunit=unit, file=trim(temporary), status='replace', &
                  access='stream', form='unformatted', iostat=io)
            if (io == 0) then
                write (unit, iostat=io) record
                close (unit)
            end if
            status = io
            if (status == 0) then
                entries(1)%path = 'diagnostics.json'
                entries(1)%role = 'lint-diagnostics'
                entries(1)%kind = IMMUTABLE_BLOB
                entries(1)%mode = 420
                paths(1) = temporary
                call action_result_publish_files(store, action, paths, entries, &
                                                 result_id, status)
            end if
            if (status /= ACTION_RESULT_OK) &
                error = 'fo lint --deep: cannot publish diagnostics cache result'
        end if
        call delete_tmpfile(temporary)
        if (len(error) > 0) then
            if (.not. valid) then
                diagnostics = '[]'
                count = 0
            end if
            return
        end if
        exitcode = tool_exit
        if (count > 0) exitcode = 1
    end subroutine lint_deep_run

    subroutine input_key(root, files, n_files, tool, key, error)
        character(len=*), intent(in) :: root, files(:), tool
        integer, intent(in) :: n_files
        character(len=64), intent(out) :: key
        character(len=:), allocatable, intent(out) :: error
        character(len=:), allocatable :: material
        character(len=4096) :: cwd
        integer :: status, i

        key = ''
        error = ''
        call process_getcwd(cwd, status)
        if (status /= 0) then
            error = 'fo lint --deep: cannot identify working directory'
            return
        end if
        material = 'fo-deep-lint-v2:'//json_escape_string(trim(cwd))//':'// &
                   json_escape_string(root)
        call hash_input(tool, .true., material, error)
        if (len(error) == 0) &
            call hash_input(root//'/fpm.toml', .false., material, error)
        if (len(error) == 0) &
            call hash_input(trim(cwd)//'/.fluff.nml', .false., material, error)
        if (len(error) == 0) &
            call hash_input(trim(cwd)//'/fluff.nml', .false., material, error)
        do i = 1, n_files
            if (len(error) > 0) return
            call hash_input(trim(files(i)), .true., material, error)
        end do
        if (len(error) == 0) key = sha256_string(material)
    end subroutine input_key

    subroutine hash_input(path, required, material, error)
        character(len=*), intent(in) :: path
        logical, intent(in) :: required
        character(len=:), allocatable, intent(inout) :: material
        character(len=:), allocatable, intent(out) :: error
        character(len=64) :: digest
        integer :: status
        logical :: exists

        error = ''
        inquire (file=path, exist=exists)
        digest = 'missing'
        if (exists) then
            call sha256_file(path, digest, status)
            if (status /= 0) error = 'fo lint --deep: cannot hash input '//path
        else if (required) then
            error = 'fo lint --deep: missing input '//path
        end if
        material = material//new_line('a')//json_escape_string(path)//':'//trim(digest)
    end subroutine hash_input

    subroutine restore_cached(store, action, temporary, output, hit, error)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action, temporary
        character(len=:), allocatable, intent(out) :: output, error
        logical, intent(out) :: hit
        type(action_result_read_t) :: read
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=64) :: result_id
        integer :: status, release_status

        hit = .false.
        output = ''
        error = ''
        call action_result_read_acquire(store, action, read, status)
        if (status == ACTION_RESULT_MISSING) return
        if (status /= ACTION_RESULT_OK) then
            error = 'fo lint --deep: cannot acquire cached diagnostics'
            return
        end if
        call action_result_read_lookup(store, read, entries, result_id, status)
        if (status == ACTION_RESULT_OK) then
            if (size(entries) /= 1) status = -1
        end if
        if (status == ACTION_RESULT_OK) then
            if (entries(1)%path /= 'diagnostics.json' .or. &
                entries(1)%role /= 'lint-diagnostics' .or. &
                entries(1)%kind /= IMMUTABLE_BLOB) status = -1
        end if
        if (status == ACTION_RESULT_OK) &
            call action_result_materialize_blob(store, entries(1)%object_id, &
                                                temporary, 420, status)
        if (status == ACTION_RESULT_OK) &
            call read_text_file_alloc(temporary, output, status)
        call action_result_read_release(store, read, release_status)
        if (status /= ACTION_RESULT_OK .or. release_status /= ACTION_RESULT_OK) then
            error = 'fo lint --deep: invalid or unreadable cached diagnostics'
            return
        end if
        hit = .true.
    end subroutine restore_cached

    function trim_json_whitespace(text) result(value)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: value
        character(len=*), parameter :: whitespace = ' '//achar(9)//achar(10)//achar(13)
        integer :: first, last

        first = verify(text, whitespace)
        value = ''
        if (first == 0) return
        last = verify(text, whitespace, back=.true.)
        value = text(first:last)
    end function trim_json_whitespace

    subroutine decode_cached(record, diagnostics, exitcode, valid)
        character(len=*), intent(in) :: record
        character(len=:), allocatable, intent(out) :: diagnostics
        integer, intent(out) :: exitcode
        logical, intent(out) :: valid
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        integer :: schema, start, member, seen(3)
        logical :: found, expect_array

        diagnostics = '[]'
        exitcode = 2
        valid = .false.
        call json_extract_int(record, 'schema', schema, found)
        if (.not. found) return
        if (schema /= 1) return
        call json_extract_int(record, 'exit_code', exitcode, found)
        if (.not. found) return
        if (exitcode < 0 .or. exitcode > 1) return
        call json_parser_init_strict(parser, record)
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_OBJECT_START) return
        expect_array = .false.
        start = 0
        seen = 0
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ERROR) return
            if (event%event_type == JSON_END_OF_INPUT) exit
            if (expect_array) then
                if (event%event_type /= JSON_ARRAY_START) return
                start = event%raw_start
                expect_array = .false.
            else if (event%event_type == JSON_KEY .and. parser%depth == 1) then
                select case (event%string_val)
                case ('schema')
                    member = 1
                case ('exit_code')
                    member = 2
                case ('diagnostics')
                    member = 3
                    expect_array = .true.
                case default
                    return
                end select
                seen(member) = seen(member) + 1
                if (seen(member) > 1) return
            else if (event%event_type == JSON_ARRAY_END .and. parser%depth == 1) then
                if (start > 0) diagnostics = record(start:event%raw_end)
            end if
        end do
        valid = start > 0 .and. all(seen == 1)
    end subroutine decode_cached

    subroutine validate_diagnostics(diagnostics, count, valid)
        character(len=*), intent(in) :: diagnostics
        integer, intent(out) :: count
        logical, intent(out) :: valid
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        integer :: start

        count = 0
        valid = .false.
        start = 0
        call json_parser_init_strict(parser, diagnostics)
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_ARRAY_START) return
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ERROR) return
            if (event%event_type == JSON_END_OF_INPUT) exit
            if (event%event_type == JSON_OBJECT_START .and. parser%depth == 2) then
                start = event%raw_start
            else if (event%event_type == JSON_OBJECT_END .and. &
                     parser%depth == 1) then
                if (start == 0) return
                if (.not. valid_diagnostic(diagnostics(start:event%raw_end))) return
                count = count + 1
                start = 0
            else if (parser%depth == 1) then
                ! Reject scalar or nested-array entries.
                return
            end if
        end do
        valid = start == 0
    end subroutine validate_diagnostics

    logical function valid_diagnostic(object) result(valid)
        character(len=*), intent(in) :: object
        character(len=8), parameter :: strings(4) = &
                                       ['file    ', 'code    ', 'message ', 'severity']
        character(len=21), parameter :: positions(4) = &
                                    ['location.start.line  ', 'location.start.column', &
                                       'location.end.line    ', 'location.end.column  ']
        character(len=:), allocatable :: value
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        integer :: i, coordinate, seen(6)
        logical :: found, fixes_array

        valid = .false.
        do i = 1, size(strings)
            call json_extract_string(object, trim(strings(i)), value, found)
            if (.not. found) return
            if (index(value, achar(0)) > 0) return
        end do
        select case (value)
        case ('error', 'warning', 'info', 'hint')
        case default
            return
        end select
        do i = 1, size(positions)
            call json_extract_int(object, trim(positions(i)), coordinate, found)
            if (.not. found) return
            if (coordinate < 0) return
        end do
        seen = 0
        fixes_array = .false.
        call json_parser_init_strict(parser, object)
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ERROR) return
            if (event%event_type == JSON_END_OF_INPUT) exit
            if (fixes_array) then
                if (event%event_type /= JSON_ARRAY_START) return
                fixes_array = .false.
            else if (event%event_type == JSON_KEY .and. parser%depth == 1) then
                i = 0
                select case (event%string_val)
                case ('file')
                    i = 1
                case ('code')
                    i = 2
                case ('message')
                    i = 3
                case ('severity')
                    i = 4
                case ('location')
                    i = 5
                case ('fixes')
                    i = 6
                    fixes_array = .true.
                end select
                if (i > 0) then
                    seen(i) = seen(i) + 1
                    if (seen(i) > 1) return
                end if
            end if
        end do
        valid = .true.
    end function valid_diagnostic

    subroutine lint_deep_print(diagnostics)
        character(len=*), intent(in) :: diagnostics
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        character(len=:), allocatable :: object, file, code, message, severity
        integer :: start, line, column
        logical :: found

        start = 0
        call json_parser_init_strict(parser, diagnostics)
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ERROR .or. &
                event%event_type == JSON_END_OF_INPUT) exit
            if (event%event_type == JSON_OBJECT_START .and. parser%depth == 2) &
                start = event%raw_start
            if (event%event_type /= JSON_OBJECT_END .or. parser%depth /= 1) cycle
            if (start == 0) cycle
            object = diagnostics(start:event%raw_end)
            call json_extract_string(object, 'file', file, found)
            call json_extract_string(object, 'code', code, found)
            call json_extract_string(object, 'message', message, found)
            call json_extract_string(object, 'severity', severity, found)
            call json_extract_int(object, 'location.start.line', line, found)
            call json_extract_int(object, 'location.start.column', column, found)
            write (output_unit, '(a,":",i0,":",i0,": ",a,1x,a,": ",a)') &
                file, line, column, severity, code, message
        end do
    end subroutine lint_deep_print

end module fo_lint_deep
