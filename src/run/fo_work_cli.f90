module fo_work_cli
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fx_json_build, only: json_escape_string
    use fo_process, only: process_exit
    use fo_work_modes, only: work_handle, work_owner_run, read_work_file
    implicit none
    private
    public :: work_cli_run

    integer, parameter :: ARG_LEN = 4096, TEXT_LEN = 65536

contains

    subroutine work_cli_run()
        character(len=ARG_LEN), allocatable :: args(:)
        character(len=ARG_LEN) :: action, project_dir, mode, task_file
        character(len=ARG_LEN) :: state_dir, work_id
        character(len=TEXT_LEN) :: task_json
        character(len=:), allocatable :: request_json, response
        integer :: nargs, i, n_fields, max_workers, exitcode, read_error
        logical :: show_help, max_seen

        nargs = max(0, command_argument_count() - 1)
        allocate (args(max(1, nargs)))
        args = ''
        do i = 1, nargs
            call get_command_argument(i + 1, args(i))
        end do
        action = 'start'
        project_dir = '.'
        mode = ''
        task_file = ''
        state_dir = ''
        work_id = ''
        max_workers = 1
        max_seen = .false.
        show_help = .false.
        i = 1
        if (nargs > 0 .and. len_trim(args(1)) > 0) then
            if (args(1)(1:1) /= '-') then
                action = trim(args(1))
                i = 2
            end if
        end if
        do while (i <= nargs)
            select case (trim(args(i)))
            case ('--help', '-h')
                show_help = .true.
                exit
            case ('--dir')
                call required_value(args, nargs, i, project_dir, exitcode)
                if (exitcode /= 0) call cli_error('missing value for --dir')
            case ('--mode')
                call required_value(args, nargs, i, mode, exitcode)
                if (exitcode /= 0) call cli_error('missing value for --mode')
            case ('--max-workers')
                block
                    character(len=ARG_LEN) :: raw
                    call required_value(args, nargs, i, raw, exitcode)
                    if (exitcode /= 0) call cli_error('missing value for --max-workers')
                    read (raw, *, iostat=exitcode) max_workers
                    if (exitcode /= 0) call cli_error('--max-workers must be an integer')
                    max_seen = .true.
                end block
            case ('--tasks')
                call required_value(args, nargs, i, task_file, exitcode)
                if (exitcode /= 0) call cli_error('missing value for --tasks')
            case ('--state-dir')
                call required_value(args, nargs, i, state_dir, exitcode)
                if (exitcode /= 0) call cli_error('missing value for --state-dir')
            case ('--work-id')
                call required_value(args, nargs, i, work_id, exitcode)
                if (exitcode /= 0) call cli_error('missing value for --work-id')
            case ('--json')
                continue
            case default
                call cli_error('unknown fo work option: '//trim(args(i)))
            end select
            i = i + 1
        end do
        if (show_help) then
            call print_usage()
            return
        end if
        if (trim(action) == 'run') then
            if (len_trim(state_dir) == 0 .or. len_trim(work_id) == 0) then
                call cli_error('internal work runner requires --state-dir and --work-id')
            end if
            call work_owner_run(trim(project_dir), trim(state_dir), &
                trim(work_id), exitcode)
            if (exitcode /= 0) call process_exit(exitcode)
            return
        end if
        if (trim(action) /= 'start' .and. trim(action) /= 'status' .and. &
            trim(action) /= 'cancel') then
            call cli_error('work action must be start, status, or cancel')
        end if
        n_fields = 0
        request_json = '{'
        if (trim(action) == 'start') then
            if (len_trim(mode) == 0) call cli_error('start requires --mode serial|parallel')
            call append_string_field(request_json, 'mode', trim(mode), n_fields)
            if (max_seen .or. trim(mode) == 'parallel') then
                call append_int_field(request_json, 'max_workers', max_workers, n_fields)
            end if
            if (len_trim(task_file) > 0) then
                call read_work_file(trim(task_file), task_json, read_error)
                if (read_error /= 0) then
                    call cli_error('cannot read --tasks JSON array (limit 65536 bytes)')
                end if
                call append_raw_field(request_json, 'tasks', trim(task_json), n_fields)
            end if
        end if
        request_json = request_json//'}'
        call work_handle(trim(action), trim(project_dir), request_json, &
            response, exitcode)
        if (len_trim(response) == 0) response = '{}'
        write (output_unit, '(a)') response
        if (exitcode /= 0) call process_exit(exitcode)
    end subroutine work_cli_run

    subroutine required_value(args, nargs, index_arg, value, ierr)
        character(len=*), intent(in) :: args(:)
        integer, intent(in) :: nargs
        integer, intent(inout) :: index_arg
        character(len=*), intent(out) :: value
        integer, intent(out) :: ierr

        value = ''
        ierr = 0
        if (index_arg >= nargs) then
            ierr = 1
            return
        end if
        index_arg = index_arg + 1
        value = args(index_arg)
    end subroutine required_value

    subroutine append_string_field(json, key, value, n_fields)
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: key, value
        integer, intent(inout) :: n_fields

        if (n_fields > 0) json = json//','
        json = json//'"'//trim(key)//'":"'//json_escape_string(value)//'"'
        n_fields = n_fields + 1
    end subroutine append_string_field

    subroutine append_int_field(json, key, value, n_fields)
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        integer, intent(inout) :: n_fields
        character(len=32) :: number

        write (number, '(i0)') value
        if (n_fields > 0) json = json//','
        json = json//'"'//trim(key)//'":'//trim(number)
        n_fields = n_fields + 1
    end subroutine append_int_field

    subroutine append_raw_field(json, key, value, n_fields)
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: key, value
        integer, intent(inout) :: n_fields

        if (n_fields > 0) json = json//','
        json = json//'"'//trim(key)//'":'//trim(value)
        n_fields = n_fields + 1
    end subroutine append_raw_field

    subroutine print_usage()
        write (output_unit, '(a)') 'fo work start --mode serial [--dir DIR]'
        write (output_unit, '(a)') &
            'fo work start --mode parallel --max-workers N --tasks FILE [--dir DIR]'
        write (output_unit, '(a)') 'fo work status [--dir DIR]'
        write (output_unit, '(a)') 'fo work cancel [--dir DIR]'
        write (output_unit, '(a)') &
            'Task FILE is a JSON array; each task has id, argv and optional '
        write (output_unit, '(a)') &
            'depends_on, files, apis, abis, worktree, resources.'
    end subroutine print_usage

    subroutine cli_error(message)
        character(len=*), intent(in) :: message
        write (error_unit, '(a)') 'fo work: '//trim(message)
        call print_usage()
        call process_exit(2)
    end subroutine cli_error

end module fo_work_cli
