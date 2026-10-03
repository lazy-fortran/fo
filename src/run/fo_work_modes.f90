module fo_work_modes
    use, intrinsic :: iso_fortran_env, only: output_unit, int64
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char, c_ptr, c_associated
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init, &
        json_parser_next, JSON_OBJECT_START, JSON_OBJECT_END, JSON_ARRAY_START, &
        JSON_ARRAY_END, JSON_KEY, JSON_STRING, JSON_INTEGER, JSON_ERROR, &
        JSON_END_OF_INPUT
    use fx_json_build, only: json_escape_string
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_fs, only: fs_find_executable, fs_rename, fs_sleep_ms
    use fo_gremlin_state, only: gremlin_session_t, gremlin_lease_t, &
        gremlin_session_acquire, gremlin_session_publish, &
        gremlin_session_read, gremlin_session_release, &
        gremlin_session_request_stop, gremlin_session_stop_requested, &
        gremlin_lease_acquire, gremlin_lease_release
    use fo_gremlin_supervisor, only: gremlin_handle
    use fo_process, only: argv_push, process_start_argv_logged, &
        process_poll_pid, process_cancel_pid
    use fo_util, only: make_sibling_tmpfile, delete_tmpfile, &
        extract_json_field
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096, TEXT_MAX = 65536
    integer, parameter :: MAX_TASKS = 32, MAX_FIELDS = 32, MAX_ARGS = 32
    integer, parameter :: NAME_LEN = 128, VALUE_LEN = 1024
    character(len=*), parameter :: WORK_LANE = 'work-mode'
    character(len=*), parameter :: CAMPAIGN_LANE = 'work-campaign'

    type :: work_task_t
        character(len=NAME_LEN) :: id = ''
        character(len=PATH_LEN) :: worktree = ''
        character(len=NAME_LEN) :: depends(MAX_FIELDS) = ''
        character(len=PATH_LEN) :: files(MAX_FIELDS) = ''
        character(len=NAME_LEN) :: apis(MAX_FIELDS) = ''
        character(len=NAME_LEN) :: abis(MAX_FIELDS) = ''
        character(len=VALUE_LEN) :: argv(MAX_ARGS) = ''
        character(len=16) :: resources(4) = ''
        integer :: n_depends = 0, n_files = 0, n_apis = 0, n_abis = 0
        integer :: n_argv = 0, n_resources = 0
        character(len=24) :: state = 'pending'
        character(len=32) :: blocked_reason = ''
        integer :: pid = 0, exitcode = 0
        character(len=PATH_LEN) :: log_file = ''
        type(gremlin_lease_t) :: leases(5)
        integer :: n_leases = 0
    end type work_task_t

    type :: work_request_t
        character(len=16) :: mode = ''
        character(len=128) :: work_id = ''
        character(len=HASH_LEN) :: policy_key = ''
        character(len=PATH_LEN) :: state_dir = ''
        character(len=PATH_LEN) :: project_dir = ''
        integer :: max_workers = 1
        integer :: n_tasks = 0
        type(work_task_t), allocatable :: tasks(:)
    end type work_request_t

    public :: work_handle, work_owner_run, read_work_file

    interface
        function c_realpath(path, resolved) bind(C, name='realpath') result(ptr)
            import :: c_char, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: resolved(*)
            type(c_ptr) :: ptr
        end function c_realpath
    end interface

contains

    subroutine work_handle(action, project_dir, request_json, response_json, exitcode)
        character(len=*), intent(in) :: action, project_dir, request_json
        character(len=:), allocatable, intent(out) :: response_json
        integer, intent(out) :: exitcode

        select case (trim(action))
        case ('start', 'work_start')
            call work_start(project_dir, request_json, response_json, exitcode)
        case ('status', 'work_status')
            call work_status(project_dir, response_json, exitcode)
        case ('cancel', 'work_cancel')
            call work_cancel(project_dir, response_json, exitcode)
        case default
            call error_json('unknown work action', response_json)
            exitcode = 2
        end select
    end subroutine work_handle

    subroutine parse_request(raw, request, ierr, message)
        character(len=*), intent(in) :: raw
        type(work_request_t), intent(out) :: request
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(json_parser_t) :: parser
        type(json_event_t) :: event, value
        character(len=128) :: key

        ierr = 0
        message = ''
        request = work_request_t()
        allocate (request%tasks(MAX_TASKS))
        call json_parser_init(parser, raw)
        call json_parser_next(parser, event)
        if (event%event_type /= JSON_OBJECT_START) then
            ierr = 1
            message = 'work request must be a JSON object'
            return
        end if
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_OBJECT_END) exit
            if (event%event_type == JSON_END_OF_INPUT .or. &
                event%event_type == JSON_ERROR) then
                ierr = 1
                message = 'work request contains malformed JSON'
                return
            end if
            if (event%event_type /= JSON_KEY) then
                call skip_json_value(parser, event, ierr)
                if (ierr /= 0) exit
                cycle
            end if
            key = ''
            if (allocated(event%string_val)) key = event%string_val
            call json_parser_next(parser, value)
            select case (trim(key))
            case ('mode')
                if (value%event_type /= JSON_STRING) then
                    ierr = 1
                    message = 'mode must be serial or parallel'
                    exit
                end if
                request%mode = value%string_val
            case ('max_workers')
                if (value%event_type /= JSON_INTEGER) then
                    ierr = 1
                    message = 'max_workers must be an integer'
                    exit
                end if
                request%max_workers = value%int_val
            case ('tasks')
                if (request%mode == 'serial') then
                    ierr = 1
                    message = 'serial mode does not accept coding worker tasks'
                    exit
                end if
                call parse_task_array(parser, value, request, ierr, message)
                if (ierr /= 0) exit
            case default
                call skip_json_value(parser, value, ierr)
                if (ierr /= 0) then
                    message = 'work request contains malformed JSON'
                    exit
                end if
            end select
        end do
        if (ierr /= 0) return
        if (request%mode /= 'serial' .and. request%mode /= 'parallel') then
            ierr = 1
            message = 'mode must be serial or parallel'
            return
        end if
        if (request%mode == 'serial' .and. request%n_tasks > 0) then
            ierr = 1
            message = 'serial mode does not accept coding worker tasks'
            return
        end if
        if (request%max_workers < 1 .or. request%max_workers > MAX_TASKS) then
            ierr = 1
            message = 'max_workers must be between 1 and 32'
            return
        end if
        if (request%mode == 'parallel') then
            if (request%n_tasks == 0) then
                ierr = 1
                message = 'parallel mode requires a nonempty tasks array'
                return
            end if
            call validate_tasks(request, ierr, message)
        end if
    end subroutine parse_request

    subroutine parse_task_array(parser, first, request, ierr, message)
        type(json_parser_t), intent(inout) :: parser
        type(json_event_t), intent(in) :: first
        type(work_request_t), intent(inout) :: request
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(json_event_t) :: event

        ierr = 0
        message = ''
        if (first%event_type /= JSON_ARRAY_START) then
            ierr = 1
            message = 'tasks must be an array of task objects'
            return
        end if
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ARRAY_END) exit
            if (event%event_type /= JSON_OBJECT_START) then
                ierr = 1
                message = 'each task must be a JSON object'
                return
            end if
            if (request%n_tasks >= MAX_TASKS) then
                ierr = 1
                message = 'tasks exceeds the limit of 32'
                return
            end if
            request%n_tasks = request%n_tasks + 1
            call parse_task_object(parser, request%tasks(request%n_tasks), &
                ierr, message)
            if (ierr /= 0) return
        end do
    end subroutine parse_task_array

    subroutine parse_task_object(parser, task, ierr, message)
        type(json_parser_t), intent(inout) :: parser
        type(work_task_t), intent(inout) :: task
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(json_event_t) :: event, value
        character(len=128) :: key

        ierr = 0
        message = ''
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_OBJECT_END) exit
            if (event%event_type /= JSON_KEY) then
                ierr = 1
                message = 'task object contains malformed JSON'
                return
            end if
            key = ''
            if (allocated(event%string_val)) key = event%string_val
            call json_parser_next(parser, value)
            select case (trim(key))
            case ('id')
                if (value%event_type /= JSON_STRING) then
                    ierr = 1
                    message = 'task id must be a string'
                    return
                end if
                task%id = value%string_val
            case ('worktree')
                if (value%event_type /= JSON_STRING) then
                    ierr = 1
                    message = 'task worktree must be a string'
                    return
                end if
                task%worktree = value%string_val
            case ('depends_on')
                call parse_string_array(parser, value, task%depends, &
                    task%n_depends, ierr, message)
            case ('files')
                call parse_string_array(parser, value, task%files, &
                    task%n_files, ierr, message)
            case ('apis')
                call parse_string_array(parser, value, task%apis, &
                    task%n_apis, ierr, message)
            case ('abis')
                call parse_string_array(parser, value, task%abis, &
                    task%n_abis, ierr, message)
            case ('argv')
                call parse_string_array(parser, value, task%argv, &
                    task%n_argv, ierr, message)
            case ('resources')
                call parse_string_array(parser, value, task%resources, &
                    task%n_resources, ierr, message)
            case default
                call skip_json_value(parser, value, ierr)
                if (ierr /= 0) message = 'task object contains malformed JSON'
            end select
            if (ierr /= 0) return
        end do
    end subroutine parse_task_object

    subroutine parse_string_array(parser, first, values, count, ierr, message)
        type(json_parser_t), intent(inout) :: parser
        type(json_event_t), intent(in) :: first
        character(len=*), intent(out) :: values(:)
        integer, intent(out) :: count, ierr
        character(len=*), intent(out) :: message

        type(json_event_t) :: event

        count = 0
        ierr = 0
        message = ''
        if (first%event_type /= JSON_ARRAY_START) then
            ierr = 1
            message = 'task lists must be arrays of strings'
            return
        end if
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ARRAY_END) exit
            if (event%event_type /= JSON_STRING .or. &
                .not. allocated(event%string_val)) then
                ierr = 1
                message = 'task lists must contain only strings'
                return
            end if
            if (count >= size(values)) then
                ierr = 1
                message = 'task list exceeds its bounded capacity'
                return
            end if
            if (len(event%string_val) > len(values)) then
                ierr = 1
                message = 'task list entry is too long'
                return
            end if
            count = count + 1
            values(count) = event%string_val
        end do
    end subroutine parse_string_array

    subroutine skip_json_value(parser, first, ierr)
        type(json_parser_t), intent(inout) :: parser
        type(json_event_t), intent(in) :: first
        integer, intent(out) :: ierr
        type(json_event_t) :: event
        integer :: start_depth

        ierr = 0
        if (first%event_type /= JSON_OBJECT_START .and. &
            first%event_type /= JSON_ARRAY_START) return
        start_depth = parser%depth
        do while (parser%depth >= start_depth .and. parser%depth > 0)
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ERROR .or. &
                event%event_type == JSON_END_OF_INPUT) then
                ierr = 1
                return
            end if
        end do
    end subroutine skip_json_value

    subroutine validate_tasks(request, ierr, message)
        type(work_request_t), intent(in) :: request
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: i, j, k
        logical :: found

        ierr = 0
        message = ''
        do i = 1, request%n_tasks
            if (len_trim(request%tasks(i)%id) == 0 .or. &
                request%tasks(i)%n_argv == 0) then
                ierr = 1
                message = 'each parallel task needs a nonempty id and argv'
                return
            end if
            if (len_trim(request%tasks(i)%argv(1)) == 0) then
                ierr = 1
                message = 'task argv must start with a nonempty executable'
                return
            end if
            do j = 1, i - 1
                if (trim(request%tasks(i)%id) == trim(request%tasks(j)%id)) then
                    ierr = 1
                    message = 'task ids must be unique'
                    return
                end if
            end do
            do j = 1, request%tasks(i)%n_depends
                found = .false.
                do k = 1, request%n_tasks
                    if (trim(request%tasks(i)%depends(j)) == &
                        trim(request%tasks(k)%id)) found = .true.
                end do
                if (.not. found) then
                    ierr = 1
                    message = 'task dependency names an unknown task'
                    return
                end if
                if (trim(request%tasks(i)%depends(j)) == &
                    trim(request%tasks(i)%id)) then
                    ierr = 1
                    message = 'task cannot depend on itself'
                    return
                end if
            end do
            do j = 1, request%tasks(i)%n_resources
                select case (trim(request%tasks(i)%resources(j)))
                case ('cpu', 'memory', 'build', 'test')
                    cycle
                case default
                    ierr = 1
                    message = 'resources may contain cpu, memory, build, or test'
                    return
                end select
            end do
        end do
        if (has_dependency_cycle(request)) then
            ierr = 1
            message = 'task dependency graph contains a cycle'
        end if
    end subroutine validate_tasks

    logical function has_dependency_cycle(request) result(cyclic)
        type(work_request_t), intent(in) :: request
        integer :: marks(MAX_TASKS), i

        marks = 0
        cyclic = .false.
        do i = 1, request%n_tasks
            if (marks(i) == 0) call visit_task(i, request, marks, cyclic)
            if (cyclic) return
        end do
    end function has_dependency_cycle

    recursive subroutine visit_task(index_task, request, marks, cyclic)
        integer, intent(in) :: index_task
        type(work_request_t), intent(in) :: request
        integer, intent(inout) :: marks(:)
        logical, intent(inout) :: cyclic
        integer :: i, dep

        if (cyclic) return
        if (marks(index_task) == 1) then
            cyclic = .true.
            return
        end if
        if (marks(index_task) == 2) return
        marks(index_task) = 1
        do i = 1, request%tasks(index_task)%n_depends
            dep = task_index(request, trim(request%tasks(index_task)%depends(i)))
            if (dep > 0) call visit_task(dep, request, marks, cyclic)
        end do
        marks(index_task) = 2
    end subroutine visit_task

    integer function task_index(request, id) result(index_task)
        type(work_request_t), intent(in) :: request
        character(len=*), intent(in) :: id
        integer :: i

        index_task = 0
        do i = 1, request%n_tasks
            if (trim(request%tasks(i)%id) == trim(id)) then
                index_task = i
                return
            end if
        end do
    end function task_index

    subroutine acquire_startup_gate(project_dir, lease, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(gremlin_lease_t), intent(out) :: lease
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(kind=c_char) :: resolved(PATH_LEN)
        character(len=PATH_LEN) :: canonical
        character(len=HASH_LEN + 11) :: resource
        type(c_ptr) :: resolved_ptr
        integer :: i, attempt

        canonical = ''
        resolved = c_null_char
        resolved_ptr = c_realpath(trim(project_dir)//c_null_char, resolved)
        if (.not. c_associated(resolved_ptr)) then
            ierr = 2
            message = 'project directory cannot be resolved'
            return
        end if
        do i = 1, PATH_LEN
            if (resolved(i) == c_null_char) exit
            canonical(i:i) = achar(iachar(resolved(i)))
        end do
        if (i > PATH_LEN) then
            ierr = 2
            message = 'project directory is too long'
            return
        end if
        resource = 'work-start-'//policy_digest(trim(canonical))
        do attempt = 1, 200
            call gremlin_lease_acquire(trim(resource), 1, lease, ierr, message)
            if (ierr == 0) return
            if (ierr /= 11 .and. ierr /= 16) return
            call fs_sleep_ms(50)
        end do
        message = 'work startup capacity unavailable'
    end subroutine acquire_startup_gate

    subroutine work_start(project_dir, request_json, response, exitcode)
        character(len=*), intent(in) :: project_dir, request_json
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode
        type(gremlin_lease_t) :: startup_lease
        character(len=PATH_LEN) :: message
        integer :: ierr, release_error

        call acquire_startup_gate(project_dir, startup_lease, ierr, message)
        if (ierr /= 0) then
            call error_json(trim(message), response)
            exitcode = 2
            return
        end if
        call work_start_locked(project_dir, request_json, response, exitcode)
        call gremlin_lease_release(startup_lease, release_error, message)
        if (release_error /= 0) then
            call error_json('cannot release work startup lease', response)
            exitcode = 2
        end if
    end subroutine work_start

    subroutine wait_start_barrier(ierr)
        integer, intent(out) :: ierr
        character(len=PATH_LEN) :: barrier
        integer :: status, attempt
        logical :: released

        ierr = 0
        barrier = ''
        call get_environment_variable('FO_WORK_START_TEST_BARRIER', &
            barrier, status=status)
        if (status /= 0 .or. len_trim(barrier) == 0) return
        call write_atomic(trim(barrier)//'.ready', 'ready', ierr)
        if (ierr /= 0) return
        do attempt = 1, 200
            inquire (file=trim(barrier)//'.go', exist=released)
            if (released) return
            call fs_sleep_ms(50)
        end do
        ierr = 110
    end subroutine wait_start_barrier

    subroutine work_start_locked(project_dir, request_json, response, exitcode)
        character(len=*), intent(in) :: project_dir, request_json
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(work_request_t) :: request
        type(gremlin_session_t) :: session
        character(len=PATH_LEN) :: message, owner_start, executable
        character(len=TEXT_MAX) :: status_text
        character(len=128) :: live_id, stored_policy, stored_work_id
        character(len=PATH_LEN) :: request_path, status_path, log_file
        character(len=:), allocatable :: packed
        integer :: ierr, owner_pid, pid, spawn_exit, n_args, i
        logical :: executable_ok

        exitcode = 0
        call parse_request(request_json, request, ierr, message)
        if (ierr /= 0) then
            call error_json(trim(message), response)
            exitcode = 2
            return
        end if
        if (len_trim(project_dir) == 0) then
            call error_json('project directory is required', response)
            exitcode = 2
            return
        end if
        request%project_dir = project_dir
        request%policy_key = policy_digest(request_json)
        call gremlin_session_acquire(project_dir, WORK_LANE, session, ierr, message)
        if (ierr /= 0) then
            call error_json(trim(message), response)
            exitcode = 2
            return
        end if
        request%state_dir = session%state_dir
        request_path = trim(session%state_dir)//'/work.request'
        status_path = trim(session%state_dir)//'/work.status'
        if (session%attached) then
            do i = 1, 60
                call gremlin_session_read(project_dir, WORK_LANE, live_id, &
                    owner_pid, owner_start, status_text, ierr, message)
                if (ierr == 0 .and. len_trim(status_text) > 0) exit
                call fs_sleep_ms(50)
            end do
            if (ierr /= 0 .or. len_trim(status_text) == 0) then
                call error_json('active work owner has not published state', response)
                exitcode = 2
                return
            end if
            call extract_json_field(status_text, 'policy_key', stored_policy)
            if (trim(stored_policy) /= trim(request%policy_key)) then
                call error_json('active work session has an incompatible request', response)
                exitcode = 2
                return
            end if
            response = trim(status_text)
            return
        end if

        request%work_id = session%session_id
        call write_atomic(request_path, trim(request_json), ierr)
        if (ierr /= 0) then
            call gremlin_session_release(session, spawn_exit, message)
            call error_json('cannot save work request: '//trim(message), response)
            exitcode = 2
            return
        end if
        status_text = make_status(request, 'starting', 0)
        call write_atomic(status_path, trim(status_text), ierr)
        if (ierr /= 0) then
            call gremlin_session_release(session, spawn_exit, message)
            call error_json('cannot publish initial work state', response)
            exitcode = 2
            return
        end if
        call gremlin_session_release(session, ierr, message)
        if (ierr /= 0) then
            call error_json('cannot reserve work owner: '//trim(message), response)
            exitcode = 2
            return
        end if
        call wait_start_barrier(ierr)
        if (ierr /= 0) then
            call error_json('work startup test barrier failed', response)
            exitcode = 2
            return
        end if
        call find_self_executable(executable, executable_ok)
        if (.not. executable_ok) then
            call error_json('cannot locate current fo executable', response)
            exitcode = 2
            return
        end if
        call make_sibling_tmpfile(status_path, log_file)
        n_args = 0
        call argv_push(packed, n_args, trim(executable))
        call argv_push(packed, n_args, 'work')
        call argv_push(packed, n_args, 'run')
        call argv_push(packed, n_args, '--dir')
        call argv_push(packed, n_args, trim(project_dir))
        call argv_push(packed, n_args, '--state-dir')
        call argv_push(packed, n_args, trim(request%state_dir))
        call argv_push(packed, n_args, '--work-id')
        call argv_push(packed, n_args, trim(request%work_id))
        call process_start_argv_logged(trim(project_dir), packed, n_args, &
            log_file, pid, spawn_exit)
        if (spawn_exit /= 0) then
            call delete_tmpfile(log_file)
            call error_json('cannot launch work owner', response)
            exitcode = 2
            return
        end if
        do i = 1, 60
            call fs_sleep_ms(50)
            call gremlin_session_read(project_dir, WORK_LANE, live_id, owner_pid, &
                owner_start, status_text, ierr, message)
            if (ierr == 0 .and. len_trim(status_text) > 0) exit
        end do
        if (i > 60 .or. ierr /= 0 .or. len_trim(status_text) == 0) then
            call process_cancel_pid(pid, spawn_exit)
            call error_json('work owner did not publish ready state', response)
            exitcode = 2
            return
        end if
        call extract_json_field(status_text, 'policy_key', stored_policy)
        call extract_json_field(status_text, 'work_id', stored_work_id)
        if (trim(stored_policy) /= trim(request%policy_key) .or. &
            trim(stored_work_id) /= trim(request%work_id)) then
            call error_json('a concurrent work start selected another request', response)
            exitcode = 2
            return
        end if
        response = trim(status_text)
    end subroutine work_start_locked

    subroutine work_status(project_dir, response, exitcode)
        character(len=*), intent(in) :: project_dir
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode
        type(gremlin_lease_t) :: startup_lease
        character(len=PATH_LEN) :: message
        integer :: ierr, release_error

        call acquire_startup_gate(project_dir, startup_lease, ierr, message)
        if (ierr /= 0) then
            call error_json(trim(message), response)
            exitcode = 2
            return
        end if
        call work_status_locked(project_dir, response, exitcode)
        call gremlin_lease_release(startup_lease, release_error, message)
        if (release_error /= 0) then
            call error_json('cannot release work startup lease', response)
            exitcode = 2
        end if
    end subroutine work_status

    subroutine work_status_locked(project_dir, response, exitcode)
        character(len=*), intent(in) :: project_dir
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        character(len=PATH_LEN) :: message
        character(len=TEXT_MAX) :: status_text
        character(len=PATH_LEN) :: session_id, owner_start
        integer :: owner_pid, ierr, release_error
        logical :: temporary_owner

        exitcode = 0
        response = ''
        call gremlin_session_acquire(project_dir, WORK_LANE, session, ierr, message)
        if (ierr /= 0) then
            call error_json('no work session has been recorded', response)
            exitcode = 2
            return
        end if
        temporary_owner = session%owner
        if (session%attached) then
            call gremlin_session_read(project_dir, WORK_LANE, session_id, &
                owner_pid, owner_start, status_text, ierr, message)
            if (ierr == 0 .and. len_trim(status_text) > 0) then
                response = trim(status_text)
            else
                call read_status_file(session%state_dir, status_text)
                response = trim(status_text)
            end if
        else
            call read_status_file(session%state_dir, status_text)
            response = trim(status_text)
            if (len_trim(response) == 0) then
                call error_json('no work session has been recorded', response)
                exitcode = 2
            end if
        end if
        if (temporary_owner) then
            call gremlin_session_release(session, release_error, message)
            if (release_error /= 0 .and. exitcode == 0) then
                call error_json('cannot release temporary work status lease', response)
                exitcode = 2
            end if
        end if
    end subroutine work_status_locked

    subroutine work_cancel(project_dir, response, exitcode)
        character(len=*), intent(in) :: project_dir
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode
        type(gremlin_lease_t) :: startup_lease
        character(len=PATH_LEN) :: message
        integer :: ierr, release_error

        call acquire_startup_gate(project_dir, startup_lease, ierr, message)
        if (ierr /= 0) then
            call error_json(trim(message), response)
            exitcode = 2
            return
        end if
        call work_cancel_locked(project_dir, response, exitcode)
        call gremlin_lease_release(startup_lease, release_error, message)
        if (release_error /= 0) then
            call error_json('cannot release work startup lease', response)
            exitcode = 2
        end if
    end subroutine work_cancel

    subroutine work_cancel_locked(project_dir, response, exitcode)
        character(len=*), intent(in) :: project_dir
        character(len=:), allocatable, intent(out) :: response
        integer, intent(out) :: exitcode

        type(gremlin_session_t) :: session
        character(len=TEXT_MAX) :: message, status_text
        character(len=PATH_LEN) :: session_id, owner_start
        integer :: owner_pid, ierr, release_error

        exitcode = 0
        call gremlin_session_acquire(project_dir, WORK_LANE, session, ierr, message)
        if (ierr /= 0) then
            call error_json('no active work session', response)
            exitcode = 2
            return
        end if
        if (session%owner) then
            call read_status_file(session%state_dir, status_text)
            call gremlin_session_release(session, release_error, message)
            if (release_error /= 0) then
                call error_json('cannot release work cancellation check', response)
                exitcode = 2
            else if (len_trim(status_text) == 0) then
                call error_json('no work session has been recorded', response)
                exitcode = 2
            else
                response = trim(status_text)
            end if
            return
        end if
        call gremlin_session_read(project_dir, WORK_LANE, session_id, owner_pid, &
            owner_start, status_text, ierr, message)
        if (ierr /= 0) then
            call error_json(trim(message), response)
            exitcode = 2
            return
        end if
        call gremlin_session_request_stop(project_dir, WORK_LANE, &
            trim(session_id), ierr, message)
        if (ierr /= 0) then
            call error_json('cannot request work cancellation: '//trim(message), response)
            exitcode = 2
            return
        end if
        call cancel_response(status_text, response)
    end subroutine work_cancel_locked

    subroutine work_owner_run(project_dir, state_dir, work_id, exitcode)
        character(len=*), intent(in) :: project_dir, state_dir, work_id
        integer, intent(out) :: exitcode

        type(work_request_t) :: request
        type(gremlin_session_t) :: session
        type(gremlin_lease_t) :: campaign_lease
        character(len=TEXT_MAX) :: raw_request, status_text
        character(len=PATH_LEN) :: message, request_path, status_path
        character(len=128) :: campaign_id, saved_work_id, saved_policy
        character(len=:), allocatable :: response
        integer :: ierr, release_error, spawned_total, i, j
        logical :: have_campaign_lease, stopping, stop_requested, workers_stopped
        logical :: deferred_campaign, campaign_stopped

        exitcode = 0
        have_campaign_lease = .false.
        stopping = .false.
        request_path = trim(state_dir)//'/work.request'
        status_path = trim(state_dir)//'/work.status'
        call read_work_file(request_path, raw_request, ierr)
        if (ierr /= 0) then
            call error_json('cannot read saved work request', response)
            call write_atomic(status_path, trim(response), release_error)
            exitcode = 2
            return
        end if
        call parse_request(trim(raw_request), request, ierr, message)
        if (ierr /= 0) then
            call error_json('saved work request is invalid: '//trim(message), response)
            call write_atomic(status_path, trim(response), release_error)
            exitcode = 2
            return
        end if
        request%project_dir = project_dir
        request%state_dir = state_dir
        request%work_id = work_id
        request%policy_key = policy_digest(trim(raw_request))
        call read_work_file(status_path, status_text, ierr)
        if (ierr /= 0) then
            exitcode = 2
            return
        end if
        call extract_json_field(status_text, 'work_id', saved_work_id)
        call extract_json_field(status_text, 'policy_key', saved_policy)
        if (trim(saved_work_id) /= trim(work_id) .or. &
            trim(saved_policy) /= trim(request%policy_key)) then
            exitcode = 2
            return
        end if
        deferred_campaign = .false.
        if (request%mode == 'parallel' .and. resource_capacity('test') == 1) then
            do i = 1, request%n_tasks
                do j = 1, request%tasks(i)%n_resources
                    if (request%tasks(i)%resources(j) == 'test') deferred_campaign = .true.
                end do
            end do
        end if
        call gremlin_session_acquire(project_dir, WORK_LANE, session, ierr, message)
        if (ierr /= 0 .or. .not. session%owner) then
            write (output_unit, '(a)') 'fo work: cannot acquire work owner: '//trim(message)
            exitcode = 2
            return
        end if
        if (.not. deferred_campaign) then
            call start_campaign(project_dir, campaign_lease, campaign_id, &
                have_campaign_lease, ierr, message)
            if (ierr /= 0) then
                call publish_error_status(session, request, trim(message))
                call gremlin_session_release(session, release_error, message)
                exitcode = 2
                return
            end if
        end if
        spawned_total = 0
        call publish_work_status(session, request, 'running', spawned_total, ierr, message)
        if (ierr /= 0) then
            if (have_campaign_lease) then
                do
                    call stop_campaign(project_dir, campaign_id, campaign_stopped)
                    if (campaign_stopped) exit
                    call fs_sleep_ms(100)
                end do
                call gremlin_lease_release(campaign_lease, release_error, message)
            end if
            call gremlin_session_release(session, release_error, message)
            exitcode = 2
            return
        end if

        if (request%mode == 'parallel') then
            do while (.not. stopping)
                call gremlin_session_stop_requested(session, stop_requested, &
                    ierr, message)
                if (ierr /= 0) then
                    stopping = .true.
                    exit
                end if
                if (stop_requested) then
                    stopping = .true.
                    exit
                end if
                call poll_workers(request, session, spawned_total, ierr, message)
                if (ierr /= 0) then
                    call publish_work_status(session, request, 'worker_error', &
                        spawned_total, release_error, message)
                    stopping = .true.
                    exit
                end if
                call mark_dependency_failures(request)
                call schedule_ready_workers(request, session, spawned_total, &
                    ierr, message)
                if (ierr /= 0) then
                    call publish_work_status(session, request, 'admission_error', &
                        spawned_total, release_error, message)
                    stopping = .true.
                    exit
                end if
                call publish_work_status(session, request, task_overall_state(request), &
                    spawned_total, ierr, message)
                if (ierr /= 0) then
                    stopping = .true.
                    exit
                end if
                if (all_tasks_terminal(request)) exit
                call fs_sleep_ms(40)
            end do
        end if

        if (deferred_campaign .and. all_tasks_terminal(request) .and. &
            .not. stopping) then
            call start_campaign(project_dir, campaign_lease, campaign_id, &
                have_campaign_lease, ierr, message)
            if (ierr /= 0) then
                call publish_error_status(session, request, trim(message))
                stopping = .true.
            end if
        end if

        ! The work session owns the campaign until explicit cancellation, even
        ! after its task graph reaches a terminal state.
        do while (.not. stopping)
            call gremlin_session_stop_requested(session, stop_requested, &
                ierr, message)
            if (ierr /= 0 .or. stop_requested) then
                stopping = .true.
            else
                call fs_sleep_ms(100)
            end if
        end do
        call publish_work_status(session, request, 'cancelling', spawned_total, &
            ierr, message)
        do
            call cancel_workers(request, workers_stopped)
            if (workers_stopped) exit
            call publish_work_status(session, request, 'cancelling', &
                spawned_total, ierr, message)
            call fs_sleep_ms(100)
        end do
        if (have_campaign_lease) then
            do
                call stop_campaign(project_dir, campaign_id, campaign_stopped)
                if (campaign_stopped) exit
                call publish_work_status(session, request, 'cancelling', &
                    spawned_total, ierr, message)
                call fs_sleep_ms(100)
            end do
        end if
        if (have_campaign_lease) then
            do
                call gremlin_lease_release(campaign_lease, release_error, message)
                if (release_error == 0) exit
                call fs_sleep_ms(100)
            end do
        end if
        do
            call publish_work_status(session, request, 'cancelled', spawned_total, &
                ierr, message)
            if (ierr == 0) exit
            call fs_sleep_ms(100)
        end do
        call gremlin_session_release(session, release_error, message)
        if (release_error /= 0) exitcode = 2
    end subroutine work_owner_run

    subroutine poll_workers(request, session, spawned_total, ierr, message)
        type(work_request_t), intent(inout) :: request
        type(gremlin_session_t), intent(in) :: session
        integer, intent(in) :: spawned_total
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: i, exitcode
        logical :: done

        ierr = 0
        message = ''
        do i = 1, request%n_tasks
            if (request%tasks(i)%state /= 'running') cycle
            call process_poll_pid(request%tasks(i)%pid, done, exitcode)
            if (.not. done) cycle
            request%tasks(i)%pid = 0
            request%tasks(i)%exitcode = exitcode
            if (exitcode == 0) then
                request%tasks(i)%state = 'succeeded'
            else
                request%tasks(i)%state = 'failed'
            end if
            call release_task_leases(request%tasks(i), ierr, message)
            if (ierr /= 0) return
            call publish_work_status(session, request, task_overall_state(request), &
                spawned_total, ierr, message)
            if (ierr /= 0) return
        end do
    end subroutine poll_workers

    subroutine mark_dependency_failures(request)
        type(work_request_t), intent(inout) :: request
        integer :: i, j, dep
        logical :: failed

        do i = 1, request%n_tasks
            if (request%tasks(i)%state /= 'pending') cycle
            failed = .false.
            do j = 1, request%tasks(i)%n_depends
                dep = task_index(request, trim(request%tasks(i)%depends(j)))
                if (dep <= 0) cycle
                if (request%tasks(dep)%state == 'failed' .or. &
                    request%tasks(dep)%state == 'skipped' .or. &
                    request%tasks(dep)%state == 'cancelled') failed = .true.
            end do
            if (failed) then
                request%tasks(i)%state = 'skipped'
                request%tasks(i)%blocked_reason = 'dependency_failed'
            end if
        end do
    end subroutine mark_dependency_failures

    subroutine schedule_ready_workers(request, session, spawned_total, ierr, message)
        type(work_request_t), intent(inout) :: request
        type(gremlin_session_t), intent(in) :: session
        integer, intent(inout) :: spawned_total
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        integer :: active_count, i, spawn_error
        logical :: conflict, available

        ierr = 0
        message = ''
        do
            active_count = running_count(request)
            if (active_count >= request%max_workers) return
            available = .false.
            do i = 1, request%n_tasks
                if (request%tasks(i)%state /= 'pending') cycle
                if (.not. dependencies_ready(request, i)) then
                    request%tasks(i)%blocked_reason = 'waiting_for_dependencies'
                    cycle
                end if
                conflict = has_active_conflict(request, i)
                if (conflict) then
                    request%tasks(i)%blocked_reason = 'ownership_conflict'
                    cycle
                end if
                call start_task(request, i, spawned_total, available, &
                    spawn_error, message)
                if (spawn_error /= 0) then
                    ierr = spawn_error
                    return
                end if
                if (available) then
                    call publish_work_status(session, request, &
                        task_overall_state(request), spawned_total, ierr, message)
                    if (ierr /= 0) return
                    exit
                end if
            end do
            if (.not. available) return
        end do
    end subroutine schedule_ready_workers

    subroutine start_task(request, index_task, spawned_total, started, ierr, message)
        type(work_request_t), intent(inout) :: request
        integer, intent(in) :: index_task
        integer, intent(inout) :: spawned_total
        logical, intent(out) :: started
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(work_task_t) :: task
        character(len=VALUE_LEN) :: env_extra
        character(len=PATH_LEN) :: cwd
        character(len=:), allocatable :: packed
        integer :: i, n_args, spawn_exit, lease_error
        logical :: lease_available

        ierr = 0
        message = ''
        started = .false.
        request%tasks(index_task)%blocked_reason = ''
        request%tasks(index_task)%n_leases = 0
        call acquire_one_lease(request%tasks(index_task), 'worker', &
            lease_available, lease_error, message)
        if (.not. lease_available) then
            if (lease_error == 0 .or. lease_error == 11 .or. lease_error == 16) then
                request%tasks(index_task)%blocked_reason = 'worker_capacity'
                return
            end if
            ierr = lease_error
            return
        end if
        call acquire_one_lease(request%tasks(index_task), 'cpu', &
            lease_available, lease_error, message)
        if (.not. lease_available) then
            call release_task_leases(request%tasks(index_task), lease_error, message)
            if (lease_error /= 0) then
                ierr = lease_error
                return
            end if
            request%tasks(index_task)%blocked_reason = 'cpu_capacity'
            return
        end if
        call acquire_one_lease(request%tasks(index_task), 'memory', &
            lease_available, lease_error, message)
        if (.not. lease_available) then
            call release_task_leases(request%tasks(index_task), lease_error, message)
            if (lease_error /= 0) then
                ierr = lease_error
                return
            end if
            request%tasks(index_task)%blocked_reason = 'memory_capacity'
            return
        end if
        do i = 1, request%tasks(index_task)%n_resources
            if (request%tasks(index_task)%resources(i) == 'cpu' .or. &
                request%tasks(index_task)%resources(i) == 'memory') cycle
            call acquire_one_lease(request%tasks(index_task), &
                trim(request%tasks(index_task)%resources(i)), lease_available, &
                lease_error, message)
            if (.not. lease_available) then
                call release_task_leases(request%tasks(index_task), &
                    lease_error, message)
                if (lease_error /= 0) then
                    ierr = lease_error
                    return
                end if
                request%tasks(index_task)%blocked_reason = &
                    trim(request%tasks(index_task)%resources(i))//'_capacity'
                return
            end if
        end do
        cwd = request%project_dir
        if (len_trim(request%tasks(index_task)%worktree) > 0) then
            cwd = request%tasks(index_task)%worktree
        end if
        call task_log_path(request%state_dir, request%work_id, index_task, &
            request%tasks(index_task)%log_file)
        packed = ''
        n_args = 0
        do i = 1, request%tasks(index_task)%n_argv
            call argv_push(packed, n_args, &
                trim(request%tasks(index_task)%argv(i)))
        end do
        env_extra = 'FO_JOBS=1;FO_TEST_JOBS=1'
        call process_start_argv_logged(trim(cwd), packed, n_args, &
            trim(request%tasks(index_task)%log_file), &
            request%tasks(index_task)%pid, spawn_exit, trim(env_extra))
        if (spawn_exit /= 0) then
            request%tasks(index_task)%state = 'failed'
            request%tasks(index_task)%exitcode = spawn_exit
            call release_task_leases(request%tasks(index_task), lease_error, message)
            if (lease_error /= 0) then
                ierr = lease_error
                return
            end if
            started = .true.
            return
        end if
        request%tasks(index_task)%state = 'running'
        request%tasks(index_task)%blocked_reason = ''
        spawned_total = spawned_total + 1
        started = .true.
    end subroutine start_task

    subroutine acquire_one_lease(task, kind, available, ierr, message)
        type(work_task_t), intent(inout) :: task
        character(len=*), intent(in) :: kind
        logical, intent(out) :: available
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(gremlin_lease_t) :: lease
        integer :: capacity

        available = .false.
        ierr = 0
        message = ''
        capacity = resource_capacity(kind)
        call gremlin_lease_acquire('work-'//trim(kind), capacity, lease, &
            ierr, message)
        if (ierr == 11 .or. ierr == 16) then
            ierr = 0
            return
        end if
        if (ierr /= 0) return
        if (task%n_leases >= size(task%leases)) then
            call gremlin_lease_release(lease, ierr, message)
            ierr = 2
            message = 'task resource lease capacity exceeded'
            return
        end if
        task%n_leases = task%n_leases + 1
        task%leases(task%n_leases) = lease
        available = .true.
    end subroutine acquire_one_lease

    subroutine release_task_leases(task, ierr, message)
        type(work_task_t), intent(inout) :: task
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: status, marker_error
        character(len=PATH_LEN) :: marker
        logical, save :: injected = .false.

        ierr = 0
        message = ''
        do while (task%n_leases > 0)
            if (task%n_leases == 2 .and. .not. injected) then
                marker = ''
                call get_environment_variable('FO_WORK_TEST_RELEASE_FAIL_ONCE', &
                    marker, status=status)
                if (status == 0 .and. len_trim(marker) > 0) then
                    injected = .true.
                    call write_atomic(trim(marker), 'injected', marker_error)
                    ierr = 5
                    message = 'injected lease release failure'
                    return
                end if
            end if
            call gremlin_lease_release(task%leases(task%n_leases), ierr, message)
            if (ierr /= 0) return
            task%n_leases = task%n_leases - 1
        end do
    end subroutine release_task_leases

    integer function resource_capacity(kind) result(capacity)
        character(len=*), intent(in) :: kind
        character(len=64) :: variable, value
        integer :: status

        select case (trim(kind))
        case ('worker')
            variable = 'FO_WORKER_CAPACITY'
            capacity = MAX_TASKS
        case ('cpu')
            variable = 'FO_WORK_CPU_CAPACITY'
            capacity = 4
        case ('memory')
            variable = 'FO_WORK_MEMORY_CAPACITY'
            capacity = 4
        case ('build')
            variable = 'FO_WORK_BUILD_CAPACITY'
            capacity = 1
        case ('test')
            variable = 'FO_WORK_TEST_CAPACITY'
            capacity = 2
        case default
            variable = ''
            capacity = 1
        end select
        if (len_trim(variable) > 0) then
            call get_environment_variable(trim(variable), status=status)
            if (status == 0) then
                value = ''
                call get_environment_variable(trim(variable), value=value, &
                    status=status)
                if (status == 0) read (value, *, iostat=status) capacity
            end if
        end if
        capacity = max(1, min(1024, capacity))
    end function resource_capacity

    logical function dependencies_ready(request, index_task) result(ready)
        type(work_request_t), intent(in) :: request
        integer, intent(in) :: index_task
        integer :: i, dep

        ready = .true.
        do i = 1, request%tasks(index_task)%n_depends
            dep = task_index(request, trim(request%tasks(index_task)%depends(i)))
            if (dep <= 0) then
                ready = .false.
                return
            end if
            if (request%tasks(dep)%state /= 'succeeded') then
                ready = .false.
                return
            end if
        end do
    end function dependencies_ready

    logical function has_active_conflict(request, index_task) result(conflict)
        type(work_request_t), intent(in) :: request
        integer, intent(in) :: index_task
        integer :: i

        conflict = .false.
        do i = 1, request%n_tasks
            if (request%tasks(i)%state /= 'running') cycle
            if (tasks_conflict(request%tasks(index_task), request%tasks(i))) then
                conflict = .true.
                return
            end if
        end do
    end function has_active_conflict

    logical function tasks_conflict(a, b) result(conflict)
        type(work_task_t), intent(in) :: a, b
        integer :: i, j

        conflict = .false.
        do i = 1, a%n_files
            do j = 1, b%n_files
                if (trim(a%files(i)) == trim(b%files(j))) conflict = .true.
            end do
        end do
        do i = 1, a%n_apis
            do j = 1, b%n_apis
                if (trim(a%apis(i)) == trim(b%apis(j))) conflict = .true.
            end do
        end do
        do i = 1, a%n_abis
            do j = 1, b%n_abis
                if (trim(a%abis(i)) == trim(b%abis(j))) conflict = .true.
            end do
        end do
    end function tasks_conflict

    integer function running_count(request) result(count)
        type(work_request_t), intent(in) :: request
        integer :: i

        count = 0
        do i = 1, request%n_tasks
            if (request%tasks(i)%state == 'running') count = count + 1
        end do
    end function running_count

    logical function all_tasks_terminal(request) result(terminal)
        type(work_request_t), intent(in) :: request
        integer :: i

        terminal = .true.
        do i = 1, request%n_tasks
            if (request%tasks(i)%state == 'pending' .or. &
                request%tasks(i)%state == 'running') terminal = .false.
        end do
    end function all_tasks_terminal

    function task_overall_state(request) result(state)
        type(work_request_t), intent(in) :: request
        character(len=24) :: state
        integer :: i

        state = 'tasks_running'
        if (all_tasks_terminal(request)) then
            state = 'tasks_complete'
            do i = 1, request%n_tasks
                if (request%tasks(i)%state == 'failed' .or. &
                    request%tasks(i)%state == 'skipped') then
                    state = 'tasks_failed'
                    exit
                end if
            end do
            return
        end if
        if (running_count(request) == 0) state = 'tasks_blocked'
    end function task_overall_state

    subroutine cancel_workers(request, workers_stopped)
        type(work_request_t), intent(inout) :: request
        logical, intent(out) :: workers_stopped
        integer :: i, cancel_error, release_error
        character(len=PATH_LEN) :: message

        workers_stopped = .true.
        do i = 1, request%n_tasks
            if (request%tasks(i)%state == 'running') then
                if (request%tasks(i)%pid > 0) then
                    call process_cancel_pid(request%tasks(i)%pid, cancel_error)
                    if (cancel_error /= 0) then
                        request%tasks(i)%blocked_reason = 'cancellation_failed'
                        workers_stopped = .false.
                        cycle
                    end if
                    request%tasks(i)%pid = 0
                end if
                call release_task_leases(request%tasks(i), release_error, message)
                if (release_error /= 0) then
                    request%tasks(i)%blocked_reason = 'lease_release_failed'
                    workers_stopped = .false.
                    cycle
                end if
                request%tasks(i)%state = 'cancelled'
                request%tasks(i)%blocked_reason = 'work_cancelled'
            else if (request%tasks(i)%state == 'pending') then
                request%tasks(i)%state = 'cancelled'
                request%tasks(i)%blocked_reason = 'work_cancelled'
            end if
        end do
    end subroutine cancel_workers

    subroutine start_campaign(project_dir, lease, campaign_id, started, ierr, message)
        character(len=*), intent(in) :: project_dir
        type(gremlin_lease_t), intent(out) :: lease
        character(len=*), intent(out) :: campaign_id
        logical, intent(out) :: started
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=:), allocatable :: response
        integer :: release_error
        character(len=PATH_LEN) :: release_message

        started = .false.
        campaign_id = ''
        call gremlin_lease_acquire('work-test', resource_capacity('test'), &
            lease, ierr, message)
        if (ierr /= 0) then
            message = 'test campaign capacity unavailable'
            return
        end if
        call gremlin_handle('start', project_dir, &
            '{"lane_id":"'//CAMPAIGN_LANE// &
            '","jobs":1,"random_count":32}', response, ierr)
        if (ierr /= 0) then
            message = 'cannot start Gremlin campaign: '//trim(response)
            call gremlin_lease_release(lease, release_error, release_message)
            return
        end if
        call extract_json_field(response, 'session_id', campaign_id)
        started = .true.
    end subroutine start_campaign

    subroutine stop_campaign(project_dir, campaign_id, stopped)
        character(len=*), intent(in) :: project_dir, campaign_id
        logical, intent(out) :: stopped
        character(len=:), allocatable :: response
        character(len=TEXT_MAX) :: status_text
        character(len=PATH_LEN) :: owner_start, message
        character(len=128) :: live_id
        integer :: exitcode, ierr, owner_pid

        call gremlin_session_read(project_dir, CAMPAIGN_LANE, live_id, &
            owner_pid, owner_start, status_text, ierr, message)
        stopped = ierr /= 0
        if (.not. stopped) stopped = trim(live_id) /= trim(campaign_id)
        if (stopped) return
        call gremlin_handle('stop', project_dir, &
            '{"lane_id":"'//CAMPAIGN_LANE//'","session_id":"'// &
            trim(campaign_id)//'"}', response, exitcode)
    end subroutine stop_campaign

    subroutine publish_error_status(session, request, message)
        type(gremlin_session_t), intent(in) :: session
        type(work_request_t), intent(in) :: request
        character(len=*), intent(in) :: message
        character(len=:), allocatable :: text
        integer :: ierr
        character(len=PATH_LEN) :: publish_message

        text = '{"action":"work_status","work_id":"'// &
            trim(request%work_id)//'","status":"error","error":"'// &
            quote(message)//'"}'
        call write_atomic(trim(session%state_dir)//'/work.status', text, ierr)
        call gremlin_session_publish(session, text, ierr, publish_message)
    end subroutine publish_error_status

    subroutine publish_work_status(session, request, status, spawned_total, &
            ierr, message)
        type(gremlin_session_t), intent(in) :: session
        type(work_request_t), intent(in) :: request
        character(len=*), intent(in) :: status
        integer, intent(in) :: spawned_total
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=:), allocatable :: text
        integer :: file_error

        text = make_status(request, status, spawned_total)
        call write_atomic(trim(session%state_dir)//'/work.status', text, file_error)
        if (file_error /= 0) then
            ierr = file_error
            message = 'cannot atomically publish work status'
            return
        end if
        call gremlin_session_publish(session, text, ierr, message)
    end subroutine publish_work_status

    function make_status(request, status, spawned_total) result(text)
        type(work_request_t), intent(in) :: request
        character(len=*), intent(in) :: status
        integer, intent(in) :: spawned_total
        character(len=:), allocatable :: text
        character(len=24) :: task_status
        character(len=32) :: number
        integer :: i

        task_status = status
        if (request%mode == 'serial') task_status = 'main_session_registered'
        if (index(status, 'tasks_') == 1) task_status = status
        write (number, '(i0)') request%max_workers
        text = '{"action":"work_status","work_id":"'// &
            quote(trim(request%work_id))//'","policy_key":"'// &
            trim(request%policy_key)//'","mode":"'//trim(request%mode)// &
            '","status":"'//trim(status)//'","task_status":"'// &
            trim(task_status)//'","max_workers":'//trim(number)
        write (number, '(i0)') spawned_total
        text = text//',"spawned_workers":'//trim(number)
        write (number, '(i0)') running_count(request)
        text = text//',"active_workers":'//trim(number)// &
            ',"may_promote":false,"campaign_lane":"'//CAMPAIGN_LANE//'"'
        if (request%mode == 'serial') then
            text = text//',"editor":"main-session"'
        end if
        text = text//',"tasks":['
        do i = 1, request%n_tasks
            if (i > 1) text = text//','
            text = text//'{"id":"'//quote(trim(request%tasks(i)%id))// &
                '","state":"'//trim(request%tasks(i)%state)//'"'
            if (request%tasks(i)%pid > 0) then
                write (number, '(i0)') request%tasks(i)%pid
                text = text//',"pid":'//trim(number)
            end if
            if (request%tasks(i)%exitcode /= 0) then
                write (number, '(i0)') request%tasks(i)%exitcode
                text = text//',"exitcode":'//trim(number)
            end if
            if (len_trim(request%tasks(i)%blocked_reason) > 0) then
                text = text//',"blocked_reason":"'// &
                    quote(trim(request%tasks(i)%blocked_reason))//'"'
            end if
            if (len_trim(request%tasks(i)%log_file) > 0) then
                text = text//',"log_file":"'// &
                    quote(trim(request%tasks(i)%log_file))//'"'
            end if
            text = text//'}'
        end do
        text = text//']}'
    end function make_status

    subroutine write_atomic(path, text, ierr)
        character(len=*), intent(in) :: path, text
        integer, intent(out) :: ierr
        character(len=PATH_LEN) :: stage
        integer :: unit, ios, rename_error

        call make_sibling_tmpfile(path, stage)
        open (newunit=unit, file=trim(stage), status='replace', &
            action='write', access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            ierr = ios
            return
        end if
        write (unit, iostat=ios) trim(text)
        close (unit, iostat=rename_error)
        if (ios == 0) ios = rename_error
        if (ios /= 0) then
            call delete_tmpfile(stage)
            ierr = ios
            return
        end if
        rename_error = fs_rename(trim(stage), trim(path))
        if (rename_error /= 0) then
            call delete_tmpfile(stage)
            ierr = rename_error
            return
        end if
        ierr = 0
    end subroutine write_atomic

    subroutine read_status_file(state_dir, text)
        character(len=*), intent(in) :: state_dir
        character(len=*), intent(out) :: text
        integer :: ierr

        call read_work_file(trim(state_dir)//'/work.status', text, ierr)
    end subroutine read_status_file

    subroutine read_work_file(path, text, ierr)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: text
        integer, intent(out) :: ierr
        integer(int64) :: file_size
        integer :: unit

        text = ''
        inquire (file=path, size=file_size, iostat=ierr)
        if (ierr /= 0) return
        if (file_size < 1 .or. file_size > len(text)) then
            ierr = 1
            return
        end if
        open (newunit=unit, file=path, status='old', action='read', &
            access='stream', form='unformatted', iostat=ierr)
        if (ierr /= 0) return
        read (unit, iostat=ierr) text(1:int(file_size))
        close (unit)
    end subroutine read_work_file

    subroutine task_log_path(state_dir, work_id, task_number, path)
        character(len=*), intent(in) :: state_dir, work_id
        integer, intent(in) :: task_number
        character(len=*), intent(out) :: path
        character(len=16) :: number

        write (number, '(i0)') task_number
        path = trim(state_dir)//'/worker-'//trim(work_id)//'-'// &
            trim(number)//'.log'
    end subroutine task_log_path

    subroutine find_self_executable(path, ok)
        character(len=*), intent(out) :: path
        logical, intent(out) :: ok
        character(len=PATH_LEN) :: argument
        integer :: status, arg_len

        argument = ''
        call get_command_argument(0, argument, arg_len, status)
        ok = .false.
        if (status == 0 .and. arg_len > 0 .and. arg_len <= len(argument)) then
            call fs_find_executable(trim(argument), path, ok)
            if (ok) return
        end if
        call fs_find_executable('fo', path, ok)
    end subroutine find_self_executable

    function policy_digest(request_json) result(digest)
        character(len=*), intent(in) :: request_json
        character(len=HASH_LEN) :: digest
        character(len=TEXT_MAX) :: part(1)

        part = ''
        part(1) = request_json
        digest = cache_digest(part, 1)
    end function policy_digest

    subroutine cancel_response(status_text, response)
        character(len=*), intent(in) :: status_text
        character(len=:), allocatable, intent(out) :: response
        character(len=128) :: work_id

        work_id = ''
        call extract_json_field(status_text, 'work_id', work_id)
        response = '{"action":"work_cancel","status":"cancel_requested",'// &
            '"work_id":"'//quote(trim(work_id))//'"}'
    end subroutine cancel_response

    subroutine error_json(message, response)
        character(len=*), intent(in) :: message
        character(len=:), allocatable, intent(out) :: response

        response = '{"error":"'//quote(message)//'"}'
    end subroutine error_json

    function quote(value) result(text)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: text

        text = json_escape_string(trim(value))
    end function quote

end module fo_work_modes
