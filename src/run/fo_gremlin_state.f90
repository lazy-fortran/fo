module fo_gremlin_state
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_size_t
    implicit none
    private

    integer, parameter, public :: GREMLIN_STATE_TEXT_MAX = 65536
    integer, parameter :: PATH_LEN = 4096, ID_LEN = 128, START_LEN = 64

    type, public :: gremlin_session_t
        character(len=:), allocatable :: state_dir
        character(len=:), allocatable :: session_id
        character(len=:), allocatable :: project_key
        character(len=:), allocatable :: lane_id
        character(len=:), allocatable :: owner_start
        character(len=:), allocatable :: recovered_session_id
        integer :: owner_pid = 0
        integer :: lock_fd = -1
        logical :: owner = .false.
        logical :: attached = .false.
    end type gremlin_session_t

    type, public :: gremlin_lease_t
        character(len=:), allocatable :: resource
        integer :: slot = -1
        integer :: lock_fd = -1
    end type gremlin_lease_t

    public :: gremlin_session_acquire, gremlin_session_publish
    public :: gremlin_session_read, gremlin_session_release
    public :: gremlin_session_state_dir
    public :: gremlin_session_recovery_complete
    public :: gremlin_session_request_stop, gremlin_session_stop_requested
    public :: gremlin_session_process_matches
    public :: gremlin_lease_acquire, gremlin_lease_release
    public :: gremlin_generation_register, gremlin_generation_lease_acquire
    public :: gremlin_generation_pin, gremlin_generation_prune
    public :: gremlin_generation_register_at, gremlin_generation_lease_acquire_at
    public :: gremlin_generation_pin_at, gremlin_generation_prune_at
    public :: gremlin_generation_root

    interface
        function c_session_state_dir(project, lane, dir, cap) &
                bind(C, name='fo_gremlin_session_state_dir') result(ierr)
            import :: c_char, c_int, c_size_t
            character(kind=c_char), intent(in) :: project(*), lane(*)
            character(kind=c_char), intent(out) :: dir(*)
            integer(c_size_t), value :: cap
            integer(c_int) :: ierr
        end function c_session_state_dir

        function c_session_acquire(project, lane, dir, dircap, session, sessioncap, &
                recovered, recoveredcap, fd, owner, pid, start, startcap) &
                bind(C, name='fo_gremlin_session_acquire') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project(*), lane(*)
            character(kind=c_char), intent(out) :: dir(*), session(*), recovered(*)
            character(kind=c_char), intent(out) :: start(*)
            integer(c_int), value :: dircap, sessioncap, recoveredcap, startcap
            integer(c_int), intent(out) :: fd, owner, pid
            integer(c_int) :: ierr
        end function c_session_acquire

        function c_session_release(dir, session, fd) &
                bind(C, name='fo_gremlin_session_release') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: dir(*), session(*)
            integer(c_int), value :: fd
            integer(c_int) :: ierr
        end function c_session_release

        function c_session_recovery_complete(dir, session, recovered, fd) &
                bind(C, name='fo_gremlin_session_recovery_complete') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: dir(*), session(*), recovered(*)
            integer(c_int), value :: fd
            integer(c_int) :: ierr
        end function c_session_recovery_complete

        function c_session_read(project, lane, dir, dircap, session, sessioncap, &
                pid, start, startcap, status, statuscap) &
                bind(C, name='fo_gremlin_session_read') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project(*), lane(*)
            character(kind=c_char), intent(out) :: dir(*), session(*), start(*), status(*)
            integer(c_int), value :: dircap, sessioncap, startcap, statuscap
            integer(c_int), intent(out) :: pid
            integer(c_int) :: ierr
        end function c_session_read

        function c_session_publish(dir, session, text, textlen, fd) &
                bind(C, name='fo_gremlin_session_publish') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: dir(*), session(*), text(*)
            integer(c_int), value :: textlen, fd
            integer(c_int) :: ierr
        end function c_session_publish

        function c_process_matches(pid, start) &
                bind(C, name='fo_gremlin_process_matches') result(matches)
            import :: c_char, c_int
            integer(c_int), value :: pid
            character(kind=c_char), intent(in) :: start(*)
            integer(c_int) :: matches
        end function c_process_matches

        function c_session_request_stop(project, lane, session) &
                bind(C, name='fo_gremlin_session_request_stop') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project(*), lane(*), session(*)
            integer(c_int) :: ierr
        end function c_session_request_stop

        function c_session_stop_requested(dir, session) &
                bind(C, name='fo_gremlin_session_stop_requested') result(requested)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: dir(*), session(*)
            integer(c_int) :: requested
        end function c_session_stop_requested

        function c_lease_acquire(kind, capacity, fd, slot) &
                bind(C, name='fo_gremlin_lease_acquire') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: kind(*)
            integer(c_int), value :: capacity
            integer(c_int), intent(inout) :: fd, slot
            integer(c_int) :: ierr
        end function c_lease_acquire

        function c_lease_release(fd) &
                bind(C, name='fo_gremlin_lease_release') result(ierr)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: ierr
        end function c_lease_release

        function c_generation_register(id) &
                bind(C, name='fo_gremlin_generation_register') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: id(*)
            integer(c_int) :: ierr
        end function c_generation_register

        function c_generation_lease_acquire(id, fd) &
                bind(C, name='fo_gremlin_generation_lease_acquire') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: id(*)
            integer(c_int), intent(out) :: fd
            integer(c_int) :: ierr
        end function c_generation_lease_acquire

        function c_generation_pin(id, pinned) &
                bind(C, name='fo_gremlin_generation_pin') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: id(*)
            integer(c_int), value :: pinned
            integer(c_int) :: ierr
        end function c_generation_pin

        function c_generation_prune(id) &
                bind(C, name='fo_gremlin_generation_prune') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: id(*)
            integer(c_int) :: ierr
        end function c_generation_prune

        function c_generation_register_at(root) &
                bind(C, name='fo_gremlin_generation_register_at') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int) :: ierr
        end function c_generation_register_at

        function c_generation_lease_acquire_at(root, fd) &
                bind(C, name='fo_gremlin_generation_lease_acquire_at') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), intent(out) :: fd
            integer(c_int) :: ierr
        end function c_generation_lease_acquire_at

        function c_generation_pin_at(root, pinned) &
                bind(C, name='fo_gremlin_generation_pin_at') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), value :: pinned
            integer(c_int) :: ierr
        end function c_generation_pin_at

        function c_generation_prune_at(root) &
                bind(C, name='fo_gremlin_generation_prune_at') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int) :: ierr
        end function c_generation_prune_at

        function c_generation_root(id, root, cap) &
                bind(C, name='fo_gremlin_generation_root') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: id(*)
            character(kind=c_char), intent(out) :: root(*)
            integer(c_int), value :: cap
            integer(c_int) :: ierr
        end function c_generation_root
    end interface

contains

    subroutine gremlin_session_state_dir(project_dir, lane_id, state_dir, ierr, message)
        character(len=*), intent(in) :: project_dir, lane_id
        character(len=*), intent(out) :: state_dir, message
        integer, intent(out) :: ierr
        character(kind=c_char) :: dir(PATH_LEN)
        integer(c_int) :: c_error

        dir = c_null_char
        c_error = c_session_state_dir(trim(project_dir)//c_null_char, &
            trim(lane_id)//c_null_char, dir, int(size(dir), c_size_t))
        ierr = int(c_error)
        state_dir = c_string(dir)
        message = error_text(ierr)
    end subroutine gremlin_session_state_dir

    subroutine gremlin_session_acquire(project_dir, lane_id, session, ierr, message)
        character(len=*), intent(in) :: project_dir, lane_id
        type(gremlin_session_t), intent(out) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(kind=c_char) :: dir(PATH_LEN), sid(ID_LEN), recovered(ID_LEN)
        character(kind=c_char) :: start(START_LEN)
        integer(c_int) :: fd, is_owner, pid, c_error

        dir = c_null_char
        sid = c_null_char
        recovered = c_null_char
        start = c_null_char
        fd = -1_c_int
        is_owner = 0_c_int
        pid = 0_c_int
        c_error = c_session_acquire(trim(project_dir)//c_null_char, &
            trim(lane_id)//c_null_char, dir, int(PATH_LEN, c_int), sid, &
            int(ID_LEN, c_int), recovered, int(ID_LEN, c_int), fd, is_owner, pid, &
            start, int(START_LEN, c_int))
        ierr = int(c_error)
        message = error_text(ierr)
        if (ierr /= 0) return

        session%state_dir = c_string(dir)
        session%session_id = c_string(sid)
        session%project_key = trim(project_dir)
        session%lane_id = trim(lane_id)
        session%owner_start = c_string(start)
        session%recovered_session_id = c_string(recovered)
        session%owner_pid = int(pid)
        session%lock_fd = int(fd)
        session%owner = is_owner /= 0
        session%attached = .not. session%owner
    end subroutine gremlin_session_acquire

    subroutine gremlin_session_recovery_complete(session, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error

        ierr = 0
        message = ''
        if (.not. allocated(session%recovered_session_id)) return
        if (len_trim(session%recovered_session_id) == 0) return
        if (.not. session%owner) then
            ierr = 1
            message = 'only the live owner may complete journal recovery'
            return
        end if
        c_error = c_session_recovery_complete(session%state_dir//c_null_char, &
            session%session_id//c_null_char, &
            session%recovered_session_id//c_null_char, int(session%lock_fd, c_int))
        ierr = int(c_error)
        message = error_text(ierr)
    end subroutine gremlin_session_recovery_complete

    subroutine gremlin_session_publish(session, status_text, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        character(len=*), intent(in) :: status_text
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error

        if (.not. session%owner .or. .not. allocated(session%state_dir) .or. &
            .not. allocated(session%session_id)) then
            ierr = 1
            message = 'only the live session owner may publish status'
            return
        end if
        if (len(status_text) > GREMLIN_STATE_TEXT_MAX) then
            ierr = 7
            message = 'status exceeds the bounded state record size'
            return
        end if
        c_error = c_session_publish(session%state_dir//c_null_char, &
            session%session_id//c_null_char, status_text//c_null_char, &
            int(len(status_text), c_int), int(session%lock_fd, c_int))
        ierr = int(c_error)
        message = error_text(ierr)
    end subroutine gremlin_session_publish

    subroutine gremlin_session_read(project_dir, lane_id, session_id, owner_pid, &
            owner_start, status_text, ierr, message)
        character(len=*), intent(in) :: project_dir, lane_id
        character(len=*), intent(out) :: session_id, owner_start, status_text
        integer, intent(out) :: owner_pid, ierr
        character(len=*), intent(out) :: message

        character(kind=c_char) :: dir(PATH_LEN), sid(ID_LEN), start(START_LEN)
        character(kind=c_char) :: status(GREMLIN_STATE_TEXT_MAX + 1)
        integer(c_int) :: pid, c_error

        dir = c_null_char
        sid = c_null_char
        start = c_null_char
        status = c_null_char
        pid = 0_c_int
        c_error = c_session_read(trim(project_dir)//c_null_char, &
            trim(lane_id)//c_null_char, dir, int(PATH_LEN, c_int), sid, &
            int(ID_LEN, c_int), pid, start, int(START_LEN, c_int), status, &
            int(size(status), c_int))
        ierr = int(c_error)
        message = error_text(ierr)
        session_id = c_string(sid)
        owner_pid = int(pid)
        owner_start = c_string(start)
        status_text = c_string(status)
    end subroutine gremlin_session_read

    subroutine gremlin_session_release(session, ierr, message)
        type(gremlin_session_t), intent(inout) :: session
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error

        if (.not. session%owner .or. session%lock_fd < 0) then
            ierr = 1
            message = 'only the live session owner may release ownership'
            return
        end if
        c_error = c_session_release(session%state_dir//c_null_char, &
            session%session_id//c_null_char, int(session%lock_fd, c_int))
        ierr = int(c_error)
        message = error_text(ierr)
        if (ierr == 0) then
            session%lock_fd = -1
            session%owner = .false.
        end if
    end subroutine gremlin_session_release

    subroutine gremlin_session_request_stop(project_dir, lane_id, session_id, ierr, message)
        character(len=*), intent(in) :: project_dir, lane_id, session_id
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error

        c_error = c_session_request_stop(trim(project_dir)//c_null_char, &
            trim(lane_id)//c_null_char, trim(session_id)//c_null_char)
        ierr = int(c_error)
        message = error_text(ierr)
    end subroutine gremlin_session_request_stop

    subroutine gremlin_session_stop_requested(session, requested, ierr, message)
        type(gremlin_session_t), intent(in) :: session
        logical, intent(out) :: requested
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: result

        requested = .false.
        if (.not. session%owner .or. .not. allocated(session%state_dir)) then
            ierr = 1
            message = 'only the live session owner may poll stop requests'
            return
        end if
        result = c_session_stop_requested(session%state_dir//c_null_char, &
            session%session_id//c_null_char)
        if (result < 0) then
            ierr = -int(result)
        else
            ierr = 0
            requested = result /= 0
        end if
        message = error_text(ierr)
    end subroutine gremlin_session_stop_requested

    logical function gremlin_session_process_matches(session)
        type(gremlin_session_t), intent(in) :: session
        integer(c_int) :: result

        gremlin_session_process_matches = .false.
        if (session%owner_pid <= 0 .or. .not. allocated(session%owner_start)) return
        result = c_process_matches(int(session%owner_pid, c_int), &
            session%owner_start//c_null_char)
        gremlin_session_process_matches = result /= 0
    end function gremlin_session_process_matches

    subroutine gremlin_lease_acquire(resource, capacity, lease, ierr, message)
        character(len=*), intent(in) :: resource
        integer, intent(in) :: capacity
        type(gremlin_lease_t), intent(out) :: lease
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: fd, slot, c_error

        fd = -1_c_int
        slot = -1_c_int
        c_error = c_lease_acquire(trim(resource)//c_null_char, &
            int(capacity, c_int), fd, slot)
        ierr = int(c_error)
        message = error_text(ierr)
        if (ierr /= 0) return
        lease%resource = trim(resource)
        lease%slot = int(slot)
        lease%lock_fd = int(fd)
    end subroutine gremlin_lease_acquire

    subroutine gremlin_lease_release(lease, ierr, message)
        type(gremlin_lease_t), intent(inout) :: lease
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error

        if (lease%lock_fd < 0) then
            ierr = 1
            message = 'resource lease is not held'
            return
        end if
        c_error = c_lease_release(int(lease%lock_fd, c_int))
        ierr = int(c_error)
        message = error_text(ierr)
        if (ierr == 0) then
            lease%lock_fd = -1
            lease%slot = -1
        end if
    end subroutine gremlin_lease_release

    subroutine gremlin_generation_register(generation_id, ierr, message)
        character(len=*), intent(in) :: generation_id
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error

        c_error = c_generation_register(trim(generation_id)//c_null_char)
        ierr = int(c_error)
        message = error_text(ierr)
    end subroutine gremlin_generation_register

    subroutine gremlin_generation_lease_acquire(generation_id, lease, ierr, message)
        character(len=*), intent(in) :: generation_id
        type(gremlin_lease_t), intent(out) :: lease
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: fd, c_error

        fd = -1_c_int
        c_error = c_generation_lease_acquire(trim(generation_id)//c_null_char, fd)
        ierr = int(c_error)
        message = error_text(ierr)
        if (ierr /= 0) return
        lease%resource = 'generation:'//trim(generation_id)
        lease%slot = 0
        lease%lock_fd = int(fd)
    end subroutine gremlin_generation_lease_acquire

    subroutine gremlin_generation_pin(generation_id, pinned, ierr, message)
        character(len=*), intent(in) :: generation_id
        logical, intent(in) :: pinned
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error, c_pinned

        c_pinned = 0_c_int
        if (pinned) c_pinned = 1_c_int
        c_error = c_generation_pin(trim(generation_id)//c_null_char, c_pinned)
        ierr = int(c_error)
        message = error_text(ierr)
    end subroutine gremlin_generation_pin

    subroutine gremlin_generation_prune(generation_id, ierr, message)
        character(len=*), intent(in) :: generation_id
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error

        c_error = c_generation_prune(trim(generation_id)//c_null_char)
        ierr = int(c_error)
        message = error_text(ierr)
    end subroutine gremlin_generation_prune

    subroutine gremlin_generation_register_at(root, ierr, message)
        character(len=*), intent(in) :: root
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error

        c_error = c_generation_register_at(trim(root)//c_null_char)
        ierr = int(c_error)
        message = error_text(ierr)
    end subroutine gremlin_generation_register_at

    subroutine gremlin_generation_lease_acquire_at(root, lease, ierr, message)
        character(len=*), intent(in) :: root
        type(gremlin_lease_t), intent(out) :: lease
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: fd, c_error

        fd = -1_c_int
        c_error = c_generation_lease_acquire_at(trim(root)//c_null_char, fd)
        ierr = int(c_error)
        message = error_text(ierr)
        if (ierr /= 0) return
        lease%resource = 'generation:'//trim(root)
        lease%slot = 0
        lease%lock_fd = int(fd)
    end subroutine gremlin_generation_lease_acquire_at

    subroutine gremlin_generation_pin_at(root, pinned, ierr, message)
        character(len=*), intent(in) :: root
        logical, intent(in) :: pinned
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error, c_pinned

        c_pinned = 0_c_int
        if (pinned) c_pinned = 1_c_int
        c_error = c_generation_pin_at(trim(root)//c_null_char, c_pinned)
        ierr = int(c_error)
        message = error_text(ierr)
    end subroutine gremlin_generation_pin_at

    subroutine gremlin_generation_prune_at(root, ierr, message)
        character(len=*), intent(in) :: root
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: c_error

        c_error = c_generation_prune_at(trim(root)//c_null_char)
        ierr = int(c_error)
        message = error_text(ierr)
    end subroutine gremlin_generation_prune_at

    subroutine gremlin_generation_root(generation_id, root, ierr, message)
        character(len=*), intent(in) :: generation_id
        character(len=*), intent(out) :: root
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(kind=c_char) :: c_root(len(root))
        integer(c_int) :: c_error

        c_root = c_null_char
        c_error = c_generation_root(trim(generation_id)//c_null_char, c_root, &
            int(len(root), c_int))
        ierr = int(c_error)
        message = error_text(ierr)
        root = ''
        if (ierr == 0) root = c_string(c_root)
    end subroutine gremlin_generation_root

    function c_string(buffer) result(value)
        character(kind=c_char), intent(in) :: buffer(:)
        character(len=:), allocatable :: value
        integer :: i, n

        n = size(buffer)
        do i = 1, size(buffer)
            if (buffer(i) == c_null_char) then
                n = i - 1
                exit
            end if
        end do
        allocate(character(len=n) :: value)
        do i = 1, n
            value(i:i) = buffer(i)
        end do
    end function c_string

    function error_text(ierr) result(message)
        integer, intent(in) :: ierr
        character(len=128) :: message
        select case (ierr)
        case (0)
            message = ''
        case (11)
            message = 'resource capacity is full or configured inconsistently'
        case (16)
            message = 'resource is busy because a lease or pin is still active'
        case (1)
            message = 'operation requires current session or lease ownership'
        case default
            write (message, '(a,i0)') 'Gremlin state provider error: ', ierr
        end select
    end function error_text

end module fo_gremlin_state
