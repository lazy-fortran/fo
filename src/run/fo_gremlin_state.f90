module fo_gremlin_state
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_size_t
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_generation_manifest, only: generation_manifest_release
    use fx_immutable_store, only: IMMUTABLE_OK
    use fo_fs, only: fs_sleep_ms
    implicit none
    private

    integer, parameter, public :: GREMLIN_STATE_TEXT_MAX = 65536
    integer, parameter :: PATH_LEN = 4096, ID_LEN = 128, START_LEN = 64
    integer, parameter :: DRIVER_DIGEST_LEN = 64
    integer, parameter :: LEASE_GROUP_MAX = 2

    type, public :: gremlin_session_t
        character(len=:), allocatable :: state_dir
        character(len=:), allocatable :: session_id
        character(len=:), allocatable :: project_key
        character(len=:), allocatable :: lane_id
        character(len=:), allocatable :: owner_start
        character(len=:), allocatable :: recovered_session_id
        character(len=PATH_LEN) :: driver_path = ''
        character(len=DRIVER_DIGEST_LEN) :: driver_digest = ''
        integer(int64) :: driver_size = 0_int64
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

    type, public :: gremlin_lease_group_t
        type(gremlin_lease_t) :: members(LEASE_GROUP_MAX)
        integer :: count = 0
        integer :: scope_fd = -1, guardian_pid = 0
        character(len=PATH_LEN) :: scope = '', previous_scope = ''
        logical :: closing = .false.
    end type gremlin_lease_group_t

    public :: gremlin_session_acquire, gremlin_session_publish
    public :: gremlin_session_read, gremlin_session_release
    public :: gremlin_session_state_dir
    public :: gremlin_session_recovery_complete
    public :: gremlin_session_request_stop, gremlin_session_stop_requested
    public :: gremlin_session_process_matches
    public :: gremlin_lease_acquire, gremlin_lease_release
    public :: gremlin_host_lease_acquire_weighted, gremlin_host_lease_release
    public :: gremlin_generation_register_lease_at
    public :: gremlin_generation_lease_acquire_at
    public :: gremlin_generation_pin_at, gremlin_generation_prune_at
    public :: gremlin_generation_prune_inactive

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

        function c_host_lease_acquire_weighted(kind, capacity, weight, fds, slots, &
                authority, guardian, scope, previous, text_capacity) &
                bind(C, name='fo_gremlin_host_lease_acquire_weighted') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: kind(*)
            integer(c_int), value :: capacity, weight
            integer(c_int), intent(out) :: fds(*), slots(*)
            integer(c_int), intent(out) :: authority, guardian
            character(kind=c_char), intent(out) :: scope(*), previous(*)
            integer(c_int), value :: text_capacity
            integer(c_int) :: ierr
        end function c_host_lease_acquire_weighted

        function c_host_lease_close(fd) bind(C, name='fo_gremlin_host_lease_close') &
                result(ierr)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: ierr
        end function c_host_lease_close

        function c_host_scope_retire(authority, guardian, scope, previous) &
                bind(C, name='fo_gremlin_host_scope_retire') result(ierr)
            import :: c_char, c_int
            integer(c_int), intent(inout) :: authority, guardian
            character(kind=c_char), intent(in) :: scope(*), previous(*)
            integer(c_int) :: ierr
        end function c_host_scope_retire

        function c_lease_is_busy(ierr) bind(C, name='fo_gremlin_lease_is_busy') &
                result(is_busy)
            import :: c_int
            integer(c_int), value :: ierr
            integer(c_int) :: is_busy
        end function c_lease_is_busy

        function c_lease_release(fd) &
                bind(C, name='fo_gremlin_lease_release') result(ierr)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: ierr
        end function c_lease_release

        function c_generation_register_lease_at(root, fd) &
                bind(C, name='fo_gremlin_generation_register_lease_at') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), intent(out) :: fd
            integer(c_int) :: ierr
        end function c_generation_register_lease_at

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

        function c_generation_prune_candidates(root, paths, slot_size, limit, &
                min_age) bind(C, name='fo_gremlin_generation_prune_candidates') &
                result(count)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            character(kind=c_char), intent(out) :: paths(*)
            integer(c_int), value :: slot_size, limit, min_age
            integer(c_int) :: count
        end function c_generation_prune_candidates

        function c_generation_mark_release(root, store, owner) &
                bind(C, name='fo_gremlin_generation_mark_release') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), store(*), owner(*)
            integer(c_int) :: ierr
        end function c_generation_mark_release

        function c_generation_pending_release(root, store, store_cap, owner, owner_cap) &
                bind(C, name='fo_gremlin_generation_pending_release') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            character(kind=c_char), intent(out) :: store(*), owner(*)
            integer(c_int), value :: store_cap, owner_cap
            integer(c_int) :: ierr
        end function c_generation_pending_release

        function c_generation_forget(root) &
                bind(C, name='fo_gremlin_generation_forget') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int) :: ierr
        end function c_generation_forget

        integer(c_int) function c_generation_lock(path) &
                bind(C, name='fo_c_generation_lock')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_generation_lock

        subroutine c_generation_unlock(fd) bind(C, name='fo_c_generation_unlock')
            import :: c_int
            integer(c_int), value :: fd
        end subroutine c_generation_unlock
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
        integer :: attempt
        integer(c_int), parameter :: ENOENT_CODE = 2_c_int

        ! Recovering a dead owner races its exiting descendants, whose records
        ! and processes can vanish mid-scan (ENOENT). Recovery is idempotent and
        ! releases the lane lock on failure, so retry such a transient failure.
        do attempt = 1, 5
            dir = c_null_char
            sid = c_null_char
            recovered = c_null_char
            start = c_null_char
            fd = -1_c_int
            is_owner = 0_c_int
            pid = 0_c_int
            c_error = c_session_acquire(trim(project_dir)//c_null_char, &
                trim(lane_id)//c_null_char, dir, int(PATH_LEN, c_int), sid, &
                int(ID_LEN, c_int), recovered, int(ID_LEN, c_int), fd, is_owner, &
                pid, start, int(START_LEN, c_int))
            if (c_error /= ENOENT_CODE) exit
            call fs_sleep_ms(50)
        end do
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

    subroutine gremlin_host_lease_acquire_weighted(resource, capacity, weight, &
            group, busy, ierr, message)
        character(len=*), intent(in) :: resource
        integer, intent(in) :: capacity, weight
        type(gremlin_lease_group_t), intent(out) :: group
        logical, intent(out) :: busy
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: fds(LEASE_GROUP_MAX), slots(LEASE_GROUP_MAX), c_error
        integer(c_int) :: authority, guardian
        character(kind=c_char) :: scope(PATH_LEN), previous(PATH_LEN)
        integer :: i

        group = gremlin_lease_group_t()
        if (weight < 1 .or. weight > LEASE_GROUP_MAX .or. capacity < weight) then
            busy = .false.
            ierr = 1
            message = 'invalid host lease capacity or weight'
            return
        end if
        fds = -1_c_int
        slots = -1_c_int
        c_error = c_host_lease_acquire_weighted(trim(resource)//c_null_char, &
            int(capacity, c_int), int(weight, c_int), fds, slots, authority, guardian, &
            scope, previous, int(PATH_LEN, c_int))
        ierr = int(c_error)
        busy = c_lease_is_busy(c_error) /= 0_c_int
        message = error_text(ierr)
        if (ierr /= 0) return
        group%count = weight
        group%scope_fd = int(authority)
        group%guardian_pid = int(guardian)
        group%scope = c_string(scope)
        group%previous_scope = c_string(previous)
        do i = 1, weight
            group%members(i)%resource = trim(resource)
            group%members(i)%slot = int(slots(i))
            group%members(i)%lock_fd = int(fds(i))
        end do
    end subroutine gremlin_host_lease_acquire_weighted

    subroutine gremlin_host_lease_release(group, ierr, message)
        type(gremlin_lease_group_t), intent(inout) :: group
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer :: i, release_error
        integer(c_int) :: c_error, authority, guardian

        ierr = 0
        message = ''
        authority = int(group%scope_fd, c_int)
        guardian = int(group%guardian_pid, c_int)
        c_error = c_host_scope_retire(authority, guardian, &
            trim(group%scope)//c_null_char, trim(group%previous_scope)//c_null_char)
        group%scope_fd = int(authority)
        group%guardian_pid = int(guardian)
        ! A closing ancestor retains its reservation until already admitted
        ! children finish. New descendants must reacquire ordinary capacity.
        if (c_lease_is_busy(c_error) /= 0_c_int) then
            group%closing = .true.
            return
        end if
        if (c_error /= 0_c_int) then
            ierr = int(c_error)
            message = error_text(ierr)
            return
        end if
        do i = 1, group%count
            if (group%members(i)%lock_fd < 0) cycle
            c_error = c_host_lease_close(int(group%members(i)%lock_fd, c_int))
            release_error = int(c_error)
            group%members(i)%lock_fd = -1
            group%members(i)%slot = -1
            if (ierr == 0 .and. release_error /= 0) ierr = release_error
        end do
        group%count = 0
        group%closing = .false.
        message = error_text(ierr)
    end subroutine gremlin_host_lease_release

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





    subroutine gremlin_generation_register_lease_at(root, lease, ierr, message)
        character(len=*), intent(in) :: root
        type(gremlin_lease_t), intent(out) :: lease
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer(c_int) :: fd, c_error

        fd = -1_c_int
        c_error = c_generation_register_lease_at(trim(root)//c_null_char, fd)
        ierr = int(c_error)
        message = error_text(ierr)
        if (ierr /= 0) return
        lease%resource = 'generation:'//trim(root)
        lease%slot = 0
        lease%lock_fd = int(fd)
    end subroutine gremlin_generation_register_lease_at

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
        character(len=PATH_LEN) :: lock_path, store_root
        character(len=ID_LEN) :: root_owner
        character(kind=c_char) :: c_store(PATH_LEN), c_owner(ID_LEN)
        integer(c_int) :: c_error, lock_fd
        integer :: slash, locator_status
        logical :: has_manifest, exists, pending

        ierr = 1
        message = ''
        slash = index(trim(root), '/', back=.true.)
        if (slash < 2 .or. slash >= len_trim(root)) then
            message = 'generation root path is invalid'
            return
        end if
        lock_path = root(:slash - 1)//'/.locks/'//root(slash + 1:len_trim(root))
        lock_fd = c_generation_lock(trim(lock_path)//c_null_char)
        if (lock_fd < 0_c_int) then
            message = 'cannot lock generation publication during prune'
            return
        end if
        inquire (file=trim(root), exist=exists)
        c_store = c_null_char
        c_owner = c_null_char
        c_error = c_generation_pending_release( &
            trim(root)//c_null_char, c_store, &
            int(PATH_LEN, c_int), c_owner, int(ID_LEN, c_int))
        pending = c_error == 0_c_int
        if (c_error /= 0_c_int .and. c_error /= 2_c_int) then
            ierr = int(c_error)
            message = 'cannot read pending generation root release'
            call c_generation_unlock(lock_fd)
            return
        end if
        if (pending) then
            store_root = c_string(c_store)
            root_owner = c_string(c_owner)
        end if
        if (.not. exists .and. .not. pending) then
            c_error = c_generation_forget( &
                trim(root)//c_null_char)
            ierr = int(c_error)
            message = error_text(ierr)
            call c_generation_unlock(lock_fd)
            return
        end if
        inquire (file=trim(root)//'/manifest.id', exist=has_manifest)
        if (has_manifest .and. .not. pending) then
            call read_generation_locator(trim(root)//'/store.root', store_root, &
                locator_status)
            if (locator_status == 0) call read_generation_locator( &
                trim(root)//'/root.owner', root_owner, locator_status)
            if (locator_status /= 0) then
                message = 'generation manifest is missing its store locator'
                call c_generation_unlock(lock_fd)
                return
            end if
            c_error = c_generation_mark_release(trim(root)//c_null_char, &
                trim(store_root)//c_null_char, trim(root_owner)//c_null_char)
            if (c_error /= 0_c_int) then
                ierr = int(c_error)
                message = 'cannot durably record pending generation root release'
                call c_generation_unlock(lock_fd)
                return
            end if
            pending = .true.
        end if

        if (exists) then
            c_error = c_generation_prune_at(trim(root)//c_null_char)
            ierr = int(c_error)
            message = error_text(ierr)
        else
            ierr = 0
            message = ''
        end if
        if (ierr == 0 .and. pending) then
            call generation_manifest_release(trim(store_root), &
                root(slash + 1:len_trim(root)), trim(root_owner), ierr)
            if (ierr /= IMMUTABLE_OK) &
                message = 'generation pruned; pending Fx root release remains'
        end if
        if (ierr == 0) then
            c_error = c_generation_forget( &
                trim(root)//c_null_char)
            ierr = int(c_error)
            if (ierr /= 0) message = 'generation pruned but locator cleanup failed'
        end if
        call c_generation_unlock(lock_fd)
    end subroutine gremlin_generation_prune_at

    subroutine gremlin_generation_prune_inactive(anchor, ierr, message, min_age)
        character(len=*), intent(in) :: anchor
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer, intent(in), optional :: min_age
        integer, parameter :: LIMIT = 8
        character(kind=c_char) :: roots(PATH_LEN * LIMIT)
        integer(c_int) :: count
        integer :: age, i, status
        character(len=PATH_LEN) :: root, detail

        age = 60
        if (present(min_age)) age = min_age
        roots = c_null_char
        count = c_generation_prune_candidates(trim(anchor)//c_null_char, roots, &
            int(PATH_LEN, c_int), int(LIMIT, c_int), int(age, c_int))
        ierr = 0
        message = ''
        if (count < 0_c_int) then
            ierr = -int(count)
            message = 'cannot list inactive generations: '//error_text(ierr)
            return
        end if
        do i = 1, int(count)
            root = c_string(roots((i - 1) * PATH_LEN + 1:i * PATH_LEN))
            call gremlin_generation_prune_at(trim(root), status, detail)
            if (status == 0 .or. status == 2 .or. status == 16) cycle
            if (ierr == 0) then
                ierr = status
                message = 'cannot prune inactive generation '//trim(root)//': '// &
                    trim(detail)
            end if
        end do
    end subroutine gremlin_generation_prune_inactive

    subroutine read_generation_locator(path, value, ierr)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: value
        integer, intent(out) :: ierr
        integer :: unit, ios

        value = ''
        ierr = 1
        open (newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) value
        close (unit)
        if (ios /= 0 .or. len_trim(value) == 0) return
        if (index(value, achar(0)) /= 0 .or. &
            index(value, achar(10)) /= 0 .or. index(value, achar(13)) /= 0) return
        ierr = 0
    end subroutine read_generation_locator


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
