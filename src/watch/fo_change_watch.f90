module fo_change_watch
    !! Shared filesystem change provider for Gremlin and `fo watch`.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, &
        c_ptr, c_null_ptr, c_associated
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_watch, only: watcher_t, watcher_init, watcher_add, watcher_poll, &
        watcher_close, watcher_mark_self_written, watcher_remove
    use fo_gremlin_generation, only: generation_context_t
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096
    integer, parameter, public :: CHANGE_MODIFY = 1
    integer, parameter, public :: CHANGE_CREATE = 2
    integer, parameter, public :: CHANGE_DELETE = 3
    integer, parameter, public :: CHANGE_RECONCILE = 4

    interface
        function native_open(ierr) bind(C, name='fo_change_native_open') result(handle)
            import :: c_ptr, c_int
            integer(c_int), intent(out) :: ierr
            type(c_ptr) :: handle
        end function native_open
        subroutine native_close(handle) bind(C, name='fo_change_native_close')
            import :: c_ptr
            type(c_ptr), value :: handle
        end subroutine native_close
        subroutine native_clear(handle) bind(C, name='fo_change_native_clear_roots')
            import :: c_ptr
            type(c_ptr), value :: handle
        end subroutine native_clear
        integer(c_int) function native_root(handle, path) &
                bind(C, name='fo_change_native_root')
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: handle
            character(c_char), intent(in) :: path(*)
        end function native_root
        integer(c_int) function native_reconcile(handle) &
                bind(C, name='fo_change_native_reconcile')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
        end function native_reconcile
        integer(c_int) function native_poll(handle, timeout, path, capacity, kind) &
                bind(C, name='fo_change_native_poll')
            import :: c_ptr, c_int, c_char
            type(c_ptr), value :: handle
            integer(c_int), value :: timeout, capacity
            character(c_char), intent(out) :: path(*)
            integer(c_int), intent(out) :: kind
        end function native_poll
        subroutine native_self(handle, path) bind(C, name='fo_change_native_self')
            import :: c_ptr, c_char
            type(c_ptr), value :: handle
            character(c_char), intent(in) :: path(*)
        end subroutine native_self
        integer(c_int) function fo_change_watch_realpath(path, resolved, capacity) &
                bind(C, name='fo_change_watch_realpath')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: resolved(*)
            integer(c_int), value :: capacity
        end function fo_change_watch_realpath
        integer(c_int) function fo_change_watch_is_dir(path) &
                bind(C, name='fo_change_watch_is_dir')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_change_watch_is_dir
    end interface

    type, public :: change_watch_t
        type(watcher_t) :: watcher
        type(c_ptr) :: native = c_null_ptr
        integer :: capture_count = 0
        character(len=PATH_LEN), allocatable :: roots(:)
        logical, allocatable :: active(:)
        integer :: n_roots = 0
    end type change_watch_t

    public :: change_watch_init, change_watch_add_root, change_watch_add_context
    public :: change_watch_poll, change_watch_close, change_watch_relevant
    public :: change_watch_mark_self_written

contains

    subroutine change_watch_init(watch, project_root, ierr)
        type(change_watch_t), intent(inout) :: watch
        character(len=*), intent(in) :: project_root
        integer, intent(out) :: ierr

        integer(c_int) :: native_error

        call change_watch_close(watch)
        watch%native = native_open(native_error)
        ierr = native_error
        if (ierr /= 0) return
        if (.not. c_associated(watch%native)) then
            call watcher_init(watch%watcher, ierr)
            if (ierr /= 0) return
        end if
        allocate(watch%roots(1))
        allocate(watch%active(1))
        watch%roots(1) = canonical_root(project_root)
        watch%active(1) = .true.
        watch%n_roots = 1
        if (len_trim(watch%roots(1)) == 0) then
            ierr = 1
            call change_watch_close(watch)
            return
        end if
        if (c_associated(watch%native)) then
            call sync_native_roots(watch, ierr)
            if (ierr /= 0) call change_watch_close(watch)
            return
        end if
        ! Subscribe to the parent before scanning recursively. The second add
        ! reconciles directories created while fx_watch was walking the tree.
        call watcher_add(watch%watcher, parent_path(trim(watch%roots(1))), &
            .false., ierr)
        if (ierr /= 0) then
            call change_watch_close(watch)
            return
        end if
        call watcher_add(watch%watcher, trim(watch%roots(1)), .true., ierr)
        if (ierr == 0) then
            call watcher_add(watch%watcher, trim(watch%roots(1)), .true., ierr)
        end if
        if (ierr /= 0) call change_watch_close(watch)
    end subroutine change_watch_init

    subroutine change_watch_add_context(watch, context, ierr)
        type(change_watch_t), intent(inout) :: watch
        type(generation_context_t), intent(in) :: context
        integer, intent(out) :: ierr

        character(len=PATH_LEN) :: root
        integer :: i, j

        ierr = 0
        watch%active(:) = .false.
        if (watch%n_roots > 0) watch%active(1) = .true.
        do i = 1, size(context%inputs)
            root = canonical_or_entry(context%inputs(i)%source_root)
            if (len_trim(root) == 0) cycle
            call change_watch_add_root(watch, trim(root), ierr)
            if (ierr /= 0) return
            do j = 1, watch%n_roots
                if (trim(watch%roots(j)) == trim(root)) watch%active(j) = .true.
            end do
        end do
        if (c_associated(watch%native)) then
            call sync_native_roots(watch, ierr)
        else
            do j = 2, watch%n_roots
                if (watch%active(j)) cycle
                call watcher_remove(watch%watcher, trim(watch%roots(j)), ierr)
                if (ierr /= 0) return
            end do
        end if
    end subroutine change_watch_add_context

    subroutine change_watch_add_root(watch, path, ierr)
        type(change_watch_t), intent(inout) :: watch
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr

        character(len=PATH_LEN), allocatable :: updated(:)
        logical, allocatable :: updated_active(:)
        character(len=PATH_LEN) :: root
        integer :: j
        logical :: exists

        root = canonical_root(path)
        if (len_trim(root) == 0) root = canonical_or_entry(path)
        exists = .false.
        do j = 1, watch%n_roots
            if (trim(watch%roots(j)) == trim(root)) then
                exists = .true.
                watch%active(j) = .true.
            end if
        end do
        if (exists) then
            ierr = 0
            return
        end if
        if (.not. c_associated(watch%native)) then
            call watcher_add(watch%watcher, parent_path(trim(root)), .false., ierr)
            if (ierr /= 0) return
            call watcher_add(watch%watcher, trim(root), .true., ierr)
            if (ierr /= 0) return
        end if
        allocate(updated(watch%n_roots + 1))
        allocate(updated_active(watch%n_roots + 1))
        if (watch%n_roots > 0) updated(:watch%n_roots) = watch%roots
        if (watch%n_roots > 0) updated_active(:watch%n_roots) = watch%active
        updated(watch%n_roots + 1) = root
        updated_active(watch%n_roots + 1) = .true.
        call move_alloc(updated, watch%roots)
        call move_alloc(updated_active, watch%active)
        watch%n_roots = watch%n_roots + 1
        ierr = 0
        if (c_associated(watch%native)) call sync_native_roots(watch, ierr)
    end subroutine change_watch_add_root

    subroutine change_watch_poll(watch, timeout_ms, changed_path, event_type, &
            got_event, ierr)
        type(change_watch_t), intent(inout) :: watch
        integer, intent(in) :: timeout_ms
        character(len=*), intent(out) :: changed_path
        integer, intent(out) :: event_type
        logical, intent(out) :: got_event
        integer, intent(out) :: ierr

        character(len=PATH_LEN) :: candidate
        integer :: kind, nul, attempts
        integer(c_int) :: native_kind
        logical :: received
        integer(int64) :: deadline, now, rate, remaining

        changed_path = ''
        event_type = 0
        got_event = .false.
        ierr = 0
        if (c_associated(watch%native)) then
            ierr = native_poll(watch%native, max(0, timeout_ms), candidate, &
                len(candidate), native_kind)
            if (ierr /= 0) return
            nul = index(candidate, achar(0))
            if (nul > 0) candidate(nul:) = ' '
            changed_path = candidate
            event_type = native_kind
            got_event = native_kind /= 0
            return
        end if
        call system_clock(count=now, count_rate=rate)
        deadline = now + max(0_int64, int(timeout_ms, int64))*rate/1000_int64
        do attempts = 1, 1024
            call system_clock(count=now, count_rate=rate)
            remaining = max(0_int64, (deadline - now)*1000_int64/max(1_int64, rate))
            call watcher_poll(watch%watcher, candidate, kind, int(remaining), received, ierr)
            if (ierr /= 0) return
            if (.not. received) return
            call reconcile_roots(watch, ierr)
            if (ierr /= 0) return
            if (.not. change_watch_relevant(watch, trim(candidate))) cycle
            changed_path = candidate
            event_type = kind
            got_event = .true.
            return
        end do
    end subroutine change_watch_poll

    logical function change_watch_relevant(watch, path) result(relevant)
        type(change_watch_t), intent(in) :: watch
        character(len=*), intent(in) :: path
        integer :: i, n
        character(len=PATH_LEN) :: rel

        relevant = .false.
        do i = 1, watch%n_roots
            if (.not. watch%active(i)) cycle
            n = len_trim(watch%roots(i))
            if (trim(path) == trim(watch%roots(i))) then
                relevant = .true.
                return
            end if
            if (len_trim(path) <= n) cycle
            if (path(:n) /= trim(watch%roots(i))) cycle
            if (path(n + 1:n + 1) /= '/') cycle
            rel = path(n + 2:)
            if (excluded_path(trim(rel))) cycle
            relevant = .true.
            return
        end do
    end function change_watch_relevant

    logical function excluded_path(path) result(excluded)
        character(len=*), intent(in) :: path
        integer :: first, last

        excluded = .false.
        first = 1
        do while (first <= len_trim(path))
            last = index(path(first:), '/')
            if (last == 0) last = len_trim(path) - first + 2
            select case (path(first:first + last - 2))
            case ('.git', '.hg', '.svn', '.bzr', '.gremlin')
                excluded = .true.
                return
            case ('build')
                if (first == 1) then
                    excluded = .true.
                    return
                end if
            end select
            first = first + last
        end do
    end function excluded_path

    subroutine sync_native_roots(watch, ierr)
        type(change_watch_t), intent(inout) :: watch
        integer, intent(out) :: ierr
        integer :: i

        call native_clear(watch%native)
        do i = 1, watch%n_roots
            if (.not. watch%active(i)) cycle
            ierr = native_root(watch%native, trim(watch%roots(i))//c_null_char)
            if (ierr /= 0) return
        end do
        ierr = native_reconcile(watch%native)
    end subroutine sync_native_roots

    subroutine reconcile_roots(watch, ierr)
        type(change_watch_t), intent(inout) :: watch
        integer, intent(out) :: ierr
        integer :: i, add_error
        logical :: is_directory

        ierr = 0
        do i = 1, watch%n_roots
            if (.not. watch%active(i)) cycle
            is_directory = directory_exists(trim(watch%roots(i)))
            if (is_directory) then
                call watcher_add(watch%watcher, trim(watch%roots(i)), .true., &
                    add_error)
                if (add_error /= 0) then
                    ierr = add_error
                    return
                end if
            end if
        end do
    end subroutine reconcile_roots

    function parent_path(path) result(parent)
        character(len=*), intent(in) :: path
        character(len=PATH_LEN) :: parent
        integer :: slash

        parent = ''
        slash = index(trim(path), '/', back=.true.)
        if (slash <= 1) then
            parent = '/'
        else
            parent = path(:slash - 1)
        end if
    end function parent_path

    function canonical_or_entry(path) result(root)
        character(len=*), intent(in) :: path
        character(len=PATH_LEN) :: root
        character(len=PATH_LEN) :: parent

        root = canonical_root(path)
        if (len_trim(root) > 0) return
        parent = canonical_root(parent_path(trim(path)))
        if (len_trim(parent) == 0) return
        root = trim(parent)//'/'//basename(trim(path))
    end function canonical_or_entry

    function basename(path) result(name)
        character(len=*), intent(in) :: path
        character(len=PATH_LEN) :: name
        integer :: slash

        slash = index(trim(path), '/', back=.true.)
        name = path(slash + 1:)
    end function basename

    logical function directory_exists(path) result(exists)
        character(len=*), intent(in) :: path

        exists = fo_change_watch_is_dir(trim(path)//c_null_char) /= 0
    end function directory_exists

    subroutine change_watch_close(watch)
        type(change_watch_t), intent(inout) :: watch

        if (c_associated(watch%native)) call native_close(watch%native)
        watch%native = c_null_ptr
        call watcher_close(watch%watcher)
        watch%capture_count = 0
        if (allocated(watch%roots)) deallocate(watch%roots)
        if (allocated(watch%active)) deallocate(watch%active)
        watch%n_roots = 0
    end subroutine change_watch_close

    subroutine change_watch_mark_self_written(watch, path)
        type(change_watch_t), intent(inout) :: watch
        character(len=*), intent(in) :: path

        if (c_associated(watch%native)) then
            call native_self(watch%native, trim(path)//c_null_char)
        else
            call watcher_mark_self_written(watch%watcher, path)
        end if
    end subroutine change_watch_mark_self_written

    function canonical_root(path) result(root)
        character(len=*), intent(in) :: path
        character(len=PATH_LEN) :: root
        integer(c_int) :: ierr
        integer :: n, nul

        root = ''
        ierr = fo_change_watch_realpath(trim(path)//c_null_char, root, len(root))
        if (ierr /= 0) return
        nul = index(root, achar(0))
        if (nul > 0) root(nul:) = ' '
        n = len_trim(root)
        do while (n > 1 .and. root(n:n) == '/')
            root(n:n) = ' '
            n = n - 1
        end do
    end function canonical_root

end module fo_change_watch
