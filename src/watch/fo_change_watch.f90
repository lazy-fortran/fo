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
        function native_open(ierr, diagnostic, capacity) &
                bind(C, name='fo_change_native_open_diagnostic') result(handle)
            import :: c_ptr, c_int, c_char
            integer(c_int), intent(out) :: ierr
            character(c_char), intent(out) :: diagnostic(*)
            integer(c_int), value :: capacity
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
        integer(c_int) function native_pending(handle) &
                bind(C, name='fo_change_native_pending')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
        end function native_pending
        subroutine native_error_text(error, buffer, capacity) &
                bind(C, name='fo_change_watch_error_text')
            import :: c_char, c_int
            integer(c_int), value :: error, capacity
            character(c_char), intent(out) :: buffer(*)
        end subroutine native_error_text
        subroutine native_diagnostic(handle, buffer, capacity) &
                bind(C, name='fo_change_native_diagnostic')
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: handle
            character(c_char), intent(out) :: buffer(*)
            integer(c_int), value :: capacity
        end subroutine native_diagnostic
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

    subroutine change_watch_init(watch, project_root, ierr, message)
        type(change_watch_t), intent(inout) :: watch
        character(len=*), intent(in) :: project_root
        integer, intent(out) :: ierr
        character(len=*), intent(out), optional :: message

        integer(c_int) :: native_error
        character(len=PATH_LEN) :: native_message
        character(kind=c_char) :: open_detail(PATH_LEN)
        integer :: i

        if (present(message)) message = ''
        call change_watch_close(watch)
        open_detail = c_null_char
        watch%native = native_open(native_error, open_detail, &
            int(size(open_detail), c_int))
        ierr = native_error
        if (ierr /= 0) then
            if (present(message)) then
                native_message = ''
                do i = 1, size(open_detail)
                    if (open_detail(i) == c_null_char) exit
                    native_message(i:i) = open_detail(i)
                end do
                if (len_trim(native_message) > 0) then
                    message = 'initialize declared root "'//trim(project_root)// &
                        '": '//trim(native_message)
                else
                    message = provider_error('open native provider', project_root, ierr)
                end if
            end if
            return
        end if
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
            call sync_native_roots(watch, ierr, message)
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

    subroutine change_watch_add_context(watch, context, ierr, message)
        type(change_watch_t), intent(inout) :: watch
        type(generation_context_t), intent(in) :: context
        integer, intent(out) :: ierr
        character(len=*), intent(out), optional :: message

        character(len=PATH_LEN) :: root
        integer :: i, j

        if (present(message)) message = ''
        ierr = 0
        watch%active(:) = .false.
        if (watch%n_roots > 0) watch%active(1) = .true.
        if (allocated(context%input_inventory%roots)) then
            do i = 1, context%input_inventory%root_count
                ! Derived configuration is immutable per capture; its authored
                ! CMake inputs and declared dependency roots are watched below.
                if (trim(context%input_inventory%roots(i)%canonical_alias) == &
                    'cmake-context') cycle
                root = canonical_or_entry( &
                    context%input_inventory%roots(i)%physical_path)
                if (len_trim(root) == 0) cycle
                call change_watch_add_root(watch, trim(root), ierr, message)
                if (ierr /= 0) return
                do j = 1, watch%n_roots
                    if (trim(watch%roots(j)) == trim(root)) &
                        watch%active(j) = .true.
                end do
            end do
        end if
        do i = 1, size(context%inputs)
            root = canonical_or_entry(context%inputs(i)%source_root)
            if (len_trim(root) == 0) cycle
            call change_watch_add_root(watch, trim(root), ierr, message)
            if (ierr /= 0) return
            do j = 1, watch%n_roots
                if (trim(watch%roots(j)) == trim(root)) watch%active(j) = .true.
            end do
        end do
        if (c_associated(watch%native)) then
            call sync_native_roots(watch, ierr, message)
        else
            do j = 2, watch%n_roots
                if (watch%active(j)) cycle
                call watcher_remove(watch%watcher, trim(watch%roots(j)), ierr)
                if (ierr /= 0) return
            end do
        end if
    end subroutine change_watch_add_context

    subroutine change_watch_add_root(watch, path, ierr, message)
        type(change_watch_t), intent(inout) :: watch
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr
        character(len=*), intent(out), optional :: message

        character(len=PATH_LEN), allocatable :: updated(:)
        logical, allocatable :: updated_active(:)
        character(len=PATH_LEN) :: root
        integer :: j
        logical :: exists, reactivated

        if (present(message)) message = ''
        root = canonical_root(path)
        if (len_trim(root) == 0) root = canonical_or_entry(path)
        exists = .false.
        reactivated = .false.
        do j = 1, watch%n_roots
            if (trim(watch%roots(j)) == trim(root)) then
                exists = .true.
                if (.not. watch%active(j)) reactivated = .true.
                watch%active(j) = .true.
            end if
        end do
        if (exists) then
            ierr = 0
            if (reactivated) then
                if (c_associated(watch%native)) then
                    call sync_native_roots(watch, ierr, message)
                else
                    call watcher_add(watch%watcher, parent_path(trim(root)), .false., ierr)
                    if (ierr /= 0) return
                    call watcher_add(watch%watcher, trim(root), .true., ierr)
                end if
            end if
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
        if (c_associated(watch%native)) &
            call sync_native_roots(watch, ierr, message)
    end subroutine change_watch_add_root

    subroutine change_watch_poll(watch, timeout_ms, changed_path, event_type, &
            got_event, ierr, provider_quiet)
        type(change_watch_t), intent(inout) :: watch
        integer, intent(in) :: timeout_ms
        character(len=*), intent(out) :: changed_path
        integer, intent(out) :: event_type
        logical, intent(out) :: got_event
        integer, intent(out) :: ierr

        logical, intent(out), optional :: provider_quiet
        character(len=PATH_LEN) :: candidate
        integer :: kind, nul, attempts
        integer(c_int) :: native_kind
        logical :: received
        integer(int64) :: deadline, now, rate, remaining

        changed_path = ''
        event_type = 0
        got_event = .false.
        ierr = 0
        if (present(provider_quiet)) provider_quiet = .false.
        if (c_associated(watch%native)) then
            ierr = native_poll(watch%native, max(0, timeout_ms), candidate, &
                len(candidate), native_kind)
            if (ierr /= 0) return
            nul = index(candidate, achar(0))
            if (nul > 0) candidate(nul:) = ' '
            changed_path = candidate
            event_type = native_kind
            got_event = native_kind /= 0
            if (present(provider_quiet)) then
                if (.not. got_event) provider_quiet = native_pending(watch%native) == 0
            end if
            return
        end if
        call system_clock(count=now, count_rate=rate)
        deadline = now + max(0_int64, int(timeout_ms, int64))*rate/1000_int64
        do attempts = 1, 1024
            call system_clock(count=now, count_rate=rate)
            remaining = max(0_int64, (deadline - now)*1000_int64/max(1_int64, rate))
            call watcher_poll(watch%watcher, candidate, kind, int(remaining), received)
            if (.not. received) then
                if (present(provider_quiet)) provider_quiet = .true.
                return
            end if
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

    subroutine sync_native_roots(watch, ierr, message)
        type(change_watch_t), intent(inout) :: watch
        integer, intent(out) :: ierr
        character(len=*), intent(out), optional :: message
        integer :: i, first_active

        if (present(message)) message = ''
        call native_clear(watch%native)
        first_active = 0
        do i = 1, watch%n_roots
            if (.not. watch%active(i)) cycle
            if (first_active == 0) first_active = i
            ierr = native_root(watch%native, trim(watch%roots(i))//c_null_char)
            if (ierr /= 0) then
                if (present(message)) then
                    message = native_error_detail(watch%native)
                    if (len_trim(message) == 0) message = provider_error( &
                        'register declared root', trim(watch%roots(i)), ierr)
                end if
                return
            end if
        end do
        ierr = native_reconcile(watch%native)
        if (ierr /= 0 .and. present(message)) then
            message = native_error_detail(watch%native)
            if (len_trim(message) == 0 .and. first_active > 0) &
                message = provider_error('reconcile', &
                trim(watch%roots(first_active)), ierr)
        end if
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

    function provider_error(operation, root, error) result(message)
        character(len=*), intent(in) :: operation, root
        integer, intent(in) :: error
        character(len=PATH_LEN) :: message
        character(kind=c_char) :: error_text(256)
        integer :: i, n

        message = trim(operation)//' root "'//trim(root)//'" failed: '
        call native_error_text(int(error, c_int), error_text, &
            int(size(error_text), c_int))
        n = len_trim(message)
        do i = 1, size(error_text)
            if (error_text(i) == c_null_char) exit
            n = n + 1
            message(n:n) = error_text(i)
        end do
    end function provider_error

    function native_error_detail(handle) result(message)
        type(c_ptr), intent(in) :: handle
        character(len=PATH_LEN) :: message
        character(kind=c_char) :: detail(PATH_LEN)
        integer :: i

        message = ''
        detail = c_null_char
        call native_diagnostic(handle, detail, int(size(detail), c_int))
        do i = 1, size(detail)
            if (detail(i) == c_null_char) exit
            message(i:i) = detail(i)
        end do
    end function native_error_detail

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
