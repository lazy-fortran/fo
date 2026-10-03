program test_change_watch
    use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_associated, c_char, &
        c_null_char
    use fx_watch, only: watcher_init
    use fo_change_watch, only: change_watch_t, change_watch_poll, change_watch_close
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree
    use fo_util, only: make_tmpfile
    implicit none
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
        integer(c_int) function close_fd(fd) bind(C, name='close')
            import :: c_int
            integer(c_int), value :: fd
        end function close_fd
        integer(c_int) function chmod(path, mode) bind(C, name='chmod')
            import :: c_char, c_int
            character(c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function chmod
    end interface
    character(len=4096) :: root, directory, file, event, line
    character(len=64) :: fdinfo
    type(c_ptr) :: watch
    integer(c_int) :: ierr, kind
    integer :: unit, ios, limit, i, stage, descriptor

    call test_fx_error_contract()
    watch = native_open(ierr)
    if (.not. c_associated(watch)) then
        if (ierr /= 0) error stop 'cannot initialize native watcher'
        print *, 'native Linux fixture skipped'
        stop
    end if
    call make_tmpfile('fo-change-provider', root)
    call fs_make_dir(trim(root))
    directory = trim(root)//'/nested'
    file = trim(directory)//'/input'
    stage = 1
    kind = 0
    ierr = native_root(watch, trim(root)//c_null_char)
    if (ierr /= 0) call fail()
    ierr = native_reconcile(watch)
    if (ierr /= 0) call fail()

    ! Populate a new subtree before polling its creation notification. The
    ! reconciler must include its contents, then observe subsequent edits.
    stage = 2
    call fs_make_dir(trim(directory))
    call write_input()
    ierr = native_poll(watch, 100, event, len(event), kind)
    if (ierr /= 0 .or. kind /= 4) call fail()
    call write_input()
    ierr = native_poll(watch, 100, event, len(event), kind)
    if (ierr /= 0 .or. kind /= 1) call fail()
    if (event(:len_trim(file)) /= trim(file)) call fail()

    ! Alternate create/delete to overflow the actual kernel queue. There are
    ! no directory events here, so reconciliation can only mean queue loss.
    stage = 3
    limit = 16384
    open (newunit=unit, file='/proc/sys/fs/inotify/max_queued_events', &
        status='old', action='read', iostat=ios)
    if (ios == 0) then
        read (unit, *, iostat=ios) limit
        close (unit)
    end if
    if (limit < 1 .or. limit > 262144) call fail()
    do i = 1, limit + 1
        call write_input()
        call fs_remove_file(trim(file))
    end do
    do i = 1, limit + 64
        ierr = native_poll(watch, 100, event, len(event), kind)
        if (ierr /= 0) call fail()
        if (kind == 4) exit
    end do
    if (kind /= 4) call fail()
    call write_input()
    ierr = native_poll(watch, 100, event, len(event), kind)
    if (ierr /= 0 .or. kind == 0) call fail()

    ! Retiring a closure root drops its subscriptions.
    stage = 4
    call native_clear(watch)
    ierr = native_reconcile(watch)
    if (ierr /= 0) call fail()
    call write_input()
    ierr = native_poll(watch, 0, event, len(event), kind)
    if (ierr /= 0 .or. kind /= 0) call fail()

    ! Permission failures propagate instead of appearing as an idle poll.
    stage = 5
    ierr = chmod(trim(directory)//c_null_char, 0)
    if (ierr /= 0) call fail()
    ierr = native_root(watch, trim(directory)//c_null_char)
    if (ierr /= 0) call fail()
    ierr = native_reconcile(watch)
    if (ierr /= 13) call fail()
    ierr = chmod(trim(directory)//c_null_char, int(o'700'))
    ! Closing the kernel descriptor must report a polling error. Find it using
    ! Linux's public fdinfo view rather than depending on provider struct layout.
    stage = 6
    descriptor = -1
    do i = 3, 64
        write (fdinfo, '(a,i0)') '/proc/self/fdinfo/', i
        open (newunit=unit, file=trim(fdinfo), status='old', &
            action='read', iostat=ios)
        if (ios /= 0) cycle
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'inotify wd:') /= 1) cycle
            descriptor = i
            exit
        end do
        close (unit)
        if (descriptor >= 0) exit
    end do
    if (descriptor < 0) call fail()
    ierr = close_fd(descriptor)
    if (ierr /= 0) call fail()
    ierr = native_poll(watch, 0, event, len(event), kind)
    if (ierr == 0) call fail()
    call native_close(watch)
    call fs_remove_tree(trim(root))
    print *, 'change provider: subtree catch-up, overflow, pruning, error verified'
contains
    subroutine test_fx_error_contract()
        type(change_watch_t) :: fallback
        character(len=4096) :: changed
        integer :: error, event_kind, ignored
        logical :: got_event

        ! Exercise the shared fx fallback on every host using a real backend fd.
        call watcher_init(fallback%watcher, error)
        if (error /= 0) error stop 'cannot initialize fx fallback'
        call change_watch_poll(fallback, 0, changed, event_kind, got_event, error)
        if (error /= 0 .or. got_event) error stop 'fallback timeout contract'
        ignored = close_fd(fallback%watcher%fd)
        if (ignored /= 0) error stop 'cannot induce fallback descriptor failure'
        call change_watch_poll(fallback, 0, changed, event_kind, got_event, error)
        if (error == 0 .or. got_event) error stop 'fallback error was hidden'
        call change_watch_close(fallback)
    end subroutine test_fx_error_contract

    subroutine write_input()
        integer :: output, status

        open (newunit=output, file=trim(file), status='replace', iostat=status)
        if (status /= 0) call fail()
        write (output, '(a)') 'input'
        close (output)
    end subroutine write_input

    subroutine fail()
        integer :: ignored

        print *, 'change provider failed stage:', stage, 'error:', ierr, 'kind:', kind
        ignored = chmod(trim(directory)//c_null_char, int(o'700'))
        call native_close(watch)
        call fs_remove_tree(trim(root))
        error stop 1
    end subroutine fail
end program test_change_watch
