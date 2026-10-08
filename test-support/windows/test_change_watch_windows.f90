program test_change_watch_windows_abi
    use, intrinsic :: iso_c_binding
    implicit none
    interface
        function native_open(error) bind(c, name='fo_change_native_open') result(handle)
            import c_int, c_ptr
            integer(c_int) :: error
            type(c_ptr) :: handle
        end function
        function native_root(handle, path) bind(c, name='fo_change_native_root') result(error)
            import c_int, c_ptr, c_char
            type(c_ptr), value :: handle
            character(c_char), intent(in) :: path(*)
            integer(c_int) :: error
        end function
        function native_reconcile(handle) bind(c, name='fo_change_native_reconcile') result(error)
            import c_int, c_ptr
            type(c_ptr), value :: handle
            integer(c_int) :: error
        end function
        function native_poll(handle, timeout, path, capacity, kind) &
            bind(c, name='fo_change_native_poll') result(error)
            import c_int, c_ptr, c_char
            type(c_ptr), value :: handle
            integer(c_int), value :: timeout, capacity
            character(c_char) :: path(*)
            integer(c_int) :: kind, error
        end function
        function native_pending(handle) bind(c, name='fo_change_native_pending') result(pending)
            import c_int, c_ptr
            type(c_ptr), value :: handle
            integer(c_int) :: pending
        end function
        subroutine native_close(handle) bind(c, name='fo_change_native_close')
            import c_ptr
            type(c_ptr), value :: handle
        end subroutine
        function fixture_setup(path, capacity) bind(c, name='wp_setup') result(ok)
            import c_int, c_char
            character(c_char) :: path(*)
            integer(c_int), value :: capacity
            integer(c_int) :: ok
        end function
        function fixture_write(path, contents) bind(c, name='wp_write') result(ok)
            import c_int, c_char
            character(c_char), intent(in) :: path(*), contents(*)
            integer(c_int) :: ok
        end function
        subroutine fixture_cleanup(path) bind(c, name='wp_cleanup')
            import c_char
            character(c_char), intent(in) :: path(*)
        end subroutine
    end interface
    character(kind=c_char, len=4096) :: root, file, event
    type(c_ptr) :: handle
    integer(c_int) :: error, kind, ok
    integer :: i, length
    logical :: observed, quiet
    ok = fixture_setup(root, int(len(root), c_int))
    if (ok /= 1) error stop 'native fixture setup failed'
    length = index(root, c_null_char) - 1
    file = root(:length)//'/input.f90'//c_null_char
    handle = native_open(error)
    if (error /= 0 .or. .not. c_associated(handle)) error stop 'native open ABI failed'
    error = native_root(handle, root)
    if (error /= 0) error stop 'native root ABI failed'
    error = native_reconcile(handle)
    if (error /= 0) error stop 'native reconcile ABI failed'
    ok = fixture_write(file, 'program from_fortran'//c_null_char)
    if (ok /= 1) error stop 'fixture file creation failed'
    observed = .false.
    do i = 1, 20
        event = ''
        error = native_poll(handle, 100_c_int, event, int(len(event), c_int), kind)
        if (error /= 0) error stop 'native poll ABI failed'
        if (kind == 2 .and. event == file) observed = .true.
        if (observed) exit
    end do
    if (.not. observed) error stop 'Fortran did not receive exact UTF8 create event'
    quiet = .false.
    do i = 1, 20
        event = ''
        error = native_poll(handle, 100_c_int, event, int(len(event), c_int), kind)
        if (error /= 0) error stop 'native drain ABI failed'
        if (kind == 0 .and. native_pending(handle) == 0) quiet = .true.
        if (quiet) exit
    end do
    if (.not. quiet) error stop 'Fortran provider failed to reach quiet'
    call native_close(handle)
    call fixture_cleanup(root)
    print '(a)', 'PASS native Windows Fortran ISO_C_BINDING events and quiescence'
end program
