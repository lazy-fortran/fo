program test_process_boundary
    use, intrinsic :: iso_c_binding, only: c_int, c_funptr, c_funloc, c_associated
    use fo_boundary_signal, only: ignore_term
    implicit none
    interface
        function c_signal(number, handler) bind(C, name='signal') result(old_handler)
            import :: c_int, c_funptr
            integer(c_int), value :: number
            type(c_funptr), value :: handler
            type(c_funptr) :: old_handler
        end function c_signal
        integer(c_int) function c_fork() bind(C, name='fork')
            import :: c_int
        end function c_fork
        integer(c_int) function c_getpid() bind(C, name='getpid')
            import :: c_int
        end function c_getpid
        integer(c_int) function c_setpgid(pid, group) bind(C, name='setpgid')
            import :: c_int
            integer(c_int), value :: pid, group
        end function c_setpgid
        integer(c_int) function c_usleep(usec) bind(C, name='usleep')
            import :: c_int
            integer(c_int), value :: usec
        end function c_usleep
    end interface

    integer(c_int) :: child, grandchild, ignored
    character(len=4096) :: prefix
    type(c_funptr) :: old_handler

    call get_environment_variable('FO_PROCESS_BOUNDARY_PREFIX', prefix)
    if (len_trim(prefix) == 0) stop
    old_handler = c_signal(15_c_int, c_funloc(ignore_term))
    if (c_associated(old_handler)) stop 2
    call write_pid(trim(prefix), 'owner')
    child = c_fork()
    if (child < 0_c_int) stop 3
    if (child == 0_c_int) then
        ignored = c_setpgid(0_c_int, 0_c_int)
        old_handler = c_signal(15_c_int, c_funloc(ignore_term))
        if (.not. c_associated(old_handler)) stop 6
        call write_pid(trim(prefix), 'nested')
        grandchild = c_fork()
        if (grandchild < 0_c_int) stop 4
        if (grandchild == 0_c_int) then
            ignored = c_setpgid(0_c_int, 0_c_int)
            old_handler = c_signal(15_c_int, c_funloc(ignore_term))
            if (.not. c_associated(old_handler)) stop 7
            call write_pid(trim(prefix), 'deep')
            call heartbeat(trim(prefix), 'deep')
        end if
        call wait_for_file(trim(prefix)//'.deep.pid')
        call heartbeat(trim(prefix), 'nested')
    end if
    call wait_for_file(trim(prefix)//'.nested.pid')
    call wait_for_file(trim(prefix)//'.deep.pid')
    call heartbeat(trim(prefix), 'owner')
contains
    subroutine write_pid(target, role)
        character(len=*), intent(in) :: target, role
        integer :: unit
        open(newunit=unit, file=target//'.'//role//'.pid', status='replace')
        write (unit, '(i0)') c_getpid()
        close(unit)
    end subroutine write_pid

    subroutine wait_for_file(path)
        character(len=*), intent(in) :: path
        integer :: attempt
        logical :: exists
        do attempt = 1, 1000
            inquire(file=path, exist=exists)
            if (exists) return
            ignored = c_usleep(10000_c_int)
        end do
        stop 5
    end subroutine wait_for_file

    subroutine heartbeat(target, role)
        character(len=*), intent(in) :: target, role
        integer :: unit
        do
            open(newunit=unit, file=target//'.'//role//'.heartbeat', &
                status='unknown', position='append', action='write')
            write (unit, '(a)') 'alive'
            close(unit)
            ignored = c_usleep(20000_c_int)
        end do
    end subroutine heartbeat
end program test_process_boundary

subroutine ignore_term(signal_number) bind(C)
    use, intrinsic :: iso_c_binding, only: c_int
    integer(c_int), value :: signal_number
    if (signal_number == 0_c_int) return
end subroutine ignore_term
