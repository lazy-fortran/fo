program fo_test_fpm_command
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_loc, &
        c_null_char, c_null_ptr
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    interface
        function c_execv(path, arguments) bind(C, name='execv') result(code)
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            type(c_ptr), intent(in) :: arguments(*)
            integer(c_int) :: code
        end function c_execv

        subroutine c_exit(code) bind(C, name='exit')
            import :: c_int
            integer(c_int), value :: code
        end subroutine c_exit
    end interface

    character(len=4096) :: executable, call_log, argument
    character(kind=c_char), allocatable, target :: argument_bytes(:, :)
    type(c_ptr), allocatable, target :: arguments(:)
    integer :: argument_count, argument_length, i, j, status, unit
    integer(c_int) :: exec_status

    call_log = ''
    call get_environment_variable('FO_TEST_FPM_CALL_LOG', call_log, status=status)
    if (status /= 0 .or. len_trim(call_log) == 0) then
        write (error_unit, '(a)') 'FPM command fixture has no call log'
        flush (error_unit)
        call c_exit(91_c_int)
    end if
    executable = ''
    call get_environment_variable('FO_TEST_REAL_FPM', executable, status=status)
    if (status /= 0 .or. len_trim(executable) == 0) then
        write (error_unit, '(a)') 'FPM command fixture has no real executable'
        flush (error_unit)
        call c_exit(92_c_int)
    end if

    argument_count = command_argument_count()
    open (newunit=unit, file=trim(call_log), status='unknown', &
        position='append', action='write', iostat=status)
    if (status /= 0) call c_exit(93_c_int)
    do i = 1, argument_count
        call get_command_argument(i, argument, status=status)
        if (status /= 0) call c_exit(94_c_int)
        if (i > 1) write (unit, '(a)', advance='no') ' '
        write (unit, '(a)', advance='no') trim(argument)
    end do
    write (unit, '(a)') ''
    close (unit)

    allocate (argument_bytes(4097, argument_count + 1))
    allocate (arguments(argument_count + 2))
    argument_bytes = c_null_char
    do i = 1, argument_count + 1
        if (i == 1) then
            argument = trim(executable)
        else
            call get_command_argument(i - 1, argument, status=status)
            if (status /= 0) call c_exit(95_c_int)
        end if
        argument_length = len_trim(argument)
        do j = 1, argument_length
            argument_bytes(j, i) = achar(iachar(argument(j:j)), kind=c_char)
        end do
        arguments(i) = c_loc(argument_bytes(1, i))
    end do
    arguments(argument_count + 2) = c_null_ptr
    exec_status = c_execv(argument_bytes(:, 1), arguments)
    if (exec_status == 0_c_int) call c_exit(0_c_int)
    write (error_unit, '(a)') 'FPM command fixture could not exec fpm'
    flush (error_unit)
    call c_exit(96_c_int)
end program fo_test_fpm_command
