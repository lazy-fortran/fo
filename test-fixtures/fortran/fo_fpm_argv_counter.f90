program fo_fpm_argv_counter
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

    character(len=4096) :: real_fpm, log_path, argument
    character(kind=c_char), allocatable, target :: argument_bytes(:,:)
    type(c_ptr), allocatable :: arguments(:)
    integer :: argument_count, argument_length, i, j, unit, status

    call get_environment_variable('FO_TEST_REAL_FPM', real_fpm, status=status)
    if (status /= 0 .or. len_trim(real_fpm) == 0) then
        write (error_unit, '(a)') 'fpm fixture has no pinned executable'
        call c_exit(87_c_int)
    end if
    call get_environment_variable('FO_TEST_FPM_LOG', log_path, status=status)
    if (status /= 0 .or. len_trim(log_path) == 0) then
        write (error_unit, '(a)') 'fpm fixture has no argv log path'
        call c_exit(88_c_int)
    end if

    argument_count = command_argument_count()
    allocate (argument_bytes(4097, argument_count + 1))
    allocate (arguments(argument_count + 2))
    argument_bytes = c_null_char
    argument = trim(real_fpm)
    call store_argument(argument, 1)
    do i = 1, argument_count
        call get_command_argument(i, argument, status=status)
        if (status /= 0) call c_exit(89_c_int)
        call store_argument(argument, i + 1)
    end do
    arguments(argument_count + 2) = c_null_ptr

    open (newunit=unit, file=trim(log_path), status='unknown', &
        position='append', action='write', iostat=status)
    if (status /= 0) call c_exit(90_c_int)
    write (unit, '(a)', advance='no') 'fpm'
    do i = 1, argument_count
        call get_command_argument(i, argument)
        write (unit, '(1x,a)', advance='no') trim(argument)
    end do
    write (unit, '()')
    close (unit)

    if (c_execv(trim(real_fpm)//c_null_char, arguments) == 0_c_int) &
        call c_exit(0_c_int)
    write (error_unit, '(a)') 'fpm fixture could not exec its pinned binary'
    call c_exit(91_c_int)

contains

    subroutine store_argument(value, index)
        character(len=*), intent(in) :: value
        integer, intent(in) :: index
        integer :: k

        argument_length = len_trim(value)
        do k = 1, argument_length
            argument_bytes(k, index) = achar(iachar(value(k:k)), kind=c_char)
        end do
        arguments(index) = c_loc(argument_bytes(1, index))
    end subroutine store_argument

end program fo_fpm_argv_counter
