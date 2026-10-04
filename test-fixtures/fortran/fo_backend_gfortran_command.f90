program fo_backend_gfortran_command
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

        function c_setenv(name, value, overwrite) bind(C, name='setenv') &
                result(code)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: code
        end function c_setenv

        subroutine c_exit(code) bind(C, name='exit')
            import :: c_int
            integer(c_int), value :: code
        end subroutine c_exit
    end interface

    character(len=4096) :: executable, real_compiler, marker, argument
    character(len=32) :: depth
    character(kind=c_char), allocatable, target :: argument_bytes(:,:)
    type(c_ptr), allocatable :: arguments(:)
    integer :: argument_count, argument_length, i, j, marker_status, status
    integer :: marker_unit
    integer(c_int) :: c_status

    call get_command_argument(0, executable)
    if (fixture_name(executable) == 'ld.lld') then
        write (error_unit, '(a)') 'fixture linker rejected the LLD attempt'
        flush (error_unit)
        call c_exit(1_c_int)
    end if

    marker = ''
    call get_environment_variable('FO_TEST_BACKEND_LLD_MARKER', marker, &
        status=marker_status)
    if (marker_status == 0 .and. len_trim(marker) > 0) then
        argument_count = command_argument_count()
        do i = 1, argument_count
            call get_command_argument(i, argument)
            if (trim(argument) /= '-fuse-ld=lld') cycle
            open (newunit=marker_unit, file=trim(marker), status='replace', &
                iostat=status)
            if (status /= 0) call c_exit(91_c_int)
            write (marker_unit, '(a)') 'lld selected'
            close (marker_unit)
            call c_exit(1_c_int)
        end do
    end if

    call get_environment_variable('FO_TEST_BACKEND_REAL_COMPILER', &
        real_compiler, status=status)
    if (status /= 0 .or. len_trim(real_compiler) == 0) then
        write (error_unit, '(a)') 'fixture has no real compiler path'
        flush (error_unit)
        call c_exit(87_c_int)
    end if

    call get_environment_variable('FO_TEST_BACKEND_WRAPPER_DEPTH', depth, &
        status=status)
    if (status == 0) then
        write (error_unit, '(a)') 'compiler fixture recursion rejected'
        flush (error_unit)
        call c_exit(86_c_int)
    end if

    argument_count = command_argument_count()
    allocate (argument_bytes(4097, argument_count + 1))
    allocate (arguments(argument_count + 2))
    argument_bytes = c_null_char
    do i = 1, argument_count + 1
        if (i == 1) then
            argument = trim(real_compiler)
        else
            call get_command_argument(i - 1, argument, status=status)
            if (status /= 0) call c_exit(88_c_int)
        end if
        argument_length = len_trim(argument)
        do j = 1, argument_length
            argument_bytes(j, i) = achar(iachar(argument(j:j)), kind=c_char)
        end do
        arguments(i) = c_loc(argument_bytes(1, i))
    end do
    arguments(argument_count + 2) = c_null_ptr

    c_status = c_setenv('FO_TEST_BACKEND_WRAPPER_DEPTH'//c_null_char, &
        '1'//c_null_char, 1_c_int)
    if (c_status /= 0_c_int) call c_exit(89_c_int)
    c_status = c_execv(trim(real_compiler)//c_null_char, arguments)
    write (error_unit, '(a)') 'fixture could not exec the real compiler'
    flush (error_unit)
    call c_exit(90_c_int)

contains

    function fixture_name(path) result(name)
        character(len=*), intent(in) :: path
        character(len=len(path)) :: name
        integer :: i, separator, path_length

        name = ''
        path_length = len_trim(path)
        if (path_length == 0) return
        separator = 0
        do i = path_length, 1, -1
            if (path(i:i) /= '/') cycle
            separator = i
            exit
        end do
        name = path(separator + 1:path_length)
    end function fixture_name

end program fo_backend_gfortran_command
