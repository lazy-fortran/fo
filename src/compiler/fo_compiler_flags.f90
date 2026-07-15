module fo_compiler_flags
    implicit none
    private

    character(len=*), parameter, public :: ARRAY_TEMPORARY_WARNING_FLAG = &
        '-Warray-temporaries'

    public :: append_array_temporary_warning_flag, compiler_is_gfortran

contains

    pure logical function compiler_is_gfortran(compiler)
        character(len=*), intent(in) :: compiler
        character(len=len(compiler)) :: lowered
        integer :: i, code

        lowered = compiler
        do i = 1, len_trim(lowered)
            code = iachar(lowered(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) &
                lowered(i:i) = achar(code + iachar('a') - iachar('A'))
        end do
        compiler_is_gfortran = index(lowered, 'gfortran') > 0 .or. &
            index(lowered, 'gnu fortran') > 0
    end function compiler_is_gfortran

    pure subroutine append_array_temporary_warning_flag(compiler, flags)
        character(len=*), intent(in) :: compiler
        character(len=*), intent(inout) :: flags
        integer :: required

        if (.not. compiler_is_gfortran(compiler)) return
        if (index(flags, 'array-temporaries') > 0) return
        required = len_trim(flags) + len(ARRAY_TEMPORARY_WARNING_FLAG)
        if (len_trim(flags) > 0) required = required + 1
        if (required > len(flags)) return
        if (len_trim(flags) > 0) then
            flags = trim(flags)//' '//ARRAY_TEMPORARY_WARNING_FLAG
        else
            flags = ARRAY_TEMPORARY_WARNING_FLAG
        end if
    end subroutine append_array_temporary_warning_flag

end module fo_compiler_flags
