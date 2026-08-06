module fo_compiler_flags
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t, &
        COMPILER_GFORTRAN
    implicit none
    private

    character(len=*), parameter, public :: ARRAY_TEMPORARY_WARNING_FLAG = &
        '-Warray-temporaries'

    public :: append_array_temporary_warning_flag, append_pipe_flag
    public :: compiler_is_gfortran, compiler_is_nvfortran, compiler_is_ifx, &
        compiler_supports_fuse_ld

contains

    pure logical function compiler_is_gfortran(compiler)
        character(len=*), intent(in) :: compiler
        type(compiler_dialect_t) :: dialect

        dialect = compiler_dialect(compiler)
        compiler_is_gfortran = dialect%kind == COMPILER_GFORTRAN
    end function compiler_is_gfortran

    pure logical function compiler_is_nvfortran(compiler)
        use fo_compiler_dialect, only: COMPILER_NVFORTRAN
        character(len=*), intent(in) :: compiler
        type(compiler_dialect_t) :: dialect

        dialect = compiler_dialect(compiler)
        compiler_is_nvfortran = dialect%kind == COMPILER_NVFORTRAN
    end function compiler_is_nvfortran

    pure logical function compiler_is_ifx(compiler)
        use fo_compiler_dialect, only: COMPILER_IFX
        character(len=*), intent(in) :: compiler
        type(compiler_dialect_t) :: dialect

        dialect = compiler_dialect(compiler)
        compiler_is_ifx = dialect%kind == COMPILER_IFX
    end function compiler_is_ifx

    pure logical function compiler_supports_fuse_ld(compiler)
        character(len=*), intent(in) :: compiler
        type(compiler_dialect_t) :: dialect

        dialect = compiler_dialect(compiler)
        compiler_supports_fuse_ld = dialect%supports_fuse_ld()
    end function compiler_supports_fuse_ld

    pure subroutine append_pipe_flag(compiler, flags)
        character(len=*), intent(in) :: compiler
        character(len=*), intent(inout) :: flags
        integer :: required

        if (.not. compiler_is_gfortran(compiler)) return
        if (index(' '//trim(flags)//' ', ' -pipe ') > 0) return
        required = len_trim(flags) + len('-pipe')
        if (len_trim(flags) > 0) required = required + 1
        if (required > len(flags)) return
        if (len_trim(flags) > 0) then
            flags = trim(flags)//' -pipe'
        else
            flags = '-pipe'
        end if
    end subroutine append_pipe_flag

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
