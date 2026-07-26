module fo_linker_policy
    use fo_compiler_flags, only: compiler_supports_fuse_ld
    use fo_fs, only: fs_find_executable
    implicit none
    private

    public :: linker_should_try_lld, select_linker

contains

    pure logical function linker_should_try_lld(compiler, lld_available, preference)
        character(len=*), intent(in) :: compiler, preference
        logical, intent(in) :: lld_available
        character(len=len(preference)) :: lowered
        integer :: i, code

        lowered = preference
        do i = 1, len_trim(lowered)
            code = iachar(lowered(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) &
                lowered(i:i) = achar(code + iachar('a') - iachar('A'))
        end do
        linker_should_try_lld = .false.
        if (.not. lld_available) return
        if (.not. compiler_supports_fuse_ld(compiler)) return
        if (trim(lowered) == 'default' .or. trim(lowered) == 'ld') return
        linker_should_try_lld = len_trim(lowered) == 0 .or. &
            trim(lowered) == 'auto' .or. trim(lowered) == 'lld'
    end function linker_should_try_lld

    subroutine select_linker(compiler, use_lld)
        character(len=*), intent(in) :: compiler
        logical, intent(out) :: use_lld

        character(len=512) :: lld_path
        character(len=32) :: preference
        integer :: status
        logical :: available

        preference = ''
        call get_environment_variable('FO_LINKER', preference, status=status)
        if (status /= 0) preference = ''
        call fs_find_executable('ld.lld', lld_path, available)
        use_lld = linker_should_try_lld(compiler, available, preference)
    end subroutine select_linker

end module fo_linker_policy
