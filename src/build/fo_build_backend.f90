module fo_build_backend
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private
    public :: backend_t, detect_backend, BACKEND_FPM, BACKEND_CMAKE, BACKEND_NONE

    integer, parameter :: BACKEND_NONE = 0
    integer, parameter :: BACKEND_FPM = 1
    integer, parameter :: BACKEND_CMAKE = 2

    type :: backend_t
        integer :: kind = BACKEND_NONE
        character(len=512) :: project_dir = '.'
    contains
        procedure :: build => backend_build
        procedure :: test => backend_test
    end type backend_t

contains

    function detect_backend(dir) result(b)
        character(len=*), intent(in) :: dir
        type(backend_t) :: b
        logical :: has_fpm, has_cmake

        b%project_dir = dir

        inquire(file=trim(dir)//'/fpm.toml', exist=has_fpm)
        inquire(file=trim(dir)//'/CMakeLists.txt', exist=has_cmake)

        if (has_fpm) then
            b%kind = BACKEND_FPM
        else if (has_cmake) then
            b%kind = BACKEND_CMAKE
        else
            b%kind = BACKEND_NONE
        end if
    end function detect_backend

    subroutine backend_build(self, exitcode, flags)
        class(backend_t), intent(in) :: self
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: flags

        integer :: cmdstat
        character(len=2048) :: cmd

        select case (self%kind)
        case (BACKEND_FPM)
            if (present(flags) .and. len_trim(flags) > 0) then
                cmd = 'cd '//trim(self%project_dir)// &
                    ' && fpm build --flag "'//trim(flags)//'" 2>&1'
            else
                cmd = 'cd '//trim(self%project_dir)//' && fpm build 2>&1'
            end if
        case (BACKEND_CMAKE)
            if (present(flags) .and. len_trim(flags) > 0) then
                cmd = 'cd '//trim(self%project_dir)// &
                    ' && cmake -S . -B build -G Ninja'// &
                    ' -DCMAKE_Fortran_FLAGS="'//trim(flags)//'"'// &
                    ' 2>&1 && cmake --build build 2>&1'
            else
                cmd = 'cd '//trim(self%project_dir)// &
                    ' && cmake -S . -B build -G Ninja 2>&1'// &
                    ' && cmake --build build 2>&1'
            end if
        case default
            write(error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            return
        end select

        call execute_command_line(cmd, exitstat=exitcode, cmdstat=cmdstat, wait=.true.)
        if (cmdstat /= 0) exitcode = 1
    end subroutine backend_build

    subroutine backend_test(self, exitcode)
        class(backend_t), intent(in) :: self
        integer, intent(out) :: exitcode

        integer :: cmdstat
        character(len=2048) :: cmd

        select case (self%kind)
        case (BACKEND_FPM)
            cmd = 'cd '//trim(self%project_dir)//' && fpm test 2>&1'
        case (BACKEND_CMAKE)
            cmd = 'cd '//trim(self%project_dir)// &
                ' && cd build && ctest --output-on-failure 2>&1'
        case default
            write(error_unit, '(a)') 'fo: no build backend detected'
            exitcode = 1
            return
        end select

        call execute_command_line(cmd, exitstat=exitcode, cmdstat=cmdstat, wait=.true.)
        if (cmdstat /= 0) exitcode = 1
    end subroutine backend_test

end module fo_build_backend
