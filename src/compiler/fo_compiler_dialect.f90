module fo_compiler_dialect
    implicit none
    private

    integer, parameter, public :: COMPILER_UNKNOWN = 0
    integer, parameter, public :: COMPILER_GFORTRAN = 1
    integer, parameter, public :: COMPILER_NVFORTRAN = 2
    integer, parameter, public :: COMPILER_IFX = 3
    integer, parameter, public :: COMPILER_FLANG = 4

    type, public :: compiler_dialect_t
        integer :: kind = COMPILER_UNKNOWN
        character(len=512) :: command = ''
    contains
        procedure, public :: base_flags => dialect_base_flags
        procedure, public :: module_flags => dialect_module_flags
        procedure, public :: profile_flags => dialect_profile_flags
        procedure, public :: translate_flag => dialect_translate_flag
        procedure, public :: openmp_flag => dialect_openmp_flag
        procedure, public :: supports_fuse_ld => dialect_supports_fuse_ld
        procedure, public :: is_flang => dialect_is_flang
    end type compiler_dialect_t

    public :: compiler_dialect, selected_compiler_command

contains

    pure function compiler_dialect(command) result(dialect)
        character(len=*), intent(in) :: command
        type(compiler_dialect_t) :: dialect
        character(len=len(command)) :: lowered

        lowered = lowercase(command)
        dialect%command = trim(command)
        if (index(lowered, 'nvfortran') > 0 .or. &
            index(lowered, 'nvidia') > 0 .or. index(lowered, 'pgfortran') > 0) then
            dialect%kind = COMPILER_NVFORTRAN
        else if (index(lowered, 'ifx') > 0) then
            dialect%kind = COMPILER_IFX
        else if (index(lowered, 'flang') > 0) then
            dialect%kind = COMPILER_FLANG
        else if (index(lowered, 'gfortran') > 0 .or. &
                index(lowered, 'gnu fortran') > 0) then
            dialect%kind = COMPILER_GFORTRAN
        end if
    end function compiler_dialect

    function selected_compiler_command() result(command)
        character(len=512) :: command
        integer :: status

        command = ''
        call get_environment_variable('FO_FC', command, status=status)
        if (status /= 0 .or. len_trim(command) == 0) then
            call get_environment_variable('FC', command, status=status)
        end if
        if (status /= 0 .or. len_trim(command) == 0) command = 'gfortran'
    end function selected_compiler_command

    function dialect_base_flags(self) result(flags)
        class(compiler_dialect_t), intent(in) :: self
        character(len=:), allocatable :: flags

        select case (self%kind)
        case (COMPILER_GFORTRAN)
            flags = '-ffree-line-length-none -fimplicit-none -pipe'
        case (COMPILER_NVFORTRAN)
            flags = '-Mfree -Mbackslash'
        case (COMPILER_IFX)
            flags = '-free'
        case (COMPILER_FLANG)
            flags = '-fimplicit-none'
        case default
            flags = ''
        end select
    end function dialect_base_flags

    function dialect_module_flags(self, directory) result(flags)
        class(compiler_dialect_t), intent(in) :: self
        character(len=*), intent(in) :: directory
        character(len=:), allocatable :: flags
        character(len=1), parameter :: separator = char(10)
        character(len=32) :: module_option

        select case (self%kind)
        case (COMPILER_GFORTRAN)
            module_option = '-J'
        case (COMPILER_NVFORTRAN, COMPILER_IFX)
            module_option = '-module'
        case (COMPILER_FLANG)
            module_option = '-module-dir'
        case default
            module_option = '-J'
        end select
        flags = trim(module_option)//separator//trim(directory)//separator// &
            '-I'//separator//trim(directory)
    end function dialect_module_flags

    function dialect_profile_flags(self, name) result(flags)
        class(compiler_dialect_t), intent(in) :: self
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: flags
        character(len=len(name)) :: lowered

        lowered = lowercase(name)
        select case (self%kind)
        case (COMPILER_GFORTRAN)
            select case (trim(lowered))
            case ('debug')
                flags = '-g -O0 -fcheck=all -fbacktrace'
            case ('release')
                flags = '-O3 -funroll-loops'
            case ('asan')
                flags = '-g -O0 -fcheck=all -fbacktrace '// &
                    '-fsanitize=address,undefined'
            case default
                flags = ''
            end select
        case (COMPILER_NVFORTRAN)
            select case (trim(lowered))
            case ('debug', 'asan')
                flags = '-g -O0 -Mbounds -Mchkptr -Mchkstk -traceback'
            case ('release')
                flags = '-O3'
            case default
                flags = ''
            end select
        case (COMPILER_IFX)
            select case (trim(lowered))
            case ('debug', 'asan')
                flags = '-g -O0 -check all -traceback'
            case ('release')
                flags = '-O3'
            case default
                flags = ''
            end select
        case (COMPILER_FLANG)
            select case (trim(lowered))
            case ('debug', 'asan')
                flags = '-g -O0'
            case ('release')
                flags = '-O3'
            case default
                flags = ''
            end select
        case default
            flags = ''
        end select
    end function dialect_profile_flags

    function dialect_translate_flag(self, flag) result(mapped)
        !! Translate fpm's GNU-shaped project flags into this compiler's
        !! spelling. Empty output means the compiler has no safe equivalent;
        !! source-level `implicit none` remains the portable correctness gate.
        class(compiler_dialect_t), intent(in) :: self
        character(len=*), intent(in) :: flag
        character(len=:), allocatable :: mapped

        mapped = trim(flag)
        select case (self%kind)
        case (COMPILER_NVFORTRAN)
            select case (trim(flag))
            case ('-ffree-form')
                mapped = '-Mfree'
            case ('-ffixed-form')
                mapped = '-Mnofree'
            case ('-fimplicit-none', '-Werror=implicit-interface')
                mapped = ''
            end select
        case (COMPILER_IFX)
            select case (trim(flag))
            case ('-ffree-form')
                mapped = '-free'
            case ('-ffixed-form')
                mapped = '-fixed'
            case ('-fimplicit-none')
                mapped = '-implicit-none'
            case ('-Werror=implicit-interface')
                mapped = ''
            end select
        case (COMPILER_FLANG)
            if (trim(flag) == '-Werror=implicit-interface') mapped = ''
        end select
    end function dialect_translate_flag

    function dialect_openmp_flag(self) result(flag)
        class(compiler_dialect_t), intent(in) :: self
        character(len=:), allocatable :: flag

        select case (self%kind)
        case (COMPILER_GFORTRAN, COMPILER_FLANG)
            flag = '-fopenmp'
        case (COMPILER_NVFORTRAN)
            flag = '-mp'
        case (COMPILER_IFX)
            flag = '-qopenmp'
        case default
            flag = ''
        end select
    end function dialect_openmp_flag

    pure logical function dialect_supports_fuse_ld(self)
        class(compiler_dialect_t), intent(in) :: self

        dialect_supports_fuse_ld = self%kind == COMPILER_GFORTRAN .or. &
            self%kind == COMPILER_FLANG
    end function dialect_supports_fuse_ld

    pure logical function dialect_is_flang(self)
        class(compiler_dialect_t), intent(in) :: self

        dialect_is_flang = self%kind == COMPILER_FLANG
    end function dialect_is_flang

    pure function lowercase(text) result(lowered)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lowered
        integer :: i, code

        lowered = text
        do i = 1, len(text)
            code = iachar(lowered(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                lowered(i:i) = achar(code + iachar('a') - iachar('A'))
            end if
        end do
    end function lowercase

end module fo_compiler_dialect
