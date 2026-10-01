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
        procedure, public :: debug_info_diet_flag => dialect_debug_info_diet_flag
        procedure, public :: debug_info_flag => dialect_debug_info_flag
        procedure, public :: pic_flag => dialect_pic_flag
        procedure, public :: sections_split_flags => dialect_sections_split_flags
        procedure, public :: requests_sanitizer => dialect_requests_sanitizer
        procedure, public :: gc_sections_link_flag => dialect_gc_sections_link_flag
        procedure, public :: requests_debug_info => dialect_requests_debug_info
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

    pure function dialect_debug_info_flag(self, kind) result(flag)
        !! The debug-info flag a manifest asks for: `g0`, `line-tables`, `full`.
        !! Empty kind means "the manifest has no opinion", so fo emits nothing
        !! and its own default profile decides.
        class(compiler_dialect_t), intent(in) :: self
        character(len=*), intent(in) :: kind
        character(len=32) :: flag

        select case (trim(kind))
        case ('g0')
            flag = '-g0'
        case ('line-tables')
            flag = '-gline-tables-only'
        case ('full')
            flag = '-g'
        case default
            flag = ''
        end select
    end function dialect_debug_info_flag

    pure function dialect_pic_flag(self, on) result(flag)
        !! `-fPIC` when the project asks for position-independent code, empty
        !! otherwise. This is what makes a shared `libffc` possible at all:
        !! without it the linker refuses the archive with
        !! "relocation R_X86_64_PC32 against symbol ... can not be used when
        !! making a shared object; recompile with -fPIC". Measured cost on a
        !! unit compile here is nil (0.1089 s vs 0.1092 s), while it removes
        !! the need to relink a ~31 MB static archive into every one of ~500
        !! test executables (8.2 GB of `build/`).
        class(compiler_dialect_t), intent(in) :: self
        logical, intent(in) :: on
        character(len=:), allocatable :: flag

        if (.not. on) return
        select case (self%kind)
        case (COMPILER_GFORTRAN, COMPILER_FLANG)
            flag = '-fPIC'
        case default
            flag = ''
        end select
    end function dialect_pic_flag

    pure function dialect_debug_info_diet_flag(self, request_flags) result(flag)
        !! The debug-info flag the default build profile should add so the
        !! compiler emits no DWARF, or empty when it must not.
        !!
        !! A 500-binary test tree pays for full debug info in every link even
        !! though nobody attaches a debugger to a test run: measured here, the
        !! default `fo build` of ffc carried 0.74 GB of `.debug_*` sections
        !! across 9.66 GB of executables.  The default profile therefore asks
        !! for `-g0`, and `--debug`/`--asan` keep their own `-g`, which the
        !! caller passes in request_flags and which must not be overwritten.
        class(compiler_dialect_t), intent(in) :: self
        character(len=*), intent(in) :: request_flags
        character(len=:), allocatable :: flag

        if (self%requests_debug_info(request_flags)) return
        select case (self%kind)
        case (COMPILER_GFORTRAN, COMPILER_FLANG)
            flag = '-g0'
        case default
            flag = ''
        end select
    end function dialect_debug_info_diet_flag

    pure function dialect_sections_split_flags(self, request_flags) result(flags)
        !! One section per function and per global, so the link step can drop
        !! everything a given executable never reaches.
        !!
        !! Every one of ffc's 503 executables links the whole 55 MB library
        !! archive today: 9.66 GB of binaries whose median size is 20.9 MB,
        !! because a test of `ubound` still carries every lowering in the tree.
        !! A sanitizer build must not split: its instrumentation lives in
        !! constructor tables that section garbage collection can collect, and a
        !! sanitizer that silently collects nothing is worse than a big binary.
        class(compiler_dialect_t), intent(in) :: self
        character(len=*), intent(in) :: request_flags
        character(len=:), allocatable :: flags

        if (self%requests_sanitizer(request_flags)) return
        select case (self%kind)
        case (COMPILER_GFORTRAN, COMPILER_FLANG)
            flags = '-ffunction-sections -fdata-sections'
        case default
            flags = ''
        end select
    end function dialect_sections_split_flags

    pure logical function dialect_requests_sanitizer(self, request_flags)
        !! True when the request asked for a sanitizer, by `-fsanitize=...`
        !! or by the `asan` profile spelling of it.
        class(compiler_dialect_t), intent(in) :: self
        character(len=*), intent(in) :: request_flags

        dialect_requests_sanitizer = index(request_flags, '-fsanitize') > 0
    end function dialect_requests_sanitizer

    pure function dialect_gc_sections_link_flag(self, request_flags) result(flag)
        !! The link half of `dialect_sections_split_flags`. Empty when the
        !! compile step did not split, so the two never disagree.
        class(compiler_dialect_t), intent(in) :: self
        character(len=*), intent(in) :: request_flags
        character(len=:), allocatable :: flag

        if (len_trim(self%sections_split_flags(request_flags)) == 0) return
        select case (self%kind)
        case (COMPILER_GFORTRAN, COMPILER_FLANG)
            flag = '-Wl,--gc-sections'
        case default
            flag = ''
        end select
    end function dialect_gc_sections_link_flag

    pure logical function dialect_requests_debug_info(self, request_flags)
        !! True when the caller already asked for debug info, either with a
        !! profile flag (`--debug`, `--asan`) or directly (`-g`, `-g1`,
        !! `-gline-tables-only`, `-ggdb`).  `-g0` is a request for none.
        class(compiler_dialect_t), intent(in) :: self
        character(len=*), intent(in) :: request_flags

        character(len=512) :: rest
        integer :: pos

        dialect_requests_debug_info = .false.
        rest = adjustl(request_flags)
        do while (len_trim(rest) > 0)
            pos = index(rest, ' ')
            if (pos == 0) pos = len_trim(rest) + 1
            if (rest(1:2) == '-g' .and. rest(1:min(pos - 1, 3)) /= '-g0') then
                dialect_requests_debug_info = .true.
                return
            end if
            rest = adjustl(rest(pos:))
        end do
    end function dialect_requests_debug_info

    function dialect_base_flags(self) result(flags)
        class(compiler_dialect_t), intent(in) :: self
        character(len=:), allocatable :: flags

        select case (self%kind)
        case (COMPILER_GFORTRAN)
            flags = '-ffree-line-length-none -fimplicit-none -cpp -pipe'
        case (COMPILER_NVFORTRAN)
            flags = '-Mfree -Mbackslash -Mpreprocess'
        case (COMPILER_IFX)
            flags = '-free -fpp'
        case (COMPILER_FLANG)
            flags = '-fimplicit-none -cpp'
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
            case ('-fimplicit-none', '-fno-implicit-none', &
                    '-Werror=implicit-interface')
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
            case ('-fno-implicit-none')
                ! ifx allows implicit typing unless -implicit-none is given,
                ! and its baseline does not set it, so nothing to undo.
                mapped = ''
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
