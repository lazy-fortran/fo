program test_compiler_dialect
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t, &
        COMPILER_GFORTRAN, COMPILER_NVFORTRAN, COMPILER_IFX, COMPILER_FLANG
    use fo_util, only: make_tmpfile, delete_tmpfile
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0
    call test_gfortran_policy()
    call test_nvfortran_policy()
    call test_nvfortran_error_stop_sources()
    call test_ifx_policy()
    call test_flang_policy()
    call report()

contains

    subroutine check(condition, name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: name

        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//trim(name)
        end if
    end subroutine check

    subroutine test_gfortran_policy()
        type(compiler_dialect_t) :: dialect

        dialect = compiler_dialect('/usr/bin/gfortran')
        call check(dialect%kind == COMPILER_GFORTRAN, 'gfortran is identified')
        call check(index(dialect%base_flags(), '-fimplicit-none') > 0, &
            'gfortran keeps implicit-none')
        call check(index(dialect%base_flags(), '-cpp') > 0, &
            'gfortran preprocesses lowercase Fortran sources')
        call check(index(dialect%module_flags('/tmp/mod'), '-J') > 0, &
            'gfortran uses -J for modules')
        call check(dialect%supports_fuse_ld(), 'gfortran supports fuse-ld')
    end subroutine test_gfortran_policy

    subroutine test_nvfortran_policy()
        type(compiler_dialect_t) :: dialect
        character(len=:), allocatable :: flags

        dialect = compiler_dialect('/opt/nvidia/bin/nvfortran')
        call check(dialect%kind == COMPILER_NVFORTRAN, 'nvfortran is identified')
        call check(index(dialect%base_flags(), '-Mfree') > 0, &
            'nvfortran uses the free-form flag')
        call check(index(dialect%base_flags(), '-Mbackslash') > 0, &
            'nvfortran preserves backslashes in character literals')
        call check(index(dialect%base_flags(), '-Mpreprocess') > 0, &
            'nvfortran preprocesses lowercase Fortran sources')
        call check(trim(dialect%translate_flag('-ffree-form')) == '-Mfree', &
            'nvfortran translates fpm free-form flags')
        call check(len_trim(dialect%translate_flag('-fimplicit-none')) == 0, &
            'nvfortran does not receive an unsupported implicit-none flag')
        call check(index(dialect%module_flags('/tmp/mod'), '-module') > 0, &
            'nvfortran uses -module for modules')
        call check(index(dialect%module_flags('/tmp/mod'), '-J') == 0, &
            'nvfortran does not receive the GNU module flag')
        call check(trim(dialect%openmp_flag()) == '-mp', &
            'nvfortran uses -mp for OpenMP')
        flags = dialect%profile_flags('debug')
        call check(index(flags, '-Mbounds') > 0, &
            'nvfortran debug enables bounds checking')
        call check(.not. dialect%supports_fuse_ld(), &
            'nvfortran avoids GNU linker flags')
    end subroutine test_nvfortran_policy

    subroutine test_nvfortran_error_stop_sources()
        !! Compile the real sources containing variable ERROR STOP codes with
        !! nvfortran when that compiler is the active test lane.  This is an
        !! independent compiler oracle for the 26.5 EXIT-token regression.
        type(compiler_dialect_t) :: dialect
        character(len=512) :: compiler, object_path, command
        character(len=256) :: source
        integer :: i, status, exitcode

        call get_environment_variable('FO_FC', compiler, status=status)
        if (status /= 0 .or. len_trim(compiler) == 0) return
        dialect = compiler_dialect(trim(compiler))
        if (dialect%kind /= COMPILER_NVFORTRAN) return

        do i = 1, 2
            call make_tmpfile('fo_nvfortran_error_stop', object_path)
            if (i == 1) then
                source = 'src/cover/fo_cover.f90'
            else
                source = 'src/build/fo_ffc_cli.f90'
            end if
            command = trim(compiler)//' -Mfree -Mbackslash -Mpreprocess '// &
                '-module build/fo/mod -Ibuild/fo/mod -c '//trim(source)// &
                ' -o '//trim(object_path)
            call execute_command_line(trim(command), exitstat=exitcode)
            call check(exitcode == 0, 'nvfortran compiles '//trim(source))
            call delete_tmpfile(object_path)
        end do
    end subroutine test_nvfortran_error_stop_sources

    subroutine test_ifx_policy()
        type(compiler_dialect_t) :: dialect

        dialect = compiler_dialect('ifx')
        call check(dialect%kind == COMPILER_IFX, 'ifx is identified')
        call check(index(dialect%base_flags(), '-free') > 0, &
            'ifx uses the free-form flag')
        call check(index(dialect%base_flags(), '-fpp') > 0, &
            'ifx preprocesses lowercase Fortran sources')
        call check(trim(dialect%translate_flag('-ffree-form')) == '-free', &
            'ifx translates fpm free-form flags')
        call check(index(dialect%module_flags('/tmp/mod'), '-module') > 0, &
            'ifx uses -module for modules')
        call check(trim(dialect%openmp_flag()) == '-qopenmp', &
            'ifx uses -qopenmp for OpenMP')
        dialect = compiler_dialect('ifort')
        call check(dialect%kind /= COMPILER_IFX, &
            'legacy ifort is not treated as ifx')
    end subroutine test_ifx_policy

    subroutine test_flang_policy()
        type(compiler_dialect_t) :: dialect

        dialect = compiler_dialect('flang-new')
        call check(dialect%kind == COMPILER_FLANG, 'flang is identified')
        call check(index(dialect%base_flags(), '-cpp') > 0, &
            'flang preprocesses lowercase Fortran sources')
        call check(index(dialect%module_flags('/tmp/mod'), '-module-dir') > 0, &
            'flang uses -module-dir for modules')
        call check(trim(dialect%openmp_flag()) == '-fopenmp', &
            'flang uses -fopenmp for OpenMP')
        call check(dialect%is_flang(), 'flang predicate is true')
    end subroutine test_flang_policy

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'compiler-dialect: pass=', n_pass, &
            ' fail=', n_fail
        if (n_fail > 0) stop 1
    end subroutine report

end program test_compiler_dialect
