program test_debug_info_diet
    !! The default build profile emits no DWARF; a caller that asked for debug
    !! info keeps it. Measured on ffc, 503 default-profile executables carried
    !! 0.74 GB of `.debug_*` sections that no test run ever reads.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t, &
        COMPILER_GFORTRAN, COMPILER_FLANG, COMPILER_NVFORTRAN
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_default_diet()
    call test_requested_debug_survives()
    call test_other_compilers()
    call test_manifest_budget()
    call report()

contains

    subroutine check(cond, name)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//name
        end if
    end subroutine check

    subroutine test_default_diet()
        type(compiler_dialect_t) :: d

        d = compiler_dialect('gfortran')
        call check(trim(d%debug_info_diet_flag('')) == '-g0', &
            'gfortran default build asks for -g0')
        call check(trim(d%debug_info_diet_flag('-O3 -funroll-loops')) == '-g0', &
            'release profile still gets the diet')
    end subroutine test_default_diet

    subroutine test_requested_debug_survives()
        type(compiler_dialect_t) :: d

        d = compiler_dialect('gfortran')
        call check(.not. d%requests_debug_info('-g0'), '-g0 is not debug info')
        call check(d%requests_debug_info('-g -O0 -fcheck=all -fbacktrace'), &
            'debug profile requests debug info')
        call check(d%requests_debug_info('-g'), 'bare -g requests debug info')
        call check(d%requests_debug_info('-gline-tables-only'), &
            '-gline-tables-only requests debug info')
        call check(d%requests_debug_info('-ggdb3'), '-ggdb3 requests debug info')
        call check(len_trim(d%debug_info_diet_flag( &
            '-g -O0 -fcheck=all -fbacktrace')) == 0, &
            'debug profile keeps its own -g, no -g0 appended')
        call check(len_trim(d%debug_info_diet_flag('-gline-tables-only')) == 0, &
            'explicit line-tables request is not overwritten')
        call check(trim(d%debug_info_diet_flag('-g0')) == '-g0', &
            'explicit -g0 request is idempotent')
        call check(index(d%base_flags(), '-g') == 0, &
            'base flags carry no debug flag of their own')
    end subroutine test_requested_debug_survives

    subroutine test_other_compilers()
        type(compiler_dialect_t) :: d

        d = compiler_dialect('flang-new')
        call check(d%kind == COMPILER_FLANG, 'flang detected')
        call check(trim(d%debug_info_diet_flag('')) == '-g0', &
            'flang default build asks for -g0')
        d = compiler_dialect('nvfortran')
        call check(d%kind == COMPILER_NVFORTRAN, 'nvfortran detected')
        call check(len_trim(d%debug_info_diet_flag('')) == 0, &
            'nvfortran default emits no debug info already')
        call check(.not. d%requests_debug_info(''), 'empty request on nvfortran')
        call check(COMPILER_GFORTRAN == 1, 'gfortran kind stable')
    end subroutine test_other_compilers


    subroutine test_manifest_budget()
        !! A project declares its debug-info budget in fpm.toml; the dialect
        !! maps it to the flag, and an unset budget emits nothing so fo's
        !! default still decides.
        type(compiler_dialect_t) :: d
        type(fpm_config_t) :: config
        integer :: unit, ios

        d = compiler_dialect('gfortran')
        call check(trim(d%debug_info_flag('g0')) == '-g0', 'g0 maps to -g0')
        call check(trim(d%debug_info_flag('line-tables')) == '-gline-tables-only', &
            'line-tables maps to -gline-tables-only')
        call check(trim(d%debug_info_flag('full')) == '-g', 'full maps to -g')
        call check(len_trim(d%debug_info_flag('')) == 0, 'no budget, no flag')

        call write_manifest('/var/tmp/ffc-goal/diet-g0/fpm.toml', 'g0')
        call fpm_config_parse('/var/tmp/ffc-goal/diet-g0', config, ios)
        call check(ios == 0, 'manifest with debug-info parses')
        call check(trim(config%debug_info) == 'g0', 'manifest budget is g0')

        call write_manifest('/var/tmp/ffc-goal/diet-bad/fpm.toml', 'sideways')
        call fpm_config_parse('/var/tmp/ffc-goal/diet-bad', config, ios)
        call check(ios == 0, 'manifest with a bad budget still parses')
        call check(len_trim(config%debug_info) == 0, 'bad budget is ignored')
    end subroutine test_manifest_budget

    subroutine write_manifest(path, budget)
        character(len=*), intent(in) :: path, budget

        integer :: unit, cut
        character(len=512) :: dir

        cut = index(path(1:len_trim(path)), '/fpm.toml')
        dir = path(1:cut - 1)
        call execute_command_line('mkdir -p '//trim(dir))
        open (newunit=unit, file=trim(path), action='write', status='replace')
        write (unit, '(a)') 'name = "diet"'
        write (unit, '(a)') ''
        write (unit, '(a)') '[extra.fo]'
        write (unit, '(a)') 'debug-info = "'//trim(budget)//'"'
        close (unit)
    end subroutine write_manifest

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'debug-info diet: pass=', n_pass, &
            ' fail=', n_fail
        if (n_fail > 0) stop 1
    end subroutine report

end program test_debug_info_diet