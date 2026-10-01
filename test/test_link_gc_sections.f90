program test_link_gc_sections
    !! Compile-side section splitting and link-side garbage collection are one
    !! decision: a split without `--gc-sections` costs size and buys nothing,
    !! and a sanitizer whose instrumentation tables get collected reports nothing.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_default_splits_and_collects()
    call test_sanitizer_keeps_everything()
    call test_pair_never_disagrees()
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

    subroutine test_default_splits_and_collects()
        type(compiler_dialect_t) :: d

        d = compiler_dialect('gfortran')
        call check(trim(d%sections_split_flags('')) == &
            '-ffunction-sections -fdata-sections', 'gfortran splits sections')
        call check(trim(d%gc_sections_link_flag('')) == '-Wl,--gc-sections', &
            'gfortran links with --gc-sections')
    end subroutine test_default_splits_and_collects

    subroutine test_sanitizer_keeps_everything()
        type(compiler_dialect_t) :: d

        d = compiler_dialect('gfortran')
        call check(d%requests_sanitizer('-fsanitize=address,undefined'), &
            'asan request detected')
        call check(.not. d%requests_sanitizer('-O2'), 'plain -O2 is no sanitizer')
        call check(len_trim(d%sections_split_flags('-fsanitize=address')) == 0, &
            'asan build does not split sections')
        call check(len_trim(d%gc_sections_link_flag('-fsanitize=address')) == 0, &
            'asan build does not collect sections')
        call check(len_trim(d%gc_sections_link_flag(&
            '-g -O0 -fcheck=all -fbacktrace -fsanitize=address,undefined')) == 0, &
            'full asan profile collects nothing')
    end subroutine test_sanitizer_keeps_everything

    subroutine test_pair_never_disagrees()
        type(compiler_dialect_t) :: d
        character(len=256) :: reqs(4)
        integer :: i

        reqs(1) = ''
        reqs(2) = '-O3 -funroll-loops'
        reqs(3) = '-g -O0 -fcheck=all -fbacktrace'
        reqs(4) = '-g -O0 -fcheck=all -fbacktrace -fsanitize=address,undefined'
        d = compiler_dialect('gfortran')
        do i = 1, 4
            call check((len_trim(d%sections_split_flags(reqs(i))) > 0) .eqv. &
                (len_trim(d%gc_sections_link_flag(reqs(i))) > 0), &
                'split and gc agree for request '//trim(reqs(i)))
        end do
        d = compiler_dialect('nvfortran')
        call check(len_trim(d%sections_split_flags('')) == 0, &
            'nvfortran gets no gnu section flags')
        call check(len_trim(d%gc_sections_link_flag('')) == 0, &
            'nvfortran gets no gnu linker flags')
    end subroutine test_pair_never_disagrees

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'gc-sections: pass=', n_pass, &
            ' fail=', n_fail
        if (n_fail > 0) stop 1
    end subroutine report

end program test_link_gc_sections
