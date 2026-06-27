program test_profile
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_build_backend, only: profile_flags
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_debug_has_fcheck()
    call test_asan_has_sanitizer()
    call test_unknown_is_empty()
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

    subroutine test_debug_has_fcheck()
        call check(index(profile_flags('debug'), '-fcheck=all') > 0, &
            'debug includes -fcheck=all')
        call check(index(profile_flags('debug'), '-g') > 0, &
            'debug includes -g')
    end subroutine test_debug_has_fcheck

    subroutine test_asan_has_sanitizer()
        call check(index(profile_flags('asan'), &
            '-fsanitize=address,undefined') > 0, &
            'asan includes sanitizer')
        call check(index(profile_flags('asan'), '-fcheck=all') > 0, &
            'asan includes -fcheck=all')
    end subroutine test_asan_has_sanitizer

    subroutine test_unknown_is_empty()
        call check(len_trim(profile_flags('bogus')) == 0, &
            'unknown profile empty')
    end subroutine test_unknown_is_empty

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'profile: pass=', n_pass, &
            ' fail=', n_fail
        if (n_fail > 0) stop 1
    end subroutine report

end program test_profile
