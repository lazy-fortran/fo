program test_compiler_service
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_compiler_service, only: compiler_service_request_t, &
        compiler_service_result_t, lfortran_service_stub_t, &
        fortfront_service_stub_t, COMPILER_SERVICE_UNAVAILABLE
    implicit none

    type(compiler_service_request_t) :: request
    type(compiler_service_result_t) :: result
    type(lfortran_service_stub_t) :: lfortran
    type(fortfront_service_stub_t) :: fortfront
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0
    request%source_path = 'fixture.f90'

    call assert(lfortran%name() == 'lfortran', 'LFortran adapter has a stable name')
    call assert(.not. lfortran%available(), 'LFortran adapter remains unwired')
    call lfortran%compile(request, result)
    call assert(.not. result%supported, 'LFortran stub refuses compilation')
    call assert(result%exitcode == COMPILER_SERVICE_UNAVAILABLE, &
        'LFortran stub reports service unavailable')
    call assert(index(result%diagnostic, 'fixture.f90') > 0, &
        'LFortran stub preserves request context')

    call assert(fortfront%name() == 'fortfront', 'Fortfront adapter has a stable name')
    call assert(.not. fortfront%available(), 'Fortfront adapter remains unwired')
    call fortfront%compile(request, result)
    call assert(.not. result%supported, 'Fortfront stub refuses compilation')
    call assert(result%exitcode == COMPILER_SERVICE_UNAVAILABLE, &
        'Fortfront stub reports service unavailable')

    write (output_unit, '(a,i0,a,i0,a)') 'compiler_service: ', n_pass, &
        ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', message
        end if
    end subroutine assert

end program test_compiler_service
