program check_flang
    use, intrinsic :: iso_fortran_env, only: compiler_version
    use smoke_value, only: answer
    implicit none
    character(len=:), allocatable :: version

    version = compiler_version()
    if (index(version, 'flang') == 0 .and. index(version, 'Flang') == 0) &
        error stop 'test was not compiled by Flang'
    if (answer() /= 42) error stop 'module result is incorrect'
    print '(a)', version
end program check_flang
