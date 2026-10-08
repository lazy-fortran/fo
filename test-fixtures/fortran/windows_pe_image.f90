program windows_pe_image
    use fo_pe_image, only: pe_image_valid
    implicit none
    character(len=1024) :: path
    integer :: i, failures
    logical :: valid, expected

    if (command_argument_count() < 5) stop 2
    failures = 0
    do i = 1, command_argument_count()
        call get_command_argument(i, path)
        call pe_image_valid(trim(path), valid)
        expected = i <= 2
        if (valid .neqv. expected) then
            failures = failures + 1
            print *, 'FAIL: image=', trim(path), ' accepted=', valid
        end if
    end do
    print *, 'native-pe-image-errors=', failures
    if (failures /= 0) stop 1
end program windows_pe_image
