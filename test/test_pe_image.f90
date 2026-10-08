program test_pe_image
    use fo_pe_image, only: pe_image_valid
    use fo_util, only: make_tmpfile, delete_tmpfile
    implicit none
    character(len=4096) :: image, prefix
    character(len=1024) :: partial, missing
    integer, parameter :: lengths(5) = [2, 64, 128, 256, 4096]
    integer :: u, i
    logical :: valid, exists

    ! The OS has loaded this complete image independently of the validator.
    call get_command_argument(0, image)
    open (newunit=u, file=trim(image), access='stream', form='unformatted', &
        status='old', action='read')
    read (u) prefix
    close (u)
    call pe_image_valid(trim(image), valid)
    if (valid .neqv. (prefix(:2) == 'MZ')) error stop 'loaded image format'

    call make_tmpfile('pe-image-partial', partial)
    do i = 1, size(lengths)
        open (newunit=u, file=trim(partial), access='stream', form='unformatted', &
            status='replace', action='write')
        write (u) prefix(:lengths(i))
        close (u)
        call pe_image_valid(trim(partial), valid)
        if (valid) error stop 'accepted incomplete executable'
    end do
    open (newunit=u, file=trim(partial), access='stream', form='unformatted', &
        status='replace', action='write')
    write (u) 'ordinary text'
    close (u)
    call pe_image_valid(trim(partial), valid)
    if (valid) error stop 'accepted ordinary text'
    call delete_tmpfile(partial)

    missing = trim(partial)//'.missing'
    inquire (file=trim(missing), exist=exists)
    if (exists) error stop 'missing-path fixture collision'
    call pe_image_valid(trim(missing), valid)
    if (valid) error stop 'accepted missing executable'
    print '(a)', 'test_pe_image: PASS'
end program test_pe_image
