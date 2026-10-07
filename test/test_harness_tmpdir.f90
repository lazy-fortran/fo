program test_harness_tmpdir
    use fo_test_harness, only: make_scratch, environment_value, assert_true, &
        file_exists, finish_assertions
    implicit none

    character(:), allocatable :: scratch, tmpdir
    integer :: tmpdir_length

    tmpdir = trim(environment_value('TMPDIR'))
    call assert_true(len(tmpdir) > 0, 'test requires an assigned private TMPDIR')
    do while (len(tmpdir) > 1)
        tmpdir_length = len(tmpdir)
        if (tmpdir(tmpdir_length:tmpdir_length) /= '/') exit
        tmpdir = tmpdir(:tmpdir_length - 1)
    end do
    call make_scratch('fo-harness-tmpdir-oracle', scratch)
    call assert_true(index(scratch, tmpdir // '/') == 1 .or. tmpdir == '/', &
        'fixture scratch is created inside TMPDIR')

    call finish_assertions()
    call assert_true(.not. file_exists(scratch), &
        'registered scratch is removed during harness cleanup')
    call finish_assertions()
end program test_harness_tmpdir
