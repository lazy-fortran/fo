program test_fs_lock_stale
    !! fs_lock_stale: a lock whose pid is running is held, one whose pid is
    !! gone is stale, and one without a pid is stale only once older than
    !! max_age seconds.
    use fo_fs, only: fs_lock_stale, fs_make_dir, fs_remove_tree, fs_write_text, fs_sleep_ms
    use fo_process, only: process_getpid
    use fo_test_harness, only: make_scratch, join_path, finish_assertions, assert_true
    implicit none
    character(:), allocatable :: scratch, lock
    character(len=16) :: digits

    call make_scratch('fo-lock-stale', scratch)
    lock = join_path(scratch, 'x.fo-acquire-lock')

    call fs_make_dir(lock)
    write (digits, '(i0)') process_getpid()
    call fs_write_text(lock//'/pid', trim(digits))
    call assert_true(.not. fs_lock_stale(lock, 0), 'a running owner holds the lock')

    ! 2^30 exceeds every pid_max, so no such process exists.
    call fs_write_text(lock//'/pid', '1073741824')
    call assert_true(fs_lock_stale(lock, 3600), 'a lock whose owner is gone is stale')

    call fs_remove_tree(lock)
    call fs_make_dir(lock)
    call assert_true(.not. fs_lock_stale(lock, 60), &
        'a fresh lock without a pid is being taken')
    call fs_sleep_ms(1500)
    call assert_true(fs_lock_stale(lock, 1), 'an old lock without a pid is stale')

    call fs_remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'fs-lock-stale: dead and abandoned locks are stale'
end program test_fs_lock_stale
