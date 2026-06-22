program test_stat_memo
    !! The persistent file-hash memo must return the true sha256, reuse it while
    !! (mtime,size) are unchanged, recompute when the file changes, and reload
    !! its persisted entries after a save.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_stat_memo, only: memo_hash_file, memo_save, memo_reset
    use fx_hash, only: sha256_file
    use fo_process, only: process_getpid
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_matches_direct_sha256()
    call test_recomputes_on_change()
    call test_persists_across_reset()

    write (output_unit, '(a,i0,a,i0,a)') 'stat_memo: ', n_pass, ' pass, ', &
        n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_matches_direct_sha256()
        character(len=512) :: f
        character(len=64) :: memo_h, direct_h
        integer :: ierr

        call write_file(f, 'hello memo')
        call memo_hash_file(trim(f), memo_h, ierr)
        call sha256_file(trim(f), direct_h, ierr)
        call assert(trim(memo_h) == trim(direct_h), &
            'memo hash equals direct sha256')
        ! second call returns the same (served from memo)
        call memo_hash_file(trim(f), memo_h, ierr)
        call assert(trim(memo_h) == trim(direct_h), 'repeat hash stable')
        call execute_command_line('rm -f '//trim(f))
    end subroutine test_matches_direct_sha256

    subroutine test_recomputes_on_change()
        character(len=512) :: f
        character(len=64) :: h1, h2, direct
        integer :: ierr

        call write_file(f, 'first contents')
        call memo_hash_file(trim(f), h1, ierr)
        ! change content (and size) so (mtime,size) differ
        call write_file(f, 'second, longer contents that change the size')
        call memo_hash_file(trim(f), h2, ierr)
        call sha256_file(trim(f), direct, ierr)
        call assert(trim(h1) /= trim(h2), 'hash changes after edit')
        call assert(trim(h2) == trim(direct), 'post-edit hash is correct')
        call execute_command_line('rm -f '//trim(f))
    end subroutine test_recomputes_on_change

    subroutine test_persists_across_reset()
        character(len=512) :: f
        character(len=64) :: h1, h2, direct
        integer :: ierr

        call write_file(f, 'persist me')
        call memo_hash_file(trim(f), h1, ierr)
        call memo_save()
        call memo_reset()              ! drop in-memory state; force reload
        call memo_hash_file(trim(f), h2, ierr)
        call sha256_file(trim(f), direct, ierr)
        call assert(trim(h2) == trim(direct), 'hash correct after reload')
        call assert(trim(h1) == trim(h2), 'reloaded hash matches pre-save')
        call execute_command_line('rm -f '//trim(f))
    end subroutine test_persists_across_reset

    subroutine write_file(path, text)
        character(len=*), intent(out) :: path
        character(len=*), intent(in) :: text
        integer :: u, count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,i0,a,i0)') '/tmp/fo_statmemo-', process_getpid(), &
            '-', serial + count
        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') text
        close (u)
    end subroutine write_file

end program test_stat_memo
