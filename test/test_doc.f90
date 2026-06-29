program test_doc
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_doc, only: fo_doc_run, collect_public_symbols
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_collect_symbols()
    call test_doc_writes_index()
    call report()

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//msg
        end if
    end subroutine assert

    subroutine write_sample(dir)
        character(len=*), intent(in) :: dir
        integer :: u, stat

        call execute_command_line('rm -rf '//trim(dir)//' && mkdir -p '// &
            trim(dir)//'/src', wait=.true., exitstat=stat)
        open (newunit=u, file=trim(dir)//'/src/mymod.f90', status='replace', &
            action='write')
        write (u, '(a)') 'module mymod'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') '    private'
        write (u, '(a)') '    public :: alpha, beta'
        write (u, '(a)') 'contains'
        write (u, '(a)') '    subroutine alpha()'
        write (u, '(a)') '    end subroutine alpha'
        write (u, '(a)') '    subroutine beta()'
        write (u, '(a)') '    end subroutine beta'
        write (u, '(a)') 'end module mymod'
        close (u)
    end subroutine write_sample

    subroutine test_collect_symbols()
        character(len=128) :: syms(64)
        integer :: n
        logical :: has_alpha, has_beta
        integer :: i

        call write_sample('/tmp/fo_doc_unit')
        call collect_public_symbols('/tmp/fo_doc_unit/src/mymod.f90', syms, n)

        has_alpha = .false.
        has_beta = .false.
        do i = 1, n
            if (trim(syms(i)) == 'alpha') has_alpha = .true.
            if (trim(syms(i)) == 'beta') has_beta = .true.
        end do
        call assert(has_alpha, 'collect finds public alpha')
        call assert(has_beta, 'collect finds public beta')
    end subroutine test_collect_symbols

    subroutine test_doc_writes_index()
        integer :: ec, u, ios
        character(len=512) :: line
        logical :: found_mod

        call write_sample('/tmp/fo_doc_int')
        call fo_doc_run('/tmp/fo_doc_int', .false., ec)
        call assert(ec == 0, 'fo_doc_run exits zero')

        found_mod = .false.
        open (newunit=u, file='/tmp/fo_doc_int/build/fo/doc/index.md', &
            status='old', action='read', iostat=ios)
        call assert(ios == 0, 'index.md written')
        if (ios == 0) then
            do
                read (u, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (index(line, 'mymod') > 0) found_mod = .true.
            end do
            close (u)
        end if
        call assert(found_mod, 'index.md lists mymod')
    end subroutine test_doc_writes_index

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'doc: pass=', n_pass, &
            ' fail=', n_fail
        if (n_fail > 0) stop 1
    end subroutine report

end program test_doc
