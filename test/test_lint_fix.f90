program test_lint_fix
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_lint, only: lint_fix_dir
    use fo_util, only: make_tmpfile
    use fo_fs, only: fs_make_dir, fs_remove_tree
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_fix_removes_unused_imports()
    call test_keeps_import_used_in_submodule()

    write (output_unit, '(a,i0,a,i0,a)') 'lint_fix: ', n_pass, ' pass, ', &
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

    subroutine test_fix_removes_unused_imports()
        character(len=512) :: base, dir, src
        character(len=4096) :: body
        integer :: n_removed, n_remaining, u

        call make_tmpfile('fo_lint_fix', base)
        dir = trim(base)//'_d'
        src = trim(dir)//'/src'
        call fs_make_dir(trim(src))

        open (newunit=u, file=trim(src)//'/m.f90', status='replace')
        write (u, '(a)') 'module m'
        write (u, '(a)') '    use iso_fortran_env, only: int32, real64, error_unit'
        write (u, '(a)') '    use foo_mod, only: used_a, unused_b, used_c'
        write (u, '(a)') '    use bar_mod, only: only_unused'
        write (u, '(a)') '    use multi_mod, only: keep1, dropme, &'
        write (u, '(a)') '        keep2'
        write (u, '(a)') '    use allgone_mod, only: g1, &'
        write (u, '(a)') '        g2'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') '    private'
        write (u, '(a)') 'contains'
        write (u, '(a)') '    subroutine s()'
        write (u, '(a)') '        integer(int32) :: x'
        write (u, '(a)') '        real(real64) :: y'
        write (u, '(a)') '        x = used_a + used_c + keep1 + keep2 + g2'
        write (u, '(a)') '        y = real(x, real64)'
        write (u, '(a)') '    end subroutine s'
        write (u, '(a)') 'end module m'
        close (u)

        call lint_fix_dir(trim(dir), n_removed, n_remaining)

        call assert(n_removed == 5, 'removes all five unused imports')
        call assert(n_remaining == 0, 'no unused imports remain')

        call read_whole(trim(src)//'/m.f90', body)

        call assert(index(body, 'error_unit') == 0, 'drops error_unit')
        call assert(index(body, 'unused_b') == 0, 'drops unused_b')
        call assert(index(body, 'only_unused') == 0, 'drops only_unused')
        call assert(index(body, 'bar_mod') == 0, 'drops now-empty bar_mod use')
        call assert(index(body, 'dropme') == 0, 'drops dropme')
        call assert(index(body, 'g1') == 0, 'drops g1')

        call assert(index(body, 'int32') > 0, 'keeps int32')
        call assert(index(body, 'used_a') > 0, 'keeps used_a')
        call assert(index(body, 'keep1, &') > 0, 'keeps comma before continuation')
        call assert(index(body, 'only: &') > 0, 'empty first line keeps continuation')
        call assert(index(body, 'keep2') > 0, 'keeps continuation symbol keep2')
        call assert(index(body, 'g2') > 0, 'keeps continuation symbol g2')

        call fs_remove_tree(trim(dir))
    end subroutine test_fix_removes_unused_imports

    subroutine test_keeps_import_used_in_submodule()
        !! A module import consumed only inside a submodule (which inherits the
        !! module's imports) must not be reported or removed as unused.
        character(len=512) :: base, dir, src
        character(len=4096) :: body
        integer :: n_removed, n_remaining, u

        call make_tmpfile('fo_lint_submod', base)
        dir = trim(base)//'_d'
        src = trim(dir)//'/src'
        call fs_make_dir(trim(src))

        open (newunit=u, file=trim(src)//'/parent.f90', status='replace')
        write (u, '(a)') 'module parent'
        write (u, '(a)') '    use grid_mod, only: grid_t, build_grid'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') '    private'
        write (u, '(a)') '    public :: work'
        write (u, '(a)') '    interface'
        write (u, '(a)') '        module subroutine work(g)'
        write (u, '(a)') '            import :: grid_t'
        write (u, '(a)') '            type(grid_t), intent(inout) :: g'
        write (u, '(a)') '        end subroutine work'
        write (u, '(a)') '    end interface'
        write (u, '(a)') 'end module parent'
        close (u)

        open (newunit=u, file=trim(src)//'/child.f90', status='replace')
        write (u, '(a)') 'submodule(parent) child'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') '    module subroutine work(g)'
        write (u, '(a)') '        type(grid_t), intent(inout) :: g'
        write (u, '(a)') '        g = build_grid()'
        write (u, '(a)') '    end subroutine work'
        write (u, '(a)') 'end submodule child'
        close (u)

        call lint_fix_dir(trim(dir), n_removed, n_remaining)

        call assert(n_removed == 0, 'keeps imports used only in a submodule')

        call read_whole(trim(src)//'/parent.f90', body)
        call assert(index(body, 'build_grid') > 0, 'build_grid import preserved')
        call assert(index(body, 'grid_t') > 0, 'grid_t import preserved')

        call fs_remove_tree(trim(dir))
    end subroutine test_keeps_import_used_in_submodule

    subroutine read_whole(path, text)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: text

        character(len=1024) :: line
        integer :: u, iostat, n

        text = ''
        n = 0
        open (newunit=u, file=trim(path), status='old', iostat=iostat)
        if (iostat /= 0) return
        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            text(n + 1:) = trim(line)//char(10)
            n = n + len_trim(line) + 1
        end do
        close (u)
    end subroutine read_whole

end program test_lint_fix
