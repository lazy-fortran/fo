program test_pic_shared_link
    !! `-fPIC` is the precondition for a shared libffc, and this proves it at
    !! the linker rather than by reading a flag string: the same source,
    !! compiled with and without the flag, must fail and then succeed as a
    !! shared object. Falsification is built in - delete `-fPIC` from the
    !! second compile and the last check goes red.
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t
    implicit none

    character(len=*), parameter :: dir = '/var/tmp/fo-pic-probe'
    character(len=512) :: cmd
    integer :: st
    logical :: ok

    ok = .true.

    call execute_command_line('mkdir -p '//dir, exitstat=st)
    call write_probes()

    ! Dialect contract: off emits nothing, on emits the real flag.
    block
        type(compiler_dialect_t) :: d
        d = compiler_dialect('gfortran')
        call check('pic off emits nothing', len_trim(d%pic_flag(.false.)) == 0)
        call check('pic on emits -fPIC', trim(d%pic_flag(.true.)) == '-fPIC')
        if (.not. ok) stop 1
    end block

    ! The linker is the oracle. Without PIC it refuses; with PIC it accepts.
    ! Two objects, not one. A single object is PIC-compatible under a modern
    ! default toolchain, so the refusal never fires and the check would pass
    ! for the wrong reason. The ffc archive failed over a *cross-object*
    ! vtable reference - an object using a type defined in another object -
    ! so the probe compiles the module and its user separately and links both.
    call compile_pair('nofpic', '')
    write (cmd, '(a)') 'cd '//dir//' && gcc -shared -o nofpic.so '// &
        'nofpic_mod.o nofpic_use.o > /dev/null 2>&1'
    call execute_command_line(cmd, exitstat=st)
    call check('shared link WITHOUT -fPIC is refused', st /= 0)

    call compile_pair('fpic', '-fPIC')
    write (cmd, '(a)') 'cd '//dir//' && gcc -shared -o fpic.so '// &
        'fpic_mod.o fpic_use.o > /dev/null 2>&1'
    call execute_command_line(cmd, exitstat=st)
    call check('shared link WITH -fPIC succeeds', st == 0)

    if (ok) then
        print *, 'PASS: -fPIC is what makes the shared link possible'
    else
        stop 1
    end if

contains

    subroutine write_source()
        !! Same source twice: one compiled plain, one with -fPIC, so the only
        !! difference between the two shared links is the flag under test.
        call write_probe(dir//'/'//'nofpic.f90')
        call write_probe(dir//'/'//'fpic.f90')
    end subroutine write_source

    subroutine write_probes()
        !! Two sources per flavour: the module that owns the vtable, and the
        !! user that references it across the object boundary.
        call write_probe(dir//'/'//'nofpic_mod.f90')
        call write_user(dir//'/'//'nofpic_use.f90')
        call write_probe(dir//'/'//'fpic_mod.f90')
        call write_user(dir//'/'//'fpic_use.f90')
    end subroutine write_probes

    subroutine compile_pair(stem, extra)
        !! Compile module then user, in that order: the user needs the .mod.
        character(len=*), intent(in) :: stem, extra
        character(len=700) :: c
        integer :: st

        write (c, '(a,3a)') 'cd '//dir//' && gfortran -c -O0 '//trim(extra)// &
            ' '//stem//'_mod.f90 -o '//stem//'_mod.o'
        call execute_command_line(c//' > /dev/null 2>&1', exitstat=st)
        call check('module compiles ('//stem//')', st == 0)
        write (c, '(a,3a)') 'cd '//dir//' && gfortran -c -O0 '//trim(extra)// &
            ' -I. '//stem//'_use.f90 -o '//stem//'_use.o'
        call execute_command_line(c//' > /dev/null 2>&1', exitstat=st)
        call check('user compiles ('//stem//')', st == 0)
    end subroutine compile_pair

    subroutine write_probe(path)
        !! One probe source: a polymorphic call, which is what puts a vtable
        !! reference into the object and makes the non-PIC shared link fail.
        character(len=*), intent(in) :: path
        integer :: u

        open (newunit=u, file=path, status='replace', action='write')
        write (u, '(a)') 'module probe_mod'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') '    type, public :: base_t'
        write (u, '(a)') '    contains'
        write (u, '(a)') '        procedure, pass(this) :: val => base_val'
        write (u, '(a)') '    end type base_t'
        write (u, '(a)') '    type, public, extends(base_t) :: child_t'
        write (u, '(a)') '    contains'
        write (u, '(a)') '        procedure, pass(this) :: val => child_val'
        write (u, '(a)') '    end type child_t'
        write (u, '(a)') 'contains'
        write (u, '(a)') '    integer function base_val(this) result(r)'
        write (u, '(a)') '        class(base_t), intent(in) :: this'
        write (u, '(a)') '        r = 1'
        write (u, '(a)') '    end function base_val'
        write (u, '(a)') '    integer function child_val(this) result(r)'
        write (u, '(a)') '        class(child_t), intent(in) :: this'
        write (u, '(a)') '        r = 2'
        write (u, '(a)') '    end function child_val'
        write (u, '(a)') '    integer function poly(x) result(r)'
        write (u, '(a)') '        class(base_t), intent(in) :: x'
        write (u, '(a)') '        r = x%val()'
        write (u, '(a)') '    end function poly'
        write (u, '(a)') 'end module probe_mod'
        close (u)
    end subroutine write_probe

    subroutine write_user(path)
        character(len=*), intent(in) :: path
        integer :: u

        open (newunit=u, file=path, status='replace', action='write')
        write (u, '(a)') 'integer function useit() result(r)'
        write (u, '(a)') '    use probe_mod, only: base_t, child_t'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') '    class(base_t), allocatable :: x'
        write (u, '(a)') '    allocate (child_t :: x)'
        write (u, '(a)') '    r = x%val()'
        write (u, '(a)') 'end function useit'
        close (u)
    end subroutine write_user

    subroutine check(label, cond)
        character(len=*), intent(in) :: label
        logical, intent(in) :: cond

        if (cond) then
            print '(a)', 'PASS: '//label
        else
            print '(a)', 'FAIL: '//label
            ok = .false.
        end if
    end subroutine check

end program test_pic_shared_link
