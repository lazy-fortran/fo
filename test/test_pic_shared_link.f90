program test_pic_shared_link
    !! Prove shared-library PIC support with real links and a consumer that
    !! calls across the library boundary. Linux also requires the same source
    !! compiled without PIC to fail its shared link. Darwin can accept that
    !! source without an explicit PIC flag, so refusal is not its oracle.
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_util, only: make_tmpfile
    use fo_test_harness, only: run_list, argv
    implicit none

    character(len=512) :: dir, library
    integer :: st
    logical :: ok, macos

    ok = .true.

    inquire (file='/usr/lib/dyld', exist=macos)
    call make_tmpfile('fo_pic_probe', dir)
    call fs_make_dir(trim(dir))
    call write_probes()

    ! Dialect contract: off emits nothing, on emits the real flag.
    block
        type(compiler_dialect_t) :: d
        d = compiler_dialect('gfortran')
        call check('pic off emits nothing', len_trim(d%pic_flag(.false.)) == 0)
        call check('pic on emits -fPIC', trim(d%pic_flag(.true.)) == '-fPIC')
        if (.not. ok) stop 1
    end block

    ! The GNU linker refuses the non-PIC objects and accepts the PIC objects.
    ! Two objects, not one. A single object is PIC-compatible under a modern
    ! default toolchain, so the refusal never fires and the check would pass
    ! for the wrong reason. The ffc archive failed over a *cross-object*
    ! vtable reference - an object using a type defined in another object -
    ! so the probe compiles the module and its user separately and links both.
    if (.not. macos) then
        call compile_pair('nofpic', '')
        call run_list(argv('gcc', '-shared', '-o', 'nofpic.so', &
            'nofpic_mod.o', 'nofpic_use.o'), st, cwd=trim(dir))
        call check('shared link WITHOUT -fPIC is refused', st /= 0)
    end if

    call compile_pair('fpic', '-fPIC')
    if (macos) then
        library = trim(dir)//'/libfpic.dylib'
        call run_list(argv('gfortran', '-dynamiclib', '-o', &
            trim(library), 'fpic_mod.o', 'fpic_use.o'), st, cwd=trim(dir))
    else
        library = trim(dir)//'/libfpic.so'
        call run_list(argv('gcc', '-shared', '-o', trim(library), &
            'fpic_mod.o', 'fpic_use.o'), st, cwd=trim(dir))
    end if
    call check('shared link WITH -fPIC succeeds', st == 0)
    if (st == 0) call check_consumer()

    call fs_remove_tree(trim(dir))
    if (ok) then
        print *, 'PASS: PIC shared-library consumer returns 2'
    else
        stop 1
    end if

contains

    subroutine write_source()
        !! Same source twice: one compiled plain, one with -fPIC, so the only
        !! difference between the two shared links is the flag under test.
        call write_probe(trim(dir)//'/'//'nofpic.f90')
        call write_probe(trim(dir)//'/'//'fpic.f90')
    end subroutine write_source

    subroutine write_probes()
        !! Two sources per flavour: the module that owns the vtable, and the
        !! user that references it across the object boundary.
        call write_probe(trim(dir)//'/'//'nofpic_mod.f90')
        call write_user(trim(dir)//'/'//'nofpic_use.f90')
        call write_probe(trim(dir)//'/'//'fpic_mod.f90')
        call write_user(trim(dir)//'/'//'fpic_use.f90')
    end subroutine write_probes

    subroutine compile_pair(stem, extra)
        !! Compile module then user, in that order: the user needs the .mod.
        character(len=*), intent(in) :: stem, extra
        character(len=64) :: flag
        integer :: st

        flag = '-O0'
        if (len_trim(extra) > 0) flag = trim(extra)
        call run_list(argv('gfortran', '-c', '-O0', flag, &
            stem//'_mod.f90', '-o', stem//'_mod.o'), st, cwd=trim(dir))
        call check('module compiles ('//stem//')', st == 0)
        call run_list(argv('gfortran', '-c', '-O0', flag, '-I.', &
            stem//'_use.f90', '-o', stem//'_use.o'), st, cwd=trim(dir))
        call check('user compiles ('//stem//')', st == 0)
    end subroutine compile_pair

    subroutine check_consumer()
        integer :: u, status

        open (newunit=u, file=trim(dir)//'/consumer.f90', &
            status='replace', action='write')
        write (u, '(a)') 'program consumer'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') '    interface'
        write (u, '(a)') '        integer function useit()'
        write (u, '(a)') '        end function useit'
        write (u, '(a)') '    end interface'
        write (u, '(a)') '    if (useit() /= 2) error stop 1'
        write (u, '(a)') 'end program consumer'
        close (u)
        call run_list(argv('gfortran', 'consumer.f90', trim(library), &
            '-Wl,-rpath,'//trim(dir), '-o', 'consumer'), status, cwd=trim(dir))
        call check('external shared-library consumer links', status == 0)
        if (status /= 0) return
        call run_list(argv(trim(dir)//'/consumer'), status, cwd=trim(dir))
        call check('external shared-library consumer returns 2', status == 0)
    end subroutine check_consumer

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
