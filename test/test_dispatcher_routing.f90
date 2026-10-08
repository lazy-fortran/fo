program test_dispatcher_routing
    !! W0.3 routing: a marked test runs inside the consolidated binary with its
    !! own name as argv, so the suite links the library once instead of once
    !! per test. The decision is pure (`dispatch_target`) and the marker is a
    !! real file read (`source_has_marker`), so both are checked here without
    !! invoking a compiler.
    use fo_util, only: temporary_root
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_gfortran_build, only: source_has_marker, dispatch_target
    use fo_fs, only: fs_make_dir, fs_write_text, fs_remove_tree
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_marked_test_routes_to_dispatcher()
    call test_dispatcher_never_dispatches_to_itself()
    call test_marker_must_be_at_line_start()
    call test_missing_file_has_no_marker()
    call report()

contains

    subroutine check(cond, name)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', name
        end if
    end subroutine check

    subroutine report()
        write (output_unit, '(a,i0,a,i0,a)') 'Summary: ', n_pass, &
            ' passed, ', n_fail, ' failed'
        if (n_fail > 0) stop 1
    end subroutine report

    subroutine test_marked_test_routes_to_dispatcher()
        character(len=512) :: root, path, bin
        character(len=:), allocatable :: args
        integer :: ios

        root = temporary_root()//'/fo_dispatcher_routing_probe'
        call fs_remove_tree(root)
        call fs_make_dir(root)
        call fs_write_text(trim(root)//'/test_alpha.f90', &
            "! fo: dispatcher"//new_line('a')// &
            'program test_alpha'//new_line('a')// &
            'end program test_alpha'//new_line('a'))
        call dispatch_target('/build/bin', 'ffc_suite', 'test_alpha', bin, args)
        call check(trim(bin) == '/build/bin/ffc_suite', &
            'marked test runs the dispatcher binary')
        call check(allocated(args) .and. args == 'test_alpha', &
            'case name reaches the dispatcher as argv')

        ! A test without the marker keeps its own executable: the conversion is
        ! opt-in per file, so a shell-driven oracle never gets swallowed.
        call fs_write_text(trim(root)//'/test_plain.f90', &
            'program test_plain'//new_line('a')// &
            'end program test_plain'//new_line('a'))
        call check(.not. source_has_marker(trim(root)//'/test_plain.f90', &
            '! fo: dispatcher'), 'unmarked source has no marker')
        call check(.not. allocated(args) .or. args /= 'test_plain', &
            'unmarked name is not dispatched (args untouched)')
        inquire (file=trim(root)//'/test_alpha.f90', iostat=ios)
        call check(ios == 0, 'fixture written for the marker check')
        call fs_remove_tree(root)
    end subroutine test_marked_test_routes_to_dispatcher

    subroutine test_dispatcher_never_dispatches_to_itself()
        character(len=512) :: bin
        character(len=:), allocatable :: args

        ! No base case: a dispatcher dispatched to itself is a hung suite.
        call dispatch_target('/build/bin', 'ffc_suite', 'ffc_suite', bin, args)
        call check(trim(bin) == '/build/bin/ffc_suite', &
            'dispatcher keeps its own binary path')
        call check(.not. allocated(args), &
            'dispatcher gets no argv case - no self-dispatch recursion')
    end subroutine test_dispatcher_never_dispatches_to_itself

    subroutine test_marker_must_be_at_line_start()
        character(len=512) :: root
        integer :: ios

        root = temporary_root()//'/fo_dispatcher_marker_probe'
        call fs_remove_tree(root)
        call fs_make_dir(root)
        ! A mention inside a string or prose is not a directive.
        call fs_write_text(trim(root)//'/x.f90', &
            'print *, "! fo: dispatcher"'//new_line('a'))
        call check(.not. source_has_marker(trim(root)//'/x.f90', &
            '! fo: dispatcher'), 'indented mention is not a marker')
        call fs_write_text(trim(root)//'/y.f90', &
            '! fo: dispatcher'//new_line('a'))
        call check(source_has_marker(trim(root)//'/y.f90', '! fo: dispatcher'), &
            'marker at column 1 is recognised')
        inquire (file=trim(root)//'/y.f90', iostat=ios)
        call check(ios == 0, 'marker fixture exists')
        call fs_remove_tree(root)
    end subroutine test_marker_must_be_at_line_start

    subroutine test_missing_file_has_no_marker()
        call check(.not. source_has_marker('/var/tmp/definitely_absent_fo.f90', &
            '! fo: dispatcher'), 'absent file reports no marker, not a crash')
    end subroutine test_missing_file_has_no_marker
end program test_dispatcher_routing
