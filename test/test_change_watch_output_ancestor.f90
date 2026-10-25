program test_change_watch_output_ancestor
    !! Creating the parent directories of a relative CMake build root
    !! (FO_CMAKE_BUILD_DIR=build_cmake/<config>) inside a fresh project is
    !! fo's own output, not an input change. Inputs placed beside the build
    !! root inside such a parent still report.
    use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_associated, c_char, &
        c_null_char
    use fo_change_watch, only: change_watch_t, change_watch_relevant
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_util, only: make_tmpfile
    implicit none
    interface
        function native_open(ierr) bind(C, name='fo_change_native_open') result(handle)
            import :: c_ptr, c_int
            integer(c_int), intent(out) :: ierr
            type(c_ptr) :: handle
        end function native_open
        subroutine native_close(handle) bind(C, name='fo_change_native_close')
            import :: c_ptr
            type(c_ptr), value :: handle
        end subroutine native_close
        integer(c_int) function native_exclude(handle, path) &
                bind(C, name='fo_change_native_exclude_root')
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: handle
            character(c_char), intent(in) :: path(*)
        end function native_exclude
        integer(c_int) function native_root(handle, path) &
                bind(C, name='fo_change_native_root')
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: handle
            character(c_char), intent(in) :: path(*)
        end function native_root
        integer(c_int) function native_reconcile(handle) &
                bind(C, name='fo_change_native_reconcile')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
        end function native_reconcile
        integer(c_int) function native_poll(handle, timeout, path, capacity, kind) &
                bind(C, name='fo_change_native_poll')
            import :: c_ptr, c_int, c_char
            type(c_ptr), value :: handle
            integer(c_int), value :: timeout, capacity
            character(c_char), intent(out) :: path(*)
            integer(c_int), intent(out) :: kind
        end function native_poll
    end interface
    character(len=4096) :: root, parent, output, event
    type(c_ptr) :: watch
    type(change_watch_t) :: portable
    integer(c_int) :: ierr, kind
    integer :: i, stage

    ierr = 0
    kind = 0
    call make_tmpfile('fo-change-output-ancestor', root)
    call fs_make_dir(trim(root))
    parent = trim(root)//'/build_cmake'
    output = trim(parent)//'/ff-Debug'

    ! Portable relevance rule shared by the fallback watcher and fo watch.
    stage = 1
    allocate (portable%roots(1), portable%active(1))
    portable%roots(1) = root
    portable%active(1) = .true.
    portable%n_roots = 1
    portable%excluded_root = output
    if (.not. change_watch_relevant(portable, trim(root))) call fail()
    if (.not. change_watch_relevant(portable, trim(root)//'/input.f90')) call fail()
    if (change_watch_relevant(portable, trim(parent))) call fail()
    if (change_watch_relevant(portable, trim(output))) call fail()
    if (change_watch_relevant(portable, trim(output)//'/CMakeCache.txt')) call fail()
    if (.not. change_watch_relevant(portable, trim(parent)//'/beside.f90')) call fail()
    if (.not. change_watch_relevant(portable, trim(root)//'/build_cmake2')) call fail()

    watch = native_open(ierr)
    if (.not. c_associated(watch)) then
        if (ierr /= 0) call fail()
        call fs_remove_tree(trim(root))
        print *, 'native Linux fixture skipped; portable rule verified'
        stop
    end if
    stage = 2
    ierr = native_exclude(watch, trim(output)//c_null_char)
    if (ierr /= 0) call fail()
    ierr = native_root(watch, trim(root)//c_null_char)
    if (ierr /= 0) call fail()
    ierr = native_reconcile(watch)
    if (ierr /= 0) call fail()

    ! fo configures CMake into a build root whose parent does not exist yet.
    stage = 3
    call fs_make_dir(trim(parent))
    call fs_make_dir(trim(output))
    call write_file(trim(output)//'/CMakeCache.txt')
    do i = 1, 5
        ierr = native_poll(watch, 100, event, len(event), kind)
        if (ierr /= 0 .or. kind /= 0) call fail()
    end do

    ! The parent stays subscribed, so an input beside the build root reports.
    stage = 4
    call write_file(trim(parent)//'/beside.f90')
    ierr = native_poll(watch, 200, event, len(event), kind)
    if (ierr /= 0 .or. kind == 0) call fail()

    stage = 5
    do i = 1, 20
        ierr = native_poll(watch, 50, event, len(event), kind)
        if (ierr /= 0) call fail()
        if (kind == 0) exit
    end do
    call write_file(trim(root)//'/input.f90')
    ierr = native_poll(watch, 200, event, len(event), kind)
    if (ierr /= 0 .or. kind == 0) call fail()

    call native_close(watch)
    call fs_remove_tree(trim(root))
    print *, 'change watch: build-root ancestors are output, siblings stay inputs'
contains
    subroutine write_file(path)
        character(len=*), intent(in) :: path
        integer :: unit
        open (newunit=unit, file=path, status='replace', action='write')
        write (unit, '(a)') 'x'
        close (unit)
    end subroutine write_file

    subroutine fail()
        print '(a,i0,a,i0,a,i0)', 'stage ', stage, ' ierr ', ierr, ' kind ', kind
        call fs_remove_tree(trim(root))
        error stop 'change watch output-ancestor contract failed'
    end subroutine fail
end program test_change_watch_output_ancestor
