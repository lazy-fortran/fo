program test_clean_stale_graph
    !! `fo clean --stale` decides liveness from the build's own records: a link
    !! archive stays while an executable linked against it exists, an object stays
    !! while its source exists, a live build lock stops the run, there is no
    !! candidate cap, and an interrupted run is finished by the next one.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_clean_stale, only: clean_stale_run
    use fo_process, only: process_getpid
    use fo_test_harness, only: process_result_t, string_list_t, list_add, &
        make_scratch, join_path, write_text, remove_tree, make_directory, &
        file_exists, assert_true, assert_process_ok, assert_contains, &
        finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(:), allocatable :: scratch, driver

    call make_scratch('fo-clean-stale-graph', scratch)
    call unit_graph_tree(join_path(scratch, 'unit'))
    call real_build(join_path(scratch, 'real'), join_path(scratch, 'cache'))
    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine unit_graph_tree(root)
        character(len=*), intent(in) :: root
        integer, parameter :: ORPHANS = 4200
        integer :: n_removed, i
        integer(int64) :: freed
        logical :: blocked
        character(len=16) :: id

        call make_directory(join_path(root, 'src'))
        call make_directory(join_path(root, 'build/fo/lib'))
        call make_directory(join_path(root, 'build/fo/obj'))
        call make_directory(join_path(root, 'build/fo/bin'))
        call write_text(join_path(root, 'src/m.f90'), 'module m'//new_line('a')// &
            'end module m'//new_line('a'))
        call write_text(join_path(root, 'build/fo/obj/src_m.f90.o'), 'live object')
        call write_text(join_path(root, 'build/fo/obj/src_gone.f90.o'), 'orphan')
        call write_text(join_path(root, 'build/fo/obj/external_0123.o'), 'external')
        call write_text(join_path(root, 'build/fo/lib/objects_linked.a'), 'linked')
        call write_text(join_path(root, 'build/fo/lib/objects_unlinked.a'), 'unlinked')
        call write_text(join_path(root, 'build/fo/lib/objects_dead.a'), 'dead exe')
        call write_text(join_path(root, 'build/fo/bin/prog'), 'binary')
        call write_text(join_path(root, 'build/fo/link-graph'), &
            join_path(root, 'build/fo/bin/prog')//achar(9)// &
            join_path(root, 'build/fo/lib/objects_linked.a')//achar(10)// &
            join_path(root, 'build/fo/bin/missing')//achar(9)// &
            join_path(root, 'build/fo/lib/objects_dead.a')//achar(10))
        do i = 1, ORPHANS
            write (id, '(i0)') i
            call write_text(join_path(root, 'build/fo/obj/src_bulk'//trim(id)// &
                '.f90.o'), 'orphan')
        end do

        ! A live build holds the project lock: nothing may be removed.
        call make_directory(join_path(root, 'build/fo/.lock'))
        call write_text(join_path(root, 'build/fo/.lock/pid'), pid_text())
        call clean_stale_run(root, 0, n_removed, freed, blocked=blocked)
        call assert_true(blocked .and. n_removed == 0, &
            'a live build lock blocks the run')
        call assert_true(file_exists(join_path(root, 'build/fo/obj/src_gone.f90.o')), &
            'nothing is removed while the lock is live')
        call remove_tree(join_path(root, 'build/fo/.lock'))

        ! An interrupted run left a journal naming a live archive and a stale one.
        call write_text(join_path(root, 'build/fo/clean-stale.journal'), &
            join_path(root, 'build/fo/lib/objects_linked.a')//achar(10)// &
            join_path(root, 'build/fo/lib/objects_unlinked.a')//achar(10))

        call clean_stale_run(root, 0, n_removed, freed, blocked=blocked)
        call assert_true(.not. blocked, 'the run is not blocked without a lock')
        call assert_true(file_exists(join_path(root, 'build/fo/lib/objects_linked.a')), &
            'an archive linked by an existing executable survives keep=0')
        call assert_true(.not. file_exists(join_path(root, &
            'build/fo/lib/objects_unlinked.a')), 'an unlinked archive is removed')
        call assert_true(.not. file_exists(join_path(root, &
            'build/fo/lib/objects_dead.a')), &
            'an archive whose only executable is gone is removed')
        call assert_true(file_exists(join_path(root, 'build/fo/obj/src_m.f90.o')), &
            'an object with an existing source survives')
        call assert_true(file_exists(join_path(root, 'build/fo/obj/external_0123.o')), &
            'an object of an external source is never judged')
        call assert_true(.not. file_exists(join_path(root, &
            'build/fo/obj/src_gone.f90.o')), 'an orphaned object is removed')
        call assert_true(.not. file_exists(join_path(root, &
            'build/fo/obj/src_bulk4200.f90.o')), &
            'candidates beyond 4096 are removed too')
        call assert_true(n_removed >= ORPHANS + 3, &
            'the run reports every removed entry')
        call assert_true(.not. file_exists(join_path(root, &
            'build/fo/clean-stale.journal')), 'a finished run leaves no journal')
        call assert_true(file_exists(join_path(root, 'build/fo/bin/prog')), &
            'executables are never candidates')
    end subroutine unit_graph_tree

    function pid_text() result(text)
        character(len=:), allocatable :: text
        character(len=24) :: buffer

        write (buffer, '(i0)') process_getpid()
        text = trim(buffer)
    end function pid_text

    subroutine real_build(root, cache)
        character(len=*), intent(in) :: root, cache
        type(process_result_t) :: result
        type(string_list_t) :: arguments
        character(:), allocatable :: archive_dir

        call resolve_driver(driver)
        call make_directory(join_path(root, 'src'))
        call make_directory(join_path(root, 'app'))
        call make_directory(cache)
        call write_text(join_path(root, 'fpm.toml'), 'name = "linkonly"')
        call write_text(join_path(root, 'src/greet.f90'), &
            'module greet'//new_line('a')//'contains'//new_line('a')// &
            'subroutine hello()'//new_line('a')//"print '(a)', 'hello'"// &
            new_line('a')//'end subroutine hello'//new_line('a')// &
            'end module greet'//new_line('a'))
        call write_text(join_path(root, 'src/extra.f90'), &
            'module extra'//new_line('a')//'end module extra'//new_line('a'))
        call write_text(join_path(root, 'app/main.f90'), &
            'program main'//new_line('a')//'use greet'//new_line('a')// &
            'call hello()'//new_line('a')//'end program main'//new_line('a'))

        arguments = string_list_t()
        call list_add(arguments, 'build')
        call run_fo(driver, arguments, root, cache, result, timeout_ms=600000)
        call assert_process_ok(result, 'link-only fixture builds')
        archive_dir = join_path(root, 'build/fo/lib')
        call assert_true(file_exists(join_path(root, 'build/fo/link-graph')), &
            'the build records which executable links which archive')
        call assert_true(file_exists(join_path(root, 'build/fo/obj/src_extra.f90.o')), &
            'the unused module compiled to an object')

        call remove_source(join_path(root, 'src/extra.f90'))
        arguments = string_list_t()
        call list_add(arguments, 'clean')
        call list_add(arguments, '--stale')
        call list_add(arguments, '--keep')
        call list_add(arguments, '0')
        call run_fo(driver, arguments, root, cache, result)
        call assert_process_ok(result, 'clean --stale succeeds')
        call assert_contains(result%stdout, 'removed', &
            'clean --stale reports what it removed')
        call assert_true(.not. file_exists(join_path(root, &
            'build/fo/obj/src_extra.f90.o')), &
            'the object of the deleted source is removed')
        call assert_true(file_exists(join_path(root, 'build/fo/obj/src_greet.f90.o')), &
            'the object of an existing source stays')
        call assert_true(has_archive(archive_dir), &
            'the archive used only at link time survives keep=0')

        arguments = string_list_t()
        call list_add(arguments, 'build')
        call run_fo(driver, arguments, root, cache, result, timeout_ms=600000)
        call assert_process_ok(result, 'the project still builds and links')
        arguments = string_list_t()
        call list_add(arguments, 'exec')
        call list_add(arguments, 'linkonly')
        call run_fo(driver, arguments, root, cache, result, timeout_ms=600000)
        call assert_process_ok(result, 'the linked executable still runs')
        call assert_contains(result%stdout, 'hello', 'the executable prints')
    end subroutine real_build

    subroutine remove_source(path)
        use fo_fs, only: fs_remove_file
        character(len=*), intent(in) :: path

        call fs_remove_file(path)
    end subroutine remove_source

    logical function has_archive(directory)
        use fo_fs, only: fs_collect_files
        character(len=*), intent(in) :: directory
        character(len=512) :: files(16)
        integer :: n

        call fs_collect_files(directory, 'objects_', '.a', '', files, n, .false.)
        has_archive = n > 0
    end function has_archive

end program test_clean_stale_graph
