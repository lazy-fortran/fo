program test_change_watch_provider
    !! Public shared-provider behavior fixture, independent of Gremlin lifecycle.
    use fo_change_watch, only: change_watch_t, change_watch_init, &
        change_watch_add_root, change_watch_poll, change_watch_close, &
        CHANGE_RECONCILE
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree, fs_rename, &
        fs_sleep_ms, fs_write_text
    use fo_util, only: make_tmpfile
    implicit none

    type(change_watch_t) :: watch
    character(len=4096) :: scratch, project, dependency, ignored
    character(len=4096) :: nested, retired, changed, error_text
    character(len=4096) :: created_file, renamed_file
    integer :: ierr, kind, captures, rename_error
    logical :: got_event

    call change_watch_init(watch, '.', ierr, error_text)
    call require(ierr == 0, 'large project tree init: '//trim(error_text))
    call change_watch_close(watch)

    call make_tmpfile('fo-change-provider-public', scratch)
    project = trim(scratch)//'/project'
    dependency = trim(scratch)//'/dependency'
    ignored = trim(scratch)//'/ignored'
    nested = trim(project)//'/nested'
    retired = trim(project)//'/retired'
    created_file = trim(nested)//'/created-after-start.f90'
    renamed_file = trim(nested)//'/renamed.f90'
    call fs_make_dir(trim(nested))
    call fs_make_dir(trim(dependency))
    call fs_make_dir(trim(ignored))
    call fs_write_text(trim(nested)//'/initial.f90', 'program initial')

    call change_watch_init(watch, trim(project), ierr, error_text)
    call require(ierr == 0, 'init: '//trim(error_text))
    call change_watch_add_root(watch, trim(dependency), ierr, error_text)
    call require(ierr == 0, 'declared dependency root: '//trim(error_text))

    captures = 0
    call change_watch_poll(watch, 300, changed, kind, got_event, ierr)
    call require(ierr == 0, 'idle poll returned an error')
    call require(.not. got_event, 'idle provider emitted a change')
    call require(captures == 0, 'idle provider caused an expensive capture')

    call fs_write_text(trim(created_file), 'program fresh')
    call await_path_nonblocking(trim(created_file))
    call settle_events()
    call fs_write_text(trim(created_file), 'program changed')
    call await_path(trim(created_file))
    call settle_events()
    rename_error = fs_rename(trim(created_file), trim(renamed_file))
    call require(rename_error == 0, 'cannot rename watched file')
    call await_path(trim(created_file))
    call settle_events()
    call fs_remove_file(trim(renamed_file))
    call await_path(trim(renamed_file))
    call settle_events()
    call fs_write_text(trim(dependency)//'/dependency.f90', 'program dependency')
    call await_path(trim(dependency)//'/dependency.f90')
    call settle_events()

    captures = 0
    call fs_write_text(trim(ignored)//'/outside.f90', 'program ignored')
    call assert_idle('undeclared root')

    rename_error = fs_rename(trim(nested), trim(retired))
    call require(rename_error == 0, 'cannot rename watched directory')
    call fs_make_dir(trim(nested))
    call fs_write_text(trim(nested)//'/replacement.f90', 'program replacement')
    captures = 0
    call await_reconcile('directory replacement')
    call settle_events()
    call require(captures == 1, 'directory replacement caused multiple dirty wakes')

    rename_error = fs_rename(trim(project), trim(project)//'-lost')
    call require(rename_error == 0, 'cannot replace watched root')
    captures = 0
    call await_reconcile('lost root detection')
    call settle_events()
    call require(captures == 1, 'lost root caused multiple dirty wakes')

    call fs_make_dir(trim(project))
    call fs_make_dir(trim(project)//'/restored')
    call fs_write_text(trim(project)//'/restored/recovered.f90', 'program restored')
    captures = 0
    call await_reconcile('lost root recovery')
    call settle_events()
    call require(captures == 1, 'root recovery caused multiple dirty wakes')
    captures = 0
    call assert_idle('recovered root')

    call change_watch_close(watch)
    call fs_remove_tree(trim(scratch))
    print '(a)', 'shared change provider: roots, events, idle and recovery passed'

contains

    subroutine require(ok, message)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: message
        if (ok) return
        call change_watch_close(watch)
        call fs_remove_tree(trim(scratch))
        error stop message
    end subroutine require

    subroutine await_path(expected)
        character(len=*), intent(in) :: expected
        integer :: attempt

        do attempt = 1, 40
            call change_watch_poll(watch, 100, changed, kind, got_event, ierr)
            call require(ierr == 0, 'event poll returned an error')
            if (.not. got_event) cycle
            captures = captures + 1
            if (index(trim(changed), trim(expected)) > 0) return
            if (kind == CHANGE_RECONCILE) return
        end do
        call require(.false., 'timed out waiting for '//trim(expected))
    end subroutine await_path

    subroutine await_path_nonblocking(expected)
        character(len=*), intent(in) :: expected
        integer :: attempt

        do attempt = 1, 80
            call change_watch_poll(watch, 0, changed, kind, got_event, ierr)
            call require(ierr == 0, 'nonblocking poll returned an error')
            if (got_event) then
                captures = captures + 1
                if (index(trim(changed), trim(expected)) > 0) return
                if (kind == CHANGE_RECONCILE) return
            end if
            call fs_sleep_ms(50)
        end do
        call require(.false., 'nonblocking poll timed out waiting for '//trim(expected))
    end subroutine await_path_nonblocking

    subroutine await_reconcile(label)
        character(len=*), intent(in) :: label
        integer :: attempt

        do attempt = 1, 40
            call change_watch_poll(watch, 100, changed, kind, got_event, ierr)
            call require(ierr == 0, trim(label)//' poll returned an error')
            if (.not. got_event) cycle
            captures = captures + 1
            if (kind == CHANGE_RECONCILE) return
        end do
        call require(.false., 'timed out waiting for '//trim(label)//' rescan')
    end subroutine await_reconcile

    subroutine settle_events()
        integer :: quiet

        quiet = 0
        do while (quiet < 3)
            call change_watch_poll(watch, 100, changed, kind, got_event, ierr)
            call require(ierr == 0, 'settle poll returned an error')
            if (got_event) then
                captures = captures + 1
                quiet = 0
            else
                quiet = quiet + 1
            end if
        end do
    end subroutine settle_events

    subroutine assert_idle(label)
        character(len=*), intent(in) :: label
        integer :: quiet

        quiet = 0
        do while (quiet < 3)
            call change_watch_poll(watch, 100, changed, kind, got_event, ierr)
            call require(ierr == 0, trim(label)//' poll returned an error')
            if (got_event) then
                call require(.false., trim(label)//' produced a relevant event')
                quiet = 0
            else
                quiet = quiet + 1
            end if
        end do
        call require(captures == 0, trim(label)//' triggered an expensive capture')
    end subroutine assert_idle

end program test_change_watch_provider
