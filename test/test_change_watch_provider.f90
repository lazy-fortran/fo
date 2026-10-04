program test_change_watch_provider
    !! Public shared-provider behavior fixture, independent of Gremlin lifecycle.
    use, intrinsic :: iso_c_binding, only: c_int, c_long
    use fo_change_watch, only: change_watch_t, change_watch_init, &
        change_watch_add_root, change_watch_poll, change_watch_close, &
        CHANGE_RECONCILE
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree, fs_rename, &
        fs_sleep_ms, fs_write_text
    use fo_util, only: make_tmpfile
    implicit none

    type, bind(C) :: rlimit_t
        integer(c_long) :: current, maximum
    end type rlimit_t
    interface
        integer(c_int) function getrlimit(resource, limits) bind(C, name='getrlimit')
            import :: c_int, rlimit_t
            integer(c_int), value :: resource
            type(rlimit_t), intent(out) :: limits
        end function getrlimit
        integer(c_int) function setrlimit(resource, limits) bind(C, name='setrlimit')
            import :: c_int, rlimit_t
            integer(c_int), value :: resource
            type(rlimit_t), intent(in) :: limits
        end function setrlimit
    end interface

    type(change_watch_t) :: watch
    type(change_watch_t) :: diagnostic_watch
    type(rlimit_t) :: saved_limits, test_limits
    character(len=4096) :: scratch, project, dependency, ignored
    character(len=4096) :: nested, retired, changed, error_text, ignored_dir
    character(len=4096) :: latency_dir
    character(len=4096) :: created_file, renamed_file
    integer :: ierr, kind, captures, rename_error, clock_start, clock_end, clock_rate
    integer :: attempt
    logical :: got_event
    logical :: provider_quiet
    logical :: linux_inotify

    inquire(file='/proc/sys/fs/inotify/max_queued_events', exist=linux_inotify)
    if (linux_inotify) call verify_open_diagnostic()

    do attempt = 1, 12
        call change_watch_init(watch, '.', ierr, error_text)
        call require(ierr == 0, 'large project tree init: '//trim(error_text))
        call require(len_trim(error_text) == 0, &
            'successful provider init returned a false-positive diagnostic')
        call change_watch_close(watch)
    end do

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
    call system_clock(count=clock_start, count_rate=clock_rate)
    call change_watch_poll(watch, 300, changed, kind, got_event, ierr)
    call system_clock(count=clock_end)
    call require(ierr == 0, 'idle poll returned an error')
    call require(.not. got_event, 'idle provider emitted a change')
    call require(captures == 0, 'idle provider caused an expensive capture')
    call require((clock_end - clock_start)*1000.0/real(max(1, clock_rate)) < 400.0, &
        'idle poll exceeded its timeout plus scheduling allowance')

    latency_dir = trim(project)//'/latency-probe'
    call fs_make_dir(trim(latency_dir))
    call fs_sleep_ms(50)
    call assert_zero_poll_latency('pending directory event')
    call fs_sleep_ms(150)
    call system_clock(count=clock_start, count_rate=clock_rate)
    call change_watch_poll(watch, 0, changed, kind, got_event, ierr, provider_quiet)
    call system_clock(count=clock_end)
    call require(ierr == 0, 'expired dirty-state zero-timeout poll failed')
    call require(.not. got_event, &
        'zero-timeout poll reconciled after the quiet deadline')
    call require(.not. provider_quiet, &
        'pending reconciliation was reported quiet after its deadline')
    call require((clock_end - clock_start)*1000.0/real(max(1, clock_rate)) < 50.0, &
        'expired dirty-state zero-timeout poll blocked')
    call change_watch_poll(watch, 300, changed, kind, got_event, ierr)
    call require(ierr == 0, 'positive-budget dirty-state poll failed')
    call require(got_event .and. kind == CHANGE_RECONCILE, &
        'positive-budget poll did not complete pending reconciliation')
    captures = captures + 1
    call settle_events()
    captures = 0

    call fs_write_text(trim(created_file), 'program fresh')
    call await_path_nonblocking(trim(created_file))
    call settle_events()
    call fs_write_text(trim(created_file), 'program changed')
    call await_path(trim(created_file))
    call settle_events()
    rename_error = fs_rename(trim(created_file), trim(renamed_file))
    call require(rename_error == 0, 'cannot rename watched file')
    call await_rename(trim(created_file), trim(renamed_file))
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

    captures = 0
    do attempt = 1, 3
        select case (attempt)
        case (1)
            ignored_dir = trim(project)//'/.git'
        case (2)
            ignored_dir = trim(project)//'/.gremlin'
        case (3)
            ignored_dir = trim(project)//'/build'
        end select
        call fs_make_dir(trim(ignored_dir))
        call fs_write_text(trim(ignored_dir)//'/discard.f90', 'program ignored')
        call fs_make_dir(trim(ignored_dir)//'/nested')
        call assert_zero_poll_latency('excluded '//trim(ignored_dir))
        rename_error = fs_rename(trim(ignored_dir)//'/nested', &
            trim(ignored_dir)//'/renamed')
        call require(rename_error == 0, 'cannot rename excluded directory')
        call assert_idle('excluded rename '//trim(ignored_dir))
    end do

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

    subroutine verify_open_diagnostic()
        integer(c_int) :: status

        status = getrlimit(7_c_int, saved_limits)
        if (status /= 0) error stop 'cannot read descriptor limit for diagnostic oracle'
        test_limits = saved_limits
        test_limits%current = 0_c_long
        status = setrlimit(7_c_int, test_limits)
        if (status /= 0) error stop 'cannot lower descriptor limit for diagnostic oracle'
        call change_watch_init(diagnostic_watch, '.', ierr, error_text)
        status = setrlimit(7_c_int, saved_limits)
        call change_watch_close(diagnostic_watch)
        if (status /= 0) error stop 'cannot restore descriptor limit after diagnostic oracle'
        if (ierr == 0) error stop 'descriptor exhaustion did not fail provider initialization'
        if (index(trim(error_text), 'initialize declared root "."') == 0) &
            error stop 'provider diagnostic lost its declared root context'
        if (index(trim(error_text), 'open inotify provider') == 0) &
            error stop 'provider diagnostic lost its open operation: '//trim(error_text)
        if (index(trim(error_text), 'errno 24') == 0) &
            error stop 'provider diagnostic lost the exact OS error'
    end subroutine verify_open_diagnostic

    subroutine await_path(expected)
        character(len=*), intent(in) :: expected

        do attempt = 1, 40
            call change_watch_poll(watch, 100, changed, kind, got_event, ierr)
            call require(ierr == 0, 'event poll returned an error')
            if (.not. got_event) cycle
            captures = captures + 1
            if (trim(changed) == trim(expected)) return
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
                if (trim(changed) == trim(expected)) return
            end if
            call fs_sleep_ms(50)
        end do
        call require(.false., 'nonblocking poll timed out waiting for '//trim(expected))
    end subroutine await_path_nonblocking

    subroutine await_rename(old_path, new_path)
        character(len=*), intent(in) :: old_path, new_path
        integer :: attempt
        logical :: new_exists, old_exists

        do attempt = 1, 40
            call change_watch_poll(watch, 100, changed, kind, got_event, ierr)
            call require(ierr == 0, 'rename poll returned an error')
            if (.not. got_event) cycle
            captures = captures + 1
            if (trim(changed) == trim(new_path)) return
            if (kind /= CHANGE_RECONCILE) cycle
            inquire(file=trim(new_path), exist=new_exists)
            inquire(file=trim(old_path), exist=old_exists)
            if (new_exists .and. .not. old_exists) return
        end do
        call require(.false., 'timed out waiting for verified rename')
    end subroutine await_rename

    subroutine assert_zero_poll_latency(label)
        character(len=*), intent(in) :: label
        integer :: attempt, start_count, end_count, rate

        do attempt = 1, 4
            call system_clock(count=start_count, count_rate=rate)
            call change_watch_poll(watch, 0, changed, kind, got_event, ierr)
            call system_clock(count=end_count)
            call require(ierr == 0, trim(label)//' zero-timeout poll failed')
            call require((end_count - start_count)*1000.0/real(max(1, rate)) < 50.0, &
                trim(label)//' zero-timeout poll blocked')
            if (got_event) captures = captures + 1
        end do
    end subroutine assert_zero_poll_latency

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
