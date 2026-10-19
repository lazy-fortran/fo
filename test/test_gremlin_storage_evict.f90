program test_gremlin_storage_evict
    !! Storage retention over a cap reclaims exactly the dead, unused lanes and
    !! the stale-pinned generations; a live owner's lane, a lane with a running
    !! recorded process, a recently used lane, a leased generation, a generation
    !! a live lane names and a recently pinned generation stay and remain usable.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char, &
        c_null_ptr, c_ptr
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use fo_util, only: temporary_root, int_text
    use fo_cache, only: cache_file_digest, HASH_LEN
    use fo_fs, only: fs_make_dir, fs_write_text
    use fo_input_inventory, only: input_inventory_t, input_declaration_t, &
        input_inventory_discover
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_gremlin_state, only: gremlin_session_t, gremlin_session_acquire, &
        gremlin_session_release, gremlin_session_publish, gremlin_client_identity, &
        gremlin_generation_pin_at, gremlin_generation_register_lease_at, &
        gremlin_generation_lease_acquire_at, gremlin_lease_t, gremlin_lease_release
    use fo_gremlin_storage, only: gremlin_storage_evict, gremlin_storage_report_t
    use fo_process, only: process_getpid
    use fo_test_harness, only: set_mtime, remove_tree
    implicit none

    interface
        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
        integer(c_long_long) function c_time(t) bind(C, name='time')
            import :: c_long_long, c_ptr
            type(c_ptr), value :: t
        end function c_time
    end interface

    character(len=1024) :: root, state, gremlin, project, driver, message
    character(len=64) :: start
    character(len=256) :: names(2)
    character(len=HASH_LEN) :: driver_digest
    integer(int64) :: driver_size
    integer(c_long_long) :: old_ns
    integer :: ierr, pid
    type(input_declaration_t) :: declarations(0)
    type(generation_context_t) :: context
    type(generation_t) :: stale, leased, named, fresh
    type(gremlin_session_t) :: dead, crashed, running, live, recent, again
    type(gremlin_lease_t) :: lease, held
    type(gremlin_storage_report_t) :: report
    logical :: exists

    root = temporary_root()//'/fo-storage-evict-'//trim(int_text(process_getpid()))
    state = trim(root)//'/state'
    gremlin = trim(state)//'/fo/gremlin'
    project = trim(root)//'/project'
    driver = trim(root)//'/driver-image'
    call remove_tree(trim(root))
    call fs_make_dir(trim(project)//'/src')
    ierr = c_setenv('FO_GREMLIN_STATE_DIR'//c_null_char, trim(state)//c_null_char, &
        1_c_int)
    ierr = c_setenv('FO_CACHE_DIR'//c_null_char, trim(state)//'/fo'//c_null_char, &
        1_c_int)
    call fs_write_text(trim(project)//'/fpm.toml', 'name = "evict"')
    call fs_write_text(trim(driver), 'verified test driver image')
    call cache_file_digest(trim(driver), driver_digest)
    inquire (file=trim(driver), size=driver_size)
    context%toolchain = 'test-toolchain'
    context%flags = '-O0'
    context%environment = 'test-env'
    context%base_commit = 'base'
    context%patch_digest = 'patch'
    context%driver_path = trim(driver)
    context%driver_digest = driver_digest
    context%driver_size = driver_size
    old_ns = (c_time(c_null_ptr) - 3_c_long_long * 86400_c_long_long) * &
        1000000000_c_long_long

    ! Generations: four distinct captures, each registered and pinned.
    call capture(stale, 'a')
    call capture(leased, 'b')
    call capture(named, 'c')
    call capture(fresh, 'd')
    call gremlin_generation_lease_acquire_at(trim(leased%root), held, ierr, message)
    call require(ierr == 0, 'a generation lease is held: '//trim(message))
    call age_generation(stale)
    call age_generation(leased)
    call age_generation(named)

    ! Lanes. A gracefully stopped owner and a crashed owner are dead.
    call open_lane('dead', dead)
    call fs_write_text(trim(dead%state_dir)//'/logs/old.log', repeat('x', 4096))
    call gremlin_session_release(dead, ierr, message)
    call require(ierr == 0, 'release the stopped lane: '//trim(message))
    call age_lane(dead%state_dir, ['logs/old.log'])

    call open_lane('crashed', crashed)
    call gremlin_session_release(crashed, ierr, message)
    call fs_write_text(trim(crashed%state_dir)//'/owner', &
        'gone-session'//achar(10)//'2147480000'//achar(10)//'1-1'//achar(10))
    call age_lane(crashed%state_dir, ['owner'])

    ! A dead owner whose recorded async process (this test) still runs.
    call gremlin_client_identity(.false., pid, start, ierr)
    call require(ierr == 0, 'identify the test process')
    call open_lane('running', running)
    call gremlin_session_release(running, ierr, message)
    call fs_make_dir(trim(running%state_dir)//'/async-processes/7-7')
    call fs_write_text(trim(running%state_dir)//'/async-processes/7-7/'// &
        trim(int_text(pid))//'-'//trim(start)//'.session', 'record')
    names(1) = 'async-processes/7-7/'//trim(int_text(pid))//'-'//trim(start)// &
        '.session'
    names(2) = 'async-processes/7-7'
    call age_lane(running%state_dir, names)

    ! A live owner (this process holds its owner.lock) naming a generation.
    call open_lane('live', live)
    call gremlin_session_publish(live, '{"state":"idle","active_generation":"'// &
        trim(named%identity)//'"}', ierr, message)
    call require(ierr == 0, 'publish the live lane status: '//trim(message))
    call age_lane(live%state_dir, [character(len=16) :: 'status', 'owner'])

    call open_lane('recent', recent)
    call fs_write_text(trim(recent%state_dir)//'/logs/new.log', 'fresh')
    call gremlin_session_release(recent, ierr, message)

    call gremlin_storage_evict(trim(gremlin), huge(1_int64), 3600_int64, .false., &
        report, ierr, message)
    call require(ierr == 0 .and. report%total_bytes > 4096 .and. &
        report%lanes + report%generations == 0, 'under the cap nothing is reclaimed')

    call gremlin_storage_evict(trim(gremlin), 1_int64, 3600_int64, .true., report, &
        ierr, message)
    call require(ierr == 0, 'dry run: '//trim(message))
    call require(report%lanes == 2 .and. report%generations == 1, &
        'dry run finds the two dead lanes and the stale generation, got '// &
        trim(int_text(report%lanes))//' lanes '// &
        trim(int_text(report%generations))//' generations')
    call require(present_dir(dead%state_dir) .and. present_dir(stale%root), &
        'a dry run changes nothing')

    call gremlin_storage_evict(trim(gremlin), 1_int64, 3600_int64, .false., report, &
        ierr, message)
    call require(ierr == 0, 'eviction: '//trim(message))
    call require(report%lanes == 2 .and. report%generations == 1 .and. &
        report%kept == 0, 'eviction reclaims exactly the dead lanes and the '// &
        'stale generation')
    call require(.not. present_dir(dead%state_dir), 'the stopped lane is gone')
    call require(.not. present_dir(crashed%state_dir), 'the crashed lane is gone')
    call require(.not. present_dir(stale%root), 'the stale-pinned generation is gone')
    call require(present_dir(running%state_dir), &
        'a lane with a running recorded process stays')
    call require(present_dir(live%state_dir), 'the live lane stays')
    inquire (file=trim(recent%state_dir)//'/logs/new.log', exist=exists)
    call require(exists, 'the recently used lane stays with its log')
    call require(present_dir(leased%root) .and. present_dir(named%root) .and. &
        present_dir(fresh%root), 'leased, named and fresh generations stay')
    inquire (file=sidecar(named)//'/pinned', exist=exists)
    call require(exists, 'the named generation keeps its pin')
    inquire (file=sidecar(fresh)//'/pinned', exist=exists)
    call require(exists, 'the fresh generation keeps its pin')

    ! Still usable: the live owner publishes, the held lease releases, and an
    ! evicted lane starts again on a fresh directory.
    call gremlin_session_publish(live, '{"state":"idle"}', ierr, message)
    call require(ierr == 0, 'the live owner still publishes: '//trim(message))
    call gremlin_session_release(live, ierr, message)
    call require(ierr == 0, 'the live owner still releases: '//trim(message))
    call gremlin_lease_release(held, ierr, message)
    call require(ierr == 0, 'the held generation lease releases: '//trim(message))
    call gremlin_generation_lease_acquire_at(trim(named%root), lease, ierr, message)
    call require(ierr == 0, 'the named generation is still leasable: '//trim(message))
    call gremlin_lease_release(lease, ierr, message)
    call open_lane('dead', again)
    call gremlin_session_release(again, ierr, message)
    call require(ierr == 0, 'an evicted lane starts again: '//trim(message))

    call remove_tree(trim(root))
    write (*, '(a)') 'test_gremlin_storage_evict: PASS'

contains

    subroutine capture(generation, fill)
        type(generation_t), intent(out) :: generation
        character(len=1), intent(in) :: fill
        type(input_inventory_t) :: inventory

        call fs_write_text(trim(project)//'/src/a.f90', repeat(fill, 3000))
        call input_inventory_discover(trim(project), declarations, inventory, &
            ierr, message)
        call require(ierr == 0, 'inventory discovery: '//trim(message))
        context%input_inventory = inventory
        call generation_capture(trim(project), trim(state)//'/fo', context, &
            generation, ierr, message)
        call require(ierr == 0, 'generation capture: '//trim(message))
        call gremlin_generation_register_lease_at(trim(generation%root), lease, ierr, &
            message)
        call require(ierr == 0, 'register a generation: '//trim(message))
        call gremlin_lease_release(lease, ierr, message)
        call gremlin_generation_pin_at(trim(generation%root), .true., ierr, message)
        call require(ierr == 0, 'pin a generation: '//trim(message))
    end subroutine capture

    function sidecar(generation) result(path)
        type(generation_t), intent(in) :: generation
        character(len=:), allocatable :: path
        integer :: slash

        slash = index(trim(generation%root), '/', back=.true.)
        path = generation%root(:slash)//'.state/'//trim(generation%identity)
    end function sidecar

    subroutine age_generation(generation)
        type(generation_t), intent(in) :: generation

        call set_mtime(sidecar(generation)//'/registered', old_ns)
        call set_mtime(sidecar(generation)//'/pinned', old_ns)
        call set_mtime(sidecar(generation)//'/lease.lock', old_ns)
    end subroutine age_generation

    subroutine open_lane(lane, session)
        character(len=*), intent(in) :: lane
        type(gremlin_session_t), intent(out) :: session

        call gremlin_session_acquire(trim(project), lane, session, ierr, message)
        call require(ierr == 0 .and. session%owner, 'own lane '//lane//': '// &
            trim(message))
        call fs_make_dir(trim(session%state_dir)//'/logs')
    end subroutine open_lane

    subroutine age_lane(dir, entries)
        !! Age the lane's entries (owner.lock never counts as use).
        character(len=*), intent(in) :: dir
        character(len=*), intent(in) :: entries(:)
        integer :: i

        do i = 1, size(entries)
            call set_mtime(trim(dir)//'/'//trim(entries(i)), old_ns)
        end do
        call set_mtime(trim(dir)//'/logs', old_ns)
        inquire (file=trim(dir)//'/async-processes/.', exist=exists)
        if (exists) call set_mtime(trim(dir)//'/async-processes', old_ns)
    end subroutine age_lane

    logical function present_dir(path)
        character(len=*), intent(in) :: path

        inquire (file=trim(path)//'/.', exist=present_dir)
    end function present_dir

    subroutine require(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label

        if (condition) then
            write (*, '(a)') 'PASS: '//trim(label)
        else
            write (error_unit, '(a)') 'FAIL: '//trim(label)
            error stop 1
        end if
    end subroutine require

end program test_gremlin_storage_evict
