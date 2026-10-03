program test_gremlin_generation
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char, c_int, c_size_t
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_fs, only: fs_make_dir, fs_rename, fs_write_text
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_process, only: process_getpid
    implicit none

    interface
        subroutine initialize_directory_oracle() &
                bind(C, name='generation_oracle_initialize')
        end subroutine initialize_directory_oracle
        subroutine arm_directory_oracle(mode) bind(C, name='generation_oracle_arm')
            import :: c_int
            integer(c_int), intent(in), value :: mode
        end subroutine arm_directory_oracle
        integer(c_int) function directory_oracle_state(which) &
                bind(C, name='generation_oracle_state')
            import :: c_int
            integer(c_int), intent(in), value :: which
        end function directory_oracle_state
        subroutine release_directory_oracle(swapped) &
                bind(C, name='generation_oracle_release')
            import :: c_int
            integer(c_int), intent(in), value :: swapped
        end subroutine release_directory_oracle

        integer(c_int) function fo_c_generation_list_tree(root, manifest) &
                bind(C, name='fo_c_generation_list_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), manifest(*)
        end function fo_c_generation_list_tree

        integer(c_int) function fo_c_generation_remove_stage(path) &
                bind(C, name='fo_c_generation_remove_stage')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fo_c_generation_remove_stage

        integer(c_int) function c_symlink(target, linkpath) &
                bind(C, name='symlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: target(*), linkpath(*)
        end function c_symlink

        integer(c_int) function c_unlink(path) bind(C, name='unlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_unlink

        integer(c_int) function c_rename(oldpath, newpath) bind(C, name='rename')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: oldpath(*), newpath(*)
        end function c_rename

        integer(c_int) function c_usleep(usec) bind(C, name='usleep')
            import :: c_int
            integer(c_int), value :: usec
        end function c_usleep

        integer(c_size_t) function c_readlink(path, buffer, buffer_size) &
                bind(C, name='readlink')
            import :: c_char, c_size_t
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: buffer_size
        end function c_readlink

        integer(c_int) function c_access(path, mode) bind(C, name='access')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_access

        integer(c_int) function c_chmod(path, mode) bind(C, name='chmod')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_chmod
    end interface

    integer :: n_pass, n_fail, race_ierr, generation_count
    integer(c_int), parameter :: WRITE_ACCESS = 2_c_int
    integer(c_int), parameter :: EXEC_ACCESS = 1_c_int
    integer(c_int), parameter :: MODE_NONEXEC = 420_c_int, MODE_EXEC = 493_c_int
    character(len=512) :: root, project, project_link, dependency, cache
    character(len=512) :: source_link, escaping_link, external_target
    character(len=512) :: excluded_link
    character(len=512) :: alternate_source
    character(len=512) :: concurrent_cache, race_project, source_file
    character(len=512) :: link_race_project, outside_source, link_race_cache
    character(len=256) :: message
    type(generation_context_t) :: context, race_context
    type(generation_t) :: first, reused, source_changed, include_changed
    type(generation_t) :: dependency_changed, build_source_changed
    type(generation_t) :: metadata_changed, race_generation
    type(generation_t) :: mode_changed, mode_restored
    type(generation_t) :: parallel_one, parallel_two
    logical :: exists, atomic_edit_observed
    integer :: parallel_ierr_one, parallel_ierr_two
    integer :: parent_swap_count
    character(len=256) :: parallel_message_one, parallel_message_two
    integer(c_int) :: remove_rc, symlink_rc, escaping_rc, chmod_rc

    n_pass = 0
    n_fail = 0
    call initialize_directory_oracle()
    root = '/var/tmp/fo-gremlin-generation-'//int_text(process_getpid())
    project = trim(root)//'/project'
    dependency = trim(root)//'/fortfront'
    cache = trim(root)//'/cache'
    remove_rc = fo_c_generation_remove_stage(trim(root)//c_null_char)
    call check(remove_rc == 0, 'previous read-only fixture was removed')
    call fs_make_dir(trim(project)//'/src')
    call fs_make_dir(trim(project)//'/src/build')
    call fs_make_dir(trim(project)//'/include')
    call fs_make_dir(trim(project)//'/build')
    call fs_make_dir(trim(project)//'/.gremlin')
    call fs_make_dir(trim(project)//'/.git')
    call fs_make_dir(trim(project)//'/.hg')
    call fs_make_dir(trim(project)//'/.svn')
    call fs_make_dir(trim(project)//'/.bzr')
    call fs_make_dir(trim(dependency)//'/src')
    call fs_write_text(trim(project)//'/src/main.f90', 'program version_one')
    alternate_source = trim(project)//'/src/alternate.f90'
    call fs_write_text(trim(alternate_source), 'program version_one')
    call fs_write_text(trim(project)//'/src/build/fo_gremlin_provider.f90', &
        'module generation_provider_v1')
    call fs_write_text(trim(project)//'/include/config.inc', 'include_one')
    call fs_write_text(trim(project)//'/build/old.o', 'transient')
    call fs_write_text(trim(project)//'/.gremlin/state', 'transient')
    call fs_write_text(trim(project)//'/.git/config', 'metadata')
    call fs_write_text(trim(project)//'/.hg/store', 'metadata')
    call fs_write_text(trim(project)//'/.svn/wc.db', 'metadata')
    call fs_write_text(trim(project)//'/.bzr/branch', 'metadata')
    call fs_write_text(trim(dependency)//'/src/runtime.f90', 'runtime_one')
    source_link = trim(project)//'/src/source-alias.f90'
    symlink_rc = c_symlink('main.f90'//c_null_char, &
        trim(source_link)//c_null_char)
    call check(symlink_rc == 0, 'in-tree relative file link fixture is created')

    context%toolchain = 'gfortran 14 test'
    context%flags = '-O0 -g'
    context%environment = 'OMP_NUM_THREADS=1'
    context%base_commit = 'base-commit'
    context%patch_digest = 'uncommitted-patch-digest'
    allocate (context%inputs(1))
    context%inputs(1)%label = 'path-dependency:../fortfront'
    context%inputs(1)%source_root = trim(dependency)
    context%inputs(1)%destination = '../fortfront'

    call generation_capture(trim(project), trim(cache), context, first, &
        race_ierr, message)
    call check(race_ierr == 0, &
        'capture publishes an immutable generation with an in-tree file link')
    if (race_ierr /= 0) write (error_unit, '(a)') trim(message)
    project_link = trim(root)//'/project-link'
    symlink_rc = c_symlink(trim(project)//c_null_char, &
        trim(project_link)//c_null_char)
    call check(symlink_rc == 0, 'symlink fixture is created')
    if (symlink_rc == 0) then
        call generation_capture(trim(project_link), trim(cache), context, reused, &
            race_ierr, message)
        call check(race_ierr /= 0, 'a symlink input root is rejected')
    end if
    concurrent_cache = trim(root)//'/parallel-cache'
    call run_parallel_captures(trim(project), trim(concurrent_cache), context, &
        parallel_one, parallel_ierr_one, parallel_message_one, parallel_two, &
        parallel_ierr_two, parallel_message_two)
    call check(parallel_ierr_one == 0 .and. parallel_ierr_two == 0 .and. &
        parallel_one%identity == parallel_two%identity, &
        'concurrent identical captures return one generation identity')
    generation_count = count_generation_roots(trim(concurrent_cache))
    call check(generation_count == 1, &
        'concurrent publication stores one validated generation')
    if (race_ierr == 0) then
        call check(c_access(trim(first%root)//'/identity.txt'//c_null_char, &
            WRITE_ACCESS) /= 0, 'published generation metadata is read-only')
        call check(c_access(trim(first%root)//'/bundle'//c_null_char, &
            WRITE_ACCESS) /= 0, 'published input bundle directory is read-only')
        call check(c_access(trim(first%project_root)//'/src/main.f90'// &
            c_null_char, WRITE_ACCESS) /= 0, &
            'published project inputs are read-only')
        call check(file_equals(trim(first%project_root)//'/src/main.f90', &
            'program version_one'), 'snapshot preserves original source')
        call check(file_equals(trim(first%project_root)// &
            '/src/source-alias.f90', &
            'program version_one'), &
            'snapshot resolves a relative sibling link to its captured target')
        call check(link_target_equals(trim(first%project_root)// &
            '/src/source-alias.f90', 'main.f90'), &
            'snapshot preserves the raw relative link target')
        call check(file_equals(trim(first%project_root)// &
            '/src/build/fo_gremlin_provider.f90', &
            'module generation_provider_v1'), &
            'snapshot preserves source below src/build')
        call check(c_access(trim(first%project_root)// &
            '/src/build/fo_gremlin_provider.f90'//c_null_char, &
            WRITE_ACCESS) /= 0, 'captured src/build source is read-only')
        call check(file_equals(trim(first%project_root)//'/include/config.inc', &
            'include_one'), 'snapshot preserves included input')
        call check(file_equals(trim(first%project_root)// &
            '/../fortfront/src/runtime.f90', 'runtime_one'), &
            'snapshot preserves sibling path dependency')
        inquire (file=trim(first%project_root)//'/build/old.o', exist=exists)
        call check(.not. exists, 'build outputs are excluded from the snapshot')
        inquire (file=trim(first%project_root)//'/.gremlin/state', exist=exists)
        call check(.not. exists, 'Gremlin outputs are excluded from the snapshot')
        inquire (file=trim(first%project_root)//'/.git/config', exist=exists)
        call check(.not. exists, 'Git metadata is excluded from the snapshot')
        inquire (file=trim(first%project_root)//'/.hg/store', exist=exists)
        call check(.not. exists, 'Mercurial metadata is excluded from the snapshot')
        inquire (file=trim(first%project_root)//'/.svn/wc.db', exist=exists)
        call check(.not. exists, 'Subversion metadata is excluded from the snapshot')
        inquire (file=trim(first%project_root)//'/.bzr/branch', exist=exists)
        call check(.not. exists, 'Bazaar metadata is excluded from the snapshot')
        call fs_write_text(trim(first%project_root)//'/build/probe.txt', 'writable')
        call check(file_equals(trim(first%project_root)//'/build/probe.txt', &
            'writable'), 'excluded build directory remains writable')
    end if

    call generation_capture(trim(project), trim(cache), context, reused, &
        race_ierr, message)
    call check(race_ierr == 0 .and. reused%identity == first%identity .and. &
        trim(reused%root) == trim(first%root), &
        'unchanged inputs reuse the existing content-addressed generation')
    generation_count = count_generation_roots(trim(cache))
    call check(generation_count == 1, &
        'unchanged recapture does not grow the generation store')

    remove_rc = c_unlink(trim(source_link)//c_null_char)
    call check(remove_rc == 0, 'original relative link is removed')
    symlink_rc = c_symlink('alternate.f90'//c_null_char, &
        trim(source_link)//c_null_char)
    call check(symlink_rc == 0, 'same-content alternate link is created')
    call generation_capture(trim(project), trim(cache), context, reused, &
        race_ierr, message)
    call check(race_ierr == 0 .and. reused%identity /= first%identity, &
        'changing raw link text changes generation identity for equal content')
    call check(link_target_equals(trim(reused%project_root)// &
        '/src/source-alias.f90', 'alternate.f90'), &
        'snapshot retains the changed raw link text')
    remove_rc = c_unlink(trim(source_link)//c_null_char)
    call check(remove_rc == 0, 'alternate relative link is removed')
    symlink_rc = c_symlink('main.f90'//c_null_char, &
        trim(source_link)//c_null_char)
    call check(symlink_rc == 0, 'original relative link is restored')

    call fs_write_text(trim(project)//'/src/main.f90', 'program version_two')
    call generation_capture(trim(project), trim(cache), context, source_changed, &
        race_ierr, message)
    call check(race_ierr == 0 .and. source_changed%identity /= first%identity, &
        'source changes invalidate generation identity')
    call check(file_equals(trim(first%project_root)//'/src/main.f90', &
        'program version_one'), 'live source edit leaves old snapshot unchanged')
    call check(file_equals(trim(first%project_root)//'/src/source-alias.f90', &
        'program version_one'), &
        'changing the link target leaves captured link content unchanged')
    call check(file_equals(trim(source_changed%project_root)// &
        '/src/source-alias.f90', 'program version_two'), &
        'new generation captures the current relative-link target')

    external_target = trim(root)//'/outside-input.txt'
    escaping_link = trim(project)//'/escaping-alias'
    call fs_write_text(trim(external_target), 'external')
    escaping_rc = c_symlink('../outside-input.txt'//c_null_char, &
        trim(escaping_link)//c_null_char)
    call check(escaping_rc == 0, 'escaping relative link fixture is created')
    call generation_capture(trim(project), trim(cache), context, reused, &
        race_ierr, message)
    call check(race_ierr /= 0, &
        'relative file link resolving outside the input root is rejected')
    remove_rc = c_unlink(trim(escaping_link)//c_null_char)
    call check(remove_rc == 0, 'escaping link fixture is removed')

    excluded_link = trim(project)//'/build-alias'
    escaping_rc = c_symlink('build/old.o'//c_null_char, &
        trim(excluded_link)//c_null_char)
    call check(escaping_rc == 0, 'excluded-output link fixture is created')
    generation_count = count_generation_roots(trim(cache))
    call generation_capture(trim(project), trim(cache), context, reused, &
        race_ierr, message)
    call check(race_ierr /= 0, &
        'a link to an excluded root build output is rejected')
    call check(count_generation_roots(trim(cache)) == generation_count, &
        'rejecting an excluded-output link publishes no generation')
    remove_rc = c_unlink(trim(excluded_link)//c_null_char)
    call check(remove_rc == 0, 'excluded-output link fixture is removed')

    call check_directory_read_errors()
    call check_trailing_space_paths()

    call fs_write_text(trim(project)//'/include/config.inc', 'include_two')
    call generation_capture(trim(project), trim(cache), context, include_changed, &
        race_ierr, message)
    call check(race_ierr == 0 .and. &
        include_changed%identity /= source_changed%identity, &
        'include changes invalidate generation identity')

    call fs_write_text(trim(dependency)//'/src/runtime.f90', 'runtime_two')
    call generation_capture(trim(project), trim(cache), context, &
        dependency_changed, race_ierr, message)
    call check(race_ierr == 0 .and. &
        dependency_changed%identity /= include_changed%identity, &
        'path dependency changes invalidate generation identity')

    call fs_write_text(trim(project)//'/src/build/fo_gremlin_provider.f90', &
        'module generation_provider_v2')
    call generation_capture(trim(project), trim(cache), context, &
        build_source_changed, race_ierr, message)
    call check(race_ierr == 0 .and. &
        build_source_changed%identity /= dependency_changed%identity, &
        'src/build source changes invalidate generation identity')
    call check(file_equals(trim(build_source_changed%project_root)// &
        '/src/build/fo_gremlin_provider.f90', &
        'module generation_provider_v2'), &
        'new generation contains the edited src/build source')
    call check(file_equals(trim(first%project_root)// &
        '/src/build/fo_gremlin_provider.f90', &
        'module generation_provider_v1'), &
        'src/build edit leaves the older snapshot unchanged')

    source_file = trim(project)//'/src/main.f90'
    chmod_rc = c_chmod(trim(source_file)//c_null_char, MODE_EXEC)
    call check(chmod_rc == 0, 'source executable mode is set')
    call generation_capture(trim(project), trim(cache), context, mode_changed, &
        race_ierr, message)
    call check(race_ierr == 0 .and. &
        mode_changed%identity /= build_source_changed%identity, &
        'source executable mode changes invalidate generation identity')
    call check(c_access(trim(mode_changed%project_root)//'/src/main.f90'// &
        c_null_char, EXEC_ACCESS) == 0, &
        'snapshot preserves the source executable bit')
    call check(c_access(trim(dependency_changed%project_root)// &
        '/src/main.f90'//c_null_char, EXEC_ACCESS) /= 0, &
        'older snapshot preserves its original non-executable mode')
    chmod_rc = c_chmod(trim(source_file)//c_null_char, MODE_NONEXEC)
    call check(chmod_rc == 0, 'source non-executable mode is restored')
    call generation_capture(trim(project), trim(cache), context, mode_restored, &
        race_ierr, message)
    call check(race_ierr == 0 .and. &
        mode_restored%identity == build_source_changed%identity, &
        'restored file mode reuses its unchanged generation')

    context%inputs(1)%source_root = ''
    call generation_capture(trim(project), trim(cache), context, metadata_changed, &
        race_ierr, message)
    call check(race_ierr /= 0, 'empty path dependency roots are rejected')
    context%inputs(1)%source_root = trim(dependency)

    context%toolchain = 'gfortran 15 test'
    call generation_capture(trim(project), trim(cache), context, metadata_changed, &
        race_ierr, message)
    call check(race_ierr == 0 .and. &
        metadata_changed%identity /= build_source_changed%identity, &
        'toolchain changes invalidate generation identity')
    context%toolchain = 'gfortran 14 test'
    context%flags = '-O2 -g'
    call generation_capture(trim(project), trim(cache), context, metadata_changed, &
        race_ierr, message)
    call check(race_ierr == 0 .and. &
        metadata_changed%identity /= build_source_changed%identity, &
        'compiler flag changes invalidate generation identity')
    context%flags = '-O0 -g'
    context%environment = 'OMP_NUM_THREADS=2'
    call generation_capture(trim(project), trim(cache), context, metadata_changed, &
        race_ierr, message)
    call check(race_ierr == 0 .and. &
        metadata_changed%identity /= build_source_changed%identity, &
        'environment changes invalidate generation identity')
    context%environment = 'OMP_NUM_THREADS=1'
    context%base_commit = 'different-base'
    call generation_capture(trim(project), trim(cache), context, metadata_changed, &
        race_ierr, message)
    call check(race_ierr == 0 .and. &
        metadata_changed%identity /= build_source_changed%identity, &
        'base commit changes invalidate generation identity')
    context%base_commit = 'base-commit'
    context%patch_digest = 'different-patch-digest'
    call generation_capture(trim(project), trim(cache), context, metadata_changed, &
        race_ierr, message)
    call check(race_ierr == 0 .and. &
        metadata_changed%identity /= build_source_changed%identity, &
        'patch digest changes invalidate generation identity')

    race_project = trim(root)//'/race-project'
    call fs_make_dir(trim(race_project)//'/src')
    call write_version_file(trim(race_project)//'/src/value.dat', 'A', &
        32 * 1024 * 1024)
    race_context%toolchain = 'gfortran race fixture'
    race_context%flags = '-O0'
    race_context%base_commit = 'race-base'
    race_context%patch_digest = 'race-patch'
    call run_capture_during_atomic_edits(trim(race_project), trim(cache), &
        race_context, race_generation, race_ierr, message, &
        atomic_edit_observed)
    call check(atomic_edit_observed, &
        'atomic source edit followed the completed first staged copy')
    call check(file_is_byte_value(trim(race_project)//'/src/value.dat', 'H', &
        32 * 1024 * 1024), &
        'concurrent editor completed all atomic source revisions')
    call check(race_ierr /= 0, &
        'capture rejects input that changes during concurrent atomic edits')
    inquire (file=trim(race_generation%root), exist=exists)
    call check(.not. exists, &
        'unstable input capture does not publish a generation')

    link_race_project = trim(root)//'/link-race-project'
    outside_source = trim(root)//'/outside-source'
    link_race_cache = trim(root)//'/link-race-cache'
    call fs_make_dir(trim(link_race_project)//'/src')
    call fs_make_dir(trim(outside_source))
    call write_version_file(trim(link_race_project)//'/src/value.dat', 'I')
    call write_version_file(trim(outside_source)//'/value.dat', 'X')
    call write_version_file(trim(outside_source)//'/alias.dat', 'X')
    call write_version_file(trim(link_race_project)// &
        '/src/.directory-oracle-marker', 'P', 32)
    call write_version_file(trim(link_race_project)// &
        '/.directory-oracle-marker', 'R', 32)
    call write_version_file(trim(outside_source)// &
        '/.directory-oracle-marker', 'X', 32)
    symlink_rc = c_symlink('value.dat'//c_null_char, &
        (trim(link_race_project)//'/src/alias.dat')//c_null_char)
    call check(symlink_rc == 0, 'descriptor race link fixture is created')
    race_context%toolchain = 'gfortran descriptor race fixture'
    race_context%flags = '-O0'
    race_context%base_commit = 'descriptor-race-base'
    race_context%patch_digest = 'descriptor-race-patch'
    call run_capture_during_parent_swaps(trim(link_race_project), &
        trim(link_race_cache), race_context, &
        race_generation, race_ierr, message, parent_swap_count)
    call check(parent_swap_count > 0, &
        'source parent was replaced by an escaping symlink during capture')
    call check(directory_oracle_state(4_c_int) == 1, &
        'pause matched the unique marker through the already-open src descriptor')
    call check(directory_oracle_state(2_c_int) == 1, &
        'src traversal pause was observed within the bounded wait')
    call check(directory_oracle_state(3_c_int) == 1, &
        'src traversal resumed only after the completed parent swap')
    call check(directory_oracle_state(5_c_int) == 0, &
        'src traversal handshake completed before its release timeout')
    if (race_ierr == 0) then
        call check(file_is_byte_value(trim(race_generation%project_root)// &
            '/src/value.dat', 'I'), &
            'descriptor-rooted capture never copies external replacement data')
        call check(link_target_equals(trim(race_generation%project_root)// &
            '/src/alias.dat', 'value.dat'), &
            'descriptor-rooted snapshot preserves the in-tree link')
    else
        inquire (file=trim(race_generation%root), exist=exists)
        call check(.not. exists, &
            'rejected parent-swap capture publishes no generation')
        call check(count_generation_roots(trim(link_race_cache)) == 0, &
            'rejected parent-swap capture leaves no published generation')
    end if
    call check(.not. has_staging_entries(trim(link_race_cache)), &
        'parent-swap capture leaves no partial staging artifacts')

    call check(.not. has_staging_entries(trim(cache)), &
        'successful and rejected captures leave no staging artifacts')
    generation_count = count_generation_roots(trim(cache))
    call check(generation_count <= 12, &
        'disk use tracks unique input generations rather than capture attempts')
    remove_rc = fo_c_generation_remove_stage(trim(root)//c_null_char)
    call check(remove_rc == 0, 'fixture data and generation CAS are cleaned up')
    write (output_unit, '(a,i0,a,i0)') 'gremlin generation: ', n_pass, &
        ' pass, ', n_fail
    if (n_fail > 0) then
        write (error_unit, '(a)') 'gremlin generation fixture failed'
        stop 1
    end if

contains

    subroutine check_directory_read_errors()
        integer(c_int) :: rc
        integer :: before
        character(len=512) :: manifest
        manifest = trim(root)//'/io-error.list'
        call arm_directory_oracle(1_c_int)
        rc = fo_c_generation_list_tree(trim(project)//c_null_char, &
            trim(manifest)//c_null_char)
        call check(directory_oracle_state(1_c_int) == 1, &
            'directory reader delivered real entries before injected EIO')
        call check(rc == 5, 'directory inventory propagates readdir EIO')
        before = count_generation_roots(trim(cache))
        call arm_directory_oracle(1_c_int)
        call generation_capture(trim(project), trim(cache), context, &
            race_generation, race_ierr, message)
        call check(directory_oracle_state(1_c_int) == 1 .and. race_ierr /= 0, &
            'capture rejects directory I/O failure after partial enumeration')
        call check(count_generation_roots(trim(cache)) == before, &
            'directory I/O failure publishes no generation')
        call check(.not. has_staging_entries(trim(cache)), &
            'directory I/O failure removes partial staging data')
    end subroutine check_directory_read_errors

    subroutine check_trailing_space_paths()
        character(len=512) :: space_project, space_cache, manifest
        character(len=32) :: names(3)
        type(generation_context_t) :: space_context
        type(generation_t) :: baseline, rejected
        integer :: i, rc, count_before
        integer(c_int) :: rename_rc, list_rc, link_rc
        space_project = trim(root)//'/space-project'
        space_cache = trim(root)//'/space-cache'
        manifest = trim(root)//'/space.list'
        call fs_make_dir(trim(space_project)//'/nested')
        call fs_write_text(trim(space_project)//'/target', 'same bytes')
        call fs_write_text(trim(space_project)//'/extra', 'same bytes')
        call fs_write_text(trim(space_project)//'/nested/child', 'same bytes')
        link_rc = c_symlink('target'//c_null_char, &
            trim(space_project)//'/alias'//c_null_char)
        call check(link_rc == 0, 'trailing-space link oracle fixture is created')
        space_context%toolchain = 'space-path-oracle'
        call generation_capture(trim(space_project), trim(space_cache), &
            space_context, baseline, rc, message)
        call check(rc == 0, 'ordinary path spellings publish a baseline')
        names = [character(len=32) :: 'alias', 'extra', 'nested']
        do i = 1, size(names)
            rename_rc = c_rename(trim(space_project)//'/'//trim(names(i))// &
                c_null_char, trim(space_project)//'/'//trim(names(i))//' '// &
                c_null_char)
            call check(rename_rc == 0, 'name gains an exact trailing space')
            list_rc = fo_c_generation_list_tree(trim(space_project)// &
                c_null_char, trim(manifest)//c_null_char)
            call check(list_rc /= 0, &
                'inventory rejects trailing-space '//trim(names(i)))
            count_before = count_generation_roots(trim(space_cache))
            call generation_capture(trim(space_project), trim(space_cache), &
                space_context, rejected, rc, message)
            call check(rc /= 0, &
                'capture rejects trailing-space '//trim(names(i)))
            call check(count_generation_roots(trim(space_cache)) == count_before, &
                'trailing-space rejection publishes no aliased identity')
            call check(.not. has_staging_entries(trim(space_cache)), &
                'trailing-space rejection removes partial staging data')
            rename_rc = c_rename(trim(space_project)//'/'//trim(names(i))//' '// &
                c_null_char, trim(space_project)//'/'//trim(names(i))//c_null_char)
            call check(rename_rc == 0, 'exact ordinary path spelling is restored')
        end do
    end subroutine check_trailing_space_paths

    subroutine check(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', description
        end if
    end subroutine check

    subroutine run_capture_during_atomic_edits(project_path, cache_path, &
            capture_context, generation, ierr, error_message, edit_observed)
        character(len=*), intent(in) :: project_path, cache_path
        type(generation_context_t), intent(in) :: capture_context
        type(generation_t), intent(out) :: generation
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: error_message
        logical, intent(out) :: edit_observed
        character(len=512) :: data_path
        integer(c_int) :: ignored_wait

        data_path = trim(project_path)//'/src/value.dat'
        !$omp parallel sections num_threads(2) shared(generation, ierr, error_message)
        !$omp section
        call generation_capture(project_path, cache_path, capture_context, &
            generation, ierr, error_message)
        !$omp section
        edit_observed = wait_for_completed_stage_copy(cache_path, &
            'src/value.dat', 'A', 32 * 1024 * 1024)
        if (edit_observed) call write_version_file(data_path, 'H', &
            32 * 1024 * 1024)
        !$omp end parallel sections
    end subroutine run_capture_during_atomic_edits

    logical function wait_for_completed_stage_copy(cache_path, relative_file, &
            expected, n_bytes)
        character(len=*), intent(in) :: cache_path, relative_file, expected
        integer, intent(in) :: n_bytes
        character(len=512) :: manifest, record, staged_file
        integer :: attempt, unit, ios
        integer(c_int) :: rc, ignored_wait
        wait_for_completed_stage_copy = .false.
        manifest = trim(root)//'/atomic-stage.list'
        do attempt = 1, 5000
            rc = fo_c_generation_list_tree(trim(cache_path)// &
                '/gremlin/generations/.capture'//c_null_char, &
                trim(manifest)//c_null_char)
            if (rc == 0) then
                open (newunit=unit, file=trim(manifest), status='old', &
                    action='read', iostat=ios)
                if (ios == 0) then
                    do
                        read (unit, '(a)', iostat=ios) record
                        if (ios /= 0) exit
                        if (index(trim(record), &
                            '/stage/bundle/project/'//trim(relative_file)) == 0) &
                            cycle
                        staged_file = trim(cache_path)// &
                            '/gremlin/generations/.capture/'//trim(record(7:))
                        if (file_is_byte_value(trim(staged_file), expected, &
                            n_bytes)) then
                            wait_for_completed_stage_copy = .true.
                            exit
                        end if
                    end do
                    close (unit, status='delete')
                end if
            end if
            if (wait_for_completed_stage_copy) return
            ignored_wait = c_usleep(1000_c_int)
        end do
    end function wait_for_completed_stage_copy

    subroutine run_capture_during_parent_swaps(project_path, cache_path, &
            capture_context, generation, ierr, error_message, &
            swap_count)
        character(len=*), intent(in) :: project_path, cache_path
        type(generation_context_t), intent(in) :: capture_context
        type(generation_t), intent(out) :: generation
        integer, intent(out) :: ierr, swap_count
        character(len=*), intent(out) :: error_message
        character(len=512) :: source_dir, parked_dir
        integer :: iteration, rename_rc, link_rc, ignored
        logical :: paused

        source_dir = trim(project_path)//'/src'
        parked_dir = trim(project_path)//'/src-parked'
        swap_count = 0
        call arm_directory_oracle(2_c_int)
        !$omp parallel sections num_threads(2) &
        !$omp& shared(generation, ierr, error_message, swap_count)
        !$omp section
        call generation_capture(project_path, cache_path, capture_context, &
            generation, ierr, error_message)
        !$omp section
        ! Pause at EOF of the pinned src descriptor, after it was opened and
        ! enumerated but before its saved child names are looked up.
        do iteration = 1, 5000
            paused = directory_oracle_state(2_c_int) == 1
            if (paused) exit
            ignored = c_usleep(1000_c_int)
        end do
        if (paused) then
            rename_rc = c_rename(trim(source_dir)//c_null_char, &
                trim(parked_dir)//c_null_char)
            if (rename_rc == 0) then
                link_rc = c_symlink('../outside-source'//c_null_char, &
                    trim(source_dir)//c_null_char)
                if (link_rc == 0) swap_count = 1
            end if
        end if
        call release_directory_oracle(int(swap_count, c_int))
        !$omp end parallel sections
        ! Hold the replacement through capture's completion, including lookup.
        if (swap_count > 0) ignored = c_unlink(trim(source_dir)//c_null_char)
        if (paused) rename_rc = c_rename(trim(parked_dir)//c_null_char, &
            trim(source_dir)//c_null_char)
        call arm_directory_oracle(0_c_int)
    end subroutine run_capture_during_parent_swaps

    subroutine run_parallel_captures(project_path, cache_path, capture_context, &
            first_generation, first_ierr, first_message, second_generation, &
            second_ierr, second_message)
        character(len=*), intent(in) :: project_path, cache_path
        type(generation_context_t), intent(in) :: capture_context
        type(generation_t), intent(out) :: first_generation, second_generation
        integer, intent(out) :: first_ierr, second_ierr
        character(len=*), intent(out) :: first_message, second_message

        !$omp parallel sections num_threads(2) &
        !$omp& shared(first_generation, first_ierr, first_message, &
        !$omp& second_generation, second_ierr, second_message)
        !$omp section
        call generation_capture(project_path, cache_path, capture_context, &
            first_generation, first_ierr, first_message)
        !$omp section
        call generation_capture(project_path, cache_path, capture_context, &
            second_generation, second_ierr, second_message)
        !$omp end parallel sections
    end subroutine run_parallel_captures

    subroutine write_version_file(path, value, n_bytes)
        character(len=*), intent(in) :: path, value
        integer, intent(in), optional :: n_bytes
        character(len=512) :: temporary
        integer :: unit, ios, rename_rc, nbytes
        nbytes = 2 * 1024 * 1024
        if (present(n_bytes)) nbytes = n_bytes
        temporary = trim(path)//'.tmp'
        open (newunit=unit, file=trim(temporary), status='replace', &
            access='stream', form='unformatted', action='write', iostat=ios)
        if (ios /= 0) return
        write (unit, iostat=ios) repeat(value, nbytes)
        close (unit)
        if (ios == 0) rename_rc = fs_rename(trim(temporary), trim(path))
    end subroutine write_version_file

    logical function file_equals(path, expected)
        character(len=*), intent(in) :: path, expected
        character(len=512) :: contents
        integer :: unit, ios
        file_equals = .false.
        open (newunit=unit, file=trim(path), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) contents
        close (unit)
        if (ios == 0) file_equals = trim(contents) == trim(expected)
    end function file_equals

    logical function link_target_equals(path, expected)
        character(len=*), intent(in) :: path, expected
        character(kind=c_char) :: buffer(512)
        integer(c_size_t) :: n
        integer :: i
        link_target_equals = .false.
        n = c_readlink(trim(path)//c_null_char, buffer, &
            int(size(buffer), c_size_t))
        if (n /= len(expected)) return
        do i = 1, len(expected)
            if (buffer(i) /= expected(i:i)) return
        end do
        link_target_equals = .true.
    end function link_target_equals

    logical function file_is_byte_value(path, expected, n_bytes)
        character(len=*), intent(in) :: path
        character(len=*), intent(in), optional :: expected
        integer, intent(in), optional :: n_bytes
        character(len=1), allocatable :: bytes(:)
        integer :: unit, ios, i, nbytes, file_size
        file_is_byte_value = .false.
        nbytes = 2 * 1024 * 1024
        if (present(n_bytes)) nbytes = n_bytes
        inquire (file=trim(path), size=file_size, iostat=ios)
        if (ios /= 0 .or. file_size < nbytes) return
        allocate (bytes(nbytes))
        open (newunit=unit, file=trim(path), status='old', action='read', &
            access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        read (unit, iostat=ios) bytes
        close (unit)
        if (ios /= 0) return
        do i = 2, size(bytes)
            if (bytes(i) /= bytes(1)) return
        end do
        file_is_byte_value = .true.
        if (present(expected)) file_is_byte_value = bytes(1) == expected
    end function file_is_byte_value

    logical function has_staging_entries(cache_root)
        character(len=*), intent(in) :: cache_root
        character(len=512) :: dummy
        integer :: unit, ios
        integer(c_int) :: rc
        character(len=512) :: manifest
        has_staging_entries = .false.
        manifest = trim(root)//'/staging.list'
        rc = fo_c_generation_list_tree(trim(cache_root)// &
            '/gremlin/generations/.capture'//c_null_char, &
            trim(manifest)//c_null_char)
        if (rc /= 0) return
        open (newunit=unit, file=trim(manifest), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) dummy
        close (unit, status='delete')
        has_staging_entries = ios == 0
    end function has_staging_entries

    integer function count_generation_roots(cache_root) result(count)
        character(len=*), intent(in) :: cache_root
        character(len=512) :: manifest, record
        integer(c_int) :: rc
        integer :: unit, ios, split
        count = 0
        manifest = trim(root)//'/generation-list.txt'
        rc = fo_c_generation_list_tree(trim(cache_root)// &
            '/gremlin/generations'//c_null_char, trim(manifest)//c_null_char)
        if (rc /= 0) return
        open (newunit=unit, file=trim(manifest), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) record
            if (ios < 0) exit
            if (ios /= 0) exit
            if (record(1:2) /= 'D ') cycle
            split = index(trim(record(7:)), '/')
            if (split /= 0) cycle
            if (trim(record(7:)) == '.capture') cycle
            if (trim(record(7:)) == '.locks') cycle
            count = count + 1
        end do
        close (unit, status='delete')
    end function count_generation_roots

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
        text = trim(adjustl(text))
    end function int_text

end program test_gremlin_generation

! Keep PROGRAM first for the native scanner. C-bound controls avoid a self-use
! dependency from the program to this module in the same compilation unit.
! Interpose only in this test executable; production has no fault/pause hooks.
module generation_directory_oracle
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_int, c_intptr_t, &
        c_char, c_size_t, c_null_char, c_null_ptr, c_associated, c_f_pointer, &
        c_f_procpointer
    implicit none
    private
    integer :: directory_fault_mode = 0, entry_count = 0
    logical :: directory_paused = .false., directory_release = .false.
    logical :: directory_fault_observed = .false.
    logical :: directory_pause_completed = .false.
    logical :: directory_descriptor_matched = .false.
    logical :: directory_pause_timed_out = .false.
    logical :: directory_swap_completed = .false.
    abstract interface
        function directory_reader(dir) bind(C) result(entry)
            import :: c_ptr
            type(c_ptr), intent(in), value :: dir
            type(c_ptr) :: entry
        end function directory_reader
        function errno_reader() bind(C) result(address)
            import :: c_ptr
            type(c_ptr) :: address
        end function errno_reader
    end interface
    procedure(directory_reader), pointer :: real_readdir => null()
    procedure(errno_reader), pointer :: real_errno => null()
    interface
        function c_dlsym(handle, name) bind(C, name='dlsym') result(address)
            import :: c_ptr, c_char
            type(c_ptr), intent(in), value :: handle
            character(kind=c_char), intent(in) :: name(*)
            type(c_ptr) :: address
        end function c_dlsym
        integer(c_int) function oracle_usleep(usec) bind(C, name='usleep')
            import :: c_int
            integer(c_int), intent(in), value :: usec
        end function oracle_usleep
        integer(c_int) function oracle_dirfd(dir) bind(C, name='dirfd')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: dir
        end function oracle_dirfd
        integer(c_int) function oracle_openat(fd, path, flags) &
                bind(C, name='openat')
            import :: c_char, c_int
            integer(c_int), intent(in), value :: fd, flags
            character(kind=c_char), intent(in) :: path(*)
        end function oracle_openat
        integer(c_intptr_t) function oracle_read(fd, bytes, count) &
                bind(C, name='read')
            import :: c_char, c_int, c_intptr_t, c_size_t
            integer(c_int), intent(in), value :: fd
            character(kind=c_char), intent(out) :: bytes(*)
            integer(c_size_t), intent(in), value :: count
        end function oracle_read
        integer(c_int) function oracle_close(fd) bind(C, name='close')
            import :: c_int
            integer(c_int), intent(in), value :: fd
        end function oracle_close
    end interface
contains
    subroutine initialize_directory_oracle() &
            bind(C, name='generation_oracle_initialize')
        type(c_ptr) :: next, address
        type(c_funptr) :: function_address
        next = transfer(-1_c_intptr_t, next)
        address = c_dlsym(next, 'readdir'//c_null_char)
        if (.not. c_associated(address)) error stop 'cannot resolve libc readdir'
        function_address = transfer(address, function_address)
        call c_f_procpointer(function_address, real_readdir)
        address = c_dlsym(next, '__errno_location'//c_null_char)
        if (.not. c_associated(address)) &
            address = c_dlsym(next, '__error'//c_null_char)
        if (.not. c_associated(address)) error stop 'cannot resolve libc errno'
        function_address = transfer(address, function_address)
        call c_f_procpointer(function_address, real_errno)
    end subroutine initialize_directory_oracle

    subroutine arm_directory_oracle(mode) bind(C, name='generation_oracle_arm')
        integer(c_int), intent(in), value :: mode
        directory_fault_mode = mode
        if (mode == 0) return
        entry_count = 0
        directory_fault_observed = .false.
        directory_paused = .false.
        directory_release = .false.
        directory_pause_completed = .false.
        directory_descriptor_matched = .false.
        directory_pause_timed_out = .false.
        directory_swap_completed = .false.
    end subroutine arm_directory_oracle

    logical function directory_descriptor_matches(dir) result(matches)
        type(c_ptr), intent(in), value :: dir
        character(kind=c_char) :: bytes(33)
        integer :: i
        integer(c_int) :: fd, marker_fd, close_rc
        integer(c_intptr_t) :: n
        matches = .false.
        fd = oracle_dirfd(dir)
        if (fd < 0) return
        ! Only original src has exactly 32 P bytes at this fixture-only name.
        ! Root has R bytes and outside has X bytes, so neither can match.
        marker_fd = oracle_openat(fd, '.directory-oracle-marker'//c_null_char, &
            0_c_int) ! POSIX O_RDONLY.
        if (marker_fd < 0) return
        n = oracle_read(marker_fd, bytes, int(size(bytes), c_size_t))
        close_rc = oracle_close(marker_fd)
        if (close_rc /= 0) return
        if (n /= 32) return
        do i = 1, 32
            if (bytes(i) /= 'P') return
        end do
        matches = .true.
    end function directory_descriptor_matches

    integer(c_int) function directory_oracle_state(which) &
            bind(C, name='generation_oracle_state') result(state)
        integer(c_int), intent(in), value :: which
        logical :: observed
        observed = .false.
        select case (which)
        case (1)
            observed = directory_fault_observed
        case (2)
            !$omp atomic read
            observed = directory_paused
        case (3)
            observed = directory_pause_completed
        case (4)
            observed = directory_descriptor_matched
        case (5)
            observed = directory_pause_timed_out
        end select
        state = 0
        if (observed) state = 1
    end function directory_oracle_state

    subroutine release_directory_oracle(swapped) &
            bind(C, name='generation_oracle_release')
        integer(c_int), intent(in), value :: swapped
        directory_swap_completed = swapped == 1
        !$omp atomic write
        directory_release = .true.
    end subroutine release_directory_oracle

    function oracle_readdir(dir) bind(C, name='readdir') result(entry)
        type(c_ptr), intent(in), value :: dir
        type(c_ptr) :: entry
        integer(c_int), pointer :: errno_value
        integer(c_int) :: ignored
        integer :: attempt, saved_errno
        logical :: released

        if (.not. associated(real_readdir)) call initialize_directory_oracle()
        entry = real_readdir(dir)
        if (directory_fault_mode == 1) then
            if (c_associated(entry)) then
                entry_count = entry_count + 1
                if (entry_count < 3) return
            end if
            call c_f_pointer(real_errno(), errno_value)
            errno_value = 5 ! POSIX EIO on supported Linux/macOS platforms.
            entry = c_null_ptr
            directory_fault_observed = entry_count >= 3
            directory_fault_mode = 0
            entry_count = 0
        else if (directory_fault_mode == 2) then
            if (c_associated(entry)) return
            call c_f_pointer(real_errno(), errno_value)
            saved_errno = errno_value
            if (.not. directory_descriptor_matches(dir)) then
                errno_value = saved_errno
                return
            end if
            directory_descriptor_matched = .true.
            directory_fault_mode = 0
            !$omp atomic write
            directory_paused = .true.
            do attempt = 1, 10000
                !$omp atomic read
                released = directory_release
                if (released) then
                    directory_pause_completed = directory_swap_completed
                    errno_value = saved_errno
                    return
                end if
                ignored = oracle_usleep(1000_c_int)
            end do
            directory_pause_timed_out = .true.
            call c_f_pointer(real_errno(), errno_value)
            errno_value = 5
        end if
    end function oracle_readdir
end module generation_directory_oracle
