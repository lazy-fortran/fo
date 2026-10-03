program test_gremlin_generation
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char, c_int
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_fs, only: fs_make_dir, fs_rename, fs_write_text
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_process, only: process_getpid
    implicit none

    interface
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
    character(len=512) :: concurrent_cache, race_project, source_file
    character(len=256) :: message
    type(generation_context_t) :: context, race_context
    type(generation_t) :: first, reused, source_changed, include_changed
    type(generation_t) :: dependency_changed, build_source_changed
    type(generation_t) :: metadata_changed, race_generation
    type(generation_t) :: mode_changed, mode_restored
    type(generation_t) :: parallel_one, parallel_two
    logical :: exists
    integer :: parallel_ierr_one, parallel_ierr_two
    character(len=256) :: parallel_message_one, parallel_message_two
    integer(c_int) :: remove_rc, symlink_rc, chmod_rc

    n_pass = 0
    n_fail = 0
    root = '/tmp/fo-gremlin-generation-'//int_text(process_getpid())
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
    call check(race_ierr == 0, 'capture publishes an immutable generation')
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

    call fs_write_text(trim(project)//'/src/main.f90', 'program version_two')
    call generation_capture(trim(project), trim(cache), context, source_changed, &
        race_ierr, message)
    call check(race_ierr == 0 .and. source_changed%identity /= first%identity, &
        'source changes invalidate generation identity')
    call check(file_equals(trim(first%project_root)//'/src/main.f90', &
        'program version_one'), 'live source edit leaves old snapshot unchanged')

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
    call write_version_file(trim(race_project)//'/src/value.dat', 'A')
    race_context%toolchain = 'gfortran race fixture'
    race_context%flags = '-O0'
    race_context%base_commit = 'race-base'
    race_context%patch_digest = 'race-patch'
    call run_capture_during_atomic_edits(trim(race_project), trim(cache), &
        race_context, race_generation, race_ierr, message)
    call check(file_is_byte_value(trim(race_project)//'/src/value.dat', 'H'), &
        'concurrent editor completed all atomic source revisions')
    call check(race_ierr /= 0, &
        'capture rejects input that changes during concurrent atomic edits')
    inquire (file=trim(race_generation%root), exist=exists)
    call check(.not. exists, &
        'unstable input capture does not publish a generation')

    call check(.not. has_staging_entries(trim(cache)), &
        'successful and rejected captures leave no staging artifacts')
    generation_count = count_generation_roots(trim(cache))
    call check(generation_count <= 11, &
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
            capture_context, generation, ierr, error_message)
        character(len=*), intent(in) :: project_path, cache_path
        type(generation_context_t), intent(in) :: capture_context
        type(generation_t), intent(out) :: generation
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: error_message
        character(len=512) :: data_path
        integer :: iteration

        data_path = trim(project_path)//'/src/value.dat'
        !$omp parallel sections num_threads(2) shared(generation, ierr, error_message)
        !$omp section
        call generation_capture(project_path, cache_path, capture_context, &
            generation, ierr, error_message)
        !$omp section
        do iteration = 1, 8
            call write_version_file(data_path, achar(64 + iteration))
        end do
        !$omp end parallel sections
    end subroutine run_capture_during_atomic_edits

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

    subroutine write_version_file(path, value)
        character(len=*), intent(in) :: path, value
        character(len=512) :: temporary
        integer :: unit, ios, rename_rc
        temporary = trim(path)//'.tmp'
        open (newunit=unit, file=trim(temporary), status='replace', &
            access='stream', form='unformatted', action='write', iostat=ios)
        if (ios /= 0) return
        write (unit, iostat=ios) repeat(value, 2 * 1024 * 1024)
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

    logical function file_is_byte_value(path, expected)
        character(len=*), intent(in) :: path
        character(len=*), intent(in), optional :: expected
        character(len=1), allocatable :: bytes(:)
        integer :: unit, ios, i
        file_is_byte_value = .false.
        allocate (bytes(2 * 1024 * 1024))
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
