program test_gremlin_log_text
    use fo_test_harness, only: make_scratch, write_text, read_text, remove_tree
    use fo_test_harness, only: join_path, make_directory, assert_true
    use fo_cmake_diagnostics, only: cmake_diagnostic_t, cmake_diagnostic_begin, &
        cmake_diagnostic_finish, cmake_diagnostic_retain
    use fo_process, only: argv_push
    use fo_cache, only: cache_file_digest, HASH_LEN
    use fo_test_harness, only: assert_contains, assert_equal_string, finish_assertions
    use fo_gremlin_log_text, only: gremlin_retain_build_log, gremlin_log_tail
    implicit none

    character(:), allocatable :: scratch, log_file, full_log, retained
    character(:), allocatable :: view, build, preload, packed, manifest, source_log
    character(len=HASH_LEN) :: digest
    type(cmake_diagnostic_t) :: capture
    integer :: n_args, i
    character(len=8) :: suffix
    logical :: exists
    character(len=*), parameter :: DIAGNOSTIC = &
        'CMake Error at CMakeLists.txt:7 (find_package): no package'

    call make_scratch('fo-gremlin-build-log', scratch)
    log_file = join_path(scratch, 'build.log')
    full_log = join_path(join_path(scratch, 'view-tmp'), 'fo-build.tmp')

    ! A failed candidate build names its full log inside private scratch that is
    ! deleted with the view; the lane log must keep the diagnostic itself.
    call write_text(full_log, DIAGNOSTIC//new_line('a'))
    call write_text(log_file, 'fo: build failed: CMake Generate step failed.'// &
        new_line('a')//'fo: full log: '//full_log//new_line('a'))
    call gremlin_retain_build_log(log_file)
    call remove_tree(join_path(scratch, 'view-tmp'))
    retained = read_text(log_file)
    call assert_contains(retained, DIAGNOSTIC, 'lane log keeps the full build log')
    call assert_contains(retained, 'CMake Generate step failed', &
        'lane log keeps the original summary')

    ! Without a full-log reference, or with a vanished one, the log is unchanged.
    call write_text(log_file, 'plain failure'//new_line('a'))
    call gremlin_retain_build_log(log_file)
    call assert_equal_string(read_text(log_file), 'plain failure'//new_line('a'), &
        'log without a reference is unchanged')
    call write_text(log_file, 'fo: full log: '//full_log//new_line('a'))
    call gremlin_retain_build_log(log_file)
    call assert_equal_string(read_text(log_file), 'fo: full log: '//full_log// &
        new_line('a'), 'log with a vanished reference is unchanged')

    call write_text(log_file, 'first'//new_line('a')//'second error'//new_line('a'))
    call assert_equal_string(gremlin_log_tail(log_file, 13), 'second error', &
        'log tail is bounded and flattened to one line')
    call assert_equal_string(gremlin_log_tail(join_path(scratch, 'absent'), 12), '', &
        'missing log has an empty tail')

    ! The command-selected preload is retained byte-for-byte before view cleanup.
    ! The command argv and long literal path survive without reconstruction.
    view = scratch//'/private-view-with-a-long-source-name'
    build = view//'/build'
    preload = view//'/.fo-cmake/cache.cmake'
    full_log = view//'/scratch/configure.log'
    call make_directory(build//'/CMakeFiles')
    call write_text(preload, 'set(PROBE_LIBRARY "/sdk/lib/libprobe.a")'//new_line('a'))
    call write_text(full_log, 'loading initial cache file '//preload//new_line('a'))
    call write_text(build//'/CMakeCache.txt', 'PROBE_WORKS:INTERNAL=FALSE'//new_line('a'))
    call write_text(build//'/CMakeFiles/CMakeError.log', 'public try-link failure')
    do i = 1, 70
        write(suffix, '(i8.8)') i
        call write_text(build//'/.cmake/api/v1/reply/reply-'//suffix//'.json', '{}')
    end do
    call write_text(build//'/.cmake/api/v1/reply/index-public.json', '{"public":true}')
    n_args = 0
    call argv_push(packed, n_args, 'cmake')
    call argv_push(packed, n_args, '-C')
    call argv_push(packed, n_args, preload)
    call cmake_diagnostic_begin(capture, view, build, packed, n_args, full_log, preload)
    call cmake_diagnostic_finish(capture, 1, full_log)
    manifest = read_text(full_log//'.cmake-diagnostics/manifest.json')
    call assert_contains(manifest, '"argv":["cmake","-C","'//preload//'"', &
        'receipt retains the exact submitted configure argv')
    call assert_contains(manifest, '"preload_unchanged":true', &
        'prelaunch and failed-view preload bytes have the same hash')
    call assert_contains(manifest, '"status":"absent"', &
        'missing optional diagnostics are recorded explicitly')
    call assert_contains(manifest, 'index-public.json', &
        'the File API index survives a bounded reply collection')
    call assert_contains(manifest, '"file_api_collection_status":1', &
        'a reply collection reaching its cap records truncation')
    call cache_file_digest(preload, digest)
    call assert_contains(manifest, trim(digest), 'manifest binds actual preload bytes')
    call write_text(log_file, 'fo: full log: '//full_log//new_line('a'))
    call gremlin_retain_build_log(log_file, source_log)
    call assert_equal_string(source_log, full_log, 'actual configure log path is exposed')
    call cmake_diagnostic_retain(source_log, log_file, repeat('a', 64), &
        'public-diagnostic-session', view)
    call remove_tree(view)
    retained = read_text(log_file//'.cmake-diagnostics/member-1.bin')
    call assert_equal_string(retained, &
        'set(PROBE_LIBRARY "/sdk/lib/libprobe.a")'//new_line('a'), &
        'exact prelaunch cache bytes survive deletion of the owned view')
    call assert_contains(read_text(log_file//'.cmake-diagnostics/owner.json'), &
        'public-diagnostic-session', 'retained manifest has owning session identity')

    ! Oversized preload caches are rejected explicitly rather than truncated.
    call make_directory(view)
    call write_text(preload, repeat('x', 2097153))
    call write_text(full_log, 'configure failed')
    call cmake_diagnostic_begin(capture, view, build, packed, n_args, full_log, preload)
    call cmake_diagnostic_finish(capture, 1, full_log)
    call assert_contains(read_text(full_log//'.cmake-diagnostics/manifest.json'), &
        '"status":"size-limit"', 'oversized preload has an explicit capture failure')
    call remove_tree(view)

    ! Successful configure leaves no diagnostic tree to accumulate.
    call write_text(full_log, 'configure passed')
    call cmake_diagnostic_begin(capture, view, build, packed, n_args, full_log, '')
    call cmake_diagnostic_finish(capture, 0, full_log)
    inquire(file=full_log//'.cmake-diagnostics', exist=exists)
    call assert_true(.not. exists, 'successful configure diagnostic scratch is removed')

    call remove_tree(scratch)
    call finish_assertions()
end program test_gremlin_log_text
