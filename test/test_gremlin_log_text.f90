program test_gremlin_log_text
    use fo_test_harness, only: make_scratch, write_text, read_text, remove_tree
    use fo_test_harness, only: join_path
    use fo_test_harness, only: assert_contains, assert_equal_string, finish_assertions
    use fo_gremlin_log_text, only: gremlin_retain_build_log, gremlin_log_tail
    implicit none

    character(:), allocatable :: scratch, log_file, full_log, retained
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

    call remove_tree(scratch)
    call finish_assertions()
end program test_gremlin_log_text
