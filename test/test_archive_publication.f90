program test_archive_publication
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: remove_path, file_exists, assert_process_ok
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_file_exists, finish_assertions
    use fo_test_harness, only: spawn_process, poll_process
    use fo_test_harness, only: terminate_process_group
    use fo_test_harness, only: read_text, process_alive, start_sentinel, stop_sentinel
    use fo_test_harness, only: spawn_heartbeat_process
    use fo_test_cli, only: resolve_driver, run_fo, run_external, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, fake_bin, real_ar
    character(:), allocatable :: real_fc, real_gcc, archive, old_archive, marker, lib_dir, bin_dir
    character(:), allocatable :: digest_before
    character(:), allocatable :: timeout_scratch, heartbeat, baseline, alive_sample
    character(:), allocatable :: shared_extension, truncated_image
    type(string_list_t) :: args, env
    type(process_result_t) :: result, child_result, external
    type(json_value_t) :: report
    integer :: child = -1, status, count, before_calls, timed_child, sentinel
    logical :: found, macos

    call resolve_driver(driver)
    call make_scratch('fo-archive-publication-fortran', scratch)
    call run_external('/usr/bin/uname', words([character(len=8) :: '-s']), &
        scratch, external)
    call assert_process_ok(external, 'identify native publication platform')
    macos = external%stdout == 'Darwin' // new_line('a')
    shared_extension = '.so'
    if (macos) shared_extension = '.dylib'
    truncated_image = join_path(scratch, 'truncated-native.image')
    project = join_path(scratch, 'project')
    cache = join_path(scratch, 'cache')
    fake_bin = join_path(scratch, 'bin')
    marker = join_path(scratch, 'writer.marker')
    lib_dir = join_path(project, 'build/fo/lib')
    bin_dir = join_path(project, 'build/fo/bin')
    call make_directory(project)
    call make_directory(fake_bin)
    real_ar = locate('ar')
    real_fc = locate('gfortran')
    real_gcc = locate('gcc')
    call write_project()
    call install_wrappers()

    args = words(['build'])
    call invoke(args, 'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'initial archive publication build')
    call find_output(lib_dir, 'objects_', '.a', archive, count)
    call assert_equal_integer(count, 1, 'one keyed archive is visible after build')
    call verify_archive(archive)
    args = words([character(len=32) :: 'test', '--all', '--json'])
    call invoke(args, 'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'tests consume published archive')
    call parse_json_report(result, report, 'archive consumer test report')
    call check_tests_pass(report, 2, 'archive consumers pass')
    call check_concurrent_test_links()
    args = words([character(len=32) :: 'exec', 'probe'])
    call invoke(args, 'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'real executable links against the archive')
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'published executable returns expected behavior')
    call save_native_prefix(join_path(bin_dir, 'probe'))

    call invoke(words([character(len=32) :: 'build', '--release']), &
        'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'release profile builds independently')
    call invoke(words([character(len=32) :: 'exec', '--release', 'probe']), &
        'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'release profile executable runs')
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'release profile produces correct behavior')
    call invoke(words([character(len=32) :: 'exec', '--no-build', 'probe']), &
        'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'debug profile remains independently executable')
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'release artifacts do not replace default-profile output')
    call exercise_stale_archive(archive)

    before_calls = log_lines(join_path(scratch, 'ar.log'))
    args = words(['build'])
    call invoke(args, 'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'warm archive build')
    call assert_equal_integer(log_lines(join_path(scratch, 'ar.log')), before_calls, &
        'unchanged archive inputs reuse the validated artifact')
    call check_warm_named_archive(archive)

    call write_text(archive, 'partial stale archive')
    old_archive = archive
    call write_text(join_path(project, 'app/probe.f90'), app_source('force a fresh link'))
    args = words(['build'])
    call invoke(args, 'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'corrupt archive is rebuilt')
    call verify_archive(archive)
    call assert_true(read_file(archive) /= 'partial stale archive', &
        'completed publication replaces corrupt cached bytes')

    call write_text(join_path(project, 'src/right.f90'), right_source(23))
    call write_text(join_path(project, 'src/left_impl.f90'), left_impl_source(19))
    digest_before = file_digest(old_archive)
    args = words(['build'])
    call invoke(args, 'fail', 'normal', 'normal', result)
    call assert_true(result%exit_code /= 0, 'archiver failure propagates to fo')
    call find_output(lib_dir, 'objects_', '.a', archive, count)
    call assert_file_exists(old_archive, 'failed writer preserves a visible stale artifact')
    call assert_equal_string(file_digest(old_archive), digest_before, &
        'failed staged archive leaves old archive bytes unchanged')
    args = words(['build'])
    call invoke(args, 'omit', 'normal', 'normal', result)
    call assert_true(result%exit_code /= 0, 'archive membership mismatch is rejected')
    call assert_equal_string(file_digest(old_archive), digest_before, &
        'membership rejection leaves old archive bytes unchanged')
    call invoke(words(['build']), 'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'valid archive can replace failed stale output')
    call invoke(words([character(len=32) :: 'exec', 'probe']), &
        'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'recovered archive consumer runs')
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'archive recovered with updated module behavior')

    marker = join_path(scratch, 'writer.marker')
    if (file_exists(marker)) call remove_path(marker)
    call remove_all_archives()
    env = base_environment('delay', 'normal', 'normal')
    args = string_list_t()
    call list_add(args, driver)
    call list_add(args, 'build')
    call spawn_process(args, project, child, env)
    call wait_for(marker, 100, child, found)
    call assert_true(found, 'fake archiver holds a partial staged archive')
    call assert_file_exists(marker, 'fake archiver holds a partial staged archive')
    call find_output(lib_dir, 'objects_', '.a', archive, count)
    call assert_equal_integer(count, 0, 'partial bytes are hidden during publication')
    call call_fo(['build'], 'normal', 'normal', 'normal', child_result)
    call assert_process_ok(child_result, 'concurrent build waits for atomic archive owner')
    call poll_until(child, status)
    call assert_equal_integer(status, 0, 'delayed archive producer completes')
    call find_output(lib_dir, 'objects_', '.a', archive, count)
    call assert_equal_integer(count, 1, 'one complete archive becomes visible')
    call verify_archive(archive)

    args = words([character(len=32) :: 'test', '--all', '--json'])
    digest_before = directory_digest(bin_dir)
    call write_text(join_path(project, 'test/test_alpha.f90'), &
        test_source('test_alpha', 'force-link-failure'))
    call invoke(args, 'normal', 'fail', 'normal', result)
    call assert_true(result%exit_code /= 0, 'failed executable link propagates')
    call assert_equal_string(directory_digest(bin_dir), digest_before, &
        'failed staged executable links preserve stable binaries')
    call assert_no_stages(bin_dir, '.fo-link.tmp-')
    call write_text(join_path(project, 'test/test_alpha.f90'), &
        test_source('test_alpha', 'force-truncated-link'))
    call invoke(words([character(len=32) :: 'test', '--all', '--json']), &
        'normal', 'truncated', 'normal', result)
    call assert_true(result%exit_code /= 0, 'magic-only executable is rejected')
    call assert_equal_string(directory_digest(bin_dir), digest_before, &
        'truncated staged executable preserves stable binaries')
    call assert_no_stages(bin_dir, '.fo-link.tmp-')
    call invoke(words([character(len=32) :: 'test', '--all', '--json']), &
        'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'clean link retry recovers after staged failures')
    call parse_json_report(result, report, 'recovered test report')
    call check_tests_pass(report, 2, 'recovered test executables pass')

    call write_text(join_path(project, 'fpm.toml'), shared_manifest())
    marker = join_path(scratch, 'shared.marker')
    if (file_exists(marker)) call remove_path(marker)
    env = base_environment('normal', 'normal', 'delay')
    args = string_list_t()
    call list_add(args, driver)
    call list_add(args, 'test')
    call list_add(args, '--all')
    call list_add(args, '--json')
    call spawn_process(args, project, child, env)
    call wait_for(marker, 150, child, found)
    call assert_true(found, 'shared linker pauses inside private stage')
    call assert_file_exists(marker, 'shared linker pauses inside private stage')
    call find_output(lib_dir, 'libproject_', shared_extension, archive, count)
    call assert_equal_integer(count, 0, 'partial shared library is not published')
    call call_fo([character(len=32) :: 'test', '--all', '--json'], &
        'normal', 'normal', 'normal', child_result)
    call assert_process_ok(child_result, 'concurrent shared-library consumer succeeds')
    call poll_until(child, status)
    call assert_equal_integer(status, 0, 'shared library publisher completes')
    call find_output(lib_dir, 'libproject_', shared_extension, archive, count)
    call assert_equal_integer(count, 1, 'one shared library is atomically published')
    call verify_image(archive)
    call invoke(words([character(len=32) :: 'exec', 'probe']), &
        'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'shared-linked executable runs')
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'shared-linked program behavior remains correct')

    before_calls = log_lines(join_path(scratch, 'shared.log'))
    call write_text(join_path(project, 'test/test_beta.f90'), &
        test_source('test_beta', 'shared-warm'))
    call invoke(words([character(len=32) :: 'test', '--all', '--json']), &
        'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'warm shared-library consumers pass')
    call parse_json_report(result, report, 'warm shared test report')
    call check_tests_pass(report, 2, 'warm shared test report passes')
    call assert_equal_integer(log_lines(join_path(scratch, 'shared.log')), before_calls, &
        'unchanged shared-library inputs reuse the validated image')

    call write_text(archive, 'invalid prior shared library')
    call write_text(join_path(project, 'test/test_alpha.f90'), test_source('test_alpha', 'repair'))
    call invoke(words([character(len=32) :: 'test', '--all', '--json']), &
        'normal', 'normal', 'fail', result)
    call assert_true(result%exit_code /= 0, 'shared-linker failure propagates')
    call assert_no_stages(lib_dir, '.fo-shared.tmp-')
    call assert_equal_string(read_file(archive), 'invalid prior shared library', &
        'failed shared stage leaves prior final path intact')
    call invoke(words([character(len=32) :: 'test', '--all', '--json']), &
        'normal', 'normal', 'truncated', result)
    call assert_true(result%exit_code /= 0, 'truncated shared image is rejected')
    call assert_no_stages(lib_dir, '.fo-shared.tmp-')
    call assert_equal_string(read_file(archive), 'invalid prior shared library', &
        'rejected shared stage preserves old library')
    call invoke(words([character(len=32) :: 'test', '--all', '--json']), &
        'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'valid shared-library retry succeeds')
    call invoke(words([character(len=32) :: 'exec', 'probe']), &
        'normal', 'normal', 'normal', result)
    call assert_process_ok(result, 'recovered shared-linked program runs')
    call assert_equal_string(result%stdout, '42' // new_line('a'), &
        'recovered published shared artifact is behaviorally valid')

    call test_no_cache_shared_cleanup(archive)
    call assert_true(log_lines(join_path(scratch, 'ar.log')) > 0, &
        'archiver wrapper log records independent observations')
    call assert_true(log_lines(join_path(scratch, 'fc.log')) > 0, &
        'compiler wrapper log records independent observations')
    if (macos) then
        call assert_true(log_lines(join_path(scratch, 'shared.log')) > 0, &
            'Fortran driver shared-link wrapper records independent observations')
    else
        call assert_true(log_lines(join_path(scratch, 'gcc.log')) > 0, &
            'native shared-link wrapper log records independent observations')
    end if

    call make_scratch('fo-archive-timeout-cleanup', timeout_scratch)
    heartbeat = join_path(timeout_scratch, 'heartbeat')
    call start_sentinel(sentinel)
    call spawn_heartbeat_process('heartbeat', timeout_scratch, child)
    timed_child = child
    call wait_for(heartbeat, 100, child, found)
    if (found) then
        call assert_true(process_alive(child), 'heartbeat owner stays alive during setup')
        baseline = read_text(heartbeat)
        call assert_true(len(baseline) > 0, 'heartbeat has bytes before timeout')
        call pause_repeatedly(5)
        alive_sample = read_text(heartbeat)
        call assert_true(len(alive_sample) > len(baseline), &
            'heartbeat advances while its child is alive')
        call assert_true(process_alive(child), 'heartbeat child remains alive before timeout')
    else
        call assert_true(.false., 'heartbeat child writes before forced timeout')
    end if
    call wait_for('never-created', 5, child, found)
    call assert_true(.not. found, 'forced publication marker timeout is observed')
    call assert_true(.not. process_alive(timed_child), &
        'timed-out publication child is reaped')
    if (file_exists(heartbeat)) then
        baseline = read_text(heartbeat)
        call pause_repeatedly(5)
        call assert_equal_string(read_text(heartbeat), baseline, &
            'publication heartbeat stops after timeout cleanup')
    else
        call assert_true(.false., 'heartbeat file remains for post-timeout check')
    end if
    call assert_true(process_alive(sentinel), &
        'unrelated sentinel survives publication group cleanup')
    call stop_sentinel(sentinel)
    call remove_tree(timeout_scratch)
    call assert_true(.not. file_exists(timeout_scratch), &
        'forced-timeout scratch is removed after child cleanup')

    if (child > 0) call cleanup_child()
    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'archive-publication: archive, executable and shared output checks pass'

contains

    function words(values) result(list)
        character(len=*), intent(in) :: values(:)
        type(string_list_t) :: list
        integer :: i

        do i = 1, size(values)
            call list_add(list, trim(values(i)))
        end do
    end function words

    subroutine make_directory(path)
        character(len=*), intent(in) :: path
        call run_external('/bin/mkdir', words([character(len=512) :: '-p', path]), &
            scratch, external)
        call assert_process_ok(external, 'create fixture directory')
    end subroutine make_directory

    function locate(name) result(path)
        character(len=*), intent(in) :: name
        character(:), allocatable :: path
        type(process_result_t) :: located

        select case (name)
        case ('ar')
            path = '/usr/bin/ar'
        case ('gfortran')
            call run_external('/bin/sh', &
                words([character(len=64) :: '-c', 'command -v gfortran']), &
                scratch, located)
            call assert_process_ok(located, 'locate actual Fortran compiler')
            path = trim(located%stdout)
            if (len(path) > 0) then
                if (path(len(path):) == new_line('a')) path = path(:len(path)-1)
            end if
        case ('gcc')
            path = '/usr/bin/gcc'
        case default
            path = ''
        end select
    end function locate

    subroutine write_project()
        call write_text(join_path(project, 'fpm.toml'), &
            'name = "archive_publication_probe"' // new_line('a'))
        call write_text(join_path(project, 'src/left.f90'), left_source())
        call write_text(join_path(project, 'src/left_impl.f90'), left_impl_source(20))
        call write_text(join_path(project, 'src/right.f90'), right_source(22))
        call write_text(join_path(project, 'app/probe.f90'), app_source('initial'))
        call write_text(join_path(project, 'test/test_alpha.f90'), test_source('test_alpha', 'alpha'))
        call write_text(join_path(project, 'test/test_beta.f90'), test_source('test_beta', 'beta'))
    end subroutine write_project

    function left_source() result(source)
        character(:), allocatable :: source
        source = 'module archive_left' // new_line('a') // 'implicit none' // new_line('a') // &
            'interface' // new_line('a') // 'module function left_value() result(value)' // &
            new_line('a') // 'integer :: value' // new_line('a') // &
            'end function left_value' // new_line('a') // 'end interface' // new_line('a') // &
            'end module archive_left' // new_line('a')
    end function left_source

    function left_impl_source(value) result(source)
        integer, intent(in) :: value
        character(:), allocatable :: source
        character(16) :: number
        write(number, '(i0)') value
        source = 'submodule (archive_left) archive_left_impl' // new_line('a') // &
            'implicit none' // new_line('a') // 'contains' // new_line('a') // &
            'module procedure left_value' // new_line('a') // 'value = ' // trim(number) // &
            new_line('a') // 'end procedure left_value' // new_line('a') // &
            'end submodule archive_left_impl' // &
            new_line('a')
    end function left_impl_source

    function right_source(value) result(source)
        integer, intent(in) :: value
        character(:), allocatable :: source
        character(16) :: number
        write(number, '(i0)') value
        source = 'module archive_right' // new_line('a') // 'contains' // new_line('a') // &
            'integer function right_value()' // new_line('a') // 'right_value = ' // &
            trim(number) // new_line('a') // 'end function right_value' // new_line('a') // &
            'end module archive_right' // new_line('a')
    end function right_source

    function app_source(comment) result(source)
        character(len=*), intent(in) :: comment
        character(:), allocatable :: source
        source = 'program probe' // new_line('a') // &
            'use archive_left, only: left_value' // new_line('a') // &
            'use archive_right, only: right_value' // new_line('a') // 'implicit none' // &
            new_line('a') // "print '(i0)', left_value() + right_value()" // new_line('a') // &
            '! ' // comment // new_line('a') // 'end program probe' // new_line('a')
    end function app_source

    function test_source(name, marker_text) result(source)
        character(len=*), intent(in) :: name, marker_text
        character(:), allocatable :: source
        source = 'program ' // name // new_line('a') // &
            'use archive_left, only: left_value' // new_line('a') // &
            'use archive_right, only: right_value' // new_line('a') // 'implicit none' // &
            new_line('a') // 'if (left_value() + right_value() /= 42) stop 1' // &
            new_line('a') // "print '(a)', '" // marker_text // "'" // new_line('a') // &
            'end program ' // name // new_line('a')
    end function test_source

    function shared_manifest() result(text)
        character(:), allocatable :: text
        text = 'name = "archive_publication_probe"' // new_line('a') // '[build]' // &
            new_line('a') // 'auto-executables = true' // new_line('a') // &
            'auto-tests = true' // new_line('a') // '[extra.fo]' // new_line('a') // &
            'link = "shared"' // new_line('a') // 'pic = "true"' // new_line('a')
    end function shared_manifest

    subroutine install_wrappers()
        call write_text(join_path(fake_bin, 'ar'), ar_wrapper())
        call write_text(join_path(fake_bin, 'gfortran'), fc_wrapper())
        call write_text(join_path(fake_bin, 'gcc'), gcc_wrapper())
        call chmod_executable(join_path(fake_bin, 'ar'))
        call chmod_executable(join_path(fake_bin, 'gfortran'))
        call chmod_executable(join_path(fake_bin, 'gcc'))
    end subroutine install_wrappers

    function ar_wrapper() result(text)
        character(:), allocatable :: text
        text = '#!/bin/sh' // new_line('a') // &
            'echo "$FO_TEST_AR_MODE $PPID $*" >> "$FO_TEST_AR_LOG"' // new_line('a') // &
            'if [ "$1" = rcs ]; then' // new_line('a') // &
            ' case "$FO_TEST_AR_MODE" in' // new_line('a') // &
            ' fail) echo partial > "$2"; exit 37 ;;' // new_line('a') // &
            ' omit) exec "$FO_TEST_REAL_AR" rcs "$2" ;;' // new_line('a') // &
            ' delay) echo partial > "$2"; : > "$FO_TEST_MARKER"; sleep 1; rm -f "$2" ;;' // new_line('a') // &
            ' esac' // new_line('a') // 'fi' // new_line('a') // &
            'exec "$FO_TEST_REAL_AR" "$@"' // new_line('a')
    end function ar_wrapper

    function fc_wrapper() result(text)
        character(:), allocatable :: text
        text = '#!/bin/sh' // new_line('a') // &
            'out=; next=no; compile=no; shared=no; test_link=no' // new_line('a') // &
            'for arg in "$@"; do' // new_line('a') // &
            ' if [ "$next" = yes ]; then out=$arg; next=no; fi' // new_line('a') // &
            ' if [ "$arg" = -o ]; then next=yes; fi' // new_line('a') // &
            ' if [ "$arg" = -c ]; then compile=yes; fi' // new_line('a') // &
            ' if [ "$arg" = -dynamiclib ]; then shared=yes; fi' // new_line('a') // &
            ' if [ "$arg" = -shared ]; then shared=yes; fi' // new_line('a') // &
            'done' // new_line('a') // &
            'case "$out" in */.fo-shared.tmp-*/*) shared=yes ;; esac' // new_line('a') // &
            'echo "$FO_TEST_LINK_MODE $PPID $out" >> "$FO_TEST_FC_LOG"' // new_line('a') // &
            'case "$out" in */build/fo/bin/*) test_link=yes ;; esac' // new_line('a') // &
            'if [ "$test_link" = yes ]; then echo start >> "$FO_TEST_FC_LOG"; fi' // &
            new_line('a') // 'case "$out" in */build/fo/bin/*)' // new_line('a') // &
            ' if [ "$FO_TEST_LINK_MODE" = fail ]; then echo broken > "$out"; exit 41; fi' // &
            new_line('a') // ' if [ "$FO_TEST_LINK_MODE" = truncated ]; then ' // &
            'cp "$FO_TEST_TRUNCATED_IMAGE" "$out"; exit 0; fi' // &
            new_line('a') // 'esac' // new_line('a') // &
            'if [ "$shared" = yes ]; then' // new_line('a') // &
            ' echo "$FO_TEST_SHARED_MODE $PPID $out" >> "$FO_TEST_SHARED_LOG"' // new_line('a') // &
            ' case "$FO_TEST_SHARED_MODE" in' // new_line('a') // &
            ' fail) echo broken > "$out"; exit 39 ;;' // new_line('a') // &
            ' truncated) cp "$FO_TEST_TRUNCATED_IMAGE" "$out"; exit 0 ;;' // &
            new_line('a') // &
            ' delay) echo partial > "$out"; : > "$FO_TEST_MARKER"; sleep 1; rm -f "$out" ;;' // new_line('a') // &
            ' esac' // new_line('a') // 'fi' // new_line('a') // &
            'result=0; "$FO_TEST_REAL_FC" "$@" || result=$?' // new_line('a') // &
            'if [ "$test_link" = yes ]; then echo done >> "$FO_TEST_FC_LOG"; fi' // &
            new_line('a') // 'exit "$result"' // new_line('a')
    end function fc_wrapper

    function gcc_wrapper() result(text)
        character(:), allocatable :: text
        text = '#!/bin/sh' // new_line('a') // &
            'out=; next=no; shared=no' // new_line('a') // &
            'for arg in "$@"; do' // new_line('a') // &
            ' if [ "$next" = yes ]; then out=$arg; next=no; fi' // new_line('a') // &
            ' if [ "$arg" = -o ]; then next=yes; fi' // new_line('a') // &
            ' if [ "$arg" = -shared ] || [ "$arg" = -dynamiclib ]; then shared=yes; fi' // &
            new_line('a') // 'done' // new_line('a') // &
            'case "$out" in */.fo-shared.tmp-*/*) shared=yes ;; esac' // new_line('a') // &
            'echo "$FO_TEST_SHARED_MODE $PPID $out" >> "$FO_TEST_GCC_LOG"' // new_line('a') // &
            'if [ "$shared" = yes ]; then' // new_line('a') // &
            ' case "$FO_TEST_SHARED_MODE" in' // new_line('a') // &
            ' fail) echo broken > "$out"; exit 39 ;;' // new_line('a') // &
            ' truncated) cp "$FO_TEST_TRUNCATED_IMAGE" "$out"; exit 0 ;;' // &
            new_line('a') // &
            ' delay) echo partial > "$out"; : > "$FO_TEST_MARKER"; sleep 1; rm -f "$out" ;;' // &
            new_line('a') // ' esac' // new_line('a') // 'fi' // new_line('a') // &
            'exec "$FO_TEST_REAL_GCC" "$@"' // new_line('a')
    end function gcc_wrapper

    subroutine chmod_executable(path)
        character(len=*), intent(in) :: path
        call run_external('/bin/chmod', words([character(len=512) :: '755', path]), &
            scratch, external)
        call assert_process_ok(external, 'mark wrapper executable')
    end subroutine chmod_executable

    function base_environment(ar_mode, link_mode, shared_mode) result(environment)
        character(len=*), intent(in) :: ar_mode, link_mode, shared_mode
        type(string_list_t) :: environment
        character(:), allocatable :: path

        path = fake_bin // ':' // environment_value('PATH')
        call list_add(environment, 'PATH=' // path)
        call list_add(environment, 'FO_CACHE_DIR=' // cache)
        call list_add(environment, 'FO_JOBS=2')
        call list_add(environment, 'FO_FC=' // join_path(fake_bin, 'gfortran'))
        call list_add(environment, 'FO_TEST_AR_MODE=' // trim(ar_mode))
        call list_add(environment, 'FO_TEST_LINK_MODE=' // trim(link_mode))
        call list_add(environment, 'FO_TEST_SHARED_MODE=' // trim(shared_mode))
        call list_add(environment, 'FO_TEST_REAL_AR=' // real_ar)
        call list_add(environment, 'FO_TEST_REAL_FC=' // real_fc)
        call list_add(environment, 'FO_TEST_REAL_GCC=' // real_gcc)
        call list_add(environment, 'FO_TEST_TRUNCATED_IMAGE=' // truncated_image)
        call list_add(environment, 'FO_TEST_AR_LOG=' // join_path(scratch, 'ar.log'))
        call list_add(environment, 'FO_TEST_FC_LOG=' // join_path(scratch, 'fc.log'))
        call list_add(environment, 'FO_TEST_GCC_LOG=' // join_path(scratch, 'gcc.log'))
        call list_add(environment, 'FO_TEST_SHARED_LOG=' // join_path(scratch, 'shared.log'))
        call list_add(environment, 'FO_TEST_MARKER=' // marker)
    end function base_environment

    subroutine invoke(arguments, ar_mode, link_mode, shared_mode, child_result)
        type(string_list_t), intent(in) :: arguments
        character(len=*), intent(in) :: ar_mode, link_mode, shared_mode
        type(process_result_t), intent(out) :: child_result
        type(string_list_t) :: environment

        environment = base_environment(ar_mode, link_mode, shared_mode)
        call run_fo(driver, arguments, project, cache, child_result, environment, 120000)
    end subroutine invoke

    subroutine call_fo(arguments, ar_mode, link_mode, shared_mode, child_result)
        character(len=*), intent(in) :: arguments(:)
        character(len=*), intent(in) :: ar_mode, link_mode, shared_mode
        type(process_result_t), intent(out) :: child_result

        call invoke(words(arguments), ar_mode, link_mode, shared_mode, child_result)
    end subroutine call_fo

    function environment_value(name) result(value)
        character(len=*), intent(in) :: name
        character(:), allocatable :: value
        integer :: length
        call get_environment_variable(name, length=length)
        allocate(character(len=length) :: value)
        call get_environment_variable(name, value=value)
    end function environment_value

    subroutine find_output(directory, prefix, suffix, path, count)
        character(len=*), intent(in) :: directory, prefix, suffix
        character(:), allocatable, intent(out) :: path
        integer, intent(out) :: count
        type(process_result_t) :: listing
        type(string_list_t) :: command
        integer :: first, last, length
        character(:), allocatable :: name

        path = ''
        count = 0
        call list_add(command, '-1')
        call list_add(command, directory)
        call run_external('/bin/ls', command, scratch, listing)
        if (listing%exit_code /= 0) return
        first = 1
        do while (first <= len(listing%stdout))
            last = index(listing%stdout(first:), new_line('a'))
            if (last == 0) last = len(listing%stdout) - first + 2
            length = last - 1
            if (length > 0) then
                name = listing%stdout(first:first + length - 1)
                if (index(name, prefix) == 1 .and. ends_with(name, suffix)) then
                    count = count + 1
                    path = join_path(directory, name)
                end if
            end if
            first = first + last
        end do
    end subroutine find_output

    logical function ends_with(value, suffix)
        character(len=*), intent(in) :: value, suffix
        ends_with = .false.
        if (len(suffix) == 0) then
            ends_with = .true.
            return
        end if
        if (len(value) < len(suffix)) return
        ends_with = value(len(value) - len(suffix) + 1:) == suffix
    end function ends_with

    subroutine verify_archive(path)
        character(len=*), intent(in) :: path
        type(process_result_t) :: listing
        type(string_list_t) :: command
        call list_add(command, 't')
        call list_add(command, path)
        call run_external(real_ar, command, scratch, listing)
        call assert_process_ok(listing, 'independently inspect complete archive members')
        call assert_true(count_lines(listing%stdout) >= 3, &
            'archive includes module and submodule object members')
    end subroutine verify_archive

    subroutine verify_image(path)
        character(len=*), intent(in) :: path
        character(:), allocatable :: bytes
        type(process_result_t) :: native_header
        bytes = read_file(path)
        call assert_true(len(bytes) > 4, 'published shared image is not a short stub')
        if (macos) then
            call run_external('/usr/bin/otool', &
                words([character(len=512) :: '-hv', path]), scratch, native_header)
            call assert_process_ok(native_header, &
                'Apple otool reads shared image header')
            call assert_true(index(native_header%stdout, 'DYLIB') > 0, &
                'shared image has independently checked Mach-O dylib type')
            return
        end if
        if (len(bytes) >= 4) then
            call assert_equal_string(bytes(:4), char(127) // 'ELF', &
                'shared image has independently checked ELF magic')
        end if
    end subroutine verify_image

    subroutine save_native_prefix(path)
        !! Retain a real header but omit its declared payload. This models a
        !! successful wrapper leaving a truncated image on either platform.
        character(len=*), intent(in) :: path
        character(:), allocatable :: bytes
        integer :: unit

        bytes = read_file(path)
        call assert_true(len(bytes) > 128, 'native fixture has a header and payload')
        if (len(bytes) <= 128) return
        open (newunit=unit, file=truncated_image, access='stream', &
            form='unformatted', status='replace', action='write')
        write (unit) bytes(:128)
        close (unit)
    end subroutine save_native_prefix

    subroutine check_concurrent_test_links()
        character(:), allocatable :: text
        integer :: first, last, length, active, maximum

        active = 0
        maximum = 0
        text = read_file(join_path(scratch, 'fc.log'))
        first = 1
        do while (first <= len(text))
            last = index(text(first:), new_line('a'))
            if (last == 0) last = len(text) - first + 2
            length = last - 1
            if (length >= 5) then
                if (text(first:first + 4) == 'start') then
                    active = active + 1
                    maximum = max(maximum, active)
                else if (text(first:first + 3) == 'done') then
                    active = active - 1
                end if
            end if
            first = first + last
        end do
        call assert_true(maximum >= 2, 'two test executable links overlap as archive consumers')
    end subroutine check_concurrent_test_links

    subroutine remove_all_archives()
        type(process_result_t) :: listing
        type(string_list_t) :: command
        integer :: first, last, length
        character(:), allocatable :: name

        call list_add(command, '-1')
        call list_add(command, lib_dir)
        call run_external('/bin/ls', command, scratch, listing)
        if (listing%exit_code /= 0) return
        first = 1
        do while (first <= len(listing%stdout))
            last = index(listing%stdout(first:), new_line('a'))
            if (last == 0) last = len(listing%stdout) - first + 2
            length = last - 1
            if (length > 0) then
                name = listing%stdout(first:first + length - 1)
                if (index(name, 'objects_') == 1 .and. ends_with(name, '.a')) &
                    call remove_path(join_path(lib_dir, name))
            end if
            first = first + last
        end do
    end subroutine remove_all_archives

    subroutine assert_no_stages(directory, prefix)
        character(len=*), intent(in) :: directory, prefix
        character(:), allocatable :: path
        integer :: count
        call find_output(directory, prefix, '', path, count)
        call assert_equal_integer(count, 0, 'owned temporary output directory is removed')
    end subroutine assert_no_stages

    subroutine wait_for(path, limit, process_id, found)
        character(len=*), intent(in) :: path
        integer, intent(in) :: limit
        integer, intent(inout) :: process_id
        logical, intent(out) :: found
        integer :: i, ignored_status

        found = .false.
        do i = 1, limit
            if (file_exists(path)) then
                found = .true.
                return
            end if
            call pause()
        end do
        call terminate_owned(process_id, ignored_status)
    end subroutine wait_for

    subroutine poll_until(process_id, exit_status)
        integer, intent(inout) :: process_id
        integer, intent(out) :: exit_status
        integer :: i
        exit_status = 999
        do i = 1, 300
            call poll_process(process_id, exit_status)
            if (exit_status /= 999) then
                if (exit_status == -999 .or. exit_status == -998) then
                    call terminate_owned(process_id, exit_status)
                    call assert_true(.false., 'poll background publication process')
                else
                    process_id = -1
                end if
                return
            end if
            call pause()
        end do
        call terminate_owned(process_id, exit_status)
        call assert_true(.false., 'background publication process completes')
    end subroutine poll_until

    subroutine cleanup_child()
        integer :: ignored_status

        call terminate_owned(child, ignored_status)
    end subroutine cleanup_child

    subroutine terminate_owned(process_id, exit_status)
        integer, intent(inout) :: process_id
        integer, intent(out) :: exit_status
        logical :: owned_reaped

        call terminate_process_group(process_id, exit_status, owned_reaped)
        call assert_true(owned_reaped, 'owned publication process is reaped before cleanup')
        if (.not. owned_reaped) error stop 'preserving scratch after reap failure'
    end subroutine terminate_owned

    subroutine pause()
        call run_external('/bin/sleep', words([character(len=16) :: '0.02']), scratch, external)
    end subroutine pause

    subroutine pause_repeatedly(count)
        integer, intent(in) :: count
        integer :: i

        do i = 1, count
            call pause()
        end do
    end subroutine pause_repeatedly

    integer function log_lines(path) result(count)
        character(len=*), intent(in) :: path
        character(:), allocatable :: text
        count = 0
        if (.not. file_exists(path)) return
        text = read_file(path)
        count = count_lines(text)
    end function log_lines

    integer function count_lines(text) result(count)
        character(len=*), intent(in) :: text
        integer :: i
        count = 0
        do i = 1, len(text)
            if (text(i:i) == new_line('a')) count = count + 1
        end do
        if (len(text) > 0) then
            if (text(len(text):) /= new_line('a')) count = count + 1
        end if
    end function count_lines

    function read_file(path) result(text)
        character(len=*), intent(in) :: path
        character(:), allocatable :: text
        integer :: unit, size_bytes, status
        inquire(file=path, size=size_bytes, iostat=status)
        if (status /= 0) then
            text = ''
            return
        end if
        if (size_bytes < 0) then
            text = ''
            return
        end if
        allocate(character(len=size_bytes) :: text)
        open(newunit=unit, file=path, access='stream', form='unformatted', status='old')
        if (size_bytes > 0) read(unit) text
        close(unit)
    end function read_file

    function file_digest(path) result(digest)
        character(len=*), intent(in) :: path
        character(:), allocatable :: digest
        type(string_list_t) :: command
        type(process_result_t) :: hashed
        if (macos) then
            call list_add(command, '-a')
            call list_add(command, '256')
        end if
        call list_add(command, path)
        if (macos) then
            call run_external('/usr/bin/shasum', command, scratch, hashed)
        else
            call run_external('/usr/bin/sha256sum', command, scratch, hashed)
        end if
        call assert_process_ok(hashed, 'compute independent file digest')
        digest = ''
        if (len(hashed%stdout) < 64) return
        digest = hashed%stdout(:64)
    end function file_digest

    function directory_digest(directory) result(digest)
        character(len=*), intent(in) :: directory
        character(:), allocatable :: digest, name, file
        type(string_list_t) :: command
        type(process_result_t) :: listing
        integer :: first, last, length

        digest = ''
        call list_add(command, '-1')
        call list_add(command, directory)
        call run_external('/bin/ls', command, scratch, listing)
        if (listing%exit_code /= 0) return
        first = 1
        do while (first <= len(listing%stdout))
            last = index(listing%stdout(first:), new_line('a'))
            if (last == 0) last = len(listing%stdout) - first + 2
            length = last - 1
            if (length > 0) then
                name = listing%stdout(first:first + length - 1)
                if (index(name, 'test_') == 1) then
                    file = join_path(directory, name)
                    digest = digest // name // file_digest(file)
                end if
            end if
            first = first + last
        end do
    end function directory_digest

    subroutine exercise_stale_archive(archive_path)
        character(len=*), intent(in) :: archive_path
        character(:), allocatable :: stale_dir, stale_source, stale_object, module_dir
        character(:), allocatable :: source
        type(string_list_t) :: command, fo_arguments
        type(process_result_t) :: result
        integer :: before, after

        stale_dir = join_path(scratch, 'stale-object')
        stale_source = join_path(stale_dir, 'right.f90')
        stale_object = join_path(stale_dir, 'src_right.f90.o')
        module_dir = join_path(project, 'build/fo/mod')
        source = 'module archive_right' // new_line('a') // 'contains' // new_line('a') // &
            'integer function right_value()' // new_line('a') // 'right_value = 29' // &
            new_line('a') // 'end function right_value' // new_line('a') // &
            'end module archive_right' // new_line('a')
        call write_text(stale_source, source)
        call make_directory(join_path(stale_dir, 'mod'))
        command = words([character(len=512) :: '-c', '-fPIC', '-I', module_dir, '-J', &
            join_path(stale_dir, 'mod'), '-o', stale_object, stale_source])
        call run_external(real_fc, command, stale_dir, result)
        call assert_process_ok(result, 'compile independent stale archive member')
        command = words([character(len=512) :: 'rcs', archive_path, stale_object])
        call run_external(real_ar, command, stale_dir, result)
        call assert_process_ok(result, 'inject stale but structurally valid member bytes')
        before = archive_rcs_calls(join_path(scratch, 'ar.log'))
        fo_arguments = words([character(len=32) :: 'exec', 'probe'])
        call invoke(fo_arguments, 'normal', 'normal', 'normal', result)
        call assert_process_ok(result, 'stale member triggers archive validation')
        call assert_equal_string(result%stdout, '42' // new_line('a'), &
            'stale member bytes are not used by the published executable')
        after = archive_rcs_calls(join_path(scratch, 'ar.log'))
        call assert_true(after > before, 'stale member content causes archive rebuild')
    end subroutine exercise_stale_archive

    subroutine check_warm_named_archive(archive_path)
        character(len=*), intent(in) :: archive_path
        character(:), allocatable :: source, execution_marker, expected_digest
        type(process_result_t) :: result
        type(json_value_t) :: report
        integer :: before
        character(len=1), parameter :: nl = new_line('a')

        execution_marker = join_path(scratch, 'warm-named-executed')
        source = 'program test_alpha'//nl// &
            'use archive_left, only: left_value'//nl// &
            'use archive_right, only: right_value'//nl// &
            'integer :: unit'//nl// &
            'if (left_value() + right_value() /= 42) stop 1'//nl// &
            "open(newunit=unit, file='"//execution_marker//"', status='replace')"//nl// &
            "write(unit, '(a)') 'warm-current'"//nl//'close(unit)'//nl//'end program'//nl
        ! A changed selected main defeats the whole-build stamp fast path. Its
        ! library object vector stays fixed and must reuse the complete archive.
        call write_text(join_path(project, 'test/test_alpha.f90'), source)
        expected_digest = file_digest(archive_path)
        before = log_lines(join_path(scratch, 'ar.log'))
        call invoke(words([character(len=32) :: 'test', 'test_alpha', '--json']), &
            'normal', 'normal', 'normal', result)
        call assert_process_ok(result, 'warm named case uses validated archive')
        call parse_json_report(result, report, 'warm named archive report')
        call check_tests_pass(report, 1, 'warm named archive consumer passes')
        call assert_equal_string(read_text(execution_marker), 'warm-current'//nl, &
            'the changed named program actually executes against the archive')
        call assert_equal_integer(log_lines(join_path(scratch, 'ar.log')), before, &
            'complete warm archive receipt removes all per-member ar subprocesses')
        call assert_equal_string(file_digest(archive_path), expected_digest, &
            'warm archive reuse leaves its complete bytes unchanged')

        call write_text(join_path(fake_bin, 'ar'), ar_wrapper()//nl//'# revised archiver image'//nl)
        call chmod_executable(join_path(fake_bin, 'ar'))
        call write_text(join_path(project, 'test/test_alpha.f90'), source//'! new tool action'//nl)
        call invoke(words([character(len=32) :: 'test', 'test_alpha', '--json']), &
            'fail', 'normal', 'normal', result)
        call assert_true(result%exit_code /= 0, &
            'a changed archiver image invalidates the receipt and exposes actual archiver failure')
        call assert_equal_string(file_digest(archive_path), expected_digest, &
            'failed replacement under a new archiver preserves the published archive')
        call write_text(join_path(fake_bin, 'ar'), ar_wrapper())
        call chmod_executable(join_path(fake_bin, 'ar'))
        call remove_path(execution_marker)
        call invoke(words([character(len=32) :: 'test', 'test_alpha', '--json']), &
            'normal', 'normal', 'normal', result)
        call assert_process_ok(result, 'restored archiver consumes its unchanged validated artifact')
        call assert_equal_string(read_text(execution_marker), 'warm-current'//nl, &
            'named execution recovers after the invalidated archiver action fails')
    end subroutine check_warm_named_archive

    subroutine test_no_cache_shared_cleanup(shared_path)
        character(len=*), intent(in) :: shared_path
        character(:), allocatable :: saved_cache, broken_cache
        type(process_result_t) :: result

        saved_cache = cache
        broken_cache = join_path(scratch, 'cache-is-a-file')
        call write_text(broken_cache, 'cache initialization failure')
        cache = broken_cache
        call write_text(shared_path, 'invalidate image without a CAS directory')
        call write_text(join_path(project, 'test/test_alpha.f90'), &
            test_source('test_alpha', 'no-cache-shared-rebuild'))
        call invoke(words([character(len=32) :: 'test', '--all', '--json']), &
            'normal', 'normal', 'normal', result)
        call assert_no_stages(lib_dir, '.fo-shared.tmp-')

        call write_text(shared_path, 'invalidate image before failed no-cache link')
        call write_text(join_path(project, 'test/test_alpha.f90'), &
            test_source('test_alpha', 'no-cache-shared-failure'))
        call invoke(words([character(len=32) :: 'test', '--all', '--json']), &
            'normal', 'normal', 'fail', result)
        call assert_true(result%exit_code /= 0, &
            'shared-link failure propagates when cache initialization is unavailable')
        call assert_no_stages(lib_dir, '.fo-shared.tmp-')
        cache = saved_cache
    end subroutine test_no_cache_shared_cleanup

    integer function archive_rcs_calls(path) result(count)
        character(len=*), intent(in) :: path
        character(:), allocatable :: text
        integer :: first, last, length

        count = 0
        if (.not. file_exists(path)) return
        text = read_file(path)
        first = 1
        do while (first <= len(text))
            last = index(text(first:), new_line('a'))
            if (last == 0) last = len(text) - first + 2
            length = last - 1
            if (length >= 5) then
                if (index(text(first:first + length - 1), ' rcs ') > 0) count = count + 1
            end if
            first = first + last
        end do
    end function archive_rcs_calls

    subroutine check_tests_pass(document, expected, context)
        type(json_value_t), intent(in) :: document
        integer, intent(in) :: expected
        character(len=*), intent(in) :: context
        type(json_value_t) :: tests, entry, field
        integer :: i
        tests = json_member(document, 'tests')
        call assert_equal_integer(json_size(tests), expected, context // ': expected case count')
        do i = 1, json_size(tests)
            entry = json_element(tests, i)
            field = json_member(entry, 'status')
            call assert_equal_string(json_string_value(field), 'pass', context // ': case passes')
        end do
    end subroutine check_tests_pass

end program test_archive_publication
