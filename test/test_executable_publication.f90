program test_executable_publication
    !! Replacement of published executables and shared libraries. A rebuild must
    !! replace an app binary that a peer is running, and a truncated shared
    !! library in the reuse path must be replaced rather than linked again.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, read_text
    use fo_test_harness, only: make_directory, remove_tree, file_exists
    use fo_test_harness, only: assert_true, assert_equal_string
    use fo_test_harness, only: assert_equal_integer, assert_process_ok
    use fo_test_harness, only: assert_file_exists, finish_assertions
    use fo_test_harness, only: spawn_process, poll_process
    use fo_test_harness, only: terminate_process_group
    use fo_test_harness, only: current_directory, remove_path
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_fs, only: fs_copy_exec
    implicit none

    character(len=1), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch

    call resolve_driver(driver)
    call make_scratch('fo-executable-publication', scratch)
    call check_running_peer_replacement()
    call check_truncated_shared_replacement()
    call check_owned_staging_survives_cleanup()
    call check_competing_action_cache()
    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') &
        'executable-publication: peer, shared, staging and cache checks pass'

contains

    subroutine check_competing_action_cache()
        !! A peer replaces the public binary at the exact publication boundary.
        !! The original action must still restore its own observable behavior.
        character(:), allocatable :: project, peer, cache, shim, marker, cwd
        character(:), allocatable :: output, app
        type(string_list_t) :: build_args, args, env, no_args
        type(process_result_t) :: result, ran
        integer :: round

        call run_external('/usr/bin/uname', no_args, scratch, result)
        call assert_process_ok(result, 'identify publication interposition platform')
        if (result%stdout /= 'Linux' // nl) then
            write (*, '(a)') 'SKIP: deterministic rename interposition requires Linux'
            return
        end if
        project = join_path(scratch, 'cache_owner')
        peer = join_path(scratch, 'cache_peer')
        cache = join_path(scratch, 'publication_cache')
        shim = join_path(scratch, 'publication_peer.so')
        marker = join_path(scratch, 'publication_peer.marker')
        output = join_path(project, 'build/fo/bin/selected')
        app = join_path(project, 'build/fo/app/selected')
        call current_directory(cwd)
        call list_add(args, '-shared')
        call list_add(args, '-fPIC')
        call list_add(args, '-Wall')
        call list_add(args, '-Werror')
        call list_add(args, join_path(cwd, 'test-fixtures/c/link_publication_peer.c'))
        call list_add(args, '-ldl')
        call list_add(args, '-o')
        call list_add(args, shim)
        call run_external('/usr/bin/cc', args, scratch, result)
        call assert_process_ok(result, 'compile deterministic publication peer')
        call make_directory(join_path(peer, 'app'))
        call write_text(join_path(peer, 'fpm.toml'), staging_manifest())
        call write_text(join_path(peer, 'app/selected.f90'), &
            'program selected' // nl // 'print "(a)", "peer"' // nl // &
            'end program selected' // nl)
        build_args = words([character(len=16) :: 'build'])
        call run_fo(driver, build_args, peer, cache, result, timeout_ms=300000)
        call assert_process_ok(result, 'build complete competing action binary')
        call make_directory(join_path(project, 'app'))
        call write_text(join_path(project, 'fpm.toml'), staging_manifest())
        call write_text(join_path(project, 'app/selected.f90'), selected_source())
        call list_add(env, 'LD_PRELOAD=' // shim)
        call list_add(env, 'TMPDIR=' // scratch)
        call list_add(env, 'FO_TEST_PUBLISH_TARGET=' // output)
        call list_add(env, 'FO_TEST_PUBLISH_PEER=' // &
            join_path(peer, 'build/fo/bin/selected'))
        call list_add(env, 'FO_TEST_PUBLISH_MARKER=' // marker)
        do round = 1, 2
            call remove_path(marker)
            call run_fo(driver, build_args, project, cache, result, env, &
                timeout_ms=300000)
            call assert_process_ok(result, 'publisher completes after peer replacement')
            call assert_file_exists(marker, 'peer replaces the actual published output')
            call run_external(app, no_args, project, ran)
            call assert_process_ok(ran, 'complete peer replacement executes')
            call assert_equal_string(ran%stdout, 'peer' // nl, &
                'public output independently demonstrates conflicting publication')
            call remove_path(output)
            call remove_path(app)
            call run_fo(driver, build_args, project, cache, result, timeout_ms=300000)
            call assert_process_ok(result, &
                'owner restores after conflicting publication')
            call run_external(app, no_args, project, ran)
            call assert_process_ok(ran, 'restored action binary executes')
            call assert_equal_string(ran%stdout, 'selected' // nl, &
                'fresh and restored publications retain their own cached behavior')
            call remove_path(output)
            call remove_path(app)
        end do
    end subroutine check_competing_action_cache

    subroutine check_running_peer_replacement()
        !! A rebuild replaces a published app binary while a peer runs it. The
        !! peer keeps the binary it started with, and later runs get the rebuild.
        character(:), allocatable :: project, cache, app, started, release
        character(:), allocatable :: output
        type(string_list_t) :: build_args, peer_command, no_args
        type(process_result_t) :: built, rebuilt, ran
        integer :: peer, status

        project = join_path(scratch, 'running_peer_probe')
        cache = join_path(scratch, 'running_peer_cache')
        app = join_path(project, 'build/fo/app/probe')
        started = join_path(project, 'started.marker')
        release = join_path(project, 'release.marker')
        output = join_path(project, 'result.txt')
        call make_directory(project)
        call make_directory(join_path(project, 'app'))
        call write_text(join_path(project, 'fpm.toml'), &
            'name = "running_peer_probe"' // nl)
        call write_text(join_path(project, 'app/probe.f90'), probe_source('one'))
        build_args = words([character(len=16) :: 'build'])
        call run_fo(driver, build_args, project, cache, built, timeout_ms=300000)
        call assert_process_ok(built, 'initial build publishes the probe')
        call assert_file_exists(app, 'initial probe binary is published')

        call list_add(peer_command, app)
        call spawn_process(peer_command, project, peer)
        call wait_for_file(started)
        call assert_file_exists(started, 'running peer reaches its start marker')

        call write_text(join_path(project, 'app/probe.f90'), probe_source('two'))
        call run_fo(driver, build_args, project, cache, rebuilt, timeout_ms=300000)
        call assert_process_ok(rebuilt, &
            'rebuild replaces a published binary that a peer runs')

        call write_text(release, 'release' // nl)
        call wait_for_exit(peer, status)
        call assert_equal_integer(status, 0, &
            'running peer exits normally before its release deadline')
        call assert_equal_string(read_text(output), 'one' // nl, &
            'running peer keeps the binary it started with')

        call run_external(app, no_args, project, ran, timeout_ms=60000)
        call assert_process_ok(ran, 'replaced probe binary runs')
        call assert_equal_string(ran%stdout, 'two' // nl, &
            'published probe is the rebuilt binary')
    end subroutine check_running_peer_replacement

    subroutine check_truncated_shared_replacement()
        !! A published shared library that is truncated but keeps its ELF prefix
        !! must not be reused. The next relink against it replaces it.
        character(:), allocatable :: project, cache, library, peer, peer_library
        character(:), allocatable :: peer_source
        type(string_list_t) :: test_args
        type(process_result_t) :: built, rebuilt
        integer(int64) :: library_bytes
        integer :: at, rc

        project = join_path(scratch, 'shared_reuse_probe')
        cache = join_path(scratch, 'shared_reuse_cache')
        call make_directory(project)
        call make_directory(join_path(project, 'src'))
        call make_directory(join_path(project, 'test'))
        call write_text(join_path(project, 'fpm.toml'), shared_manifest())
        call write_text(join_path(project, 'src/answer_mod.f90'), answer_source())
        call write_text(join_path(project, 'test/test_alpha.f90'), &
            test_source('test_alpha', 'alpha one'))
        call write_text(join_path(project, 'test/test_beta.f90'), &
            test_source('test_beta', 'beta one'))
        test_args = words([character(len=16) :: 'test', '--all'])
        call run_fo(driver, test_args, project, cache, built, timeout_ms=300000)
        call assert_process_ok(built, 'shared-library tests build and pass')

        call find_library(join_path(project, 'build/fo/lib'), library)
        call assert_true(len(library) > 0, 'shared library is published')
        if (len(library) == 0) return
        call keep_prefix(library, 128)

        ! Changing one test source forces a relink against the published library.
        call write_text(join_path(project, 'test/test_beta.f90'), &
            test_source('test_beta', 'beta two'))
        call run_fo(driver, test_args, project, cache, rebuilt, timeout_ms=300000)
        call assert_process_ok(rebuilt, &
            'truncated published shared library is replaced before relinking')
        library_bytes = file_bytes(library)
        call assert_true(library_bytes > 128_int64, &
            'replacement shared library is complete')

        ! A different complete image keeps valid headers and the same length,
        ! but implements the wrong answer. Reuse must verify its actual bytes.
        peer = join_path(scratch, 'shared_peer/shared_reuse_probe')
        call make_directory(join_path(peer, 'src'))
        call make_directory(join_path(peer, 'test'))
        call write_text(join_path(peer, 'fpm.toml'), shared_manifest())
        peer_source = answer_source()
        at = index(peer_source, 'answer = 42')
        peer_source(at:at + 10) = 'answer = 43'
        call write_text(join_path(peer, 'src/answer_mod.f90'), peer_source)
        call write_text(join_path(peer, 'test/test_peer.f90'), &
            'program test_peer' // nl // &
            'use answer_mod, only: answer' // nl // &
            'if (answer() /= 43) error stop 1' // nl // &
            'end program test_peer' // nl)
        call run_fo(driver, test_args, peer, cache, built, timeout_ms=300000)
        call assert_process_ok(built, 'independent wrong-answer shared image runs')
        call find_library(join_path(peer, 'build/fo/lib'), peer_library)
        call assert_true(len(peer_library) > 0, 'complete peer library is available')
        if (len(peer_library) == 0) return
        call assert_true(file_bytes(peer_library) == library_bytes, &
            'wrong-answer peer has the original image length')
        rc = fs_copy_exec(peer_library, library)
        call assert_equal_integer(rc, 0, &
            'replace image with complete conflicting bytes')
        call write_text(join_path(project, 'test/test_beta.f90'), &
            test_source('test_beta', 'beta three'))
        call run_fo(driver, test_args, project, cache, rebuilt, timeout_ms=300000)
        call assert_process_ok(rebuilt, &
            'complete wrong-answer shared library is replaced before relinking')
    end subroutine check_truncated_shared_replacement

    subroutine check_owned_staging_survives_cleanup()
        !! Switching auto-executables off removes stale published binaries, but a
        !! staging directory owned by a concurrent producer must survive it.
        character(:), allocatable :: project, cache, staged, selected
        type(string_list_t) :: build_args
        type(process_result_t) :: built

        project = join_path(scratch, 'staging_cleanup_probe')
        cache = join_path(scratch, 'staging_cleanup_cache')
        staged = join_path(project, 'build/fo/bin/.fo-link.tmp-4242-1-1/binary')
        selected = join_path(project, 'build/fo/bin/selected')
        call make_directory(join_path(project, 'app'))
        call make_directory(join_path(project, 'build/fo/bin/.fo-link.tmp-4242-1-1'))
        call write_text(join_path(project, 'fpm.toml'), staging_manifest())
        call write_text(join_path(project, 'app/selected.f90'), selected_source())
        call write_text(staged, 'concurrent output' // nl)
        build_args = words([character(len=16) :: 'build'])
        call run_fo(driver, build_args, project, cache, built, timeout_ms=300000)
        call assert_process_ok(built, 'build with auto-executables off succeeds')
        call assert_file_exists(selected, 'declared executable is published')
        call assert_file_exists(staged, 'concurrent staging output survives cleanup')
        call assert_equal_string(read_text(staged), 'concurrent output' // nl, &
            'concurrent staging output is untouched')
    end subroutine check_owned_staging_survives_cleanup

    function probe_source(version) result(text)
        !! Fixture app that stays alive until release.marker appears. Its release
        !! deadline is 1200 polls of 0.05 s, about 60 s. Expiry exits with status
        !! 3 and publishes no result, so the test reports it as a failure.
        character(len=*), intent(in) :: version
        character(:), allocatable :: text

        text = 'program probe' // nl // &
            '    implicit none' // nl // &
            '    character(len=*), parameter :: version = "' // version // &
            '"' // nl // &
            '    integer :: attempt, unit_id' // nl // &
            '    logical :: released' // nl // &
            '    open (newunit=unit_id, file="started.marker", ' // &
            'status="replace")' // nl // &
            '    close (unit_id)' // nl // &
            '    released = .false.' // nl // &
            '    do attempt = 1, 1200' // nl // &
            '        inquire (file="release.marker", exist=released)' // nl // &
            '        if (released) exit' // nl // &
            '        call execute_command_line("/bin/sleep 0.05")' // nl // &
            '    end do' // nl // &
            '    inquire (file="release.marker", exist=released)' // nl // &
            '    if (.not. released) error stop 3' // nl // &
            '    open (newunit=unit_id, file="result.txt", ' // &
            'status="replace")' // nl // &
            '    write (unit_id, "(a)") version' // nl // &
            '    close (unit_id)' // nl // &
            '    print "(a)", version' // nl // &
            'end program probe' // nl
    end function probe_source

    function answer_source() result(text)
        character(:), allocatable :: text

        text = 'module answer_mod' // nl // &
            '    implicit none' // nl // &
            '    private' // nl // &
            '    public :: answer' // nl // &
            'contains' // nl // &
            '    integer function answer()' // nl // &
            '        answer = 42' // nl // &
            '    end function answer' // nl // &
            'end module answer_mod' // nl
    end function answer_source

    function test_source(name, label) result(text)
        character(len=*), intent(in) :: name, label
        character(:), allocatable :: text

        text = 'program ' // name // nl // &
            '    use answer_mod, only: answer' // nl // &
            '    implicit none' // nl // &
            '    if (answer() /= 42) stop 1' // nl // &
            '    print "(a)", "' // label // '"' // nl // &
            'end program ' // name // nl
    end function test_source

    function shared_manifest() result(text)
        character(:), allocatable :: text

        text = 'name = "shared_reuse_probe"' // nl // &
            '[build]' // nl // &
            'auto-executables = true' // nl // &
            'auto-tests = true' // nl // &
            '[extra.fo]' // nl // &
            'link = "shared"' // nl // &
            'pic = "true"' // nl
    end function shared_manifest

    function staging_manifest() result(text)
        character(:), allocatable :: text

        text = 'name = "staging_cleanup_probe"' // nl // &
            '[build]' // nl // &
            'auto-executables = false' // nl // &
            '[[executable]]' // nl // &
            'name = "selected"' // nl // &
            'source-dir = "app"' // nl // &
            'main = "selected.f90"' // nl
    end function staging_manifest

    function selected_source() result(text)
        character(:), allocatable :: text

        text = 'program selected' // nl // &
            '    print "(a)", "selected"' // nl // &
            'end program selected' // nl
    end function selected_source

    subroutine find_library(directory, path)
        !! Return the published shared library in directory, or an empty path.
        character(len=*), intent(in) :: directory
        character(:), allocatable, intent(out) :: path
        character(:), allocatable :: name
        type(process_result_t) :: listing
        type(string_list_t) :: args
        integer :: first, last

        path = ''
        call list_add(args, '-1')
        call list_add(args, directory)
        call run_external('/bin/ls', args, scratch, listing)
        if (listing%exit_code /= 0) return
        first = 1
        do while (first <= len(listing%stdout))
            last = index(listing%stdout(first:), nl)
            if (last == 0) then
                last = len(listing%stdout) + 1
            else
                last = first + last - 1
            end if
            name = listing%stdout(first:last - 1)
            if (index(name, 'libshared_reuse_probe_') == 1) then
                if (len(name) >= 3) then
                    if (name(len(name) - 2:) == '.so') then
                        path = join_path(directory, name)
                        return
                    end if
                end if
                if (len(name) >= 6) then
                    if (name(len(name) - 5:) == '.dylib') then
                        path = join_path(directory, name)
                        return
                    end if
                end if
            end if
            first = last + 1
        end do
    end subroutine find_library

    subroutine keep_prefix(path, count)
        !! Keep only the first count bytes, as an interrupted writer would leave.
        character(len=*), intent(in) :: path
        integer, intent(in) :: count
        character(len=:), allocatable :: head
        integer :: unit_id

        allocate (character(len=count) :: head)
        open (newunit=unit_id, file=path, access='stream', form='unformatted', &
            status='old', action='read')
        read (unit_id) head
        close (unit_id)
        open (newunit=unit_id, file=path, access='stream', form='unformatted', &
            status='replace', action='write')
        write (unit_id) head
        close (unit_id)
    end subroutine keep_prefix

    function file_bytes(path) result(bytes)
        character(len=*), intent(in) :: path
        integer(int64) :: bytes

        bytes = 0_int64
        inquire (file=path, size=bytes)
    end function file_bytes

    function words(items) result(list)
        character(len=*), intent(in) :: items(:)
        type(string_list_t) :: list
        integer :: i

        do i = 1, size(items)
            call list_add(list, trim(items(i)))
        end do
    end function words

    subroutine wait_for_file(path)
        !! Yield until path exists or a bounded number of polls has elapsed.
        character(len=*), intent(in) :: path
        integer :: attempt

        do attempt = 1, 600
            if (file_exists(path)) return
            call pause_briefly()
        end do
    end subroutine wait_for_file

    subroutine wait_for_exit(peer, status)
        !! Reap the peer, or stop it and fail when it does not exit in time.
        integer, intent(inout) :: peer
        integer, intent(out) :: status
        integer :: attempt
        logical :: reaped

        status = 999
        do attempt = 1, 1500
            call poll_process(peer, status)
            if (status /= 999) then
                peer = -1
                return
            end if
            call pause_briefly()
        end do
        call terminate_process_group(peer, status, reaped)
        call assert_true(reaped, 'stuck running peer is reaped')
        call assert_true(.false., 'running peer exits before timeout')
    end subroutine wait_for_exit

    subroutine pause_briefly()
        !! Poll interval only, never an ordering step. Both callers are bounded
        !! waits: wait_for_file on started.marker, and wait_for_exit on the peer's
        !! status, which only poll_process can observe because it reaps the peer.
        type(process_result_t) :: ignored
        type(string_list_t) :: args

        call list_add(args, '0.02')
        call run_external('/bin/sleep', args, scratch, ignored)
    end subroutine pause_briefly

end program test_executable_publication
