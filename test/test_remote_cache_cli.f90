program test_remote_cache_cli
    !! The remote tier of the compile cache, against a directory as the store
    !! (curl reads and writes file:// URLs): a build that may write uploads
    !! what it compiles, and a build on an empty local cache takes those
    !! results and compiles nothing. An archive with a member that is not a
    !! compile output is refused and the unit is compiled instead.
    !!
    !! Over HTTP (a stand-in for curl that keeps the store in a directory and
    !! answers like the server) the token and proxy of the login file reach
    !! curl, and a download is taken only when its SHA-256 equals the
    !! X-Content-SHA256 header: without the header, or with another archive
    !! under the key, the units are compiled. A store that rejects the request
    !! is asked once and named in a message, and a login for another server
    !! does not send its token.
    use fo_fs, only: fs_find_executable
    use fo_test_harness, only: process_result_t, string_list_t, list_add, argv, run_list
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree, set_mode
    use fo_test_harness, only: make_directory, read_text, file_exists
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_contains, &
        finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(:), allocatable :: driver, scratch, project, remote, wrapper, log, listing, key
    character(:), allocatable :: store, requests, login
    character(len=4096) :: real_fc, path
    type(string_list_t) :: build, exec, writer, reader, http_writer, http_reader
    type(process_result_t) :: result
    integer :: status, eol
    logical :: located

    call resolve_driver(driver)
    call make_scratch('fo-remote-cache', scratch)
    project = join_path(scratch, 'project')
    remote = join_path(scratch, 'remote')
    log = join_path(scratch, 'compiles.log')
    call make_directory(remote)
    call make_directory(join_path(remote, 'shared'))
    call write_text(join_path(project, 'fpm.toml'), 'name = "shared"'//nl)
    call write_text(join_path(project, 'src/m.f90'), 'module m'//nl// &
        '    implicit none'//nl//'    integer, parameter :: answer = 42'//nl//'end module'//nl)
    call write_text(join_path(project, 'app/main.f90'), 'program main'//nl// &
        '    use m, only: answer'//nl//'    print *, answer'//nl//'end program'//nl)

    ! The compiler under a wrapper that records every compilation of a source.
    call fs_find_executable('gfortran', real_fc, located)
    call assert_true(located, 'a Fortran compiler is installed')
    wrapper = join_path(scratch, 'bin/gfortran')
    call write_text(wrapper, '#!/bin/sh'//nl//'for a in "$@"; do case "$a" in */project/*.f90) '// &
        'echo "$a" >> "'//log//'";; esac; done'//nl//'exec "'//trim(real_fc)//'" "$@"'//nl)
    call set_mode(wrapper, int(o'755'))

    call list_add(build, 'build')
    call list_add(exec, 'exec')
    call list_add(exec, 'shared')
    call list_add(writer, 'FO_FC='//wrapper)
    call list_add(writer, 'FO_REMOTE_CACHE=file://'//remote)
    call list_add(writer, 'FO_REMOTE_CACHE_WRITE=shared')
    call list_add(reader, 'FO_FC='//wrapper)
    call list_add(reader, 'FO_REMOTE_CACHE=file://'//remote)

    call run_fo(driver, build, project, join_path(scratch, 'cache-writer'), result, writer, 120000)
    call assert_equal_integer(result%exit_code, 0, &
        'the writing build succeeds: '//result%stdout//result%stderr)
    call assert_contains(read_text(log), 'm.f90', 'the writing build compiles the module')
    call run_list(argv('ls', join_path(remote, 'shared')), status, listing)
    call assert_true(status == 0 .and. count_lines(listing) == 2, &
        'both compiled units are uploaded: '//listing)

    ! Nothing local, the same sources at the same path (the key covers it).
    call remove_tree(join_path(project, 'build'))
    call write_text(log, '')
    call run_fo(driver, build, project, join_path(scratch, 'cache-reader'), result, reader, 120000)
    call assert_equal_integer(result%exit_code, 0, &
        'the reading build succeeds: '//result%stdout//result%stderr)
    call assert_true(len_trim(read_text(log)) == 0, &
        'a build with the remote results compiles nothing: '//read_text(log))
    call run_fo(driver, exec, project, join_path(scratch, 'cache-reader'), result, reader, 120000)
    call assert_contains(result%stdout, '42', 'the program built from them runs')

    ! An archive that carries anything but compile outputs is not unpacked.
    eol = index(listing, nl)
    key = listing(1:eol - 1)
    call make_directory(join_path(scratch, 'evil'))
    call write_text(join_path(scratch, 'evil/object'), 'not an object'//nl)
    call write_text(join_path(scratch, 'evil/extra.sh'), 'echo no'//nl)
    call run_list(argv('tar', '-cf', join_path(join_path(remote, 'shared'), key), '-C', &
        join_path(scratch, 'evil'), 'object', 'extra.sh'), status)
    call assert_equal_integer(status, 0, 'a foreign archive replaces a stored result')
    call remove_tree(join_path(project, 'build'))
    call write_text(log, '')
    call run_fo(driver, build, project, join_path(scratch, 'cache-third'), result, reader, 120000)
    call assert_equal_integer(result%exit_code, 0, &
        'the build succeeds beside a foreign archive: '//result%stdout//result%stderr)
    call assert_true(count_lines(read_text(log)) == 1, &
        'only the unit with the refused archive is compiled: '//read_text(log))
    call assert_true(.not. file_exists(join_path(project, 'build/extra.sh')), &
        'nothing of the refused archive reaches the build')
    call run_fo(driver, exec, project, join_path(scratch, 'cache-third'), result, reader, 120000)
    call assert_contains(result%stdout, '42', 'the program still runs')

    ! The same over HTTP, with curl replaced by a store in a directory.
    store = join_path(scratch, 'http-store')
    requests = join_path(scratch, 'requests')
    login = join_path(scratch, 'login')
    call make_directory(store)
    call write_text(login, 'server=http://store.invalid'//nl//'token=sesame'//nl// &
        'proxy=http://proxy.invalid:3128'//nl)
    call write_text(join_path(scratch, 'fake/curl'), '#!/bin/sh'//nl// &
        'store="'//store//'"; url=; out=; hdr=; up=; sum='//nl// &
        'while IFS= read -r l; do'//nl// &
        '    echo "$l" >> "'//requests//'"'//nl// &
        '    v=${l#*\"}; v=${v%\"}'//nl// &
        '    case $l in'//nl// &
        '    url*) url=$v;; output*) out=$v;; dump-header*) hdr=$v;; upload-file*) up=$v;;'//nl// &
        '    header*X-Content-SHA256*) sum=${v#*: };;'//nl// &
        '    esac'//nl// &
        'done'//nl// &
        'key=${url##*/}'//nl// &
        'if [ -n "$up" ]; then cp "$up" "$store/$key"; echo "$sum" > "$store/$key.sum"; '// &
        'exit 0; fi'//nl// &
        'if [ -f "$store/deny" ]; then echo "HTTP/1.1 401 Unauthorized" > "$hdr"; '// &
        'exit 22; fi'//nl// &
        'if [ ! -f "$store/$key" ]; then echo "HTTP/1.1 404 Not Found" > "$hdr"; '// &
        'exit 22; fi'//nl// &
        'cp "$store/$key" "$out"'//nl// &
        'echo "HTTP/1.1 200 OK" > "$hdr"'//nl// &
        '[ ! -f "$store/$key.sum" ] || '// &
        'echo "X-Content-SHA256: $(cat "$store/$key.sum")" >> "$hdr"'//nl)
    call set_mode(join_path(scratch, 'fake/curl'), int(o'755'))
    call get_environment_variable('PATH', path)
    call list_add(http_writer, 'FO_FC='//wrapper)
    call list_add(http_writer, 'PATH='//join_path(scratch, 'fake')//':'//trim(path))
    call list_add(http_writer, 'FO_REMOTE_CACHE=http://store.invalid/cache/v1')
    call list_add(http_writer, 'FO_REMOTE_CACHE_LOGIN='//login)
    http_reader = http_writer
    call list_add(http_writer, 'FO_REMOTE_CACHE_WRITE=shared')

    call remove_tree(join_path(project, 'build'))
    call run_fo(driver, build, project, join_path(scratch, 'http-writer'), result, &
        http_writer, 120000)
    call assert_equal_integer(result%exit_code, 0, &
        'the writing build over HTTP succeeds: '//result%stdout//result%stderr)
    call run_list(argv('ls', store), status, listing)
    call assert_true(status == 0 .and. count_lines(listing) == 4, &
        'both units are uploaded with their checksums: '//listing)
    call assert_contains(read_text(requests), 'header = "Authorization: Bearer sesame"', &
        'the token of the login file is sent')
    call assert_contains(read_text(requests), 'proxy = "http://proxy.invalid:3128"', &
        'the proxy of the login file is used')

    call remove_tree(join_path(project, 'build'))
    call write_text(log, '')
    call run_fo(driver, build, project, join_path(scratch, 'http-reader'), result, &
        http_reader, 120000)
    call assert_equal_integer(result%exit_code, 0, &
        'the reading build over HTTP succeeds: '//result%stdout//result%stderr)
    call assert_true(len_trim(read_text(log)) == 0, &
        'downloads with a matching checksum are used: '//read_text(log))

    ! A login for another server keeps its token.
    call write_text(login, 'server=http://store.invalid.example'//nl//'token=sesame'//nl)
    call write_text(requests, '')
    call remove_tree(join_path(project, 'build'))
    call run_fo(driver, build, project, join_path(scratch, 'http-other'), result, &
        http_reader, 120000)
    call assert_contains(read_text(requests), 'url = "http://store.invalid/cache/v1/shared/', &
        'the store is asked with a login for another server')
    call assert_true(index(read_text(requests), 'sesame') == 0, &
        'the token of another server is not sent: '//read_text(requests))

    ! A store that rejects the request is asked once, not once per unit.
    call write_text(join_path(store, 'deny'), '')
    call write_text(requests, '')
    call write_text(log, '')
    call remove_tree(join_path(project, 'build'))
    call run_fo(driver, build, project, join_path(scratch, 'http-denied'), result, &
        http_reader, 120000)
    call assert_equal_integer(result%exit_code, 0, &
        'the build succeeds beside a rejecting store: '//result%stdout//result%stderr)
    call assert_true(count_lines(read_text(log)) == 2, &
        'both units are compiled beside a rejecting store: '//read_text(log))
    call assert_contains(result%stderr, 'remote cache http://store.invalid/cache/v1: '// &
        'lookup failed (HTTP 401', 'the rejection is reported')
    call assert_true(count_text(read_text(requests), 'url = ') == 1, &
        'the rejecting store is asked once: '//read_text(requests))
    call run_list(argv('rm', join_path(store, 'deny')), status)

    ! A server that sends no checksum is not trusted.
    call run_list(argv('sh', '-c', 'cd "$1" && for f in *.sum; do mv "$f" "$f.off"; done', &
        'sh', store), status)
    call remove_tree(join_path(project, 'build'))
    call write_text(log, '')
    call run_fo(driver, build, project, join_path(scratch, 'http-bare'), result, &
        http_reader, 120000)
    call assert_equal_integer(result%exit_code, 0, &
        'the build succeeds without checksums: '//result%stdout//result%stderr)
    call assert_true(count_lines(read_text(log)) == 2, &
        'downloads without a checksum are not used: '//read_text(log))

    ! Each key now answers with the other unit's archive and its own checksum.
    call run_list(argv('sh', '-c', 'cd "$1" && for f in *.off; do mv "$f" "${f%.off}"; done '// &
        '&& set -- $(ls | grep -v sum) && mv "$1" t && mv "$2" "$1" && mv t "$2"', &
        'sh', store), status)
    call assert_equal_integer(status, 0, 'the stored archives are exchanged')
    call remove_tree(join_path(project, 'build'))
    call write_text(log, '')
    call run_fo(driver, build, project, join_path(scratch, 'http-wrong'), result, &
        http_reader, 120000)
    call assert_equal_integer(result%exit_code, 0, &
        'the build succeeds beside wrong downloads: '//result%stdout//result%stderr)
    call assert_true(count_lines(read_text(log)) == 2, &
        'downloads with another checksum are not used: '//read_text(log))
    call assert_contains(result%stderr, 'has a wrong checksum or content', &
        'a refused download is reported')
    call run_fo(driver, exec, project, join_path(scratch, 'http-wrong'), result, &
        http_reader, 120000)
    call assert_contains(result%stdout, '42', 'the program runs after refused downloads')

    call remove_tree(scratch)
    call finish_assertions()

contains

    integer function count_lines(text) result(n)
        character(len=*), intent(in) :: text
        integer :: i

        n = 0
        do i = 1, len(text)
            if (text(i:i) == nl) n = n + 1
        end do
    end function count_lines

    integer function count_text(text, part) result(n)
        character(len=*), intent(in) :: text, part
        integer :: at, step

        n = 0
        at = 1
        do
            step = index(text(at:), part)
            if (step == 0) exit
            n = n + 1
            at = at + step
        end do
    end function count_text

end program test_remote_cache_cli
