program test_remote_cache_cli
    !! The remote tier of the compile cache, against a directory as the store
    !! (curl reads and writes file:// URLs): a build that may write uploads
    !! what it compiles, and a build on an empty local cache takes those
    !! results and compiles nothing. An archive with a member that is not a
    !! compile output is refused and the unit is compiled instead.
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
    character(len=4096) :: real_fc
    type(string_list_t) :: build, exec, writer, reader
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

    ! Another machine: nothing local, the same sources.
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

end program test_remote_cache_cli
