program test_update_dirty_cli
    !! `fo update` must retain locally edited dependency checkouts.
    use fo_util, only: temporary_root
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_process, only: process_getpid, process_getcwd
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_test_harness, only: run_list, argv, run_git
    implicit none

    character(len=1024) :: root, fo_bin, cwd, output
    integer :: status, n_pass, n_fail
    logical :: exists_binary

    n_pass = 0
    n_fail = 0
    call get_environment_variable('FO_BIN', fo_bin, status=status)
    if (status /= 0 .or. len_trim(fo_bin) == 0) then
        call process_getcwd(cwd, status)
        if (status /= 0) stop 1
        fo_bin = trim(cwd)//'/build/fo/app/fo'
    end if
    inquire (file=trim(fo_bin), exist=exists_binary)
    if (.not. exists_binary) then
        write (error_unit, '(a)') 'FO_BIN missing for update CLI fixture'
        stop 1
    end if

    write (root, '(a,i0)') temporary_root()//'/fo_update_dirty_cli-', process_getpid()
    call fs_remove_tree(trim(root))
    call write_file(trim(root)//'/fpm.toml', &
        'name = "update_fixture"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'up = { git = "https://example.invalid/up.git" }')
    call init_checkout('a_clean')
    call init_checkout('up')
    call write_file(trim(root)//'/build/dependencies/up/source.txt', 'edited')
    call write_file(trim(root)//'/build/cache.toml', 'cache')
    call write_file(trim(root)//'/build/gfortran_AB12CD34EF56/up.mod', 'module')

    call run_update(status, output)
    call assert(status /= 0, 'tracked edit makes CLI fail')
    call assert(index(output, 'up') > 0 .and. &
        index(output, 'local changes') > 0, 'CLI names the dirty checkout')
    call assert(index(output, 'commit or stash') > 0, &
        'CLI explains how to proceed')
    call assert(file_has(trim(root)//'/build/dependencies/up/source.txt', 'edited'), &
        'tracked edit survives')

    call run_git(trim(root)//'/build/dependencies/up', &
        [character(len=16) :: 'restore', 'source.txt'], status)
    call assert(status == 0, 'restore tracked fixture edit')
    call write_file(trim(root)//'/build/dependencies/up/new.txt', 'untracked')
    call run_update(status, output)
    call assert(status /= 0, 'untracked edit makes CLI fail')
    call assert(file_has(trim(root)//'/build/dependencies/up/new.txt', 'untracked'), &
        'untracked edit survives')
    call assert(file_has(trim(root)//'/build/dependencies/a_clean/source.txt', &
        'baseline'), 'other checkout survives atomically')
    call assert(file_has(trim(root)//'/build/cache.toml', 'cache'), &
        'fpm cache record survives refusal')
    call assert(file_has(trim(root)//'/build/gfortran_AB12CD34EF56/up.mod', &
        'module'), 'compiled object survives refusal')

    call run_git(trim(root)//'/build/dependencies/up', &
        [character(len=16) :: 'add', 'new.txt'], status)
    call assert(status == 0, 'stage fixture edits')
    call run_git(trim(root)//'/build/dependencies/up', &
        [character(len=16) :: 'commit', '-qm', 'clean'], status)
    call assert(status == 0, 'commit fixture edits')

    call run_update(status, output)
    call assert(status == 0, 'clean checkout refresh succeeds')
    call assert(index(output, 'next build re-fetches') > 0, &
        'CLI reports successful refresh')
    call assert(.not. file_exists(trim(root)//'/build/dependencies/up'), &
        'clean dependency checkout removed')
    call assert(.not. file_exists(trim(root)//'/build/dependencies/a_clean'), &
        'other clean checkout removed')
    call assert(.not. file_exists(trim(root)//'/build/cache.toml'), &
        'fpm cache record invalidated')
    call assert(.not. file_exists(trim(root)// &
        '/build/gfortran_AB12CD34EF56/up.mod'), &
        'compiled dependency profile invalidated')

    call fs_remove_tree(trim(root))
    write (output_unit, '(a,i0,a,i0,a)') 'update_dirty_cli: ', n_pass, &
        ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    logical function file_exists(path) result(found)
        character(len=*), intent(in) :: path
        inquire (file=trim(path), exist=found)
    end function file_exists

    logical function file_has(path, expected) result(matches)
        character(len=*), intent(in) :: path, expected
        character(len=1024) :: line
        integer :: file_unit, file_ios

        matches = .false.
        open (newunit=file_unit, file=trim(path), status='old', iostat=file_ios)
        if (file_ios /= 0) return
        read (file_unit, '(a)', iostat=file_ios) line
        close (file_unit)
        if (file_ios == 0) matches = trim(line) == expected
    end function file_has

    subroutine assert(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//description
        end if
    end subroutine assert

    subroutine write_file(path, content)
        character(len=*), intent(in) :: path, content
        integer :: slash, file_unit, exitcode

        slash = index(trim(path), '/', back=.true.)
        call fs_make_dir(path(:slash - 1), exitcode)
        if (exitcode /= 0) stop 1
        open (newunit=file_unit, file=trim(path), status='replace')
        write (file_unit, '(a)') trim(content)
        close (file_unit)
    end subroutine write_file

    subroutine init_checkout(name)
        character(len=*), intent(in) :: name
        character(len=1024) :: checkout
        integer :: exitcode

        checkout = trim(root)//'/build/dependencies/'//trim(name)
        call write_file(trim(checkout)//'/source.txt', 'baseline')
        call run_git(trim(checkout), [character(len=16) :: 'init', '-q'], exitcode)
        if (exitcode /= 0) stop 1
        call run_git(trim(checkout), [character(len=16) :: 'add', 'source.txt'], exitcode)
        if (exitcode /= 0) stop 1
        call run_git(trim(checkout), [character(len=16) :: 'commit', '-qm', 'baseline'], &
            exitcode)
        if (exitcode /= 0) stop 1
    end subroutine init_checkout

    subroutine run_update(exitcode, result)
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: result
        character(:), allocatable :: log_text
        integer :: first_line_end

        call run_list(argv(trim(fo_bin), 'update'), exitcode, &
            log_text, cwd=trim(root))
        first_line_end = index(log_text, new_line('a'))
        if (first_line_end == 0) first_line_end = len(log_text) + 1
        result = log_text(:first_line_end - 1)
    end subroutine run_update

end program test_update_dirty_cli
