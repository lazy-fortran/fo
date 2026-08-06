program test_dep_update
    !! Refreshing external dependencies.
    !!
    !! The behaviour under test is what a build sees afterwards: the cached
    !! clone and the compiled dependency objects are gone, the project's own
    !! native build tree is untouched, and a dependency whose sources have been
    !! removed while its objects remain is reported so the build re-fetches it
    !! instead of linking artifacts nothing can reproduce.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_dep_update, only: dep_update_run, dep_update_missing_sources, &
        MAX_UPDATE_NAMES
    use fo_process, only: process_getpid
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_update_drops_clone_and_objects()
    call test_update_keeps_native_tree()
    call test_path_only_project_has_nothing_to_refresh()
    call test_missing_sources_are_reported()

    write (output_unit, '(a,i0,a,i0,a)') 'dep_update: ', n_pass, ' pass, ', &
        n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_update_drops_clone_and_objects()
        character(len=512) :: root
        integer :: n_deps
        logical :: refreshed

        call make_git_project(root)
        call dep_update_run(trim(root), n_deps, refreshed)
        call assert(refreshed, 'update reports a refresh for a git dependency')
        call assert(n_deps == 1, 'update counts the external dependency')
        call assert(.not. exists(trim(root)//'/build/dependencies/up'), &
            'update drops the cached clone')
        call assert(.not. exists(trim(root)// &
            '/build/gfortran_AB12CD34EF56/up.mod'), &
            'update drops the compiled dependency objects')
        call remove_tree(root)
    end subroutine test_update_drops_clone_and_objects

    subroutine test_update_keeps_native_tree()
        character(len=512) :: root
        integer :: n_deps
        logical :: refreshed

        call make_git_project(root)
        call dep_update_run(trim(root), n_deps, refreshed)
        call assert(exists(trim(root)//'/build/fo/mod/own.mod'), &
            'update keeps the project native build tree')
        call remove_tree(root)
    end subroutine test_update_keeps_native_tree

    subroutine test_path_only_project_has_nothing_to_refresh()
        character(len=512) :: root
        integer :: n_deps
        logical :: refreshed

        call make_tmp('fo_test_update_path', root)
        call write_project(root, '[dependencies]'//new_line('a')// &
            'sibling = { path = "../sibling" }')
        call touch(trim(root)//'/build/gfortran_AB12CD34EF56/keep.mod')
        call dep_update_run(trim(root), n_deps, refreshed)
        call assert(.not. refreshed, 'a path-only project needs no refresh')
        call assert(n_deps == 0, 'path dependencies are not counted')
        call assert(exists(trim(root)//'/build/gfortran_AB12CD34EF56/keep.mod'), &
            'a path-only project keeps its build tree')
        call remove_tree(root)
    end subroutine test_path_only_project_has_nothing_to_refresh

    subroutine test_missing_sources_are_reported()
        character(len=512) :: root
        character(len=256) :: names(MAX_UPDATE_NAMES)
        integer :: n_names

        call make_git_project(root)
        call dep_update_missing_sources(trim(root), names, n_names)
        call assert(n_names == 0, 'a present clone is not reported missing')

        call remove_tree(trim(root)//'/build/dependencies')
        call dep_update_missing_sources(trim(root), names, n_names)
        call assert(n_names == 1, 'a removed clone is reported missing')
        if (n_names == 1) then
            call assert(trim(names(1)) == 'up', &
                'the missing dependency is named')
        end if
        call remove_tree(root)
    end subroutine test_missing_sources_are_reported

    subroutine make_git_project(root)
        character(len=*), intent(out) :: root

        call make_tmp('fo_test_update_git', root)
        call write_project(root, '[dependencies]'//new_line('a')// &
            'up = { git = "https://example.invalid/up.git", branch = "main" }')
        call touch(trim(root)//'/build/dependencies/up/fpm.toml')
        call touch(trim(root)//'/build/gfortran_AB12CD34EF56/up.mod')
        call touch(trim(root)//'/build/fo/mod/own.mod')
    end subroutine make_git_project

    subroutine write_project(dir, deps_block)
        character(len=*), intent(in) :: dir, deps_block
        integer :: u

        call execute_command_line('mkdir -p '//trim(dir))
        open (newunit=u, file=trim(dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "p"'
        if (len_trim(deps_block) > 0) write (u, '(a)') trim(deps_block)
        close (u)
    end subroutine write_project

    subroutine touch(path)
        character(len=*), intent(in) :: path
        integer :: u, cut

        cut = index(trim(path), '/', back=.true.)
        call execute_command_line('mkdir -p '//trim(path(:cut - 1)))
        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') 'x'
        close (u)
    end subroutine touch

    logical function exists(path) result(found)
        character(len=*), intent(in) :: path

        inquire (file=trim(path), exist=found)
    end function exists

    subroutine remove_tree(path)
        character(len=*), intent(in) :: path

        call execute_command_line('rm -rf '//trim(path))
    end subroutine remove_tree

    subroutine make_tmp(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path
        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a,i0)') '/tmp/', trim(prefix), '-', &
            process_getpid(), '-', count, '-', serial
    end subroutine make_tmp

end program test_dep_update
