program test_dep_resolve
    !! Path-dependency closure resolution: transitive walk, dedup of a diamond,
    !! source-dir from the dep's own manifest, and path normalization.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_dep_resolve, only: resolved_src_t, resolve_dep_srcs, MAX_RESOLVED, &
        normalize_path, join_path
    use fo_process, only: process_getpid
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_normalize()
    call test_join()
    call test_transitive_and_dedup()
    call test_registry_counted_unresolved()
    call test_linked_worktree_fallback()
    call test_direct_path_wins_in_worktree()
    call test_malformed_git_pointer_keeps_direct_path()
    call test_missing_gitdir_keeps_direct_path()
    call test_directory_git_keeps_direct_path()

    write (output_unit, '(a,i0,a,i0,a)') 'dep_resolve: ', n_pass, ' pass, ', &
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

    subroutine test_normalize()
        character(len=512) :: out

        call normalize_path('/a/b/../c', out)
        call assert(trim(out) == '/a/c', 'collapse a/b/../c -> /a/c')
        call normalize_path('/a/./b/', out)
        call assert(trim(out) == '/a/b', 'drop . segment and trailing slash')
        call normalize_path('/a/b/c/../../d', out)
        call assert(trim(out) == '/a/d', 'collapse two levels')
    end subroutine test_normalize

    subroutine test_join()
        character(len=512) :: out

        call join_path('/proj', '../dep', out)
        call assert(trim(out) == '/dep', 'join relative ../dep against /proj')
        call join_path('/proj', '/abs/dep', out)
        call assert(trim(out) == '/abs/dep', 'absolute rel wins')
    end subroutine test_join

    subroutine test_transitive_and_dedup()
        !! root -> a (path), root -> b (path), a -> b (path). b must appear once.
        character(len=512) :: base, root, da, db
        type(resolved_src_t) :: out(MAX_RESOLVED)
        integer :: n_out, n_unres, ierr, i, n_b

        call make_tmp('fo_test_resolve', base)
        root = trim(base)//'/root'
        da = trim(base)//'/a'
        db = trim(base)//'/b'
        call mkproj(root, '[dependencies]'//new_line('a')// &
            'a = { path = "../a" }'//new_line('a')//'b = { path = "../b" }')
        call mkproj(da, '[dependencies]'//new_line('a')//'b = { path = "../b" }')
        call mkproj(db, '')
        ! give each a src dir so record_dep_src resolves a real source-dir
        call execute_command_line('mkdir -p '//trim(da)//'/src '//trim(db)//'/src')

        call resolve_dep_srcs(trim(root), out, n_out, n_unres, ierr)
        call assert(ierr == 0, 'resolve succeeds')
        call assert(n_out == 2, 'two distinct path deps (diamond deduped)')
        n_b = 0
        do i = 1, n_out
            if (index(out(i)%src_dir, '/b/src') > 0) n_b = n_b + 1
        end do
        call assert(n_b == 1, 'shared dep b compiled once')
    end subroutine test_transitive_and_dedup

    subroutine test_registry_counted_unresolved()
        !! A registry/version dep is not a path dep: it is counted as unresolved
        !! and contributes no source dir here.
        character(len=512) :: base, root
        type(resolved_src_t) :: out(MAX_RESOLVED)
        integer :: n_out, n_unres, ierr

        call make_tmp('fo_test_resolve_reg', base)
        root = trim(base)//'/root'
        call mkproj(root, '[dependencies]'//new_line('a')//'stdlib = "*"')
        call resolve_dep_srcs(trim(root), out, n_out, n_unres, ierr)
        call assert(ierr == 0, 'resolve with registry dep succeeds')
        call assert(n_out == 0, 'registry dep yields no path source dir')
        call assert(n_unres == 1, 'registry dep counted as unresolved')
    end subroutine test_registry_counted_unresolved

    subroutine test_linked_worktree_fallback()
        character(len=512) :: base, primary, sibling, worktree
        type(resolved_src_t) :: out(MAX_RESOLVED)
        integer :: n_out, n_unres, ierr

        call make_tmp('fo_test_worktree_dep', base)
        primary = trim(base)//'/main/app'
        sibling = trim(base)//'/main/lib'
        worktree = trim(base)//'/worktrees/x'
        call mkproj(primary, '')
        call mkproj(sibling, '')
        call mkproj(worktree, '[dependencies]'//new_line('a')// &
            'lib = { path = "../lib" }')
        call write_git_pointer(worktree, primary)

        call resolve_dep_srcs(worktree, out, n_out, n_unres, ierr)
        call assert(ierr == 0, 'linked worktree dependency resolves')
        call assert(n_out == 1, 'linked worktree finds one dependency')
        call assert(trim(out(1)%dir) == trim(sibling), &
            'missing direct sibling falls back to primary checkout sibling')
    end subroutine test_linked_worktree_fallback

    subroutine test_direct_path_wins_in_worktree()
        character(len=512) :: base, primary, primary_dep, direct_dep, worktree
        type(resolved_src_t) :: out(MAX_RESOLVED)
        integer :: n_out, n_unres, ierr

        call make_tmp('fo_test_worktree_direct', base)
        primary = trim(base)//'/main/app'
        primary_dep = trim(base)//'/main/lib'
        worktree = trim(base)//'/worktrees/app-wt'
        direct_dep = trim(base)//'/worktrees/lib'
        call mkproj(primary, '')
        call mkproj(primary_dep, '')
        call mkproj(direct_dep, '')
        call mkproj(worktree, '[dependencies]'//new_line('a')// &
            'lib = { path = "../lib" }')
        call write_git_pointer(worktree, primary)

        call resolve_dep_srcs(worktree, out, n_out, n_unres, ierr)
        call assert(ierr == 0, 'direct worktree dependency resolves')
        call assert(trim(out(1)%dir) == trim(direct_dep), &
            'existing manifest-relative dependency remains authoritative')
    end subroutine test_direct_path_wins_in_worktree

    subroutine test_malformed_git_pointer_keeps_direct_path()
        character(len=512) :: base, worktree, expected
        type(resolved_src_t) :: out(MAX_RESOLVED)
        integer :: n_out, n_unres, ierr, unit

        call make_tmp('fo_test_worktree_bad_git', base)
        worktree = trim(base)//'/worktrees/app-wt'
        expected = trim(base)//'/worktrees/lib'
        call mkproj(worktree, '[dependencies]'//new_line('a')// &
            'lib = { path = "../lib" }')
        open (newunit=unit, file=trim(worktree)//'/.git', status='replace')
        write (unit, '(a)') 'not a gitdir pointer'
        close (unit)

        call resolve_dep_srcs(worktree, out, n_out, n_unres, ierr)
        call assert(ierr == 0, 'malformed git pointer does not fail resolution')
        call assert(trim(out(1)%dir) == trim(expected), &
            'malformed git pointer keeps direct missing path behavior')
    end subroutine test_malformed_git_pointer_keeps_direct_path

    subroutine test_missing_gitdir_keeps_direct_path()
        character(len=512) :: base, worktree, expected
        type(resolved_src_t) :: out(MAX_RESOLVED)
        integer :: n_out, n_unres, ierr, unit

        call make_tmp('fo_test_worktree_missing_gitdir', base)
        worktree = trim(base)//'/worktrees/app-wt'
        expected = trim(base)//'/worktrees/lib'
        call mkproj(worktree, '[dependencies]'//new_line('a')// &
            'lib = { path = "../lib" }')
        open (newunit=unit, file=trim(worktree)//'/.git', status='replace')
        write (unit, '(a)') 'gitdir: '//trim(base)// &
            '/main/app/.git/worktrees/missing'
        close (unit)

        call resolve_dep_srcs(worktree, out, n_out, n_unres, ierr)
        call assert(trim(out(1)%dir) == trim(expected), &
            'nonexistent gitdir target keeps direct missing path behavior')
    end subroutine test_missing_gitdir_keeps_direct_path

    subroutine test_directory_git_keeps_direct_path()
        character(len=512) :: base, worktree, expected
        type(resolved_src_t) :: out(MAX_RESOLVED)
        integer :: n_out, n_unres, ierr

        call make_tmp('fo_test_worktree_git_dir', base)
        worktree = trim(base)//'/worktrees/app-wt'
        expected = trim(base)//'/worktrees/lib'
        call mkproj(worktree, '[dependencies]'//new_line('a')// &
            'lib = { path = "../lib" }')
        call execute_command_line('mkdir -p '//trim(worktree)//'/.git')

        call resolve_dep_srcs(worktree, out, n_out, n_unres, ierr)
        call assert(trim(out(1)%dir) == trim(expected), &
            'directory-style git metadata keeps direct missing path behavior')
    end subroutine test_directory_git_keeps_direct_path

    subroutine write_git_pointer(worktree, primary)
        character(len=*), intent(in) :: worktree, primary
        character(len=128) :: worktree_name
        integer :: unit, slash

        slash = index(trim(worktree), '/', back=.true.)
        worktree_name = worktree(slash + 1:)
        call execute_command_line('mkdir -p '//trim(primary)// &
            '/.git/worktrees/'//trim(worktree_name))
        open (newunit=unit, file=trim(worktree)//'/.git', status='replace')
        write (unit, '(a)') 'gitdir: '//trim(primary)//'/.'// &
            'git/worktrees/'//trim(worktree_name)
        close (unit)
    end subroutine write_git_pointer

    subroutine mkproj(dir, deps_block)
        character(len=*), intent(in) :: dir, deps_block
        integer :: u

        call execute_command_line('mkdir -p '//trim(dir))
        open (newunit=u, file=trim(dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "p"'
        if (len_trim(deps_block) > 0) write (u, '(a)') trim(deps_block)
        close (u)
    end subroutine mkproj

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

end program test_dep_resolve
