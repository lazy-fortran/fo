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
