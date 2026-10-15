program test_symlinked_project_cli
    !! A project reached through a symlink (`link -> real`, the shape of
    !! /var -> /private/var on Darwin) behaves like the same project reached
    !! by its real path: build, test --list, test --only-changed after an
    !! edit, and lint of an absolute path spelled through the link.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_lines, append_text
    use fo_test_harness, only: make_symlink, remove_tree, assert_equal_integer
    use fo_test_harness, only: assert_contains, assert_not_contains, assert_true
    use fo_test_harness, only: assert_equal_string, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, cache, real_dir, link_dir
    type(process_result_t) :: from_real, from_link

    call resolve_driver(driver)
    call make_scratch('fo-symlinked-project-cli', scratch)
    cache = join_path(scratch, 'cache')
    real_dir = join_path(scratch, 'real')
    link_dir = join_path(scratch, 'link')

    call write_lines(join_path(real_dir, 'fpm.toml'), [character(len=32) :: &
        'name = "linked"'])
    call write_lines(join_path(real_dir, 'src/mod_a.f90'), [character(len=48) :: &
        'module mod_a', 'implicit none', 'end module'])
    call write_lines(join_path(real_dir, 'test/sel_a.f90'), [character(len=48) :: &
        'program sel_a', 'use mod_a', 'implicit none', 'end program'])
    call write_lines(join_path(real_dir, 'test/sel_b.f90'), [character(len=48) :: &
        'program sel_b', 'implicit none', 'end program'])
    call write_lines(join_path(real_dir, '.gitignore'), [character(len=8) :: 'build/'])
    call git(real_dir, 'init --quiet')
    call git(real_dir, 'add -A')
    call git(real_dir, '-c user.name=T -c user.email=t@example.invalid '// &
        '-c commit.gpgsign=false commit --quiet -m base')
    call make_symlink(real_dir, link_dir)

    call fo(link_dir, 'build', '', from_link)
    call assert_equal_integer(from_link%exit_code, 0, 'build through the link: '// &
        from_link%stderr)
    call fo(real_dir, 'build', '', from_real)
    call assert_equal_integer(from_real%exit_code, 0, 'build through the real path: '// &
        from_real%stderr)

    call fo(link_dir, 'test', '--list', from_link)
    call fo(real_dir, 'test', '--list', from_real)
    call assert_equal_integer(from_link%exit_code, from_real%exit_code, &
        'test --list exit code agrees')
    call assert_contains(from_link%stdout, 'sel_a', 'link --list names sel_a')
    call assert_contains(from_link%stdout, 'sel_b', 'link --list names sel_b')
    call assert_equal_string(from_link%stdout, from_real%stdout, &
        'test --list output agrees')

    call append_text(join_path(real_dir, 'src/mod_a.f90'), '! edited'//new_line('a'))
    call fo(link_dir, 'test', '--only-changed', from_link)
    call fo(real_dir, 'test', '--only-changed', from_real)
    call assert_equal_integer(from_link%exit_code, from_real%exit_code, &
        'test --only-changed exit code agrees')
    call assert_contains(from_link%stdout, 'sel_a', &
        'link --only-changed selects the affected case')
    call assert_not_contains(from_link%stdout, 'sel_b', &
        'link --only-changed skips the unaffected case')
    call assert_contains(from_real%stdout, 'sel_a', &
        'real --only-changed selects the affected case')
    call assert_not_contains(from_real%stdout, 'sel_b', &
        'real --only-changed skips the unaffected case')

    call fo(link_dir, 'lint', join_path(link_dir, 'src/mod_a.f90'), from_link)
    call fo(real_dir, 'lint', join_path(real_dir, 'src/mod_a.f90'), from_real)
    call assert_equal_integer(from_link%exit_code, from_real%exit_code, &
        'lint of an absolute path via the link: exit code agrees')
    call assert_true((len_trim(from_link%stdout) > 0) .eqv. &
        (len_trim(from_real%stdout) > 0), 'lint output presence agrees')

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine git(dir, text)
        character(len=*), intent(in) :: dir, text
        type(string_list_t) :: arguments
        type(process_result_t) :: result
        integer :: pos, next

        pos = 1
        do while (pos <= len_trim(text))
            next = index(text(pos:), ' ')
            if (next == 0) next = len_trim(text) - pos + 2
            if (next > 1) call list_add(arguments, text(pos:pos + next - 2))
            pos = pos + next
        end do
        call run_external('git', arguments, dir, result)
        call assert_equal_integer(result%exit_code, 0, 'git '//text//': '// &
            result%stderr)
    end subroutine git

    subroutine fo(dir, first, second, result)
        character(len=*), intent(in) :: dir, first, second
        type(process_result_t), intent(out) :: result
        type(string_list_t) :: arguments, env

        call list_add(arguments, first)
        if (len(second) > 0) call list_add(arguments, second)
        call list_add(env, 'FO_JOBS=2')
        call run_fo(driver, arguments, dir, cache, result, env, 240000)
    end subroutine fo

end program test_symlinked_project_cli
