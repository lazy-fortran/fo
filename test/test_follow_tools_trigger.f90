program test_follow_tools_trigger
    !! fo starts an installed follow-tools.sh in the background when its last
    !! check is older than five minutes, not when it is recent, and never
    !! with FO_NO_FOLLOW=1. Started from a Git hook in a linked worktree, it
    !! leaves that worktree's HEAD and the commit alone.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: remove_path, file_exists, sleep_ms, assert_true
    use fo_test_harness, only: finish_assertions, set_mode, assert_process_ok
    use fo_test_harness, only: assert_equal_string
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fx_string, only: strip
    implicit none

    character(:), allocatable :: driver, scratch, state
    type(process_result_t) :: result

    call resolve_driver(driver)
    call make_scratch('fo-follow', scratch)
    state = join_path(scratch, 'state')
    call write_text(join_path(state, 'src/fortyard/scripts/follow-tools.sh'), &
        ': > "'//state//'/ran"; : > "'//state//'/checked"'//new_line('a'))

    call invoke('')
    call assert_true(appears(join_path(state, 'ran')), 'a first run starts the follower')

    call remove_path(join_path(state, 'ran'))
    call invoke('')
    call sleep_ms(1500)
    call assert_true(.not. file_exists(join_path(state, 'ran')), &
        'a check less than five minutes old starts nothing')

    call remove_path(join_path(state, 'checked'))
    call invoke('FO_NO_FOLLOW=1')
    call sleep_ms(1500)
    call assert_true(.not. file_exists(join_path(state, 'ran')), &
        'FO_NO_FOLLOW=1 starts nothing')

    ! The stable copy beside the state is preferred to the clone's.
    call remove_path(join_path(state, 'checked'))
    call write_text(join_path(state, 'follow-tools.sh'), &
        ': > "'//state//'/ran-stable"; : > "'//state//'/checked"'//new_line('a'))
    call invoke('')
    call assert_true(appears(join_path(state, 'ran-stable')), 'the stable copy runs')
    call assert_true(.not. file_exists(join_path(state, 'ran')), 'the clone''s copy does not')

    call hooked_commit()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'follow-tools-trigger: started when due, otherwise not'

contains

    subroutine hooked_commit()
        !! Git runs a pre-commit hook with GIT_DIR and GIT_INDEX_FILE naming the
        !! committing worktree. A follower that inherits them checks out in that
        !! worktree instead of its own clone, as follow-tools.sh does.
        character(:), allocatable :: repo, tree, clone, nl

        nl = new_line('a')
        repo = join_path(scratch, 'repo')
        tree = join_path(scratch, 'tree')
        clone = join_path(state, 'src/fortyard')
        call git(scratch, [character(len=256) :: 'init', '-q', repo])
        call git(scratch, [character(len=256) :: 'init', '-q', clone])
        call write_text(join_path(repo, 'a.txt'), '1'//nl)
        call git(repo, [character(len=256) :: 'add', 'a.txt'])
        call git(repo, [character(len=256) :: 'commit', '-qm', 'one'])
        call git(repo, [character(len=256) :: 'commit', '-q', '--allow-empty', '-m', 'two'])
        call git(clone, [character(len=256) :: 'commit', '-q', '--allow-empty', '-m', 'one'])
        call git(clone, [character(len=256) :: 'commit', '-q', '--allow-empty', '-m', 'two'])
        call git(repo, [character(len=256) :: 'worktree', 'add', '-q', '-b', 'topic', tree])
        call write_text(join_path(state, 'follow-tools.sh'), &
            'git -C "'//clone//'" checkout -q --detach HEAD~1'//nl// &
            ': > "'//state//'/checked"; : > "'//state//'/ran-hook"'//nl)
        call remove_path(join_path(state, 'checked'))
        ! The hook returns once the follower has run, so its checkout precedes
        ! Git's update of HEAD.
        call write_text(join_path(scratch, 'hooks/pre-commit'), '#!/bin/sh'//nl// &
            '"'//driver//'" fmt --check missing.f90 >/dev/null 2>&1'//nl// &
            'i=0; while [ ! -e "'//state//'/ran-hook" ] && [ $i -lt 300 ]; do'//nl// &
            '    sleep 0.1; i=$((i + 1))'//nl//'done'//nl//'exit 0'//nl)
        call set_mode(join_path(scratch, 'hooks/pre-commit'), int(o'755'))
        call write_text(join_path(tree, 'a.txt'), '2'//nl)
        call git(tree, [character(len=256) :: '-c', &
            'core.hooksPath='//join_path(scratch, 'hooks'), 'commit', '-qam', 'hooked'])
        call assert_true(file_exists(join_path(state, 'ran-hook')), &
            'the hook started the follower')
        call git(tree, [character(len=256) :: 'symbolic-ref', 'HEAD'])
        call assert_equal_string(strip(result%stdout), 'refs/heads/topic', &
            'the worktree stays on its branch')
        call git(tree, [character(len=256) :: 'log', '-1', '--format=%s', 'topic'])
        call assert_equal_string(strip(result%stdout), 'hooked', 'the commit lands on the branch')
        call git(clone, [character(len=256) :: 'log', '-1', '--format=%s'])
        call assert_equal_string(strip(result%stdout), 'one', &
            'the follower checks out in its own clone')
    end subroutine hooked_commit

    subroutine git(dir, words)
        character(len=*), intent(in) :: dir, words(:)
        type(string_list_t) :: arguments, environment
        integer :: i

        call list_add(arguments, '-C')
        call list_add(arguments, dir)
        do i = 1, size(words)
            call list_add(arguments, trim(words(i)))
        end do
        call list_add(environment, 'FY_TOOLS_HOME='//state)
        call list_add(environment, 'GIT_CONFIG_GLOBAL=/dev/null')
        call list_add(environment, 'GIT_CONFIG_NOSYSTEM=1')
        call list_add(environment, 'GIT_AUTHOR_NAME=fo test')
        call list_add(environment, 'GIT_AUTHOR_EMAIL=fo@example.invalid')
        call list_add(environment, 'GIT_COMMITTER_NAME=fo test')
        call list_add(environment, 'GIT_COMMITTER_EMAIL=fo@example.invalid')
        call run_external('git', arguments, scratch, result, environment, timeout_ms=60000)
        call assert_process_ok(result, 'git '//trim(words(1)))
    end subroutine git

    subroutine invoke(extra)
        character(len=*), intent(in) :: extra
        type(string_list_t) :: arguments, environment

        call list_add(arguments, 'fmt')
        call list_add(arguments, '--check')
        call list_add(arguments, 'missing.f90')
        call list_add(environment, 'FY_TOOLS_HOME='//state)
        if (len(extra) > 0) call list_add(environment, extra)
        call run_fo(driver, arguments, scratch, join_path(scratch, 'cache'), result, &
            extra_environment=environment, timeout_ms=60000)
    end subroutine invoke

    logical function appears(path)
        character(len=*), intent(in) :: path
        integer :: i

        appears = .false.
        do i = 1, 50
            if (file_exists(path)) then
                appears = .true.
                return
            end if
            call sleep_ms(100)
        end do
    end function appears
end program test_follow_tools_trigger
