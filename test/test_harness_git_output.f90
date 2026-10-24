program test_harness_git_output
    use fo_test_harness, only: make_scratch, write_text, run_git, run_process, &
        argv, process_result_t, assert_process_ok, assert_true, &
        assert_equal_integer, assert_equal_string, finish_assertions
    implicit none

    character(:), allocatable :: project, output, expected
    type(process_result_t) :: native
    integer :: status
    character(len=*), parameter :: nl = new_line('a')

    call make_scratch('fo-harness-git-output', project)
    call run_git(project, [character(len=16) :: 'init'], status)
    call assert_equal_integer(status, 0, 'initialize synthetic Git repository')
    call write_text(project//'/value', 'synthetic Git output fixture'//nl)
    call run_git(project, [character(len=16) :: 'add', '.'], status)
    call assert_equal_integer(status, 0, 'stage synthetic Git fixture')
    call run_git(project, [character(len=32) :: 'commit', '-qm', 'fixture'], status)
    call assert_equal_integer(status, 0, 'commit synthetic Git fixture')

    call run_process(argv('git', '-C', project, 'rev-parse', 'HEAD'), '.', native)
    call assert_process_ok(native, 'native Git identifies the fixture commit')
    call assert_true(len(native%stdout) > 1, 'native Git returns a commit identity')
    if (len(native%stdout) <= 1) call finish_assertions()
    expected = trim(native%stdout(:len(native%stdout) - 1))//' HEAD'//nl// &
        trim(native%stdout(:len(native%stdout) - 1))//' refs/heads/main'//nl
    call run_git(project, [character(len=16) :: 'show-ref', '--head'], &
        status, output)
    call check_output(expected, 'capture native refs through run_git')

    call run_git(project, [character(len=16) :: 'rev-parse', '--show-prefix'], &
        status, output)
    call check_output(nl, 'replace captured output with a shorter native result')
    call run_git(project, [character(len=16) :: 'show-ref', '--head'], &
        status, output)
    call check_output(expected, 'replace captured output with a longer native result')
    call finish_assertions()

contains

    subroutine check_output(value, label)
        character(len=*), intent(in) :: value, label

        call assert_equal_integer(status, 0, label//' exits successfully')
        call assert_true(allocated(output), label//' allocates output')
        if (.not. allocated(output)) return
        call assert_equal_integer(len(output), len(value), label//' retains its length')
        if (len(output) /= len(value)) return
        call assert_equal_string(output, value, label//' retains its bytes')
    end subroutine check_output

end program test_harness_git_output
