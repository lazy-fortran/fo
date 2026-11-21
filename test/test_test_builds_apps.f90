program test_test_builds_apps
    !! A test that runs the project's own app runs the app of the current
    !! sources: plain `fo test` builds the apps too, also after an app edit.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project
    type(process_result_t) :: result
    type(string_list_t) :: arguments

    call resolve_driver(driver)
    call make_scratch('fo-test-apps', scratch)
    project = join_path(scratch, 'project')
    call write_text(join_path(project, 'fpm.toml'), 'name = "greeter"'//nl)
    call write_text(join_path(project, 'test/test_greet.f90'), &
        'program test_greet'//nl// &
        '    implicit none'//nl// &
        '    character(len=16) :: got, want'//nl// &
        '    integer :: u, status'//nl// &
        '    call execute_command_line("'//project//'/build/fo/app/greet > '// &
        project//'/got.txt", exitstat=status)'//nl// &
        '    if (status /= 0) error stop "the app did not run"'//nl// &
        '    open (newunit=u, file="'//project//'/got.txt")'//nl// &
        '    read (u, "(a)") got'//nl// &
        '    close (u)'//nl// &
        '    open (newunit=u, file="'//project//'/want.txt")'//nl// &
        '    read (u, "(a)") want'//nl// &
        '    close (u)'//nl// &
        '    if (got /= want) error stop "the app is stale"'//nl// &
        'end program test_greet'//nl)

    call greeting('first')
    call run_test('the test runs the app built by fo test')
    call greeting('second')
    call run_test('the test runs the app rebuilt after its edit')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'test-builds-apps: fo test runs current apps'

contains

    subroutine greeting(word)
        character(len=*), intent(in) :: word

        call write_text(join_path(project, 'app/greet.f90'), &
            'program greet'//nl//'    print "(a)", "'//word//'"'//nl// &
            'end program greet'//nl)
        call write_text(join_path(project, 'want.txt'), word//nl)
    end subroutine greeting

    subroutine run_test(what)
        character(len=*), intent(in) :: what

        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, 'test_greet')
        call run_fo(driver, arguments, project, join_path(scratch, 'cache'), result, &
            timeout_ms=300000)
        call assert_process_ok(result, what)
    end subroutine run_test
end program test_test_builds_apps
