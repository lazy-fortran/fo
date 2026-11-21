program test_test_since
    !! fo test --since REF selects the cases that the commits since REF
    !! affect, as a landing check needs; --only-changed on the same clean
    !! tree selects none.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_process_ok, assert_contains, assert_not_contains, &
        finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project
    type(process_result_t) :: result

    call resolve_driver(driver)
    call make_scratch('fo-test-since', scratch)
    project = join_path(scratch, 'project')
    call write_text(join_path(project, 'fpm.toml'), 'name = "since_probe"'//nl)
    call module_source('a', 1)
    call module_source('b', 2)
    call test_source('a', 1)
    call test_source('b', 2)
    call git([character(len=32) :: 'init', '-q'])
    call git([character(len=32) :: 'add', '-A'])
    call git([character(len=32) :: 'commit', '-qm', 'Base'])
    call module_source('a', 3)
    call test_source('a', 3)
    call git([character(len=32) :: 'commit', '-qam', 'Change a'])

    call fo([character(len=32) :: 'test', '--since', 'HEAD~1', '--list'])
    call assert_process_ok(result, 'fo test --since lists')
    call assert_contains(result%stdout, 'test_a', 'the changed module''s test is selected')
    call assert_not_contains(result%stdout, 'test_b', 'the unchanged module''s test is not')
    call fo([character(len=32) :: 'test', '--since', 'HEAD~1'])
    call assert_process_ok(result, 'fo test --since runs the selected test')

    call fo([character(len=32) :: 'test', '--only-changed', '--list'])
    call assert_process_ok(result, 'fo test --only-changed lists')
    call assert_not_contains(result%stdout, 'test_a', 'a clean tree selects nothing')

    call fo([character(len=32) :: 'test', '--since', 'no-such-ref', '--list'])
    call assert_contains(result%stdout//result%stderr, 'no-such-ref', &
        'an unknown revision is named')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'test-since: --since selects the cases affected since REF'

contains

    subroutine module_source(name, value)
        character(len=*), intent(in) :: name
        integer, intent(in) :: value
        character(len=8) :: digits
        write (digits, '(i0)') value
        call write_text(join_path(project, 'src/'//name//'.f90'), &
            'module '//name//nl//'    implicit none'//nl// &
            '    integer, parameter :: value_'//name//' = '//trim(digits)//nl// &
            'end module '//name//nl)
    end subroutine module_source

    subroutine test_source(name, value)
        character(len=*), intent(in) :: name
        integer, intent(in) :: value
        character(len=8) :: digits
        write (digits, '(i0)') value
        call write_text(join_path(project, 'test/test_'//name//'.f90'), &
            'program test_'//name//nl//'    use '//name//nl//'    implicit none'//nl// &
            '    if (value_'//name//' /= '//trim(digits)//') error stop 1'//nl// &
            'end program test_'//name//nl)
    end subroutine test_source

    subroutine git(words)
        character(len=*), intent(in) :: words(:)
        type(string_list_t) :: arguments
        integer :: i
        do i = 1, size(words)
            call list_add(arguments, trim(words(i)))
        end do
        call run_external('git', arguments, project, result)
        call assert_process_ok(result, 'git fixture command')
    end subroutine git

    subroutine fo(words)
        character(len=*), intent(in) :: words(:)
        type(string_list_t) :: arguments
        integer :: i
        do i = 1, size(words)
            call list_add(arguments, trim(words(i)))
        end do
        call run_fo(driver, arguments, project, join_path(scratch, 'cache'), result, &
            timeout_ms=300000)
    end subroutine fo
end program test_test_since
