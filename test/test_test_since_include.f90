program test_test_since_include
    !! fo test --since: a changed include file (Fortran include or #include)
    !! selects the tests of the sources that include it, not every case.
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
    call make_scratch('fo-test-since-include', scratch)
    project = join_path(scratch, 'project')
    call write_text(join_path(project, 'fpm.toml'), 'name = "include_probe"'//nl)
    call write_text(join_path(project, 'src/a.f90'), &
        'module a'//nl//'    implicit none'//nl//'contains'//nl// &
        '    include ''a_body.inc'''//nl//'end module a'//nl)
    call write_text(join_path(project, 'src/b.F90'), &
        'module b'//nl//'    implicit none'//nl//'contains'//nl// &
        '#include "b_body.inc"'//nl//'end module b'//nl)
    call body('a', 1)
    call body('b', 2)
    call test_source('a', 1)
    call test_source('b', 2)
    call git([character(len=32) :: 'init', '-q'])
    call git([character(len=32) :: 'add', '-A'])
    call git([character(len=32) :: 'commit', '-qm', 'Base'])

    call body('a', 3)
    call test_source('a', 3)
    call git([character(len=32) :: 'commit', '-qam', 'Change the include of a'])
    call fo([character(len=32) :: 'test', '--since', 'HEAD~1', '--list'])
    call assert_process_ok(result, 'fo test --since lists')
    call assert_contains(result%stdout, 'test_a', 'the includer''s test is selected')
    call assert_not_contains(result%stdout, 'test_b', 'the other test is not')
    call assert_not_contains(result%stdout//result%stderr, 'affected: widened', &
        'a Fortran include does not widen the selection')

    call body('b', 4)
    call test_source('b', 4)
    call git([character(len=32) :: 'commit', '-qam', 'Change the include of b'])
    call fo([character(len=32) :: 'test', '--since', 'HEAD~1', '--list'])
    call assert_contains(result%stdout, 'test_b', 'the #include user''s test is selected')
    call assert_not_contains(result%stdout, 'test_a', 'the other test is not')
    call assert_not_contains(result%stdout//result%stderr, 'affected: widened', &
        'a preprocessor include does not widen the selection')
    call fo([character(len=32) :: 'test', '--since', 'HEAD~2'])
    call assert_process_ok(result, 'the selected tests pass')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'test-since-include: an include selects its includers'

contains

    subroutine body(name, value)
        character(len=*), intent(in) :: name
        integer, intent(in) :: value
        character(len=8) :: digits
        write (digits, '(i0)') value
        call write_text(join_path(project, 'src/'//name//'_body.inc'), &
            '    integer function value_'//name//'()'//nl// &
            '        value_'//name//' = '//trim(digits)//nl// &
            '    end function value_'//name//nl)
    end subroutine body

    subroutine test_source(name, value)
        character(len=*), intent(in) :: name
        integer, intent(in) :: value
        character(len=8) :: digits
        write (digits, '(i0)') value
        call write_text(join_path(project, 'test/test_'//name//'.f90'), &
            'program test_'//name//nl//'    use '//name//nl//'    implicit none'//nl// &
            '    if (value_'//name//'() /= '//trim(digits)//') error stop 1'//nl// &
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
end program test_test_since_include
