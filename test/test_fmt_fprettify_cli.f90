program test_fmt_fprettify_cli
    !! A project with an fprettify configuration checks its sources with
    !! fprettify, so fo fmt must accept fprettify output unchanged: select-type
    !! guards, continuation alignment and OpenMP sentinel lines included.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, read_text
    use fo_test_harness, only: assert_equal_string, assert_process_ok
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project, source, after
    type(process_result_t) :: result
    type(string_list_t) :: command
    integer :: status

    call execute_command_line('fprettify --version > /dev/null 2>&1', &
        exitstat=status)
    if (status /= 0) then
        print '(a)', 'SKIP: fprettify is not installed'
        stop
    end if

    call resolve_driver(driver)
    call make_scratch('fo-fmt-fprettify', scratch)
    project = join_path(scratch, 'project')
    call write_text(join_path(project, 'fpm.toml'), 'name = "probe"'//nl)
    call write_text(join_path(project, '.fprettify'), 'indent = 4'//nl// &
        'line-length = 88'//nl)
    source = 'module probe'//nl// &
        '    use iso_fortran_env, only: dp => real64, int32, &'//nl// &
        '                               int64'//nl// &
        '    implicit none'//nl// 'contains'//nl// &
        '    subroutine show(x)'//nl// '        class(*), intent(in) :: x'//nl// &
        '!$      integer :: nthreads'//nl// '        select type (x)'//nl// &
        '        type is (integer)'//nl// '            print *, x'//nl// &
        '        class default'//nl// '            print *, "other"'//nl// &
        '        end select'//nl// '    end subroutine show'//nl// &
        'end module probe'//nl
    call write_text(join_path(project, 'src/probe.f90'), source)

    command = string_list_t()
    call list_add(command, 'fmt')
    call list_add(command, '--check')
    call list_add(command, 'src/probe.f90')
    call run_fo(driver, command, project, join_path(scratch, '.fo-cache'), result)
    call assert_process_ok(result, 'fprettify-formatted source passes fo fmt --check')

    command = string_list_t()
    call list_add(command, 'fmt')
    call list_add(command, 'src/probe.f90')
    call run_fo(driver, command, project, join_path(scratch, '.fo-cache'), result)
    call assert_process_ok(result, 'fo fmt runs with an fprettify configuration')
    after = read_text(join_path(project, 'src/probe.f90'))
    call assert_equal_string(after, source, 'fo fmt keeps fprettify output unchanged')
    call finish_assertions()
end program test_fmt_fprettify_cli
