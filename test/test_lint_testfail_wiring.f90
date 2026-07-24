program test_lint_testfail_wiring
    !! The failure-path rule is only worth having if the staged `fo` pipeline
    !! actually runs it. It previously lived inside lint_compiler, which the
    !! pipeline never calls, so a vacuous test could be added and `fo` would
    !! still print "Lint: OK". These tests cover the entry point the pipeline
    !! uses, on a real project layout.
    use fo_lint, only: lint_warning_t, lint_testfail_files, MAX_WARNINGS
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call require_vacuous_test_is_reported()
    call require_honest_test_is_not_reported()
    call require_source_program_is_ignored()

    write (*, '(a,i0,a,i0,a)') 'lint_testfail_wiring: ', n_pass, ' pass, ', &
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
            write (*, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine make_project(root)
        !! A minimal fpm project with a test/ directory.
        character(len=*), intent(in) :: root
        integer :: u

        call execute_command_line('rm -rf "'//trim(root)//'"', wait=.true.)
        call execute_command_line('mkdir -p "'//trim(root)//'/test"', wait=.true.)
        call execute_command_line('mkdir -p "'//trim(root)//'/src"', wait=.true.)
        open (newunit=u, file=trim(root)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "wiring_fixture"'
        close (u)
    end subroutine make_project

    subroutine write_program(path, body)
        character(len=*), intent(in) :: path, body
        integer :: u

        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') trim(body)
        close (u)
    end subroutine write_program

    subroutine scan(root, path, n_found)
        character(len=*), intent(in) :: root, path
        integer, intent(out) :: n_found
        type(lint_warning_t), allocatable :: w(:)
        character(len=512) :: sel(1)

        allocate (w(MAX_WARNINGS))
        sel(1) = trim(path)
        call lint_testfail_files(trim(root), sel, 1, w, n_found)
        deallocate (w)
    end subroutine scan

    subroutine require_vacuous_test_is_reported()
        character(len=512) :: root, path
        integer :: n

        root = '/tmp/fo_wiring_vacuous'
        call make_project(root)
        path = trim(root)//'/test/test_tallies_only.f90'
        call write_program(path, &
            'program test_tallies_only'//new_line('a')// &
            '    implicit none'//new_line('a')// &
            '    integer :: failures'//new_line('a')// &
            '    failures = 1'//new_line('a')// &
            '    if (failures > 0) print *, "some tests failed"'//new_line('a')// &
            'end program test_tallies_only')

        call scan(root, path, n)
        call assert(n == 1, &
            'a test that only prints its failures is reported through the '// &
            'pipeline entry point')
        call execute_command_line('rm -rf "'//trim(root)//'"', wait=.true.)
    end subroutine require_vacuous_test_is_reported

    subroutine require_honest_test_is_not_reported()
        character(len=512) :: root, path
        integer :: n

        root = '/tmp/fo_wiring_honest'
        call make_project(root)
        path = trim(root)//'/test/test_stops.f90'
        call write_program(path, &
            'program test_stops'//new_line('a')// &
            '    implicit none'//new_line('a')// &
            '    integer :: failures'//new_line('a')// &
            '    failures = 1'//new_line('a')// &
            '    if (failures > 0) stop 1'//new_line('a')// &
            'end program test_stops')

        call scan(root, path, n)
        call assert(n == 0, 'a test with a nonzero stop is not reported')
        call execute_command_line('rm -rf "'//trim(root)//'"', wait=.true.)
    end subroutine require_honest_test_is_not_reported

    subroutine require_source_program_is_ignored()
        !! A program under src/ is a tool, not a check, and may exit 0 always.
        character(len=512) :: root, path
        integer :: n

        root = '/tmp/fo_wiring_srcprog'
        call make_project(root)
        path = trim(root)//'/src/tool_main.f90'
        call write_program(path, &
            'program tool_main'//new_line('a')// &
            '    implicit none'//new_line('a')// &
            '    print *, "done"'//new_line('a')// &
            'end program tool_main')

        call scan(root, path, n)
        call assert(n == 0, 'a program outside the test directory is ignored')
        call execute_command_line('rm -rf "'//trim(root)//'"', wait=.true.)
    end subroutine require_source_program_is_ignored

end program test_lint_testfail_wiring
